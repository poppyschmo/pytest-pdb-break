;;; pytest-pdb-break.el --- pytest-pdb-break runner -*- lexical-binding: t -*-

;; Author: Jane Soko <poppyschmo@protonmail.com>
;; URL: https://github.com/poppyschmo/pytest-pdb-break
;; Version: 0.0.1
;; Keywords: python testing
;; Package-Requires: ((emacs "25"))

;;; Commentary:

;; Installation: no MELPA, but `straight.el' users can use this recipe:
;;
;;   '(:host github :repo "poppyschmo/pytest-pdb-break"
;;     :files (:defaults "emacs/*.el" (:exclude "emacs/*-test.el")))
;;
;; Usage: with point in the body of some test, run M-x `pytest-pdb-break-here'
;;
;; This command can only handle the "console script"/entry-point invocation
;; style (as opposed to "python -m pytest"). If a pytest executable doesn't
;; appear in PATH, `pytest-pdb-break-pytest-executable' must be set.
;;
;; Regarding the completion modifications:
;;
;;   1. they're only meant for use with pdb++.
;;   2. they may not respond correctly when summoned by third-party tools like
;;      `company-capf'.
;;
;; TODO detect presence of pdb++
;; TODO make completion wrapper work in "interactive" REPL
;; TODO tramp
;; TODO add option to inject pdb++
;;

;;; Code:

(require 'find-func)
(require 'subr-x)
(require 'python)

(defgroup pytest-pdb-break nil
  "Emacs integration for the pdb-break pytest plugin."
  :prefix "pytest-pdb-break-"
  :group 'pytest)

(defcustom pytest-pdb-break-extra-opts nil
  "List of extra args passed to pytest.
May be useful in a `dir-locals-file'. For example, this `python-mode'
entry unsets cmd-line options from a project ini:
\(pytest-pdb-break-extra-opts \"-o\" \"addopts=\")."
  :group 'pytest
  :type 'list)

(defcustom pytest-pdb-break-pytest-executable nil
  "Path to an alternate pytest executable.
If set, `executable-find' won't be consulted. For certain actions, like
querying helpers, this script's shebanged Python may be used in place of
`python-shell-interpreter'."
  :group 'pytest
  :type 'string)

(defvar pytest-pdb-break-processes nil
  "List of processes started via `pytest-pdb-break-here'.")

(defvar pytest-pdb-break--home nil
  "Real path of this project's root directory.")

(defun pytest-pdb-break--homer ()
  "Find the real path of the directory containing this file.
Store absolute form (with trailing sep) in `pytest-pdb-break--home'.
This is the root path of cloned repo, not a \"lisp\" sub directory."
  (let ((drefd (file-truename (find-library-name "pytest-pdb-break")))
        root)
    (if (fboundp 'ffip-project-root)
        (setq root (let ((default-directory drefd))
                     (file-truename (ffip-project-root))))
      (setq root (file-name-directory drefd)
            root (and root (directory-file-name root))
            root (and root (file-name-directory root))))
    (if (and root (file-exists-p (concat root "pytest_pdb_break.py")))
        (setq pytest-pdb-break--home root)
      (error "Cannot find pytest-pdb-break's home directory"))))

(defun pytest-pdb-break-get-pytest-executable ()
  "Return pytest executable"
  (if pytest-pdb-break-pytest-executable
      pytest-pdb-break-pytest-executable
    (python-shell-with-environment
     (let ((pytest-exe (executable-find "pytest")))
       (if pytest-exe
           pytest-exe
         (error "pytest not found\nexec-path: %s\nprocess-environment: %s\n"
                exec-path process-environment))))))

(defvar pytest-pdb-break--tempdir nil
  "Temporary directory for this session.
Should be an absolute path ending in a slash.")

(defvar pytest-pdb-break--isolated-lib nil
  "Temporary directory containing plugin and metadata.
Absolute path to a subdir of `pytest-pdb-break--tempdir'.
Should end in a slash.")

(defun pytest-pdb-break--on-kill-emacs ()
  "Remove session tempdir `pytest-pdb-break--tempdir'."
  ;; This needs to be redone if adding tramp support
  (let ((pat (format "^%s[^/]+" (regexp-quote temporary-file-directory))))
    (when (and pytest-pdb-break--tempdir
               (string-match-p pat pytest-pdb-break--tempdir))
      (delete-directory pytest-pdb-break--tempdir 'recursive))))

(defun pytest-pdb-break--create-tempdir ()
  "Return path to temporary directory."
  (when pytest-pdb-break--tempdir
    (error "pytest-pdb-break--tempdir already set to: %s"
           pytest-pdb-break--tempdir))
  (let ((prefix (format "emacs-%s-pytest-pdb-break-" (user-uid))))
    (setq pytest-pdb-break--tempdir (file-name-as-directory
                                     (make-temp-file prefix t)))
    ;; `server-ensure-safe-dir' takes some extra pains, but requiring it may
    ;; not be desirable. Hopefully setting user perms is enough.
    (chmod pytest-pdb-break--tempdir #o0700)
    (add-hook 'kill-emacs-hook #'pytest-pdb-break--on-kill-emacs))
  pytest-pdb-break--tempdir)

(defun pytest-pdb-break-get-isolated-lib (&optional interpreter)
  "Return path to an isolated plugin installation.
Use INTERPRETER or `python-shell-interpreter' to run the helper script."
  (if pytest-pdb-break--isolated-lib
      pytest-pdb-break--isolated-lib
    (let* ((home (or pytest-pdb-break--home (pytest-pdb-break--homer)))
           (tmpdir (or pytest-pdb-break--tempdir
                       (pytest-pdb-break--create-tempdir)))
           (name (file-name-as-directory (concat tmpdir "self")))
           (script (concat home "helpers/main.py")))
      (with-temp-buffer
        (unless (zerop (call-process (or interpreter
                                         python-shell-interpreter)
                                     nil
                                     (current-buffer) nil
                                     script "install_plugin" name))
          (error "Error calling %s\nscript: %s\nname: %s\noutput: ...\n%s"
                 python-shell-interpreter script name (buffer-string))))
      (setq pytest-pdb-break--isolated-lib name))))

(defun pytest-pdb-break--extract-shebang (pytest-exe)
  "Extract PYTEST-EXE's shebanged interpreter."
  (let (maybe)
    (with-temp-buffer
      (insert-file-contents-literally pytest-exe)
      (goto-char (point-min))
      (if (and (looking-at "#!\\([^\r\n]+\\)")
               (setq maybe (match-string-no-properties 1))
               (file-executable-p maybe))
          maybe
        (error "Cannot find python exe\npytest-exe: %s\nrejected: %s\n"
               pytest-exe (buffer-substring (point) (point-at-eol)))))))

(defvar pytest-pdb-break--exe-alist nil)

(defun pytest-pdb-break-get-python-interpreter (pytest-exe &optional force)
  "Return the python interpreter used by pytest-exe.
With FORCE, update."
  (let* ((entry (assoc pytest-exe pytest-pdb-break--exe-alist))
         (value (cdr entry)))
    (if (and (not force) value)
        value
      (condition-case err
          (setq value (pytest-pdb-break--extract-shebang pytest-exe))
        (error
         (when entry
           (setq pytest-pdb-break--exe-alist
                 (delete entry pytest-pdb-break--exe-alist)))
         (signal (car err) (cdr err))))
      (unless entry
        (push (setq entry (list pytest-exe)) pytest-pdb-break--exe-alist))
      (setcdr entry value))))

(defun pytest-pdb-break--get-node-id ()
  "Return list of node-id components for test at point."
  (let (file test parts)
    (if (fboundp 'elpy-test-at-point)
        (let ((four (elpy-test-at-point)))
          (setq file (nth 1 four)
                test (nth 3 four)))
      (setq file buffer-file-name
            test (python-info-current-defun)))
    (unless (and test (string-match-p "\\<[Tt]est" test))
      (error "No test found"))
    (setq parts (split-string test "\\."))
    (when (caddr parts)
      (setq parts (list (pop parts) (pop parts))))
    (cons file parts)))

(defun pytest-pdb-break--get-args (line-no node-id-parts)
  "Generate args for subprocess.
LINE-NO and NODE-ID-PARTS are as required by the main command."
  (let ((nodeid (mapconcat #'identity node-id-parts "::"))
        (break (format "--break=%s:%s" (car node-id-parts) line-no)))
    (append pytest-pdb-break-extra-opts (list break nodeid))))

;; FIXME only add this wrapper when pdb++ is detected. These amputee
;; candidates most likely come courtesy of the fancycompleter package.
(defun pytest-pdb-break-ad-around-get-completions (orig process import input)
  "Advice wrapper for ORIG `python-shell-completion-get-completions'.
If PROCESS is ours, prepend INPUT to results. With IMPORT, ignore."
  (let ((rv (funcall orig process import input)))
    (if (or import
            (not (memq process pytest-pdb-break-processes))
            (null rv)
            (string= input "")
            (not (memq ?. (append input nil)))) ; not dotty
        rv
      (when (not (cdr rv)) ; |rv| = 1
        (if (string-match-p "\\.__$" (car rv))
            (setq rv (funcall orig process import (car rv)))
          (setq input "")))
      (when (string-match "^\\(.+\\.\\)[^.]+$" input)
        (setq input (match-string 1 input)))
      (mapcar (apply-partially #'concat input) rv))))

(defvar-local pytest-pdb-break--process nil)
(defvar-local pytest-pdb-break--parent-buffer nil)

(define-minor-mode pytest-pdb-break-mode
  "A minor mode for Python comint buffers running a pytest PDB session."
  :group 'pytest-pdb-break
  (let ((proc (or pytest-pdb-break--process
                  (get-buffer-process (current-buffer)))))
    (if pytest-pdb-break-mode
        (progn
          (unless (process-live-p proc)
            (error "No live process associated with %S" (current-buffer)))
          (cl-pushnew proc pytest-pdb-break-processes)
          (advice-add 'python-shell-completion-get-completions :around
                      #'pytest-pdb-break-ad-around-get-completions)
          (setq-local python-shell-completion-native-enable nil)
          (add-hook 'kill-buffer-hook
                    (lambda nil (pytest-pdb-break-mode -1))
                    nil t))
      ;; Forget proc even if it's still running
      (setq pytest-pdb-break-processes
            (seq-filter #'process-live-p ; proc may be nil
                        (remq proc pytest-pdb-break-processes)))
      (unless pytest-pdb-break-processes
        (advice-remove 'python-shell-completion-get-completions
                       'pytest-pdb-break-ad-around-get-completions)))))

;;;###autoload
(defun pytest-pdb-break-here (line-no node-id-parts)
  "Run pytest on the test at point and break at LINE-NO.
NODE-ID-PARTS is a list of pytest node-id components."
  (interactive (list (line-number-at-pos) (pytest-pdb-break--get-node-id)))
  (let* ((process-environment (append process-environment nil))
         (python-shell-buffer-name "pytest-PDB")
         (proc-name (python-shell-get-process-name 'dedicated))
         (proc-buffer-name (format "*%s*" proc-name)))
    (defvar python-shell--interpreter)
    (defvar python-shell--interpreter-args)
    ;; Maybe pop to existing proc's buffer instead?
    (when (comint-check-proc proc-buffer-name)
      (error "%s is already running" proc-buffer-name))
    (let* ((pytest-exe (pytest-pdb-break-get-pytest-executable))
           (pyexe (pytest-pdb-break-get-python-interpreter pytest-exe))
           (python-shell-extra-pythonpaths
            (append (list (pytest-pdb-break-get-isolated-lib pyexe))
                    python-shell-extra-pythonpaths))
           ;; Make pdb++ prompt trigger non-native-completion fallback
           (python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[+>)]+ ")
           (args (pytest-pdb-break--get-args line-no node-id-parts))
           (fake-arg-string (mapconcat #'identity (cons "-mpytest" args) " "))
           ;; Produces warning in 25.x: Making foo local to bar while let-bound
           (python-shell--parent-buffer (current-buffer))
           (python-shell--interpreter-args fake-arg-string)
           (python-shell--interpreter pyexe)
           buffer)
      (save-excursion
        ;; Allow "calculate-" funcs to consider "python-shell-" options and
        ;; modify process-environment and exec-path accordingly
        (python-shell-with-environment
         (setq buffer (apply #'make-comint-in-buffer proc-name proc-buffer-name
                             pytest-exe nil args))
         ;; Only python- prefixed local vars get cloned in child buffer
         (with-current-buffer buffer
           (inferior-python-mode)
           (setq pytest-pdb-break--process (get-buffer-process buffer)
                 pytest-pdb-break--parent-buffer python-shell--parent-buffer)
           (pytest-pdb-break-mode +1))
         (display-buffer buffer))))))


(provide 'pytest-pdb-break)

;;; pytest-pdb-break ends here
