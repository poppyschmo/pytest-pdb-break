;;; pytest-pdb-break.el --- pytest-pdb-break runner -*- lexical-binding: t -*-

;; Author: Jane Soko <poppyschmo@protonmail.com>
;; URL: https://github.com/poppyschmo/pytest-pdb-break
;; Version: 0.0.1
;; Keywords: python testing
;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; Installation: no MELPA, but `straight.el' users can use this recipe:
;;
;;   '(:host github :repo "poppyschmo/pytest-pdb-break"
;;     :files (:defaults "emacs/*.el" (:exclude "emacs/*-test.el")))
;;
;; Usage: with point in some test, run M-x `pytest-pdb-break-here'
;;
;; Note: the completion modifications are useless without pdb++. No idea if
;; they hold up when summoned by `company-capf'.
;;
;; TODO finish tests
;; TODO detect presence of pdb++
;; TODO make completion wrapper work in "interactive" command repl
;; TODO tramp
;; TODO add option to inject pdb++

;;; Code:

(require 'find-func)
(require 'json)
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

(defcustom pytest-pdb-break-alt-interpreter nil
  "Path to an alternate Python executable.
If set, overrides `python-shell-interpreter' when obtaining config info
and running main command."
  :group 'pytest
  :type 'list)

(defvar pytest-pdb-break-config-info-alist nil
  "An alist with members ((INTERPRETER . PLIST) ...).
Set by the main command the first time it's invoked in a buffer. PLIST
is obtained by querying the external config-info helper. Entries are
retrieved and modified with `python-shell-with-environment' active,
meaning `python-shell-exec-path', `python-shell-virtualenv-root' etc.,
all have an effect, unless `pytest-pdb-break-alt-interpreter' is set.")

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

(defun pytest-pdb-break--query-config (&rest args)
  "Return a plist with items :registered BOOL and :rootdir STRING.
Include any ARGS when calling external helper."
  (let* ((home (or pytest-pdb-break--home (pytest-pdb-break--homer)))
         (helper (concat home "get_config_info.py"))
         (json-object-type 'plist)
         json-false)
    (with-temp-buffer
      (if (zerop (apply #'call-process
                        (append (list python-shell-interpreter helper
                                      (current-buffer) nil)
                                (and args (cons "-" args)))))
          (progn (goto-char (point-min)) (json-read))
        (error "Error calling %s: %s" helper (buffer-string))))))

(defvar-local pytest-pdb-break--config-info nil
  "Value of active config-info-alist entry.")

(defun pytest-pdb-break-get-config-info (&optional node-id-parts force)
  "Maybe call config-info helper, respecting options and environment.
Include `pytest-pdb-break-extra-opts', if any, along with NODE-ID-PARTS.
With FORCE, always update. Return entry in config-info alist."
  (python-shell-with-environment
    (let* ((python-shell-interpreter (or pytest-pdb-break-alt-interpreter
                                         python-shell-interpreter))
           (pyexe-path (executable-find python-shell-interpreter))
           (entry (assoc pyexe-path pytest-pdb-break-config-info-alist))
           result)
      (condition-case err
          (if (and (not force) entry)
              (if (cdr entry)
                  (setq pytest-pdb-break--config-info (cdr entry))
                (pytest-pdb-break-get-config-info node-id-parts 'force))
            (let ((args (append pytest-pdb-break-extra-opts
                                (and node-id-parts
                                     (list (mapconcat #'identity
                                                      node-id-parts "::"))))))
              (setq result (apply #'pytest-pdb-break--query-config args)))
            (unless entry
              (push (setq entry (list pyexe-path))
                    pytest-pdb-break-config-info-alist))
            (setq pytest-pdb-break--config-info
                  (setcdr entry (nconc (list :exe pyexe-path) result))))
        (error
         (when entry
           (setq pytest-pdb-break-config-info-alist
                 (delete entry pytest-pdb-break-config-info-alist)))
         (signal (car err) (cdr err))))
      entry)))

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

(defun pytest-pdb--get-rootdir ()
  "Return value of :rootdir from the active config-info plist.
Ensure it has a trailing slash."
  (cl-assert (member :rootdir pytest-pdb-break--config-info) t)
  (file-name-directory (plist-get pytest-pdb-break--config-info :rootdir)))

(defun pytest-pdb-break--check-command-p (command)
  "Run COMMAND in Python, return t if exit code is 0, nil otherwise."
  (zerop (call-process python-shell-interpreter nil nil nil "-c" command)))

(defun pytest-pdb-break--has-plugin-p ()
  "Return non-nil if plugin is loadable."
  ;; Allows bypassing get-info when running the command non-interactively,
  ;; although config-info would still need populating by other means
  (cl-assert (member :registered pytest-pdb-break--config-info) t)
  (plist-get pytest-pdb-break--config-info :registered))

(defun pytest-pdb-break--make-arg-string (line-no node-id-parts installed)
  "Prepare arg string for `python-shell-make-comint'.
LINE-NO and NODE-ID-PARTS are as required by the main command. INSTALLED
should be non-nil if pytest sees the plugin."
  (let* ((nodeid (mapconcat #'identity node-id-parts "::"))
         (break (format "--break=%s:%s" (car node-id-parts) line-no))
         (xtra (append (unless (or installed
                                   (member "pytest_pdb_break"
                                           pytest-pdb-break-extra-opts))
                         '("-p" "pytest_pdb_break"))
                       pytest-pdb-break-extra-opts))
         (args (append (cons "-mpytest" xtra) (list break nodeid))))
    (combine-and-quote-strings args)))

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
  :after-hook (when (buffer-live-p pytest-pdb-break--parent-buffer)
                (with-current-buffer pytest-pdb-break--parent-buffer
                  (setq pytest-pdb-break--config-info nil)))
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
(defun pytest-pdb-break-here (line-no node-id-parts root-dir)
  "Run pytest on the test at point and break at LINE-NO.
NODE-ID-PARTS is a list of pytest node-id components. ROOT-DIR is
normally a project/repo root directory containing a pytest config."
  (interactive (let ((node-id-parts (pytest-pdb-break--get-node-id)))
                 (pytest-pdb-break-get-config-info node-id-parts)
                 (list (line-number-at-pos)
                       node-id-parts
                       (pytest-pdb--get-rootdir))))
  (let* ((default-directory root-dir)
         (process-environment (append process-environment nil))
         (installed (pytest-pdb-break--has-plugin-p))
         (python-shell-extra-pythonpaths
          (append (unless installed (list pytest-pdb-break--home))
                  python-shell-extra-pythonpaths))
         ;; Make pdb++ prompt trigger non-native-completion fallback
         (python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[+>)]+ ")
         (python-shell-interpreter (or pytest-pdb-break-alt-interpreter
                                       python-shell-interpreter))
         (python-shell-interpreter-args (pytest-pdb-break--make-arg-string
                                         line-no node-id-parts installed))
         (python-shell-buffer-name "pytest-PDB")
         (proc (run-python nil 'dedicated 'show))
         (parbuf (current-buffer)))
    ;; Only python- prefixed local vars get cloned in child buffer
    (with-current-buffer (process-buffer proc)
      (setq pytest-pdb-break--process proc
            pytest-pdb-break--parent-buffer parbuf)
      (pytest-pdb-break-mode +1))))


(provide 'pytest-pdb-break)

;;; pytest-pdb-break ends here
