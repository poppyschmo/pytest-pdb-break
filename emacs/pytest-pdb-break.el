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
;; TODO tramp
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
  "List of extra args passed to all pytest invocations.
For one-offs, it's easier to call this package's commands with one or
more universal args. This option is best used in a `dir-locals-file'.
For example, this `python-mode' entry unsets cmd-line options from a
project ini: (pytest-pdb-break-extra-opts \"-o\" \"addopts=\")."
  :group 'pytest
  :type 'list)

(defcustom pytest-pdb-break-pytest-executable nil
  "Path to an alternate pytest executable.
If set, `executable-find' won't be consulted. For certain actions, like
querying helpers, this script's shebanged Python may be used in place of
`python-shell-interpreter'."
  :group 'pytest
  :type 'string)

(defcustom pytest-pdb-break-options-function
  'pytest-pdb-break-default-options-function
  "Function determining any additional options to pass to pytest.
Handed a single arg, which, if non-nil, must be an integer like that
provided by (interactive \"P\"). Must return a list of strings or nil.
See `pytest-pdb-break-default-options-function'."
  :group 'pytest
  :type 'function)

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
  "Return the current pytest executable."
  (if pytest-pdb-break-pytest-executable
      pytest-pdb-break-pytest-executable
    (python-shell-with-environment
      (let ((pytest-exe (executable-find "pytest")))
        (if pytest-exe
            pytest-exe
          (error "Pytest not found\nexec-path: %s\nprocess-environment: %s\n"
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
    (error "Var pytest-pdb-break--tempdir already set to: %s"
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
  "Return the python interpreter used by PYTEST-EXE.
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

(defun pytest-pdb-break--get-args (session-opts line-no node-id-parts)
  "Generate args for subprocess.
SESSION-OPTS, LINE-NO, and NODE-ID-PARTS are as required by the main
command, `pytest-pdb-break-here' (which see)."
  (let ((nodeid (mapconcat #'identity node-id-parts "::"))
        (break (format "--break=%s:%s" (car node-id-parts) line-no)))
    (append pytest-pdb-break-extra-opts session-opts (list break nodeid))))

(defvar pytest-pdb-break--setup-code-addendum nil)
(defvar pytest-pdb-break--setup-code-reassignment "
__PYTHON_EL_get_completions = _wrap_pyel(__PYTHON_EL_get_completions)
del _wrap_pyel
")

(defun pytest-pdb-break--get-modified-setup-code ()
  "Return revised completion setup-code."
  (unless pytest-pdb-break--setup-code-addendum
    (let ((srcfile (concat (or pytest-pdb-break--home
                               (pytest-pdb-break--homer))
                           "emacs/setup_code_wrapper.py")))
      (with-temp-buffer
        (let ((coding-system-for-read "utf-8"))
          (insert-file-contents-literally srcfile))
        (goto-char (point-max))
        (insert pytest-pdb-break--setup-code-reassignment)
        (setq pytest-pdb-break--setup-code-addendum
              (buffer-string)))))
  (concat python-shell-completion-setup-code
          pytest-pdb-break--setup-code-addendum))

(define-error 'pytest-pdb-break-process-exists
  "Live process already exists" 'error)

(defun pytest-pdb-break--get-proc-name ()
  "Generate a process name and ensure it's available."
  (let* ((python-shell-buffer-name "pytest-PDB")
         (proc-name (python-shell-get-process-name 'dedicated))
         (proc-buffer-name (format "*%s*" proc-name)))
    ;; Caller may opt to pop to existing proc's buffer
    (when (comint-check-proc proc-buffer-name)
      (signal 'pytest-pdb-break-process-exists (list proc-name)))
    proc-name))

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
          (setq-local python-shell-completion-native-enable nil)
          (setq-local python-shell-completion-setup-code
                      (pytest-pdb-break--get-modified-setup-code))
          (add-hook 'kill-buffer-hook
                    (lambda nil (pytest-pdb-break-mode -1))
                    nil t))
      ;; Forget proc even if it's still running
      (setq pytest-pdb-break-processes
            (seq-filter #'process-live-p ; proc may be nil
                        (remq proc pytest-pdb-break-processes))))))

(defvar pytest-pdb-break-prompt-regexp  "[(<]*[Ii]?[Pp]db[+>)]+ "
  "The default `python-shell-prompt-pdb-regexp' with an extra +.")

(defvar pytest-pdb-break--options-history nil)

(defun pytest-pdb-break--read-session-options ()
  "Ask for additional options and return the resulting string.
Shell quoting won't work. Values containing spaces should be enclosed in
double quotes, e.g., prompt: -foO \"--data={\\\"bar\\\": 1}\" ./baz/"
  (let ((comint-file-name-chars
         (replace-regexp-in-string "[,:=]" "" comint-file-name-chars))
        minibuffer-allow-text-properties)
    (minibuffer-with-setup-hook
        (lambda nil (add-hook 'completion-at-point-functions
                              'comint-completion-at-point nil t))
      (read-from-minibuffer
       "options: " nil minibuffer-local-shell-command-map
       nil 'pytest-pdb-break--options-history
       (car pytest-pdb-break--options-history) t))))

(defun pytest-pdb-break-default-options-function (&optional n)
  "Return a previously used options list or ask for a new one.
Without N, return the most recent, which may be nil. When N is positive,
ask for new options. When N is negative, return the N+1-th previous
list. When N is 0, insert \"\" into history and return nil."
  (let ((raw (cond
              ((null n) (car pytest-pdb-break--options-history))
              ((< n 0) (or (nth (- n) pytest-pdb-break--options-history)
                           (car (last pytest-pdb-break--options-history))))
              ((> n 0) (pytest-pdb-break--read-session-options))
              (t (car (cl-pushnew "" pytest-pdb-break--options-history))))))
    (and raw (split-string-and-unquote raw))))

(defun pytest-pdb-break--interpret-prefix-arg (arg)
  "Convert prefix ARG to number if non-nil."
  ;; FIXME use `prefix-numeric-value' instead
  ;; (pcase arg
  ;;   (`(,u) u)
  ;;   ((and u (pred numberp)) u)
  ;;   ('- -1))
  (and arg (prefix-numeric-value arg)))

;;;###autoload
(defun pytest-pdb-break-here (session-opts line-no node-id-parts)
  "Run pytest on the test at point and break at LINE-NO.
When called non-interactively, NODE-ID-PARTS should be a list of pytest
node-id components and SESSION-OPTS a list of additional options. See
`pytest-pdb-break-default-options-function' for `prefix-arg' behavior."
  (interactive
   (list (funcall pytest-pdb-break-options-function
                  (pytest-pdb-break--interpret-prefix-arg
                   current-prefix-arg))
         (line-number-at-pos)
         (pytest-pdb-break--get-node-id)))
  (let* ((process-environment (append process-environment nil))
         (proc-name (pytest-pdb-break--get-proc-name)))
    (defvar python-shell--interpreter)
    (defvar python-shell--interpreter-args)
    (let* ((pytest-exe (pytest-pdb-break-get-pytest-executable))
           (pyexe (pytest-pdb-break-get-python-interpreter pytest-exe))
           (python-shell-extra-pythonpaths
            (append (list (pytest-pdb-break-get-isolated-lib pyexe))
                    python-shell-extra-pythonpaths))
           ;; Make pdb++ prompt trigger non-native-completion fallback
           (python-shell-prompt-pdb-regexp pytest-pdb-break-prompt-regexp)
           (args (pytest-pdb-break--get-args session-opts
                                             line-no node-id-parts))
           ;; Triggers local-while-let-bound warning in 25.x
           (python-shell--parent-buffer (current-buffer))
           ;; Ensure ``python-shell-prompt-detect'' doesn't use ipython, etc.
           (python-shell--interpreter pyexe)
           python-shell--interpreter-args
           buffer)
      (save-excursion
        ;; Allow "calculate-" funcs to consider "python-shell-" options and
        ;; modify process-environment and exec-path accordingly
        (python-shell-with-environment
          (setq buffer (apply #'make-comint-in-buffer proc-name nil
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
