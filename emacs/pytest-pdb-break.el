;;; pytest-pdb-break.el --- A pytest PDB launcher -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Jane Soko

;; Author: Jane Soko <poppyschmo@protonmail.com>
;; URL: https://github.com/poppyschmo/pytest-pdb-break
;; Version: 0.0.2
;; Keywords: languages, tools
;; Package-Requires: ((emacs "25"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Usage: with point in the body of some test, run M-x `pytest-pdb-break-here'
;;
;; This command can only handle the "console script"/entry-point invocation
;; style (as opposed to "python -m pytest"). If a pytest executable doesn't
;; appear in PATH, `pytest-pdb-break-pytest-executable' must be set.
;;
;; TODO:
;; - Fixture breaking
;; - Option-name completions (would require external helper)
;; - Tramp
;;

;;; Code:

(require 'find-func)
(require 'subr-x)
(require 'python)

(defgroup pytest-pdb-break nil
  "Emacs integration for the pytest plugin of the same name."
  :prefix "pytest-pdb-break-"
  :group 'languages)

(defcustom pytest-pdb-break-extra-opts nil
  "List of extra options passed to all pytest invocations.

For one-offs, see `pytest-pdb-break-options-function', which prompts for
session options. This variable is best used in a `dir-locals-file'. For
example, this `python-mode' entry unsets cmd-line options from a project
ini: (pytest-pdb-break-extra-opts \"-o\" \"addopts=\")."
  :group 'pytest-pdb-break
  :type 'list)

(defcustom pytest-pdb-break-pytest-executable nil
  "Path to an alternate pytest executable.

If set, `executable-find' won't be consulted. For certain actions, like
querying helpers, this script's shebanged Python may be used in place of
`python-shell-interpreter'."
  :group 'pytest-pdb-break
  :type 'string)

(defcustom pytest-pdb-break-options-function
  'pytest-pdb-break-default-options-function
  "Function determining any additional options to pass to pytest.

Handed a single arg, which is either nil or an integer like that
provided by (interactive \"P\"). Must return a list of strings or nil.
See `pytest-pdb-break-default-options-function'."
  :group 'pytest-pdb-break
  :type 'function)

(defcustom pytest-pdb-break-alt-installation nil
  "Path to an existing installation of the pytest plugin.

If set, this is used in lieu of creating a per-session, \"isolated\"
installation. Useful for \"editable\" setups (working dir with egg
info)."
  :group 'pytest-pdb-break
  :type 'string)

(defvar pytest-pdb-break-processes nil
  "List of processes started via `pytest-pdb-break-here'.")

(defvar pytest-pdb-break--py-home nil
  "Directory containing the uninstalled plugin and non-el scripts.")

(defun pytest-pdb-break--homer (&optional this-file)
  "Return the directory containing the plugin's setup.py script.

And store the result in `pytest-pdb-break--py-home' as an absolute path
with trailing sep. The likeliest locations are the root of the cloned
repo or a \"lib\" subtree of the installed package. THIS-FILE is used as
a starting point, if provided."
  (let* ((this (or this-file (find-library-name "pytest-pdb-break")))
         (parent/ (file-name-directory this))
         (lib/? (file-name-as-directory (expand-file-name "lib" parent/))))
    (cond
     ((file-exists-p (expand-file-name "pytest_pdb_break.py" lib/?))
      (setq pytest-pdb-break--py-home lib/?))
     ((and (string= "emacs" (file-name-base (directory-file-name parent/)))
           (setq lib/? (file-name-directory (directory-file-name parent/)))
           (file-exists-p (expand-file-name "pytest_pdb_break.py" lib/?)))
      (setq pytest-pdb-break--py-home lib/?))
     ((file-symlink-p this)  ; only dereference as a last resort
      (pytest-pdb-break--homer (file-truename this)))
     (t (error "Cannot find pytest-pdb-break's Python files")))))

(defun pytest-pdb-break-get-pytest-executable ()
  "Return the current pytest executable."
  (if pytest-pdb-break-pytest-executable
      pytest-pdb-break-pytest-executable
    (python-shell-with-environment
     (let ((pytest-exe (executable-find "pytest")))
       (if pytest-exe
           pytest-exe
         (error "Pytest executable not found"))))))

(defvar pytest-pdb-break--tempdir nil
  "Temporary directory for this session.

Should be an absolute path ending in a slash.")

(defvar pytest-pdb-break--isolated nil
  "Temporary directory containing plugin and metadata.

This should be an absolute path ending end in a slash and should point to a
proper \"library\" installation, without dependencies, under
`pytest-pdb-break--tempdir'.")

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
    ;; `server-ensure-safe-dir' takes some extra pains when setting up its
    ;; tempdir. We're running whatever's in pytest-pdb-break.py, so hopefully
    ;; setting user perms is enough, portability issues aside.
    (chmod pytest-pdb-break--tempdir #o0700)
    (add-hook 'kill-emacs-hook #'pytest-pdb-break--on-kill-emacs))
  pytest-pdb-break--tempdir)

(defun pytest-pdb-break-get-isolated (&optional interpreter)
  "Return path to an isolated plugin installation.

Use INTERPRETER or `python-shell-interpreter' to run the helper script."
  (if pytest-pdb-break--isolated
      pytest-pdb-break--isolated
    (let* ((home (or pytest-pdb-break--py-home (pytest-pdb-break--homer)))
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
          ;; The traceback dumped here isn't helpful because the script calls
          ;; another subprocess; try exporting PYTEST_PDB_BREAK_INSTALL_LOGFILE
          (error "Call to %s install_plugin returned non-zero" script)))
      (setq pytest-pdb-break--isolated name))))

(defun pytest-pdb-break--extract-shebang (pytest-exe)
  "Extract PYTEST-EXE's shebanged interpreter."
  (with-temp-buffer
    (insert-file-contents-literally pytest-exe)
    (goto-char (point-min))
    (let (maybe)
      (if (and (looking-at "#!\\(.+\\)$")
               (setq maybe (match-string-no-properties 1))
               (file-executable-p maybe))
          maybe
        (error "Cannot find interpreter for pytest exe %S" pytest-exe)))))

(defvar pytest-pdb-break--exe-alist nil)

(defun pytest-pdb-break-get-python-interpreter (pytest-exe &optional force)
  "Return PYTEST-EXE's interpreter. With FORCE, update existing."
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


(define-error 'pytest-pdb-break-test-not-found "Test not found" 'error)

(defun pytest-pdb-break--get-node-id ()
  "Return list of node-id components for test at point."
  (let* ((test (python-info-current-defun))
         case-fold-search
         (func (apply-partially #'string-match-p "^\\(Test\\|test_\\)"))
         (parts (and test (seq-take-while func (split-string test "\\.")))))
    (cond ((cdr parts)
           (setq parts (if (= (elt (car parts) 0) ?T)
                           (and (= (elt (cadr parts) 0) ?t)
                                (list (pop parts) (pop parts)))
                         (list (pop parts)))))
          ((and (car parts) (= (elt (car parts) 0) ?T))
           (setq parts nil)))
    (unless parts
      (signal 'pytest-pdb-break-test-not-found (list test)))
    (cons buffer-file-name parts)))

(defun pytest-pdb-break--get-args (session-opts breakpoint node-id-parts)
  "Generate arguments for the pytest subprocess.

SESSION-OPTS, BREAKPOINT, and NODE-ID-PARTS are as required by the main
command, `pytest-pdb-break-here' (which see)."
  (let ((nodeid (mapconcat #'identity node-id-parts "::"))
        (break (format "--break=%s:%s"
                       (or (car-safe breakpoint) (car node-id-parts))
                       (or (cdr-safe breakpoint) breakpoint))))
    (append pytest-pdb-break-extra-opts session-opts (list break nodeid))))

(defvar pytest-pdb-break--setup-code-addendum nil)
(defvar pytest-pdb-break--setup-code-reassignment "
__PYTHON_EL_get_completions = _wrap_pyel(__PYTHON_EL_get_completions)
del _wrap_pyel
")

(defun pytest-pdb-break--get-modified-setup-code ()
  "Return revised completion setup-code."
  (unless pytest-pdb-break--setup-code-addendum
    (let ((srcfile (concat (or pytest-pdb-break--py-home
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

(defconst pytest-pdb-break--proc-base-name "pytest-PDB"
  "Base portion of subprocess names. Don't change this.

String-sending functions that rely on `python-shell-get-process-name'
need `python-shell-buffer-name' set to this in source buffers during PDB
sessions. If a local binding already exists, it's stashed and restored
later. This may not be desirable in certain situations, but it's
hard-wired, for now." )

(defvar-local pytest-pdb-break--process nil)
(defvar-local pytest-pdb-break--parent-buffer nil)
(defvar-local pytest-pdb-break--existing-python-shell-buffer-name nil)

(defun pytest-pdb-break--get-proc-name ()
  "Generate a process name and ensure it's available."
  (let* ((python-shell-buffer-name pytest-pdb-break--proc-base-name)
         (proc-name (python-shell-get-process-name 'dedicated))
         (proc-buffer-name (format "*%s*" proc-name)))
    ;; Caller may opt to pop to existing proc's buffer
    (when (comint-check-proc proc-buffer-name)
      (signal 'pytest-pdb-break-process-exists (list proc-name)))
    proc-name))

(defun pytest-pdb-break--maybe-get-parent-buffer ()
  "Try to divine the source buffer from an inferior-shell buffer."
  (let* ((bufname (buffer-name))
         (pat (concat "\\(" (regexp-quote pytest-pdb-break--proc-base-name)
                      "\\|" (regexp-quote python-shell-buffer-name) "\\)"
                      "\\[\\(.+\\)\\]\\*$"))
         (m (and (string-match pat bufname) (match-string 2 bufname)))
         (buf (and m (get-buffer m))))
    (and buf (with-current-buffer buf (eq major-mode 'python-mode)) buf)))

(defun pytest-pdb-break--set-shell-buffer-name (parent-buffer)
  "Set `python-shell-buffer-name' in PARENT-BUFFER."
  (with-current-buffer parent-buffer
    (when (local-variable-p 'python-shell-buffer-name)
      (message "Moving existing python-shell-buffer-name %S to %s"
               python-shell-buffer-name
               'pytest-pdb-break--existing-python-shell-buffer-name)
      (setq pytest-pdb-break--existing-python-shell-buffer-name
            python-shell-buffer-name))
    (setq-local python-shell-buffer-name
                pytest-pdb-break--proc-base-name)))

(defun pytest-pdb-break--kill-shell-buffer-name (parent-buffer)
  "Remove or restore `python-shell-buffer-name' in PARENT-BUFFER."
  (with-current-buffer parent-buffer
    (when (and (local-variable-p 'python-shell-buffer-name)
               python-shell-buffer-name
               (string= python-shell-buffer-name
                        pytest-pdb-break--proc-base-name))
      (if pytest-pdb-break--existing-python-shell-buffer-name
          (progn
            (setq-local python-shell-buffer-name
                        pytest-pdb-break--existing-python-shell-buffer-name)
            (kill-local-variable
             'pytest-pdb-break--existing-python-shell-buffer-name))
        (kill-local-variable 'python-shell-buffer-name)))))

(define-minor-mode pytest-pdb-break-mode
  "A minor mode for Python comint buffers running a pytest PDB session."
  :group 'pytest-pdb-break
  (let ((proc (or pytest-pdb-break--process
                  (get-buffer-process (current-buffer))))
        (parbuf (or pytest-pdb-break--parent-buffer
                    (pytest-pdb-break--maybe-get-parent-buffer))))
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
                    nil t)
          (when parbuf (pytest-pdb-break--set-shell-buffer-name parbuf)))
      (when (eq major-mode 'inferior-python-mode)
        (when parbuf (pytest-pdb-break--kill-shell-buffer-name parbuf))
        (kill-local-variable 'pytest-pdb-break--process)
        (kill-local-variable 'pytest-pdb-break--parent-buffer)
        (kill-local-variable 'python-shell-completion-setup-code)
        (kill-local-variable 'python-shell-completion-native-enable))
      ;; Forget proc even if it's still running
      (setq pytest-pdb-break-processes
            (seq-filter #'process-live-p ; proc may be nil
                        (delq proc pytest-pdb-break-processes))))))

(defvar pytest-pdb-break-prompt-regexp  "[(<]*[Ii]?[Pp]db[+>)]+ "
  "The default `python-shell-prompt-pdb-regexp' with an extra +.")

(defvar pytest-pdb-break--options-history nil)

(defun pytest-pdb-break--maybe-tare-options-history ()
  "Add the empty string to the front of the options history.

Remove other instances and return the modified list."
  (setq pytest-pdb-break--options-history
        (cons "" (delete "" pytest-pdb-break--options-history))))

(defun pytest-pdb-break--read-session-options ()
  "Ask for additional options and return the resulting string.

Shell quoting won't work. Values containing spaces should be enclosed in
double quotes, e.g., prompt: -foO \"--data={\\\"bar\\\": 1}\" ./baz/"
  (let ((comint-file-name-chars
         (replace-regexp-in-string "[,:=]" "" comint-file-name-chars))
        minibuffer-allow-text-properties outstr)
    (minibuffer-with-setup-hook
        (lambda nil (add-hook 'completion-at-point-functions
                              'comint-completion-at-point nil t))
      (setq outstr (read-from-minibuffer
                    "options: " (car pytest-pdb-break--options-history)
                    minibuffer-local-shell-command-map
                    nil '(pytest-pdb-break--options-history . 1)
                    nil t))
      (when (string-empty-p outstr)
        (pytest-pdb-break--maybe-tare-options-history))
      outstr)))

(defun pytest-pdb-break-default-options-function (&optional n)
  "Return a previously used options list or ask for a new one.

Without N, return the most recent, which may be nil. When N is positive,
ask for new options. When N is negative, return that many entries before
the most recent. When N is 0, add \"\" to the front of the history and
return nil."
  (let ((raw (cond
              ((null n) (car pytest-pdb-break--options-history))
              ((< n 0) (or (nth (- n) pytest-pdb-break--options-history)
                           (car (last pytest-pdb-break--options-history))))
              ((> n 0) (pytest-pdb-break--read-session-options))
              (t (car (pytest-pdb-break--maybe-tare-options-history))))))
    (and raw (split-string-and-unquote raw))))

(defun pytest-pdb-break--interpret-prefix-arg (arg)
  "Convert prefix ARG to number if non-nil."
  (and arg (prefix-numeric-value arg)))

;;;###autoload
(defun pytest-pdb-break-here (session-opts breakpoint node-id-parts)
  "Run pytest on the test at point and break at BREAKPOINT.

BREAKPOINT may be a line number or cons of the form (FILENAME . LNUM).
NODE-ID-PARTS should be a list of pytest node-id components and
SESSION-OPTS a list of additional options. `prefix-arg' behavior is
determined by `pytest-pdb-break-options-function'."
  (interactive
   (list (funcall pytest-pdb-break-options-function
                  (pytest-pdb-break--interpret-prefix-arg
                   current-prefix-arg))
         nil nil))
  ;; As per (info "(elisp) Programming Tips"), the "repetition" thing
  (unless node-id-parts (setq node-id-parts (pytest-pdb-break--get-node-id)))
  (let ((process-environment (append process-environment nil))
        (proc-name (pytest-pdb-break--get-proc-name)))
    (defvar python-shell--interpreter)
    (defvar python-shell--interpreter-args)
    (let* ((pytest-exe (pytest-pdb-break-get-pytest-executable))
           (pyexe (pytest-pdb-break-get-python-interpreter pytest-exe))
           (python-shell-extra-pythonpaths
            (append (list (or pytest-pdb-break-alt-installation
                              (pytest-pdb-break-get-isolated pyexe)))
                    python-shell-extra-pythonpaths))
           ;; Make pdb++ prompt trigger non-native-completion fallback
           (python-shell-prompt-pdb-regexp pytest-pdb-break-prompt-regexp)
           (args (pytest-pdb-break--get-args session-opts
                                             (or breakpoint
                                                 (line-number-at-pos))
                                             node-id-parts))
           ;; Triggers local-while-let-bound warning in 25.x
           (python-shell--parent-buffer (current-buffer))
           ;; Ensure `python-shell-prompt-detect' doesn't use ipython, etc.
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

;;; pytest-pdb-break.el ends here
