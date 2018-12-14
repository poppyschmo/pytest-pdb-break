;;; pytest-pdb-break.el ---- integration demo -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; Installation: no MELPA, but `straight.el' users can use this recipe:
;;
;;   '(:host github :repo "poppyschmo/pytest-pdb-break"
;;     :files (:defaults "emacs/*.el" (:exclude "emacs/*-test.el")))
;;
;; Usage: `pytest-pdb-break-here'
;;
;; Note: the completion modifications are useless without pdb++. No idea if
;; they hold up when summoned by `company-capf'.
;;
;; TODO: ert

;;; Code:

(require 'find-func)
(require 'json)
(require 'subr-x)
(require 'python)

(defvar-local pytest-pdb-break-extra-args nil
  "List of extra args passed to pytest.
May be useful in a `dir-locals-file'. For example, this `python-mode'
entry unsets cmd-line options from a project ini:
\(pytest-pdb-break-extra-args \"-o\" \"addopts=\").")

(defvar-local pytest-pdb-break-alt-interpreter nil
  "Path to an alternate Python executable.
If set, overrides `python-shell-interpreter' when obtaining config info
and running main command.")

(defvar-local pytest-pdb-break-config-info-alist nil
  "An alist with members ((INTERPRETER . PLIST) ...).
Set by the main command the first time it's invoked in a buffer. PLIST
is obtained by querying the external config-info helper. Entries are
retrieved and modified with `python-shell-with-environment' active,
meaning `python-shell-exec-path', `python-shell-virtualenv-root' etc.,
all have an effect, unless `pytest-pdb-break-alt-interpreter' is set.")

(defvar pytest-pdb-break-after-functions nil
  "Abnormal hook for adding a process sentinel, etc.
Sole arg is the pytest buffer process PROC, which may be nil upon
failure. Hook is run with process buffer current.")

(defvar pytest-pdb-break--dry-run nil)

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

(defun pytest-pdb-break--query-config ()
  "Return a plist with items :registered BOOL and :rootdir STRING."
  (let* ((home (or pytest-pdb-break--home (pytest-pdb-break--homer)))
         (helper (concat home "get_config_info.py"))
         (json-object-type 'plist)
         json-false)
    (with-temp-buffer
      (if (zerop (call-process python-shell-interpreter helper
                               (current-buffer) nil))
          (progn (goto-char (point-min)) (json-read))
        (error "Error calling %s: %s" helper (buffer-string))))))

(defvar-local pytest-pdb-break--config-info nil
  "Active and/or most recent config-info-alist entry.")

(defun pytest-pdb-break-get-config-info (&optional force)
  "Maybe call config-info helper, respecting options and environment.
With FORCE, always update. Return entry in config-info alist."
  (python-shell-with-environment
    (let* ((python-shell-interpreter (or pytest-pdb-break-alt-interpreter
                                         python-shell-interpreter))
           (pyexe-path (executable-find python-shell-interpreter))
           (entry (assoc pyexe-path pytest-pdb-break-config-info-alist))
           result)
      (unless (and (not force) entry)
        (setq result (pytest-pdb-break--query-config))
        (unless entry
          (push (setq entry (list pyexe-path))
                pytest-pdb-break-config-info-alist))
        (setq pytest-pdb-break--config-info
              (setcdr entry (nconc (list :exe pyexe-path) result))))
      entry)))

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

(defun pytest-pdb-break--get-node-id ()
  "Return list of node-id components for test at point."
  (let (file test parts)
    (if (fboundp 'elpy-test-at-point)
        (let ((four (elpy-test-at-point)))
          (setq file (nth 1 four)
                test (nth 3 four)))
      (setq file buffer-file-name
            test (python-info-current-defun)))
    (unless (and test (string-match "[Tt]est" test))
      (error "No test found"))
    (setq parts (split-string test "\\."))
    (when (caddr parts)
      (setq parts (list (pop parts) (pop parts))))
    (cons file parts)))

;; TODO verify this is needed even though we're explicitly naming a node id
;; TODO use root finders from tox, projectile, ggtags, magit, etc.
(defun pytest-pdb--get-default-dir ()
  "Maybe return project root, otherwise `default-directory'."
  (or (and (bound-and-true-p elpy-shell-use-project-root)
           (fboundp 'elpy-project-root)
           (elpy-project-root))
      default-directory))

(defun pytest-pdb-break-buffer-teardown (proc)
  "Cleanup a pytest-pdb-break comint buffer.
PROC is the buffer's current process."
  (setq pytest-pdb-break-processes
        (seq-filter #'process-live-p
                    (remq proc pytest-pdb-break-processes)))
  (unless pytest-pdb-break-processes
    (advice-remove 'python-shell-completion-get-completions
                   'pytest-pdb-break-ad-around-get-completions)))

(defun pytest-pdb-break-buffer-setup (proc)
  "Setup a pytest-pdb-break comint buffer.
PROC is the buffer's current process."
  (add-to-list 'pytest-pdb-break-processes proc)
  (advice-add 'python-shell-completion-get-completions :around
              #'pytest-pdb-break-ad-around-get-completions)
  (with-current-buffer (process-buffer proc)
    (setq-local python-shell-completion-native-enable nil)
    (add-hook 'kill-buffer-hook (apply-partially
                                 #'pytest-pdb-break-buffer-teardown
                                 proc)
              nil t)
    (run-hook-with-args 'pytest-pdb-break-after-functions proc)))

(defun pytest-pdb-break--check-command-p (command)
  "Run COMMAND in Python, return t if exit code is 0, nil otherwise."
  (zerop (call-process python-shell-interpreter nil nil nil "-c" command)))

(defun pytest-pdb-break--getenv (var)
  "Look up VAR in `process-environment', return nil if unset or empty."
  (and (setq var (getenv var)) (not (string= var "")) var))

(defun pytest-pdb-break-has-plugin-p ()
  "Return non-nil if plugin is loadable."
  (cl-assert (member :registered pytest-pdb-break--config-info) t)
  (plist-get pytest-pdb-break--config-info :registered))

(defun pytest-pdb-break-add-pythonpath ()
  "Add plugin root to a copy of `process-environment'.
Return the latter."
  (let* ((process-environment (append process-environment nil))
         (existing (pytest-pdb-break--getenv "PYTHONPATH"))
         (existing (and existing (parse-colon-path existing)))
         (found (pytest-pdb-break--homer)))
    (when found
      (setenv "PYTHONPATH" (string-join (cons found existing) ":")))
    process-environment))

;;;###autoload
(defun pytest-pdb-break-here (lnum node-info root-dir)
  "Drop into pdb after spawning an inferior pytest process, go to LNUM.
NODE-INFO is a list of pytest node-id components. ROOT-DIR is the
project/repo's root directory."
  (interactive (list (line-number-at-pos)
                     (pytest-pdb-break--get-node-id)
                     (pytest-pdb--get-default-dir)))
  (let* ((default-directory root-dir)
         (file (car node-info))
         (argstr (mapconcat #'identity node-info "::"))
         (break (format "--break=%s:%s" file lnum))
         (installed (pytest-pdb-break-has-plugin-p))
         (xtra pytest-pdb-break-extra-args)
         (xtra (if (or installed (member "pytest_pdb_break" xtra))
                   xtra (append '("-p" "pytest_pdb_break") xtra)))
         (args (append (cons "-mpytest" xtra) (list break argstr)))
         (process-environment (if installed process-environment
                                (pytest-pdb-break-add-pythonpath)))
         ;; Make pdb++ prompt trigger non-native-completion fallback
         (python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[+>)]+ ")
         (python-shell-interpreter (or pytest-pdb-break-alt-interpreter
                                       python-shell-interpreter))
         (python-shell-interpreter-args (apply #'mapconcat
                                               (list #'identity args " ")))
         (cmd (python-shell-calculate-command))
         (proc (and (not pytest-pdb-break--dry-run) (run-python cmd nil t))))
    (if proc
        (pytest-pdb-break-buffer-setup proc)
      (message "Would've run: %S\nfrom: %S" cmd default-directory))))


(provide 'pytest-pdb-break)

;; Local Variables:
;; flycheck-disabled-checkers: nil
;; End:

;;; pytest-pdb-break ends here
