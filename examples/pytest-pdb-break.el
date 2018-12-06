;;; pytest-pdb-break.el ---- integration demo -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; This obviously isn't on MELPA, but `straight.el' users can use this recipe:
;;
;;   '(:host github :repo "poppyschmo/pytest-pdb-break"
;;     :files (:defaults "examples/*.el"))
;;
;; Note: the completion modifications are useless without pdb++. No idea if
;; they hold up when summoned by `company-capf'.
;;
;; TODO:
;; * Add option to hack PYTHONPATH for environments where the plugin itself
;;   isn't installed (only useful to users of straight-like package managers)
;;

;;; Code:

(require 'python)
;; (require 'elpy nil t)

(defvar-local pytest-pdb-break-extra-args nil
  "List of extra args passed to pytest.
May be useful in a `dir-locals-file'. For example, this `python-mode'
entry unsets cmd-line options from a project ini:
\(pytest-pdb-break-extra-args \"-o\" \"addopts=\").")

(defvar-local pytest-pdb-break-interpreter nil
  "If nil, use `python-shell-interpreter'.")

(defvar pytest-pdb-break-after-functions nil
  "Abnormal hook for adding a process sentinel, etc.
Sole arg is the pytest buffer process PROC, which may be nil upon
failure. Hook is run with process buffer current.")

(defvar pytest-pdb-break--dry-run nil)

(defvar pytest-pdb-break-processes nil
  "List of processes started via `pytest-pdb-break-here'.")

;; XXX pretty sure this is just a workaround; not familiar enough with
;; python.el to know either way
;; TODO ask Emacs list if there's not already some built-in means of
;; handling amputee candidates
(defun pytest-pdb-break-ad-around-get-completions (orig process import input)
  "Advice wrapper for ORIG `python-shell-completion-get-completions'.
If PROCESS belongs to us, prepend INPUT to results. With IMPORT, ignore."
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
         (args (append (cons "-mpytest" pytest-pdb-break-extra-args)
                       (list break argstr)))
         ;; Make pdb++ prompt trigger non-native-completion fallback
         (python-shell-prompt-pdb-regexp "[(<]*[Ii]?[Pp]db[+>)]+ ")
         (python-shell-interpreter (or pytest-pdb-break-interpreter
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
