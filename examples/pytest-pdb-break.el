;;; pytest-pdb-break.el ---- integration demo -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26"))

;;; Commentary:

;; Would be nice to just use the provided runner, but can't figure out how to
;; convert the running proc buffer to a Python inferior cleanly.
;;
;; Without this pytest plugin, you could do something like:
;;
;;   (sit-for 2)
;;   (python-shell-send-string (format "b %s:%d" file lnum))
;;   (python-shell-send-string "c")
;;
;; But this often fails.


;;; Code:
(require 'python)
(require 'elpy nil t)

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
failure.")

(defvar pytest-pdb-break--dry-run nil)

(defun pytest-pdb--get-node-id ()
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

(defun pytest-pdb-break-here (lnum node-info root-dir)
  "Drop into pdb after spawning an inferior pytest process, go to LNUM.
NODE-INFO is a list of pytest node-id components. ROOT-DIR is the
project/repo's root directory."
  (interactive (list (line-number-at-pos)
                     (pytest-pdb--get-node-id)
                     (pytest-pdb--get-default-dir)))
  (let* ((default-directory root-dir)
         (file (car node-info))
         (argstr (mapconcat #'identity node-info "::"))
         (break (format "--break=%s:%s" file lnum))
         (args (append (cons "-mpytest" pytest-pdb-break-extra-args)
                       (list break argstr)))
         (python-shell-interpreter (or pytest-pdb-break-interpreter
                                       python-shell-interpreter))
         (python-shell-interpreter-args (apply #'mapconcat
                                               (list #'identity args " ")))
         ;; python-shell-prompt-detect-failure-warning ; suppress startup spam
         (cmd (python-shell-calculate-command))
         (proc (and (not (bound-and-true-p pytest-pdb-break--dry-run))
                    (run-python cmd nil t))))
    (unless proc
      (message "Would've run: %S\nfrom: %S" cmd default-directory))
    (run-hook-with-args 'pytest-pdb-break-after-functions proc)))


(provide 'pytest-pdb-break)

;; Local Variables:
;; flycheck-disabled-checkers: nil
;; End:

;;; pytest-pdb-break ends here
