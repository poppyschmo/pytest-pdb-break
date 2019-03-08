;;; pytest-pdb-break-extra.el --- Convenience tools -*- lexical-binding: t -*-

;;; Commentary:

;; This file should contain:
;;
;; - Miscellaneous helpers that enable integration with other tools and
;;   libraries
;;
;; - Runners (only one, so far) that extend normal `python-mode' behavior
;;   but may not use the namesake pytest plugin
;;
;; `pytest-pdb-break-run-fail' is like `elpy-test-pytest-runner' except with
;; support for pytest's --pdb option.  For now, point must be located within
;; the confines of the surrounding test body.  In the future, some prefix arg
;; may spawn a `completing-read' buffer to offer a choice.
;;
;; Elpy users can try adding `pytest-pdb-break-advise-elpy-shell-get-proc' to
;; `pytest-pdb-break-mode-hook'.  It helps `elpy-shell-get-or-create-process'
;; recognize this package's processes as valid.
;;
;; Note: if there's an issue with the feature/prefix mismatch in this file, it
;; can be renamed to `pytest-pdb-break-x' or simply narrowed in scope to
;; `pytest-pdb-break-run-fail', since that's all it really provides, at the
;; moment.

;;; Code:

(require 'compile)
(require 'pytest-pdb-break)

(defcustom pytest-pdb-break-run-fail-options '("-x" "--pdb")
  "Default cmd-line options for `pytest-pdb-break-run-fail'."
  :group 'pytest-pdb-break
  :type 'list)

(defun pytest-pdb-break-go-inferior (proc)
  "Enable PDB interaction in PROC, a failed pytest session.
This converts major or minor (comint) compilation modes to
`inferior-python-mode'."
  (let* ((python-shell-prompt-pdb-regexp pytest-pdb-break-prompt-regexp)
         (parbuf (process-get proc 'pytest-pdb-break--parent-buffer))
         (proc-name (if parbuf
                        (with-current-buffer parbuf
                          (pytest-pdb-break--get-proc-name))
                      (pytest-pdb-break--get-proc-name)))
         (proc-buffer-name (format "*%s*" proc-name))
         reshow)
    (with-current-buffer (process-buffer proc)
      (pcase major-mode
        ('comint-mode
         (compilation--unsetup))
        ('compilation-mode
         (read-only-mode -1)
         (kill-all-local-variables)
         (comint-mode)
         (set-process-filter proc 'comint-output-filter)
         ;; Run the comint filter once to force prompt detection. Should
         ;; maybe look for other indicators in addition to the former major
         ;; mode when setting flag.
         (setq reshow t))
        ('inferior-python-mode
         (error "Setup called again on converted buffer"))
        (_ (error "Can't handle mode %s" major-mode))) ; impossible
      (setq-local comint-ptyp process-connection-type)
      (setq compilation-in-progress (delq proc compilation-in-progress))
      (when (buffer-live-p (get-buffer proc-buffer-name))
        (kill-buffer proc-buffer-name))
      (message "Switching %s to %s" (buffer-name) proc-buffer-name)
      (rename-buffer proc-buffer-name)
      (defvar python-shell--interpreter)
      (defvar python-shell--interpreter-args)
      (python-shell-with-environment
       (let ((python-shell--parent-buffer parbuf)
             python-shell--interpreter
             python-shell--interpreter-args)
         (inferior-python-mode)
         (setq pytest-pdb-break--process proc
               pytest-pdb-break--parent-buffer parbuf)
         (pytest-pdb-break-mode +1)
         (let ((win (get-buffer-window proc-buffer-name)))
           (and win (select-window win)))
         (if (not reshow)
             (comint-goto-process-mark)
           ;; Markers haven't been set yet, so just assume bol is prompt start
           (let ((was (buffer-substring (point-at-bol) (process-mark proc))))
             (delete-region (point-at-bol) (process-mark proc))
             (comint-output-filter proc was))
           (set-marker (process-mark proc) (goto-char (point-max)))))))))

(defvar pytest-pdb-break--prompt-watcher-function
  'pytest-pdb-break-go-inferior)

(defun pytest-pdb-break--run-fail-compilation-filter ()
  "Look for PDB prompt on pytest failure.
When using something like `elpy-test-pytest-runner', add this to
`compilation-filter-hook'."
  (when (save-excursion (beginning-of-line)
                        (looking-at-p pytest-pdb-break-prompt-regexp))
    (remove-hook 'compilation-filter-hook
                 'pytest-pdb-break--run-fail-compilation-filter t)
    (let ((proc (get-buffer-process (current-buffer))))
      (funcall pytest-pdb-break--prompt-watcher-function proc))))

(defun pytest-pdb-break--run-fail-comint-process-filter (proc input-string)
  "Defer to default output filter till PDB prompt is encountered.
PROC and INPUT-STRING are as required by `comint-output-filter'."
  (when (with-temp-buffer
          (insert input-string)
          (goto-char (point-max))
          (beginning-of-line)
          (looking-at-p pytest-pdb-break-prompt-regexp))
    (set-process-filter proc 'comint-output-filter)
    (funcall pytest-pdb-break--prompt-watcher-function proc))
  ;; Must run after modifications
  (comint-output-filter proc input-string))

;;;###autoload
(defun pytest-pdb-break-run-fail ()
  "Run pytest on test at point, dropping into PDB on first failure.
Otherwise, finish normally."
  (interactive)
  (let* ((process-environment (append process-environment nil))
         (nid (mapconcat #'identity (pytest-pdb-break--get-node-id) "::"))
         (args (mapcar #'shell-quote-argument
                       `("pytest" ,@pytest-pdb-break-extra-opts
                         ,@pytest-pdb-break-run-fail-options ,nid)))
         (cmd-line (mapconcat #'identity args " "))
         (parbuf (current-buffer))
         buf)
    (python-shell-with-environment
      ;; For `compilation-shell-minor-mode', `compilation-start' delegates to
      ;; `shell-file-name', which knows nothing of `exec-path', meaning
      ;; `python-shell-calculate-exec-path' has no effect.
      (setenv "PATH" (string-join exec-path path-separator))
      (setq buf (compile cmd-line t)))
    (with-current-buffer buf
      (setq pytest-pdb-break--process (get-buffer-process buf))
      (process-put pytest-pdb-break--process
                   'pytest-pdb-break--parent-buffer parbuf)
      ;; XXX tried using `comint-output-filter-functions' but markers were
      ;; clobbered unless manually stashed/restored (which probably means was
      ;; doing it wrong, but for now...).
      (set-process-filter pytest-pdb-break--process
                          'pytest-pdb-break--run-fail-comint-process-filter))))

(defun pytest-pdb-break--elpy-shell-get-or-create-process-advice
    (orig &rest rest)
  "Advice around `elpy-shell-get-or-create-process'.
Return the source buffer's child process or call ORIG with REST."
  (condition-case exc
      (progn (pytest-pdb-break--get-proc-name) (apply orig rest))
    (pytest-pdb-break-process-exists (get-process (cadr exc)))))

;;;###autoload
(defun pytest-pdb-break-advise-elpy-shell-get-proc ()
  "A minor-mode hookee to help Elpy's send-related commands.
This is experimental, but `elpy-shell-send-statement' and
`elpy-shell-send-defun' seem to work.  Unlike the stock send commands,
the Elpy versions echo the input to the REPL (with proper indentation
and continuation ellipses)."
  (let ((a 'pytest-pdb-break--elpy-shell-get-or-create-process-advice))
    (if pytest-pdb-break-mode
        ;; Not sure it's worth checking for membership beforehand
        (unless (advice-member-p a 'elpy-shell-get-or-create-process)
          (advice-add 'elpy-shell-get-or-create-process :around a))
      (unless pytest-pdb-break-processes
        (advice-remove 'elpy-shell-get-or-create-process a)))))

(provide 'pytest-pdb-break-extra)
;;; pytest-pdb-break-extra.el ends here
