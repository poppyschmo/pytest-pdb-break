;;; pytest-pdb-break-test.el ---- tests -*- lexical-binding: t -*-

(require 'ert)
(require 'pytest-pdb-break)

(defvar pytest-pdb-break-test-home
  (file-name-as-directory (getenv "PYTEST_PDB_BREAK_TEST_HOME")))

(eval-when-compile

  (defvar pytest-pdb-break-test-temp
    (file-name-as-directory (concat (temporary-file-directory)
                                    "pytest-pdb-break-test")))

  (defun pytest-pdb-break-test--unprefix (name)
    "Return truncated test NAME (string)."
    (when (symbolp name) (setq name (symbol-name name)))
    (replace-regexp-in-string
     (regexp-quote "pytest-pdb-break-test-") "" name))

  (defun pytest-pdb-break-test--name-to-envvar (name)
    (setq name (pytest-pdb-break-test--unprefix name)
          name (concat "pytest-pdb-break-test-" name))
    (upcase (replace-regexp-in-string "-" "_" name)))

  ) ; eval-when-compile

(defmacro pytest-pdb-break-test-with-tmpdir (name &rest body)
  "Run BODY in temp directory NAME.
If name is not a string, use test name. Existing files are clobbered."
  (unless (stringp name)
    (push name body)
    (setq name '(pytest-pdb-break-test--unprefix
                 (ert-test-name (ert-running-test)))))
  `(let* (($name ,name)
          ($target (file-name-as-directory
                    (concat pytest-pdb-break-test-temp $name))))
     (when (file-exists-p $target) (delete-directory $target t))
     (make-directory $target t)
     (let ((default-directory $target)) ,@body)))

(defmacro pytest-pdb-break-test-homer-fixture ()
  '(pytest-pdb-break-test-with-tmpdir
    (should-not pytest-pdb-break--home)
    (should (string= (pytest-pdb-break--homer)
                     pytest-pdb-break-test-home))
    (should (directory-name-p pytest-pdb-break--home)) ; ends in /
    (should (string= pytest-pdb-break--home
                     pytest-pdb-break-test-home))))

;; TODO link ffip
(ert-deftest pytest-pdb-break-test-homer ()
  (ert-info ("Find cloned repo containing pytest plugin")
    (should-not (fboundp 'ffip-project-root))
    (pytest-pdb-break-test-homer-fixture)))

(defmacro pytest-pdb-break-test-homer-setup-fixture
    (subproc-form dir-body test-sym info-msg)
  "Run SUBPROC-FORM in another emacs.
TEST-SYM unquoted sym. DIR-BODY sets up build dir."
  (let ((env-var (pytest-pdb-break-test--name-to-envvar test-sym))
        (tdir-name (concat (pytest-pdb-break-test--unprefix test-sym)
                           "-setup")))
    `(if (getenv ,env-var)
         ,subproc-form
       (let* ((tdir (concat pytest-pdb-break-test-temp ,tdir-name "/"))
              (targ (file-truename "pytest-pdb-break.el"))
              (us (file-truename "pytest-pdb-break-test.el"))
              (dir (pytest-pdb-break-test-with-tmpdir
                    ,(concat tdir-name "/build")
                    ,dir-body
                    (byte-compile-file "./pytest-pdb-break.el")
                    default-directory))
              (file (pytest-pdb-break-test-with-tmpdir
                     ,(concat tdir-name "/script")
                     (copy-file us "./")
                     (file-truename "./pytest-pdb-break-test.el")))
              (logfile (concat tdir "test.out"))
              (script '(ert-run-tests-batch-and-exit ',test-sym))
              (args (list "-Q" "--batch" "-L" dir "-l" file
                          "--eval" (format "%S" script)))
              (process-environment (append process-environment nil))
              (default-directory tdir)
              ec)
         (ert-info (,info-msg)
           (setenv ,env-var "1")
           (setq ec (apply #'call-process "emacs" nil
                           (list :file logfile) nil args))
           (should (zerop ec)))))))

(ert-deftest pytest-pdb-break-test-homer-symlink ()
  (pytest-pdb-break-test-homer-setup-fixture
   ;; subproc-form
   (progn
     (should-not (fboundp 'ffip-project-root))
     (should (file-symlink-p (find-library-name "pytest-pdb-break")))
     (pytest-pdb-break-test-homer-fixture))
   ;; dir-body
   (make-symbolic-link targ "./")
   ;; test-sym
   pytest-pdb-break-test-homer-symlink
   ;; info-msg
   "Find home, resolving symlinks"))

(ert-deftest pytest-pdb-break-test-homer-missing ()
  (pytest-pdb-break-test-homer-setup-fixture
   ;; subproc-form
   (let ((rv (should-error (pytest-pdb-break-test-homer-fixture)))
         (case-fold-search t))
     (should (string-match-p "cannot find.*home" (cadr rv)))
     (should-not pytest-pdb-break--home))
   ;; dir-body
   (copy-file targ "./")
   ;; test-sym
   pytest-pdb-break-test-homer-missing
   ;; info-msg
   "No cloned repo (pytest plugin) found"))

;;; pytest-pdb-break-test ends here
