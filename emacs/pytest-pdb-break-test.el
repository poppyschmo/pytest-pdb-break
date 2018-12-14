;;; pytest-pdb-break-test.el ---- tests -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This leaves around 30M of junk under /tmp/pytest-pdb-break-test/

(require 'ert)
(require 'pytest-pdb-break)

(defvar pytest-pdb-break-test-repo-root
  (file-name-as-directory
   (file-truename (getenv "PYTEST_PDB_BREAK_TEST_REPO_ROOT"))))

(defvar pytest-pdb-break-test-lisp-root
  (concat pytest-pdb-break-test-repo-root "emacs/"))

(defvar pytest-pdb-break-test-pytest-plugin
  (concat pytest-pdb-break-test-repo-root "pytest_pdb_break.py"))

(defvar pytest-pdb-break-test-lisp-main
  (concat pytest-pdb-break-test-lisp-root "pytest-pdb-break.el"))

(defvar pytest-pdb-break-test-lisp-this
  (concat pytest-pdb-break-test-lisp-root "pytest-pdb-break-test.el"))

(ert-deftest pytest-pdb-break-test-ert-setup ()
  (should (file-exists-p pytest-pdb-break-test-repo-root))
  (should (file-exists-p pytest-pdb-break-test-lisp-root))
  (should (file-exists-p pytest-pdb-break-test-pytest-plugin))
  (should (file-exists-p pytest-pdb-break-test-lisp-main))
  (should (file-exists-p pytest-pdb-break-test-lisp-this)))

(eval-when-compile ; BEG

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

  ) ; e-w-c--------- END

(defmacro pytest-pdb-break-test-with-environment (&rest body)
  ;; Covers PATH, PYTHONPATH, VIRTUAL_ENV
  `(let (($orig (sxhash-equal (list process-environment exec-path))))
     (let ((process-environment (append process-environment nil))
           (exec-path (append exec-path nil)))
       ,@body)
     (should (= $orig (sxhash-equal (list process-environment exec-path))))))

(defmacro pytest-pdb-break-test-with-tmpdir (tail &rest body)
  "Run BODY in a temp directory, clobbering existing files.
The directory inherits the test's name, minus the feature prefix, with
an optional TAIL appended. To create a subdir, TAIL should start with a
dir sep."
  (let ((name '(pytest-pdb-break-test--unprefix
                (ert-test-name (ert-running-test)))))
    (if (stringp tail)
        (setq name `(concat ,name ,tail))
      (push tail body))
    `(let (($tmpdir (file-name-as-directory
                     (concat pytest-pdb-break-test-temp ,name))))
       (when (file-exists-p $tmpdir) (delete-directory $tmpdir t))
       (make-directory $tmpdir t)
       (let ((default-directory $tmpdir)) ,@body))))

(defmacro pytest-pdb-break-test-homer-fixture ()
  '(pytest-pdb-break-test-with-tmpdir
    (should-not pytest-pdb-break--home)
    (should (string= (pytest-pdb-break--homer)
                     pytest-pdb-break-test-repo-root))
    (should (directory-name-p pytest-pdb-break--home)) ; ends in /
    (should (string= pytest-pdb-break--home
                     pytest-pdb-break-test-repo-root))))

;; TODO link ffip
(ert-deftest pytest-pdb-break-test-homer ()
  (ert-info ("Find cloned repo containing pytest plugin")
    (should-not (fboundp 'ffip-project-root))
    (pytest-pdb-break-test-homer-fixture)))

(defmacro pytest-pdb-break-test-homer-setup-fixture (subform dir-body info-msg)
  "Run SUBFORM as this test in an Emacs subprocess.
DIR-BODY sets up build dir. Spit INFO-MSG."
  `(let* ((test-sym (ert-test-name (ert-running-test)))
          (env-var (pytest-pdb-break-test--name-to-envvar test-sym)))
     (if (getenv env-var)
         ,subform
       (pytest-pdb-break-test-with-tmpdir
        "-setup"
        (let* ((dir (pytest-pdb-break-test-with-tmpdir
                     "-setup/build"
                     ,dir-body
                     (byte-compile-file "./pytest-pdb-break.el")
                     default-directory))
               (file (pytest-pdb-break-test-with-tmpdir
                      "-setup/script"
                      (copy-file pytest-pdb-break-test-lisp-this "./")
                      (file-truename "./pytest-pdb-break-test.el")))
               (logfile (file-truename "test.out"))
               (script `(ert-run-tests-batch-and-exit ',test-sym))
               (args (list "-Q" "--batch" "-L" dir "-l" file
                           "--eval" (format "%S" script)))
               (process-environment (append process-environment nil))
               ec)
          (ert-info (,info-msg)
            (setenv env-var "1")
            (setq ec (apply #'call-process "emacs" nil
                            (list :file logfile) nil args))
            (should (zerop ec))))))))

(ert-deftest pytest-pdb-break-test-homer-symlink ()
  (pytest-pdb-break-test-homer-setup-fixture
   ;; subform
   (progn
     (should-not (fboundp 'ffip-project-root))
     (should (file-symlink-p (find-library-name "pytest-pdb-break")))
     (pytest-pdb-break-test-homer-fixture))
   ;; dir-body
   (make-symbolic-link pytest-pdb-break-test-lisp-main "./")
   ;; info-msg
   "Find home, resolving symlinks"))

(ert-deftest pytest-pdb-break-test-homer-missing ()
  (pytest-pdb-break-test-homer-setup-fixture
   ;; subform
   (let ((exc (should-error (pytest-pdb-break-test-homer-fixture)))
         (case-fold-search t))
     (should (string-match-p "cannot find.*home" (cadr exc)))
     (should-not pytest-pdb-break--home))
   ;; dir-body
   (copy-file pytest-pdb-break-test-lisp-main "./")
   ;; info-msg
   "No cloned repo (pytest plugin) found"))

(defvar pytest-pdb-break-test--requirements
  `((bare)
    (base "pytest")
    (pdbpp "pytest" "pdbpp")
    (self ,pytest-pdb-break-test-repo-root)
    (self_pdbpp "pdbpp" ,pytest-pdb-break-test-repo-root)))

(defun pytest-pdb-break-test--get-venv-path (name)
  (cl-assert (symbolp name))
  (cl-assert (assq name pytest-pdb-break-test--requirements))
  (concat pytest-pdb-break-test-temp
          (format ".venv_%s" (symbol-name name)) "/"))

(defun pytest-pdb-break-test--get-requirements (name)
  (cl-assert (symbolp name))
  (cdr (assq name pytest-pdb-break-test--requirements)))

(defmacro pytest-pdb-break-test-ensure-venv (name &rest body)
  "Run BODY in a temp directory with a modified environment.
NAME is a venv from --get-requirements Does not modify `PATH' or
`VIRTUAL_ENV' or `python-shell-interpreter'. Binds `$pyexe', `$venvbin',
and `$venv'. Doesn't use pip3 or python3 because venvs are all
created with python3."
  `(pytest-pdb-break-test-with-tmpdir
    (pytest-pdb-break-test-with-environment
     (let* (($venv (pytest-pdb-break-test--get-venv-path ,name))
            ($venvbin (concat $venv "bin/"))
            ($pyexe (concat $venvbin "python"))
            pipexe logfile requirements)
       (unless (file-exists-p $venv) ; trailing/ ok
         (setq pipexe (concat $venvbin "pip")
               logfile (concat default-directory "pip.out")
               requirements (pytest-pdb-break-test--get-requirements ,name))
         (should (file-name-absolute-p logfile))
         (should (zerop (call-process "python3" nil (list :file logfile) nil
                                      "-mvenv" $venv)))
         (should (file-executable-p pipexe))
         (should (file-executable-p $pyexe))
         (should-not (equal (executable-find "pip") pipexe))
         (should-not (equal (executable-find "python") $pyexe))
         (unless (eq ,name 'bare)
           (should (zerop (apply #'call-process pipexe nil
                                 (list :file logfile) nil
                                 "install" requirements)))))
       ,@body))))

(defmacro pytest-pdb-break-test--query-wrap (&rest body)
  `(let (($rootdir (directory-file-name default-directory))
         ($rv 'sentinel))
     (cl-flet (($callit () (setq $rv (pytest-pdb-break--query-config))))
       ,@body)
     (should (json-plist-p $rv))
     (should (= 4 (length $rv)))
     (should (string= $rootdir (plist-get $rv :rootdir)))))

(ert-deftest pytest-pdb-break-test-query-helper-unregistered ()
  (pytest-pdb-break-test-ensure-venv
   'base
   (with-temp-buffer
     (insert "[pytest]\n")
     (write-file "setup.cfg"))
   (let ((python-shell-interpreter $pyexe))
     (ert-info ("Explicit py-shell-int, unreg, curdir")
       (pytest-pdb-break-test--query-wrap
        ($callit)
        (should-not (plist-get $rv :registered))))
     (ert-info ("Explicit py-shell-int, unreg, subdir")
       (pytest-pdb-break-test--query-wrap
        (pytest-pdb-break-test-with-tmpdir
         "/tests"
         (should-not (string= $rootdir default-directory))
         (with-temp-buffer
           (insert "def test_foo():" "\n\t" "assert True")
           (write-file "test_subdir.py"))
         ($callit))
        (should-not (plist-get $rv :registered)))))
   (ert-info ("PATH, exec-path, unreg, curdir, no explicit py-shell-int")
     (pytest-pdb-break-test--query-wrap
      (pytest-pdb-break-test-with-environment
       (setenv "PATH" (format "%s:%s" $venvbin (getenv "PATH")))
       (setq exec-path (cons $venvbin exec-path))
       (should (string= (executable-find python-shell-interpreter) $pyexe))
       ($callit)
       (should-not (plist-get $rv :registered)))))))

(ert-deftest pytest-pdb-break-test-query-helper-registered ()
  (pytest-pdb-break-test-ensure-venv
   'self
   (let ((python-shell-interpreter $pyexe))
     (ert-info ("Explicit py-shell-int, curdir")
       (pytest-pdb-break-test--query-wrap
        ($callit)
        (should (plist-get $rv :registered)))))))

(ert-deftest pytest-pdb-break-test-query-helper-error ()
  (pytest-pdb-break-test-ensure-venv
   'bare
   (let ((python-shell-interpreter $pyexe))
     ;; The error is raised by --query-config
     (ert-info ("No packages")
       (let ((exc (should-error (pytest-pdb-break--query-config))))
         (should (string-match-p "Error calling" (cadr exc)))
         (should (string-match-p "No module named '_pytest'" (cadr exc)))
         (with-temp-buffer
           (insert (cadr exc))
           (write-file "error.out")))))))

(provide 'pytest-pdb-break-test)
;;; pytest-pdb-break-test ends here
