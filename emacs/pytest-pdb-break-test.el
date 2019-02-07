;;; pytest-pdb-break-test.el ---- tests -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This leaves around 30M of junk under /tmp/pytest-pdb-break-test/
;;
;; If revisiting the `query-helper'/`config-info'-centered approach, see
;; 2824cc74d1e05bdbd2aed580f4a5844c4cae0495 or earlier. These used -mpytest,
;; rootdir as cwd, etc.

;;; Code:

(require 'ert)
(require 'pytest-pdb-break)

(defvar pytest-pdb-break-test-tests
  '(pytest-pdb-break-test-ert-setup
    pytest-pdb-break-test-upstream-env-updaters
    pytest-pdb-break-test-homer
    pytest-pdb-break-test-homer-symlink
    pytest-pdb-break-test-homer-missing
    pytest-pdb-break-test-on-kill-emacs
    pytest-pdb-break-test-create-tempdir
    pytest-pdb-break-test-get-isolated-lib
    pytest-pdb-break-test-get-pytest-executable
    pytest-pdb-break-test-get-python-interpreter
    pytest-pdb-break-test-get-node-id
    pytest-pdb-break-test-get-args
    pytest-pdb-break-test-get-modified-setup-code
    pytest-pdb-break-test-get-proc-name
    pytest-pdb-break-test-minor-mode
    pytest-pdb-break-test-main-command-basic
    pytest-pdb-break-test-main-command-completion))

(defvar pytest-pdb-break-test-repo-root
  (file-name-as-directory
   (file-truename (getenv "PYTEST_PDB_BREAK_TEST_REPO_ROOT"))))

(defvar pytest-pdb-break-test-tempdir
  (file-name-as-directory
   (file-truename (getenv "PYTEST_PDB_BREAK_TEST_TEMPDIR"))))

(defvar pytest-pdb-break-test-temp
  (concat pytest-pdb-break-test-tempdir "emacs/"))

(defvar pytest-pdb-break-test-lisp-root
  (concat pytest-pdb-break-test-repo-root "emacs/"))

(defvar pytest-pdb-break-test-pytest-plugin
  (concat pytest-pdb-break-test-repo-root "pytest_pdb_break.py"))

(defvar pytest-pdb-break-test-lisp-main
  (concat pytest-pdb-break-test-lisp-root "pytest-pdb-break.el"))

(defvar pytest-pdb-break-test-lisp-this
  (concat pytest-pdb-break-test-lisp-root "pytest-pdb-break-test.el"))

(ert-deftest pytest-pdb-break-test-ert-setup ()
  ;; Eval: (compile "make PAT=ert-setup")
  (should (seq-set-equal-p (mapcar 'ert-test-name (ert-select-tests t t))
                           pytest-pdb-break-test-tests))
  (should (file-exists-p pytest-pdb-break-test-repo-root))
  (should (file-exists-p pytest-pdb-break-test-tempdir))
  (should (file-exists-p pytest-pdb-break-test-lisp-root))
  (should (file-exists-p pytest-pdb-break-test-pytest-plugin))
  (should (file-exists-p pytest-pdb-break-test-lisp-main))
  (should (file-exists-p pytest-pdb-break-test-lisp-this))
  (should-not (getenv "VIRTUAL_ENV"))
  (should-not (getenv "PYTHONPATH")))

(eval-when-compile ; BEG

  (unless (fboundp 'seq-set-equal-p) ; 25
    (defun seq-set-equal-p (p q) (not (cl-set-exclusive-or p q))))

  (unless (fboundp 'file-attribute-user-id) ; 25
    (defun file-attribute-user-id (a) (nth 2 a)))

  (unless (fboundp 'file-attribute-modes) ; 25
    (defun file-attribute-modes (a) (nth 8 a)))

  (unless (fboundp 'caddr) ; 25
    (defun caddr (x) (nth 2 x)))

  (unless (fboundp 'sxhash-equal) ; 25
    (defalias 'sxhash-equal 'sxhash))

  (defun pytest-pdb-break-test--unprefix (name)
    "Return truncated test NAME (string)."
    (when (symbolp name) (setq name (symbol-name name)))
    (replace-regexp-in-string
     (regexp-quote "pytest-pdb-break-test-") "" name))

  (defun pytest-pdb-break-test--name-to-envvar (name)
    (setq name (pytest-pdb-break-test--unprefix name)
          name (concat "pytest-pdb-break-test-" name))
    (upcase (replace-regexp-in-string "-" "_" name)))

  (defun pytest-pdb-break-test-invoked-with-debug-p ()
    ;; (let ((earg (car (last command-line-args))))
    ;;   (string-match-p "ert-run-tests-interactively" earg))
    (not noninteractive))

  ) ; e-w-c--------- END

(defmacro pytest-pdb-break-test-with-environment (&rest body)
  "Run BODY in a temporary environment.
This is for modifying PATH, PYTHONPATH, VIRTUAL_ENV, etc."
  (let ((orig (make-symbol "orig"))
        (rv (make-symbol "rv")))
    `(let ((,orig (sxhash-equal (list process-environment exec-path)))
           (,rv (let ((process-environment (append process-environment nil))
                      (exec-path (append exec-path nil)))
                  ,@body)))
       (should (= ,orig (sxhash-equal (list process-environment
                                            exec-path))))
       ,rv)))

(defmacro pytest-pdb-break-test-with-tmpdir (tail &rest body)
  "Run BODY in a temp directory, clobbering existing files.
The directory inherits the test's name, minus the feature prefix, with
an optional TAIL appended. If TAIL doesn't start with a dir sep (slash),
the dir name itself is altered (suffixed). To create a subdir, TAIL
should start with a dir sep."
  (let ((name '(pytest-pdb-break-test--unprefix
                (ert-test-name (ert-running-test))))
        (tmpdir (make-symbol "tmpdir")))
    (if (stringp tail)
        (setq name `(concat ,name ,tail))
      (push tail body))
    `(let ((,tmpdir (file-name-as-directory
                     (concat pytest-pdb-break-test-temp ,name))))
       (when (file-exists-p ,tmpdir) (delete-directory ,tmpdir t))
       (make-directory ,tmpdir t)
       (let ((default-directory ,tmpdir)) ,@body))))

(defmacro pytest-pdb-break-test-with-conditional-env-var (present absent)
  "Run PRESENT if test's name has been exported as an env var.
Otherwise, run ABSENT. Vars `$test-sym' and `$env-var' are bound to the
current test func and its env-var-ized string. High risk of infinite
looping."
  `(let* (($test-sym (ert-test-name (ert-running-test)))
          ($env-var (pytest-pdb-break-test--name-to-envvar $test-sym)))
     (if (getenv $env-var)
         ,present
       ,@(macroexp-unprogn absent))))

(defmacro pytest-pdb-break-test-with-python-buffer (&rest body)
  "Run BODY in a `python-mode' temp buffer with a temp environment.
Note: this does *not* create and cd to a temp dir."
  `(pytest-pdb-break-test-with-environment
    (with-temp-buffer
      (let (python-indent-guess-indent-offset)
        (python-mode))
      ,@body)))

(ert-deftest pytest-pdb-break-test-upstream-env-updaters ()
  "Describe expected behavior of built-in `python-mode' interface.
Show that it doesn't restore environment to previous state. Certain
options are idiosyncratic and unintuitive.

Note: for local files, `python-shell-with-environment' calls both
`process-environment' and `exec-path' \"calculate\" funcs, in that
order. Re `python-shell-virtualenv-root': prepended exec path entry and
generated VIRTUAL_ENV var never end in a /, even when orig does.
"
  ;; Eval: (compile "make PAT=upstream-env-updaters")
  (cl-macrolet ((before (&rest rest) `(ert-info ("Before") ,@rest))
                (during (&rest rest) `(ert-info ("During") ,@rest))
                (after (&rest rest) `(ert-info ("After") ,@rest))
                (both (m b x y z)
                      `(ert-info (,m)
                         (pytest-pdb-break-test-with-python-buffer
                          ,x (let (,b) ,y) ,z)
                         (pytest-pdb-break-test-with-python-buffer
                          ,x (python-shell-with-environment ,y) ,z))))
    (ert-info ((concat "Changes made via "
                       "`python-shell-calculate-process-environment' "
                       "persist for existing environment variables"))
      ;; First two don't use well-knowns (baseline)
      (both "Already present"
            (process-environment (python-shell-calculate-process-environment))
            (before (should-not (file-remote-p default-directory))
                    (setenv "FOOVAR" "1"))
            (during (setenv "FOOVAR" "2")
                    (should (string= (getenv "FOOVAR") "2")))
            (after (should (string= (getenv "FOOVAR") "2"))))
      (both "Non-existent"
            (process-environment (python-shell-calculate-process-environment))
            (before (should-not (getenv "FOOVAR")))
            (during (setenv "FOOVAR" "1")
                    (should (string= (getenv "FOOVAR") "1")))
            (after (should-not (getenv "FOOVAR"))))
      ;; Options offered by `python-mode'
      (let ((python-shell-virtualenv-root "/tmp/pytest-pdb-break-test/foo"))
        (both
         "Setting `python-shell-virtualenv-root' sets VIRTUAL_ENV env var"
         (process-environment (python-shell-calculate-process-environment))
         (before (should-not (getenv "VIRTUAL_ENV")))
         (during (should (getenv "VIRTUAL_ENV")))
         (after (should-not (getenv "VIRTUAL_ENV")))))
      (let* ((newpp (concat pytest-pdb-break-test-repo-root "bar"))
             (oldpp (concat pytest-pdb-break-test-repo-root "foo"))
             (fullpp (format "%s:%s" newpp oldpp))
             (python-shell-extra-pythonpaths (list newpp)))
        (both
         "Setting `python-shell-extra-pythonpaths' sets PYTHONPATH"
         (process-environment (python-shell-calculate-process-environment))
         (before (should-not (getenv "PYTHONPATH")))
         (during (should (equal (getenv "PYTHONPATH") newpp)))
         (after (should-not (getenv "PYTHONPATH"))))
        (both
         "Mods to preexisting PYTHONPATH via `extra-pythonpaths' persist"
         (process-environment (python-shell-calculate-process-environment))
         (before (should (equal python-shell-extra-pythonpaths (list newpp)))
                 (should-not (getenv "PYTHONPATH"))
                 (setenv "PYTHONPATH" oldpp))
         (during (should (equal (getenv "PYTHONPATH") fullpp)))
         (after (should (equal (getenv "PYTHONPATH") fullpp)))))
      (let ((python-shell-process-environment '("VIRTUAL_ENV=/tmp/foo"
                                                "PATH=/tmp/foo:/the/rest")))
        (both
         "Only mutates PATH because it's present already in env"
         (process-environment (python-shell-calculate-process-environment))
         (before (should-not (getenv "VIRTUAL_ENV"))
                 (should (getenv "PATH"))) ; obvious but affirs claim
         (during (should (string= (getenv "PATH") "/tmp/foo:/the/rest"))
                 (should (string= (getenv "VIRTUAL_ENV") "/tmp/foo")))
         (after (should-not (getenv "VIRTUAL_ENV"))
                (should (string= (getenv "PATH") "/tmp/foo:/the/rest"))))))
    (ert-info ("`py-sh-calc-exec-path' doesn't mutate `exec-path' (safe)")
      (let ((orig (sxhash-equal exec-path))
            (new "/tmp/pytest-pdb-break-test/foo"))
        (both "Arbitrary changes to `exec-path' only present during interim"
              (exec-path (python-shell-calculate-exec-path))
              (before (should-not (string= (caddr exec-path) new)))
              (during (setcar (cddr exec-path) new) ; don't use setf in test
                      (should (string= (caddr exec-path) new)))
              (after (should (= (sxhash-equal exec-path) orig)))))
      (let ((python-shell-virtualenv-root "/tmp/pytest-pdb-break-test/foo")
            (binned  "/tmp/pytest-pdb-break-test/foo/bin"))
        (both "Sets VIRTUAL_ENV env var from `python-shell-virtualenv-root'"
              (exec-path (python-shell-calculate-exec-path))
              (should-not (member binned exec-path))
              (during (should (string= binned (car exec-path))))
              (should-not (member binned exec-path))))
      (let ((python-shell-process-environment '("VIRTUAL_ENV=/tmp/foo"
                                                "PATH=/tmp/foo:/the/rest"))
            (orig (sxhash-equal exec-path)))
        (should-not (member "/tmp/foo" exec-path))
        (should-not (member "/tmp/foo/" exec-path))
        (both "Env vars not added to `exec-path'"
              (exec-path (python-shell-calculate-exec-path))
              (before (should (= (sxhash-equal exec-path) orig)))
              (during (should (= (sxhash-equal exec-path) orig)))
              (after (should (= (sxhash-equal exec-path) orig))))))))

(defmacro pytest-pdb-break-test-homer-fixture ()
  "Common assertions for the home-finder."
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

(cl-defun pytest-pdb-break-test-run-ert-in-subprocess
    (test-file selector &key load-path-dir env-vars logfile)
  "Run SELECTOR in subproc and return exit code.
If selector is a symbol, it must be quoted. Emacs option -L is set to
LOAD-PATH-DIR or \".\", and option -l is passed TEST-FILE. Test output
is printed to LOGFILE. Export ENV-VARS in alist of consed key-val
strings ((NAME . VALUE) ...)."
  (setq logfile (file-truename (or logfile
                                   (format "%s-ert.out"
                                           (file-name-base test-file)))))
  (let* ((script `(ert-run-tests-batch-and-exit ,selector))
         (args (list "-Q" "--batch" "-L" (or load-path-dir ".") "-l" test-file
                     "--eval" (format "%S" script))))
    (let ((process-environment (append process-environment nil)))
      (dolist (pair env-vars)
        (when (stringp (cdr pair))
          (setenv (car pair) (cdr pair))))
      (apply #'call-process "emacs" nil `(:file ,logfile) nil args))))

(defmacro pytest-pdb-break-test-homer-setup-fixture (subform dir-body info-msg)
  "Run SUBFORM as the calling test in an Emacs ERT subprocess.
DIR-BODY sets up build dir. INFO-MSG is passed to `ert-info'."
  (setq info-msg (or info-msg (format "ERT subproc in %s" default-directory)))
  `(pytest-pdb-break-test-with-conditional-env-var
    ,subform
    (pytest-pdb-break-test-with-tmpdir
     "-setup"
     (let ((file (pytest-pdb-break-test-with-tmpdir
                  "-setup/script"
                  (copy-file pytest-pdb-break-test-lisp-this "./")
                  (file-truename "./pytest-pdb-break-test.el")))
           (dir (pytest-pdb-break-test-with-tmpdir
                 "-setup/build"
                 ,dir-body
                 (byte-compile-file "./pytest-pdb-break.el")
                 default-directory)))
       (ert-info (,info-msg)
         (should (zerop (pytest-pdb-break-test-run-ert-in-subprocess
                         file
                         (list 'quote $test-sym)
                         :load-path-dir dir
                         :env-vars (list (cons $env-var "1"))
                         :logfile "test.out"))))))))

(ert-deftest pytest-pdb-break-test-homer-symlink ()
  ;; Eval: (compile "make PAT=homer-symlink")
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
  ;; Eval: (compile "make PAT=homer-missing")
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

(ert-deftest pytest-pdb-break-test-on-kill-emacs ()
  ;; Eval: (compile "make PAT=on-kill-emacs")
  (pytest-pdb-break-test-with-conditional-env-var
   (ert-info ("In subprocess")
     (should (file-exists-p (file-truename "fake-isolib")))
     (setq pytest-pdb-break--tempdir (file-truename "fake-isolib"))
     (add-hook 'kill-emacs-hook #'pytest-pdb-break--on-kill-emacs))
   (ert-info ("In setup")
     (pytest-pdb-break-test-with-tmpdir
      (should (string-match-p "on-kill-emacs/$" default-directory))
      (make-directory "fake-isolib/fake-egg-info" t)
      (with-temp-file "fake-isolib/fake.py" (insert "# foo"))
      (should (file-exists-p (file-truename "fake-isolib")))
      (should (zerop (pytest-pdb-break-test-run-ert-in-subprocess
                      pytest-pdb-break-test-lisp-this
                      (list 'quote $test-sym)
                      :load-path-dir pytest-pdb-break-test-lisp-root
                      :env-vars (list (cons $env-var "1"))
                      :logfile "test.out")))
      (should-not (file-exists-p (file-truename "fake-isolib")))))))

(ert-deftest pytest-pdb-break-test-create-tempdir ()
  ;; Eval: (compile "make PAT=create-tempdir")
  (pytest-pdb-break-test-with-conditional-env-var
   (ert-info ("In subprocess")
     (let ((rv (pytest-pdb-break--create-tempdir))
           attrs)
       (should (file-exists-p rv))
       (should (equal rv pytest-pdb-break--tempdir))
       (setq attrs (file-attributes rv))
       (should (eq (file-attribute-user-id attrs) (user-uid)))
       (should (string= (file-attribute-modes attrs) "drwx------"))
       (with-temp-file "tempdir-location.out"
         (insert pytest-pdb-break--tempdir))))
   (ert-info ("In setup")
     (pytest-pdb-break-test-with-tmpdir
      (should (zerop (pytest-pdb-break-test-run-ert-in-subprocess
                      pytest-pdb-break-test-lisp-this
                      (list 'quote $test-sym)
                      :load-path-dir pytest-pdb-break-test-lisp-root
                      :env-vars (list (cons $env-var "1"))
                      :logfile "test.out")))
      (let ((dirwas (with-temp-buffer (insert-file-contents-literally
                                       "tempdir-location.out")
                                      (buffer-string))))
        (should (directory-name-p dirwas))
        (should-not (file-exists-p dirwas)))))))

(defvar pytest-pdb-break-test--exes `((base) (bare) (self)))

(defun pytest-pdb-break-test--get-pyexe (name)
  "Ask helper for canonical path to python exe in venv NAME.
Returns true name to existing, versioned python exe with predefined
requirements installed."
  (cl-assert (symbolp name))
  (cl-assert (assq name pytest-pdb-break-test--exes))
  (let ((prog (concat pytest-pdb-break-test-repo-root "helpers/main.py"))
        (found (cdr (assq name pytest-pdb-break-test--exes)))
        (pat (format "^%s\\.venvs/[[:digit:].]+/%s/bin/python[[:digit:].]+$"
                     (regexp-quote pytest-pdb-break-test-tempdir)
                     (symbol-name name)))
        pipexe binpath basename)
    (unless found
      (cl-assert (file-exists-p prog))
      (with-temp-buffer
        (setenv "PYTEST_PDB_BREAK_INSTALL_LOGFILE" "pip.log")
        (if (zerop (call-process "python3" nil (current-buffer) nil prog
                                 "get_pyexe" (symbol-name name)))
            (setq found (buffer-string))
          (error "Error calling %s: %s" prog (buffer-string))))
      (cl-assert (string-match-p pat found) t)
      (cl-assert (file-executable-p found) t)
      (setq binpath (file-name-directory found)
            pipexe (concat binpath "pip")
            basename (file-relative-name found binpath))
      (cl-assert (file-executable-p pipexe))
      ;; Should maybe also check if not an empty dummy file
      (cl-assert (not (equal (executable-find "pip") pipexe)))
      (cl-assert (not (equal (executable-find basename) found)))
      (setcdr (assq name pytest-pdb-break-test--exes) found))
    found))

(defmacro pytest-pdb-break-test-ensure-venv (name &rest body)
  "Run BODY in a temp directory and temp environment.
NAME is a venv from --get-requirements Does not modify `PATH' or
`VIRTUAL_ENV' or `python-shell-interpreter'. Binds `$pyexe', `$venvbin',
and `$venv'. The latter two have trailing slashes. Doesn't use pip3 or
python3 because venvs are all created with python3 (not sure if this is
a sound choice)."
  `(pytest-pdb-break-test-with-tmpdir
    (pytest-pdb-break-test-with-environment
     (let* (($pyexe (pytest-pdb-break-test--get-pyexe ,name))
            ($venvbin (file-name-directory $pyexe))
            ($venv (file-name-directory (directory-file-name $venvbin))))
       (should (file-exists-p $venv))
       (should (file-exists-p $venvbin))
       (should (directory-name-p $venv))
       (should (directory-name-p $venvbin))
       ,@body))))

(ert-deftest pytest-pdb-break-test-get-isolated-lib ()
  ;; Eval: (compile "make PAT=get-isolated-lib")
  (pytest-pdb-break-test-ensure-venv
   'bare
   (make-directory "fake-tmpdir")
   (setenv "PYTEST_PDB_BREAK_INSTALL_LOGFILE" "helper.log")
   (let* ((pytest-pdb-break--home pytest-pdb-break-test-repo-root)
          (pytest-pdb-break--tempdir (file-name-as-directory
                                      (file-truename "fake-tmpdir")))
          pytest-pdb-break--isolated-lib
          (rv (pytest-pdb-break-get-isolated-lib $pyexe)))
     (should (file-exists-p rv))
     (should (directory-name-p rv))
     (should (string= rv pytest-pdb-break--isolated-lib))
     (should (file-exists-p (concat rv "/pytest_pdb_break.py")))
     (should (string-match-p (regexp-quote pytest-pdb-break--tempdir) rv))
     (should-not (member #'pytest-pdb-break--on-kill-emacs kill-emacs-hook))
     (should-not (null (directory-files rv nil "\\.*-info"))))
   (should-not pytest-pdb-break--isolated-lib)))

(ert-deftest pytest-pdb-break-test-get-pytest-executable ()
  ;; Eval: (compile "make PAT=get-pytest-executable")
  (pytest-pdb-break-test-ensure-venv
   'base
   (ert-info ("Real exe found")
     (let ((python-shell-virtualenv-root $venv)
           pytest-pdb-break-pytest-executable)
       (should (string= (concat $venvbin "pytest")
                        (pytest-pdb-break-get-pytest-executable)))))
   (ert-info ("Custom option, if set returned unconditionally")
     (let ((pytest-pdb-break-pytest-executable "/tmp/foo"))
       (should (string= "/tmp/foo"
                        (pytest-pdb-break-get-pytest-executable)))))))

(ert-deftest pytest-pdb-break-test-get-python-interpreter ()
  ;; Eval: (compile "make PAT=get-python-interpreter")
  (pytest-pdb-break-test-ensure-venv
   'base
   (let ((pytest-exe (concat default-directory "fake-pytest"))
         (python-exe (concat default-directory "fake-python"))
         pytest-pdb-break--exe-alist
         exc)
     (with-temp-file pytest-exe
       (insert "#!" python-exe "\n"))
     (ert-info ("Shebanged file not found")
       (setq exc (should-error (pytest-pdb-break-get-python-interpreter
                                pytest-exe)))
       (should-not pytest-pdb-break--exe-alist)
       (should (string-match-p "Cannot find.*" (cadr exc))))
     (push (list pytest-exe) pytest-pdb-break--exe-alist)
     (with-temp-file python-exe (insert "#!/bin/true\n"))
     (ert-info ("Not executable, bad entry cleared from alist")
       (setq exc (should-error (pytest-pdb-break-get-python-interpreter
                                pytest-exe)))
       (should-not pytest-pdb-break--exe-alist)
       (should (string-match-p "Cannot find.*" (cadr exc))))
     (chmod python-exe #o0700)
     (ert-info ("Mocked path returned when target is executable")
       (should (string= python-exe (pytest-pdb-break-get-python-interpreter
                                    pytest-exe)))
       (should (equal (alist-get pytest-exe pytest-pdb-break--exe-alist)
                      python-exe)))
     (setcdr (assoc pytest-exe pytest-pdb-break--exe-alist) "/tmp/foo")
     (ert-info ("Existing alist value returned unconditionally")
       (should (string= "/tmp/foo"
                        (pytest-pdb-break-get-python-interpreter
                         pytest-exe))))
     (ert-info ("Existing alist value replaced with force option")
       (should (string= python-exe
                        (pytest-pdb-break-get-python-interpreter
                         pytest-exe 'force)))
       (should (equal pytest-pdb-break--exe-alist
                      (list (cons pytest-exe python-exe))))))
   (ert-info ("Real match returned successfully")
     (let* ((python-shell-virtualenv-root $venv)
            (pytest-exe (python-shell-with-environment
                         (executable-find "pytest")))
            rv)
       (should (file-executable-p pytest-exe))
       (setq rv (pytest-pdb-break-get-python-interpreter pytest-exe))
       (ert-info ("Matches verified")
         (should (string= rv $pyexe)))))))

(defvar pytest-pdb-break-test--get-node-id-sources
  '("
def test_foo():
    somevar = 1
    assert True

def may_be_a_test():
    pass

def something_else():
    assert 1
"
    "
class TestFoo:
    def test_foo(self):
        assert True

    def test_bar(self):
        somevar = True
        # comment
        assert somevar
")
  "The first line (1) is a single newline char.")

(ert-deftest pytest-pdb-break-test-get-node-id ()
  ;; Eval: (compile "make PAT=get-node-id")
  ;; For now, assume Elpy branch is infallible, and just test ours
  (let ((source-one (nth 0 pytest-pdb-break-test--get-node-id-sources))
        (source-two (nth 1 pytest-pdb-break-test--get-node-id-sources))
        case-fold-search)
    (pytest-pdb-break-test-with-tmpdir
     (pytest-pdb-break-test-with-python-buffer
      (should-not (fboundp 'elpy-test-at-point))
      (insert source-one)
      (write-file "s1.py") ; doesn't filter based on file name
      (ert-info ("Simple Python func, no docstring, comments")
        (should (equal buffer-file-name (expand-file-name "s1.py")))
        (should (and (goto-char (point-min))
                     (search-forward "assert True")
                     (goto-char (match-beginning 0))))
        (should (equal (python-info-current-defun)
                       "test_foo"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_foo"))))
      (ert-info ("Match pattern is liberal")
        (should (and (goto-char (point-min))
                     (search-forward "pass")
                     (goto-char (match-beginning 0))))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "may_be_a_test"))))
      (ert-info ("Rejects funcs without 'test' in name")
        (should (and (goto-char (point-min))
                     (search-forward "assert 1")
                     (goto-char (match-beginning 0))))
        (should (equal (python-info-current-defun) "something_else"))
        (let ((exc (should-error (pytest-pdb-break--get-node-id))))
          (should (equal exc '(error "No test found"))))))
     (pytest-pdb-break-test-with-python-buffer
      (insert source-two)
      (write-file "s2.py")
      (ert-info ("Simple Python class, two methods")
        (should (and (goto-char (point-min))
                     (search-forward "# comment")
                     (goto-char (match-beginning 0))))
        (should (equal (python-info-current-defun)
                       "TestFoo.test_bar"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "TestFoo" "test_bar"))))
      (ert-info ("Space between raises error")
        (should (and (goto-char (point-min))
                     (search-forward "assert True")
                     (progn (end-of-line)
                            (forward-char 1)
                            (looking-at-p "\n"))))
        (should-not (python-info-current-defun))
        (let ((exc (should-error (pytest-pdb-break--get-node-id))))
          (should (equal exc '(error "No test found")))))))))

(ert-deftest pytest-pdb-break-test-get-args ()
  ;; Eval: (compile "make PAT=get-args")
  (let* ((file "/tmp/a.py")
         (node-info (list file "test_a")))
    (ert-info ("No extras")
      (should-not pytest-pdb-break-extra-opts)
      (should (equal (pytest-pdb-break--get-args 9 node-info)
                     '("--break=/tmp/a.py:9" "/tmp/a.py::test_a"))))
    (ert-info ("Extras")
      (let ((pytest-pdb-break-extra-opts '("-p" "no:foo")))
        (should (equal (pytest-pdb-break--get-args 9 node-info)
                       '("-p" "no:foo"
                         "--break=/tmp/a.py:9"
                         "/tmp/a.py::test_a")))
        (should (equal pytest-pdb-break-extra-opts '("-p" "no:foo")))))))

(ert-deftest pytest-pdb-break-test-get-modified-setup-code ()
  ;; Eval: (compile "make PAT=get-modified-setup-code")
  (setq pytest-pdb-break--setup-code-addendum nil)
  (pytest-pdb-break-test-with-tmpdir
   (let ((orig-home pytest-pdb-break--home))
     (ert-info ("Relies on homer, source file must exist")
       (let* ((pytest-pdb-break--home default-directory)
              (exc (should-error (pytest-pdb-break--get-modified-setup-code))))
         (should (member (car exc) '(file-missing file-error)))))
     ;; No condition-case handler for resetting to nil
     (should (equal orig-home pytest-pdb-break--home))
     (should-not pytest-pdb-break--setup-code-addendum)))
  (ert-info ("Ordering of source snippets")
    (with-temp-buffer
      (insert (pytest-pdb-break--get-modified-setup-code))
      (goto-char (point-min))
      (ert-info ("Orig first, source second, call-snippet last")
        (should (search-forward python-shell-completion-setup-code nil t))
        (should (search-forward pytest-pdb-break--setup-code-addendum nil t))
        (backward-char (length pytest-pdb-break--setup-code-reassignment))
        (should (looking-at-p (regexp-quote
                               pytest-pdb-break--setup-code-reassignment)))))))

(ert-deftest pytest-pdb-break-test-get-proc-name ()
  ;; Eval: (compile "make PAT=get-proc-name")
  (pytest-pdb-break-test-with-tmpdir
   (pytest-pdb-break-test-with-python-buffer
    (let ((sample-source (nth 1 pytest-pdb-break-test--get-node-id-sources)))
      (insert sample-source)
      (write-file "sample.py"))
    (ert-info ("Proc reflects buffer name")
      (let ((rv (pytest-pdb-break--get-proc-name)))
        (should-not (get-buffer "*pytest-PDB[sample.py]*"))
        (should (string= rv  "pytest-PDB[sample.py]"))))
    (ert-info ("Correct signal raised")
      (let* ((buf (get-buffer-create "*pytest-PDB[sample.py]*"))
             (proc (start-process "pytest-PDB[sample.py]" buf "sleep" "10"))
             (exc (should-error (pytest-pdb-break--get-proc-name))))
        (should (eq (car exc) 'pytest-pdb-break-process-exists))
        (should (string= (process-name proc) (cadr exc)))
        (when (process-live-p proc)
          (set-process-query-on-exit-flag proc nil)
          (kill-process proc)
          (kill-buffer buf)))))))

(ert-deftest pytest-pdb-break-test-minor-mode ()
  ;; Eval: (compile "make PAT=minor-mode")
  (setq pytest-pdb-break--setup-code-addendum nil)
  (should-not pytest-pdb-break-processes)
  (should-not (buffer-live-p nil)) ; reminder (no error)
  (should-not (process-live-p nil))
  (cl-macrolet ((inside (&rest rest) `(ert-info ("Inside") ,@rest))
                (outside (&rest rest) `(ert-info ("Outside") ,@rest))
                (rip (x &optional y)
                     `(pytest-pdb-break-test-with-environment
                       (should-not pytest-pdb-break-processes)
                       (should-not pytest-pdb-break--parent-buffer)
                       (should-not pytest-pdb-break--process)
                       (let (proc)
                         (with-temp-buffer
                           (setq proc (start-process "sleeper" (current-buffer)
                                                     "sleep" "60"))
                           (should (process-buffer proc)) ; proc dies w. buffer
                           (set-process-query-on-exit-flag proc nil)
                           ,x)
                         (while (process-live-p proc) (sleep-for 0.01))
                         ,y)))
                (codechk () '(local-variable-p
                              'python-shell-completion-setup-code)))
    (ert-info ("Error when buffer has no process")
      (with-temp-buffer
        (let ((exc (should-error (pytest-pdb-break-mode +1))))
          (should (string-match-p "No live process associated" (cadr exc)))))
      (should-not pytest-pdb-break-processes))
    (ert-info ("Error when process is dead")
      (rip (inside (should (eq proc (get-buffer-process (current-buffer))))
                   (kill-process proc)
                   (while (process-live-p proc) (sleep-for 0.01))
                   (should proc)
                   (should-not (get-buffer-process (current-buffer)))
                   (let ((exc (should-error (pytest-pdb-break-mode +1))))
                     (should (string-match-p "No live process" (cadr exc)))))
           (outside (should-not pytest-pdb-break-processes))))
    (ert-info ("Normal, no *--proc var")
      (rip (inside (pytest-pdb-break-mode +1)
                   (should (memq proc pytest-pdb-break-processes))
                   (should (codechk))
                   (should (local-variable-p 'kill-buffer-hook))
                   (should-not (eq t (car kill-buffer-hook))))
           (outside (should-not (codechk))
                    (should-not pytest-pdb-break-processes))))
    (ert-info ("Normal, with *--proc, parbuf prop")
      (let ((parbuf (current-buffer)))
        (rip (inside (setq pytest-pdb-break--process proc
                           pytest-pdb-break--parent-buffer parbuf)
                     (should (local-variable-p 'pytest-pdb-break--process))
                     (pytest-pdb-break-mode +1))
             (outside (should-not pytest-pdb-break-processes)))))
    (ert-info ("Deactivation behavior")
      (let ((s1 (start-process "s1" (current-buffer) "sleep" "30")))
        (set-process-query-on-exit-flag s1 nil)
        (rip (inside (setq pytest-pdb-break--process proc) ; else latest picked
                     (pytest-pdb-break-mode +1)
                     (let ((t1 (start-process "t1" (current-buffer) "true")))
                       (setq pytest-pdb-break-processes
                             (nconc pytest-pdb-break-processes (list s1 t1)))
                       (while (process-live-p t1) (sleep-for 0.001))
                       (should (seq-set-equal-p pytest-pdb-break-processes
                                                (list proc s1 t1))))
                     (pytest-pdb-break-mode -1)
                     (should (codechk))  ; buffer-local var not removed
                     (should (equal pytest-pdb-break-processes (list s1))))
             (outside (kill-process s1)
                      (should-not pytest-pdb-break--process)
                      (pytest-pdb-break-mode -1)
                      (should-not (codechk))
                      (should-not (local-variable-p 'kill-buffer-hook))))))))

;; TODO find the proper built-in way to do this
(defun pytest-pdb-break-test--expect-timeout (pattern &optional max-secs)
  "Dumb waiter. Wait MAX-SECS for PATTERN before process mark."
  (let ((st (float-time))
        (want-disp (if (boundp '$debuggin?) $debuggin?
                     (pytest-pdb-break-test-invoked-with-debug-p)))
        (max-secs (or max-secs 5))
        found)
    (while (and (< (- (float-time) st) max-secs)
                (not (setq found (looking-back pattern nil))))
      (and want-disp (redisplay t))
      (sleep-for 0.01))
    found))

(defmacro pytest-pdb-break--main-command-fixture (linepat &rest body)
  "Run main command assertions in BODY from LINEPAT."
  `(pytest-pdb-break-test-ensure-venv
    'base
    (let ((sample-source (nth 1 pytest-pdb-break-test--get-node-id-sources))
          (python-shell-virtualenv-root $venv)
          (python-shell-interpreter $pyexe)
          (pytest-pdb-break--setup-code-addendum nil)
          (pytest-pdb-break-processes (append pytest-pdb-break-processes nil))
          ($debuggin? (pytest-pdb-break-test-invoked-with-debug-p))
          (start-dir default-directory)
          case-fold-search)
      (pytest-pdb-break-test-with-python-buffer
       (insert sample-source)
       (write-file "test_class.py")
       (should (and (goto-char (point-min))
                    (search-forward ,linepat)
                    (goto-char (match-beginning 0))))
       (call-interactively 'pytest-pdb-break-here)
       (should (get-buffer "*pytest-PDB[test_class.py]*"))
       (with-current-buffer "*pytest-PDB[test_class.py]*"
         (advice-add 'ding :override (lambda (&rest _r) (ignore))
                     '((name . neuter)))
         (unwind-protect
             (progn
               (should (eq major-mode 'inferior-python-mode))
               (should pytest-pdb-break-mode)
               (should pytest-pdb-break--process)
               (should pytest-pdb-break--parent-buffer)
               (should (equal (buffer-name pytest-pdb-break--parent-buffer)
                              "test_class.py"))
               (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
               (let ((inhibit-message t)) ,@body))
           (advice-remove 'ding 'neuter)
           (should-not (advice-member-p 'neuter 'ding))
           (let ((cap (buffer-string)))
             (with-temp-buffer
               (insert (format "debuggin?: %S\n"
                               (pytest-pdb-break-test-invoked-with-debug-p)))
               (insert cap)
               (write-file (expand-file-name "cap.out" start-dir))))
           ;; Keep buffer open for inspection when running interactively
           (unless $debuggin?
             (when (process-live-p pytest-pdb-break--process)
               (set-process-query-on-exit-flag pytest-pdb-break--process nil)
               (kill-process pytest-pdb-break--process))
             (kill-buffer))))))))

(ert-deftest pytest-pdb-break-test-main-command-basic ()
  ;; Eval: (compile "make PAT=main-command-basic")
  (pytest-pdb-break--main-command-fixture
   "assert True"
   (ert-info ("Break in first method")
     (should (save-excursion
               (search-backward-regexp ">.*\\.py(4)test_foo()\n"
                                       (point-min) t))))
   (comint-send-string pytest-pdb-break--process "c\n")
   (should (pytest-pdb-break-test--expect-timeout "finished\n.*"))))

(ert-deftest pytest-pdb-break-test-main-command-completion ()
  ;; Eval: (compile "make PAT=main-command-completion")
  ;; Eval: (compile "make debug PAT=main-command-completion")
  (pytest-pdb-break--main-command-fixture
   "# comment"
   (should (member 'python-shell-completion-at-point
                   completion-at-point-functions))
   (ert-info ("Break in second method")
     (should (save-excursion
               (search-backward-regexp ">.*\\.py(9)test_bar()\n"
                                       (point-min) t))))
   (ert-info ("Local variable, single result autocompletes")
     (execute-kbd-macro "somev\t")
     (should-not (get-buffer "*Completions*"))
     (should (looking-back "somevar"))
     (execute-kbd-macro [13])
     (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
     (should (save-excursion (search-backward "True"))))
   (ert-info ("Attr in command loop")
     (execute-kbd-macro "import sys\r")
     (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
     (execute-kbd-macro "sys.\t")
     (should (get-buffer "*Completions*"))
     (with-current-buffer "*Completions*"
       (save-excursion (goto-char (point-min))
                       (should (search-forward "sys.path"))
                       (should (search-forward "sys.version"))))
     (execute-kbd-macro "version_info.maj\t")
     (should (looking-back "sys\\.version_info\\.major"))
     (execute-kbd-macro [13])
     (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
     (should (save-excursion (search-backward-regexp "\n3\n"))))
   (ert-info ("PDB command, interactive REPL")
     (execute-kbd-macro "inter\t")
     (should (looking-back "interact"))
     (execute-kbd-macro [13])
     (should (pytest-pdb-break-test--expect-timeout ">>> "))
     (should (save-excursion (search-backward-regexp "[*]interactive[*]")))
     (ert-info ("No commands in interactive")
       (execute-kbd-macro "inter\t")
       (should (looking-back "inter"))
       (execute-kbd-macro "act\r")
       (should (pytest-pdb-break-test--expect-timeout ">>> "))
       (should (save-excursion (search-backward-regexp "NameError"))))
     (ert-info ("Locals from test available")
       (execute-kbd-macro "somev\t")
       (should (looking-back "somevar"))
       (execute-kbd-macro [13])
       (should (pytest-pdb-break-test--expect-timeout ">>> ")))
     (ert-info ("New locals")
       (execute-kbd-macro "import pathlib\r")
       (should (pytest-pdb-break-test--expect-timeout ">>> "))
       (execute-kbd-macro "pathlib.\t")
       (with-current-buffer "*Completions*"
         (save-excursion (goto-char (point-min))
                         (should (search-forward "pathlib.Path"))))
       (execute-kbd-macro "Path\r")
       (should (pytest-pdb-break-test--expect-timeout ">>> ")))
     (execute-kbd-macro [4]))
   (ert-info ("Interactive locals removed")
     (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
     (execute-kbd-macro "pathlib\r")
     (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
     (should (save-excursion (search-backward-regexp "NameError"))))
   (ert-info ("Command completion still works")
     (let ((pbef (point)))
       (execute-kbd-macro "whe\t")
       (should (pytest-pdb-break-test--expect-timeout "where"))
       (execute-kbd-macro [13])
       (should (pytest-pdb-break-test--expect-timeout "(Pdb) "))
       (should (save-excursion
                 (search-backward-regexp ">.*\\.py(9)test_bar()"
                                         pbef t)))))
   (execute-kbd-macro "c\r")
   (should (pytest-pdb-break-test--expect-timeout
            (if $debuggin? "passed.*\n?" "finished.*\n?")))))

(provide 'pytest-pdb-break-test)
;;; pytest-pdb-break-test ends here
