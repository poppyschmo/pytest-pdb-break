;;; pytest-pdb-break-test.el ---- tests -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This leaves around 40M of junk under /tmp/pytest-pdb-break-test/

;;; Code:

(require 'ert)
(require 'pytest-pdb-break)

(defvar pytest-pdb-break-test-tests
  '(pytest-pdb-break-test-ert-setup
    pytest-pdb-break-test-upstream-env-updaters
    pytest-pdb-break-test-homer
    pytest-pdb-break-test-homer-symlink
    pytest-pdb-break-test-homer-missing
    pytest-pdb-break-test-query-helper-unregistered
    pytest-pdb-break-test-query-helper-registered
    pytest-pdb-break-test-query-helper-error
    pytest-pdb-break-test-get-config-info-error
    pytest-pdb-break-test-get-config-info-unregistered
    pytest-pdb-break-test-get-node-id
    pytest-pdb-break-test-make-arg-string
    pytest-pdb-break-test-minor-mode))

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
  (should (seq-set-equal-p (mapcar 'ert-test-name (ert-select-tests t t))
                           pytest-pdb-break-test-tests))
  (should (file-exists-p pytest-pdb-break-test-repo-root))
  (should (file-exists-p pytest-pdb-break-test-lisp-root))
  (should (file-exists-p pytest-pdb-break-test-pytest-plugin))
  (should (file-exists-p pytest-pdb-break-test-lisp-main))
  (should (file-exists-p pytest-pdb-break-test-lisp-this))
  (should-not (getenv "VIRTUAL_ENV"))
  (should-not (getenv "PYTHONPATH")))

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
  "Run BODY in a temporary environment.
This is for modifying PATH, PYTHONPATH, VIRTUAL_ENV, etc."
  ;; Consider unsetting all known PYTHON* env vars
  (let ((orig (make-symbol "orig")))
    `(let ((,orig (sxhash-equal (list process-environment exec-path))))
       (let ((process-environment (append process-environment nil))
             (exec-path (append exec-path nil)))
         ,@body)
       (should (= ,orig (sxhash-equal (list process-environment
                                            exec-path)))))))

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
options are idiosyncratic and unintuitive."
  (cl-macrolet ((before (&rest rest) `(ert-info ("Before") ,@rest))
                (during (&rest rest) `(ert-info ("During") ,@rest))
                (after (&rest rest) `(ert-info ("After") ,@rest))
                (both (m b x y z)
                      `(ert-info (,m)
                         (pytest-pdb-break-test-with-python-buffer
                          ,x (let (,b) ,y) ,z)
                         (pytest-pdb-break-test-with-python-buffer
                          ,x (python-shell-with-environment ,y) ,z))))
    ;; For local files, `python-shell-with-environment' calls both
    ;; `process-environment' and `exec-path' "calculate" funcs.
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
                 (should (getenv "PATH"))) ; obvious but affirsm claim
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

(defmacro pytest-pdb-break-test-homer-setup-fixture (subform dir-body info-msg)
  "Run SUBFORM as the calling test in an Emacs ERT subprocess.
DIR-BODY sets up build dir. INFO-MSG is passed to `ert-info'."
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
  "Return full path of temp venv given shorthand symbol NAME."
  (cl-assert (symbolp name))
  (cl-assert (assq name pytest-pdb-break-test--requirements))
  (concat pytest-pdb-break-test-temp
          (format ".venv_%s" (symbol-name name)) "/"))

(defun pytest-pdb-break-test--get-requirements (name)
  "Return requirements for venv named by symbol NAME."
  (cl-assert (symbolp name))
  (cdr (assq name pytest-pdb-break-test--requirements)))

(defmacro pytest-pdb-break-test-ensure-venv (name &rest body)
  "Run BODY in a temp directory and temp environment.
NAME is a venv from --get-requirements Does not modify `PATH' or
`VIRTUAL_ENV' or `python-shell-interpreter'. Binds `$pyexe', `$venvbin',
and `$venv'. The latter two have trailing slashes. Doesn't use pip3 or
python3 because venvs are all created with python3 (not sure if this is
a sound choice)."
  (let ((piplog (make-symbol "piplog"))
        (pipexe (make-symbol "pipexe"))
        (requirements (make-symbol "requirements")))
    `(pytest-pdb-break-test-with-tmpdir
      (pytest-pdb-break-test-with-environment
       (let* (($venv (pytest-pdb-break-test--get-venv-path ,name))
              ($venvbin (concat $venv "bin/"))
              ($pyexe (concat $venvbin "python"))
              ;; XXX this is a conceit to prevent this variable from persisting
              ;; between tests. It may need to be reset manually within body.
              pytest-pdb-break-config-info-alist
              ;;
              ,pipexe ,piplog ,requirements)
         (unless (file-exists-p $venv) ; trailing/ ok
           (setq ,pipexe  (concat $venvbin "pip")
                 ,piplog (concat default-directory "pip.out")
                 ,requirements (pytest-pdb-break-test--get-requirements ,name))
           (should (file-name-absolute-p ,piplog))
           (should (zerop (call-process "python3" nil (list :file ,piplog) nil
                                        "-mvenv" $venv)))
           (should (file-executable-p ,pipexe ))
           (should (file-executable-p $pyexe))
           (should-not (equal (executable-find "pip") ,pipexe ))
           (should-not (equal (executable-find "python") $pyexe))
           (unless (eq ,name 'bare)
             (should (zerop (apply #'call-process ,pipexe  nil
                                   (list :file ,piplog) nil
                                   "install" ,requirements)))))
         ,@body)))))

(defmacro pytest-pdb-break-test--query-wrap (&rest body)
  "Run BODY with common assertions/bindings for config-info-helper func."
  `(let (($rootdir (directory-file-name default-directory))
         ($rv (gensym)))
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

(ert-deftest pytest-pdb-break-test-get-config-info-error ()
  (cl-macrolet
      ((fails
        (setup logfile)
        `(pytest-pdb-break-test-with-python-buffer
          ,setup
          (should (cl-notany python-shell-extra-pythonpaths
                             python-shell-exec-path
                             python-shell-virtualenv-root
                             ;; ours
                             pytest-pdb-break--config-info))
          (let ((exc (should-error (pytest-pdb-break-get-config-info))))
            (should (eq 'error (car exc)))
            (should (string-match-p "Error calling" (cadr exc)))
            (with-temp-buffer (insert (cadr exc)) (write-file ,logfile)))
          (should-not pytest-pdb-break-config-info-alist)
          (should-not pytest-pdb-break--config-info))))
    (pytest-pdb-break-test-ensure-venv
     'base
     (ert-info ("System Python has no pytest")
       (fails (progn (should-not python-shell-process-environment)
                     (should-not pytest-pdb-break-config-info-alist))
              "error_no_pytest.out"))
     (ert-info ("Manually set VIRTUAL_ENV")
       (fails (progn (should-not python-shell-process-environment)
                     (should-not pytest-pdb-break-config-info-alist)
                     (setenv "VIRTUAL_ENV" $venv))
              "error_virtual_env_manual.out"))
     (ert-info ("Set extra env vars to override")
       (let ((python-shell-process-environment
              (list (format "PATH=%s:%s" $venvbin (getenv "PATH"))
                    (format "VIRTUAL_ENV=%s" $venvbin))))
         (fails (should python-shell-process-environment)
                "error_proc_env_api.out")))
     (ert-info ("Invalid cdr for alist entry")
       (let ((pytest-pdb-break-config-info-alist
              (list (list (executable-find python-shell-interpreter)))))
         (fails (progn
                  (should-not python-shell-process-environment)
                  (should pytest-pdb-break-config-info-alist))
                "error_invalid_cdr.out")
         (should-not pytest-pdb-break-config-info-alist))))))

(ert-deftest pytest-pdb-break-test-get-config-info-unregistered ()
  (cl-macrolet
      ((rinse-repeat
        (before after)
        `(pytest-pdb-break-test-with-python-buffer
          (should-not pytest-pdb-break--config-info)
          ,before
          (let ((rv (pytest-pdb-break-get-config-info)))
            (should (= 6 (length pytest-pdb-break--config-info)))
            (should (eq (cdr rv) pytest-pdb-break--config-info))
            (should (eq (car rv)
                        (plist-get pytest-pdb-break--config-info :exe)))
            (should pytest-pdb-break-config-info-alist)
            (should (eq (assoc (car rv) pytest-pdb-break-config-info-alist)
                        rv)))
          ,after
          ;; Effectively global
          (setq pytest-pdb-break-config-info-alist nil))))
    (pytest-pdb-break-test-ensure-venv
     'base
     (ert-info ("Set virtual environment root")
       ;; See upstream test above re -= slash
       (let ((python-shell-virtualenv-root $venv)
             (unslashed (directory-file-name $venvbin)))
         (rinse-repeat
          (should-not (member unslashed exec-path))
          (progn
            (should-not (directory-name-p
                         (plist-get pytest-pdb-break--config-info :rootdir)))
            (should-not (member unslashed exec-path))))))
     (ert-info ("Set extra `exec-path' items to prepend")
       (let ((python-shell-exec-path (list $venvbin)))
         (rinse-repeat
          (should-not (member $venvbin exec-path))
          (progn
            (should-not (directory-name-p
                         (plist-get pytest-pdb-break--config-info :rootdir)))
            (should-not (member $venvbin exec-path))))))
     (ert-info ("Baseline for \"Don't shell out ...\"")
       (let ((python-shell-interpreter $pyexe))
         (rinse-repeat
          (should-not pytest-pdb-break-config-info-alist)
          (should (equal pytest-pdb-break--config-info
                         (list :exe $pyexe
                               :registered nil
                               :rootdir (directory-file-name
                                         default-directory)))))))
     (ert-info ("Don't shell out when entry exists, even with invalid plist")
       (let* ((python-shell-interpreter $pyexe)
              (fake-entry (list $pyexe
                                :exe $pyexe
                                :fookey "fooval"
                                :barkey "barval"))
              (pytest-pdb-break-config-info-alist (list fake-entry)))
         (rinse-repeat
          (should pytest-pdb-break-config-info-alist)
          (should (equal pytest-pdb-break--config-info (cdr fake-entry)))))))))

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
"))

(ert-deftest pytest-pdb-break-test-get-node-id ()
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

(ert-deftest pytest-pdb-break-test-make-arg-string ()
  (let* ((file "/tmp/a.py")
         (node-info (list file "test_a")))
    (ert-info ("Plugin present")
      (should-not pytest-pdb-break-extra-opts)
      (should (string= (pytest-pdb-break--make-arg-string 9 node-info t)
                       "-mpytest --break=/tmp/a.py:9 /tmp/a.py::test_a")))
    (ert-info ("Plugin present, extras")
      (let* ((pytest-pdb-break-extra-opts '("-p" "no:foo"))
             (rv (pytest-pdb-break--make-arg-string 9 node-info t))
             (x "-mpytest -p no:foo --break=/tmp/a.py:9 /tmp/a.py::test_a"))
        (should (string= rv x))
        (should (equal pytest-pdb-break-extra-opts '("-p" "no:foo")))))
    (ert-info ("Plugin missing")
      (should-not pytest-pdb-break-extra-opts)
      (should (string= (pytest-pdb-break--make-arg-string 9 node-info nil)
                       (concat "-mpytest -p pytest_pdb_break "
                               "--break=/tmp/a.py:9 /tmp/a.py::test_a"))))
    (ert-info ("Plugin missing, extras overlap")
      (let* ((pytest-pdb-break-extra-opts '("-p" "pytest_pdb_break"))
             (rv (pytest-pdb-break--make-arg-string 9 node-info nil)))
        (should (string= rv (concat
                             "-mpytest -p pytest_pdb_break "
                             "--break=/tmp/a.py:9 /tmp/a.py::test_a")))
        (should (equal pytest-pdb-break-extra-opts
                       '("-p" "pytest_pdb_break")))))))

(ert-deftest pytest-pdb-break-test-minor-mode ()
  ;; Eval: (compile "make PAT=minor-mode")
  (should-not pytest-pdb-break-processes)
  (should-not (buffer-live-p nil)) ; reminder (no error)
  (should-not (process-live-p nil))
  (cl-macrolet ((inside (&rest rest) `(ert-info ("Inside") ,@rest))
                (outside (&rest rest) `(ert-info ("Outside") ,@rest))
                (rip (x &optional y)
                     `(pytest-pdb-break-test-with-environment
                       (let (proc)
                         (with-temp-buffer
                           (setq proc (start-process "sleeper" (current-buffer)
                                                     "sleep" "3"))
                           (set-process-query-on-exit-flag proc nil) ; crutch
                           ,x)
                         ,y)
                       (setq pytest-pdb-break-processes nil))))
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
      (rip (inside (should-not pytest-pdb-break--process)
                   (pytest-pdb-break-mode +1)
                   (should (memq proc pytest-pdb-break-processes))
                   (should (advice-member-p
                            'pytest-pdb-break-ad-around-get-completions
                            #'python-shell-completion-get-completions))
                   (should (local-variable-p 'kill-buffer-hook))
                   (should-not (eq t (car kill-buffer-hook))))
           (outside (should-not (advice-member-p
                                 'pytest-pdb-break-ad-around-get-completions
                                 #'python-shell-completion-get-completions))
                    (should-not pytest-pdb-break-processes)
                    (should-not (local-variable-p
                                 'pytest-pdb-break--config-info)))))
    (ert-info ("Normal, with *--proc, parbuf prop")
      (let ((parbuf (current-buffer)))
        (should-not (local-variable-p 'pytest-pdb-break--config-info))
        (rip (inside (setq pytest-pdb-break--process proc
                           pytest-pdb-break--parent-buffer parbuf)
                     (should (local-variable-p 'pytest-pdb-break--process))
                     (pytest-pdb-break-mode +1))
             (outside (should-not pytest-pdb-break-processes)
                      (should (local-variable-p
                               'pytest-pdb-break--config-info))))))
    (ert-info ("Deactivation behavior")
      (let ((s1 (start-process "s1" (current-buffer) "sleep" "3")))
        (set-process-query-on-exit-flag s1 nil)
        (rip (inside (pytest-pdb-break-mode +1)
                     (let ((t1 (start-process "t1" (current-buffer) "true")))
                       (nconc pytest-pdb-break-processes (list s1 t1))
                       (while (process-live-p t1) (sleep-for 0.001))
                       (should (seq-set-equal-p pytest-pdb-break-processes
                                                (list proc s1 t1))))
                     (pytest-pdb-break-mode -1)
                     (should (seq-set-equal-p pytest-pdb-break-processes
                                              (list s1))))
             (outside (kill-process s1)
                      (should-not pytest-pdb-break--process)
                      (should (advice-member-p
                               'pytest-pdb-break-ad-around-get-completions
                               #'python-shell-completion-get-completions))
                      (pytest-pdb-break-mode -1)
                      (should (process-live-p proc))
                      (should-not (advice-member-p
                                   'pytest-pdb-break-ad-around-get-completions
                                   #'python-shell-completion-get-completions))
                      (should-not (local-variable-p 'kill-buffer-hook))
                      (kill-process proc)))))))

(provide 'pytest-pdb-break-test)
;;; pytest-pdb-break-test ends here
