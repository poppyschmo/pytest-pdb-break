;;; pytest-pdb-break-test.el --- Tests -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This leaves around 30M of junk under /tmp/pytest-pdb-break-test/
;;
;; If revisiting the `query-helper'/`config-info'-centered approach, see
;; 2824cc74d1e05bdbd2aed580f4a5844c4cae0495 or earlier.  These used -mpytest,
;; rootdir as cwd, etc.

;;; Code:

(require 'ert)
(require 'term)
(require 'pytest-pdb-break)
(require 'pytest-pdb-break-extra)

(defvar pytest-pdb-break-test-tests
  '(pytest-pdb-break-test-ert-setup
    pytest-pdb-break-test-library-version
    pytest-pdb-break-test-upstream-env-updaters
    pytest-pdb-break-test-dump-internal-error
    pytest-pdb-break-test-homer
    pytest-pdb-break-test-homer-installed
    pytest-pdb-break-test-homer-symlink
    pytest-pdb-break-test-homer-missing
    pytest-pdb-break-test-on-kill-emacs
    pytest-pdb-break-test-create-tempdir
    pytest-pdb-break-test-call-interpreter
    pytest-pdb-break-test-get-isolated
    pytest-pdb-break-test-get-pytest-executable
    pytest-pdb-break-test-get-interpreter-version
    pytest-pdb-break-test-extract-shebang
    pytest-pdb-break-test-get-python-interpreter
    pytest-pdb-break-test-call-helper-json
    pytest-pdb-break-test-prompt-for-test-item
    pytest-pdb-break-test-get-node-id
    pytest-pdb-break-test-get-args
    pytest-pdb-break-test-get-modified-setup-code
    pytest-pdb-break-test-get-proc-name
    pytest-pdb-break-test-maybe-get-parent-buffer
    pytest-pdb-break-test-minor-mode
    pytest-pdb-break-test-set-shell-buffer-name
    pytest-pdb-break-test-kill-shell-buffer-name
    pytest-pdb-break-test-interpret-prefix-arg
    pytest-pdb-break-test-read-session-options
    pytest-pdb-break-test-default-options-function
    pytest-pdb-break-test-main-command-min-version
    pytest-pdb-break-test-main-command-basic
    pytest-pdb-break-test-main-command-send-string
    pytest-pdb-break-test-main-command-completion
    pytest-pdb-break-test-run-fail-compilation-filter
    pytest-pdb-break-test-run-fail-comint-process-filter
    pytest-pdb-break-test-go-inferior
    pytest-pdb-break-test-completion-compat
    pytest-pdb-break-test-run-fail-stay
    pytest-pdb-break-test-run-fail-switch
    pytest-pdb-break-test-elpy-shell-get-or-create-process-advice
    pytest-pdb-break-test-advise-elpy-shell-get-proc
    pytest-pdb-break-test-checkdoc))

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

(defvar pytest-pdb-break-test-lisp-extra
  (concat pytest-pdb-break-test-lisp-root "pytest-pdb-break-extra.el"))

(defvar pytest-pdb-break-test-lisp-this
  (concat pytest-pdb-break-test-lisp-root "pytest-pdb-break-test.el"))

(defvar pytest-pdb-break-test-skip-plugin
  (getenv "PYTEST_PDB_BREAK_TEST_SKIP_PLUGIN")
  "Don't run tests involving the actual pytest plugin.
Calls to external helpers are fair game, but the actual plugin depends
on Python 3.6.")

(ert-deftest pytest-pdb-break-test-ert-setup ()
  ;; Eval: (compile "make PAT=ert-setup")
  (should (seq-set-equal-p (mapcar 'ert-test-name (ert-select-tests t t))
                           pytest-pdb-break-test-tests))
  (should (file-exists-p pytest-pdb-break-test-repo-root))
  (should (file-exists-p pytest-pdb-break-test-tempdir))
  (should (file-exists-p pytest-pdb-break-test-lisp-root))
  (should (file-exists-p pytest-pdb-break-test-pytest-plugin))
  (should (file-exists-p pytest-pdb-break-test-lisp-main))
  (should (file-exists-p pytest-pdb-break-test-lisp-extra))
  (should (file-exists-p pytest-pdb-break-test-lisp-this))
  (should-not (getenv "VIRTUAL_ENV"))
  (should-not (getenv "PYTHONPATH")))

(eval-when-compile

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
    (not noninteractive)))

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
an optional TAIL appended.  If TAIL doesn't start with a dir
sep (slash), the dir name itself is altered (suffixed).  To create a
subdir, TAIL should start with a dir sep."
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

(defmacro pytest-pdb-break-test--with-messages (&rest body)
  "Run BODY without echoing and return whatever would have been."
  `(let ((inhibit-message t)
         (where (with-current-buffer (messages-buffer)
                  ;; Don't save excursion
                  (set-marker (make-marker) (goto-char (point-max))))))
     ,@body
     (with-current-buffer (messages-buffer)
       (buffer-substring where (point-max)))))

(defmacro pytest-pdb-break-test-with-conditional-env-var (present absent)
  "Run PRESENT if test's name has been exported as an env var.
Otherwise, run ABSENT.  Vars `$test-sym' and `$env-var' are bound to the
current test func and its env-var-ized string.  High risk of infinite
looping."
  `(let* (($test-sym (ert-test-name (ert-running-test)))
          ($env-var (pytest-pdb-break-test--name-to-envvar $test-sym)))
     (if (getenv $env-var)
         ,present
       ,@(macroexp-unprogn absent))))

(defmacro pytest-pdb-break-test-with-python-buffer (&rest body)
  "Run BODY in a `python-mode' temp buffer with a temp environment.
Note: this does *not* create and cd to a temp dir.  Helper `$get-there'
moves to next occurrence of a fixed string (and returns point) or
returns nil."
  `(pytest-pdb-break-test-with-environment
    (cl-flet (($get-there (lambda (s) (and (goto-char (point-min))
                                           (search-forward s)
                                           (goto-char (match-beginning 0))))))
      (with-temp-buffer
        (let (python-indent-guess-indent-offset)
          (python-mode))
        ,@body))))

(ert-deftest pytest-pdb-break-test-library-version ()
  ;; Eval: (compile "make PAT=library-version")
  (let (ours theirs)
    (with-temp-buffer
      (let ((default-directory pytest-pdb-break-test-repo-root))
        (should (file-exists-p "setup.py"))
        (should (zerop (call-process "python3" nil (current-buffer) nil
                                     "setup.py" "--version")))
        (goto-char (point-min))
        (setq theirs (buffer-substring (point) (point-at-eol)))))
    (with-temp-buffer
      (insert-file-contents-literally pytest-pdb-break-test-lisp-main)
      (goto-char (point-min))
      (should (setq ours (and (search-forward-regexp "^;; Version: \\(.+\\)$")
                              (match-string-no-properties 1)))))
    (should (string= ours theirs))))

(ert-deftest pytest-pdb-break-test-upstream-env-updaters ()
  "Describe expected behavior of built-in `python-mode' interface.
Show that it doesn't restore environment to previous state.

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

(ert-deftest pytest-pdb-break-test-dump-internal-error ()
  ;; Eval: (compile "make PAT=dump-internal-error")
  (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
  (let (capped)
    (pytest-pdb-break-test-with-tmpdir
     (pytest-pdb-break--dump-internal-error "1\n2\n3")
     (with-current-buffer pytest-pdb-break--errors-buffer-name
       (goto-char (point-min))
       (should (search-forward-regexp "[[].+[]][[:space:]123]+")))
     (pytest-pdb-break--dump-internal-error "a\nb\nc")
     (with-current-buffer pytest-pdb-break--errors-buffer-name
       (goto-char (point-min))
       (should (search-forward-regexp "[[].+[]][[:space:]123]+"))
       (should (search-forward-regexp "[[].+[]][[:space:]abc]+"))
       (setq capped (buffer-string))
       (kill-buffer))
     (with-temp-file "abc123.out"
       (insert capped)))))

(defmacro pytest-pdb-break-test-homer-repo-fixture ()
  "Common assertions for the home-finder when not installed.
Likely running from the cloned repo."
  '(pytest-pdb-break-test-with-tmpdir
    (should-not pytest-pdb-break--py-home)
    (should (string= (pytest-pdb-break--homer)
                     pytest-pdb-break-test-repo-root))
    (should (directory-name-p pytest-pdb-break--py-home)) ; ends in /
    (should (string= pytest-pdb-break--py-home
                     pytest-pdb-break-test-repo-root))))

(ert-deftest pytest-pdb-break-test-homer ()
  ;; Eval: (compile "make SYM=pytest-pdb-break-test-homer")
  (ert-info ("Find cloned repo containing pytest plugin")
    (pytest-pdb-break-test-homer-repo-fixture)))

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
DIR-BODY sets up build dir.  INFO-MSG is passed to `ert-info'."
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
                 (byte-compile-file "./pytest-pdb-break-extra.el")
                 default-directory)))
       (ert-info (,info-msg)
         (should (zerop (pytest-pdb-break-test-run-ert-in-subprocess
                         file
                         (list 'quote $test-sym)
                         :load-path-dir dir
                         :env-vars (list (cons $env-var "1"))
                         :logfile "test.out"))))))))

(ert-deftest pytest-pdb-break-test-homer-installed ()
  ;; Eval: (compile "make PAT=homer-installed")
  ;; The package.el layout in which emacs lisp files live in the root
  (pytest-pdb-break-test-homer-setup-fixture
   (ert-info ("Subproc")
     (should-not pytest-pdb-break--py-home)
     (pytest-pdb-break-test-with-tmpdir
      (let ((expected (concat pytest-pdb-break-test-temp
                              (pytest-pdb-break-test--unprefix $test-sym)
                              "-setup/build/lib/")))
        (should (string= (pytest-pdb-break--homer) expected))
        (should (directory-name-p pytest-pdb-break--py-home)) ; ends in /
        (should (string= pytest-pdb-break--py-home expected)))))
   (ert-info ("Setup")
     (copy-file pytest-pdb-break-test-lisp-main "./")
     (copy-file pytest-pdb-break-test-lisp-extra "./")
     (mkdir "lib")
     (with-temp-file "lib/pytest_pdb_break.py"
       (ignore)))
   ;; info-msg
   "Python package installed with 'lib' subdir"))

(ert-deftest pytest-pdb-break-test-homer-symlink ()
  ;; Eval: (compile "make PAT=homer-symlink")
  (pytest-pdb-break-test-homer-setup-fixture
   (ert-info ("Subproc")
     (should-not (fboundp 'ffip-project-root))
     (should (file-symlink-p (find-library-name "pytest-pdb-break")))
     (should (file-symlink-p (find-library-name "pytest-pdb-break-extra")))
     (pytest-pdb-break-test-homer-repo-fixture))
   (ert-info ("Setup")
     (make-symbolic-link pytest-pdb-break-test-lisp-main "./")
     (make-symbolic-link pytest-pdb-break-test-lisp-extra "./"))
   ;; info-msg
   "Find home, resolving symlinks"))

(ert-deftest pytest-pdb-break-test-homer-missing ()
  ;; Eval: (compile "make PAT=homer-missing")
  (pytest-pdb-break-test-homer-setup-fixture
   (ert-info ("Subrpoc")
     ;; Looks "above" in pytest-pdb-break-test-temp, then for "./lib"
     (let ((exc (should-error (pytest-pdb-break-test-homer-repo-fixture)))
           (case-fold-search t))
       (should (string-match-p "cannot find.*files" (cadr exc)))
       (should-not pytest-pdb-break--py-home)))
   (ert-info ("Setup")
     (copy-file pytest-pdb-break-test-lisp-main "./")
     (copy-file pytest-pdb-break-test-lisp-extra "./"))
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
`VIRTUAL_ENV' or `python-shell-interpreter'.  Binds `$pyexe',
`$venvbin', and `$venv'.  The latter two have trailing slashes.  Doesn't
use pip3 or python3 because venvs are all created with python3 (not sure
if this is a sound choice)."
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

(ert-deftest pytest-pdb-break-test-call-interpreter ()
  ;; Eval: (compile "make PAT=call-interpreter")
  (pytest-pdb-break-test-ensure-venv
   'bare
   (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
   (ert-info ("Successful call")
     (let ((rv (pytest-pdb-break--call-interpreter
                $pyexe "-c" "import sys; print(sys.argv)" "--" "foo")))
       (should (string-match-p "[[].-c.*foo.[]]" rv))))
   (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
   (ert-info ("Bad call")
     (let ((exc (should-error
                 (pytest-pdb-break--call-interpreter $pyexe "-c" "foo")))
           case-fold-search outstr)
       (should (string-match-p "Call to.*exited.*" (cadr exc)))
       (with-current-buffer
           pytest-pdb-break--errors-buffer-name
         (setq outstr (buffer-string))
         (with-temp-file "traceback.out" (insert outstr))
         (goto-char (point-min))
         (should (search-forward-regexp "Traceback"))
         (should (search-forward-regexp "NameError"))
         (kill-buffer))))
   (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
   (ert-info ("Successful call, no error")
     (let ((rv (pytest-pdb-break--call-interpreter
                $pyexe 'no-error
                "-c" "import sys; print(sys.argv)" "--" "foo")))
       (should (consp rv))
       (should (= 0 (car rv)))
       (should (string-match-p "[[].-c.*foo.[]]" (cdr rv)))))
   (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
   (ert-info ("Bad call, no error")
     (let ((rv (pytest-pdb-break--call-interpreter $pyexe t "-c" "foo"))
           case-fold-search)
       (should (not (zerop (car rv))))
       (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
       (should (string-match-p "Traceback" (cdr rv)))))
   (should-not (get-buffer pytest-pdb-break--errors-buffer-name))))

(ert-deftest pytest-pdb-break-test-get-isolated ()
  ;; Eval: (compile "make PAT=get-isolated")
  (pytest-pdb-break-test-ensure-venv
   'bare
   (make-directory "fake-tmpdir")
   (setenv "PYTEST_PDB_BREAK_INSTALL_LOGFILE" "helper.log")
   (let* ((pytest-pdb-break--py-home pytest-pdb-break-test-repo-root)
          (pytest-pdb-break--tempdir (file-name-as-directory
                                      (file-truename "fake-tmpdir")))
          pytest-pdb-break--isolated
          (rv (pytest-pdb-break-get-isolated $pyexe)))
     (should (file-exists-p rv))
     (should (directory-name-p rv))
     (should (string= rv pytest-pdb-break--isolated))
     (should (file-exists-p (concat rv "/pytest_pdb_break.py")))
     (should (string-match-p (regexp-quote pytest-pdb-break--tempdir) rv))
     (should-not (member #'pytest-pdb-break--on-kill-emacs kill-emacs-hook))
     (should-not (null (directory-files rv nil "\\.*-info"))))
   (should-not pytest-pdb-break--isolated)))

(ert-deftest pytest-pdb-break-test-get-pytest-executable ()
  ;; Eval: (compile "make PAT=get-pytest-executable")
  (ert-info ("Not found")
    (let* ((exec-path (list (concat pytest-pdb-break-test-temp "foo")))
           (exc (should-error (pytest-pdb-break-get-pytest-executable))))
      (should (string-match-p "not found" (cadr exc)))))
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

(ert-deftest pytest-pdb-break-test-get-interpreter-version ()
  ;; Eval: (compile "make PAT=get-interpreter-version")
  (pytest-pdb-break-test-ensure-venv
   'bare
   (should-not pytest-pdb-break--versions-alist)
   (let (pytest-pdb-break--versions-alist)
     (ert-info ("Fake interpreter")
       (let ((python-exe (concat default-directory "fake-python")))
         (with-temp-file python-exe
           (insert "#!/bin/sh\nprintf '3.7.2'\n"))
         (chmod python-exe #o0700)
         (should (string=
                  "3.7.2"
                  (pytest-pdb-break--get-interpreter-version python-exe)))
         (should (equal `(,python-exe . "3.7.2")
                        (assoc python-exe pytest-pdb-break--versions-alist)))
         (ert-info ("Returns existing")
           (with-temp-file python-exe (insert "#!/bin/sh\nexit 1\n"))
           (chmod python-exe #o0700)
           (should (string=
                    "3.7.2"
                    (pytest-pdb-break--get-interpreter-version python-exe))))
         (ert-info ("Update")
           (with-temp-file python-exe
             (insert "#!/bin/sh\nprintf '2.7.2'\n"))
           (chmod python-exe #o0700)
           (should (string=
                    "2.7.2"
                    (pytest-pdb-break--get-interpreter-version python-exe
                                                               'force)))
           (should (equal `((,python-exe . "2.7.2"))
                          pytest-pdb-break--versions-alist)))))
     (ert-info ("Real interpreter")
       (let ((rv (pytest-pdb-break--get-interpreter-version $pyexe)))
         (should (version<= "2.7" rv))
         (should (version< rv "4.0")))))
   (should-not pytest-pdb-break--versions-alist)))

(defun pytest-pdb-break-test--remove-pytest-pyenv ()
  "Return exec path without pytest or pyenv bins."
  (seq-filter (lambda (p)
                (let ((exec-path (list p)))
                  (not (or (executable-find "pyenv")
                           (executable-find "pytest")))))
              exec-path))

(ert-deftest pytest-pdb-break-test-extract-shebang ()
  ;; Eval: (compile "make PAT=extract-shebang-pyenv-shim")
  (pytest-pdb-break-test-with-tmpdir
   (let* ((pyenv-libexec (concat default-directory "pyenv-libexec/"))
          (versions-bin (concat default-directory "versions-bin/"))
          (fake-pyenv (concat pyenv-libexec "pyenv"))
          (pytest-exe (concat versions-bin "pytest"))
          (exec-path (cons (directory-file-name pyenv-libexec)
                           (pytest-pdb-break-test--remove-pytest-pyenv)))
          (real-python (executable-find "python"))
          (shim (concat default-directory "shim")))
     (mkdir pyenv-libexec)
     (mkdir versions-bin)
     (with-temp-file "shim"
       (insert "#!/usr/bin/env bash\nset -e\nexport PYENV_ROOT"))
     (with-temp-file fake-pyenv
       (insert (format "#!/bin/sh\necho %s" pytest-exe))) ; want newline
     (with-temp-file pytest-exe
       (insert "#!" real-python "\n\n# -*- coding: utf-8 -*-\nimport re\n"))
     (chmod shim #o0700)
     (chmod fake-pyenv #o0700)
     (chmod pytest-exe #o0700)
     (should (string= (pytest-pdb-break--extract-shebang shim)
                      real-python)))))

(ert-deftest pytest-pdb-break-test-get-python-interpreter ()
  ;; Eval: (compile "make PAT=get-python-interpreter")
  (pytest-pdb-break-test-ensure-venv
   'base
   (let ((pytest-exe (concat default-directory "fake-pytest"))
         (python-exe (concat default-directory "fake-python"))
         pytest-pdb-break--exe-alist
         exc)
     (with-temp-file pytest-exe
       (insert "#!" python-exe "\n\n# -*- coding: utf-8 -*-\nimport re\n"))
     (ert-info ("Shebanged file not found")
       (setq exc (should-error (pytest-pdb-break-get-python-interpreter
                                pytest-exe)))
       (should-not pytest-pdb-break--exe-alist)
       (should (string-match-p "Cannot find.*" (cadr exc)))
       (should (get-buffer pytest-pdb-break--errors-buffer-name))
       (kill-buffer pytest-pdb-break--errors-buffer-name))
     (push (list pytest-exe) pytest-pdb-break--exe-alist)
     (with-temp-file python-exe (insert "#!/bin/true\n"))
     (ert-info ("Not executable, bad entry cleared from alist")
       (setq exc (should-error (pytest-pdb-break-get-python-interpreter
                                pytest-exe)))
       (should-not pytest-pdb-break--exe-alist)
       (should (string-match-p "Cannot find.*" (cadr exc)))
       (should (get-buffer pytest-pdb-break--errors-buffer-name))
       (kill-buffer pytest-pdb-break--errors-buffer-name))
     (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
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
         (should (string= rv $pyexe)))))
   (should-not (get-buffer pytest-pdb-break--errors-buffer-name))))

(defvar pytest-pdb-break-test--get-node-id-sources
  '("
def test_foo():
    somevar = 1
    assert True

def may_be_a_test():
    pass
"
    "
class TestFoo:
    def test_foo(self):
        assert True

    def test_bar(self):
        somevar = True
        # comment
        assert somevar
"
    "
class TestFoo:
    def test_foo(self):
        def test_f():
            return 1
        assert test_f()

    def test_bar(self):
        def f():
            return 2
        assert f()

    def not_test_baz(self):
        def test_f():
            return 3
        assert test_f()
")
  "The first line (1) is a single newline char.")

(ert-deftest pytest-pdb-break-test-call-helper-json ()
  ;; Eval: (compile "make PAT=call-helper-json")
  ;; Eval: (compile "make debug PAT=call-helper-json")
  (pytest-pdb-break-test-ensure-venv
   'base
   (dotimes (n 3)
     (with-temp-file (format "test_s%d.py" n)
       (insert (nth n pytest-pdb-break-test--get-node-id-sources))))
   (ert-info ("Call get_node_ids")
     (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
     (let* ((data (pytest-pdb-break--call-helper-json $pyexe "get_node_ids")))
       (should (equal "test_s0.py::test_foo" (nth 0 data)))))
   (unless pytest-pdb-break-test-skip-plugin
     (ert-info ("Call get_collected")
       (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
       (let* ((data (pytest-pdb-break--call-helper-json $pyexe
                                                        "get_collected")))
         (should (equal `(:file ,(concat default-directory "test_s0.py")
                                :lnum 2
                                :name "test_foo"
                                :class_name nil
                                :func_name "test_foo"
                                :param_id nil
                                :nodeid "test_s0.py::test_foo")
                        (nth 0 data))))))
   (ert-info ("Call get_node_ids, pytest error")
     (should-not (get-buffer pytest-pdb-break--errors-buffer-name))
     (let* ((exc (should-error (pytest-pdb-break--call-helper-json
                                $pyexe "get_node_ids" "--bad-option"))))
       (should (string-match-p "Call to.*exited [[:digit:]]+" (cadr exc)))
       (should (get-buffer pytest-pdb-break--errors-buffer-name))
       (unless (pytest-pdb-break-test-invoked-with-debug-p)
         (kill-buffer pytest-pdb-break--errors-buffer-name))))
   (unless (pytest-pdb-break-test-invoked-with-debug-p)
     (should-not (get-buffer pytest-pdb-break--errors-buffer-name)))))

(ert-deftest pytest-pdb-break-test-prompt-for-test-item ()
  ;; Eval: (compile "make PAT=prompt-for-test-item")
  ;; Eval: (compile "make debug PAT=prompt-for-test-item")
  (skip-unless (null pytest-pdb-break-test-skip-plugin))
  (pytest-pdb-break-test-ensure-venv
   'base
   (dotimes (n 3)
     (with-temp-file (format "test_s%d.py" n)
       (insert (nth n pytest-pdb-break-test--get-node-id-sources))))
   (add-hook 'term-mode-hook (lambda () (term-reset-size 20 120)))
   (let* ((logfile (file-truename "ert.out")) ; must be bound here
          ;; When called interactively, CWD reverts to this project's lisp dir
          (src `(progn (require 'pytest-pdb-break)
                       (push ,$venvbin exec-path)
                       (setq enable-recursive-minibuffers t)
                       (with-current-buffer (messages-buffer)
                         (setq default-directory ,default-directory))))
          (args (list "-Q" "-nw" "-L" pytest-pdb-break-test-lisp-root
                      "--eval" (format "%S" src)))
          (buf (apply #'make-term "ert-term" "emacs" nil args))
          (proc (get-buffer-process buf)))
     (with-current-buffer buf
       (unwind-protect
           (ert-info ("Term subprocess")
             (term-char-mode)
             (goto-char (process-mark proc))
             (term-send-raw-string "\C-x\C-f:")
             (should (pytest-pdb-break-test--expect-simple "Find file.*/?"))
             (term-send-raw-string "test_s0.py\n")
             (term-send-raw-string "\e>")
             (ert-info ("Files prompt")
               (term-send-raw-string "\e:")
               (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
               (term-send-string
                proc "(pytest-pdb-break--prompt-for-test-item nil)\n")
               (ert-info ("Common prefix autocompletes")
                 (should (pytest-pdb-break-test--expect-simple "File: ?"))
                 (term-send-string proc "\t")
                 (should (pytest-pdb-break-test--expect-simple "/test_s ?")))
               (ert-info ("Completions buffer on tab")
                 (term-send-string proc "\t")
                 (term-send-raw-string "\e:")
                 (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
                 (term-send-string proc "(get-buffer \"*Completions*\")\n")
                 (should (pytest-pdb-break-test--expect-simple "/test_s ?"))
                 (term-send-string proc "1.py\n"))
               (ert-info ("Test prompt")
                 (should (pytest-pdb-break-test--expect-simple "Test: ?"))
                 (term-send-string proc "\t\t")
                 (should (pytest-pdb-break-test--expect-simple "\\.test_"))
                 (term-send-string proc "f\t")
                 (should (pytest-pdb-break-test--expect-simple "test_foo"))
                 (term-send-string proc "\n"))
               (ert-info ("Check return value")
                 (term-send-raw-string "\e:")
                 (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
                 (term-send-string proc
                                   "(switch-to-buffer (messages-buffer))\n")
                 (term-send-raw-string "\e>")
                 (should (pytest-pdb-break-test--expect-simple "Mark set\n.*"))
                 (term-line-mode)
                 (should (search-backward-regexp
                          "/test_s1.*TestFoo.*[[:space:]]*.*test_foo"))))
             (term-send-raw-string "\C-x\C-c")
             (should (pytest-pdb-break-test--timeout
                      (lambda nil (not (process-live-p proc))))))
         (unless (pytest-pdb-break-test-invoked-with-debug-p)
           (write-file logfile)
           (kill-buffer buf)))))))

(ert-deftest pytest-pdb-break-test-get-node-id ()
  ;; Eval: (compile "make PAT=get-node-id")
  ;; For now, assume Elpy branch is infallible, and just test ours
  (let ((source-one (nth 0 pytest-pdb-break-test--get-node-id-sources))
        (source-two (nth 1 pytest-pdb-break-test--get-node-id-sources))
        (source-three (nth 2 pytest-pdb-break-test--get-node-id-sources))
        case-fold-search)
    (pytest-pdb-break-test-with-tmpdir
     (pytest-pdb-break-test-with-python-buffer
      (insert source-one)
      (write-file "s1.py") ; doesn't filter based on file name
      (ert-info ("Simple Python func, no docstring, comments")
        (should (equal buffer-file-name (expand-file-name "s1.py")))
        (should ($get-there "assert True"))
        (should (equal (python-info-current-defun)
                       "test_foo"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_foo"))))
      (ert-info ("Name must start with 'test'")
        (should ($get-there "pass"))
        (let ((exc (should-error (pytest-pdb-break--get-node-id))))
          (should (equal (car exc) 'pytest-pdb-break-test-not-found))
          (should (equal (cadr exc) "may_be_a_test")))))
     (pytest-pdb-break-test-with-python-buffer
      (insert source-two)
      (write-file "s2.py")
      (ert-info ("Simple Python class, two methods")
        (should ($get-there "# comment"))
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
          (should (equal (car exc) 'pytest-pdb-break-test-not-found))
          (should-not (cadr exc)))))
     (pytest-pdb-break-test-with-python-buffer
      (insert source-three)
      (write-file "s3.py")
      (ert-info ("Nested: Test / test / test") ; upcase T means cls
        (should ($get-there "return 1"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "TestFoo" "test_foo"))))
      (ert-info ("Nested: Test / test /non")
        (should ($get-there "return 2"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "TestFoo" "test_bar"))))
      (ert-info ("Nested: Test / non / test")
        (should ($get-there "return 3"))
        (let ((exc (should-error (pytest-pdb-break--get-node-id))))
          (should (equal (car exc) 'pytest-pdb-break-test-not-found))
          (should (equal (cadr exc) "TestFoo.not_test_baz.test_f")))))
     (pytest-pdb-break-test-with-python-buffer
      (insert (replace-regexp-in-string
               "(self)" "()"
               (replace-regexp-in-string "class TestFoo"
                                         "def test_foo_top()"
                                         source-three)))
      (write-file "s4.py")
      (ert-info ("Nested: test / test / test")
        (should ($get-there "return 1"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_foo_top"))))
      (ert-info ("Nested: test / test / non")
        (should ($get-there "return 2"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_foo_top"))))
      (ert-info ("Nested: test / non / test")
        (should ($get-there "return 3"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_foo_top")))))
     (pytest-pdb-break-test-with-python-buffer
      (insert (replace-regexp-in-string
               "(self)" "()"
               (replace-regexp-in-string "^    \\|class TestFoo.*$"
                                         "" source-three)))
      (write-file "s5.py")
      (ert-info ("Nested: test / test")
        (should ($get-there "return 1"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_foo"))))
      (ert-info ("Nested: test / non")
        (should ($get-there "return 2"))
        (should (equal (pytest-pdb-break--get-node-id)
                       (list buffer-file-name "test_bar"))))
      (ert-info ("Nested: non / test")
        (should ($get-there "return 3"))
        (let ((exc (should-error (pytest-pdb-break--get-node-id))))
          (should (equal (car exc) 'pytest-pdb-break-test-not-found))
          (should (equal (cadr exc) "not_test_baz.test_f"))))))))

(ert-deftest pytest-pdb-break-test-get-args ()
  ;; Eval: (compile "make PAT=get-args")
  (let ((parts (list "/tmp/a.py" "test_a"))
        (session '("-p" "my_plugin")))
    (ert-info ("No extras")
      (should-not pytest-pdb-break-extra-opts)
      (should (equal (pytest-pdb-break--get-args nil 9 parts)
                     '("--break=/tmp/a.py:9" "/tmp/a.py::test_a"))))
    (ert-info ("Session, no extras")
      (should-not pytest-pdb-break-extra-opts)
      (should (equal (pytest-pdb-break--get-args session 9 parts)
                     '("-p" "my_plugin"
                       "--break=/tmp/a.py:9" "/tmp/a.py::test_a"))))
    (ert-info ("Extras")
      (let ((pytest-pdb-break-extra-opts '("-p" "no:foo")))
        (should (equal (pytest-pdb-break--get-args nil 9 parts)
                       '("-p" "no:foo"
                         "--break=/tmp/a.py:9"
                         "/tmp/a.py::test_a")))
        (should (equal pytest-pdb-break-extra-opts '("-p" "no:foo")))))
    (ert-info ("Session, extras")
      (let ((pytest-pdb-break-extra-opts '("-p" "no:foo")))
        (should (equal (pytest-pdb-break--get-args session 9 parts)
                       '("-p" "no:foo" "-p" "my_plugin"
                         "--break=/tmp/a.py:9"
                         "/tmp/a.py::test_a")))
        (should (equal pytest-pdb-break-extra-opts '("-p" "no:foo")))))
    (ert-info ("Cons form")
      (should (equal (pytest-pdb-break--get-args nil '("/tmp/b.py" . 9) parts)
                     '("--break=/tmp/b.py:9" "/tmp/a.py::test_a"))))))

(ert-deftest pytest-pdb-break-test-get-modified-setup-code ()
  ;; Eval: (compile "make PAT=get-modified-setup-code")
  (setq pytest-pdb-break--setup-code-addendum nil)
  (pytest-pdb-break-test-with-tmpdir
   (let ((orig-home pytest-pdb-break--py-home))
     (ert-info ("Relies on homer, source file must exist")
       (let* ((pytest-pdb-break--py-home default-directory)
              (exc (should-error (pytest-pdb-break--get-modified-setup-code))))
         (should (member (car exc) '(file-missing file-error)))))
     ;; No condition-case handler for resetting to nil
     (should (equal orig-home pytest-pdb-break--py-home))
     (should-not pytest-pdb-break--setup-code-addendum))
   (ert-info ("Ordering of source snippets")
     (with-temp-file "joined.out"
       (insert (pytest-pdb-break--get-modified-setup-code))
       (goto-char (point-min))
       (ert-info ("Orig first, source second, call-snippet last")
         (should (search-forward python-shell-completion-setup-code nil t))
         (should (search-forward pytest-pdb-break--setup-code-addendum nil t))
         (backward-char (length pytest-pdb-break--setup-code-reassignment))
         (should (looking-at-p (regexp-quote
                                pytest-pdb-break--setup-code-reassignment))))))))

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

(ert-deftest pytest-pdb-break-test-maybe-get-parent-buffer ()
  ;; Eval: (compile "make PAT=maybe-get-parent-buffer")
  (pytest-pdb-break-test-with-python-buffer
   (should (string= (rename-buffer "sample.py") "sample.py"))
   (let ((parbuf (current-buffer)))
     (ert-info ("Our proc name")
       (with-temp-buffer
         (should (string= (rename-buffer "*pytest-PDB[sample.py]*")
                          "*pytest-PDB[sample.py]*"))
         (should (eq parbuf (pytest-pdb-break--maybe-get-parent-buffer)))))
     (ert-info ("Default proc name")
       (should (string= python-shell-buffer-name "Python"))
       (with-temp-buffer
         (should (string= (rename-buffer "*Python[sample.py]*")
                          "*Python[sample.py]*"))
         (should (eq parbuf (pytest-pdb-break--maybe-get-parent-buffer)))))))
  (with-temp-buffer
    (should (string= (rename-buffer "sample.txt") "sample.txt"))
    (should (eq major-mode 'fundamental-mode))
    (ert-info ("Parent is wrong mode")
      (with-temp-buffer
        (should (string= (rename-buffer "*pytest-PDB[sample.txt]*")
                         "*pytest-PDB[sample.txt]*"))
        (should-not (pytest-pdb-break--maybe-get-parent-buffer))))))

(ert-deftest pytest-pdb-break-test-set-shell-buffer-name ()
  ;; Eval: (compile "make PAT=set-shell-buffer-name")
  (should-not pytest-pdb-break--existing-python-shell-buffer-name)
  (ert-info ("No existing")
    (with-temp-buffer
      (let ((parbuf (current-buffer)))
        (with-temp-buffer ; currently unnecessary
          (pytest-pdb-break--set-shell-buffer-name parbuf)))
      (should (string= python-shell-buffer-name "pytest-PDB"))
      (should (local-variable-p 'python-shell-buffer-name))))
  (ert-info ("Existing, but ours")
    (should-not pytest-pdb-break--existing-python-shell-buffer-name)
    (pytest-pdb-break-test-with-python-buffer
     (setq-local python-shell-buffer-name "pytest-PDB")
     (with-temp-buffer
       (should (string-empty-p
                (pytest-pdb-break-test--with-messages
                 (pytest-pdb-break--set-shell-buffer-name
                  (current-buffer)))))
       (should (string= python-shell-buffer-name "pytest-PDB"))
       (should (local-variable-p 'python-shell-buffer-name)))))
  (ert-info ("Existing")
    (should-not pytest-pdb-break--existing-python-shell-buffer-name)
    (with-temp-buffer
      (setq-local python-shell-buffer-name "Foo")
      (should (string-match-p
               "Moving existing"
               (pytest-pdb-break-test--with-messages
                (pytest-pdb-break--set-shell-buffer-name (current-buffer)))))
      (should (local-variable-p 'python-shell-buffer-name))
      (should (local-variable-p
               'pytest-pdb-break--existing-python-shell-buffer-name))
      (should (string= python-shell-buffer-name "pytest-PDB"))
      (should (string= pytest-pdb-break--existing-python-shell-buffer-name
                       "Foo")))))

(ert-deftest pytest-pdb-break-test-kill-shell-buffer-name ()
  ;; Eval: (compile "make PAT=kill-shell-buffer-name")
  (ert-info ("No stashed (no existing)")
    (with-temp-buffer
      (ert-info ("Mismatch")
        (setq-local python-shell-buffer-name "Foo")
        (pytest-pdb-break--kill-shell-buffer-name (current-buffer))
        (should (string= python-shell-buffer-name "Foo"))
        (should (local-variable-p 'python-shell-buffer-name)))
      (ert-info ("Killed")
        (setq python-shell-buffer-name pytest-pdb-break--proc-base-name)
        (pytest-pdb-break--kill-shell-buffer-name (current-buffer))
        (should-not (local-variable-p 'python-shell-buffer-name))
        (should-not (string= python-shell-buffer-name
                             pytest-pdb-break--proc-base-name)))))
  (ert-info ("Restore from stashed")
    (with-temp-buffer
      (setq pytest-pdb-break--existing-python-shell-buffer-name "Foo")
      (should (local-variable-p
               'pytest-pdb-break--existing-python-shell-buffer-name))
      (setq-local python-shell-buffer-name pytest-pdb-break--proc-base-name)
      (pytest-pdb-break--kill-shell-buffer-name (current-buffer))
      (should (string= python-shell-buffer-name "Foo"))
      (should (local-variable-p 'python-shell-buffer-name))
      (should-not pytest-pdb-break--existing-python-shell-buffer-name)
      (should-not (local-variable-p
                   'pytest-pdb-break--existing-python-shell-buffer-name)))))

(ert-deftest pytest-pdb-break-test-minor-mode ()
  ;; Eval: (compile "make PAT=minor-mode")
  (setq pytest-pdb-break--setup-code-addendum nil)
  (should-not pytest-pdb-break-processes)
  (should-not (buffer-live-p nil)) ; reminder (no error)
  (should-not (process-live-p nil))
  (cl-macrolet ((inside (&rest rest) `(ert-info ("Inside") ,@rest))
                (outside (&rest rest) `(ert-info ("Outside") ,@rest))
                (rip (x &optional y &key (args '("sleep" "60")))
                     `(pytest-pdb-break-test-with-environment
                       (should-not pytest-pdb-break-processes)
                       (should-not pytest-pdb-break--process)
                       (let (proc)
                         (with-temp-buffer
                           (setq proc (start-process ,(car args)
                                                     (current-buffer) ,@args))
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
                   ;; Call to get-buffer-process may return #<killed-buffer>
                   (let ((exc (should-error (pytest-pdb-break-mode +1))))
                     (should (string-match-p "No live process" (cadr exc)))))
           (outside (should-not pytest-pdb-break-processes))))
    (ert-info ("Normal, no *--process var")
      (rip (inside (pytest-pdb-break-mode +1)
                   (should (memq proc pytest-pdb-break-processes))
                   (should (codechk))
                   (should (local-variable-p 'kill-buffer-hook))
                   (should-not (eq t (car kill-buffer-hook))))
           (outside (should-not (codechk))
                    (should-not pytest-pdb-break-processes))))
    (ert-info ("Normal, with *--process var")
      (rip (inside (setq pytest-pdb-break--process proc)
                   (should (local-variable-p 'pytest-pdb-break--process))
                   (pytest-pdb-break-mode +1))
           (outside (should-not pytest-pdb-break-processes))))
    (ert-info ("Deactivation behavior")
      (pytest-pdb-break-test-with-tmpdir
       (let ((pyexe (pytest-pdb-break-test--get-pyexe 'base))
             (s1 (start-process "s1" (current-buffer) "sleep" "30")))
         (set-process-query-on-exit-flag s1 nil)
         (rip (inside (defvar python-shell--interpreter)
                      (defvar python-shell--interpreter-args)
                      (let (python-shell--interpreter
                            python-shell--interpreter-args)
                        (inferior-python-mode))
                      (setq pytest-pdb-break--process proc) ; else most recent
                      (setq pytest-pdb-break--parent-buffer
                            (get-buffer-create "dummy-parent.py"))
                      (pytest-pdb-break-mode +1)
                      (should (eq pytest-pdb-break--process proc))
                      (let ((t1 (start-process "t1" (current-buffer) "true")))
                        (nconc pytest-pdb-break-processes (list s1 t1))
                        (while (process-live-p t1) (sleep-for 0.001))
                        (should (seq-set-equal-p pytest-pdb-break-processes
                                                 (list proc s1 t1))))
                      (pytest-pdb-break-mode -1)
                      ;; buffer-local vars removed
                      (should-not (local-variable-p
                                   'python-shell-completion-native-enable))
                      (should-not (codechk))
                      (should-not pytest-pdb-break--process)
                      (should-not (local-variable-p
                                   'pytest-pdb-break--process))
                      (should-not pytest-pdb-break--parent-buffer)
                      (should-not (local-variable-p
                                   'pytest-pdb-break--parent-buffer))
                      (should (equal pytest-pdb-break-processes (list s1))))
              (outside (kill-process s1)
                       (should-not pytest-pdb-break--process)
                       (pytest-pdb-break-mode -1)
                       (should-not (codechk))
                       (should-not (local-variable-p 'kill-buffer-hook))
                       (let ((b (get-buffer "dummy-parent.py")))
                         (should (buffer-live-p b))
                         (kill-buffer b)))
              :args (pyexe "-c" "import pdb; pdb.set_trace()")))))))

(ert-deftest pytest-pdb-break-test-interpret-prefix-arg ()
  ;; Eval: (compile "make PAT=interpret-prefix-arg")
  (ert-info ("Nothing")
    (should-not (pytest-pdb-break--interpret-prefix-arg nil)))
  (ert-info ("Digit arg")
    (should (= (pytest-pdb-break--interpret-prefix-arg 0) 0))
    (should (= (pytest-pdb-break--interpret-prefix-arg 4) 4)))
  (ert-info ("Universal arg")
    (should (= (pytest-pdb-break--interpret-prefix-arg '(4)) 4))
    (should (= (pytest-pdb-break--interpret-prefix-arg '(16)) 16)))
  (ert-info ("Negative arg")
    (should (= (pytest-pdb-break--interpret-prefix-arg '-) -1))
    (should (= (pytest-pdb-break--interpret-prefix-arg '-1) -1))))

(defun pytest-pdb-break-test--timeout (func &optional max-secs)
  "Dumb waiter.  Wait MAX-SECS for FUNC to return non-nil."
  (let ((st (float-time))
        (max-secs (or max-secs 5))
        found)
    (while (and (< (- (float-time) st) max-secs)
                (not (setq found (funcall func))))
      (sleep-for 0.01))
    found))

(defun pytest-pdb-break-test--expect-simple (pattern)
  "Wait 5 secs for PATTERN before point."
  (pytest-pdb-break-test--timeout (lambda () (looking-back pattern nil))))

(defun pytest-pdb-break-test--expect-last (proc pattern)
  "Search forward for PATTERN in most recent output from PROC.
Use \\' for end of string, and $ for end of line."
  (or (pytest-pdb-break-test--timeout
       (lambda nil
         (string-match-p pattern
                         (buffer-substring-no-properties
                          comint-last-output-start (process-mark proc)))))
      (error "\nNeedle: %s\nHaystack: %s" pattern
             (buffer-substring-no-properties comint-last-output-start
                                             (process-mark proc)))))

(defun pytest-pdb-break-test--expect-marker (proc mkr pattern)
  "Wait for PATTERN between MKR and PROC's `process-mark'."
  (pytest-pdb-break-test--timeout
   (lambda ()
     (when (save-excursion
             (and (goto-char mkr)
                  (re-search-forward pattern (process-mark proc) t)))
       (set-marker mkr (match-end 0) (process-buffer proc))))))

(defmacro pytest-pdb-break-test--with-comint-expect (&rest body)
  "Bind some expect helpers around BODY in a comint buffer.
`$expect' -> `pytest-pdb-break-test--expect-marker'
`$pleted' -> `pytest-pdb-break-test--expect-simple'."
  `(let ((ex-mark (set-marker (make-marker) (point-min) (current-buffer)))
         (proc (or pytest-pdb-break--process
                   (get-buffer-process (current-buffer)))))
     (cl-macrolet
         (($expect (pat) `(should (pytest-pdb-break-test--expect-marker
                                   proc ex-mark ,pat)))
          ($pleted (pat) `(should (pytest-pdb-break-test--expect-simple
                                   ,pat))))
       ,@body)))

(ert-deftest pytest-pdb-break-test-read-session-options ()
  ;; Eval: (compile "make PAT=read-session-options")
  ;; Eval: (compile "make debug PAT=read-session-options")
  (pytest-pdb-break-test-with-tmpdir
   (pytest-pdb-break-test-with-environment
    (with-temp-file "somefile.suffix" (insert "text"))
    (with-temp-file "notherfile.suffix" (insert "more text"))
    (add-hook 'term-mode-hook (lambda () (term-reset-size 20 80)))
    (let* ((logfile (file-truename "ert.out")) ; must be bound here
           ;; When called interactively, CWD reverts to this project's lisp dir
           (src `(progn (require 'pytest-pdb-break)
                        (with-current-buffer (messages-buffer)
                          (setq default-directory ,default-directory))))
           (args (list "-Q" "-nw" "-L" pytest-pdb-break-test-lisp-root
                       "--eval" (format "%S" src)))
           (buf (apply #'make-term "ert-term" "emacs" nil args))
           (proc (get-buffer-process buf)))
      (with-current-buffer buf
        (unwind-protect
            (ert-info ("Term subprocess")
              (term-char-mode)
              (goto-char (process-mark proc))
              (term-send-raw-string "\e:")
              (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
              (term-send-string proc "(switch-to-buffer (messages-buffer))\n")
              (term-send-raw-string "\e>")
              (ert-info ("Completion of file in current directory")
                (term-send-raw-string "\e:")
                (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
                (term-send-string
                 proc "(pytest-pdb-break--read-session-options)\n")
                (should (pytest-pdb-break-test--expect-simple "options: ?"))
                (term-send-string proc "--foo=./some\t")
                (should (pytest-pdb-break-test--expect-simple
                         "file\\.suffix ?"))
                (term-send-string proc "nothe\t")
                (should (pytest-pdb-break-test--expect-simple
                         "notherfile\\.suffix ?"))
                (term-send-string proc "\n"))
              (ert-info ("Entry added to history")
                (term-send-raw-string "\e:")
                (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
                (term-send-string proc "pytest-pdb-break--options-history\n")
                (should (pytest-pdb-break-test--expect-simple
                         (concat  "(\"--foo=\\./somefile\\.suffix ?"
                                  "notherfile\\.suffix ?\").*"))))
              (ert-info ("Entry appears as initial input")
                (term-send-raw-string "\e:")
                (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
                (term-send-string
                 proc "(pytest-pdb-break--read-session-options)\n")
                (should (pytest-pdb-break-test--expect-simple
                         "options: .*\\.suffix ?"))
                (term-send-raw-string "\C-a\C-k")
                (should (pytest-pdb-break-test--expect-simple "options: ?"))
                (term-send-string proc "\n"))
              (ert-info ("Empty string added to history")
                (term-send-raw-string "\e:")
                (should (pytest-pdb-break-test--expect-simple "Eval: ?"))
                (term-send-string proc "pytest-pdb-break--options-history\n")
                (should (pytest-pdb-break-test--expect-simple
                         (concat  "(\"\" \"--foo=\\./somefile\\.suffix ?"
                                  "notherfile\\.suffix ?\").*"))))
              (term-send-raw-string "\C-x\C-c")
              (should (pytest-pdb-break-test--timeout
                       (lambda nil (not (process-live-p proc))))))
          (unless (pytest-pdb-break-test-invoked-with-debug-p)
            (write-file logfile)
            (kill-buffer buf))))))))

(ert-deftest pytest-pdb-break-test-default-options-function ()
  ;; Eval: (compile "make PAT=default-options-function")
  (let ((called 0)
        pytest-pdb-break--options-history)
    (ert-info ("Null hist, null arg")
      (should-not (pytest-pdb-break-default-options-function nil))
      (should-not pytest-pdb-break--options-history))
    (ert-info ("Non-null hist, null arg")
      (push "--foo" pytest-pdb-break--options-history)
      (should (equal (pytest-pdb-break-default-options-function nil)
                     '("--foo")))
      (should (equal '("--foo") pytest-pdb-break--options-history)))
    (ert-info ("Negative arg")
      (push "--bar" pytest-pdb-break--options-history)
      (should (equal (pytest-pdb-break-default-options-function -1)
                     '("--foo")))
      (push "--baz" pytest-pdb-break--options-history)
      (should (equal (pytest-pdb-break-default-options-function -2)
                     '("--foo")))
      (ert-info ("Overshoot")
        (should (equal (pytest-pdb-break-default-options-function -10)
                       '("--foo")))))
    (ert-info ("Zero arg")
      (should-not (pytest-pdb-break-default-options-function 0))
      (should-not (pytest-pdb-break-default-options-function nil))
      (should (string= "" (car pytest-pdb-break--options-history)))
      (should-not (string= "" (cadr pytest-pdb-break--options-history))))
    ;; This is only meant to show that history may be modified somehow
    (advice-add 'pytest-pdb-break--read-session-options
                :override (lambda (&rest r)
                            (should-not r)
                            (push (format "%S" (setq called (1+ called)))
                                  pytest-pdb-break--options-history)
                            (car pytest-pdb-break--options-history))
                '((name . over)))
    (should (advice-member-p 'over 'pytest-pdb-break--read-session-options))
    (ert-info ("Positive arg")
      (should (equal (pytest-pdb-break-default-options-function 4) '("1")))
      (should (equal (pytest-pdb-break-default-options-function 16) '("2")))
      (should (equal (pytest-pdb-break-default-options-function nil) '("2"))))
    (advice-remove 'pytest-pdb-break--read-session-options 'over)
    (should-not (advice-member-p 'over
                                 'pytest-pdb-break--read-session-options)))
  (should-not pytest-pdb-break--options-history))

(defmacro pytest-pdb-break-test--inferior-python-setup-fixture (&rest body)
  "Run BODY with common setup."
  `(pytest-pdb-break-test-ensure-venv
    'base
    (let ((sample-source (nth 1 pytest-pdb-break-test--get-node-id-sources))
          (python-shell-virtualenv-root $venv)
          (python-shell-interpreter $pyexe)
          (pytest-pdb-break--setup-code-addendum nil)
          (pytest-pdb-break-processes (append pytest-pdb-break-processes nil))
          ($debuggin? (pytest-pdb-break-test-invoked-with-debug-p))
          (start-dir default-directory)
          (inhibit-message t)
          case-fold-search)
      (pytest-pdb-break-test-with-python-buffer
       ,@body))))

(defmacro pytest-pdb-break--main-command-fixture (linepat &rest body)
  "Run main command assertions in BODY from LINEPAT."
  `(pytest-pdb-break-test--inferior-python-setup-fixture
    (insert sample-source)
    (write-file "test_class.py")
    (should ($get-there ,linepat))
    (call-interactively 'pytest-pdb-break-here)
    (should (get-buffer "*pytest-PDB[test_class.py]*"))
    (with-current-buffer "*pytest-PDB[test_class.py]*"
      (advice-add 'ding :override (lambda (&rest _r) (ignore))
                  '((name . neuter)))
      (unwind-protect
          (ert-info ("In pytest-PDB buffer")
            (should (eq major-mode 'inferior-python-mode))
            (should pytest-pdb-break-mode)
            (should pytest-pdb-break--process)
            (should (local-variable-p 'pytest-pdb-break--parent-buffer))
            (should (eq pytest-pdb-break--parent-buffer
                        (get-buffer "test_class.py")))
            (should (pytest-pdb-break-test--expect-simple "(Pdb) "))
            ,@body
            (should-not (get-buffer "*Warnings*")))
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
          (kill-buffer)
          (let ((cb (get-buffer "*Completions*")))
            (when cb (kill-buffer cb))))))))

;; Note: none of the following cases covers `pytest-pdb-break-alt-installation'
;; because its implementation is trivial

(ert-deftest pytest-pdb-break-test-main-command-min-version ()
  ;; Eval: (compile "make PAT=main-command-min-version")
  (skip-unless pytest-pdb-break-test-skip-plugin)
  (let ((exc (should-error (pytest-pdb-break--main-command-fixture
                            "assert True" (ignore)))))
    (should (string-match-p "Python version.*less than.*" (cadr exc)))))

(ert-deftest pytest-pdb-break-test-main-command-basic ()
  ;; Eval: (compile "make PAT=main-command-basic")
  (skip-unless (null pytest-pdb-break-test-skip-plugin))
  (pytest-pdb-break--main-command-fixture
   "assert True"
   (ert-info ("Break in first method")
     (should (save-excursion  ; already at prompt, so must search back
               (search-backward-regexp ">.*\\.py(4)test_foo()$" nil t))))
   (comint-send-string pytest-pdb-break--process "c\n")
   (should (pytest-pdb-break-test--expect-simple "finished\n.*"))))

(ert-deftest pytest-pdb-break-test-main-command-send-string ()
  "Test compatibility of `python.el''s string-sending functions."
  ;; Eval: (compile "make PAT=main-command-send-string")
  ;; Eval: (compile "make debug PAT=main-command-send-string")
  (skip-unless (null pytest-pdb-break-test-skip-plugin))
  ;; XXX prompt is not filtered and appears before output; but when
  ;; interacting with a live Emacs instance, this doesn't happen
  (pytest-pdb-break--main-command-fixture
   "assert True"
   (cl-flet (($expect (pat) (pytest-pdb-break-test--expect-last
                             pytest-pdb-break--process pat)))
     (ert-info ("Break in first method")
       (should (save-excursion  ; already at prompt, so must search back
                 (search-backward-regexp ">.*\\.py(4)test_foo()$" nil t))))
     (ert-info ("Send string noninteractively (no minibuffer)")
       (with-current-buffer pytest-pdb-break--parent-buffer
         (python-shell-send-string "myvar = 21 * 2"))
       (ert-info ("Output is not echoed") ; same for all that follows
         (should-not (save-excursion
                       (search-backward-regexp "myvar" (point-min) t))))
       (execute-kbd-macro "myvar\r")
       (should ($expect "42$"))
       (should ($expect "(Pdb) \\'")))
     (ert-info ("Send defun")
       (with-current-buffer pytest-pdb-break--parent-buffer
         (goto-char (point-max))
         (insert "\ndef some_func(x):\n    return x * 2")
         (save-buffer)
         (should ($get-there "return x * 2"))
         (call-interactively #'python-shell-send-defun))
       (should ($expect "(Pdb) \\'"))
       (execute-kbd-macro "some_func(myvar)\r")
       (should ($expect "84$")))
     (ert-info ("Interactive")
       (execute-kbd-macro "interact\r")
       (should ($expect ">>> \\'"))
       (with-current-buffer pytest-pdb-break--parent-buffer
         (goto-char (point-max))
         (insert "\ndef nother_func(x):\n    return x * 3")
         (save-buffer)
         (should ($get-there "return x * 3"))
         (call-interactively #'python-shell-send-defun))
       (should ($expect ">>> \\'"))
       (execute-kbd-macro "nother_func(myvar)\r")
       (should ($expect "126$"))
       (execute-kbd-macro [4]))
     (should ($expect "(Pdb) \\'"))
     (comint-send-string pytest-pdb-break--process "c\n")
     (unless $debuggin?
       (should (pytest-pdb-break-test--expect-simple "finished\n.*"))))))

(defmacro pytest-pdb-break-test--completion-playlist ()
  "Completion procedure for command loop and interactive REPL.
PROC must be a comint-derived process.  For use with source #2 in
TestFoo.test_bar(), starting at line 9."
  '(pytest-pdb-break-test--with-comint-expect
    (ert-info ("Local variable, single result autocompletes")
      (execute-kbd-macro "somev\t")
      (should-not (get-buffer "*Completions*"))
      ;; XXX would think the kbd-macro and looking-back call might race, but
      ;; guess they don't (i.e., seems no waiter needed)
      ($pleted "somevar")
      (execute-kbd-macro [13])
      ($expect "^True$")
      ($expect "(Pdb) \\'"))
    (ert-info ("Attr in command loop")
      (execute-kbd-macro "import sys\r")
      ($expect "(Pdb) \\'")
      (execute-kbd-macro "sys.\t")
      (should (get-buffer "*Completions*"))
      (with-current-buffer "*Completions*"
        (save-excursion (goto-char (point-min))
                        (should (search-forward "sys.path"))
                        (should (search-forward "sys.version"))))
      (execute-kbd-macro "version_info.maj\t")
      ($pleted "sys\\.version_info\\.major")
      (execute-kbd-macro [13])
      ($expect "^3$")
      ($expect "(Pdb) \\'"))
    (ert-info ("PDB command, interactive REPL")
      (execute-kbd-macro "inter\t")
      ($pleted "interact")
      (execute-kbd-macro [13])
      ($expect "[*]interactive[*]$")
      ($expect ">>> \\'")
      (ert-info ("No commands in interactive")
        (execute-kbd-macro "inter\t")
        ($pleted "inter")
        (execute-kbd-macro "act\r")
        ($expect "NameError")
        ($expect ">>> \\'"))
      (ert-info ("Locals from test available")
        (execute-kbd-macro "somev\t")
        ($pleted "somevar")
        (execute-kbd-macro [13])
        ($expect ">>> \\'"))
      (ert-info ("New locals")
        (execute-kbd-macro "import pathlib\r")
        ($expect ">>> \\'")
        (execute-kbd-macro "pathlib.\t")
        (with-current-buffer "*Completions*"
          (save-excursion (goto-char (point-min))
                          (should (search-forward "pathlib.Path"))))
        (execute-kbd-macro "Path\r")
        ($expect ">>> \\'"))
      (execute-kbd-macro [4]))
    (ert-info ("Interactive locals removed")
      ($expect "(Pdb) \\'")
      (execute-kbd-macro "pathlib\r")
      ($expect "NameError")
      ($expect "(Pdb) \\'"))
    (ert-info ("Command completion still works")
      (execute-kbd-macro "whe\t")
      ($pleted "where")
      (execute-kbd-macro [13])
      ($expect ">.*\\.py(9)test_bar()$")
      ($expect "(Pdb) \\'"))
    (execute-kbd-macro "c\r")))

(ert-deftest pytest-pdb-break-test-main-command-completion ()
  ;; Eval: (compile "make PAT=main-command-completion")
  ;; Eval: (compile "make debug PAT=main-command-completion")
  (skip-unless (null pytest-pdb-break-test-skip-plugin))
  (pytest-pdb-break--main-command-fixture
   "# comment"
   (should (member 'python-shell-completion-at-point
                   completion-at-point-functions))
   (ert-info ("Break in second method")
     ;; Already waiting, so last-output-start has been cleared
     (should (save-excursion (search-backward-regexp ">.*\\.py(9)test_bar()$"
                                                     nil t))))
   (pytest-pdb-break-test--completion-playlist)
   (should (pytest-pdb-break-test--expect-simple
            (if $debuggin? "passed.*\n?" "finished.*\n?")))))

(ert-deftest pytest-pdb-break-test-completion-compat ()
  "This is for testing against Python 3.5.
The main pytest plugin requires 3.6+, but the minor-mode shouldn't."
  ;; Eval: (compile "make PAT=completion-compat")
  ;; Eval: (compile "make debug PAT=completion-compat")
  (pytest-pdb-break-test--inferior-python-setup-fixture
   ;; No -m option in pdb.main <3.7, i.e., can't just -m this
   (insert sample-source "\n\nTestFoo().test_bar()")
   (write-file "test_one.py")
   (with-temp-file ".pdbrc"
     (insert "b 9\nc\n"))
   (let* ((buf (compile (format "%s -mpdb test_one.py" $pyexe)))
          ($proc (get-buffer-process buf)))
     (with-current-buffer buf
       (setq-local python-shell-interpreter $pyexe)
       (add-hook 'compilation-filter-hook
                 'pytest-pdb-break--run-fail-compilation-filter nil t)
       (advice-add 'ding :override (lambda (&rest _r) (ignore))
                   '((name . neuter)))
       (unwind-protect
           (ert-info ("Main")
             (should (pytest-pdb-break-test--timeout
                      (lambda nil (and pytest-pdb-break-mode
                                       (looking-back "(Pdb) " nil)))))
             (switch-to-buffer buf)
             (pytest-pdb-break-test--completion-playlist)
             (execute-kbd-macro "quit\r")
             (should (pytest-pdb-break-test--timeout
                      (lambda nil (not (process-live-p $proc))))))
         (unless $debuggin?
           (advice-remove 'ding 'neuter)
           (setq default-directory start-dir)
           (write-file "capped.out")
           (when (process-live-p pytest-pdb-break--process)
             (set-process-query-on-exit-flag pytest-pdb-break--process nil)
             (kill-process pytest-pdb-break--process))
           (kill-buffer)
           (let ((cb (get-buffer "*Completions*")))
             (when cb (kill-buffer cb)))))))))

(ert-deftest pytest-pdb-break-test-run-fail-compilation-filter ()
  ;; Eval: (compile "make PAT=run-fail-compilation-filter")
  ;; Eval: (compile "make debug PAT=run-fail-compilation-filter")
  (pytest-pdb-break-test-with-tmpdir
   (let ((inhibit-message t))
     (with-current-buffer
         (compile (format "%s -c 'import pdb; pdb.set_trace()'"
                          (pytest-pdb-break-test--get-pyexe 'base)))
       (unwind-protect
           (let ((proc (get-buffer-process (current-buffer))))
             (setq-local
              pytest-pdb-break--prompt-watcher-function
              (lambda (p)
                (should-not
                 (member 'pytest-pdb-break--run-fail-compilation-filter
                         compilation-filter-hook))
                (process-send-string p "help\n")))
             (add-hook 'compilation-filter-hook
                       'pytest-pdb-break--run-fail-compilation-filter nil t)
             (should (eq major-mode 'compilation-mode))
             (should (pytest-pdb-break-test--timeout
                      (lambda ()
                        (goto-char (point-min))
                        (search-forward "topic" (point-max) t))))
             (should (pytest-pdb-break-test--timeout
                      (lambda () (goto-char (point-max))
                        (looking-back "(Pdb) " nil))))
             (process-send-string proc "c\n")
             (should (pytest-pdb-break-test--timeout
                      (lambda nil (not (process-live-p proc))))))
         (unless (pytest-pdb-break-test-invoked-with-debug-p)
           (write-file "compilation.out")))))))

(ert-deftest pytest-pdb-break-test-run-fail-comint-process-filter ()
  ;; Eval: (compile "make PAT=run-fail-comint-process-filter")
  ;; Eval: (compile "make debug PAT=run-fail-comint-process-filter")
  (pytest-pdb-break-test-with-tmpdir
   (let* ((compilation-buffer-name-function
           (lambda (m) (format "*test-%s-filter*" m)))
          (inhibit-message t)
          (buf (compile (format "%s -c 'import pdb; pdb.set_trace()'"
                                (pytest-pdb-break-test--get-pyexe 'base))
                        t))
          (proc (get-buffer-process buf)))
     (with-current-buffer buf
       (unwind-protect
           (ert-info ("Comint buffer")
             (setq-local pytest-pdb-break--prompt-watcher-function
                         (lambda (p)
                           (should (eq (process-filter p)
                                       'comint-output-filter))
                           (process-send-string p "help\n")
                           (comint-goto-process-mark)))
             (set-process-filter
              proc 'pytest-pdb-break--run-fail-comint-process-filter)
             (should (eq major-mode 'comint-mode))
             (should (pytest-pdb-break-test--timeout
                      (lambda ()
                        (goto-char (point-min))
                        (search-forward "topic" (point-max) t))))
             (comint-goto-process-mark)
             (should (pytest-pdb-break-test--expect-simple "(Pdb) "))
             (should-not buffer-read-only)
             ;; Current buffer must be displayed for macros to work!
             (switch-to-buffer buf)
             (execute-kbd-macro "c\r")
             (should (pytest-pdb-break-test--timeout
                      (lambda nil (not (process-live-p proc))))))
         (unless (pytest-pdb-break-test-invoked-with-debug-p)
           ;; When proc ends, we're popped back to parbuf
           (with-current-buffer buf
             (write-file "comint.out"))))))))

(defmacro pytest-pdb-break-test-go-inferior-fixture (mode &rest body)
  "Run BODY in MODE."
  `(pytest-pdb-break-test-ensure-venv
    'base
    (let* ((inhibit-message t)
           (python-shell-virtualenv-root $venv)
           (python-shell-interpreter $pyexe)
           (pytest-pdb-break--setup-code-addendum nil)
           (pytest-pdb-break-processes (append pytest-pdb-break-processes nil))
           (buf (compile (format "%s -c 'import pdb; pdb.set_trace()'" $pyexe)
                         ,(eq mode 'comint-mode)))
           ($proc (get-buffer-process buf))
           echoed new-name)
      (with-current-buffer buf
        (should (pytest-pdb-break-test--timeout
                 (lambda () (goto-char (point-max))
                   (looking-back "(Pdb) " nil))))
        (setq-local python-shell-interpreter $pyexe)
        (setq echoed (pytest-pdb-break-test--with-messages
                      (pytest-pdb-break-go-inferior $proc)))
        (should-not (member $proc compilation-in-progress))
        (should (string-match-p "pytest-PDB" (setq new-name (buffer-name))))
        (should (string-match-p (concat "Switching .* to "
                                        (regexp-quote new-name))
                                echoed))
        (should (eq major-mode 'inferior-python-mode))
        (should pytest-pdb-break-mode)
        (ert-info ("No parbuf because run-fail didn't run here")
          (should-not pytest-pdb-break--parent-buffer))
        (should (= (point) (process-mark $proc)))
        ,@body
        (comint-goto-process-mark)
        (execute-kbd-macro "c\r")
        (should (pytest-pdb-break-test--timeout
                 (lambda nil (not (process-live-p $proc)))))))))

(ert-deftest pytest-pdb-break-test-go-inferior ()
  ;; Eval: (compile "make PAT=go-inferior")
  (pytest-pdb-break-test-go-inferior-fixture
   'compilation-mode
   (ignore))
  (pytest-pdb-break-test-go-inferior-fixture
   'comint-mode
   (ignore)))

(ert-deftest pytest-pdb-break-test-run-fail-stay ()
  ;; Eval: (compile "make PAT=run-fail-stay")
  ;; Eval: (compile "make debug PAT=run-fail-stay")
  (pytest-pdb-break-test--inferior-python-setup-fixture
   (insert sample-source)
   (write-file "test_class.py")
   (should ($get-there "assert True"))
   (call-interactively 'pytest-pdb-break-run-fail)
   (with-current-buffer (get-buffer "*compilation*")
     (unwind-protect
         (ert-info ("In run-fail buffer")
           (should (eq pytest-pdb-break--process (get-buffer-process
                                                  (current-buffer))))
           (should-not pytest-pdb-break--parent-buffer)
           (should-not (local-variable-p 'pytest-pdb-break--parent-buffer))
           (should (pytest-pdb-break-test--timeout
                    (lambda nil
                      (not (process-live-p pytest-pdb-break--process)))))
           (goto-char (point-min))
           (should (search-forward "passed"))
           (should-not (get-buffer "*Warnings*")))
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
         (kill-buffer))))))

(ert-deftest pytest-pdb-break-test-run-fail-switch ()
  ;; Eval: (compile "make PAT=run-fail-switch")
  ;; Eval: (compile "make debug PAT=run-fail-switch")
  (pytest-pdb-break-test--inferior-python-setup-fixture
   (insert (replace-regexp-in-string "assert True" "assert False"
                                     sample-source))
   (write-file "test_class.py")
   (should ($get-there "assert False"))
   (call-interactively 'pytest-pdb-break-run-fail)
   (should (pytest-pdb-break-test--timeout
            (lambda nil (get-buffer "*pytest-PDB[test_class.py]*"))))
   (with-current-buffer "*pytest-PDB[test_class.py]*"
     (unwind-protect
         (ert-info ("In run-fail buffer")
           (should (eq pytest-pdb-break--process (get-buffer-process
                                                  (current-buffer))))
           (should (local-variable-p 'pytest-pdb-break--parent-buffer))
           (should (eq pytest-pdb-break--parent-buffer
                       (get-buffer "test_class.py")))
           (should (pytest-pdb-break-test--expect-simple "(Pdb) "))
           (switch-to-buffer (current-buffer))
           (execute-kbd-macro "c\r")
           (should (pytest-pdb-break-test--timeout
                    (lambda nil
                      (not (process-live-p pytest-pdb-break--process)))))
           (should-not (get-buffer "*Warnings*")))
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
         (kill-buffer))))))

;; Note: these are mocked, so actual integrations may still fail

(ert-deftest pytest-pdb-break-test-elpy-shell-get-or-create-process-advice ()
  ;; Eval: (compile "make PAT=elpy-shell-get-or-create-process-advice")
  (should-not (featurep 'elpy-shell))
  (let* ((fake-up (lambda (&rest args) (cons 'called-with args)))
         (f 'pytest-pdb-break--elpy-shell-get-or-create-process-advice))
    (ert-info ("Orig called")
      (should (equal (funcall f fake-up 1 2 3) '(called-with 1 2 3))))
    (ert-info ("Our proc returned")
      (with-temp-buffer
        (rename-buffer "foo")
        (let* ((proc-name (pytest-pdb-break--get-proc-name))
               (child-buf (get-buffer-create (format "*%s*" proc-name)))
               (proc (with-current-buffer child-buf
                       (start-process proc-name (current-buffer)
                                      "sleep" "10"))))
          (should (eq (funcall f fake-up 1 2 3) proc)))))))

(ert-deftest pytest-pdb-break-test-advise-elpy-shell-get-proc ()
  ;; Eval: (compile "make PAT=advise-elpy-shell-get-proc")
  (unless (fboundp 'elpy-shell-get-or-create-process)
    (defun elpy-shell-get-or-create-process
        (&rest _args) (error "Called fake function")))
  (with-temp-buffer
    (let ((elpy-a 'pytest-pdb-break--elpy-shell-get-or-create-process-advice)
          (proc (start-process "pytest-PDB[foo]" (current-buffer)
                               "sleep" "10")))
      (set-process-query-on-exit-flag proc nil)
      (ert-info ("New process")
        (should-not (advice-member-p elpy-a 'elpy-shell-get-or-create-process))
        (pytest-pdb-break-mode +1)
        (pytest-pdb-break-advise-elpy-shell-get-proc)
        (should (advice-member-p elpy-a 'elpy-shell-get-or-create-process)))
      (ert-info ("Advice not removed when procs exist")
        (should pytest-pdb-break-processes)
        (pytest-pdb-break-advise-elpy-shell-get-proc)
        (should (advice-member-p elpy-a 'elpy-shell-get-or-create-process)))
      (ert-info ("Process dead")
        (kill-process proc)
        (while (process-live-p proc) (sleep-for 0.001))
        (pytest-pdb-break-mode -1)
        (pytest-pdb-break-advise-elpy-shell-get-proc)
        (should-not (advice-member-p elpy-a
                                     'elpy-shell-get-or-create-process))))))

(ert-deftest pytest-pdb-break-test-checkdoc ()
  ;; Eval: (compile "make PAT=checkdoc")
  ;; XXX this should always run last
  (skip-unless (= emacs-major-version 26))
  (pytest-pdb-break-test-with-tmpdir
   (let ((tmpdir default-directory)
         (inhibit-message t)
         output)
     (cl-macrolet
         ((checkit (msg file logfile)
                   `(ert-info (,msg)
                      (setq output (pytest-pdb-break-test--with-messages
                                    (checkdoc-file ,file)))
                      (with-temp-file ,logfile
                        (setq default-directory tmpdir)
                        (insert output))
                      ;; Stdout on failure means compilation-mode text
                      ;; properties still work (can jump to error location)
                      (should (string-empty-p output)))))
       (checkit "Run checkdoc on main file"
                pytest-pdb-break-test-lisp-main
                "main.out")
       (checkit "Run checkdoc on extras file"
                pytest-pdb-break-test-lisp-extra
                "extras.out")))))

(provide 'pytest-pdb-break-test)
;;; pytest-pdb-break-test.el ends here
