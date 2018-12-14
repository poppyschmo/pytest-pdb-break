PROG ?= emacs
FILE ?= pytest-pdb-break-test.el
DIR ?= .

ifdef PAT
	SELECTOR = "$(PAT)"
else ifdef SYM
	SELECTOR = (quote $(SYM))
else ifdef ALL
	PAT = 's/^\s*(ert-deftest\s*\([^ ]\+\).*$$/\1/'
	MEMBERS != grep 'ert-deftest' pytest-pdb-break-test.el | sed $(PAT)
	SELECTOR = (quote (member $(MEMBERS)))
else
	SELECTOR ?= (backquote (member ,@pytest-pdb-break-test-tests))
endif

export PYTEST_PDB_BREAK_TEST_REPO_ROOT = $(realpath ../)

.PHONY: batch debug dryrun

batch:
	$(PROG) -Q --batch -L $(DIR) -l $(FILE) \
		--eval '(ert-run-tests-batch-and-exit $(SELECTOR))'

debug:
	$(PROG) -Q -L $(DIR) -l $(FILE) \
		--eval '(ert-run-tests-interactively $(SELECTOR))'
