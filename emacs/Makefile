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

ifdef VIRTUAL_ENV
ifeq (x$(VIRTUAL_ENV),$(findstring x$(VIRTUAL_ENV),x$(PATH)))
	VENV_BINDIR = $(shell printf %s $$PATH | cut -d ":" -f 1)
	PATH := $(subst $(VENV_BINDIR):,,$(PATH))
	export PATH
	unexport VIRTUAL_ENV
	unexport PYTHONPATH
endif
endif

export PYTEST_PDB_BREAK_TEST_REPO_ROOT = $(realpath ../)

.PHONY: batch debug

batch:
	$(PROG) -Q --batch -L $(DIR) -l $(FILE) \
		--eval '(ert-run-tests-batch-and-exit $(SELECTOR))'

debug:
	$(PROG) -Q -L $(DIR) -l $(FILE) \
		--eval '(ert-run-tests-interactively $(SELECTOR))'
