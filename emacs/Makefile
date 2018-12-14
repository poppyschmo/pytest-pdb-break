PROG ?= emacs
FILE ?= pytest-pdb-break-test.el
DIR ?= .

SELECTOR ?= \`(member ,@pytest-pdb-break-test-tests)
# ALL = $(shell grep ert-deftest pytest-pdb-break-test.el | cut -d " " -f 2)
# SELECTOR ?= '(member $(ALL))

export PYTEST_PDB_BREAK_TEST_REPO_ROOT = $(realpath ../)

.PHONY: batch debug

batch:
	$(PROG) -Q --batch -L $(DIR) -l $(FILE) \
		--eval "(ert-run-tests-batch-and-exit $(SELECTOR))"

debug:
	$(PROG) -Q -L $(DIR) -l $(FILE) \
		--eval "(ert-run-tests-interactively $(SELECTOR))"
