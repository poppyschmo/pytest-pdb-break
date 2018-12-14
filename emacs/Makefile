PROG ?= emacs
FILE ?= pytest-pdb-break-test.el
DIR ?= .

export PYTEST_PDB_BREAK_TEST_REPO_ROOT = $(realpath ../)

.PHONY: batch debug

batch:
	$(PROG) -Q --batch -L . -l $(FILE) -f ert-run-tests-batch-and-exit

tag:
	[ -n "$(TAG)" ] && \
		$(PROG) -Q --batch -L $(DIR) -l $(FILE) \
		--eval "(ert-run-tests-batch-and-exit '(tag $(TAG)))"

debug:
	$(PROG) -Q -L . -l $(FILE) --eval "(ert-run-tests-interactively t)"
