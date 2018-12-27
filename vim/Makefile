NVIM ?= nvim
PYTEST_PDB_BREAK_TEST_TEMPDIR ?= /tmp/pytest-pdb-break-test
VENVBARE ?= $(PYTEST_PDB_BREAK_TEST_TEMPDIR)/.venv_bare
VENVBASE ?= $(PYTEST_PDB_BREAK_TEST_TEMPDIR)/.venv_base
VENVSELF ?= $(PYTEST_PDB_BREAK_TEST_TEMPDIR)/.venv_self
export PYTEST_PDB_BREAK_TEST_TEMPDIR

.phony: neovim

neovim: | $(VENVBARE) $(VENVBASE) $(VENVSELF)
	$(NVIM) -n -u test_initrc -i NONE -S test_pytest_pdb_break.vim --headless

$(VENVBARE): | $(PYTEST_PDB_BREAK_TEST_TEMPDIR)
	python3 -mvenv $@

$(VENVBASE): | $(PYTEST_PDB_BREAK_TEST_TEMPDIR)
	python3 -mvenv $@
	$@/bin/pip install pytest

$(VENVSELF): | $(PYTEST_PDB_BREAK_TEST_TEMPDIR)
	test -e $(realpath ..)/tox.ini
	python3 -mvenv $@
	$@/bin/pip install $(realpath ..)

$(PYTEST_PDB_BREAK_TEST_TEMPDIR):
	mkdir -p $@
