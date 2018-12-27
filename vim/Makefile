NVIM ?= nvim
VIM ?= vim
PYTEST_PDB_BREAK_TEST_TEMPDIR ?= /tmp/pytest-pdb-break-test
VENVBARE ?= $(PYTEST_PDB_BREAK_TEST_TEMPDIR)/.venv_bare
VENVBASE ?= $(PYTEST_PDB_BREAK_TEST_TEMPDIR)/.venv_base
VENVSELF ?= $(PYTEST_PDB_BREAK_TEST_TEMPDIR)/.venv_self
export PYTEST_PDB_BREAK_TEST_TEMPDIR

ifdef VIRTUAL_ENV
ifeq (x$(VIRTUAL_ENV),$(findstring x$(VIRTUAL_ENV),x$(PATH)))
	VENV_BINDIR = $(shell printf %s $$PATH | cut -d ":" -f 1)
	PATH := $(subst $(VENV_BINDIR):,,$(PATH))
	export PATH
	unexport VIRTUAL_ENV
	unexport PYTHONPATH
endif
endif
.phony: all neovim vim8

all:
	@echo Must specify neovim or vim8
	@exit 1

vim8: | $(VENVBARE) $(VENVBASE) $(VENVSELF)
	$(VIM) -Nnesu test_initrc -S test_pytest_pdb_break.vim

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
