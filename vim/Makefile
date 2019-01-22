NVIM ?= nvim
VIM ?= vim
PYTEST_PDB_BREAK_TEST_TEMPDIR ?= /tmp/pytest-pdb-break-test
VENVDIR = $(shell python3 ../helpers/main.py ensure_venvdir)
ifeq (,$(findstring $(PYTEST_PDB_BREAK_TEST_TEMPDIR),$(VENVDIR)))
$(error error asking helper for venv home in tempdir)
endif

VENVBARE ?= $(VENVDIR)/bare
VENVBASE ?= $(VENVDIR)/base
VENVSELF ?= $(VENVDIR)/self
export PYTEST_PDB_BREAK_TEST_VENVDIR = $(VENVDIR)
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
	python3 ../helpers/main.py get_pyexe bare >/dev/null

$(VENVBASE): | $(PYTEST_PDB_BREAK_TEST_TEMPDIR)
	python3 ../helpers/main.py get_pyexe base >/dev/null

$(VENVSELF): | $(PYTEST_PDB_BREAK_TEST_TEMPDIR)
	python3 ../helpers/main.py get_pyexe self >/dev/null

$(PYTEST_PDB_BREAK_TEST_TEMPDIR):
	mkdir -p $@
