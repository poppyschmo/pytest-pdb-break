NVIM ?= nvim
PYTEST_PDB_BREAK_TEST_TEMPDIR ?= /tmp/pytest-pdb-break-test/vim
export PYTEST_PDB_BREAK_TEST_TEMPDIR

.phony: neovim

neovim:
	$(NVIM) -n -u test_initrc -i NONE -S test_pytest_pdb_break.vim --headless
