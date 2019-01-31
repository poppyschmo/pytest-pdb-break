import re
import sys
import pytest
from pathlib import Path
from contextlib import contextmanager

# When True, don't inject this repo's root into the sys.path of testdir
# subprocesses (as is otherwise done via mini conftest)
installed = False

reporoot = Path(__file__).parent.parent
assert (reporoot / "tox.ini").exists()

# Glob pat for installed (versioned) egg-/dist-info metadata dirs
info_fnpat = "pytest_pdb_break-*.*-info"


@contextmanager
def prepended_root():
    sys.path.insert(0, str(reporoot))
    try:
        yield
    finally:
        sys.path.remove(str(reporoot))


try:
    import pytest_pdb_break
except ImportError:
    # Assume local editing is in effect and src should be directly importable
    sys.path.insert(0, str(reporoot))
else:
    if next(Path(pytest_pdb_break.__file__).parent.glob(info_fnpat), None):
        installed = True
        # Since /helpers isn't bundled as a subpackage of the main plugin, it
        # must be imported early on while still hiding everything else under /
        with prepended_root():
            import helpers  # noqa: F401
    else:
        "Repo root must already be in sys.path, likely via -m pytest"


prompt_re = r"\(Pdb[+]*\)\s?"
unansi_pat = re.compile("\x1b\\[[\\d;]+m")


def unansi(byte_string, as_list=True):
    """Remove ANSI escape sequences from pexpect output."""
    out = unansi_pat.sub("", byte_string.decode().strip())  # why strip?
    if as_list:
        return out.split("\r\n")
    out


@pytest.fixture
def testdir_setup(testdir):
    """Maybe add project root to a subtest's ``sys.path`` via conftest.

    This only applies when this project's workdir hasn't been converted
    to ``--editable``/develop mode.
    """
    testdir.makeini("""
        [pytest]
        addopts = --no-print-logs
    """)
    if not installed:
        testdir.makeconftest("""
            import sys
            sys.path.insert(0, %r)
            pytest_plugins = "pytest_pdb_break"
        """ % (str(reporoot)))
    return testdir
