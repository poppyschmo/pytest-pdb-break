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


def extend_conftest(td, rest):
    # TD is a testdir intance
    from textwrap import dedent
    conftest = td.tmpdir.join("conftest.py")
    data = dedent(rest).strip()
    conftest.write(data="\n{}".format(data), mode="a")


@pytest.fixture
def testdir_two_funcs(testdir_setup):
    # Note: unlike breakpoints, location line numbers are 0 indexed
    testdir_setup.makepyfile("""
        def test_true_int():
            # some comment
            somevar = True
            assert isinstance(True, int)   # <- line 4

        def test_false_int():              # <- line 6
            assert isinstance(False, int)
    """)
    return testdir_setup


@pytest.fixture
def testdir_class(testdir_setup):
    testdir_setup.makepyfile("""
    class TestClass:
        class_attr = 1

        def test_one(self):
            '''multi
            line docstring
            '''
            x = "this"                        # line 8
            assert "h" in x

        def test_two(self):
            x = "hello"                       # line 12
            assert hasattr(x, 'check')
    """)
    return testdir_setup


@pytest.fixture
def source_ast():
    from textwrap import dedent
    source = dedent("""
        def somefunc():                # <- line 1
            print("somefunc")

        class C:                       # <- line 4
            def f(self):
                return True

        class TestClass:               # <- line 8
            def test_foo(self):
                somevar = False

                def inner(x):          # <- line 12
                    return not x

                assert inner(somevar)

        SOME_GLOBAL = "test"

        @wrapper.attr                  # <- line 19
        def wrapped():
            print("wrapped")

        if __name__ == "__main__":     # <- line 23
            pass
    """).strip()
    return source


@pytest.fixture
def testdir_ast(testdir_setup, source_ast):
    testdir_setup.makepyfile(source_ast)
    return testdir_setup
