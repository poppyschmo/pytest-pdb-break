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
        addopts = --show-capture stdout
    """)
    if not installed:
        testdir.makeconftest("""
            import sys
            sys.path.insert(0, %r)
            pytest_plugins = "pytest_pdb_break"
        """ % (str(reporoot)))
    return testdir


def extend_conftest(td, rest):
    from textwrap import dedent
    # TD is a testdir intance
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
def testdir_simple_nested_async(testdir_setup):
    # Note: unlike breakpoints, location line numbers are 0 indexed
    testdir_setup.makepyfile("""
        def test_foo():
            import asyncio

            async def inner():
                somevar = True
                assert somevar            # <- line 6
                await asyncio.sleep(0.01)
                spam = 1
                return spam

            loop = asyncio.get_event_loop()
            res = loop.run_until_complete(inner())
            assert res == 1
    """)
    return testdir_setup


def get_mod_pdb():
    try:
        from _pytest.debugging import pdb as mod_pdb
    except ImportError:
        # Upstream: 07f20ccab618fbb3c594601c7135cccaf324f270
        from pytest_pdb_break import _pytest_version
        assert _pytest_version >= (5, 2, 1)
        import pdb as mod_pdb
    return mod_pdb


@pytest.fixture
def fix_defs(request):
    from unittest.mock import Mock
    # XXX this is actually not a mock version of get_fix_names_to_fix_defs()
    # because that one DROPS any funcs not defined in the same file (wanted)
    # whereas this currently keeps them all
    from pytest_pdb_break import _get_func_key
    fixes = {}
    for fix_list in request.session._fixturemanager._arg2fixturedefs.values():
        for fix in fix_list:
            fixes.setdefault(_get_func_key(fix.func), []).append(Mock(fix))
    assert "monkeypatch" in (n for n, l in fixes)
    return fixes
