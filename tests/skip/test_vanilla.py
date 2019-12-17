"""
Reminders/surprises/TIL-facts from the standard library relevant to this
project
"""
import os
from pathlib import Path
from subprocess import check_output


def test_lineno(testdir):
    """
    Relevance: ``pytest.Item.location`` line numbers are zero-indexed, but
    Python's are (all?) one-indexed.

    Definitely 1-indexed:

    - ``types.CodeType.co_firstlineno``

    - ``types.FrameType.f_lineno`` (based on ``co_lnotab``)

    - pdb breakpoints

    - linecache

    See docstrings and comments in inspect module.

    """
    testdir.makepyfile(test_foo="""
    def f():                               # <- line 1
        import sys
        return sys._getframe().f_lineno    # <- line 3

    def test_foo():
        assert f.__code__.co_firstlineno == 1
        assert f() == 3
    """)

    result = testdir.runpytest("test_foo.py")
    result.assert_outcomes(passed=1)


def test_popen_env_replaces():
    """subprocess.Popen env dict replaces instead of updates
    """
    out = check_output(["env"], env=dict(SOMEVAR="/tmp/foo"))
    assert out.decode().strip() == "SOMEVAR=/tmp/foo"


def test_popen_envvars_pathlike():
    """subprocess.Popen env dict values can be pathlike objects.
    """
    from unittest.mock import patch
    with patch("os.fsencode", wraps=os.fsencode) as m_of:
        d = dict(SOMEVAR=Path("/tmp/foo"))
        cmd = "bash", "-c", "declare -p SOMEVAR"
        out = check_output(cmd, env=d)
        m_of.assert_any_call(d["SOMEVAR"])
        assert b"/tmp/foo" in out
        assert "Path(" in repr(d["SOMEVAR"])
        assert b"Path(" not in out
        try:
            os.fsencode(...)
        except TypeError:
            pass
