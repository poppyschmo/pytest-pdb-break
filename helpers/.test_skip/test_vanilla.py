"""
Reminders/surprises/TIL-facts from the standard library relevant to this
project
"""
import os
from pathlib import Path
from subprocess import check_output


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
