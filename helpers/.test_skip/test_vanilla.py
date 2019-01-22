"""
TIL stuff from the standard library
"""


def test_popen_envvars():
    """subprocess.Popen env dict values can be pathlike objects.
    """
    import os
    from pathlib import Path
    from unittest.mock import patch
    from subprocess import check_output
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
