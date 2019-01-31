"""
Either:
    1. Only marginally handy as a reminder
    3. Dependent on some environmental condition
"""
import pytest

from _pytest.pytester import LineMatcher
from conftest import prompt_re, unansi

t2f4 = """
    def test_foo():
        assert True                   # <- line 2
        # comment
        assert False                  # <- line 4
"""


@pytest.mark.parametrize("disabled", [True, False])
def test_print_logs(testdir_setup, disabled):
    import os
    if not any(e in os.environ for e in ("PDBBRK_LOGYAML",
                                         "PDBBRK_LOGFILE")):
        pytest.skip("Logging helper not enabled")
    td = testdir_setup

    # ini turns log capturing OFF via --no-print-logs
    if not disabled:
        td.tmpdir.join("tox.ini").remove()

    td.makepyfile(test_file=t2f4)
    pe = td.spawn_pytest("--break=1")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_file.py(2)test_foo()",
        "->*assert True*"
    ])
    pe.sendline("c")
    rest = unansi(pe.read())

    if disabled:
        assert "Captured log call" not in rest
    else:
        LineMatcher(rest).fnmatch_lines("*Captured log call*")


def test_compat_invoke_same_after_baseline(testdir_setup):
    td = testdir_setup
    td.makepyfile(test_file=t2f4)
    pe = td.spawn_pytest("--pdb")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_file.py(4)test_foo()",
        "->*assert False*"
    ])
    pe.sendline("c")
