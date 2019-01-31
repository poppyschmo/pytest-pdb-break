"""
Largely superfluous. Occasionally handy as reminders.
"""

from _pytest.pytester import LineMatcher
from conftest import prompt_re, unansi


def test_compat_invoke_same_after_baseline(testdir_setup):
    td = testdir_setup
    td.makepyfile(test_file='''
        def test_foo():
            assert True                   # <- line 2
            # comment
            assert False                  # <- line 4
    ''')
    pe = td.spawn_pytest("--pdb")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_file.py(4)test_foo()",
        "->*assert False*"
    ])
    pe.sendline("c")
