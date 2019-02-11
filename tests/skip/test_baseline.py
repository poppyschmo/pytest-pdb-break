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


def test_completion_commands(testdir_setup):
    # Note: \x07 is the BEL char
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            assert True
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(2)test_foo()")
    pe.send("hel\t")
    pe.expect("hel\x07?p")
    pe.send("\n")
    pe.expect("Documented commands")
    pe.expect(prompt_re)
    pe.send("whe\t")
    pe.expect("whe\x07?re")
    pe.send("\n")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(2)test_foo()")
    pe.sendline("c")


def fortify_location_with_parso(filename, line_no):
    """Use parso to find function (and maybe class) name."""
    try:
        import parso
    except ImportError:
        return None
    from pytest_pdb_break import BreakLoc
    root = parso.parse(filename.read_text())
    leaf = root.get_leaf_for_position((line_no, 0))

    def find(node, tipo):
        while node.type != tipo:
            if node is root:
                return None
            node = node.parent
        return node

    func = find(leaf, "funcdef")
    if func is None:
        return None

    cand = func
    while cand and not cand.name.value.startswith("test_"):
        cand = find(cand.parent, "funcdef")
    if cand:
        func = cand

    cls = find(func, "classdef")

    return BreakLoc(file=filename, lnum=line_no, name=None,
                    class_name=cls.name.value if cls else None,
                    func_name=func.name.value,
                    param_id=None)


def test_fortify_location_against_parso(testdir_ast):
    try:
        import parso  # noqa: F401
    except ImportError:
        pytest.skip("Parso not installed")
    fortify_location = fortify_location_with_parso
    from pathlib import Path
    from pytest_pdb_break import BreakLoc
    filename = Path(
        testdir_ast.tmpdir / "test_fortify_location_against_parso.py"
    )
    assert filename.exists()
    rv = fortify_location(filename, 2)
    assert rv.equals(BreakLoc(filename, 2, None,
                              class_name=None, func_name="somefunc"))
    rv = fortify_location(filename, 6)
    assert rv.equals(BreakLoc(filename, 6, None,
                              class_name="C", func_name="f"))
    rv = fortify_location(filename, 13)
    assert rv.equals(BreakLoc(filename, 13, None,
                              class_name="TestClass", func_name="test_foo"))
    rv = fortify_location(filename, 17)
    assert rv is None


def test_compat_pdb_cls_init(testdir_setup):
    # _pytest.debugging.pytest_configure runs before ours does
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            assert True
    """)

    def standin(w, c):
        from _pytest.debugging import pytestPDB
        assert pytestPDB._pluginmanager
        assert pytestPDB._pluginmanager is c.pluginmanager
        print(c.pluginmanager)
        return object()

    from unittest.mock import patch
    with patch("pytest_pdb_break.PdbBreak", wraps=standin):
        result = testdir_setup.runpytest("--capture=no",
                                         "--break=test_file.py:2")
    result.assert_outcomes(passed=1)
    result.stdout.fnmatch_lines("<*PytestPluginManager object at *>")
    outfile = testdir_setup.tmpdir.join("stdout.out")
    outfile.write("\n".join(result.outlines))
