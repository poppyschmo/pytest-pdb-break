import pytest

from pathlib import Path
from unittest.mock import patch
from _pytest.pytester import LineMatcher
from pytest_pdb_break import BreakLoc, PdbBreak
from pexpect import EOF  # No importskip

from conftest import prompt_re, unansi


def test_breakloc(request):
    with pytest.raises(TypeError):
        BreakLoc(file="test_loc.py", lnum="1", name="test_loc_1")
    loc = BreakLoc(file="", lnum=1, name="")
    assert loc.file is None
    assert loc.name == ""
    loc = BreakLoc(file=None, lnum=1, name=None)
    assert loc.file is None
    assert loc.name is None
    loc = BreakLoc(file="test_loc.py", lnum=1, name="test_loc_1")
    assert isinstance(loc.file, Path)
    assert not loc.file.is_absolute()
    assert not loc.file.exists()

    # Equality
    assert BreakLoc("fake.py", 1, None) \
        == BreakLoc("fake.py", 1, None, func_name="test_fake")
    assert BreakLoc("fake.py", 1, None, func_name="test_fake") \
        == BreakLoc("fake.py", 1, None, class_name="test_fake")

    # Equals method
    loc = BreakLoc("fake.py", 1, None, func_name="test_fake")
    assert loc.equals(BreakLoc("fake.py", 1, None, func_name="test_fake"))
    assert not loc.equals(BreakLoc("fake.py", 1, None))

    # From arg spec
    assert BreakLoc.from_arg_spec("test_loc.py:1") \
        == BreakLoc(file="test_loc.py", lnum=1, name=None)
    assert BreakLoc.from_arg_spec(":1") \
        == BreakLoc.from_arg_spec("1") \
        == BreakLoc(file=None, lnum=1, name=None)
    assert BreakLoc.from_arg_spec("foo:bar:1") \
        == BreakLoc(file="foo:bar", lnum=1, name=None)
    assert BreakLoc.from_arg_spec("foo:bar::1") \
        == BreakLoc(file="foo:bar:", lnum=1, name=None)
    with pytest.raises(ValueError):
        assert BreakLoc.from_arg_spec("test_loc.py:")
    with pytest.raises(ValueError):
        assert BreakLoc.from_arg_spec("test_loc.py")
    with pytest.raises(ValueError):
        assert BreakLoc.from_arg_spec("")
    with pytest.raises(ValueError):
        assert BreakLoc.from_arg_spec("a:b:c")

    # From pytest item
    with patch("_pytest.nodes.Item") as mI, \
            patch("_pytest.python.Function") as mF:
        item = mF("test_loc_1", parent=mI("some_module"))
        rootdir = request.config.rootdir
        item.config.rootdir = rootdir
        item.fspath = rootdir.join("test_loc.py")
        # nodes.Item.location is a property
        #
        item.location = ("test_loc.py", 1, "test_loc_1")
        item.function.__name__ = "test_loc.py"
        item.cls = None
        expected = BreakLoc(rootdir / "test_loc.py", 2, "test_loc_1")
        assert BreakLoc.from_pytest_item(item) == expected
        #
        item.location = ("test_loc.py", 1, None)
        item.function.__name__ = "test_loc.py"
        item.cls = None
        expected = BreakLoc(rootdir / "test_loc.py", 2, "None")
        assert BreakLoc.from_pytest_item(item) == expected
        #
        item.location = ("/tmp/test_loc.py", 1, "test_loc_1")
        with pytest.raises(AssertionError):
            BreakLoc.from_pytest_item(item)


def test_resolve_wanted(tmp_path, request):
    # tmp_path is a pathlib.Path object, which has no .chdir()
    import os

    # For rootdir determination, see:
    # - _pytest.config.findpaths
    # - _pytest.config.Config._initini
    # - testing/test_config.py (pytest project)
    import py
    invocation_dir = py.path.local()
    assert Path(invocation_dir).is_absolute()
    assert invocation_dir == Path.cwd() == request.config.invocation_dir
    assert invocation_dir.strpath == str(invocation_dir)

    from unittest.mock import Mock
    rootdir = tmp_path / "rootdir"
    inst = Mock()
    inst.config = Mock(rootdir=py.path.local(rootdir),
                       invocation_dir=invocation_dir)
    _resolve_wanted = PdbBreak._resolve_wanted

    rootdir.mkdir()
    os.chdir(rootdir)
    assert Path.cwd() == rootdir

    # Relative, top-level
    file = Path(rootdir / "test_top.py")
    path = Path(file.name)
    assert not path.is_absolute()
    wanted = BreakLoc(path, 1, None)
    with pytest.raises(FileNotFoundError):
        _resolve_wanted(inst, wanted)
    file.write_text("pass")
    result = _resolve_wanted(inst, wanted)
    assert result.file.exists()
    assert result.file.is_absolute()

    # Relative, subdir
    subdir = rootdir / "subdir"
    subdir.mkdir()
    file = subdir / "test_sub.py"
    path = file.relative_to(rootdir)
    assert not path.is_absolute()
    wanted = BreakLoc(path, 1, None)
    #
    file.write_text("pass")
    result = _resolve_wanted(inst, wanted)
    assert result.file.exists()
    assert result.file.is_absolute()
    #
    os.chdir(subdir)
    result = _resolve_wanted(inst, wanted)
    assert result.file.exists()
    assert result.file.is_absolute()


def test_get_node_at_pos(source_ast):
    from pytest_pdb_break import _get_node_at_pos
    import ast
    root = ast.parse(source_ast)
    node = _get_node_at_pos(6, root)
    assert type(node.parent) is ast.FunctionDef
    assert node.parent.name == "f"
    assert type(node.parent.parent) is ast.ClassDef
    assert node.parent.parent.name == "C"


def test_fortify_location(testdir_ast):
    from pytest_pdb_break import fortify_location
    filename = Path(testdir_ast.tmpdir / "test_fortify_location.py")
    assert filename.exists()
    rv = fortify_location(filename, 2)
    assert rv.equals(BreakLoc(filename, 2, None,
                              class_name=None,
                              func_name="somefunc"))
    rv = fortify_location(filename, 6)
    assert rv.equals(BreakLoc(filename, 6, None,
                              class_name="C", func_name="f"))
    rv = fortify_location(filename, 13)
    assert rv.equals(BreakLoc(filename, 13, None,
                              class_name="TestClass", func_name="test_foo"))
    assert rv.inner is not None
    rv = fortify_location(filename, 17)
    assert rv is None
    rv = fortify_location(filename, 21)
    assert rv.equals(BreakLoc(filename, 21, None,
                              class_name=None,
                              func_name="wrapped"))
    from ast import FunctionDef
    assert type(rv.ast_obj) is FunctionDef


def test_fortify_location_aio(testdir_ast_aio):
    from pytest_pdb_break import fortify_location
    filename = Path(testdir_ast_aio.tmpdir / "test_fortify_location_aio.py")
    assert filename.exists()
    # No top-level, test_* prefixed asyncio functions allowed
    assert fortify_location(filename, 2) is None
    rv = fortify_location(filename, 9)
    assert rv.equals(BreakLoc(filename, 9, None,
                              class_name="TestClass", func_name="test_foo"))
    assert rv.inner == "inner"


def test_is_fixture(testdir_setup):
    from pytest_pdb_break import fortify_location, is_fixture
    testdir_setup.makepyfile(test_file="""
        @wrapper.attr
        def a():
            print("a")                 # <- line 3

        @wrapper.attr
        def b():
            def inner():
                return True            # <- line 8
            print("b")

        @wrapper.attr
        def c():
            class C:
                def inner(self):
                    return True        # <- line 15
            print("c")

        def d():
            print("d")                 # <- line 19
    """)
    filename = Path(testdir_setup.tmpdir.join("test_file.py"))

    wanted = fortify_location(filename, 3)
    assert wanted.func_name == "a"
    assert is_fixture(wanted, ["x", "a", "y", "z"]) is True
    assert wanted.func_name == "a"
    assert wanted.inner is None

    wanted = fortify_location(filename, 8)
    assert wanted.func_name == "inner"
    assert is_fixture(wanted, ["x", "b", "y", "z"]) is True
    assert wanted.func_name == "b"
    assert wanted.inner == "inner"

    wanted = fortify_location(filename, 15)
    assert wanted.func_name == "inner"
    assert is_fixture(wanted, ["x", "c", "y", "z"]) is True
    assert wanted.func_name == "c"
    assert wanted.inner == "inner"

    wanted = fortify_location(filename, 15)
    assert wanted.func_name == "inner"
    assert is_fixture(wanted, ["x", "y", "z"]) is False
    assert wanted.func_name == "inner"
    assert wanted.inner is None

    wanted = fortify_location(filename, 19)  # no deco
    assert wanted.func_name == "d"
    assert is_fixture(wanted, ["x", "d", "y", "z"]) is False
    assert wanted.func_name == "d"
    assert wanted.inner is None


def test_invalid_arg(testdir_setup):
    td = testdir_setup
    td.makepyfile("""
        def test_foo():
            assert True
    """)

    # No line number (argparse error)
    result = td.runpytest("--break=test_invalid_arg.py")
    assert result.ret == 4
    result.stderr.fnmatch_lines(["*usage:*", "*--break*invalid*value*"])

    # Non-existent file
    result = td.runpytest("--break=foo:99")
    assert result.ret == 3
    result.stdout.fnmatch_lines("INTERNALERROR>*FileNotFoundError*")
    # TODO usage msg appears in captured stderr but result.stderr is empty

    # Ambiguous case: no file named, but multiple given
    td.makepyfile(test_otherfile="""
        def test_bar():
            assert True

        def not_a_test():
            return 1
    """)
    result = td.runpytest("--break=1")
    assert result.ret == 3
    result.stdout.fnmatch_lines("INTERNALERROR>*RuntimeError: "
                                "unable to determine breakpoint file*")

    # Invalid line (regression): finder bails if node is None
    result = td.runpytest("--break=test_otherfile.py:99")
    assert result.ret == 3
    result.stdout.fnmatch_lines("INTERNALERROR>*RuntimeError: "
                                "unable to determine breakpoint location*")

    # No file named, but pytest arg names one
    pe = td.spawn_pytest("--break=1 test_otherfile.py")  # <- Two sep args
    # XXX API call sig is different for these spawning funcs (string)
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_otherfile.py(2)test_bar()",
        "->*assert True"
    ])
    pe.sendline("c")  # requested line is adjusted to something breakable
    pe.expect(EOF)

    # Provided location is not a test
    result = td.runpytest("--break=5", "test_otherfile.py")
    assert result.ret == 3
    result.stdout.fnmatch_lines("INTERNALERROR>*RuntimeError: "
                                "*outside of test function*")


@pytest.mark.parametrize("opts", [("--trace",),
                                  ("--pdbcls=pdb:Pdb", "--complete")])
def test_compat_usage(testdir_setup, recwarn, opts):
    result = testdir_setup.runpytest(*opts, "--break=2")
    assert result.ret == 3
    result.stdout.fnmatch_lines("INTERNALERROR>*RuntimeError*")

    if "--complete" in opts:
        base = pytest.set_trace.__self__._wrapped_pdb_cls[1].__base__
        if "complete" in base.__dict__:
            w = recwarn.pop(UserWarning)
            assert "Ignoring" in str(w.message)


def test_compat_invoke_same_before(testdir_setup):
    td = testdir_setup
    td.makepyfile(test_file="""
        def test_foo():
            assert False                  # <- line 2
            # comment
            assert True                   # <- line 4
    """)
    pe = td.spawn_pytest("--break=4 --pdb")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_file.py(2)test_foo()",
        "->*assert False*"
    ])
    pe.sendline("c")  # our bp is never set, so that's that


def test_compat_invoke_same_after(testdir_setup):
    td = testdir_setup
    td.makepyfile(test_file="""
        def test_foo():
            assert True                   # <- line 2
            # comment
            assert False                  # <- line 4
    """)
    pe = td.spawn_pytest("--break=2 --pdb")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_file.py(2)test_foo()",
        "->*assert True*"
    ])
    pe.sendline("c")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*entering PDB*",
        "*>*/test_file.py(4)test_foo()",
        "->*assert False*"
    ])
    pe.sendline("c")


def test_compat_invoke_after_other(testdir_setup):
    td = testdir_setup
    td.makepyfile(test_a="""
        def test_foo():
            assert True                   # <- line 2
    """)
    td.makepyfile(test_b="""
        def test_bar():
            assert False                  # <- line 2
    """)
    pe = td.spawn_pytest("--break=test_a.py:2 --pdb")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_a.py(2)test_foo()",
        "->*assert True*"
    ])
    pe.sendline("c")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*entering PDB*",
        "*>*/test_b.py(2)test_bar()",
        "->*assert False*"
    ])
    pe.sendline("c")


def test_collect_only(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            assert True                   # <- line 2
    """)
    # Arg parse still runs, but fake file means our init isn't called
    result = testdir_setup.runpytest_subprocess("--break=fake.py:2",
                                                "--collectonly")
    result.stdout.fnmatch_lines(["*collected*", "*no tests ran*"])


def test_two_funcs_simple(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_simple.py:4")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_two_funcs_simple.py(4)test_true_int()",
        "->*# <- line 4",
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_two_funcs_comment(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_comment.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_two_funcs_comment.py(3)test_true_int()",
        "->*somevar = True"
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_two_funcs_gap(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_gap.py:5")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    # Advances to first breakable line in next func
    befs.fnmatch_lines([
        "*>*/test_two_funcs_gap.py(7)test_false_int()",
        "->*isinstance(False, int)"
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_one_arg(testdir_setup):
    testdir_setup.makepyfile("""
        import pytest

        @pytest.fixture
        def string():
            yield "string"

        def test_string(string):
            assert string                # line 8

    """)
    pe = testdir_setup.spawn_pytest("--break=test_one_arg.py:8")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_one_arg.py(8)test_string()")
    pe.sendline("c")
    pe.expect(EOF)


def test_mark_param(testdir_setup):
    testdir_setup.makepyfile("""
        import pytest

        @pytest.mark.parametrize("name,value", [("one", 1), ("two", 2)])
        def test_number(name, value):
            print(name)
            assert len(name) > value     # line 6
    """)
    pe = testdir_setup.spawn_pytest("--break=test_mark_param.py:6 "
                                    "--capture=no")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert "one" in befs
    pe.sendline("c")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert "two" in befs
    pe.sendline("c")
    pe.expect(EOF)


@pytest.mark.parametrize("cap_method", ["fd", "sys"])
def test_capsys(testdir_setup, cap_method):
    testdir_setup.makepyfile(r"""
        def test_print(capsys):
            print("foo")
            capped = capsys.readouterr()
            assert capped.out == "foo\n"
            assert True                  # line 5
            print("bar")
            capped = capsys.readouterr()
            assert capped.out == "bar\n"
    """)  # raw string \n
    pe = testdir_setup.spawn_pytest("--break=test_capsys.py:5 "
                                    "--capture=%s" % cap_method)
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert "foo" not in befs
    lbefs = LineMatcher(befs)
    lbefs.fnmatch_lines(("*>*/test_capsys.py(5)test_print()", "->*# line 5"))
    pe.sendline("c")
    pe.expect(EOF)
    befs = unansi(pe.before)
    assert "bar" not in befs
    lbefs = LineMatcher(befs)
    lbefs.fnmatch_lines(["*[[]100%[]]*", "*1 passed*"])


def test_capsys_noglobal(testdir_setup):
    testdir_setup.makepyfile(test_file=r"""
        def test_print(capsys):
            print("foo")
            assert capsys.readouterr() == "foo\n"
    """)
    # Subsequent pty-based tests may fail when run "inprocess"
    result = testdir_setup.runpytest_subprocess("--break=test_file.py:3",
                                                "--capture=no")
    result.stdout.fnmatch_lines("*RuntimeError*capsys*global*")
    result.assert_outcomes(failed=1)


def test_request_object(testdir_setup):
    # Formerly, request.function (request._pyfuncitem.obj) would be set to
    # inst.runcall_until, which interfered with tools like testdir._makefile,
    # which uses this request.function.__name__ as a default filename.
    testdir_setup.makepyfile(test_file="""
        def test_rq(request):
            assert True
            assert request.function is test_rq
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(2)test_rq()")
    pe.sendline("c")
    pe.expect(r"\[100%\]")
    pe.expect(r"\b1 passed")


def test_class_simple(testdir_class):
    pe = testdir_class.spawn_pytest("--break=test_class_simple.py:8")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_class_simple.py(8)test_one()",
        "->*# line 8"
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_class_early(testdir_class):
    # Target docstring
    pe = testdir_class.spawn_pytest("--break=test_class_early.py:5")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_class_early.py(8)test_one()",
        "->*# line 8"
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_class_gap(testdir_class):
    pe = testdir_class.spawn_pytest("--break=test_class_gap.py:10")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_class_gap.py(12)test_two()",
        "->*# line 12"
    ])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 failed*")


def test_class_gap_named(testdir_class):
    # XXX while it's nice that this passes, it might not be desirable: if a
    # requested line precedes the start of the first test item, an error is
    # raised; but this doesn't apply to intervals between items, as shown here
    pe = testdir_class.spawn_pytest(
        "--break=test_class_gap_named.py:10 "
        "test_class_gap_named.py::TestClass::test_two"
    )
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_class_gap_named.py(12)test_two()",
        "->*# line 12"
    ])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 failed*")


def test_lower_callee(testdir_setup):
    # Regression
    testdir_setup.makepyfile("""
        def test_util():
            result = util()
            assert result              # <- line 3

        def util():
            return True
    """)
    pe = testdir_setup.spawn_pytest("--break=test_lower_callee.py:3")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_lower_callee.py(3)test_util()",
        "->*# <- line 3",
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_no_bt_all(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            assert True
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(2)test_foo()")
    pe.sendline("w")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    assert "runcall_until" not in befs.str()
    befs.fnmatch_lines("*>*/test_file.py(2)test_foo()")
    pe.sendline("c")
    pe.expect(EOF)


def test_bt_all(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            assert True
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:2 --bt-all")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(2)test_foo()")
    pe.sendline("w")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    # Everythin shown
    befs.fnmatch_lines([
        "*/_pytest/config/__init__.py(*)main()",
        "*/pytest_pdb_break.py(*)runcall_until()",
        "*>*/test_file.py(2)test_foo()"
    ])
    pe.sendline("c")
    pe.expect(EOF)


def test_unittest_simple(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        import unittest
        class TestFoo(unittest.TestCase):
            def test_foo(self):
                somevar = True            # <- line 4
                self.assertTrue(somevar)
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:4")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(4)test_foo()")
    pe.sendline("c")
    pe.expect(EOF)


def test_unittest_inner(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        import unittest
        class TestFoo(unittest.TestCase):
            def test_foo(self):
                def inner(x):
                    return x                 # <- line 5
                self.assertTrue(inner(True))
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:5")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines(["*>*/test_file.py(5)inner()",
                        "*line 5*"])
    pe.sendline("c")
    pe.expect(EOF)


def test_elsewhere_fixture_simple(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        import pytest

        @pytest.fixture
        def fixie():
            return True        # <- line 5

        def test_foo(fixie):
            assert fixie
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:5")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines(["*>*/test_file.py(5)fixie()", "->*# <- line 5"])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 passed*")


def test_elsewhere_fixture_nested_simple(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        import pytest

        @pytest.fixture
        def fixie():
            def inner():
                return True              # <- line 6
            return inner()

        def test_foo(fixie):
            assert fixie
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:6")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines(["*>*/test_file.py(6)inner()", "->*# <- line 6"])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 passed*")


@pytest.mark.parametrize("lnum", [5, 7])
def test_elsewhere_fixture_with_yield(testdir_setup, lnum):
    testdir_setup.makepyfile(test_file="""
        import pytest

        @pytest.fixture
        def fixie():
            somevar = 1        # <- line 5
            yield somevar
            del somevar        # <- line 7

        def test_foo(fixie):
            assert fixie
    """)
    pe = testdir_setup.spawn_pytest(f"--break=test_file.py:{lnum}")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([f"*>*/test_file.py({lnum})fixie()",
                        f"->*# <- line {lnum}"])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 passed*")


def test_elsewhere_fixture_with_yield_nested(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        import pytest

        @pytest.fixture
        def fixie():
            class C:
                def __init__(self):
                    print("in init")     # <- line 7
                def ran(self):
                    return True
            c = C()
            yield c
            del c

        def test_foo(fixie):
            assert fixie.ran()
    """)
    pe = testdir_setup.spawn_pytest("--break=test_file.py:7")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines(["*>*/test_file.py(7)__init__()",
                        "->*# <- line 7"])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 passed*")


@pytest.mark.parametrize("bt_all", [False, True])
def test_inner_simple(testdir_setup, bt_all):
    testdir_setup.makepyfile(test_file="""
        def test_foo():

            def inner(x):
                print("inner")  # <- line 4
                return x + 1

            assert inner(1)     # <- line 7
            assert True
    """)
    opts = "--break=test_file.py:4"
    if bt_all:
        opts += " --bt-all"
    pe = testdir_setup.spawn_pytest(opts)
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines(["*>*/test_file.py(4)inner()",
                        "->*# <- line 4"])

    # Stack
    pe.sendline("w")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    if bt_all:
        befs.fnmatch_lines([
            "*/_pytest/config/__init__.py(*)main()",
            "*/pytest_pdb_break.py(*)runcall_until()",
            "*/test_file.py(7)test_foo()",
            "*assert inner(1)*line 7"
        ])
    else:
        assert "runcall_until" not in befs.str()
        befs.fnmatch_lines(["*/test_file.py(7)test_foo()",
                            "*assert inner(1)*line 7"])

    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 passed*")


@pytest.mark.parametrize("at_def", [False, True])
def test_inner_never_called(testdir_setup, at_def):
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            def inner(x):       # <- line 2
                print("inner")  # <- line 3
                return x + 1
            assert inner
    """)
    # The True case is unfortunate, may warrant switching behavior
    lnum = (3, 2)[at_def]
    pe = testdir_setup.spawn_pytest(f"--break=test_file.py:{lnum}")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*1 passed*")


def test_completion_commands_basic(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            localvar = 42
            assert True
            assert localvar
    """)

    # Complete method already present
    from _pytest.debugging import pdb as mod_pdb
    if "complete" in mod_pdb.Pdb.__dict__:
        result = testdir_setup.runpytest_subprocess("--complete", "--break=1")
        result.stderr.fnmatch_lines("*UserWarning*Ignoring*--complete*")
        return

    # Adding our completer doesn't break builtin cmd.Cmd completion
    pe = testdir_setup.spawn_pytest("--complete --break=test_file.py:3",
                                    expect_timeout=1.0)
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(3)test_foo()")
    pe.send("hel\t")
    pe.expect("hel\x07?p")  # singleton autocompletes with intervening BEL
    pe.send("\n")
    pe.expect("Documented commands")
    pe.expect(prompt_re)

    # Local variable and keyword
    pe.send("loc\t")
    pe.expect("loc\x07?al")
    pe.send("\t\t")
    pe.expect(r"\s*locals\(?\s*localvar")  # multiline: leftmost \s is \r\n
    pe.sendline("var")
    pe.expect("42")

    # Command completion persists (plain rlcompleter didn't replace us)
    pe.send("whe\t")
    pe.expect("whe\x07?re")
    pe.send("\n")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(3)test_foo()")

    # New local
    pe.send("imp\t")
    pe.expect("imp\x07?ort")
    pe.send(" sys\n")
    pe.expect("sys")
    pe.expect(prompt_re)
    pe.send("sys.\t\t")
    pe.expect(r"\s*sys\.path.*sys\.version")
    pe.send("version\t\t")
    pe.expect(r"\s*sys\.version.*sys\.version_info")

    # Chain
    pe.send("_in\t")
    pe.expect("sys.version_in\x07?fo")
    pe.send(".m\t\t")
    pe.expect(r"\s*sys\.version_info\.major.*sys\.version_info\.minor")
    pe.send("ajor\n")
    pe.expect("3")

    # Command completion persists (plain rlcompleter didn't replace us)
    pe.send("unti\t")
    pe.expect("unti\x07?l")
    pe.send(" 4\n")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(4)test_foo()")

    pe.sendline("c")


def test_completion_commands_interact(testdir_setup):
    testdir_setup.makepyfile(test_file="""
        def test_foo():
            localvar = 42
            assert True
            assert localvar
    """)

    # Complete method already present
    from _pytest.debugging import pdb as mod_pdb
    if "complete" in mod_pdb.Pdb.__dict__:
        result = testdir_setup.runpytest_subprocess("--complete", "--break=1")
        result.stderr.fnmatch_lines("*UserWarning*Ignoring*--complete*")
        return

    # Adding our completer doesn't break builtin cmd.Cmd completion
    pe = testdir_setup.spawn_pytest("--complete --break=test_file.py:3",
                                    expect_timeout=1.0)
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_file.py(3)test_foo()")
    pe.sendline("import sys")
    pe.expect("sys")
    pe.send("inter\t")
    pe.expect("inter\x07?act")
    pe.send("\n")
    pe.expect("[*]interactive[*]")
    pe.expect(">>> ")

    # Local from test scope
    pe.send("localv\t\t")
    pe.expect("localv\x07?ar")
    pe.send("\n")
    pe.expect("42")

    # Locals from pdb cmd loop
    pe.expect(">>> ")
    pe.send("sys.\t\t")
    pe.expect(r"\s*sys\.path.*sys\.version")
    pe.send("version_info\n")
    pe.expect("3")

    # New locals
    pe.expect(">>> ")
    pe.sendline("import os")
    pe.expect("os")
    pe.send("os.path.\t\t")
    pe.expect(r"\s*os\.path\.curdir.*os\.path\.sep")
    pe.send("sys is sys\n")
    pe.expect("True")

    # Interactive locals don't pollute pdb cmd loop
    pe.send("\x04")  # ^D EOT
    pe.expect(prompt_re)
    pe.sendline("os")
    pe.expect("NameError")
    pe.sendline("localvar, sys")
    pe.expect("[(]42.*sys.*[)]")

    pe.sendline("c")


def test_simple_nested_async(testdir_simple_nested_async):
    pe = testdir_simple_nested_async.spawn_pytest(
        "--break=test_simple_nested_async.py:6"
    )
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_simple_nested_async.py(6)inner()",
        "->*# <- line 6",
    ])
    pe.sendline("c")
    pe.expect(EOF)
