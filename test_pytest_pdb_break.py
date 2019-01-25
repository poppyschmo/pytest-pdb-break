import pytest

from pathlib import Path
from unittest.mock import patch
from _pytest.pytester import LineMatcher
from pytest_pdb_break import BreakLoc, get_targets, PdbBreak
# Most of these need pexpect


prompt_re = r"\(Pdb[+]*\)\s?"


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


def test_get_targets():
    # XXX this assumes parametrized variants, whether their names are
    # auto-assigned or not, always appear in the order they'll be called.
    items = [BreakLoc(file="file_a", lnum=1, name="test_notfoo"),
             BreakLoc(file="file_b", lnum=1, name="test_foo"),
             BreakLoc(file="file_b", lnum=10, name="test_bar[one-1]"),
             BreakLoc(file="file_b", lnum=10, name="test_bar[two-2]"),
             BreakLoc(file="file_b", lnum=10, name="test_bar[three-3]"),
             BreakLoc(file="file_b", lnum=99, name="test_baz"),
             BreakLoc(file="file_c", lnum=1, name="test_notbaz")]
    assert get_targets(Path("file_b"), 30, items).popleft() == items[2]
    assert items[2].name == "test_bar[one-1]"
    items.reverse()
    assert get_targets(Path("file_b"), 30, items).popleft() == items[2]
    assert items[2].name == "test_bar[three-3]"


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
    assert invocation_dir == Path().cwd() == request.config.invocation_dir
    assert invocation_dir.strpath == str(invocation_dir)

    from unittest.mock import Mock
    rootdir = tmp_path / "rootdir"
    inst = Mock()
    inst.config = Mock(rootdir=py.path.local(rootdir),
                       invocation_dir=invocation_dir)
    _resolve_wanted = PdbBreak._resolve_wanted

    rootdir.mkdir()
    os.chdir(rootdir)
    assert Path().cwd() == rootdir

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


def unansi(byte_string, as_list=True):
    import re
    out = re.sub("\x1b\\[[\\d;]+m", "", byte_string.decode().strip())
    if as_list:
        return out.split("\r\n")
    out


@pytest.fixture
def testdir_setup(testdir):
    """Require main file."""
    testdir.makeconftest("""
        import sys
        sys.path.insert(0, %r)
        pytest_plugins = %r
    """ % (str(Path(__file__).parent), PdbBreak.__module__))
    return testdir


def test_invalid_arg(testdir_setup):
    td = testdir_setup
    td.makepyfile("""
        def test_foo():
            assert True
    """)

    # No line number (argparse error)
    result = td.runpytest("--break=test_invalid_arg.py")
    assert result.ret == 4
    lines = LineMatcher(result.stderr.lines)
    lines.fnmatch_lines(["usage:*", "*--break*invalid*value*"])

    # Non-existent file
    result = td.runpytest("--break=foo:99")
    assert result.ret == 3
    lines = LineMatcher(result.stderr.lines)
    lines.fnmatch_lines("INTERNALERROR>*FileNotFoundError*")

    # Ambiguous case: no file named, but multiple given
    td.makepyfile(test_otherfile="""
        def test_bar():
            assert True
    """)
    result = td.runpytest("--break=1")
    assert result.ret == 3
    lines = LineMatcher(result.stdout.lines[-5:])
    lines.fnmatch_lines("INTERNALERROR>*RuntimeError: "
                        "breakpoint file couldn't be determined")

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


def test_two_funcs_simple(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_simple.py:4")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_two_funcs_simple.py(4)test_true_int()",
        "->*# <- line 4",
    ])
    pe.sendline("c")


def test_two_funcs_comment(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_comment.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_two_funcs_comment.py(3)test_true_int()",
        "->*somevar = True"
    ])
    pe.sendline("c")


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


def test_mark_param(testdir_setup):
    # Only break once: with the first set of args bound
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
    pe.sendline("c")  # If called again, would get timeout error


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
    afts = unansi(pe.read(-1))
    lafts = LineMatcher(afts)
    assert "bar" not in afts
    lafts.fnmatch_lines((".*[[]100%[]]", "*= 1 passed in * seconds =*"))


def test_capsys_noglobal(testdir_setup):
    testdir_setup.makepyfile(r"""
        def test_print(capsys):
            print("foo")
            assert capsys.readouterr() == "foo\n"
    """)
    result = testdir_setup.runpytest("--break=test_capsys_noglobal.py:3",
                                     "--capture=no")
    lout = LineMatcher(result.stdout.lines)
    lout.fnmatch_lines("*RuntimeError*capsys*global*")
    result.assert_outcomes(failed=1)  # this runs as function node obj


def test_request_object(testdir_setup):
    # Formerly, request.function (request._pyfuncitem.obj) would be set to
    # inst.runcall_until, which interfered with tools like testdir._makefile,
    # which uses this request.function.__name__ as a default filename.
    testdir_setup.makepyfile("""
        def test_rq(request):
            assert True
            assert request.function is test_rq
    """)
    pe = testdir_setup.spawn_pytest("--break=test_request_object.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines("*>*/test_request_object.py(2)test_rq()")
    pe.sendline("c")
    afts = unansi(pe.read(-1))
    lafts = LineMatcher(afts)
    lafts.fnmatch_lines((".*[[]100%[]]", "*= 1 passed *"))


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


def test_class_simple(testdir_class):
    pe = testdir_class.spawn_pytest("--break=test_class_simple.py:8")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_class_simple.py(8)test_one()",
        "->*# line 8"
    ])
    pe.sendline("c")


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


def test_class_gap(testdir_class):
    pe = testdir_class.spawn_pytest("--break=test_class_gap.py:10")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*/test_class_gap.py(12)test_two()",
        "->*# line 12"
    ])
    pe.sendline("c")


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
