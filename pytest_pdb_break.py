import os
import sys
import pytest
from functools import namedtuple
from _pytest.pytester import LineMatcher
from pathlib import Path


try:
    if tuple(int(s) for s in pytest.__version__.split(".")) < (4, 0):
        raise RuntimeError("Requires at least pytest 4.0")
except ValueError:
    pass  # assume some git-describe-like string: devNN+gdeadbeef

if sys.version_info < (3, 6):
    raise RuntimeError("For now, requires Python 3.6+")

module_logger = None
try:
    from logging_helper import LoggingHelper
except ImportError:
    pass
else:
    LoggingHelper.LOGFILE = os.getenv("PDBBRK_LOGFILE")
    if LoggingHelper.LOGFILE:
        LoggingHelper.LOGDEFS = os.getenv("PDBBRK_LOGYAML")
        LoggingHelper.HANDLER_NAME = "PDB BREAK (DEBUG)"
        LoggingHelper.LEVEL = "DEBUG"
        module_logger = LoggingHelper.get_logger("<module>")


class BreakLoc(namedtuple("BreakpointLocation", "file lnum name")):
    """Data object holding filename, line number, test name."""
    def __new__(cls, *args, **kwargs):
        if len(args) == 1:
            item = args[0]
            # Should probably just invoke constructor methods directly instead
            # of playing overload cop, but space is tight in those [list comps]
            if isinstance(item, str):
                return cls.from_cmd_option_arg_spec(item)
            if hasattr(item, "location"):
                return cls.from_pytest_item(item)
        return super().__new__(cls, *args, **kwargs)

    def __repr__(self):
        rv = super().__repr__()
        if module_logger and hasattr(self.file, "parent"):
            rv = rv.replace(str(self.file.parent), "...")
        return rv

    @classmethod
    def from_pytest_item(cls, item):
        # Note: pytest.Item.location line numbers are zero-indexed, but pdb
        # breakpoints aren't, and neither are linecache's.
        assert isinstance(item, pytest.Item)
        file, lnum, name = item.location
        file = Path(file)
        # Comment in ``Config.cwd_relative_nodeid`` says "nodeid's are relative
        # to the rootpath." Seems this also applies to .location names.
        assert not file.is_absolute()
        file = Path(item.session.config.rootdir) / file
        return cls(file, lnum + 1, name)

    @classmethod
    def from_cmd_option_arg_spec(cls, string):
        file, __, lnum = string.rpartition(":")
        return cls(Path(file) if file else None, int(lnum), None)


def pytest_addoption(parser):
    group = parser.getgroup("pdb")
    group.addoption("--break",
                    action="store",
                    metavar="<file:lnum>",
                    dest="pdb_break",
                    type=BreakLoc,
                    help="One [<file>:<line-number>] pair, like foo.py:42")


def pytest_configure(config):
    wanted = config.getoption("pdb_break")
    if wanted:
        PdbBreak(wanted, config)


class PdbBreak:
    debug = False

    def __init__(self, wanted, config):
        config.pluginmanager.register(self, "pdb_break")
        self.capman = config.pluginmanager.getplugin("capturemanager")
        self.config = config
        self.wanted = wanted
        self.target = None
        if module_logger:
            self._l = LoggingHelper.get_logger("PdbBreak")
            self.debug = True
        self.last_pdb = None
        self.last_func = None

    def pytest_internalerror(self, excrepr, excinfo):
        if self.debug:  # already prints to tw w/o cap
            self._l.prinspot(1)

    def pytest_runtestloop(self, session):
        """Find target or raise."""
        if not self.wanted or not any(self.wanted):
            return
        locs = [BreakLoc(i) for i in session.items]
        self.debug and self._l.prinspotl(1)
        if self.wanted.file:
            curdir = Path().resolve()
            rootdir = Path(session.config.rootdir)
            assert rootdir.is_absolute()
            try:
                _f = self.wanted.file.resolve(True)
            except FileNotFoundError:
                if curdir.samefile(rootdir):
                    raise
                os.chdir(rootdir)
                _f = self.wanted.file.resolve(True)
            self.wanted = self.wanted._replace(file=_f)
            os.chdir(curdir)
        else:
            locs_files = set(l.file for l in locs)
            # If solo, assume good
            if len(locs_files) == 1:
                inferred = locs_files.pop()
                self.wanted = self.wanted._replace(file=inferred)
                self.debug and self._l.prinspotl(2)
            else:
                raise RuntimeError("breakpoint file couldn't be determined")
        self.targets = get_targets(self.wanted.file, self.wanted.lnum, locs)
        try:
            self.target = self.targets.popleft()
        except IndexError as exc:
            msg = "a valid breakpoint could not be determined"
            raise RuntimeError(msg) from exc
        self.debug and self._l.prinspotl(3)

    def pytest_enter_pdb(self, config, pdb):
        """Stash pytest-wrapped pdb instance.

        This workaround is necessary because ``pytestPDB.set_trace``
        doesn't return the instance.
        """
        self.last_pdb = pdb

    def trace_handoff(self, frame, event, arg):
        if frame.f_code.co_filename == str(self.wanted.file):
            if self.debug:  # ^~~~~ may be a description, e.g., <string>
                self._l.prinspect(event=event, frame=frame)
            if event == "call":
                name = frame.f_code.co_name
                if name == self.target.name:
                    # TODO verify is still correct when f_back is no longer
                    # runcall_until (e.g., target called itself)
                    frame.f_trace = self.trace_handoff
            if event == "line":
                line = frame.f_lineno
                if line >= self.wanted.lnum:
                    inst = self.last_pdb
                    if self.debug:
                        assert inst.botframe.f_code.co_filename == __file__
                        assert inst.botframe.f_code.co_name == "runcall_until"
                        assert inst.stopframe is inst.botframe
                    # This loop is meant to handle the recursive case;
                    # otherwise, there's no need to reinstrument "backwards"
                    # because those frames are internal to pytest.
                    _frame = frame
                    while _frame and _frame is not inst.botframe:
                        _frame.f_trace = inst.trace_dispatch
                        _frame = _frame.f_back
                    inst.set_break(str(self.wanted.file), line, True)
                    self.target = None
                    sys.settrace(inst.trace_dispatch)  # handoff
                    return inst.dispatch_line(frame)
        return self.trace_handoff

    def runcall_until(self, *args, **kwargs):
        """Run test with args, stopping at location.

        Exceptions raised inside this function will be reported as test
        failures. For testing, this means report output is sent to stdout
        rather than stderr (INTERNALERROR).
        """
        # If the "request" feature is in play, pyfuncitem.obj will be this
        # function. Should see if reassigning makes sense.
        from _pytest.capture import capture_fixtures
        pytest.set_trace(set_break=False)
        if self.debug:
            assert self.last_pdb
            assert self.last_func
        inst = self.last_pdb
        func = self.last_func
        # XXX maybe only provide context for capsys, ignore others?
        try:
            capfix = (kwargs.keys() & capture_fixtures).pop()
        except KeyError:
            capfix = None
        self.debug and self._l.prinspotl("pre_capfix")
        if capfix:
            self.debug and self._l.prinspotl("cap_top")
            if capfix == "capsys" and not self.capman.is_globally_capturing():
                raise RuntimeError("Cannot break inside tests using capsys "
                                   "when global capturing is disabled")
            capfix = kwargs[capfix]

            def preloop(_inst):
                # XXX runs after .setup, but global capture still active?
                super((type(_inst)), _inst).preloop()
                self.capman.suspend_global_capture(in_=True)

            def postloop(_inst):
                super((type(_inst)), _inst).postloop()
                capfix._resume()

            inst.__dict__["preloop"] = preloop.__get__(inst)
            inst.__dict__["postloop"] = postloop.__get__(inst)
            # TODO figure out why resumed state isn't already active
            # Maybe we have to run some hooks?
            capfix._resume()
            self.debug and self._l.prinspotl("cap_bot")
        from bdb import BdbQuit
        res = None
        inst.reset()
        inst.botframe = sys._getframe()
        self.debug and self._l.prinspot("post_capfix")
        inst._set_stopinfo(inst.botframe, None, 0)
        sys.settrace(self.trace_handoff)
        try:
            res = func(*args, **kwargs)
        except BdbQuit:
            pass
        finally:
            inst.quitting = True
            sys.settrace(None)
            self.last_pdb = self.last_func = None
        return res

    @pytest.hookimpl(hookwrapper=True)
    def pytest_pyfunc_call(self, pyfuncitem):
        if self.debug:
            assert self.last_pdb is None
            assert self.last_func is None
            assert not pyfuncitem._isyieldedfunction()
            self._l.prinspot(1)
        if BreakLoc(pyfuncitem) == self.target:
            self.last_func = pyfuncitem.obj
            pyfuncitem.obj = self.runcall_until
        yield
        if pyfuncitem.obj.__func__ is type(self).runcall_until:
            if self.target and self.targets:
                self.target = self.targets.popleft()
            self.debug and self._l.prinspot(2)


def get_targets(filename, upper, locations):
    """Return ranked targets from a list of item locations."""
    from operator import attrgetter
    from collections import deque
    from itertools import groupby
    neighbors = (l for l in locations if l.file == filename)
    lnumgetter = attrgetter("lnum")
    out = []
    for side, locs in groupby(neighbors, lambda l: l.lnum <= upper):
        if side:
            # Note: this isn't the same as not reversing and popping last item
            locs = sorted(locs, key=lnumgetter, reverse=True)
            if locs:
                # Not sure if different arg bindings can affect whether trace
                # is called, so just include all partialized variants for now
                out = [l for l in locs if l.lnum == locs[0].lnum] + out
        else:
            out += sorted(locs, key=lnumgetter)
    return deque(out)


# Tests for this plugin (requires pexpect)

prompt_re = r"\(Pdb[+]*\)\s?"


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
    assert get_targets("file_b", 30, items).popleft() == items[2]
    assert items[2].name == "test_bar[one-1]"
    items.reverse()
    assert get_targets("file_b", 30, items).popleft() == items[2]
    assert items[2].name == "test_bar[three-3]"


def unansi(byte_string, as_list=True):
    import re
    out = re.sub("\x1b\\[[\\d;]+m", "", byte_string.decode().strip())
    if as_list:
        return out.split("\r\n")
    out


@pytest.fixture
def testdir_setup(testdir):
    """Copy this file to testdir basetemp."""
    from pathlib import PurePath
    fpp = PurePath(__file__)
    testdir.makeconftest("""
        pytest_plugins = "%s"
    """ % fpp.stem)
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
    lines.fnmatch_lines(["usage:*", "*--break*invalid BreakLoc value*"])

    # Non-existent file
    result = td.runpytest("--break=foo:99")
    assert result.ret == 3
    lines = LineMatcher(result.stdout.lines[-5:])
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


if __name__ == "__main__" and not os.getenv("PDBBRK_HACK"):
    cmdline = ("pytest", "-p", "pytester", "--noconftest", __file__)
    if sys.platform.startswith("linux"):
        sys.stdout.flush()
        os.environ["PYTHONPATH"] = os.path.dirname(__file__)
        os.execlp("python", sys.executable, "-m", *cmdline)
    else:
        raise RuntimeError("The tests above aren't meant to be discovered."
                           " Try running:\n%s" % " ".join(cmdline))

# Copyright 2018 Jane Soko
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
