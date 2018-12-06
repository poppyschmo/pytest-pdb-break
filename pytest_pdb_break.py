import os
import sys
import pytest
from functools import namedtuple
from _pytest.pytester import LineMatcher


try:
    if tuple(int(s) for s in pytest.__version__.split(".")) < (4, 0):
        raise RuntimeError("Requires at least pytest 4.0")
except ValueError:
    pass  # assume some git-describe-like string: devNN+gdeadbeef

if sys.version_info < (3, 6):
    raise RuntimeError("For now, requires Python 3.6+")


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

    @classmethod
    def from_pytest_item(cls, item):
        # Note: pytest.Item.location line numbers are zero-indexed, but pdb
        # breakpoints aren't, and neither are linecache's.
        assert isinstance(item, pytest.Item)
        file, lnum, name = item.location
        return cls(file, lnum + 1, name)

    @classmethod
    def from_cmd_option_arg_spec(cls, string):
        file, __, lnum = string.rpartition(":")
        return cls(os.path.abspath(file) if file else None, int(lnum), None)


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


# Can't figure out how to use pdb to inspect these hooks, so use this
# abomination instead.
class LoggingHelper:
    LOGFILE = os.getenv("PDBBRK_LOGFILE")  # TTY devices only
    HANDLER_NAME = "PDB BREAK (DEBUG)"
    LEVEL = "DEBUG"

    def __init__(self):
        import logging
        self.level = getattr(logging, self.LEVEL)
        self.logger = self.get_logger(type(self).__name__)
        self._prinspect_next = {}
        self.LOGFILE.write("\n")
        self.LOGFILE.flush()

    def _log_as(self, frame, msg, exc_info=None):
        """Log msg, impersonating frame."""
        co = frame.f_code
        record = self.logger.makeRecord(
            self.logger.name, self.level, co.co_filename, frame.f_lineno,
            msg, (), exc_info, co.co_name, None, None
        )
        self.logger.handle(record)

    def prinspect(self, *args, **kwargs):
        """Print stuff."""
        from pprint import PrettyPrinter
        pp = PrettyPrinter()
        caller = sys._getframe(1)
        name = caller.f_code.co_name
        defer = kwargs.get("defer")
        if self._prinspect_next:
            for k in tuple(self._prinspect_next):
                if not defer or name != k:
                    v = self._prinspect_next.pop(k)
                    c = v.pop("defer")
                    if name == k:
                        kwargs.update(v)
                        continue
                    self._log_as(c, "\n{}".format(pp.pformat(v)))
        if args:
            locals = caller.f_locals
            kwargs.update((a, eval(a, locals)) if isinstance(a, str) else
                          a for a in args)
        if defer:
            kwargs["defer"] = caller
            self._prinspect_next.setdefault(name, {}).update(kwargs)
        else:
            self._log_as(caller, "\n{}".format(pp.pformat(kwargs)))

    from functools import partialmethod
    prinspool = partialmethod(prinspect, defer=True)

    @classmethod
    def get_logger(cls, name):
        """Return a logger with a TTY handler for cls.LEVEL."""
        # XXX Don't add LOGFILE.close or logging.shutdown to config.add_cleanup
        if not cls.LOGFILE:
            assert cls.LOGFILE is None
            return
        import logging
        assert sys.platform.startswith("linux")
        if hasattr(cls.LOGFILE, "closed"):
            assert not cls.LOGFILE.closed
            logfile = cls.LOGFILE
        else:
            logfile = open(cls.LOGFILE, "w")
            assert os.isatty(logfile.fileno())
            cls.LOGFILE = logfile
        logger = logging.getLogger(name)
        handler = getattr(cls, "_handler", None)
        if handler and handler in logger.handlers:
            return logger
        fmt = ("{dk}[%(asctime)s]{no} {dm}%(name)s"
               ".%(funcName)s:{no} %(message)s")
        clrs = dict(dk="\x1b[38;5;241m", dm="\x1b[38;5;247m", no="\x1b[m")
        handler = logging.StreamHandler(stream=logfile)
        handler.setFormatter(logging.Formatter(fmt.format(**clrs)))
        handler.set_name(cls.HANDLER_NAME)
        handler.setLevel(cls.LEVEL)
        cls._handler = handler
        logger.addHandler(handler)
        logger.setLevel(cls.LEVEL)
        return logger


class PdbBreak:
    debug = False

    def __new__(cls, *args, **kwargs):
        if not LoggingHelper.LOGFILE:
            return super().__new__(cls)
        # Manually patch with logging mixins, so MRO stays clean
        for name, member in LoggingHelper.__dict__.items():
            if not hasattr(cls, name):
                setattr(cls, name, member)
        inst = super().__new__(cls)
        LoggingHelper.__init__(inst)
        return inst

    def __init__(self, wanted, config):
        config.pluginmanager.register(self, "pdb_break")
        self.capman = config.pluginmanager.getplugin("capturemanager")
        self.config = config
        self.wanted = wanted
        self.target = None
        if getattr(self, "logger", None):
            self.debug = True
        self.last_pdb = None
        self.last_func = None

    def pytest_internalerror(self, excrepr, excinfo):
        if self.debug:  # already prints to tw w/o cap
            self.prinspect(type=excinfo.type,
                           # excrepr=excrepr,
                           value=excinfo.value)

    def pytest_runtestloop(self, session):
        """Find target or raise."""
        if not self.wanted or not any(self.wanted):
            return
        locs = [BreakLoc(i) for i in session.items]
        if self.debug:
            self.prinspool(wanted=self.wanted, locs=locs)
        if self.wanted.file:
            # Conversion to abs path on init doesn't guarantee existence
            if not os.path.isfile(self.wanted.file):
                raise RuntimeError("file '%s' not found" % self.wanted.file)
        else:
            locs_files = set(l.file for l in locs)
            # If solo, assume good
            if len(locs_files) == 1:
                inferred = os.path.abspath(locs_files.pop())
                self.wanted = self.wanted._replace(file=inferred)
                if self.debug:
                    self.prinspool(("wanted.file",
                                    "'' -> {}".format(self.wanted.file)))
            else:
                raise RuntimeError("breakpoint file couldn't be determined")
        lnum = find_breakable_line(self.wanted.file, self.wanted.lnum)
        locs = (l for l in locs if
                os.path.samefile(l.file, self.wanted.file) and l.lnum <= lnum)
        locs = sorted(locs, key=lambda i: i.lnum, reverse=True)
        self.target = next(iter(locs), None)
        if not self.target:
            raise RuntimeError("a valid breakpoint could not be determined")
        # When lines match, we're already at the breakpoint, so settrace's
        # callees won't see it.  Another tack would be to forgo calling
        # ``_set_stopinfo`` after resetting and instead just enter the target
        # already stopped (this is what --trace does).
        if self.target.lnum == lnum:
            lnum = find_breakable_line(self.wanted.file, lnum + 1)
        if lnum != self.wanted.lnum:
            if self.debug:
                self.prinspool(("wanted.lnum",
                                "{} -> {}".format(self.wanted.lnum, lnum)))
            self.wanted = self.wanted._replace(lnum=lnum)
        if self.debug:
            self.prinspool("self.target")

    def pytest_enter_pdb(self, config, pdb):
        """Stash pytest-wrapped pdb instance.

        This workaround is necessary because ``pytestPDB.set_trace``
        doesn't return the instance.
        """
        self.last_pdb = pdb

    def runcall_until(self, *args, **kwargs):
        """Run test with args, stopping at location.

        Exceptions raised inside this function will be reported as test
        failures. For testing, this means report output is sent to stdout
        rather than stderr (INTERNALERROR).
        """
        from _pytest.capture import capture_fixtures
        pytest.set_trace(set_break=False)
        assert self.last_pdb
        assert self.last_func
        inst = self.last_pdb
        func = self.last_func
        # XXX maybe only provide context for capsys, ignore others?
        try:
            capfix = (kwargs.keys() & capture_fixtures).pop()
        except KeyError:
            capfix = None
        if self.debug:
            self.prinspool(prevstdout=sys.stdout)
        if capfix:
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
        inst.set_break(self.wanted.file, self.wanted.lnum, True)
        if self.debug:
            self.prinspect(this_frame=sys._getframe(),
                           cap_method=self.capman._method,
                           kwargs=kwargs,
                           stdout=type(sys.stdout),
                           f_back=sys._getframe().f_back,
                           breaks=inst.breaks)
        from bdb import BdbQuit
        res = None
        inst.reset()
        inst._set_stopinfo(sys._getframe(), None, -1)
        # ``inst.botframe`` is set to ^^^ for us by dispatch_call, but maybe
        # nicer to set explicitly here
        sys.settrace(inst.trace_dispatch)
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
        assert self.last_pdb is None
        assert self.last_func is None
        assert not pyfuncitem._isyieldedfunction()
        # import ctypes; ctypes.string_at(0xffffffff) # gdb breakpoint
        if self.debug:
            self.prinspect(("loc equals target",
                            BreakLoc(pyfuncitem) == self.target),
                           "pyfuncitem.name",
                           "pyfuncitem.location",
                           "pyfuncitem.funcargs")
        # Note: seems like nextitem is always None
        if BreakLoc(pyfuncitem) == self.target:
            self.last_func = pyfuncitem.obj
            pyfuncitem.obj = self.runcall_until
        yield


def find_breakable_line(filename, lineno):
    """Find the next breakable line.
    Like ``pdb.Pdb.checkline`` but without frame awareness.
    """
    import linecache
    # bdb.canonic without caching or angle-bracket guard
    while True:
        line = linecache.getline(filename, lineno)
        if not line:  # blanks include newlines
            line = None
            break
        line = line.strip()
        if not line or line[0] == '#' or line[:3] in ('"""', "'''"):
            lineno += 1
        else:
            break
    if line is None:
        raise RuntimeError("Unable to find a valid breakpoint")
    return lineno


# Tests for this plugin (requires pexpect)

prompt_re = r"\(Pdb[+]*\)\s?"


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
    with open(fpp) as flor:
        with open(os.path.join(testdir.tmpdir, fpp.name), "w") as flow:
            flow.write(flor.read())
    testdir.makeconftest("""
        # import pytest
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
    lines.fnmatch_lines("INTERNALERROR>*RuntimeError: file '*/foo' not found")

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
        ">*/test_otherfile.py(2)test_bar()",
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
        ">*/test_two_funcs_simple.py(4)test_true_int()",
        "->*# <- line 4",
    ])
    pe.sendline("c")


def test_two_funcs_comment(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_comment.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        ">*/test_two_funcs_comment.py(3)test_true_int()",
        "->*somevar = True"
    ])
    pe.sendline("c")


def test_two_funcs_gap(testdir_two_funcs):
    pe = testdir_two_funcs.spawn_pytest("--break=test_two_funcs_gap.py:5")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    # Advances to first breakable line in next func
    befs.fnmatch_lines([
        ">*/test_two_funcs_gap.py(7)test_false_int()",
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
    befs.fnmatch_lines(">*/test_one_arg.py(8)test_string()")
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
    lbefs.fnmatch_lines((">*/test_capsys.py(5)test_print()", "->*# line 5"))
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
        '''docstring'''
        def test_one(self):
            x = "this"                        # line 4
            assert "h" in x

        def test_two(self):
            x = "hello"                       # line 8
            assert hasattr(x, 'check')
    """)
    return testdir_setup


def test_class_simple(testdir_class):
    pe = testdir_class.spawn_pytest("--break=test_class_simple.py:4")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        ">*/test_class_simple.py(4)test_one()",
        "->*# line 4"
    ])
    pe.sendline("c")


def test_class_early(testdir_class):
    # target docstring
    pe = testdir_class.spawn_pytest("--break=test_class_early.py:2")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        ">*/test_class_early.py(4)test_one()",
        "->*# line 4"
    ])
    pe.sendline("c")


def test_class_gap(testdir_class):
    pe = testdir_class.spawn_pytest("--break=test_class_gap.py:6")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        ">*/test_class_gap.py(8)test_two()",
        "->*# line 8"
    ])
    pe.sendline("c")


# XXX while it's nice that this works, not sure it's expected behavior; should
# figure out why, clarify intent, and state explicitly somewhere (may need to
# learn more about collection internals)
def test_class_gap_named(testdir_class):
    pe = testdir_class.spawn_pytest(
        "--break=test_class_gap_named.py:6 "
        "test_class_gap_named.py::TestClass::test_two"
    )
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        ">*/test_class_gap_named.py(8)test_two()",
        "->*# line 8"
    ])
    pe.sendline("c")


if __name__ == "__main__" and not os.getenv("PDBBRK_HACK"):
    cmdline = ("pytest", "-p", "pytester", "--noconftest", __file__)
    if sys.platform.startswith("linux"):
        sys.stdout.flush()
        os.execlp("python", sys.executable, "-m", *cmdline)
    else:
        raise RuntimeError("The tests above aren't meant to be discovered."
                           " Try running:\n%s" % " ".join(cmdline))

# :let $PDBBRK_LOGFILE = "/dev/pts/7"
# (setenv "PDBBRK_LOGFILE" "/dev/pts/7")
