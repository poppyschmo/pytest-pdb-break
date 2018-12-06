import os
import sys
import pytest
from functools import namedtuple


try:
    if tuple(int(s) for s in pytest.__version__.split(".")) < (4, 0):
        raise RuntimeError("Requires at least pytest 4.0")
except ValueError:
    pass  # assume some git-describe-like string: devNN+gdeadbeef

if sys.version_info < (3, 6):
    raise RuntimeError("For now, requires Python 3.6+")


# Note: if later adding function-based breaks, see ``compat.getlocation``
class BpLoc(namedtuple("BreakpointLocation", "file lnum name")):
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
        return cls(file, int(lnum), None)


def pytest_addoption(parser):
    group = parser.getgroup("pdb")
    group.addoption("--pdbat",
                    action="store",
                    metavar="<file:lnum>",
                    dest="usepdbat",
                    type=BpLoc,
                    help="One [<file>:<line-number>] pair, like foo.py:42")


def pytest_configure(config):
    wanted = config.getoption("usepdbat")
    # Using free-floating hooks instead of a class would mean having to repeat
    # this check in every definition
    if wanted:
        PdbAt(wanted, config)


class PdbAt:
    debug = False
    logfile = None

    # Config.option is an argparse.Namespace mapping proxy, i.e., read only.
    # So maintain an updatable copy in instance.wanted.
    def __init__(self, wanted, config):
        config.pluginmanager.register(self, "pdbat")
        self.capman = config.pluginmanager.getplugin("capturemanager")
        self.config = config
        self.wanted = wanted
        self.target = None
        self.debug = os.getenv("PDBAT_DEBUG") and True  # 0/false/off are True
        if self.debug:
            logfile = os.getenv("PDBAT_LOGFILE")
            if logfile:
                self.logfmt = "[{}] {} -- {}"
                if logfile.startswith("/dev"):
                    logfile = open(logfile, "w")
                    if sys.platform.startswith("linux"):
                        assert os.isatty(logfile.fileno())
                        self.logfmt = "[\x1b[1m{}\x1b[m] {} -- {}"
                else:
                    logfile = open(logfile, "a")
                config.add_cleanup(logfile.close)
                self.logfile = logfile
                self.printone()
            else:
                self.logfmt = "\x1b[1m{}\x1b[m -- {}"
        self.last_pdb = None
        self.last_func = None

    def pytest_internalerror(self, excrepr, excinfo):
        if self.debug and self.logfile:  # already prints to tw w/o cap
            self.prinspect("internalerror", excinfo.type,
                           # excrepr=excrepr,
                           value=excinfo.value)

    def printone(self, *args, **kwargs):
        if self.logfile:
            print(*args, **kwargs, file=self.logfile, flush=True)
        else:
            print(*args, **kwargs)

    def prinspect(self, where, header, *forms, locals=None, **kwforms):
        """Temporary print statement crutch."""
        # Can't figure out how to use pdb to inspect these hooks.
        # And GDB python is currently crocked on my system:
        # https://bugzilla.redhat.com/show_bug.cgi?id=1639242
        assert self.capman
        if self.logfile:
            from datetime import datetime
            payload = [self.logfmt.format(datetime.now(), where, header)]
        else:
            payload = [self.logfmt.format(where, header)]
        fmtstr = "{}: {}"
        payload += [fmtstr.format(*((f, eval(f, locals))
                                    if isinstance(f, str) else f))
                    for f in forms] + [fmtstr.format(k, v)
                                       for k, v in kwforms.items()]
        if self.logfile:
            print(*payload, sep="\n", file=self.logfile, flush=True)
        else:
            with self.capman.global_and_fixture_disabled():
                print(*payload, sep="\n")

    def pytest_runtestloop(self, session):
        """Find target or raise."""
        if not self.wanted or not any(self.wanted):
            return
        locs = [BpLoc(i) for i in session.items]
        if self.debug:
            self.prinspect("runtestloop", session.name,
                           wanted=self.wanted, locs=locs)
        if self.wanted.file:
            if not os.path.isfile(self.wanted.file):
                raise RuntimeError("file '%s' not found" % self.wanted.file)
        else:
            locs_files = set(l.file for l in locs)
            # If solo, assume good
            if len(locs_files) == 1:
                self.wanted = self.wanted._replace(file=locs_files.pop())
                if self.debug:
                    self.printone("wanted.file: '' -> {}"
                                  .format(self.wanted.file))
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
                self.printone("wanted.lnum: {} -> {}"
                              .format(self.wanted.lnum, lnum))
            self.wanted = self.wanted._replace(lnum=lnum)
        if self.debug:
            self.printone("target: {}".format(self.target))

    @pytest.hookimpl(hookwrapper=True)
    def pytest_fixture_setup(self, fixturedef, request):
        # Note: calling resolve_fixture_function raises exc here
        if self.debug:
            self.prinspect("fixture_setup", fixturedef.argname,
                           "fixturedef", "request",
                           "request.instance",
                           func=fixturedef.func,
                           locals=locals())
        yield

    def runcall_until(self, *args, **kwargs):
        """Run test with args, stopping at location.

        Note: the default ``pytest_pyfunc_call`` in
        ``_pytest/python.py`` doesn't use the return value.
        """
        assert self.last_pdb
        assert self.last_func
        inst = self.last_pdb
        func = self.last_func
        inst.set_break(self.wanted.file, self.wanted.lnum, True)
        if self.debug and self.logfile:
            self.prinspect("runcall_until", func,
                           ("this frame", sys._getframe()),
                           f_back=sys._getframe().f_back,
                           breaks=inst.breaks,
                           locals=locals())
        from bdb import BdbQuit
        res = None
        inst.reset()
        inst._set_stopinfo(sys._getframe(), None, -1)
        # ``inst.botframe`` is set to ^^^ for us
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

    def pytest_enter_pdb(self, config, pdb):
        """Stash modified/wrapped pdb instance.
        This workaround is necessary because ``pytestPDB.set_trace``
        doesn't return the instance.
        """
        self.last_pdb = pdb

    @pytest.hookimpl(hookwrapper=True)
    def pytest_pyfunc_call(self, pyfuncitem):
        assert self.last_pdb is None
        assert self.last_func is None
        assert not pyfuncitem._isyieldedfunction()
        # import ctypes; ctypes.string_at(0xffffffff) # gdb breakpoint
        if self.debug:
            from inspect import signature
            self.prinspect("pyfunc_call",
                           "{}{}".format(pyfuncitem.name,
                                         signature(pyfuncitem.obj)),
                           ("loc equals target",
                            BpLoc(pyfuncitem) == self.target),
                           "pyfuncitem.location",
                           "pyfuncitem.funcargs",
                           # "pyfuncitem._fixtureinfo",
                           locals=locals())
        # Note: seems like nextitem is always None
        if BpLoc(pyfuncitem) == self.target:
            # Copied from PdbInvoke and PdbTrace in _pytest/debugging.py
            self.capman.suspend_global_capture(in_=True)
            out, err = self.capman.read_global_capture()
            sys.stdout.write(out)
            sys.stdout.write(err)
            pytest.set_trace(set_break=False)
            self.last_func = pyfuncitem.obj
            pyfuncitem.obj = self.runcall_until
        yield


def find_breakable_line(filename, lineno):
    """Find the next breakable line.
    Like ``pdb.Pdb.checkline`` but without frame awareness.
    """
    import linecache
    # bdb.canonic without caching or angle-bracket guard
    filename = os.path.abspath(filename)  # skip normcase for now
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


def last_internal_error(result):
    return next(l for l in reversed(result.stdout.lines[-5:]) if
                l.startswith("INTERNALERROR"))


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
    result = td.runpytest("--pdbat=test_invalid_arg.py")
    assert "usage" in result.stderr.lines[0]
    assert "--pdbat" in result.stderr.lines[1]
    assert "invalid BpLoc value" in result.stderr.lines[1]

    # Non-existent file
    result = td.runpytest("--pdbat=foo:99")
    assert result.ret == 3
    assert "RuntimeError: file 'foo' not found" in last_internal_error(result)

    # Ambiguous case: no file named, but multiple given
    td.makepyfile(test_otherfile="""
        def test_bar():
            assert True
    """)
    result = td.runpytest("--pdbat=1")
    assert result.ret == 3
    assert ("RuntimeError: breakpoint file couldn't be determined" in
            last_internal_error(result))

    # No file named, but pytest arg names one
    pe = td.spawn_pytest("--pdbat=1 test_otherfile.py")  # <- Two sep args
    # XXX API is different for these spawning funcs (string)
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert befs[-1].lstrip().startswith("->")
    assert befs[-1].endswith("assert True")
    assert befs[-2].endswith("/test_otherfile.py(2)test_bar()")
    pe.sendline("c")  # requested line incremented ^^


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
    # Normal breakpoint
    pe = testdir_two_funcs.spawn_pytest("--pdbat=test_two_funcs_simple.py:4")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert befs[-1].lstrip().startswith("->")
    assert befs[-1].endswith("# <- line 4")
    assert befs[-2].endswith("/test_two_funcs_simple.py(4)test_true_int()")
    pe.sendline("c")


def test_two_funcs_comment(testdir_two_funcs):
    # Comment
    pe = testdir_two_funcs.spawn_pytest("--pdbat=test_two_funcs_comment.py:2")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert befs[-1].endswith("somevar = True")
    assert befs[-2].endswith("/test_two_funcs_comment.py(3)test_true_int()")
    pe.sendline("c")


def test_two_funcs_gap(testdir_two_funcs):
    # Empty line between funcs
    pe = testdir_two_funcs.spawn_pytest("--pdbat=test_two_funcs_gap.py:5")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert befs[-1].endswith("isinstance(False, int)")
    # Advances to first breakable line next func
    assert befs[-2].endswith("/test_two_funcs_gap.py(7)test_false_int()")
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
    pe = testdir_setup.spawn_pytest("--pdbat=test_one_arg.py:8")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert befs[-2].endswith("/test_one_arg.py(8)test_string()")
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
    pe = testdir_setup.spawn_pytest("--pdbat=test_mark_param.py:6 "
                                    "--capture=no")
    pe.expect(prompt_re)
    befs = unansi(pe.before)
    assert "one" in befs
    pe.sendline("c")  # If called again, would get timeout error


if __name__ == "__main__":
    # Some print statements only show up when a logfile envvar is present
    #
    # Vim:
    #   :let $PDBAT_DEBUG = "1" | let $PDBAT_LOGFILE = "/dev/pts/7"
    #
    # Emacs:
    #   (progn (setenv "PDBAT_DEBUG" "1")
    #          (setenv "PDBAT_LOGFILE" "/tmp/pdbat.log"))
    #
    if not os.getenv("PDBAT_DEVELOP"):
        cmdline = ("pytest", "-p", "pytester", "--noconftest", __file__)
        if sys.platform.startswith("linux"):
            sys.stdout.flush()
            os.execlp("python", sys.executable, "-m", *cmdline)
        else:
            raise RuntimeError("The tests above aren't meant to be discovered."
                               " Try running:\n%s" % " ".join(cmdline))
