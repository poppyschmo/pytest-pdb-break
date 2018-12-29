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
import os
import sys
import pytest
from functools import namedtuple
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


pytestPDB = pytest.set_trace.__self__


class BreakLoc(namedtuple("BreakpointLocation", "file lnum name")):
    """Data object holding filename, line number, test name."""
    __slots__ = ()

    def __new__(cls, *args, **kwargs):
        if len(args) == 1:
            item = args[0]
            if isinstance(item, str):
                return cls.from_cmd_option_arg_spec(item)
            elif hasattr(item, "location"):
                return cls.from_pytest_item(item)
        return super().__new__(cls, *args, **kwargs)

    if module_logger:
        def __repr__(self):
            if hasattr(self.file, "parent"):
                return super().__repr__().replace(str(self.file.parent), "…")
            return super().__repr__()

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
                    metavar="[<file>:]<line-no>",
                    dest="pdb_break",
                    type=BreakLoc,
                    help="run the test enclosing <line-no> and break there; "
                    "<file> may be omitted if obvious")


def pytest_configure(config):
    wanted = config.getoption("pdb_break")
    if wanted:
        PdbBreak(wanted, config)


class PdbBreak:
    debug = False

    def __init__(self, wanted, config):
        if module_logger:
            self._l = LoggingHelper.get_logger("PdbBreak")
            self.debug = True
        config.pluginmanager.register(self, "pdb_break")
        self.capman = config.pluginmanager.getplugin("capturemanager")
        self.config = config
        self.wanted = self._resolve_wanted(wanted)
        self.target = None
        self.last_pdb = None
        self.last_func = None

    def _resolve_wanted(self, wanted):
        if not wanted.file:
            return wanted
        file = wanted.file.expanduser()
        rootdir = Path(self.config.rootdir)
        assert rootdir.is_absolute()
        try:
            resolved = file.resolve(True)
        except FileNotFoundError:
            if Path().cwd().samefile(rootdir):
                raise
            resolved = (rootdir / file).resolve(True)
        return wanted._replace(file=resolved)

    def pytest_internalerror(self, excrepr, excinfo):
        if self.debug:  # already prints to tw w/o cap
            self._l.prinspot(1)

    def pytest_runtestloop(self, session):
        """Find target or raise."""
        locs = [BreakLoc(i) for i in session.items]
        self.debug and self._l.prinspotl(1)
        if not self.wanted.file:
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

    if not hasattr(pytestPDB, "_init_pdb"):
        def pytest_enter_pdb(self, config, pdb):
            """Stash pytest-wrapped pdb instance."""
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
        if hasattr(self, "pytest_enter_pdb"):
            pytestPDB.set_trace(set_break=False)
            if self.debug:
                assert self.last_pdb
            inst = self.last_pdb
        else:
            inst = self.last_pdb = pytestPDB._init_pdb()
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
