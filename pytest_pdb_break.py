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
import attr
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
    from helpers.logging_helper import LoggingHelper
    _logging_helper = LoggingHelper.from_logdefs("PDBBRK_LOGYAML")
    if _logging_helper:
        module_logger = _logging_helper.get_logger(__name__)
except Exception:
    module_logger = None


__version__ = "0.0.1"
pytestPDB = pytest.set_trace.__self__


@attr.s(frozen=True)
class BreakLoc:
    """Data object holding path-like filename, line number, test name.
    """
    file = attr.ib(converter=attr.converters.optional(lambda v:
                                                      Path(v) if v else None))
    lnum = attr.ib(validator=attr.validators.instance_of(int))
    name = attr.ib()

    class_name = attr.ib(default=None, repr=False, cmp=False, kw_only=True)
    func_name = attr.ib(default=None, repr=False, cmp=False, kw_only=True)
    param_id = attr.ib(default=None, repr=False, cmp=False, kw_only=True)

    def _replace(self, **kwargs):  # temporary
        for k, v in kwargs.items():
            object.__setattr__(self, k, v)
        return self

    @classmethod
    def from_pytest_item(cls, item):
        """Return a BreakLoc instance from a pytest Function item.

        Notes:
        1. ``pytest.Item.location`` line numbers are zero-indexed, but pdb
           breakpoints aren't, and neither are linecache's
        2. ``lnum`` may be ``-1``, as returned by ``.reportinfo()``
        3. ``name`` may be "Class.func[id]", with "id" accessible at
           ``.callspec.id`` and the rest at ``.originalname``
        4. ``name`` may be used later to target a specific call id; presently,
           it's only used by ``get_targets`` for sorting
        """
        file, lnum, name = item.location
        # Comment in ``Config.cwd_relative_nodeid`` says "nodeid's are relative
        # to the rootpath." Seems this also applies to .location names.
        assert not Path(file).is_absolute(), file
        file = item.config.rootdir.join(file)
        assert item.fspath == file
        kwargs = {}
        # TODO see if OK to save reference to item instead of just constants
        kwargs["func_name"] = item.function.__name__
        if item.cls:
            kwargs["class_name"] = item.cls.__name__
        if item.originalname:
            kwargs["param_id"] = item.callspec.id
        return cls(file, lnum + 1, str(name), **kwargs)

    @classmethod
    def from_arg_spec(cls, string):
        """Stash components of arg supplied to the --break option."""
        file, __, lnum = string.rpartition(":")
        # Pytest may be invoked from an editor via exec(), in which case these
        # might not get expanded.
        if file:
            # $TMPHOME/foo may be ~/.local/tmp/foo, so expand envvars first
            file = os.path.expanduser(os.path.expandvars(file))
        return cls(file if file else None, int(lnum), None)


def pytest_addoption(parser):
    group = parser.getgroup("pdb")
    # TODO consider using str() instead of passing constructor, which was done
    # initially so invalid args would trigger the usage/help msg.  However,
    # most errors involve file validation, which must happen later, once we
    # have access to the fully initialized config object.
    group.addoption("--break",
                    action="store",
                    metavar="[<file>:]<line-no>",
                    dest="pdb_break",
                    type=BreakLoc.from_arg_spec,
                    help="run the test enclosing <line-no> and break there; "
                    "<file> may be omitted if obvious")
    group.addoption("--bt-all",
                    action="store_true",
                    dest="pdb_break_bt_all",
                    help="include internal pytest frames in the bt stack; "
                    "default: %(default)s")


def pytest_configure(config):
    wanted = config.getoption("pdb_break")
    if wanted:
        PdbBreak(wanted, config)


class PdbBreak:
    """A class namespace for this plugin, registered as "pdb_break".

    Notes:
    1. the entrypoint mechanism registers the module_ itself as a plugin
    2. in printed pytest output (reports, etc.) the underscore may be
       replaced with a hyphen

    .. _module:
       https://pluggy.readthedocs.io/en/latest/#define-and-collect-hooks
    """

    def __init__(self, wanted, config):
        if module_logger:
            self._l = module_logger.helper.get_logger("PdbBreak")
        else:
            self._l = None
        config.pluginmanager.register(self, "pdb_break")
        self.bt_all = config.getoption("pdb_break_bt_all")
        self.config = config
        self.wanted = self._resolve_wanted(wanted)
        self.target = None
        self.last_pdb = None

    def _resolve_wanted(self, wanted):
        """Validate file component of user arg, if supplied"""
        if wanted.file is None or wanted.file.is_absolute():
            return wanted
        file = wanted.file
        try:
            resolved = file.resolve(True)
        except FileNotFoundError:
            if file.is_absolute():
                raise
            # Can't use Path.cwd() because its .samefile() calls .stat() on
            # other and expects returned obj to have .st_ino, but path.Stat
            # looks for st_st_ino
            # TODO find out if users of the plugin API are meant to fiddle with
            # LocalPath objects, or if these are only for internal use. A
            # cursory search in the CHANGELOG/bug tracker for terms like
            # pathlib, localpath, fspath, etc., didn't yield much, (occasional
            # references to "recent pathlib refactoring" circa mid 2018).
            cwd = type(self.config.rootdir)()
            for parent in (self.config.rootdir, self.config.invocation_dir):
                if not cwd.samefile(parent):
                    resolved = parent / file
                    if resolved.exists():
                        break
            else:
                raise
        return wanted._replace(file=resolved)

    if module_logger:
        def pytest_internalerror(self, excrepr, excinfo):
            self._l.pspell(1)

    if not hasattr(pytestPDB, "_init_pdb"):
        def pytest_enter_pdb(self, config, pdb):
            """Stash pytest-wrapped pdb instance."""
            self.last_pdb = pdb

    def pytest_runtestloop(self, session):
        """Find target or raise."""
        locs = [BreakLoc.from_pytest_item(i) for i in session.items]
        self._l and self._l.pspore(1)
        if not self.wanted.file:
            locs_files = set(l.file for l in locs)
            # If solo, assume good
            if len(locs_files) == 1:
                inferred = locs_files.pop()
                self.wanted = self.wanted._replace(file=inferred)
                self._l and self._l.pspore(2)
            else:
                raise RuntimeError("breakpoint file couldn't be determined")
        # TODO skip when cmdline arg is a single nodeid with function component
        self.targets = get_targets(self.wanted.file, self.wanted.lnum, locs)
        try:
            self.target = self.targets.popleft()
        except IndexError as exc:
            msg = "a valid breakpoint could not be determined"
            raise RuntimeError(msg) from exc
        self._l and self._l.pspore(3)

    def trace_handoff(self, frame, event, arg):
        """Defer to the "real" trace_dispatch after arriving at target.

        This exists to accommodate initial breakpoints that would normally
        "fall through" but are otherwise sensible.

        """
        if frame.f_code.co_filename != str(self.wanted.file):
            # This ~~~~~^ may be a description, e.g., <string>
            return self.trace_handoff

        self._l and self._l.prinspect(event=event, frame=frame)

        if (frame.f_code.co_name != self.target.func_name
                or event != "line"
                or frame.f_lineno < self.wanted.lnum):
            return self.trace_handoff

        inst = self.last_pdb
        self._l and self._l.sertall(1)
        # Reinstrument "backwards" to show pytest frames in stack
        if self.bt_all:
            _frame = frame
            while _frame.f_back:
                _frame.f_trace = inst.trace_dispatch
                _frame = _frame.f_back
            inst.botframe = _frame
        else:
            inst.botframe = frame

        # This is just for show, although it does make clear that this
        # breakpoint hasn't been saved
        inst.set_break(str(self.wanted.file), frame.f_lineno, True)
        self.target = None
        sys.settrace(inst.trace_dispatch)  # hand off
        return inst.dispatch_line(frame)

    def runcall_until(self, func, **testargs):
        """Run test with testargs, stopping at location.

        Exceptions raised inside this function will be reported as test
        failures. For testing, this means report output is sent to stdout
        rather than stderr (INTERNALERROR).
        """
        from _pytest.capture import capture_fixtures
        if hasattr(self, "pytest_enter_pdb"):
            pytestPDB.set_trace(set_break=False)
            self._l and self._l.sertall("with_enter_pdb")
            inst = self.last_pdb
        else:
            inst = self.last_pdb = pytestPDB._init_pdb()
        # XXX maybe only provide context for capsys, ignore others?
        try:
            capfix = (testargs.keys() & capture_fixtures).pop()
        except KeyError:
            capfix = None
        self._l and self._l.pspore("pre_capfix")
        if capfix:
            capman = self.config.pluginmanager.getplugin("capturemanager")
            self._l and self._l.pspore("cap_top")
            if capfix == "capsys" and not capman.is_globally_capturing():
                raise RuntimeError("Cannot break inside tests using capsys "
                                   "when global capturing is disabled")
            capfix = testargs[capfix]

            def preloop(_inst):
                # XXX runs after .setup, but global capture still active?
                super((type(_inst)), _inst).preloop()
                capman.suspend_global_capture(in_=True)

            def postloop(_inst):
                super((type(_inst)), _inst).postloop()
                capfix._resume()

            inst.__dict__["preloop"] = preloop.__get__(inst)
            inst.__dict__["postloop"] = postloop.__get__(inst)
            # TODO figure out why resumed state isn't already active
            # Maybe we have to run some hooks?
            capfix._resume()
            self._l and self._l.pspore("cap_bot")
        from bdb import BdbQuit
        inst.reset()
        inst.botframe = sys._getframe()
        self._l and self._l.pspell("post_capfix")
        inst._set_stopinfo(inst.botframe, None, 0)
        sys.settrace(self.trace_handoff)
        try:
            func(**testargs)
        except BdbQuit:
            pass
        finally:
            inst.quitting = True
            sys.settrace(None)
            self.last_pdb = None

    @pytest.hookimpl
    def pytest_pyfunc_call(self, pyfuncitem):
        if self._l:
            self._l.sertall(1)
            self._l.pspell(1)
        if BreakLoc.from_pytest_item(pyfuncitem) == self.target:
            # Mimic primary hookimpl in _pytest/python.py
            testargs = {arg: pyfuncitem.funcargs[arg] for
                        arg in pyfuncitem._fixtureinfo.argnames}
            self.runcall_until(pyfuncitem.obj, **testargs)
            #
            if self.target and self.targets:
                self.target = self.targets.popleft()
            self._l and self._l.pspell(2)
            # Skip primary hookimpl for this pyfuncitem
            return True


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
                # is called, so just include all callspec variants now
                out = [l for l in locs if l.lnum == locs[0].lnum] + out
        else:
            out += sorted(locs, key=lnumgetter)
    return deque(out)
