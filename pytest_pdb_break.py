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
import ast

import pytest
import attr
from pathlib import Path
from _pytest.runner import runtestprotocol

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


@attr.s
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
    decked = attr.ib(default=None, repr=False, cmp=False, kw_only=True)

    def equals(self, other):
        """True if class, func, param fields are equal.
        This is the same as comparing ``attr.astuple()`` values but
        leaves room for the likely addition of extraneous attrs.
        """
        relevant = ("class_name", "func_name", "param_id")
        return (self == other
                and all(getattr(self, a) == getattr(other, a) for
                        a in relevant))

    @classmethod
    def from_pytest_item(cls, item):
        """Return a BreakLoc instance from a pytest Function item.

        Notes:
        1. ``pytest.Item.location`` line numbers are zero-indexed, but pdb
           breakpoints aren't, and neither are linecache's
        2. ``lnum`` may be ``-1``, as returned by ``.reportinfo()``
        3. ``name`` may be "Class.func[id]", with "id" accessible at
           ``.callspec.id`` and the rest at ``.originalname``
        4. ``name`` is presently unused
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
                    metavar="[FILE:]LINE-NO",
                    dest="pdb_break",
                    type=BreakLoc.from_arg_spec,
                    help="run the test enclosing LINE-NO and break there; "
                    "FILE may be omitted if obvious")
    group.addoption("--bt-all",
                    action="store_true",
                    dest="pdb_break_bt_all",
                    help="include internal pytest frames in the navigable "
                    "stack; default: %(default)s")
    group.addoption("--complete",
                    action="store_true",
                    dest="pdb_break_complete",
                    help="complete object names in additon to commands; "
                    "default: %(default)s")


@pytest.hookimpl(trylast=True)
def pytest_configure(config):
    wanted = config.getoption("pdb_break")
    if not wanted:
        return
    pdbtrace = config.pluginmanager.get_plugin("pdbtrace")
    pdbcls = config.pluginmanager.get_plugin("pdbtrace")
    if pdbtrace and config.pluginmanager.is_registered(pdbtrace):
        raise RuntimeError("--break is not compatible with --trace")
    if config.getoption("pdb_break_complete"):
        if pdbcls and config.pluginmanager.is_registered(pdbcls):
            raise RuntimeError("--complete is not compatible with --pdbcls")
        elif "complete" in pytestPDB._pdb_cls.__dict__:
            from warnings import warn
            cls = pytestPDB._pdb_cls
            warn("Ignoring option --complete because "
                 f"{cls.__module__}.{cls.__name__}.complete is defined")
        else:
            add_completion(config)
    config.pluginmanager.register(PdbBreak(wanted, config), "pdb_break")


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
        self.bt_all = config.getoption("pdb_break_bt_all")
        self.config = config
        self.wanted = self._resolve_wanted(wanted)
        self.targets = None
        self.elsewhere = None
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
            cwd = type(self.config.rootdir)()
            for parent in (self.config.rootdir, self.config.invocation_dir):
                if not cwd.samefile(parent):
                    resolved = parent / file
                    if resolved.exists():
                        break
            else:
                raise
        wanted.file = resolved
        return wanted

    if module_logger:
        def pytest_internalerror(self, excrepr, excinfo):
            self._l.pspell(1)

    if not hasattr(pytestPDB, "_init_pdb"):
        def pytest_enter_pdb(self, config, pdb):
            """Stash pytest-wrapped pdb instance."""
            self.last_pdb = pdb

    def pytest_runtestloop(self, session):
        """Find a suitable target or raise RuntimeError."""
        locs = [BreakLoc.from_pytest_item(i) for i in session.items]
        self._l and self._l.pspore("enter")
        if not self.wanted.file:
            locs_files = set(l.file for l in locs)
            if len(locs_files) == 1:
                inferred = locs_files.pop()
                self.wanted.file = inferred
                self._l and self._l.pspore("rewrite")
            else:
                msg = "unable to determine breakpoint file"
                raise RuntimeError(msg)
        fortified = fortify_location(self.wanted.file, self.wanted.lnum)
        if not fortified:
            msg = "unable to determine breakpoint location"
            raise RuntimeError(msg)
        self.wanted = fortified
        self._l and self._l.pspore("fortified")
        # A test item's function name matches that of self.wanted
        if self.wanted.func_name.startswith("test_"):
            self.targets = [l for l in locs if
                            l.file == self.wanted.file and
                            l.func_name == self.wanted.func_name]
            if not self.targets:
                msg = "unable to determine breakpoint item"
                raise RuntimeError(msg)
            self._l and self._l.pspore("targets")
        elif (not session.config.option.collectonly
              and self.wanted.decked
              and self.wanted.func_name in
              session._fixturemanager._arg2fixturedefs):
            self.elsewhere = [i for i in session.items if
                              self.wanted.func_name in i.fixturenames]
        else:
            msg = "unable to handle breakpoints outside of test functions"
            raise RuntimeError(msg)

    def pytest_runtest_protocol(self, item, nextitem):
        if not self.elsewhere or item not in self.elsewhere:
            return None
        item.ihook.pytest_runtest_logstart(nodeid=item.nodeid,
                                           location=item.location)
        self.runcall_until(runtestprotocol, item=item, nextitem=nextitem)
        item.ihook.pytest_runtest_logfinish(nodeid=item.nodeid,
                                            location=item.location)
        self.elsewhere.remove(item)
        return True

    def trace_handoff(self, frame, event, arg):
        """Defer to the "real" trace_dispatch after arriving at target.

        This exists to accommodate initial breakpoints that would normally
        "fall through" but are otherwise sensible.
        """
        if frame.f_code.co_filename != str(self.wanted.file):
            # This ~~~~~^ may be a description, e.g., <string>
            return self.trace_handoff

        self._l and self._l.prinspect(event=event, frame=frame)

        if (event != "line"
                or frame.f_code.co_name != self.wanted.func_name
                or frame.f_lineno < self.wanted.lnum):
            return self.trace_handoff

        inst = self.last_pdb
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
        if capfix or func is runtestprotocol:
            capman = self.config.pluginmanager.getplugin("capturemanager")
            self._l and self._l.pspore("cap_top")
            if capfix:
                if capfix == "capsys" and not capman.is_globally_capturing():
                    raise RuntimeError("Cannot break inside tests using capsys"
                                       " while global capturing is disabled")
                capfix = testargs[capfix]

            def preloop(_inst):
                super((type(_inst)), _inst).preloop()
                capman._global_capturing.pop_outerr_to_orig()
                capman.suspend_global_capture(in_=True)

            def postloop(_inst):
                super((type(_inst)), _inst).postloop()
                capfix and capfix._resume()

            inst.__dict__["preloop"] = preloop.__get__(inst)
            inst.__dict__["postloop"] = postloop.__get__(inst)
            # TODO add test to show that resumed isn't already active
            capfix and capfix._resume()
            self._l and self._l.pspore("cap_bot")
        from bdb import BdbQuit
        inst.reset()
        self._l and self._l.pspell("post_capfix")
        sys.settrace(self.trace_handoff)
        try:
            func(**testargs)
        except BdbQuit:
            pass
        except Exception as exc:
            from _pytest.outcomes import Exit
            if not isinstance(exc, Exit) or exc.msg != "Quitting debugger":
                self._l and self._l.printback()
                raise
        finally:
            inst.quitting = True
            sys.settrace(None)
            self.last_pdb = None

    @pytest.hookimpl(tryfirst=True)
    def pytest_runtest_call(self, item):
        if (hasattr(item, "startTest")
                and self.targets
                and BreakLoc.from_pytest_item(item) == self.targets[0]):

            def _runtest(inst):
                self.runcall_until(inst._testcase, result=inst)
                if self.targets:
                    self.targets.pop(0)

            item.runtest = _runtest.__get__(item)

    def pytest_pyfunc_call(self, pyfuncitem):
        if self._l:
            self._l.sertall(1)
            self._l.pspell(1)
        if (self.targets
                and BreakLoc.from_pytest_item(pyfuncitem) == self.targets[0]):
            # Mimic primary hookimpl in _pytest/python.py
            testargs = {arg: pyfuncitem.funcargs[arg] for
                        arg in pyfuncitem._fixtureinfo.argnames}
            self.runcall_until(pyfuncitem.obj, **testargs)
            #
            if self.targets:
                self.targets.pop(0)
            self._l and self._l.pspell(2)
            # Skip primary hookimpl for this pyfuncitem
            return True


def _get_node_at_pos(line_no, node, parent=None):
    """Return first ast node at line_no.
    This is ``ast.NodeVisitor.generic_visit`` with extra breadcrumb
    business for cycling back later, if necessary.
    """
    node.parent = parent
    if hasattr(node, "lineno") and node.lineno >= line_no:
        return node
    for field, value in ast.iter_fields(node):
        rv = None
        if isinstance(value, list):
            for item in value:
                if isinstance(item, ast.AST):
                    rv = _get_node_at_pos(line_no, item, node)
                    if rv:
                        return rv
        elif isinstance(value, ast.AST):
            rv = _get_node_at_pos(line_no, value, node)
            if rv:
                return rv


def fortify_location(filename, line_no):
    """Try to flesh out location with more specific info.
    On success, return new BreakLoc object. Otherwise, return None.
    """
    try:
        root = ast.parse(Path(filename).read_text(), filename=filename)
    except Exception:
        return None
    leaf = _get_node_at_pos(line_no, root, None)

    def find(node, tipo):
        while type(node) is not tipo:
            if node is root:
                return None
            node = node.parent
        return node

    func = find(leaf, ast.FunctionDef)
    if func is None:
        return None

    cand = func
    # Fails when inner func is named test_*
    while cand and not cand.name.startswith("test_"):
        cand = find(cand.parent, ast.FunctionDef)
    if cand:
        func = cand

    cls = find(func, ast.ClassDef)

    return BreakLoc(file=filename, lnum=line_no, name=None,
                    class_name=cls.name if cls else None,
                    func_name=func.name,
                    param_id=None,
                    decked=bool(func.decorator_list))


def add_completion(config):
    """Replace pytestPDB._pdb_cls with a completion-enabled subclass.
    """
    module_logger and module_logger.sertall(1)
    orig = pytestPDB._pdb_cls

    def restore():
        pytestPDB._pdb_cls = orig

    config.add_cleanup(restore)

    import cmd
    import rlcompleter
    from code import InteractiveConsole, InteractiveInterpreter  # lhs=subcls
    from itertools import takewhile, count, filterfalse
    the_usual = [InteractiveConsole.raw_input.__code__,  # normal stdin
                 InteractiveInterpreter.runcode.__code__,  # editor hacks
                 cmd.Cmd.onecmd.__code__]  # "not interactive" sentinel

    class PdbComplete(orig):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self._l = (module_logger and
                       module_logger.helper.get_logger("PdbComplete"))

        def complete(self, text, state):
            """Dispense object and command matches.
            State caching stolen from pdb++ <https://github.com/antocuni/pdb>
            """
            try:
                if state == 0:
                    ns = self.curframe.f_globals.copy()
                    ns.update(self.curframe.f_locals)
                    whence = sys._getframe().f_back
                    while whence.f_back and whence.f_code not in the_usual:
                        whence = whence.f_back
                    icon = whence.f_locals.get("self")
                    if icon and isinstance(icon, InteractiveConsole):
                        ns.update(icon.locals)
                    else:
                        icon = None
                    cp = rlcompleter.Completer(ns).complete
                    first = takewhile(bool, (cp(text, m) for m in count()))
                    self._completions = list(first)
                    if icon is None:
                        cp = super().complete  # cmd.Cmd
                        rest = takewhile(bool, (cp(text, m) for m in count()))
                        dif = filterfalse(self._completions.__contains__, rest)
                        self._completions.extend(dif)
                    self._l and self._l.prinspect(text=text, icon=icon,
                                                  size=len(self._completions))
                try:
                    return self._completions[state]
                except IndexError:
                    return None
            except Exception:
                self._l and self._l.printback()
                return None

    pytestPDB._pdb_cls = PdbComplete
