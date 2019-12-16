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
    # May have a short g-sha as 4th component
    _pytest_version = tuple(int(s) for s in pytest.__version__.split(".")[:3])
    if _pytest_version < (5, 0):
        raise RuntimeError("Requires at least pytest 5.0")
except ValueError:
    # Assume something non-standard like devNN+gdeadbeef
    _pytest_version = None

module_logger = None
try:
    from knotty_logger import LoggingHelper

    _logging_helper = LoggingHelper.from_logdefs("PDBBRK_LOGYAML")
    if _logging_helper:
        module_logger = _logging_helper.get_logger(__name__)
except Exception:
    module_logger = None


__version__ = "0.0.6"
pytestPDB = pytest.set_trace.__self__


@attr.s
class BreakLoc:
    """Data object holding path-like filename, line number, test name.
    """

    file = attr.ib(
        converter=attr.converters.optional(lambda v: Path(v) if v else None)
    )
    lnum = attr.ib(validator=attr.validators.instance_of(int))
    name = attr.ib()

    py_obj_kind = attr.ib(default=None, repr=False, eq=False, kw_only=True)
    class_name = attr.ib(default=None, repr=False, eq=False, kw_only=True)
    func_name = attr.ib(default=None, repr=False, eq=False, kw_only=True)
    param_id = attr.ib(default=None, repr=False, eq=False, kw_only=True)
    arg_name = attr.ib(default=None, repr=False, eq=False, kw_only=True)
    inner = attr.ib(default=None, repr=False, eq=False, kw_only=True)
    func = attr.ib(default=None, repr=False, eq=False, kw_only=True)

    def equals(self, other):
        """True if class, func, param fields are equal.
        This is the same as comparing ``attr.astuple()`` values but
        leaves room for the likely addition of extraneous attrs.
        """
        relevant = (
            "py_obj_kind", "class_name", "func_name",
            "param_id", "arg_name", "func"
        )
        return self == other and all(
            getattr(self, a) == getattr(other, a) for a in relevant
        )

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
    group.addoption(
        "--break",
        action="store",
        metavar="[FILE:]LINE-NO",
        dest="pdb_break",
        type=BreakLoc.from_arg_spec,
        help="run the test enclosing LINE-NO and break there; "
        "FILE may be omitted if obvious",
    )
    group.addoption(
        "--bt-all",
        action="store_true",
        dest="pdb_break_bt_all",
        help="include internal pytest frames in the navigable "
        "stack; default: %(default)s",
    )
    group.addoption(
        "--complete",
        action="store_true",
        dest="pdb_break_complete",
        help="complete object names in additon to commands; "
        "default: %(default)s",
    )


@pytest.hookimpl(trylast=True)
def pytest_configure(config):
    wanted = config.getoption("pdb_break")
    if not wanted or config.option.collectonly:
        return
    pdbtrace = config.pluginmanager.get_plugin("pdbtrace")
    pdbcls = config.pluginmanager.get_plugin("pdbtrace")
    if pdbtrace and config.pluginmanager.is_registered(pdbtrace):
        raise RuntimeError("--break is not compatible with --trace")
    if config.getoption("pdb_break_complete"):
        if pdbcls and config.pluginmanager.is_registered(pdbcls):
            raise RuntimeError("--complete is not compatible with --pdbcls")
        else:
            capman = config.pluginmanager.getplugin("capturemanager")
            wrapped_class = pytestPDB._import_pdb_cls(capman)
            # Can't use hasattr because all pdb.Pdb inherits from cmd.Cmd
            if "complete" in wrapped_class.__base__.__dict__:
                from warnings import warn

                cls = wrapped_class
                msg = (
                    "Ignoring option --complete because "
                    "{cls.__module__}.{cls.__name__}.complete is defined"
                ).format(cls=cls)
                warn(msg)
            else:
                add_completion(config)
    config.pluginmanager.register(PdbBreak(wanted, config), "pdb-break")


class PdbBreak:
    """A class namespace for this plugin, registered as "pdb-break".

    Note: the entrypoint mechanism registers the module_ itself as a
    plugin (meaning this module).

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
        self._l and self._l.pspell("bottom")

    def _resolve_wanted(self, wanted):
        """Validate file component of user arg, if supplied."""
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

    def _ensure_wanted(self, session):
        if self.wanted.file:
            return
        try:
            assert len(session.items) == 1
            self.wanted.file = BreakLoc.from_pytest_item(session.items[0]).file
        except Exception:
            msg = "unable to determine breakpoint file"
            raise RuntimeError(msg)
        else:
            self._l and self._l.pspore("rewrite")

    def get_fix_names_to_fix_defs(self, session):
        result = {}
        thisfile = str(self.wanted.file)
        for fixes in session._fixturemanager._arg2fixturedefs.values():
            for fix in fixes:
                if fix.func.__code__.co_filename != thisfile:
                    continue
                if fix.func.__code__.co_firstlineno > self.wanted.lnum:
                    continue
                result.setdefault(fix.func.__name__, []).append(fix)
        return result

    def get_func_names_to_func_items(self, session):
        result = {}
        thisfile = str(self.wanted.file)
        for item in session.items:
            if item.function.__code__.co_filename != thisfile:
                continue
            if item.function.__code__.co_firstlineno > self.wanted.lnum:
                continue
            result.setdefault(item.function.__name__, []).append(item)
        return result

    def find_targets(self, session):
        assert self.wanted.file
        from functools import partial
        func_names = self.get_func_names_to_func_items(session)
        self._l and self._l.pspore("top")

        fortified = fortify_location(
            self.wanted.file,
            self.wanted.lnum,
            func_names=func_names,
            fixtures_finder=partial(self.get_fix_names_to_fix_defs, session)
        )

        self._l and self._l.pspore("fortified")
        if not fortified:
            msg = "unable to determine breakpoint location"
            raise RuntimeError(msg)
        self.wanted = fortified

        if self.wanted.py_obj_kind == "item":
            self.targets = func_names[self.wanted.func_name]
            self._l and self._l.pspore("targets")
        else:
            assert self.wanted.py_obj_kind == "fixture"
            assert self.wanted.arg_name
            self.elsewhere = [
                i  # item *any* discovered module
                for i in session.items
                if self.wanted.arg_name in i.fixturenames
            ]

    def pytest_runtestloop(self, session):
        """Find a suitable target or raise RuntimeError

        See::

            PyCollector.funcnamefilter
            PyCollector._matches_prefix_or_glob_option

        And config options ``python_functions`` and ``python_classes``.

        """
        self._ensure_wanted(session)
        self.find_targets(session)

    def pytest_runtest_protocol(self, item, nextitem):
        if not self.elsewhere or item not in self.elsewhere:
            return None
        item.ihook.pytest_runtest_logstart(
            nodeid=item.nodeid, location=item.location
        )
        self.runcall_until(runtestprotocol, item=item, nextitem=nextitem)
        item.ihook.pytest_runtest_logfinish(
            nodeid=item.nodeid, location=item.location
        )
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

        if (
            event != "line"
            or frame.f_lineno < self.wanted.lnum
            or (
                self.wanted.inner and frame.f_code.co_name != self.wanted.inner
            )
            or (
                not self.wanted.inner
                and frame.f_code.co_name != self.wanted.func_name
            )
        ):
            return self.trace_handoff

        inst = self.last_pdb

        if self.bt_all or self.wanted.inner:
            bot = frame
            while bot.f_back:
                bot.f_trace = inst.trace_dispatch
                bot = bot.f_back
                if (
                    not self.bt_all
                    and bot.f_code.co_name == self.wanted.func_name
                ):
                    break
            inst.botframe = bot
        else:
            inst.botframe = frame

        # This is just for show, although it does make clear that this
        # breakpoint hasn't been saved
        inst.set_break(str(self.wanted.file), frame.f_lineno, True)
        sys.settrace(inst.trace_dispatch)  # hand off
        return inst.dispatch_line(frame)

    def _handle_capture(self, func, testargs, inst):
        # XXX this is currently a mess; assumes a lot, e.g., no rcLines
        from _pytest.capture import capture_fixtures

        capman = self.config.pluginmanager.getplugin("capturemanager")
        self._l and self._l.pspore("cap_top")

        common = testargs.keys() & capture_fixtures
        capfix = common.pop() if common else None
        if capfix:
            if capfix == "capsys" and not capman.is_globally_capturing():
                raise RuntimeError(
                    "Cannot break inside tests using capsys"
                    " while global capturing is disabled"
                )
            capfix = testargs[capfix]
        elif func is not runtestprotocol:
            return

        def preloop(_inst):
            # TODO figure out why suspend from pytest's pdb.setup() gets
            # undone. This usually runs soon afterwards.
            super((type(_inst)), _inst).preloop()
            capman._global_capturing.pop_outerr_to_orig()
            capman.suspend_global_capture(in_=True)

        def postloop(_inst):
            # Runs after do_* cmds that return 1, like next, step,
            # continue, until (but not jump)
            super((type(_inst)), _inst).postloop()
            capfix and capfix._resume()

        inst.__dict__["preloop"] = preloop.__get__(inst)
        inst.__dict__["postloop"] = postloop.__get__(inst)

        # TODO add test showing that fixture may be suspended; we need it
        # to continue capturing till handoff
        capfix and capfix._resume()
        self._l and self._l.pspore("cap_bot")

    def runcall_until(self, func, **testargs):
        """Run test with testargs, stopping at location.

        Exceptions raised inside this function may be reported as test
        failures. For testing, this means report output is sent to stdout
        rather than stderr (INTERNALERROR).
        """
        if hasattr(self, "pytest_enter_pdb"):
            pytestPDB.set_trace(set_break=False)
            self._l and self._l.sertall("with_enter_pdb")
            inst = self.last_pdb
        else:
            inst = self.last_pdb = pytestPDB._init_pdb("runcall_until")
        self._l and self._l.pspore("pre_capfix")

        self._handle_capture(func, testargs, inst)

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
            # FIXME should unwind f_trace when --bt-all passed
            inst.quitting = True
            sys.settrace(None)
            self.last_pdb = None

    @pytest.hookimpl(tryfirst=True)
    def pytest_runtest_call(self, item):
        if (
            hasattr(item, "startTest")
            and self.targets
            and item is self.targets[0]
        ):

            def _runtest(inst):
                self.runcall_until(inst._testcase, result=inst)
                if self.targets:
                    self.targets.pop(0)

            item.runtest = _runtest.__get__(item)

    # TODO if function is bound, ensure class name is correct
    def pytest_pyfunc_call(self, pyfuncitem):
        if self._l:
            self._l.sertall(1)
            self._l.pspell(1)
        if self.targets and pyfuncitem is self.targets[0]:
            # Mimic primary hookimpl in _pytest/python.py
            testargs = {
                arg: pyfuncitem.funcargs[arg]
                for arg in pyfuncitem._fixtureinfo.argnames
            }
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


def fortify_location(
    filename,
    line_no,
    func_names,  # dict mapping func names to items
    fixtures_finder,  # callable returning map of func names to FixtureDefs
):
    """Resolve args into a canonical BreakLoc object and return it

    'Canonical' meaning the ``BreakLoc.func`` attribute must be taken
    from a ``Item.function`` or ``FixtureDef.func``.  On failure, return
    None.  If relevant, the ``inner`` field must be set to the innermost
    enclosing function of arg ``line_no``.
    """
    try:
        root = ast.parse(Path(filename).read_text(), filename=filename)
    except Exception:
        return None
    leaf = _get_node_at_pos(line_no, root, None)
    kind = "item"
    targets = None

    def find(node, tipo):
        while node and type(node) not in tipo:
            if node is root:
                return None
            node = node.parent
        return node

    def resolve(scope, haystack):
        while True:
            found = scope and haystack.get(scope.name)
            if not scope or found:
                break
            scope = find(scope.parent, (ast.FunctionDef, ast.AsyncFunctionDef))
        return scope, found

    inner = find(leaf, (ast.FunctionDef, ast.AsyncFunctionDef))
    if not inner:
        return None

    outer, targets = resolve(inner, func_names)
    if outer:
        assert targets
    else:
        kind = "fixture"
        outer, targets = resolve(inner, fixtures_finder())
        if not targets:
            return None
        arg_name = {t.argname for t in targets}.pop()

    if not outer or outer is inner:
        outer, inner = inner, None

    cls = find(outer, (ast.ClassDef,))

    funcs = {o.function if kind == "item" else o.func for o in targets}
    assert len(funcs) == 1

    return BreakLoc(
        file=filename,
        lnum=line_no,
        name=None,
        py_obj_kind=kind,
        class_name=cls.name if cls else None,
        func_name=outer.name,
        param_id=None,
        arg_name=arg_name if kind == "fixture" else None,
        inner=inner.name if inner else None,
        func=funcs.pop()
    )


def add_completion(config):
    """Modify the original wrapped class with completion methods

    ``_pytest.debugging.pytestPDB._import_pdb_cls`` creates the
    ``pytestPDB._wrapped_pdb_cls`` attr, which holds the cleaned output
    from ``_validate_usepdb_cls`` (the value passed to the ``--pdbcls``
    option), as well as the wrapped class, in a tuple that looks like::

        ((modname, classname), <class wrapped>)

    It's only created once (via ``_get_pdb_wrapper_class``), even when
    the first element ``[0]`` is ``None``, which is case when pytest was
    not invoked with ``--pdbcls``.
    """
    module_logger and module_logger.sertall(1)
    usepdb_cls, wrapped = pytestPDB._wrapped_pdb_cls

    def restore():
        pytestPDB._wrapped_pdb_cls = usepdb_cls, wrapped

    config.add_cleanup(restore)
    # FIXME not idempotent; should bail or raise if already patched

    import cmd
    import rlcompleter
    from code import InteractiveConsole, InteractiveInterpreter  # lhs=subcls
    from itertools import takewhile, count, filterfalse

    the_usual = [
        InteractiveConsole.raw_input.__code__,  # normal stdin
        InteractiveInterpreter.runcode.__code__,  # editor hacks
        cmd.Cmd.onecmd.__code__,
    ]  # "not interactive" sentinel

    class PdbComplete(wrapped):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self._l = module_logger and module_logger.helper.get_logger(
                "PdbComplete"
            )

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
                    self._l and self._l.prinspect(
                        text=text, icon=icon, size=len(self._completions)
                    )
                try:
                    return self._completions[state]
                except IndexError:
                    return None
            except Exception:
                self._l and self._l.printback()
                return None

    pytestPDB._wrapped_pdb_cls = usepdb_cls, PdbComplete
