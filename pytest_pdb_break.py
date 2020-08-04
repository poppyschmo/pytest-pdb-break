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
import asyncio

import attr
import pytest

from pathlib import Path

from _pytest.runner import runtestprotocol

try:
    # TODO see if ini option for min pytest version already does this
    _pytest_version = tuple(int(s) for s in pytest.__version__.split(".")[:3])
    if _pytest_version < (6, 0):
        raise RuntimeError("Requires at least pytest 6.0")
except ValueError:
    # Assume failed because something like +g1234abcd present (see PEP 440)
    # Maybe also try distutils.version's parser, if normally available
    _pytest_version = None

__version__ = "0.0.10"

pytestPDB = pytest.set_trace.__self__


@attr.s
class BreakLocation:
    """Object holding path-like filename, line number"""

    file = attr.ib(
        converter=attr.converters.optional(lambda v: Path(v) if v else None)
    )
    lnum = attr.ib(validator=attr.validators.instance_of(int))

    @classmethod
    def from_arg_spec(cls, string):
        """Make minimal instance from arg supplied to --break"""
        file, __, lnum = string.rpartition(":")
        # These won't get expanded when invoked via exec()
        if file:
            # $TMPHOME/foo may be ~/.local/tmp/foo, so expand envvars first
            file = os.path.expanduser(os.path.expandvars(file))
        return cls(file if file else None, int(lnum))


def pytest_addoption(parser):
    group = parser.getgroup("pdb")
    group.addoption(
        "--break",
        action="store",
        metavar="[FILE:]LINE-NO",
        dest="pdb_break",
        type=BreakLocation.from_arg_spec,
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


def _get_modpath_from_pytest_item(item):
    file, *_ = item.location
    assert not Path(file).is_absolute(), file
    file = item.config.rootdir.join(file)
    assert item.fspath == file
    return Path(file)


def _resolve_wanted_file(config, path):
    """Return validated path component of user arg."""
    if path is None or path.is_absolute():
        return path
    try:
        resolved = path.resolve(True)
    except FileNotFoundError:
        if path.is_absolute():
            raise
        cwd = type(config.rootdir)()
        for parent in (config.rootdir, config.invocation_dir):
            if not cwd.samefile(parent):
                resolved = parent / path
                if resolved.exists():
                    break
        else:
            raise
    return Path(resolved)


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

    wanted.file = _resolve_wanted_file(config, wanted.file)
    config.pluginmanager.register(PdbBreak(wanted, config), "pdb-break")


class PdbBreak:
    """A class namespace for this plugin, registered as "pdb-break".

    Note: the entrypoint mechanism registers the module_ itself as a
    plugin (meaning this module).

    .. _module:
       https://pluggy.readthedocs.io/en/latest/#define-and-collect-hooks
    """
    _l = None
    tinfo = None
    targets = None
    last_pdb = None
    elsewhere = None

    def __init__(self, wanted, config):
        self.bt_all = config.getoption("pdb_break_bt_all")
        self.pt_aio = config.pluginmanager.getplugin("asyncio")
        self.config = config
        self.wanted = wanted

    def _ensure_wanted(self, session):
        if self.wanted.file:
            return
        try:
            assert len(session.items) == 1
            self.wanted.file = _get_modpath_from_pytest_item(session.items[0])
        except Exception:
            msg = "unable to determine breakpoint file"
            raise RuntimeError(msg)

    def map_func_info_to_fix_defs(self, session):
        """Map fixture factory info to lists of FixtureDef objects

        Keys are tuples of name and line number.
        """
        result = {}
        thisfile = str(self.wanted.file)
        for fixes in session._fixturemanager._arg2fixturedefs.values():
            for fix in fixes:
                if fix.func.__code__.co_filename != thisfile:
                    continue
                if fix.func.__code__.co_firstlineno > self.wanted.lnum:
                    continue
                result.setdefault(_get_func_key(fix.func), []).append(fix)
        return result

    def map_func_info_to_items(self, session):
        """Map function or method info to lists of Item objects

        Keys are tuples of name and line number.
        """
        result = {}
        thisfile = str(self.wanted.file)
        for item in session.items:
            if item.function.__code__.co_filename != thisfile:
                continue
            if item.function.__code__.co_firstlineno > self.wanted.lnum:
                continue
            result.setdefault(_get_func_key(item.function), []).append(item)
        return result

    def find_targets(self, session):
        from functools import partial

        assert self.wanted.file
        func_items = self.map_func_info_to_items(session)

        fortified = fortify_location(
            self.wanted.file,
            self.wanted.lnum,
            func_items=func_items,
            fixtures_finder=partial(self.map_func_info_to_fix_defs, session)
        )

        if not fortified:
            msg = "unable to determine breakpoint location"
            raise RuntimeError(msg)
        self.tinfo = fortified

        if self.tinfo.py_obj_kind == "item":
            self.targets = func_items[
                (self.tinfo.func_name, self.tinfo.func_lnum)
            ]
        else:
            assert self.tinfo.py_obj_kind == "fixture"
            assert self.tinfo.arg_name
            self.elsewhere = [
                i  # item *any* discovered module
                for i in session.items
                if self.tinfo.arg_name in i.fixturenames
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

        if (
            event != "line"
            or frame.f_lineno < self.wanted.lnum
            or (
                self.tinfo.inner and frame.f_code.co_name != self.tinfo.inner
            )
            or (
                not self.tinfo.inner
                and frame.f_code.co_name != self.tinfo.func_name
            )
        ):
            return self.trace_handoff

        inst = self.last_pdb

        if self.bt_all or self.tinfo.inner:
            bot = frame
            while bot.f_back:
                bot.f_trace = inst.trace_dispatch
                bot = bot.f_back
                if (
                    not self.bt_all
                    and bot.f_code.co_name == self.tinfo.func_name
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
        capman = self.config.pluginmanager.getplugin("capturemanager")
        assert capman is inst._pytest_capman
        capfix = capman._capture_fixture

        if capfix:
            name = capfix.request.fixturename
            known = {"capfd", "capfdbinary", "capsys", "capsysbinary"}
            if len(known & testargs.keys()) != 1:  # set_fixture() already does
                raise RuntimeError("Can't handle fixture %s" % name)
            assert testargs[name] is capfix

            if name == "capsys" and not capman.is_globally_capturing():  # "no"
                raise RuntimeError(
                    "Cannot break inside tests using capsys"
                    " while global capturing is disabled"
                )
        elif func is not runtestprotocol:
            return

        # For debugging, use print(..., file=_inst.stdout, flush=True)
        def preloop(_inst):
            # pytest pdb.setup() suspends when inst._continue is on
            super((type(_inst)), _inst).preloop()
            if not capfix:
                capman._global_capturing.pop_outerr_to_orig()
            else:
                assert capman._global_capturing.out._state == "suspended"
                assert sys.stdout is capfix._capture.out.tmpfile
            # Bounce global capture
            capman.resume_global_capture()
            capman.suspend_global_capture(in_=True)
            if capfix:
                assert sys.stdout is not capfix._capture.out.tmpfile
            if capman._method == "fd":
                global_out_fd = capman._global_capturing.out.targetfd
                assert _inst.stdout.fileno() == global_out_fd

        def assert_state(state):
            cap = capfix._capture
            assert cap.out._state == cap.err._state == state

        def postcmd(_inst, stop, line):
            # Runs after do_* cmds, stop is 1 for next, step, continue, until
            super((type(_inst)), _inst).postcmd(stop, line)
            # Bounce capture fixture
            assert_state("started")
            capman.suspend_fixture()
            assert_state("suspended")
            capman.resume_fixture()
            assert_state("started")
            if stop:
                inst.__dict__.pop("preloop") and inst.__dict__.pop("postcmd")
            return stop

        inst.__dict__["preloop"] = preloop.__get__(inst)
        if capfix:
            inst.__dict__["postcmd"] = postcmd.__get__(inst)

        # TODO add test showing that fixture may be suspended; we need it
        # to continue capturing till handoff
        capman.resume_fixture()

    def runcall_until(self, func, **testargs):
        """Run test with testargs, stopping at location.

        Exceptions raised inside this function may be reported as test
        failures. For testing, this means report output is sent to stdout
        rather than stderr (INTERNALERROR).
        """
        inst = self.last_pdb = pytestPDB._init_pdb("runcall_until")

        self._handle_capture(func, testargs, inst)

        nofin = sys._getframe(1).f_code is self.runcall_until_async.__code__
        inst.reset()
        sys.settrace(self.trace_handoff)
        try:
            return func(**testargs)
        except Exception as exc:
            from _pytest.outcomes import Exit

            if not isinstance(exc, Exit) or exc.msg != "Quitting debugger":
                raise

            if self.tinfo.arg_name:
                self.elsewhere.clear()
            else:
                self.targets.clear()
        finally:
            # FIXME should unwind f_trace when --bt-all passed
            if not nofin:
                inst.quitting = True
                sys.settrace(None)
                self.last_pdb = None

    async def runcall_until_async(self, func, **testargs):
        """Async wrapper for runcall_until"""
        try:
            await self.runcall_until(func, **testargs)
        finally:
            self.last_pdb.quitting = True
            sys.settrace(None)
            self.last_pdb = None

    @pytest.mark.tryfirst
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

    @pytest.mark.tryfirst
    def pytest_pyfunc_call(self, pyfuncitem):
        if self.targets and pyfuncitem is self.targets[0]:
            # Mimic primary hookimpl in _pytest/python.py
            testargs = {
                arg: pyfuncitem.funcargs[arg]
                for arg in pyfuncitem._fixtureinfo.argnames
            }
            if self.pt_aio and "asyncio" in pyfuncitem.keywords:
                if hasattr(self.pt_aio, "_markers_2_fixtures"):
                    event_loop = pyfuncitem.funcargs["event_loop"]
                    coro = self.runcall_until_async(pyfuncitem.obj, **testargs)
                    event_loop.run_until_complete(
                        asyncio.ensure_future(coro, loop=event_loop)
                    )
                else:  # 0.11+
                    pyfuncitem.obj = self.runcall_until(
                        pyfuncitem.obj, **testargs
                    )
            else:
                self.runcall_until(pyfuncitem.obj, **testargs)
            #
            if self.targets:
                self.targets.pop(0)
            # Skip primary hookimpl for this pyfuncitem
            return True


def _get_func_key(func):
    """Return a tuple of func name, def line no"""
    if hasattr(func, "__wrapped__"):
        func = func.__wrapped__
    return func.__name__, func.__code__.co_firstlineno


def _get_func_key_from_node(node):
    """ Given an ast node, return a lookup key

    Key is a tuple of func name, code-obj line number.
    """
    # Addresses undocumented change in Python 3.8+ (bug?)
    return (
        node.name,
        node.decorator_list[0].lineno if node.decorator_list else node.lineno
    )


@attr.s
class TargetInfo:
    """Info about target function corresponding to pytest item or fixture
    """
    py_obj_kind = attr.ib()
    func_name = attr.ib()
    func_lnum = attr.ib()

    arg_name = attr.ib(default=None, repr=False, kw_only=True)
    inner = attr.ib(default=None, repr=False, kw_only=True)


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
    func_items,  # dict mapping func names to items
    fixtures_finder,  # callable returning map of func names to FixtureDefs
):
    """Resolve args into a TargetInfo object and return it

    The ``TargetInfo.func`` attribute must be taken from a
    ``Item.function`` or ``FixtureDef.func``.  On failure, return None.
    If relevant, the ``inner`` field must be set to the innermost
    enclosing function of arg ``line_no``.
    """
    try:
        root = ast.parse(Path(filename).read_text(), filename=filename)
    except Exception:
        return None
    leaf = _get_node_at_pos(line_no, root, None)
    kind = "item"
    targets = None
    py38 = sys.version_info >= (3, 8)

    def find(node, tipo):
        while node and type(node) not in tipo:
            if node is root:
                return None
            node = node.parent
        return node

    def resolve(scope, haystack):
        while True:
            found = scope and haystack.get(
                _get_func_key_from_node(scope) if py38 else
                (scope.name, scope.lineno)
            )
            if not scope or found:
                break
            scope = find(scope.parent, (ast.FunctionDef, ast.AsyncFunctionDef))
        return scope, found

    inner = find(leaf, (ast.FunctionDef, ast.AsyncFunctionDef))
    if not inner:
        return None

    outer, targets = resolve(inner, func_items)
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
    if cls:
        assert targets[0].cls.__name__ == cls.name

    funcs = {o.function if kind == "item" else o.func for o in targets}
    assert len(funcs) == 1
    func_name, func_lnum = _get_func_key(funcs.pop())
    assert outer.name == func_name

    return TargetInfo(
        py_obj_kind=kind,
        func_name=func_name,
        func_lnum=func_lnum,
        arg_name=arg_name if kind == "fixture" else None,
        inner=inner.name if inner else None,
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
                try:
                    return self._completions[state]
                except IndexError:
                    return None
            except Exception:
                return None

    pytestPDB._wrapped_pdb_cls = usepdb_cls, PdbComplete
