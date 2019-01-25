# https://github.com/poppyschmo/pytest-pdb-break
# License: Apache 2.0

import os
import sys
import attr
import pytest

from pathlib import Path
from .logging_helper import LoggingHelper, logging, PpLogger


# TODO check if loggers persist when running inline rather than subproc

def test_helper_no_io(testdir):
    assert logging._loggerClass is logging.Logger
    assert logging.Logger.manager.loggerClass is None

    # Seems "optional" validators only run when value is non-None
    with pytest.raises(TypeError):
        LoggingHelper(LEVEL=None)

    # Nonexistent path
    with pytest.raises(FileNotFoundError) as exc:
        LoggingHelper(LOGFILE="/tmp/fake/_____/fake")
    with pytest.raises(FileNotFoundError) as exc:
        LoggingHelper(LOGDEFS="/tmp/fake/_____/fake")

    # Baseline
    base = LoggingHelper()
    assert base == LoggingHelper(LOGFILE=None,
                                 LOGDEFS=None,
                                 HANDLER_NAME="LoggingHelper (DEBUG)",
                                 LEVEL="DEBUG",
                                 logfile=None,
                                 logdefs=None,
                                 input_filters=None,
                                 output_filters=None)

    # Empty envvar, None passed to validator
    assert os.getenv("TEST_FAKE_ENVVAR") is None
    with pytest.raises(FileNotFoundError) as excinfo:
        LoggingHelper(LOGFILE="TEST_FAKE_ENVVAR")
    assert excinfo.match("TEST_FAKE_ENVVAR")

    # Envvar exists but is not a tty
    fake_upcase = Path(testdir.tmpdir / "TEST_FAKE_ENVVAR")
    fake_upcase.touch()
    assert fake_upcase.exists()
    with pytest.raises(ValueError) as excinfo:
        LoggingHelper(LOGFILE="TEST_FAKE_ENVVAR")
    assert excinfo.match("char.*device")

    # Valid envvar
    _origenv = dict(os.environ)
    ttypath = Path("/dev/tty")
    assert ttypath.exists()
    try:
        os.environ["TEST_FAKE_ENVVAR"] = str(ttypath)
        inst = LoggingHelper(LOGFILE="TEST_FAKE_ENVVAR")
        assert inst.LOGFILE.samefile(ttypath)
        assert inst.logfile is None
    finally:
        os.environ = _origenv
        assert os.getenv("TEST_FAKE_ENVVAR") is None

    # Can't pass a string for logfile
    with pytest.raises(ValueError) as exc:
        LoggingHelper(logfile="/tmp/fake/_____/fake")
    assert exc.match("file-like")

    assert base.get_logger("foo") is None
    assert logging._loggerClass is logging.Logger
    assert logging.Logger.manager.loggerClass is None


def test_logger_class():
    from .logging_helper import logger_class
    assert logging._loggerClass is logging.Logger
    assert logging.Logger.manager.loggerClass is None

    with logger_class():
        assert logging._loggerClass is logging.Logger
        assert logging.Logger.manager.loggerClass is PpLogger
    assert logging._loggerClass is logging.Logger
    assert logging.Logger.manager.loggerClass is None

    with pytest.raises(RuntimeError), logger_class():
        raise RuntimeError("I ran")
    assert logging._loggerClass is logging.Logger
    assert logging.Logger.manager.loggerClass is None


def test_load_logdefs(testdir, monkeypatch):
    from . import logging_helper as module
    from .logging_helper import load_logdefs

    with monkeypatch.context() as m:
        m.setattr(module, "validate_logdefs", lambda _i, _a, l: l)

        # Unknown file type
        with pytest.raises(RuntimeError):
            fake_path = Path(testdir.tmpdir / "fake.ini")
            load_logdefs(fake_path)

        # Empty python fails
        testdir.makepyfile(empty="")
        with pytest.raises(RuntimeError) as excinfo:
            load_logdefs(Path("empty.py"))
        assert excinfo.match("'logdefs' not found")

        # Python no logdefs
        testdir.makepyfile(invalid="foo = dict(args=[])")
        with pytest.raises(RuntimeError) as excinfo:
            load_logdefs(Path("invalid.py"))
        assert excinfo.match("'logdefs' not found")

        # Empty yaml -> None
        testdir.makefile(".yml", empty="---\n")
        with pytest.raises(RuntimeError) as excinfo:
            load_logdefs(Path("empty.yml"))
        assert excinfo.match("Problem loading")

        # Python
        testdir.makepyfile(saner="""
            logdefs = dict(something=dict())
        """)
        assert load_logdefs(Path("saner.py")) == {"something": {}}

        # Yaml
        testdir.makefile(".yml", something="""
            something: {}
        """)
        assert load_logdefs(Path("something.yml")) == {"something": {}}


def test_validate_logdefs(testdir):
    from .logging_helper import validate_logdefs

    # Wrong type
    with pytest.raises(TypeError) as excinfo:
        validate_logdefs(None, None, "")
    assert excinfo.match("dict")

    # Empty
    with pytest.raises(ValueError) as excinfo:
        validate_logdefs(None, None, {})
    assert excinfo.match("empty")

    # Values must also be dicts
    with pytest.raises(TypeError) as excinfo:
        validate_logdefs(None, None, {"foo": "bar"})
    assert excinfo.match("dict.*not 'str'")

    # Currently valid but nonstandard
    defs = {NotImplemented: {}, Ellipsis: {}}
    assert validate_logdefs(None, None, defs) is defs

    # Invalid tag
    with pytest.raises(TypeError) as excinfo:
        validate_logdefs(None, None, {"afunc": {"atag": "bar"}})
    assert excinfo.match("afunc\\.atag.*not.*'str'")

    # Invalid key
    with pytest.raises(TypeError) as excinfo:
        validate_logdefs(None, None, {"afunc": {"atag": {"bad": []}}})
    assert excinfo.match("afunc\\.atag\\.bad.*'args'.*'kwargs'")

    # Invalid value
    with pytest.raises(TypeError) as excinfo:
        validate_logdefs(None, None, {"afunc": {"atag": {"args": "bar"}}})
    assert excinfo.match("afunc\\.atag\\.args.*list.*not.*'str'")


def test_from_logdefs(testdir):

    def make_pysrc(name):
        testdir.makepyfile(**{name: """
            from helpers.logging_helper import LoggingHelper
            logdefs = {LoggingHelper: %r}
            """ % logdefs[LoggingHelper]})

    # Invalid file
    with pytest.raises(FileNotFoundError) as excinfo:
        LoggingHelper.from_logdefs(str(testdir.tmpdir / "fake.py"))
    assert excinfo.match("fake\\.py")
    with pytest.raises(FileNotFoundError) as excinfo:
        LoggingHelper.from_logdefs("SOME_FAKE_ENVVAR")
    assert excinfo.match("SOME_FAKE_ENVVAR")

    # No init
    testdir.makepyfile(no_init="""
        logdefs = {'afunc': {'atag': {'args': []}}}
        """)
    with pytest.raises(ValueError) as excinfo:
        LoggingHelper.from_logdefs("no_init.py")
    assert excinfo.match("LOGFILE required")

    # No init, LOGFILE as kwarg
    assert LoggingHelper.from_logdefs("no_init.py", LOGFILE="/dev/tty") \
        == LoggingHelper(LOGFILE="/dev/tty",
                         LOGDEFS="no_init.py",
                         logdefs={"afunc": {"atag": {"args": []}}})

    # Init w. LOGFILE
    logdefs = {LoggingHelper: {"init": {"LOGFILE": "/dev/tty"}}}
    make_pysrc("just_logfile")
    assert LoggingHelper.from_logdefs("just_logfile.py") \
        == LoggingHelper(LOGFILE="/dev/tty",
                         LOGDEFS="just_logfile.py",
                         logdefs=logdefs)

    # Bad LOGFILE in init replaced by kwarg
    logdefs = {LoggingHelper: {"init": {"LOGFILE": "/tmp/fake"}}}
    make_pysrc("fake_logfile")
    assert LoggingHelper.from_logdefs("fake_logfile.py", LOGFILE="/dev/tty") \
        == LoggingHelper(LOGFILE="/dev/tty",
                         LOGDEFS="fake_logfile.py",
                         logdefs=logdefs)

    # Filters
    from .logging_helper import InputFilter, OutputFilter
    logdefs = {
        LoggingHelper: {"init": {"LOGFILE": "/dev/tty"},
                        "filters": {"input": {"foo": {"type": "str",
                                                      "exp": "None"}},
                                    "output": {"bar": {"pat": "baz",
                                                       "new": "spam"}}}}
    }
    make_pysrc("filters")
    assert LoggingHelper.from_logdefs("filters.py") == LoggingHelper(
        LOGFILE="/dev/tty",
        LOGDEFS="filters.py",
        logdefs=logdefs,
        input_filters={"foo": InputFilter(**{"type": "str", "exp": "None"})},
        output_filters={"bar": OutputFilter(**{"pat": "baz", "new": "spam"})})

    # Filters, kwarg precedence
    input_filters = {"foo": InputFilter(**{"type": "NoneType", "exp": "..."})}
    output_filters = {"bar": OutputFilter(**{"pat": "abc", "new": "123"})}
    assert LoggingHelper.from_logdefs("filters.py",
                                      input_filters=input_filters,
                                      output_filters=output_filters) \
        == LoggingHelper(LOGFILE="/dev/tty",
                         LOGDEFS="filters.py",
                         logdefs=logdefs,
                         input_filters=input_filters,
                         output_filters=output_filters)

    # Exec
    logdefs = {LoggingHelper: {"init": {"LOGFILE": "/dev/tty"},
                               "exec": "foo = 1"}}
    make_pysrc("exec")
    assert LoggingHelper.from_logdefs("exec.py") == LoggingHelper(
        LOGFILE="/dev/tty",
        LOGDEFS="exec.py",
        logdefs={LoggingHelper: {"init": {"LOGFILE": "/dev/tty"},
                                 "exec": "foo = 1",
                                 "dict": {"foo": 1}}}
    )


@pytest.fixture
def tty_testdir(testdir):
    child = testdir.spawn("bash -c 'tty && cat'")
    child.expect(r"/dev[\w/]+")
    assert child.match
    LOGFILE = child.match[0].decode()
    helpers_path = Path(sys.modules["helpers"].__path__[0])
    testdir.makeconftest("""
        import sys
        import pytest

        sys.path.insert(0, {hp!r})
        LOGFILE = {LOGFILE!r}

        @pytest.fixture(scope="module")
        def helper():
            from helpers.logging_helper import LoggingHelper
            helper = LoggingHelper(LOGFILE=LOGFILE)
            yield helper
            assert not helper.logfile.closed
        """.format(hp=str(helpers_path.parent), LOGFILE=LOGFILE))
    testdir.child = child
    testdir.LOGFILE = LOGFILE
    yield testdir
    assert child.isalive()


def test_get_logger(tty_testdir):
    testdir = tty_testdir
    child = testdir.child
    testdir.makepyfile(test_basic="""
        from conftest import LOGFILE
        from helpers.logging_helper import LoggingHelper

        def test_one():
            helper = LoggingHelper(LOGFILE=LOGFILE, HANDLER_NAME="BASIC")
            assert helper.logfile is None
            logger = helper.get_logger("test_one")
            assert helper._handler is not None
            assert helper._handler in logger.handlers
            assert helper._handler.get_name() == "BASIC"
            assert helper.logfile is not None
            assert logger.helper is helper
    """)
    result = testdir.runpytest_subprocess("test_basic.py::test_one")
    result.assert_outcomes(passed=1)
    child.expect("\r\n\r\n")

    testdir.makepyfile(test_twice="""
        from conftest import helper

        def test_one(helper, monkeypatch):
            logger = helper.get_logger("test_twice")
            assert helper.logfile is not None
            h_attrs = helper.__dict__.copy()
            assert h_attrs.pop("_handler")
            logger.debug("something")

            def raiser(inst, **kw):
                raise RuntimeError

            with monkeypatch.context() as m:
                m.setattr(type(logger), "_post_init", raiser)
                m.setattr(helper, "open_logfile", raiser)
                logger2 = helper.get_logger("test_twice")

            assert logger2 is logger
            assert len(logger.handlers) == 1
            assert logger.handlers[0] is helper._handler
            ldv = logger.__dict__.values()
            for v in h_attrs.values():
                assert v in ldv
            logger.debug("else")
    """)
    result = testdir.runpytest_subprocess("test_twice.py::test_one")
    result.assert_outcomes(passed=1)
    # Formatter (can't get too specific with asctime)
    child.expect("\r\n")
    child.expect(r"\[[^]]+\][^\r\n]+test_twice\.test_one:")
    child.expect("something\r\n")
    child.expect(r"\[[^]]+\][^\r\n]+test_twice\.test_one:")
    child.expect("else\r\n")


def test_prinspect(monkeypatch):
    """
    Note on partialmethod
    ~~~~~~~~~~~~~~~~~~~~~
    Wrapper gets wrapped's ``co_name``::

        >>> from functools import partialmethod

        >>> class C:
        ...     def get_caller_name(self):
        ...         return sys._getframe(1).f_code.co_name
        ...
        ...     def one(self):
        ...         return self.get_caller_name()
        ...
        ...     two = partialmethod(one)

        >>> c = C()
        >>> assert c.one() == "one" == c.two()
    """
    # Setup
    FakePpLogger = attr.make_class("PpLogger", ["name"])
    FakePpLogger.pfmt = lambda _i, _c, kw: repr(kw)
    FakePpLogger._log_as = lambda i, _f, msg: i.out.append(msg)
    inst = FakePpLogger("TEST")
    inst._prinspect_next = {}
    inst.out = []
    curframe = sys._getframe()

    # Just kwargs
    PpLogger.prinspect(inst, foo="foo", bar=[1, 2])
    expected = {"foo": "foo", "bar": [1, 2]}
    assert inst.out.pop() == f"\n{expected!r}"

    # Non-strings must be pairs accepted by dict([pair, ...])
    foo = object()
    bar = object()
    baz = object()
    PpLogger.prinspect(inst, ("foo", foo), ["bar", bar])
    expected = {"foo": foo, "bar": bar}
    assert inst.out[-1] == f"\n{expected!r}"
    assert hex(id(foo)) in inst.out[-1]  # <object at 0xdeadbeef>
    last = inst.out.pop()

    # Strings are eval'd
    PpLogger.prinspect(inst, "foo", "bar")
    assert inst.out[-1] == f"\n{expected!r}"
    assert last == inst.out.pop()

    # Args trump kwargs
    PpLogger.prinspect(inst, "foo", foo=1)
    expected = {"foo": foo}
    assert inst.out.pop() == f"\n{expected!r}"

    # Empty _next, defer
    assert "__lname" not in curframe.f_locals
    inst.out.clear()
    PpLogger.prinspect(inst, "foo", defer=True)
    assert not inst.out
    assert "test_prinspect" in inst._prinspect_next
    assert inst._prinspect_next["test_prinspect"] \
        == {"defer": curframe, "foo": foo}
    assert curframe.f_locals["__lname"] == "TEST"

    # Non-empty _next, defer
    PpLogger.prinspect(inst, "bar", defer=True)
    assert not inst.out
    assert inst._prinspect_next["test_prinspect"] \
        == {"defer": curframe, "foo": foo, "bar": bar}

    # Non-empty _next, same caller
    PpLogger.prinspect(inst, "baz")
    assert not inst._prinspect_next
    expected = {"foo": foo, "bar": bar, "baz": baz}
    assert inst.out.pop() == f"\n{expected!r}"
    assert not inst.out  # _log_as was only called once
    assert not inst._prinspect_next

    def apply(f, *args, **kwargs):
        return f(*args, **kwargs)

    def pspell(f, *args, **kwargs):
        return f(*args, **kwargs)

    # Backlog written out via _log_as prior to request
    assert curframe.f_locals["__lname"] == "TEST"
    inst._prinspect_next["test_prinspect"] = {"defer": curframe, "foo": foo}
    apply(PpLogger.prinspect, inst, "args")
    assert len(inst.out) == 2
    expected = {"args": (inst, "args")}
    assert inst.out.pop() == f"\n{expected!r}"
    expected = {"foo": foo}
    assert inst.out.pop() == f"\n{expected!r}"

    # Unless caller's name is pspell
    assert not inst._prinspect_next
    assert not inst.out
    inst._prinspect_next["test_prinspect"] = {"defer": curframe, "foo": foo}
    pspell(PpLogger.prinspect, inst, "bar")
    assert len(inst.out) == 1
    expected = {"foo": foo, "bar": bar}
    assert inst.out.pop() == f"\n{expected!r}"


def test_pspell():
    FakePpLogger = attr.make_class("PpLogger", ["name"])
    FakePpLogger.prinspect = lambda i, *a, **kw: i.out.append((a, kw))
    inst = FakePpLogger("TEST")
    inst.logdefs = {}
    inst.out = []

    # Empty logdefs
    # use case: LOGDEFS undefined but logfile open and spect calls exist
    PpLogger.pspell(inst, "noop")
    assert not inst.out

    # Non-empty logdefs but caller no key matching caller (unlikely)
    inst.logdefs["test_bogus"] = {}
    with pytest.raises(KeyError):
        PpLogger.pspell(inst, "noop")

    # Non-empty logdefs but no matching (or empty) tag
    inst.logdefs.clear()
    assert sys._getframe().f_code.co_name == "test_pspell"
    inst.logdefs["test_pspell"] = {"atag": {}}
    PpLogger.pspell(inst, "noop")
    assert not inst.out
    PpLogger.pspell(inst, "atag")
    assert not inst.out

    # Matching tag (only kwargs evaluated)
    foo = object()
    inst.logdefs["test_pspell"] = {"atag": {"args": ["foo"],
                                            "kwargs": {"a label": "foo"}}}
    PpLogger.pspell(inst, "atag")
    assert inst.out == [(("foo",), {"a label": foo})]

    # Defer only forwarded as kwarg if True
    inst.out.clear()
    PpLogger.pspell(inst, "atag", defer=True)
    assert inst.out == [(("foo",), {"a label": foo, "defer": True})]


def test_pfmt():
    FakePpLogger = attr.make_class("PpLogger", ["name"])
    FakePpLogger.filter_input = lambda i, c, items: NotImplemented
    FakePpLogger.filter_output = lambda i, c, items, output: NotImplemented
    inst = FakePpLogger("TEST")
    inst.input_filters = {}
    inst.output_filters = {}
    assert not hasattr(inst, "pp")
    assert PpLogger.pfmt(inst, None, {}) == "{}"
    assert inst.pp

    # Passes copied items to filter_input but orig to filter_output
    FakePpLogger.filter_input = lambda i, c, t: t == {} and t is not i.items
    FakePpLogger.filter_output = lambda i, c, t, out: t is i.items and out
    inst = FakePpLogger("TEST")
    inst.input_filters = {"foo": None}
    inst.output_filters = {"foo": None}
    inst.items = {}
    assert PpLogger.pfmt(inst, None, inst.items) == "True"


def test_filter_input():
    from helpers.logging_helper import InputFilter
    from types import MappingProxyType
    from functools import partial
    FakePpLogger = attr.make_class("PpLogger", ["input_filters"])
    Foo = type("Foo", (object,), {})
    foo = Foo()
    inst = FakePpLogger({})
    curframe = sys._getframe()
    filti = partial(PpLogger.filter_input, inst, curframe)

    # Base ALL
    inst.input_filters["a"] = InputFilter(exp="...")
    assert inst.input_filters["a"].all is True
    orig = MappingProxyType({"foo": foo})
    changed = MappingProxyType({"foo": ...})
    assert filti(orig.copy()) == changed

    # Base ANY
    inst.input_filters["a"].all = False
    assert filti(orig.copy()) == orig

    # No priors ALL
    inst.input_filters["a"].all = True
    inst.input_filters["a"].callers = ["test_filter_input", "fake"]
    assert filti(orig.copy()) == changed

    # Failing prior ALL
    assert inst.input_filters["a"].all is True
    inst.input_filters["a"].caller = "fake"
    assert filti(orig.copy()) == orig

    # Failing prior ANY
    inst.input_filters["a"].all = False
    assert filti(orig.copy()) == changed

    # Types ANY
    astr = "str"
    inst.input_filters["a"] = InputFilter(exp="...", type="fake", all=False)
    assert filti(orig.copy()) == orig
    inst.input_filters["a"].type = "Foo"
    assert filti(orig.copy()) == changed
    from itertools import count
    inc = count()
    assert next(inc) == 0
    inst.input_filters["a"].exp = "next(inc)"
    orig = MappingProxyType({"foo": foo, "astr": astr})
    inst.input_filters["a"].types = ["str", "int"]
    result = filti(orig.copy())
    assert result == {"foo": 1, "astr": 2}

    # Types ALL
    inst.input_filters["a"].all = True
    assert filti(orig.copy()) == orig  # must pass both type and types
    inst.input_filters["a"].type = "list"
    inst.input_filters["a"].types = ["list"]
    inst.input_filters["a"].exp = "v_.reverse() or v_"
    alist = list("list")
    orig = MappingProxyType({"alist": alist})
    assert filti(orig.copy())["alist"] == orig["alist"] == ["t", "s", "i", "l"]

    # Eval obdict
    inst.input_filters = {"a": InputFilter(exp="...", cond="True"),
                          "b": InputFilter(exp="(type(v_), type(r_))",
                                           cond="True")}
    orig = MappingProxyType({"foo": foo})
    assert filti(orig.copy()) == {"foo": (Foo, type(Ellipsis))}

    # Stop after first
    assert inst.input_filters["a"].first is False
    inst.input_filters["a"].first = True
    assert filti(orig.copy()) == {"foo": ...}


def test_filter_output():
    from helpers.logging_helper import OutputFilter
    from functools import partial
    FakePpLogger = attr.make_class("PpLogger", ["output_filters"])
    inst = FakePpLogger({})
    curframe = sys._getframe()
    filto = partial(PpLogger.filter_output, inst, curframe)

    # Runs without conditions
    inst.output_filters["a"] = OutputFilter(pat="abc", new="123")
    items = {"foo": ["abc"]}
    assert filto(items, repr(items)) == repr({"foo": ["123"]})

    # Keys and types tests pass if they match any item, fail otherwise
    inst.output_filters["a"] = OutputFilter(keys=["foo"], pat="abc", new="123")
    items = {"bar": 1}
    output = repr(items)
    assert filto(items, output) is output
    inst.output_filters["a"].types = ["int"]
    items = {"foo": ["abc"]}
    output = repr(items)
    assert filto(items, output) is output
    items["bar"] = 1
    output = repr(items)
    assert filto(items, output) == repr({"foo": ["123"], "bar": 1})

    # Method applied to entire output
    inst.output_filters["a"] = OutputFilter(pat="[oa]", new="", method="sub")
    output = repr(items)
    assert filto(items, output) == repr({"f": ["bc"], "br": 1})

    # Support re.compile objects
    import re
    pat = re.compile("[oa]")
    inst.output_filters["a"] = OutputFilter(pat=pat, new="", method="sub")
    assert filto(items, output) == repr({"f": ["bc"], "br": 1})

    # Eval
    with pytest.raises(ValueError):
        inst.output_filters["a"] = OutputFilter(pat=None, new="foo")
    one = 1  # noqa: F841
    inst.logdefs = {LoggingHelper: {"dict": {"two": 2}}}
    inst.output_filters["a"] = OutputFilter(pat=None,
                                            new="(i_['b'] + o_) * (one + two)",
                                            method="eval")
    output = "a"
    items = {"b": "b"}
    assert filto(items, output) == "bababa"

    # Same output is passed to each filter
    inst.output_filters["a"] = OutputFilter(pat="abc", new="123")
    inst.output_filters["b"] = OutputFilter(pat="123", new="ABC")
    items = {"foo": ["abc"]}
    output = repr(items)
    assert filto(items, output) == repr({"foo": ["ABC"]})

    # First finishes no matter what unless conditions fail
    inst.output_filters["a"].first = True
    assert filto(items, output) == repr({"foo": ["123"]})
