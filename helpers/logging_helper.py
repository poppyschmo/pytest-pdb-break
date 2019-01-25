# https://github.com/poppyschmo/pytest-pdb-break
# License: Apache 2.0

import attr
import os
import sys
import logging
from pathlib import Path
from functools import partialmethod
from contextlib import contextmanager


@attr.s
class InputFilter:
    exp = attr.ib()  # source string, evaled result replaces value
    # Tests
    caller = attr.ib(default=None)  # name of caller
    callers = attr.ib(default=None)  # run exp if caller's name is member
    key = attr.ib(default=None)
    keys = attr.ib(default=None)
    type = attr.ib(default=None)
    types = attr.ib(default=None)
    cond = attr.ib(default=None)  # arbitrary expression (source string)
    # Meta
    all = attr.ib(default=True)  # if False, run exp if any defined test passes
    first = attr.ib(default=False)  # don't filter k/v pair further if exp runs


@attr.s
class OutputFilter:
    new = attr.ib()  # w. method "eval", output bound to o_
    pat = attr.ib(default=None)
    method = attr.ib(default="replace")  # or "eval", "sub" (re.sub)
    keys = attr.ib(default=None)
    types = attr.ib(default=None)
    first = attr.ib(default=False)

    @pat.validator
    def validate_pat(self, attribute, value):
        if value is None and self.method != "eval":
            raise ValueError("pat must be a string or re.compile obj")


class PpLogger(logging.Logger):

    def _post_init(self, **kwargs):
        self.__dict__.update(kwargs)
        self._prinspect_next = {}
        self.addHandler(self.helper._handler)
        self.setLevel(self.LEVEL)

    def _log_as(self, frame, msg, exc_info=None):
        """Log msg, impersonating frame."""
        co = frame.f_code
        name = frame.f_locals.get("__lname", self.name)
        record = self.makeRecord(
            name, self.level, co.co_filename, frame.f_lineno,
            msg, (), exc_info, co.co_name, None, None
        )
        self.handle(record)

    def filter_input(self, caller, items):
        """Maybe replace values in items.

        If conditions pass, eval exp in the calling scope's context, with v_
        bound to the *original* value and the current replacement bound to r_.
        The result becomes the next r_. Mutable objects aren't copied
        beforehand.
        """
        caller_name = caller.f_code.co_name

        def maybe():
            yield filt.caller == caller_name if filt.caller else filt.all
            yield caller_name in filt.callers if filt.callers else filt.all
            yield k == filt.key if filt.key else filt.all
            yield k in filt.keys if filt.keys else filt.all
            yield type(v).__name__ == filt.type if filt.type else filt.all
            yield type(v).__name__ in filt.types if filt.types else filt.all
            yield eval(filt.cond,
                       caller.f_globals, locup) if filt.cond else filt.all

        for k, v in items.items():
            # A less wasteful approach would be to to swap loops and check
            # the caller name first, but it's not worth it unless adding
            # per-def whitelisting/narrowing.
            new = v
            locup = dict(caller.f_locals, c_=caller, k_=k, v_=v, r_=new)
            for filt in self.input_filters.values():
                # ALL: (not defined(t1) + test(t1)) x ...
                # ANY: defined(t1) x test(t1) + ...
                op = all if filt.all else any
                if op(maybe()):
                    new = eval(filt.exp, caller.f_globals, locup)
                    if filt.first:  # done with this filter
                        break
                    locup["r_"] = new
            items[k] = new  # preserves size
        return items

    def filter_output(self, caller, items, output):
        """Perform substitutions on entire output string.
        """
        # Smarter/nicer approach would process each PP repr as they're being
        # rendered, but that sounds complicated
        types = None
        orig = output
        for filt in self.output_filters.values():
            if filt.keys and not set(filt.keys) & set(items):
                continue
            if filt.types:
                if types is None:
                    types = set((type(v).__name__ for v in items.values()))
                if not set(filt.types) & types:
                    continue
            if filt.method == "replace":
                output = output.replace(filt.pat, filt.new)
            elif filt.method == "sub":
                if isinstance(filt.pat, str):
                    from re import sub
                    output = sub(filt.pat, filt.new, output)
                else:
                    output = filt.pat.sub(filt.new, output)
            elif filt.method == "eval":
                d = self.logdefs.get(LoggingHelper, {}).get("dict", {})
                d.update(caller.f_locals, i_=items, g_=orig, o_=output)
                output = eval(filt.new, caller.f_globals, d)
            if filt.first:
                break
        return output

    def pfmt(self, caller, items):
        """Format value."""
        if not hasattr(self, "pp"):
            try:
                from .ordered import OrderedPrettyPrinter as PP
            except ImportError:
                from pprint import PrettyPrinter as PP
            self.pp = PP()
        if self.input_filters:
            out = self.pp.pformat(self.filter_input(caller, dict(items)))
        else:
            out = self.pp.pformat(items)
        if self.output_filters:
            out = self.filter_output(caller, items, out)
        return out

    def prinspect(self, *args, **kwargs):
        """Print stuff."""
        caller = sys._getframe(1)
        name = caller.f_code.co_name
        if name == "pspell":
            caller = caller.f_back
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
                    self._log_as(c, "\n{}".format(self.pfmt(c, v)))
        if args:
            kwargs.update((a, eval(a, caller.f_globals, caller.f_locals))
                          if isinstance(a, str) else a for a in args)
        if defer:
            kwargs["defer"] = caller
            caller.f_locals.update(__lname=self.name)
            self._prinspect_next.setdefault(name, {}).update(kwargs)
        else:
            self._log_as(caller, "\n{}".format(self.pfmt(caller, kwargs)))

    def pspell(self, tag, defer=False):
        """Log an item from from logdefs dict.
        This ignores prinspect's iterable arg form. Tag can be an int.
        """
        if not self.logdefs:
            return
        caller = sys._getframe(1)
        name = caller.f_code.co_name
        defs = self.logdefs[name].get(tag)
        if not defs:
            return
        args = defs.get("args", ())
        kwargs = {k: eval(v, caller.f_globals, caller.f_locals) for
                  k, v in defs.get("kwargs", {}).items()}
        if defer:
            kwargs["defer"] = defer
        self.prinspect(*args, **kwargs)

    prinspool = partialmethod(prinspect, defer=True)
    pspore = partialmethod(pspell, defer=True)


def validate_logdefs(inst, attribute, logdefs):
    """Validate a logdefs dict.
    Does not check if func names actually exist (are interned).
    """
    from collections.abc import MutableMapping, MutableSequence
    if not isinstance(logdefs, MutableMapping):
        raise TypeError("Logdefs must be a dict")
    if not logdefs:
        raise ValueError("Logdefs cannot be empty")
    pfx = "Invalid entry in logdefs"
    for func_name, defs in logdefs.items():
        # TODO accept function objects as well as names
        if not isinstance(defs, MutableMapping):
            raise TypeError(f"{pfx}: value of {func_name} must be a dict, "
                            f"not {type(defs).__name__!r}")
        if not isinstance(func_name, str):
            continue
        for tag, args in defs.items():
            if not isinstance(args, MutableMapping):
                raise TypeError(f"{pfx}: value of {func_name}.{tag} must be a "
                                f"dict, not {type(args).__name__!r}")
            for k, v in args.items():
                path = f"{pfx}: {func_name}.{tag}.{k}"
                tv = type(v).__name__
                if k not in ["args", "kwargs"]:
                    raise TypeError(f"{path} must be 'args' or 'kwargs'")
                if k == "args" and not isinstance(v, MutableSequence):
                    raise TypeError(f"{path} must be a list, not {tv!r}")
                if k == "kwargs" and not isinstance(v, MutableMapping):
                    raise TypeError(f"{path} must be a dict, not {tv!r}")
    return logdefs


def load_logdefs(path):
    """Load logdefs from an external file at path.

    For YAML, just mimic the example.

    Python source files also work but may produce side effects.  If
    present, a dict named "logdefs" is loaded, which is equivalent to
    the YAML document-level namespace.

    Setup info should be specified under a first-child dict with the
    object ``<class helpers.logging_helper.LoggingHelper>`` as its key.
    If that contains an item named "init", the latter's values are
    passed as kwargs to LoggingHelper. Any filters defined under
    filters/input or filters/output ("filters" being a sibling of
    "init") will be instantiated and passed as members of (kwargs)
    input_filters and/or output_filters.
    """
    logdefs = None
    if path.suffix == ".py":
        g = {}
        with open(path) as flo:
            exec(flo.read(), g)
        if "logdefs" in g:
            logdefs = g["logdefs"]
        else:
            # Otherwise basically unusable: user would have to delete
            # extraneous objects, and we'd have to delete __builtins__
            raise RuntimeError("'logdefs' not found in %s" % path)
    elif path.suffix in (".yml", ".yaml"):
        try:
            import yaml
        except ImportError as exc:
            raise RuntimeError("PyYAML required: pip install pyyaml") from exc
        with open(path) as flo:
            logdefs = yaml.load(flo)
    if logdefs is None:
        raise RuntimeError("Problem loading logdefs from %s " % path)
    return validate_logdefs(None, None, logdefs)


@contextmanager
def logger_class():
    orig = PpLogger.manager.loggerClass
    try:
        PpLogger.manager.setLoggerClass(PpLogger)
        yield
    finally:
        PpLogger.manager.loggerClass = orig


def convert_env_filepath(value):
    if not value:
        return None
    if isinstance(value, Path):
        return value
    if value.isupper():
        value = os.getenv(value) or value
    return Path(value)


def validate_path(inst, attribute, value):
    try:
        value.resolve(True)
    except AttributeError as exc:
        raise ValueError("Expected pathlib.Path, got %s" %
                         type(value).__name__) from exc
    except FileNotFoundError as exc:
        exc.args += (value,)
        raise exc


def validate_chardev(inst, attribute, value):
    if not value.is_char_device():
        raise ValueError("%s must be a character device" % value)


def validate_open_tty(inst, attribute, value):
    try:
        if value.closed:
            raise ValueError("%s cannot be closed" % value)
        if not os.isatty(value.fileno()):
            raise ValueError("%s must be a tty" % value)
    except AttributeError:
        raise ValueError("%s must be a file-like object" % value)


tty_fmt = "{dk}[%(asctime)s]{no} {dm}%(name)s.%(funcName)s:{no} %(message)s"
tty_colors = dict(dk="\x1b[38;5;241m", dm="\x1b[38;5;247m", no="\x1b[m")


@attr.s
class LoggingHelper:
    LOGFILE = attr.ib(
        default=None,
        converter=attr.converters.optional(convert_env_filepath),
        validator=attr.validators.optional([attr.validators.instance_of(Path),
                                            validate_path, validate_chardev])
    )
    LOGDEFS = attr.ib(
        default=None,
        converter=attr.converters.optional(convert_env_filepath),
        validator=attr.validators.optional([attr.validators.instance_of(Path),
                                            validate_path])
    )
    HANDLER_NAME = attr.ib(
        default="LoggingHelper (DEBUG)",
        validator=attr.validators.instance_of(str)
    )
    LEVEL = attr.ib(
        default="DEBUG",
        validator=attr.validators.instance_of((int, str))
    )
    logfile = attr.ib(
        default=None,
        validator=attr.validators.optional(validate_open_tty)
    )
    logdefs = attr.ib(
        default=None,
        validator=attr.validators.optional(attr.validators.instance_of(dict))
    )
    input_filters = attr.ib(
        default=None,
        validator=attr.validators.optional(attr.validators.instance_of(dict))
    )
    output_filters = attr.ib(
        default=None,
        validator=attr.validators.optional(attr.validators.instance_of(dict))
    )

    @classmethod
    def from_logdefs(cls, LOGDEFS, **kwargs):
        """Load defs from an external source.

        This may contain init values, as well, but any kwargs passed
        here override those. See docstring of ``load_logdefs`` for more
        info.
        """
        LOGDEFS = convert_env_filepath(LOGDEFS)
        validate_path(None, None, LOGDEFS)
        logdefs = load_logdefs(LOGDEFS)
        setup = logdefs.get(LoggingHelper, {})
        init = dict(setup.get("init", {}))
        init.update(kwargs)
        if "logfile" not in init and "LOGFILE" not in init:
            raise ValueError("LOGFILE required but not in %s" % LOGDEFS)
        filters = setup.get("filters")
        # Filters already under init are assumed to be of correct type
        # TODO check anyway if initializing these with existing instances fails
        if filters:
            if "output" in filters:
                existing = init.setdefault("output_filters", {})
                new = ((k, OutputFilter(**v)) for
                       k, v in filters["output"].items() if
                       k not in existing)
                existing.update(new)
            if "input" in filters:
                existing = init.setdefault("input_filters", {})
                new = ((k, InputFilter(**v)) for
                       k, v in filters["input"].items() if
                       k not in existing)
                existing.update(new)
        if "exec" in setup:
            exec(setup["exec"], globals(), setup.setdefault("dict", {}))
        return cls(LOGDEFS=LOGDEFS, logdefs=logdefs, **init)

    def open_logfile(self):
        try:
            if self.LOGFILE:
                self.logfile = open(self.LOGFILE, "w")
            elif hasattr(self.logfile, "name"):
                self.LOGFILE = Path(self.logfile.name)
                self.logfile = None
                return self.open_logfile()
            else:
                assert self.logfile.closed
                raise RuntimeError("Cannot reopen %s" % self.logfile)
        except Exception:
            self.LOGFILE = None
            self.logfile = None
            raise
        self.logfile.write("\n")  # becomes \r\n
        if self.LOGDEFS and self.logdefs:
            self.logfile.write("Loaded logging defs from %s\n" % self.LOGDEFS)
        self.logfile.flush()

    def get_tty_handler(self):
        if hasattr(self, "_handler"):
            return self._handler
        handler = logging.StreamHandler(stream=self.logfile)
        handler.setFormatter(logging.Formatter(tty_fmt.format(**tty_colors)))
        handler.set_name(self.HANDLER_NAME)
        handler.setLevel(self.LEVEL)
        self._handler = handler
        return handler

    def get_logger(self, name):
        """Return a logger with a TTY handler for self.LEVEL."""
        if not self.LOGFILE and not self.logfile:
            assert self.LOGFILE is None
            assert self.logfile is None
            return
        if not sys.platform.startswith("linux"):
            raise RuntimeError("This logger only works on Linux")
        if not hasattr(self.logfile, "closed") or self.logfile.closed:
            self.open_logfile()
        # Passing "" as name will fetch root logger (wrong class)
        with logger_class():
            logger = logging.getLogger(name)
        if self.get_tty_handler() in logger.handlers:
            assert logger.level is not logging.NOTSET
            return logger
        logger._post_init(**dict(self.__dict__, helper=self))
        return logger
