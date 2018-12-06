# https://github.com/poppyschmo/pytest-pdb-break
# License: Apache 2.0

import os
import sys
import logging


# Can't figure out how to use pdb to inspect these hooks, so it's this for now
class LoggingHelper:
    # XXX Don't register .close or .shutdown, etc. with any teardown machinery
    # Warning: some functions eval source code in .LOGDEFS, if defined
    LOGFILE = None  # TTY devices only
    LOGDEFS = None
    HANDLER_NAME = "LoggingHelper (DEBUG)"
    LEVEL = "DEBUG"
    logfile = None
    logdefs = None

    @classmethod
    def _module_init(cls):
        assert sys.platform.startswith("linux")
        cls._prinspect_next = {}
        if hasattr(cls.logfile, "closed"):
            assert not cls.logfile.closed
            return
        cls.logfile = open(cls.LOGFILE, "w")
        assert os.isatty(cls.logfile.fileno())
        cls.logfile.write("\n")
        if cls.LOGDEFS:
            cls.load_logdefs()
        cls.logfile.flush()
        klass = type("LoggingHelperLogger", (logging.Logger, cls), {})
        logging.setLoggerClass(klass)

    @classmethod
    def load_logdefs(cls):
        # Often fails because of syntax errors, so helps to run at import time
        try:
            import yaml
        except ImportError:
            raise UserWarning("No logging defs found")
        else:
            with open(cls.LOGDEFS) as flo:
                cls.logdefs = yaml.load(flo)
        cls.logfile.write("Loaded log defs from {!r}\n".format(cls.LOGDEFS))

    def _log_as(self, frame, msg, exc_info=None):
        """Log msg, impersonating frame."""
        co = frame.f_code
        name = frame.f_locals.get("__lname", self.name)
        record = self.makeRecord(
            name, self.level, co.co_filename, frame.f_lineno,
            msg, (), exc_info, co.co_name, None, None
        )
        self.handle(record)

    def prinspect(self, *args, **kwargs):
        """Print stuff."""
        from pprint import PrettyPrinter
        pp = PrettyPrinter()
        caller = sys._getframe(1)
        name = caller.f_code.co_name
        if name == "prinspot":
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
                    self._log_as(c, "\n{}".format(pp.pformat(v)))
        if args:
            kwargs.update((a, eval(a, caller.f_globals, caller.f_locals))
                          if isinstance(a, str) else a for a in args)
        if defer:
            kwargs["defer"] = caller
            caller.f_locals.update(__lname=self.name)
            self._prinspect_next.setdefault(name, {}).update(kwargs)
        else:
            self._log_as(caller, "\n{}".format(pp.pformat(kwargs)))

    def prinspot(self, tag, defer=None):
        """Read in yaml defs, print.
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

    from functools import partialmethod
    prinspool = partialmethod(prinspect, defer=True)
    prinspotl = partialmethod(prinspot, defer=True)  # 'l'~~> "line" or "late"

    @classmethod
    def get_logger(cls, name):
        """Return a logger with a TTY handler for cls.LEVEL."""
        if not cls.LOGFILE:
            assert cls.LOGFILE is None
            return
        if not hasattr(cls, "level"):
            cls._module_init()
        assert cls.logfile
        # Passing "" as name will fetch root logger (wrong class)
        logger = logging.getLogger(name)
        handler = getattr(cls, "_handler", None)
        if handler and handler in logger.handlers:
            return logger
        fmt = ("{dk}[%(asctime)s]{no} {dm}%(name)s"
               ".%(funcName)s:{no} %(message)s")
        clrs = dict(dk="\x1b[38;5;241m", dm="\x1b[38;5;247m", no="\x1b[m")
        handler = logging.StreamHandler(stream=cls.logfile)
        handler.setFormatter(logging.Formatter(fmt.format(**clrs)))
        handler.set_name(cls.HANDLER_NAME)
        handler.setLevel(cls.LEVEL)
        cls._handler = handler
        logger.addHandler(handler)
        logger.setLevel(cls.LEVEL)
        return logger
