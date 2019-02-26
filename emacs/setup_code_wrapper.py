# This file is part of https://github.com/poppyschmo/pytest-pdb-break


def _wrap_pyel(orig):
    """Wrapper for ``__PYTHON_EL_get_completions``.

    This is meant to be tacked on to ``python-shell-completion-setup-code``,
    the first of two heredoc-like strings sent by python.el Gallina to the
    inferior process for every new completion query.

    Note: this wrapper has side effects (like import statements) which may
    adversely affect the execution of the source being debugged. It's not
    alone in this regard. ``python-shell-send-file`` adds the ``os`` and
    ``codecs`` modules to the command loop's global namespace.
    """
    import readline

    def _complete(inst, text, state):
        """Basically ``PdbComplete.complete()`` from the main module.

        Made redundant here to accommodate non-pytest PDB invocations
        (the plugin need not appear in sys.path).
        """
        if state == 0:
            ns = inst.curframe.f_globals.copy()
            ns.update(inst.curframe.f_locals)
            import sys
            import rlcompleter
            from code import InteractiveConsole
            from itertools import takewhile, count, filterfalse
            frame = sys._getframe().f_back
            el_names = ("_wpl", "__PYTHON_EL_get_completions", "<module>")
            while frame.f_back and frame.f_code.co_name in el_names:
                frame = frame.f_back
            icon = frame.f_locals.get("self")
            if icon and isinstance(icon, InteractiveConsole):
                ns.update(icon.locals)
            else:
                icon = None
            cp = rlcompleter.Completer(ns).complete
            first = takewhile(bool, (cp(text, m) for m in count()))
            inst._completions = list(first)
            if icon is None:
                cp = super(type(inst), inst).complete  # cmd.complete
                rest = takewhile(bool, (cp(text, m) for m in count()))
                uniques = filterfalse(inst._completions.__contains__, rest)
                inst._completions.extend(uniques)
        readline.set_completer(inst.complete)
        try:
            return inst._completions[state].rstrip("(")
        except (IndexError, AttributeError):
            return None

    def _wpl(text):
        _t = text
        completer = readline.get_completer()
        try:
            import bdb
            inst = completer.__self__
            if isinstance(inst, bdb.Bdb):
                import pdb
                if hasattr(pdb, "GLOBAL_PDB"):  # pdb++
                    cls = pdb.Completer
                    assert cls.__module__ == "fancycompleter"
                    if not hasattr(cls, "__PYTHON_EL_orig"):
                        # Original drops common path, only lists leaves
                        cls.__PYTHON_EL_orig = cls.attr_matches  # not restored
                        import rlcompleter
                        cls.attr_matches = rlcompleter.Completer.attr_matches
                elif (isinstance(inst, pdb.Pdb)  # quacks vanilla, so add patch
                      and hasattr(pdb, "cmd")
                      and completer.__func__ is pdb.cmd.Cmd.complete
                      and "complete" not in inst.__dict__):
                    inst.complete = _complete.__get__(inst)
                elif not hasattr(inst._completions):
                    raise RuntimeError("Unsupported PDB: {}"
                                       .format(inst.__class__))
                inst.complete(text, 0)
                return inst._completions[:]
            return orig(text)
        except Exception:
            readline.set_completer(completer)
            return orig(_t)

    return _wpl
