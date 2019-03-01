# This file is part of https://github.com/poppyschmo/pytest-pdb-break
import os
import sys
import argparse
import subprocess

from inspect import signature
from types import FunctionType
from collections.abc import MutableMapping, MutableSequence


def _main():

    def debug_cmdline(*args, **kwargs):
        if not kwargs.get("quiet"):
            print("--json: {!r}".format(pargs.json),
                  "--kwargs: {!r}".format(pargs.kwargs),
                  "--null: {!r}".format(pargs.null),
                  "args: {!r}".format(args),
                  "kwargs: {!r}".format(kwargs),
                  sep="\n")
        return kwargs.get("rv")

    allvars = vars(common)
    allvars.update(vars(ensure_venv))
    allvars.update(vars(get_collected))
    commands = {k: v for k, v in allvars.items() if
                isinstance(v, FunctionType) and not k.startswith("_")}

    ap = argparse.ArgumentParser(
        prog="ensure_venv",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="\n".join(
            "  \x1b[1m{}\x1b[m{}".format(n, signature(o)) +
            ("\n    {}".format(o.__doc__.strip().split("\n")[0])
             if o.__doc__ else "") for n, o in commands.items()
        ),
        description="Helpers for editor integratons"
    )

    commands["debug_cmdline"] = debug_cmdline

    ap.add_argument("--kwargs", action="store_true",
                    help="interpret final arg as a dict")
    ap.add_argument("--json", action="store_true",
                    help="return json")
    ap.add_argument("--null", action="store_true",
                    help="return null-separated values")
    ap.add_argument("cmd", type=lambda c: commands.get(c))
    ap.add_argument("args", nargs="*")

    pargs = ap.parse_args()

    if not pargs.cmd:
        ap.print_help()
        return 1
    if pargs.kwargs:
        *args, kwargs = pargs.args
        from ast import literal_eval
        kwargs = literal_eval(kwargs)
    else:
        args = pargs.args
        kwargs = {}

    ec = 0
    logfile = os.getenv("PYTEST_PDB_BREAK_INSTALL_LOGFILE")
    try:
        if logfile:
            common.SUBOUT = open(logfile, "w")
            common.SUBERR = subprocess.STDOUT
        # So far, no need to capture/divert stdout/stderr with --json
        rv = pargs.cmd(*args, **kwargs)
    except Exception:
        from traceback import format_exception, format_exception_only
        et, val, tb = sys.exc_info()
        rv = dict(error=format_exception_only(et, val)[0].strip(),
                  traceback=format_exception(et, val, tb))
        ec = 1
    finally:
        if logfile:
            common.SUBOUT.close()

    sep = "\x00" if pargs.null else "\n"

    if pargs.json:
        import json
        json.dump(rv, sys.stdout)
    elif isinstance(rv, MutableMapping):
        if ec:
            rv["traceback"] = "".join(rv["traceback"])
            if not pargs.null:
                print(rv["traceback"])
                return ec
        print(*("{}={}".format(*i) for i in rv.items()), sep=sep, end=sep)
    elif isinstance(rv, (MutableSequence, tuple)):
        print(*rv, sep=sep, end=sep)
    elif isinstance(rv, bool):
        ec = ec or int(not rv)
    elif rv:
        print(rv, end="")
    return ec


if __name__ == "__main__":
    from pathlib import Path
    try:
        sys.path.insert(0, str(Path(__file__).resolve(True).parent.parent))
    except TypeError:  # 3.5
        sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
    from helpers import common, ensure_venv, get_collected

    sys.exit(_main())
