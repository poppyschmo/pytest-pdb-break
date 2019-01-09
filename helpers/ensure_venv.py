import os
import sys
import shutil
from pathlib import Path
import subprocess

NAMES = ("bare", "base", "self")
DEFAULT = "/tmp/pytest-pdb-break-test"
TMPROOT = os.getenv("PYTEST_PDB_BREAK_TEST_TEMPDIR") or DEFAULT
SUBDIR = ".venvs"
_project_root = None
_tmp_venvdir = None


def get_project_root():
    """Return path to *this* project's root"""
    global _project_root
    if not _project_root:
        # __file__ may be be empty or relative
        root = Path(__file__).parent.parent
        assert root.is_absolute(), f"root: {root}"
        assert root.joinpath("tox.ini").exists()
        _project_root = root
    return _project_root


def _copy_to_libpath(libdir):
    if not hasattr(_copy_to_libpath, "src"):
        project_dir = get_project_root()
        src = project_dir / "pytest_pdb_break.py"
        assert src.exists()
        _copy_to_libpath.src = src.read_bytes()
    assert libdir.exists()
    plugin_copy = libdir / "pytest_pdb_break.py"
    return plugin_copy.write_bytes(_copy_to_libpath.src)


def _install_to_libpath(libdir, pyexe, opts=("--no-deps",)):
    """Install this project into libdir using pyexe"""
    project_dir = get_project_root()
    assert libdir.exists()
    cmdline = [pyexe, "-mpip", "install"]
    cmdline += list(opts) + ["--target", libdir, project_dir]
    return subprocess.check_call(cmdline,
                                 stdout=subprocess.DEVNULL,
                                 stderr=subprocess.DEVNULL)


def make_libpath(cwd=None, path="lib/site-packages", **install_kwargs):
    """Copy or install plugin to <path>

    Return path suitable for PYTHONPATH. With install_kwargs, install
    instead of copy. Note: pathlib.Path objs passed as values in a
    subprocess.Popen env dict are converted to strings by os.fsencode.
    """
    if cwd is None:
        cwd = Path.cwd()
    cwd, _cwd = Path(cwd), cwd
    libdir = cwd / path
    libdir.mkdir(parents=True)
    if install_kwargs:
        _install_to_libpath(libdir, **install_kwargs)
    else:
        _copy_to_libpath(libdir)
    assert libdir.joinpath("pytest_pdb_break.py").exists()
    return type(_cwd)(libdir)


def is_pyenv_shim(path):
    """Return True if path is a pyenv shim"""
    out = subprocess.check_output(["file", "-b", "-L", "--mime-type", path])
    out = out.decode()
    if out.startswith("text"):
        return "PYENV" in Path(path).read_text()
    elif out.startswith("application"):
        return False
    else:
        raise RuntimeError(f"Unrecognized file type {out} for {path}")


def update_shim_env(env, exe):
    """Add pyenv version env var for exe to env"""
    vx = str(Path(exe).name).replace("python", "")
    if not vx:
        vx = "%d.%d" % sys.version_info[:2]
    if "PYENV_VERSION" not in env or vx not in env["PYENV_VERSION"].split(":"):
        # Just overwrite, don't prepend
        env.update(PYENV_VERSION=vx)


def is_venv(path):
    """Return true if path is a virtual-env or venv bin"""
    # Site.py looks for pyvenv.cfg in sys.executable's parent
    path = Path(path)
    if path.name == "bin":
        path = path.parent
    return bool((path / "bin" / "activate").exists()
                or (path / "pyvenv.cfg").exists()
                or next(path.glob(".tox-config*"), None))


def shave_path(path, venv=None):
    """Return path, possibly without first element"""
    maybe, __, rest = path.partition(os.pathsep)
    maybe = Path(os.path.expanduser(maybe))
    assert maybe.is_absolute()
    if not venv:
        venv = Path(maybe).parent
        if not is_venv(venv):
            raise RuntimeError("venv could not be determined")
        return rest
    # Assume venv passes is_venv
    venv = Path(os.path.expanduser(venv))
    assert venv.is_absolute()
    if Path(os.path.commonpath((venv, maybe))) == venv or is_venv(maybe):
        return rest
    return path


def get_base_env(stash=None, prefix="_"):
    """Return copy of env without virtual-env PATH modification

    Also remove VIRTUAL_ENV, PYTHONPATH, PYTHONHOME or those in list
    <stash>, if supplied. Include originals with <prefix>, to aid in
    detecting removal.

    This doesn't mess with PYENV_*.
    """
    env = os.environ.copy()
    venv = env.get("VIRTUAL_ENV")
    path = env["PATH"]
    if stash is None:
        stash = ("VIRTUAL_ENV", "PYTHONPATH", "PYTHONHOME")
    for var in stash:
        try:
            env[prefix + var] = env.pop(var)
        except KeyError:
            pass
    shaved = shave_path(path, venv)
    if shaved != path:
        env[prefix + "PATH"] = path
        env["PATH"] = shaved
    return env


def get_pyvenv_cfg(parent=None):
    """Return the contents of a pyvenv.cfg as a dict"""
    if parent is None:
        parent = Path(sys.executable).parent.parent
    path = Path(parent) / "pyvenv.cfg"
    if not path.exists():
        raise FileNotFoundError(f"No pyvenv.cfg in {parent}")
    from configparser import ConfigParser
    cfg = ConfigParser()
    cfg.read_string("[main]\n" + path.read_text())
    return dict(cfg["main"])


def get_base_pyexe():
    """Find a non-virtual-env-based python executable

    If not in an active virtual environment, sys.executable is returned.
    The returned exe may not be the one that created the current venv.
    An exception is raised is versions don't match.
    """
    assert sys.version_info[0] == 3
    # With tox (and pyenv), base_prefix is same as prefix
    if not is_venv(Path(sys.executable).parent):
        return sys.executable
    version = "%d.%d" % sys.version_info[:2]
    path = shave_path(os.getenv("PATH"), os.getenv("VIRTUAL_ENV"))
    found = shutil.which("python" + version, path=path)
    if found:
        return os.path.realpath(found)
    # The previous will find the reigning pyenv shim, which may be preferable
    # to whatever created the now deactivated venv
    try:
        path = get_pyvenv_cfg().get("home")
        homed = shutil.which("python" + version, path=path)
    except FileNotFoundError:
        pass
    else:
        if homed:
            return os.path.realpath(homed)
    # Return system python if minor version matches
    path = os.defpath
    system = (shutil.which("python" + version, path=path)
              or shutil.which("python3", path=path)
              or shutil.which("python", path=path))
    out = subprocess.check_output([system, "--version"])
    if out.decode().split()[-1].startswith(version):
        return system
    raise RuntimeError("Could not determine base python exe")


def ensure_venvdir():
    """Create subdir for virtual-env versions in project tempdir"""
    global _tmp_venvdir
    if _tmp_venvdir is None:
        root = Path(TMPROOT)
        version = "%d.%d" % sys.version_info[:2]
        _tmp_venvdir = root / SUBDIR / version
        if not _tmp_venvdir.exists():
            _tmp_venvdir.mkdir(parents=True)
    return _tmp_venvdir


def get_pyexe(name):
    "Return path to python executable in temporary venv"
    if name not in NAMES:
        raise ValueError(f"{name!r} must be one of {NAMES}")
    venv = ensure_venvdir() / name
    version = "%d.%d" % sys.version_info[:2]
    pyexe = venv / "bin" / f"python{version}"
    if not pyexe.exists():
        sysexe = get_base_pyexe()
        env = get_base_env()
        if is_pyenv_shim(sysexe):
            update_shim_env(env, sysexe)
        subprocess.check_call([sysexe, "-mvenv", venv], env=env)
        assert pyexe.exists()
        if not (venv / "bin" / "pip").exists():
            from warnings import warn
            warn(f"{sysexe} did not create a pip in {pyexe.parent}")
        if name != "bare":
            if name == "base":
                subprocess.check_call([pyexe, "-mpip", "install", "pytest"])
            elif name == "self":
                subprocess.check_call([pyexe, "-mpip",
                                       "install", get_project_root()])
    return pyexe


def _main():
    import argparse
    from inspect import signature
    from types import FunctionType
    from collections.abc import MutableMapping, MutableSequence

    def debug_cmdline(*args, **kwargs):
        if not kwargs.get("quiet"):
            print(f"--json: {pargs.json!r}",
                  f"--kwargs: {pargs.kwargs!r}",
                  f"--null: {pargs.null!r}",
                  f"args: {args!r}",
                  f"kwargs: {kwargs!r}",
                  sep="\n")
        return kwargs.get("rv")

    commands = {k: v for k, v in globals().items() if
                isinstance(v, FunctionType) and not k.startswith("_")}

    ap = argparse.ArgumentParser(
        prog="ensure_venv",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="\n".join(
            f"  \x1b[1m{n}\x1b[m{signature(o)}" +
            ("\n    {}".format(o.__doc__.strip().split("\n")[0])
             if o.__doc__ else "") for n, o in commands.items()
        ),
        description="Helpers for integrations tests using virtual environments"
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

    rv = pargs.cmd(*args, **kwargs)

    sep = "\x00" if pargs.null else "\n"

    if pargs.json:
        import json
        json.dump(rv, sys.stdout)
    elif isinstance(rv, MutableMapping):
        print(*(f"{k}={v}" for k, v in rv.items()), sep=sep, end=sep)
    elif isinstance(rv, (MutableSequence, tuple)):
        print(*rv, sep=sep, end=sep)
    elif isinstance(rv, bool):
        return int(not rv)
    elif rv:
        print(rv, end="")


if __name__ == "__main__":
    __file__ = Path(__file__).resolve(True)
    sys.exit(_main())
