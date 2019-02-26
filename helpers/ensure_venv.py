# This file is part of https://github.com/poppyschmo/pytest-pdb-break
import os
import sys
import shutil
import subprocess
from pathlib import Path

NAMES = ("bare", "base", "self")
DEFAULT = "/tmp/pytest-pdb-break-test"
TMPROOT = os.getenv("PYTEST_PDB_BREAK_TEST_TEMPDIR") or DEFAULT
SUBDIR = ".venvs"
_tmp_venvdir = None


def is_pyenv_shim(path):
    """Return True if path is a pyenv shim"""
    out = subprocess.check_output(["file", "-b", "-L", "--mime-type",
                                   str(path)])
    out = out.decode()
    if out.startswith("text"):
        return "PYENV" in Path(path).read_text()
    elif out.startswith("application"):
        return False
    else:
        raise RuntimeError("Unrecognized file type {} for {}"
                           .format(out, path))


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
        venv = maybe.parent
        if not is_venv(venv):
            return path
        return rest
    venv = Path(os.path.expanduser(str(venv)))
    if not venv.is_absolute():
        try:
            venv = venv.resolve(True)
        except TypeError:
            venv = venv.resolve()
    if (Path(os.path.commonpath((str(venv), str(maybe))))
            == venv or is_venv(maybe)):
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
        raise FileNotFoundError("No pyvenv.cfg in {}".format(parent))
    from configparser import ConfigParser
    cfg = ConfigParser()
    cfg.read_string("[main]\n" + path.read_text())
    return dict(cfg["main"])


def get_base_pyexe():
    """Find a non-virtual-env-based python executable

    If not in an active virtual environment, sys.executable or a
    version-suffixed sibling is returned.

    The returned exe may not be the one that created the current venv.
    An exception is raised when versions don't match.
    """
    assert sys.version_info[0] == 3
    # With tox (and pyenv), base_prefix is same as prefix
    version = "%d.%d" % sys.version_info[:2]
    if not is_venv(Path(sys.executable).parent):
        if not sys.executable.endswith(version):
            versioned = Path(sys.executable).parent.joinpath("python{}"
                                                             .format(version))
            if versioned.exists():
                return str(versioned)
        return sys.executable
    path = shave_path(os.getenv("PATH"), os.getenv("VIRTUAL_ENV"))
    found = shutil.which("python{}".format(version), path=path)
    if found:
        # XXX why realpath here (and not elsewhere)?
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
        raise ValueError("{!r} must be one of {}".format(name, NAMES))
    venv = ensure_venvdir() / name
    version = "%d.%d" % sys.version_info[:2]
    pyexe = venv / "bin" / "python{}".format(version)
    if not pyexe.exists():
        from .common import SUBERR, SUBOUT, get_project_root
        sysexe = get_base_pyexe()
        env = get_base_env()
        if is_pyenv_shim(sysexe):
            update_shim_env(env, sysexe)

        def strung(*args):
            return [str(a) for a in args]

        subprocess.check_call(strung(sysexe, "-mvenv", venv), env=env,
                              stdout=SUBOUT, stderr=SUBERR)
        assert pyexe.exists(), "{!r} exists".format(pyexe)
        if not (venv / "bin" / "pip").exists():
            from warnings import warn
            warn("{} did not create a pip in {}".format(sysexe, pyexe.parent))
        if name != "bare":
            if name == "base":
                subprocess.check_call(
                    strung(pyexe, "-mpip", "install", "pytest"),
                    stdout=SUBOUT, stderr=SUBERR
                )
            elif name == "self":
                subprocess.check_call(strung(pyexe, "-mpip", "install",
                                             get_project_root()),
                                      stdout=SUBOUT, stderr=SUBERR)
    return pyexe
