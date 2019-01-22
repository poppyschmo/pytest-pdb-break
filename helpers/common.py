import sys
import subprocess

from pathlib import Path

SUBOUT = subprocess.DEVNULL
SUBERR = subprocess.DEVNULL

_project_root = None
default_install_opts = ("--no-index", "--no-deps", "--upgrade")


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


def install_plugin(destdir, pyexe=None, opts=default_install_opts):
    """Install this project into destdir"""
    project_dir = get_project_root()
    if pyexe is None:
        pyexe = sys.executable
    target = Path(destdir)
    assert target.exists()
    cmdline = [pyexe, "-mpip", "install", *opts,
               "--target", target, project_dir]
    return subprocess.check_call(cmdline, stdout=SUBOUT, stderr=SUBERR)


def copy_plugin(destdir, dummy_content=None):
    """Copy plugin to destdir"""
    destdir = Path(destdir)
    assert destdir.exists()
    dest = destdir / "pytest_pdb_break.py"
    if dummy_content:
        return dest.write_text(dummy_content)
    else:
        project_dir = get_project_root()
        src = project_dir / "pytest_pdb_break.py"
        assert src.exists()
        return dest.write_bytes(src.read_bytes())
