# This file is part of https://github.com/poppyschmo/pytest-pdb-break
import os
import sys
import shutil
import subprocess

from pathlib import Path

# These are only used for install-related subprocs, like venv, pip, setup.py
SUBOUT = subprocess.DEVNULL
SUBERR = subprocess.DEVNULL

ISOLATED_LIBDIR = ".lib"

_project_root = None
default_pip_opts = ("--no-index", "--no-deps", "--upgrade")


def get_project_root():
    """Return path to *this* project's root"""
    global _project_root
    if not _project_root:
        # Ensure this module was imported
        root = Path(__file__).parent.parent
        assert root.is_absolute(), "root: {}".format(root)
        assert root.joinpath("setup.py").exists()
        _project_root = root
    return _project_root


def _call_setup_in_subproc(cmdline, project_dir):
    if SUBOUT is not subprocess.DEVNULL:
        env = os.environ.copy()
        env.update(DISTUTILS_DEBUG="1")
    else:
        env = os.environ
    try:
        return subprocess.check_call([str(a) for a in cmdline],
                                     cwd=str(project_dir), env=env,
                                     stdout=SUBOUT, stderr=SUBERR)
    finally:
        for subdir in ("build", "pytest_pdb_break.egg-info"):
            path = Path(project_dir) / subdir
            if path.exists():
                shutil.rmtree(str(path))


def _install_plugin_setuptools(destdir, pyexe):
    """Install this project into destdir using setuptools
    """
    project_dir = get_project_root()
    cmdline = [pyexe] + """
        setup.py
            install_lib --install-dir {destdir}
            install_egg_info --install-dir {destdir}
            clean --all
    """.format(destdir=destdir).split()
    return _call_setup_in_subproc(cmdline, project_dir)


def _install_plugin_pip(destdir, pyexe, opts=default_pip_opts):
    """Install this project into destdir using pip"""
    project_dir = get_project_root()
    target = Path(destdir)
    assert target.exists(), target
    cmdline = [pyexe, "-mpip", "install"]
    cmdline += list(opts) + ["--target", target, project_dir]
    return subprocess.check_call([str(a) for a in cmdline],
                                 stdout=SUBOUT, stderr=SUBERR)


def install_plugin(destdir, pyexe=None, use_pip=False, **kwargs):
    """Install this project into destdir"""
    if pyexe is None:
        pyexe = sys.executable
    if use_pip:
        return _install_plugin_pip(destdir, pyexe, **kwargs)
    else:
        return _install_plugin_setuptools(destdir, pyexe)


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


def ensconce(symlink=True):
    """Create a sequestered installation in project root
    """
    project_dir = Path(get_project_root())
    subdir = project_dir / ISOLATED_LIBDIR / "self"
    if subdir.exists():
        shutil.rmtree(subdir)
    if symlink:
        subdir.mkdir(parents=True)
        link_targ = subdir / "pytest_pdb_break.py"
        link_targ.symlink_to("../../pytest_pdb_break.py")
        cmdline = [sys.executable, "setup.py",
                   "install_egg_info", "--install-dir", subdir]
        return _call_setup_in_subproc(cmdline, project_dir)
    else:
        return _install_plugin_setuptools(subdir, sys.executable)
