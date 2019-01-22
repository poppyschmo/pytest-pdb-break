from pathlib import Path
from textwrap import dedent
from unittest.mock import patch

from . import common


def test_get_project_root():
    from .common import get_project_root
    common._project_root = None
    assert get_project_root() == common._project_root
    assert isinstance(common._project_root, Path)
    assert common._project_root.is_absolute()


def test_version():
    import sys
    from . import __version__
    from subprocess import check_output
    from .common import get_project_root
    cmdline = [sys.executable,
               Path(get_project_root()) / "setup.py",
               "--version"]
    out = check_output(cmdline)
    assert __version__ == out.decode().strip()


def test_copy_plugin(tmp_path):
    from .common import copy_plugin, get_project_root
    wd = tmp_path
    src = get_project_root() / "pytest_pdb_break.py"
    dest = wd / "pytest_pdb_break.py"
    assert copy_plugin(wd) == src.stat().st_size
    assert dest.exists()
    #
    content = "fake\ncontent\n"
    assert copy_plugin(wd, content) == len(content)
    assert dest.read_text() == content


def test_install_plugin(tmp_path):
    from .common import install_plugin
    from .ensure_venv import get_base_pyexe
    wd = tmp_path
    with patch("helpers.common.get_project_root") as m_pr:
        # Would be nice to use real project, but building lags
        proj = wd / "project"
        proj.mkdir()
        (proj / "pytest_pdb_break.py").touch()
        setup_py = proj / "setup.py"
        # Note: real setup() must have a pytest11 entrypoints kwarg for
        # PluginManager.load_setuptools_entrypoints to detect it
        setup_py.write_text(dedent("""\
            import setuptools
            setuptools.setup(name="pytest_pdb_break",
                                py_modules=["pytest_pdb_break"])
        """))
        m_pr.return_value = str(proj)
        targ = wd / "target"
        targ.mkdir()
        #
        pyexe = get_base_pyexe()
        install_plugin(targ, pyexe)
        egg = next(targ.glob("*-info"))  # dist- (wheel) or egg-
        assert egg.exists()
        assert (targ / "pytest_pdb_break.py").exists()
