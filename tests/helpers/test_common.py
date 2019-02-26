import pytest

from pathlib import Path
from textwrap import dedent
from unittest.mock import patch

from helpers import common


def test_get_project_root():
    from helpers.common import get_project_root
    common._project_root = None
    assert get_project_root() == common._project_root
    assert isinstance(common._project_root, Path)
    assert common._project_root.is_absolute()
    assert common._project_root.joinpath("setup.py").exists()


def test_version():
    import sys
    from helpers import __version__
    from subprocess import check_output
    from helpers.common import get_project_root
    cmdline = [sys.executable,
               Path(get_project_root()) / "setup.py",
               "--version"]
    out = check_output([str(a) for a in cmdline])
    assert __version__ == out.decode().strip()


def test_copy_plugin(tmp_path):
    from helpers.common import copy_plugin, get_project_root
    wd = tmp_path
    src = get_project_root() / "pytest_pdb_break.py"
    dest = wd / "pytest_pdb_break.py"
    assert copy_plugin(str(wd)) == src.stat().st_size
    assert dest.exists()
    #
    content = "fake\ncontent\n"
    assert copy_plugin(str(wd), content) == len(content)
    assert dest.read_text() == content


# Note: real setup attrs must have a pytest11 entrypoint for
# PluginManager.load_setuptools_entrypoints to detect it
setup_py_src = dedent("""\
   import setuptools
   setuptools.setup(name="pytest_pdb_break", py_modules=["pytest_pdb_break"],
                    long_description=None)
""")


@pytest.mark.parametrize("func", [common._install_plugin_pip,
                                  common._install_plugin_setuptools])
def test_install_plugin(tmp_path, func):
    from helpers.ensure_venv import get_base_pyexe

    wd = tmp_path
    proj = wd / "project"
    targ = wd / "target"
    proj.mkdir()
    targ.mkdir()

    (proj / "pytest_pdb_break.py").touch()
    setup_py = proj / "setup.py"
    setup_py.write_text(setup_py_src)

    stdout = wd / "stdout.out"
    stderr = wd / "stderr.out"

    with patch("helpers.common.get_project_root") as m_pr:
        # Would be nice to use real project, but building lags
        m_pr.return_value = str(proj)
        #
        pyexe = get_base_pyexe()
        suborig = common.SUBOUT, common.SUBERR
        with stdout.open("w") as flout, stderr.open("w") as floer:
            common.SUBOUT, common.SUBERR = flout, floer
            try:
                func(str(targ), pyexe)
            finally:
                common.SUBOUT, common.SUBERR = suborig

    assert (targ / "pytest_pdb_break.py").exists()
    targ_egg = next(targ.glob("*-info"))  # dist- (wheel) or egg-
    assert targ_egg.exists()
    assert next(proj.glob("*-info"), None) is None


@pytest.mark.parametrize("wants_symlink", [True, False])
def test_ensconce(tmp_path, wants_symlink):
    wd = tmp_path
    (wd / "pytest_pdb_break.py").touch()
    setup_py = wd / "setup.py"
    setup_py.write_text(setup_py_src)

    with patch("helpers.common.get_project_root") as m_pr:
        m_pr.return_value = Path(str(wd))
        #
        suborig = common.SUBOUT, common.SUBERR
        stdout, stderr = wd / "stdout.out", wd / "stderr.out"
        with stdout.open("w") as flout, stderr.open("w") as floer:
            common.SUBOUT, common.SUBERR = flout, floer
            try:
                common.ensconce(wants_symlink)
            finally:
                common.SUBOUT, common.SUBERR = suborig

    subdir = wd / common.ISOLATED_LIBDIR / "self"
    targ = subdir / "pytest_pdb_break.py"
    assert targ.is_symlink() is wants_symlink
    targ_egg = next(subdir.glob("*-info"))
    assert targ_egg.exists()
    assert next(wd.glob("*-info"), None) is None
