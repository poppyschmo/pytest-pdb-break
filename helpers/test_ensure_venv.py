import os
import sys
import shutil
import unittest
from pathlib import Path
from textwrap import dedent
from unittest.mock import patch
from unittest.mock import DEFAULT as m_D

from . import ensure_venv

test_path = Path(ensure_venv.TMPROOT) / "ensure_venv"


def clean(*paths):
    import tempfile
    deftemp = tempfile.gettempdir()
    for path in paths:
        if path.exists():
            assert len(path.parts) > 2
            assert str(path).startswith(deftemp)
            shutil.rmtree(path)


def setUpModule():
    test_path.mkdir(parents=True, exist_ok=True)


def make_workdir(name):
    """Make workdir, go there, return Path obj."""
    workdir = test_path / name
    clean(workdir)
    workdir.mkdir()
    os.chdir(workdir)
    assert Path.cwd() == workdir
    return workdir


def make_dummy_venv_tree(path, version=None, exes=None, cfg="pyvenv.cfg"):
    path = Path(path)
    assert not path.exists()
    vbin = path / "bin"
    vbin.mkdir(parents=True)
    if not version:
        version = "%d.%d" % sys.version_info[:2]
    versioned = f"python{version}"
    for dname in ("include", f"lib/{versioned}/site-packages", "share"):
        (path / dname).mkdir(parents=True)
    exes = (exes or []) + ["python", "python3",
                           "pip", "pip3", f"pip{version}",
                           "activate", versioned]
    for fname in set(exes):
        vexe = vbin / fname
        vexe.touch()
        vexe.chmod(0o700)
    if cfg:
        Path(path / cfg).touch()
    return path


class TestEnv(unittest.TestCase):
    def test_is_venv(self):
        from .ensure_venv import is_venv
        wd = make_workdir("Env.is_venv")
        # Baseline
        v0 = wd.joinpath("venv_empty", "bin")
        v0.mkdir(parents=True)
        self.assertTrue(v0.is_absolute())
        rels = Path("venv_empty"), Path("venv_empty") / "bin"
        vals = (*rels, v0, v0.parent, str(v0), str(v0.parent))
        rvs = list(map(is_venv, vals))
        self.assertTrue(not any(rvs))
        self.assertTrue(all(isinstance(v, bool) for v in rvs))
        # Activate
        v1 = Path("venv_act") / "bin"
        v1.mkdir(parents=True)
        (v1 / "activate").touch()
        self.assertFalse(v1.is_absolute())
        self.assertTrue(is_venv(v1))
        self.assertTrue(is_venv(v1.parent))
        # cfg
        v2 = Path("venv_cfg")
        v2.mkdir()
        (v2 / "pyvenv.cfg").touch()
        self.assertTrue(is_venv(v2))
        # Tox
        v3 = Path("venv_tox")
        v3.mkdir()
        (v3 / ".tox-config1").touch()
        self.assertTrue(is_venv(v3))
        # Extra (if running in VIRTUAL_ENV)
        if os.getenv("VIRTUAL_ENV"):
            vbin = Path(sys.executable).parent
            venv = vbin.parent
            self.assertTrue(is_venv(vbin))
            self.assertTrue(is_venv(venv))

    def test_shave_path(self):
        from .ensure_venv import shave_path
        wd = make_workdir("Env.shave_path")
        #
        self.assertEqual(os.pathsep, ":")
        self.assertTrue(os.defpath.startswith(":"))
        expected = os.defpath.lstrip(":")
        with patch("helpers.ensure_venv.is_venv", return_value=True):
            # ~ expanded, passes abspath check
            PATH = "~/fake/.virtualenvs/v/bin" + os.defpath
            self.assertEqual(shave_path(PATH), expected)
        with patch("helpers.ensure_venv.is_venv", return_value=False) as m_iv:
            # Returns orig when no venv provided and check returns false
            self.assertEqual(shave_path(PATH), PATH)
            maybs = Path(os.path.expanduser("~/fake/.virtualenvs/v"))
            m_iv.assert_called_with(maybs)
        with patch("helpers.ensure_venv.is_venv", return_value=False) as m_iv:
            vbin = wd / "venv" / "bin"
            venv = vbin.parent
            PATH = str(vbin) + os.defpath
            self.assertEqual(shave_path(PATH, venv), expected)
            m_iv.assert_not_called()

    @patch.dict("helpers.ensure_venv.os.environ", os.environ.copy())
    def test_get_base_env(self):
        from .ensure_venv import get_base_env
        wd = make_workdir("Env.get_base_env")
        #
        self.assertIs(ensure_venv.os.environ, os.environ)
        vbin = wd / "venv" / "bin"
        venv = vbin.parent
        vbin.mkdir(parents=True)
        (vbin / "activate").touch()
        # VIRTUAL_ENV present
        os.environ["VIRTUAL_ENV"] = str(venv)
        os.environ["PATH"] = str(vbin) + os.defpath
        os.environ["PYTHONPATH"] = str(wd / "lib" / "fake")
        if "PYTHONHOME" in os.environ:
            os.environ.pop("PYTHONHOME")
        result = get_base_env()
        self.assertIsNot(result, os.environ)
        # Originals saved (no PYTHONHOME)
        _st = {"_" + v for v in ("PATH", "VIRTUAL_ENV", "PYTHONPATH")}
        self.assertEqual(_st & result.keys(), _st)
        self.assertNotIn("_PYTHONHOME", result)
        self.assertNotIn("VIRTUAL_ENV", result)
        self.assertNotIn("PYTHONPATH", result)
        self.assertNotEqual(os.environ["PATH"], result["PATH"])
        self.assertFalse(result["PATH"].startswith(str(vbin)))
        self.assertTrue(result["_PATH"].startswith(str(vbin)))
        # VIRTUAL_ENV not present
        self.assertEqual(os.environ.pop("VIRTUAL_ENV"), str(venv))
        result = get_base_env()
        self.assertNotIn("_VIRTUAL_ENV", result)
        self.assertNotEqual(os.environ["PATH"], result["PATH"])
        self.assertFalse(result["PATH"].startswith(str(vbin)))
        self.assertTrue(result["_PATH"].startswith(str(vbin)))

    def make_opt_bin(self, workdir, *exes):
        """Return bin path"""
        obin = workdir / "opt" / "bin"  # standin for pyenv or system
        obin.mkdir(parents=True)
        for exe in exes:
            oexe = obin / exe
            oexe.touch()
            oexe.chmod(0o700)
        return obin

    def test_get_pyvenv_cfg(self):
        from .ensure_venv import get_pyvenv_cfg
        make_workdir("Env.get_pyvenv_cfg")
        #
        venv = make_dummy_venv_tree("venv", version="3.42")
        cfg = venv / "pyvenv.cfg"
        self.assertTrue(cfg.exists())
        cfg.write_text(dedent("""\
            home = /fake/bin
            include-system-site-packages = false
            version = 3.42.1
        """))
        result = get_pyvenv_cfg(venv)
        expected = {"home": "/fake/bin",
                    "include-system-site-packages": "false",
                    "version": "3.42.1"}
        self.assertEqual(result, expected)
        # No parent
        vexe = venv / "bin" / "python3.42"
        self.assertTrue(vexe.exists())
        with patch("helpers.ensure_venv.sys", executable=vexe):
            result = get_pyvenv_cfg(venv)
            self.assertEqual(result, expected)
        # Not found
        cfg.unlink()
        with self.assertRaises(FileNotFoundError):
            get_pyvenv_cfg(venv)

    @patch("helpers.ensure_venv.sys", version_info=(3, 42))
    def test_get_base_pyexe(self, m_sys):
        from .ensure_venv import get_base_pyexe
        wd = make_workdir("Env.get_base_pyexe")
        #
        make_dummy_venv_tree("venv")
        vbin = wd / "venv" / "bin"
        vexe = vbin / "python"
        m_sys.executable = "/fake/bin/python"  # doesn't matter
        # Not a venv
        with patch("helpers.ensure_venv.is_venv", return_value=False) as m_iv:
            # No matching versioned sibling exists
            rv = get_base_pyexe()
            m_iv.assert_called_once_with(Path("/fake/bin"))
            self.assertEqual("/fake/bin/python", rv)
        obin = self.make_opt_bin(wd, "python3.42")
        m_sys.executable = str(obin / "python3")
        with patch("helpers.ensure_venv.is_venv", return_value=False) as m_iv:
            # Matching versioned sibling exists
            rv = get_base_pyexe()
            m_iv.assert_called_once_with(obin)
            self.assertEqual(str(obin / "python3.42"), rv)
        # In virtual env
        m_sys.executable = str(vexe)
        PATH = ":".join(map(str, (vbin, obin)))
        PATH += os.defpath
        env = dict(PATH=PATH, VIRTUAL_ENV=str(vbin.parent))
        with patch.dict("os.environ", env):
            rv = get_base_pyexe()
            self.assertIsInstance(rv, str)
            self.assertTrue(rv.endswith("python3.42"))
            self.assertTrue((obin / "python3.42").samefile(rv))
        # In venv with munged/unreliable PATH
        with patch("helpers.ensure_venv.shave_path", return_value=""), \
                patch("helpers.ensure_venv.get_pyvenv_cfg",
                      return_value={"home": str(obin)}):
            rv = get_base_pyexe()
            self.assertIsInstance(rv, str)
            self.assertTrue(rv.endswith("python3.42"))
            self.assertTrue((obin / "python3.42").samefile(rv))

    @patch("helpers.ensure_venv.get_pyvenv_cfg",
           side_effect=FileNotFoundError("!"))
    @patch("helpers.ensure_venv.shave_path", return_value="")
    @patch("helpers.ensure_venv.sys", version_info=(3, 42))
    def test_get_base_pyexe_fallback(self, m_sys, *args):
        from .ensure_venv import get_base_pyexe
        wd = make_workdir("Env.get_base_pyexe_fallback")
        #
        obin = self.make_opt_bin(wd, "python3.42")
        venv = make_dummy_venv_tree("venv")
        m_sys.executable = str(venv.joinpath("bin", "python"))
        # Indeterminable
        with patch("helpers.ensure_venv.os", defpath=str(obin)), \
                patch("helpers.ensure_venv.subprocess.check_output",
                      return_value=b"Python 3.42.1") as m_co:
            rv = get_base_pyexe()
            self.assertIsInstance(rv, str)
            pyexe = str(obin / "python3.42")
            m_co.assert_called_once_with([pyexe, "--version"])
            self.assertEqual(rv, pyexe)
        # Version mismatch
        with patch("helpers.ensure_venv.os", defpath=str(obin)), \
                patch("helpers.ensure_venv.subprocess.check_output",
                      return_value=b"Python 3.33.1"), \
                self.assertRaises(RuntimeError):
            get_base_pyexe()

    def test_is_pyenv_shim(self):
        from .ensure_venv import is_pyenv_shim
        wd = make_workdir("Env.is_pyenv_shim")
        #
        path = shutil.which("python", path=os.defpath)
        # File prog may not return non zero for nonexistent path
        self.assertTrue(Path(path).exists())
        self.assertFalse(is_pyenv_shim(path))
        self.assertFalse(is_pyenv_shim(Path(path)))
        #
        venv = make_dummy_venv_tree(wd / "venv")
        vexe = venv.joinpath("bin", "python")
        vexe.write_text("PYENV")
        self.assertTrue(Path(path).exists())
        self.assertTrue(is_pyenv_shim(vexe))

    @patch("helpers.ensure_venv.sys", version_info=(3, 42))
    def test_update_shim_env(self, *m_args):
        from .ensure_venv import update_shim_env
        wd = make_workdir("Env.update_shim_env")
        exe = wd / "bin" / "python3.33"
        env = {}
        update_shim_env(env, exe)
        self.assertEqual(env, {"PYENV_VERSION": "3.33"})
        #
        exe = wd / "bin" / "python3"
        update_shim_env(env, exe)
        self.assertEqual(env, {"PYENV_VERSION": "3"})
        #
        exe = wd / "bin" / "python"
        update_shim_env(env, exe)
        self.assertEqual(env, {"PYENV_VERSION": "3.42"})
        #
        env["PYENV_VERSION"] += ":4:5:6"
        update_shim_env(env, exe)
        self.assertEqual(env, {"PYENV_VERSION": "3.42:4:5:6"})


class TestGlobals(unittest.TestCase):
    def test_globals(self):
        import tempfile
        self.assertTrue(ensure_venv.TMPROOT.startswith(tempfile.gettempdir()))

    def test_ensure_venvdir(self):
        from .ensure_venv import ensure_venvdir
        try:
            self.assertIsNone(ensure_venv._tmp_venvdir)
            result = ensure_venvdir()
            self.assertEqual(result, ensure_venv._tmp_venvdir)
            self.assertTrue(isinstance(result, Path))
            self.assertTrue(result.exists())
            self.assertTrue(ensure_venv.SUBDIR in result.parts)
        finally:
            ensure_venv._tmp_venvdir = None


class TestGetPyExe(unittest.TestCase):
    def test_bad_name(self):
        from .ensure_venv import get_pyexe
        with self.assertRaises(ValueError):
            get_pyexe("fake")

    @patch("helpers.ensure_venv.sys", version_info=(3, 42))
    @patch("helpers.ensure_venv.get_base_pyexe", side_effect=RuntimeError)
    @patch("helpers.ensure_venv.ensure_venvdir")
    def test_exists(self, m_ev, m_grp, m_sys):
        from .ensure_venv import get_pyexe
        wd = make_workdir("GetPyExe.exists")
        #
        bare = make_dummy_venv_tree("bare", version=3.42)
        bexe = bare / "bin" / "python3.42"
        bexe = bexe.resolve(True)
        m_ev.return_value = wd
        result = get_pyexe("bare")
        m_ev.assert_called_once()
        m_grp.assert_not_called()
        self.assertEqual(result, bexe)

    def fake_check_call(self, cmd, *args, **kwargs):
        if "-mvenv" in cmd:
            make_dummy_venv_tree(cmd[-1])

    @patch.multiple("helpers.ensure_venv",
                    get_base_env=m_D, ensure_venvdir=m_D, is_pyenv_shim=m_D)
    @patch("helpers.ensure_venv.subprocess.check_call")
    def test_pypi(self, m_cc, ensure_venvdir, get_base_env, is_pyenv_shim):
        from .ensure_venv import get_pyexe
        from . import common
        wd = make_workdir("GetPyExe.pypi")
        m_cc.side_effect = self.fake_check_call
        ensure_venvdir.return_value = wd
        get_base_env.return_value = dict(PATH="foo:bar")
        is_pyenv_shim.return_value = False
        #
        result = get_pyexe("base")
        ensure_venvdir.assert_called_once()
        is_pyenv_shim.assert_called_once()
        versioned = "python%d.%d" % sys.version_info[:2]
        self.assertEqual(result, wd / "base" / "bin" / versioned)
        from unittest.mock import call
        sysexe = ensure_venv.get_base_pyexe()
        pyexe = wd.joinpath("base", "bin", versioned)
        m_cc.assert_has_calls([
            call([sysexe, "-mvenv", wd / "base"], env={"PATH": "foo:bar"},
                 stdout=common.SUBOUT, stderr=common.SUBERR),
            call([pyexe, "-mpip", "install", "pytest"],
                 stdout=common.SUBOUT, stderr=common.SUBERR)
        ])

    @patch("helpers.common.get_project_root", return_value="<proj root>")
    @patch("helpers.ensure_venv.is_pyenv_shim", return_value=False)
    @patch("helpers.ensure_venv.get_base_env")
    @patch("helpers.ensure_venv.subprocess.check_call")
    @patch("helpers.ensure_venv.ensure_venvdir")
    def test_local(self, m_ev, m_cc, m_be, *m_args):
        from .ensure_venv import get_pyexe
        from . import common
        wd = make_workdir("GetPyExe.local")
        m_cc.side_effect = self.fake_check_call
        m_ev.return_value = wd
        m_be.return_value = dict(PATH="foo:bar")
        #
        result = get_pyexe("self")
        m_ev.assert_called_once()
        versioned = "python%d.%d" % sys.version_info[:2]
        self.assertEqual(result, wd / "self" / "bin" / versioned)
        from unittest.mock import call
        sysexe = ensure_venv.get_base_pyexe()
        pyexe = wd.joinpath("self", "bin", versioned)
        m_cc.assert_has_calls([
            call([sysexe, "-mvenv", wd / "self"], env={"PATH": "foo:bar"},
                 stdout=common.SUBOUT, stderr=common.SUBERR),
            call([pyexe, "-mpip", "install", "<proj root>"],
                 stdout=common.SUBOUT, stderr=common.SUBERR)
        ])
