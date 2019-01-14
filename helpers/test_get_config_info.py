"""
Takeaways
    1. The presence/veracity of (pytest) file/node-id args doesn't impact the
       result
    2. Options do affect the result
    3. If an egg for a plugin exists under a sys.path entry, pytest finds it
       without -p <plugin>
    4. Otherwise the plugin must be declared explicitly somewhere, via via
       cmdline, ini file, conftest, ``main(... plugins=[...])``, etc.  Just
       being importable isn't enough (even if plugin exists in rootdir).
    5. An exception is raised if a declared plugin isn't importable
"""
import os
import sys
import json
import pytest
import subprocess
from pathlib import Path
from textwrap import dedent
from _pytest.pytester import LineMatcher
from .ensure_venv import (get_pyexe, get_base_pyexe, is_venv,
                          make_libpath, get_base_env)


def run_with_input(cmdline, instr, tmpdir, prefix=None, env_vars=None):
    """
    Note
        ``subprocess.Popen`` env dict values can be pathlike objects.
        Relevant for calling ``ensure_venv.make_libpath``.

    >>> from unittest.mock import patch
    >>> with patch("os.fsencode", wraps=os.fsencode) as m_of:
    ...     d = dict(SOMEVAR=Path("/tmp/foo"))
    ...     cmd = "bash", "-c", "declare -p SOMEVAR"
    ...     out = subprocess.check_output(cmd, env=d)
    ...     m_of.assert_any_call(d["SOMEVAR"])
    ...     assert b"/tmp/foo" in out
    ...     assert "Path(" in repr(d["SOMEVAR"])
    ...     assert b"Path(" not in out
    ...     try:
    ...         os.fsencode(...)
    ...     except TypeError:
    ...         pass
    """
    p = prefix and f"{prefix}." or ""
    env = get_base_env()
    if env_vars:
        env.update(env_vars)
    tmpdir.join(f"{p}env").write("\n".join(f"{k}={v}" for k, v in env.items()))
    tmpdir.join(f"{p}info").write(dedent(f"""\
        cwd: {os.getcwd()}
        cmdline: {cmdline!r}
        real pyexe: {get_base_pyexe()}
        sys.executable: {sys.executable}
        is ^ venv?: {is_venv(Path(sys.executable).parent)}
    """))
    PIPE = subprocess.PIPE
    # Args in cmdline can be pathlib.Path objs
    proc = subprocess.Popen(cmdline,
                            stdin=PIPE, stdout=PIPE, stderr=PIPE,
                            env=env)
    assert isinstance(instr, str)
    try:
        out, errs = proc.communicate(input=instr.encode("utf-8"), timeout=1.0)
    except subprocess.TimeoutExpired:
        proc.kill()
        out, errs = proc.communicate()
    tmpdir.join(f"{p}stdin").write(instr)
    tmpdir.join(f"{p}stdout").write(out)
    tmpdir.join(f"{p}stderr").write(errs)
    return proc.returncode, out.decode(), errs.decode()


@pytest.fixture(scope="module")
def module_tmpdir(tmpdir_factory):
    return tmpdir_factory.mktemp("external_lib", numbered=True)


@pytest.fixture(scope="module")
def ext_plugin(module_tmpdir):
    """Return LocalPath to some lib/site-packages containing plugin
    """
    path = make_libpath(module_tmpdir / "sans_egg")
    # Should only contain pytest_pdb_break.py
    pit = Path(path).iterdir()
    assert list(p.name for p in pit) == ["pytest_pdb_break.py"]
    return path


@pytest.fixture(scope="module")
def ext_fake(module_tmpdir):
    from unittest.mock import patch
    content = dedent("""\
        def pytest_addoption(parser):
            group = parser.getgroup("pdb")
            group.addoption("--break", dest="pdb_break")

        def pytest_configure(config):
            wanted = config.getoption("pdb_break")
            if wanted:
                print("fake-pdb-break:", wanted)
    """).encode("utf-8")
    from .ensure_venv import _copy_to_libpath as orig
    with patch("helpers.ensure_venv._copy_to_libpath", src=content,
               wraps=orig):
        path = make_libpath(module_tmpdir / "fake")
    return path


@pytest.fixture(scope="module")
def ext_egg(module_tmpdir):
    """Like ext_plugin but with egg-/dist-info installed by 'bare'
    """
    return make_libpath(module_tmpdir / "with_egg", pyexe=get_pyexe("bare"))


def test_baseline(testdir, request, ext_plugin):
    instr = dedent("""
        import sys
        print(*sys.path, sep="\\n")
    """)
    pyexe = get_pyexe("bare")

    # As stdin
    path_check_dir = testdir.tmpdir.mkdir("path_check")
    path_check_dir.chdir()
    result = run_with_input([pyexe], instr, path_check_dir)

    # As exec string arg
    path_check_c_dir = testdir.tmpdir.mkdir("path_check_c")
    path_check_c_dir.chdir()
    assert result == run_with_input([pyexe, "-c", instr],
                                    "", path_check_c_dir)

    rc, out, errs = result
    assert rc == 0 and out and not errs
    outlines = out.splitlines()
    assert str(testdir.tmpdir) not in outlines
    assert str(request.config.rootdir) not in outlines
    assert str(request.config.invocation_dir) not in outlines

    # With modified path
    instr = dedent("""
        import os
        import sys
        print("PYTHONPATH:", os.getenv("PYTHONPATH"))
        print(*sys.path, sep="\\n")
    """)
    libdir = ext_plugin
    env_dir = testdir.tmpdir.mkdir("env_dir")
    env_dir.chdir()
    rc, out, errs = run_with_input([pyexe], instr, env_dir,
                                   env_vars=dict(PYTHONPATH=libdir))
    assert rc == 0 and out and not errs
    lines = LineMatcher(out.splitlines())
    lines.fnmatch_lines(f"PYTHONPATH*{libdir}*")
    lines.fnmatch_lines(libdir.strpath)


@pytest.fixture(scope="module")
def source():
    from . import get_config_info  # noqa: F401
    m = sys.modules["helpers.get_config_info"]
    return m.__loader__.get_source(m.__name__)


@pytest.mark.parametrize("subdir, wantcd",
                         [("", False), ("subdir", True), ("subdir", False)])
def test_unregistered(testdir, source, subdir, wantcd, ext_plugin):
    rootdir = testdir.tmpdir
    pyexe = get_pyexe("base")
    testdir.makeini("[pytest]\n")

    relfile = os.path.join(subdir, "test_file")
    subdir = subdir and rootdir.mkdir(subdir) or rootdir
    testdir.makepyfile(**{relfile: """
        def test_foo():
            assert True
    """})

    if wantcd:
        subdir.chdir()
    invocation_dir = type(rootdir)()
    testfile = subdir.join("test_file.py")

    # No node-id
    rc, out, errs = rv = run_with_input([pyexe], source, invocation_dir,
                                        "no_nodeid")
    assert rc == 0 and out and not errs
    json.loads(out) == {"rootdir": rootdir, "registered": False}

    # Real node-id
    assert rv == run_with_input([pyexe, "-", f"{testfile}::test_foo"],
                                source, invocation_dir, "real_nodeid")

    # Fake node-id
    assert rv == run_with_input([pyexe, "-", "/tmp/fake__/fake.py::test_fake"],
                                source, invocation_dir, "fake_nodeid")

    # Importable, not installed, not requested
    assert rv == run_with_input([pyexe],
                                source, invocation_dir, prefix="nopip_nopopt",
                                env_vars=dict(PYTHONPATH=ext_plugin))

    # Explicit rootdir as option
    rc, out, errs = run_with_input([pyexe, "-", f"--rootdir={str(subdir)}"],
                                   source, invocation_dir, "rootdir_opt")
    assert rc == 0 and out and not errs
    assert json.loads(out) == {"rootdir": subdir, "registered": False}

    # Bad option
    rc, out, errs = run_with_input([pyexe, "-", "--fake"],
                                   source, invocation_dir, "bad_opt")
    assert rc != 0 and not out and errs
    with pytest.raises(json.JSONDecodeError):
        json.loads(out)
    lines = LineMatcher(errs.splitlines())
    lines.fnmatch_lines("*unrecognized argument*")


def new_repo(testdir, name="test_file", subdir=None, content=None, ini=""):
    """
    Note: kwargs don't have to be identifiers::
        >>> (lambda **kw: kw)(**{"a b c": 1})
        {'a b c': 1}
    """
    if content is None:
        content = """
            def test_foo():
                assert True
        """
    if subdir:
        r = testdir.tmpdir.mkdir(subdir)
        r.chdir()
        pyload = {f"{subdir}/{name}": content}
        testdir.makefile(".ini", **{f"{subdir}/tox": f"[pytest]\n{ini}"})
    else:
        pyload = {name: content}
        testdir.makeini(f"[pytest]\n{ini}")
        r = testdir.tmpdir
    testdir.makepyfile(**pyload)
    return r


def test_registered_local_popt(testdir, source, ext_fake):
    # Without -p option behaves identical to importable, non-repo
    repo = new_repo(testdir)
    pyexe = get_pyexe("base")

    localfile = repo / "pytest_pdb_break.py"
    localfile.write_binary(ext_fake.join("pytest_pdb_break.py").read_binary())

    rc, out, errs = run_with_input([pyexe, "-", "-p", "pytest_pdb_break"],
                                   source, repo)
    assert rc == 0 and out and not errs
    assert json.loads(out) == {"rootdir": repo, "registered": True}

    # Plugin shows up in help
    rc, out, errs = run_with_input([pyexe, "-mpytest",
                                    "-p", "pytest_pdb_break", "--help"],
                                   "", repo, "pt_help")
    assert rc == 0 and out and not errs
    LineMatcher(out.splitlines()).fnmatch_lines("*--break=*")

    # Plugin not mentioned in in report but still runs
    rc, out, errs = run_with_input([pyexe, "-mpytest",
                                    "-p", "pytest_pdb_break", "--break=99"],
                                   "", repo, "pt_opt")
    assert rc == 0 and out and not errs
    assert "plugins: pdb-break" not in out
    LineMatcher(out.splitlines()).fnmatch_lines("*fake-pdb-break*99*")


def test_registered_pip_nopopt(testdir, source, ext_egg):
    # External installation, not requested via -p option
    repo = new_repo(testdir)
    pyexe = get_pyexe("base")

    rc, out, errs = run_with_input([pyexe], source, repo,
                                   env_vars=dict(PYTHONPATH=ext_egg))
    assert rc == 0 and out and not errs
    assert json.loads(out) == {"rootdir": repo, "registered": True}

    rc, out, errs = run_with_input([pyexe, "-mpytest", "--break=99"],
                                   "", repo, "pt",
                                   env_vars=dict(PYTHONPATH=ext_egg))
    assert rc == 0 and out and not errs
    LineMatcher(out.splitlines()).fnmatch_lines("plugins: pdb-break*")


def test_registered_nopip_popt(testdir, source, ext_plugin, ext_fake):
    # Importable, not installed, requested
    repo = new_repo(testdir)
    pyexe = get_pyexe("base")

    rc, out, errs = run_with_input([pyexe, "-", "-p", "pytest_pdb_break"],
                                   source, repo,
                                   env_vars=dict(PYTHONPATH=ext_plugin))
    assert rc == 0 and out and not errs
    assert json.loads(out) == {"rootdir": repo, "registered": True}

    # Plugin shows up in help
    rc, out, errs = run_with_input([pyexe, "-mpytest",
                                    "-p", "pytest_pdb_break", "--help"],
                                   "", repo, "pt_help",
                                   env_vars=dict(PYTHONPATH=ext_plugin))
    assert rc == 0 and out and not errs
    LineMatcher(out.splitlines()).fnmatch_lines("*--break=*")

    # Plugin not mentioned in in report
    rc, out, errs = run_with_input([pyexe, "-mpytest",
                                    "-p", "pytest_pdb_break", "--break=99"],
                                   "", repo, "pt_opt",
                                   env_vars=dict(PYTHONPATH=ext_plugin))
    assert rc == 0 and out and not errs
    assert "plugins: pdb-break" not in out

    # Plugin runs
    rc, out, errs = run_with_input([pyexe, "-mpytest",
                                    "-p", "pytest_pdb_break", "--break=99"],
                                   "", repo, "pt_fake",
                                   env_vars=dict(PYTHONPATH=ext_fake))
    assert rc == 0 and out and not errs
    LineMatcher(out.splitlines()).fnmatch_lines("*fake-pdb-break*99*")


def test_registered_nopip_iniopt(testdir, source, ext_plugin, ext_fake):
    repo = new_repo(testdir, ini="addopts = -p pytest_pdb_break\n")
    pyexe = get_pyexe("base")
    rc, out, errs = run_with_input([pyexe],
                                   source, repo,
                                   env_vars=dict(PYTHONPATH=ext_plugin))
    assert rc == 0 and out and not errs
    assert json.loads(out) == {"rootdir": repo, "registered": True}

    # Plugin shows up in help
    rc, out, errs = run_with_input([pyexe, "-mpytest", "--help"],
                                   "", repo, "pt_help",
                                   env_vars=dict(PYTHONPATH=ext_plugin))
    assert rc == 0 and out and not errs
    LineMatcher(out.splitlines()).fnmatch_lines("*--break=*")

    # Plugin not mentioned in in report
    rc, out, errs = run_with_input([pyexe, "-mpytest", "--break=99"],
                                   "", repo, "pt_opt",
                                   env_vars=dict(PYTHONPATH=ext_plugin))
    assert rc == 0 and out and not errs
    assert "plugins: pdb-break" not in out

    # Plugin runs
    rc, out, errs = run_with_input([pyexe, "-mpytest", "--break=99"],
                                   "", repo, "pt_fake",
                                   env_vars=dict(PYTHONPATH=ext_fake))
    assert rc == 0 and out and not errs
    LineMatcher(out.splitlines()).fnmatch_lines("*fake-pdb-break*99*")


def test_registered_not_found(testdir, source):
    # Not importable, requested
    repo = new_repo(testdir)
    pyexe = get_pyexe("base")

    rc, out, errs = run_with_input([pyexe, "-", "-p", "pytest_pdb_break"],
                                   source, repo)
    assert rc != 0 and not out and errs
    lines = LineMatcher(errs.splitlines())
    lines.fnmatch_lines("Traceback*")
    lines.fnmatch_lines("ModuleNotFoundError*pytest_pdb_break*")
