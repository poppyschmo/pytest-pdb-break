
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


def run_with_input(cmdline, instr, tmpdir, timeout=1.0, env_vars=None):
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
    env = get_base_env()
    if env_vars:
        env.update(env_vars)
    tmpdir.join("env").write("\n".join(f"{k}={v}" for k, v in env.items()))
    tmpdir.join("info").write(dedent(f"""\
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
        out, errs = proc.communicate(input=instr.encode("utf-8"),
                                     timeout=timeout)
    except subprocess.TimeoutExpired:
        proc.kill()
        out, errs = proc.communicate()
    tmpdir.join("stdin").write(instr)
    tmpdir.join("stdout").write(out)
    tmpdir.join("stderr").write(errs)
    return proc.returncode, out.decode(), errs.decode()


def test_baseline(testdir, request):
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
    assert rc == 0
    assert out
    assert not errs
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
    libdir = make_libpath(testdir.tmpdir)
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


@pytest.mark.parametrize("relpath, stem",
                         [("", "test_top"),
                          ("one", "test_one"),
                          ("one/two", "test_two")])
def test_unregistered(testdir, source, relpath, stem):
    """
    Takeaways:
    1. the presence/veracity of (pytest) file/node-id args doesn't impact
       the result
    2. options do affect the result

    Notes:
        Kwargs don't have to be identifiers::
            >>> (lambda **kw: kw)(**{"a b c": 1})
            {'a b c': 1}
    """
    pyexe = get_pyexe("base")
    testdir.makeini("[pytest]\n")

    if relpath:
        subdir = testdir.tmpdir.join(relpath)
        # Can also use .dirpath.ensure
        Path(subdir).mkdir(parents=True)
        subdir.chdir()
        relfile = os.path.join(relpath, stem)
    else:
        subdir = testdir.tmpdir
        relfile = stem

    testdir.makepyfile(**{relfile: """
        def test_foo():
            assert True
    """})

    # No node-id
    rc, out, errs = run_with_input([pyexe], source, subdir)
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": testdir.tmpdir, "registered": False}

    # Real node-id
    full = subdir.join(stem + ".py")
    nid = "::".join([str(full), "test_foo"])
    rc, out, errs = run_with_input([pyexe, "-", nid], source, subdir)
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": testdir.tmpdir, "registered": False}

    # Fake node-id
    rc, out, errs = run_with_input([pyexe, "-",
                                    "/tmp/fake__/fake.py::test_fake"],
                                   source, subdir)
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": testdir.tmpdir, "registered": False}

    # Explicit rootdir as option
    rc, out, errs = run_with_input([pyexe, "-",
                                    f"--rootdir={str(subdir)}"],
                                   source, subdir)
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": subdir, "registered": False}

    # Bad option
    rc, out, errs = run_with_input([pyexe, "-", "--fake"],
                                   source, subdir)
    assert rc != 0 and not out and errs
    with pytest.raises(json.JSONDecodeError):
        json.loads(out)
    lines = LineMatcher(errs.splitlines())
    lines.fnmatch_lines("*unrecognized argument*")


def test_registered(testdir, source):
    """
    Takeaways:
    1. If egg appears in sys.path, pytest finds it without -p <plugin>
    1. Otherwise plugin must be declared explicitly somewhere (just being
       importable isn't enough)
    2. An exception is raised if a declared plugin not importable
    """
    def new_repo(name, ini=""):
        r = testdir.tmpdir.mkdir(name)
        r.chdir()
        testdir.makepyfile(**{f"{name}/test_something": """
            def test_foo():
                assert True
        """})
        testdir.makefile(".ini", **{f"{name}/tox": f"[pytest]\n{ini}"})
        return r

    libdir = make_libpath(testdir.tmpdir)
    pyexe = get_pyexe("base")

    repo = new_repo("base")
    rc, out, errs = run_with_input([pyexe],
                                   source, repo,
                                   env_vars=dict(PYTHONPATH=libdir))
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": repo, "registered": False}

    repo = new_repo("local_egg")
    libegg = make_libpath(testdir.tmpdir, "local_egg_lib", pyexe=pyexe)
    rc, out, errs = run_with_input([pyexe], source, repo,
                                   env_vars=dict(PYTHONPATH=libegg))
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": repo, "registered": True}

    repo = new_repo("cmdline")
    rc, out, errs = run_with_input([pyexe, "-", "-p", "pytest_pdb_break"],
                                   source, repo,
                                   env_vars=dict(PYTHONPATH=libdir))
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": repo, "registered": True}

    repo = new_repo("addopts", "addopts = -p pytest_pdb_break\n")
    rc, out, errs = run_with_input([pyexe],
                                   source, repo,
                                   env_vars=dict(PYTHONPATH=libdir))
    assert rc == 0 and out and not errs
    result = json.loads(out)
    assert result == {"rootdir": repo, "registered": True}

    repo = new_repo("nomod")
    rc, out, errs = run_with_input([pyexe, "-", "-p", "pytest_pdb_break"],
                                   source, repo)
    assert rc != 0 and not out and errs
    lines = LineMatcher(errs.splitlines())
    lines.fnmatch_lines("Traceback*")
    lines.fnmatch_lines("ModuleNotFoundError*pytest_pdb_break*")
