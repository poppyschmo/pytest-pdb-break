import pytest


def test_debugger_quit_msg(tmpdir):
    """Ensure wording of msg
    ``runcall_until`` may fail or mislead otherwise
    """
    from _pytest.debugging import pytestPDB
    from unittest.mock import patch, Mock
    with patch("_pytest.debugging.pytestPDB._pluginmanager"), \
            patch("_pytest.config.create_terminal_writer"), \
            patch("_pytest.outcomes.exit") as m_exit:
        cls = pytestPDB
        inst = cls._init_pdb("foo")
        inst.botframe = Mock()
        inst.do_quit("")
        m_exit.assert_called_with("Quitting debugger")


def test_py_local_stat(tmpdir, monkeypatch):
    """LocalPath and pathlib.Path objects aren't fully compatible"""
    # Can't use Path.cwd() because its .samefile() calls .stat() on
    # other and expects returned obj to have .st_ino, but path.Stat
    # looks for st_st_ino
    # TODO find out if users of the plugin API are meant to fiddle with
    # LocalPath objects, or if these are only for internal use. A
    # cursory search in the CHANGELOG/bug tracker for terms like
    # pathlib, localpath, fspath, etc., didn't yield much, (occasional
    # references to "recent pathlib refactoring" circa mid 2018).
    import py
    import pathlib

    tmpdir.chdir()

    p = pathlib.Path.cwd()
    q = py.path.local()

    with pytest.raises(AttributeError) as exc_info:
        p.samefile(q)

    assert exc_info.match("st_st_ino")

    with monkeypatch.context() as m:
        m.setattr(py._path.local.Stat, "__getattr__",
                  lambda inst, name:
                  getattr(inst._osstatresult,
                          name if name.startswith("st_") else "st_" + name))
        assert p.samefile(q) is True


def test_pytest_configure_hook_cwd(testdir):
    """Current directory persists between ``pytest_configure`` visits.

    This is when ``._resolve_wanted`` runs, meaning we must change back to
    invocation_dir before resolving absolute paths.

    """
    testdir.makeconftest("""
        pytest_plugins = ['pytest_one', 'pytest_two']
    """)

    common = """
        def pytest_configure(config):
            cwd = type(config.invocation_dir)()
            print('in', %%r)
            print('invocation_dir:', config.invocation_dir)
            print('cwd:', cwd)
            if config.invocation_dir == cwd:
                import os
                os.chdir(%r)
    """ % str(testdir.test_tmproot)

    testdir.makepyfile(pytest_one=common % "one")
    testdir.makepyfile(pytest_two=common % "two")

    testdir.makepyfile("""
        def test_foo(request):
            cwd = type(request.config.invocation_dir)()
            assert request.config.invocation_dir.samefile(%r)
            assert cwd.samefile(%r)
    """ % (str(testdir.tmpdir), str(testdir.test_tmproot)))

    result = testdir.runpytest("--capture=no")

    testdir.maketxtfile(log="\n".join(result.outlines))
    result.assert_outcomes(passed=1)


def test_show_capture(testdir):
    from pexpect import EOF

    testdir.makepyfile("""
        def test_foo(request):
            capopt = (
                request.config.getoption('showcapture')
                or request.config.getini('showcapture')
            )
            print('something')
            print('capopt', capopt)
            from logging import getLogger
            logger = getLogger('test_show_capture.test_foo')
            logger.error('some error')
            assert False
    """)
    child = testdir.spawn_pytest("test_show_capture.py")
    child.expect(r"[Cc]aptured.*stdout")
    child.expect(r"something")
    child.expect(r"capopt all")
    child.expect(r"[Cc]aptured.*log")
    child.expect(r"some error")
    child.expect(EOF)
