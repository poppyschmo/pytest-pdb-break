

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
