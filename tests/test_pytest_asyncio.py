import os
import sys
import pytest


plugin_installed = (
    os.getenv("TOXENV") in ("py38-asyncio", "py37-asyncio")
    or
    sys.modules.get("pytest_asyncio")
)
pytestmark = pytest.mark.skipif(
    not plugin_installed, reason="Integration dependency missing"
)


def test__meta__plugin_installed(request):
    assert request.config.pluginmanager.hasplugin("asyncio")


def test_fortify_location_plugin_present(testdir):
    from pytest_pdb_break import fortify_location, BreakLoc

    filename = testdir.copy_example("fortify/async.py")
    assert filename.exists()

    rv = fortify_location(filename, 2, True)
    assert rv.equals(BreakLoc(filename, 2, None, func_name="somefunc"))

    rv = fortify_location(filename, 9, True)
    assert rv.equals(
        BreakLoc(
            filename, 9, None, class_name="TestClass", func_name="test_foo"
        )
    )
    assert rv.inner == "inner"

    rv = fortify_location(filename, 15, True)
    assert rv.equals(
        BreakLoc(
            filename, 15, None, class_name="TestClass", func_name="test_bar"
        )
    )
    assert rv.inner is None

    rv = fortify_location(filename, 21, True)
    assert rv.equals(
        BreakLoc(
            filename, 21, None, class_name="TestClass", func_name="test_baz"
        )
    )
    assert rv.inner == "inner"
