import os
import sys
import pytest

from unittest.mock import Mock
from pexpect import EOF  # No importskip
from _pytest.pytester import LineMatcher
from conftest import prompt_re, unansi

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


def test__meta__upstream_facts():
    from pytest_asyncio.plugin import _markers_2_fixtures
    assert _markers_2_fixtures == {"asyncio": "event_loop"}


def test_simple_unknown(testdir):
    filename = testdir.copy_example("asyncio/test_simple.py")
    assert filename.exists()

    pe = testdir.spawn_pytest("--break=test_simple.py:4")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*unable to determine breakpoint*",
    ])


def test_simple(testdir):
    filename = testdir.copy_example("asyncio/test_simple.py")
    assert filename.exists()

    pe = testdir.spawn_pytest("-vvv --break=test_simple.py:19")
    pe.expect(prompt_re)
    befs = LineMatcher(unansi(pe.before))
    befs.fnmatch_lines([
        "*>*(19)test_bar()*",
        '*assert "asyncio" in request.keywords*'
    ])
    pe.sendline("c")
    pe.expect(EOF)
    befs = LineMatcher(unansi(pe.before))
    # Baz not registered as asyncio test
    befs.fnmatch_lines([
        "*warnings summary*",
        "*test_simple.py::TestClass::test_baz*",
    ])
