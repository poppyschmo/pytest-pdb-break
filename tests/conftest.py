"""
Helpers-related tests need to import a package named "helpers", which doesn't
exist. In other words, this project's namesake package isn't meant to include
any such subpackage.

Of lesser import: testdir subprocesses need a copy of this plugin, which they
``__import__`` directly via a conftest ``pytest_plugins`` global.

"""
import sys
from pathlib import Path

is_installed = False
try:
    import pytest_pdb_break
except ImportError:
    pass
else:
    is_installed = True


_rootdir = Path(__file__).parent.parent
assert (_rootdir / "tox.ini").exists()
sys.path.insert(0, str(_rootdir))


def pytest_sessionstart(session):
    """Ensure installed version has precedence over src version."""
    if is_installed:
        plugin = session.config.pluginmanager.get_plugin("pytest_pdb_break")
        assert pytest_pdb_break is plugin
        # Perhaps better to glob for something like pytest_pdb_break*.*-info
        assert Path(plugin.__file__).parent.name in ("site-packages", "self")
