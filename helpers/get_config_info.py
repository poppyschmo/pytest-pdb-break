"""
Print a JSON object containing info about the current pytest configuration.

File args (node-ids) don't affect the outcome, but pytest options definitely
do. A single-hyphen separator may be necessary.

Notes
~~~~~
This script needs to be reworked once a better understanding of pytest
internals is acquired. For now, it's best to pipe the contents to the desired
Python exe or use the -c <command> option. The working directory must that from
which pytest will be invoked.

This script should *eventually* assess whether it's safe to inject the plugin's
path into the session's ``sys.path`` via ``$PYTHONPATH``.  The current
assumption is that only module-name collisions present a problem.

``Config._preparse`` calls ``PluginManager.load_setuptools_entrypoints``, which
registers plugins with corresponding eggs containing a ``[pytest11]`` section.

A plugin may implement hooks in module scope (registered on init) and others in
some class body (registered under a different name). Seems these are treated
as separate plugins.

"""
import json
import sys
from _pytest.config import get_config

if __name__ == "__main__":
    cf = get_config()
    pm = cf.pluginmanager
    cf.parse(args=sys.argv[1:])
    # Assume pm._name2plugin and pm._plugin2hookcallers are in sync
    d = dict(registered=pm.hasplugin("pytest_pdb_break"),
             rootdir=str(cf.rootdir))
    json.dump(d, sys.stdout)
