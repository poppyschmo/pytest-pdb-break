"""
Print a JSON object containing info about the current pytest configuration.

File args (node-ids) don't affect the outcome, but pytest options definitely
do. With either, the single-hyphen separator is basically required.

Warning
~~~~~~~
When this is run as a script (file arg), the hasplugin() call may return True
regardless of ``sys.path`` or CWD. Haven't yet found a way around this. Some
PYTEST_ environment variables look promising. For now, it's best to pipe the
contents to the desired Python exe or use the -c <command> option.
"""
import json
import sys
from _pytest.config import get_plugin_manager

if __name__ == "__main__":
    pm = get_plugin_manager()
    cf = pm.hook.pytest_cmdline_parse(pluginmanager=pm, args=sys.argv[1:])
    d = dict(
        registered=pm.hasplugin("pytest_pdb_break"),
        rootdir=str(cf.rootdir)
    )
    json.dump(d, sys.stdout)
