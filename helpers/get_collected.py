# This file is part of https://github.com/poppyschmo/pytest-pdb-break

# Tried calling ``pytest --collect-only`` in a subprocess using a custom
# ``pytest_collection_finish`` hookimpl, but that seemed to take forever
# (probably for good reason). Not sure how stable/correct this is but going
# with it for now.


def _get_items(argv):
    "Return a list of collected items."
    from _pytest.config import get_config
    from _pytest.main import Session
    cf = get_config(list(argv))
    try:
        pm = cf.pluginmanager
        # See pytest 3c7438969a:
        # Replace internal config._origargs with invocation_params.args
        assert not hasattr(cf, "args")
        cf = pm.hook.pytest_cmdline_parse(pluginmanager=pm, args=list(argv))
        session = getattr(Session, "from_config", Session)(cf)
        cf.hook.pytest_sessionstart(session=session)
        session.perform_collect()
        return session.items
    finally:
        cf._ensure_unconfigure()


def get_node_ids(*args):
    "Return a list of node ids"
    items = _get_items(args)
    return [i.nodeid for i in items]


def get_locations(*args):
    """Return a list of test-item locations"""
    items = _get_items(args)
    modules = set()
    for item in items:
        filename, lnum, name = item.location
        lnum += 1
        if item.originalname:
            name = name.replace("[{}]".format(item.callspec.id), "")
        modules.add((str(filename), lnum, name))
    return sorted(modules)


def get_collected(*args):
    """Return a list of test-item location info as dicts"""
    from pytest_pdb_break import BreakLoc
    items = _get_items(args)
    locs = ((BreakLoc.from_pytest_item(i), i.nodeid) for i in items)
    return [dict(file=str(l.file), lnum=l.lnum, name=l.name,
                 class_name=l.class_name, func_name=l.func_name,
                 param_id=l.param_id, nodeid=n) for l, n in locs]
