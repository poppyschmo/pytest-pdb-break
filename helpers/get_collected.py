# This file is part of https://github.com/poppyschmo/pytest-pdb-break
from pathlib import Path

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
        if item.originalname and hasattr(item, "callspec"):
            name = name.replace("[{}]".format(item.callspec.id), "")
        modules.add((str(filename), lnum, name))
    return sorted(modules)


def get_collected(*args):
    """Return a list of test-item location info as dicts"""
    items = _get_items(args)
    return [_make_item_location(i) for i in items]


def _make_item_location(item):
    """Return dict with relevant test-item location info

    Notes
    ~~~~~
    1. ``pytest.Item.location`` line numbers are zero-indexed, but pdb
       breakpoints aren't, and neither are linecache's
    2. ``lnum`` may be ``-1``, as returned by ``.reportinfo()``
    3. ``name`` may be "Class.func[id]", with "id" accessible at
       ``.callspec.id`` and the rest at ``.originalname``
    4. ``name`` is presently unused

    """
    file, lnum, name = item.location
    assert isinstance(lnum, int)
    assert isinstance(item.nodeid, str)
    # Comment in ``Config.cwd_relative_nodeid`` says "nodeid's are
    # relative to the rootpath." Seems this also applies to .location
    # names.
    assert not Path(file).is_absolute(), file
    file = item.config.rootdir.join(file)
    assert item.fspath.strpath == file.strpath
    return dict(
        file=str(file),
        lnum=lnum + 1,
        name=str(name),
        nodeid=item.nodeid,
        func_name=item.function.__name__,
        class_name=item.cls.__name__ if item.cls else None,
        param_id=(
            item.callspec.id
            if item.originalname and hasattr(item, "callspec") else None
        ),
    )
