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
    import attr
    items = _get_items(args)
    ItemLocation = _get_item_location_cls()
    return [attr.asdict(ItemLocation.from_pytest_item(i)) for i in items]


def _get_item_location_cls():
    import attr
    from pathlib import Path

    @attr.s
    class ItemLocation:
        """Object to store relevant test item info"""
        file = attr.ib(validator=attr.validators.instance_of(str))
        lnum = attr.ib(validator=attr.validators.instance_of(int))
        name = attr.ib(validator=attr.validators.instance_of(str))

        nodeid = attr.ib(validator=attr.validators.instance_of(str))
        func_name = attr.ib(validator=attr.validators.instance_of(str))

        class_name = attr.ib(default=None)
        param_id = attr.ib(default=None)

        @classmethod
        def from_pytest_item(cls, item):
            """Return a BreakLoc instance from a pytest Function item.

            Notes:
            1. ``pytest.Item.location`` line numbers are zero-indexed, but pdb
               breakpoints aren't, and neither are linecache's
            2. ``lnum`` may be ``-1``, as returned by ``.reportinfo()``
            3. ``name`` may be "Class.func[id]", with "id" accessible at
               ``.callspec.id`` and the rest at ``.originalname``
            4. ``name`` is presently unused
            """
            file, lnum, name = item.location
            # Comment in ``Config.cwd_relative_nodeid`` says "nodeid's are
            # relative to the rootpath." Seems this also applies to .location
            # names.
            assert not Path(file).is_absolute(), file
            file = item.config.rootdir.join(file)
            assert item.fspath.strpath == file.strpath
            return cls(
                file=str(file),
                lnum=lnum + 1,
                name=str(name),
                nodeid=item.nodeid,
                func_name=item.function.__name__,
                class_name=item.cls.__name__ if item.cls else None,
                param_id=item.callspec.id if item.originalname else None,
            )

    return ItemLocation
