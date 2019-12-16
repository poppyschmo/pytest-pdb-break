import pytest
from helpers import get_collected
from pprint import pformat

f1 = """
    import pytest

    def test_a():
        assert True

    @pytest.mark.parametrize('arg', [True, False])
    def test_b(arg):
        assert arg
"""

f2 = """
    import pytest

    class TestC:
        def test_c(self):
            assert True

        @pytest.mark.parametrize('arg', [True, False])
        def test_d(self, arg):
            assert arg

        @pytest.fixture
        def myfix(self):
            return True

        def test_e(self, myfix):
            return myfix
"""

f3 = """
    import unittest
    class TestU(unittest.TestCase):
        def test_x(self):
            self.assertTrue(True)
"""


@pytest.fixture
def repo(testdir):
    testdir.makepyfile(test_fun=f1)
    testdir.makepyfile(test_cls=f2)
    testdir.makepyfile(test_ut=f3)
    return testdir


def test_get_items_args(repo):
    subdir = repo.tmpdir.join("subdir")
    subdir.mkdir()
    rv = get_collected._get_items(("-k", "False"))
    repo.tmpdir.join("got.out").write(pformat(rv))
    assert repr(rv) == "[<Function test_d[False]>, <Function test_b[False]>]"


def test_get_node_ids(repo):
    rv = get_collected.get_node_ids()
    repo.tmpdir.join("got.out").write(pformat(rv))
    assert rv == ["test_cls.py::TestC::test_c",
                  "test_cls.py::TestC::test_d[True]",
                  "test_cls.py::TestC::test_d[False]",
                  "test_cls.py::TestC::test_e",
                  "test_fun.py::test_a",
                  "test_fun.py::test_b[True]",
                  "test_fun.py::test_b[False]",
                  "test_ut.py::TestU::test_x"]


def test_get_locations(repo):
    rv = get_collected.get_locations()
    repo.tmpdir.join("got.out").write(pformat(rv))
    assert rv == [("test_cls.py", 4, "TestC.test_c"),
                  ("test_cls.py", 7, "TestC.test_d"),
                  ("test_cls.py", 15, "TestC.test_e"),
                  ("test_fun.py", 3, "test_a"),
                  ("test_fun.py", 6, "test_b"),
                  ("test_ut.py", 3, "TestU.test_x")]


def test_item_location(request):  # no i/o
    # From pytest item
    from unittest.mock import Mock, MagicMock
    from helpers.get_collected import _get_item_location_cls
    ItemLocation = _get_item_location_cls()

    item = Mock(request.node)
    rootdir = request.config.rootdir
    item.config.rootdir = rootdir
    item.fspath = rootdir.join("test_loc.py")
    #
    assert request.node.originalname is None
    item.originalname = None  # else looks for callspec, which is absent
    assert item.nodeid
    item.nodeid = "test_loc.py::test_loc_1"
    item.location = ("test_loc.py", 1, "test_loc_1")
    item.function.__name__ = "test_loc_1"
    item.function.__code__ = MagicMock()
    item.function.__code__.co_firstlineno = 42
    item.cls = None
    ItemLocation.from_pytest_item(item) == ItemLocation(
        str(rootdir / "test_loc.py"),
        2,
        "test_loc_1",
        "test_loc.py::test_loc_1",
        "test_loc_1",
    )
    #
    item.originalname = "test_loc_1"
    item.nodeid = "test_loc.py::TestClass::test_loc_1[True]"
    item.cls = Mock()
    item.cls.__name__ = "TestClass"
    item.callspec = Mock()
    item.callspec.id = "True"
    expected = ItemLocation(
        str(rootdir / "test_loc.py"),
        2,
        "test_loc_1",
        "test_loc.py::TestClass::test_loc_1[True]",
        "test_loc_1",
        "TestClass",
        "True",
    )
    assert ItemLocation.from_pytest_item(item) == expected
    #
    item.location = ("/tmp/test_loc.py", 1, "test_loc_1")
    with pytest.raises(AssertionError):
        ItemLocation.from_pytest_item(item)


def test_get_collected(repo):
    import sys
    if sys.version_info < (3, 6):
        pytest.skip("Needs pytest plugin, which is 3.6+")
    rv = get_collected.get_collected()
    repo.tmpdir.join("got.out").write(pformat(rv))
    pfun = repo.tmpdir.join("test_fun.py").strpath
    pcls = repo.tmpdir.join("test_cls.py").strpath
    put = repo.tmpdir.join("test_ut.py").strpath
    assert rv == [
        {'file': pcls, 'lnum': 4, 'name': 'TestC.test_c',
         'class_name': 'TestC', 'func_name': 'test_c',
         'param_id': None, 'nodeid': 'test_cls.py::TestC::test_c'},
        {'file': pcls, 'lnum': 7, 'name': 'TestC.test_d[True]',
         'class_name': 'TestC', 'func_name': 'test_d',
         'param_id': 'True', 'nodeid': 'test_cls.py::TestC::test_d[True]'},
        {'file': pcls, 'lnum': 7, 'name': 'TestC.test_d[False]',
         'class_name': 'TestC', 'func_name': 'test_d',
         'param_id': 'False', 'nodeid': 'test_cls.py::TestC::test_d[False]'},
        {'file': pcls, 'lnum': 15, 'name': 'TestC.test_e',
         'class_name': 'TestC', 'func_name': 'test_e',
         'param_id': None, 'nodeid': 'test_cls.py::TestC::test_e'},
        {'file': pfun, 'lnum': 3, 'name': 'test_a',
         'class_name': None, 'func_name': 'test_a',
         'param_id': None, 'nodeid': 'test_fun.py::test_a'},
        {'file': pfun, 'lnum': 6, 'name': 'test_b[True]',
         'class_name': None, 'func_name': 'test_b',
         'param_id': 'True', 'nodeid': 'test_fun.py::test_b[True]'},
        {'file': pfun, 'lnum': 6, 'name': 'test_b[False]',
         'class_name': None, 'func_name': 'test_b',
         'param_id': 'False', 'nodeid': 'test_fun.py::test_b[False]'},
        {'file': put, 'lnum': 3, 'name': 'TestU.test_x',
         'class_name': 'TestU', 'func_name': 'test_x',
         'param_id': None, 'nodeid': 'test_ut.py::TestU::test_x'}
    ]
