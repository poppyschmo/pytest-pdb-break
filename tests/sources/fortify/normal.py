def somefunc():                # <- line 1
    print("somefunc")

class C:                       # <- line 4
    def f(self):
        return True

class TestClass:               # <- line 8
    def test_foo(self):
        somevar = False

        def inner(x):          # <- line 12
            return not x

        assert inner(somevar)

wrapper = lambda x: x

@wrapper
def wrapped():                 # <- line 20
    print("wrapped")

def test_bar():                # <- line 23
    assert True

import pytest

@pytest.fixture
def baz():                     # <- line 29
    spam = 1
    return spam

def test_foo(baz):             # <- line 33
    assert 1
