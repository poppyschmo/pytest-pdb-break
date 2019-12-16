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

@wrapper                       # <- line 19
def wrapped():
    print("wrapped")

if __name__ == "__main__":     # <- line 23
    pass
