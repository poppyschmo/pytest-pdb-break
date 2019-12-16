async def somefunc():          # <- line 1
    print("somefunc")

class TestClass:               # <- line 4
    def test_foo(self):
        somevar = False

        async def inner(x):    # <- line 8
            return not x

        import asyncio
        assert asyncio.run(somevar)
