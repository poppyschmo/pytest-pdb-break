async def somefunc():          # <- line 1
    print("somefunc")

class TestClass:               # <- line 4
    def test_foo(self):
        somevar = False

        async def inner(x):    # <- line 8
            return not x

        import asyncio
        assert asyncio.run(somevar)

    async def test_bar(self):  # <- line 14
        nothervar = True
        return nothervar

    async def test_baz(self):

        async def inner(x):    # <- line 20
            return not x

        return await inner(True)
