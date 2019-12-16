import pytest


async def somefunc():                                   # <- line 4
    print("somefunc")


class TestClass:                                        # <- line 8
    def test_foo(self, event_loop):
        somevar = False

        async def inner():                              # <- line 12
            assert not somevar

        event_loop.run_until_complete(inner())

    @pytest.mark.asyncio
    async def test_bar(self, request):                  # <- line 18
        assert "asyncio" in request.keywords
        return 1

    async def test_baz(self):

        async def inner(x):                             # <- line 24
            return not x

        return await inner(True)
