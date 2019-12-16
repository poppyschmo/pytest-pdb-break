import asyncio
import pytest

pytestmark = pytest.mark.asyncio


class TestClass:                                        # <- line 7
    async def test_foo(self, event_loop):

        async def inner():                              # <- line 10
            assert 1

        await asyncio.ensure_future(inner(), loop=event_loop)

    async def test_bar(self, request):                  # <- line 15
        assert "asyncio" in request.keywords
        return 1


async def test_baz(request):
    assert "asyncio" in request.keywords

    async def inner(x):                                 # <- line 23
        assert x

    await inner(True)


async def test_spam(somefix):                           # <- line 29
    assert somefix is str


@pytest.fixture
async def somefix():                                    # <- line 34
    spam = str
    await asyncio.sleep(0.1)
    yield spam
    del spam
