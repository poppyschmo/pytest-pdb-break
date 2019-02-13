================
pytest-pdb-break
================

This is not a real pytest plugin. It's an aid for **text editors** that fires
up the debugger and fast-forwards to the point of interest. If you already
enjoy a streamlined workflow with ``breakpoint()`` snippets and/or pytest's
``--trace`` and ``--pdb`` options, there's nothing to see here.

    This basically does the equivalent of

    .. code:: console

        $ pytest --trace spam.py::test_foo
        ...
        (pdb) until 42

    or ...

    .. code:: python

        def test_foo():
            ...
            pytest.set_trace(header="line 42")

    with a few minor conveniences sprinkled in


Simpatico check
    1. You prefer a noninvasive, one-click, carry-me approach to entering the
       debugger
    2. You like tab completion of variables and attributes
    3. You want inside ``unittest.TestCase`` s or your own fixtures
       (experimental)

Don't install
    Unlike actual plugins, this isn't meant to be installed as a Python
    package. The editor will instead inject an isolated installation via
    ``PYTHONPATH``, but only while in use (no internet connection required).

WIPs
    - `Emacs <emacs/>`_
    - `Vim <vim/>`_

Caveats
    1. This thing has only ever been tried/tested on Linux
    2. It mainly exists for its author to learn about pytest, a decent
       understanding of which continues to evade
    3. It does not support the ``-m pytest`` style of invocation, meaning
       working directories are not implicitly prepended to ``sys.path``
    4. When hacking on the main pytest plugin, disregard the imperative above
       and *do install*:

       .. code:: console

           (.venv)repo@master$ pip install -e .

       If sticking with the included reference integrations, some extra
       finagling may be required. For example:

       - Disabling "isolated-lib" creation via one of the documented options
       - Manually managing the editor plugin (or making it play nice with your
         manager)

       If doing your own thing, the usual rules apply, like ensuring the right
       exec path and env vars have precedence when summoning pytest:

       .. code:: javascript

           const pytest = pty.spawn(
               'pytest', ['--break=spam.py:42', nodeid],
               { cwd: rootdir, env: Object.assign({}, process.env, modified) }
           );  // or whatever

TODOs
    1. ``asyncio``
    2. Ipdb
    3. External helper returning an ``--options`` list for invocation
       completion
