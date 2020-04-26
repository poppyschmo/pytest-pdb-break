================
pytest-pdb-break
================

This is an aid to help **traditional text editors** (Vim and Emacs) fire up the
debugger and fast-forward to the point of interest. [#f1]_ If you already have
a solid workflow with ``breakpoint()`` snippets and/or the ``--trace`` and
``--pdb`` options, there's nothing to see here.

    This basically does ...

    .. code:: console

       $ pytest --trace test_spam.py::test_foo
       ...
       (pdb) until 42

    or ...

    .. code:: python

       def test_foo():
           ...
           pytest.set_trace(header="line 42")

    with a few minor conveniences sprinkled in


Before you try
    Check out the newer ptvsd_/`DAP`_/`pydevd`_-based extensions for Vim and
    Emacs. They definitely have the potential to work seamlessly with pytest,
    as VS-Code seems to. This plugin merely launches PDB, the built-in
    GDB-style debugger.

.. _ptvsd: https://github.com/microsoft/ptvsd
.. _pydevd: https://github.com/fabioz/PyDev.Debugger
.. _DAP: https://microsoft.github.io/debug-adapter-protocol/implementors/adapters


Simpatico check
    #. You prefer a noninvasive, one-click, carry-me approach to entering the
       debugger

    #. You like the certainty of stopping afforded by the second example above

    #. You like tab completion of variables and attributes

    #. You want inside fixtures, |unittest|_'s, and pytest-asyncio_ items

.. |unittest| replace:: ``unittest.TestCase``
.. _unittest: https://docs.python.org/3.8/library/unittest.html#unittest.TestCase
.. _pytest-asyncio: https://pypi.org/project/pytest-asyncio


Don't install
    Unlike proper pytest plugins, this isn't meant to be installed as a Python
    package. [#f2]_ The editor will instead inject an isolated installation via
    ``PYTHONPATH``, but only while in use (no internet connection required).


WIPs
    - `Emacs <https://github.com/poppyschmo/pytest-pdb-break/blob/master/emacs/>`_
    - `Vim <https://github.com/poppyschmo/pytest-pdb-break/blob/master/vim/>`_


TODOs
    #. Ditch helpers in favor of client/server model

    #. Support arbitrary test names via the |pyfunc|_ and ``python_classes``
       options

    #. Ensure proper breaking in overridden fixtures

    #. Add helpers for running tests in containers

    #. Support FreeBSD


Notes/caveats
    #. Unfortunately, this thing has only ever been tried/tested on Linux

    #. It mainly exists for its author to learn about pytest, a decent
       understanding of which continues to evade


.. [#f1] It can also be used as a command-line option, e.g.
   ``pytest --break=test_file.py:42``

.. [#f2] ... unless used as a standalone command-line option

.. |pyfunc| replace:: ``"python_functions"``
.. _pyfunc: https://docs.pytest.org/en/latest/reference.html#confval-python_functions
