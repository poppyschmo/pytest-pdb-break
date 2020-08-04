from setuptools import setup

try:
    from pathlib import Path
    long_description = Path(__file__).parent.joinpath("README.rst").read_text()
except Exception:
    long_description = None

setup(
    name="pytest-pdb-break",
    version="0.0.10",
    license="Apache 2.0",
    description="A pytest command-line option for pdb breakpoints",
    long_description=long_description,
    url="https://github.com/poppyschmo/pytest-pdb-break",
    author="Jane Soko",
    author_email="poppyschmo@protonmail.com",
    python_requires=">=3.6",
    install_requires=["pytest"],
    packages=[],
    py_modules=["pytest_pdb_break"],
    entry_points={"pytest11": ["pytest_pdb_break = pytest_pdb_break"]},
    classifiers=["Framework :: Pytest",
                 "License :: OSI Approved :: Apache Software License",
                 "Programming Language :: Python :: 3.6",
                 "Programming Language :: Python :: 3.7",
                 "Programming Language :: Python :: 3.8",
                 "Topic :: Text Editors",
                 "Topic :: Software Development :: Debuggers"],
)
