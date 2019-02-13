# pytest-pdb-break

A single convenience command to run the test under the cursor in a split
and break at the current line


## Installation
```vim
  Plug 'poppyschmo/pytest-pdb-break', { 'rtp': 'vim' }
```

## Usage

With the cursor inside some test:
```
  :PytestBreakHere
```
Additional args are passed on to pytest:
```
  :PytestBreakHere -o addopts=
```

See doc for options, etc.
