#! /bin/sh

if [ $# -ne 1 ]; then
  echo "usage: find-docstring-files TOP-SRCDIR" 1>&2
  exit 1
fi

## if there is a file in the build directory tree, assume it is
## the file we are looking for.  Otherwise, get the one from the
## source tree.

if [ -f "../../scripts/DOCSTRINGS" ]; then
  echo "../../scripts/DOCSTRINGS"
else
  echo "$1/scripts/DOCSTRINGS"
fi

if [ -f "../../libinterp/DOCSTRINGS" ]; then
  echo "../../libinterp/DOCSTRINGS"
else
  echo "$1/libinterp/DOCSTRINGS"
fi
