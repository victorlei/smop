#! /bin/sh

set -e
GREP=${GREP:-grep}
SED=${SED:-sed}

srcdir="$1"
shift

for arg
do
  if [ -f "$arg" ]; then
    file="$arg"
  else
    file="$srcdir/$arg"
  fi
  if [ "`$GREP -l '^%!' $file`" ]; then
    echo "$file" | $SED "s,\\$srcdir/,,";
  fi
done
