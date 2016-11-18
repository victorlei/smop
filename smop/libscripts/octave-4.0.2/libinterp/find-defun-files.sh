#! /bin/sh

set -e
SED=${SED:-sed}
EGREP=${EGREP:-egrep}

# Some stupid egreps don't like empty elements in alternation patterns,
# so we have to repeat ourselves because some stupid egreps don't like
# empty elements in alternation patterns.

DEFUN_PATTERN="^[ \t]*DEF(CONSTFUN|CMD|UN|UN_DLD|UNX_DLD|UN_TEXT)[ \t]*\\("

srcdir="$1"
shift

for arg
do
  if [ -f "$arg" ]; then
    file="$arg"
  else
    file="$srcdir/$arg"
  fi
  if [ "`$EGREP -l "$DEFUN_PATTERN" $file`" ]; then
    echo "$file" | $SED "s,\\$srcdir/,," | $SED 's/\.cc$/.df/; s/\.ll$/.df/; s/\.in.yy$/.df/';
  fi
done
