#! /bin/sh

# Copyright (C) 2010-2015 VZLU Prague
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

CLASSES="
  double
  single
  char
  logical
  int8
  int16
  int32
  int64
  uint8
  uint16
  uint32
  uint64
  struct
  cell
  function_handle
"

if [ $# -eq 1 ]; then
  case "$1" in
    --list-files)
      echo tbcover.m
      echo bc-overloads.tst
      for class in $CLASSES; do
        echo @$class/tbcover.m
      done
      exit
    ;;
    --list-dirs)
      for class in $CLASSES; do
        echo @$class
      done
      exit
    ;;
    --list-classes)
      echo $CLASSES
      exit
    ;;
    *)
      expected_results_file="$1"
    ;;
  esac
else
  echo "usage: build_bc_overload_tests.sh expected-results-file" 1>&2
  exit 1
fi

for class in $CLASSES; do
  DIR="@$class"
  test -d $DIR || mkdir $DIR || { echo "error: could not create $DIR"; exit; }
  cat > $DIR/tbcover.m << EOF
% !!! DO NOT EDIT !!!
% generated automatically by build_bc_overload_tests.sh
function s = tbcover (x, y)
  s = '$class';
EOF
done

cat > tbcover.m << EOF
% !!! DO NOT EDIT !!!
% generated automatically by build_bc_overload_tests.sh
function s = tbcover (x, y)
  s = 'none';
EOF

if test "$1" = "overloads_only" ; then
  exit
fi

cat > bc-overloads.tst << EOF
## !!! DO NOT EDIT !!!
## THIS IS AN AUTOMATICALLY GENERATED FILE
## modify build_bc_overload_tests.sh to generate the tests you need.

%!shared ex
%! ex.double = 1;
%! ex.single = single (1);
%! ex.logical = true;
%! ex.char = 'char';
%! ex.int8  = int8 (1);
%! ex.int16 = int16 (1);
%! ex.int32 = int32 (1);
%! ex.int64 = int64 (1);
%! ex.uint8  = uint8 (1);
%! ex.uint16 = uint16 (1);
%! ex.uint32 = uint32 (1);
%! ex.uint64 = uint64 (1);
%! ex.cell = {};
%! ex.struct = struct ();
%! ex.function_handle = @numel;

EOF

cat $expected_results_file | \
while read cl1 cl2 clr ; do
  cat >> bc-overloads.tst << EOF
%% Name call
%!assert (tbcover (ex.$cl1, ex.$cl2), "$clr")
%% Handle call
%!assert ((@tbcover) (ex.$cl1, ex.$cl2), "$clr")

EOF
done

cat >> bc-overloads.tst << EOF
%%test handles through cellfun
%!test
%! f = fieldnames (ex);
%! n = numel (f);
%! s = c1 = c2 = cell (n);
%! for i = 1:n
%!   for j = 1:n
%!     c1{i,j} = ex.(f{i});
%!     c2{i,j} = ex.(f{j});
%!     s{i,j} = tbcover (ex.(f{i}), ex.(f{j}));
%!   endfor
%! endfor
%! assert (cellfun (@tbcover, c1, c2, "uniformoutput", false), s);
EOF
