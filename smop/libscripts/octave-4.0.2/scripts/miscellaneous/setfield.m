## Copyright (C) 2000-2015 Etienne Grossmann
## Copyright (C) 2009 VZLU Prague
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{sout} =} setfield (@var{s}, @var{field}, @var{val})
## @deftypefnx {Function File} {@var{sout} =} setfield (@var{s}, @var{sidx1}, @var{field1}, @var{fidx1}, @var{sidx2}, @var{field2}, @var{fidx2}, @dots{}, @var{val})
##
## Return a @emph{copy} of the structure @var{s} with the field member
## @var{field} set to the value @var{val}.
##
## For example:
##
## @example
## @group
## @var{s} = struct ();
## @var{s} = setfield (@var{s}, "foo bar", 42);
## @end group
## @end example
##
## @noindent
## This is equivalent to
##
## @example
## @var{s}.("foo bar") = 42;
## @end example
##
## @noindent
## Note that ordinary structure syntax @code{@var{s}.foo bar = 42} cannot be
## used here, as the field name is not a valid Octave identifier because of
## the space character.  Using arbitrary strings for field names is
## incompatible with @sc{matlab}, and this usage will emit a warning if the
## warning ID @code{Octave:language-extension} is enabled.
## @xref{XREFwarning_ids}.
##
## With the second calling form, set a field of a structure array.  The
## input @var{sidx} selects an element of the structure array, @var{field}
## specifies the field name of the selected element, and @var{fidx} selects
## which element of the field (in the case of an array or cell array).
## The @var{sidx}, @var{field}, and @var{fidx} inputs can be repeated to
## address nested structure array elements.  The structure array index and
## field element index must be cell arrays while the field name must be a
## string.
##
## For example:
##
## @example
## @group
## @var{s} = struct ("baz", 42);
## setfield (@var{s}, @{1@}, "foo", @{1@}, "bar", 54)
## @result{}
##   ans =
##     scalar structure containing the fields:
##       baz =  42
##       foo =
##         scalar structure containing the fields:
##           bar =  54
## @end group
## @end example
##
## The example begins with an ordinary scalar structure to which a nested
## scalar structure is added.  In all cases, if the structure index @var{sidx}
## is not specified it defaults to 1 (scalar structure).  Thus, the example
## above could be written more concisely as
## @code{setfield (@var{s}, "foo", "bar", 54)}
##
## Finally, an example with nested structure arrays:
##
## @example
## @group
## @var{sa}.foo = 1;
## @var{sa} = setfield (@var{sa}, @{2@}, "bar", @{3@}, "baz", @{1, 4@}, 5);
## @var{sa}(2).bar(3)
## @result{}
##   ans =
##     scalar structure containing the fields:
##       baz =  0   0   0   5
## @end group
## @end example
##
## Here @var{sa} is a structure array whose field at elements 1 and 2 is in
## turn another structure array whose third element is a simple scalar
## structure.  The terminal scalar structure has a field which contains a
## matrix value.
##
## Note that the same result as in the above example could be achieved by:
##
## @example
## @group
## @var{sa}.foo = 1;
## @var{sa}(2).bar(3).baz(1,4) = 5
## @end group
## @end example
## @seealso{getfield, rmfield, orderfields, isfield, fieldnames, isstruct, struct}
## @end deftypefn

## Author:  Etienne Grossmann <etienne@cs.uky.edu>

function sout = setfield (s, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  subs = varargin(1:end-1);
  val = varargin{end};
  flds = cellfun ("isclass", subs, "char");
  idxs = cellfun ("isclass", subs, "cell");
  if (! all (flds | idxs))
    error ("setfield: invalid index");
  endif

  typs = ifelse (flds, {"."}, {"()"});
  sout = subsasgn (s, struct ("type", typs, "subs", subs), val);

endfunction


%!test
%! x.a = "hello";
%! x = setfield (x, "b", "world");
%! y = struct ("a", "hello", "b", "world");
%! assert (x, y);
%!test
%! oo(1,1).f0 = 1;
%! oo = setfield (oo,{1,2},"fd",{3},"b", {1,4}, 6);
%! assert (oo(1,2).fd(3).b(1,4), 6);

## Test input validation
%!error setfield ()
%!error setfield (1)
%!error setfield (1,2)
%!error <invalid index> setfield (1,2,3)

