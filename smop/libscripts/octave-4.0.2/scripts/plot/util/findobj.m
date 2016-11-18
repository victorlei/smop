## Copyright (C) 2007-2015 Ben Abbott
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
## @deftypefn  {Function File} {@var{h} =} findobj ()
## @deftypefnx {Function File} {@var{h} =} findobj (@var{prop_name}, @var{prop_value}, @dots{})
## @deftypefnx {Function File} {@var{h} =} findobj (@var{prop_name}, @var{prop_value}, "-@var{logical_op}", @var{prop_name}, @var{prop_value})
## @deftypefnx {Function File} {@var{h} =} findobj ("-property", @var{prop_name})
## @deftypefnx {Function File} {@var{h} =} findobj ("-regexp", @var{prop_name}, @var{pattern})
## @deftypefnx {Function File} {@var{h} =} findobj (@var{hlist}, @dots{})
## @deftypefnx {Function File} {@var{h} =} findobj (@var{hlist}, "flat", @dots{})
## @deftypefnx {Function File} {@var{h} =} findobj (@var{hlist}, "-depth", @var{d}, @dots{})
## Find graphics object with specified property values.
##
## The simplest form is
##
## @example
## findobj (@var{prop_name}, @var{prop_value})
## @end example
##
## @noindent
## which returns the handles of all objects which have a property named
## @var{prop_name} that has the value @var{prop_value}.  If multiple
## property/value pairs are specified then only objects meeting all of the
## conditions are returned.
##
## The search can be limited to a particular set of objects and their
## descendants, by passing a handle or set of handles @var{hlist} as the first
## argument.
##
## The depth of the object hierarchy to search can be limited with the
## @qcode{"-depth"} argument.  An example of searching only three generations
## of children is:
##
## @example
## findobj (@var{hlist}, "-depth", 3, @var{prop_name}, @var{prop_value})
## @end example
##
## Specifying a depth @var{d} of 0, limits the search to the set of objects
## passed in @var{hlist}.  A depth @var{d} of 0 is equivalent to the
## @qcode{"flat"} argument.
##
## A specified logical operator may be applied to the pairs of @var{prop_name}
## and @var{prop_value}.  The supported logical operators are:
## @qcode{"-and"}, @qcode{"-or"},
## @qcode{"-xor"}, @qcode{"-not"}.
##
## Objects may also be matched by comparing a regular expression to the
## property values, where property values that match
## @code{regexp (@var{prop_value}, @var{pattern})} are returned.
##
## Finally, objects may be matched by property name only by using the
## @qcode{"-property"} option.
##
## Implementation Note: The search only includes objects with visible
## handles (HandleVisibility = @qcode{"on"}).  @xref{XREFfindall,,findall}, to
## search for all objects including hidden ones.
## @seealso{findall, allchild, get, set}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>

function h = findobj (varargin)

  depth = NaN;
  if (nargin == 0)
    handles = 0;
    n1 = 0;
  else
    if (! isempty (varargin{1}))
      if (ishandle (varargin{1}(1)))
        handles = varargin{1};
        n1 = 2;
      else
        handles = 0;
        n1 = 1;
      endif
    else
      ## Return [](0x1) for compatibility.
      h = zeros (0, 1);
      return;
    endif
    if (n1 <= nargin)
      if (ischar (varargin{n1}))
        if (strcmpi (varargin{n1}, "flat"))
          depth = 0;
          n1 = n1 + 1;
        endif
      else
        error ("findobj: properties and options must be strings");
      endif
    endif
  endif

  if (n1 <= nargin && nargin > 0)
    args = varargin(n1 : nargin);
  else
    args = {};
  endif

  regularexpression = [];
  property          = [];
  logicaloperator   = {};
  extranegation     = [];
  pname             = {};
  pvalue            = {};
  np = 1;
  na = 1;
  operatorprecedence = {"-not", "-and", "-or", "-xor"};

  while (na <= numel (args))
    regularexpression(np) = 0;
    property(np) = 0;
    if (numel (extranegation) < np)
      extranegation(np) = false;
    endif
    logicaloperator{np} = "and";
    if (ischar (args{na}))
      if (strcmpi (args{na}, "-property"))
        if (na + 1 <= numel (args))
          na = na + 1;
          property(np) = 1;
          pname{np} = args{na};
          na = na + 1;
          pvalue{np} = [];
          np = np + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      elseif (strcmpi (args{na}, "-regexp"))
        if (na + 2 <= numel (args))
          regularexpression(np) = 1;
          na = na + 1;
          pname{np} = args{na};
          na = na + 1;
          pvalue{np} = args{na};
          na = na + 1;
          np = np + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      elseif (strcmpi (args{na}, "-depth"))
        if (na + 1 <= numel (args))
          na = na + 1;
          depth = args{na};
          na = na + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      elseif (! strcmp (args{na}(1), "-"))
        ## Parameter/value pairs.
        if (na + 1 <= numel (args))
          pname{np} = args{na};
          na = na + 1;
          pvalue{np} = args{na};
          na = na + 1;
          if (na <= numel (args))
            if (ischar (args{na}))
              if (any (strcmpi (args{na}, operatorprecedence)))
                logicaloperator{np} = args{na}(2:end);
                na = na+1;
              endif
            else
              error ("findobj: properties and options must be strings");
            endif
          else
            logicaloperator{np} = "and";
          endif
          np = np + 1;
        else
          error ("findobj: inconsistent number of arguments");
        endif
      else
        if (strcmpi (args{na}, "-not"))
          extranegation(np) = true;
        endif
        na = na + 1;
      endif
    else
      error ("findobj: properties and options must be strings");
    endif
  endwhile

  numpairs = np - 1;
  if (! isempty (logicaloperator))
    logicaloperator = shift (logicaloperator, 1);
  endif

  ## Load all objects which qualify for being searched.
  idepth = 0;
  h = handles;
  while (numel (handles) && ! (idepth >= depth))
    children = [];
    for n = 1 : numel (handles)
      children = [children; get(handles(n), "children")];
    endfor
    handles = children;
    h = [h; children];
    idepth = idepth + 1;
  endwhile

  if (numpairs > 0)
    match = true (numel (h), numpairs);
    for nh = 1 : numel (h)
      p = get (h(nh));
      for np = 1 : numpairs
        fields = fieldnames (p);
        fieldindex = find (strcmpi (fields, pname{np}), 1);
        if (numel (fieldindex))
          pname{np} = fields{fieldindex};
          if (property(np))
            match(nh,np) = true;
          else
            if (regularexpression(np))
              found = regexp (p.(pname{np}), pvalue{np}, "once");
              if (isempty (found))
                match(nh,np) = false;
              else
                match(nh,np) = true;
              endif
            elseif (numel (p.(pname{np})) == numel (pvalue{np}))
              if (ischar (pvalue{np}) && ischar (p.(pname{np})))
                match(nh,np) = strcmpi (pvalue{np}, p.(pname{np}));
              elseif (isnumeric (pvalue{np} && isnumeric (p.(pname{np}))))
                match(nh,np) = (pvalue{np} == p.(pname{np}));
              else
                match(nh,np) = isequal (pvalue{np}, p.(pname{np}));
              endif
            else
              match(nh,np) = false;
            endif
          endif
        else
          match(nh,np) = false;
        endif
        if (extranegation(np))
          match(nh,np) = ! match(nh,np);
        endif
      endfor
    endfor

    if (numpairs > 1)
      for no = 1 : numel (operatorprecedence)
        pairs = find (strcmp (logicaloperator(2:end), ...
                              operatorprecedence{no}(2:end)));
        for np = sort (pairs, "descend")
          if (no == 1)
            match(:,np+1) = ! match(:,np+1);
            logicaloperator(np+1) = {"and"};
          else
            match(:,np) = feval (logicaloperator{np+1}, match(:,np), ...
                                 match(:,np+1));
            logicaloperator(np+1) = [];
            match(:,np+1) = [];
            numpairs = numpairs - 1;
          endif
          if (numpairs < 2)
            break;
          endif
        endfor
        if (numpairs < 2)
          break;
        endif
      endfor
    endif
  else
    match = true (numel (h), 1);
  endif

  h = h(match);
  h = h(:);
endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = findobj (gca (), "-property", "foo");
%!   assert (isempty (h));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10);
%!   set (h, "tag", "foobar");
%!   g = findobj (gcf (), "tag", "foobar", "type", "line", "color", [0 0 1]);
%!   assert (g, h);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   l = line;
%!   obj = findobj (hf, "type", "line");
%!   assert (l, obj);
%!   assert (gca, findobj (hf, "type", "axes"));
%!   assert (hf, findobj (hf, "type", "figure"));
%!   assert (isempty (findobj (hf, "type", "xyzxyz")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   subplot (2,2,1);
%!    imagesc (rand (10));
%!   subplot (2,2,2);
%!    surf (peaks);
%!   subplot (2,2,3);
%!    contour (peaks);
%!   subplot (2,2,4);
%!    plot (peaks);
%!   h1 = findobj (gcf (), "-regexp", "Type", "image|surface|hggroup");
%!   h2 = findobj (gcf (), "Type", "image",
%!                  "-or", "Type", "surface",
%!                  "-or", "Type", "hggroup");
%!   assert (h2, h1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! toolkit = graphics_toolkit ("gnuplot");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = subplot (2,2,1);
%!   h2 = subplot (2,2,2);
%!   h3 = subplot (2,2,3, "userdata", struct ("foo", "bar"));
%!   h4 = subplot (2,2,4);
%!   h = findobj (hf, "userdata", struct ("foo", "bar"));
%!   assert (h, h3);
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!test
%! toolkit = graphics_toolkit ("gnuplot");
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = subplot (2,2,1, "tag", "1");
%!   h2 = subplot (2,2,2, "tag", "2");
%!   h3 = subplot (2,2,3, "tag", "3");
%!   h4 = subplot (2,2,4, "tag", "4");
%!   h = findobj (hf, "type", "axes", "-not", "tag", "1");
%!   assert (h, [h4; h3; h2])
%! unwind_protect_cleanup
%!   close (hf);
%!   graphics_toolkit (toolkit);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h1 = subplot (2, 2, 1);
%!   set (h1, "userdata", struct ("column", 1, "row", 1));
%!   h2 = subplot (2, 2, 2);
%!   set (h2, "userdata", struct ("column", 2, "row", 1));
%!   h3 = subplot (2, 2, 3);
%!   set (h3, "userdata", struct ("column", 1, "row", 2));
%!   h4 = subplot (2, 2, 4);
%!   set (h4, "userdata", struct ("column", 2, "row", 2));
%!   h = findobj (hf, "type", "axes",
%!                "-not", "userdata", struct ("column", 1, "row", 1));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%! assert (h, [h4; h3; h2])

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ha = axes ();
%!   plot (1:10);
%!   h = findobj (hf, "type", "figure",
%!                "-or", "parent", hf,
%!                "-and", "type", "axes");
%! unwind_protect_cleanup
%!   close (hf)
%! end_unwind_protect
%! assert (h, [hf; ha])

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (hf, "tag", "foo");
%!   h1 = subplot (2,2,1, "tag", "foo");
%!   h2 = subplot (2,2,2, "tag", "bar");
%!   h3 = subplot (2,2,3, "tag", "foo");
%!   h4 = subplot (2,2,4, "tag", "bar");
%!   h = findobj (hf, "type", "axes", "-xor", "tag", "foo");
%!   assert (h, [hf; h4; h2]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax1 = subplot (2,1,1);
%!    hl1 = plot (rand (10,1));
%!   hax2 = subplot (2,1,2);
%!    hl2 = plot (rand (10,1));
%!   hobj = findobj (hf);
%!   assert (hobj, [hf; hax2; hax1; hl2; hl1]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

