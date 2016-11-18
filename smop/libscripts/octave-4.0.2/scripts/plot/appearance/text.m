## Copyright (C) 2007-2015 John W. Eaton
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
## @deftypefn  {Function File} {} text (@var{x}, @var{y}, @var{string})
## @deftypefnx {Function File} {} text (@var{x}, @var{y}, @var{z}, @var{string})
## @deftypefnx {Function File} {} text (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{h} =} text (@dots{})
## Create a text object with text @var{string} at position @var{x}, @var{y},
## (@var{z}) on the current axes.
##
## Multiple locations can be specified if @var{x}, @var{y}, (@var{z}) are
## vectors.  Multiple strings can be specified with a character matrix or
## a cell array of strings.
##
## Optional property/value pairs may be used to control the appearance of the
## text.
##
## The optional return value @var{h} is a vector of graphics handles to the
## created text objects.
## @seealso{gtext, title, xlabel, ylabel, zlabel}
## @end deftypefn

## Author: jwe

## Note: The following code is rigged for Matlab compatibility and is
##       full of hidden assumptions.  Be very wary when modifying.

function h = text (varargin)

  nargs = nargin;
  offset = 0;

  if (nargs > 2 && isnumeric (varargin{1}) && isnumeric (varargin{2}))
    x = varargin{1};
    y = varargin{2};

    if (nargin > 3 && isnumeric (varargin{3}))
      z = varargin{3};
      offset = 4;
    else
      z = zeros (size (x));
      offset = 3;
    endif

    string = varargin{offset};
    varargin(1:offset) = [];

    nx = numel (x);
    ny = numel (y);
    nz = numel (z);
    if (ischar (string))

      do_keyword_repl = true;
      nt = rows (string);
      if (nx == 1 && nt == 1)
        ## Single text object with one line
        string = {string};
      elseif (nx == 1 && nt > 1)
        ## Single text object with multiple lines
        ## FIXME: "default" or "factory" as first row
        ##        should be escaped to "\default" or "\factory"
        ##        Other rows do not require escaping.
        do_keyword_repl = false;
        string = {string};
      elseif (nx > 1 && nt == nx)
        ## Mutiple text objects with different strings
        string = cellstr (string);
      else
        ## Mutiple text objects with same string
        string = repmat ({string}, [nx, 1]);
        nt = nx;
      endif

      ## Escape special keywords
      if (do_keyword_repl)
        string = regexprep (string, '^(default|factory)$', '\\$1');
      endif

    elseif (iscell (string))

      nt = numel (string);
      if (nx == 1)
        ## Single text object with one or more lines
        string = {string};
        nt = 1;
      elseif (nx > 1 && nt == nx)
        ## Mutiple text objects with different strings
      else
        ## Mutiple text objects with same string
        string = repmat ({string}, [nx, 1]);
        nt = nx;
      endif

    else

      error ("text: STRING must be a character string or cell array of character strings");

    endif
  else  # Only PROP/VALUE pairs
    x = y = z = 0;
    nx = ny = nz = 1;
    string = {""};
    nt = 1;
  endif

  ## Any remaining inputs must occur as PROPERTY/VALUE pairs
  if (rem (numel (varargin), 2) != 0)
    print_usage ();
  endif

  ## Get axis argument which may be in a 'parent' PROP/VAL pair
  [hax, varargin] = __plt_get_axis_arg__ ("text", varargin{:});

  if (isempty (hax))
    hax = gca ();
  else
    hax = hax(1);
  endif

  ## Position argument may alse be in PROP/VAL pair
  idx = find (strcmpi (varargin, "position"), 1);
  if (idx)
    pos = varargin{idx+1};
    varargin(idx:idx+1) = [];
  else
    pos = [x(:), y(:), z(:)];
  endif

  if (nx == ny && nx == nz && (nt == nx || nt == 1 || nx == 1))
    htmp = zeros (nt, 1);
    if (nx == 1)
      htmp = __go_text__ (hax, "string", string{1},
                               ## varargin first, in case "Units" set for pos.
                               varargin{:},
                               "position", pos);
    elseif (nx == nt)
      for n = 1:nt
        htmp(n) = __go_text__ (hax, "string", string{n},
                                    varargin{:},
                                    "position", pos(n,:));
      endfor
      __request_drawnow__ ();
    else
      error ("text: dimension mismatch for coordinates and STRING");
    endif
  elseif (nt == nx || nt == 1 || nx == 1)
    error ("text: dimension mismatch for coordinates");
  else
    error ("text: dimension mismatch between coordinates and strings");
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! ha = {'left', 'center', 'right'};
%! va = {'bottom', 'middle', 'top'};
%! x = [0.25 0.5 0.75];
%! y = x;
%! for t = 0:30:359;
%!   for nh = 1:numel (ha)
%!     for nv = 1:numel (va)
%!       text (x(nh), y(nv), 'Hello World', ...
%!             'rotation', t, ...
%!             'horizontalalignment', ha{nh}, ...
%!             'verticalalignment', va{nv});
%!     end
%!   end
%! end
%! set (gca, 'xtick', [0.25, 0.5, 0.75], ...
%!           'xticklabel', ha, ...
%!           'ytick', [0.25, 0.5, 0.75], ...
%!           'yticklabel', va);
%! axis ([0 1 0 1]);
%! xlabel ('horizontal alignment');
%! ylabel ('vertical alignment');
%! title ('text alignment and rotation (0:30:360 degrees)');

%!demo
%! clf;
%! h = mesh (peaks, 'edgecolor', 0.7 * [1 1 1], ...
%!                  'facecolor', 'none', ...
%!                  'facealpha', 0);
%! for t = 0:45:359;
%!   text (25, 25, 0, 'Vertical Alignment = Bottom', ...
%!                    'rotation', t, ...
%!                    'horizontalalignment', 'left', ...
%!                    'verticalalignment', 'bottom');
%! end
%! caxis ([-100 100]);
%! title ('Vertically Aligned at Bottom');

%!demo
%! clf;
%! axis ([0 8 0 8]);
%! title (['1st title';'2nd title']);
%! xlabel (['1st xlabel';'2nd xlabel']);
%! ylabel (['1st ylabel';'2nd ylabel']);
%! text (4, 4, {'Hello', 'World'}, ...
%!       'horizontalalignment', 'center', ...
%!       'verticalalignment', 'middle');
%! grid on;

%!demo
%! clf;
%! h = mesh (peaks (), 'edgecolor', 0.7 * [1 1 1], ...
%!                     'facecolor', 'none', ...
%!                     'facealpha', 0);
%! title (['1st title';'2nd title']);
%! xlabel (['1st xlabel';'2nd xlabel']);
%! ylabel (['1st ylabel';'2nd ylabel']);
%! zlabel (['1st zlabel';'2nd zlabel']);
%! text (0, 0, 5, {'Hello', 'World'}, ...
%!       'horizontalalignment', 'center', ...
%!       'verticalalignment', 'middle');
%! hold on;
%! plot3 (0, 0, 5, '+k');

%!demo
%! clf;
%! h = text (0.5, 0.3, 'char');
%! assert ('char', class (get (h, 'string')));
%! h = text (0.5, 0.4, ['char row 1'; 'char row 2']);
%! assert ('char', class (get (h, 'string')));
%! h = text (0.5, 0.6, {'cell2str (1,1)', 'cell2str (1,2)'; 'cell2str (2,1)', 'cell2str (2,2)'});
%! assert ('cell', class (get (h, 'string')));
%! h = text (0.5, 0.8, 'foobar');
%! set (h, 'string', 1:3);
%! h = text ([0.1, 0.1], [0.3, 0.4], 'one string & two objects');
%! assert ('char', class (get (h(1), 'string')));
%! assert ('char', class (get (h(2), 'string')));
%! h = text ([0.1, 0.1], [0.5, 0.6], {'one cellstr & two objects'});
%! assert ('cell', class (get (h(1), 'string')));
%! assert ('cell', class (get (h(2), 'string')));
%! h = text ([0.1, 0.1], [0.7, 0.8], {'cellstr 1 object 1', 'cellstr 2 object 2'});
%! assert ('char', class (get (h(1), 'string')));
%! assert ('char', class (get (h(2), 'string')));
%! h = text ([0.1, 0.1], [0.1, 0.2], ['1st string & 1st object'; '2nd string & 2nd object']);
%! assert ('char', class (get (h(1), 'string')));
%! assert ('char', class (get (h(2), 'string')));
%! h = text (0.7, 0.6, 'single string');
%! assert ('char', class (get (h, 'string')));
%! h = text (0.7, 0.5, {'single cell-string'});
%! assert ('cell', class (get (h, 'string')));
%! xlabel (1:2);
%! ylabel (1:2);
%! title (1:2);

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   ## Single object with one line
%!   h = text (0.5, 0.3, "single object with one line");
%!   obs = get (h, "string");
%!   assert (class (obs), "char");
%!   assert (obs, "single object with one line");
%!
%!   ## Single object with multiple lines
%!   h = text (0.5, 0.4, ["char row 1"; "char row 2"]);
%!   obs = get (h, "string");
%!   assert (class (obs), "char");
%!   assert (obs, ["char row 1"; "char row 2"]);
%!
%!   ## Multiple objects with single line
%!   h = text ([0.1, 0.1], [0.3, 0.4], "two objects with same string");
%!   assert (class (get (h(1), "string")), "char");
%!   assert (class (get (h(2), "string")), "char");
%!   assert (get (h(1), "string"), "two objects with same string");
%!   assert (get (h(2), "string"), "two objects with same string");
%!
%!   ## Multiple objects with multiple lines
%!   h = text ([0.1, 0.1], [0.3, 0.4], ["string1"; "string2"]);
%!   assert (class (get (h(1), "string")), "char");
%!   assert (class (get (h(2), "string")), "char");
%!   assert (get (h(1), "string"), "string1");
%!   assert (get (h(2), "string"), "string2");
%!
%!   ### Tests repeated with cell input ###
%!
%!   ## Single object with one line
%!   h = text (0.5, 0.3, {"single object with one line"});
%!   obs = get (h, "string");
%!   assert (class (obs), "cell");
%!   assert (obs, {"single object with one line"});
%!
%!   ## Single object with multiple lines
%!   h = text (0.5, 0.6, {"cell2str (1,1)", "cell2str (1,2)";
%!                        "cell2str (2,1)", "cell2str (2,2)"});
%!   obs = get (h, "string");
%!   assert (class (obs), "cell");
%!   assert (obs, {"cell2str (1,1)"; "cell2str (2,1)";
%!                 "cell2str (1,2)"; "cell2str (2,2)"});
%!
%!   ## Multiple objects with single line
%!   h = text ([0.1, 0.1], [0.5, 0.6], {"two objects with same cellstr"});
%!   assert (class (get (h(1), "string")), "cell");
%!   assert (class (get (h(2), "string")), "cell");
%!   ## FIXME: is return value of cellstr, rather than string, Matlab-verified?
%!   assert (get (h(1), "string"), {"two objects with same cellstr"});
%!   assert (get (h(2), "string"), {"two objects with same cellstr"});
%!
%!   ## Multiple objects with multiple lines
%!   h = text ([0.1, 0.1], [0.7, 0.8], {"cellstr1", "cellstr2"});
%!   ## FIXME: is return value really char in Matlab?
%!   assert (class (get (h(1), "string")), "char");
%!   assert (class (get (h(2), "string")), "char");
%!   assert (get (h(1), "string"), "cellstr1");
%!   assert (get (h(2), "string"), "cellstr2");
%!
%!   ## Test special keyword processing
%!   h = text (0.5, 0.5, "default");
%!   assert (get (h, "string"), "default")
%!   h = text (0.5, 0.5, "factory");
%!   assert (get (h, "string"), "factory")
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

