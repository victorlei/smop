## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {@var{choice} =} menu (@var{title}, @var{opt1}, @dots{})
## @deftypefnx {Function File} {@var{choice} =} menu (@var{title}, @{@var{opt1}, @dots{}@})
## Display a menu with heading @var{title} and options @var{opt1}, @dots{},
## and wait for user input.
##
## If the GUI is running, or Java is available, the menu is displayed
## graphically using @code{listdlg}.  Otherwise, the title and menu options
## are printed on the console.
##
## @var{title} is a string and the options may be input as individual strings
## or as a cell array of strings.
##
## The return value @var{choice} is the number of the option selected by the
## user counting from 1.
##
## This function is useful for interactive programs.  There is no limit to the
## number of options that may be passed in, but it may be confusing to present
## more than will fit easily on one screen.
## @seealso{input, listdlg}
## @end deftypefn

## Author: jwe

function choice = menu (title, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (! ischar (title))
    error ("menu: TITLE must be a string");
  elseif (nargin > 2 && ! iscellstr (varargin))
    error ("menu: All OPTIONS must be strings");
  elseif (! ischar (varargin{1}) && ! iscellstr (varargin{1}))
    error ("menu: OPTIONS must be string or cell array of strings");
  endif

  if (isguirunning () || usejava ("awt"))
    [choice, ok] = listdlg ("Name", "menu", "PromptString", title,
                            "ListString", varargin, "SelectionMode", "Single");
    if (! ok)
      choice = 1;
    endif
  else  # console menu
    ## Force pending output to appear before the menu.
    fflush (stdout);

    ## Don't send the menu through the pager since doing that can cause
    ## major confusion.
    page_screen_output (0, "local");

    if (! isempty (title))
      printf ("%s\n", title);
    endif

    nopt = numel (varargin);
    while (1)
      for i = 1:nopt
        printf ("  [%2d] %s\n", i, varargin{i});
      endfor
      printf ("\n");
      s = input ("Select a number: ", "s");
      choice = sscanf (s, "%d");
      if (! isscalar (choice) || choice < 1 || choice > nopt)
        printf ("\nerror: input invalid or out of range\n\n");
      else
        break;
      endif
    endwhile
  endif

endfunction


%!error menu ()
%!error menu ("title")
%!error <TITLE must be a string> menu (1, "opt1")
%!error <All OPTIONS must be strings> menu ("title", "opt1", 1)
%!error <OPTIONS must be string or cell array of strings> menu ("title", 1)

