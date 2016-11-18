## Copyright (C) 2010, 2013 Martin Hepperle
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
## @deftypefn  {Function File} {@var{h} =} errordlg (@var{msg})
## @deftypefnx {Function File} {@var{h} =} errordlg (@var{msg}, @var{title})
## @deftypefnx {Function File} {@var{h} =} errordlg (@var{msg}, @var{title}, @var{createmode})
## Display @var{msg} using an error dialog box.
##
## The message may have multiple lines separated by newline characters ("\n"),
## or it may be a cellstr array with one element for each line.
##
## The optional input @var{title} (character string) can be used to set the
## dialog caption.  The default title is @qcode{"Error Dialog"}.
##
## The return value is always 1.
##
## Compatibility Note: The optional argument @var{createmode} is accepted for
## @sc{matlab} compatibility, but is not implemented.
##
## @seealso{helpdlg, inputdlg, listdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function retval = errordlg (msg, title = "Error Dialog", varargin)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  retval = message_dialog ("errordlg", msg, title, "error", varargin{:});

endfunction


%!demo
%! disp ('- test errordlg with prompt only.');
%! errordlg ('Oops, an expected error occurred');

%!demo
%! disp ('- test errordlg with prompt and caption.');
%! errordlg ('Oops another error','This is a very long and informative caption');

