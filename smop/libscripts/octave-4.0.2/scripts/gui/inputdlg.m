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
## @deftypefn  {Function File} {@var{cstr} =} inputdlg (@var{prompt})
## @deftypefnx {Function File} {@var{cstr} =} inputdlg (@var{prompt}, @var{title})
## @deftypefnx {Function File} {@var{cstr} =} inputdlg (@var{prompt}, @var{title}, @var{rowscols})
## @deftypefnx {Function File} {@var{cstr} =} inputdlg (@var{prompt}, @var{title}, @var{rowscols}, @var{defaults})
## Return user input from a multi-textfield dialog box in a cell array of
## strings, or an empty cell array if the dialog is closed by the Cancel
## button.
##
## Inputs:
##
## @table @var
## @item prompt
## A cell array with strings labeling each text field.  This input is required.
##
## @item title
## String to use for the caption of the dialog.  The default is
## @qcode{"Input Dialog"}.
##
## @item rowscols
## Specifies the size of the text fields and can take three forms:
##
## @enumerate
## @item a scalar value which defines the number of rows used for each text field.
##
## @item a vector which defines the individual number of rows used for each text field.
##
## @item a matrix which defines the individual number of rows and columns used for each text field.  In the matrix each row describes a single text field.  The first column specifies the number of input rows to use and the second column specifies the text field width.
## @end enumerate
##
## @item defaults
## A list of default values to place in each text fields.  It must be a cell
## array of strings with the same size as @var{prompt}.
## @end table
## @seealso{errordlg, helpdlg, listdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function cstr = inputdlg (prompt, title = "Input Dialog", varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (iscell (prompt))
    ## Silently extract only char elements
    prompt = prompt(cellfun ("isclass", prompt, "char"));
  elseif (ischar (prompt))
    prompt = {prompt};
  else
    error ("inputdlg: PROMPT must be a character string or cellstr array");
  endif

  if (! ischar (title))
    error ("inputdlg: TITLE must be a character string");
  endif

  switch (numel (varargin))
    case 0
      linespec = 1;
      defaults = cellstr (cell (size (prompt)));

    case 1
      linespec = varargin{1};
      defaults = cellstr (cell (size (prompt)));

    case 2
      linespec = varargin{1};
      defaults = varargin{2};
  endswitch

  ## specification of text field sizes as in Matlab
  ## Matlab requires a matrix for linespec, not a cell array...
  ## rc = [1,10; 2,20; 3,30];
  ##     c1  c2
  ## r1  1   10   first  text field is 1x10
  ## r2  2   20   second text field is 2x20
  ## r3  3   30   third  text field is 3x30
  if (! isnumeric (linespec))
    error ("inputdlg: ROWSCOLS must be numeric");
  endif

  if (isscalar (linespec))
    ## only scalar value in lineTo, copy from linespec and add defaults
    rowscols = zeros (numel (prompt), 2);
    ## cols
    rowscols(:,2) = 25;
    rowscols(:,1) = linespec;
  elseif (isvector (linespec))
    if (numel (linespec) == numel (prompt))
      ## only one column in lineTo, copy from vector linespec and add defaults
      rowscols = zeros (numel (prompt), 2);
      ## rows from colum vector linespec, columns are set to default
      rowscols(:,2) = 25;
      rowscols(:,1) = linespec(:);
    else
      error ("inputdlg: ROWSCOLS vector does not match size of PROMPT");
    endif
  elseif (ismatrix (linespec))
    if (rows (linespec) == columns (prompt) && columns (linespec) == 2)
      ## (rows x columns) match, copy array linespec
      rowscols = linespec;
    else
      error ("inputdlg: ROWSCOLS matrix does not match size of PROMPT");
    endif
  else
    ## dunno
    error ("inputdlg: unknown form of ROWSCOLS argument");
  endif
  rowscols = ceil (rowscols);

  ## convert numeric values in defaults cell array to strings
  defs = cellfun (@num2str, defaults, "UniformOutput", false);
  rc = arrayfun (@num2str, rowscols, "UniformOutput", false);

  if (__octave_link_enabled__ ())
    cstr = __octave_link_input_dialog__ (prompt, title, rowscols, defs);
  elseif (__have_feature__ ("JAVA"))
    user_inputs = javaMethod ("inputdlg", "org.octave.JDialogBox",
                              prompt, title, rc, defs);
    if (isempty (user_inputs))
      cstr = {};
    else
      cstr = cellstr (user_inputs);
    endif
  else
    error ("inputdlg is not available in this version of Octave");
  endif

endfunction


%!demo
%! disp ('- test inputdlg with prompt and caption only.');
%! prompt = {'Width','Height','Depth'};
%! dims = inputdlg (prompt, 'Enter Box Dimensions');
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!   helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
%! end

%!demo
%! disp ('- test inputdlg with prescribed scalar (2 lines per text field) and defaults.');
%! prompt = {'Width','Height','Depth'};
%! default = {'1.1','2.2','3.3'};
%! rc = 2;
%! dims = inputdlg (prompt, 'Enter Box Dimensions',rc,default);
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!    helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
%! end

%!demo
%! disp ('- test inputdlg with prescribed vector [1,2,3] for # of lines per text field and defaults.');
%! prompt = {'Width','Height','Depth'};
%! default = {'1.10', '2.10', '3.10'};
%! rc = [1,2,3];  % NOTE: must be an array
%! dims = inputdlg (prompt, 'Enter Box Dimensions',rc,default);
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!   helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
%! end

%!demo
%! disp ('- test inputdlg with prescribed row by column sizes and defaults.');
%! prompt = {'Width','Height','Depth'};
%! default = {'1.10', '2.20', '3.30'};
%! rc = [1,10; 2,20; 3,30];  % NOTE: must be an array
%! dims = inputdlg (prompt, 'Enter Box Dimensions',rc,default);
%! if (isempty (dims))
%!   helpdlg ('Canceled by user', 'Information');
%! else
%!   volume  = str2num (dims{1}) * str2num (dims{2}) * str2num (dims{3});
%!   surface = 2 * (str2num (dims{1}) * str2num (dims{2}) + ...
%!                  str2num (dims{2}) * str2num (dims{3}) + ...
%!                  str2num (dims{1}) * str2num (dims{3}));
%!   helpdlg (sprintf ('Results:\nVolume = %.3f\nSurface = %.3f', volume, surface), 'Box Dimensions');
%! end

