## Copyright (C) 2014-2015 Massimiliano Fasi
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
## @deftypefn {Function File} {@var{h} =} hgload (@var{filename})
## Load the graphics object in @var{filename} into the graphics handle @var{h}.
##
## If @var{filename} has no extension, Octave will try to find the file with
## and without the standard extension of @file{.ofig}.
## @seealso{hgsave, struct2hdl}
## @end deftypefn

## Author: Massimiliano Fasi

function h = hgload (filename)

  ## Check input arguments
  if (nargin != 1)
    print_usage ();
  endif

  ## Check file existence
  [~, ~, ext] = fileparts (filename);
  if (isempty (ext))
    if (! isempty (file_in_loadpath ([filename ".ofig"])))
      filename = [filename ".ofig"];
    elseif (isempty (file_in_loadpath (filename)))
      error ("hgload: unable to locate file %s", filename);
    endif
  else
    if (isempty (file_in_loadpath (filename)))
      error ("hgload: unable to locate file %s", filename);
    endif
  endif

  ## Load the handle
  try
    stmp = load (filename, "s_oct40");
  catch
    error ("hgload: could not load hgsave-formatted object in file %s", filename);
  end_try_catch

  h = struct2hdl (stmp.s_oct40);

endfunction


## Functional test for hgload/hgsave pair is in hgsave.m

## Test input validation
%!error hgload ()
%!error hgload (1, 2)
%!error <unable to locate file> hgload ("%%_A_REALLY_UNLIKELY_FILENAME_%%")

