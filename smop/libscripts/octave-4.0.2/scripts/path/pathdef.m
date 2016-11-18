## Copyright (C) 2005-2015 Bill Denney
## Copyright (C) 2007-2009 Ben Abbott
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
## @deftypefn {Function File} {@var{val} =} pathdef ()
## Return the default path for Octave.
##
## The path information is extracted from one of four sources.
## The possible sources, in order of preference, are:
##
## @enumerate
## @item @file{.octaverc}
##
## @item @file{~/.octaverc}
##
## @item @file{<OCTAVE_HOME>/@dots{}/<version>/m/startup/octaverc}
##
## @item Octave's path prior to changes by any octaverc file.
## @end enumerate
## @seealso{path, addpath, rmpath, genpath, savepath}
## @end deftypefn

function val = pathdef ()

  if (nargin > 0)
    print_usage ();
  endif

  ## Locate any project-specific .octaverc file.
  proj_octaverc = fullfile (pwd, ".octaverc");
  if (exist (proj_octaverc, "file"))
    proj_path = __extractpath__ (proj_octaverc);
    if (! isempty (proj_path))
      val = proj_path;
      return;
    endif
  endif

  ## Locate the user's ~/.octaverc file.
  user_octaverc = fullfile ("~", ".octaverc");
  if (exist (user_octaverc, "file"))
    user_path = __extractpath__ (user_octaverc);
    if (! isempty (user_path))
      val = user_path;
      return;
    endif
  endif

  ## No user octaverc file, locate the site octaverc file.
  pathdir = octave_config_info ("localstartupfiledir");
  site_octaverc = fullfile (pathdir, "octaverc");
  site_path = __extractpath__ (site_octaverc);
  if (! isempty (site_path))
    val = site_path;
    return;
  endif

  ## No project, user, or site octaverc file.  Use Octave's default.
  val = __pathorig__ ();

endfunction

## Extract the path information from the script/function @var{file}, created by
## @file{savepath.m}.  If successful, @code{__extractpath__} returns the path
## specified in @var{file}.

## Author: Ben Abbott <bpabbott@mac.com>

function path = __extractpath__ (savefile)

  [filelines, startline, endline] = getsavepath (savefile);
  if (startline > 0)
    tmp = regexprep (filelines(startline+1:endline-1),
                     "^.*path \\('([^\']+)'.*$", "$1");
    path = strjoin (tmp, ":");
  else
    path = "";
  endif

endfunction


## FIXME: Need some better BIST tests
%!assert (ischar (pathdef ()))

