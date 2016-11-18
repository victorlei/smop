## Copyright (C) 2005-2015 Søren Hauberg
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
## @deftypefn  {Command} {} doc @var{function_name}
## @deftypefnx {Command} {} doc
## Display documentation for the function @var{function_name} directly from an
## online version of the printed manual, using the GNU Info browser.
##
## If invoked without an argument, the manual is shown from the beginning.
##
## For example, the command @kbd{doc rand} starts the GNU Info browser at the
## @code{rand} node in the online version of the manual.
##
## Once the GNU Info browser is running, help for using it is available using
## the command @kbd{C-h}.
## @seealso{help}
## @end deftypefn

## Author: Soren Hauberg <soren@hauberg.org>
## Adapted-by: jwe

function retval = doc (fname)

  if (nargin == 0 || nargin == 1)

    ftype = 0;

    if (nargin == 1)
      ## Get the directory where the function lives.
      ## FIXME: Maybe we should have a better way of doing this?

      if (ischar (fname))
        ftype = exist (fname);
      else
        error ("doc: expecting argument to be a character string");
      endif
    else
      fname = "";
    endif

    ## if GUI is running, let it display the function
    if (isguirunning ())
      __octave_link_show_doc__ (fname);
    else

      if (ftype == 2 || ftype == 3)
        ffile = which (fname);
      else
        ffile = "";
      endif

      if (isempty (ffile))
        info_dir = octave_config_info ("infodir");
      else
        info_dir = fileparts (ffile);
      endif

      ## Determine if a file called doc.info exist in the same
      ## directory as the function.

      info_file_name = fullfile (info_dir, "doc.info");

      [stat_info, err] = stat (info_file_name);

      if (err < 0)
        info_file_name = info_file ();

        if (! exist (info_file_name, "file")
            && ! exist ([info_file_name ".gz"], "file")
            && ! exist ([info_file_name ".bz2"], "file"))
          __gripe_missing_component__ ("doc", "info-file");
        endif
      endif

      ## FIXME: Don't change the order of the arguments below because
      ## the info-emacs-info script currently expects --directory DIR as
      ## the third and fourth arguments.  Someone should fix that.

      cmd = sprintf ("\"%s\" --file \"%s\" --directory \"%s\"",
                     info_program (), info_file_name, info_dir);

      have_fname = ! isempty (fname);

      if (have_fname)
        status = system (sprintf ("%s --index-search \"%s\"", cmd, fname));
      endif


      if (! (have_fname && status == 0))
        status = system (cmd);
        if (status == 127)
          warning ("unable to find info program '%s'", info_program ());
        endif
      endif

      if (nargout > 0)
        retval = status;
      endif

    endif
  else
    print_usage ();
  endif

endfunction


%!test
%! ifile = info_file ();
%! if (exist (ifile) != 2 && exist (sprintf ("%s.gz", ifile)) != 2)
%!   error ("Info file %s or %s.gz does not exist!", ifile, ifile);
%! endif

