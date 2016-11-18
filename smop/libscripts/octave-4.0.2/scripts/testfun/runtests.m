## Copyright (C) 2010-2015 John W. Eaton
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
## @deftypefn  {Function File} {} runtests ()
## @deftypefnx {Function File} {} runtests (@var{directory})
## Execute built-in tests for all m-files in the specified @var{directory}.
##
## Test blocks in any C++ source files (@file{*.cc}) will also be executed
## for use with dynamically linked oct-file functions.
##
## If no directory is specified, operate on all directories in Octave's search
## path for functions.
## @seealso{rundemos, test, path}
## @end deftypefn

## Author: jwe

function runtests (directory)

  if (nargin == 0)
    dirs = ostrsplit (path (), pathsep ());
    do_class_dirs = true;
  elseif (nargin == 1)
    dirs = {canonicalize_file_name(directory)};
    if (isempty (dirs{1}))
      ## Search for directory name in path
      if (directory(end) == '/' || directory(end) == '\')
        directory(end) = [];
      endif
      fullname = dir_in_loadpath (directory);
      if (isempty (fullname))
        error ("runtests: DIRECTORY argument must be a valid pathname");
      endif
      dirs = {fullname};
    endif
    do_class_dirs = false;
  else
    print_usage ();
  endif

  for i = 1:numel (dirs)
    d = dirs{i};
    run_all_tests (d, do_class_dirs);
  endfor

endfunction

function run_all_tests (directory, do_class_dirs)
  flist = readdir (directory);
  dirs = {};
  no_tests = {};
  printf ("Processing files in %s:\n\n", directory);
  fflush (stdout);
  for i = 1:numel (flist)
    f = flist{i};
    if ((length (f) > 2 && strcmpi (f((end-1):end), ".m"))
        || (length (f) > 3 && strcmpi (f((end-2):end), ".cc")))
      ff = fullfile (directory, f);
      if (has_tests (ff))
        print_test_file_name (f);
        [p, n, xf, sk] = test (ff, "quiet");
        print_pass_fail (n, p);
        fflush (stdout);
      elseif (has_functions (ff))
        no_tests(end+1) = f;
      endif
    elseif (f(1) == "@")
      f = fullfile (directory, f);
      if (isdir (f))
        dirs(end+1) = f;
      endif
    endif
  endfor
  if (! isempty (no_tests))
    printf ("\nThe following files in %s have no tests:\n\n", directory);
    printf ("%s", list_in_columns (no_tests));
  endif

  ## Recurse into class directories since they are implied in the path
  if (do_class_dirs)
    for i = 1:numel (dirs)
      d = dirs{i};
      run_all_tests (d, false);
    endfor
  endif
endfunction

function retval = has_functions (f)
  n = length (f);
  if (n > 3 && strcmpi (f((end-2):end), ".cc"))
    fid = fopen (f);
    if (fid >= 0)
      str = fread (fid, "*char")';
      fclose (fid);
      retval = ! isempty (regexp (str,'^(?:DEFUN|DEFUN_DLD|DEFUNX)\>',
                                      'lineanchors', 'once'));
    else
      error ("fopen failed: %s", f);
    endif
  elseif (n > 2 && strcmpi (f((end-1):end), ".m"))
    retval = true;
  else
    retval = false;
  endif
endfunction

function retval = has_tests (f)
  fid = fopen (f);
  if (fid >= 0)
    str = fread (fid, "*char").';
    fclose (fid);
    retval = ! isempty (regexp (str, '^%!(?:test|xtest|assert|error|warning)',
                                     'lineanchors', 'once'));
  else
    error ("runtests: fopen failed: %s", f);
  endif
endfunction

function print_pass_fail (n, p)
  if (n > 0)
    printf (" PASS %4d/%-4d", p, n);
    nfail = n - p;
    if (nfail > 0)
      printf (" FAIL %d", nfail);
    endif
  endif
  puts ("\n");
endfunction

function print_test_file_name (nm)
  filler = repmat (".", 1, 55-length (nm));
  printf ("  %s %s", nm, filler);
endfunction


%!error runtests ("foo", 1)
%!error <DIRECTORY argument> runtests ("#_TOTALLY_/_INVALID_/_PATHNAME_#")

