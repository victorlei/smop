## Copyright (C) 2005-2015 David Bateman
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
## @deftypefn {Function File} {} __run_test_suite__ (@var{fcndirs}, @var{fixedtestdirs})
## Undocumented internal function.
## @end deftypefn

function __run_test_suite__ (fcndirs, fixedtestdirs)
  testsdir = octave_config_info ("octtestsdir");
  libinterptestdir = fullfile (testsdir, "libinterp");
  liboctavetestdir = fullfile (testsdir, "liboctave");
  fixedtestdir = fullfile (testsdir, "fixed");
  fcnfiledir = octave_config_info ("fcnfiledir");
  if (nargin == 0)
    fcndirs = { liboctavetestdir, libinterptestdir, fcnfiledir };
    fixedtestdirs = { fixedtestdir };
  endif
  global files_with_no_tests;
  global files_with_tests;
  files_with_no_tests = {};
  files_with_tests = {};
  ## FIXME: These names don't really make sense if we are running
  ##        tests for an installed copy of Octave.
  global topsrcdir = fcnfiledir;
  global topbuilddir = testsdir;
  pso = page_screen_output ();
  orig_wstate = warning ();
  logfile = make_absolute_filename ("fntests.log");
  unwind_protect
    page_screen_output (false);
    warning ("on", "quiet");
    warning ("off", "Octave:deprecated-function");
    try
      fid = fopen (logfile, "wt");
      if (fid < 0)
        error ("could not open %s for writing", logfile);
      endif
      test ("", "explain", fid);
      dp = dn = dxf = dsk = 0;
      puts ("\nIntegrated test scripts:\n\n");
      for i = 1:length (fcndirs)
        [p, n, xf, sk] = run_test_script (fid, fcndirs{i});
        dp += p;
        dn += n;
        dxf += xf;
        dsk += sk;
      endfor
      puts ("\nFixed test scripts:\n\n");
      for i = 1:length (fixedtestdirs)
        [p, n, xf, sk] = run_test_dir (fid, fixedtestdirs{i});
        dp += p;
        dn += n;
        dxf += xf;
        dsk += sk;
      endfor
      puts ("\nSummary:\n\n");
      nfail = dn - dp - dxf;
      printf ("  PASS    %6d\n", dp);
      printf ("  FAIL    %6d\n", nfail);
      if (dxf > 0)
        printf ("  XFAIL   %6d\n", dxf);
      endif
      if (dsk > 0)
        printf ("  SKIPPED %6d\n", dsk);
      endif
      puts ("\n");
      printf ("See the file %s for additional details.\n", logfile);
      if (dxf > 0)
        puts ("\n");
        puts ("Expected failures (listed as XFAIL above) are known bugs.\n");
        puts ("Please help improve Octave by contributing fixes for them.\n");
      endif
      if (dsk > 0)
        puts ("\n");
        puts ("Tests are most often skipped because the features they require\n");
        puts ("have been disabled.  Features are most often disabled because\n");
        puts ("they require dependencies that were not present when Octave\n");
        puts ("was built.  The configure script should have printed a summary\n");
        puts ("at the end of its run indicating which dependencies were not found.\n");
      endif

      ## Weed out deprecated and private functions
      weed_idx = cellfun (@isempty, regexp (files_with_tests, '\<deprecated\>|\<private\>', 'once'));
      files_with_tests = files_with_tests(weed_idx);
      weed_idx = cellfun (@isempty, regexp (files_with_no_tests, '\<deprecated\>|\<private\>', 'once'));
      files_with_no_tests = files_with_no_tests(weed_idx);

      report_files_with_no_tests (files_with_tests, files_with_no_tests, ".m");

      puts ("\nPlease help improve Octave by contributing tests for these files\n");
      printf ("(see the list in the file %s).\n\n", logfile);

      fprintf (fid, "\nFiles with no tests:\n\n%s",
                    list_in_columns (files_with_no_tests, 80));
      fclose (fid);
    catch
      disp (lasterr ());
    end_try_catch
  unwind_protect_cleanup
    warning ("off", "all");
    warning (orig_wstate);
    page_screen_output (pso);
  end_unwind_protect
endfunction

function print_test_file_name (nm)
  filler = repmat (".", 1, 60-length (nm));
  printf ("  %s %s", nm, filler);
endfunction

function print_pass_fail (p, n, xf, sk)
  if ((n + sk) > 0)
    printf (" PASS   %4d/%-4d", p, n);
    nfail = n - p - xf;
    if (nfail > 0)
      printf ("\n%71s %3d", "FAIL ", nfail);
    endif
    if (sk > 0)
      printf ("\n%71s %3d", "SKIP ", sk);
    endif
    if (xf > 0)
      printf ("\n%71s %3d", "XFAIL", xf);
    endif
  endif
  puts ("\n");
endfunction

function retval = has_functions (f)
  n = length (f);
  if (n > 3 && strcmpi (f((end-2):end), ".cc"))
    fid = fopen (f);
    if (fid >= 0)
      str = fread (fid, "*char")';
      fclose (fid);
      retval = ! isempty (regexp (str,'^(DEFUN|DEFUN_DLD)\>',
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
    str = fread (fid, "*char")';
    fclose (fid);
    retval = ! isempty (regexp (str,
                                '^%!(assert|error|fail|test|xtest|warning)',
                                'lineanchors', 'once'));
  else
    error ("fopen failed: %s", f);
  endif
endfunction

function [dp, dn, dxf, dsk] = run_test_dir (fid, d);
  global files_with_tests;
  global files_with_no_tests;
  lst = dir (d);
  dp = dn = dxf = dsk = 0;
  for i = 1:length (lst)
    nm = lst(i).name;
    if (lst(i).isdir
        && nm(1) != "." && ! strcmp (nm, "private") && nm(1) != "@")
      [p, n, xf, sk] = run_test_dir (fid, [d, filesep, nm]);
      dp += p;
      dn += n;
      dxf += xf;
      dsk += sk;
    endif
  endfor
  saved_dir = pwd ();
  unwind_protect
    cd (d);
    for i = 1:length (lst)
      nm = lst(i).name;
      if (length (nm) > 4 && strcmpi (nm((end-3):end), ".tst"))
        p = n = xf = sk = 0;
        ffnm = fullfile (d, nm);
        if (has_tests (ffnm))
          print_test_file_name (nm);
          [p, n, xf, sk] = test (nm, "quiet", fid);
          print_pass_fail (p, n, xf, sk);
          files_with_tests(end+1) = ffnm;
        else
          files_with_no_tests(end+1) = ffnm;
        endif
        dp += p;
        dn += n;
        dxf += xf;
        dsk += sk;
      endif
    endfor
  unwind_protect_cleanup
    cd (saved_dir);
  end_unwind_protect
endfunction

function [dp, dn, dxf, dsk] = run_test_script (fid, d);
  global files_with_tests;
  global files_with_no_tests;
  global topsrcdir;
  global topbuilddir;
  lst = dir (d);
  dp = dn = dxf = dsk = 0;
  for i = 1:length (lst)
    nm = lst(i).name;
    if (lst(i).isdir && nm(1) != ".")
      [p, n, xf, sk] = run_test_script (fid, [d, filesep, nm]);
      dp += p;
      dn += n;
      dxf += xf;
      dsk += sk;
    endif
  endfor
  for i = 1:length (lst)
    nm = lst(i).name;
    ## Ignore hidden files
    if (nm(1) == '.')
      continue
    endif
    f = fullfile (d, nm);
    if ((length (nm) > 2 && strcmpi (nm((end-1):end), ".m"))
        || (length (nm) > 4
            && (   strcmpi (nm((end-3):end), "-tst")
                || strcmpi (nm((end-3):end), ".tst"))))
      p = n = xf = 0;
      ## Only run if it contains %!test, %!assert, %!error, %!fail, or %!warning
      if (has_tests (f))
        tmp = strrep (f, [topsrcdir, filesep], "");
        tmp = strrep (tmp, [topbuilddir, filesep], "");
        print_test_file_name (tmp);
        [p, n, xf, sk] = test (f, "quiet", fid);
        print_pass_fail (p, n, xf, sk);
        dp += p;
        dn += n;
        dxf += xf;
        dsk += sk;
        files_with_tests(end+1) = f;
      else
        ## To reduce the list length, only mark .cc files that contain
        ## DEFUN definitions.
        files_with_no_tests(end+1) = f;
      endif
    endif
  endfor
  ##  printf("%s%s -> passes %d of %d tests\n", ident, d, dp, dn);
endfunction

function n = num_elts_matching_pattern (lst, pat)
  n = sum (! cellfun ("isempty", regexp (lst, pat, 'once')));
endfunction

function report_files_with_no_tests (with, without, typ)
  pat = ['\' typ "$"];
  n_with = num_elts_matching_pattern (with, pat);
  n_without = num_elts_matching_pattern (without, pat);
  n_tot = n_with + n_without;
  printf ("\n%d (of %d) %s files have no tests.\n", n_without, n_tot, typ);
endfunction


## No test coverage for internal function.  It is tested through calling fcn.
%!assert (1)

