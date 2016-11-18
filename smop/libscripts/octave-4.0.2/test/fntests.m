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

clear all;

global files_with_no_tests = {};
global files_with_tests = {};
global topsrcdir;
global topbuilddir;

currdir = canonicalize_file_name (".");

debug_on_error (true);

if (nargin == 1)
  xdir = argv (){1};
else
  xdir = ".";
endif

srcdir = canonicalize_file_name (xdir);
topsrcdir = canonicalize_file_name (fullfile (xdir, ".."));
topbuilddir = canonicalize_file_name (fullfile (currdir, ".."));

if (strcmp (currdir, srcdir))
  testdirs = {srcdir};
else
  testdirs = {currdir, srcdir};
endif

liboctave_tree = canonicalize_file_name (fullfile (topbuilddir, "liboctave"));
src_tree = canonicalize_file_name (fullfile (topbuilddir, "libinterp"));
script_tree = canonicalize_file_name (fullfile (topsrcdir, "scripts"));
local_script_tree = canonicalize_file_name (fullfile (currdir, "../scripts"));

fundirs = {liboctave_tree, src_tree, script_tree};

if (! strcmp (currdir, srcdir))
  fundirs{end+1} = local_script_tree;
endif

__run_test_suite__ (fundirs, testdirs);
