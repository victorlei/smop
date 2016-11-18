## Copyright (C) 2009-2015 Søren Hauberg
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
## @deftypefn  {Function File} {} doc_cache_create (@var{out_file}, @var{directory})
## @deftypefnx {Function File} {} doc_cache_create (@var{out_file})
## @deftypefnx {Function File} {} doc_cache_create ()
## Generate documentation cache for all functions in @var{directory}.
##
## A documentation cache is generated for all functions in @var{directory}
## which may be a single string or a cell array of strings.  The cache is used
## to speed up the function @code{lookfor}.
##
## The cache is saved in the file @var{out_file} which defaults to the value
## @file{doc-cache} if not given.
##
## If no directory is given (or it is the empty matrix), a cache for built-in
## operators, etc. is generated.
##
## @seealso{doc_cache_file, lookfor, path}
## @end deftypefn

function doc_cache_create (out_file = "doc-cache", directory = [])

  ## Check input
  if (! ischar (out_file))
    print_usage ();
  endif

  ## Generate cache
  if (isempty (directory))
    cache = gen_builtin_cache ();
  elseif (iscell (directory))
    if (all (cellfun ("isclass", directory, "char")))
      cache = gen_doc_cache_in_dir (directory);
    else
      error ("doc_cache_create: cell must contain only strings");
    endif
  elseif (ischar (directory))
     cache = gen_doc_cache_in_dir (directory);
  else
     error ("doc_cache_create: second input argument must be a string or a cell of strings");
  endif

  ## Save cache
  if (! isempty (cache))
     save_header_format_string (["# doc-cache created by Octave " OCTAVE_VERSION], "local");
     save ("-text", out_file, "cache");
  endif

endfunction

function [text, first_sentence, status] = handle_function (f, text, format)
  first_sentence = "";
  ## Skip functions that start with __ as these shouldn't be searched by lookfor
  if (length (f) > 2 && all (f (1:2) == "_"))
    status = 1;
    return;
  endif

  ## Take action depending on help text format
  switch (lower (format))
    case "plain text"
      status = 0;
    case "texinfo"
      [text, status] = __makeinfo__ (text, "plain text");
    case "html"
      [text, status] = strip_html_tags (text);
    otherwise
      status = 1;
  endswitch

  ## Did we get the help text?
  if (status != 0 || isempty (text))
    warning ("doc_cache_create: unusable help text found in file '%s'", f);
    return;
  endif

  ## Get first sentence of help text
  first_sentence = get_first_help_sentence (f);
endfunction

function cache = create_cache (list)
  cache = {};

  ## For each function:
  for n = 1:length (list)
    f = list{n};

    ## Get help text
    [text, format] = get_help_text (f);

    [text, first_sentence, status] = handle_function (f, text, format);

    ## Did we get the help text?
    if (status != 0)
      continue;
    endif

    ## Store the help text
    cache(1, end+1) = f;
    cache(2, end) = text;
    cache(3, end) = first_sentence;
  endfor
endfunction

function cache = gen_doc_cache_in_dir (directory)

  ## If 'directory' is not in the current path, add it so we search it
  dir_in_path = ismember (directory, ostrsplit (path (), pathsep ()));

  ## dirs not in path
  if (! iscell (directory))
    directory = {directory};
  endif
  dirs_notpath = {directory{! dir_in_path}};

  ## add them
  if (! isempty (dirs_notpath))
    addpath (dirs_notpath{:});
  endif

  ## create cache
  func = @(s_) create_cache (__list_functions__ (s_));
  cache = cellfun (func, directory, "UniformOutput", false);

  ## concatenate results
  cache = [cache{:}];

  ## remove dirs form path
  if (! isempty (dirs_notpath))
    rmpath (dirs_notpath{:});
  endif

endfunction

function cache = gen_builtin_cache ()
  operators = __operators__ ();
  keywords = __keywords__ ();
  builtins = __builtins__ ();
  list = {operators{:}, keywords{:}, builtins{:}};

  cache = create_cache (list);
endfunction


## No true tests desirable for this function.
## Test input validation
%!error doc_cache_create (1)

