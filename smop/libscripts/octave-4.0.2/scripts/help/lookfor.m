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
## @deftypefn  {Command} {} lookfor @var{str}
## @deftypefnx {Command} {} lookfor -all @var{str}
## @deftypefnx {Function File} {[@var{fcn}, @var{help1str}] =} lookfor (@var{str})
## @deftypefnx {Function File} {[@var{fcn}, @var{help1str}] =} lookfor ("-all", @var{str})
## Search for the string @var{str} in the documentation of all functions in the
## current function search path.
##
## By default, @code{lookfor} looks for @var{str} in just the first sentence of
## the help string for each function found.  The entire help text of each
## function can be searched by using the @qcode{"-all"} argument.  All searches
## are case insensitive.
##
## When called with no output arguments, @code{lookfor} prints the list of
## matching functions to the terminal.  Otherwise, the output argument
## @var{fcns} contains the function names and @var{help1str} contains the first
## sentence from the help string of each function.
##
## Programming Note: The ability of @code{lookfor} to correctly identify the
## first sentence of the help text is dependent on the format of the function's
## help.  All Octave core functions are correctly formatted, but the same can
## not be guaranteed for external packages and user-supplied functions.
## Therefore, the use of the @qcode{"-all"} argument may be necessary to find
## related functions that are not a part of Octave.
##
## The speed of lookup is greatly enhanced by having a cached documentation
## file.  See @code{doc_cache_create} for more information.
## @seealso{help, doc, which, path, doc_cache_create}
## @end deftypefn

function [fcn, help1str] = lookfor (str, arg2)

  if (strcmpi (str, "-all"))
    ## The difference between using '-all' and not is which part of the caches
    ## we search.  The cache is organized such that row
    ## 1) contains the function name
    ## 2) contains the full help text
    ## 3) contains the first sentence of the help text.
    str = arg2;
    search_type = 2;  # search the second row (full help text)
  else
    search_type = 3;  # search the third column (first help sentence)
  endif
  str = lower (str);  # Compare is case insensitive

  ## Search functions, operators, and keywords that come with Octave
  cache_file = doc_cache_file ();
  if (exist (cache_file, "file"))
    [fcnlist, help_text] = search_cache (str, cache_file, search_type);
    had_core_cache = true;
  else
    fcnlist = help_text = {};
    had_core_cache = false;
  endif

  ## Search functions in new path dirs.
  orig_path = ostrsplit (__pathorig__ (), pathsep ());

  ## ditto for path.
  new_path = ostrsplit (path (), pathsep ());

  ## remove  directories already covered by orig_path.
  if (had_core_cache)
    new_path = setdiff (new_path, orig_path);
  endif

  for n = 1:numel (new_path)
    fcndir = new_path{n};
    cache_file = fullfile (fcndir, "doc-cache");
    if (exist (cache_file, "file"))
      ## We have a cache in the directory, then read it and search it!
      [fcns, htext] = search_cache (str, cache_file, search_type);
      fcnlist(end+1:end+length (fcns)) = fcns;
      help_text(end+1:end+length (htext)) = htext;

    else
      ## We don't have a cache.  Search files.
      for fcn_in_fcndir = (__list_functions__ (fcndir)).'
        fn = fcn_in_fcndir{1};

        ## Skip files that start with "__"
        if (strncmp (fn, "__", 2))
          continue;
        endif

        status = 0;

        ## Extract first sentence
        try
          warn_state = warning ();
          unwind_protect
            warning ("off");
            first_sentence = get_first_help_sentence (fn, 1024);
          unwind_protect_cleanup
            warning (warn_state);
          end_unwind_protect
        catch
          status = 1;
        end_try_catch

        if (status)
          ## Error getting first help sentence
        elseif (search_type == 3)
          ## only search the first sentence of the help text
          text = first_sentence;
        elseif (search_type == 2)
          ## search entire help text
          try
            warn_state = warning ();
            unwind_protect
              warning ("off");
              [text, fmt] = get_help_text (fn);
              switch (lower (fmt))
                case "plain text"
                  status = 0;
                case "texinfo"
                  [text, status] = __makeinfo__ (text, "plain text");
                case "html"
                  [text, status] = strip_html_tags (text);
                otherwise
                  status = 1;
              endswitch
            unwind_protect_cleanup
              warning (warn_state);
            end_unwind_protect
          catch
            status = 1;
          end_try_catch
        endif

        ## Search the help text
        if (status == 0 && strfind (lower (text), str))
          fcnlist(end+1) = fn;
          help_text(end+1) = first_sentence;
        endif
      endfor
    endif
  endfor

  if (nargout == 0)
    ## Print the results (FIXME: it would be nice to break at word boundaries)
    indent = 20;
    term_width = (terminal_size ())(2);
    desc_width = term_width - indent - 2;
    indent_space = blanks (indent);
    for k = 1:length (fcnlist)
      f = fcnlist{k};
      f(end+1:indent-1) = " ";
      puts ([f " "]);
      lf = length (f);
      desc = strtrim (strrep (help_text{k}, "\n", " "));
      ldesc = length (desc);
      printf ("%s\n", desc(1:min (ldesc, desc_width - (lf - indent))));
      for start = (desc_width - (lf - indent) + 1):desc_width:ldesc
        stop = min (start + desc_width, ldesc);
        printf ("%s%s\n", indent_space, strtrim (desc (start:stop)));
      endfor
    endfor
  else
    ## Return the results instead of displaying them
    fcn = fcnlist;
    help1str = help_text;
  endif

endfunction

function [fcns, help_texts] = search_cache (str, cache_file, search_type)
  load (cache_file);
  if (! isempty (cache))
    t1 = strfind (lower (cache (1, :)), str);
    t2 = strfind (lower (cache (search_type, :)), str);
    cache_idx = find (! (cellfun ("isempty", t1) & cellfun ("isempty", t2)));
    fcns = cache(1, cache_idx);
    help_texts = cache(3, cache_idx);
  else
    fcns = help_texts = {};
  endif
endfunction

