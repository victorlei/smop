## Copyright (C) 2001-2015 Paul Kienzle
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
## @deftypefn  {Command} {} edit @var{name}
## @deftypefnx {Command} {} edit @var{field} @var{value}
## @deftypefnx {Command} {@var{value} =} edit get @var{field}
## Edit the named function, or change editor settings.
##
## If @code{edit} is called with the name of a file or function as its
## argument it will be opened in the text editor defined by @env{EDITOR}.
##
## @itemize @bullet
## @item
## If the function @var{name} is available in a file on your path and that
## file is modifiable, then it will be edited in place.  If it is a system
## function, then it will first be copied to the directory @env{HOME} (see
## below) and then edited.  If no file is found, then the m-file variant,
## ending with @qcode{".m"}, will be considered.  If still no file is found,
## then variants with a leading @qcode{"@@"} and then with both a leading
## @qcode{"@@"} and trailing @qcode{".m"} will be considered.
##
## @item
## If @var{name} is the name of a function defined in the interpreter but not
## in an m-file, then an m-file will be created in @env{HOME} to contain that
## function along with its current definition.
##
## @item
## If @code{@var{name}.cc} is specified, then it will search for
## @code{@var{name}.cc} in the path and try to modify it, otherwise it will
## create a new @file{.cc} file in the current directory.  If @var{name} happens
## to be an m-file or interpreter defined function, then the text of that
## function will be inserted into the .cc file as a comment.
##
## @item
## If @file{@var{name}.ext} is on your path then it will be edited, otherwise
## the editor will be started with @file{@var{name}.ext} in the current
## directory as the filename.  If @file{@var{name}.ext} is not modifiable,
## it will be copied to @env{HOME} before editing.
##
## @strong{Warning:} You may need to clear @var{name} before the new definition
## is available.  If you are editing a .cc file, you will need to execute
## @code{mkoctfile @file{@var{name}.cc}} before the definition will be
## available.
## @end itemize
##
## If @code{edit} is called with @var{field} and @var{value} variables, the
## value of the control field @var{field} will be set to @var{value}.  If an
## output argument is requested and the first input argument is @code{get}
## then @code{edit} will return the value of the control field @var{field}.
## If the control field does not exist, edit will return a structure
## containing all fields and values.  Thus, @code{edit get all} returns a
## complete control structure.
##
## The following control fields are used:
##
## @table @samp
## @item home
## This is the location of user local m-files.  Be sure it is in your path.
## The default is @file{~/octave}.
##
## @item author
## This is the name to put after the "## Author:" field of new functions.  By
## default it guesses from the @code{gecos} field of the password database.
##
## @item email
## This is the e-mail address to list after the name in the author field.  By
## default it guesses @code{<$LOGNAME@@$HOSTNAME>}, and if @code{$HOSTNAME}
## is not defined it uses @code{uname -n}.  You probably want to override
## this.  Be sure to use the format @code{@email{user@@host}}.
##
## @item license
##
## @table @samp
## @item gpl
## GNU General Public License (default).
##
## @item bsd
## BSD-style license without advertising clause.
##
## @item pd
## Public domain.
##
## @item "text"
## Your own default copyright and license.
## @end table
##
## Unless you specify @samp{pd}, edit will prepend the copyright statement
## with "Copyright (C) yyyy Function Author".
##
## @item mode
## This value determines whether the editor should be started in async mode
## (editor is started in the background and Octave continues) or sync mode
## (Octave waits until the editor exits).  Set it to @qcode{"sync"} to start
## the editor in sync mode.  The default is @qcode{"async"}
## (@pxref{XREFsystem,,system}).
##
## @item editinplace
## Determines whether files should be edited in place, without regard to
## whether they are modifiable or not.  The default is @code{false}.
## @end table
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

## Original version by Paul Kienzle distributed as free software in the
## public domain.

function ret = edit (varargin)

  ## Pick up globals or default them.

  persistent FUNCTION = struct ("HOME",
                                fullfile (get_home_directory (), "octave"),
                                "AUTHOR", default_user(1),
                                "EMAIL", [],
                                "LICENSE", "GPL",
                                "MODE", "async",
                                "EDITINPLACE", false);
  ## Make sure the stateval variables survive "clear functions".
  mlock;

  ## Get default editor every time in case the user has changed it
  FUNCTION.EDITOR = [EDITOR() " %s"];

  if (nargin == 1)
    ## User has supplied one arg, this can be a single file name
    ## or a cell array of strings containing multiple files to be opened
    if (iscellstr (varargin{1}))
      ## If first arg is a cell array of strings,
      ## it becomes the list of files to be edited
      editfilelist = varargin{1};
    elseif (ischar (varargin{1}))
      ## If first arg is a string, create a cell array of strings
      ## of length 1 (by copying the input cell array)
      editfilelist = varargin(1);
    else
      error ("edit: expected file to be a string or cell array of strings");
    endif
  elseif (nargin == 2)
    ## User has supplied two arguments, these could be two file names,
    ## or a combination of editor state name and new value for that state,
    ## so first check for the various states
    statevar = varargin{1};
    stateval = varargin{2};
    switch (toupper (statevar))
      case "EDITOR"
        error ("Octave:deprecated-function",
               "The EDITOR option of edit has been removed.  Use EDITOR() directly.")
        return;
      case "HOME"
        if (! isempty (stateval) && stateval(1) == "~")
          stateval = [ get_home_directory, stateval(2:end) ];
        endif
        FUNCTION.HOME = stateval;
        return;
      case "AUTHOR"
        FUNCTION.AUTHOR = stateval;
        return;
      case "EMAIL"
        FUNCTION.EMAIL = stateval;
        return;
      case "LICENSE"
        FUNCTION.LICENSE = stateval;
        return;
      case "MODE"
        if (strcmp (stateval, "sync") || strcmp (stateval, "async"))
          FUNCTION.MODE = stateval;
        else
          error ('edit: expected "edit MODE sync|async"');
        endif
        return;
      case "EDITINPLACE"
        if (ischar (stateval))
          if (strcmpi (stateval, "true"))
            stateval = true;
          elseif (strcmpi (stateval, "false"))
            stateval = false;
          else
            stateval = eval (stateval);
          endif
        endif
        FUNCTION.EDITINPLACE = stateval;
        return;
      case "GET"
        if (isfield (FUNCTION, toupper (stateval)))
          ret = FUNCTION.(toupper (stateval));
        else
          ret = FUNCTION;
        endif
        return;
      otherwise
        ## If none of the states match, assume both inputs are actually
        ## file names to be opened.
        editfilelist = varargin;
    endswitch
  elseif (nargin > 2)
    if (iscellstr (varargin))
      editfilelist = varargin;
    else
      error ("edit: if supplying more than one input all inputs must be strings containing field names to open.");
    endif
  endif

  ## Start the editor without a file if no file is given.
  if (nargin == 0)
    if (exist (FUNCTION.HOME, "dir") == 7)
      curr_dir = pwd ();
      unwind_protect
        cd (FUNCTION.HOME);
        do_edit (FUNCTION.EDITOR, "", FUNCTION.MODE);
      unwind_protect_cleanup
        cd (curr_dir);
      end_unwind_protect
    else
      do_edit (FUNCTION.EDITOR, "", FUNCTION.MODE);
    endif
    return;
  endif

  if (numel (editfilelist) > 1)

    ## Call edit on each of the files in the list if there are more than 1
    for i = 1:numel (editfilelist)
      edit (editfilelist{i});
    endfor

  else

    ## Only one file name was supplied, get it from the cell array
    file = tilde_expand (editfilelist{1});

    ## Check whether the user is trying to edit a builtin or compiled function.
    switch (exist (file))
      case {3, 5}
        error ("edit: unable to edit a built-in or compiled function");
    endswitch

    ## Checks for whether the file is
    ## absolute or relative should be handled inside file_in_loadpath.
    ## That way, it will be possible to look up files correctly given
    ## partial path information.  For example, you should be able to
    ## edit a particular overloaded function by doing any one of
    ##
    ##   edit classname/foo
    ##   edit classname/foo.m
    ##   edit @classname/foo
    ##   edit @classname/foo.m
    ##
    ## This functionality is needed for other functions as well (at least
    ## help and type; there may be more).  So the place to fix that is in
    ## file_in_loadpath, possibly with some help from the load_path class.

    ## The code below includes a portion that serves as a place-holder for
    ## the changes suggested above.

    ## Create list of explicit and implicit file names.
    filelist = {file};
    ## If file has no extension, add file.m and file.cc to the list.
    idx = rindex (file, ".");
    if (idx == 0)
      if (isempty (regexp (file, '\.m$')))
        ## No ".m" at the end of the file, add to the list.
        filelist(end+1) = [file ".m"];
      endif
      if (isempty (regexp (file, '\.cc$')))
        ## No ".cc" at the end of the file, add to the list.
        filelist(end+1) = [file ".cc"];
      endif
    endif

    ## If the file includes a path, it may be an overloaded function.
    if (! index (file, "@") && strchr (file, '/\'))
      ## No "@" at the beginning of the file, add to the list.
      numfiles = numel (filelist);
      for n = 1:numfiles
        filelist(n+numfiles) = ["@" filelist{n}];
      endfor
    endif

    ## Search the entire path for the 1st instance of a file in the list.
    fileandpath = "";
    for n = 1:numel (filelist)
      filetoedit = file_in_path (path, filelist{n});
      if (! isempty (filetoedit))
        ## The path is explicitly included.
        fileandpath = filetoedit;
        break;
      endif
    endfor

    if (! isempty (fileandpath))
      ## If the file exists, then edit it.
      if (FUNCTION.EDITINPLACE)
        ## Edit in place even if it is protected.
        do_edit (FUNCTION.EDITOR, fileandpath, FUNCTION.MODE);
        return;
      else
        ## If the file is modifiable in place then edit it,
        ## otherwise make a copy in HOME and then edit it.
        fid = fopen (fileandpath, "r+t");
        if (fid < 0)
          from = fileandpath;
          fileandpath = [FUNCTION.HOME, from(rindex(from, filesep):end)];
          [status, msg] = copyfile (from, fileandpath, 1);
          if (status == 0)
            error (msg);
          endif
        else
          fclose (fid);
        endif
        do_edit (FUNCTION.EDITOR, fileandpath, FUNCTION.MODE);
        return;
      endif
    endif

    ## If editing a new file, prompt for creation if gui is running
    if (isguirunning ())
      if (! __octave_link_edit_file__ (file, "prompt"));
        return;
      endif
    endif

    ## If editing a new file that is neither an m-file nor an oct-file,
    ## just edit it.
    ## If in gui-mode, create it before or editor would prompt again.
    fileandpath = file;
    idx = rindex (file, ".");
    if (idx)
      name = file(1:idx-1);
      ext = file(idx+1:end);
    else
      name = file;
      ext = "";
    endif
    if (! any (strcmp (ext, {"cc", "m"})))
      ## Some unknown file.  Create and open it or just open it.
      if (isempty (ext))
        fileandpath = [fileandpath ".m"];  # Add .m extension per default
      endif
      if (isguirunning ())
        ## Write the initial file (if there is anything to write)
        ## Give user the opportunity to change the file extension
        fileandpath = uiputfile (fileandpath);
        fid = fopen (fileandpath, "wt");
        if (fid < 0)
          error ("edit: could not create %s", fileandpath);
        endif
        fclose (fid);
      endif
      do_edit (FUNCTION.EDITOR, fileandpath, FUNCTION.MODE);
      return;
    endif

    ## The file doesn't exist in path so
    ## create it, put in the function template, and edit it.

    ## Guess the email name if it was not given.
    if (isempty (FUNCTION.EMAIL))
      host = getenv ("HOSTNAME");
      if (isempty (host) && ispc ())
        host = getenv ("COMPUTERNAME");
      endif
      if (isempty (host))
        [~, host] = system ("uname -n");
        ## trim newline from end of hostname
        if (! isempty (host))
          host = host(1:end-1);
        endif
      endif
      if (isempty (host))
        FUNCTION.EMAIL = " ";
      else
        FUNCTION.EMAIL = ["<" default_user(0) "@" host ">"];
      endif
    endif

    ## Fill in the revision string.
    now = localtime (time);
    revs = ["Created: " strftime("%Y-%m-%d",now)];

    ## Fill in the copyright string.
    copyright = [strftime("Copyright (C) %Y ",now) FUNCTION.AUTHOR];

    ## Fill in the author tag field.
    author = ["Author: " FUNCTION.AUTHOR " " FUNCTION.EMAIL];

    ## Fill in the header.
    uclicense = toupper (FUNCTION.LICENSE);
    switch (uclicense)
      case "GPL"
        head = cstrcat (copyright, "\n\n", "\
This program is free software; you can redistribute it and/or modify it\n\
under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program.  If not, see <http://www.gnu.org/licenses/>.\
");
        tail = [author, "\n", revs];

      case "BSD"
        head = cstrcat (copyright, "\n\n", "\
This program is free software; redistribution and use in source and\n\
binary forms, with or without modification, are permitted provided that\n\
the following conditions are met:\n\
\n\
   1.Redistributions of source code must retain the above copyright\n\
     notice, this list of conditions and the following disclaimer.\n\
   2.Redistributions in binary form must reproduce the above copyright\n\
     notice, this list of conditions and the following disclaimer in the\n\
     documentation and/or other materials provided with the distribution.\n\
\n\
THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND\n\
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n\
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n\
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE\n\
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n\
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS\n\
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n\
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n\
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY\n\
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF\n\
SUCH DAMAGE.\
");
        tail = [author, "\n", revs];

      case "PD"
        head = "";
        tail = [author, "\n", revs, "\n\n", ...
                "This program is granted to the public domain."];

      otherwise
        head = "";
        tail = [copyright, "\n\n", FUNCTION.LICENSE, "\n", author, "\n", revs];
    endswitch

    ## Generate the function template.
    exists = exist (name);
    switch (ext)
      case {"cc", "C", "cpp"}
        if (isempty (head))
          comment = ["/*\n\n", tail, "\n\n*/\n\n"];
        else
          comment = ["/*\n\n", head, "\n\n", tail, "\n\n*/\n\n"];
        endif
        ## If we are shadowing an m-file, paste the code for the m-file.
        if (any (exists == [2, 103]))
          code = ['\ ', strrep(type(name){1}, "\n", "\n// ")];
        else
          code = " ";
        endif
        body = ["#include <octave/oct.h>\n\n",               ...
                "DEFUN_DLD(" name ", args, nargout, \"\\\n", ...
                name, "\\n\\\n\")\n{\n",                     ...
                "  octave_value_list retval;\n",             ...
                "  int nargin = args.length ();\n\n",        ...
                code, "\n  return retval;\n}\n"];

        text = [comment, body];
      case "m"
        ## If we are editing a function defined on the fly, paste the code.
        if (any (exists == [2, 103]))
          body = type (name){1};
        else
          body = ["function [retval] = " name " (input1, input2)\n\n" ...
                  "endfunction\n"];
        endif
        if (isempty (head))
          comment = ["## -*- texinfo -*- \n## @deftypefn {Function File} " ...
                     "{@var{retval} =} " name                              ...
                     " (@var{input1}, @var{input2})\n##\n"                 ...
                     "## @seealso{}\n## @end deftypefn\n\n"                ...
                     "## " strrep(tail, "\n", "\n## ") "\n\n"];
        else
          comment = ["## " strrep(head,"\n","\n## ") "\n\n"                ...
                     "## -*- texinfo -*- \n## @deftypefn {Function File} " ...
                     "{@var{retval} =} " name                              ...
                     " (@var{input1}, @var{input2})\n##\n"                 ...
                     "## @seealso{}\n## @end deftypefn\n\n"                ...
                     "## " strrep(tail, "\n", "\n## ") "\n\n"];
        endif
        text = [comment, body];
    endswitch

    ## Write the initial file (if there is anything to write)
    fid = fopen (fileandpath, "wt");
    if (fid < 0)
      error ("edit: could not create %s", fileandpath);
    endif
    fputs (fid, text);
    fclose (fid);

    do_edit (FUNCTION.EDITOR, fileandpath, FUNCTION.MODE);

  endif

endfunction

## Return the name associated with the current user ID.
##
## If LONG_FORM is 1, return the full name.  This will be the
## default author.  Otherwise return the login name.
## login@host will be the default email address.

function retval = default_user (long_form)

  ent = getpwuid (getuid);
  if (! isstruct (ent))
    retval = getenv ("USER");
    if (isempty (retval))
      retval = getenv ("USERNAME");
    endif
  elseif (long_form)
    retval = ent.gecos;
    pos = strfind (retval, ",");
    if (! isempty (pos))
      retval = retval(1:pos-1);
    endif
  else
    retval = ent.name;
  endif

endfunction

function do_edit (editor, file, mode)

  if (isguirunning ())
    __octave_link_edit_file__ (file);
  else
    system (sprintf (undo_string_escapes (editor), ['"' file '"']), [], mode);
  endif

endfunction


%!test
%! s.home = edit ("get", "home");
%! s.author = edit ("get", "author");
%! s.email = edit ("get", "email");
%! s.license = edit ("get", "license");
%! s.editinplace = edit ("get", "editinplace");
%! s.mode = edit ("get", "mode");
%! edit home none
%! edit author none
%! edit email none
%! edit license none
%! edit ("editinplace", ! s.editinplace)
%! if (s.mode(1) == "a")
%!   edit mode sync
%! else
%!   edit mode async
%! endif
%! edit ("home", s.home);
%! edit ("author", s.author);
%! edit ("email", s.email);
%! edit ("license", s.license);
%! edit ("editinplace", s.editinplace);
%! edit ("mode", s.mode);
%! assert (edit ("get", "home"), s.home);
%! assert (edit ("get", "author"), s.author);
%! assert (edit ("get", "email"), s.email);
%! assert (edit ("get", "license"), s.license);
%! assert (edit ("get", "editinplace"), s.editinplace);
%! assert (edit ("get", "mode"), s.mode);

