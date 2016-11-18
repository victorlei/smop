/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstring>

#include <iomanip>
#include <set>
#include <string>

#include "file-stat.h"
#include "oct-env.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-regexp.h"
#include "str-vec.h"

#include <defaults.h>
#include "Cell.h"
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "octave-link.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-class.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "symtab.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Defines layout for the whos/who -long command
static std::string Vwhos_line_format
  = "  %a:4; %ln:6; %cs:16:6:1;  %rb:12;  %lc:-1;\n";

void
clear_mex_functions (void)
{
  symbol_table::clear_mex_functions ();
}

void
clear_function (const std::string& nm)
{
  symbol_table::clear_function (nm);
}

void
clear_variable (const std::string& nm)
{
  symbol_table::clear_variable (nm);
}

void
clear_symbol (const std::string& nm)
{
  symbol_table::clear_symbol (nm);
}

// Attributes of variables and functions.

// Is this octave_value a valid function?

octave_function *
is_valid_function (const std::string& fcn_name,
                   const std::string& warn_for, bool warn)
{
  octave_function *ans = 0;

  if (! fcn_name.empty ())
    {
      octave_value val = symbol_table::find_function (fcn_name);

      if (val.is_defined ())
        ans = val.function_value (true);
    }

  if (! ans && warn)
    error ("%s: the symbol '%s' is not valid as a function",
           warn_for.c_str (), fcn_name.c_str ());

  return ans;
}

octave_function *
is_valid_function (const octave_value& arg,
                   const std::string& warn_for, bool warn)
{
  octave_function *ans = 0;

  std::string fcn_name;

  if (arg.is_string ())
    {
      fcn_name = arg.string_value ();

      ans = is_valid_function (fcn_name, warn_for, warn);
    }
  else if (warn)
    error ("%s: expecting function name as argument", warn_for.c_str ());

  return ans;
}

octave_function *
extract_function (const octave_value& arg, const std::string& warn_for,
                  const std::string& fname, const std::string& header,
                  const std::string& trailer)
{
  octave_function *retval = 0;

  retval = is_valid_function (arg, warn_for, 0);

  if (! retval)
    {
      if (arg.is_string ())
        {
          std::string s = arg.string_value ();

          std::string cmd = header;
          cmd.append (s);
          cmd.append (trailer);

          int parse_status;

          eval_string (cmd, true, parse_status, 0);

          if (parse_status == 0)
            {
              retval = is_valid_function (fname, warn_for, 0);

              if (! retval)
                {
                  error ("%s: '%s' is not valid as a function",
                         warn_for.c_str (), fname.c_str ());
                  return retval;
                }

              warning ("%s: passing function body as a string is obsolete; please use anonymous functions",
                       warn_for.c_str ());
            }
          else
            error ("%s: '%s' is not valid as a function",
                   warn_for.c_str (), fname.c_str ());
        }
      else
        error ("%s: expecting first argument to be a string",
               warn_for.c_str ());
    }

  return retval;
}

string_vector
get_struct_elts (const std::string& text)
{
  int n = 1;

  size_t pos = 0;

  size_t len = text.length ();

  while ((pos = text.find ('.', pos)) != std::string::npos)
    {
      if (++pos == len)
        break;

      n++;
    }

  string_vector retval (n);

  pos = 0;

  for (int i = 0; i < n; i++)
    {
      len = text.find ('.', pos);

      if (len != std::string::npos)
        len -= pos;

      retval[i] = text.substr (pos, len);

      if (len != std::string::npos)
        pos += len + 1;
    }

  return retval;
}

static inline bool
is_variable (const std::string& name)
{
  bool retval = false;

  if (! name.empty ())
    {
      octave_value val = symbol_table::varval (name);

      retval = val.is_defined ();
    }

  return retval;
}

string_vector
generate_struct_completions (const std::string& text,
                             std::string& prefix, std::string& hint)
{
  string_vector names;

  size_t pos = text.rfind ('.');

  if (pos != std::string::npos)
    {
      if (pos == text.length ())
        hint = "";
      else
        hint = text.substr (pos+1);

      prefix = text.substr (0, pos);

      std::string base_name = prefix;

      pos = base_name.find_first_of ("{(.");

      if (pos != std::string::npos)
        base_name = base_name.substr (0, pos);

      if (is_variable (base_name))
        {
          int parse_status;

          unwind_protect frame;

          frame.protect_var (error_state);
          frame.protect_var (warning_state);

          frame.protect_var (discard_error_messages);
          frame.protect_var (discard_warning_messages);

          discard_error_messages = true;
          discard_warning_messages = true;

          octave_value tmp = eval_string (prefix, true, parse_status);

          frame.run ();

          if (tmp.is_defined ()
              && (tmp.is_map () || tmp.is_java () || tmp.is_classdef_object ()))
            names = tmp.map_keys ();
        }
    }

  return names;
}

// FIXME: this will have to be much smarter to work "correctly".

bool
looks_like_struct (const std::string& text)
{
  bool retval = (! text.empty ()
                 && text != "."
                 && text.find_first_of (file_ops::dir_sep_chars ()) == std::string::npos
                 && text.find ("..") == std::string::npos
                 && text.rfind ('.') != std::string::npos);

#if 0
  symbol_record *sr = curr_sym_tab->lookup (text);

  if (sr && ! sr->is_function ())
    {
      int parse_status;

      unwind_protect frame;

      frame.protect_var (discard_error_messages);
      frame.protect_var (error_state);

      discard_error_messages = true;

      octave_value tmp = eval_string (text, true, parse_status);

      frame.run ();

      retval = (tmp.is_defined () && tmp.is_map ());
    }
#endif

  return retval;
}

static octave_value
do_isglobal (const octave_value_list& args)
{
  octave_value retval = false;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  if (! args(0).is_string ())
    {
      error ("isglobal: NAME must be a string");
      return retval;
    }

  std::string name = args(0).string_value ();

  return symbol_table::is_global (name);
}

DEFUN (isglobal, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isglobal (@var{name})\n\
Return true if @var{name} is a globally visible variable.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
global x\n\
isglobal (\"x\")\n\
   @result{} 1\n\
@end group\n\
@end example\n\
@seealso{isvarname, exist}\n\
@end deftypefn")
{
  return do_isglobal (args);
}

static octave_value
safe_symbol_lookup (const std::string& symbol_name)
{
  octave_value retval;

  unwind_protect frame;
  interpreter_try (frame);

  retval = symbol_table::find (symbol_name);

  error_state = 0;

  return retval;
}

int
symbol_exist (const std::string& name, const std::string& type)
{
  if (is_keyword (name))
    return 0;

  bool search_any = type == "any";
  bool search_var = type == "var";
  bool search_dir = type == "dir";
  bool search_file = type == "file";
  bool search_builtin = type == "builtin";

  if (search_any || search_var)
    {
      octave_value val = symbol_table::varval (name);

      if (val.is_constant () || val.is_object ()
          || val.is_function_handle ()
          || val.is_anonymous_function ()
          || val.is_inline_function ())
        return 1;

      if (search_var)
        return 0;
    }

  // We shouldn't need to look in the global symbol table, since any name
  // that is visible in the current scope will be in the local symbol table.

  octave_value val;

  if (search_any || search_builtin)
    {
      // FIXME: safe_symbol_lookup will attempt unsafe load of .oct/.mex file.
      // This can cause a segfault.  To catch this would require temporarily
      // diverting the SIGSEGV exception handler and then restoring it.
      // See bug #36067.
      val = safe_symbol_lookup (name);

      if (val.is_defined () && val.is_builtin_function ())
        return 5;

      if (search_builtin)
        return 0;
    }

  if (search_any || search_file || search_dir)
    {
      std::string file_name = lookup_autoload (name);

      if (file_name.empty ())
        file_name = load_path::find_fcn (name);

      size_t len = file_name.length ();

      if (len > 0)
        {
          if (search_any || search_file)
            {
              if (len > 4 && (file_name.substr (len-4) == ".oct"
                              || file_name.substr (len-4) == ".mex"))
                return 3;
              else
                return 2;
            }
        }

      file_name = file_in_path (name, "");

      if (file_name.empty ())
        file_name = name;

      file_stat fs (file_name);

      if (fs)
        {
          if (search_any || search_file)
            {
              if (fs.is_dir ())
                return 7;

              len = file_name.length ();

              if (len > 4 && (file_name.substr (len-4) == ".oct"
                              || file_name.substr (len-4) == ".mex"))
                return 3;
              else
                return 2;
            }
          else if (search_dir && fs.is_dir ())
            return 7;
        }

      if (search_file || search_dir)
        return 0;
    }

  // Command line function which Matlab does not support
  if (search_any && val.is_defined () && val.is_user_function ())
    return 103;

  return 0;
}

#define GET_IDX(LEN) \
  static_cast<int> ((LEN-1) * static_cast<double> (rand ()) / RAND_MAX)

std::string
unique_symbol_name (const std::string& basename)
{
  static const std::string alpha
    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

  static size_t len = alpha.length ();

  std::string nm = basename + alpha[GET_IDX (len)];

  size_t pos = nm.length ();

  if (nm.substr (0, 2) == "__")
    nm.append ("__");

  while (symbol_exist (nm, "any"))
    nm.insert (pos++, 1, alpha[GET_IDX (len)]);

  return nm;
}

DEFUN (exist, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{c} =} exist (@var{name})\n\
@deftypefnx {Built-in Function} {@var{c} =} exist (@var{name}, @var{type})\n\
Check for the existence of @var{name} as a variable, function, file,\n\
directory, or class.\n\
\n\
The return code @var{c} is one of\n\
\n\
@table @asis\n\
@item 1\n\
@var{name} is a variable.\n\
\n\
@item 2\n\
@var{name} is an absolute file name, an ordinary file in Octave's\n\
@code{path}, or (after appending @samp{.m}) a function file in Octave's\n\
@code{path}.\n\
\n\
@item 3\n\
@var{name} is a @samp{.oct} or @samp{.mex} file in Octave's @code{path}.\n\
\n\
@item 5\n\
@var{name} is a built-in function.\n\
\n\
@item 7\n\
@var{name} is a directory.\n\
\n\
@item 103\n\
@var{name} is a function not associated with a file (entered on the command\n\
line).\n\
\n\
@item 0\n\
@var{name} does not exist.\n\
@end table\n\
\n\
If the optional argument @var{type} is supplied, check only for symbols of\n\
the specified type.  Valid types are\n\
\n\
@table @asis\n\
@item @qcode{\"var\"}\n\
Check only for variables.\n\
\n\
@item @qcode{\"builtin\"}\n\
Check only for built-in functions.\n\
\n\
@item @qcode{\"dir\"}\n\
Check only for directories.\n\
\n\
@item @qcode{\"file\"}\n\
Check only for files and directories.\n\
\n\
@item @qcode{\"class\"}\n\
Check only for classes.  (Note: This option is accepted, but not currently\n\
implemented)\n\
@end table\n\
\n\
If no type is given, and there are multiple possible matches for name,\n\
@code{exist} will return a code according to the following priority list:\n\
variable, built-in function, oct-file, directory, file, class.\n\
\n\
@code{exist} returns 2 if a regular file called @var{name} is present in\n\
Octave's search path.  If you want information about other types of files\n\
not on the search path you should use some combination of the functions\n\
@code{file_in_path} and @code{stat} instead.\n\
\n\
@seealso{file_in_loadpath, file_in_path, dir_in_loadpath, stat}\n\
@end deftypefn")
{
  octave_value retval = false;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();

          if (nargin == 2)
            {
              if (args(1).is_string ())
                {
                  std::string type = args(1).string_value ();

                  if (type == "class")
                    warning ("exist: \"class\" type argument is not implemented");

                  retval = symbol_exist (name, type);
                }
              else
                error ("exist: TYPE must be a string");
            }
          else
            retval = symbol_exist (name);
        }
      else
        error ("exist: NAME must be a string");
    }
  else
    print_usage ();

  return retval;
}

/*
%!shared dirtmp, __var1
%! dirtmp = P_tmpdir ();
%! __var1 = 1;

%!assert (exist ("__%Highly_unlikely_name%__"), 0)
%!assert (exist ("__var1"), 1)
%!assert (exist ("__var1", "var"), 1)
%!assert (exist ("__var1", "builtin"), 0)
%!assert (exist ("__var1", "dir"), 0)
%!assert (exist ("__var1", "file"), 0)

%!test
%! if (isunix ())
%!   assert (exist ("/bin/sh"), 2);
%!   assert (exist ("/bin/sh", "file"), 2);
%!   assert (exist ("/bin/sh", "dir"), 0);
%!   assert (exist ("/dev/null"), 2);
%!   assert (exist ("/dev/null", "file"), 2);
%!   assert (exist ("/dev/null", "dir"), 0);
%! endif

%!assert (exist ("print_usage"), 2)
%!assert (exist ("print_usage.m"), 2)
%!assert (exist ("print_usage", "file"), 2)
%!assert (exist ("print_usage", "dir"), 0)

## Don't search path for rooted relative file names
%!assert (exist ("plot.m", "file"), 2);
%!assert (exist ("./plot.m", "file"), 0);
%!assert (exist ("./%nonexistentfile%", "file"), 0);
%!assert (exist ("%nonexistentfile%", "file"), 0);

## Don't search path for absolute file names
%!test
%! tname = tempname (pwd ());
%! unwind_protect
%!   ## open/close file to create it, equivalent of touch
%!   fid = fopen (tname, "w");
%!   fclose (fid);
%!   [~, fname] = fileparts (tname);
%!   assert (exist (fullfile (pwd (), fname), "file"), 2);
%! unwind_protect_cleanup
%!   unlink (tname);
%! end_unwind_protect
%! assert (exist (fullfile (pwd (), "%nonexistentfile%"), "file"), 0);

%!testif HAVE_CHOLMOD
%! assert (exist ("chol"), 3);
%! assert (exist ("chol.oct"), 3);
%! assert (exist ("chol", "file"), 3);
%! assert (exist ("chol", "builtin"), 0);

%!assert (exist ("sin"), 5)
%!assert (exist ("sin", "builtin"), 5)
%!assert (exist ("sin", "file"), 0)

%!assert (exist (dirtmp), 7)
%!assert (exist (dirtmp, "dir"), 7)
%!assert (exist (dirtmp, "file"), 7)

%!error exist ()
%!error exist (1,2,3)
%!warning <"class" type argument is not implemented> exist ("a", "class");
%!error <TYPE must be a string> exist ("a", 1)
%!error <NAME must be a string> exist (1)

*/

octave_value
lookup_function_handle (const std::string& nm)
{
  octave_value val = symbol_table::varval (nm);

  return val.is_function_handle () ? val : octave_value ();
}

octave_value
get_global_value (const std::string& nm, bool silent)
{
  octave_value val = symbol_table::global_varval (nm);

  if (val.is_undefined () && ! silent)
    error ("get_global_value: undefined symbol '%s'", nm.c_str ());

  return val;
}

void
set_global_value (const std::string& nm, const octave_value& val)
{
  symbol_table::global_assign (nm, val);
}

octave_value
get_top_level_value (const std::string& nm, bool silent)
{
  octave_value val = symbol_table::top_level_varval (nm);

  if (val.is_undefined () && ! silent)
    error ("get_top_level_value: undefined symbol '%s'", nm.c_str ());

  return val;
}

void
set_top_level_value (const std::string& nm, const octave_value& val)
{
  symbol_table::top_level_assign (nm, val);
}

// Variable values.

static bool
wants_local_change (const octave_value_list& args, int& nargin)
{
  bool retval = false;

  if (nargin == 2)
    {
      if (args(1).is_string () && args(1).string_value () == "local")
        {
          nargin = 1;
          retval = true;
        }
      else
        {
          error_with_cfn ("expecting second argument to be \"local\"");
          nargin = 0;
        }
    }

  return retval;
}

template <class T>
bool try_local_protect (T& var)
{
  octave_user_code *curr_usr_code = octave_call_stack::caller_user_code ();
  octave_user_function *curr_usr_fcn = 0;
  if (curr_usr_code && curr_usr_code->is_user_function ())
    curr_usr_fcn = dynamic_cast<octave_user_function *> (curr_usr_code);

  if (curr_usr_fcn && curr_usr_fcn->local_protect (var))
    return true;
  else
    return false;
}

octave_value
set_internal_variable (bool& var, const octave_value_list& args,
                       int nargout, const char *nm)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning ("\"local\" has no effect outside a function");
    }

  if (nargin == 1)
    {
      bool bval = args(0).bool_value ();

      if (! error_state)
        var = bval;
      else
        error ("%s: expecting arg to be a logical value", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (char& var, const octave_value_list& args,
                       int nargout, const char *nm)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning ("\"local\" has no effect outside a function");
    }

  if (nargin == 1)
    {
      std::string sval = args(0).string_value ();

      if (! error_state)
        {
          switch (sval.length ())
            {
            case 1:
              var = sval[0];
              break;

            case 0:
              var = '\0';
              break;

            default:
              error ("%s: argument must be a single character", nm);
              break;
            }
        }
      else
        error ("%s: argument must be a single character", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (int& var, const octave_value_list& args,
                       int nargout, const char *nm,
                       int minval, int maxval)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning ("\"local\" has no effect outside a function");
    }

  if (nargin == 1)
    {
      int ival = args(0).int_value ();

      if (! error_state)
        {
          if (ival < minval)
            error ("%s: expecting arg to be greater than %d", nm, minval);
          else if (ival > maxval)
            error ("%s: expecting arg to be less than or equal to %d",
                   nm, maxval);
          else
            var = ival;
        }
      else
        error ("%s: expecting arg to be an integer value", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (double& var, const octave_value_list& args,
                       int nargout, const char *nm,
                       double minval, double maxval)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning ("\"local\" has no effect outside a function");
    }

  if (nargin == 1)
    {
      double dval = args(0).scalar_value ();

      if (! error_state)
        {
          if (dval < minval)
            error ("%s: expecting arg to be greater than %g", minval);
          else if (dval > maxval)
            error ("%s: expecting arg to be less than or equal to %g", maxval);
          else
            var = dval;
        }
      else
        error ("%s: expecting arg to be a scalar value", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
                       int nargout, const char *nm, bool empty_ok)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning ("\"local\" has no effect outside a function");
    }

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string sval = args(0).string_value ();

          if (empty_ok || ! sval.empty ())
            var = sval;
          else
            error ("%s: value must not be empty", nm);
        }
      else
        error ("%s: first argument must be a string", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (int& var, const octave_value_list& args,
                       int nargout, const char *nm, const char **choices)
{
  octave_value retval;
  int nchoices = 0;
  while (choices[nchoices] != 0)
    nchoices++;

  int nargin = args.length ();
  assert (var < nchoices);

  if (nargout > 0 || nargin == 0)
    retval = choices[var];

  if (wants_local_change (args, nargin))
    {
      if (! try_local_protect (var))
        warning ("\"local\" has no effect outside a function");
    }

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string sval = args(0).string_value ();

          int i = 0;
          for (; i < nchoices; i++)
            {
              if (sval == choices[i])
                {
                  var = i;
                  break;
                }
            }
          if (i == nchoices)
            error ("%s: value not allowed (\"%s\")", nm, sval.c_str ());
        }
      else
        error ("%s: first argument must be a string", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

struct
whos_parameter
{
  char command;
  char modifier;
  int parameter_length;
  int first_parameter_length;
  int balance;
  std::string text;
  std::string line;
};

static void
print_descriptor (std::ostream& os, std::list<whos_parameter> params)
{
  // This method prints a line of information on a given symbol
  std::list<whos_parameter>::iterator i = params.begin ();
  std::ostringstream param_buf;

  octave_preserve_stream_state stream_state (os);

  while (i != params.end ())
    {
      whos_parameter param = *i;

      if (param.command != '\0')
        {
          // Do the actual printing
          switch (param.modifier)
            {
            case 'l':
              os << std::setiosflags (std::ios::left)
                 << std::setw (param.parameter_length);
              param_buf << std::setiosflags (std::ios::left)
                        << std::setw (param.parameter_length);
              break;

            case 'r':
              os << std::setiosflags (std::ios::right)
                 << std::setw (param.parameter_length);
              param_buf << std::setiosflags (std::ios::right)
                        << std::setw (param.parameter_length);
              break;

            case 'c':
              if (param.command != 's')
                {
                  os << std::setiosflags (std::ios::left)
                     << std::setw (param.parameter_length);
                  param_buf << std::setiosflags (std::ios::left)
                            << std::setw (param.parameter_length);
                }
              break;

            default:
              os << std::setiosflags (std::ios::left)
                 << std::setw (param.parameter_length);
              param_buf << std::setiosflags (std::ios::left)
                        << std::setw (param.parameter_length);
            }

          if (param.command == 's' && param.modifier == 'c')
            {
              int a, b;

              if (param.modifier == 'c')
                {
                  a = param.first_parameter_length - param.balance;
                  a = (a < 0 ? 0 : a);
                  b = param.parameter_length - a - param.text . length ();
                  b = (b < 0 ? 0 : b);
                  os << std::setiosflags (std::ios::left) << std::setw (a)
                     << "" << std::resetiosflags (std::ios::left) << param.text
                     << std::setiosflags (std::ios::left)
                     << std::setw (b) << ""
                     << std::resetiosflags (std::ios::left);
                  param_buf << std::setiosflags (std::ios::left)
                            << std::setw (a)
                            << "" << std::resetiosflags (std::ios::left)
                            << param.line
                            << std::setiosflags (std::ios::left)
                            << std::setw (b) << ""
                            << std::resetiosflags (std::ios::left);
                }
            }
          else
            {
              os << param.text;
              param_buf << param.line;
            }
          os << std::resetiosflags (std::ios::left)
             << std::resetiosflags (std::ios::right);
          param_buf << std::resetiosflags (std::ios::left)
                    << std::resetiosflags (std::ios::right);
          i++;
        }
      else
        {
          os << param.text;
          param_buf << param.line;
          i++;
        }
    }

  os << param_buf.str ();
}

// FIXME: This is a bit of a kluge.  We'd like to just use val.dims()
// and if val is an object, expect that dims will call size if it is
// overloaded by a user-defined method.  But there are currently some
// unresolved const issues that prevent that solution from working.
// This same kluge is done in symtab.cc (do_workspace_info), fix there too.

std::string
get_dims_str (const octave_value& val)
{
  octave_value tmp = val;

  Matrix sz = tmp.size ();

  dim_vector dv = dim_vector::alloc (sz.numel ());

  for (octave_idx_type i = 0; i < dv.length (); i++)
    dv(i) = sz(i);

  return dv.str ();
}

class
symbol_info_list
{
private:
  struct symbol_info
  {
    symbol_info (const symbol_table::symbol_record& sr,
                 const std::string& expr_str = std::string (),
                 const octave_value& expr_val = octave_value ())
      : name (expr_str.empty () ? sr.name () : expr_str),
        varval (expr_val.is_undefined () ? sr.varval () : expr_val),
        is_automatic (sr.is_automatic ()),
        is_complex (varval.is_complex_type ()),
        is_formal (sr.is_formal ()),
        is_global (sr.is_global ()),
        is_persistent (sr.is_persistent ())
    { }

    void display_line (std::ostream& os,
                       const std::list<whos_parameter>& params) const
    {
      std::string dims_str = get_dims_str (varval);

      std::list<whos_parameter>::const_iterator i = params.begin ();

      octave_preserve_stream_state stream_state (os);

      while (i != params.end ())
        {
          whos_parameter param = *i;

          if (param.command != '\0')
            {
              // Do the actual printing.

              switch (param.modifier)
                {
                case 'l':
                  os << std::setiosflags (std::ios::left)
                     << std::setw (param.parameter_length);
                  break;

                case 'r':
                  os << std::setiosflags (std::ios::right)
                     << std::setw (param.parameter_length);
                  break;

                case 'c':
                  if (param.command == 's')
                    {
                      int front = param.first_parameter_length
                                  - dims_str.find ('x');
                      int back = param.parameter_length
                                 - dims_str.length ()
                                 - front;
                      front = (front > 0) ? front : 0;
                      back = (back > 0) ? back : 0;

                      os << std::setiosflags (std::ios::left)
                         << std::setw (front)
                         << ""
                         << std::resetiosflags (std::ios::left)
                         << dims_str
                         << std::setiosflags (std::ios::left)
                         << std::setw (back)
                         << ""
                         << std::resetiosflags (std::ios::left);
                    }
                  else
                    {
                      os << std::setiosflags (std::ios::left)
                         << std::setw (param.parameter_length);
                    }
                  break;

                default:
                  error ("whos_line_format: modifier '%c' unknown",
                         param.modifier);

                  os << std::setiosflags (std::ios::right)
                     << std::setw (param.parameter_length);
                }

              switch (param.command)
                {
                case 'a':
                  {
                    char tmp[6];

                    tmp[0] = (is_automatic ? 'a' : ' ');
                    tmp[1] = (is_complex ? 'c' : ' ');
                    tmp[2] = (is_formal ? 'f' : ' ');
                    tmp[3] = (is_global ? 'g' : ' ');
                    tmp[4] = (is_persistent ? 'p' : ' ');
                    tmp[5] = 0;

                    os << tmp;
                  }
                  break;

                case 'b':
                  os << varval.byte_size ();
                  break;

                case 'c':
                  os << varval.class_name ();
                  break;

                case 'e':
                  os << varval.capacity ();
                  break;

                case 'n':
                  os << name;
                  break;

                case 's':
                  if (param.modifier != 'c')
                    os << dims_str;
                  break;

                case 't':
                  os << varval.type_name ();
                  break;

                default:
                  error ("whos_line_format: command '%c' unknown",
                         param.command);
                }

              os << std::resetiosflags (std::ios::left)
                 << std::resetiosflags (std::ios::right);
              i++;
            }
          else
            {
              os << param.text;
              i++;
            }
        }
    }

    std::string name;
    octave_value varval;
    bool is_automatic;
    bool is_complex;
    bool is_formal;
    bool is_global;
    bool is_persistent;
  };

public:
  symbol_info_list (void) : lst () { }

  symbol_info_list (const symbol_info_list& sil) : lst (sil.lst) { }

  symbol_info_list& operator = (const symbol_info_list& sil)
  {
    if (this != &sil)
      lst = sil.lst;

    return *this;
  }

  ~symbol_info_list (void) { }

  void append (const symbol_table::symbol_record& sr)
  {
    lst.push_back (symbol_info (sr));
  }

  void append (const symbol_table::symbol_record& sr,
               const std::string& expr_str,
               const octave_value& expr_val)
  {
    lst.push_back (symbol_info (sr, expr_str, expr_val));
  }

  size_t size (void) const { return lst.size (); }

  bool empty (void) const { return lst.empty (); }

  octave_map
  map_value (const std::string& caller_function_name, int nesting_level) const
  {
    size_t len = lst.size ();

    Cell name_info (len, 1);
    Cell size_info (len, 1);
    Cell bytes_info (len, 1);
    Cell class_info (len, 1);
    Cell global_info (len, 1);
    Cell sparse_info (len, 1);
    Cell complex_info (len, 1);
    Cell nesting_info (len, 1);
    Cell persistent_info (len, 1);

    std::list<symbol_info>::const_iterator p = lst.begin ();

    for (size_t j = 0; j < len; j++)
      {
        const symbol_info& si = *p++;

        octave_scalar_map ni;

        ni.assign ("function", caller_function_name);
        ni.assign ("level", nesting_level);

        name_info(j) = si.name;
        global_info(j) = si.is_global;
        persistent_info(j) = si.is_persistent;

        octave_value val = si.varval;

        size_info(j) = val.size ();
        bytes_info(j) = val.byte_size ();
        class_info(j) = val.class_name ();
        sparse_info(j) = val.is_sparse_type ();
        complex_info(j) = val.is_complex_type ();
        nesting_info(j) = ni;
      }

    octave_map info;

    info.assign ("name", name_info);
    info.assign ("size", size_info);
    info.assign ("bytes", bytes_info);
    info.assign ("class", class_info);
    info.assign ("global", global_info);
    info.assign ("sparse", sparse_info);
    info.assign ("complex", complex_info);
    info.assign ("nesting", nesting_info);
    info.assign ("persistent", persistent_info);

    return info;
  }

  void display (std::ostream& os)
  {
    if (! lst.empty ())
      {
        size_t bytes = 0;
        size_t elements = 0;

        std::list<whos_parameter> params = parse_whos_line_format ();

        print_descriptor (os, params);

        octave_stdout << "\n";

        for (std::list<symbol_info>::const_iterator p = lst.begin ();
             p != lst.end (); p++)
          {
            p->display_line (os, params);

            octave_value val = p->varval;

            elements += val.capacity ();
            bytes += val.byte_size ();
          }

        os << "\nTotal is " << elements
           << (elements == 1 ? " element" : " elements")
           << " using " << bytes << (bytes == 1 ? " byte" : " bytes")
           << "\n";
      }
  }

  // Parse the string whos_line_format, and return a parameter list,
  // containing all information needed to print the given
  // attributtes of the symbols.
  std::list<whos_parameter> parse_whos_line_format (void)
  {
    int idx;
    size_t format_len = Vwhos_line_format.length ();
    char garbage;
    std::list<whos_parameter> params;

    size_t bytes1;
    int elements1;

    std::string param_string = "abcenst";
    Array<int> param_length (dim_vector (param_string.length (), 1));
    Array<std::string> param_names (dim_vector (param_string.length (), 1));
    size_t pos_a, pos_b, pos_c, pos_e, pos_n, pos_s, pos_t;

    pos_a = param_string.find ('a'); // Attributes
    pos_b = param_string.find ('b'); // Bytes
    pos_c = param_string.find ('c'); // Class
    pos_e = param_string.find ('e'); // Elements
    pos_n = param_string.find ('n'); // Name
    pos_s = param_string.find ('s'); // Size
    pos_t = param_string.find ('t'); // Type

    param_names(pos_a) = "Attr";
    param_names(pos_b) = "Bytes";
    param_names(pos_c) = "Class";
    param_names(pos_e) = "Elements";
    param_names(pos_n) = "Name";
    param_names(pos_s) = "Size";
    param_names(pos_t) = "Type";

    for (size_t i = 0; i < param_string.length (); i++)
      param_length(i) = param_names(i).length ();

    // The attribute column needs size 5.
    param_length(pos_a) = 5;

    // Calculating necessary spacing for name column,
    // bytes column, elements column and class column

    for (std::list<symbol_info>::const_iterator p = lst.begin ();
         p != lst.end (); p++)
      {
        std::stringstream ss1, ss2;
        std::string str;

        str = p->name;
        param_length(pos_n) = ((str.length ()
                                > static_cast<size_t> (param_length(pos_n)))
                               ? str.length () : param_length(pos_n));

        octave_value val = p->varval;

        str = val.type_name ();
        param_length(pos_t) = ((str.length ()
                                > static_cast<size_t> (param_length(pos_t)))
                               ? str.length () : param_length(pos_t));

        elements1 = val.capacity ();
        ss1 << elements1;
        str = ss1.str ();
        param_length(pos_e) = ((str.length ()
                                > static_cast<size_t> (param_length(pos_e)))
                               ? str.length () : param_length(pos_e));

        bytes1 = val.byte_size ();
        ss2 << bytes1;
        str = ss2.str ();
        param_length(pos_b) = ((str.length ()
                                > static_cast<size_t> (param_length(pos_b)))
                               ? str.length () : param_length (pos_b));
      }

    idx = 0;
    while (static_cast<size_t> (idx) < format_len)
      {
        whos_parameter param;
        param.command = '\0';

        if (Vwhos_line_format[idx] == '%')
          {
            bool error_encountered = false;
            param.modifier = 'r';
            param.parameter_length = 0;

            int a = 0;
            int b = -1;
            int balance = 1;
            unsigned int items;
            size_t pos;
            std::string cmd;

            // Parse one command from whos_line_format
            cmd = Vwhos_line_format.substr (idx, Vwhos_line_format.length ());
            pos = cmd.find (';');
            if (pos != std::string::npos)
              cmd = cmd.substr (0, pos+1);
            else
              error ("parameter without ; in whos_line_format");

            idx += cmd.length ();

            // FIXME: use iostream functions instead of sscanf!

            if (cmd.find_first_of ("crl") != 1)
              items = sscanf (cmd.c_str (), "%c%c:%d:%d:%d;",
                              &garbage, &param.command, &a, &b, &balance);
            else
              items = sscanf (cmd.c_str (), "%c%c%c:%d:%d:%d;",
                              &garbage, &param.modifier, &param.command,
                              &a, &b, &balance) - 1;

            if (items < 2)
              {
                error ("whos_line_format: parameter structure without command in whos_line_format");
                error_encountered = true;
              }

            // Insert data into parameter
            param.first_parameter_length = 0;
            pos = param_string.find (param.command);
            if (pos != std::string::npos)
              {
                param.parameter_length = param_length(pos);
                param.text = param_names(pos);
                param.line.assign (param_names(pos).length (), '=');

                param.parameter_length = (a > param.parameter_length
                                          ? a : param.parameter_length);
                if (param.command == 's' && param.modifier == 'c' && b > 0)
                  param.first_parameter_length = b;
              }
            else
              {
                error ("whos_line_format: '%c' is not a command",
                       param.command);
                error_encountered = true;
              }

            if (param.command == 's')
              {
                // Have to calculate space needed for printing
                // matrix dimensions Space needed for Size column is
                // hard to determine in prior, because it depends on
                // dimensions to be shown. That is why it is
                // recalculated for each Size-command int first,
                // rest = 0, total;
                int rest = 0;
                int first = param.first_parameter_length;
                int total = param.parameter_length;

                for (std::list<symbol_info>::const_iterator p = lst.begin ();
                     p != lst.end (); p++)
                  {
                    octave_value val = p->varval;
                    std::string dims_str = get_dims_str (val);
                    int first1 = dims_str.find ('x');
                    int total1 = dims_str.length ();
                    int rest1 = total1 - first1;
                    rest = (rest1 > rest ? rest1 : rest);
                    first = (first1 > first ? first1 : first);
                    total = (total1 > total ? total1 : total);
                  }

                if (param.modifier == 'c')
                  {
                    if (first < balance)
                      first += balance - first;
                    if (rest + balance < param.parameter_length)
                      rest += param.parameter_length - rest - balance;

                    param.parameter_length = first + rest;
                    param.first_parameter_length = first;
                    param.balance = balance;
                  }
                else
                  {
                    param.parameter_length = total;
                    param.first_parameter_length = 0;
                  }
              }
            else if (param.modifier == 'c')
              {
                error ("whos_line_format: modifier 'c' not available for command '%c'",
                       param.command);
                error_encountered = true;
              }

            // What happens if whos_line_format contains negative numbers
            // at param_length positions?
            param.balance = (b < 0 ? 0 : param.balance);
            param.first_parameter_length = (b < 0 ? 0 :
                                            param.first_parameter_length);
            param.parameter_length = (a < 0
                                      ? 0
                                      : (param.parameter_length
                                         < param_length(pos_s)
                                         ? param_length(pos_s)
                                         : param.parameter_length));

            // Parameter will not be pushed into parameter list if ...
            if (! error_encountered)
              params.push_back (param);
          }
        else
          {
            // Text string, to be printed as it is ...
            std::string text;
            size_t pos;
            text = Vwhos_line_format.substr (idx, Vwhos_line_format.length ());
            pos = text.find ('%');
            if (pos != std::string::npos)
              text = text.substr (0, pos);

            // Push parameter into list ...
            idx += text.length ();
            param.text=text;
            param.line.assign (text.length (), ' ');
            params.push_back (param);
          }
      }

    return params;
  }

private:
  std::list<symbol_info> lst;

};

static octave_value
do_who (int argc, const string_vector& argv, bool return_list,
        bool verbose = false, std::string msg = std::string ())
{
  octave_value retval;

  std::string my_name = argv[0];

  bool global_only = false;
  bool have_regexp = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-file")
        {
          // FIXME: This is an inefficient manner to implement this as the
          // variables are loaded in to a temporary context and then treated.
          // It would be better to refecat symbol_info_list to not store the
          // symbol records and then use it in load-save.cc (do_load) to
          // implement this option there so that the variables are never
          // stored at all.
          if (i == argc - 1)
            error ("whos: -file argument must be followed by a file name");
          else
            {
              std::string nm = argv[i + 1];

              unwind_protect frame;

              // Set up temporary scope.

              symbol_table::scope_id tmp_scope = symbol_table::alloc_scope ();
              frame.add_fcn (symbol_table::erase_scope, tmp_scope);

              symbol_table::set_scope (tmp_scope);

              octave_call_stack::push (tmp_scope, 0);
              frame.add_fcn (octave_call_stack::pop);

              frame.add_fcn (symbol_table::clear_variables);

              feval ("load", octave_value (nm), 0);

              if (! error_state)
                {
                  std::string newmsg = std::string ("Variables in the file ") +
                                       nm + ":\n\n";

                  retval =  do_who (i, argv, return_list, verbose, newmsg);
                }
            }

          return retval;
        }
      else if (argv[i] == "-regexp")
        have_regexp = true;
      else if (argv[i] == "global")
        global_only = true;
      else if (argv[i][0] == '-')
        warning ("%s: unrecognized option '%s'", my_name.c_str (),
                 argv[i].c_str ());
      else
        break;
    }

  int npats = argc - i;
  string_vector pats;
  if (npats > 0)
    {
      pats.resize (npats);
      for (int j = 0; j < npats; j++)
        pats[j] = argv[i+j];
    }
  else
    {
      pats.resize (++npats);
      pats[0] = "*";
    }

  symbol_info_list symbol_stats;
  std::list<std::string> symbol_names;

  for (int j = 0; j < npats; j++)
    {
      std::string pat = pats[j];

      if (have_regexp)
        {
          std::list<symbol_table::symbol_record> tmp = global_only
            ? symbol_table::regexp_global_variables (pat)
            : symbol_table::regexp_variables (pat);

          for (std::list<symbol_table::symbol_record>::const_iterator
               p = tmp.begin (); p != tmp.end (); p++)
            {
              if (p->is_variable ())
                {
                  if (verbose)
                    symbol_stats.append (*p);
                  else
                    symbol_names.push_back (p->name ());
                }
            }
        }
      else
        {
          size_t pos = pat.find_first_of (".({");

          if (pos != std::string::npos && pos > 0)
            {
              if (verbose)
                {
                  // NOTE: we can only display information for
                  // expressions based on global values if the variable is
                  // global in the current scope because we currently have
                  // no way of looking up the base value in the global
                  // scope and then evaluating the arguments in the
                  // current scope.

                  std::string base_name = pat.substr (0, pos);

                  if (symbol_table::is_variable (base_name))
                    {
                      symbol_table::symbol_record sr
                        = symbol_table::find_symbol (base_name);

                      if (! global_only || sr.is_global ())
                        {
                          int parse_status;

                          octave_value expr_val
                            = eval_string (pat, true, parse_status);

                          if (! error_state)
                            symbol_stats.append (sr, pat, expr_val);
                          else
                            return retval;
                        }
                    }
                }
            }
          else
            {
              std::list<symbol_table::symbol_record> tmp = global_only
                ? symbol_table::glob_global_variables (pat)
                : symbol_table::glob_variables (pat);

              for (std::list<symbol_table::symbol_record>::const_iterator
                   p = tmp.begin (); p != tmp.end (); p++)
                {
                  if (p->is_variable ())
                    {
                      if (verbose)
                        symbol_stats.append (*p);
                      else
                        symbol_names.push_back (p->name ());
                    }
                }
            }
        }
    }

  if (return_list)
    {
      if (verbose)
        {
          std::string caller_function_name;
          octave_function *caller = octave_call_stack::caller ();
          if (caller)
            caller_function_name = caller->name ();

          retval = symbol_stats.map_value (caller_function_name, 1);
        }
      else
        retval = Cell (string_vector (symbol_names));
    }
  else if (! (symbol_stats.empty () && symbol_names.empty ()))
    {
      if (msg.length () == 0)
        if (global_only)
          octave_stdout << "Global variables:\n\n";
        else
          octave_stdout << "Variables in the current scope:\n\n";
      else
        octave_stdout << msg;

      if (verbose)
        symbol_stats.display (octave_stdout);
      else
        {
          string_vector names (symbol_names);

          names.list_in_columns (octave_stdout);
        }

      octave_stdout << "\n";
    }

  return retval;
}

DEFUN (who, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} who\n\
@deftypefnx {Command} {} who pattern @dots{}\n\
@deftypefnx {Command} {} who option pattern @dots{}\n\
@deftypefnx {Command} {C =} who (\"pattern\", @dots{})\n\
List currently defined variables matching the given patterns.\n\
\n\
Valid pattern syntax is the same as described for the @code{clear} command.\n\
If no patterns are supplied, all variables are listed.\n\
\n\
By default, only variables visible in the local scope are displayed.\n\
\n\
The following are valid options, but may not be combined.\n\
\n\
@table @code\n\
@item global\n\
List variables in the global scope rather than the current scope.\n\
\n\
@item -regexp\n\
The patterns are considered to be regular expressions when matching the\n\
variables to display.  The same pattern syntax accepted by the @code{regexp}\n\
function is used.\n\
\n\
@item -file\n\
The next argument is treated as a filename.  All variables found within the\n\
specified file are listed.  No patterns are accepted when reading variables\n\
from a file.\n\
@end table\n\
\n\
If called as a function, return a cell array of defined variable names\n\
matching the given patterns.\n\
@seealso{whos, isglobal, isvarname, exist, regexp}\n\
@end deftypefn")
{
  octave_value retval;

  if (nargout < 2)
    {
      int argc = args.length () + 1;

      string_vector argv = args.make_argv ("who");

      if (! error_state)
        retval = do_who (argc, argv, nargout == 1);
    }
  else
    print_usage ();

  return retval;
}

DEFUN (whos, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} whos\n\
@deftypefnx {Command} {} whos pattern @dots{}\n\
@deftypefnx {Command} {} whos option pattern @dots{}\n\
@deftypefnx {Built-in Function} {S =} whos (\"pattern\", @dots{})\n\
Provide detailed information on currently defined variables matching the\n\
given patterns.\n\
\n\
Options and pattern syntax are the same as for the @code{who} command.\n\
\n\
Extended information about each variable is summarized in a table with the\n\
following default entries.\n\
\n\
@table @asis\n\
@item Attr\n\
Attributes of the listed variable.  Possible attributes are:\n\
\n\
@table @asis\n\
@item blank\n\
Variable in local scope\n\
\n\
@item @code{a}\n\
Automatic variable.  An automatic variable is one created by the\n\
interpreter, for example @code{argn}.\n\
\n\
@item @code{c}\n\
Variable of complex type.\n\
\n\
@item @code{f}\n\
Formal parameter (function argument).\n\
\n\
@item @code{g}\n\
Variable with global scope.\n\
\n\
@item @code{p}\n\
Persistent variable.\n\
@end table\n\
\n\
@item Name\n\
The name of the variable.\n\
\n\
@item Size\n\
The logical size of the variable.  A scalar is 1x1, a vector is\n\
@nospell{1xN} or @nospell{Nx1}, a 2-D matrix is @nospell{MxN}.\n\
\n\
@item Bytes\n\
The amount of memory currently used to store the variable.\n\
\n\
@item Class\n\
The class of the variable.  Examples include double, single, char, uint16,\n\
cell, and struct.\n\
@end table\n\
\n\
The table can be customized to display more or less information through\n\
the function @code{whos_line_format}.\n\
\n\
If @code{whos} is called as a function, return a struct array of defined\n\
variable names matching the given patterns.  Fields in the structure\n\
describing each variable are: name, size, bytes, class, global, sparse,\n\
complex, nesting, persistent.\n\
@seealso{who, whos_line_format}\n\
@end deftypefn")
{
  octave_value retval;

  if (nargout < 2)
    {
      int argc = args.length () + 1;

      string_vector argv = args.make_argv ("whos");

      if (! error_state)
        retval = do_who (argc, argv, nargout == 1, true);
    }
  else
    print_usage ();

  return retval;
}

// Defining variables.

void
bind_ans (const octave_value& val, bool print)
{
  static std::string ans = "ans";

  if (val.is_defined ())
    {
      if (val.is_cs_list ())
        {
          octave_value_list lst = val.list_value ();

          for (octave_idx_type i = 0; i < lst.length (); i++)
            bind_ans (lst(i), print);
        }
      else
        {
          symbol_table::force_assign (ans, val);

          if (print)
            val.print_with_name (octave_stdout, ans);
        }
    }
}

void
bind_internal_variable (const std::string& fname, const octave_value& val)
{
  octave_value_list args;

  args(0) = val;

  feval (fname, args, 0);
}

void
mlock (void)
{
  octave_function *fcn = octave_call_stack::current ();

  if (fcn)
    fcn->lock ();
  else
    error ("mlock: invalid use outside a function");
}

void
munlock (const std::string& nm)
{
  octave_value val = symbol_table::find_function (nm);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
        fcn->unlock ();
    }
}

bool
mislocked (const std::string& nm)
{
  bool retval = false;

  octave_value val = symbol_table::find_function (nm);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
        retval = fcn->islocked ();
    }

  return retval;
}

DEFUN (mlock, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mlock ()\n\
Lock the current function into memory so that it can't be cleared.\n\
@seealso{munlock, mislocked, persistent}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 0)
    {
      octave_function *fcn = octave_call_stack::caller ();

      if (fcn)
        fcn->lock ();
      else
        error ("mlock: invalid use outside a function");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (munlock, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} munlock ()\n\
@deftypefnx {Built-in Function} {} munlock (@var{fcn})\n\
Unlock the named function @var{fcn}.\n\
\n\
If no function is named then unlock the current function.\n\
@seealso{mlock, mislocked, persistent}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();
          munlock (name);
        }
      else
        error ("munlock: FCN must be a string");
    }
  else if (args.length () == 0)
    {
      octave_function *fcn = octave_call_stack::caller ();

      if (fcn)
        fcn->unlock ();
      else
        error ("munlock: invalid use outside a function");
    }
  else
    print_usage ();

  return retval;
}


DEFUN (mislocked, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} mislocked ()\n\
@deftypefnx {Built-in Function} {} mislocked (@var{fcn})\n\
Return true if the named function @var{fcn} is locked.\n\
\n\
If no function is named then return true if the current function is locked.\n\
@seealso{mlock, munlock, persistent}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();
          retval = mislocked (name);
        }
      else
        error ("mislocked: FCN must be a string");
    }
  else if (args.length () == 0)
    {
      octave_function *fcn = octave_call_stack::caller ();

      if (fcn)
        retval = fcn->islocked ();
      else
        error ("mislocked: invalid use outside a function");
    }
  else
    print_usage ();

  return retval;
}

// Deleting names from the symbol tables.

static inline bool
name_matches_any_pattern (const std::string& nm, const string_vector& argv,
                          int argc, int idx, bool have_regexp = false)
{
  bool retval = false;

  for (int k = idx; k < argc; k++)
    {
      std::string patstr = argv[k];
      if (! patstr.empty ())
        {
          if (have_regexp)
            {
              if (is_regexp_match (patstr, nm))
                {
                  retval = true;
                  break;
                }
            }
          else
            {
              glob_match pattern (patstr);

              if (pattern.match (nm))
                {
                  retval = true;
                  break;
                }
            }
        }
    }

  return retval;
}

static inline void
maybe_warn_exclusive (bool exclusive)
{
  if (exclusive)
    warning ("clear: ignoring --exclusive option");
}

static void
do_clear_functions (const string_vector& argv, int argc, int idx,
                    bool exclusive = false)
{
  if (idx == argc)
    symbol_table::clear_functions ();
  else
    {
      if (exclusive)
        {
          string_vector fcns = symbol_table::user_function_names ();

          int fcount = fcns.length ();

          for (int i = 0; i < fcount; i++)
            {
              std::string nm = fcns[i];

              if (! name_matches_any_pattern (nm, argv, argc, idx))
                symbol_table::clear_function (nm);
            }
        }
      else
        {
          while (idx < argc)
            symbol_table::clear_function_pattern (argv[idx++]);
        }
    }
}

static void
do_clear_globals (const string_vector& argv, int argc, int idx,
                  bool exclusive = false)
{
  if (idx == argc)
    {
      string_vector gvars = symbol_table::global_variable_names ();

      int gcount = gvars.length ();

      for (int i = 0; i < gcount; i++)
        symbol_table::clear_global (gvars[i]);
    }
  else
    {
      if (exclusive)
        {
          string_vector gvars = symbol_table::global_variable_names ();

          int gcount = gvars.length ();

          for (int i = 0; i < gcount; i++)
            {
              std::string nm = gvars[i];

              if (! name_matches_any_pattern (nm, argv, argc, idx))
                symbol_table::clear_global (nm);
            }
        }
      else
        {
          while (idx < argc)
            symbol_table::clear_global_pattern (argv[idx++]);
        }
    }
}

static void
do_clear_variables (const string_vector& argv, int argc, int idx,
                    bool exclusive = false, bool have_regexp = false)
{
  if (idx == argc)
    symbol_table::clear_variables ();
  else
    {
      if (exclusive)
        {
          string_vector lvars = symbol_table::variable_names ();

          int lcount = lvars.length ();

          for (int i = 0; i < lcount; i++)
            {
              std::string nm = lvars[i];

              if (! name_matches_any_pattern (nm, argv, argc, idx, have_regexp))
                symbol_table::clear_variable (nm);
            }
        }
      else
        {
          if (have_regexp)
            while (idx < argc)
              symbol_table::clear_variable_regexp (argv[idx++]);
          else
            while (idx < argc)
              symbol_table::clear_variable_pattern (argv[idx++]);
        }
    }
}

static void
do_clear_symbols (const string_vector& argv, int argc, int idx,
                  bool exclusive = false)
{
  if (idx == argc)
    symbol_table::clear_variables ();
  else
    {
      if (exclusive)
        {
          // FIXME: is this really what we want, or do we
          // somehow want to only clear the functions that are not
          // shadowed by local variables?  It seems that would be a
          // bit harder to do.

          do_clear_variables (argv, argc, idx, exclusive);
          do_clear_functions (argv, argc, idx, exclusive);
        }
      else
        {
          while (idx < argc)
            symbol_table::clear_symbol_pattern (argv[idx++]);
        }
    }
}

static void
do_matlab_compatible_clear (const string_vector& argv, int argc, int idx)
{
  // This is supposed to be mostly Matlab compatible.

  for (; idx < argc; idx++)
    {
      if (argv[idx] == "all"
          && ! symbol_table::is_local_variable ("all"))
        {
          symbol_table::clear_all ();
        }
      else if (argv[idx] == "functions"
               && ! symbol_table::is_local_variable ("functions"))
        {
          do_clear_functions (argv, argc, ++idx);
        }
      else if (argv[idx] == "global"
               && ! symbol_table::is_local_variable ("global"))
        {
          do_clear_globals (argv, argc, ++idx);
        }
      else if (argv[idx] == "variables"
               && ! symbol_table::is_local_variable ("variables"))
        {
          symbol_table::clear_variables ();
        }
      else if (argv[idx] == "classes"
               && ! symbol_table::is_local_variable ("classes"))
        {
          symbol_table::clear_objects ();
          octave_class::clear_exemplar_map ();
          symbol_table::clear_all ();
        }
      else
        {
          symbol_table::clear_symbol_pattern (argv[idx]);
        }
    }
}

#define CLEAR_OPTION_ERROR(cond) \
  do \
    { \
      if (cond) \
        { \
          print_usage (); \
          return retval; \
        } \
    } \
  while (0)

DEFUN (clear, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Command} {} clear [options] pattern @dots{}\n\
Delete the names matching the given patterns from the symbol table.\n\
\n\
The pattern may contain the following special characters:\n\
\n\
@table @code\n\
@item ?\n\
Match any single character.\n\
\n\
@item *\n\
Match zero or more characters.\n\
\n\
@item [ @var{list} ]\n\
Match the list of characters specified by @var{list}.  If the first\n\
character is @code{!} or @code{^}, match all characters except those\n\
specified by @var{list}.  For example, the pattern @samp{[a-zA-Z]} will\n\
match all lowercase and uppercase alphabetic characters.\n\
@end table\n\
\n\
For example, the command\n\
\n\
@example\n\
clear foo b*r\n\
@end example\n\
\n\
@noindent\n\
clears the name @code{foo} and all names that begin with the letter\n\
@code{b} and end with the letter @code{r}.\n\
\n\
If @code{clear} is called without any arguments, all user-defined\n\
variables (local and global) are cleared from the symbol table.\n\
\n\
If @code{clear} is called with at least one argument, only the visible\n\
names matching the arguments are cleared.  For example, suppose you have\n\
defined a function @code{foo}, and then hidden it by performing the\n\
assignment @code{foo = 2}.  Executing the command @kbd{clear foo} once\n\
will clear the variable definition and restore the definition of\n\
@code{foo} as a function.  Executing @kbd{clear foo} a second time will\n\
clear the function definition.\n\
\n\
The following options are available in both long and short form\n\
\n\
@table @code\n\
@item -all, -a\n\
Clear all local and global user-defined variables and all functions from the\n\
symbol table.\n\
\n\
@item -exclusive, -x\n\
Clear the variables that don't match the following pattern.\n\
\n\
@item -functions, -f\n\
Clear the function names and the built-in symbols names.\n\
\n\
@item -global, -g\n\
Clear global symbol names.\n\
\n\
@item -variables, -v\n\
Clear local variable names.\n\
\n\
@item -classes, -c\n\
Clears the class structure table and clears all objects.\n\
\n\
@item -regexp, -r\n\
The arguments are treated as regular expressions as any variables that\n\
match will be cleared.\n\
@end table\n\
\n\
With the exception of @code{exclusive}, all long options can be used\n\
without the dash as well.\n\
@seealso{who, whos, exist}\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("clear");

  if (! error_state)
    {
      if (argc == 1)
        {
          do_clear_globals (argv, argc, true);
          do_clear_variables (argv, argc, true);

          octave_link::clear_workspace ();
        }
      else
        {
          int idx = 0;

          bool clear_all = false;
          bool clear_functions = false;
          bool clear_globals = false;
          bool clear_variables = false;
          bool clear_objects = false;
          bool exclusive = false;
          bool have_regexp = false;
          bool have_dash_option = false;

          while (++idx < argc)
            {
              if (argv[idx] == "-all" || argv[idx] == "-a")
                {
                  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

                  have_dash_option = true;
                  clear_all = true;
                }
              else if (argv[idx] == "-exclusive" || argv[idx] == "-x")
                {
                  have_dash_option = true;
                  exclusive = true;
                }
              else if (argv[idx] == "-functions" || argv[idx] == "-f")
                {
                  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

                  have_dash_option = true;
                  clear_functions = true;
                }
              else if (argv[idx] == "-global" || argv[idx] == "-g")
                {
                  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

                  have_dash_option = true;
                  clear_globals = true;
                }
              else if (argv[idx] == "-variables" || argv[idx] == "-v")
                {
                  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

                  have_dash_option = true;
                  clear_variables = true;
                }
              else if (argv[idx] == "-classes" || argv[idx] == "-c")
                {
                  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

                  have_dash_option = true;
                  clear_objects = true;
                }
              else if (argv[idx] == "-regexp" || argv[idx] == "-r")
                {
                  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

                  have_dash_option = true;
                  have_regexp = true;
                }
              else
                break;
            }

          if (idx <= argc)
            {
              if (! have_dash_option)
                {
                  do_matlab_compatible_clear (argv, argc, idx);
                }
              else
                {
                  if (clear_all)
                    {
                      maybe_warn_exclusive (exclusive);

                      if (++idx < argc)
                        warning
                          ("clear: ignoring extra arguments after -all");

                      symbol_table::clear_all ();
                    }
                  else if (have_regexp)
                    {
                      do_clear_variables (argv, argc, idx, exclusive, true);
                    }
                  else if (clear_functions)
                    {
                      do_clear_functions (argv, argc, idx, exclusive);
                    }
                  else if (clear_globals)
                    {
                      do_clear_globals (argv, argc, idx, exclusive);
                    }
                  else if (clear_variables)
                    {
                      do_clear_variables (argv, argc, idx, exclusive);
                    }
                  else if (clear_objects)
                    {
                      symbol_table::clear_objects ();
                      octave_class::clear_exemplar_map ();
                      symbol_table::clear_all ();
                    }
                  else
                    {
                      do_clear_symbols (argv, argc, idx, exclusive);
                    }
                }

              octave_link::set_workspace ();
            }
        }
    }

  return retval;
}

DEFUN (whos_line_format, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} whos_line_format ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} whos_line_format (@var{new_val})\n\
@deftypefnx {Built-in Function} {} whos_line_format (@var{new_val}, \"local\")\n\
Query or set the format string used by the command @code{whos}.\n\
\n\
A full format string is:\n\
@c Set example in small font to prevent overfull line\n\
\n\
@smallexample\n\
%[modifier]<command>[:width[:left-min[:balance]]];\n\
@end smallexample\n\
\n\
The following command sequences are available:\n\
\n\
@table @code\n\
@item %a\n\
Prints attributes of variables (g=global, p=persistent, f=formal parameter,\n\
a=automatic variable).\n\
\n\
@item %b\n\
Prints number of bytes occupied by variables.\n\
\n\
@item %c\n\
Prints class names of variables.\n\
\n\
@item %e\n\
Prints elements held by variables.\n\
\n\
@item %n\n\
Prints variable names.\n\
\n\
@item %s\n\
Prints dimensions of variables.\n\
\n\
@item %t\n\
Prints type names of variables.\n\
@end table\n\
\n\
Every command may also have an alignment modifier:\n\
\n\
@table @code\n\
@item l\n\
Left alignment.\n\
\n\
@item r\n\
Right alignment (default).\n\
\n\
@item c\n\
Column-aligned (only applicable to command %s).\n\
@end table\n\
\n\
The @code{width} parameter is a positive integer specifying the minimum\n\
number of columns used for printing.  No maximum is needed as the field will\n\
auto-expand as required.\n\
\n\
The parameters @code{left-min} and @code{balance} are only available when the\n\
column-aligned modifier is used with the command @samp{%s}.\n\
@code{balance} specifies the column number within the field width which will\n\
be aligned between entries.  Numbering starts from 0 which indicates the\n\
leftmost column.  @code{left-min} specifies the minimum field width to the\n\
left of the specified balance column.\n\
\n\
The default format is:\n\
\n\
@qcode{\"  %a:4; %ln:6; %cs:16:6:1;  %rb:12;  %lc:-1;@xbackslashchar{}n\"}\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{whos}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (whos_line_format);
}

static std::string Vmissing_function_hook = "__unimplemented__";

DEFUN (missing_function_hook, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} missing_function_hook ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} missing_function_hook (@var{new_val})\n\
@deftypefnx {Built-in Function} {} missing_function_hook (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the function to call when\n\
an unknown identifier is requested.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{missing_component_hook}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (missing_function_hook);
}

void maybe_missing_function_hook (const std::string& name)
{
  // Don't do this if we're handling errors.
  if (buffer_error_messages == 0 && ! Vmissing_function_hook.empty ())
    {
      octave_value val = symbol_table::find_function (Vmissing_function_hook);

      if (val.is_defined ())
        {
          // Ensure auto-restoration.
          unwind_protect frame;
          frame.protect_var (Vmissing_function_hook);

          // Clear the variable prior to calling the function.
          const std::string func_name = Vmissing_function_hook;
          Vmissing_function_hook.clear ();

          // Call.
          feval (func_name, octave_value (name));
        }
    }
}

DEFUN (__varval__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __varval__ (@var{name})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        retval = symbol_table::varval (args(0).string_value ());
      else
        error ("__varval__: expecting argument to be variable name");
    }
  else
    print_usage ();

  return retval;
}

static std::string Vmissing_component_hook;

DEFUN (missing_component_hook, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} missing_component_hook ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} missing_component_hook (@var{new_val})\n\
@deftypefnx {Built-in Function} {} missing_component_hook (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the function to call when\n\
a component of Octave is missing.\n\
\n\
This can be useful for packagers that may split the Octave installation into\n\
multiple sub-packages, for example, to provide a hint to users for how to\n\
install the missing components.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
\n\
The hook function is expected to be of the form\n\
\n\
@example\n\
@var{fcn} (@var{component})\n\
@end example\n\
\n\
Octave will call @var{fcn} with the name of the function that requires the\n\
component and a string describing the missing component.  The hook function\n\
should return an error message to be displayed.\n\
@seealso{missing_function_hook}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (missing_component_hook);
}
