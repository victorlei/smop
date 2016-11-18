/*

Copyright (C) 1996-2015 John W. Eaton

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

#include <sstream>

#include "str-vec.h"

#include <defaults.h>
#include "Cell.h"
#include "builtins.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-obj.h"
#include "ov-usr-fcn.h"
#include "ov.h"
#include "pager.h"
#include "pt-eval.h"
#include "pt-jit.h"
#include "pt-jump.h"
#include "pt-misc.h"
#include "pt-pr-code.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "symtab.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "parse.h"
#include "profiler.h"
#include "variables.h"
#include "ov-fcn-handle.h"

// Whether to optimize subsasgn method calls.
static bool Voptimize_subsasgn_calls = true;

// The character to fill with when creating string arrays.
extern char Vstring_fill_char;   // see pt-mat.cc

std::map<std::string, octave_value>
octave_user_code::subfunctions (void) const
{
  return std::map<std::string, octave_value> ();
}

// User defined scripts.


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_user_script,
                                     "user-defined script",
                                     "user-defined script");

octave_user_script::octave_user_script (void)
  : octave_user_code (), cmd_list (0), file_name (),
    t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    call_depth (-1)
{ }

octave_user_script::octave_user_script (const std::string& fnm,
                                        const std::string& nm,
                                        tree_statement_list *cmds,
                                        const std::string& ds)
  : octave_user_code (nm, ds), cmd_list (cmds), file_name (fnm),
    t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    call_depth (-1)
{
  if (cmd_list)
    cmd_list->mark_as_script_body ();
}

octave_user_script::octave_user_script (const std::string& fnm,
                                        const std::string& nm,
                                        const std::string& ds)
  : octave_user_code (nm, ds), cmd_list (0), file_name (fnm),
    t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    call_depth (-1)
{ }

octave_user_script::~octave_user_script (void)
{
  if (cmd_list)
    cmd_list->remove_all_breakpoints (file_name);

  delete cmd_list;
}

octave_value_list
octave_user_script::subsref (const std::string&,
                             const std::list<octave_value_list>&, int)
{
  octave_value_list retval;

  ::error ("invalid use of script %s in index expression", file_name.c_str ());

  return retval;
}

octave_value_list
octave_user_script::do_multi_index_op (int nargout,
                                       const octave_value_list& args)
{
  octave_value_list retval;

  unwind_protect frame;

  if (! error_state)
    {
      if (args.length () == 0 && nargout == 0)
        {
          if (cmd_list)
            {
              frame.protect_var (call_depth);
              call_depth++;

              if (call_depth < Vmax_recursion_depth)
                {
                  octave_call_stack::push (this);

                  frame.add_fcn (octave_call_stack::pop);

                  frame.protect_var (tree_evaluator::statement_context);
                  tree_evaluator::statement_context = tree_evaluator::script;

                  BEGIN_PROFILER_BLOCK (octave_user_script)

                  cmd_list->accept (*current_evaluator);

                  END_PROFILER_BLOCK

                  if (tree_return_command::returning)
                    tree_return_command::returning = 0;

                  if (tree_break_command::breaking)
                    tree_break_command::breaking--;
                }
              else
                ::error ("max_recursion_depth exceeded");
            }
        }
      else
        error ("invalid call to script %s", file_name.c_str ());
    }

  return retval;
}

void
octave_user_script::accept (tree_walker& tw)
{
  tw.visit_octave_user_script (*this);
}

// User defined functions.


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_user_function,
                                     "user-defined function",
                                     "user-defined function");

// Ugh.  This really needs to be simplified (code/data?
// extrinsic/intrinsic state?).

octave_user_function::octave_user_function
  (symbol_table::scope_id sid, tree_parameter_list *pl,
   tree_parameter_list *rl, tree_statement_list *cl)
  : octave_user_code (std::string (), std::string ()),
    param_list (pl), ret_list (rl), cmd_list (cl),
    lead_comm (), trail_comm (), file_name (),
    location_line (0), location_column (0),
    parent_name (), t_parsed (static_cast<time_t> (0)),
    t_checked (static_cast<time_t> (0)),
    system_fcn_file (false), call_depth (-1),
    num_named_args (param_list ? param_list->length () : 0),
    subfunction (false), inline_function (false),
    anonymous_function (false), nested_function (false),
    class_constructor (none), class_method (false),
    parent_scope (-1), local_scope (sid),
    curr_unwind_protect_frame (0)
#ifdef HAVE_LLVM
    , jit_info (0)
#endif
{
  if (cmd_list)
    cmd_list->mark_as_function_body ();

  if (local_scope >= 0)
    symbol_table::set_curr_fcn (this, local_scope);
}

octave_user_function::~octave_user_function (void)
{
  if (cmd_list)
    cmd_list->remove_all_breakpoints (file_name);

  delete param_list;
  delete ret_list;
  delete cmd_list;
  delete lead_comm;
  delete trail_comm;

#ifdef HAVE_LLVM
  delete jit_info;
#endif

  // FIXME: this is really playing with fire.
  symbol_table::erase_scope (local_scope);
}

octave_user_function *
octave_user_function::define_ret_list (tree_parameter_list *t)
{
  ret_list = t;

  return this;
}

void
octave_user_function::stash_fcn_file_name (const std::string& nm)
{
  file_name = nm;
}

// If there is no explicit end statement at the end of the function,
// relocate the no_op that was generated for the end of file condition
// to appear on the next line after the last statement in the file, or
// the next line after the function keyword if there are no statements.
// More precisely, the new location should probably be on the next line
// after the end of the parameter list, but we aren't tracking that
// information (yet).

void
octave_user_function::maybe_relocate_end_internal (void)
{
  if (cmd_list && ! cmd_list->empty ())
    {
      tree_statement *last_stmt = cmd_list->back ();

      if (last_stmt && last_stmt->is_end_of_fcn_or_script ()
          && last_stmt->is_end_of_file ())
        {
          tree_statement_list::reverse_iterator
            next_to_last_elt = cmd_list->rbegin ();

          next_to_last_elt++;

          int new_eof_line;
          int new_eof_col;

          if (next_to_last_elt == cmd_list->rend ())
            {
              new_eof_line = beginning_line ();
              new_eof_col = beginning_column ();
            }
          else
            {
              tree_statement *next_to_last_stmt = *next_to_last_elt;

              new_eof_line = next_to_last_stmt->line ();
              new_eof_col = next_to_last_stmt->column ();
            }

          last_stmt->set_location (new_eof_line + 1, new_eof_col);
        }
    }
}

void
octave_user_function::maybe_relocate_end (void)
{
  std::map<std::string, octave_value> fcns = subfunctions ();

  if (! fcns.empty ())
    {
      for (std::map<std::string, octave_value>::iterator p = fcns.begin ();
           p != fcns.end (); p++)
        {
          octave_user_function *f = (p->second).user_function_value ();

          if (f)
            f->maybe_relocate_end_internal ();
        }
    }

  maybe_relocate_end_internal ();
}

std::string
octave_user_function::profiler_name (void) const
{
  std::ostringstream result;

  if (is_anonymous_function ())
    result << "anonymous@" << fcn_file_name ()
           << ":" << location_line << ":" << location_column;
  else if (is_subfunction ())
    result << parent_fcn_name () << ">" << name ();
  else if (is_class_method ())
    result << "@" << dispatch_class () << "/" << name ();
  else if (is_class_constructor () || is_classdef_constructor ())
    result << "@" << name ();
  else if (is_inline_function ())
    result << "inline@" << fcn_file_name ()
           << ":" << location_line << ":" << location_column;
  else
    result << name ();

  return result.str ();
}

void
octave_user_function::mark_as_system_fcn_file (void)
{
  if (! file_name.empty ())
    {
      // We really should stash the whole path to the file we found,
      // when we looked it up, to avoid possible race conditions...
      // FIXME
      //
      // We probably also don't need to get the library directory
      // every time, but since this function is only called when the
      // function file is parsed, it probably doesn't matter that
      // much.

      std::string ff_name = fcn_file_in_path (file_name);

      if (Vfcn_file_dir == ff_name.substr (0, Vfcn_file_dir.length ()))
        system_fcn_file = true;
    }
  else
    system_fcn_file = false;
}

bool
octave_user_function::takes_varargs (void) const
{
  return (param_list && param_list->takes_varargs ());
}

bool
octave_user_function::takes_var_return (void) const
{
  return (ret_list && ret_list->takes_varargs ());
}

void
octave_user_function::lock_subfunctions (void)
{
  symbol_table::lock_subfunctions (local_scope);
}

void
octave_user_function::unlock_subfunctions (void)
{
  symbol_table::unlock_subfunctions (local_scope);
}

std::map<std::string, octave_value>
octave_user_function::subfunctions (void) const
{
  return symbol_table::subfunctions_defined_in_scope (local_scope);
}

bool
octave_user_function::has_subfunctions (void) const
{
  return ! subfcn_names.empty ();
}

void
octave_user_function::stash_subfunction_names
  (const std::list<std::string>& names)
{
  subfcn_names = names;
}

octave_value_list
octave_user_function::all_va_args (const octave_value_list& args)
{
  octave_value_list retval;

  octave_idx_type n = args.length () - num_named_args;

  if (n > 0)
    retval = args.slice (num_named_args, n);

  return retval;
}

octave_value_list
octave_user_function::subsref (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               int nargout)
{
  return octave_user_function::subsref (type, idx, nargout, 0);
}

octave_value_list
octave_user_function::subsref (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               int nargout,
                               const std::list<octave_lvalue>* lvalue_list)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
        int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

        retval = do_multi_index_op (tmp_nargout, idx.front (),
                                    idx.size () == 1 ? lvalue_list : 0);
      }
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

octave_value_list
octave_user_function::do_multi_index_op (int nargout,
                                         const octave_value_list& args)
{
  return do_multi_index_op (nargout, args, 0);
}

octave_value_list
octave_user_function::do_multi_index_op (int nargout,
                                         const octave_value_list& _args,
                                         const std::list<octave_lvalue>* lvalue_list)
{
  octave_value_list retval;

  if (error_state)
    return retval;

  if (! cmd_list)
    return retval;

  // If this function is a classdef constructor, extract the first input
  // argument, which must be the partially constructed object instance.

  octave_value_list args (_args);
  octave_value_list ret_args;

  if (is_classdef_constructor ())
    {
      if (args.length () > 0)
        {
          ret_args = args.slice (0, 1, true);
          args = args.slice (1, args.length () - 1, true);
        }
      else
        panic_impossible ();
    }

#ifdef HAVE_LLVM
  if (is_special_expr ()
      && tree_jit::execute (*this, args, retval))
    return retval;
#endif

  int nargin = args.length ();

  unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth >= Vmax_recursion_depth)
    {
      ::error ("max_recursion_depth exceeded");
      return retval;
    }

  // Save old and set current symbol table context, for
  // eval_undefined_error().

  int context = active_context ();

  octave_call_stack::push (this, local_scope, context);
  frame.add_fcn (octave_call_stack::pop);

  if (call_depth > 0 && ! is_anonymous_function ())
    {
      symbol_table::push_context ();

      frame.add_fcn (symbol_table::pop_context);
    }

  string_vector arg_names = args.name_tags ();

  if (param_list && ! param_list->varargs_only ())
    {
      param_list->define_from_arg_vector (args);
      if (error_state)
        return retval;
    }

  // For classdef constructor, pre-populate the output arguments
  // with the pre-initialized object instance, extracted above.

  if (is_classdef_constructor ())
    {
      if (ret_list)
        {
          ret_list->define_from_arg_vector (ret_args);
          if (error_state)
            return retval;
        }
      else
        {
          ::error ("%s: invalid classdef constructor, no output argument defined",
                   dispatch_class ().c_str ());
          return retval;
        }
    }

  // Force parameter list to be undefined when this function exits.
  // Doing so decrements the reference counts on the values of local
  // variables that are also named function parameters.

  if (param_list)
    frame.add_method (param_list, &tree_parameter_list::undefine);

  // Force return list to be undefined when this function exits.
  // Doing so decrements the reference counts on the values of local
  // variables that are also named values returned by this function.

  if (ret_list)
    frame.add_method (ret_list, &tree_parameter_list::undefine);

  if (call_depth == 0)
    {
      // Force symbols to be undefined again when this function
      // exits.
      //
      // This cleanup function is added to the unwind_protect stack
      // after the calls to clear the parameter lists so that local
      // variables will be cleared before the parameter lists are
      // cleared.  That way, any function parameters that have been
      // declared global will be unmarked as global before they are
      // undefined by the clear_param_list cleanup function.

      frame.add_fcn (symbol_table::clear_variables);
    }

  bind_automatic_vars (arg_names, nargin, nargout, all_va_args (args),
                       lvalue_list);

  frame.add_method (this, &octave_user_function::restore_warning_states);

  bool echo_commands = (Vecho_executing_commands & ECHO_FUNCTIONS);

  if (echo_commands)
    print_code_function_header ();

  // Set pointer to the current unwind_protect frame to allow
  // certain builtins register simple cleanup in a very optimized manner.
  // This is *not* intended as a general-purpose on-cleanup mechanism,
  frame.protect_var (curr_unwind_protect_frame);
  curr_unwind_protect_frame = &frame;

  // Evaluate the commands that make up the function.

  frame.protect_var (tree_evaluator::statement_context);
  tree_evaluator::statement_context = tree_evaluator::function;

  BEGIN_PROFILER_BLOCK (octave_user_function)

  if (is_special_expr ())
    {
      tree_expression *expr = special_expr ();

      if (expr)
        retval = (lvalue_list
                  ? expr->rvalue (nargout, lvalue_list)
                  : expr->rvalue (nargout));
    }
  else
    cmd_list->accept (*current_evaluator);

  END_PROFILER_BLOCK

  if (echo_commands)
    print_code_function_trailer ();

  if (tree_return_command::returning)
    tree_return_command::returning = 0;

  if (tree_break_command::breaking)
    tree_break_command::breaking--;

  if (error_state)
    return retval;

  // Copy return values out.

  if (ret_list && ! is_special_expr ())
    {
      ret_list->initialize_undefined_elements (my_name, nargout, Matrix ());

      Cell varargout;

      if (ret_list->takes_varargs ())
        {
          octave_value varargout_varval = symbol_table::varval ("varargout");

          if (varargout_varval.is_defined ())
            {
              varargout = varargout_varval.cell_value ();

              if (error_state)
                error ("expecting varargout to be a cell array object");
            }
        }

      if (! error_state)
        retval = ret_list->convert_to_const_vector (nargout, varargout);
    }

  return retval;
}

void
octave_user_function::accept (tree_walker& tw)
{
  tw.visit_octave_user_function (*this);
}

tree_expression *
octave_user_function::special_expr (void)
{
  assert (is_special_expr ());
  assert (cmd_list->length () == 1);

  tree_statement *stmt = cmd_list->front ();
  return stmt->expression ();
}

bool
octave_user_function::subsasgn_optimization_ok (void)
{
  bool retval = false;
  if (Voptimize_subsasgn_calls
      && param_list && ret_list
      && param_list->length () > 0 && ! param_list->varargs_only ()
      && ret_list->length () == 1 && ! ret_list->takes_varargs ())
    {
      tree_identifier *par1 = param_list->front ()->ident ();
      tree_identifier *ret1 = ret_list->front ()->ident ();
      retval = par1->name () == ret1->name ();
    }

  return retval;
}

#if 0
void
octave_user_function::print_symtab_info (std::ostream& os) const
{
  symbol_table::print_info (os, local_scope);
}
#endif

void
octave_user_function::print_code_function_header (void)
{
  tree_print_code tpc (octave_stdout, VPS4);

  tpc.visit_octave_user_function_header (*this);
}

void
octave_user_function::print_code_function_trailer (void)
{
  tree_print_code tpc (octave_stdout, VPS4);

  tpc.visit_octave_user_function_trailer (*this);
}

void
octave_user_function::bind_automatic_vars
  (const string_vector& arg_names, int nargin, int nargout,
   const octave_value_list& va_args,
   const std::list<octave_lvalue> *lvalue_list)
{
  if (! arg_names.empty ())
    {
      // It is better to save this in the hidden variable .argn. and
      // then use that in the inputname function instead of using argn,
      // which might be redefined in a function.  Keep the old argn name
      // for backward compatibility of functions that use it directly.

      symbol_table::force_assign ("argn",
                                  charMatrix (arg_names, Vstring_fill_char));
      symbol_table::force_assign (".argn.", Cell (arg_names));

      symbol_table::mark_hidden (".argn.");

      symbol_table::mark_automatic ("argn");
      symbol_table::mark_automatic (".argn.");
    }

  symbol_table::force_assign (".nargin.", nargin);
  symbol_table::force_assign (".nargout.", nargout);

  symbol_table::mark_hidden (".nargin.");
  symbol_table::mark_hidden (".nargout.");

  symbol_table::mark_automatic (".nargin.");
  symbol_table::mark_automatic (".nargout.");

  symbol_table::assign (".saved_warning_states.");

  symbol_table::mark_automatic (".saved_warning_states.");
  symbol_table::mark_automatic (".saved_warning_states.");

  if (takes_varargs ())
    symbol_table::assign ("varargin", va_args.cell_value ());

  // Force .ignored. variable to be undefined by default.
  symbol_table::assign (".ignored.");

  if (lvalue_list)
    {
      octave_idx_type nbh = 0;
      for (std::list<octave_lvalue>::const_iterator p = lvalue_list->begin ();
           p != lvalue_list->end (); p++)
        nbh += p->is_black_hole ();

      if (nbh > 0)
        {
          // Only assign the hidden variable if black holes actually present.
          Matrix bh (1, nbh);
          octave_idx_type k = 0;
          octave_idx_type l = 0;
          for (std::list<octave_lvalue>::const_iterator
               p = lvalue_list->begin (); p != lvalue_list->end (); p++)
            {
              if (p->is_black_hole ())
                bh(l++) = k+1;
              k += p->numel ();
            }

          symbol_table::assign (".ignored.", bh);
        }
    }

  symbol_table::mark_hidden (".ignored.");
  symbol_table::mark_automatic (".ignored.");
}

void
octave_user_function::restore_warning_states (void)
{
  octave_value val = symbol_table::varval (".saved_warning_states.");

  if (val.is_defined ())
    {
      // Don't use the usual approach of attempting to extract a value
      // and then checking error_state since this code might be
      // executing when error_state is already set.  But do fail
      // spectacularly if .saved_warning_states. is not an octave_map
      // (or octave_scalar_map) object.

      if (! val.is_map ())
        panic_impossible ();

      octave_map m = val.map_value ();

      Cell ids = m.contents ("identifier");
      Cell states = m.contents ("state");

      for (octave_idx_type i = 0; i < m.numel (); i++)
        Fwarning (ovl (states(i), ids(i)));
    }
}

DEFUN (nargin, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} nargin ()\n\
@deftypefnx {Built-in Function} {} nargin (@var{fcn})\n\
Report the number of input arguments to a function.\n\
\n\
Called from within a function, return the number of arguments passed to the\n\
function.  At the top level, return the number of command line arguments\n\
passed to Octave.\n\
\n\
If called with the optional argument @var{fcn}---a function name or handle---\n\
return the declared number of arguments that the function can accept.\n\
\n\
If the last argument to @var{fcn} is @var{varargin} the returned value is\n\
negative.  For example, the function @code{union} for sets is declared as\n\
\n\
@example\n\
@group\n\
function [y, ia, ib] = union (a, b, varargin)\n\
\n\
and\n\
\n\
nargin (\"union\")\n\
@result{} -3\n\
@end group\n\
@end example\n\
\n\
Programming Note: @code{nargin} does not work on built-in functions.\n\
@seealso{nargout, narginchk, varargin, inputname}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value func = args(0);

      if (func.is_string ())
        {
          std::string name = func.string_value ();
          func = symbol_table::find_function (name);
          if (func.is_undefined ())
            {
              error ("nargin: invalid function name: %s", name.c_str ());
              return retval;
            }
        }

      octave_function *fcn_val = func.function_value ();
      if (fcn_val)
        {
          octave_user_function *fcn = fcn_val->user_function_value (true);

          if (fcn)
            {
              tree_parameter_list *param_list = fcn->parameter_list ();

              retval = param_list ? param_list->length () : 0;
              if (fcn->takes_varargs ())
                retval = -1 - retval;
            }
          else
            {
              // Matlab gives up for histc,
              // so maybe it's ok that that we give up somtimes too?
              error ("nargin: nargin information not available for built-in functions");
            }
        }
      else
        error ("nargin: FCN must be a string or function handle");
    }
  else if (nargin == 0)
    {
      retval = symbol_table::varval (".nargin.");

      if (retval.is_undefined ())
        retval = 0;
    }
  else
    print_usage ();

  return retval;
}

DEFUN (nargout, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} nargout ()\n\
@deftypefnx {Built-in Function} {} nargout (@var{fcn})\n\
Report the number of output arguments from a function.\n\
\n\
Called from within a function, return the number of values the caller expects\n\
to receive.  At the top level, @code{nargout} with no argument is undefined\n\
and will produce an error.\n\
\n\
If called with the optional argument @var{fcn}---a function name or\n\
handle---return the number of declared output values that the function can\n\
produce.\n\
\n\
If the final output argument is @var{varargout} the returned value is\n\
negative.\n\
\n\
For example,\n\
\n\
@example\n\
f ()\n\
@end example\n\
\n\
@noindent\n\
will cause @code{nargout} to return 0 inside the function @code{f} and\n\
\n\
@example\n\
[s, t] = f ()\n\
@end example\n\
\n\
@noindent\n\
will cause @code{nargout} to return 2 inside the function @code{f}.\n\
\n\
In the second usage,\n\
\n\
@example\n\
nargout (@@histc) \% or nargout (\"histc\")\n\
@end example\n\
\n\
@noindent\n\
will return 2, because @code{histc} has two outputs, whereas\n\
\n\
@example\n\
nargout (@@imread)\n\
@end example\n\
\n\
@noindent\n\
will return -2, because @code{imread} has two outputs and the second is\n\
@var{varargout}.\n\
\n\
Programming Note.  @code{nargout} does not work for built-in functions and\n\
returns -1 for all anonymous functions.\n\
@seealso{nargin, varargout, isargout, nthargout}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value func = args(0);

      if (func.is_string ())
        {
          std::string name = func.string_value ();
          func = symbol_table::find_function (name);
          if (func.is_undefined ())
            {
              error ("nargout: invalid function name: %s", name.c_str ());
              return retval;
            }
        }

      if (func.is_inline_function ())
        {
          retval = 1;
          return retval;
        }

      if (func.is_function_handle ())
        {
          octave_fcn_handle *fh = func.fcn_handle_value ();
          std::string fh_nm = fh->fcn_name ();

          if (fh_nm == octave_fcn_handle::anonymous)
            {
              retval = -1;
              return retval;
            }
        }

      octave_function *fcn_val = func.function_value ();
      if (fcn_val)
        {
          octave_user_function *fcn = fcn_val->user_function_value (true);

          if (fcn)
            {
              tree_parameter_list *ret_list = fcn->return_list ();

              retval = ret_list ? ret_list->length () : 0;

              if (fcn->takes_var_return ())
                retval = -1 - retval;
            }
          else
            {
              // JWE said this information is not available (2011-03-10)
              // without making intrusive changes to Octave.
              // Matlab gives up for histc,
              // so maybe it's ok that we give up somtimes too?
              error ("nargout: nargout information not available for built-in functions.");
            }
        }
      else
        error ("nargout: FCN must be a string or function handle");
    }
  else if (nargin == 0)
    {
      if (! symbol_table::at_top_level ())
        {
          retval = symbol_table::varval (".nargout.");

          if (retval.is_undefined ())
            retval = 0;
        }
      else
        error ("nargout: invalid call at top level");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (optimize_subsasgn_calls, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} optimize_subsasgn_calls ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} optimize_subsasgn_calls (@var{new_val})\n\
@deftypefnx {Built-in Function} {} optimize_subsasgn_calls (@var{new_val}, \"local\")\n\
Query or set the internal flag for subsasgn method call optimizations.\n\
\n\
If true, Octave will attempt to eliminate the redundant copying when calling\n\
the subsasgn method of a user-defined class.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (optimize_subsasgn_calls);
}

static bool val_in_table (const Matrix& table, double val)
{
  if (table.is_empty ())
    return false;

  octave_idx_type i = table.lookup (val, ASCENDING);
  return (i > 0 && table(i-1) == val);
}

static bool isargout1 (int nargout, const Matrix& ignored, double k)
{
  if (k != xround (k) || k <= 0)
    {
      error ("isargout: K must be a positive integer");
      return false;
    }
  else
    return (k == 1 || k <= nargout) && ! val_in_table (ignored, k);
}

DEFUN (isargout, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isargout (@var{k})\n\
Within a function, return a logical value indicating whether the argument\n\
@var{k} will be assigned to a variable on output.\n\
\n\
If the result is false, the argument has been ignored during the function\n\
call through the use of the tilde (~) special output argument.  Functions\n\
can use @code{isargout} to avoid performing unnecessary calculations for\n\
outputs which are unwanted.\n\
\n\
If @var{k} is outside the range @code{1:max (nargout)}, the function returns\n\
false.  @var{k} can also be an array, in which case the function works\n\
element-by-element and a logical array is returned.  At the top level,\n\
@code{isargout} returns an error.\n\
@seealso{nargout, varargout, nthargout}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (! symbol_table::at_top_level ())
        {
          int nargout1 = symbol_table::varval (".nargout.").int_value ();
          if (error_state)
            {
              error ("isargout: internal error");
              return retval;
            }

          Matrix ignored;
          octave_value tmp = symbol_table::varval (".ignored.");
          if (tmp.is_defined ())
            ignored = tmp.matrix_value ();

          if (args(0).is_scalar_type ())
            {
              double k = args(0).double_value ();
              if (! error_state)
                retval = isargout1 (nargout1, ignored, k);
            }
          else if (args(0).is_numeric_type ())
            {
              const NDArray ka = args(0).array_value ();
              if (! error_state)
                {
                  boolNDArray r (ka.dims ());
                  for (octave_idx_type i = 0;
                       i < ka.numel () && ! error_state;
                       i++)
                    r(i) = isargout1 (nargout1, ignored, ka(i));

                  retval = r;
                }
            }
          else
            gripe_wrong_type_arg ("isargout", args(0));
        }
      else
        error ("isargout: invalid call at top level");
    }
  else
    print_usage ();

  return retval;
}

/*
%!function [x, y] = try_isargout ()
%!  if (isargout (1))
%!    if (isargout (2))
%!      x = 1; y = 2;
%!    else
%!      x = -1;
%!    endif
%!  else
%!    if (isargout (2))
%!      y = -2;
%!    else
%!      error ("no outputs requested");
%!    endif
%!  endif
%!endfunction
%!
%!test
%! [x, y] = try_isargout ();
%! assert ([x, y], [1, 2]);
%!
%!test
%! [x, ~] = try_isargout ();
%! assert (x, -1);
%!
%!test
%! [~, y] = try_isargout ();
%! assert (y, -2);
%!
%!error [~, ~] = try_isargout ();
%!
%% Check to see that isargout isn't sticky:
%!test
%! [x, y] = try_isargout ();
%! assert ([x, y], [1, 2]);
%!
%% It should work without ():
%!test
%! [~, y] = try_isargout;
%! assert (y, -2);
%!
%% It should work in function handles, anonymous functions, and cell
%% arrays of handles or anonymous functions.
%!test
%! fh = @try_isargout;
%! af = @() try_isargout;
%! c = {fh, af};
%! [~, y] = fh ();
%! assert (y, -2);
%! [~, y] = af ();
%! assert (y, -2);
%! [~, y] = c{1}();
%! assert (y, -2);
%! [~, y] = c{2}();
%! assert (y, -2);
*/
