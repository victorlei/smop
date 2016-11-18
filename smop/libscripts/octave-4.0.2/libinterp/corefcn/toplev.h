/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_toplev_h)
#define octave_toplev_h 1

#include <cstdio>

#include <deque>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;
class octave_user_script;
class tree_statement;
class tree_statement_list;
class charMatrix;

#include "quit.h"

#include "input.h"
#include "oct-map.h"
#include "symtab.h"


typedef void (*octave_exit_func) (int);
extern OCTINTERP_API octave_exit_func octave_exit;

extern OCTINTERP_API bool quit_allowed;

extern OCTINTERP_API bool quitting_gracefully;

extern OCTINTERP_API int exit_status;

extern OCTINTERP_API void
clean_up_and_exit (int status, bool safe_to_return = false);

extern OCTINTERP_API void recover_from_exception (void);

extern OCTINTERP_API int main_loop (void);

extern OCTINTERP_API void
octave_add_atexit_function (const std::string& fname);

extern OCTINTERP_API bool
octave_remove_atexit_function (const std::string& fname);

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

class
OCTINTERP_API
octave_call_stack
{
protected:

  octave_call_stack (void) : cs (), curr_frame (0) { }

public:

  class stack_frame
  {
  public:

    friend class octave_call_stack;

    stack_frame (octave_function *fcn = 0, symbol_table::scope_id scope = 0,
                 symbol_table::context_id context = 0, size_t prev = 0)
      : m_fcn (fcn), m_line (-1), m_column (-1), m_scope (scope),
        m_context (context), m_prev (prev)
    { }

    stack_frame (const stack_frame& elt)
      : m_fcn (elt.m_fcn), m_line (elt.m_line), m_column (elt.m_column),
        m_scope (elt.m_scope), m_context (elt.m_context), m_prev (elt.m_prev)
    { }

    int line (void) const { return m_line; }

    int column (void) const { return m_column; }

    std::string fcn_file_name (void) const;

    std::string fcn_name (bool print_subfn = true) const;

  private:

    octave_function *m_fcn;
    int m_line;
    int m_column;
    symbol_table::scope_id m_scope;
    symbol_table::context_id m_context;
    size_t m_prev;
  };

  typedef std::deque<stack_frame>::iterator iterator;
  typedef std::deque<stack_frame>::const_iterator const_iterator;

  typedef std::deque<stack_frame>::reverse_iterator reverse_iterator;
  typedef std::deque<stack_frame>::const_reverse_iterator const_reverse_iterator;

  static void create_instance (void);

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      create_instance ();

    if (! instance)
      {
        ::error ("unable to create call stack object!");

        retval = false;
      }

    return retval;
  }

  // Current function (top of stack).
  static octave_function *current (void)
  {
    return instance_ok () ? instance->do_current () : 0;
  }

  // Current line in current function.
  static int current_line (void)
  {
    return instance_ok () ? instance->do_current_line () : -1;
  }

  // Current column in current function.
  static int current_column (void)
  {
    return instance_ok () ? instance->do_current_column () : -1;
  }

  // Line in user code caller.
  static int caller_user_code_line (void)
  {
    return instance_ok () ? instance->do_caller_user_code_line () : -1;
  }

  // Column in user code caller.
  static int caller_user_code_column (void)
  {
    return instance_ok () ? instance->do_caller_user_code_column () : -1;
  }

  // Caller function, may be built-in.
  static octave_function *caller (void)
  {
    return instance_ok () ? instance->do_caller () : 0;
  }

  static size_t current_frame (void)
  {
    return instance_ok () ? instance->do_current_frame () : 0;
  }

  static size_t size (void)
  {
    return instance_ok () ? instance->do_size () : 0;
  }

  static size_t num_user_code_frames (octave_idx_type& curr_user_frame)
  {
    return instance_ok ()
           ? instance->do_num_user_code_frames (curr_user_frame) : 0;
  }

  static symbol_table::scope_id current_scope (void)
  {
    return instance_ok () ? instance->do_current_scope () : 0;
  }

  static symbol_table::context_id current_context (void)
  {
    return instance_ok () ? instance->do_current_context () : 0;
  }

  /*
  static stack_frame frame (size_t idx)
  {
    return instance_ok () ? instance->do_frame (idx) : stack_frame ();
  }
  */
  // Function at location N on the call stack (N == 0 is current), may
  // be built-in.
  static octave_function *element (size_t n)
  {
    return instance_ok () ? instance->do_element (n) : 0;
  }

  // First user-defined function on the stack.
  static octave_user_code *caller_user_code (size_t nskip = 0)
  {
    return instance_ok () ? instance->do_caller_user_code (nskip) : 0;
  }

  // Return TRUE if all elements on the call stack are scripts.
  static bool all_scripts (void)
  {
    return instance_ok () ? instance->do_all_scripts () : false;
  }

  static void
  push (octave_function *f,
        symbol_table::scope_id scope = symbol_table::current_scope (),
        symbol_table::context_id context = symbol_table::current_context ())
  {
    if (instance_ok ())
      instance->do_push (f, scope, context);
  }

  static void
  push (symbol_table::scope_id scope = symbol_table::current_scope (),
        symbol_table::context_id context = symbol_table::current_context ())
  {
    if (instance_ok ())
      instance->do_push (0, scope, context);
  }

  static void set_location (int l, int c)
  {
    if (instance_ok ())
      instance->do_set_location (l, c);
  }

  static void set_line (int l)
  {
    if (instance_ok ())
      instance->do_set_line (l);
  }

  static void set_column (int c)
  {
    if (instance_ok ())
      instance->do_set_column (c);
  }

  static bool goto_frame (size_t n = 0, bool verbose = false)
  {
    return instance_ok () ? instance->do_goto_frame (n, verbose) : false;
  }

  static void restore_frame (size_t n)
  {
    goto_frame (n);
  }

  static bool goto_frame_relative (int n, bool verbose = false)
  {
    return instance_ok ()
           ? instance->do_goto_frame_relative (n, verbose) : false;
  }

  static void goto_caller_frame (void)
  {
    if (instance_ok ())
      instance->do_goto_caller_frame ();
  }

  static void goto_base_frame (void)
  {
    if (instance_ok ())
      instance->do_goto_base_frame ();
  }

  static octave_map backtrace (size_t nskip = 0)
  {
    octave_idx_type curr_user_frame = -1;

    return instance_ok ()
           ? instance->do_backtrace (nskip, curr_user_frame, true)
           : octave_map ();
  }

  static octave_map backtrace (size_t nskip, octave_idx_type& curr_user_frame,
                               bool print_subfn = true)
  {
    return instance_ok ()
           ? instance->do_backtrace (nskip, curr_user_frame, print_subfn)
           : octave_map ();
  }

  static std::list<octave_call_stack::stack_frame>
  backtrace_frames (size_t nskip = 0)
  {
    octave_idx_type curr_user_frame = -1;

    return instance_ok ()
           ? instance->do_backtrace_frames (nskip, curr_user_frame)
           : std::list<octave_call_stack::stack_frame> ();
  }

  static std::list<octave_call_stack::stack_frame>
  backtrace_frames (size_t nskip, octave_idx_type& curr_user_frame)
  {
    return instance_ok ()
           ? instance->do_backtrace_frames (nskip, curr_user_frame)
           : std::list<octave_call_stack::stack_frame> ();
  }

  static octave_map empty_backtrace (void);

  static void pop (void)
  {
    if (instance_ok ())
      instance->do_pop ();
  }

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }

private:

  // The current call stack.
  std::deque<stack_frame> cs;

  size_t curr_frame;

  static octave_call_stack *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  int do_current_line (void) const;

  int do_current_column (void) const;

  int do_caller_user_code_line (void) const;

  int do_caller_user_code_column (void) const;

  octave_function *do_caller (void) const
  {
    return curr_frame > 1 ? cs[curr_frame-1].m_fcn : cs[0].m_fcn;
  }

  size_t do_current_frame (void) { return curr_frame; }

  size_t do_size (void) { return cs.size (); }

  size_t do_num_user_code_frames (octave_idx_type& curr_user_frame) const;

  symbol_table::scope_id do_current_scope (void) const
  {
    return curr_frame > 0 && curr_frame < cs.size ()
           ? cs[curr_frame].m_scope : 0;
  }

  symbol_table::context_id do_current_context (void) const
  {
    return curr_frame > 0 && curr_frame < cs.size ()
           ? cs[curr_frame].m_context : 0;
  }

  /*  const stack_frame& do_frame (size_t idx)
  {
    static stack_frame foobar;

    return idx < cs.size () ? cs[idx] : foobar;
  }
  */
  octave_function *do_element (size_t n)
  {
    octave_function *retval = 0;

    if (cs.size () > n)
      {
        stack_frame& elt = cs[n];
        retval = elt.m_fcn;
      }

    return retval;
  }

  octave_user_code *do_caller_user_code (size_t nskip) const;

  bool do_all_scripts (void) const;

  void do_push (octave_function *fcn, symbol_table::scope_id scope,
                symbol_table::context_id context)
  {
    size_t prev_frame = curr_frame;
    curr_frame = cs.size ();
    cs.push_back (stack_frame (fcn, scope, context, prev_frame));
    symbol_table::set_scope_and_context (scope, context);
  }

  octave_function *do_current (void) const
  {
    octave_function *retval = 0;

    if (! cs.empty ())
      {
        const stack_frame& elt = cs[curr_frame];
        retval = elt.m_fcn;
      }

    return retval;
  }

  void do_set_location (int l, int c)
  {
    if (! cs.empty ())
      {
        stack_frame& elt = cs.back ();

        elt.m_line = l;
        elt.m_column = c;
      }
  }

  void do_set_line (int l)
  {
    if (! cs.empty ())
      {
        stack_frame& elt = cs.back ();

        elt.m_line = l;
      }
  }

  void do_set_column (int c)
  {
    if (! cs.empty ())
      {
        stack_frame& elt = cs.back ();

        elt.m_column = c;
      }
  }

  std::list<octave_call_stack::stack_frame>
  do_backtrace_frames (size_t nskip, octave_idx_type& curr_user_frame) const;

  octave_map do_backtrace (size_t nskip,
                           octave_idx_type& curr_user_frame,
                           bool print_subfn) const;

  bool do_goto_frame (size_t n, bool verbose);

  bool do_goto_frame_relative (int n, bool verbose);

  void do_goto_caller_frame (void);

  void do_goto_base_frame (void);

  void do_pop (void)
  {
    if (cs.size () > 1)
      {
        const stack_frame& elt = cs.back ();
        curr_frame = elt.m_prev;
        cs.pop_back ();
        const stack_frame& new_elt = cs[curr_frame];
        symbol_table::set_scope_and_context (new_elt.m_scope, new_elt.m_context);
      }
  }

  void do_clear (void) { cs.clear (); }
};

// Call a function with exceptions handled to avoid problems with
// errors while shutting down.

#define OCTAVE_IGNORE_EXCEPTION(E) \
  catch (E) \
    { \
      std::cerr << "error: ignoring " #E " while preparing to exit" << std::endl; \
      recover_from_exception (); \
    }

#define OCTAVE_SAFE_CALL(F, ARGS) \
  do \
    { \
      try \
        { \
          unwind_protect frame; \
 \
          frame.protect_var (Vdebug_on_error); \
          frame.protect_var (Vdebug_on_warning); \
 \
          Vdebug_on_error = false; \
          Vdebug_on_warning = false; \
 \
          F ARGS; \
        } \
      OCTAVE_IGNORE_EXCEPTION (octave_interrupt_exception) \
      OCTAVE_IGNORE_EXCEPTION (octave_execution_exception) \
      OCTAVE_IGNORE_EXCEPTION (std::bad_alloc) \
 \
      if (error_state) \
        error_state = 0; \
    } \
  while (0)

#endif
