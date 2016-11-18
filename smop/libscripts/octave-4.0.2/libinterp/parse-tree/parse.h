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

#if !defined (octave_parse_h)
#define octave_parse_h 1

#include <cstdio>

#include <string>

#include <stack>
#include <vector>
#include <map>

#include "lex.h"
#include "symtab.h"
#include "token.h"

class octave_comment_list;
class octave_function;
class octave_user_function;
class tree;
class tree_anon_fcn_handle;
class tree_argument_list;
class tree_array_list;
class tree_cell;
class tree_classdef;
class tree_classdef_attribute_list;
class tree_classdef_body;
class tree_classdef_enum_block;
class tree_classdef_enum_list;
class tree_classdef_events_block;
class tree_classdef_events_list;
class tree_classdef_methods_block;
class tree_classdef_methods_list;
class tree_classdef_properties_block;
class tree_classdef_property_list;
class tree_classdef_superclass_list;
class tree_colon_expression;
class tree_command;
class tree_constant;
class tree_decl_command;
class tree_decl_init_list;
class tree_expression;
class tree_fcn_handle;
class tree_funcall;
class tree_function_def;
class tree_identifier;
class tree_if_clause;
class tree_if_command;
class tree_if_command_list;
class tree_index_expression;
class tree_matrix;
class tree_matrix;
class tree_parameter_list;
class tree_statement;
class tree_statement_list;
class tree_statement_listtree_statement;
class tree_switch_case;
class tree_switch_case_list;
class tree_switch_command;

#include "oct-obj.h"

// Nonzero means print parser debugging info (-d).
extern int octave_debug;

// TRUE means we printed messages about reading startup files.
extern bool reading_startup_message_printed;

extern OCTINTERP_API std::string
get_help_from_file (const std::string& nm, bool& symbol_found,
                    std::string& file);

extern OCTINTERP_API std::string
get_help_from_file (const std::string& nm, bool& symbol_found);

extern OCTINTERP_API std::string lookup_autoload (const std::string& nm);

extern OCTINTERP_API string_vector autoloaded_functions (void);

extern OCTINTERP_API string_vector
reverse_lookup_autoload (const std::string& nm);

extern OCTINTERP_API octave_function *
load_fcn_from_file (const std::string& file_name,
                    const std::string& dir_name = std::string (),
                    const std::string& dispatch_type = std::string (),
                    const std::string& package_name = std::string (),
                    const std::string& fcn_name = std::string (),
                    bool autoload = false);

extern OCTINTERP_API void
source_file (const std::string& file_name,
             const std::string& context = std::string (),
             bool verbose = false, bool require_file = true,
             const std::string& warn_for = std::string ());

extern OCTINTERP_API octave_value_list
feval (const std::string& name,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (octave_function *fcn,
       const octave_value_list& args = octave_value_list (),
       int nargout = 0);

extern OCTINTERP_API octave_value_list
feval (const octave_value_list& args, int nargout = 0);

extern OCTINTERP_API octave_value_list
eval_string (const std::string&, bool silent, int& parse_status, int hargout);

extern OCTINTERP_API octave_value
eval_string (const std::string&, bool silent, int& parse_status);

extern OCTINTERP_API void cleanup_statement_list (tree_statement_list **lst);

// Global access to currently active lexer.
// FIXME: to be removed after more parser+lexer refactoring.
extern octave_base_lexer *LEXER;

class
octave_base_parser
{
public:

  octave_base_parser (octave_base_lexer& lxr)
    : endfunction_found (false),
      autoloading (false), fcn_file_from_relative_lookup (false),
      parsing_subfunctions (false), max_fcn_depth (0),
      curr_fcn_depth (0), primary_fcn_scope (-1),
      curr_class_name (), curr_package_name (), function_scopes (),
      primary_fcn_ptr (0), subfunction_names (), classdef_object (0),
      stmt_list (0), lexer (lxr)
  { }

  ~octave_base_parser (void);

  void reset (void);

  // Error mesages for mismatched end tokens.
  void end_error (const char *type, token::end_tok_type ettype, int l, int c);

  // Check to see that end tokens are properly matched.
  bool end_token_ok (token *tok, token::end_tok_type expected);

  // Maybe print a warning if an assignment expression is used as the
  // test in a logical expression.
  void maybe_warn_assign_as_truth_value (tree_expression *expr);

  // Maybe print a warning about switch labels that aren't constants.
  void maybe_warn_variable_switch_label (tree_expression *expr);

  // Finish building a range.
  tree_expression *finish_colon_expression (tree_colon_expression *e);

  // Build a constant.
  tree_constant *make_constant (int op, token *tok_val);

  // Build a function handle.
  tree_fcn_handle *make_fcn_handle (token *tok_val);

  // Build an anonymous function handle.
  tree_anon_fcn_handle *
  make_anon_fcn_handle (tree_parameter_list *param_list, tree_statement *stmt);

  // Build a binary expression.
  tree_expression *
  make_binary_op (int op, tree_expression *op1, token *tok_val,
                  tree_expression *op2);

  // Build a boolean expression.
  tree_expression *
  make_boolean_op (int op, tree_expression *op1, token *tok_val,
                   tree_expression *op2);

  // Build a prefix expression.
  tree_expression *
  make_prefix_op (int op, tree_expression *op1, token *tok_val);

  // Build a postfix expression.
  tree_expression *
  make_postfix_op (int op, tree_expression *op1, token *tok_val);

  // Build an unwind-protect command.
  tree_command *
  make_unwind_command (token *unwind_tok, tree_statement_list *body,
                       tree_statement_list *cleanup, token *end_tok,
                       octave_comment_list *lc, octave_comment_list *mc);

  // Build a try-catch command.
  tree_command *
  make_try_command (token *try_tok, tree_statement_list *body,
                    char catch_sep, tree_statement_list *cleanup,
                    token *end_tok, octave_comment_list *lc,
                    octave_comment_list *mc);

  // Build a while command.
  tree_command *
  make_while_command (token *while_tok, tree_expression *expr,
                      tree_statement_list *body, token *end_tok,
                      octave_comment_list *lc);

  // Build a do-until command.
  tree_command *
  make_do_until_command (token *until_tok, tree_statement_list *body,
                         tree_expression *expr, octave_comment_list *lc);

  // Build a for command.
  tree_command *
  make_for_command (int tok_id, token *for_tok, tree_argument_list *lhs,
                    tree_expression *expr, tree_expression *maxproc,
                    tree_statement_list *body, token *end_tok,
                    octave_comment_list *lc);

  // Build a break command.
  tree_command *make_break_command (token *break_tok);

  // Build a continue command.
  tree_command *make_continue_command (token *continue_tok);

  // Build a return command.
  tree_command *make_return_command (token *return_tok);

  // Start an if command.
  tree_if_command_list *
  start_if_command (tree_expression *expr, tree_statement_list *list);

  // Finish an if command.
  tree_if_command *
  finish_if_command (token *if_tok, tree_if_command_list *list,
                     token *end_tok, octave_comment_list *lc);

  // Build an elseif clause.
  tree_if_clause *
  make_elseif_clause (token *elseif_tok, tree_expression *expr,
                      tree_statement_list *list, octave_comment_list *lc);

  // Finish a switch command.
  tree_switch_command *
  finish_switch_command (token *switch_tok, tree_expression *expr,
                         tree_switch_case_list *list, token *end_tok,
                         octave_comment_list *lc);

  // Build a switch case.
  tree_switch_case *
  make_switch_case (token *case_tok, tree_expression *expr,
                    tree_statement_list *list, octave_comment_list *lc);

  // Build an assignment to a variable.
  tree_expression *
  make_assign_op (int op, tree_argument_list *lhs, token *eq_tok,
                  tree_expression *rhs);

  // Define a script.
  void make_script (tree_statement_list *cmds, tree_statement *end_script);

  // Begin defining a function.
  octave_user_function *
  start_function (tree_parameter_list *param_list, tree_statement_list *body,
                  tree_statement *end_function);

  // Create a no-op statement for end_function.
  tree_statement *make_end (const std::string& type, bool eof, int l, int c);

  // Do most of the work for defining a function.
  octave_user_function *
  frob_function (const std::string& fname, octave_user_function *fcn);

  // Finish defining a function.
  tree_function_def *
  finish_function (tree_parameter_list *ret_list,
                   octave_user_function *fcn, octave_comment_list *lc,
                   int l, int c);

  // Reset state after parsing function.
  void
  recover_from_parsing_function (void);

  tree_funcall *
  make_superclass_ref (const std::string& method_nm,
                       const std::string& class_nm);

  tree_funcall *
  make_meta_class_query (const std::string& class_nm);

  tree_classdef *
  make_classdef (token *tok_val, tree_classdef_attribute_list *a,
                 tree_identifier *id, tree_classdef_superclass_list *sc,
                 tree_classdef_body *body, token *end_tok,
                 octave_comment_list *lc);

  tree_classdef_properties_block *
  make_classdef_properties_block (token *tok_val,
                                  tree_classdef_attribute_list *a,
                                  tree_classdef_property_list *plist,
                                  token *end_tok, octave_comment_list *lc);

  tree_classdef_methods_block *
  make_classdef_methods_block (token *tok_val,
                               tree_classdef_attribute_list *a,
                               tree_classdef_methods_list *mlist,
                               token *end_tok, octave_comment_list *lc);

  tree_classdef_events_block *
  make_classdef_events_block (token *tok_val,
                              tree_classdef_attribute_list *a,
                              tree_classdef_events_list *elist,
                              token *end_tok, octave_comment_list *lc);

  tree_classdef_enum_block *
  make_classdef_enum_block (token *tok_val,
                            tree_classdef_attribute_list *a,
                            tree_classdef_enum_list *elist,
                            token *end_tok, octave_comment_list *lc);

  octave_user_function *
  start_classdef_external_method (tree_identifier *id,
                                  tree_parameter_list *pl);

  tree_function_def *
  finish_classdef_external_method (octave_user_function *fcn,
                                   tree_parameter_list *ret_list,
                                   octave_comment_list *cl);

  // Make an index expression.
  tree_index_expression *
  make_index_expression (tree_expression *expr,
                         tree_argument_list *args, char type);

  // Make an indirect reference expression.
  tree_index_expression *
  make_indirect_ref (tree_expression *expr, const std::string&);

  // Make an indirect reference expression with dynamic field name.
  tree_index_expression *
  make_indirect_ref (tree_expression *expr, tree_expression *field);

  // Make a declaration command.
  tree_decl_command *
  make_decl_command (int tok, token *tok_val, tree_decl_init_list *lst);

  // Validate matrix or cell
  bool validate_array_list (tree_expression *e);

  // Validate matrix object used in "[lhs] = ..." assignments.
  tree_argument_list *validate_matrix_for_assignment (tree_expression *e);

  // Finish building an array_list (common action for finish_matrix
  // and finish_cell).
  tree_expression *finish_array_list (tree_array_list *a);

  // Finish building a matrix list.
  tree_expression *finish_matrix (tree_matrix *m);

  // Finish building a cell list.
  tree_expression *finish_cell (tree_cell *c);

  // Maybe print a warning.  Duh.
  void maybe_warn_missing_semi (tree_statement_list *);

  // Set the print flag for a statement based on the separator type.
  tree_statement_list *
  set_stmt_print_flag (tree_statement_list *, char, bool);

  // Finish building a statement.
  template <class T>
  tree_statement *make_statement (T *arg);

  // Create a statement list.
  tree_statement_list *make_statement_list (tree_statement *stmt);

  // Append a statement to an existing statement list.
  tree_statement_list *
  append_statement_list (tree_statement_list *list, char sep,
                         tree_statement *stmt, bool warn_missing_semi);

  // Generic error messages.
  void bison_error (const char *s);

  // Have we found an explicit end to a function?
  bool endfunction_found;

  // TRUE means we are in the process of autoloading a function.
  bool autoloading;

  // TRUE means the current function file was found in a relative path
  // element.
  bool fcn_file_from_relative_lookup;

  // FALSE if we are still at the primary function. Subfunctions can
  // only be declared inside function files.
  bool parsing_subfunctions;

  // Maximum function depth detected.  Used to determine whether
  // we have nested functions or just implicitly ended subfunctions.
  int max_fcn_depth;

  // = 0 currently outside any function.
  // = 1 inside the primary function or a subfunction.
  // > 1 means we are looking at a function definition that seems to be
  //     inside a function. Note that the function still might not be a
  //     nested function.
  int curr_fcn_depth;

  // Scope where we install all subfunctions and nested functions. Only
  // used while reading function files.
  symbol_table::scope_id primary_fcn_scope;

  // Name of the current class when we are parsing class methods or
  // constructors.
  std::string curr_class_name;

  // Name of the current package when we are parsing an element contained
  // in a package directory (+-directory).
  std::string curr_package_name;

  // A stack holding the nested function scopes being parsed.
  // We don't use std::stack, because we want the clear method. Also, we
  // must access one from the top
  std::vector<symbol_table::scope_id> function_scopes;

  // Pointer to the primary user function or user script function.
  octave_function *primary_fcn_ptr;

  // List of subfunction names, initially in the order they are
  // installed in the symbol table, then ordered as they appear in the
  // file.  Eventually stashed in the primary function object.
  std::list<std::string> subfunction_names;

  // Pointer to the classdef object we just parsed, if any.
  tree_classdef *classdef_object;

  // Result of parsing input.
  tree_statement_list *stmt_list;

  // State of the lexer.
  octave_base_lexer& lexer;

private:

  // No copying!

  octave_base_parser (const octave_base_parser&);

  octave_base_parser& operator = (const octave_base_parser&);
};

class
octave_parser : public octave_base_parser
{
public:

  octave_parser (void)
    : octave_base_parser (*(new octave_lexer ()))
  { }

  octave_parser (FILE *file)
    : octave_base_parser (*(new octave_lexer (file)))
  { }

  octave_parser (const std::string& eval_string)
    : octave_base_parser (*(new octave_lexer (eval_string)))
  { }

  octave_parser (octave_lexer& lxr)
    : octave_base_parser (lxr)
  { }

  ~octave_parser (void) { }

  int run (void);

private:

  // No copying!

  octave_parser (const octave_parser&);

  octave_parser& operator = (const octave_parser&);
};

class
octave_push_parser : public octave_base_parser
{
public:

  octave_push_parser (void)
    : octave_base_parser (*(new octave_push_lexer ())), parser_state (0)
  {
    init ();
  }

  ~octave_push_parser (void);

  void init (void);

  int run (const std::string& input, bool eof);

private:

  // Internal state of the Bison parser.
  void *parser_state;

  // No copying!

  octave_push_parser (const octave_push_parser&);

  octave_push_parser& operator = (const octave_push_parser&);
};

#endif
