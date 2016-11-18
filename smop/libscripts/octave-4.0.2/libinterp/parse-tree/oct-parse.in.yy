/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2009 David Grundberg
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

// Parser for Octave.

// C decarations.

%{
#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cstdio>
#include <cstdlib>

#include <iostream>
#include <map>
#include <sstream>

#include "Cell.h"
#include "Matrix.h"
#include "cmd-edit.h"
#include "cmd-hist.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"

#include "comment-list.h"
#include "defaults.h"
#include "defun.h"
#include "dirfns.h"
#include "dynamic-ld.h"
#include "error.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "ov-null-mat.h"
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pt-all.h"
#include "pt-eval.h"
#include "pt-funcall.h"
#include "symtab.h"
#include "token.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// oct-parse.h must be included after pt-all.h
#include <oct-parse.h>

extern int octave_lex (YYSTYPE *, void *);

// Global access to currently active lexer.
// FIXME -- to be removed after more parser+lexer refactoring.
octave_base_lexer *LEXER = 0;

#if defined (GNULIB_NAMESPACE)
// Calls to the following functions appear in the generated output from
// Bison without the namespace tag.  Redefine them so we will use them
// via the gnulib namespace.
#define fclose GNULIB_NAMESPACE::fclose
#define fprintf GNULIB_NAMESPACE::fprintf
#define malloc GNULIB_NAMESPACE::malloc
#endif

// TRUE means we printed messages about reading startup files.
bool reading_startup_message_printed = false;

// List of autoloads (function -> file mapping).
static std::map<std::string, std::string> autoload_map;

// Forward declarations for some functions defined at the bottom of
// the file.

static void yyerror (octave_base_parser& parser, const char *s);

#define ABORT_PARSE \
  do \
    { \
      yyerrok; \
      if (interactive && ! lexer.input_from_eval_string ()) \
        YYACCEPT; \
      else \
        YYABORT; \
    } \
  while (0)

#define lexer parser.lexer
#define scanner lexer.scanner

%}

// Bison declarations.

// The grammar currently has 14 shift/reduce conflicts.  Ensure that
// we notice if that number changes.

%expect 14

%API_PREFIX_DECL%

// We are using the pure parser interface and the reentrant lexer
// interface but the Octave parser and lexer are NOT properly
// reentrant because both still use many global variables.  It should be
// safe to create a parser object and call it while anotehr parser
// object is active (to parse a callback function while the main
// interactive parser is waiting for input, for example) if you take
// care to properly save and restore (typically with an unwind_protect
// object) relevant global values before and after the nested call.

%define api.pure
%PUSH_PULL_DECL%
%parse-param { octave_base_parser& parser }
%lex-param { void *scanner }

%union
{
  int dummy_type;

  // The type of the basic tokens returned by the lexer.
  token *tok_val;

  // Comment strings that we need to deal with mid-rule.
  octave_comment_list *comment_type;

  // Types for the nonterminals we generate.
  char punct_type;
  tree *tree_type;
  tree_matrix *tree_matrix_type;
  tree_cell *tree_cell_type;
  tree_expression *tree_expression_type;
  tree_constant *tree_constant_type;
  tree_fcn_handle *tree_fcn_handle_type;
  tree_funcall *tree_funcall_type;
  tree_function_def *tree_function_def_type;
  tree_anon_fcn_handle *tree_anon_fcn_handle_type;
  tree_identifier *tree_identifier_type;
  tree_index_expression *tree_index_expression_type;
  tree_colon_expression *tree_colon_expression_type;
  tree_argument_list *tree_argument_list_type;
  tree_parameter_list *tree_parameter_list_type;
  tree_command *tree_command_type;
  tree_if_command *tree_if_command_type;
  tree_if_clause *tree_if_clause_type;
  tree_if_command_list *tree_if_command_list_type;
  tree_switch_command *tree_switch_command_type;
  tree_switch_case *tree_switch_case_type;
  tree_switch_case_list *tree_switch_case_list_type;
  tree_decl_elt *tree_decl_elt_type;
  tree_decl_init_list *tree_decl_init_list_type;
  tree_decl_command *tree_decl_command_type;
  tree_statement *tree_statement_type;
  tree_statement_list *tree_statement_list_type;
  octave_user_function *octave_user_function_type;

  tree_classdef *tree_classdef_type;
  tree_classdef_attribute* tree_classdef_attribute_type;
  tree_classdef_attribute_list* tree_classdef_attribute_list_type;
  tree_classdef_superclass* tree_classdef_superclass_type;
  tree_classdef_superclass_list* tree_classdef_superclass_list_type;
  tree_classdef_body* tree_classdef_body_type;
  tree_classdef_property* tree_classdef_property_type;
  tree_classdef_property_list* tree_classdef_property_list_type;
  tree_classdef_properties_block* tree_classdef_properties_block_type;
  tree_classdef_methods_list* tree_classdef_methods_list_type;
  tree_classdef_methods_block* tree_classdef_methods_block_type;
  tree_classdef_event* tree_classdef_event_type;
  tree_classdef_events_list* tree_classdef_events_list_type;
  tree_classdef_events_block* tree_classdef_events_block_type;
  tree_classdef_enum* tree_classdef_enum_type;
  tree_classdef_enum_list* tree_classdef_enum_list_type;
  tree_classdef_enum_block* tree_classdef_enum_block_type;
}

// Tokens with line and column information.
%token <tok_val> '=' ':' '-' '+' '*' '/'
%token <tok_val> ADD_EQ SUB_EQ MUL_EQ DIV_EQ LEFTDIV_EQ POW_EQ
%token <tok_val> EMUL_EQ EDIV_EQ ELEFTDIV_EQ EPOW_EQ AND_EQ OR_EQ
%token <tok_val> LSHIFT_EQ RSHIFT_EQ LSHIFT RSHIFT
%token <tok_val> EXPR_AND_AND EXPR_OR_OR
%token <tok_val> EXPR_AND EXPR_OR EXPR_NOT
%token <tok_val> EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%token <tok_val> LEFTDIV EMUL EDIV ELEFTDIV EPLUS EMINUS
%token <tok_val> HERMITIAN TRANSPOSE
%token <tok_val> PLUS_PLUS MINUS_MINUS POW EPOW
%token <tok_val> NUM IMAG_NUM
%token <tok_val> STRUCT_ELT
%token <tok_val> NAME
%token <tok_val> END
%token <tok_val> DQ_STRING SQ_STRING
%token <tok_val> FOR PARFOR WHILE DO UNTIL
%token <tok_val> IF ELSEIF ELSE
%token <tok_val> SWITCH CASE OTHERWISE
%token <tok_val> BREAK CONTINUE FUNC_RET
%token <tok_val> UNWIND CLEANUP
%token <tok_val> TRY CATCH
%token <tok_val> GLOBAL PERSISTENT
%token <tok_val> FCN_HANDLE
%token <tok_val> CLASSDEF
%token <tok_val> PROPERTIES METHODS EVENTS ENUMERATION
%token <tok_val> METAQUERY
%token <tok_val> SUPERCLASSREF
%token <tok_val> FQ_IDENT
%token <tok_val> GET SET
%token <tok_val> FCN

// Other tokens.
%token<dummy_type> END_OF_INPUT LEXICAL_ERROR
%token<dummy_type> INPUT_FILE
// %token VARARGIN VARARGOUT

%token<dummy_type> '(' ')' '[' ']' '{' '}' '.' ',' ';' '@' '\n'

// Nonterminals we construct.
%type <dummy_type> indirect_ref_op decl_param_init push_fcn_symtab
%type <dummy_type> param_list_beg param_list_end stmt_begin parse_error
%type <comment_type> stash_comment
%type <tok_val> function_beg classdef_beg
%type <punct_type> sep_no_nl opt_sep_no_nl nl opt_nl sep opt_sep
%type <tree_type> input
%type <tree_constant_type> string constant magic_colon
%type <tree_anon_fcn_handle_type> anon_fcn_handle
%type <tree_fcn_handle_type> fcn_handle
%type <tree_matrix_type> matrix_rows
%type <tree_cell_type> cell_rows
%type <tree_expression_type> matrix cell
%type <tree_expression_type> primary_expr oper_expr power_expr
%type <tree_expression_type> simple_expr colon_expr assign_expr expression
%type <tree_identifier_type> identifier fcn_name magic_tilde
%type <tree_funcall_type> superclass_identifier meta_identifier
%type <octave_user_function_type> function1 function2
%type <tree_index_expression_type> word_list_cmd
%type <tree_colon_expression_type> colon_expr1
%type <tree_argument_list_type> arg_list word_list assign_lhs
%type <tree_argument_list_type> cell_or_matrix_row
%type <tree_parameter_list_type> param_list param_list1 param_list2
%type <tree_parameter_list_type> return_list return_list1
%type <tree_command_type> command select_command loop_command
%type <tree_command_type> jump_command except_command
%type <tree_function_def_type> function
%type <tree_classdef_type> classdef
%type <tree_command_type> file
%type <tree_if_command_type> if_command
%type <tree_if_clause_type> elseif_clause else_clause
%type <tree_if_command_list_type> if_cmd_list1 if_cmd_list
%type <tree_switch_command_type> switch_command
%type <tree_switch_case_type> switch_case default_case
%type <tree_switch_case_list_type> case_list1 case_list
%type <tree_decl_elt_type> decl2 param_list_elt
%type <tree_decl_init_list_type> decl1
%type <tree_decl_command_type> declaration
%type <tree_statement_type> statement function_end
%type <tree_statement_list_type> simple_list simple_list1 list list1
%type <tree_statement_list_type> opt_list
%type <tree_classdef_attribute_type> attr
%type <tree_classdef_attribute_list_type> attr_list opt_attr_list
%type <tree_classdef_superclass_type> superclass
%type <tree_classdef_superclass_list_type> superclass_list opt_superclass_list
%type <tree_classdef_body_type> class_body
%type <tree_classdef_property_type> class_property
%type <tree_classdef_property_list_type> property_list
%type <tree_classdef_properties_block_type> properties_block
%type <tree_classdef_methods_list_type> methods_list
%type <tree_classdef_methods_block_type> methods_block
%type <tree_classdef_event_type> class_event
%type <tree_classdef_events_list_type> events_list
%type <tree_classdef_events_block_type> events_block
%type <tree_classdef_enum_type> class_enum
%type <tree_classdef_enum_list_type> enum_list
%type <tree_classdef_enum_block_type> enum_block
%type <tree_function_def_type> method_decl method
%type <octave_user_function_type> method_decl1

// Precedence and associativity.
%right '=' ADD_EQ SUB_EQ MUL_EQ DIV_EQ LEFTDIV_EQ POW_EQ EMUL_EQ EDIV_EQ ELEFTDIV_EQ EPOW_EQ OR_EQ AND_EQ LSHIFT_EQ RSHIFT_EQ
%left EXPR_OR_OR
%left EXPR_AND_AND
%left EXPR_OR
%left EXPR_AND
%left EXPR_LT EXPR_LE EXPR_EQ EXPR_NE EXPR_GE EXPR_GT
%left LSHIFT RSHIFT
%left ':'
%left '-' '+' EPLUS EMINUS
%left '*' '/' LEFTDIV EMUL EDIV ELEFTDIV
%right UNARY EXPR_NOT
%left POW EPOW HERMITIAN TRANSPOSE
%right PLUS_PLUS MINUS_MINUS
%left '(' '.' '{'

// How to clean up if there is a parse error.  We handle deleting tokens
// and comments seperately and separators are just characters.  The
// remaining items are dynamically allocated parse tree objects that
// must be deleted.  Use the wildcard case (<*>) to detect unhandled
// cases (for example, a new semantic type is added but not handled
// here).

%destructor { } <tok_val>
%destructor { } <punct_type>
%destructor { } <comment_type>
%destructor { } <>

%destructor { delete $$; } <tree_type>
%destructor { delete $$; } <tree_matrix_type>
%destructor { delete $$; } <tree_cell_type>
%destructor { delete $$; } <tree_expression_type>
%destructor { delete $$; } <tree_constant_type>
%destructor { delete $$; } <tree_fcn_handle_type>
%destructor { delete $$; } <tree_funcall_type>
%destructor { delete $$; } <tree_function_def_type>
%destructor { delete $$; } <tree_anon_fcn_handle_type>
%destructor { delete $$; } <tree_identifier_type>
%destructor { delete $$; } <tree_index_expression_type>
%destructor { delete $$; } <tree_colon_expression_type>
%destructor { delete $$; } <tree_argument_list_type>
%destructor { delete $$; } <tree_parameter_list_type>
%destructor { delete $$; } <tree_command_type>
%destructor { delete $$; } <tree_if_command_type>
%destructor { delete $$; } <tree_if_clause_type>
%destructor { delete $$; } <tree_if_command_list_type>
%destructor { delete $$; } <tree_switch_command_type>
%destructor { delete $$; } <tree_switch_case_type>
%destructor { delete $$; } <tree_switch_case_list_type>
%destructor { delete $$; } <tree_decl_elt_type>
%destructor { delete $$; } <tree_decl_init_list_type>
%destructor { delete $$; } <tree_decl_command_type>
%destructor { delete $$; } <tree_statement_type>
%destructor { delete $$; } <tree_statement_list_type>
%destructor { delete $$; } <octave_user_function_type>

%destructor { delete $$; } <tree_classdef_type>
%destructor { delete $$; } <tree_classdef_attribute_type>
%destructor { delete $$; } <tree_classdef_attribute_list_type>
%destructor { delete $$; } <tree_classdef_superclass_type>
%destructor { delete $$; } <tree_classdef_superclass_list_type>
%destructor { delete $$; } <tree_classdef_body_type>
%destructor { delete $$; } <tree_classdef_property_type>
%destructor { delete $$; } <tree_classdef_property_list_type>
%destructor { delete $$; } <tree_classdef_properties_block_type>
%destructor { delete $$; } <tree_classdef_methods_list_type>
%destructor { delete $$; } <tree_classdef_methods_block_type>
%destructor { delete $$; } <tree_classdef_event_type>
%destructor { delete $$; } <tree_classdef_events_list_type>
%destructor { delete $$; } <tree_classdef_events_block_type>
%destructor { delete $$; } <tree_classdef_enum_type>
%destructor { delete $$; } <tree_classdef_enum_list_type>
%destructor { delete $$; } <tree_classdef_enum_block_type>

// Defining a generic destructor generates a warning if destructors are
// already explicitly declared for all types.
//
// %destructor {
//    warning_with_id
//      ("Octave:parser-destructor",
//       "possible memory leak in cleanup following parse error");
// } <*>

// Where to start.
%start input

%%

// ==============================
// Statements and statement lists
// ==============================

input           : simple_list '\n'
                  {
                    $$ = 0;
                    parser.stmt_list = $1;
                    YYACCEPT;
                  }
                | simple_list END_OF_INPUT
                  {
                    $$ = 0;
                    lexer.end_of_input = true;
                    parser.stmt_list = $1;
                    YYACCEPT;
                  }
                | parse_error
                  {
                    $$ = 0;
                    ABORT_PARSE;
                  }
                ;

simple_list     : opt_sep_no_nl
                  {
                    YYUSE ($1);

                    $$ = 0;
                  }
                | simple_list1 opt_sep_no_nl
                  { $$ = parser.set_stmt_print_flag ($1, $2, false); }
                ;

simple_list1    : statement
                  { $$ = parser.make_statement_list ($1); }
                | simple_list1 sep_no_nl statement
                  { $$ = parser.append_statement_list ($1, $2, $3, false); }
                ;

opt_list        : // empty
                  { $$ = new tree_statement_list (); }
                | list
                  { $$ = $1; }
                ;

list            : list1 opt_sep
                  { $$ = parser.set_stmt_print_flag ($1, $2, true); }
                ;

list1           : statement
                  { $$ = parser.make_statement_list ($1); }
                | list1 sep statement
                  { $$ = parser.append_statement_list ($1, $2, $3, true); }
                ;

statement       : expression
                  { $$ = parser.make_statement ($1); }
                | command
                  { $$ = parser.make_statement ($1); }
                | word_list_cmd
                  { $$ = parser.make_statement ($1); }
                ;

// =================
// Word-list command
// =================

// These are not really like expressions since they can't appear on
// the RHS of an assignment.  But they are also not like commands (IF,
// WHILE, etc.

word_list_cmd   : identifier word_list
                  {
                    $$ = parser.make_index_expression ($1, $2, '(');
                    if (! $$)
                      {
                        // make_index_expression deleted $1 and $2.
                        ABORT_PARSE;
                      }
                  }
                ;

word_list       : string
                  { $$ = new tree_argument_list ($1); }
                | word_list string
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

// ===========
// Expressions
// ===========

identifier      : NAME
                  {
                    symbol_table::symbol_record *sr = $1->sym_rec ();
                    $$ = new tree_identifier (*sr, $1->line (), $1->column ());
                  }
                ;

superclass_identifier
                : SUPERCLASSREF
                  {
                    std::string method_nm = $1->superclass_method_name ();
                    std::string class_nm = $1->superclass_class_name ();

                    $$ = parser.make_superclass_ref (method_nm, class_nm);
                  }
                ;

meta_identifier : METAQUERY
                  {
                    std::string class_nm = $1->text ();

                    $$ = parser.make_meta_class_query (class_nm);
                  }
                ;

string          : DQ_STRING
                  { $$ = parser.make_constant (DQ_STRING, $1); }
                | SQ_STRING
                  { $$ = parser.make_constant (SQ_STRING, $1); }
                ;

constant        : NUM
                  { $$ = parser.make_constant (NUM, $1); }
                | IMAG_NUM
                  { $$ = parser.make_constant (IMAG_NUM, $1); }
                | string
                  { $$ = $1; }
                ;

matrix          : '[' matrix_rows ']'
                  { $$ = parser.finish_matrix ($2); }
                ;

matrix_rows     : cell_or_matrix_row
                  { $$ = $1 ? new tree_matrix ($1) : 0; }
                | matrix_rows ';' cell_or_matrix_row
                  {
                    if ($1)
                      {
                        if ($3)
                          $1->append ($3);

                        $$ = $1;
                      }
                    else
                      $$ = $3 ? new tree_matrix ($3) : 0;
                  }
                ;

cell            : '{' cell_rows '}'
                  { $$ = parser.finish_cell ($2); }
                ;

cell_rows       : cell_or_matrix_row
                  { $$ = $1 ? new tree_cell ($1) : 0; }
                | cell_rows ';' cell_or_matrix_row
                  {
                    if ($1)
                      {
                        if ($3)
                          $1->append ($3);

                        $$ = $1;
                      }
                    else
                      $$ = $3 ? new tree_cell ($3) : 0;
                  }
                ;

// tree_argument_list objects can't be empty or have leading or trailing
// commas, but those are all allowed in matrix and cell array rows.

cell_or_matrix_row
                : // empty
                  { $$ = 0; }
                | ','
                  { $$ = 0; }
                | arg_list
                  { $$ = $1; }
                | arg_list ','
                  { $$ = $1; }
                | ',' arg_list
                  { $$ = $2; }
                | ',' arg_list ','
                  { $$ = $2; }
                ;

fcn_handle      : '@' FCN_HANDLE
                  {
                    $$ = parser.make_fcn_handle ($2);
                    lexer.looking_at_function_handle--;
                  }
                ;

anon_fcn_handle : '@' param_list stmt_begin statement
                  {
                    $$ = parser.make_anon_fcn_handle ($2, $4);
                    lexer.nesting_level.remove ();
                  }
                ;

primary_expr    : identifier
                  { $$ = $1; }
                | constant
                  { $$ = $1; }
                | fcn_handle
                  { $$ = $1; }
                | matrix
                  {
                    lexer.looking_at_matrix_or_assign_lhs = false;
                    $$ = $1;
                  }
                | cell
                  { $$ = $1; }
                | meta_identifier
                  { $$ = $1; }
                | superclass_identifier
                  { $$ = $1; }
                | '(' expression ')'
                  { $$ = $2->mark_in_parens (); }
                ;

magic_colon     : ':'
                  {
                    YYUSE ($1);

                    octave_value tmp (octave_value::magic_colon_t);
                    $$ = new tree_constant (tmp);
                  }
                ;

magic_tilde     : EXPR_NOT
                  {
                    YYUSE ($1);

                    $$ = new tree_black_hole ();
                  }
                ;

arg_list        : expression
                  { $$ = new tree_argument_list ($1); }
                | magic_colon
                  { $$ = new tree_argument_list ($1); }
                | magic_tilde
                  { $$ = new tree_argument_list ($1); }
                | arg_list ',' magic_colon
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                | arg_list ',' magic_tilde
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                | arg_list ',' expression
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

indirect_ref_op : '.'
                  {
                    $$ = 0;
                    lexer.looking_at_indirect_ref = true;
                  }
                ;

oper_expr       : primary_expr
                  { $$ = $1; }
                | oper_expr PLUS_PLUS
                  { $$ = parser.make_postfix_op (PLUS_PLUS, $1, $2); }
                | oper_expr MINUS_MINUS
                  { $$ = parser.make_postfix_op (MINUS_MINUS, $1, $2); }
                | oper_expr '(' ')'
                  {
                    $$ = parser.make_index_expression ($1, 0, '(');
                    if (! $$)
                      {
                        // make_index_expression deleted $1.
                        ABORT_PARSE;
                      }
                  }
                | oper_expr '(' arg_list ')'
                  {
                    $$ = parser.make_index_expression ($1, $3, '(');
                    if (! $$)
                      {
                        // make_index_expression deleted $1 and $3.
                        ABORT_PARSE;
                      }
                  }
                | oper_expr '{' '}'
                  {
                    $$ = parser.make_index_expression ($1, 0, '{');
                    if (! $$)
                      {
                        // make_index_expression deleted $1.
                        ABORT_PARSE;
                      }
                  }
                | oper_expr '{' arg_list '}'
                  {
                    $$ = parser.make_index_expression ($1, $3, '{');
                    if (! $$)
                      {
                        // make_index_expression deleted $1 and $3.
                        ABORT_PARSE;
                      }
                  }
                | oper_expr HERMITIAN
                  { $$ = parser.make_postfix_op (HERMITIAN, $1, $2); }
                | oper_expr TRANSPOSE
                  { $$ = parser.make_postfix_op (TRANSPOSE, $1, $2); }
                | oper_expr indirect_ref_op STRUCT_ELT
                  { $$ = parser.make_indirect_ref ($1, $3->text ()); }
                | oper_expr indirect_ref_op '(' expression ')'
                  { $$ = parser.make_indirect_ref ($1, $4); }
                | PLUS_PLUS oper_expr %prec UNARY
                  { $$ = parser.make_prefix_op (PLUS_PLUS, $2, $1); }
                | MINUS_MINUS oper_expr %prec UNARY
                  { $$ = parser.make_prefix_op (MINUS_MINUS, $2, $1); }
                | EXPR_NOT oper_expr %prec UNARY
                  { $$ = parser.make_prefix_op (EXPR_NOT, $2, $1); }
                | '+' oper_expr %prec UNARY
                  { $$ = parser.make_prefix_op ('+', $2, $1); }
                | '-' oper_expr %prec UNARY
                  { $$ = parser.make_prefix_op ('-', $2, $1); }
                | oper_expr POW power_expr
                  { $$ = parser.make_binary_op (POW, $1, $2, $3); }
                | oper_expr EPOW power_expr
                  { $$ = parser.make_binary_op (EPOW, $1, $2, $3); }
                | oper_expr '+' oper_expr
                  { $$ = parser.make_binary_op ('+', $1, $2, $3); }
                | oper_expr '-' oper_expr
                  { $$ = parser.make_binary_op ('-', $1, $2, $3); }
                | oper_expr '*' oper_expr
                  { $$ = parser.make_binary_op ('*', $1, $2, $3); }
                | oper_expr '/' oper_expr
                  { $$ = parser.make_binary_op ('/', $1, $2, $3); }
                | oper_expr EPLUS oper_expr
                  { $$ = parser.make_binary_op ('+', $1, $2, $3); }
                | oper_expr EMINUS oper_expr
                  { $$ = parser.make_binary_op ('-', $1, $2, $3); }
                | oper_expr EMUL oper_expr
                  { $$ = parser.make_binary_op (EMUL, $1, $2, $3); }
                | oper_expr EDIV oper_expr
                  { $$ = parser.make_binary_op (EDIV, $1, $2, $3); }
                | oper_expr LEFTDIV oper_expr
                  { $$ = parser.make_binary_op (LEFTDIV, $1, $2, $3); }
                | oper_expr ELEFTDIV oper_expr
                  { $$ = parser.make_binary_op (ELEFTDIV, $1, $2, $3); }
                ;

power_expr      : primary_expr
                  { $$ = $1; }
                | power_expr PLUS_PLUS
                  { $$ = parser.make_postfix_op (PLUS_PLUS, $1, $2); }
                | power_expr MINUS_MINUS
                  { $$ = parser.make_postfix_op (MINUS_MINUS, $1, $2); }
                | power_expr '(' ')'
                  {
                    $$ = parser.make_index_expression ($1, 0, '(');
                    if (! $$)
                      {
                        // make_index_expression deleted $1.
                        ABORT_PARSE;
                      }
                  }
                | power_expr '(' arg_list ')'
                  {
                    $$ = parser.make_index_expression ($1, $3, '(');
                    if (! $$)
                      {
                        // make_index_expression deleted $1 and $3.
                        ABORT_PARSE;
                      }
                  }
                | power_expr '{' '}'
                  {
                    $$ = parser.make_index_expression ($1, 0, '{');
                    if (! $$)
                      {
                        // make_index_expression deleted $1.
                        ABORT_PARSE;
                      }
                  }
                | power_expr '{' arg_list '}'
                  {
                    $$ = parser.make_index_expression ($1, $3, '{');
                    if (! $$)
                      {
                        // make_index_expression deleted $1 and $3.
                        ABORT_PARSE;
                      }
                  }
                | power_expr indirect_ref_op STRUCT_ELT
                  { $$ = parser.make_indirect_ref ($1, $3->text ()); }
                | power_expr indirect_ref_op '(' expression ')'
                  { $$ = parser.make_indirect_ref ($1, $4); }
                | PLUS_PLUS power_expr %prec POW
                  { $$ = parser.make_prefix_op (PLUS_PLUS, $2, $1); }
                | MINUS_MINUS power_expr %prec POW
                  { $$ = parser.make_prefix_op (MINUS_MINUS, $2, $1); }
                | EXPR_NOT power_expr %prec POW
                  { $$ = parser.make_prefix_op (EXPR_NOT, $2, $1); }
                | '+' power_expr %prec POW
                  { $$ = parser.make_prefix_op ('+', $2, $1); }
                | '-' power_expr %prec POW
                  { $$ = parser.make_prefix_op ('-', $2, $1); }
                ;

colon_expr      : colon_expr1
                  { $$ = parser.finish_colon_expression ($1); }
                ;

colon_expr1     : oper_expr
                  { $$ = new tree_colon_expression ($1); }
                | colon_expr1 ':' oper_expr
                  {
                    YYUSE ($2);

                    if (! ($$ = $1->append ($3)))
                      {
                        delete $1;
                        delete $3;
                        ABORT_PARSE;
                      }
                  }
                ;

simple_expr     : colon_expr
                  { $$ = $1; }
                | simple_expr LSHIFT simple_expr
                  { $$ = parser.make_binary_op (LSHIFT, $1, $2, $3); }
                | simple_expr RSHIFT simple_expr
                  { $$ = parser.make_binary_op (RSHIFT, $1, $2, $3); }
                | simple_expr EXPR_LT simple_expr
                  { $$ = parser.make_binary_op (EXPR_LT, $1, $2, $3); }
                | simple_expr EXPR_LE simple_expr
                  { $$ = parser.make_binary_op (EXPR_LE, $1, $2, $3); }
                | simple_expr EXPR_EQ simple_expr
                  { $$ = parser.make_binary_op (EXPR_EQ, $1, $2, $3); }
                | simple_expr EXPR_GE simple_expr
                  { $$ = parser.make_binary_op (EXPR_GE, $1, $2, $3); }
                | simple_expr EXPR_GT simple_expr
                  { $$ = parser.make_binary_op (EXPR_GT, $1, $2, $3); }
                | simple_expr EXPR_NE simple_expr
                  { $$ = parser.make_binary_op (EXPR_NE, $1, $2, $3); }
                | simple_expr EXPR_AND simple_expr
                  { $$ = parser.make_binary_op (EXPR_AND, $1, $2, $3); }
                | simple_expr EXPR_OR simple_expr
                  { $$ = parser.make_binary_op (EXPR_OR, $1, $2, $3); }
                | simple_expr EXPR_AND_AND simple_expr
                  { $$ = parser.make_boolean_op (EXPR_AND_AND, $1, $2, $3); }
                | simple_expr EXPR_OR_OR simple_expr
                  { $$ = parser.make_boolean_op (EXPR_OR_OR, $1, $2, $3); }
                ;

assign_lhs      : simple_expr
                  {
                    $$ = parser.validate_matrix_for_assignment ($1);

                    if ($$)
                      { lexer.looking_at_matrix_or_assign_lhs = false; }
                    else
                      {
                        // validate_matrix_for_assignment deleted $1.
                        ABORT_PARSE;
                      }
                  }
                ;

assign_expr     : assign_lhs '=' expression
                  { $$ = parser.make_assign_op ('=', $1, $2, $3); }
                | assign_lhs ADD_EQ expression
                  { $$ = parser.make_assign_op (ADD_EQ, $1, $2, $3); }
                | assign_lhs SUB_EQ expression
                  { $$ = parser.make_assign_op (SUB_EQ, $1, $2, $3); }
                | assign_lhs MUL_EQ expression
                  { $$ = parser.make_assign_op (MUL_EQ, $1, $2, $3); }
                | assign_lhs DIV_EQ expression
                  { $$ = parser.make_assign_op (DIV_EQ, $1, $2, $3); }
                | assign_lhs LEFTDIV_EQ expression
                  { $$ = parser.make_assign_op (LEFTDIV_EQ, $1, $2, $3); }
                | assign_lhs POW_EQ expression
                  { $$ = parser.make_assign_op (POW_EQ, $1, $2, $3); }
                | assign_lhs LSHIFT_EQ expression
                  { $$ = parser.make_assign_op (LSHIFT_EQ, $1, $2, $3); }
                | assign_lhs RSHIFT_EQ expression
                  { $$ = parser.make_assign_op (RSHIFT_EQ, $1, $2, $3); }
                | assign_lhs EMUL_EQ expression
                  { $$ = parser.make_assign_op (EMUL_EQ, $1, $2, $3); }
                | assign_lhs EDIV_EQ expression
                  { $$ = parser.make_assign_op (EDIV_EQ, $1, $2, $3); }
                | assign_lhs ELEFTDIV_EQ expression
                  { $$ = parser.make_assign_op (ELEFTDIV_EQ, $1, $2, $3); }
                | assign_lhs EPOW_EQ expression
                  { $$ = parser.make_assign_op (EPOW_EQ, $1, $2, $3); }
                | assign_lhs AND_EQ expression
                  { $$ = parser.make_assign_op (AND_EQ, $1, $2, $3); }
                | assign_lhs OR_EQ expression
                  { $$ = parser.make_assign_op (OR_EQ, $1, $2, $3); }
                ;

expression      : simple_expr
                  {
                    if ($1 && ($1->is_matrix () || $1->is_cell ()))
                      {
                        if (parser.validate_array_list ($1))
                          $$ = $1;
                        else
                          {
                            delete $1;
                            ABORT_PARSE;
                          }
                      }
                    else
                      $$ = $1;
                  }
                | assign_expr
                  { $$ = $1; }
                | anon_fcn_handle
                  { $$ = $1; }
                ;

// ================================================
// Commands, declarations, and function definitions
// ================================================

command         : declaration
                  { $$ = $1; }
                | select_command
                  { $$ = $1; }
                | loop_command
                  { $$ = $1; }
                | jump_command
                  { $$ = $1; }
                | except_command
                  { $$ = $1; }
                | function
                  { $$ = $1; }
                | file
                  { $$ = $1; }
                ;

// =====================
// Declaration statemnts
// =====================

declaration     : GLOBAL decl1
                  {
                    $$ = parser.make_decl_command (GLOBAL, $1, $2);
                    lexer.looking_at_decl_list = false;
                  }
                | PERSISTENT decl1
                  {
                    $$ = parser.make_decl_command (PERSISTENT, $1, $2);
                    lexer.looking_at_decl_list = false;
                  }
                ;

decl1           : decl2
                  { $$ = new tree_decl_init_list ($1); }
                | decl1 decl2
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

decl_param_init : // empty
                  {
                    $$ = 0;
                    lexer.looking_at_initializer_expression = true;
                  }

decl2           : identifier
                  { $$ = new tree_decl_elt ($1); }
                | identifier '=' decl_param_init expression
                  {
                    YYUSE ($2);

                    lexer.looking_at_initializer_expression = false;
                    $$ = new tree_decl_elt ($1, $4);
                  }
                ;

// ====================
// Selection statements
// ====================

select_command  : if_command
                  { $$ = $1; }
                | switch_command
                  { $$ = $1; }
                ;

// ============
// If statement
// ============

if_command      : IF stash_comment if_cmd_list END
                  {
                    if (! ($$ = parser.finish_if_command ($1, $3, $4, $2)))
                      {
                        // finish_if_command deleted $3.
                        ABORT_PARSE;
                      }
                  }
                ;

if_cmd_list     : if_cmd_list1
                  { $$ = $1; }
                | if_cmd_list1 else_clause
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

if_cmd_list1    : expression stmt_begin opt_sep opt_list
                  {
                    YYUSE ($3);

                    $1->mark_braindead_shortcircuit ();

                    $$ = parser.start_if_command ($1, $4);
                  }
                | if_cmd_list1 elseif_clause
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

elseif_clause   : ELSEIF stash_comment opt_sep expression stmt_begin opt_sep opt_list
                  {
                    YYUSE ($3);
                    YYUSE ($6);

                    $4->mark_braindead_shortcircuit ();

                    $$ = parser.make_elseif_clause ($1, $4, $7, $2);
                  }
                ;

else_clause     : ELSE stash_comment opt_sep opt_list
                  {
                    YYUSE ($1);
                    YYUSE ($3);

                    $$ = new tree_if_clause ($4, $2);
                  }
                ;

// ================
// Switch statement
// ================

switch_command  : SWITCH stash_comment expression opt_sep case_list END
                  {
                    YYUSE ($4);

                    if (! ($$ = parser.finish_switch_command ($1, $3, $5, $6, $2)))
                      {
                        // finish_switch_command deleted $3 adn $5.
                        ABORT_PARSE;
                      }
                  }
                ;

case_list       : // empty
                  { $$ = new tree_switch_case_list (); }
                | default_case
                  { $$ = new tree_switch_case_list ($1); }
                | case_list1
                  { $$ = $1; }
                | case_list1 default_case
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

case_list1      : switch_case
                  { $$ = new tree_switch_case_list ($1); }
                | case_list1 switch_case
                  {
                    $1->append ($2);
                    $$ = $1;
                  }
                ;

switch_case     : CASE stash_comment opt_sep expression stmt_begin opt_sep opt_list
                  {
                    YYUSE ($3);
                    YYUSE ($6);

                    $$ = parser.make_switch_case ($1, $4, $7, $2);
                  }
                ;

default_case    : OTHERWISE stash_comment opt_sep opt_list
                  {
                    YYUSE ($1);
                    YYUSE ($3);

                    $$ = new tree_switch_case ($4, $2);
                  }
                ;

// =======
// Looping
// =======

loop_command    : WHILE stash_comment expression stmt_begin opt_sep opt_list END
                  {
                    YYUSE ($5);

                    $3->mark_braindead_shortcircuit ();

                    if (! ($$ = parser.make_while_command ($1, $3, $6, $7, $2)))
                      {
                        // make_while_command deleted $3 and $6.
                        ABORT_PARSE;
                      }
                  }
                | DO stash_comment opt_sep opt_list UNTIL expression
                  {
                    YYUSE ($1);
                    YYUSE ($3);

                    $$ = parser.make_do_until_command ($5, $4, $6, $2);
                  }
                | FOR stash_comment assign_lhs '=' expression stmt_begin opt_sep opt_list END
                  {
                    YYUSE ($4);
                    YYUSE ($7);

                    if (! ($$ = parser.make_for_command (FOR, $1, $3, $5, 0,
                                                         $8, $9, $2)))
                      {
                        // make_for_command deleted $3, $5, and $8.
                        ABORT_PARSE;
                      }
                  }
                | FOR stash_comment '(' assign_lhs '=' expression ')' opt_sep opt_list END
                  {
                    YYUSE ($5);
                    YYUSE ($8);

                    if (! ($$ = parser.make_for_command (FOR, $1, $4, $6, 0,
                                                         $9, $10, $2)))
                      {
                        // make_for_command deleted $4, $6, and $9.
                        ABORT_PARSE;
                      }
                  }
                | PARFOR stash_comment assign_lhs '=' expression stmt_begin opt_sep opt_list END
                  {
                    YYUSE ($4);
                    YYUSE ($7);

                    if (! ($$ = parser.make_for_command (PARFOR, $1, $3, $5,
                                                         0, $8, $9, $2)))
                      {
                        // make_for_command deleted $3, $5, and $8.
                        ABORT_PARSE;
                      }
                  }
                | PARFOR stash_comment '(' assign_lhs '=' expression ',' expression ')' opt_sep opt_list END
                  {
                    YYUSE ($5);
                    YYUSE ($10);

                    if (! ($$ = parser.make_for_command (PARFOR, $1, $4, $6,
                                                         $8, $11, $12, $2)))
                      {
                        // make_for_command deleted $4, $6, $8, and $11.
                        ABORT_PARSE;
                      }
                  }
                ;

// =======
// Jumping
// =======

jump_command    : BREAK
                  { $$ = parser.make_break_command ($1); }
                | CONTINUE
                  { $$ = parser.make_continue_command ($1); }
                | FUNC_RET
                  { $$ = parser.make_return_command ($1); }
                ;

// ==========
// Exceptions
// ==========

except_command  : UNWIND stash_comment opt_sep opt_list CLEANUP
                  stash_comment opt_sep opt_list END
                  {
                    YYUSE ($3);
                    YYUSE ($5);
                    YYUSE ($7);

                    if (! ($$ = parser.make_unwind_command ($1, $4, $8, $9, $2, $6)))
                      {
                        // make_unwind_command deleted $4 and $8.
                        ABORT_PARSE;
                      }
                  }
                | TRY stash_comment opt_sep opt_list CATCH stash_comment
                  opt_sep opt_list END
                  {
                    YYUSE ($3);
                    YYUSE ($5);
                    YYUSE ($7);

                    if (! ($$ = parser.make_try_command ($1, $4, $7, $8, $9, $2, $6)))
                      {
                        // make_try_command deleted $4 and $8.
                        ABORT_PARSE;
                      }
                  }
                | TRY stash_comment opt_sep opt_list END
                  {
                    YYUSE ($3);

                    if (! ($$ = parser.make_try_command ($1, $4, 0, 0, $5, $2, 0)))
                      {
                        // make_try_command deleted $4.
                        ABORT_PARSE;
                      }
                  }
                ;

// ===========================================
// Some 'subroutines' for function definitions
// ===========================================

push_fcn_symtab : // empty
                  {
                    $$ = 0;

                    parser.curr_fcn_depth++;

                    if (parser.max_fcn_depth < parser.curr_fcn_depth)
                      parser.max_fcn_depth = parser.curr_fcn_depth;

                    lexer.symtab_context.push (symbol_table::alloc_scope ());

                    parser.function_scopes.push_back
                     (lexer.symtab_context.curr_scope ());

                    if (! lexer.reading_script_file
                        && parser.curr_fcn_depth == 1
                        && ! parser.parsing_subfunctions)
                      parser.primary_fcn_scope
                        = lexer.symtab_context.curr_scope ();

                    if (lexer.reading_script_file
                        && parser.curr_fcn_depth > 1)
                      parser.bison_error ("nested functions not implemented in this context");
                  }
                ;

// ===========================
// List of function parameters
// ===========================

param_list_beg  : '('
                  {
                    $$ = 0;
                    lexer.looking_at_parameter_list = true;

                    if (lexer.looking_at_function_handle)
                      {
                        lexer.symtab_context.push (symbol_table::alloc_scope ());
                        lexer.looking_at_function_handle--;
                        lexer.looking_at_anon_fcn_args = true;
                      }
                  }
                ;

param_list_end  : ')'
                  {
                    $$ = 0;
                    lexer.looking_at_parameter_list = false;
                    lexer.looking_for_object_index = false;
                  }
                ;

param_list      : param_list_beg param_list1 param_list_end
                  {
                    if ($2)
                      lexer.mark_as_variables ($2->variable_names ());

                    $$ = $2;
                  }
                | param_list_beg error
                  {
                    parser.bison_error ("invalid parameter list");
                    $$ = 0;
                    ABORT_PARSE;
                  }
                ;

param_list1     : // empty
                  { $$ = 0; }
                | param_list2
                  {
                    $1->mark_as_formal_parameters ();
                    if ($1->validate (tree_parameter_list::in))
                      {
                        lexer.mark_as_variables ($1->variable_names ());
                        $$ = $1;
                      }
                    else
                      {
                        delete $1;
                        ABORT_PARSE;
                      }
                  }
                ;

param_list2     : param_list_elt
                  { $$ = new tree_parameter_list ($1); }
                | param_list2 ',' param_list_elt
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

param_list_elt  : decl2
                  { $$ = $1; }
                | magic_tilde
                  { $$ = new tree_decl_elt ($1); }
                ;

// ===================================
// List of function return value names
// ===================================

return_list     : '[' ']'
                  {
                    lexer.looking_at_return_list = false;

                    $$ = new tree_parameter_list ();
                  }
                | identifier
                  {
                    lexer.looking_at_return_list = false;

                    tree_parameter_list *tmp = new tree_parameter_list ($1);

                    // Even though this parameter list can contain only
                    // a single identifier, we still need to validate it
                    // to check for varargin or varargout.

                    if (tmp->validate (tree_parameter_list::out))
                      $$ = tmp;
                    else
                      {
                        delete tmp;
                        ABORT_PARSE;
                      }
                  }
                | '[' return_list1 ']'
                  {
                    lexer.looking_at_return_list = false;

                    // Check for duplicate parameter names, varargin,
                    // or varargout.

                    if ($2->validate (tree_parameter_list::out))
                      $$ = $2;
                    else
                      {
                        delete $2;
                        ABORT_PARSE;
                      }
                  }
                ;

return_list1    : identifier
                  { $$ = new tree_parameter_list (new tree_decl_elt ($1)); }
                | return_list1 ',' identifier
                  {
                    $1->append (new tree_decl_elt ($3));
                    $$ = $1;
                  }
                ;

// =======================
// Script or function file
// =======================

file            : INPUT_FILE opt_nl opt_list END_OF_INPUT
                  {
                    YYUSE ($2);

                    if (lexer.reading_fcn_file)
                      {
                        // Delete the dummy statement_list we created
                        // after parsing the function.  Any function
                        // definitions found in the file have already
                        // been stored in the symbol table or in
                        // octave_base_parser::primary_fcn_ptr.

                        delete $3;
                      }
                    else
                      {
                        tree_statement *end_of_script
                          = parser.make_end ("endscript", true,
                                             lexer.input_line_number,
                                             lexer.current_input_column);

                        parser.make_script ($3, end_of_script);
                      }

                    $$ = 0;
                  }
                | INPUT_FILE opt_nl classdef opt_sep END_OF_INPUT
                  {
                    YYUSE ($2);
                    YYUSE ($4);

                    if (lexer.reading_classdef_file)
                      parser.classdef_object = $3;

                    $$ = 0;
                  }
                ;

// ===================
// Function definition
// ===================

function_beg    : push_fcn_symtab FCN
                  {
                    $$ = $2;
                    if (lexer.reading_classdef_file
                        || lexer.parsing_classdef)
                      lexer.maybe_classdef_get_set_method = true;
                  }
                ;

function        : function_beg stash_comment function1
                  {
                    $$ = parser.finish_function (0, $3, $2, $1->line (),
                                                 $1->column ());
                    parser.recover_from_parsing_function ();
                  }
                | function_beg stash_comment return_list '=' function1
                  {
                    YYUSE ($4);

                    $$ = parser.finish_function ($3, $5, $2, $1->line (),
                                                 $1->column ());
                    parser.recover_from_parsing_function ();
                  }
                ;

fcn_name        : identifier
                  {
                    std::string id_name = $1->name ();

                    lexer.parsed_function_name.top () = true;
                    lexer.maybe_classdef_get_set_method = false;

                    $$ = $1;
                  }
                | GET '.' identifier
                  {
                    YYUSE ($1);

                    lexer.parsed_function_name.top () = true;
                    lexer.maybe_classdef_get_set_method = false;
                    lexer.parsing_classdef_get_method = true;
                    $$ = $3;
                  }
                | SET '.' identifier
                  {
                    YYUSE ($1);

                    lexer.parsed_function_name.top () = true;
                    lexer.maybe_classdef_get_set_method = false;
                    lexer.parsing_classdef_set_method = true;
                    $$ = $3;
                  }
                ;

function1       : fcn_name function2
                  {
                    std::string fname = $1->name ();

                    delete $1;

                    if (lexer.parsing_classdef_get_method)
                      fname.insert (0, "get.");
                    else if (lexer.parsing_classdef_set_method)
                      fname.insert (0, "set.");

                    lexer.parsing_classdef_get_method = false;
                    lexer.parsing_classdef_set_method = false;

                    $$ = parser.frob_function (fname, $2);
                  }
                ;

function2       : param_list opt_sep opt_list function_end
                  {
                    YYUSE ($2);

                    $$ = parser.start_function ($1, $3, $4);
                  }
                | opt_sep opt_list function_end
                  {
                    YYUSE ($1);

                    $$ = parser.start_function (0, $2, $3);
                  }
                ;

function_end    : END
                  {
                    parser.endfunction_found = true;
                    if (parser.end_token_ok ($1, token::function_end))
                      $$ = parser.make_end ("endfunction", false,
                                            $1->line (), $1->column ());
                    else
                      ABORT_PARSE;
                  }
                | END_OF_INPUT
                  {
// A lot of tests are based on the assumption that this is OK
//                  if (lexer.reading_script_file)
//                    {
//                      parser.bison_error ("function body open at end of script");
//                      YYABORT;
//                    }

                    if (parser.endfunction_found)
                      {
                        parser.bison_error ("inconsistent function endings -- "
                                 "if one function is explicitly ended, "
                                 "so must all the others");
                        YYABORT;
                      }

                    if (! (lexer.reading_fcn_file || lexer.reading_script_file
                           || lexer.input_from_eval_string ()))
                      {
                        parser.bison_error ("function body open at end of input");
                        YYABORT;
                      }

                    if (lexer.reading_classdef_file)
                      {
                        parser.bison_error ("classdef body open at end of input");
                        YYABORT;
                      }

                    $$ = parser.make_end ("endfunction", true,
                                          lexer.input_line_number,
                                          lexer.current_input_column);
                  }
                ;

// ========
// Classdef
// ========

classdef_beg    : CLASSDEF
                  {
                    if (! lexer.reading_classdef_file)
                      {
                        parser.bison_error ("classdef must appear inside a file containing only a class definition");
                        YYABORT;
                      }

                    lexer.parsing_classdef = true;
                    $$ = $1;
                  }
                ;

classdef        : classdef_beg stash_comment opt_attr_list identifier opt_superclass_list opt_sep class_body opt_sep END
                  {
                    YYUSE ($6);
                    YYUSE ($8);

                    lexer.parsing_classdef = false;

                    if (! ($$ = parser.make_classdef ($1, $3, $4, $5, $7, $9, $2)))
                      {
                        // make_classdef deleted $3, $4, $5, and $7.
                        ABORT_PARSE;
                      }
                  }
                | classdef_beg stash_comment opt_attr_list identifier opt_superclass_list opt_sep END
                  {
                    YYUSE ($6);

                    lexer.parsing_classdef = false;

                    if (! ($$ = parser.make_classdef ($1, $3, $4, $5, 0, $7, $2)))
                      {
                        // make_classdef deleted $3, $4, and $5.
                        ABORT_PARSE;
                      }
                  }
                ;

opt_attr_list   : // empty
                  { $$ = 0; }
                | '(' attr_list ')'
                  { $$ = $2; }
                ;

attr_list       : attr
                  { $$ = new tree_classdef_attribute_list ($1); }
                | attr_list ',' attr
                  {
                    $1->append ($3);
                    $$ = $1;
                  }
                ;

attr            : identifier
                  { $$ = new tree_classdef_attribute ($1); }
                | identifier '=' decl_param_init expression
                  {
                    YYUSE ($2);

                    lexer.looking_at_initializer_expression = false;
                    $$ = new tree_classdef_attribute ($1, $4);
                  }
                | EXPR_NOT identifier
                  {
                    YYUSE ($1);

                    $$ = new tree_classdef_attribute ($2, false);
                  }
                ;

opt_superclass_list
                : // empty
                  { $$ = 0; }
                | superclass_list
                  { $$ = $1; }
                ;

superclass_list : EXPR_LT
                  {
                    YYUSE ($1);

                    lexer.enable_fq_identifier ();
                  }
                  superclass
                  { $$ = new tree_classdef_superclass_list ($3); }
                | superclass_list EXPR_AND
                  {
                    YYUSE ($2);

                    lexer.enable_fq_identifier ();
                  }
                  superclass
                  {
                    $1->append ($4);
                    $$ = $1;
                  }
                ;

superclass      : FQ_IDENT
                  { $$ = new tree_classdef_superclass ($1->text ()); }
                ;

class_body      : properties_block
                  { $$ = new tree_classdef_body ($1); }
                | methods_block
                  { $$ = new tree_classdef_body ($1); }
                | events_block
                  { $$ = new tree_classdef_body ($1); }
                | enum_block
                  { $$ = new tree_classdef_body ($1); }
                | class_body opt_sep properties_block
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                | class_body opt_sep methods_block
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                | class_body opt_sep events_block
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                | class_body opt_sep enum_block
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                ;

properties_block
                : PROPERTIES stash_comment opt_attr_list opt_sep property_list opt_sep END
                  {
                    YYUSE ($4);
                    YYUSE ($6);

                    if (! ($$ = parser.make_classdef_properties_block
                           ($1, $3, $5, $7, $2)))
                      {
                        // make_classdef_properties_block delete $3 and $5.
                        ABORT_PARSE;
                      }
                  }
                | PROPERTIES stash_comment opt_attr_list opt_sep END
                  {
                    YYUSE ($4);

                    if (! ($$ = parser.make_classdef_properties_block
                           ($1, $3, 0, $5, $2)))
                      {
                        // make_classdef_properties_block delete $3.
                        ABORT_PARSE;
                      }
                  }
                ;

property_list
                : class_property
                  { $$ = new tree_classdef_property_list ($1); }
                | property_list opt_sep class_property
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                ;

class_property  : identifier
                  { $$ = new tree_classdef_property ($1); }
                | identifier '=' decl_param_init expression ';'
                  {
                    YYUSE ($2);

                    lexer.looking_at_initializer_expression = false;
                    $$ = new tree_classdef_property ($1, $4);
                  }
                ;

methods_block   : METHODS stash_comment opt_attr_list opt_sep methods_list opt_sep END
                  {
                    YYUSE ($4);
                    YYUSE ($6);

                    if (! ($$ = parser.make_classdef_methods_block
                           ($1, $3, $5, $7, $2)))
                      {
                        // make_classdef_methods_block deleted $3 and $5.
                        ABORT_PARSE;
                      }
                  }
                | METHODS stash_comment opt_attr_list opt_sep END
                  {
                    YYUSE ($4);

                    if (! ($$ = parser.make_classdef_methods_block
                           ($1, $3, 0, $5, $2)))
                      {
                        // make_classdef_methods_block deleted $3.
                        ABORT_PARSE;
                      }
                  }
                ;
                ;

method_decl1    : identifier
                  {
                    if (! ($$ = parser.start_classdef_external_method ($1, 0)))
                      ABORT_PARSE;
                  }
                | identifier param_list
                  {
                    if (! ($$ = parser.start_classdef_external_method ($1, $2)))
                      ABORT_PARSE;
                  }
                ;

method_decl     : stash_comment method_decl1
                  { $$ = parser.finish_classdef_external_method ($2, 0, $1); }
                | stash_comment return_list '='
                  {
                    YYUSE ($3);

                    lexer.defining_func++;
                    lexer.parsed_function_name.push (false);
                  }
                  method_decl1
                  {
                    lexer.defining_func--;
                    lexer.parsed_function_name.pop ();
                    $$ = parser.finish_classdef_external_method ($5, $2, $1);
                  }
                ;

method          : method_decl
                  { $$ = $1; }
                | function
                  { $$ = $1; }
                ;

methods_list    : method
                  {
                    octave_value fcn;
                    if ($1)
                      fcn = $1->function ();
                    delete $1;
                    $$ = new tree_classdef_methods_list (fcn);
                  }
                | methods_list opt_sep method
                  {
                    YYUSE ($2);

                    octave_value fcn;
                    if ($3)
                      fcn = $3->function ();
                    delete $3;

                    $1->append (fcn);
                    $$ = $1;
                  }
                ;

events_block    : EVENTS stash_comment opt_attr_list opt_sep events_list opt_sep END
                  {
                    YYUSE ($4);
                    YYUSE ($6);

                    if (! ($$ = parser.make_classdef_events_block
                           ($1, $3, $5, $7, $2)))
                      {
                        // make_classdef_events_block deleted $3 and $5.
                        ABORT_PARSE;
                      }
                  }
                | EVENTS stash_comment opt_attr_list opt_sep END
                  {
                    YYUSE ($4);

                    if (! ($$ = parser.make_classdef_events_block
                           ($1, $3, 0, $5, $2)))
                      {
                        // make_classdef_events_block deleted $3.
                        ABORT_PARSE;
                      }
                  }
                ;

events_list     : class_event
                  { $$ = new tree_classdef_events_list ($1); }
                | events_list opt_sep class_event
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                ;

class_event     : identifier
                  { $$ = new tree_classdef_event ($1); }
                ;

enum_block      : ENUMERATION stash_comment opt_attr_list opt_sep enum_list opt_sep END
                  {
                    YYUSE ($4);
                    YYUSE ($6);

                    if (! ($$ = parser.make_classdef_enum_block
                           ($1, $3, $5, $7, $2)))
                      {
                        // make_classdef_enum_block deleted $3 and $5.
                        ABORT_PARSE;
                      }
                  }
                | ENUMERATION stash_comment opt_attr_list opt_sep END
                  {
                    YYUSE ($4);

                    if (! ($$ = parser.make_classdef_enum_block
                           ($1, $3, 0, $5, $2)))
                      {
                        // make_classdef_enum_block deleted $3.
                        ABORT_PARSE;
                      }
                  }
                ;

enum_list       : class_enum
                  { $$ = new tree_classdef_enum_list ($1); }
                | enum_list opt_sep class_enum
                  {
                    YYUSE ($2);

                    $1->append ($3);
                    $$ = $1;
                  }
                ;

class_enum      : identifier '(' expression ')'
                  { $$ = new tree_classdef_enum ($1, $3); }
                ;

// =============
// Miscellaneous
// =============

stmt_begin      : // empty
                  {
                    $$ = 0;
                    lexer.at_beginning_of_statement = true;
                  }
                ;

stash_comment   : // empty
                  { $$ = lexer.get_comment (); }
                ;

parse_error     : LEXICAL_ERROR
                  {
                    $$ = 0;
                    parser.bison_error ("parse error");
                  }
                | error
                  { $$ = 0; }
                ;

sep_no_nl       : ','
                  { $$ = ','; }
                | ';'
                  { $$ = ';'; }
                | sep_no_nl ','
                  { $$ = $1; }
                | sep_no_nl ';'
                  { $$ = $1; }
                ;

opt_sep_no_nl   : // empty
                  { $$ = 0; }
                | sep_no_nl
                  { $$ = $1; }
                ;

opt_nl          : // empty
                  { $$ = 0; }
                | nl
                  { $$ = $1; }
                ;

nl              : '\n'
                  { $$ = '\n'; }
                | nl '\n'
                  { $$ = $1; }
                ;

sep             : ','
                  { $$ = ','; }
                | ';'
                  { $$ = ';'; }
                | '\n'
                  { $$ = '\n'; }
                | sep ','
                  { $$ = $1; }
                | sep ';'
                  { $$ = $1; }
                | sep '\n'
                  { $$ = $1; }
                ;

opt_sep         : // empty
                  { $$ = 0; }
                | sep
                  { $$ = $1; }
                ;

%%

// Generic error messages.

#undef lexer

static void
yyerror (octave_base_parser& parser, const char *s)
{
  parser.bison_error (s);
}

octave_base_parser::~octave_base_parser (void)
{
  delete stmt_list;

  delete &lexer;
}

void
octave_base_parser::reset (void)
{
  endfunction_found = false;
  autoloading = false;
  fcn_file_from_relative_lookup = false;
  parsing_subfunctions = false;
  max_fcn_depth = 0;
  curr_fcn_depth = 0;
  primary_fcn_scope = -1;
  curr_class_name = "";
  curr_package_name = "";
  function_scopes.clear ();
  primary_fcn_ptr  = 0;
  subfunction_names.clear ();

  delete stmt_list;
  stmt_list = 0;

  lexer.reset ();
}

// Error mesages for mismatched end tokens.

void
octave_base_parser::end_error (const char *type, token::end_tok_type ettype,
                               int l, int c)
{
  static const char *fmt
    = "'%s' command matched by '%s' near line %d column %d";

  switch (ettype)
    {
    case token::simple_end:
      error (fmt, type, "end", l, c);
      break;

    case token::classdef_end:
      error (fmt, type, "endclassdef", l, c);
      break;

    case token::enumeration_end:
      error (fmt, type, "endenumeration", l, c);
      break;

    case token::events_end:
      error (fmt, type, "endevents", l, c);
      break;

    case token::for_end:
      error (fmt, type, "endfor", l, c);
      break;

    case token::function_end:
      error (fmt, type, "endfunction", l, c);
      break;

    case token::if_end:
      error (fmt, type, "endif", l, c);
      break;

    case token::methods_end:
      error (fmt, type, "endmethods", l, c);
      break;

    case token::parfor_end:
      error (fmt, type, "endparfor", l, c);
      break;

    case token::properties_end:
      error (fmt, type, "endproperties", l, c);
      break;

    case token::switch_end:
      error (fmt, type, "endswitch", l, c);
      break;

    case token::try_catch_end:
      error (fmt, type, "end_try_catch", l, c);
      break;

    case token::unwind_protect_end:
      error (fmt, type, "end_unwind_protect", l, c);
      break;

    case token::while_end:
      error (fmt, type, "endwhile", l, c);
      break;

    default:
      panic_impossible ();
      break;
    }
}

// Check to see that end tokens are properly matched.

bool
octave_base_parser::end_token_ok (token *tok, token::end_tok_type expected)
{
  bool retval = true;

  token::end_tok_type ettype = tok->ettype ();

  if (ettype != expected && ettype != token::simple_end)
    {
      retval = false;

      bison_error ("parse error");

      int l = tok->line ();
      int c = tok->column ();

      switch (expected)
        {
        case token::classdef_end:
          end_error ("classdef", ettype, l, c);
          break;

        case token::enumeration_end:
          end_error ("enumeration", ettype, l, c);
          break;

        case token::events_end:
          end_error ("events", ettype, l, c);
          break;

        case token::for_end:
          end_error ("for", ettype, l, c);
          break;

        case token::function_end:
          end_error ("function", ettype, l, c);
          break;

        case token::if_end:
          end_error ("if", ettype, l, c);
          break;

        case token::methods_end:
          end_error ("methods", ettype, l, c);
          break;

        case token::parfor_end:
          end_error ("parfor", ettype, l, c);
          break;

        case token::properties_end:
          end_error ("properties", ettype, l, c);
          break;

        case token::switch_end:
          end_error ("switch", ettype, l, c);
          break;

        case token::try_catch_end:
          end_error ("try", ettype, l, c);
          break;

        case token::unwind_protect_end:
          end_error ("unwind_protect", ettype, l, c);
          break;

        case token::while_end:
          end_error ("while", ettype, l, c);
          break;

        default:
          panic_impossible ();
          break;
        }
    }

  return retval;
}

// Maybe print a warning if an assignment expression is used as the
// test in a logical expression.

void
octave_base_parser::maybe_warn_assign_as_truth_value (tree_expression *expr)
{
  if (expr->is_assignment_expression ()
      && expr->paren_count () < 2)
    {
      if (lexer.fcn_file_full_name.empty ())
        warning_with_id
          ("Octave:assign-as-truth-value",
           "suggest parenthesis around assignment used as truth value");
      else
        warning_with_id
          ("Octave:assign-as-truth-value",
           "suggest parenthesis around assignment used as truth value near line %d, column %d in file '%s'",
           expr->line (), expr->column (), lexer.fcn_file_full_name.c_str ());
    }
}

// Maybe print a warning about switch labels that aren't constants.

void
octave_base_parser::maybe_warn_variable_switch_label (tree_expression *expr)
{
  if (! expr->is_constant ())
    {
      if (lexer.fcn_file_full_name.empty ())
        warning_with_id ("Octave:variable-switch-label",
                         "variable switch label");
      else
        warning_with_id
          ("Octave:variable-switch-label",
           "variable switch label near line %d, column %d in file '%s'",
           expr->line (), expr->column (), lexer.fcn_file_full_name.c_str ());
    }
}

// Finish building a range.

tree_expression *
octave_base_parser::finish_colon_expression (tree_colon_expression *e)
{
  tree_expression *retval = e;

  unwind_protect frame;

  frame.protect_var (error_state);
  frame.protect_var (warning_state);

  frame.protect_var (discard_error_messages);
  frame.protect_var (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  tree_expression *base = e->base ();
  tree_expression *limit = e->limit ();
  tree_expression *incr = e->increment ();

  if (base)
    {
      if (limit)
        {
          if (base->is_constant () && limit->is_constant ()
              && (! incr || (incr && incr->is_constant ())))
            {
              octave_value tmp = e->rvalue1 ();

              if (! (error_state || warning_state))
                {
                  tree_constant *tc_retval
                    = new tree_constant (tmp, base->line (), base->column ());

                  std::ostringstream buf;

                  tree_print_code tpc (buf);

                  e->accept (tpc);

                  tc_retval->stash_original_text (buf.str ());

                  delete e;

                  retval = tc_retval;
                }
            }
        }
      else
        {
          e->preserve_base ();
          delete e;

          retval = base;
        }
    }

  return retval;
}

// Make a constant.

tree_constant *
octave_base_parser::make_constant (int op, token *tok_val)
{
  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_constant *retval = 0;

  switch (op)
    {
    case NUM:
      {
        octave_value tmp (tok_val->number ());
        retval = new tree_constant (tmp, l, c);
        retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case IMAG_NUM:
      {
        octave_value tmp (Complex (0.0, tok_val->number ()));
        retval = new tree_constant (tmp, l, c);
        retval->stash_original_text (tok_val->text_rep ());
      }
      break;

    case DQ_STRING:
    case SQ_STRING:
      {
        std::string txt = tok_val->text ();

        char delim = op == DQ_STRING ? '"' : '\'';
        octave_value tmp (txt, delim);

        if (txt.empty ())
          {
            if (op == DQ_STRING)
              tmp = octave_null_str::instance;
            else
              tmp = octave_null_sq_str::instance;
          }

        retval = new tree_constant (tmp, l, c);

        if (op == DQ_STRING)
          txt = undo_string_escapes (txt);

        // FIXME -- maybe this should also be handled by
        // tok_val->text_rep () for character strings?
        retval->stash_original_text (delim + txt + delim);
      }
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

// Make a function handle.

tree_fcn_handle *
octave_base_parser::make_fcn_handle (token *tok_val)
{
  int l = tok_val->line ();
  int c = tok_val->column ();

  tree_fcn_handle *retval = new tree_fcn_handle (tok_val->text (), l, c);

  return retval;
}

// Make an anonymous function handle.

tree_anon_fcn_handle *
octave_base_parser::make_anon_fcn_handle (tree_parameter_list *param_list,
                                          tree_statement *stmt)
{
  // FIXME -- need to get these from the location of the @ symbol.
  int l = lexer.input_line_number;
  int c = lexer.current_input_column;

  tree_parameter_list *ret_list = 0;

  symbol_table::scope_id fcn_scope = lexer.symtab_context.curr_scope ();

  lexer.symtab_context.pop ();

  stmt->set_print_flag (false);

  tree_statement_list *body = new tree_statement_list (stmt);

  body->mark_as_anon_function_body ();

  tree_anon_fcn_handle *retval
    = new tree_anon_fcn_handle (param_list, ret_list, body, fcn_scope, l, c);
  // FIXME: Stash the filename.  This does not work and produces
  // errors when executed.
  //retval->stash_file_name (lexer.fcn_file_name);

  return retval;
}

// Build a binary expression.

tree_expression *
octave_base_parser::make_binary_op (int op, tree_expression *op1,
                                    token *tok_val, tree_expression *op2)
{
  octave_value::binary_op t = octave_value::unknown_binary_op;

  switch (op)
    {
    case POW:
      t = octave_value::op_pow;
      break;

    case EPOW:
      t = octave_value::op_el_pow;
      break;

    case '+':
      t = octave_value::op_add;
      break;

    case '-':
      t = octave_value::op_sub;
      break;

    case '*':
      t = octave_value::op_mul;
      break;

    case '/':
      t = octave_value::op_div;
      break;

    case EMUL:
      t = octave_value::op_el_mul;
      break;

    case EDIV:
      t = octave_value::op_el_div;
      break;

    case LEFTDIV:
      t = octave_value::op_ldiv;
      break;

    case ELEFTDIV:
      t = octave_value::op_el_ldiv;
      break;

    case LSHIFT:
      t = octave_value::op_lshift;
      break;

    case RSHIFT:
      t = octave_value::op_rshift;
      break;

    case EXPR_LT:
      t = octave_value::op_lt;
      break;

    case EXPR_LE:
      t = octave_value::op_le;
      break;

    case EXPR_EQ:
      t = octave_value::op_eq;
      break;

    case EXPR_GE:
      t = octave_value::op_ge;
      break;

    case EXPR_GT:
      t = octave_value::op_gt;
      break;

    case EXPR_NE:
      t = octave_value::op_ne;
      break;

    case EXPR_AND:
      t = octave_value::op_el_and;
      break;

    case EXPR_OR:
      t = octave_value::op_el_or;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return maybe_compound_binary_expression (op1, op2, l, c, t);
}

// Build a boolean expression.

tree_expression *
octave_base_parser::make_boolean_op (int op, tree_expression *op1,
                                     token *tok_val, tree_expression *op2)
{
  tree_boolean_expression::type t;

  switch (op)
    {
    case EXPR_AND_AND:
      t = tree_boolean_expression::bool_and;
      break;

    case EXPR_OR_OR:
      t = tree_boolean_expression::bool_or;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_boolean_expression (op1, op2, l, c, t);
}

// Build a prefix expression.

tree_expression *
octave_base_parser::make_prefix_op (int op, tree_expression *op1,
                                    token *tok_val)
{
  octave_value::unary_op t = octave_value::unknown_unary_op;

  switch (op)
    {
    case EXPR_NOT:
      t = octave_value::op_not;
      break;

    case '+':
      t = octave_value::op_uplus;
      break;

    case '-':
      t = octave_value::op_uminus;
      break;

    case PLUS_PLUS:
      t = octave_value::op_incr;
      break;

    case MINUS_MINUS:
      t = octave_value::op_decr;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_prefix_expression (op1, l, c, t);
}

// Build a postfix expression.

tree_expression *
octave_base_parser::make_postfix_op (int op, tree_expression *op1,
                                     token *tok_val)
{
  octave_value::unary_op t = octave_value::unknown_unary_op;

  switch (op)
    {
    case HERMITIAN:
      t = octave_value::op_hermitian;
      break;

    case TRANSPOSE:
      t = octave_value::op_transpose;
      break;

    case PLUS_PLUS:
      t = octave_value::op_incr;
      break;

    case MINUS_MINUS:
      t = octave_value::op_decr;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = tok_val->line ();
  int c = tok_val->column ();

  return new tree_postfix_expression (op1, l, c, t);
}

// Build an unwind-protect command.

tree_command *
octave_base_parser::make_unwind_command (token *unwind_tok,
                                         tree_statement_list *body,
                                         tree_statement_list *cleanup_stmts,
                                         token *end_tok,
                                         octave_comment_list *lc,
                                         octave_comment_list *mc)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::unwind_protect_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = unwind_tok->line ();
      int c = unwind_tok->column ();

      retval = new tree_unwind_protect_command (body, cleanup_stmts,
                                                lc, mc, tc, l, c);
    }
  else
    {
      delete body;
      delete cleanup_stmts;
    }

  return retval;
}

// Build a try-catch command.

tree_command *
octave_base_parser::make_try_command (token *try_tok,
                                      tree_statement_list *body,
                                      char catch_sep,
                                      tree_statement_list *cleanup_stmts,
                                      token *end_tok,
                                      octave_comment_list *lc,
                                      octave_comment_list *mc)
{
  tree_command *retval = 0;

  if (end_token_ok (end_tok, token::try_catch_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = try_tok->line ();
      int c = try_tok->column ();

      tree_identifier *id = 0;

      if (! catch_sep && cleanup_stmts && ! cleanup_stmts->empty ())
        {
          tree_statement *stmt = cleanup_stmts->front ();

          if (stmt)
            {
              tree_expression *expr = stmt->expression ();

              if (expr && expr->is_identifier ())
                {
                  id = dynamic_cast<tree_identifier *> (expr);

                  cleanup_stmts->pop_front ();

                  stmt->set_expression (0);
                  delete stmt;
                }
            }
        }

      retval = new tree_try_catch_command (body, cleanup_stmts, id,
                                           lc, mc, tc, l, c);
    }
  else
    {
      delete body;
      delete cleanup_stmts;
    }

  return retval;
}

// Build a while command.

tree_command *
octave_base_parser::make_while_command (token *while_tok,
                                        tree_expression *expr,
                                        tree_statement_list *body,
                                        token *end_tok,
                                        octave_comment_list *lc)
{
  tree_command *retval = 0;

  maybe_warn_assign_as_truth_value (expr);

  if (end_token_ok (end_tok, token::while_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      lexer.looping--;

      int l = while_tok->line ();
      int c = while_tok->column ();

      retval = new tree_while_command (expr, body, lc, tc, l, c);
    }
  else
    {
      delete expr;
      delete body;
    }

  return retval;
}

// Build a do-until command.

tree_command *
octave_base_parser::make_do_until_command (token *until_tok,
                                           tree_statement_list *body,
                                           tree_expression *expr,
                                           octave_comment_list *lc)
{
  maybe_warn_assign_as_truth_value (expr);

  octave_comment_list *tc = lexer.comment_buf.get_comment ();

  lexer.looping--;

  int l = until_tok->line ();
  int c = until_tok->column ();

  return new tree_do_until_command (expr, body, lc, tc, l, c);
}

// Build a for command.

tree_command *
octave_base_parser::make_for_command (int tok_id, token *for_tok,
                                      tree_argument_list *lhs,
                                      tree_expression *expr,
                                      tree_expression *maxproc,
                                      tree_statement_list *body,
                                      token *end_tok,
                                      octave_comment_list *lc)
{
  tree_command *retval = 0;

  bool parfor = tok_id == PARFOR;

  if (end_token_ok (end_tok, parfor ? token::parfor_end : token::for_end))
    {
      expr->mark_as_for_cmd_expr ();

      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      lexer.looping--;

      int l = for_tok->line ();
      int c = for_tok->column ();

      if (lhs->length () == 1)
        {
          tree_expression *tmp = lhs->remove_front ();

          retval = new tree_simple_for_command (parfor, tmp, expr, maxproc,
                                                body, lc, tc, l, c);

          delete lhs;
        }
      else
        {
          if (parfor)
            bison_error ("invalid syntax for parfor statement");
          else
            retval = new tree_complex_for_command (lhs, expr, body,
                                                   lc, tc, l, c);
        }
    }
  else
    {
      delete lhs;
      delete expr;
      delete maxproc;
      delete body;
    }

  return retval;
}

// Build a break command.

tree_command *
octave_base_parser::make_break_command (token *break_tok)
{
  int l = break_tok->line ();
  int c = break_tok->column ();

  return new tree_break_command (l, c);
}

// Build a continue command.

tree_command *
octave_base_parser::make_continue_command (token *continue_tok)
{
  int l = continue_tok->line ();
  int c = continue_tok->column ();

  return new tree_continue_command (l, c);
}

// Build a return command.

tree_command *
octave_base_parser::make_return_command (token *return_tok)
{
  int l = return_tok->line ();
  int c = return_tok->column ();

  return new tree_return_command (l, c);
}

// Start an if command.

tree_if_command_list *
octave_base_parser::start_if_command (tree_expression *expr,
                                      tree_statement_list *list)
{
  maybe_warn_assign_as_truth_value (expr);

  tree_if_clause *t = new tree_if_clause (expr, list);

  return new tree_if_command_list (t);
}

// Finish an if command.

tree_if_command *
octave_base_parser::finish_if_command (token *if_tok,
                                       tree_if_command_list *list,
                                       token *end_tok,
                                       octave_comment_list *lc)
{
  tree_if_command *retval = 0;

  if (end_token_ok (end_tok, token::if_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = if_tok->line ();
      int c = if_tok->column ();

      if (list && ! list->empty ())
        {
          tree_if_clause *elt = list->front ();

          if (elt)
            {
              elt->line (l);
              elt->column (c);
            }
        }

      retval = new tree_if_command (list, lc, tc, l, c);
    }
  else
    delete list;

  return retval;
}

// Build an elseif clause.

tree_if_clause *
octave_base_parser::make_elseif_clause (token *elseif_tok,
                                        tree_expression *expr,
                                        tree_statement_list *list,
                                        octave_comment_list *lc)
{
  maybe_warn_assign_as_truth_value (expr);

  int l = elseif_tok->line ();
  int c = elseif_tok->column ();

  return new tree_if_clause (expr, list, lc, l, c);
}

// Finish a switch command.

tree_switch_command *
octave_base_parser::finish_switch_command (token *switch_tok,
                                           tree_expression *expr,
                                           tree_switch_case_list *list,
                                           token *end_tok,
                                           octave_comment_list *lc)
{
  tree_switch_command *retval = 0;

  if (end_token_ok (end_tok, token::switch_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = switch_tok->line ();
      int c = switch_tok->column ();

      if (list && ! list->empty ())
        {
          tree_switch_case *elt = list->front ();

          if (elt)
            {
              elt->line (l);
              elt->column (c);
            }
        }

      retval = new tree_switch_command (expr, list, lc, tc, l, c);
    }
  else
    {
      delete expr;
      delete list;
    }

  return retval;
}

// Build a switch case.

tree_switch_case *
octave_base_parser::make_switch_case (token *case_tok,
                                      tree_expression *expr,
                                      tree_statement_list *list,
                                      octave_comment_list *lc)
{
  maybe_warn_variable_switch_label (expr);

  int l = case_tok->line ();
  int c = case_tok->column ();

  return new tree_switch_case (expr, list, lc, l, c);
}

// Build an assignment to a variable.

tree_expression *
octave_base_parser::make_assign_op (int op, tree_argument_list *lhs,
                                    token *eq_tok, tree_expression *rhs)
{
  tree_expression *retval = 0;

  octave_value::assign_op t = octave_value::unknown_assign_op;

  switch (op)
    {
    case '=':
      t = octave_value::op_asn_eq;
      break;

    case ADD_EQ:
      t = octave_value::op_add_eq;
      break;

    case SUB_EQ:
      t = octave_value::op_sub_eq;
      break;

    case MUL_EQ:
      t = octave_value::op_mul_eq;
      break;

    case DIV_EQ:
      t = octave_value::op_div_eq;
      break;

    case LEFTDIV_EQ:
      t = octave_value::op_ldiv_eq;
      break;

    case POW_EQ:
      t = octave_value::op_pow_eq;
      break;

    case LSHIFT_EQ:
      t = octave_value::op_lshift_eq;
      break;

    case RSHIFT_EQ:
      t = octave_value::op_rshift_eq;
      break;

    case EMUL_EQ:
      t = octave_value::op_el_mul_eq;
      break;

    case EDIV_EQ:
      t = octave_value::op_el_div_eq;
      break;

    case ELEFTDIV_EQ:
      t = octave_value::op_el_ldiv_eq;
      break;

    case EPOW_EQ:
      t = octave_value::op_el_pow_eq;
      break;

    case AND_EQ:
      t = octave_value::op_el_and_eq;
      break;

    case OR_EQ:
      t = octave_value::op_el_or_eq;
      break;

    default:
      panic_impossible ();
      break;
    }

  int l = eq_tok->line ();
  int c = eq_tok->column ();

  if (lhs->is_simple_assign_lhs ())
    {
      tree_expression *tmp = lhs->remove_front ();

      retval = new tree_simple_assignment (tmp, rhs, false, l, c, t);

      delete lhs;
    }
  else if (t == octave_value::op_asn_eq)
    return new tree_multi_assignment (lhs, rhs, false, l, c);
  else
    bison_error ("computed multiple assignment not allowed");

  return retval;
}

// Define a script.

void
octave_base_parser::make_script (tree_statement_list *cmds,
                                 tree_statement *end_script)
{
  if (! cmds)
    cmds = new tree_statement_list ();

  cmds->append (end_script);

  octave_user_script *script
    = new octave_user_script (lexer.fcn_file_full_name,
                              lexer.fcn_file_name,
                              cmds, lexer.help_text);

  lexer.help_text = "";

  octave_time now;

  script->stash_fcn_file_time (now);

  primary_fcn_ptr = script;
}

// Begin defining a function.

octave_user_function *
octave_base_parser::start_function (tree_parameter_list *param_list,
                                    tree_statement_list *body,
                                    tree_statement *end_fcn_stmt)
{
  // We'll fill in the return list later.

  if (! body)
    body = new tree_statement_list ();

  body->append (end_fcn_stmt);

  octave_user_function *fcn
    = new octave_user_function (lexer.symtab_context.curr_scope (),
                                param_list, 0, body);

  if (fcn)
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      fcn->stash_trailing_comment (tc);
      fcn->stash_fcn_end_location (end_fcn_stmt->line (),
                                   end_fcn_stmt->column ());
    }

  return fcn;
}

tree_statement *
octave_base_parser::make_end (const std::string& type, bool eof, int l, int c)
{
  return make_statement (new tree_no_op_command (type, eof, l, c));
}

// Do most of the work for defining a function.

octave_user_function *
octave_base_parser::frob_function (const std::string& fname,
                                   octave_user_function *fcn)
{
  std::string id_name = fname;

  // If input is coming from a file, issue a warning if the name of
  // the file does not match the name of the function stated in the
  // file.  Matlab doesn't provide a diagnostic (it ignores the stated
  // name).
  if (! autoloading && lexer.reading_fcn_file
      && curr_fcn_depth == 1 && ! parsing_subfunctions)
  {
    // FIXME -- should lexer.fcn_file_name already be
    // preprocessed when we get here?  It seems to only be a
    // problem with relative file names.

    std::string nm = lexer.fcn_file_name;

    size_t pos = nm.find_last_of (file_ops::dir_sep_chars ());

    if (pos != std::string::npos)
      nm = lexer.fcn_file_name.substr (pos+1);

    if (nm != id_name)
      {
        warning_with_id
          ("Octave:function-name-clash",
           "function name '%s' does not agree with function file name '%s'",
           id_name.c_str (), lexer.fcn_file_full_name.c_str ());

        id_name = nm;
      }
  }

  if (lexer.reading_fcn_file || lexer.reading_classdef_file || autoloading)
    {
      octave_time now;

      fcn->stash_fcn_file_name (lexer.fcn_file_full_name);
      fcn->stash_fcn_file_time (now);
      fcn->mark_as_system_fcn_file ();

      if (fcn_file_from_relative_lookup)
        fcn->mark_relative ();

      if (curr_fcn_depth > 1 || parsing_subfunctions)
        {
          fcn->stash_parent_fcn_name (lexer.fcn_file_name);

          if (curr_fcn_depth > 1)
            fcn->stash_parent_fcn_scope (function_scopes[function_scopes.size ()-2]);
          else
            fcn->stash_parent_fcn_scope (primary_fcn_scope);
        }

      if (lexer.parsing_class_method)
        {
          if (curr_class_name == id_name)
            fcn->mark_as_class_constructor ();
          else
            fcn->mark_as_class_method ();

          fcn->stash_dispatch_class (curr_class_name);
        }

      std::string nm = fcn->fcn_file_name ();

      file_stat fs (nm);

      if (fs && fs.is_newer (now))
        warning_with_id ("Octave:future-time-stamp",
                         "time stamp for '%s' is in the future", nm.c_str ());
    }
  else if (! input_from_tmp_history_file
           && ! lexer.force_script
           && lexer.reading_script_file
           && lexer.fcn_file_name == id_name)
    {
      warning ("function '%s' defined within script file '%s'",
               id_name.c_str (), lexer.fcn_file_full_name.c_str ());
    }

  fcn->stash_function_name (id_name);

  if (! lexer.help_text.empty () && curr_fcn_depth == 1
      && ! parsing_subfunctions)
    {
      fcn->document (lexer.help_text);

      lexer.help_text = "";
    }

  if (lexer.reading_fcn_file && curr_fcn_depth == 1
      && ! parsing_subfunctions)
    primary_fcn_ptr = fcn;

  return fcn;
}

tree_function_def *
octave_base_parser::finish_function (tree_parameter_list *ret_list,
                                     octave_user_function *fcn,
                                     octave_comment_list *lc,
                                     int l, int c)
{
  tree_function_def *retval = 0;

  if (ret_list)
    ret_list->mark_as_formal_parameters ();

  if (fcn)
    {
      std::string nm = fcn->name ();
      std::string file = fcn->fcn_file_name ();

      std::string tmp = nm;
      if (! file.empty ())
        tmp += ": " + file;

      symbol_table::cache_name (fcn->scope (), tmp);

      if (lc)
        fcn->stash_leading_comment (lc);

      fcn->define_ret_list (ret_list);

      if (curr_fcn_depth > 1 || parsing_subfunctions)
        {
          fcn->mark_as_subfunction ();
          fcn->stash_fcn_location (l, c);

          subfunction_names.push_back (nm);

          if (endfunction_found && function_scopes.size () > 1)
            {
              symbol_table::scope_id pscope
                = function_scopes[function_scopes.size ()-2];

              symbol_table::install_nestfunction (nm, octave_value (fcn),
                                                  pscope);
            }
          else
            symbol_table::install_subfunction (nm, octave_value (fcn),
                                               primary_fcn_scope);
        }

      if (curr_fcn_depth == 1 && fcn)
        symbol_table::update_nest (fcn->scope ());

      if (! lexer.reading_fcn_file && curr_fcn_depth == 1)
        {
          // We are either reading a script file or defining a function
          // at the command line, so this definition creates a
          // tree_function object that is placed in the parse tree.
          // Otherwise, it is just inserted in the symbol table,
          // either as a subfunction or nested function (see above),
          // or as the primary function for the file, via
          // primary_fcn_ptr (see also load_fcn_from_file,,
          // parse_fcn_file, and
          // symbol_table::fcn_info::fcn_info_rep::find_user_function).

          retval = new tree_function_def (fcn);
        }
    }

  return retval;
}

void
octave_base_parser::recover_from_parsing_function (void)
{
  lexer.symtab_context.pop ();

  if (lexer.reading_fcn_file && curr_fcn_depth == 1
      && ! parsing_subfunctions)
    parsing_subfunctions = true;

  curr_fcn_depth--;
  function_scopes.pop_back ();

  lexer.defining_func--;
  lexer.parsed_function_name.pop ();
  lexer.looking_at_return_list = false;
  lexer.looking_at_parameter_list = false;
}

tree_funcall *
octave_base_parser::make_superclass_ref (const std::string& method_nm,
                                         const std::string& class_nm)
{
  octave_value_list args;

  args(1) = class_nm;
  args(0) = method_nm;

  octave_value fcn
    = symbol_table::find_built_in_function ("__superclass_reference__");

  return new tree_funcall (fcn, args);
}

tree_funcall *
octave_base_parser::make_meta_class_query (const std::string& class_nm)
{
  octave_value_list args;

  args(0) = class_nm;

  octave_value fcn
    = symbol_table::find_built_in_function ("__meta_class_query__");

  return new tree_funcall (fcn, args);
}

// A CLASSDEF block defines a class that has a constructor and other
// methods, but it is not an executable command.  Parsing the block
// makes some changes in the symbol table (inserting the constructor
// and methods, and adding to the list of known objects) and creates
// a parse tree containing meta information about the class.

tree_classdef *
octave_base_parser::make_classdef (token *tok_val,
                                   tree_classdef_attribute_list *a,
                                   tree_identifier *id,
                                   tree_classdef_superclass_list *sc,
                                   tree_classdef_body *body, token *end_tok,
                                   octave_comment_list *lc)
{
  tree_classdef *retval = 0;

  std::string cls_name = id->name ();

  std::string nm = lexer.fcn_file_name;

  size_t pos = nm.find_last_of (file_ops::dir_sep_chars ());

  if (pos != std::string::npos)
    nm = lexer.fcn_file_name.substr (pos+1);

  if (nm != cls_name)
    bison_error ("invalid classdef definition, the class name must match the file name");
  else if (end_token_ok (end_tok, token::classdef_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = tok_val->line ();
      int c = tok_val->column ();

      if (! body)
        body = new tree_classdef_body ();

      retval = new tree_classdef (a, id, sc, body, lc, tc,
                                  curr_package_name, l, c);
    }

  if (! retval)
    {
      delete a;
      delete id;
      delete sc;
      delete body;
    }

  return retval;
}

tree_classdef_properties_block *
octave_base_parser::make_classdef_properties_block (token *tok_val,
                                                    tree_classdef_attribute_list *a,
                                                    tree_classdef_property_list *plist,
                                                    token *end_tok,
                                                    octave_comment_list *lc)
{
  tree_classdef_properties_block *retval = 0;

  if (end_token_ok (end_tok, token::properties_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = tok_val->line ();
      int c = tok_val->column ();

      if (! plist)
        plist = new tree_classdef_property_list ();

      retval = new tree_classdef_properties_block (a, plist, lc, tc, l, c);
    }
  else
    {
      delete a;
      delete plist;
    }

  return retval;
}

tree_classdef_methods_block *
octave_base_parser::make_classdef_methods_block (token *tok_val,
                                                 tree_classdef_attribute_list *a,
                                                 tree_classdef_methods_list *mlist,
                                                 token *end_tok,
                                                 octave_comment_list *lc)
{
  tree_classdef_methods_block *retval = 0;

  if (end_token_ok (end_tok, token::methods_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = tok_val->line ();
      int c = tok_val->column ();

      if (! mlist)
        mlist = new tree_classdef_methods_list ();

      retval = new tree_classdef_methods_block (a, mlist, lc, tc, l, c);
    }
  else
    {
      delete a;
      delete mlist;
    }

  return retval;
}

tree_classdef_events_block *
octave_base_parser::make_classdef_events_block (token *tok_val,
                                                tree_classdef_attribute_list *a,
                                                tree_classdef_events_list *elist,
                                                token *end_tok,
                                                octave_comment_list *lc)
{
  tree_classdef_events_block *retval = 0;

  if (end_token_ok (end_tok, token::events_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = tok_val->line ();
      int c = tok_val->column ();

      if (! elist)
        elist = new tree_classdef_events_list ();

      retval = new tree_classdef_events_block (a, elist, lc, tc, l, c);
    }
  else
    {
      delete a;
      delete elist;
    }

  return retval;
}

tree_classdef_enum_block *
octave_base_parser::make_classdef_enum_block (token *tok_val,
                                              tree_classdef_attribute_list *a,
                                              tree_classdef_enum_list *elist,
                                              token *end_tok,
                                              octave_comment_list *lc)
{
  tree_classdef_enum_block *retval = 0;

  if (end_token_ok (end_tok, token::enumeration_end))
    {
      octave_comment_list *tc = lexer.comment_buf.get_comment ();

      int l = tok_val->line ();
      int c = tok_val->column ();

      if (! elist)
        elist = new tree_classdef_enum_list ();

      retval = new tree_classdef_enum_block (a, elist, lc, tc, l, c);
    }
  else
    {
      delete a;
      delete elist;
    }

  return retval;
}

octave_user_function*
octave_base_parser::start_classdef_external_method (tree_identifier *id,
                                                    tree_parameter_list *pl)
{
  octave_user_function* retval = 0;

  // External methods are only allowed within @-folders. In this case,
  // curr_class_name will be non-empty.

  if (! curr_class_name.empty ())
    {

      std::string mname = id->name ();

      // Methods that cannot be declared outside the classdef file:
      // - methods with '.' character (e.g. property accessors)
      // - class constructor
      // - `delete'

      if (mname.find_first_of (".") == std::string::npos
          && mname != "delete"
          && mname != curr_class_name)
        {
          // Create a dummy function that is used until the real method
          // is loaded.

          retval = new octave_user_function (-1, pl);

          retval->stash_function_name (mname);

          int l = id->line ();
          int c = id->column ();

          retval->stash_fcn_location (l, c);
        }
      else
        bison_error ("invalid external method declaration, an external "
                     "method cannot be the class constructor, `delete' "
                     "or have a dot (.) character in its name");
    }
  else
    bison_error ("external methods are only allowed in @-folders");

  if (! retval)
    delete id;

  return retval;
}

tree_function_def *
octave_base_parser::finish_classdef_external_method (octave_user_function *fcn,
                                                     tree_parameter_list *ret_list,
                                                     octave_comment_list *cl)
{
  if (ret_list)
    fcn->define_ret_list (ret_list);

  if (cl)
    fcn->stash_leading_comment (cl);

  int l = fcn->beginning_line ();
  int c = fcn->beginning_column ();

  return new tree_function_def (fcn, l, c);
}

// Make an index expression.

tree_index_expression *
octave_base_parser::make_index_expression (tree_expression *expr,
                                           tree_argument_list *args,
                                           char type)
{
  tree_index_expression *retval = 0;

  if (args && args->has_magic_tilde ())
    {
      bison_error ("invalid use of empty argument (~) in index expression");

      delete expr;
      delete args;

      return retval;
    }

  int l = expr->line ();
  int c = expr->column ();

  if (! expr->is_postfix_indexed ())
    expr->set_postfix_index (type);

  if (expr->is_index_expression ())
    {
      tree_index_expression *tmp = static_cast<tree_index_expression *> (expr);

      tmp->append (args, type);

      retval = tmp;
    }
  else
    retval = new tree_index_expression (expr, args, l, c, type);

  return retval;
}

// Make an indirect reference expression.

tree_index_expression *
octave_base_parser::make_indirect_ref (tree_expression *expr,
                                       const std::string& elt)
{
  tree_index_expression *retval = 0;

  int l = expr->line ();
  int c = expr->column ();

  if (! expr->is_postfix_indexed ())
    expr->set_postfix_index ('.');

  if (expr->is_index_expression ())
    {
      tree_index_expression *tmp = static_cast<tree_index_expression *> (expr);

      tmp->append (elt);

      retval = tmp;
    }
  else
    retval = new tree_index_expression (expr, elt, l, c);

  lexer.looking_at_indirect_ref = false;

  return retval;
}

// Make an indirect reference expression with dynamic field name.

tree_index_expression *
octave_base_parser::make_indirect_ref (tree_expression *expr,
                                       tree_expression *elt)
{
  tree_index_expression *retval = 0;

  int l = expr->line ();
  int c = expr->column ();

  if (! expr->is_postfix_indexed ())
    expr->set_postfix_index ('.');

  if (expr->is_index_expression ())
    {
      tree_index_expression *tmp = static_cast<tree_index_expression *> (expr);

      tmp->append (elt);

      retval = tmp;
    }
  else
    retval = new tree_index_expression (expr, elt, l, c);

  lexer.looking_at_indirect_ref = false;

  return retval;
}

// Make a declaration command.

tree_decl_command *
octave_base_parser::make_decl_command (int tok, token *tok_val,
                                       tree_decl_init_list *lst)
{
  tree_decl_command *retval = 0;

  int l = tok_val->line ();
  int c = tok_val->column ();

  switch (tok)
    {
    case GLOBAL:
      retval = new tree_global_command (lst, l, c);
      break;

    case PERSISTENT:
      if (curr_fcn_depth > 0)
        retval = new tree_persistent_command (lst, l, c);
      else
        {
          if (lexer.reading_script_file)
            warning ("ignoring persistent declaration near line %d of file '%s'",
                     l, lexer.fcn_file_full_name.c_str ());
          else
            warning ("ignoring persistent declaration near line %d", l);
        }
      break;

    default:
      panic_impossible ();
      break;
    }

  return retval;
}

bool
octave_base_parser::validate_array_list (tree_expression *e)
{
  bool retval = true;

  tree_array_list *al = dynamic_cast<tree_array_list *> (e);

  for (tree_array_list::iterator i = al->begin (); i != al->end (); i++)
    {
      tree_argument_list *row = *i;

      if (row && row->has_magic_tilde ())
        {
          retval = false;
          if (e->is_matrix ())
             bison_error ("invalid use of tilde (~) in matrix expression");
           else
             bison_error ("invalid use of tilde (~) in cell expression");
          break;
        }
    }

  return retval;
}

tree_argument_list *
octave_base_parser::validate_matrix_for_assignment (tree_expression *e)
{
  tree_argument_list *retval = 0;

  if (e->is_constant ())
    {
      octave_value ov = e->rvalue1 ();

      if (ov.is_empty ())
        bison_error ("invalid empty left hand side of assignment");
      else
        bison_error ("invalid constant left hand side of assignment");

      delete e;
    }
  else
    {
      bool is_simple_assign = true;

      tree_argument_list *tmp = 0;

      if (e->is_matrix ())
        {
          tree_matrix *mat = dynamic_cast<tree_matrix *> (e);

          if (mat && mat->size () == 1)
            {
              tmp = mat->front ();
              mat->pop_front ();
              delete e;
              is_simple_assign = false;
            }
        }
      else
        tmp = new tree_argument_list (e);

      if (tmp && tmp->is_valid_lvalue_list ())
        {
          lexer.mark_as_variables (tmp->variable_names ());
          retval = tmp;
        }
      else
        {
          bison_error ("invalid left hand side of assignment");
          delete tmp;
        }

      if (retval && is_simple_assign)
        retval->mark_as_simple_assign_lhs ();
    }

  return retval;
}

// Finish building an array_list.

tree_expression *
octave_base_parser::finish_array_list (tree_array_list *array_list)
{
  tree_expression *retval = array_list;

  unwind_protect frame;

  frame.protect_var (error_state);
  frame.protect_var (warning_state);

  frame.protect_var (discard_error_messages);
  frame.protect_var (discard_warning_messages);

  discard_error_messages = true;
  discard_warning_messages = true;

  if (array_list->all_elements_are_constant ())
    {
      octave_value tmp = array_list->rvalue1 ();

      if (! (error_state || warning_state))
        {
          tree_constant *tc_retval
            = new tree_constant (tmp, array_list->line (),
                                 array_list->column ());

          std::ostringstream buf;

          tree_print_code tpc (buf);

          array_list->accept (tpc);

          tc_retval->stash_original_text (buf.str ());

          delete array_list;

          retval = tc_retval;
        }
    }

  return retval;
}

// Finish building a matrix list.

tree_expression *
octave_base_parser::finish_matrix (tree_matrix *m)
{
  return (m
          ? finish_array_list (m)
          : new tree_constant (octave_null_matrix::instance));
}

// Finish building a cell list.

tree_expression *
octave_base_parser::finish_cell (tree_cell *c)
{
  return (c
          ? finish_array_list (c)
          : new tree_constant (octave_value (Cell ())));
}

void
octave_base_parser::maybe_warn_missing_semi (tree_statement_list *t)
{
  if (curr_fcn_depth > 0)
    {
      tree_statement *tmp = t->back ();

      if (tmp->is_expression ())
        warning_with_id
          ("Octave:missing-semicolon",
           "missing semicolon near line %d, column %d in file '%s'",
            tmp->line (), tmp->column (), lexer.fcn_file_full_name.c_str ());
    }
}

tree_statement_list *
octave_base_parser::set_stmt_print_flag (tree_statement_list *list,
                                         char sep, bool warn_missing_semi)
{
  tree_statement *tmp = list->back ();

  switch (sep)
    {
    case ';':
      tmp->set_print_flag (false);
      break;

    case 0:
    case ',':
    case '\n':
      tmp->set_print_flag (true);
      if (warn_missing_semi)
        maybe_warn_missing_semi (list);
      break;

    default:
      warning ("unrecognized separator type!");
      break;
    }

  // Even if a statement is null, we add it to the list then remove it
  // here so that the print flag is applied to the correct statement.

  if (tmp->is_null_statement ())
    {
      list->pop_back ();
      delete tmp;
    }

  return list;
}

// Finish building a statement.
template <class T>
tree_statement *
octave_base_parser::make_statement (T *arg)
{
  octave_comment_list *comment = lexer.get_comment ();

  return new tree_statement (arg, comment);
}

tree_statement_list *
octave_base_parser::make_statement_list (tree_statement *stmt)
{
  return new tree_statement_list (stmt);
}

tree_statement_list *
octave_base_parser::append_statement_list (tree_statement_list *list,
                                           char sep, tree_statement *stmt,
                                           bool warn_missing_semi)
{
  set_stmt_print_flag (list, sep, warn_missing_semi);

  list->append (stmt);

  return list;
}

void
octave_base_parser::bison_error (const char *s)
{
  int err_col = lexer.current_input_column - 1;

  std::ostringstream output_buf;

  if (lexer.reading_fcn_file || lexer.reading_script_file || lexer.reading_classdef_file)
    output_buf << "parse error near line " << lexer.input_line_number
               << " of file " << lexer.fcn_file_full_name;
  else
    output_buf << "parse error:";

  if (s && strcmp (s, "parse error") != 0)
    output_buf << "\n\n  " << s;

  output_buf << "\n\n";

  std::string curr_line = lexer.current_input_line;

  if (! curr_line.empty ())
    {
      size_t len = curr_line.length ();

      if (curr_line[len-1] == '\n')
        curr_line.resize (len-1);

      // Print the line, maybe with a pointer near the error token.

      output_buf << ">>> " << curr_line << "\n";

      if (err_col == 0)
        err_col = len;

      for (int i = 0; i < err_col + 3; i++)
        output_buf << " ";

      output_buf << "^";
    }

  output_buf << "\n";

  std::string msg = output_buf.str ();

  parse_error ("%s", msg.c_str ());
}

int
octave_parser::run (void)
{
  return octave_parse (*this);
}

octave_push_parser::~octave_push_parser (void)
{
  yypstate_delete (static_cast<yypstate *> (parser_state));
}

void
octave_push_parser::init (void)
{
  parser_state = yypstate_new ();
}

// Parse input from INPUT.  Pass TRUE for EOF if the end of INPUT should
// finish the parse.

int
octave_push_parser::run (const std::string& input, bool eof)
{
  int status = -1;

  dynamic_cast<octave_push_lexer&> (lexer).append_input (input, eof);

  do
    {
      YYSTYPE lval;

      int token = octave_lex (&lval, scanner);

      if (token < 0)
        {
          if (! eof && lexer.at_end_of_buffer ())
            {
              status = -1;
              break;
            }
        }

      yypstate *pstate = static_cast<yypstate *> (parser_state);

      status = octave_push_parse (pstate, token, &lval, *this);
    }
  while (status == YYPUSH_MORE);

  return status;
}

static void
safe_fclose (FILE *f)
{
  if (f)
    fclose (static_cast<FILE *> (f));
}

static octave_function *
parse_fcn_file (const std::string& full_file, const std::string& file,
                const std::string& dispatch_type,
                const std::string& package_name,
                bool require_file, bool force_script, bool autoload,
                bool relative_lookup, const std::string& warn_for)
{
  unwind_protect frame;

  octave_function *fcn_ptr = 0;

  // Open function file and parse.

  FILE *in_stream = command_editor::get_input_stream ();

  frame.add_fcn (command_editor::set_input_stream, in_stream);

  frame.add_fcn (command_history::ignore_entries,
                 command_history::ignoring_entries ());

  command_history::ignore_entries ();

  FILE *ffile = 0;

  if (! full_file.empty ())
    ffile = gnulib::fopen (full_file.c_str (), "rb");

  if (ffile)
    {
      frame.add_fcn (safe_fclose, ffile);

      // octave_base_parser constructor sets this for us.
      frame.protect_var (LEXER);

      octave_parser parser (ffile);

      parser.curr_class_name = dispatch_type;
      parser.curr_package_name = package_name;
      parser.autoloading = autoload;
      parser.fcn_file_from_relative_lookup = relative_lookup;

      parser.lexer.force_script = force_script;
      parser.lexer.prep_for_file ();
      parser.lexer.parsing_class_method = ! dispatch_type.empty ();

      parser.lexer.fcn_file_name = file;
      parser.lexer.fcn_file_full_name = full_file;

      int status = parser.run ();

      fcn_ptr = parser.primary_fcn_ptr;

      if (status == 0)
        {
          if (parser.lexer.reading_classdef_file
              && parser.classdef_object)
            {
              // Convert parse tree for classdef object to
              // meta.class info (and stash it in the symbol
              // table?).  Return pointer to constructor?

              if (fcn_ptr)
                panic_impossible ();

              bool is_at_folder = ! dispatch_type.empty ();

              fcn_ptr =
                parser.classdef_object->make_meta_class (is_at_folder);
            }
          else if (fcn_ptr)
            {
              fcn_ptr->maybe_relocate_end ();

              if (parser.parsing_subfunctions)
                {
                  if (! parser.endfunction_found)
                    parser.subfunction_names.reverse ();

                  fcn_ptr->stash_subfunction_names (parser.subfunction_names);
                }
            }
        }
      else
        error ("parse error while reading file %s", full_file.c_str ());
    }
  else if (require_file)
    error ("no such file, '%s'", full_file.c_str ());
  else if (! warn_for.empty ())
    error ("%s: unable to open file '%s'", warn_for.c_str (),
           full_file.c_str ());

  return fcn_ptr;
}

std::string
get_help_from_file (const std::string& nm, bool& symbol_found,
                    std::string& full_file)
{
  std::string retval;

  full_file = fcn_file_in_path (nm);

  std::string file = full_file;

  size_t file_len = file.length ();

  if ((file_len > 4 && file.substr (file_len-4) == ".oct")
      || (file_len > 4 && file.substr (file_len-4) == ".mex")
      || (file_len > 2 && file.substr (file_len-2) == ".m"))
    {
      file = octave_env::base_pathname (file);
      file = file.substr (0, file.find_last_of ('.'));

      size_t pos = file.find_last_of (file_ops::dir_sep_str ());
      if (pos != std::string::npos)
        file = file.substr (pos+1);
    }

  if (! file.empty ())
    {
      symbol_found = true;

      octave_function *fcn
        = parse_fcn_file (full_file, file, "", "", true, false, false, false,
                          "");

      if (fcn)
        {
          retval = fcn->doc_string ();

          delete fcn;
        }
    }

  return retval;
}

std::string
get_help_from_file (const std::string& nm, bool& symbol_found)
{
  std::string file;
  return get_help_from_file (nm, symbol_found, file);
}

std::string
lookup_autoload (const std::string& nm)
{
  std::string retval;

  typedef std::map<std::string, std::string>::const_iterator am_iter;

  am_iter p = autoload_map.find (nm);

  if (p != autoload_map.end ())
    retval = load_path::find_file (p->second);

  return retval;
}

string_vector
autoloaded_functions (void)
{
  string_vector names (autoload_map.size ());

  octave_idx_type i = 0;
  typedef std::map<std::string, std::string>::const_iterator am_iter;
  for (am_iter p = autoload_map.begin (); p != autoload_map.end (); p++)
    names[i++] = p->first;

  return names;
}

string_vector
reverse_lookup_autoload (const std::string& nm)
{
  string_vector names;

  typedef std::map<std::string, std::string>::const_iterator am_iter;
  for (am_iter p = autoload_map.begin (); p != autoload_map.end (); p++)
    if (nm == p->second)
      names.append (p->first);

  return names;
}

octave_function *
load_fcn_from_file (const std::string& file_name, const std::string& dir_name,
                    const std::string& dispatch_type,
                    const std::string& package_name,
                    const std::string& fcn_name, bool autoload)
{
  octave_function *retval = 0;

  unwind_protect frame;

  std::string nm = file_name;

  size_t nm_len = nm.length ();

  std::string file;

  bool relative_lookup = false;

  file = nm;

  if ((nm_len > 4 && nm.substr (nm_len-4) == ".oct")
      || (nm_len > 4 && nm.substr (nm_len-4) == ".mex")
      || (nm_len > 2 && nm.substr (nm_len-2) == ".m"))
    {
      nm = octave_env::base_pathname (file);
      nm = nm.substr (0, nm.find_last_of ('.'));

      size_t pos = nm.find_last_of (file_ops::dir_sep_str ());
      if (pos != std::string::npos)
        nm = nm.substr (pos+1);
    }

  relative_lookup = ! octave_env::absolute_pathname (file);

  file = octave_env::make_absolute (file);

  int len = file.length ();

  if (len > 4 && file.substr (len-4, len-1) == ".oct")
    {
      if (autoload && ! fcn_name.empty ())
        nm = fcn_name;

      retval = octave_dynamic_loader::load_oct (nm, file, relative_lookup);
    }
  else if (len > 4 && file.substr (len-4, len-1) == ".mex")
    {
      // Temporarily load m-file version of mex-file, if it exists,
      // to get the help-string to use.

      octave_function *tmpfcn = parse_fcn_file (file.substr (0, len - 2),
                                                nm, dispatch_type,
                                                package_name, false,
                                                autoload, autoload,
                                                relative_lookup, "");

      retval = octave_dynamic_loader::load_mex (nm, file, relative_lookup);

      if (tmpfcn)
        retval->document (tmpfcn->doc_string ());
      delete tmpfcn;
    }
  else if (len > 2)
    {
      retval = parse_fcn_file (file, nm, dispatch_type, package_name, true,
                               autoload, autoload, relative_lookup, "");
    }

  if (retval)
    {
      retval->stash_dir_name (dir_name);
      retval->stash_package_name (package_name);

      if (retval->is_user_function ())
        {
          symbol_table::scope_id id = retval->scope ();

          symbol_table::stash_dir_name_for_subfunctions (id, dir_name);
        }
    }

  return retval;
}

DEFUN (autoload, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{autoload_map} =} autoload ()\n\
@deftypefnx {Built-in Function} {} autoload (@var{function}, @var{file})\n\
@deftypefnx {Built-in Function} {} autoload (@dots{}, \"remove\")\n\
Define @var{function} to autoload from @var{file}.\n\
\n\
The second argument, @var{file}, should be an absolute file name or a file\n\
name in the same directory as the function or script from which the autoload\n\
command was run.  @var{file} @emph{should not} depend on the Octave load\n\
path.\n\
\n\
Normally, calls to @code{autoload} appear in PKG_ADD script files that are\n\
evaluated when a directory is added to Octave's load path.  To avoid having\n\
to hardcode directory names in @var{file}, if @var{file} is in the same\n\
directory as the PKG_ADD script then\n\
\n\
@example\n\
autoload (\"foo\", \"bar.oct\");\n\
@end example\n\
\n\
@noindent\n\
will load the function @code{foo} from the file @code{bar.oct}.  The above\n\
usage when @code{bar.oct} is not in the same directory, or usages such as\n\
\n\
@example\n\
autoload (\"foo\", file_in_loadpath (\"bar.oct\"))\n\
@end example\n\
\n\
@noindent\n\
are strongly discouraged, as their behavior may be unpredictable.\n\
\n\
With no arguments, return a structure containing the current autoload map.\n\
\n\
If a third argument @qcode{\"remove\"} is given, the function is cleared and\n\
not loaded anymore during the current Octave session.\n\
\n\
@seealso{PKG_ADD}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      Cell func_names (dim_vector (autoload_map.size (), 1));
      Cell file_names (dim_vector (autoload_map.size (), 1));

      octave_idx_type i = 0;
      typedef std::map<std::string, std::string>::const_iterator am_iter;
      for (am_iter p = autoload_map.begin (); p != autoload_map.end (); p++)
        {
          func_names(i) = p->first;
          file_names(i) = p->second;

          i++;
        }

      octave_map m;

      m.assign ("function", func_names);
      m.assign ("file", file_names);

      retval = m;
    }
  else if (nargin == 2 || nargin == 3)
    {
      string_vector argv = args.make_argv ("autoload");

      if (! error_state)
        {
          std::string nm = argv[2];

          if (! octave_env::absolute_pathname (nm))
            {
              octave_user_code *fcn = octave_call_stack::caller_user_code ();

              bool found = false;

              if (fcn)
                {
                  std::string fname = fcn->fcn_file_name ();

                  if (! fname.empty ())
                    {
                      fname = octave_env::make_absolute (fname);
                      fname = fname.substr (0, fname.find_last_of (file_ops::dir_sep_str ()) + 1);

                      file_stat fs (fname + nm);

                      if (fs.exists ())
                        {
                          nm = fname + nm;
                          found = true;
                        }
                    }
                }
              if (! found)
                warning_with_id ("Octave:autoload-relative-file-name",
                                 "autoload: '%s' is not an absolute file name",
                                 nm.c_str ());
            }
          if (nargin == 2)
            autoload_map[argv[1]] = nm;
          else if (nargin == 3)
            {
              if (argv[3].compare ("remove") != 0)
                error_with_id ("Octave:invalid-input-arg",
                               "autoload: third argument can only be 'remove'");

              // Remove function from symbol table and autoload map.
              symbol_table::clear_dld_function (argv[1]);
              autoload_map.erase (argv[1]);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

void
source_file (const std::string& file_name, const std::string& context,
             bool verbose, bool require_file, const std::string& warn_for)
{
  // Map from absolute name of script file to recursion level.  We
  // use a map instead of simply placing a limit on recursion in the
  // source_file function so that two mutually recursive scripts
  // written as
  //
  //   foo1.m:
  //   ------
  //   foo2
  //
  //   foo2.m:
  //   ------
  //   foo1
  //
  // and called with
  //
  //   foo1
  //
  // (for example) will behave the same if they are written as
  //
  //   foo1.m:
  //   ------
  //   source ("foo2.m")
  //
  //   foo2.m:
  //   ------
  //   source ("foo1.m")
  //
  // and called with
  //
  //   source ("foo1.m")
  //
  // (for example).

  static std::map<std::string, int> source_call_depth;

  std::string file_full_name = file_ops::tilde_expand (file_name);

  file_full_name = octave_env::make_absolute (file_full_name);

  unwind_protect frame;

  if (source_call_depth.find (file_full_name) == source_call_depth.end ())
    source_call_depth[file_full_name] = -1;

  frame.protect_var (source_call_depth[file_full_name]);

  source_call_depth[file_full_name]++;

  if (source_call_depth[file_full_name] >= Vmax_recursion_depth)
    {
      error ("max_recursion_depth exceeded");
      return;
    }

  if (! context.empty ())
    {
      if (context == "caller")
        octave_call_stack::goto_caller_frame ();
      else if (context == "base")
        octave_call_stack::goto_base_frame ();
      else
        error ("source: context must be \"caller\" or \"base\"");

      if (! error_state)
        frame.add_fcn (octave_call_stack::pop);
    }

  if (! error_state)
    {
      octave_function *fcn = parse_fcn_file (file_full_name, file_name,
                                             "", "", require_file, true,
                                             false, false, warn_for);

      if (! error_state)
        {
          if (fcn && fcn->is_user_script ())
            {
              octave_value_list args;

              if (verbose)
                {
                  std::cout << "executing commands from " << file_full_name << " ... ";
                  reading_startup_message_printed = true;
                  std::cout.flush ();
                }

              fcn->do_multi_index_op (0, args);

              if (verbose)
                std::cout << "done." << std::endl;

              delete fcn;
            }
        }
      else
        error ("source: error sourcing file '%s'",
               file_full_name.c_str ());
    }
}

DEFUN (mfilename, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} mfilename ()\n\
@deftypefnx {Built-in Function} {} mfilename (\"fullpath\")\n\
@deftypefnx {Built-in Function} {} mfilename (\"fullpathext\")\n\
Return the name of the currently executing file.\n\
\n\
When called from outside an m-file return the empty string.\n\
\n\
Given the argument @qcode{\"fullpath\"}, include the directory part of the\n\
file name, but not the extension.\n\
\n\
Given the argument @qcode{\"fullpathext\"}, include the directory part of\n\
the file name and the extension.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 1)
    {
      print_usage ();
      return retval;
    }

  std::string arg;

  if (nargin == 1)
    {
      arg = args(0).string_value ();

      if (error_state)
        {
          error ("mfilename: expecting argument to be a character string");
          return retval;
        }
    }

  std::string fname;

  octave_user_code *fcn = octave_call_stack::caller_user_code ();

  if (fcn)
    {
      fname = fcn->fcn_file_name ();

      if (fname.empty ())
        fname = fcn->name ();
    }

  if (arg == "fullpathext")
    retval = fname;
  else
    {
      size_t dpos = fname.rfind (file_ops::dir_sep_char ());
      size_t epos = fname.rfind ('.');

      if (epos <= dpos)
        epos = std::string::npos;

      fname = (epos != std::string::npos) ? fname.substr (0, epos) : fname;

      if (arg == "fullpath")
        retval = fname;
      else
        retval = (dpos != std::string::npos) ? fname.substr (dpos+1) : fname;
    }

  return retval;
}

DEFUN (source, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} source (@var{file})\n\
Parse and execute the contents of @var{file}.\n\
\n\
This is equivalent to executing commands from a script file, but without\n\
requiring the file to be named @file{@var{file}.m}.\n\
@seealso{run}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string file_name = args(0).string_value ();

      if (! error_state)
        {
          std::string context;

          if (nargin == 2)
            context = args(1).string_value ();

          if (! error_state)
            source_file (file_name, context);
          else
            error ("source: expecting context to be character string");
        }
      else
        error ("source: expecting file name as argument");
    }
  else
    print_usage ();

  return retval;
}

// Evaluate an Octave function (built-in or interpreted) and return
// the list of result values.  NAME is the name of the function to
// call.  ARGS are the arguments to the function.  NARGOUT is the
// number of output arguments expected.

octave_value_list
feval (const std::string& name, const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  octave_value fcn = symbol_table::find_function (name, args);

  if (fcn.is_defined ())
    retval = fcn.do_multi_index_op (nargout, args);
  else
    {
      maybe_missing_function_hook (name);
      if (! error_state)
        error ("feval: function '%s' not found", name.c_str ());
    }

  return retval;
}

octave_value_list
feval (octave_function *fcn, const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  if (fcn)
    retval = fcn->do_multi_index_op (nargout, args);

  return retval;
}

static octave_value_list
get_feval_args (const octave_value_list& args)
{
  return args.slice (1, args.length () - 1, true);
}


// Evaluate an Octave function (built-in or interpreted) and return
// the list of result values.  The first element of ARGS should be a
// string containing the name of the function to call, then the rest
// are the actual arguments to the function.  NARGOUT is the number of
// output arguments expected.

octave_value_list
feval (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      octave_value f_arg = args(0);

      if (f_arg.is_string ())
        {
          std::string name = f_arg.string_value ();

          if (! error_state)
            {
              octave_value_list tmp_args = get_feval_args (args);

              retval = feval (name, tmp_args, nargout);
            }
        }
      else if (f_arg.is_function_handle ()
               || f_arg.is_anonymous_function ()
               || f_arg.is_inline_function ())
        {
          const octave_value_list tmp_args = get_feval_args (args);

          retval = f_arg.do_multi_index_op (nargout, tmp_args);
        }
      else
        error ("feval: first argument must be a string, inline function or a function handle");
    }

  return retval;
}

DEFUN (feval, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} feval (@var{name}, @dots{})\n\
Evaluate the function named @var{name}.\n\
\n\
Any arguments after the first are passed as inputs to the named function.\n\
For example,\n\
\n\
@example\n\
@group\n\
feval (\"acos\", -1)\n\
     @result{} 3.1416\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
calls the function @code{acos} with the argument @samp{-1}.\n\
\n\
The function @code{feval} can also be used with function handles of any sort\n\
(@pxref{Function Handles}).  Historically, @code{feval} was the only way to\n\
call user-supplied functions in strings, but function handles are now\n\
preferred due to the cleaner syntax they offer.  For example,\n\
\n\
@example\n\
@group\n\
@var{f} = @@exp;\n\
feval (@var{f}, 1)\n\
    @result{} 2.7183\n\
@var{f} (1)\n\
    @result{} 2.7183\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
are equivalent ways to call the function referred to by @var{f}.  If it\n\
cannot be predicted beforehand whether @var{f} is a function handle,\n\
function name in a string, or inline function then @code{feval} can be used\n\
instead.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    retval = feval (args, nargout);
  else
    print_usage ();

  return retval;
}

DEFUN (builtin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@dots{}] =} builtin (@var{f}, @dots{})\n\
Call the base function @var{f} even if @var{f} is overloaded to another\n\
function for the given type signature.\n\
\n\
This is normally useful when doing object-oriented programming and there is\n\
a requirement to call one of Octave's base functions rather than the\n\
overloaded one of a new class.\n\
\n\
A trivial example which redefines the @code{sin} function to be the\n\
@code{cos} function shows how @code{builtin} works.\n\
\n\
@example\n\
@group\n\
sin (0)\n\
  @result{} 0\n\
function y = sin (x), y = cos (x); endfunction\n\
sin (0)\n\
  @result{} 1\n\
builtin (\"sin\", 0)\n\
  @result{} 0\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      const std::string name (args(0).string_value ());

      if (! error_state)
        {
          octave_value fcn = symbol_table::builtin_find (name);

          if (fcn.is_defined ())
            retval = feval (fcn.function_value (), args.splice (0, 1),
                            nargout);
          else
            error ("builtin: lookup for symbol '%s' failed", name.c_str ());
        }
      else
        error ("builtin: function name (F) must be a string");
    }
  else
    print_usage ();

  return retval;
}

octave_value_list
eval_string (const std::string& eval_str, bool silent,
             int& parse_status, int nargout)
{
  octave_value_list retval;

  octave_parser parser (eval_str);

  do
    {
      parser.reset ();

      parse_status = parser.run ();

      if (parse_status == 0)
        {
          if (parser.stmt_list)
            {
              tree_statement *stmt = 0;

              if (parser.stmt_list->length () == 1
                  && (stmt = parser.stmt_list->front ())
                  && stmt->is_expression ())
                {
                  tree_expression *expr = stmt->expression ();

                  if (silent)
                    expr->set_print_flag (false);

                  bool do_bind_ans = false;

                  if (expr->is_identifier ())
                    {
                      tree_identifier *id
                        = dynamic_cast<tree_identifier *> (expr);

                      do_bind_ans = (! id->is_variable ());
                    }
                  else
                    do_bind_ans = (! expr->is_assignment_expression ());

                  retval = expr->rvalue (nargout);

                  if (do_bind_ans && ! (error_state || retval.empty ()))
                    bind_ans (retval(0), expr->print_result ());

                  if (nargout == 0)
                    retval = octave_value_list ();
                }
              else if (nargout == 0)
                parser.stmt_list->accept (*current_evaluator);
              else
                error ("eval: invalid use of statement list");

              if (error_state
                  || tree_return_command::returning
                  || tree_break_command::breaking
                  || tree_continue_command::continuing)
                break;
            }
          else if (parser.lexer.end_of_input)
            break;
        }
    }
  while (parse_status == 0);

  return retval;
}

octave_value
eval_string (const std::string& eval_str, bool silent, int& parse_status)
{
  octave_value retval;

  octave_value_list tmp = eval_string (eval_str, silent, parse_status, 1);

  if (! tmp.empty ())
    retval = tmp(0);

  return retval;
}

static octave_value_list
eval_string (const octave_value& arg, bool silent, int& parse_status,
             int nargout)
{
  std::string s = arg.string_value ();

  if (error_state)
    {
      error ("eval: expecting std::string argument");
      return octave_value (-1);
    }

  return eval_string (s, silent, parse_status, nargout);
}

void
cleanup_statement_list (tree_statement_list **lst)
{
  if (*lst)
    {
      delete *lst;
      *lst = 0;
    }
}

DEFUN (eval, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} eval (@var{try})\n\
@deftypefnx {Built-in Function} {} eval (@var{try}, @var{catch})\n\
Parse the string @var{try} and evaluate it as if it were an Octave\n\
program.\n\
\n\
If execution fails, evaluate the optional string @var{catch}.\n\
\n\
The string @var{try} is evaluated in the current context, so any results\n\
remain available after @code{eval} returns.\n\
\n\
The following example creates the variable @var{A} with the approximate\n\
value of 3.1416 in the current workspace.\n\
\n\
@example\n\
eval (\"A = acos(-1);\");\n\
@end example\n\
\n\
If an error occurs during the evaluation of @var{try} then the @var{catch}\n\
string is evaluated, as the following example shows:\n\
\n\
@example\n\
@group\n\
eval ('error (\"This is a bad example\");',\n\
      'printf (\"This error occurred:\\n%s\\n\", lasterr ());');\n\
     @print{} This error occurred:\n\
        This is a bad example\n\
@end group\n\
@end example\n\
\n\
Programming Note: if you are only using @code{eval} as an error-capturing\n\
mechanism, rather than for the execution of arbitrary code strings,\n\
Consider using try/catch blocks or unwind_protect/unwind_protect_cleanup\n\
blocks instead.  These techniques have higher performance and don't introduce\n\
the security considerations that the evaluation of arbitrary code does.\n\
@seealso{evalin}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      unwind_protect frame;

      if (nargin > 1)
        {
          frame.protect_var (buffer_error_messages);
          buffer_error_messages++;
        }

      int parse_status = 0;

      octave_value_list tmp = eval_string (args(0), nargout > 0,
                                           parse_status, nargout);

      if (nargin > 1 && (parse_status != 0 || error_state))
        {
          error_state = 0;

          // Set up for letting the user print any messages from
          // errors that occurred in the first part of this eval().

          buffer_error_messages--;

          tmp = eval_string (args(1), nargout > 0, parse_status, nargout);

          if (nargout > 0)
            retval = tmp;
        }
      else if (nargout > 0)
        retval = tmp;
    }
  else
    print_usage ();

  return retval;
}

/*

%!shared x
%! x = 1;

%!assert (eval ("x"), 1)
%!assert (eval ("x;"))
%!assert (eval ("x;"), 1);

%!test
%! y = eval ("x");
%! assert (y, 1);

%!test
%! y = eval ("x;");
%! assert (y, 1);

%!test
%! eval ("x = 1;")
%! assert (x,1);

%!test
%! eval ("flipud = 2;");
%! assert (flipud, 2);

%!function y = __f ()
%!  eval ("flipud = 2;");
%!  y = flipud;
%!endfunction
%!assert (__f(), 2)

% bug #35645
%!test
%! [a,] = gcd (1,2);
%! [a,b,] = gcd (1, 2);

*/

DEFUN (assignin, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} assignin (@var{context}, @var{varname}, @var{value})\n\
Assign @var{value} to @var{varname} in context @var{context}, which\n\
may be either @qcode{\"base\"} or @qcode{\"caller\"}.\n\
@seealso{evalin}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 3)
    {
      std::string context = args(0).string_value ();

      if (! error_state)
        {
          unwind_protect frame;

          if (context == "caller")
            octave_call_stack::goto_caller_frame ();
          else if (context == "base")
            octave_call_stack::goto_base_frame ();
          else
            error ("assignin: CONTEXT must be \"caller\" or \"base\"");

          if (! error_state)
            {
              frame.add_fcn (octave_call_stack::pop);

              std::string nm = args(1).string_value ();

              if (! error_state)
                {
                  if (valid_identifier (nm))
                    symbol_table::assign (nm, args(2));
                  else
                    error ("assignin: invalid variable name in argument VARNAME");
                }
              else
                error ("assignin: VARNAME must be a string");
            }
        }
      else
        error ("assignin: CONTEXT must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (evalin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} evalin (@var{context}, @var{try})\n\
@deftypefnx {Built-in Function} {} evalin (@var{context}, @var{try}, @var{catch})\n\
Like @code{eval}, except that the expressions are evaluated in the context\n\
@var{context}, which may be either @qcode{\"caller\"} or @qcode{\"base\"}.\n\
@seealso{eval, assignin}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 1)
    {
      std::string context = args(0).string_value ();

      if (! error_state)
        {
          unwind_protect frame;

          if (context == "caller")
            octave_call_stack::goto_caller_frame ();
          else if (context == "base")
            octave_call_stack::goto_base_frame ();
          else
            error ("evalin: CONTEXT must be \"caller\" or \"base\"");

          if (! error_state)
            {
              frame.add_fcn (octave_call_stack::pop);

              if (nargin > 2)
                {
                  frame.protect_var (buffer_error_messages);
                  buffer_error_messages++;
                }

              int parse_status = 0;

              octave_value_list tmp = eval_string (args(1), nargout > 0,
                                                   parse_status, nargout);

              if (nargout > 0)
                retval = tmp;

              if (nargin > 2 && (parse_status != 0 || error_state))
                {
                  error_state = 0;

                  // Set up for letting the user print any messages from
                  // errors that occurred in the first part of this eval().

                  buffer_error_messages--;

                  tmp = eval_string (args(2), nargout > 0,
                                     parse_status, nargout);

                  retval = (nargout > 0) ? tmp : octave_value_list ();
                }
            }
        }
      else
        error ("evalin: CONTEXT must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__parser_debug_flag__, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} __parser_debug_flag__ ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} __parser_debug_flag__ (@var{new_val})\n\
Query or set the internal flag that determines whether Octave's parser prints\n\
debug information as it processes an expression.\n\
@seealso{__lexer_debug_flag__}\n\
@end deftypefn")
{
  octave_value retval;

  bool debug_flag = octave_debug;

  retval = set_internal_variable (debug_flag, args, nargout,
                                  "__parser_debug_flag__");

  octave_debug = debug_flag;

  return retval;
}

DEFUN (__parse_file__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __parse_file__ (@var{file}, @var{verbose})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string file = args(0).string_value ();

      std::string full_file = octave_env::make_absolute (file);

      size_t file_len = file.length ();

      if ((file_len > 4 && file.substr (file_len-4) == ".oct")
          || (file_len > 4 && file.substr (file_len-4) == ".mex")
          || (file_len > 2 && file.substr (file_len-2) == ".m"))
        {
          file = octave_env::base_pathname (file);
          file = file.substr (0, file.find_last_of ('.'));

          size_t pos = file.find_last_of (file_ops::dir_sep_str ());
          if (pos != std::string::npos)
            file = file.substr (pos+1);
        }

      if (! error_state)
        {
          if (nargin == 2)
            octave_stdout << "parsing " << full_file << std::endl;

          octave_function *fcn = parse_fcn_file (full_file, file, "", "",
                                                 true, false, false,
                                                 false, "__parse_file__");

          if (fcn)
            delete fcn;
        }
      else
        error ("__parse_file__: expecting file name as argument");
    }
  else
    print_usage ();

  return retval;
}
