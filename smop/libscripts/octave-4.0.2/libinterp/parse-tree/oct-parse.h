/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_OCTAVE_PARSE_TREE_OCT_PARSE_H_INCLUDED
# define YY_OCTAVE_PARSE_TREE_OCT_PARSE_H_INCLUDED
/* Debug traces.  */
#ifndef OCTAVE_DEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define OCTAVE_DEBUG 1
#  else
#   define OCTAVE_DEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define OCTAVE_DEBUG 0
# endif /* ! defined YYDEBUG */
#endif  /* ! defined OCTAVE_DEBUG */
#if OCTAVE_DEBUG
extern int octave_debug;
#endif

/* Token type.  */
#ifndef OCTAVE_TOKENTYPE
# define OCTAVE_TOKENTYPE
  enum octave_tokentype
  {
    ADD_EQ = 258,
    SUB_EQ = 259,
    MUL_EQ = 260,
    DIV_EQ = 261,
    LEFTDIV_EQ = 262,
    POW_EQ = 263,
    EMUL_EQ = 264,
    EDIV_EQ = 265,
    ELEFTDIV_EQ = 266,
    EPOW_EQ = 267,
    AND_EQ = 268,
    OR_EQ = 269,
    LSHIFT_EQ = 270,
    RSHIFT_EQ = 271,
    LSHIFT = 272,
    RSHIFT = 273,
    EXPR_AND_AND = 274,
    EXPR_OR_OR = 275,
    EXPR_AND = 276,
    EXPR_OR = 277,
    EXPR_NOT = 278,
    EXPR_LT = 279,
    EXPR_LE = 280,
    EXPR_EQ = 281,
    EXPR_NE = 282,
    EXPR_GE = 283,
    EXPR_GT = 284,
    LEFTDIV = 285,
    EMUL = 286,
    EDIV = 287,
    ELEFTDIV = 288,
    EPLUS = 289,
    EMINUS = 290,
    HERMITIAN = 291,
    TRANSPOSE = 292,
    PLUS_PLUS = 293,
    MINUS_MINUS = 294,
    POW = 295,
    EPOW = 296,
    NUM = 297,
    IMAG_NUM = 298,
    STRUCT_ELT = 299,
    NAME = 300,
    END = 301,
    DQ_STRING = 302,
    SQ_STRING = 303,
    FOR = 304,
    PARFOR = 305,
    WHILE = 306,
    DO = 307,
    UNTIL = 308,
    IF = 309,
    ELSEIF = 310,
    ELSE = 311,
    SWITCH = 312,
    CASE = 313,
    OTHERWISE = 314,
    BREAK = 315,
    CONTINUE = 316,
    FUNC_RET = 317,
    UNWIND = 318,
    CLEANUP = 319,
    TRY = 320,
    CATCH = 321,
    GLOBAL = 322,
    PERSISTENT = 323,
    FCN_HANDLE = 324,
    CLASSDEF = 325,
    PROPERTIES = 326,
    METHODS = 327,
    EVENTS = 328,
    ENUMERATION = 329,
    METAQUERY = 330,
    SUPERCLASSREF = 331,
    FQ_IDENT = 332,
    GET = 333,
    SET = 334,
    FCN = 335,
    END_OF_INPUT = 336,
    LEXICAL_ERROR = 337,
    INPUT_FILE = 338,
    UNARY = 339
  };
#endif
/* Tokens.  */
#define ADD_EQ 258
#define SUB_EQ 259
#define MUL_EQ 260
#define DIV_EQ 261
#define LEFTDIV_EQ 262
#define POW_EQ 263
#define EMUL_EQ 264
#define EDIV_EQ 265
#define ELEFTDIV_EQ 266
#define EPOW_EQ 267
#define AND_EQ 268
#define OR_EQ 269
#define LSHIFT_EQ 270
#define RSHIFT_EQ 271
#define LSHIFT 272
#define RSHIFT 273
#define EXPR_AND_AND 274
#define EXPR_OR_OR 275
#define EXPR_AND 276
#define EXPR_OR 277
#define EXPR_NOT 278
#define EXPR_LT 279
#define EXPR_LE 280
#define EXPR_EQ 281
#define EXPR_NE 282
#define EXPR_GE 283
#define EXPR_GT 284
#define LEFTDIV 285
#define EMUL 286
#define EDIV 287
#define ELEFTDIV 288
#define EPLUS 289
#define EMINUS 290
#define HERMITIAN 291
#define TRANSPOSE 292
#define PLUS_PLUS 293
#define MINUS_MINUS 294
#define POW 295
#define EPOW 296
#define NUM 297
#define IMAG_NUM 298
#define STRUCT_ELT 299
#define NAME 300
#define END 301
#define DQ_STRING 302
#define SQ_STRING 303
#define FOR 304
#define PARFOR 305
#define WHILE 306
#define DO 307
#define UNTIL 308
#define IF 309
#define ELSEIF 310
#define ELSE 311
#define SWITCH 312
#define CASE 313
#define OTHERWISE 314
#define BREAK 315
#define CONTINUE 316
#define FUNC_RET 317
#define UNWIND 318
#define CLEANUP 319
#define TRY 320
#define CATCH 321
#define GLOBAL 322
#define PERSISTENT 323
#define FCN_HANDLE 324
#define CLASSDEF 325
#define PROPERTIES 326
#define METHODS 327
#define EVENTS 328
#define ENUMERATION 329
#define METAQUERY 330
#define SUPERCLASSREF 331
#define FQ_IDENT 332
#define GET 333
#define SET 334
#define FCN 335
#define END_OF_INPUT 336
#define LEXICAL_ERROR 337
#define INPUT_FILE 338
#define UNARY 339

/* Value type.  */
#if ! defined OCTAVE_STYPE && ! defined OCTAVE_STYPE_IS_DECLARED

union OCTAVE_STYPE
{
#line 150 "parse-tree/oct-parse.yy" /* yacc.c:1909  */

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

#line 288 "parse-tree/oct-parse.h" /* yacc.c:1909  */
};

typedef union OCTAVE_STYPE OCTAVE_STYPE;
# define OCTAVE_STYPE_IS_TRIVIAL 1
# define OCTAVE_STYPE_IS_DECLARED 1
#endif



#ifndef YYPUSH_MORE_DEFINED
# define YYPUSH_MORE_DEFINED
enum { YYPUSH_MORE = 4 };
#endif

typedef struct octave_pstate octave_pstate;

int octave_parse (octave_base_parser& parser);
int octave_push_parse (octave_pstate *ps, int pushed_char, OCTAVE_STYPE const *pushed_val, octave_base_parser& parser);
int octave_pull_parse (octave_pstate *ps, octave_base_parser& parser);
octave_pstate * octave_pstate_new (void);
void octave_pstate_delete (octave_pstate *ps);

#endif /* !YY_OCTAVE_PARSE_TREE_OCT_PARSE_H_INCLUDED  */
