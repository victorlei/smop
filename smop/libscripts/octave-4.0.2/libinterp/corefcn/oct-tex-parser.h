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

#ifndef YY_OCTAVE_TEX_COREFCN_OCT_TEX_PARSER_H_INCLUDED
# define YY_OCTAVE_TEX_COREFCN_OCT_TEX_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef OCTAVE_TEX_DEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define OCTAVE_TEX_DEBUG 1
#  else
#   define OCTAVE_TEX_DEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define OCTAVE_TEX_DEBUG 0
# endif /* ! defined YYDEBUG */
#endif  /* ! defined OCTAVE_TEX_DEBUG */
#if OCTAVE_TEX_DEBUG
extern int octave_tex_debug;
#endif
/* "%code requires" blocks.  */
#line 54 "corefcn/oct-tex-parser.yy" /* yacc.c:1909  */
#include <string>

#line 55 "corefcn/oct-tex-parser.h" /* yacc.c:1909  */

/* Token type.  */
#ifndef OCTAVE_TEX_TOKENTYPE
# define OCTAVE_TEX_TOKENTYPE
  enum octave_tex_tokentype
  {
    BF = 258,
    IT = 259,
    SL = 260,
    RM = 261,
    FONTNAME = 262,
    FONTSIZE = 263,
    COLOR = 264,
    COLOR_RGB = 265,
    START = 266,
    END = 267,
    SUPER = 268,
    SUB = 269,
    CH = 270,
    NUM = 271,
    SYM = 272,
    SCRIPT = 273,
    STR = 274
  };
#endif
/* Tokens.  */
#define BF 258
#define IT 259
#define SL 260
#define RM 261
#define FONTNAME 262
#define FONTSIZE 263
#define COLOR 264
#define COLOR_RGB 265
#define START 266
#define END 267
#define SUPER 268
#define SUB 269
#define CH 270
#define NUM 271
#define SYM 272
#define SCRIPT 273
#define STR 274

/* Value type.  */
#if ! defined OCTAVE_TEX_STYPE && ! defined OCTAVE_TEX_STYPE_IS_DECLARED

union OCTAVE_TEX_STYPE
{
#line 56 "corefcn/oct-tex-parser.yy" /* yacc.c:1909  */

  /* Leaf symbols produced by the scanner */
  char                       ch;
  double                     num;
  int                        sym;

  /* Used for string buffering */
  std::string*               str;

  /* Objects produced by the parser */
  text_element*              e_base;
  text_element_list*         e_list;

#line 119 "corefcn/oct-tex-parser.h" /* yacc.c:1909  */
};

typedef union OCTAVE_TEX_STYPE OCTAVE_TEX_STYPE;
# define OCTAVE_TEX_STYPE_IS_TRIVIAL 1
# define OCTAVE_TEX_STYPE_IS_DECLARED 1
#endif



int octave_tex_parse (text_parser_tex& parser);

#endif /* !YY_OCTAVE_TEX_COREFCN_OCT_TEX_PARSER_H_INCLUDED  */
