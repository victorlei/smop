/*

Copyright (C) 2013-2015 Michael Goffioul

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

%{
#define YYDEBUG 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "txt-eng.h"
#include "oct-tex-parser.h"

extern int octave_tex_lex (YYSTYPE *, void *);
static void yyerror (text_parser_tex& parser, const char *s);

#if defined (GNULIB_NAMESPACE)
// Calls to the following functions appear in the generated output from
// Bison without the namespace tag.  Redefine them so we will use them
// via the gnulib namespace.
#define fclose GNULIB_NAMESPACE::fclose
#define fprintf GNULIB_NAMESPACE::fprintf
#define malloc GNULIB_NAMESPACE::malloc
#endif

#define scanner parser.get_scanner ()
%}

%API_PREFIX_DECL%

%define api.pure
%parse-param { text_parser_tex& parser }
%lex-param { void *scanner }

%code requires {#include <string>}

%union {
  /* Leaf symbols produced by the scanner */
  char                       ch;
  double                     num;
  int                        sym;

  /* Used for string buffering */
  std::string*               str;

  /* Objects produced by the parser */
  text_element*              e_base;
  text_element_list*         e_list;
}

%token BF IT SL RM
%token FONTNAME FONTSIZE
%token COLOR COLOR_RGB
%token START END SUPER SUB
%token<ch> CH
%token<num> NUM
%token<sym> SYM

%type<str> simple_string
%type<e_base> string_element symbol_element
%type<e_base> superscript_element subscript_element combined_script_element
%type<e_base> font_modifier_element fontname_element fontsize_element color_element
%type<e_list> string_element_list scoped_string_element_list

/* Make sure there's no memory leak on parse error. */
%destructor { } <ch> <num> <sym>
%destructor { delete $$; } <*>

%nonassoc SCRIPT
%nonassoc SUB SUPER

%nonassoc STR
%nonassoc CH

%start string

%%

simple_string			: CH
				  { $$ = new std::string (1, $1); }
				| simple_string CH
				  { $1->append (1, $2); $$ = $1; }
				;

symbol_element			: SYM
				  { $$ = new text_element_symbol ($1); }
				;

font_modifier_element		: BF
				  { $$ = new text_element_fontstyle (text_element_fontstyle::bold); }
				| IT
				  { $$ = new text_element_fontstyle (text_element_fontstyle::italic); }
				| SL
				  { $$ = new text_element_fontstyle (text_element_fontstyle::oblique); }
				| RM
				  { $$ = new text_element_fontstyle (text_element_fontstyle::normal); }
				;

fontsize_element		: FONTSIZE START NUM END
				  { $$ = new text_element_fontsize ($3); }
				;

fontname_element		: FONTNAME START simple_string END
				  {
				    $$ = new text_element_fontname (*$3);
				    delete $3;
				  }
				;

color_element			: COLOR START simple_string END
				  {
				    $$ = new text_element_color (*$3);
				    delete $3;
				  }
				| COLOR_RGB START NUM NUM NUM END
				  {
				    $$ = new text_element_color ($3, $4, $5);
				  }
				;

string_element			: simple_string %prec STR
				  {
				    $$ = new text_element_string (*$1);
				    delete $1;
				  }
				| scoped_string_element_list
				  /* This is just to avoid a warning in bison. */
				  { $$ = $1; }
				| symbol_element
				| font_modifier_element
				| fontsize_element
				| fontname_element
				| color_element
				| superscript_element %prec SCRIPT
				| subscript_element %prec SCRIPT
				| combined_script_element
				;

superscript_element		: SUPER CH
				  { $$ = new text_element_superscript ($2); }
				| SUPER scoped_string_element_list
				  { $$ = new text_element_superscript ($2); }
				| SUPER symbol_element
				  { $$ = new text_element_superscript ($2); }
				;

subscript_element		: SUB CH
				  { $$ = new text_element_subscript ($2); }
				| SUB scoped_string_element_list
				  { $$ = new text_element_subscript ($2); }
				| SUB symbol_element
				  { $$ = new text_element_subscript ($2); }
				;

combined_script_element		: subscript_element superscript_element
				  { $$ = new text_element_combined ($1, $2); }
				| superscript_element subscript_element
				  { $$ = new text_element_combined ($1, $2); }
				;

string_element_list		: string_element
				  { $$ = new text_element_list ($1); }
				| string_element_list string_element
				  { $1->push_back ($2); $$ = $1; }
				;

scoped_string_element_list	: START string_element_list END
				  { $$ = $2; }
				| START END
				  { $$ = new text_element_list (); }
				;

string				: /* empty */
				  { parser.set_parse_result (new text_element_string ("")); }
				| string_element_list
				  { parser.set_parse_result ($1); }
				;

%%

text_element*
text_parser_tex::parse (const std::string& s)
{
  octave_tex_debug = 0;

  if (init_lexer (s))
    {
      result = 0;

      if (octave_tex_parse (*this) == 0)
        return result;
    }

  return new text_element_string (s);
}

static void
yyerror (text_parser_tex&, const char *s)
{
  fprintf (stderr, "TeX parse error: %s\n", s);
}
