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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>

#include "error.h"
#include "oct-obj.h"
#include "symtab.h"
#include "token.h"
#include "utils.h"

token::token (int tv, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = generic_token;
}

token::token (int tv, bool is_kw, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = is_kw ? keyword_token : generic_token;
}

token::token (int tv, const char *s, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = string_token;
  str = new std::string (s);
}

token::token (int tv, const std::string& s, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = string_token;
  str = new std::string (s);
}

token::token (int tv, double d, const std::string& s, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = double_token;
  num = d;
  orig_text = s;
}

token::token (int tv, end_tok_type t, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = ettype_token;
  et = t;
}

token::token (int tv, symbol_table::symbol_record *s, int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = sym_rec_token;
  sr = s;
}

token::token (int tv, const std::string& mth, const std::string& cls,
              int l, int c)
{
  maybe_cmd = false;
  tspc = false;
  line_num = l;
  column_num = c;
  tok_val = tv;
  type_tag = scls_name_token;
  sc.method_nm = new std::string (mth);
  sc.class_nm = new std::string (cls);
}

token::~token (void)
{
  if (type_tag == string_token)
    delete str;

  if (type_tag == scls_name_token)
    {
      delete sc.method_nm;
      delete sc.class_nm;
    }
}

std::string
token::text (void) const
{
  assert (type_tag == string_token);
  return *str;
}

std::string
token::symbol_name (void) const
{
  assert (type_tag == sym_rec_token);
  return sr->name ();
}

double
token::number (void) const
{
  assert (type_tag == double_token);
  return num;
}

token::token_type
token::ttype (void) const
{
  return type_tag;
}

token::end_tok_type
token::ettype (void) const
{
  assert (type_tag == ettype_token);
  return et;
}

symbol_table::symbol_record *
token::sym_rec (void)
{
  assert (type_tag == sym_rec_token);
  return sr;
}

std::string
token::superclass_method_name (void)
{
  assert (type_tag == scls_name_token);
  return *sc.method_nm;
}

std::string
token::superclass_class_name (void)
{
  assert (type_tag == scls_name_token);
  return *sc.class_nm;
}

std::string
token::text_rep (void)
{
  return orig_text;
}
