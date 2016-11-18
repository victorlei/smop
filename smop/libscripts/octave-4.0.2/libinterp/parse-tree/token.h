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

#if !defined (octave_token_h)
#define octave_token_h 1

#include <string>

#include "symtab.h"

class
token
{
public:

  enum token_type
  {
    generic_token,
    keyword_token,
    string_token,
    double_token,
    ettype_token,
    sym_rec_token,
    scls_name_token,
  };

  enum end_tok_type
  {
    simple_end,
    classdef_end,
    enumeration_end,
    events_end,
    for_end,
    function_end,
    if_end,
    methods_end,
    parfor_end,
    properties_end,
    switch_end,
    try_catch_end,
    unwind_protect_end,
    while_end,
  };

  token (int tv, int l = -1, int c = -1);
  token (int tv, bool is_keyword, int l = -1, int c = -1);
  token (int tv, const char *s, int l = -1, int c = -1);
  token (int tv, const std::string& s, int l = -1, int c = -1);
  token (int tv, double d, const std::string& s = std::string (),
         int l = -1, int c = -1);
  token (int tv, end_tok_type t, int l = -1, int c = -1);
  token (int tv, symbol_table::symbol_record *s, int l = -1, int c = -1);
  token (int tv, const std::string& mth, const std::string& cls,
         int l = -1, int c = -1);

  ~token (void);

  void mark_may_be_command (void) { maybe_cmd = true; }
  bool may_be_command (void) const { return maybe_cmd; }

  void mark_trailing_space (void) { tspc = true; }
  bool space_follows_token (void) const { return tspc; }

  int token_value (void) const { return tok_val; }
  bool token_value_is (int tv) const { return tv == tok_val; }

  int line (void) const { return line_num; }
  int column (void) const { return column_num; }

  bool is_keyword (void) const
  {
    return type_tag == keyword_token || type_tag == ettype_token;
  }

  bool is_symbol (void) const
  {
    return type_tag == sym_rec_token;
  }

  std::string text (void) const;
  std::string symbol_name (void) const;
  double number (void) const;
  token_type ttype (void) const;
  end_tok_type ettype (void) const;
  symbol_table::symbol_record *sym_rec (void);

  std::string superclass_method_name (void);
  std::string superclass_class_name (void);

  std::string text_rep (void);

private:

  // No copying!

  token (const token& tok);

  token& operator = (const token& tok);

  bool maybe_cmd;
  bool tspc;
  int line_num;
  int column_num;
  int tok_val;
  token_type type_tag;
  union
  {
    std::string *str;
    double num;
    end_tok_type et;
    symbol_table::symbol_record *sr;
    struct
    {
      std::string *method_nm;
      std::string *class_nm;
    } sc;
  };
  std::string orig_text;
};

#endif
