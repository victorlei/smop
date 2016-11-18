/*

Copyright (C) 2012 John W. Eaton
Copyright (C) 2005-2015 David Bateman

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

#if !defined (octave_lo_regexp_h)
#define octave_lo_regexp_h 1

#include <list>
#include <sstream>
#include <string>

#include "Array.h"
#include "Matrix.h"
#include "base-list.h"
#include "str-vec.h"

class
OCTAVE_API
regexp
{
public:

  class opts;
  class match_data;

  regexp (const std::string& pat = "",
          const regexp::opts& opt = regexp::opts (),
          const std::string& w = "regexp")
    : pattern (pat), options (opt), data (0), named_pats (),
      nnames (0), named_idx (), who (w)
  {
    compile_internal ();
  }

  regexp (const regexp& rx)
    : pattern (rx.pattern), data (rx.data), named_pats (rx.named_pats),
      nnames (rx.nnames), named_idx (rx.named_idx)
  { }

  regexp& operator = (const regexp& rx)
  {
    if (this != &rx)
      {
        pattern = rx.pattern;
        data = rx.data;
        named_pats = rx.named_pats;
        nnames = rx.nnames;
        named_idx = rx.named_idx;
      }

    return *this;
  }

  ~regexp (void) { free (); }

  void compile (const std::string& pat,
                const regexp::opts& opt = regexp::opts ())
  {
    pattern = pat;
    options = opt;
    compile_internal ();
  }

  match_data match (const std::string& buffer);

  bool is_match (const std::string& buffer);

  Array<bool> is_match (const string_vector& buffer);

  std::string replace (const std::string& buffer,
                       const std::string& replacement);

  class opts
  {
  public:

    opts (void)
      : x_case_insensitive (false), x_dotexceptnewline (false),
        x_emptymatch (false), x_freespacing (false), x_lineanchors (false),
        x_once (false) { }

    opts (const opts& o)
      : x_case_insensitive (o.x_case_insensitive),
        x_dotexceptnewline (o.x_dotexceptnewline),
        x_emptymatch (o.x_emptymatch),
        x_freespacing (o.x_freespacing),
        x_lineanchors (o.x_lineanchors),
        x_once (o.x_once)
    { }

    opts& operator = (const opts& o)
    {
      if (this != &o)
        {
          x_case_insensitive = o.x_case_insensitive;
          x_dotexceptnewline = o.x_dotexceptnewline;
          x_emptymatch = o.x_emptymatch;
          x_freespacing = o.x_freespacing;
          x_lineanchors = o.x_lineanchors;
          x_once = o.x_once;
        }

      return *this;
    }

    ~opts (void) { }

    void case_insensitive (bool val) { x_case_insensitive = val; }
    void dotexceptnewline (bool val) { x_dotexceptnewline = val; }
    void emptymatch (bool val) { x_emptymatch = val; }
    void freespacing (bool val) { x_freespacing = val; }
    void lineanchors (bool val) { x_lineanchors = val; }
    void once (bool val) { x_once = val; }

    bool case_insensitive (void) const { return x_case_insensitive; }
    bool dotexceptnewline (void) const { return x_dotexceptnewline; }
    bool emptymatch (void) const { return x_emptymatch; }
    bool freespacing (void) const { return x_freespacing; }
    bool lineanchors (void) const { return x_lineanchors; }
    bool once (void) const { return x_once; }

  private:

    bool x_case_insensitive;
    bool x_dotexceptnewline;
    bool x_emptymatch;
    bool x_freespacing;
    bool x_lineanchors;
    bool x_once;
  };

  class match_element
  {
  public:

    match_element (const string_vector& nt, const string_vector& t,
                   const std::string& ms, const Matrix& te,
                   double s, double e)
      : x_match_string (ms), x_named_tokens (nt), x_tokens (t),
        x_token_extents (te), x_start (s), x_end (e)
    { }

    match_element (const match_element &a)
      : x_match_string (a.x_match_string),
        x_named_tokens (a.x_named_tokens), x_tokens (a.x_tokens),
        x_token_extents (a.x_token_extents),
        x_start (a.x_start), x_end (a.x_end)
    { }

    std::string match_string (void) const { return x_match_string; }
    string_vector named_tokens (void) const { return x_named_tokens; }
    string_vector tokens (void) const { return x_tokens; }
    Matrix token_extents (void) const { return x_token_extents; }
    double start (void) const { return x_start; }
    double end (void) const { return x_end; }

  private:

    std::string x_match_string;
    string_vector x_named_tokens;
    string_vector x_tokens;
    Matrix x_token_extents;
    double x_start;
    double x_end;
  };

  class match_data : public octave_base_list<match_element>
  {
  public:

    match_data (void)
      : octave_base_list<match_element> (), named_pats ()
    { }

    match_data (const std::list<match_element>& l, const string_vector& np)
      : octave_base_list<match_element> (l), named_pats (np)
    { }

    match_data (const match_data& rx_lst)
      : octave_base_list<match_element> (rx_lst),
        named_pats (rx_lst.named_pats)
    { }

    match_data& operator = (const match_data& rx_lst)
    {
      if (this != &rx_lst)
        {
          octave_base_list<match_element>::operator = (rx_lst);
          named_pats = rx_lst.named_pats;
        }

      return *this;
    }

    ~match_data (void) { }

    string_vector named_patterns (void) { return named_pats; }

  private:

    string_vector named_pats;
  };

private:

  // The pattern we've been asked to match.
  std::string pattern;

  opts options;

  // Internal data describing the regular expression.
  void *data;

  std::string m;
  string_vector named_pats;
  int nnames;
  Array<int> named_idx;
  std::string who;

  void free (void);

  void compile_internal (void);
};

inline regexp::match_data
regexp_match (const std::string& pat,
              const std::string& buffer,
              const regexp::opts& opt = regexp::opts (),
              const std::string& who = "regexp")
{
  regexp rx (pat, opt, who);

  return rx.match (buffer);
}

inline bool
is_regexp_match (const std::string& pat,
                 const std::string& buffer,
                 const regexp::opts& opt = regexp::opts (),
                 const std::string& who = "regexp")
{
  regexp rx (pat, opt, who);

  return rx.is_match (buffer);
}

inline Array<bool>
is_regexp_match (const std::string& pat,
                 const string_vector& buffer,
                 const regexp::opts& opt = regexp::opts (),
                 const std::string& who = "regexp")
{
  regexp rx (pat, opt, who);

  return rx.is_match (buffer);
}

inline std::string
regexp_replace (const std::string& pat,
                const std::string& buffer,
                const std::string& replacement,
                const regexp::opts& opt = regexp::opts (),
                const std::string& who = "regexp")
{
  regexp rx (pat, opt, who);

  return rx.replace (buffer, replacement);
}

#endif
