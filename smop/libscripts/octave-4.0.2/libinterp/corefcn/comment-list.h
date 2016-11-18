/*

Copyright (C) 2000-2015 John W. Eaton

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

#if !defined (octave_comment_list_h)
#define octave_comment_list_h 1

#include <string>

#include <base-list.h>

extern std::string get_comment_text (void);

extern char *get_comment_text_c_str (void);

extern void save_comment_text (const std::string& text);

class
octave_comment_elt
{
public:

  enum comment_type
  {
    unknown,
    block,
    full_line,
    end_of_line,
    doc_string,
    copyright
  };

  octave_comment_elt (const std::string& s = std::string (),
                      comment_type t = unknown)
    : txt (s), typ (t) { }

  octave_comment_elt (const octave_comment_elt& oc)
    : txt (oc.txt), typ (oc.typ) { }

  octave_comment_elt& operator = (const octave_comment_elt& oc)
  {
    if (this != &oc)
      {
        txt = oc.txt;
        typ = oc.typ;
      }

    return *this;
  }

  std::string text (void) const { return txt; }

  comment_type type (void) const { return typ; }

  ~octave_comment_elt (void) { }

private:

  // The text of the comment.
  std::string txt;

  // The type of comment.
  comment_type typ;
};

class
octave_comment_list : public octave_base_list<octave_comment_elt>
{
public:

  octave_comment_list (void) { }

  void append (const octave_comment_elt& elt)
  { octave_base_list<octave_comment_elt>::append (elt); }

  void append (const std::string& s,
               octave_comment_elt::comment_type t = octave_comment_elt::unknown)
  { append (octave_comment_elt (s, t)); }

  octave_comment_list *dup (void) const;
};

#endif
