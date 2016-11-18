/*

Copyright (C) 1996-2015 John W. Eaton

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

#if !defined (octave_pt_h)
#define octave_pt_h 1

#include <string>

#include <iosfwd>

class octave_function;
class tree_walker;

// Base class for the parse tree.

class
tree
{
public:

  tree (int l = -1, int c = -1)
    : line_num (l), column_num (c), bp (false) { }

  virtual ~tree (void) { }

  virtual int line (void) const { return line_num; }

  virtual int column (void) const { return column_num; }

  void line (int l) { line_num = l; }

  void column (int c) { column_num = c; }

  void set_location (int l, int c)
  {
    line_num = l;
    column_num = c;
  }

  virtual void set_breakpoint (void) { bp = true; }

  virtual void delete_breakpoint (void) { bp = false; }

  bool is_breakpoint (void) const { return bp; }

  std::string str_print_code (void);

  virtual void accept (tree_walker& tw) = 0;

private:

  // The input line and column where we found the text that was
  // eventually converted to this tree node.
  int line_num;
  int column_num;

  // Breakpoint flag.
  bool bp;

  // No copying!

  tree (const tree&);

  tree& operator = (const tree&);
};

#endif
