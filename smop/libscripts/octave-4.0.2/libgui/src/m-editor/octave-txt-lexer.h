/*

Copyright (C) 2013-2015 Torsten

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

// Author: Torsten <ttl@justmail.de>

#if !defined (octave_txt_lexer_h)
#define octave_txt_lexer_h 1

#include <Qsci/qsciscintilla.h>
#include <Qsci/qscilexer.h>


class octave_txt_lexer : public QsciLexer
{
  Q_OBJECT

public:

  virtual const char *language () const;
  virtual const char *lexer () const;
  virtual QString description (int style) const;

};

#endif