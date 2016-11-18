/*

Copyright (C) 2014 Torsten

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

#if !defined (octave_cmd_h)
#define octave_cmd_h 1

#include <QString>
#include <QFileInfo>

class octave_cmd
{
public:

  octave_cmd () { };
  virtual ~octave_cmd () { };

  virtual void execute () { };
};


// ---------------------------------------------------------------------
//  class octave_cmd_exec

class octave_cmd_exec : public octave_cmd
{
public:

  octave_cmd_exec (const QString& cmd) : octave_cmd () { _cmd = cmd; };
  void execute ();

private:

  QString _cmd;
};


// ---------------------------------------------------------------------
//  class octave_cmd_eval

class octave_cmd_eval : public octave_cmd
{
public:

  octave_cmd_eval (const QFileInfo& info) : octave_cmd () { _info = info; };
  void execute ();

private:

  QFileInfo _info;
};
#endif
