/*

Copyright (C) 2011-2015 Michael Goffioul

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

#ifndef __QtHandles_Backend__
#define __QtHandles_Backend__ 1

#include <QObject>

#include "graphics.h"

namespace QtHandles
{

class Object;
class ObjectProxy;

class Backend :
  public QObject,
  public base_graphics_toolkit
{
  Q_OBJECT

public:
  Backend (void);

  ~Backend (void);

  bool is_valid (void) const { return true; }

  void redraw_figure (const graphics_object& h) const;

  void update (const graphics_object& obj, int pId);

  bool initialize (const graphics_object& obj);

  void finalize (const graphics_object& obj);

  void print_figure (const graphics_object& go,
                     const std::string& term,
                     const std::string& file_cmd, bool /*mono*/,
                     const std::string& /*debug_file*/) const;

  static Object* toolkitObject (const graphics_object& go);

  static ObjectProxy* toolkitObjectProxy (const graphics_object& go);

signals:
  void createObject (double handle);
};

}; // namespace QtHandles

#endif
