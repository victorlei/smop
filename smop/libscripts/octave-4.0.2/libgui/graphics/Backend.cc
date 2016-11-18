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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QApplication>
#include <QThread>

#include <stdint.h>

#include "Backend.h"
#include "Logger.h"
#include "Object.h"
#include "ObjectFactory.h"
#include "ObjectProxy.h"

//#if INTPTR_MAX == INT32_MAX
//# define OCTAVE_PTR_TYPE octave_uint32
//# define OCTAVE_INTPTR_TYPE uint32_t
//# define OCTAVE_PTR_SCALAR uint32_scalar_value
//#else
# define OCTAVE_PTR_TYPE octave_uint64
# define OCTAVE_INTPTR_TYPE uint64_t
# define OCTAVE_PTR_SCALAR uint64_scalar_value
//#endif

namespace QtHandles
{

static std::string
toolkitObjectProperty (const graphics_object& go)
{
  if (go.isa ("figure"))
    return std::string ("__plot_stream__");
  else if (go.isa ("uicontrol")
           || go.isa ("uipanel")
           || go.isa ("uimenu")
           || go.isa ("uicontextmenu")
           || go.isa ("uitoolbar")
           || go.isa ("uipushtool")
           || go.isa ("uitoggletool"))
    return std::string ("__object__");
  else
    qCritical ("QtHandles::Backend: no __object__ property known for object "
               "of type %s", go.type ().c_str ());

  return std::string ();
}

Backend::Backend (void)
  : QObject (), base_graphics_toolkit ("qt")
{
  ObjectFactory* factory = ObjectFactory::instance ();

  connect (this, SIGNAL (createObject (double)),
           factory, SLOT (createObject (double)));
}

Backend::~Backend (void)
{
}

bool
Backend::initialize (const graphics_object& go)
{
  if (go.isa ("figure")
      || go.isa ("uicontrol")
      || go.isa ("uipanel")
      || go.isa ("uimenu")
      || go.isa ("uicontextmenu")
      || go.isa ("uitoolbar")
      || go.isa ("uipushtool")
      || go.isa ("uitoggletool"))
    {
      Logger::debug ("Backend::initialize %s from thread %08x",
                     go.type ().c_str (), QThread::currentThreadId ());

      ObjectProxy* proxy = new ObjectProxy ();
      graphics_object gObj (go);

      OCTAVE_PTR_TYPE tmp (reinterpret_cast <OCTAVE_INTPTR_TYPE> (proxy));
      gObj.get_properties ().set(toolkitObjectProperty (go), tmp);

      emit createObject (go.get_handle ().value ());

      return true;
    }

  return false;
}

void
Backend::update (const graphics_object& go, int pId)
{
  // Rule out obvious properties we want to ignore.
  if (pId == figure::properties::ID___PLOT_STREAM__
      || pId == uicontrol::properties::ID___OBJECT__
      || pId == uipanel::properties::ID___OBJECT__
      || pId == uimenu::properties::ID___OBJECT__
      || pId == uicontextmenu::properties::ID___OBJECT__
      || pId == uitoolbar::properties::ID___OBJECT__
      || pId == uipushtool::properties::ID___OBJECT__
      || pId == uitoggletool::properties::ID___OBJECT__
      || pId == base_properties::ID___MODIFIED__)
    return;

  Logger::debug ("Backend::update %s(%d) from thread %08x",
                 go.type ().c_str (), pId, QThread::currentThreadId ());

  ObjectProxy* proxy = toolkitObjectProxy (go);

  if (proxy)
    {
      if (go.isa ("uicontrol")
          && pId == uicontrol::properties::ID_STYLE)
        {
          // Special case: we need to recreate the control widget
          // associated with the octave graphics_object

          finalize (go);
          initialize (go);
        }
      else
        proxy->update (pId);
    }
}

void
Backend::finalize (const graphics_object& go)
{
  Logger::debug ("Backend::finalize %s from thread %08x",
                 go.type ().c_str (), QThread::currentThreadId ());

  ObjectProxy* proxy = toolkitObjectProxy (go);

  if (proxy)
    {
      proxy->finalize ();
      delete proxy;

      graphics_object gObj (go);

      gObj.get_properties ().set (toolkitObjectProperty (go), Matrix ());
    }
}

void
Backend::redraw_figure (const graphics_object& go) const
{
  if (go.get_properties ().is_visible ())
    {
      ObjectProxy* proxy = toolkitObjectProxy (go);

      if (proxy)
        proxy->redraw ();
    }
}

void
Backend::print_figure (const graphics_object& go,
                            const std::string& term,
                            const std::string& file_cmd, bool /*mono*/,
                            const std::string& /*debug_file*/) const
{
  if (go.get_properties ().is_visible ())
    {
      ObjectProxy* proxy = toolkitObjectProxy (go);

      if (proxy)
        proxy->print (QString::fromStdString (file_cmd),
                      QString::fromStdString (term));
    }
}

Object*
Backend::toolkitObject (const graphics_object& go)
{
  ObjectProxy* proxy = toolkitObjectProxy (go);

  if (proxy)
    return proxy->object ();

  return 0;
}

ObjectProxy*
Backend::toolkitObjectProxy (const graphics_object& go)
{
  if (go)
    {
      octave_value ov = go.get (toolkitObjectProperty (go));

      if (ov.is_defined () && ! ov.is_empty ())
        {
          OCTAVE_INTPTR_TYPE ptr = ov.OCTAVE_PTR_SCALAR ().value ();

          if (! error_state)
            return reinterpret_cast<ObjectProxy*> (ptr);
        }
    }

  return 0;
}

};
