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

#include <QString>

#include "oct-mutex.h"

#include "Object.h"
#include "ObjectProxy.h"

namespace QtHandles
{

ObjectProxy::ObjectProxy (Object* obj)
  : QObject (), m_object (0)
{
  init (obj);
}

void
ObjectProxy::init (Object* obj)
{
  if (obj != m_object)
    {
      if (m_object)
        {
          disconnect (this, SIGNAL (sendUpdate (int)),
                      m_object, SLOT (slotUpdate (int)));
          disconnect (this, SIGNAL (sendFinalize (void)),
                      m_object, SLOT (slotFinalize (void)));
          disconnect (this, SIGNAL (sendRedraw (void)),
                      m_object, SLOT (slotRedraw (void)));
          disconnect (this, SIGNAL (sendPrint (const QString&, const QString&)),
                      m_object, SLOT (slotPrint (const QString&, const QString&)));
        }

      m_object = obj;

      if (m_object)
        {
          connect (this, SIGNAL (sendUpdate (int)),
                   m_object, SLOT (slotUpdate (int)));
          connect (this, SIGNAL (sendFinalize (void)),
                   m_object, SLOT (slotFinalize (void)));
          connect (this, SIGNAL (sendRedraw (void)),
                   m_object, SLOT (slotRedraw (void)));
          connect (this, SIGNAL (sendPrint (const QString&, const QString&)),
                   m_object, SLOT (slotPrint (const QString&, const QString&)));
        }
    }
}

void
ObjectProxy::setObject (Object* obj)
{
  emit sendFinalize ();
  init (obj);
}

void
ObjectProxy::update (int pId)
{
  if (octave_thread::is_octave_thread ())
    emit sendUpdate (pId);
  else
    m_object->slotUpdate (pId);
}

void
ObjectProxy::finalize (void)
{
  emit sendFinalize ();
  init (0);
}

void
ObjectProxy::redraw (void)
{
  emit sendRedraw ();
}

void
ObjectProxy::print (const QString& file_cmd, const QString& term)
{
  emit sendPrint (file_cmd, term);
}

};
