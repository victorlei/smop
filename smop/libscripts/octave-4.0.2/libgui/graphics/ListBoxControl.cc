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

#include <QListWidget>

#include "Container.h"
#include "ListBoxControl.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

static void
updateSelection (QListWidget* list, const Matrix& value)
{
  octave_idx_type n = value.numel ();
  int lc = list->count ();

  list->clearSelection ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      int idx = xround (value(i));

      if (1 <= idx && idx <= lc)
        {
          list->item (idx-1)->setSelected (true);
          if (i == 0
              && list->selectionMode () == QAbstractItemView::SingleSelection)
            break;
        }
      else
        {
          // Invalid selection.
          list->clearSelection ();
          break;
        }
    }
}

ListBoxControl*
ListBoxControl::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      Container* container = parent->innerContainer ();

      if (container)
        return new ListBoxControl (go, new QListWidget (container));
    }

  return 0;
}

ListBoxControl::ListBoxControl (const graphics_object& go, QListWidget* list)
  : BaseControl (go, list), m_blockCallback (false)
{
  uicontrol::properties& up = properties<uicontrol> ();

  list->addItems (Utils::fromStringVector (up.get_string_vector ()));
  if ((up.get_max () - up.get_min ()) > 1)
    list->setSelectionMode (QAbstractItemView::ExtendedSelection);
  else
    list->setSelectionMode (QAbstractItemView::SingleSelection);
  Matrix value = up.get_value ().matrix_value ();
  if (value.numel () > 0)
    {
      octave_idx_type n = value.numel ();
      int lc = list->count ();

      for (octave_idx_type i = 0; i < n; i++)
        {
          int idx = xround (value(i));

          if (1 <= idx && idx <= lc)
            {
              list->item (idx-1)->setSelected (true);
              if (i == 0 && (list->selectionMode ()
                             == QAbstractItemView::SingleSelection))
                break;
            }
        }
    }

  list->removeEventFilter (this);
  list->viewport ()->installEventFilter (this);

  connect (list, SIGNAL (itemSelectionChanged (void)),
           SLOT (itemSelectionChanged (void)));
}

ListBoxControl::~ListBoxControl (void)
{
}

void
ListBoxControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QListWidget* list = qWidget<QListWidget> ();

  switch (pId)
    {
    case uicontrol::properties::ID_STRING:
      m_blockCallback = true;
      list->clear ();
      list->addItems (Utils::fromStringVector (up.get_string_vector ()));
      updateSelection (list, up.get_value ().matrix_value ());
      m_blockCallback = false;
      break;

    case uicontrol::properties::ID_MIN:

    case uicontrol::properties::ID_MAX:
      if ((up.get_max () - up.get_min ()) > 1)
        list->setSelectionMode (QAbstractItemView::ExtendedSelection);
      else
        list->setSelectionMode (QAbstractItemView::SingleSelection);
      break;

    case uicontrol::properties::ID_VALUE:
      m_blockCallback = true;
      updateSelection (list, up.get_value ().matrix_value ());
      m_blockCallback = false;
      break;

    default:
      BaseControl::update (pId);
      break;
    }
}

void
ListBoxControl::itemSelectionChanged (void)
{
  if (! m_blockCallback)
    {
      QListWidget* list = qWidget<QListWidget> ();

      QModelIndexList l = list->selectionModel ()->selectedIndexes ();
      Matrix value (dim_vector (1, l.size ()));
      int i = 0;

      foreach (const QModelIndex& idx, l)
       value(i++) = (idx.row () + 1);

      gh_manager::post_set (m_handle, "value", octave_value (value), false);
      gh_manager::post_callback (m_handle, "callback");
    }
}

}; // namespace QtHandles
