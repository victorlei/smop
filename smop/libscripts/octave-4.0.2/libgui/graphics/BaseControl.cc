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

#include <QEvent>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QWidget>

#include "BaseControl.h"
#include "ContextMenu.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

static void
updatePalette (const uicontrol::properties& props, QWidget* w)
{
  QPalette p = w->palette ();

  if (props.style_is ("edit")
      || props.style_is ("listbox")
      || props.style_is ("popupmenu"))
    {
      p.setColor (QPalette::Base,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::Text,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
    }
  else if (props.style_is ("pushbutton")
           || props.style_is ("togglebutton"))
    {
      p.setColor (QPalette::Button,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::ButtonText,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
    }
  else
    {
      p.setColor (QPalette::Window,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::WindowText,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
    }

  w->setPalette (p);
}

BaseControl::BaseControl (const graphics_object& go, QWidget* w)
  : Object (go, w), m_normalizedFont (false), m_keyPressHandlerDefined (false)
{
  init (w);
}

void
BaseControl::init (QWidget* w, bool callBase)
{
  if (callBase)
    Object::init (w, callBase);

  uicontrol::properties& up = properties<uicontrol> ();

  Matrix bb = up.get_boundingbox (false);
  w->setGeometry (xround (bb(0)), xround (bb(1)),
                  xround (bb(2)), xround (bb(3)));
  w->setFont (Utils::computeFont<uicontrol> (up, bb(3)));
  updatePalette (up, w);
  w->setEnabled (up.enable_is ("on"));
  w->setToolTip (Utils::fromStdString (up.get_tooltipstring ()));
  w->setVisible (up.is_visible ());
  m_keyPressHandlerDefined = ! up.get_keypressfcn ().is_empty ();

  w->installEventFilter (this);

  m_normalizedFont = up.fontunits_is ("normalized");
}

BaseControl::~BaseControl (void)
{
}

void
BaseControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QWidget* w = qWidget<QWidget> ();

   switch (pId)
    {
    case uicontrol::properties::ID_POSITION:
        {
          Matrix bb = up.get_boundingbox (false);
          w->setGeometry (xround (bb(0)), xround (bb(1)),
                          xround (bb(2)), xround (bb(3)));
        }
      break;

    case uicontrol::properties::ID_FONTNAME:
    case uicontrol::properties::ID_FONTSIZE:
    case uicontrol::properties::ID_FONTWEIGHT:
    case uicontrol::properties::ID_FONTANGLE:
      w->setFont (Utils::computeFont<uicontrol> (up));
      break;

    case uicontrol::properties::ID_FONTUNITS:
      // FIXME: We shouldn't have to do anything, octave should update
      //        the "fontsize" property automatically to the new units.
      //        Hence the actual font used shouldn't change.
      m_normalizedFont = up.fontunits_is ("normalized");
      break;

    case uicontrol::properties::ID_BACKGROUNDCOLOR:
    case uicontrol::properties::ID_FOREGROUNDCOLOR:
      updatePalette (up, w);
      break;

    case uicontrol::properties::ID_ENABLE:
      w->setEnabled (up.enable_is ("on"));
      break;

    case uicontrol::properties::ID_TOOLTIPSTRING:
      w->setToolTip (Utils::fromStdString (up.get_tooltipstring ()));
      break;

    case base_properties::ID_VISIBLE:
      w->setVisible (up.is_visible ());
      break;

    case uicontrol::properties::ID_KEYPRESSFCN:
      m_keyPressHandlerDefined = ! up.get_keypressfcn ().is_empty ();
      break;

    default:
      break;
    }
}

bool
BaseControl::eventFilter (QObject* watched, QEvent* xevent)
{
  switch (xevent->type ())
    {
    case QEvent::Resize:
      if (m_normalizedFont)
        {
          gh_manager::auto_lock lock;

          qWidget<QWidget> ()->setFont (Utils::computeFont<uicontrol>
                                        (properties<uicontrol> ()));
        }
      break;

    case QEvent::MouseButtonPress:
      {
        gh_manager::auto_lock lock;

        QMouseEvent* m = dynamic_cast<QMouseEvent*> (xevent);
        graphics_object go = object ();
        uicontrol::properties& up = Utils::properties<uicontrol> (go);
        graphics_object fig = go.get_ancestor ("figure");

        if (m->button () != Qt::LeftButton
            || ! up.enable_is ("on"))
          {
            gh_manager::post_set (fig.get_handle (), "selectiontype",
                                  Utils::figureSelectionType (m), false);
            gh_manager::post_set (fig.get_handle (), "currentpoint",
                                  Utils::figureCurrentPoint (fig, m),
                                  false);
            gh_manager::post_callback (fig.get_handle (),
                                       "windowbuttondownfcn");
            gh_manager::post_callback (m_handle, "buttondownfcn");

            if (m->button () == Qt::RightButton)
              ContextMenu::executeAt (up, m->globalPos ());
          }
        else
          {
            if (up.style_is ("listbox"))
              gh_manager::post_set (fig.get_handle (), "selectiontype",
                                    Utils::figureSelectionType (m), false);
            else
              gh_manager::post_set (fig.get_handle (), "selectiontype",
                                    octave_value ("normal"), false);
          }
      }
      break;

    case QEvent::MouseMove:
      if (qWidget<QWidget> ()->hasMouseTracking ())
        {
          gh_manager::auto_lock lock;

          QMouseEvent* m = dynamic_cast<QMouseEvent*> (xevent);
          graphics_object go = object ();
          graphics_object fig = go.get_ancestor ("figure");

          gh_manager::post_set (fig.get_handle (), "currentpoint",
                                Utils::figureCurrentPoint (fig, m), false);
          gh_manager::post_callback (fig.get_handle (), "windowbuttonmotionfcn");
        }
      break;

    case QEvent::KeyPress:
      if (m_keyPressHandlerDefined)
        {
          gh_manager::auto_lock lock;

          octave_scalar_map keyData =
            Utils::makeKeyEventStruct (dynamic_cast<QKeyEvent*> (xevent));
          graphics_object fig = object ().get_ancestor ("figure");

          gh_manager::post_set (fig.get_handle (), "currentcharacter",
                                keyData.getfield ("Character"), false);
          gh_manager::post_callback (m_handle, "keypressfcn", keyData);
        }
      break;

    default:
      break;
    }

  return Object::eventFilter (watched, xevent);
}

}; // namespace QtHandles
