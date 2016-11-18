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
#include <QFrame>
#include <QLabel>
#include <QMouseEvent>
#include <QTimer>

#include "Canvas.h"
#include "Container.h"
#include "ContextMenu.h"
#include "Panel.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

static int
frameStyleFromProperties (const uipanel::properties& pp)
{
  if (pp.bordertype_is ("none"))
    return QFrame::NoFrame;
  else if (pp.bordertype_is ("etchedin"))
    return (QFrame::Box | QFrame::Sunken);
  else if (pp.bordertype_is ("etchedout"))
    return (QFrame::Box | QFrame::Raised);
  else if (pp.bordertype_is ("beveledin"))
    return (QFrame::Panel | QFrame::Sunken);
  else if (pp.bordertype_is ("beveledout"))
    return (QFrame::Panel | QFrame::Raised);
  else
    return (QFrame::Panel | QFrame::Plain);
}

static void
setupPalette (const uipanel::properties& pp, QPalette& p)
{
  p.setColor (QPalette::Window,
              Utils::fromRgb (pp.get_backgroundcolor_rgb ()));
  p.setColor (QPalette::WindowText,
              Utils::fromRgb (pp.get_foregroundcolor_rgb ()));
  p.setColor (QPalette::Light,
              Utils::fromRgb (pp.get_highlightcolor_rgb ()));
  p.setColor (QPalette::Dark,
              Utils::fromRgb (pp.get_shadowcolor_rgb ()));
}

static int
borderWidthFromProperties (const uipanel::properties& pp)
{
  int bw = 0;

  if (! pp.bordertype_is ("none"))
    {
      bw = xround (pp.get_borderwidth ());
      if (pp.bordertype_is ("etchedin") || pp.bordertype_is ("etchedout"))
        bw *= 2;
    }

  return bw;
}

Panel*
Panel::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      Container* container = parent->innerContainer ();

      if (container)
        return new Panel (go, new QFrame (container));
    }

  return 0;
}

Panel::Panel (const graphics_object& go, QFrame* frame)
  : Object (go, frame), m_container (0), m_title (0), m_blockUpdates (false)
{
  uipanel::properties& pp = properties<uipanel> ();

  frame->setObjectName ("UIPanel");
  frame->setAutoFillBackground (true);
  Matrix bb = pp.get_boundingbox (false);
  frame->setGeometry (xround (bb(0)), xround (bb(1)),
                      xround (bb(2)), xround (bb(3)));
  frame->setFrameStyle (frameStyleFromProperties (pp));
  frame->setLineWidth (xround (pp.get_borderwidth ()));
  QPalette pal = frame->palette ();
  setupPalette (pp, pal);
  frame->setPalette (pal);

  m_container = new Container (frame);
  m_container->canvas (m_handle);

  if (frame->hasMouseTracking ())
    {
      foreach (QWidget* w, frame->findChildren<QWidget*> ())
        { w->setMouseTracking (true); }
    }

  QString title = Utils::fromStdString (pp.get_title ());
  if (! title.isEmpty ())
    {
      m_title = new QLabel (title, frame);
      m_title->setAutoFillBackground (true);
      m_title->setContentsMargins (4, 0, 4, 0);
      m_title->setPalette (pal);
      m_title->setFont (Utils::computeFont<uipanel> (pp, bb(3)));
    }

  frame->installEventFilter (this);
  m_container->installEventFilter (this);

  if (pp.is_visible ())
    QTimer::singleShot (0, frame, SLOT (show (void)));
  else
    frame->hide ();
}

Panel::~Panel (void)
{
}

bool
Panel::eventFilter (QObject* watched, QEvent* xevent)
{
  if (! m_blockUpdates)
    {
      if (watched == qObject ())
        {
          switch (xevent->type ())
            {
            case QEvent::Resize:
                {
                  gh_manager::auto_lock lock;
                  graphics_object go = object ();

                  if (go.valid_object ())
                    {
                      if (m_title)
                        {
                          const uipanel::properties& pp =
                            Utils::properties<uipanel> (go);

                          if (pp.fontunits_is ("normalized"))
                            {
                              QFrame* frame = qWidget<QFrame> ();

                              m_title->setFont (Utils::computeFont<uipanel>
                                                (pp, frame->height ()));
                              m_title->resize (m_title->sizeHint ());
                            }
                        }
                      updateLayout ();
                    }
                }
              break;

            case QEvent::MouseButtonPress:
                {
                  QMouseEvent* m = dynamic_cast<QMouseEvent*> (xevent);

                  if (m->button () == Qt::RightButton)
                    {
                      gh_manager::auto_lock lock;

                      ContextMenu::executeAt (properties (), m->globalPos ());
                    }
                }
              break;

            default:
              break;
            }
        }
      else if (watched == m_container)
        {
          switch (xevent->type ())
            {
            case QEvent::Resize:
              if (qWidget<QWidget> ()->isVisible ())
                {
                  gh_manager::auto_lock lock;

                  properties ().update_boundingbox ();
                }
              break;

            default:
              break;
            }
        }
    }

  return false;
}

void
Panel::update (int pId)
{
  uipanel::properties& pp = properties<uipanel> ();
  QFrame* frame = qWidget<QFrame> ();

  m_blockUpdates = true;

  switch (pId)
    {
    case uipanel::properties::ID_POSITION:
      {
        Matrix bb = pp.get_boundingbox (false);

        frame->setGeometry (xround (bb(0)), xround (bb(1)),
                            xround (bb(2)), xround (bb(3)));
        updateLayout ();
      }
      break;

    case uipanel::properties::ID_BORDERWIDTH:
      frame->setLineWidth (xround (pp.get_borderwidth ()));
      updateLayout ();
      break;

    case uipanel::properties::ID_BACKGROUNDCOLOR:
    case uipanel::properties::ID_FOREGROUNDCOLOR:
    case uipanel::properties::ID_HIGHLIGHTCOLOR:
    case uipanel::properties::ID_SHADOWCOLOR:
      {
        QPalette pal = frame->palette ();

        setupPalette (pp, pal);
        frame->setPalette (pal);
        if (m_title)
          m_title->setPalette (pal);
      }
      break;

    case uipanel::properties::ID_TITLE:
      {
        QString title = Utils::fromStdString (pp.get_title ());

        if (title.isEmpty ())
          {
            if (m_title)
              delete m_title;
            m_title = 0;
          }
        else
          {
            if (! m_title)
              {
                QPalette pal = frame->palette ();

                m_title = new QLabel (title, frame);
                m_title->setAutoFillBackground (true);
                m_title->setContentsMargins (4, 0, 4, 0);
                m_title->setPalette (pal);
                m_title->setFont (Utils::computeFont<uipanel> (pp));
                m_title->show ();
              }
            else
              {
                m_title->setText (title);
                m_title->resize (m_title->sizeHint ());
              }
          }
        updateLayout ();
      }
      break;

    case uipanel::properties::ID_TITLEPOSITION:
      updateLayout ();
      break;

    case uipanel::properties::ID_BORDERTYPE:
      frame->setFrameStyle (frameStyleFromProperties (pp));
      updateLayout ();
      break;

    case uipanel::properties::ID_FONTNAME:
    case uipanel::properties::ID_FONTSIZE:
    case uipanel::properties::ID_FONTWEIGHT:
    case uipanel::properties::ID_FONTANGLE:
      if (m_title)
        {
          m_title->setFont (Utils::computeFont<uipanel> (pp));
          m_title->resize (m_title->sizeHint ());
          updateLayout ();
        }
      break;

    case uipanel::properties::ID_VISIBLE:
      frame->setVisible (pp.is_visible ());
      updateLayout ();
      break;

    default:
      break;
    }

  m_blockUpdates = false;
}

void
Panel::redraw (void)
{
  Canvas* canvas = m_container->canvas (m_handle);

  if (canvas)
    canvas->redraw ();
}

void
Panel::updateLayout (void)
{
  uipanel::properties& pp = properties<uipanel> ();
  QFrame* frame = qWidget<QFrame> ();

  Matrix bb = pp.get_boundingbox (true);
  int bw = borderWidthFromProperties (pp);

  frame->setFrameRect (QRect (xround (bb(0)) - bw, xround (bb(1)) - bw,
                              xround (bb(2)) + 2*bw, xround (bb(3)) + 2*bw));
  m_container->setGeometry (xround (bb(0)), xround (bb(1)),
                            xround (bb(2)), xround (bb(3)));

  if (m_blockUpdates)
    pp.update_boundingbox ();

  if (m_title)
    {
      QSize sz = m_title->sizeHint ();
      int offset = 5;

      if (pp.titleposition_is ("lefttop"))
        m_title->move (bw+offset, 0);
      else if (pp.titleposition_is ("righttop"))
        m_title->move (frame->width () - bw - offset - sz.width (), 0);
      else if (pp.titleposition_is ("leftbottom"))
        m_title->move (bw+offset, frame->height () - sz.height ());
      else if (pp.titleposition_is ("rightbottom"))
        m_title->move (frame->width () - bw - offset - sz.width (),
                       frame->height () - sz.height ());
      else if (pp.titleposition_is ("centertop"))
        m_title->move (frame->width () / 2 - sz.width () / 2, 0);
      else if (pp.titleposition_is ("centerbottom"))
        m_title->move (frame->width () / 2 - sz.width () / 2,
                       frame->height () - sz.height ());
    }
}

};
