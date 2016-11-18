/*  Copyright (C) 2008 e_k (e_k@users.sourceforge.net)
    Copyright (C) 2012-2015 Jacob Dawid <jacob.dawid@cybercatalyst.com>

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public License
    along with this library; see the file COPYING.LIB.  If not, write to
    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301, USA.
*/

#include <QDebug>

#include "unix/QUnixTerminalImpl.h"
#include "unix/kpty.h"

#include <termios.h>

QUnixTerminalImpl::QUnixTerminalImpl(QWidget *parent)
    : QTerminal(parent) {
    setMinimumSize(300, 200);
    initialize();
}

void QUnixTerminalImpl::initialize()
{
    m_terminalView = new TerminalView(this);
    m_terminalView->setKeyboardCursorShape(TerminalView::UnderlineCursor);
    m_terminalView->setBlinkingCursor(true);
    m_terminalView->setBellMode(TerminalView::NotifyBell);
    m_terminalView->setTerminalSizeHint(true);
    m_terminalView->setContextMenuPolicy(Qt::CustomContextMenu);
    m_terminalView->setTripleClickMode(TerminalView::SelectWholeLine);
    m_terminalView->setTerminalSizeStartup(true);
    m_terminalView->setSize(80, 40);
    m_terminalView->setScrollBarPosition(TerminalView::ScrollBarRight);

    connect(m_terminalView, SIGNAL(customContextMenuRequested(QPoint)),
            this, SLOT(handleCustomContextMenuRequested(QPoint)));

    connect (m_terminalView, SIGNAL (interrupt_signal (void)),
             this, SLOT (terminal_interrupt ()));

#ifdef Q_OS_MAC
    QFont font = QFont("Monaco");
    font.setStyleHint(QFont::TypeWriter);
    font.setPointSize(11);
#else
    QFont font = QFont("Monospace");
    font.setStyleHint(QFont::TypeWriter);
    font.setPointSize(10);
#endif
    setTerminalFont(font);
    setFocusPolicy (Qt::StrongFocus);
    setFocusProxy(m_terminalView);
    setFocus(Qt::OtherFocusReason);

    m_kpty = new KPty();
    m_kpty->open();

    m_terminalModel = new TerminalModel(m_kpty);
    m_terminalModel->setAutoClose(true);
    m_terminalModel->setCodec(QTextCodec::codecForName("UTF-8"));
    m_terminalModel->setHistoryType(HistoryTypeBuffer (1000));
    m_terminalModel->setDarkBackground(true);
    m_terminalModel->setKeyBindings("");
    m_terminalModel->run();
    m_terminalModel->addView(m_terminalView);
    connectToPty();
}
void QUnixTerminalImpl::setScrollBufferSize(int value)
{
  if (value > 0)
    {
      m_terminalModel->clearHistory ();
      m_terminalModel->setHistoryType (HistoryTypeBuffer ( value ));
    }
  else
    m_terminalModel->setHistoryType (HistoryTypeNone ());
}

void QUnixTerminalImpl::connectToPty()
{
    // Store the file descriptor associated with the STDERR stream onto
    // another temporary file descriptor for reconnect in the destructor.
    fdstderr = dup (STDERR_FILENO);

    int fds = m_kpty->slaveFd();

    dup2 (fds, STDIN_FILENO);
    dup2 (fds, STDOUT_FILENO);
    dup2 (fds, STDERR_FILENO);

    if(!isatty(STDIN_FILENO)) {
        qDebug("Error: stdin is not a tty.");
    }

    if(!isatty(STDOUT_FILENO)) {
        qDebug("Error: stdout is not a tty.");
    }

    if(!isatty(STDERR_FILENO)) {
        qDebug("Error: stderr is not a tty.");
    }
}

QUnixTerminalImpl::~QUnixTerminalImpl()
{
    // Restore stderr so that any errors at exit might appear somewhere.
    dup2 (fdstderr, STDERR_FILENO);

    emit destroyed();
}

void QUnixTerminalImpl::setTerminalFont(const QFont &font)
{
    if(!m_terminalView)
        return;
    m_terminalView->setVTFont(font);
}

void QUnixTerminalImpl::setSize(int h, int v)
{
    if(!m_terminalView)
        return;
    m_terminalView->setSize(h, v);
}

void QUnixTerminalImpl::sendText(const QString& text)
{
    m_terminalModel->sendText(text);
}

void QUnixTerminalImpl::setCursorType(CursorType type, bool blinking)
{
    switch(type) {
        case UnderlineCursor: m_terminalView->setKeyboardCursorShape(TerminalView::UnderlineCursor); break;
        case BlockCursor: m_terminalView->setKeyboardCursorShape(TerminalView::BlockCursor); break;
        case IBeamCursor: m_terminalView->setKeyboardCursorShape(TerminalView::IBeamCursor); break;
    }
    m_terminalView->setBlinkingCursor(blinking);
}

// FIXME -- not sure how to make these work properly given the way the
// Unix terminal handles colors.
void QUnixTerminalImpl::setBackgroundColor (const QColor& color)
  {
    ColorEntry cols[TABLE_COLORS];

    const ColorEntry * curr_cols = m_terminalView->colorTable();
    for(int i=0;i<TABLE_COLORS;i++)
    {
     cols[i] = curr_cols[i];
    }

    cols[DEFAULT_BACK_COLOR].color = color;

    m_terminalView->setColorTable(cols);

  }
void QUnixTerminalImpl::setForegroundColor (const QColor& color)
{
    ColorEntry cols[TABLE_COLORS];

    const ColorEntry * curr_cols = m_terminalView->colorTable();
    for(int i=0;i<TABLE_COLORS;i++)
    {
     cols[i] = curr_cols[i];
    }

    cols[DEFAULT_FORE_COLOR].color = color;

    m_terminalView->setColorTable(cols);


}
void QUnixTerminalImpl::setSelectionColor (const QColor& color) { }

void QUnixTerminalImpl::setCursorColor (bool useForegroundColor,
                                        const QColor& color)
{
  m_terminalView->setKeyboardCursorColor (useForegroundColor, color);
}

void QUnixTerminalImpl::showEvent(QShowEvent *)
{
    m_terminalView->updateImage();
    m_terminalView->repaint();
    m_terminalView->update();
}

void QUnixTerminalImpl::resizeEvent(QResizeEvent*)
{
    m_terminalView->resize(this->size());
    m_terminalView->updateImage();
    m_terminalView->repaint();
    m_terminalView->update();
}

void QUnixTerminalImpl::copyClipboard()
{
    m_terminalView->copyClipboard (_extra_interrupt);
}

void QUnixTerminalImpl::pasteClipboard()
{
    m_terminalView->pasteClipboard();
}

void QUnixTerminalImpl::selectAll()
{
    m_terminalView->selectAll();
}


QString QUnixTerminalImpl::selectedText ()
{
  return m_terminalView->selectedText ();
}

void
QUnixTerminalImpl::has_extra_interrupt (bool extra)
{
  _extra_interrupt = extra;
}
