/*
    This file is part of Konsole

    Copyright (C) 2006-2007 by Robert Knight <robertknight@gmail.com>
    Copyright (C) 1997,1998 by Lars Doelle <lars.doelle@on-line.de>

    Rewritten for QT4 by e_k <e_k at users.sourceforge.net>, Copyright (C)2008
    Copyright (C) 2012-2015 Jacob Dawid <jacob.dawid@cybercatalyst.com>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301  USA.
*/

// Own
#include "unix/TerminalModel.h"

// Standard
#include <assert.h>
#include <stdlib.h>

// Qt
#include <QApplication>
#include <QtCore/QByteRef>
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QRegExp>
#include <QtCore/QStringList>
#include <QtCore>

#include "unix/TerminalView.h"
#include "unix/Vt102Emulation.h"

TerminalModel::TerminalModel(KPty *kpty) :
    _shellProcess(0)
  , _emulation(0)
  , _monitorActivity(false)
  , _monitorSilence(false)
  , _notifiedActivity(false)
  , _autoClose(true)
  , _wantedClose(false)
  , _silenceSeconds(10)
  , _addToUtmp(false)
  , _fullScripting(false)
  , _hasDarkBackground(false)
{
    _kpty = kpty;

    //create emulation backend
    _emulation = new Vt102Emulation();
    connect( _emulation, SIGNAL( stateSet(int) ),
             this, SLOT( activityStateSet(int) ) );
    connect( _emulation, SIGNAL( changeTabTextColorRequest( int ) ),
             this, SIGNAL( changeTabTextColorRequest( int ) ) );
    connect( _emulation, SIGNAL(profileChangeCommandReceived(const QString&)),
             this, SIGNAL( profileChangeCommandReceived(const QString&)) );
    // TODO
    // connect( _emulation,SIGNAL(imageSizeChanged(int,int)) , this ,
    //        SLOT(onEmulationSizeChange(int,int)) );

    _selfListener = new SelfListener(kpty->masterFd());
    _selfListener->start();
    connect( _selfListener, SIGNAL(recvData(const char*,int)),
             this, SLOT(onReceiveBlock(const char*,int)), Qt::BlockingQueuedConnection);

    connect( _emulation, SIGNAL(sendData(const char*,int))
             ,this,SLOT(sendData(const char*,int)));

    //connect( _emulation,SIGNAL(lockPtyRequest(bool)),_shellProcess,SLOT(lockPty(bool)) );
    //connect( _emulation,SIGNAL(useUtf8Request(bool)),_shellProcess,SLOT(setUtf8Mode(bool)) );


    //connect( _shellProcess,SIGNAL(done(int)), this, SLOT(done(int)) );

    //setup timer for monitoring session activity
    _monitorTimer = new QTimer(this);
    _monitorTimer->setSingleShot(true);
    connect(_monitorTimer, SIGNAL(timeout()), this, SLOT(monitorTimerDone()));
}

void TerminalModel::setDarkBackground(bool darkBackground)
{
    _hasDarkBackground = darkBackground;
}
bool TerminalModel::hasDarkBackground() const
{
    return _hasDarkBackground;
}

void TerminalModel::setCodec(QTextCodec* codec)
{
    emulation()->setCodec(codec);
}

QList<TerminalView*> TerminalModel::views() const
{
    return _views;
}

void TerminalModel::addView(TerminalView* widget)
{
    Q_ASSERT( !_views.contains(widget) );

    _views.append(widget);

    if ( _emulation != 0 )
    {
        // connect emulation - view signals and slots
        connect( widget , SIGNAL(keyPressedSignal(QKeyEvent*)) , _emulation ,
                 SLOT(sendKeyEvent(QKeyEvent*)) );
        connect( widget , SIGNAL(mouseSignal(int,int,int,int)) , _emulation ,
                 SLOT(sendMouseEvent(int,int,int,int)) );
        connect( widget , SIGNAL(sendStringToEmu(const char*)) , _emulation ,
                 SLOT(sendString(const char*)) );

        // allow emulation to notify view when the foreground process
        // indicates whether or not it is interested in mouse signals
        connect( _emulation , SIGNAL(programUsesMouseChanged(bool)) , widget ,
                 SLOT(setUsesMouse(bool)) );

        widget->setUsesMouse( _emulation->programUsesMouse() );

        widget->setScreenWindow(_emulation->createWindow());
    }

    //connect view signals and slots
    QObject::connect( widget ,SIGNAL(changedContentSizeSignal(int,int)),this,
                      SLOT(onViewSizeChange(int,int)));

    QObject::connect( widget ,SIGNAL(destroyed(QObject*)) , this ,
                      SLOT(viewDestroyed(QObject*)) );
    //slot for close
    //QObject::connect(this, SIGNAL(finished()), widget, SLOT(close()));
}

void TerminalModel::viewDestroyed(QObject* view)
{
    TerminalView* display = (TerminalView*)view;

    Q_ASSERT( _views.contains(display) );

    removeView(display);
}

void TerminalModel::sendData(const char *buf, int len)
{
    ssize_t bytesWritten = ::write(_kpty->masterFd(), buf, len);
    (void)bytesWritten;
}

void TerminalModel::removeView(TerminalView* widget)
{
    _views.removeAll(widget);

    disconnect(widget,0,this,0);

    if ( _emulation != 0 )
    {
        // disconnect
        //  - key presses signals from widget
        //  - mouse activity signals from widget
        //  - string sending signals from widget
        //
        //  ... and any other signals connected in addView()
        disconnect( widget, 0, _emulation, 0);

        // disconnect state change signals emitted by emulation
        disconnect( _emulation , 0 , widget , 0);
    }

    // close the session automatically when the last view is removed
    if ( _views.count() == 0 )
    {
        close();
    }
}

void TerminalModel::run()
{
    emit started();
}

void TerminalModel::monitorTimerDone()
{
    //FIXME: The idea here is that the notification popup will appear to tell the user than output from
    //the terminal has stopped and the popup will disappear when the user activates the session.
    //
    //This breaks with the addition of multiple views of a session.  The popup should disappear
    //when any of the views of the session becomes active


    //FIXME: Make message text for this notification and the activity notification more descriptive.
    if (_monitorSilence) {
        //    KNotification::event("Silence", ("Silence in session '%1'", _nameTitle), QPixmap(),
        //                    QApplication::activeWindow(),
        //                    KNotification::CloseWhenWidgetActivated);
        emit stateChanged(NOTIFYSILENCE);
    }
    else
    {
        emit stateChanged(NOTIFYNORMAL);
    }

    _notifiedActivity=false;
}

void TerminalModel::activityStateSet(int state)
{
    if (state==NOTIFYBELL)
    {
        emit bellRequest("");
    }
    else if (state==NOTIFYACTIVITY)
    {
        if (_monitorSilence) {
            _monitorTimer->start(_silenceSeconds*1000);
        }

        if ( _monitorActivity ) {
            //FIXME:  See comments in Session::monitorTimerDone()
            if (!_notifiedActivity) {
                //        KNotification::event("Activity", ("Activity in session '%1'", _nameTitle), QPixmap(),
                //                        QApplication::activeWindow(),
                //        KNotification::CloseWhenWidgetActivated);
                _notifiedActivity=true;
            }
        }
    }

    if ( state==NOTIFYACTIVITY && !_monitorActivity )
        state = NOTIFYNORMAL;
    if ( state==NOTIFYSILENCE && !_monitorSilence )
        state = NOTIFYNORMAL;

    emit stateChanged(state);
}

void TerminalModel::onViewSizeChange(int /*height*/, int /*width*/)
{
    updateTerminalSize();
}
void TerminalModel::onEmulationSizeChange(int lines , int columns)
{
    setSize( QSize(lines,columns) );
}

void TerminalModel::updateTerminalSize()
{
    QListIterator<TerminalView*> viewIter(_views);

    int minLines = -1;
    int minColumns = -1;

    // minimum number of lines and columns that views require for
    // their size to be taken into consideration ( to avoid problems
    // with new view widgets which haven't yet been set to their correct size )
    const int VIEW_LINES_THRESHOLD = 2;
    const int VIEW_COLUMNS_THRESHOLD = 2;

    //select largest number of lines and columns that will fit in all visible views
    while ( viewIter.hasNext() )
    {
        TerminalView* view = viewIter.next();
        if ( view->isHidden() == false &&
             view->lines() >= VIEW_LINES_THRESHOLD &&
             view->columns() >= VIEW_COLUMNS_THRESHOLD )
        {
            minLines = (minLines == -1) ? view->lines() : qMin( minLines , view->lines() );
            minColumns = (minColumns == -1) ? view->columns() : qMin( minColumns , view->columns() );
        }
    }

    // backend emulation must have a _terminal of at least 1 column x 1 line in size
    if ( minLines > 0 && minColumns > 0 )
    {
        _emulation->setImageSize( minLines , minColumns );
        _kpty->setWinSize (minLines, minColumns);
        //_shellProcess->setWindowSize( minLines , minColumns );
    }
}

void TerminalModel::refresh()
{
}

void TerminalModel::close()
{
    _autoClose = true;
    _wantedClose = true;
}

void TerminalModel::sendText(const QString &text) const
{
    _emulation->sendText(text);
}

TerminalModel::~TerminalModel()
{
    delete _emulation;
}

void TerminalModel::setProfileKey(const QString& key)
{
    _profileKey = key;
    emit profileChanged(key);
}
QString TerminalModel::profileKey() const { return _profileKey; }

void TerminalModel::done(int)
{
    emit finished();
}

Emulation* TerminalModel::emulation() const
{
    return _emulation;
}

QString TerminalModel::keyBindings() const
{
    return _emulation->keyBindings();
}

void TerminalModel::setKeyBindings(const QString &id)
{
    _emulation->setKeyBindings(id);
}

void TerminalModel::setHistoryType(const HistoryType &hType)
{
    _emulation->setHistory(hType);
}

const HistoryType& TerminalModel::historyType() const
{
    return _emulation->history();
}

void TerminalModel::clearHistory()
{
    _emulation->clearHistory();
}

// unused currently
bool TerminalModel::isMonitorActivity() const { return _monitorActivity; }
// unused currently
bool TerminalModel::isMonitorSilence()  const { return _monitorSilence; }

void TerminalModel::setMonitorActivity(bool _monitor)
{
    _monitorActivity=_monitor;
    _notifiedActivity=false;

    activityStateSet(NOTIFYNORMAL);
}

void TerminalModel::setMonitorSilence(bool _monitor)
{
    if (_monitorSilence==_monitor)
        return;

    _monitorSilence=_monitor;
    if (_monitorSilence)
    {
        _monitorTimer->start(_silenceSeconds*1000);
    }
    else
        _monitorTimer->stop();

    activityStateSet(NOTIFYNORMAL);
}

void TerminalModel::setMonitorSilenceSeconds(int seconds)
{
    _silenceSeconds=seconds;
    if (_monitorSilence) {
        _monitorTimer->start(_silenceSeconds*1000);
    }
}

void TerminalModel::setAddToUtmp(bool set)
{
    _addToUtmp = set;
}

void TerminalModel::onReceiveBlock(const char* buf, int len )
{
    _emulation->receiveData( buf, len );
    emit receivedData( QString::fromLatin1( buf, len ) );
}

QSize TerminalModel::size()
{
    return _emulation->imageSize();
}

void TerminalModel::setSize(const QSize& size)
{
    if ((size.width() <= 1) || (size.height() <= 1))
        return;

    emit resizeRequest(size);
}
