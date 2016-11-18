EXTRA_DIST += \
  qterminal-module.mk

noinst_HEADERS += \
  qterminal/libqterminal/QTerminal.h \
  qterminal/libqterminal/win32/QTerminalColors.h \
  qterminal/libqterminal/win32/QWinTerminalImpl.h \
  qterminal/libqterminal/unix/BlockArray.h \
  qterminal/libqterminal/unix/Character.h \
  qterminal/libqterminal/unix/CharacterColor.h \
  qterminal/libqterminal/unix/Emulation.h \
  qterminal/libqterminal/unix/ExtendedDefaultTranslator.h \
  qterminal/libqterminal/unix/ExtendedDefaultTranslatorMac.h \
  qterminal/libqterminal/unix/Filter.h \
  qterminal/libqterminal/unix/History.h \
  qterminal/libqterminal/unix/KeyboardTranslator.h \
  qterminal/libqterminal/unix/konsole_wcwidth.h \
  qterminal/libqterminal/unix/kpty.h \
  qterminal/libqterminal/unix/kpty_p.h \
  qterminal/libqterminal/unix/LineFont.h \
  qterminal/libqterminal/unix/QUnixTerminalImpl.h \
  qterminal/libqterminal/unix/Screen.h \
  qterminal/libqterminal/unix/ScreenWindow.h \
  qterminal/libqterminal/unix/TerminalCharacterDecoder.h \
  qterminal/libqterminal/unix/Vt102Emulation.h \
  qterminal/libqterminal/unix/SelfListener.h \
  qterminal/libqterminal/unix/TerminalModel.h \
  qterminal/libqterminal/unix/TerminalView.h

qterminal_libqterminal_la_MOC = \
  qterminal/libqterminal/moc-QTerminal.cc

nodist_qterminal_libqterminal_la_SOURCES = $(qterminal_libqterminal_la_MOC)

qterminal_libqterminal_la_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  @QT_CPPFLAGS@ \
  -I$(srcdir)/qterminal/libqterminal \
  -I$(srcdir)/src

qterminal_libqterminal_la_CFLAGS = $(AM_CFLAGS)

qterminal_libqterminal_la_CXXFLAGS = $(AM_CXXFLAGS)

if WIN32_TERMINAL

qterminal_libqterminal_la_SOURCES = \
  qterminal/libqterminal/win32/QTerminalColors.cpp \
  qterminal/libqterminal/win32/QWinTerminalImpl.cpp \
  qterminal/libqterminal/QTerminal.cc

qterminal_libqterminal_la_MOC += \
  qterminal/libqterminal/win32/moc-QWinTerminalImpl.cc

qterminal_libqterminal_la_CPPFLAGS += -DUNICODE

# This flag is required to let MOC know about Q_OS_WIN32.
MOC_CPPFLAGS += -DQ_OS_WIN32

else

qterminal_libqterminal_la_SOURCES = \
  qterminal/libqterminal/unix/BlockArray.cpp \
  qterminal/libqterminal/unix/Emulation.cpp \
  qterminal/libqterminal/unix/Filter.cpp \
  qterminal/libqterminal/unix/History.cpp \
  qterminal/libqterminal/unix/KeyboardTranslator.cpp \
  qterminal/libqterminal/unix/konsole_wcwidth.cpp \
  qterminal/libqterminal/unix/kpty.cpp \
  qterminal/libqterminal/unix/QUnixTerminalImpl.cpp \
  qterminal/libqterminal/unix/Screen.cpp \
  qterminal/libqterminal/unix/ScreenWindow.cpp \
  qterminal/libqterminal/unix/TerminalCharacterDecoder.cpp \
  qterminal/libqterminal/unix/Vt102Emulation.cpp \
  qterminal/libqterminal/unix/SelfListener.cpp \
  qterminal/libqterminal/unix/TerminalModel.cpp \
  qterminal/libqterminal/unix/TerminalView.cpp \
  qterminal/libqterminal/QTerminal.cc

qterminal_libqterminal_la_MOC += \
  qterminal/libqterminal/unix/moc-Emulation.cc \
  qterminal/libqterminal/unix/moc-Filter.cc \
  qterminal/libqterminal/unix/moc-QUnixTerminalImpl.cc \
  qterminal/libqterminal/unix/moc-ScreenWindow.cc \
  qterminal/libqterminal/unix/moc-SelfListener.cc \
  qterminal/libqterminal/unix/moc-TerminalModel.cc \
  qterminal/libqterminal/unix/moc-TerminalView.cc \
  qterminal/libqterminal/unix/moc-Vt102Emulation.cc

endif

noinst_LTLIBRARIES += qterminal/libqterminal.la

CLEANFILES += $(qterminal_libqterminal_la_MOC)
