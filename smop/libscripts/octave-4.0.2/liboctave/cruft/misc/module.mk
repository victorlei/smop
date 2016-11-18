EXTRA_DIST += \
  cruft/misc/module.mk \
  cruft/misc/d1mach-tst.for

CRUFT_SOURCES += \
  cruft/misc/blaswrap.c \
  cruft/misc/cquit.c \
  cruft/misc/d1mach.f \
  cruft/misc/f77-extern.cc \
  cruft/misc/f77-fcn.c \
  cruft/misc/i1mach.f \
  cruft/misc/lo-error.c \
  cruft/misc/quit.cc \
  cruft/misc/r1mach.f

CRUFT_INC += \
  cruft/misc/f77-fcn.h \
  cruft/misc/lo-error.h \
  cruft/misc/quit.h
