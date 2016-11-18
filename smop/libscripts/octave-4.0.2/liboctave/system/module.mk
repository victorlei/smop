EXTRA_DIST += \
  system/module.mk

SYSTEM_INC = \
  system/dir-ops.h \
  system/file-ops.h \
  system/file-stat.h \
  system/lo-sysdep.h \
  system/mach-info.h \
  system/oct-env.h \
  system/oct-group.h \
  system/oct-openmp.h \
  system/oct-passwd.h \
  system/oct-syscalls.h \
  system/oct-time.h \
  system/oct-uname.h \
  system/pathlen.h \
  system/sysdir.h \
  system/syswait.h

SYSTEM_SRC = \
  system/dir-ops.cc \
  system/file-ops.cc \
  system/file-stat.cc \
  system/lo-sysdep.cc \
  system/mach-info.cc \
  system/oct-env.cc \
  system/oct-group.cc \
  system/oct-passwd.cc \
  system/oct-syscalls.cc \
  system/oct-time.cc \
  system/oct-uname.cc

noinst_LTLIBRARIES += system/libsystem.la

system_libsystem_la_SOURCES = $(SYSTEM_SRC)
system_libsystem_la_CPPFLAGS = $(liboctave_la_CPPFLAGS)

liboctave_la_LIBADD += system/libsystem.la
