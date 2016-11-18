## Copyright (C) 2006-2015 Keith Goodman
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Command} {} mkoctfile [-options] file @dots{}
## @deftypefnx {Function File} {[@var{output}, @var{status}] =} mkoctfile (@dots{})
##
## The @code{mkoctfile} function compiles source code written in C, C++, or
## Fortran.  Depending on the options used with @code{mkoctfile}, the
## compiled code can be called within Octave or can be used as a stand-alone
## application.
##
## @code{mkoctfile} can be called from the shell prompt or from the Octave
## prompt.  Calling it from the Octave prompt simply delegates the call to
## the shell prompt.  The output is stored in the @var{output} variable and
## the exit status in the @var{status} variable.
##
## @code{mkoctfile} accepts the following options, all of which are optional
## except for the file name of the code you wish to compile:
##
## @table @samp
## @item -I DIR
## Add the include directory DIR to compile commands.
##
## @item -D DEF
## Add the definition DEF to the compiler call.
##
## @item -l LIB
## Add the library LIB to the link command.
##
## @item -L DIR
## Add the library directory DIR to the link command.
##
## @item  -M
## @itemx --depend
## Generate dependency files (.d) for C and C++ source files.
##
## @item -R DIR
## Add the run-time path to the link command.
##
## @item @nospell{-Wl,@dots{}}
## Pass flags though the linker like @nospell{"-Wl,-rpath=@dots{}"}.
## The quotes are needed since commas are interpreted as command
## separators.
##
## @item -W@dots{}
## Pass flags though the compiler like @nospell{"-Wa,OPTION"}.
##
## @item -c
## Compile but do not link.
##
## @item -g
## Enable debugging options for compilers.
##
## @item  -o FILE
## @itemx --output FILE
## Output file name.  Default extension is .oct (or .mex if @samp{--mex} is
## specified) unless linking a stand-alone executable.
##
## @item  -p VAR
## @itemx --print VAR
## Print the configuration variable VAR@.  Recognized variables are:
##
## @example
##    ALL_CFLAGS                  INCFLAGS
##    ALL_CXXFLAGS                INCLUDEDIR
##    ALL_FFLAGS                  LAPACK_LIBS
##    ALL_LDFLAGS                 LD_CXX
##    AR                          LDFLAGS
##    BLAS_LIBS                   LD_STATIC_FLAG
##    CC                          LFLAGS
##    CFLAGS                      LIBDIR
##    CPICFLAG                    LIBOCTAVE
##    CPPFLAGS                    LIBOCTINTERP
##    CXX                         LIBS
##    CXXFLAGS                    OCTAVE_HOME
##    CXXPICFLAG                  OCTAVE_LIBS
##    DEPEND_EXTRA_SED_PATTERN    OCTAVE_LINK_DEPS
##    DEPEND_FLAGS                OCTAVE_LINK_OPTS
##    DL_LD                       OCTAVE_PREFIX
##    DL_LDFLAGS                  OCTINCLUDEDIR
##    F77                         OCTLIBDIR
##    F77_INTEGER8_FLAG           OCT_LINK_DEPS
##    FFLAGS                      OCT_LINK_OPTS
##    FFTW3F_LDFLAGS              RANLIB
##    FFTW3F_LIBS                 RDYNAMIC_FLAG
##    FFTW3_LDFLAGS               READLINE_LIBS
##    FFTW3_LIBS                  SED
##    FFTW_LIBS                   SPECIAL_MATH_LIB
##    FLIBS                       XTRA_CFLAGS
##    FPICFLAG                    XTRA_CXXFLAGS
## @end example
##
## @item --link-stand-alone
## Link a stand-alone executable file.
##
## @item --mex
## Assume we are creating a MEX file.  Set the default output extension to
## ".mex".
##
## @item  -s
## @itemx --strip
## Strip the output file.
##
## @item  -v
## @itemx --verbose
## Echo commands as they are executed.
##
## @item file
## The file to compile or link.  Recognized file types are
##
## @example
## @group
##    .c    C source
##    .cc   C++ source
##    .C    C++ source
##    .cpp  C++ source
##    .f    Fortran source (fixed form)
##    .F    Fortran source (fixed form)
##    .f90  Fortran source (free form)
##    .F90  Fortran source (free form)
##    .o    object file
##    .a    library file
## @end group
## @end example
##
## @end table
## @end deftypefn

function [output, status] = mkoctfile (varargin)

  bindir = octave_config_info ("bindir");
  ext = octave_config_info ("EXEEXT");

  shell_script = fullfile (bindir,
                           sprintf ("mkoctfile-%s%s", OCTAVE_VERSION, ext));

  if (! exist (shell_script, "file"))
    __gripe_missing_component__ ("mkoctfile", "mkoctfile");
  endif

  cmd = ['"' shell_script '"'];
  for i = 1:nargin
    cmd = [cmd ' "' varargin{i} '"'];
  endfor

  [sys, out] = system (cmd);

  if (nargout > 0)
    [output, status] = deal (out, sys);
  else
    printf ("%s", out);
  endif

  if (sys != 0)
    warning ("mkoctfile exited with failure status");
  endif

endfunction

