/*

Copyright (C) 1994-2015 John W. Eaton

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

#if !defined (octave_defun_dld_h)
#define octave_defun_dld_h 1

#if defined (octave_defun_h)
#error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

// Define a builtin function that may be loaded dynamically at run
// time.
//
// If Octave is not configured for dynamic linking of builtin
// functions, this is the same as DEFUN, except that it will generate
// an extra externally visible function.
//
// The first DECLARE_FUN is for the benefit of the installer function
// and the second is for the definition of the function.

#if defined (MAKE_BUILTINS)

#define DEFUN_DLD(name, args_name, nargout_name, doc) \
  DEFUN_DLD_INTERNAL (name, args_name, nargout_name, doc)

// This one can be used when 'name' cannot be used directly (if it is
// already defined as a macro).  In that case, name is already a
// quoted string, and the internal name of the function must be passed
// too (the convention is to use a prefix of "F", so "foo" becomes
// "Ffoo") as well as the name of the generated installer function
// (the convention is to use a prefix of "G", so "foo" becomes "Gfoo").

#define DEFUNX_DLD(name, fname, gname, args_name, nargout_name, doc) \
  DEFUNX_DLD_INTERNAL (name, fname, args_name, nargout_name, doc)

#else

#define DEFUN_DLD(name, args_name, nargout_name, doc) \
  DECLARE_FUN (name, args_name, nargout_name); \
  DEFINE_FUN_INSTALLER_FUN (name, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFUNX_DLD(name, fname, gname, args_name, nargout_name, doc) \
  DECLARE_FUNX (fname, args_name, nargout_name); \
  DEFINE_FUNX_INSTALLER_FUN (name, fname, gname, doc) \
  DECLARE_FUNX (fname, args_name, nargout_name)

#endif

#endif
