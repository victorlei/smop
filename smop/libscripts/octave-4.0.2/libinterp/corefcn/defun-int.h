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

#if !defined (octave_defun_int_h)
#define octave_defun_int_h 1

#include <string>

#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "symtab.h"
#include "version.h"

class octave_value;

extern OCTINTERP_API void print_usage (void);
extern OCTINTERP_API void print_usage (const std::string&);

extern OCTINTERP_API void check_version (const std::string& version,
                                         const std::string& fcn);

extern OCTINTERP_API void
install_builtin_function (octave_builtin::fcn f, const std::string& name,
                          const std::string& file, const std::string& doc,
                          bool can_hide_function = true);

extern OCTINTERP_API void
install_dld_function (octave_dld_function::fcn f, const std::string& name,
                      const octave_shlib& shl, const std::string& doc,
                      bool relative = false);

extern OCTINTERP_API void
install_mex_function (void *fptr, bool fmex, const std::string& name,
                      const octave_shlib& shl, bool relative = false);

extern OCTINTERP_API void
alias_builtin (const std::string& alias, const std::string& name);

// Gets the shlib of the currently executing DLD function, if any.
extern OCTINTERP_API octave_shlib
get_current_shlib (void);

// This is a convenience class that calls the above function automatically at
// construction time. When deriving new classes, you can either use it as a
// field or as a parent (with multiple inheritance).

class octave_auto_shlib : public octave_shlib
{
public:
  octave_auto_shlib (void)
    : octave_shlib (get_current_shlib ()) { }
  octave_auto_shlib (const octave_shlib& shl)
    : octave_shlib (shl) { }
};

extern OCTINTERP_API bool
defun_isargout (int, int);

extern OCTINTERP_API void
defun_isargout (int, int, bool *);

#define DECLARE_FUNX(name, args_name, nargout_name) \
  OCTAVE_EXPORT octave_value_list \
  name (const octave_value_list& args_name, int nargout_name)

#define DECLARE_FUN(name, args_name, nargout_name) \
  DECLARE_FUNX (F ## name, args_name, nargout_name)

// Define the code that will be used to insert the new function into
// the symbol table.  We look for this name instead of the actual
// function so that we can easily install the doc std::string too.

typedef bool (*octave_dld_fcn_installer) (const octave_shlib&, bool relative);

typedef octave_function *
  (*octave_dld_fcn_getter) (const octave_shlib&, bool relative);

#define DEFINE_FUN_INSTALLER_FUN(name, doc) \
  DEFINE_FUNX_INSTALLER_FUN(#name, F ## name, G ## name, doc)

#define DEFINE_FUNX_INSTALLER_FUN(name, fname, gname, doc) \
  extern "C" \
  OCTAVE_EXPORT \
  octave_function * \
  gname (const octave_shlib& shl, bool relative) \
  { \
    octave_function *retval = 0; \
 \
    check_version (OCTAVE_API_VERSION, name); \
 \
    if (! error_state) \
      { \
        octave_dld_function *fcn = octave_dld_function::create (fname, shl, name, doc); \
 \
        if (relative) \
          fcn->mark_relative (); \
 \
        retval = fcn; \
      } \
 \
    return retval; \
  }

// MAKE_BUILTINS is defined to extract function names and related
// information and create the *.df files that are eventually used to
// create the builtins.cc file.

#if defined (MAKE_BUILTINS)

// Generate code to install name in the symbol table.  The script
// mkdefs will create a .def file for every .cc file that uses DEFUN,
// or DEFCMD.

#define DEFUN_INTERNAL(name, args_name, nargout_name, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUN_INTERNAL (name, args_name, nargout_name, doc) \
  END_INSTALL_BUILTIN

#define DEFCONSTFUN_INTERNAL(name, args_name, nargout_name, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFCONSTFUN_INTERNAL (name, args_name, nargout_name, doc) \
  END_INSTALL_BUILTIN

#define DEFUNX_INTERNAL(name, fname, args_name, nargout_name, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUNX_INTERNAL (name, fname, args_name, nargout_name, doc) \
  END_INSTALL_BUILTIN

// Generate code to install name in the symbol table.  The script
// mkdefs will create a .def file for every .cc file that uses
// DEFUN_DLD.

#define DEFUN_DLD_INTERNAL(name, args_name, nargout_name, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUN_DLD_INTERNAL (name, args_name, nargout_name, doc) \
  END_INSTALL_BUILTIN

#define DEFUNX_DLD_INTERNAL(name, fname, args_name, nargout_name, doc) \
  BEGIN_INSTALL_BUILTIN \
    XDEFUNX_DLD_INTERNAL (name, fname, args_name, nargout_name, doc) \
  END_INSTALL_BUILTIN

// Generate code for making another name for an existing function.

#define DEFALIAS_INTERNAL(alias, name) \
  BEGIN_INSTALL_BUILTIN \
    XDEFALIAS_INTERNAL(alias, name) \
  END_INSTALL_BUILTIN

#else /* ! MAKE_BUILTINS */

// Generate the first line of the function definition.  This ensures
// that the internal functions all have the same signature.

#define DEFUN_INTERNAL(name, args_name, nargout_name, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFCONSTFUN_INTERNAL(name, args_name, nargout_name, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFUNX_INTERNAL(name, fname, args_name, nargout_name, doc) \
  DECLARE_FUNX (fname, args_name, nargout_name)

// No definition is required for an alias.

#define DEFALIAS_INTERNAL(alias, name)

#endif /* ! MAKE_BUILTINS */

#endif
