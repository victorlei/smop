/*

Copyright (C) 1999-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#include <map>

#if defined (HAVE_SHL_LOAD_API)
#include <cerrno>
#include <cstring>
#endif

#if defined (HAVE_DYLD_API)
#include <mach-o/dyld.h>
#endif

extern "C"
{
#if defined (HAVE_DLOPEN_API)
#if defined (HAVE_DLFCN_H)
#include <dlfcn.h>
#else
extern void *dlopen (const char *, int);
extern const char *dlerror (void);
extern void *dlsym (void *, const char *);
extern int dlclose (void *);
#endif
#elif defined (HAVE_SHL_LOAD_API)
#include <dl.h>
#elif defined (HAVE_LOADLIBRARY_API)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
}

#include "file-stat.h"
#include "lo-error.h"
#include "oct-shlib.h"
#include "str-vec.h"

octave_shlib::shlib_rep::shlib_rep (const std::string& f)
  : count (1), file (f), tm_loaded (), fcn_names ()
{
  instances[f] = this;

  if (is_out_of_date ())
    (*current_liboctave_warning_with_id_handler)
      ("Octave:warn-future-time-stamp",
       "timestamp on file %s is in the future", file.c_str ());
}

bool
octave_shlib::shlib_rep::is_out_of_date (void) const
{
  file_stat fs (file);
  return (fs && fs.is_newer (tm_loaded));
}

void
octave_shlib::shlib_rep::fake_reload (void)
{
  // We can't actually reload the library, but we'll pretend we did.
  file_stat fs (file);
  if (fs && fs.is_newer (tm_loaded))
    {
      tm_loaded = fs.mtime ();

      (*current_liboctave_warning_with_id_handler)
        ("Octave:library-reload",
         "library %s not reloaded due to existing references", file.c_str ());
    }
}

octave_shlib::shlib_rep *
octave_shlib::shlib_rep::get_instance (const std::string& f, bool fake)
{
  shlib_rep *retval = 0;
  std::map<std::string, shlib_rep *>::iterator p = instances.find (f);
  if (p != instances.end ())
    {
      retval = p->second;
      retval->count++;
      if (fake)
        retval->fake_reload ();
    }
  else
    retval = new_instance (f);

  return retval;
}

void
octave_shlib::shlib_rep::add_fcn_name (const std::string& name)
{
  fcn_names_iterator p = fcn_names.find (name);

  if (p == fcn_names.end ())
    fcn_names[name] = 1;
  else
    ++(p->second);
}

bool
octave_shlib::shlib_rep::remove_fcn_name (const std::string& fcn_name)
{
  bool retval = false;

  fcn_names_iterator p = fcn_names.find (fcn_name);

  if (p != fcn_names.end () && --(p->second) == 0)
    {
      fcn_names.erase (fcn_name);
      retval = true;
    }

  return retval;
}

void
octave_shlib::shlib_rep::do_close_hook (octave_shlib::close_hook cl_hook)
{
  for (fcn_names_iterator p = fcn_names.begin (); p != fcn_names.end (); p++)
    cl_hook (p->first);

  fcn_names.clear ();
}

std::map<std::string, octave_shlib::shlib_rep *> octave_shlib::shlib_rep::instances;

octave_shlib::shlib_rep octave_shlib::nil_rep;

#if defined (HAVE_DLOPEN_API)

class
octave_dlopen_shlib : public octave_shlib::shlib_rep
{
public:

  octave_dlopen_shlib (const std::string& f);

  ~octave_dlopen_shlib (void);

  void *search (const std::string& name,
                octave_shlib::name_mangler mangler = 0);

  // FIXME: this is possibly redundant because failure to open a library will
  // normally throw an exception, avoiding the construction of an invalid
  // library. Leave it here for possible future use.

  bool is_open (void) const { return (library != 0); }

private:

  // No copying!

  octave_dlopen_shlib (const octave_dlopen_shlib&);

  octave_dlopen_shlib& operator = (const octave_dlopen_shlib&);

  void *library;
};

octave_dlopen_shlib::octave_dlopen_shlib (const std::string& f)
  : octave_shlib::shlib_rep (f), library (0)
{
  int flags = 0;

  // Use RTLD_NOW to resolve all symbols before dlopen returns.
  // By using this option, dlopen will detect errors and Octave
  // won't exit if there are unresolved symbols in the file we are
  // loading, and we may even get a useful diagnostic.
#if defined (RTLD_NOW)
  flags |= RTLD_NOW;
#endif

  library = dlopen (file.c_str (), flags);

  if (! library)
    {
      const char *msg = dlerror ();

      if (msg)
        (*current_liboctave_error_handler) ("%s: failed to load: %s",
                                            file.c_str (), msg);
      else
        (*current_liboctave_error_handler) ("%s: failed to load",
                                            file.c_str ());
    }
}

octave_dlopen_shlib::~octave_dlopen_shlib (void)
{
  if (library)
    dlclose (library);
}

void *
octave_dlopen_shlib::search (const std::string& name,
                             octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

      if (mangler)
        sym_name = mangler (name);

      function = dlsym (library, sym_name.c_str ());
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is not open", file.c_str ());

  return function;
}

#elif defined (HAVE_SHL_LOAD_API)

class
octave_shl_load_shlib : public octave_shlib::shlib_rep
{
public:

  octave_shl_load_shlib (const std::string& f);

  ~octave_shl_load_shlib (void);

  void *search (const std::string& name,
                octave_shlib::name_mangler mangler = 0);

  bool is_open (void) const { return (library != 0); }

private:

  // No copying!

  octave_shl_load_shlib (const octave_shl_load_shlib&);

  octave_shl_load_shlib& operator = (const octave_shl_load_shlib&);

  shl_t library;
};

octave_shl_load_shlib::octave_shl_load_shlib (const std::string& f)
  : octave_shlib::shlib_rep (f), library (0)
{
  file = f;

  library = shl_load (file.c_str (), BIND_IMMEDIATE, 0L);

  if (! library)
    {
      using namespace std;
      (*current_liboctave_error_handler) ("%s", gnulib::strerror (errno));
    }
}

octave_shl_load_shlib::~octave_shl_load_shlib (void)
{
  if (library)
    shl_unload (library);
}

void *
octave_shl_load_shlib::search (const std::string& name,
                               octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

      if (mangler)
        sym_name = mangler (name);

      int status = shl_findsym (&library, sym_name.c_str (),
                                TYPE_UNDEFINED, &function);
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is not open", file.c_str ());

  return function;
}

#elif defined (HAVE_LOADLIBRARY_API)

class
octave_w32_shlib: public octave_shlib::shlib_rep
{
public:

  octave_w32_shlib (const std::string& f);

  ~octave_w32_shlib (void);

  void *search (const std::string& name,
                octave_shlib::name_mangler mangler = 0);

  bool is_open (void) const { return (handle != 0); }

private:

  // No copying!

  octave_w32_shlib (const octave_w32_shlib&);

  octave_w32_shlib& operator = (const octave_w32_shlib&);

  HINSTANCE handle;
};

octave_w32_shlib::octave_w32_shlib (const std::string& f)
  : octave_shlib::shlib_rep (f), handle (0)
{
  handle = LoadLibrary (file.c_str ());

  if (! handle)
    {
      DWORD lastError = GetLastError ();
      char *msg;

      switch (lastError)
        {
        case ERROR_MOD_NOT_FOUND:
        case ERROR_DLL_NOT_FOUND:
          msg = "could not find library or dependents";
          break;

        case ERROR_INVALID_DLL:
          msg = "library or its dependents are damaged";
          break;

        case ERROR_DLL_INIT_FAILED:
          msg = "library initialization routine failed";
          break;

        default:
          msg = "library open failed";
        }

      (*current_liboctave_error_handler) ("%s: %s", msg, file.c_str ());
    }
}

octave_w32_shlib::~octave_w32_shlib (void)
{
  if (handle)
    FreeLibrary (handle);
}

extern "C"
{
  void * octave_w32_search (HINSTANCE handle, const char * name);
}

void *
octave_w32_shlib::search (const std::string& name,
                          octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

      if (mangler)
        sym_name = mangler (name);

      function = octave_w32_library_search (handle, sym_name.c_str ());
    }
  else
    (*current_liboctave_error_handler)
      ("shared library %s is not open", file.c_str ());

  return function;
}

#elif defined (HAVE_DYLD_API)

class
octave_dyld_shlib : public octave_shlib::shlib_rep
{
public:

  octave_dyld_shlib (void);

  ~octave_dyld_shlib (void);

  void open (const std::string& f);

  void *search (const std::string& name,
                octave_shlib::name_mangler mangler = 0);

  void close (octave_shlib::close_hook cl_hook = 0);

  bool is_open (void) const {return (handle != 0); }

private:

  // No copying!

  octave_dyld_shlib (const octave_dyld_shlib&);

  octave_dyld_shlib& operator = (const octave_dyld_shlib&);

  NSObjectFileImage img;
  NSModule handle;
};

octave_dyld_shlib::octave_dyld_shlib (const std::string& f)
  : octave_shlib::shlib_rep (f), handle (0)
{
  int returnCode = NSCreateObjectFileImageFromFile (file.c_str (), &img);

  if (NSObjectFileImageSuccess == returnCode)
    {
      handle = NSLinkModule (img, file.c_str (),
                             (NSLINKMODULE_OPTION_RETURN_ON_ERROR
                              | NSLINKMODULE_OPTION_PRIVATE));
      if (! handle)
        {
          NSLinkEditErrors ler;
          int lerno;
          const char *file2;
          const char *errstr = 0;

          NSLinkEditError (&ler, &lerno, &file2, &errstr);

          if (! errstr)
            errstr = "unspecified error";

          (*current_liboctave_error_handler)
            ("%s: %s", file.c_str (), errstr);
        }
    }
  else
    {
      (*current_liboctave_error_handler)
        ("got NSObjectFileImageReturnCode %d", returnCode);

      // FIXME: should use NSLinkEditError () to get
      // more info on what went wrong.
    }
}

octave_dyld_shlib::~octave_dyld_shlib (void)
{
  if (handle)
    NSUnLinkModule (handle, NSUNLINKMODULE_OPTION_RESET_LAZY_REFERENCES);

  NSDestroyObjectFileImage (img);
}

void *
octave_dyld_shlib::search (const std::string& name,
                           octave_shlib::name_mangler mangler)
{
  void *function = 0;

  if (is_open ())
    {
      std::string sym_name = name;

      if (mangler)
        sym_name = mangler (name);

      NSSymbol symbol = NSLookupSymbolInModule (handle, sym_name.c_str ());

      if (symbol)
        {
          function = NSAddressOfSymbol (symbol);
        }
    }
  else
    (*current_liboctave_error_handler)
      ("bundle %s is not open", file.c_str ());

  return function;
}

#endif

octave_shlib::shlib_rep *
octave_shlib::shlib_rep::new_instance (const std::string& f)
{
#if defined (HAVE_DLOPEN_API)
  return new octave_dlopen_shlib (f);
#elif defined (HAVE_SHL_LOAD_API)
  return new octave_shl_load_shlib (f);
#elif defined (HAVE_LOADLIBRARY_API)
  return new octave_w32_shlib (f);
#elif defined (HAVE_DYLD_API)
  return new octave_dyld_shlib (f);
#else
  (*current_liboctave_error_handler)
    ("no API for dynamic loading is available");
  return new shlib_rep ();
#endif
}
