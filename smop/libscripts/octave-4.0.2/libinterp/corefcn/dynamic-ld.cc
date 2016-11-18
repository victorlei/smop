/*

Copyright (C) 1993-2015 John W. Eaton

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

#include <iostream>
#include <list>

#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"
#include "singleton-cleanup.h"

#include <defaults.h>

#include "defun.h"
#include "dynamic-ld.h"
#include "ov-fcn.h"
#include "ov-dld-fcn.h"
#include "ov-mex-fcn.h"
#include "parse.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

#define STRINGIFY(s) STRINGIFY1(s)
#define STRINGIFY1(s) #s

class
octave_shlib_list
{
public:

  typedef std::list<octave_shlib>::iterator iterator;
  typedef std::list<octave_shlib>::const_iterator const_iterator;

  static void append (const octave_shlib& shl);

  static void remove (octave_shlib& shl, octave_shlib::close_hook cl_hook = 0);

  static octave_shlib find_file (const std::string& file_name);

  static void display (void);

private:

  octave_shlib_list (void) : lib_list () { }

  ~octave_shlib_list (void) { }

  void do_append (const octave_shlib& shl);

  void do_remove (octave_shlib& shl, octave_shlib::close_hook cl_hook = 0);

  octave_shlib do_find_file (const std::string& file_name) const;

  void do_display (void) const;

  static octave_shlib_list *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static bool instance_ok (void);

  // List of libraries we have loaded.
  std::list<octave_shlib> lib_list;

  // No copying!

  octave_shlib_list (const octave_shlib_list&);

  octave_shlib_list& operator = (const octave_shlib_list&);
};

octave_shlib_list *octave_shlib_list::instance = 0;

void
octave_shlib_list::do_append (const octave_shlib& shl)
{
  lib_list.push_back (shl);
}

void
octave_shlib_list::do_remove (octave_shlib& shl,
                              octave_shlib::close_hook cl_hook)
{
  for (iterator p = lib_list.begin (); p != lib_list.end (); p++)
    {
      if (*p == shl)
        {
          // Erase first to avoid potentially invalidating the pointer by the
          // following hooks.
          lib_list.erase (p);

          shl.close (cl_hook);

          break;
        }
    }
}

octave_shlib
octave_shlib_list::do_find_file (const std::string& file_name) const
{
  octave_shlib retval;

  for (const_iterator p = lib_list.begin (); p != lib_list.end (); p++)
    {
      if (p->file_name () == file_name)
        {
          retval = *p;
          break;
        }
    }

  return retval;
}

void
octave_shlib_list::do_display (void) const
{
  std::cerr << "current shared libraries:" << std::endl;
  for (const_iterator p = lib_list.begin (); p != lib_list.end (); p++)
    std::cerr << "  " << p->file_name () << std::endl;
}

bool
octave_shlib_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_shlib_list ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create shared library list object!");

      retval = false;
    }

  return retval;
}

void
octave_shlib_list::append (const octave_shlib& shl)
{
  if (instance_ok ())
    instance->do_append (shl);
}

void
octave_shlib_list::remove (octave_shlib& shl,
                           octave_shlib::close_hook cl_hook)
{
  if (instance_ok ())
    instance->do_remove (shl, cl_hook);
}

octave_shlib
octave_shlib_list::find_file (const std::string& file_name)
{
  return (instance_ok ())
         ? instance->do_find_file (file_name) : octave_shlib ();
}

void
octave_shlib_list::display (void)
{
  if (instance_ok ())
    instance->do_display ();
}

octave_dynamic_loader *octave_dynamic_loader::instance = 0;

bool octave_dynamic_loader::doing_load = false;

bool
octave_dynamic_loader::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_dynamic_loader ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create dynamic loader object!");

      retval = false;
    }

  return retval;
}

static void
do_clear_function (const std::string& fcn_name)
{
  warning_with_id ("Octave:reload-forces-clear", "  %s", fcn_name.c_str ());

  symbol_table::clear_dld_function (fcn_name);
}

static void
clear (octave_shlib& oct_file)
{
  if (oct_file.number_of_functions_loaded () > 1)
    {
      warning_with_id ("Octave:reload-forces-clear",
                       "reloading %s clears the following functions:",
                       oct_file.file_name ().c_str ());

      octave_shlib_list::remove (oct_file, do_clear_function);
    }
  else
    octave_shlib_list::remove (oct_file, symbol_table::clear_dld_function);
}

octave_function *
octave_dynamic_loader::do_load_oct (const std::string& fcn_name,
                                    const std::string& file_name,
                                    bool relative)
{
  octave_function *retval = 0;

  unwind_protect frame;

  frame.protect_var (octave_dynamic_loader::doing_load);

  doing_load = true;

  octave_shlib oct_file = octave_shlib_list::find_file (file_name);

  if (oct_file && oct_file.is_out_of_date ())
    clear (oct_file);

  if (! oct_file)
    {
      oct_file.open (file_name);

      if (! error_state && oct_file)
        octave_shlib_list::append (oct_file);
    }

  if (! error_state)
    {
      if (oct_file)
        {
          void *function = oct_file.search (fcn_name, name_mangler);

          if (! function)
            {
              // FIXME: can we determine this C mangling scheme
              // automatically at run time or configure time?

              function = oct_file.search (fcn_name, name_uscore_mangler);
            }

          if (function)
            {
              octave_dld_fcn_getter f
                = FCN_PTR_CAST (octave_dld_fcn_getter, function);

              retval = f (oct_file, relative);

              if (! retval)
                ::error ("failed to install .oct file function '%s'",
                         fcn_name.c_str ());
            }
        }
      else
        ::error ("%s is not a valid shared library",
                 file_name.c_str ());
    }

  return retval;
}

octave_function *
octave_dynamic_loader::do_load_mex (const std::string& fcn_name,
                                    const std::string& file_name,
                                    bool /*relative*/)
{
  octave_function *retval = 0;

  unwind_protect frame;

  frame.protect_var (octave_dynamic_loader::doing_load);

  doing_load = true;

  octave_shlib mex_file = octave_shlib_list::find_file (file_name);

  if (mex_file && mex_file.is_out_of_date ())
    clear (mex_file);

  if (! mex_file)
    {
      mex_file.open (file_name);

      if (! error_state && mex_file)
        octave_shlib_list::append (mex_file);
    }

  if (! error_state)
    {
      if (mex_file)
        {
          void *function = 0;

          bool have_fmex = false;

          function = mex_file.search (fcn_name, mex_mangler);

          if (! function)
            {
              // FIXME: can we determine this C mangling scheme
              // automatically at run time or configure time?

              function = mex_file.search (fcn_name, mex_uscore_mangler);

              if (! function)
                {
                  function = mex_file.search (fcn_name, mex_f77_mangler);

                  if (function)
                    have_fmex = true;
                }
            }

          if (function)
            retval = new octave_mex_function (function, have_fmex,
                                              mex_file, fcn_name);
          else
            ::error ("failed to install .mex file function '%s'",
                     fcn_name.c_str ());
        }
      else
        ::error ("%s is not a valid shared library",
                 file_name.c_str ());
    }

  return retval;
}

bool
octave_dynamic_loader::do_remove_oct (const std::string& fcn_name,
                                      octave_shlib& shl)
{
  bool retval = false;

  // We don't need to do anything if this is called because we are in
  // the process of reloading a .oct file that has changed.

  if (! doing_load)
    {
      retval = shl.remove (fcn_name);

      if (shl.number_of_functions_loaded () == 0)
        octave_shlib_list::remove (shl);
    }

  return retval;
}

bool
octave_dynamic_loader::do_remove_mex (const std::string& fcn_name,
                                      octave_shlib& shl)
{
  bool retval = false;

  // We don't need to do anything if this is called because we are in
  // the process of reloading a .oct file that has changed.

  if (! doing_load)
    {
      retval = shl.remove (fcn_name);

      if (shl.number_of_functions_loaded () == 0)
        octave_shlib_list::remove (shl);
    }

  return retval;
}

octave_function *
octave_dynamic_loader::load_oct (const std::string& fcn_name,
                                 const std::string& file_name,
                                 bool relative)
{
  return (instance_ok ())
         ? instance->do_load_oct (fcn_name, file_name, relative) : 0;
}

octave_function *
octave_dynamic_loader::load_mex (const std::string& fcn_name,
                                 const std::string& file_name,
                                 bool relative)
{
  return (instance_ok ())
         ? instance->do_load_mex (fcn_name, file_name, relative) : 0;
}

bool
octave_dynamic_loader::remove_oct (const std::string& fcn_name,
                                   octave_shlib& shl)
{
  return (instance_ok ()) ? instance->do_remove_oct (fcn_name, shl) : false;
}

bool
octave_dynamic_loader::remove_mex (const std::string& fcn_name,
                                   octave_shlib& shl)
{
  return (instance_ok ()) ? instance->do_remove_mex (fcn_name, shl) : false;
}

std::string
octave_dynamic_loader::name_mangler (const std::string& name)
{
  return "G" + name;
}

std::string
octave_dynamic_loader::name_uscore_mangler (const std::string& name)
{
  return "_G" + name;
}

std::string
octave_dynamic_loader::mex_mangler (const std::string&)
{
  return "mexFunction";
}

std::string
octave_dynamic_loader::mex_uscore_mangler (const std::string&)
{
  return "_mexFunction";
}

std::string
octave_dynamic_loader::mex_f77_mangler (const std::string&)
{
  return STRINGIFY (F77_FUNC (mexfunction, MEXFUNCTION));
}
