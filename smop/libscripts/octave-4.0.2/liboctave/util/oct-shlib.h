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

#if !defined (octave_oct_shlib_h)
#define octave_oct_shlib_h 1

#include <string>
#include <map>

#include "oct-time.h"
#include "oct-refcount.h"

class
OCTAVE_API
octave_shlib
{
public: // FIXME: make this class private?

  typedef std::string (*name_mangler) (const std::string&);
  typedef void (*close_hook) (const std::string&);

  class shlib_rep
  {
  public:

    shlib_rep (void)
      : count (1), file (), tm_loaded (time_t ()), fcn_names () { }

  protected:

    shlib_rep (const std::string& f);

  public:

    virtual ~shlib_rep (void)
    {
      instances.erase (file);
    }

    virtual bool is_open (void) const
    { return false; }

    virtual void *search (const std::string&, name_mangler = 0)
    { return 0; }

    bool is_out_of_date (void) const;

    // This method will be overridden conditionally.
    static shlib_rep *new_instance (const std::string& f);

    static shlib_rep *get_instance (const std::string& f, bool fake);

    octave_time time_loaded (void) const
    { return tm_loaded; }

    std::string file_name (void) const
    { return file; }

    size_t num_fcn_names (void) const { return fcn_names.size (); }

    void add_fcn_name (const std::string&);

    bool remove_fcn_name (const std::string&);

    void do_close_hook (close_hook cl_hook);

  public:

    octave_refcount<int> count;

  protected:

    void fake_reload (void);

    std::string file;
    octave_time tm_loaded;

    // Set of hooked function names.
    typedef std::map<std::string, size_t>::iterator fcn_names_iterator;
    typedef std::map<std::string, size_t>::const_iterator fcn_names_const_iterator;

    std::map<std::string, size_t> fcn_names;

    static std::map<std::string, shlib_rep *> instances;
  };

private:

  static shlib_rep nil_rep;

public:

  octave_shlib (void) : rep (&nil_rep) { rep->count++; }

  octave_shlib (const std::string& f, bool fake = true)
    : rep (shlib_rep::get_instance (f, fake)) { }

  ~octave_shlib (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  octave_shlib (const octave_shlib& sl)
    : rep (sl.rep)
  {
    rep->count++;
  }

  octave_shlib& operator = (const octave_shlib& sl)
  {
    if (rep != sl.rep)
      {
        if (--rep->count == 0)
          delete rep;

        rep = sl.rep;
        rep->count++;
      }

    return *this;
  }

  bool operator == (const octave_shlib& sl) const
  { return (rep == sl.rep); }

  operator bool () const { return rep->is_open (); }

  void open (const std::string& f)
  { *this = octave_shlib (f); }

  void close (close_hook cl_hook = 0)
  {
    if (cl_hook)
      rep->do_close_hook (cl_hook);

    *this = octave_shlib ();
  }

  void *search (const std::string& nm, name_mangler mangler = 0) const
  {
    void *f = rep->search (nm, mangler);
    if (f)
      rep->add_fcn_name (nm);

    return f;
  }

  void add (const std::string& name)
  { rep->add_fcn_name (name); }

  bool remove (const std::string& name)
  { return rep->remove_fcn_name (name); }

  size_t number_of_functions_loaded (void) const
  { return rep->num_fcn_names (); }

  bool is_out_of_date (void) const
  { return rep->is_out_of_date (); }

  std::string file_name (void) const
  { return rep->file_name (); }

  octave_time time_loaded (void) const
  { return rep->time_loaded (); }

private:

  shlib_rep *rep;
};

#endif
