/*

Copyright (C) 2012-2015 Michael Goffioul

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

#if !defined (octave_classdef_h)
#define octave_classdef_h 1

#include <map>
#include <set>
#include <string>

#include "oct-map.h"
#include "oct-refcount.h"
#include "ov-base.h"
#include "symtab.h"

class cdef_object;
class cdef_class;
class cdef_property;
class cdef_method;
class cdef_package;

class tree_classdef;

// This is mainly a boostrap class to declare the expected interface.
// The actual base class is cdef_class_base, which is declared after
// cdef_object, such that it can contain cdef_object objects.
class
cdef_object_rep
{
public:
  friend class cdef_object;

public:
  cdef_object_rep (void) : refcount (1) { }

  virtual ~cdef_object_rep (void) { }

  virtual cdef_class get_class (void) const;

  virtual void set_class (const cdef_class&)
  { gripe_invalid_object ("set_class"); }

  virtual cdef_object_rep* clone (void) const
  {
    gripe_invalid_object ("clone");
    return new cdef_object_rep ();
  }

  virtual cdef_object_rep* empty_clone (void) const
  {
    gripe_invalid_object ("empty_clone");
    return new cdef_object_rep ();
  }

  virtual cdef_object_rep* copy (void) const
  {
    gripe_invalid_object ("copy");
    return new cdef_object_rep ();
  }

  virtual cdef_object_rep* make_array (void) const
  {
    gripe_invalid_object ("make_array");
    return new cdef_object_rep ();
  }

  virtual bool is_array (void) const { return false; }

  virtual bool is_value_object (void) const { return false; }

  virtual bool is_handle_object (void) const { return false; }

  virtual bool is_meta_object (void) const { return false; }

  virtual Array<cdef_object> array_value (void) const
  {
    gripe_invalid_object ("array_value");
    return Array<cdef_object> ();
  }

  virtual void put (const std::string&, const octave_value&)
  { gripe_invalid_object ("put"); }

  virtual octave_value get (const std::string&) const
  {
    gripe_invalid_object ("get");
    return octave_value ();
  }

  virtual octave_value_list
  subsref (const std::string&, const std::list<octave_value_list>&,
           int, size_t&, const cdef_class&, bool)
  {
    gripe_invalid_object ("subsref");
    return octave_value_list ();
  }

  virtual octave_value
  subsasgn (const std::string&, const std::list<octave_value_list>&,
            const octave_value&)
  {
    gripe_invalid_object ("subsasgn");
    return octave_value ();
  }

  virtual string_vector map_keys (void) const;

  virtual bool is_valid (void) const { return false; }

  std::string class_name (void) const;

  virtual void mark_for_construction (const cdef_class&)
  { gripe_invalid_object ("mark_for_construction"); }

  virtual bool is_constructed_for (const cdef_class&) const
  {
    gripe_invalid_object ("is_constructed_for");
    return false;
  }

  virtual bool is_partially_constructed_for (const cdef_class&) const
  {
    gripe_invalid_object ("is_partially_constructed_for");
    return false;
  }

  virtual void mark_as_constructed (void)
  { gripe_invalid_object ("mark_as_constructed"); }

  virtual void mark_as_constructed (const cdef_class&)
  { gripe_invalid_object ("mark_as_constructed"); }

  virtual bool is_constructed (void) const
  {
    gripe_invalid_object ("is_constructed");
    return false;
  }

  virtual octave_idx_type static_count (void) const { return 0; }

  virtual void destroy (void) { delete this; }

  void release (void)
  {
    if (--refcount == static_count ())
      destroy ();
  }

  virtual dim_vector dims (void) const { return dim_vector (); }

protected:
  /* reference count */
  octave_refcount<octave_idx_type> refcount;

protected:
  /* Restricted copying */
  cdef_object_rep (const cdef_object_rep&)
    : refcount (1) { }

private:
  /* No assignment */
  cdef_object_rep& operator = (const cdef_object_rep& );

  void gripe_invalid_object (const char *who) const
  { error ("%s: invalid object", who); }
};

class
cdef_object
{
public:
  /* FIXME: use a null object */
  cdef_object (void)
    : rep (new cdef_object_rep ()) { }

  cdef_object (const cdef_object& obj)
    : rep (obj.rep)
  {
    rep->refcount++;
  }

  cdef_object (cdef_object_rep *r)
    : rep (r) { }

  virtual ~cdef_object (void)
  { rep->release (); }

  cdef_object& operator = (const cdef_object& obj)
  {
    if (rep != obj.rep)
      {
        rep->release ();

        rep = obj.rep;
        rep->refcount++;
      }

    return *this;
  }

  cdef_class get_class (void) const;

  void set_class (const cdef_class& cls) { rep->set_class (cls); }

  std::string class_name (void) const
  { return rep->class_name (); }

  cdef_object clone (void) const
  { return cdef_object (rep->clone ()); }

  cdef_object empty_clone (void) const
  { return cdef_object (rep->empty_clone ()); }

  dim_vector dims (void) const { return rep->dims (); }

  cdef_object make_array (void) const
  { return cdef_object (rep->make_array ()); }

  cdef_object copy (void) const
  { return cdef_object (rep->copy ()); }

  bool is_array (void) const { return rep->is_array (); }

  bool is_value_object (void) const { return rep->is_value_object (); }

  bool is_handle_object (void) const { return rep->is_handle_object (); }

  bool is_meta_object (void) const { return rep->is_meta_object (); }

  Array<cdef_object> array_value (void) const { return rep->array_value (); }

  void put (const std::string& pname, const octave_value& val)
  { rep->put (pname, val); }

  octave_value get (const std::string& pname) const
  { return rep->get (pname); }

  octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout, size_t& skip, const cdef_class& context,
           bool auto_add = false)
  { return rep->subsref (type, idx, nargout, skip, context, auto_add); }

  octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs, int ignore_copies = 0)
  {
    make_unique (ignore_copies);
    return rep->subsasgn (type, idx, rhs);
  }

  string_vector map_keys (void) const { return rep->map_keys (); }

  octave_map map_value (void) const;

  const cdef_object_rep* get_rep (void) const { return rep; }

  bool ok (void) const { return rep->is_valid (); }

  void mark_for_construction (const cdef_class& cls)
  { rep->mark_for_construction (cls); }

  bool is_constructed (void) const { return rep->is_constructed (); }

  bool is_constructed_for (const cdef_class& cls) const
  { return rep->is_constructed_for (cls); }

  bool is_partially_constructed_for (const cdef_class& cls) const
  { return rep->is_partially_constructed_for (cls); }

  void mark_as_constructed (void) { rep->mark_as_constructed (); }

  void mark_as_constructed (const cdef_class& cls)
  { rep->mark_as_constructed (cls); }

  bool is (const cdef_object& obj) const { return rep == obj.rep; }

protected:
  cdef_object_rep* get_rep (void) { return rep; }

  void make_unique (int ignore_copies)
  {
    if (rep->refcount > ignore_copies + 1)
      *this = clone ();
  }

private:
  cdef_object_rep *rep;
};

class
cdef_object_base : public cdef_object_rep
{
public:
  cdef_object_base (void)
    : cdef_object_rep (), klass ()
  {
    register_object ();
  }

  ~cdef_object_base (void) { unregister_object (); }

  cdef_class get_class (void) const;

  void set_class (const cdef_class& cls);

  cdef_object_rep* empty_clone (void) const
  { return new cdef_object_base (*this); }

  cdef_object_rep* make_array (void) const;

protected:
  // Restricted copying!
  cdef_object_base (const cdef_object_base& obj)
    : cdef_object_rep (obj), klass (obj.klass)
  {
    register_object ();
  }

private:
  void register_object (void);

  void unregister_object (void);

private:
  // The class of the object
  cdef_object klass;

private:
  // No assignment!
  cdef_object_base& operator = (const cdef_object_base&);
};

class
cdef_object_array : public cdef_object_base
{
public:
  cdef_object_array (void) : cdef_object_base () { }

  cdef_object_array (const Array<cdef_object>& a)
    : cdef_object_base (), array (a) { }

  cdef_object_rep* clone (void) const
  { return new cdef_object_array (*this); }

  dim_vector dims (void) const { return array.dims (); }

  bool is_valid (void) const { return true; }

  bool is_array (void) const { return true; }

  Array<cdef_object> array_value (void) const { return array; }

  octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout, size_t& skip, const cdef_class& context,
           bool auto_add);

  octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs);

private:
  Array<cdef_object> array;

private:
  void fill_empty_values (void) { fill_empty_values (array); }

  void fill_empty_values (Array<cdef_object>& arr);

  // Private copying!
  cdef_object_array (const cdef_object_array& obj)
    : cdef_object_base (obj), array (obj.array) { }

  // No assignment!
  cdef_object_array& operator = (const cdef_object_array&);
};

class
cdef_object_scalar : public cdef_object_base
{
public:
  cdef_object_scalar (void) : cdef_object_base () { }

  ~cdef_object_scalar (void) { }

  dim_vector dims (void) const { return dim_vector (1, 1); }

  void put (const std::string& pname, const octave_value& val)
  { map.assign (pname, val); }

  octave_value get (const std::string& pname) const
  {
    Cell val = map.contents (pname);

    if (val.numel () > 0)
      return val(0, 0);
    else
      {
        error ("get: unknown slot: %s", pname.c_str ());
        return octave_value ();
      }
  }

  octave_value_list
  subsref (const std::string& type, const std::list<octave_value_list>& idx,
           int nargout, size_t& skip, const cdef_class& context,
           bool auto_add);

  octave_value
  subsasgn (const std::string& type, const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  void mark_for_construction (const cdef_class&);

  bool is_constructed_for (const cdef_class& cls) const;

  bool is_partially_constructed_for (const cdef_class& cls) const;

  void mark_as_constructed (void) { ctor_list.clear (); }

  void mark_as_constructed (const cdef_class& cls);

  bool is_constructed (void) const { return ctor_list.empty (); }

protected:
  // Object property values
  octave_scalar_map map;

  // Internal/temporary structure used during object construction
  std::map< cdef_class, std::list<cdef_class> > ctor_list;

protected:
  // Restricted object copying!
  cdef_object_scalar (const cdef_object_scalar& obj)
    : cdef_object_base (obj), map (obj.map), ctor_list (obj.ctor_list) { }

private:
  // No assignment!
  cdef_object_scalar& operator = (const cdef_object_scalar&);
};

class
handle_cdef_object : public cdef_object_scalar
{
public:
  handle_cdef_object (void)
    : cdef_object_scalar () { }

  ~handle_cdef_object (void);

  cdef_object_rep* clone (void) const
  {
    handle_cdef_object *obj = const_cast<handle_cdef_object *> (this);
    obj->refcount++;
    return obj;
  }

  cdef_object_rep* copy (void) const
  { return new handle_cdef_object (*this); }

  bool is_valid (void) const { return true; }

  bool is_handle_object (void) const { return true; }

protected:
  // Restricted copying!
  handle_cdef_object (const handle_cdef_object& obj)
    : cdef_object_scalar (obj) { }

private:
  // No assignment
  handle_cdef_object& operator = (const handle_cdef_object&);
};

class
value_cdef_object : public cdef_object_scalar
{
public:
  value_cdef_object (void)
    : cdef_object_scalar () { }

  ~value_cdef_object (void);

  cdef_object_rep* clone (void) const
  { return new value_cdef_object (*this); }

  cdef_object_rep* copy (void) const { return clone (); }

  bool is_valid (void) const { return true; }

  bool is_value_object (void) const { return true; }

private:
  // Private copying!
  value_cdef_object (const value_cdef_object& obj)
    : cdef_object_scalar (obj) { }

  // No assignment!
  value_cdef_object& operator = (const value_cdef_object&);
};

class
cdef_meta_object_rep : public handle_cdef_object
{
public:
  cdef_meta_object_rep (void)
    : handle_cdef_object () { }

  ~cdef_meta_object_rep (void) { }

  cdef_object_rep* copy (void) const
  { return new cdef_meta_object_rep (*this); }

  bool is_meta_object (void) const { return true; }

  virtual bool is_class (void) const { return false; }

  virtual bool is_property (void) const { return false; }

  virtual bool is_method (void) const { return false; }

  virtual bool is_package (void) const { return false; }

  virtual octave_value_list
  meta_subsref (const std::string& /* type */,
                const std::list<octave_value_list>& /* idx */,
                int /* nargout */)
  {
    ::error ("subsref: invalid meta object");
    return octave_value_list ();
  }

  virtual void meta_release (void) { }

  virtual bool meta_is_postfix_index_handled (char /* type */) const
  { return false; }

protected:
  // Restricted copying!
  cdef_meta_object_rep (const cdef_meta_object_rep& obj)
    : handle_cdef_object (obj) { }

private:
  // No assignment!
  cdef_meta_object_rep& operator = (const cdef_meta_object_rep&);
};

class
cdef_meta_object : public cdef_object
{
public:
  cdef_meta_object (void)
    : cdef_object () { }

  cdef_meta_object (const cdef_meta_object& obj)
    : cdef_object (obj) { }

  cdef_meta_object (cdef_meta_object_rep *r)
    : cdef_object (r) { }

  // Object consistency is checked in sub-classes.
  cdef_meta_object (const cdef_object& obj)
    : cdef_object (obj) { }

  ~cdef_meta_object (void) { }

  bool is_class (void) const { return get_rep ()->is_class (); }

  bool is_property (void) const { return get_rep ()->is_property (); }

  bool is_method (void) const { return get_rep ()->is_method (); }

  bool is_package (void) const { return get_rep ()->is_package (); }

  octave_value_list
  meta_subsref (const std::string& type,
                const std::list<octave_value_list>& idx, int nargout)
  { return get_rep ()->meta_subsref (type, idx, nargout); }

  void meta_release (void) { get_rep ()->meta_release (); }

  bool meta_is_postfix_index_handled (char type) const
  { return get_rep ()->meta_is_postfix_index_handled (type); }

private:
  cdef_meta_object_rep* get_rep (void)
  { return dynamic_cast<cdef_meta_object_rep *> (cdef_object::get_rep ()); }

  const cdef_meta_object_rep* get_rep (void) const
  { return dynamic_cast<const cdef_meta_object_rep *> (cdef_object::get_rep ()); }
};

class
cdef_class : public cdef_meta_object
{
private:

  class
  cdef_class_rep : public cdef_meta_object_rep
  {
  public:
    cdef_class_rep (void)
      : cdef_meta_object_rep (), member_count (0), handle_class (false),
        object_count (0), meta (false) { }

    cdef_class_rep (const std::list<cdef_class>& superclasses);

    cdef_object_rep* copy (void) const { return new cdef_class_rep (*this); }

    bool is_class (void) const { return true; }

    std::string get_name (void) const
    { return get ("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_abstract (void) const { return get ("Abstract").bool_value (); }

    bool is_sealed (void) const { return get ("Sealed").bool_value (); }

    cdef_method find_method (const std::string& nm, bool local = false);

    void install_method (const cdef_method& meth);

    Cell get_methods (void);

    cdef_property find_property (const std::string& nm);

    void install_property (const cdef_property& prop);

    Cell get_properties (int mode);

    std::map<std::string, cdef_property> get_property_map (int mode);

    string_vector get_names (void);

    void set_directory (const std::string& dir) { directory = dir; }

    std::string get_directory (void) const { return directory; }

    void delete_object (cdef_object obj);

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    void meta_release (void);

    bool meta_is_postfix_index_handled (char type) const
    { return (type == '(' || type == '.'); }

    octave_value construct (const octave_value_list& args);

    cdef_object construct_object (const octave_value_list& args);

    void initialize_object (cdef_object& obj);

    void run_constructor (cdef_object& obj, const octave_value_list& args);

    void mark_as_handle_class (void) { handle_class = true; }

    bool is_handle_class (void) const { return handle_class; }

    void register_object (void) { object_count++; }

    void unregister_object (void) { object_count--; }

    octave_idx_type static_count (void) const { return member_count; }

    void destroy (void)
    {
      if (member_count)
        {
          refcount++;
          cdef_class lock (this);

          member_count = 0;
          method_map.clear ();
          property_map.clear ();
        }
      else
        delete this;
    }

    void mark_as_meta_class (void) { meta = true; }

    bool is_meta_class (void) const { return meta; }

  private:
    void load_all_methods (void);

    void find_names (std::set<std::string>& names, bool all);

    void find_properties (std::map<std::string,cdef_property>& props,
                          int mode = 0);

    void find_methods (std::map<std::string, cdef_method>& meths,
                       bool only_inherited);

    cdef_class wrap (void)
    {
      refcount++;
      return cdef_class (this);
    }

  private:
    // The @-directory were this class is loaded from.
    // (not used yet)
    std::string directory;

    // The methods defined by this class.
    std::map<std::string,cdef_method> method_map;

    // The properties defined by this class.
    std::map<std::string,cdef_property> property_map;

    // The number of members in this class (methods, properties...)
    octave_idx_type member_count;

    // TRUE if this class is a handle class. A class is a handle
    // class when the abstract "handle" class is one of its superclasses.
    bool handle_class;

    // The list of super-class constructors that are called implicitly by the
    // the classdef engine when creating an object. These constructors are not
    // called explicitly by the class constructor.
    std::list<cdef_class> implicit_ctor_list;

    // The number of objects of this class.
    octave_refcount<octave_idx_type> object_count;

    // TRUE if this class is a built-in meta class.
    bool meta;

    // Utility iterator typedef's.
    typedef std::map<std::string,cdef_method>::iterator method_iterator;
    typedef std::map<std::string,cdef_method>::const_iterator method_const_iterator;
    typedef std::map<std::string,cdef_property>::iterator property_iterator;
    typedef std::map<std::string,cdef_property>::const_iterator property_const_iterator;

  private:
    cdef_class_rep (const cdef_class_rep& c)
      : cdef_meta_object_rep (c), directory (c.directory),
        method_map (c.method_map), property_map (c.property_map),
        member_count (c.member_count), handle_class (c.handle_class),
        implicit_ctor_list (c.implicit_ctor_list),
        object_count (c.object_count), meta (c.meta) { }
  };

public:
  // Create and invalid class object
  cdef_class (void)
    : cdef_meta_object () { }

  cdef_class (const std::string& nm,
              const std::list<cdef_class>& superclasses)
    : cdef_meta_object (new cdef_class_rep (superclasses))
  { get_rep ()->set_name (nm); }

  cdef_class (const cdef_class& cls)
    : cdef_meta_object (cls) { }

  cdef_class (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_class ())
      error ("internal error: invalid assignment from %s to meta.class object",
             class_name ().c_str ());
  }

  cdef_class& operator = (const cdef_class& cls)
  {
    cdef_object::operator= (cls);

    return *this;
  }

  cdef_method find_method (const std::string& nm, bool local = false);

  void install_method (const cdef_method& meth)
  { get_rep ()->install_method (meth); }

  Cell get_methods (void) { return get_rep ()->get_methods (); }

  cdef_property find_property (const std::string& nm);

  void install_property (const cdef_property& prop)
  { get_rep ()->install_property (prop); }

  Cell get_properties (int mode = property_normal)
  { return get_rep ()->get_properties (mode); }

  std::map<std::string, cdef_property>
  get_property_map (int mode = property_normal)
  { return get_rep ()->get_property_map (mode); }

  string_vector get_names (void) { return get_rep ()->get_names (); }

  bool is_abstract (void) const { return get_rep ()->is_abstract (); }

  bool is_sealed (void) const { return get_rep ()->is_sealed (); }

  void set_directory (const std::string& dir)
  { get_rep ()->set_directory (dir); }

  std::string get_directory (void) const
  { return get_rep ()->get_directory (); }

  std::string get_name (void) const
  { return get_rep ()->get_name (); }

  bool is_builtin (void) const
  { return get_directory ().empty (); }

  void delete_object (cdef_object obj)
  { get_rep ()->delete_object (obj); }

  static cdef_class make_meta_class (tree_classdef* t,
                                     bool is_at_folder = false);

  octave_function* get_method_function (const std::string& nm);

  octave_function* get_constructor_function (void)
  { return get_method_function (get_name ()); }

  octave_value construct (const octave_value_list& args)
  { return get_rep ()->construct (args); }

  cdef_object construct_object (const octave_value_list& args)
  { return get_rep ()->construct_object (args); }

  void initialize_object (cdef_object& obj)
  { get_rep ()->initialize_object (obj); }

  void run_constructor (cdef_object& obj, const octave_value_list& args)
  { get_rep ()->run_constructor (obj, args); }

  void mark_as_handle_class (void)
  { get_rep ()->mark_as_handle_class (); }

  bool is_handle_class (void) const
  { return get_rep ()->is_handle_class (); }

  void mark_as_meta_class (void) { get_rep ()->mark_as_meta_class (); }

  bool is_meta_class (void) const { return get_rep ()->is_meta_class (); }

  static const cdef_class& meta_class (void) { return _meta_class; }
  static const cdef_class& meta_property (void) { return _meta_property; }
  static const cdef_class& meta_method (void) { return _meta_method; }
  static const cdef_class& meta_package (void) { return _meta_package; }

  void register_object (void) { get_rep ()->register_object (); }

  void unregister_object (void) { get_rep ()->unregister_object (); }

public:
  enum
  {
    property_normal,
    property_inherited,
    property_all
  };

private:
  cdef_class_rep* get_rep (void)
  { return dynamic_cast<cdef_class_rep *> (cdef_object::get_rep ()); }

  const cdef_class_rep* get_rep (void) const
  { return dynamic_cast<const cdef_class_rep *> (cdef_object::get_rep ()); }

  friend bool operator == (const cdef_class&, const cdef_class&);
  friend bool operator != (const cdef_class&, const cdef_class&);
  friend bool operator < (const cdef_class&, const cdef_class&);

private:
  static cdef_class _meta_class;
  static cdef_class _meta_property;
  static cdef_class _meta_method;
  static cdef_class _meta_package;

  friend void install_classdef (void);
};

inline bool
operator == (const cdef_class& clsa, const cdef_class& clsb)
// FIXME: is this really the right way to check class equality?
{ return (clsa.get_rep () == clsb.get_rep ()); }

inline bool
operator != (const cdef_class& clsa, const cdef_class& clsb)
{ return ! (clsa == clsb); }

// This is only to be able to use cdef_class as map keys.
inline bool
operator < (const cdef_class& clsa, const cdef_class& clsb)
{ return clsa.get_rep () < clsb.get_rep (); }

class
cdef_property : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_property_rep : public cdef_meta_object_rep
  {
  public:
    cdef_property_rep (void)
      : cdef_meta_object_rep () { }

    cdef_object_rep* copy (void) const { return new cdef_property_rep (*this); }

    bool is_property (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_constant (void) const { return get("Constant").bool_value (); }

    octave_value get_value (bool do_check_access = true,
                            const std::string& who = std::string ());

    octave_value get_value (const cdef_object& obj,
                            bool do_check_access = true,
                            const std::string& who = std::string ());

    void set_value (cdef_object& obj, const octave_value& val,
                    bool do_check_access = true,
                    const std::string& who = std::string ());

    bool check_get_access (void) const;

    bool check_set_access (void) const;

  private:
    cdef_property_rep (const cdef_property_rep& p)
      : cdef_meta_object_rep (p) { }

    bool is_recursive_set (const cdef_object& obj) const;

    cdef_property wrap (void)
    {
      refcount++;
      return cdef_property (this);
    }
  };

public:
  cdef_property (void) : cdef_meta_object () { }

  cdef_property (const std::string& nm)
    : cdef_meta_object (new cdef_property_rep ())
  { get_rep ()->set_name (nm); }

  cdef_property (const cdef_property& prop)
    : cdef_meta_object (prop) { }

  cdef_property (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_property ())
      error ("internal error: invalid assignment from %s to meta.property object",
             class_name ().c_str ());
  }

  cdef_property& operator = (const cdef_property& prop)
  {
    cdef_object::operator= (prop);

    return *this;
  }

  octave_value get_value (const cdef_object& obj, bool do_check_access = true,
                          const std::string& who = std::string ())
  { return get_rep ()->get_value (obj, do_check_access, who); }

  octave_value get_value (bool do_check_access = true,
                          const std::string& who = std::string ())
  { return get_rep ()->get_value (do_check_access, who); }

  void set_value (cdef_object& obj, const octave_value& val,
                  bool do_check_access = true,
                  const std::string& who = std::string ())
  { get_rep ()->set_value (obj, val, do_check_access, who); }

  bool check_get_access (void) const
  { return get_rep ()->check_get_access (); }

  bool check_set_access (void) const
  { return get_rep ()->check_set_access (); }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  bool is_constant (void) const { return get_rep ()->is_constant (); }

private:
  cdef_property_rep* get_rep (void)
  { return dynamic_cast<cdef_property_rep *> (cdef_object::get_rep ()); }

  const cdef_property_rep* get_rep (void) const
  { return dynamic_cast<const cdef_property_rep *> (cdef_object::get_rep ()); }
};

class
cdef_method : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_method_rep : public cdef_meta_object_rep
  {
  public:
    cdef_method_rep (void)
      : cdef_meta_object_rep (), function (), dispatch_type ()
    { }

    cdef_object_rep* copy (void) const { return new cdef_method_rep(*this); }

    bool is_method (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    bool is_static (void) const { return get("Static").bool_value (); }

    octave_value get_function (void) const { return function; }

    void set_function (const octave_value& fcn) { function = fcn; }

    bool check_access (void) const;

    bool is_external (void) const { return ! dispatch_type.empty (); }

    void mark_as_external (const std::string& dtype)
    { dispatch_type = dtype; }

    octave_value_list execute (const octave_value_list& args, int nargout,
                               bool do_check_access = true,
                               const std::string& who = std::string ());

    octave_value_list execute (const cdef_object& obj,
                               const octave_value_list& args, int nargout,
                               bool do_check_access = true,
                               const std::string& who = std::string ());

    bool is_constructor (void) const;

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    bool meta_is_postfix_index_handled (char type) const
    { return (type == '(' || type == '.'); }

  private:
    cdef_method_rep (const cdef_method_rep& m)
      : cdef_meta_object_rep (m), function (m.function),
        dispatch_type (m.dispatch_type)
    { }

    void check_method (void);

    cdef_method wrap (void)
    {
      refcount++;
      return cdef_method (this);
    }

  private:
    octave_value function;

    // When non-empty, the method is externally defined and this member
    // is used to cache the dispatch type to look for the method.
    std::string dispatch_type;
  };

public:
  cdef_method (void) : cdef_meta_object () { }

  cdef_method (const std::string& nm)
    : cdef_meta_object (new cdef_method_rep ())
  { get_rep ()->set_name (nm); }

  cdef_method (const cdef_method& meth)
    : cdef_meta_object (meth) { }

  cdef_method (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_method ())
      error ("internal error: invalid assignment from %s to meta.method object",
             class_name ().c_str ());
  }

  cdef_method& operator = (const cdef_method& meth)
  {
    cdef_object::operator= (meth);

    return *this;
  }

  /* normal invokation */
  octave_value_list execute (const octave_value_list& args, int nargout,
                             bool do_check_access = true,
                             const std::string& who = std::string ())
  { return get_rep ()->execute (args, nargout, do_check_access, who); }

  /* dot-invokation: object is pushed as 1st argument */
  octave_value_list execute (const cdef_object& obj,
                             const octave_value_list& args, int nargout,
                             bool do_check_access = true,
                             const std::string& who = std::string ())
  { return get_rep ()->execute (obj, args, nargout, do_check_access, who); }

  bool check_access (void) const { return get_rep ()->check_access (); }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  bool is_static (void) const { return get_rep ()->is_static (); }

  void set_function (const octave_value& fcn)
  { get_rep ()->set_function (fcn); }

  octave_value get_function (void) const
  { return get_rep ()->get_function (); }

  bool is_constructor (void) const
  { return get_rep ()->is_constructor (); }

  bool is_external (void) const { return get_rep ()->is_external (); }

  void mark_as_external (const std::string& dtype)
  { get_rep ()->mark_as_external (dtype); }

private:
  cdef_method_rep* get_rep (void)
  { return dynamic_cast<cdef_method_rep *> (cdef_object::get_rep ()); }

  const cdef_method_rep* get_rep (void) const
  { return dynamic_cast<const cdef_method_rep *> (cdef_object::get_rep ()); }
};

inline cdef_class
cdef_object_rep::get_class (void) const
{
  gripe_invalid_object ("get_class");
  return cdef_class ();
}

inline std::string
cdef_object_rep::class_name (void) const
{ return get_class ().get_name (); }

inline cdef_class
cdef_object::get_class (void) const
{ return rep->get_class (); }

inline cdef_class
cdef_object_base::get_class (void) const
{ return cdef_class (klass); }

inline void
cdef_object_base::set_class (const cdef_class& cls)
{
  if ((klass.ok () && cls.ok () && cls != get_class ())
      || (klass.ok () && ! cls.ok ())
      || (! klass.ok () && cls.ok ()))
    {
      unregister_object ();
      klass = cls;
      register_object ();
    }
}

inline void
cdef_object_base::register_object (void)
{
  if (klass.ok ())
    {
      cdef_class cls (get_class ());

      if (! error_state && cls.ok ())
        cls.register_object ();
    }
}

inline void
cdef_object_base::unregister_object (void)
{
  if (klass.ok ())
    {
      cdef_class cls (get_class ());

      if (! error_state && cls.ok ())
        cls.unregister_object ();
    }
}

inline cdef_object_rep*
cdef_object_base::make_array (void) const
{
  cdef_object_rep* r = new cdef_object_array ();

  r->set_class (get_class ());

  return r;
}

inline cdef_method
cdef_class::find_method (const std::string& nm, bool local)
{ return get_rep ()->find_method (nm, local); }

inline cdef_property
cdef_class::find_property (const std::string& nm)
{ return get_rep ()->find_property (nm); }

class
cdef_package : public cdef_meta_object
{
  friend class cdef_class;

private:

  class
  cdef_package_rep : public cdef_meta_object_rep
  {
  public:
    cdef_package_rep (void)
      : cdef_meta_object_rep (), member_count (0) { }

    ~cdef_package_rep (void) { }

    cdef_object_rep* copy (void) const { return new cdef_package_rep (*this); }

    bool is_package (void) const { return true; }

    std::string get_name (void) const { return get("Name").string_value (); }

    void set_name (const std::string& nm) { put ("Name", nm); }

    void install_class (const cdef_class& cls, const std::string& nm);

    void install_function (const octave_value& fcn, const std::string& nm);

    void install_package (const cdef_package& pack, const std::string& nm);

    Cell get_classes (void) const;

    Cell get_functions (void) const;

    Cell get_packages (void) const;

    octave_idx_type static_count (void) const { return member_count; }

    void destroy (void)
    {
      if (member_count)
        {
          refcount++;
          cdef_package lock (this);

          member_count = 0;
          class_map.clear ();
          package_map.clear ();
        }
      else
        delete this;
    }

    octave_value_list
    meta_subsref (const std::string& type,
                  const std::list<octave_value_list>& idx, int nargout);

    void meta_release (void);

    bool meta_is_postfix_index_handled (char type) const
    { return (type == '.'); }

    octave_value find (const std::string& nm);

  private:
    std::string full_name;
    std::map<std::string, cdef_class> class_map;
    std::map<std::string, octave_value> function_map;
    std::map<std::string, cdef_package> package_map;

    // The number of registered members in this package (classes, packages).
    // This only accounts for the members that back-reference to this package.
    octave_idx_type member_count;

    typedef std::map<std::string, cdef_class>::iterator class_iterator;
    typedef std::map<std::string, cdef_class>::const_iterator class_const_iterator;
    typedef std::map<std::string, octave_value>::iterator function_iterator;
    typedef std::map<std::string, octave_value>::const_iterator function_const_iterator;
    typedef std::map<std::string, cdef_package>::iterator package_iterator;
    typedef std::map<std::string, cdef_package>::const_iterator package_const_iterator;

  private:
    cdef_package_rep (const cdef_package_rep& p)
      : cdef_meta_object_rep (p), full_name (p.full_name),
        class_map (p.class_map), function_map (p.function_map),
        package_map (p.package_map), member_count (p.member_count)
    { }

    cdef_package wrap (void)
    {
      refcount++;
      return cdef_package (this);
    }
  };

public:
  cdef_package (void) : cdef_meta_object () { }

  cdef_package (const std::string& nm)
    : cdef_meta_object (new cdef_package_rep ())
  { get_rep ()->set_name (nm); }

  cdef_package (const cdef_package& pack)
    : cdef_meta_object (pack) { }

  cdef_package (const cdef_object& obj)
    : cdef_meta_object (obj)
  {
    // This should never happen...
    if (! is_package ())
      error ("internal error: invalid assignment from %s to meta.package object",
             class_name ().c_str ());
  }

  cdef_package& operator = (const cdef_package& pack)
  {
    cdef_object::operator= (pack);

    return *this;
  }

  void install_class (const cdef_class& cls, const std::string& nm)
  { get_rep ()->install_class (cls, nm); }

  void install_function (const octave_value& fcn, const std::string& nm)
  { get_rep ()->install_function (fcn, nm); }

  void install_package (const cdef_package& pack, const std::string& nm)
  { get_rep ()->install_package (pack, nm); }

  Cell get_classes (void) const
  { return get_rep ()->get_classes (); }

  Cell get_functions (void) const
  { return get_rep ()->get_functions (); }

  Cell get_packages (void) const
  { return get_rep ()->get_packages (); }

  std::string get_name (void) const { return get_rep ()->get_name (); }

  octave_value find (const std::string& nm) { return get_rep ()->find (nm); }

  static const cdef_package& meta (void) { return _meta; }

private:
  cdef_package_rep* get_rep (void)
  { return dynamic_cast<cdef_package_rep *> (cdef_object::get_rep ()); }

  const cdef_package_rep* get_rep (void) const
  { return dynamic_cast<const cdef_package_rep *> (cdef_object::get_rep ()); }

private:
  static cdef_package _meta;

  friend void install_classdef (void);
};

class
octave_classdef : public octave_base_value
{
public:
  octave_classdef (void)
    : octave_base_value (), object () { }

  octave_classdef (const cdef_object& obj)
    : octave_base_value (), object (obj) { }

  octave_classdef (const octave_classdef& obj)
    : octave_base_value (obj), object (obj.object) { }

  octave_base_value* clone (void) const
  { return new octave_classdef (object.clone ()); }

  octave_base_value* empty_clone (void) const
  { return new octave_classdef (object.empty_clone ()); }

  cdef_object get_object (void) const { return object; }

  cdef_object& get_object_ref (void) { return object; }

  bool is_defined (void) const { return true; }

  bool is_map (void) const { return false; }

  bool is_object (void) const { return true; }

  bool is_classdef_object (void) const { return true; }

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  void print_with_name (std::ostream& os, const std::string& name,
                        bool print_padding = true);

  bool is_instance_of (const std::string& cls_name) const;

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list retval = subsref (type, idx, 1);
    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        bool auto_add);

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  string_vector map_keys (void) const { return object.map_keys (); }

  octave_map map_value (void) const { return object.map_value (); }

  dim_vector dims (void) const { return object.dims (); }

private:
  cdef_object object;

private:

public:
  int type_id (void) const { return t_id; }
  std::string type_name (void) const { return t_name; }
  std::string class_name (void) const { return object.class_name (); }

  static int static_type_id (void) { return t_id; }
  static std::string static_type_name (void) { return t_name; }
  static std::string static_class_name (void) { return "<unknown>"; }
  static void register_type (void);

private:
  static int t_id;

  static const std::string t_name;
};

inline octave_value
to_ov (const cdef_object& obj)
{
  if (obj.ok ())
    return octave_value (new octave_classdef (obj));
  else
    return octave_value (Matrix ());
}

inline octave_value
to_ov (const octave_value& ov)
{ return ov; }

inline cdef_object
to_cdef (const octave_value& val)
{
  if (val.type_name () == "object")
    return dynamic_cast<octave_classdef *> (val.internal_rep ())->get_object ();
  else
    {
      error ("cannot convert `%s' into `object'", val.type_name().c_str ());
      return cdef_object ();
    }
}

inline cdef_object&
to_cdef_ref (const octave_value& val)
{
  static cdef_object empty;

  if (val.type_name () == "object")
    return dynamic_cast<octave_classdef *> (val.internal_rep ())->get_object_ref ();
  else
    {
      error ("cannot convert `%s' into `object'", val.type_name().c_str ());
      return empty;
    }
}

inline cdef_object
to_cdef (const cdef_object& obj)
{ return obj; }

OCTINTERP_API void install_classdef (void);

class
cdef_manager
{
public:

  static cdef_class find_class (const std::string& name,
                                bool error_if_not_found = true,
                                bool load_if_not_found = true)
  {
    if (instance_ok ())
      return instance->do_find_class (name, error_if_not_found,
                                      load_if_not_found);

    return cdef_class ();
  }

  static octave_function* find_method_symbol (const std::string& method_name,
      const std::string& class_name)
  {
    if (instance_ok ())
      return instance->do_find_method_symbol (method_name, class_name);

    return 0;
  }

  static cdef_package find_package (const std::string& name,
                                    bool error_if_not_found = true,
                                    bool load_if_not_found = true)
  {
    if (instance_ok ())
      return instance->do_find_package (name, error_if_not_found,
                                        load_if_not_found);

    return cdef_package ();
  }

  static octave_function* find_package_symbol (const std::string& pack_name)
  {
    if (instance_ok ())
      return instance->do_find_package_symbol (pack_name);

    return 0;
  }

  static void register_class (const cdef_class& cls)
  {
    if (instance_ok ())
      instance->do_register_class (cls);
  }

  static void unregister_class (const cdef_class& cls)
  {
    if (instance_ok ())
      instance->do_unregister_class (cls);
  }

  static void register_package (const cdef_package& pkg)
  {
    if (instance_ok ())
      instance->do_register_package (pkg);
  }

  static void unregister_package (const cdef_package& pkg)
  {
    if (instance_ok ())
      instance->do_unregister_package (pkg);
  }

private:

  cdef_manager (void) { }

  cdef_manager (const cdef_manager&);

  cdef_manager& operator = (const cdef_manager&);

  ~cdef_manager (void) { }

  static void create_instance (void);

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      create_instance ();

    if (! instance)
      {
        ::error ("unable to create cdef_manager!");

        retval = false;
      }

    return retval;
  }

  static void cleanup_instance (void)
  {
    delete instance;

    instance = 0;
  }

  cdef_class do_find_class (const std::string& name, bool error_if_not_found,
                            bool load_if_not_found);

  octave_function* do_find_method_symbol (const std::string& method_name,
                                          const std::string& class_name);

  cdef_package do_find_package (const std::string& name,
                                bool error_if_not_found,
                                bool load_if_not_found);

  octave_function* do_find_package_symbol (const std::string& pack_name);

  void do_register_class (const cdef_class& cls)
  { all_classes[cls.get_name ()] = cls; }

  void do_unregister_class (const cdef_class& cls)
  { all_classes.erase(cls.get_name ()); }

  void do_register_package (const cdef_package& pkg)
  { all_packages[pkg.get_name ()] = pkg; }

  void do_unregister_package (const cdef_package& pkg)
  { all_packages.erase(pkg.get_name ()); }

private:

  // The single cdef_manager instance
  static cdef_manager *instance;

  // All registered/loaded classes
  std::map<std::string, cdef_class> all_classes;

  // All registered/loaded packages
  std::map<std::string, cdef_package> all_packages;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
