/*

Copyright (C) 1993-2015 John W. Eaton
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

#if !defined (octave_symtab_h)
#define octave_symtab_h 1

#include <deque>
#include <list>
#include <map>
#include <set>
#include <string>

#include "glob-match.h"
#include "lo-regexp.h"

class tree_argument_list;
class octave_user_function;

#include "oct-obj.h"
#include "workspace-element.h"
#include "oct-refcount.h"
#include "ov.h"

class
OCTINTERP_API
symbol_table
{
public:

  static octave_value dummy_octave_value;

  typedef int scope_id;
  typedef size_t context_id;

  class
  scope_id_cache
  {
  protected:

    typedef std::set<scope_id>::iterator set_iterator;
    typedef std::set<scope_id>::const_iterator set_const_iterator;

    // We start with 2 because we allocate 0 for the global symbols
    // and 1 for the top-level workspace.

    scope_id_cache (void) : next_available (2), in_use (), free_list () { }

  public:

    ~scope_id_cache (void) { }

    static scope_id alloc (void)
    {
      return instance_ok () ? instance->do_alloc () : -1;
    }

    static void free (scope_id scope)
    {
      if (instance_ok ())
        return instance->do_free (scope);
    }

    static std::list<scope_id> scopes (void)
    {
      return instance_ok () ? instance->do_scopes () : std::list<scope_id> ();
    }

    static void create_instance (void);

    static bool instance_ok (void)
    {
      bool retval = true;

      if (! instance)
        create_instance ();

      if (! instance)
        {
          ::error ("unable to create scope_id_cache object!");

          retval = false;
        }

      return retval;
    }

  private:

    // No copying!

    scope_id_cache (const scope_id_cache&);

    scope_id_cache& operator = (const scope_id_cache&);

    static scope_id_cache *instance;

    static void cleanup_instance (void) { delete instance; instance = 0; }

    // The next available scope not in the free list.
    scope_id next_available;

    // The set of scope IDs that are currently allocated.
    std::set<scope_id> in_use;

    // The set of scope IDs that are currently available.
    std::set<scope_id> free_list;

    scope_id do_alloc (void)
    {
      scope_id retval;

      set_iterator p = free_list.begin ();

      if (p != free_list.end ())
        {
          retval = *p;
          free_list.erase (p);
        }
      else
        retval = next_available++;

      in_use.insert (retval);

      return retval;
    }

    void do_free (scope_id scope)
    {
      set_iterator p = in_use.find (scope);

      if (p != in_use.end ())
        {
          in_use.erase (p);
          free_list.insert (scope);
        }
      else
        error ("free_scope: scope %d not found!", scope);
    }

    std::list<scope_id> do_scopes (void) const
    {
      std::list<scope_id> retval;

      for (set_const_iterator p = in_use.begin (); p != in_use.end (); p++)
        retval.push_back (*p);

      retval.sort ();

      return retval;
    }
  };

  class fcn_info;

  class
  symbol_record
  {
  public:

    // generic variable
    static const unsigned int local = 1;

    // varargin, argn, .nargin., .nargout.
    // (FIXME -- is this really used now?)
    static const unsigned int automatic = 2;

    // formal parameter
    static const unsigned int formal = 4;

    // not listed or cleared (.nargin., .nargout.)
    static const unsigned int hidden = 8;

    // inherited from parent scope; not cleared at function exit
    static const unsigned int inherited = 16;

    // global (redirects to global scope)
    static const unsigned int global = 32;

    // not cleared at function exit
    static const unsigned int persistent = 64;

    // this symbol may NOT become a variable.
    // (symbol added to a static workspace)
    static const unsigned int added_static = 128;

  private:

    class
    symbol_record_rep
    {
    public:

      symbol_record_rep (scope_id s, const std::string& nm,
                         const octave_value& v, unsigned int sc)
        : decl_scope (s), curr_fcn (0), name (nm), value_stack (),
          storage_class (sc), finfo (), valid (true), count (1)
      {
        value_stack.push_back (v);
      }

      void assign (const octave_value& value,
                   context_id context = xdefault_context)
      {
        varref (context) = value;
      }

      void assign (octave_value::assign_op op,
                   const std::string& type,
                   const std::list<octave_value_list>& idx,
                   const octave_value& value,
                   context_id context = xdefault_context)
      {
        varref(context).assign (op, type, idx, value);
      }

      void assign (octave_value::assign_op op, const octave_value& value,
                   context_id context = xdefault_context)
      {
        varref(context).assign (op, value);
      }

      void do_non_const_unary_op (octave_value::unary_op op,
                                  context_id context = xdefault_context)
      {
        varref(context).do_non_const_unary_op (op);
      }

      void do_non_const_unary_op (octave_value::unary_op op,
                                  const std::string& type,
                                  const std::list<octave_value_list>& idx,
                                  context_id context = xdefault_context)
      {
        varref(context).do_non_const_unary_op (op, type, idx);
      }

      octave_value& varref (context_id context = xdefault_context)
      {
        // We duplicate global_varref and persistent_varref here to
        // avoid calling deprecated functions.

        if (is_global ())
          {
            symbol_table::global_table_iterator p
              = symbol_table::global_table.find (name);

            return (p == symbol_table::global_table.end ())
                   ? symbol_table::global_table[name] : p->second;
          }
        else if (is_persistent ())
          {
            symbol_table *inst
              = symbol_table::get_instance (symbol_table::current_scope ());

            return inst ? inst->do_persistent_varref (name) : dummy_octave_value;
          }
        else
          {
            if (context == xdefault_context)
              context = active_context ();

            context_id n = value_stack.size ();
            while (n++ <= context)
              value_stack.push_back (octave_value ());

            return value_stack[context];
          }
      }

      octave_value varval (context_id context = xdefault_context) const
      {
        if (is_global ())
          return symbol_table::global_varval (name);
        else if (is_persistent ())
          return symbol_table::persistent_varval (name);
        else
          {
            if (context == xdefault_context)
              context = active_context ();

            if (context < value_stack.size ())
              return value_stack[context];
            else
              return octave_value ();
          }
      }

      void push_context (scope_id s)
      {
        if (! (is_persistent () || is_global ())
            && s == scope ())
          value_stack.push_back (octave_value ());
      }

      // If pop_context returns 0, we are out of values and this element
      // of the symbol table should be deleted.  This can happen for
      // functions like
      //
      //   function foo (n)
      //     if (n > 0)
      //       foo (n-1);
      //     else
      //       eval ("x = 1");
      //     endif
      //   endfunction
      //
      // Here, X should only exist in the final stack frame.

      size_t pop_context (scope_id s)
      {
        size_t retval = 1;

        if (! (is_persistent () || is_global ())
            && s == scope ())
          {
            value_stack.pop_back ();
            retval = value_stack.size ();
          }

        return retval;
      }

      void clear (void) { clear (scope ()); }

      void clear (scope_id s)
      {
        if (! (is_hidden () || is_inherited ())
            && s == scope ())
          {
            if (is_global ())
              unmark_global ();

            if (is_persistent ())
              {
                symbol_table::persistent_assign (name, varval ());

                unmark_persistent ();
              }

            assign (octave_value ());
          }
      }

      bool is_defined (context_id context = xdefault_context) const
      {
        if (context == xdefault_context)
          context = active_context ();

        return varval (context).is_defined ();
      }

      bool is_valid (void) const
      {
        return valid;
      }

      bool is_variable (context_id context) const
      {
        if (context == xdefault_context)
          context = active_context ();

        return (! is_local () || is_defined (context));
      }

      bool is_local (void) const { return storage_class & local; }
      bool is_automatic (void) const { return storage_class & automatic; }
      bool is_formal (void) const { return storage_class & formal; }
      bool is_hidden (void) const { return storage_class & hidden; }
      bool is_inherited (void) const { return storage_class & inherited; }
      bool is_global (void) const { return storage_class & global; }
      bool is_persistent (void) const { return storage_class & persistent; }
      bool is_added_static (void) const {return storage_class & added_static; }

      void mark_local (void) { storage_class |= local; }
      void mark_automatic (void) { storage_class |= automatic; }
      void mark_formal (void) { storage_class |= formal; }
      void mark_hidden (void) { storage_class |= hidden; }
      void mark_inherited (void) { storage_class |= inherited; }
      void mark_global (void)
      {
        if (is_persistent ())
          error ("can't make persistent variable %s global", name.c_str ());
        else
          storage_class |= global;
      }
      void mark_persistent (void)
      {
        if (is_global ())
          error ("can't make global variable %s persistent", name.c_str ());
        else
          storage_class |= persistent;
      }
      void mark_added_static (void) { storage_class |= added_static; }

      void unmark_local (void) { storage_class &= ~local; }
      void unmark_automatic (void) { storage_class &= ~automatic; }
      void unmark_formal (void) { storage_class &= ~formal; }
      void unmark_hidden (void) { storage_class &= ~hidden; }
      void unmark_inherited (void) { storage_class &= ~inherited; }
      void unmark_global (void) { storage_class &= ~global; }
      void unmark_persistent (void) { storage_class &= ~persistent; }
      void unmark_added_static (void) { storage_class &= ~added_static; }

      void init_persistent (void)
      {
        if (! is_defined ())
          {
            mark_persistent ();

            assign (symbol_table::persistent_varval (name));
          }
        // FIXME: this causes trouble with recursive calls.
        // else
        //   error ("unable to declare existing variable persistent");
      }

      void invalidate (void)
      {
        valid = false;
      }

      void erase_persistent (void)
      {
        unmark_persistent ();
        symbol_table::erase_persistent (name);
      }

      OCTINTERP_API context_id active_context (void) const;

      scope_id scope (void) const { return decl_scope; }

      void set_curr_fcn (octave_user_function *fcn)
      {
        curr_fcn = fcn;
      }

      symbol_record_rep *dup (scope_id new_scope) const
      {
        return new symbol_record_rep (new_scope, name, varval (),
                                      storage_class);
      }

      void dump (std::ostream& os, const std::string& prefix) const;

      scope_id decl_scope;

      octave_user_function* curr_fcn;

      std::string name;

      std::deque<octave_value> value_stack;

      unsigned int storage_class;

      fcn_info *finfo;

      bool valid;

      octave_refcount<size_t> count;

    private:

      // No copying!

      symbol_record_rep (const symbol_record_rep& ov);

      symbol_record_rep& operator = (const symbol_record_rep&);
    };

  public:

    symbol_record (scope_id s = xcurrent_scope,
                   const std::string& nm = std::string (),
                   const octave_value& v = octave_value (),
                   unsigned int sc = local)
      : rep (new symbol_record_rep (s, nm, v, sc)) { }

    symbol_record (const symbol_record& sr)
      : rep (sr.rep)
    {
      rep->count++;
    }

    symbol_record& operator = (const symbol_record& sr)
    {
      if (this != &sr)
        {
          if (--rep->count == 0)
            delete rep;

          rep = sr.rep;
          rep->count++;
        }

      return *this;
    }

    ~symbol_record (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    symbol_record dup (scope_id new_scope) const
    {
      return symbol_record (rep->dup (new_scope));
    }

    const std::string& name (void) const { return rep->name; }

    void rename (const std::string& new_name) { rep->name = new_name; }

    octave_value
    find (const octave_value_list& args = octave_value_list ()) const;

    void assign (const octave_value& value,
                 context_id context = xdefault_context)
    {
      rep->assign (value, context);
    }

    void assign (octave_value::assign_op op,
                 const std::string& type,
                 const std::list<octave_value_list>& idx,
                 const octave_value& value,
                 context_id context = xdefault_context)
    {
      rep->assign (op, type, idx, value, context);
    }

    void assign (octave_value::assign_op op, const octave_value& value,
                 context_id context = xdefault_context)
    {
      rep->assign (op, value, context);
    }

    void do_non_const_unary_op (octave_value::unary_op op)
    {
      rep->do_non_const_unary_op (op);
    }

    void do_non_const_unary_op (octave_value::unary_op op,
                                const std::string& type,
                                const std::list<octave_value_list>& idx)
    {
      rep->do_non_const_unary_op (op, type, idx);
    }

    // Delete when deprecated varref functions are removed.
    octave_value& varref (context_id context = xdefault_context)
    {
      return rep->varref (context);
    }

    octave_value varval (context_id context = xdefault_context) const
    {
      return rep->varval (context);
    }

    void push_context (scope_id s) { rep->push_context (s); }

    size_t pop_context (scope_id s) { return rep->pop_context (s); }

    void clear (void) { rep->clear (); }

    void clear (scope_id s) { rep->clear (s); }

    bool is_defined (context_id context = xdefault_context) const
    {
      return rep->is_defined (context);
    }

    bool is_undefined (context_id context = xdefault_context) const
    {
      return ! rep->is_defined (context);
    }

    bool is_valid (void) const
    {
      return rep->is_valid ();
    }

    bool is_variable (context_id context = xdefault_context) const
    {
      return rep->is_variable (context);
    }

    bool is_local (void) const { return rep->is_local (); }
    bool is_automatic (void) const { return rep->is_automatic (); }
    bool is_formal (void) const { return rep->is_formal (); }
    bool is_global (void) const { return rep->is_global (); }
    bool is_hidden (void) const { return rep->is_hidden (); }
    bool is_inherited (void) const { return rep->is_inherited (); }
    bool is_persistent (void) const { return rep->is_persistent (); }
    bool is_added_static (void) const { return rep->is_added_static (); }

    void mark_local (void) { rep->mark_local (); }
    void mark_automatic (void) { rep->mark_automatic (); }
    void mark_formal (void) { rep->mark_formal (); }
    void mark_hidden (void) { rep->mark_hidden (); }
    void mark_inherited (void) { rep->mark_inherited (); }
    void mark_global (void) { rep->mark_global (); }
    void mark_persistent (void) { rep->mark_persistent (); }
    void mark_added_static (void) { rep->mark_added_static (); }

    void unmark_local (void) { rep->unmark_local (); }
    void unmark_automatic (void) { rep->unmark_automatic (); }
    void unmark_formal (void) { rep->unmark_formal (); }
    void unmark_hidden (void) { rep->unmark_hidden (); }
    void unmark_inherited (void) { rep->unmark_inherited (); }
    void unmark_global (void) { rep->unmark_global (); }
    void unmark_persistent (void) { rep->unmark_persistent (); }
    void unmark_added_static (void) { rep->unmark_added_static (); }

    void init_persistent (void) { rep->init_persistent (); }

    void erase_persistent (void) { rep->erase_persistent (); }

    void invalidate (void) { rep->invalidate (); }

    context_id active_context (void) const { return rep->active_context (); }

    scope_id scope (void) const { return rep->scope (); }

    unsigned int xstorage_class (void) const { return rep->storage_class; }

    void set_curr_fcn (octave_user_function *fcn) { rep->set_curr_fcn (fcn); }

    void
    dump (std::ostream& os, const std::string& prefix = std::string ()) const
    {
      rep->dump (os, prefix);
    }

  private:

    symbol_record_rep *rep;

    symbol_record (symbol_record_rep *new_rep) : rep (new_rep) { }
  };

  static symbol_record dummy_symbol_record;

  // Always access a symbol from the current scope.
  // Useful for scripts, as they may be executed in more than one scope.
  class
  symbol_reference
  {
  public:

    symbol_reference (void) : scope (-1) { }

    symbol_reference (const symbol_record& record,
                      scope_id curr_scope = symbol_table::current_scope ())
      : scope (curr_scope), sym (record)
    { }

    symbol_reference (const symbol_reference& ref)
      : scope (ref.scope), sym (ref.sym)
    { }

    symbol_reference& operator = (const symbol_reference& ref)
    {
      if (this != &ref)
        {
          scope = ref.scope;
          sym = ref.sym;
        }
      return *this;
    }

    bool is_black_hole (void) const { return scope < 0; }

    // The name is the same regardless of scope.
    const std::string& name (void) const { return sym.name (); }

    symbol_record *operator-> (void)
    {
      update ();
      return &sym;
    }

    symbol_record *operator-> (void) const
    {
      update ();
      return &sym;
    }

    // can be used to place symbol_reference in maps, we don't overload < as
    // it doesn't make any sense for symbol_reference
    struct comparator
    {
      bool operator ()(const symbol_reference& lhs,
                       const symbol_reference& rhs) const
      {
        return lhs.name () < rhs.name ();
      }
    };
  private:

    void update (void) const
    {
      scope_id curr_scope = symbol_table::current_scope ();

      if (scope != curr_scope || ! sym.is_valid ())
        {
          scope = curr_scope;
          sym = symbol_table::insert (sym.name ());
        }
    }

    mutable scope_id scope;
    mutable symbol_record sym;
  };

  class
  fcn_info
  {
  public:

    typedef std::map<std::string, std::string> dispatch_map_type;

    typedef std::map<scope_id, octave_value>::const_iterator
      scope_val_const_iterator;
    typedef std::map<scope_id, octave_value>::iterator scope_val_iterator;

    typedef std::map<std::string, octave_value>::const_iterator
      str_val_const_iterator;
    typedef std::map<std::string, octave_value>::iterator str_val_iterator;

    typedef dispatch_map_type::const_iterator dispatch_map_const_iterator;
    typedef dispatch_map_type::iterator dispatch_map_iterator;

  private:

    class
    fcn_info_rep
    {
    public:

      fcn_info_rep (const std::string& nm)
        : name (nm), package_name (), subfunctions (), private_functions (),
          class_constructors (), class_methods (), dispatch_map (),
          cmdline_function (), autoload_function (), function_on_path (),
          built_in_function (), count (1)
      {
        size_t pos = name.rfind ('.');

        if (pos != std::string::npos)
          {
            package_name = name.substr (0, pos);
            name = name.substr (pos+1);
          }
      }

      octave_value load_private_function (const std::string& dir_name);

      octave_value load_class_constructor (void);

      octave_value load_class_method (const std::string& dispatch_type);

      octave_value find (const octave_value_list& args, bool local_funcs);

      octave_value builtin_find (void);

      octave_value find_method (const std::string& dispatch_type);

      octave_value find_autoload (void);

      octave_value find_package (void);

      octave_value find_user_function (void);

      bool is_user_function_defined (void) const
      {
        return function_on_path.is_defined ();
      }

      octave_value find_function (const octave_value_list& args,
                                  bool local_funcs)
      {
        return find (args, local_funcs);
      }

      void lock_subfunction (scope_id scope)
      {
        scope_val_iterator p = subfunctions.find (scope);

        if (p != subfunctions.end ())
          p->second.lock ();
      }

      void unlock_subfunction (scope_id scope)
      {
        scope_val_iterator p = subfunctions.find (scope);

        if (p != subfunctions.end ())
          p->second.unlock ();
      }

      std::pair<std::string, octave_value>
      subfunction_defined_in_scope (scope_id scope) const
      {
        scope_val_const_iterator p = subfunctions.find (scope);

        return p == subfunctions.end ()
               ? std::pair<std::string, octave_value> ()
               : std::pair<std::string, octave_value> (name, p->second);
      }

      void erase_subfunction (scope_id scope)
      {
        scope_val_iterator p = subfunctions.find (scope);

        if (p != subfunctions.end ())
          subfunctions.erase (p);
      }

      void mark_subfunction_in_scope_as_private (scope_id scope,
                                                 const std::string& class_name);

      void install_cmdline_function (const octave_value& f)
      {
        cmdline_function = f;
      }

      void install_subfunction (const octave_value& f, scope_id scope)
      {
        subfunctions[scope] = f;
      }

      void install_user_function (const octave_value& f)
      {
        function_on_path = f;
      }

      void install_built_in_function (const octave_value& f)
      {
        built_in_function = f;
      }

      template <class T>
      void
      clear_map (std::map<T, octave_value>& map, bool force = false)
      {
        typename std::map<T, octave_value>::iterator p = map.begin ();

        while (p != map.end ())
          {
            if (force || ! p->second.islocked ())
              map.erase (p++);
            else
              p++;
          }
      }

      void clear_autoload_function (bool force = false)
      {
        if (force || ! autoload_function.islocked ())
          autoload_function = octave_value ();
      }

      // We also clear command line functions here, as these are both
      // "user defined"
      void clear_user_function (bool force = false)
      {
        if (force || ! function_on_path.islocked ())
          function_on_path = octave_value ();

        if (force || ! cmdline_function.islocked ())
          cmdline_function = octave_value ();
      }

      void clear_mex_function (void)
      {
        if (function_on_path.is_mex_function ())
          clear_user_function ();
      }

      void clear_package (void)
      {
        package = octave_value ();
      }

      void clear (bool force = false)
      {
        clear_map (subfunctions, force);
        clear_map (private_functions, force);
        clear_map (class_constructors, force);
        clear_map (class_methods, force);

        clear_autoload_function (force);
        clear_user_function (force);
        clear_package ();
      }

      void add_dispatch (const std::string& type, const std::string& fname)
      {
        dispatch_map[type] = fname;
      }

      void clear_dispatch (const std::string& type)
      {
        dispatch_map_iterator p = dispatch_map.find (type);

        if (p != dispatch_map.end ())
          dispatch_map.erase (p);
      }

      void print_dispatch (std::ostream& os) const;

      std::string help_for_dispatch (void) const;

      dispatch_map_type get_dispatch (void) const { return dispatch_map; }

      void dump (std::ostream& os, const std::string& prefix) const;

      std::string full_name (void) const
      {
        if (package_name.empty ())
          return name;
        else
          return package_name + "." + name;
      }

      std::string name;

      std::string package_name;

      // Scope id to function object.
      std::map<scope_id, octave_value> subfunctions;

      // Directory name to function object.
      std::map<std::string, octave_value> private_functions;

      // Class name to function object.
      std::map<std::string, octave_value> class_constructors;

      // Dispatch type to function object.
      std::map<std::string, octave_value> class_methods;

      // Legacy dispatch map (dispatch type name to function name).
      dispatch_map_type dispatch_map;

      octave_value cmdline_function;

      octave_value autoload_function;

      octave_value function_on_path;

      octave_value package;

      octave_value built_in_function;

      octave_refcount<size_t> count;

    private:

      octave_value xfind (const octave_value_list& args, bool local_funcs);

      octave_value x_builtin_find (void);

      // No copying!

      fcn_info_rep (const fcn_info_rep&);

      fcn_info_rep& operator = (const fcn_info_rep&);
    };

  public:

    fcn_info (const std::string& nm = std::string ())
      : rep (new fcn_info_rep (nm)) { }

    fcn_info (const fcn_info& fi) : rep (fi.rep)
    {
      rep->count++;
    }

    fcn_info& operator = (const fcn_info& fi)
    {
      if (this != &fi)
        {
          if (--rep->count == 0)
            delete rep;

          rep = fi.rep;
          rep->count++;
        }

      return *this;
    }

    ~fcn_info (void)
    {
      if (--rep->count == 0)
        delete rep;
    }

    octave_value find (const octave_value_list& args = octave_value_list (),
                       bool local_funcs = true)
    {
      return rep->find (args, local_funcs);
    }

    octave_value builtin_find (void)
    {
      return rep->builtin_find ();
    }

    octave_value find_method (const std::string& dispatch_type) const
    {
      return rep->find_method (dispatch_type);
    }

    octave_value find_built_in_function (void) const
    {
      return rep->built_in_function;
    }

    octave_value find_cmdline_function (void) const
    {
      return rep->cmdline_function;
    }

    octave_value find_autoload (void)
    {
      return rep->find_autoload ();
    }

    octave_value find_user_function (void)
    {
      return rep->find_user_function ();
    }

    bool is_user_function_defined (void) const
    {
      return rep->is_user_function_defined ();
    }

    octave_value find_function (const octave_value_list& args
                                = octave_value_list (),
                                bool local_funcs = true)
    {
      return rep->find_function (args, local_funcs);
    }

    void lock_subfunction (scope_id scope)
    {
      rep->lock_subfunction (scope);
    }

    void unlock_subfunction (scope_id scope)
    {
      rep->unlock_subfunction (scope);
    }

    std::pair<std::string, octave_value>
    subfunction_defined_in_scope (scope_id scope = xcurrent_scope) const
    {
      return rep->subfunction_defined_in_scope (scope);
    }

    void erase_subfunction (scope_id scope)
    {
      rep->erase_subfunction (scope);
    }

    void mark_subfunction_in_scope_as_private (scope_id scope,
                                               const std::string& class_name)
    {
      rep->mark_subfunction_in_scope_as_private (scope, class_name);
    }

    void install_cmdline_function (const octave_value& f)
    {
      rep->install_cmdline_function (f);
    }

    void install_subfunction (const octave_value& f, scope_id scope)
    {
      rep->install_subfunction (f, scope);
    }

    void install_user_function (const octave_value& f)
    {
      rep->install_user_function (f);
    }

    void install_built_in_function (const octave_value& f)
    {
      rep->install_built_in_function (f);
    }

    void clear (bool force = false) { rep->clear (force); }

    void clear_user_function (bool force = false)
    {
      rep->clear_user_function (force);
    }

    void clear_autoload_function (bool force = false)
    {
      rep->clear_autoload_function (force);
    }

    void clear_mex_function (void) { rep->clear_mex_function (); }

    void add_dispatch (const std::string& type, const std::string& fname)
    {
      rep->add_dispatch (type, fname);
    }

    void clear_dispatch (const std::string& type)
    {
      rep->clear_dispatch (type);
    }

    void print_dispatch (std::ostream& os) const
    {
      rep->print_dispatch (os);
    }

    std::string help_for_dispatch (void) const
    { return rep->help_for_dispatch (); }

    dispatch_map_type get_dispatch (void) const
    {
      return rep->get_dispatch ();
    }

    void
    dump (std::ostream& os, const std::string& prefix = std::string ()) const
    {
      rep->dump (os, prefix);
    }

  private:

    fcn_info_rep *rep;
  };

  static scope_id global_scope (void) { return xglobal_scope; }
  static scope_id top_scope (void) { return xtop_scope; }

  static scope_id current_scope (void) { return xcurrent_scope; }

  static context_id current_context (void) { return xcurrent_context; }

  static scope_id alloc_scope (void) { return scope_id_cache::alloc (); }

  static void set_scope (scope_id scope)
  {
    if (scope == xglobal_scope)
      error ("can't set scope to global");
    else if (scope != xcurrent_scope)
      {
        all_instances_iterator p = all_instances.find (scope);

        if (p == all_instances.end ())
          {
            symbol_table *inst = new symbol_table (scope);

            if (inst)
              all_instances[scope] = instance = inst;
          }
        else
          instance = p->second;

        xcurrent_scope = scope;
        xcurrent_context = 0;
      }
  }

  static void set_scope_and_context (scope_id scope, context_id context)
  {
    if (scope == xglobal_scope)
      error ("can't set scope to global");
    else
      {
        if (scope != xcurrent_scope)
          {
            all_instances_iterator p = all_instances.find (scope);

            if (p == all_instances.end ())
              error ("scope not found!");
            else
              {
                instance = p->second;

                xcurrent_scope = scope;

                xcurrent_context = context;
              }
          }
        else
          xcurrent_context = context;
      }
  }

  static void erase_scope (scope_id scope)
  {
    assert (scope != xglobal_scope);

    erase_subfunctions_in_scope (scope);

    all_instances_iterator p = all_instances.find (scope);

    if (p != all_instances.end ())
      {
        delete p->second;

        all_instances.erase (p);

        free_scope (scope);
      }
  }

  static void erase_subfunctions_in_scope (scope_id scope)
  {
    for (fcn_table_iterator q = fcn_table.begin (); q != fcn_table.end (); q++)
      q->second.erase_subfunction (scope);
  }

  static void
  mark_subfunctions_in_scope_as_private (scope_id scope,
                                         const std::string& class_name)
  {
    for (fcn_table_iterator q = fcn_table.begin (); q != fcn_table.end (); q++)
      q->second.mark_subfunction_in_scope_as_private (scope, class_name);
  }

  static scope_id dup_scope (scope_id scope)
  {
    scope_id retval = -1;

    symbol_table *inst = get_instance (scope);

    if (inst)
      {
        scope_id new_scope = alloc_scope ();

        symbol_table *new_symbol_table = new symbol_table (scope);

        if (new_symbol_table)
          {
            all_instances[new_scope] = new_symbol_table;

            inst->do_dup_scope (*new_symbol_table);

            retval = new_scope;
          }
      }

    return retval;
  }

  static std::list<scope_id> scopes (void)
  {
    return scope_id_cache::scopes ();
  }

  static symbol_record
  find_symbol (const std::string& name, scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_find_symbol (name) :
      symbol_record (scope);
  }

  static void
  inherit (scope_id scope, scope_id donor_scope, context_id donor_context)
  {
    symbol_table *inst = get_instance (scope);

    if (inst)
      {
        symbol_table *donor_symbol_table = get_instance (donor_scope);

        if (donor_symbol_table)
          inst->do_inherit (*donor_symbol_table, donor_context);
      }
  }

  static bool at_top_level (void) { return xcurrent_scope == xtop_scope; }

  // Find a value corresponding to the given name in the table.
  static octave_value
  find (const std::string& name,
        const octave_value_list& args = octave_value_list (),
        bool skip_variables = false,
        bool local_funcs = true);

  static octave_value builtin_find (const std::string& name);

  // Insert a new name in the table.
  static symbol_record& insert (const std::string& name,
                                scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_insert (name) : symbol_table::dummy_symbol_record;
  }

  static void rename (const std::string& old_name,
                      const std::string& new_name,
                      scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);

    if (inst)
      inst->do_rename (old_name, new_name);
  }

  static void assign (const std::string& name,
                      const octave_value& value = octave_value (),
                      scope_id scope = xcurrent_scope,
                      context_id context = xdefault_context,
                      bool force_add = false)
  {
    symbol_table *inst = get_instance (scope);

    if (inst)
      inst->do_assign (name, value, context, force_add);
  }

  // Use assign (name, value, scope, context, force_add) instead.
  static octave_value&
  varref (const std::string& name, scope_id scope = xcurrent_scope,
          context_id context = xdefault_context, bool force_add = false)
          GCC_ATTR_DEPRECATED
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_varref (name, context, force_add) : dummy_octave_value;
  }

  // Convenience function to simplify
  // octave_user_function::bind_automatic_vars

  static void force_assign (const std::string& name,
                            const octave_value& value = octave_value (),
                            scope_id scope = xcurrent_scope,
                            context_id context = xdefault_context)
  {
    assign (name, value, scope, context, true);
  }

  // Use force_assign (name, value, scope, context) instead.
  static octave_value&
  force_varref (const std::string& name, scope_id scope = xcurrent_scope,
                context_id context = xdefault_context) GCC_ATTR_DEPRECATED
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_varref (name, context, true) : dummy_octave_value;
  }

  static octave_value varval (const std::string& name,
                              scope_id scope = xcurrent_scope,
                              context_id context = xdefault_context)
  {
    symbol_table *inst = get_instance (scope);

    return inst ? inst->do_varval (name, context) : octave_value ();
  }

  static void
  global_assign (const std::string& name,
                 const octave_value& value = octave_value ())

  {
    global_table_iterator p = global_table.find (name);

    if (p == global_table.end ())
      global_table[name] = value;
    else
      p->second = value;
  }

  // Use global_assign (name, value) instead.
  static octave_value&
  global_varref (const std::string& name) GCC_ATTR_DEPRECATED

  {
    global_table_iterator p = global_table.find (name);

    return (p == global_table.end ()) ? global_table[name] : p->second;
  }

  static octave_value
  global_varval (const std::string& name)
  {
    global_table_const_iterator p = global_table.find (name);

    return (p != global_table.end ()) ? p->second : octave_value ();
  }

  static void
  top_level_assign (const std::string& name,
                    const octave_value& value = octave_value ())
  {
    assign (name, value, top_scope (), 0);
  }

  // Use top_level_assign (name, value) instead.
  static octave_value&
  top_level_varref (const std::string& name) GCC_ATTR_DEPRECATED
  {
    symbol_table *inst = get_instance (top_scope ());

    return inst ? inst->do_varref (name, 0, true) : dummy_octave_value;
  }

  static octave_value
  top_level_varval (const std::string& name)
  {
    return varval (name, top_scope (), 0);
  }

  static void
  persistent_assign (const std::string& name,
                     const octave_value& value = octave_value ())
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_persistent_assign (name, value);
  }

  // Use persistent_assign (name, value) instead.
  static octave_value& persistent_varref (const std::string& name)
  GCC_ATTR_DEPRECATED
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_persistent_varref (name) : dummy_octave_value;
  }

  static octave_value persistent_varval (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_persistent_varval (name) : octave_value ();
  }

  static void erase_persistent (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_erase_persistent (name);
  }

  static bool is_variable (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_is_variable (name) : false;
  }

  static bool
  is_built_in_function_name (const std::string& name)
  {
    octave_value val = find_built_in_function (name);

    return val.is_defined ();
  }

  static octave_value
  find_method (const std::string& name, const std::string& dispatch_type)
  {
    fcn_table_const_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      return p->second.find_method (dispatch_type);
    else
      {
        fcn_info finfo (name);

        octave_value fcn = finfo.find_method (dispatch_type);

        if (fcn.is_defined ())
          fcn_table[name] = finfo;

        return fcn;
      }
  }

  static octave_value
  find_built_in_function (const std::string& name)
  {
    fcn_table_const_iterator p = fcn_table.find (name);

    return (p != fcn_table.end ())
           ? p->second.find_built_in_function () : octave_value ();
  }

  static octave_value
  find_autoload (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    return (p != fcn_table.end ())
           ? p->second.find_autoload () : octave_value ();
  }

  static octave_value
  find_function (const std::string& name,
                 const octave_value_list& args = octave_value_list (),
                 bool local_funcs = true);

  static octave_value find_user_function (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    return (p != fcn_table.end ())
           ? p->second.find_user_function () : octave_value ();
  }

  static void install_cmdline_function (const std::string& name,
                                        const octave_value& fcn)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.install_cmdline_function (fcn);
      }
    else
      {
        fcn_info finfo (name);

        finfo.install_cmdline_function (fcn);

        fcn_table[name] = finfo;
      }
  }

  // Install subfunction FCN named NAME.  SCOPE is the scope of the
  // primary function corresponding to this subfunction.

  static void install_subfunction (const std::string& name,
                                   const octave_value& fcn,
                                   scope_id scope)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.install_subfunction (fcn, scope);
      }
    else
      {
        fcn_info finfo (name);

        finfo.install_subfunction (fcn, scope);

        fcn_table[name] = finfo;
      }
  }

  static void install_nestfunction (const std::string& name,
                                    const octave_value& fcn,
                                    scope_id parent_scope);

  static void update_nest (scope_id scope)
  {
    symbol_table *inst = get_instance (scope);
    if (inst)
      inst->do_update_nest ();
  }

  static void install_user_function (const std::string& name,
                                     const octave_value& fcn)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.install_user_function (fcn);
      }
    else
      {
        fcn_info finfo (name);

        finfo.install_user_function (fcn);

        fcn_table[name] = finfo;
      }
  }

  static void install_built_in_function (const std::string& name,
                                         const octave_value& fcn)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.install_built_in_function (fcn);
      }
    else
      {
        fcn_info finfo (name);

        finfo.install_built_in_function (fcn);

        fcn_table[name] = finfo;
      }
  }

  static void clear (const std::string& name)
  {
    clear_variable (name);
  }

  static void clear_all (bool force = false)
  {
    clear_variables ();

    clear_global_pattern ("*");

    clear_functions (force);
  }

  static void clear_variables (scope_id scope)
  {
    symbol_table *inst = get_instance (scope);

    if (inst)
      inst->do_clear_variables ();
  }

  // This is split for unwind_protect.
  static void clear_variables (void)
  {
    clear_variables (xcurrent_scope);
  }

  static void clear_objects (scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);

    if (inst)
      inst->do_clear_objects ();
  }

  static void clear_functions (bool force = false)
  {
    for (fcn_table_iterator p = fcn_table.begin (); p != fcn_table.end (); p++)
      p->second.clear (force);
  }

  static void clear_function (const std::string& name)
  {
    clear_user_function (name);
  }

  static void clear_global (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_global (name);
  }

  static void clear_variable (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_variable (name);
  }

  static void clear_symbol (const std::string& name)
  {
    // FIXME: are we supposed to do both here?

    clear_variable (name);
    clear_function (name);
  }

  static void clear_function_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (fcn_table_iterator p = fcn_table.begin (); p != fcn_table.end (); p++)
      {
        if (pattern.match (p->first))
          p->second.clear_user_function ();
      }
  }

  static void clear_global_pattern (const std::string& pat)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_global_pattern (pat);
  }

  static void clear_variable_pattern (const std::string& pat)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_variable_pattern (pat);
  }

  static void clear_variable_regexp (const std::string& pat)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_clear_variable_regexp (pat);
  }

  static void clear_symbol_pattern (const std::string& pat)
  {
    // FIXME: are we supposed to do both here?

    clear_variable_pattern (pat);
    clear_function_pattern (pat);
  }

  static void clear_user_function (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.clear_user_function ();
      }
    // FIXME: is this necessary, or even useful?
    // else
    //   error ("clear: no such function '%s'", name.c_str ());
  }

  // This clears oct and mex files, incl. autoloads.
  static void clear_dld_function (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.clear_autoload_function ();
        finfo.clear_user_function ();
      }
  }

  static void clear_mex_functions (void)
  {
    for (fcn_table_iterator p = fcn_table.begin (); p != fcn_table.end (); p++)
      {
        fcn_info& finfo = p->second;

        finfo.clear_mex_function ();
      }
  }

  static bool set_class_relationship (const std::string& sup_class,
                                      const std::string& inf_class);

  static bool is_superiorto (const std::string& a, const std::string& b);

  static void alias_built_in_function (const std::string& alias,
                                       const std::string& name)
  {
    octave_value fcn = find_built_in_function (name);

    if (fcn.is_defined ())
      {
        fcn_info finfo (alias);

        finfo.install_built_in_function (fcn);

        fcn_table[alias] = finfo;
      }
    else
      panic ("alias: '%s' is undefined", name.c_str ());
  }

  static void add_dispatch (const std::string& name, const std::string& type,
                            const std::string& fname)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.add_dispatch (type, fname);
      }
    else
      {
        fcn_info finfo (name);

        finfo.add_dispatch (type, fname);

        fcn_table[name] = finfo;
      }
  }

  static void clear_dispatch (const std::string& name, const std::string& type)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.clear_dispatch (type);
      }
  }

  static void print_dispatch (std::ostream& os, const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        finfo.print_dispatch (os);
      }
  }

  static fcn_info::dispatch_map_type get_dispatch (const std::string& name)
  {
    fcn_info::dispatch_map_type retval;

    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        retval = finfo.get_dispatch ();
      }

    return retval;
  }

  static std::string help_for_dispatch (const std::string& name)
  {
    std::string retval;

    fcn_table_iterator p = fcn_table.find (name);

    if (p != fcn_table.end ())
      {
        fcn_info& finfo = p->second;

        retval = finfo.help_for_dispatch ();
      }

    return retval;
  }

  static void push_context (void)
  {
    if (xcurrent_scope == xglobal_scope || xcurrent_scope == xtop_scope)
      error ("invalid call to xymtab::push_context");
    else
      {
        symbol_table *inst = get_instance (xcurrent_scope);

        if (inst)
          inst->do_push_context ();
      }
  }

  static void pop_context (void)
  {
    if (xcurrent_scope == xglobal_scope || xcurrent_scope == xtop_scope)
      error ("invalid call to xymtab::pop_context");
    else
      {
        symbol_table *inst = get_instance (xcurrent_scope);

        if (inst)
          inst->do_pop_context ();
      }
  }

  // For unwind_protect.
  static void pop_context (void *) { pop_context (); }

  static void mark_automatic (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_mark_automatic (name);
  }

  static void mark_hidden (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_mark_hidden (name);
  }

  static void mark_global (const std::string& name)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    if (inst)
      inst->do_mark_global (name);
  }

  // exclude: Storage classes to exclude, you can OR them together
  static std::list<symbol_record>
  all_variables (scope_id scope = xcurrent_scope,
                 context_id context = xdefault_context,
                 bool defined_only = true,
                 unsigned int exclude = symbol_record::hidden)
  {
    symbol_table *inst = get_instance (scope);

    return inst
           ? inst->do_all_variables (context, defined_only, exclude)
           : std::list<symbol_record> ();
  }

  static std::list<symbol_record> glob (const std::string& pattern)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_glob (pattern) : std::list<symbol_record> ();
  }

  static std::list<symbol_record> regexp (const std::string& pattern)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_regexp (pattern) : std::list<symbol_record> ();
  }

  static std::list<symbol_record> glob_variables (const std::string& pattern)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_glob (pattern, true) : std::list<symbol_record> ();
  }

  static std::list<symbol_record> regexp_variables (const std::string& pattern)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_regexp (pattern, true) : std::list<symbol_record> ();
  }

  static std::list<symbol_record>
  glob_global_variables (const std::string& pattern)
  {
    std::list<symbol_record> retval;

    glob_match pat (pattern);

    for (global_table_const_iterator p = global_table.begin ();
         p != global_table.end (); p++)
      {
        // We generate a list of symbol_record objects so that
        // the results from glob_variables and glob_global_variables
        // may be handled the same way.

        if (pat.match (p->first))
          retval.push_back (symbol_record (xglobal_scope,
                                           p->first, p->second,
                                           symbol_record::global));
      }

    return retval;
  }

  static std::list<symbol_record>
  regexp_global_variables (const std::string& pattern)
  {
    std::list<symbol_record> retval;

    ::regexp pat (pattern);

    for (global_table_const_iterator p = global_table.begin ();
         p != global_table.end (); p++)
      {
        // We generate a list of symbol_record objects so that
        // the results from regexp_variables and regexp_global_variables
        // may be handled the same way.

        if (pat.is_match (p->first))
          retval.push_back (symbol_record (xglobal_scope,
                                           p->first, p->second,
                                           symbol_record::global));
      }

    return retval;
  }

  static std::list<symbol_record> glob_variables (const string_vector& patterns)
  {
    std::list<symbol_record> retval;

    size_t len = patterns.length ();

    for (size_t i = 0; i < len; i++)
      {
        std::list<symbol_record> tmp = glob_variables (patterns[i]);

        retval.insert (retval.begin (), tmp.begin (), tmp.end ());
      }

    return retval;
  }

  static std::list<symbol_record> regexp_variables
    (const string_vector& patterns)
  {
    std::list<symbol_record> retval;

    size_t len = patterns.length ();

    for (size_t i = 0; i < len; i++)
      {
        std::list<symbol_record> tmp = regexp_variables (patterns[i]);

        retval.insert (retval.begin (), tmp.begin (), tmp.end ());
      }

    return retval;
  }

  static std::list<std::string> user_function_names (void)
  {
    std::list<std::string> retval;

    for (fcn_table_iterator p = fcn_table.begin ();
         p != fcn_table.end (); p++)
      {
        if (p->second.is_user_function_defined ())
          retval.push_back (p->first);
      }

    if (! retval.empty ())
      retval.sort ();

    return retval;
  }

  static std::list<std::string> global_variable_names (void)
  {
    std::list<std::string> retval;

    for (global_table_const_iterator p = global_table.begin ();
         p != global_table.end (); p++)
      retval.push_back (p->first);

    retval.sort ();

    return retval;
  }

  static std::list<std::string> top_level_variable_names (void)
  {
    symbol_table *inst = get_instance (xtop_scope);

    return inst ? inst->do_variable_names () : std::list<std::string> ();
  }

  static std::list<std::string> variable_names (void)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst ? inst->do_variable_names () : std::list<std::string> ();
  }

  static std::list<std::string> built_in_function_names (void)
  {
    std::list<std::string> retval;

    for (fcn_table_const_iterator p = fcn_table.begin ();
         p != fcn_table.end (); p++)
      {
        octave_value fcn = p->second.find_built_in_function ();

        if (fcn.is_defined ())
          retval.push_back (p->first);
      }

    if (! retval.empty ())
      retval.sort ();

    return retval;
  }

  static std::list<std::string> cmdline_function_names (void)
  {
    std::list<std::string> retval;

    for (fcn_table_const_iterator p = fcn_table.begin ();
         p != fcn_table.end (); p++)
      {
        octave_value fcn = p->second.find_cmdline_function ();

        if (fcn.is_defined ())
          retval.push_back (p->first);
      }

    if (! retval.empty ())
      retval.sort ();

    return retval;
  }

  static bool is_local_variable (const std::string& name)
  {
    if (xcurrent_scope == xglobal_scope)
      return false;
    else
      {
        symbol_table *inst = get_instance (xcurrent_scope);

        return inst ? inst->do_is_local_variable (name) : false;
      }
  }

  static bool is_global (const std::string& name)
  {
    if (xcurrent_scope == xglobal_scope)
      return true;
    else
      {
        symbol_table *inst = get_instance (xcurrent_scope);

        return inst ? inst->do_is_global (name) : false;
      }
  }

  static std::list<workspace_element> workspace_info (void)
  {
    symbol_table *inst = get_instance (xcurrent_scope);

    return inst
           ? inst->do_workspace_info () : std::list<workspace_element> ();
  }

  static void dump (std::ostream& os, scope_id scope = xcurrent_scope);

  static void dump_global (std::ostream& os);

  static void dump_functions (std::ostream& os);

  static void cache_name (scope_id scope, const std::string& name)
  {
    symbol_table *inst = get_instance (scope, false);

    if (inst)
      inst->do_cache_name (name);
  }

  static void lock_subfunctions (scope_id scope = xcurrent_scope)
  {
    for (fcn_table_iterator p = fcn_table.begin ();
         p != fcn_table.end (); p++)
      p->second.lock_subfunction (scope);
  }

  static void unlock_subfunctions (scope_id scope = xcurrent_scope)
  {
    for (fcn_table_iterator p = fcn_table.begin ();
         p != fcn_table.end (); p++)
      p->second.unlock_subfunction (scope);
  }

  static std::map<std::string, octave_value>
  subfunctions_defined_in_scope (scope_id scope = xcurrent_scope)
  {
    std::map<std::string, octave_value> retval;

    for (fcn_table_const_iterator p = fcn_table.begin ();
         p != fcn_table.end (); p++)
      {
        std::pair<std::string, octave_value> tmp
          = p->second.subfunction_defined_in_scope (scope);

        std::string nm = tmp.first;

        if (! nm.empty ())
          retval[nm] = tmp.second;
      }

    return retval;
  }

  static void free_scope (scope_id scope)
  {
    if (scope == xglobal_scope || scope == xtop_scope)
      error ("can't free global or top-level scopes!");
    else
      symbol_table::scope_id_cache::free (scope);
  }

  static void stash_dir_name_for_subfunctions (scope_id scope,
                                               const std::string& dir_name);

  static void add_to_parent_map (const std::string& classname,
                                 const std::list<std::string>& parent_list)
  {
    parent_map[classname] = parent_list;
  }

  static std::list<std::string>
  parent_classes (const std::string& dispatch_type)
  {
    std::list<std::string> retval;

    const_parent_map_iterator it = parent_map.find (dispatch_type);

    if (it != parent_map.end ())
      retval = it->second;

    for (std::list<std::string>::const_iterator lit = retval.begin ();
         lit != retval.end (); lit++)
      {
        // Search for parents of parents and append them to the list.

        // FIXME: should we worry about a circular inheritance graph?

        std::list<std::string> parents = parent_classes (*lit);

        if (! parents.empty ())
          retval.insert (retval.end (), parents.begin (), parents.end ());
      }

    return retval;
  }

  static octave_user_function *get_curr_fcn (scope_id scope = xcurrent_scope)
  {
    symbol_table *inst = get_instance (scope);
    return inst->curr_fcn;
  }

  static void set_curr_fcn (octave_user_function *curr_fcn,
                            scope_id scope = xcurrent_scope)
  {
    assert (scope != xtop_scope && scope != xglobal_scope);
    symbol_table *inst = get_instance (scope);
    // FIXME: normally, functions should not usurp each other's scope.
    // If for any incredible reason this is needed, call
    // set_user_function (0, scope) first. This may cause problems with
    // nested functions, as the curr_fcn of symbol_records must be updated.
    assert (inst->curr_fcn == 0 || curr_fcn == 0);
    inst->curr_fcn = curr_fcn;
  }

  static void cleanup (void);

private:

  // No copying!

  symbol_table (const symbol_table&);

  symbol_table& operator = (const symbol_table&);

  typedef std::map<std::string, symbol_record>::const_iterator
    table_const_iterator;
  typedef std::map<std::string, symbol_record>::iterator
    table_iterator;

  typedef std::map<std::string, octave_value>::const_iterator
    global_table_const_iterator;
  typedef std::map<std::string, octave_value>::iterator
    global_table_iterator;

  typedef std::map<std::string, octave_value>::const_iterator
    persistent_table_const_iterator;
  typedef std::map<std::string, octave_value>::iterator
    persistent_table_iterator;

  typedef std::map<scope_id, symbol_table*>::const_iterator
    all_instances_const_iterator;
  typedef std::map<scope_id, symbol_table*>::iterator
    all_instances_iterator;

  typedef std::map<std::string, fcn_info>::const_iterator
    fcn_table_const_iterator;
  typedef std::map<std::string, fcn_info>::iterator
    fcn_table_iterator;

  // The scope of this symbol table.
  scope_id my_scope;

  // Name for this table (usually the file name of the function
  // corresponding to the scope);
  std::string table_name;

  // Map from symbol names to symbol info.
  std::map<std::string, symbol_record> table;

  // Child nested functions.
  std::vector<symbol_table*> nest_children;

  // Parent nested function (may be null).
  symbol_table *nest_parent;

  // The associated user code (may be null).
  octave_user_function *curr_fcn;

  // If true then no variables can be added.
  bool static_workspace;

  // Map from names of global variables to values.
  static std::map<std::string, octave_value> global_table;

  // Map from names of persistent variables to values.
  std::map<std::string, octave_value> persistent_table;

  // Pointer to symbol table for current scope (variables only).
  static symbol_table *instance;

  // Map from scope id to symbol table instances.
  static std::map<scope_id, symbol_table*> all_instances;

  // Map from function names to function info (subfunctions, private
  // functions, class constructors, class methods, etc.)
  static std::map<std::string, fcn_info> fcn_table;

  // Mape from class names to set of classes that have lower
  // precedence.
  static std::map<std::string, std::set<std::string> > class_precedence_table;

  typedef std::map<std::string, std::set<std::string> >::const_iterator
    class_precedence_table_const_iterator;
  typedef std::map<std::string, std::set<std::string> >::iterator
    class_precedence_table_iterator;

  // Map from class names to parent class names.
  static std::map<std::string, std::list<std::string> > parent_map;

  typedef std::map<std::string, std::list<std::string> >::const_iterator
    const_parent_map_iterator;
  typedef std::map<std::string, std::list<std::string> >::iterator
    parent_map_iterator;

  static const scope_id xglobal_scope;
  static const scope_id xtop_scope;

  static scope_id xcurrent_scope;

  static context_id xcurrent_context;

  static const context_id xdefault_context = static_cast<context_id> (-1);

  symbol_table (scope_id scope)
    : my_scope (scope), table_name (), table (), nest_children (),
      nest_parent (0), curr_fcn (0), static_workspace (false),
      persistent_table () { }

  ~symbol_table (void) { }

  static symbol_table *get_instance (scope_id scope, bool create = true)
  {
    symbol_table *retval = 0;

    bool ok = true;

    if (scope != xglobal_scope)
      {
        if (scope == xcurrent_scope)
          {
            if (! instance && create)
              {
                symbol_table *inst = new symbol_table (scope);

                if (inst)
                  {
                    all_instances[scope] = instance = inst;

                    if (scope == xtop_scope)
                      instance->do_cache_name ("top-level");
                  }
              }

            if (! instance)
              ok = false;

            retval = instance;
          }
        else
          {
            all_instances_iterator p = all_instances.find (scope);

            if (p == all_instances.end ())
              {
                if (create)
                  {
                    retval = new symbol_table (scope);

                    if (retval)
                      all_instances[scope] = retval;
                    else
                      ok = false;
                  }
                else
                  ok = false;
              }
            else
              retval = p->second;
          }
      }

    if (! ok)
      error ("unable to %s symbol_table object for scope %d!",
             create ? "create" : "find", scope);

    return retval;
  }

  void add_nest_child (symbol_table& st)
  {
    assert (!st.nest_parent);
    nest_children.push_back (&st);
    st.nest_parent = this;
  }

  void insert_symbol_record (const symbol_record& sr)
  {
    table[sr.name ()] = sr;
  }

  void
  do_dup_scope (symbol_table& new_symbol_table) const
  {
    for (table_const_iterator p = table.begin (); p != table.end (); p++)
      new_symbol_table.insert_symbol_record (p->second.dup (new_symbol_table
                                                            .my_scope));
  }

  symbol_record do_find_symbol (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p == table.end ())
      return do_insert (name);
    else
      return p->second;
  }

  void do_inherit (symbol_table& donor_table, context_id donor_context)
  {
    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
        symbol_record& sr = p->second;

        if (! (sr.is_automatic () || sr.is_formal ()))
          {
            std::string nm = sr.name ();

            if (nm != "__retval__")
              {
                octave_value val = donor_table.do_varval (nm, donor_context);

                if (val.is_defined ())
                  {
                    sr.assign (val, 0);

                    sr.mark_inherited ();
                  }
              }
          }
      }
  }

  static fcn_info *get_fcn_info (const std::string& name)
  {
    fcn_table_iterator p = fcn_table.find (name);
    return p != fcn_table.end () ? &p->second : 0;
  }

  octave_value
  do_find (const std::string& name, const octave_value_list& args,
           bool skip_variables, bool local_funcs);

  octave_value do_builtin_find (const std::string& name);

  symbol_record& do_insert (const std::string& name, bool force_add = false)
  {
    table_iterator p = table.find (name);

    if (p == table.end ())
      {
        symbol_record ret (my_scope, name);

        if (nest_parent && nest_parent->look_nonlocal (name, ret))
          return table[name] = ret;
        else
          {
            if (static_workspace && ! force_add)
              ret.mark_added_static ();

            return table[name] = ret;
          }
      }
    else
      return p->second;
  }

  void do_rename (const std::string& old_name, const std::string& new_name)
  {
    table_iterator p = table.find (old_name);

    if (p != table.end ())
      {
        symbol_record sr = p->second;

        sr.rename (new_name);

        table.erase (p);

        table[new_name] = sr;
      }
  }

  void do_assign (const std::string& name, const octave_value& value,
                  context_id context, bool force_add)
  {
    table_iterator p = table.find (name);

    if (p == table.end ())
      {
        symbol_record& sr = do_insert (name, force_add);

        sr.assign (value, context);
      }
    else
      p->second.assign (value, context);
  }

  // Use do_assign (name, value, context, force_add) instead.
  // Delete when deprecated varref functions are removed.
  octave_value& do_varref (const std::string& name, context_id context,
                           bool force_add)
  {
    table_iterator p = table.find (name);

    if (p == table.end ())
      {
        symbol_record& sr = do_insert (name, force_add);

        return sr.varref (context);
      }
    else
      return p->second.varref (context);
  }

  octave_value do_varval (const std::string& name, context_id context) const
  {
    table_const_iterator p = table.find (name);

    return (p != table.end ()) ? p->second.varval (context) : octave_value ();
  }

  void do_persistent_assign (const std::string& name, const octave_value& value)
  {
    persistent_table_iterator p = persistent_table.find (name);

    if (p == persistent_table.end ())
      persistent_table[name] = value;
    else
      p->second = value;
  }

  // Use do_persistent_assign (name, value) instead.
  // Delete when deprecated varref functions are removed.
  octave_value& do_persistent_varref (const std::string& name)
  {
    persistent_table_iterator p = persistent_table.find (name);

    return (p == persistent_table.end ())
           ? persistent_table[name] : p->second;
  }

  octave_value do_persistent_varval (const std::string& name)
  {
    persistent_table_const_iterator p = persistent_table.find (name);

    return (p != persistent_table.end ()) ? p->second : octave_value ();
  }

  void do_erase_persistent (const std::string& name)
  {
    persistent_table_iterator p = persistent_table.find (name);

    if (p != persistent_table.end ())
      persistent_table.erase (p);
  }

  bool do_is_variable (const std::string& name) const
  {
    bool retval = false;

    table_const_iterator p = table.find (name);

    if (p != table.end ())
      {
        const symbol_record& sr = p->second;

        retval = sr.is_variable ();
      }

    return retval;
  }

  void do_push_context (void)
  {
    for (table_iterator p = table.begin (); p != table.end (); p++)
      p->second.push_context (my_scope);
  }

  void do_pop_context (void)
  {
    table_iterator p = table.begin ();

    while (p != table.end ())
      {
        if (p->second.pop_context (my_scope) == 0)
          table.erase (p++);
        else
          p++;
      }
  }

  void do_clear_variables (void)
  {
    for (table_iterator p = table.begin (); p != table.end (); p++)
      p->second.clear (my_scope);
  }

  void do_clear_objects (void)
  {
    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
        symbol_record& sr = p->second;
        octave_value val = sr.varval ();
        if (val.is_object ())
          p->second.clear (my_scope);
      }
  }

  void do_clear_global (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p != table.end ())
      {
        symbol_record& sr = p->second;

        if (sr.is_global ())
          sr.unmark_global ();
      }

    global_table_iterator q = global_table.find (name);

    if (q != global_table.end ())
      global_table.erase (q);

  }

  void do_clear_variable (const std::string& name)
  {
    table_iterator p = table.find (name);

    if (p != table.end ())
      p->second.clear (my_scope);
  }

  void do_clear_global_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
        symbol_record& sr = p->second;

        if (sr.is_global () && pattern.match (sr.name ()))
          sr.unmark_global ();
      }

    global_table_iterator q = global_table.begin ();

    while (q != global_table.end ())
      {
        if (pattern.match (q->first))
          global_table.erase (q++);
        else
          q++;
      }


  }

  void do_clear_variable_pattern (const std::string& pat)
  {
    glob_match pattern (pat);

    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
        symbol_record& sr = p->second;

        if (sr.is_defined () || sr.is_global ())
          {
            if (pattern.match (sr.name ()))
              sr.clear (my_scope);
          }
      }
  }

  void do_clear_variable_regexp (const std::string& pat)
  {
    ::regexp pattern (pat);

    for (table_iterator p = table.begin (); p != table.end (); p++)
      {
        symbol_record& sr = p->second;

        if (sr.is_defined () || sr.is_global ())
          {
            if (pattern.is_match (sr.name ()))
              sr.clear (my_scope);
          }
      }
  }

  void do_mark_automatic (const std::string& name)
  {
    do_insert (name).mark_automatic ();
  }

  void do_mark_hidden (const std::string& name)
  {
    do_insert (name).mark_hidden ();
  }

  void do_mark_global (const std::string& name)
  {
    do_insert (name).mark_global ();
  }

  std::list<symbol_record>
  do_all_variables (context_id context, bool defined_only,
                    unsigned int exclude) const
  {
    std::list<symbol_record> retval;

    for (table_const_iterator p = table.begin (); p != table.end (); p++)
      {
        const symbol_record& sr = p->second;

        if ((defined_only && ! sr.is_defined (context))
            || (sr.xstorage_class () & exclude))
          continue;

        retval.push_back (sr);
      }

    return retval;
  }

  std::list<symbol_record> do_glob (const std::string& pattern,
                                    bool vars_only = false) const
  {
    std::list<symbol_record> retval;

    glob_match pat (pattern);

    for (table_const_iterator p = table.begin (); p != table.end (); p++)
      {
        if (pat.match (p->first))
          {
            const symbol_record& sr = p->second;

            if (vars_only && ! sr.is_variable ())
              continue;

            retval.push_back (sr);
          }
      }

    return retval;
  }

  std::list<symbol_record> do_regexp (const std::string& pattern,
                                      bool vars_only = false) const
  {
    std::list<symbol_record> retval;

    ::regexp pat (pattern);

    for (table_const_iterator p = table.begin (); p != table.end (); p++)
      {
        if (pat.is_match (p->first))
          {
            const symbol_record& sr = p->second;

            if (vars_only && ! sr.is_variable ())
              continue;

            retval.push_back (sr);
          }
      }

    return retval;
  }

  std::list<std::string> do_variable_names (void)
  {
    std::list<std::string> retval;

    for (table_const_iterator p = table.begin (); p != table.end (); p++)
      {
        if (p->second.is_variable ())
          retval.push_back (p->first);
      }

    retval.sort ();

    return retval;
  }

  bool do_is_local_variable (const std::string& name) const
  {
    table_const_iterator p = table.find (name);

    return (p != table.end ()
            && ! p->second.is_global ()
            && p->second.is_defined ());
  }

  bool do_is_global (const std::string& name) const
  {
    table_const_iterator p = table.find (name);

    return p != table.end () && p->second.is_global ();
  }

  std::list<workspace_element> do_workspace_info (void) const;

  void do_dump (std::ostream& os);

  void do_cache_name (const std::string& name) { table_name = name; }

  void do_update_nest (void);

  bool look_nonlocal (const std::string& name, symbol_record& result)
  {
    table_iterator p = table.find (name);
    if (p == table.end ())
      {
        if (nest_parent)
          return nest_parent->look_nonlocal (name, result);
      }
    else if (! p->second.is_automatic ())
      {
        result = p->second;
        return true;
      }

    return false;
  }
};

extern bool out_of_date_check (octave_value& function,
                               const std::string& dispatch_type = std::string (),
                               bool check_relative = true);

extern OCTINTERP_API std::string
get_dispatch_type (const octave_value_list& args);
extern OCTINTERP_API std::string
get_dispatch_type (const octave_value_list& args, builtin_type_t& builtin_type);

#endif
