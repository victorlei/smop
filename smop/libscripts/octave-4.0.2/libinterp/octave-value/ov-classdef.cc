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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>

#include "defun.h"
#include "load-path.h"
#include "ov-builtin.h"
#include "ov-classdef.h"
#include "ov-fcn-handle.h"
#include "ov-typeinfo.h"
#include "ov-usr-fcn.h"
#include "pt-assign.h"
#include "pt-classdef.h"
#include "pt-funcall.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-walk.h"
#include "singleton-cleanup.h"
#include "symtab.h"
#include "toplev.h"

// Define to 1 to enable debugging statements.
#define DEBUG_TRACE 0

static void
gripe_method_access (const std::string& from, const cdef_method& meth)
{
  octave_value acc = meth.get ("Access");
  std::string acc_s;

  if (acc.is_string ())
    acc_s = acc.string_value ();
  else
    acc_s = "class-restricted";

  error ("%s: method `%s' has %s access and cannot be run in this context",
         from.c_str (), meth.get_name ().c_str (), acc_s.c_str ());
}

static void
gripe_property_access (const std::string& from, const cdef_property& prop,
                       bool is_set = false)
{
  octave_value acc = prop.get (is_set ? "SetAccess" : "GetAccess");
  std::string acc_s;

  if (acc.is_string ())
    acc_s = acc.string_value ();
  else
    acc_s = "class-restricted";

  if (is_set)
    error ("%s: property `%s' has %s access and cannot be set in this context",
           from.c_str (), prop.get_name ().c_str (), acc_s.c_str ());
  else
    error ("%s: property `%s' has %s access and cannot be obtained in this context",
           from.c_str (), prop.get_name ().c_str (), acc_s.c_str ());
}

static std::string
get_base_name (const std::string& nm)
{
  std::string::size_type pos = nm.find_last_of ('.');

  if (pos != std::string::npos)
    return nm.substr (pos + 1);

  return nm;
}

static void
make_function_of_class (const std::string& class_name,
                        const octave_value& fcn)
{
  octave_function *of = fcn.function_value ();

  if (! error_state)
    {
      of->stash_dispatch_class (class_name);

      octave_user_function *uf = of->user_function_value (true);

      if (! error_state && uf)
        {
          if (get_base_name (class_name) == uf->name ())
            {
              uf->mark_as_class_constructor ();
              uf->mark_as_classdef_constructor ();
            }
          else
            uf->mark_as_class_method ();
        }
    }
}

static void
make_function_of_class (const cdef_class& cls, const octave_value& fcn)
{
  make_function_of_class (cls.get_name (), fcn);
}

static octave_value
make_fcn_handle (octave_builtin::fcn ff, const std::string& nm)
{
  octave_value fcn (new octave_builtin (ff, nm));

  octave_value fcn_handle (new octave_fcn_handle (fcn, nm));

  return fcn_handle;
}

static octave_value
make_fcn_handle (const octave_value& fcn, const std::string& nm)
{
  octave_value retval;

  if (fcn.is_defined ())
    retval = octave_value (new octave_fcn_handle (fcn, nm));

  return retval;
}

inline octave_value_list
execute_ov (octave_value val, const octave_value_list& args, int nargout)
{
  std::list<octave_value_list> idx (1, args);

  std::string type ("(");

  return val.subsref (type, idx, nargout);
}

static cdef_class
lookup_class (const std::string& name, bool error_if_not_found = true,
              bool load_if_not_found = true)
{
  return cdef_manager::find_class (name, error_if_not_found,
                                   load_if_not_found);
}

static cdef_class
lookup_class (const cdef_class& cls)
{
  // FIXME: placeholder for the time being, the purpose
  //        is to centralized any class update activity here.

  return cls;
}

static cdef_class
lookup_class (const octave_value& ov)
{
  if (ov.is_string())
    return lookup_class (ov.string_value ());
  else
    {
      cdef_class cls (to_cdef (ov));

      if (! error_state)
        return lookup_class (cls);
    }

  return cdef_class ();
}

static std::list<cdef_class>
lookup_classes (const Cell& cls_list)
{
  std::list<cdef_class> retval;

  for (int i = 0; i < cls_list.numel (); i++)
    {
      cdef_class c = lookup_class (cls_list(i));

      if (! error_state)
        retval.push_back (c);
      else
        {
          retval.clear ();
          break;
        }
    }

  return retval;
}

static octave_value
to_ov (const std::list<cdef_class>& class_list)
{
  Cell cls (class_list.size (), 1);
  int i = 0;

  for (std::list<cdef_class>::const_iterator it = class_list.begin ();
       it != class_list.end (); ++it, ++i)
    cls(i) = to_ov (*it);

  return octave_value (cls);
}

static bool
is_superclass (const cdef_class& clsa, const cdef_class& clsb,
               bool allow_equal = true, int max_depth = -1)
{
  bool retval = false;

  if (allow_equal && clsa == clsb)
    retval = true;
  else if (max_depth != 0)
    {
      Cell c = clsb.get ("SuperClasses").cell_value ();

      for (int i = 0; ! error_state && ! retval && i < c.numel (); i++)
        {
          cdef_class cls = lookup_class (c(i));

          if (! error_state)
            retval = is_superclass (clsa, cls, true,
                                    max_depth < 0 ? max_depth : max_depth-1);
        }
    }

  return retval;
}

inline bool
is_strict_superclass (const cdef_class& clsa, const cdef_class& clsb)
{ return is_superclass (clsa, clsb, false); }

inline bool
is_direct_superclass (const cdef_class& clsa, const cdef_class& clsb)
{ return is_superclass (clsa, clsb, false, 1); }

static octave_value_list
class_get_properties (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get_properties ();
    }

  return retval;
}

static cdef_class
get_class_context (std::string& name, bool& in_constructor)
{
  cdef_class cls;

  octave_function* fcn = octave_call_stack::current ();

  in_constructor = false;

  if (fcn && (fcn->is_class_method ()
              || fcn->is_classdef_constructor ()
              || fcn->is_anonymous_function_of_class ()
              || (fcn->is_private_function ()
                  && ! fcn->dispatch_class ().empty ())))
    {
      cls = lookup_class (fcn->dispatch_class ());
      if (! error_state)
        {
          name = fcn->name ();
          in_constructor = fcn->is_classdef_constructor ();
        }
    }

  return cls;
}

inline cdef_class
get_class_context (void)
{
  std::string dummy_string;
  bool dummy_bool;

  return get_class_context (dummy_string, dummy_bool);
}

static bool
in_class_method (const cdef_class& cls)
{
  cdef_class ctx = get_class_context ();

  return (ctx.ok () && is_superclass (ctx, cls));
}

static bool
check_access (const cdef_class& cls, const octave_value& acc,
              const std::string& meth_name = std::string (),
              const std::string& prop_name = std::string (),
              bool is_prop_set = false)
{
  if (acc.is_string ())
    {
      std::string acc_s = acc.string_value ();

      if (acc_s == "public")
        return true;

      cdef_class ctx = get_class_context ();

      // The access is private or protected, this requires a
      // valid class context.

      if (! error_state && ctx.ok ())
        {
          if (acc_s == "private")
            return (ctx == cls);
          else if (acc_s == "protected")
            {
              if (is_superclass (cls, ctx))
                // Calling a protected method in a superclass.
                return true;
              else if (is_strict_superclass (ctx, cls))
                {
                  // Calling a protected method or property in a derived class.
                  // This is only allowed if the context class knows about it
                  // and has access to it.

                  if (! meth_name.empty ())
                    {
                      cdef_method m = ctx.find_method (meth_name);

                      if (m.ok ())
                        return check_access (ctx, m.get ("Access"), meth_name);

                      return false;
                    }
                  else if (! prop_name.empty ())
                    {
                      cdef_property p = ctx.find_property (prop_name);

                      if (p.ok ())
                        {
                          octave_value p_access = p.get (is_prop_set ?
                                                         "SetAccess" :
                                                         "GetAccess");

                          return check_access (ctx, p_access, meth_name,
                                               prop_name, is_prop_set);
                        }

                      return false;
                    }
                  else
                    panic_impossible ();
                }

              return false;
            }
          else
            panic_impossible ();
        }
    }
  else if (acc.is_cell ())
    {
      Cell acc_c = acc.cell_value ();

      cdef_class ctx = get_class_context ();

      // At this point, a class context is always required.

      if (! error_state && ctx.ok ())
        {
          if (ctx == cls)
            return true;

          for (int i = 0; ! error_state && i < acc.numel (); i++)
            {
              cdef_class acc_cls (to_cdef (acc_c(i)));

              if (! error_state)
                {
                  if (is_superclass (acc_cls, ctx))
                    return true;
                }
            }
        }
    }
  else
    error ("invalid property/method access in class `%s'",
           cls.get_name ().c_str ());

  return false;
}

static bool
is_dummy_method (const octave_value& fcn)
{
  bool retval = false;

  if (fcn.is_defined ())
    {
      if (fcn.is_user_function ())
        {
          octave_user_function *uf = fcn.user_function_value (true);

          if (! uf || ! uf->body ())
            retval = true;
        }
    }
  else
    retval = true;

  return retval;
}

bool
is_method_executing (const octave_value& ov, const cdef_object& obj)
{
  octave_function* stack_fcn = octave_call_stack::current ();

  octave_function* method_fcn = ov.function_value (true);

  // Does the top of the call stack match our target function?

  if (stack_fcn && stack_fcn == method_fcn)
    {
      octave_user_function* uf = method_fcn->user_function_value (true);

      // We can only check the context object for user-function (not builtin),
      // where we have access to the parameters (arguments and return values).
      // That's ok as there's no need to call this function for builtin
      // methods.

      if (uf)
        {
          // At this point, the method is executing, but we still need to
          // check the context object for which the method is executing. For
          // methods, it's the first argument of the function; for ctors, it
          // is the first return value.

          tree_parameter_list* pl = uf->is_classdef_constructor ()
            ? uf->return_list () : uf->parameter_list ();

          if (pl && pl->size () > 0)
            {
              octave_value arg0 = pl->front ()->lvalue ().value ();

              if (arg0.is_defined () && arg0.type_name () == "object")
                {
                  cdef_object arg0_obj = to_cdef (arg0);

                  return obj.is (arg0_obj);
                }
            }
        }
    }

  return false;
}

static octave_value_list
class_get_methods (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      retval(0) = cls.get_methods ();
    }

  return retval;
}

static octave_value_list
class_get_superclasses (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls (to_cdef (args(0)));

      Cell classes = cls.get ("SuperClasses").cell_value ();

      retval(0) = to_ov (lookup_classes (classes));
    }

  return retval;
}

static octave_value_list
class_get_inferiorclasses (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls (to_cdef (args(0)));

      Cell classes = cls.get ("InferiorClasses").cell_value ();

      retval(0) = to_ov (lookup_classes (classes));
    }

  return retval;
}

static octave_value_list
class_fromName (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        retval(0) = to_ov (lookup_class (name));
      else
        error ("fromName: invalid class name, expected a string value");
    }
  else
    error ("fromName: invalid number of parameters");

  return retval;
}

static octave_value_list
class_fevalStatic (const octave_value_list& args, int nargout)
{
  octave_value_list retval;

  if (args.length () > 1 && args(0).type_name () == "object")
    {
      cdef_class cls (to_cdef (args(0)));

      if (! error_state)
        {
          std::string meth_name = args(1).string_value ();

          if (! error_state)
            {
              cdef_method meth = cls.find_method (meth_name);

              if (meth.ok ())
                {
                  if (meth.is_static ())
                    retval = meth.execute (args.splice (0, 2), nargout,
                                           true, "fevalStatic");
                  else
                    error ("fevalStatic: method `%s' is not static",
                           meth_name.c_str ());
                }
              else
                error ("fevalStatic: method not found: %s",
                       meth_name.c_str ());
            }
          else
            error ("fevalStatic: invalid method name, expected a string value");
        }
      error ("fevalStatic: invalid object, expected a meta.class object");
    }
  else
    error ("fevalStatic: invalid arguments");

  return retval;
}

static octave_value_list
class_getConstant (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 2 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.class")
    {
      cdef_class cls = to_cdef (args(0));

      if (! error_state)
        {
          std::string prop_name = args(1).string_value ();

          if (! error_state)
            {
              cdef_property prop = cls.find_property (prop_name);

              if (prop.ok ())
                {
                  if (prop.is_constant ())
                    retval(0) = prop.get_value (true, "getConstant");
                  else
                    error ("getConstant: property `%s' is not constant",
                           prop_name.c_str ());
                }
              else
                error ("getConstant: property not found: %s",
                       prop_name.c_str ());
            }
          else
            error ("getConstant: invalid property name, expected a string value");
        }
      else
        error ("getConstant: invalid object, expected a meta.class object");
    }
  else
    error ("getConstant: invalid arguments");

  return retval;
}

#define META_CLASS_CMP(OP, CLSA, CLSB, FUN) \
static octave_value_list \
class_ ## OP (const octave_value_list& args, int /* nargout */) \
{ \
  octave_value_list retval; \
\
  if (args.length () == 2 \
      && args(0).type_name () == "object" \
      && args(1).type_name () == "object" \
      && args(0).class_name () == "meta.class" \
      && args(1).class_name () == "meta.class") \
    { \
      cdef_class clsa = to_cdef (args(0)); \
\
      cdef_class clsb = to_cdef (args(1)); \
\
      if (! error_state) \
        retval(0) = FUN (CLSA, CLSB); \
      else \
        error (#OP ": invalid objects, expected meta.class objects"); \
    } \
  else \
    error (#OP ": invalid arguments"); \
\
  return retval; \
}

META_CLASS_CMP (lt, clsb, clsa, is_strict_superclass)
META_CLASS_CMP (le, clsb, clsa, is_superclass)
META_CLASS_CMP (gt, clsa, clsb, is_strict_superclass)
META_CLASS_CMP (ge, clsa, clsb, is_superclass)
META_CLASS_CMP (eq, clsa, clsb, operator==)
META_CLASS_CMP (ne, clsa, clsb, operator!=)

octave_value_list
property_get_defaultvalue (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1 && args(0).type_name () == "object")
    {
      cdef_property prop (to_cdef (args(0)));

      retval(0) = prop.get ("DefaultValue");

      if (! retval(0).is_defined ())
        error_with_id ("Octave:class:NotDefaultDefined",
                       "no default value for property `%s'",
                       prop.get_name ().c_str ());
    }

  return retval;
}

static octave_value_list
handle_delete (const octave_value_list& /* args */, int /* nargout */)
{
  octave_value_list retval;

  // FIXME: implement this

  return retval;
}

static cdef_class
make_class (const std::string& name,
            const std::list<cdef_class>& super_list = std::list<cdef_class> ())
{
  cdef_class cls (name, super_list);

  cls.set_class (cdef_class::meta_class ());
  cls.put ("Abstract", false);
  cls.put ("ConstructOnLoad", false);
  cls.put ("ContainingPackage", Matrix ());
  cls.put ("Description", std::string ());
  cls.put ("DetailedDescription", std::string ());
  cls.put ("Events", Cell ());
  cls.put ("Hidden", false);
  cls.put ("InferiorClasses", Cell ());
  cls.put ("Methods", Cell ());
  cls.put ("Properties", Cell ());
  cls.put ("Sealed", false);

  if (name == "handle")
    {
      cls.put ("HandleCompatible", true);
      cls.mark_as_handle_class ();
    }
  else if (super_list.empty ())
    {
      cls.put ("HandleCompatible", false);
    }
  else
    {
      bool all_handle_compatible = true;
      bool has_handle_class = false;

      for (std::list<cdef_class>::const_iterator it = super_list.begin ();
           it != super_list.end (); ++it)
        {
          all_handle_compatible = all_handle_compatible
                                  && it->get ("HandleCompatible").bool_value ();
          has_handle_class = has_handle_class || it->is_handle_class ();
        }

      if (has_handle_class && ! all_handle_compatible)
        ::error ("%s: cannot mix handle and non-HandleCompatible classes",
                 name.c_str ());
      else
        {
          cls.put ("HandleCompatible", all_handle_compatible);
          if (has_handle_class)
            cls.mark_as_handle_class ();
        }
    }

  if (error_state)
    return cdef_class ();

  if (! name.empty ())
    cdef_manager::register_class (cls);

  return cls;
}

static cdef_class
make_class (const std::string& name, const cdef_class& super)
{
  return make_class (name, std::list<cdef_class> (1, super));
}

static cdef_class
make_meta_class (const std::string& name, const cdef_class& super)
{
  cdef_class cls = make_class (name, super);

  cls.put ("Sealed", true);
  cls.mark_as_meta_class ();

  return cls;
}

static cdef_property
make_property (const cdef_class& cls, const std::string& name,
               const octave_value& get_method = Matrix (),
               const std::string& get_access = "public",
               const octave_value& set_method = Matrix (),
               const std::string& set_access = "public")
{
  cdef_property prop (name);

  prop.set_class (cdef_class::meta_property ());
  prop.put ("Description", std::string ());
  prop.put ("DetailedDescription", std::string ());
  prop.put ("Abstract", false);
  prop.put ("Constant", false);
  prop.put ("GetAccess", get_access);
  prop.put ("SetAccess", set_access);
  prop.put ("Dependent", false);
  prop.put ("Transient", false);
  prop.put ("Hidden", false);
  prop.put ("GetObservable", false);
  prop.put ("SetObservable", false);
  prop.put ("GetMethod", get_method);
  prop.put ("SetMethod", set_method);
  prop.put ("DefiningClass", to_ov (cls));
  prop.put ("DefaultValue", octave_value ());
  prop.put ("HasDefault", false);

  std::string class_name = cls.get_name ();

  if (! get_method.is_empty ())
    make_function_of_class (class_name, get_method);
  if (! set_method.is_empty ())
    make_function_of_class (class_name, set_method);

  return prop;
}

inline cdef_property
make_attribute (const cdef_class& cls, const std::string& name)
{
  return make_property (cls, name, Matrix (), "public", Matrix (), "private");
}

static cdef_method
make_method (const cdef_class& cls, const std::string& name,
             const octave_value& fcn,const std::string& m_access = "public",
             bool is_static = false)
{
  cdef_method meth (name);

  meth.set_class (cdef_class::meta_method ());
  meth.put ("Abstract", false);
  meth.put ("Access", m_access);
  meth.put ("DefiningClass", to_ov (cls));
  meth.put ("Description", std::string ());
  meth.put ("DetailedDescription", std::string ());
  meth.put ("Hidden", false);
  meth.put ("Sealed", true);
  meth.put ("Static", is_static);

  if (fcn.is_defined ())
    make_function_of_class (cls, fcn);

  meth.set_function (fcn);

  if (is_dummy_method (fcn))
    meth.mark_as_external (cls.get_name ());

  return meth;
}

inline cdef_method
make_method (const cdef_class& cls, const std::string& name,
             octave_builtin::fcn ff, const std::string& m_access = "public",
             bool is_static = false)
{
  octave_value fcn (new octave_builtin (ff, name));

  return make_method (cls, name, fcn, m_access, is_static);
}

static cdef_package
make_package (const std::string& nm,
              const std::string& parent = std::string ())
{
  cdef_package pack (nm);

  pack.set_class (cdef_class::meta_package ());
  if (parent.empty ())
    pack.put ("ContainingPackage", Matrix ());
  else
    pack.put ("ContainingPackage", to_ov (cdef_manager::find_package (parent)));

  if (! nm.empty ())
    cdef_manager::register_package (pack);

  return pack;
}

//----------------------------------------------------------------------------


int octave_classdef::t_id (-1);

const std::string octave_classdef::t_name ("object");

void
octave_classdef::register_type (void)
{
  t_id = octave_value_typeinfo::register_type
    (octave_classdef::t_name, "<unknown>",
     octave_value (new octave_classdef ()));
}

octave_value_list
octave_classdef::subsref (const std::string& type,
                          const std::list<octave_value_list>& idx,
                          int nargout)
{
  size_t skip = 0;
  octave_value_list retval;

  cdef_class cls = object.get_class ();

  if (! in_class_method (cls) && ! called_from_builtin ())
    {
      cdef_method meth = cls.find_method ("subsref");

      if (meth.ok ())
        {
          octave_value_list args;

          args(1) = make_idx_args (type, idx, "subsref");

          if (! error_state)
            {
              count++;
              args(0) = octave_value (this);

              retval = meth.execute (args, nargout, true, "subsref");
            }

          return retval;
        }
    }

  // At this point, the default subsref mechanism must be used.

  retval = object.subsref (type, idx, nargout, skip, cdef_class ());

  if (! error_state)
    {
      if (type.length () > skip && idx.size () > skip)
        retval = retval(0).next_subsref (nargout, type, idx, skip);
    }

  return retval;
}

octave_value
octave_classdef::subsref (const std::string& type,
                          const std::list<octave_value_list>& idx,
                          bool auto_add)
{
  size_t skip = 0;
  octave_value_list retval;

  // This variant of subsref is used to create temporary values when doing
  // assignment with multi-level indexing. AFAIK this is only used for internal
  // purpose (not sure we should even implement this) and any overload subsref
  // should not be called.

  retval = object.subsref (type, idx, 1, skip, cdef_class (), auto_add);

  if (! error_state)
    {
      if (type.length () > skip && idx.size () > skip)
        retval = retval(0).next_subsref (1, type, idx, skip);
    }

  return retval.length () > 0 ? retval(0) : octave_value ();
}

octave_value
octave_classdef::subsasgn (const std::string& type,
                           const std::list<octave_value_list>& idx,
                           const octave_value& rhs)
{
  octave_value retval;

  cdef_class cls = object.get_class ();

  if (! in_class_method (cls) && ! called_from_builtin ())
    {
      cdef_method meth = cls.find_method ("subsasgn");

      if (meth.ok ())
        {
          octave_value_list args;

          args(1) = make_idx_args (type, idx, "subsasgn");

          if (! error_state)
            {
              count++;
              args(0) = octave_value (this);
              args(2) = rhs;

              octave_value_list retlist;

              retlist = meth.execute (args, 1, true, "subsasgn");

              if (! error_state)
                {
                  if (retlist.length () > 0)
                    retval = retlist(0);
                  else
                    ::error ("overloaded method `subsasgn' did not return any value");
                }
            }
        }
    }

  if (! error_state && ! retval.is_defined ())
    retval = object.subsasgn (type, idx, rhs);

  return retval;
}

octave_value
octave_classdef::undef_subsasgn (const std::string& type,
                                 const std::list<octave_value_list>& idx,
                                 const octave_value& rhs)
{
  if (type.length () == 1 && type[0] == '(')
    {
      object = object.make_array ();

      if (! error_state)
        return subsasgn (type, idx, rhs);
    }
  else
    return octave_base_value::undef_subsasgn (type, idx, rhs);

  return octave_value ();
}

void
octave_classdef::print (std::ostream& os, bool)
{
  if (! called_from_builtin ())
    {
      cdef_method meth = object.get_class ().find_method ("disp");

      if (meth.ok ())
        {
          octave_value_list args;

          count++;
          args(0) = octave_value (this);

          indent (os);
          meth.execute (args, 0, true, "disp");

          return;
        }
    }

  print_raw (os);
}

void
octave_classdef::print_raw (std::ostream& os, bool) const
{
  indent (os);
  os << "<object ";
  if (object.is_array ())
    os << "array ";
  os << class_name () << ">";
  newline (os);
}

bool
octave_classdef::print_name_tag (std::ostream& os,
                                 const std::string& name) const
{
  return octave_base_value::print_name_tag (os, name);
}

void
octave_classdef::print_with_name (std::ostream& os, const std::string& name,
                                  bool print_padding)
{
  cdef_method meth = object.get_class ().find_method ("display");

  if (meth.ok ())
    {
      octave_value_list args;

      count++;
      args(0) = octave_value (this);

      string_vector arg_names (1);

      arg_names[0] = name;
      args.stash_name_tags (arg_names);

      indent (os);
      meth.execute (args, 0, true, "display");
    }
  else
    octave_base_value::print_with_name (os, name, print_padding);
}

bool
octave_classdef::is_instance_of (const std::string& cls_name) const
{
  cdef_class cls = lookup_class (cls_name, false, false);

  if (cls.ok ())
    return is_superclass (cls, object.get_class ());

  return false;
}

//----------------------------------------------------------------------------

class octave_classdef_meta : public octave_function
{
public:
  octave_classdef_meta (const cdef_meta_object& obj)
    : object (obj) { }

  ~octave_classdef_meta (void)
  { object.meta_release (); }

  octave_function* function_value (bool = false) { return this; }

  octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout)
  { return object.meta_subsref (type, idx, nargout); }

  octave_value
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx)
  {
    octave_value_list retval;

    retval = subsref (type, idx, 1);

    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx)
  {
    // Emulate ()-type meta subsref

    std::list<octave_value_list> l (1, idx);
    std::string type ("(");

    return subsref (type, l, nargout);
  }

  bool is_postfix_index_handled (char type) const
  { return object.meta_is_postfix_index_handled (type); }

  bool
  is_classdef_constructor (const std::string& cname = std::string ()) const
  {
    bool retval = false;

    if (object.is_class ())
      {
        if (cname.empty ())
          retval = true;
        else
          {
            cdef_class cls (object);

            if (cls.get_name () == cname)
              retval = true;
          }
      }

    return retval;
  }

private:
  cdef_meta_object object;
};

//----------------------------------------------------------------------------

class octave_classdef_superclass_ref : public octave_function
{
public:
  octave_classdef_superclass_ref (const octave_value_list& a)
    : octave_function (), args (a) { }

  ~octave_classdef_superclass_ref (void) { }

  octave_function* function_value (bool = false) { return this; }

  octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout)
  {
    size_t skip = 0;
    octave_value_list retval;

    switch (type[0])
      {
      case '(':
        skip = 1;
        retval = do_multi_index_op (type.length () > 1 ? 1 : nargout,
                                    idx.front ());
        break;
      default:
        retval = do_multi_index_op (1, octave_value_list ());
        break;
      }

    if (! error_state)
      {
        if (type.length () > skip && idx.size () > skip
            && retval.length () > 0)
          retval = retval(0).next_subsref (nargout, type, idx, skip);
      }

    return retval;
  }

  octave_value
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx)
  {
    octave_value_list retval;

    retval = subsref (type, idx, 1);

    return (retval.length () > 0 ? retval(0) : octave_value ());
  }

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx)
  {
    octave_value_list retval;

    std::string meth_name;
    bool in_constructor;
    cdef_class ctx;

    ctx = get_class_context (meth_name, in_constructor);

    if (! error_state && ctx.ok ())
      {
        std::string mname = args(0).string_value ();
        std::string cname = args(1).string_value ();

        cdef_class cls = lookup_class (cname);

        if (! error_state)
          {
            if (in_constructor)
              {
                if (is_direct_superclass (cls, ctx))
                  {
                    if (is_constructed_object (mname))
                      {
                        octave_value sym = symbol_table::varval (mname);

                        cls.run_constructor (to_cdef_ref (sym), idx);

                        retval(0) = sym;
                      }
                    else
                      ::error ("cannot call superclass constructor with "
                               "variable `%s'", mname.c_str ());
                  }
                else
                  ::error ("`%s' is not a direct superclass of `%s'",
                           cname.c_str (), ctx.get_name ().c_str ());
              }
            else
              {
                if (mname == meth_name)
                  {
                    if (is_strict_superclass (cls, ctx))
                      {
                        // I see 2 possible implementations here:
                        // 1) use cdef_object::subsref with a different class
                        //    context; this avoids duplicating code, but
                        //    assumes the object is always the first argument
                        // 2) lookup the method manually and call
                        //    cdef_method::execute; this duplicates part of
                        //    logic in cdef_object::subsref, but avoid the
                        //    assumption of 1)
                        // Not being sure about the assumption of 1), I
                        // go with option 2) for the time being.

                        cdef_method meth = cls.find_method (meth_name, false);

                        if (meth.ok ())
                          retval = meth.execute (idx, nargout, true,
                                                 meth_name);
                        else
                          ::error ("no method `%s' found in superclass `%s'",
                                   meth_name.c_str (), cname.c_str ());
                      }
                    else
                      ::error ("`%s' is not a superclass of `%s'",
                               cname.c_str (), ctx.get_name ().c_str ());
                  }
                else
                  ::error ("method name mismatch (`%s' != `%s')",
                           mname.c_str (), meth_name.c_str ());
              }
          }
      }
    else if (! error_state)
      ::error ("superclass calls can only occur in methods or constructors");

    return retval;
  }

private:
  bool is_constructed_object (const std::string nm)
  {
    octave_function *of = octave_call_stack::current ();

    if (of->is_classdef_constructor ())
      {
        octave_user_function *uf = of->user_function_value (true);

        if (uf)
          {
            tree_parameter_list *ret_list = uf->return_list ();

            if (ret_list && ret_list->length () == 1)
              return (ret_list->front ()->name () == nm);
          }
      }

    return false;
  }

private:
  octave_value_list args;
};

//----------------------------------------------------------------------------

octave_map
cdef_object::map_value (void) const
{
  octave_map retval;

  warning_with_id ("Octave:classdef-to-struct",
                   "struct: converting a classdef object into a struct "
                   "overrides the access restrictions defined for properties. "
                   "All properties are returned, including private and "
                   "protected ones.");

  cdef_class cls = get_class ();

  if (cls.ok ())
    {
      std::map<std::string, cdef_property> props;

      props = cls.get_property_map (cdef_class::property_all);

      for (std::map<std::string, cdef_property>::iterator it = props.begin ();
           it != props.end (); ++it)
        {
          octave_value pvalue;

          if (is_array ())
            {
              Array<cdef_object> a_obj = array_value ();

              Cell cvalue (a_obj.dims ());

              for (octave_idx_type i = 0; i < a_obj.numel (); i++)
                {
                  cvalue (i) = it->second.get_value (a_obj(i), false);

                  if (error_state)
                    break;
                }

              if (! error_state)
                retval.setfield (it->first, cvalue);
            }
          else
            {
              Cell cvalue (dim_vector (1, 1),
                           it->second.get_value (*this, false));

              if (! error_state)
                retval.setfield (it->first, cvalue);
            }

          if (error_state)
            break;
        }
    }

  return retval;
}

string_vector
cdef_object_rep::map_keys (void) const
{
  cdef_class cls = get_class ();

  if (cls.ok ())
    return cls.get_names ();

  return string_vector ();
}

octave_value_list
cdef_object_scalar::subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout, size_t& skip,
                             const cdef_class& context, bool auto_add)
{
  skip = 0;

  cdef_class cls = (context.ok () ? context : get_class ());

  octave_value_list retval;

  if (! cls.ok ())
    return retval;

  switch (type[0])
    {
    case '.':
      {
        std::string name = (idx.front ())(0).string_value ();

        cdef_method meth = cls.find_method (name);

        if (meth.ok ())
          {
            int _nargout = (type.length () > 2 ? 1 : nargout);

            octave_value_list args;

            skip = 1;

            if (type.length () > 1 && type[1] == '(')
              {
                std::list<octave_value_list>::const_iterator it = idx.begin ();

                args = *++it;

                skip++;
              }

            if (meth.is_static ())
              retval = meth.execute (args, _nargout, true, "subsref");
            else
              {
                refcount++;
                retval = meth.execute (cdef_object (this), args, _nargout,
                                       true, "subsref");
              }
          }

        if (skip == 0 && ! error_state)
          {
            cdef_property prop = cls.find_property (name);

            if (prop.ok ())
              {
                if (prop.is_constant ())
                  retval(0) = prop.get_value (true, "subsref");
                else
                  {
                    refcount++;
                    retval(0) = prop.get_value (cdef_object (this),
                                                true, "subsref");
                  }

                skip = 1;
              }
            else
              error ("subsref: unknown method or property: %s", name.c_str ());
          }
        break;
      }

    case '(':
      {
        refcount++;

        cdef_object this_obj (this);

        Array<cdef_object> arr (dim_vector (1, 1), this_obj);

        cdef_object new_obj = cdef_object (new cdef_object_array (arr));

        new_obj.set_class (get_class ());

        retval = new_obj.subsref (type, idx, nargout, skip, cls, auto_add);
      }
      break;

    default:
      error ("object cannot be indexed with `%c'", type[0]);
      break;
    }

  return retval;
}

octave_value
cdef_object_scalar::subsasgn (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              const octave_value& rhs)
{
  octave_value retval;

  cdef_class cls = get_class ();

  switch (type[0])
    {
    case '.':
      {
        std::string name = (idx.front ())(0).string_value ();

        if (! error_state)
          {
            cdef_property prop = cls.find_property (name);

            if (prop.ok ())
              {
                if (prop.is_constant ())
                  error ("subsasgn: cannot assign constant property: %s",
                         name.c_str ());
                else
                  {
                    refcount++;

                    cdef_object obj (this);

                    if (type.length () == 1)
                      {
                        prop.set_value (obj, rhs, true, "subsasgn");

                        if (! error_state)
                          retval = to_ov (obj);
                      }
                    else
                      {
                        octave_value val =
                          prop.get_value (obj, true, "subsasgn");

                        if (! error_state)
                          {
                            std::list<octave_value_list> args (idx);

                            args.erase (args.begin ());

                            val = val.assign (octave_value::op_asn_eq,
                                              type.substr (1), args, rhs);

                            if (! error_state)
                              {
                                if (val.class_name () != "object"
                                    || ! to_cdef (val).is_handle_object ())
                                  prop.set_value (obj, val, true, "subsasgn");

                                if (! error_state)
                                  retval = to_ov (obj);
                              }
                          }
                      }
                  }
              }
            else
              error ("subsasgn: unknown property: %s", name.c_str ());
          }
      }
      break;

    case '(':
      {
        refcount++;

        cdef_object this_obj (this);

        Array<cdef_object> arr (dim_vector (1, 1), this_obj);

        cdef_object new_obj = cdef_object (new cdef_object_array (arr));

        new_obj.set_class (get_class ());

        octave_value tmp = new_obj.subsasgn (type, idx, rhs);

        if (! error_state)
          retval = tmp;
      }
      break;

    default:
      error ("subsasgn: object cannot be index with `%c'", type[0]);
      break;
    }

  return retval;
}

void
cdef_object_scalar::mark_for_construction (const cdef_class& cls)
{
  std::string cls_name = cls.get_name ();

  Cell supcls = cls.get ("SuperClasses").cell_value ();

  if (! error_state)
    {
      std::list<cdef_class> supcls_list = lookup_classes (supcls);

      if (! error_state)
        ctor_list[cls] = supcls_list;
    }
}

octave_value_list
cdef_object_array::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int /* nargout */, size_t& skip,
                            const cdef_class& /* context */, bool auto_add)
{
  octave_value_list retval;

  skip = 1;

  switch (type[0])
    {
    case '(':
      {
        const octave_value_list& ival = idx.front ();
        bool is_scalar = true;
        Array<idx_vector> iv (dim_vector (1, ival.length ()));

        if (ival.empty ())
          {
            ::error ("can't index %s object(s) with empty parentheses",
                     class_name ().c_str ());
            break;
          }

        for (int i = 0; ! error_state && i < ival.length (); i++)
          {
            iv(i) = ival(i).index_vector ();
            if (! error_state)
              is_scalar = is_scalar && iv(i).is_scalar ();
          }

        if (! error_state)
          {
            Array<cdef_object> ires = array.index (iv, auto_add);

            if (! error_state)
              {
                // If resizing is enabled (auto_add = true), it's possible
                // indexing was out-of-bound and the result array contains
                // invalid cdef_objects.

                if (auto_add)
                  fill_empty_values (ires);

                if (is_scalar)
                  retval(0) = to_ov (ires(0));
                else
                  {
                    cdef_object array_obj (new cdef_object_array (ires));

                    array_obj.set_class (get_class ());

                    retval(0) = to_ov (array_obj);
                  }
              }
          }
      }
      break;

    case '.':
      if (type.size () == 1 && idx.size () == 1)
        {
          Cell c (dims ());

          octave_idx_type n = array.numel ();

          // dummy variables
          size_t dummy_skip;
          cdef_class dummy_cls;

          for (octave_idx_type i = 0; i < n; i++)
            {
              octave_value_list r = array(i).subsref (type, idx, 1, dummy_skip,
                                                      dummy_cls);

              if (! error_state)
                {
                  if (r.length () > 0)
                    c(i) = r(0);
                }
              else
                break;
            }

          if (! error_state)
            retval(0) = octave_value (c, true);

          break;
        }
      // fall through "default"

    default:
      ::error ("can't perform indexing operation on array of %s objects",
               class_name ().c_str ());
      break;
    }

  return retval;
}

octave_value
cdef_object_array::subsasgn (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      if (type.length () == 1)
        {
          cdef_object rhs_obj = to_cdef (rhs);

          if (! error_state)
            {
              if (rhs_obj.get_class () == get_class ())
                {
                  const octave_value_list& ival = idx.front ();
                  bool is_scalar = true;
                  Array<idx_vector> iv (dim_vector (1, ival.length ()));

                  for (int i = 0; ! error_state && i < ival.length (); i++)
                    {
                      iv(i) = ival(i).index_vector ();
                      if (! error_state)
                        is_scalar = is_scalar && iv(i).is_scalar ();
                    }

                  if (! error_state)
                    {
                      Array<cdef_object> rhs_mat;

                      if (! rhs_obj.is_array ())
                        {
                          rhs_mat = Array<cdef_object> (dim_vector (1, 1));
                          rhs_mat(0) = rhs_obj;
                        }
                      else
                        rhs_mat = rhs_obj.array_value ();

                      if (! error_state)
                        {
                          octave_idx_type n = array.numel ();

                          array.assign (iv, rhs_mat, cdef_object ());

                          if (! error_state)
                            {
                              if (array.numel () > n)
                                fill_empty_values ();

                              if (! error_state)
                                {
                                  refcount++;
                                  retval = to_ov (cdef_object (this));
                                }
                            }
                        }
                    }
                }
              else
                ::error ("can't assign %s object into array of %s objects.",
                         rhs_obj.class_name ().c_str (),
                         class_name ().c_str ());
            }
        }
      else
        {
          const octave_value_list& ival = idx.front ();

          bool is_scalar = true;

          Array<idx_vector> iv (dim_vector (1, ival.length ()));

          for (int i = 0; ! error_state && i < ival.length (); i++)
            {
              iv(i) = ival(i).index_vector ();

              if (! error_state)
                {
                  is_scalar = is_scalar && iv(i).is_scalar ();

                  if (! is_scalar)
                    error ("subsasgn: invalid indexing for object array "
                           "assignment, the index must reference a single "
                           "object in the array.");
                }
            }

          if (! error_state)
            {
              Array<cdef_object> a = array.index (iv, true);

              if (a.numel () != 1)
                error ("subsasgn: invalid indexing for object array "
                       "assignment");

              if (! error_state)
                {
                  cdef_object obj = a(0);

                  int ignore_copies = 0;

                  // If the object in 'a' is not valid, this means the index
                  // was out-of-bound and we need to create a new object.

                  if (! obj.ok ())
                    obj = get_class ().construct_object (octave_value_list ());
                  else
                    // Optimize the subsasgn call to come. There are 2 copies
                    // that we can safely ignore:
                    // - 1 in "array"
                    // - 1 in "a"
                    ignore_copies = 2;

                  std::list<octave_value_list> next_idx (idx);

                  next_idx.erase (next_idx.begin ());

                  octave_value tmp = obj.subsasgn (type.substr (1), next_idx,
                                                   rhs, ignore_copies);

                  if (! error_state)
                    {
                      cdef_object robj = to_cdef (tmp);

                      if (robj.ok ()
                          && ! robj.is_array ()
                          && robj.get_class () == get_class ())
                        {
                          // Small optimization, when dealing with handle
                          // objects, we don't need to re-assign the result
                          // of subsasgn back into the array.

                          if (! robj.is (a(0)))
                            {
                              Array<cdef_object> rhs_a (dim_vector (1, 1),
                                                        robj);

                              octave_idx_type n = array.numel ();

                              array.assign (iv, rhs_a);

                              if (array.numel () > n)
                                fill_empty_values ();
                            }

                          refcount++;

                          retval = to_ov (cdef_object (this));
                        }
                      else
                        error ("subasgn: invalid assignment into array of %s "
                               "objects", class_name ().c_str ());
                    }
                }
            }
        }
      break;

    default:
      ::error ("can't perform indexing operation on array of %s objects",
               class_name ().c_str ());
      break;
    }

  return retval;
}

void
cdef_object_array::fill_empty_values (Array<cdef_object>& arr)
{
  cdef_class cls = get_class ();

  if (! error_state)
    {
      cdef_object obj;

      int n = arr.numel ();

      for (int i = 0; ! error_state && i < n; i++)
        {
          if (! arr.xelem (i).ok ())
            {
              if (! obj.ok ())
                {
                  obj = cls.construct_object (octave_value_list ());

                  if (! error_state)
                    arr.xelem (i) = obj;
                }
              else
                arr.xelem (i) = obj.copy ();
            }
        }
    }
}

bool
cdef_object_scalar::is_constructed_for (const cdef_class& cls) const
{
  return (is_constructed ()
          || ctor_list.find (cls) == ctor_list.end ());
}

bool
cdef_object_scalar::is_partially_constructed_for (const cdef_class& cls) const
{
  std::map< cdef_class, std::list<cdef_class> >::const_iterator it;

  if (is_constructed ())
    return true;
  else if ((it = ctor_list.find (cls)) == ctor_list.end ()
           || it->second.empty ())
    return true;

  for (std::list<cdef_class>::const_iterator lit = it->second.begin ();
       lit != it->second.end (); ++lit)
    if (! is_constructed_for (*lit))
      return false;

  return true;
}

void
cdef_object_scalar::mark_as_constructed (const cdef_class& cls)
{
  ctor_list.erase (cls);
}

handle_cdef_object::~handle_cdef_object (void)
{
#if DEBUG_TRACE
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (handle)" << std::endl;
#endif
}

value_cdef_object::~value_cdef_object (void)
{
#if DEBUG_TRACE
  std::cerr << "deleting " << get_class ().get_name ()
            << " object (value)" << std::endl;
#endif
}

cdef_class::cdef_class_rep::cdef_class_rep (const std::list<cdef_class>&
                                            superclasses)
  : cdef_meta_object_rep (), member_count (0), handle_class (false),
    object_count (0), meta (false)
{
  put ("SuperClasses", to_ov (superclasses));
  implicit_ctor_list = superclasses;
}

cdef_method
cdef_class::cdef_class_rep::find_method (const std::string& nm, bool local)
{
  method_iterator it = method_map.find (nm);

  if (it == method_map.end ())
    {
      // FIXME: look into class directory
    }
  else
    {
      cdef_method& meth = it->second;

      // FIXME: check if method reload needed

      if (meth.ok ())
        return meth;
    }

  if (! local)
    {
      // Look into superclasses

      Cell super_classes = get ("SuperClasses").cell_value ();

      for (int i = 0; i < super_classes.numel (); i++)
        {
          cdef_class cls = lookup_class (super_classes(i));

          if (! error_state)
            {
              cdef_method meth = cls.find_method (nm);

              if (meth.ok ())
                return meth;
            }
        }
    }

  return cdef_method ();
}

class ctor_analyzer : public tree_walker
{
public:
  ctor_analyzer (const std::string& ctor, const std::string& obj)
    : tree_walker (), who (ctor), obj_name (obj) { }

  void visit_statement_list (tree_statement_list& t)
  {
    for (tree_statement_list::const_iterator it = t.begin ();
         ! error_state && it != t.end (); ++it)
      (*it)->accept (*this);
  }

  void visit_statement (tree_statement& t)
  {
    if (t.is_expression ())
      t.expression ()->accept (*this);
  }

  void visit_simple_assignment (tree_simple_assignment& t)
  {
    t.right_hand_side ()->accept (*this);
  }

  void visit_multi_assignment (tree_multi_assignment& t)
  {
    t.right_hand_side ()->accept (*this);
  }

  void visit_index_expression (tree_index_expression& t)
  {
    t.expression ()->accept (*this);
  }

  void visit_funcall (tree_funcall& t)
  {
    octave_value fcn = t.function ();

    if (fcn.is_function ())
      {
        octave_function *of = fcn.function_value (true);

        if (of)
          {
            if (of->name () == "__superclass_reference__")
              {
                octave_value_list args = t.arguments ();

                if (args(0).string_value () == obj_name)
                  {
                    std::string class_name = args(1).string_value ();

                    cdef_class cls = lookup_class (class_name, false);

                    if (cls.ok ())
                      ctor_list.push_back (cls);
                  }
              }
          }
      }
  }

  std::list<cdef_class> get_constructor_list (void) const
  { return ctor_list; }

  // NO-OP
  void visit_anon_fcn_handle (tree_anon_fcn_handle&) { }
  void visit_argument_list (tree_argument_list&) { }
  void visit_binary_expression (tree_binary_expression&) { }
  void visit_break_command (tree_break_command&) { }
  void visit_colon_expression (tree_colon_expression&) { }
  void visit_continue_command (tree_continue_command&) { }
  void visit_global_command (tree_global_command&) { }
  void visit_persistent_command (tree_persistent_command&) { }
  void visit_decl_elt (tree_decl_elt&) { }
  void visit_decl_init_list (tree_decl_init_list&) { }
  void visit_simple_for_command (tree_simple_for_command&) { }
  void visit_complex_for_command (tree_complex_for_command&) { }
  void visit_octave_user_script (octave_user_script&) { }
  void visit_octave_user_function (octave_user_function&) { }
  void visit_function_def (tree_function_def&) { }
  void visit_identifier (tree_identifier&) { }
  void visit_if_clause (tree_if_clause&) { }
  void visit_if_command (tree_if_command&) { }
  void visit_if_command_list (tree_if_command_list&) { }
  void visit_switch_case (tree_switch_case&) { }
  void visit_switch_case_list (tree_switch_case_list&) { }
  void visit_switch_command (tree_switch_command&) { }
  void visit_matrix (tree_matrix&) { }
  void visit_cell (tree_cell&) { }
  void visit_no_op_command (tree_no_op_command&) { }
  void visit_constant (tree_constant&) { }
  void visit_fcn_handle (tree_fcn_handle&) { }
  void visit_parameter_list (tree_parameter_list&) { }
  void visit_postfix_expression (tree_postfix_expression&) { }
  void visit_prefix_expression (tree_prefix_expression&) { }
  void visit_return_command (tree_return_command&) { }
  void visit_return_list (tree_return_list&) { }
  void visit_try_catch_command (tree_try_catch_command&) { }
  void visit_unwind_protect_command (tree_unwind_protect_command&) { }
  void visit_while_command (tree_while_command&) { }
  void visit_do_until_command (tree_do_until_command&) { }

private:
  /* The name of the constructor being analyzed */
  std::string who;

  /* The name of the first output argument of the constructor */
  std::string obj_name;

  /* The list of superclass constructors that are explicitly called */
  std::list<cdef_class> ctor_list;
};

void
cdef_class::cdef_class_rep::install_method (const cdef_method& meth)
{
  method_map[meth.get_name ()] = meth;

  member_count++;

  if (meth.is_constructor ())
    {
      // Analyze the constructor code to determine what superclass
      // constructors are called explicitly.

      octave_function *of = meth.get_function ().function_value (true);

      if (of)
        {
          octave_user_function *uf = of->user_function_value (true);

          if (uf)
            {
              tree_parameter_list *ret_list = uf->return_list ();
              tree_statement_list *body = uf->body ();

              if (ret_list && ret_list->size () == 1)
                {
                  std::string obj_name = ret_list->front ()->name ();
                  ctor_analyzer a (meth.get_name (), obj_name);

                  body->accept (a);
                  if (! error_state)
                    {
                      std::list<cdef_class> explicit_ctor_list
                        = a.get_constructor_list ();

                      for (std::list<cdef_class>::const_iterator
                           it = explicit_ctor_list.begin ();
                           ! error_state && it != explicit_ctor_list.end ();
                           ++it)
                        {
#if DEBUG_TRACE
                          std::cerr << "explicit superclass constructor: "
                                    << it->get_name () << std::endl;
#endif

                          implicit_ctor_list.remove (*it);
                        }
                    }
                }
              else
                ::error ("%s: invalid constructor output arguments",
                         meth.get_name ().c_str ());
            }
        }
    }
}

void
cdef_class::cdef_class_rep::load_all_methods (void)
{
  // FIXME: re-scan class directory
}

Cell
cdef_class::cdef_class_rep::get_methods (void)
{
  std::map<std::string,cdef_method> meths;

  find_methods (meths, false);

  if (! error_state)
    {
      Cell c (meths.size (), 1);

      int idx = 0;

      for (std::map<std::string,cdef_method>::const_iterator
            it = meths.begin (); it != meths.end (); ++it, ++idx)
        c (idx, 0) = to_ov (it->second);

      return c;
    }

  return Cell ();
}

void
cdef_class::cdef_class_rep::find_methods (std::map<std::string,
                                          cdef_method>& meths,
                                          bool only_inherited)
{
  load_all_methods ();

  method_const_iterator it;

  for (it = method_map.begin (); it != method_map.end (); ++it)
    {
      if (! it->second.is_constructor ())
        {
          std::string nm = it->second.get_name ();

          if (meths.find (nm) == meths.end ())
            {
              if (only_inherited)
                {
                  octave_value acc = it->second.get ("Access");

                  if (! acc.is_string ()
                      || acc.string_value () == "private")
                    continue;
                }

              meths[nm] = it->second;
            }
        }
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      if (! error_state)
        cls.get_rep ()->find_methods (meths, true);
      else
        break;
    }
}

cdef_property
cdef_class::cdef_class_rep::find_property (const std::string& nm)
{
  property_iterator it = property_map.find (nm);

  if (it != property_map.end ())
    {
      cdef_property& prop = it->second;

      if (prop.ok ())
        return prop;
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      if (! error_state)
        {
          cdef_property prop = cls.find_property (nm);

          if (prop.ok ())
            return prop;
        }
    }

  return cdef_property ();
}

void
cdef_class::cdef_class_rep::install_property (const cdef_property& prop)
{
  property_map[prop.get_name ()] = prop;

  member_count++;
}

Cell
cdef_class::cdef_class_rep::get_properties (int mode)
{
  std::map<std::string,cdef_property> props;

  props = get_property_map (mode);

  if (! error_state)
    {
      Cell c (props.size (), 1);

      int idx = 0;

      for (std::map<std::string,cdef_property>::const_iterator
            it = props.begin (); it != props.end (); ++it, ++idx)
        c (idx, 0) = to_ov (it->second);

      return c;
    }

  return Cell ();
}

std::map<std::string, cdef_property>
cdef_class::cdef_class_rep::get_property_map (int mode)
{
  std::map<std::string,cdef_property> props;

  find_properties (props, mode);

  return props;
}

void
cdef_class::cdef_class_rep::find_properties (std::map<std::string,
                                             cdef_property>& props,
                                             int mode)
{
  property_const_iterator it;

  for (it = property_map.begin (); ! error_state && it != property_map.end ();
       ++it)
    {
      std::string nm = it->second.get_name ();

      if (props.find (nm) == props.end ())
        {
          if (mode == property_inherited)
            {
              octave_value acc = it->second.get ("GetAccess");

              if (! acc.is_string ()
                  || acc.string_value () == "private")
                continue;
            }

          props[nm] = it->second;
        }
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; ! error_state && i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      if (! error_state)
        cls.get_rep ()->find_properties (props,
                                         (mode == property_all ?
                                          property_all :
                                          property_inherited));
      else
        break;
    }
}

void
cdef_class::cdef_class_rep::find_names (std::set<std::string>& names,
                                        bool all)
{
  load_all_methods ();

  for (method_const_iterator it = method_map.begin ();
       ! error_state && it != method_map.end(); ++it)
    {
      if (! it->second.is_constructor ())
        {
          std::string nm = it->second.get_name ();

          if (! all)
            {
              octave_value acc = it->second.get ("Access");

              if (! acc.is_string()
                  || acc.string_value () != "public")
                continue;
            }

          names.insert (nm);
        }
    }

  for (property_const_iterator it = property_map.begin ();
       ! error_state && it != property_map.end (); ++it)
    {
      std::string nm = it->second.get_name ();

      if (! all)
        {
          octave_value acc = it->second.get ("GetAccess");

          if (! acc.is_string()
              || acc.string_value () != "public")
            continue;
        }

      names.insert (nm);
    }

  // Look into superclasses

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; ! error_state && i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      if (! error_state)
        cls.get_rep ()->find_names (names, all);
      else
        break;
    }
}

string_vector
cdef_class::cdef_class_rep::get_names (void)
{
  std::set<std::string> names;

  find_names (names, false);

  if (! error_state)
    {
      string_vector v (names.size ());

      int idx = 0;
      for (std::set<std::string>::const_iterator it = names.begin ();
           it != names.end (); ++it, ++idx)
        v[idx] = *it;

      return v.sort (true);
    }

  return string_vector ();
}

void
cdef_class::cdef_class_rep::delete_object (cdef_object obj)
{
  method_iterator it = method_map.find ("delete");

  if (it != method_map.end ())
    {
      cdef_class cls = obj.get_class ();

      obj.set_class (wrap ());

      it->second.execute (obj, octave_value_list (), 0, false);

      obj.set_class (cls);
    }

  // FIXME: should we destroy corresponding properties here?

  // Call "delete" in super classes

  Cell super_classes = get ("SuperClasses").cell_value ();

  for (int i = 0; i < super_classes.numel (); i++)
    {
      cdef_class cls = lookup_class (super_classes(i));

      if (!error_state)
        cls.delete_object (obj);
    }
}

octave_value_list
cdef_class::cdef_class_rep::meta_subsref (const std::string& type,
                                          const std::list<octave_value_list>& idx,
                                          int nargout)
{
  size_t skip = 1;

  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      // Constructor call

#if DEBUG_TRACE
      std::cerr << "constructor" << std::endl;
#endif

      retval(0) = construct (idx.front ());
      break;

    case '.':
      // Static method, constant (or property?)

#if DEBUG_TRACE
      std::cerr << "static method/property" << std::endl;
#endif

      if (idx.front ().length () == 1)
        {
          std::string nm = idx.front ()(0).string_value ();

          if (! error_state)
            {
              cdef_method meth = find_method (nm);

              if (meth.ok ())
                {
                  if (meth.is_static ())
                    {
                      octave_value_list args;

                      if (type.length () > 1 && idx.size () > 1
                          && type[1] == '(')
                        {
                          args = *(++(idx.begin ()));
                          skip++;
                        }

                      retval = meth.execute (args, (type.length () > skip
                                                    ? 1 : nargout), true,
                                             "meta.class");
                    }
                  else
                    ::error ("method `%s' is not static", nm.c_str ());
                }
              else
                {
                  cdef_property prop = find_property (nm);

                  if (prop.ok ())
                    {
                      if (prop.is_constant ())
                        retval(0) = prop.get_value (true, "meta.class");
                      else
                        ::error ("property `%s' is not constant",
                                 nm.c_str ());
                    }
                  else
                    ::error ("no such method or property `%s'", nm.c_str ());
                }
            }
          else
            ::error ("invalid meta.class indexing, expected a method or property name");
        }
      else
        ::error ("invalid meta.class indexing");
      break;

    default:
      ::error ("invalid meta.class indexing");
      break;
    }

  if (! error_state)
    {
      if (type.length () > skip && idx.size () > skip && ! retval.empty ())
        retval = retval(0).next_subsref (nargout, type, idx, skip);
    }

  return retval;
}

void
cdef_class::cdef_class_rep::meta_release (void)
{
  cdef_manager::unregister_class (wrap ());
}

void
cdef_class::cdef_class_rep::initialize_object (cdef_object& obj)
{
  // Populate the object with default property values

  std::list<cdef_class> super_classes = lookup_classes (
                                          get ("SuperClasses").cell_value ());

  if (! error_state)
    {
      for (std::list<cdef_class>::iterator it = super_classes.begin ();
           ! error_state && it != super_classes.end (); ++it)
        it->initialize_object (obj);

      if (! error_state)
        {
          for (property_const_iterator it = property_map.begin ();
               ! error_state && it != property_map.end (); ++it)
            {
              if (! it->second.get ("Dependent").bool_value ())
                {
                  octave_value pvalue = it->second.get ("DefaultValue");

                  if (pvalue.is_defined ())
                    obj.put (it->first, pvalue);
                  else
                    obj.put (it->first, octave_value (Matrix ()));
                }
            }

          if (! error_state)
            {
              refcount++;
              obj.mark_for_construction (cdef_class (this));
            }
        }
    }
}

void
cdef_class::cdef_class_rep::run_constructor (cdef_object& obj,
                                             const octave_value_list& args)
{
  octave_value_list empty_args;

  for (std::list<cdef_class>::const_iterator it = implicit_ctor_list.begin ();
       ! error_state && it != implicit_ctor_list.end (); ++it)
    {
      cdef_class supcls = lookup_class (*it);

      if (! error_state)
        supcls.run_constructor (obj, empty_args);
    }

  if (error_state)
    return;

  std::string cls_name = get_name ();
  std::string ctor_name = get_base_name (cls_name);

  cdef_method ctor = find_method (ctor_name);

  if (ctor.ok ())
    {
      octave_value_list ctor_args (args);
      octave_value_list ctor_retval;

      ctor_args.prepend (to_ov (obj));
      ctor_retval = ctor.execute (ctor_args, 1, true, "constructor");

      if (! error_state)
        {
          if (ctor_retval.length () == 1)
            obj = to_cdef (ctor_retval(0));
          else
            {
              ::error ("%s: invalid number of output arguments for classdef constructor",
                       ctor_name.c_str ());
              return;
            }
        }
    }

  obj.mark_as_constructed (wrap ());
}

octave_value
cdef_class::cdef_class_rep::construct (const octave_value_list& args)
{
  cdef_object obj = construct_object (args);

  if (! error_state && obj.ok ())
    return to_ov (obj);

  return octave_value ();
}

cdef_object
cdef_class::cdef_class_rep::construct_object (const octave_value_list& args)
{
  if (! is_abstract ())
    {
      cdef_object obj;

      if (is_meta_class ())
        {
          // This code path is only used to create empty meta objects
          // as filler for the empty values within a meta object array.

          cdef_class this_cls = wrap ();

          static cdef_object empty_class;

          if (this_cls == cdef_class::meta_class ())
            {
              if (! empty_class.ok ())
                empty_class = make_class ("", std::list<cdef_class> ());
              obj = empty_class;
            }
          else if (this_cls == cdef_class::meta_property ())
            {
              static cdef_property empty_property;

              if (! empty_class.ok ())
                empty_class = make_class ("", std::list<cdef_class> ());
              if (! empty_property.ok ())
                empty_property = make_property (empty_class, "");
              obj = empty_property;
            }
          else if (this_cls == cdef_class::meta_method ())
            {
              static cdef_method empty_method;

              if (! empty_class.ok ())
                empty_class = make_class ("", std::list<cdef_class> ());
              if (! empty_method.ok ())
                empty_method = make_method (empty_class, "", octave_value ());
              obj = empty_method;
            }
          else if (this_cls == cdef_class::meta_package ())
            {
              static cdef_package empty_package;

              if (! empty_package.ok ())
                empty_package = make_package ("");
              obj = empty_package;
            }
          else
            panic_impossible ();

          return obj;
        }
      else
        {
          if (is_handle_class ())
            obj = cdef_object (new handle_cdef_object ());
          else
            obj = cdef_object (new value_cdef_object ());
          obj.set_class (wrap ());

          initialize_object (obj);

          if (! error_state)
            {
              run_constructor (obj, args);

              if (! error_state)
                return obj;
            }
        }
    }
  else
    error ("cannot instantiate object for abstract class `%s'",
           get_name ().c_str ());

  return cdef_object ();
}

static octave_value
compute_attribute_value (tree_classdef_attribute* t)
{
  if (t->expression ())
    {
      if (t->expression ()->is_identifier ())
        {
          std::string s = t->expression ()->name ();

          if (s == "public")
            return std::string ("public");
          else if (s == "protected")
            return std::string ("protected");
          else if (s == "private")
            return std::string ("private");
        }

      return t->expression ()->rvalue1 ();
    }
  else
    return octave_value (true);
}

template<class T>
static std::string
attribute_value_to_string (T* t, octave_value v)
{
  if (v.is_string ())
    return v.string_value ();
  else if (t->expression ())
    return t->expression ()->original_text ();
  else
    return std::string ("true");
}

cdef_class
cdef_class::make_meta_class (tree_classdef* t, bool is_at_folder)
{
  cdef_class retval;
  std::string class_name, full_class_name;

  // Class creation

  class_name = full_class_name = t->ident ()->name ();
  if (! t->package_name ().empty ())
    full_class_name = t->package_name () + "." + full_class_name;

#if DEBUG_TRACE
  std::cerr << "class: " << full_class_name << std::endl;
#endif

  std::list<cdef_class> slist;

  if (t->superclass_list ())
    {
      for (tree_classdef_superclass_list::iterator it =
             t->superclass_list ()->begin ();
           ! error_state && it != t->superclass_list ()->end (); ++it)
        {
          std::string sclass_name = (*it)->class_name ();

#if DEBUG_TRACE
          std::cerr << "superclass: " << sclass_name << std::endl;
#endif

          cdef_class sclass = lookup_class (sclass_name);

          if (! error_state)
            {
              if (! sclass.get ("Sealed").bool_value ())
                slist.push_back (sclass);
              else
                {
                  ::error ("`%s' cannot inherit from `%s', because it is sealed",
                           full_class_name.c_str (), sclass_name.c_str ());
                  return retval;
                }
            }
          else
            return retval;

        }
    }

  retval = ::make_class (full_class_name, slist);

  if (error_state)
    return cdef_class ();

  // Package owning this class

  if (! t->package_name ().empty ())
    {
      cdef_package pack = cdef_manager::find_package (t->package_name ());

      if (! error_state && pack.ok ())
        retval.put ("ContainingPackage", to_ov (pack));
    }

  // Class attributes

  if (t->attribute_list ())
    {
      for (tree_classdef_attribute_list::iterator
           it = t->attribute_list ()->begin ();
           it != t->attribute_list ()->end ();
           ++it)
        {
          std::string aname = (*it)->ident ()->name ();
          octave_value avalue = compute_attribute_value (*it);

#if DEBUG_TRACE
          std::cerr << "class attribute: " << aname << " = "
                    << attribute_value_to_string (*it, avalue) << std::endl;
#endif

          retval.put (aname, avalue);
        }
    }

  tree_classdef_body* b = t->body ();

  if (b)
    {
      // Keep track of the get/set accessor methods. They will be used
      // later on when creating properties.

      std::map<std::string, octave_value> get_methods;
      std::map<std::string, octave_value> set_methods;

      // Method blocks

      std::list<tree_classdef_methods_block *> mb_list = b->methods_list ();

      for (tree_classdef_body::methods_list_iterator it = mb_list.begin ();
           it != mb_list.end (); ++it)
        {
          std::map<std::string, octave_value> amap;

#if DEBUG_TRACE
          std::cerr << "method block" << std::endl;
#endif

          // Method attributes

          if ((*it)->attribute_list ())
            {
              for (tree_classdef_attribute_list::iterator ait =
                     (*it)->attribute_list ()->begin ();
                   ait != (*it)->attribute_list ()->end (); ++ait)
                {
                  std::string aname = (*ait)->ident ()->name ();
                  octave_value avalue = compute_attribute_value (*ait);

#if DEBUG_TRACE
                  std::cerr << "method attribute: " << aname << " = "
                            << attribute_value_to_string (*ait, avalue)
                            << std::endl;
#endif

                  amap[aname] = avalue;
                }
            }

          // Methods

          if ((*it)->element_list ())
            {
              for (tree_classdef_methods_list::iterator mit =
                     (*it)->element_list ()->begin ();
                   mit != (*it)->element_list ()->end (); ++mit)
                {
                  std::string mname = mit->function_value ()->name ();
                  std::string mprefix = mname.substr (0, 4);

                  if (mprefix == "get.")
                    get_methods[mname.substr (4)] =
                      make_fcn_handle (*mit, full_class_name + ">" + mname);
                  else if (mprefix == "set.")
                    set_methods[mname.substr (4)] =
                      make_fcn_handle (*mit, full_class_name + ">" + mname);
                  else
                    {
                      cdef_method meth = make_method (retval, mname, *mit);

#if DEBUG_TRACE
                      std::cerr << (mname == class_name ? "constructor"
                                                        : "method")
                                << ": " << mname << std::endl;
#endif

                      for (std::map<std::string, octave_value>::iterator
                           ait = amap.begin (); ait != amap.end (); ++ait)
                        meth.put (ait->first, ait->second);

                      retval.install_method (meth);
                    }
                }
            }
        }

      if (is_at_folder)
        {
          // Look for all external methods visible on octave path at the
          // time of loading of the class.
          //
          // TODO: This is an "extension" to Matlab behavior, which only
          // looks in the @-folder containing the original classdef
          // file. However, this is easier to implement it that way at
          // the moment.

          std::list<std::string> external_methods =
            load_path::methods (full_class_name);

          for (std::list<std::string>::const_iterator
               it = external_methods.begin ();
               it != external_methods.end ();
               ++it)
            {
              // TODO: should we issue a warning if the method is already
              // defined in the classdef file?

              if (*it != class_name
                  && ! retval.find_method (*it, true).ok ())
                {
                  // Create a dummy method that is used until the actual
                  // method is loaded.

                  octave_user_function *fcn = new octave_user_function ();

                  fcn->stash_function_name (*it);

                  cdef_method meth = make_method (retval, *it,
                                                  octave_value (fcn));

                  retval.install_method (meth);
                }
            }
        }

      // Property blocks

      // FIXME: default property expression should be able to call static
      //        methods of the class being constructed. A restricted CLASSNAME
      //        symbol should be added to the scope before evaluating default
      //        value expressions.

      std::list<tree_classdef_properties_block *> pb_list
        = b->properties_list ();

      for (tree_classdef_body::properties_list_iterator it = pb_list.begin ();
           it != pb_list.end (); ++it)
        {
          std::map<std::string, octave_value> amap;

#if DEBUG_TRACE
          std::cerr << "property block" << std::endl;
#endif

          // Property attributes

          if ((*it)->attribute_list ())
            {
              for (tree_classdef_attribute_list::iterator ait =
                     (*it)->attribute_list ()->begin ();
                   ait != (*it)->attribute_list ()->end (); ++ait)
                {
                  std::string aname = (*ait)->ident ()->name ();
                  octave_value avalue = compute_attribute_value (*ait);

#if DEBUG_TRACE
                  std::cerr << "property attribute: " << aname << " = "
                            << attribute_value_to_string (*ait, avalue)
                            << std::endl;
#endif

                  if (aname == "Access")
                    {
                      amap["GetAccess"] = avalue;
                      amap["SetAccess"] = avalue;
                    }
                  else
                    amap[aname] = avalue;
                }
            }

          // Properties

          if ((*it)->element_list ())
            {
              for (tree_classdef_property_list::iterator pit =
                     (*it)->element_list ()->begin ();
                   pit != (*it)->element_list ()->end (); ++pit)
                {
                  std::string prop_name = (*pit)->ident ()->name ();

                  cdef_property prop = ::make_property (retval, prop_name);

#if DEBUG_TRACE
                  std::cerr << "property: " << (*pit)->ident ()->name ()
                            << std::endl;
#endif

                  if ((*pit)->expression ())
                    {
                      octave_value pvalue = (*pit)->expression ()->rvalue1 ();

#if DEBUG_TRACE
                      std::cerr << "property default: "
                                << attribute_value_to_string (*pit, pvalue)
                                << std::endl;
#endif

                      prop.put ("DefaultValue", pvalue);
                    }

                  // Install property attributes.  This is done before assigning
                  // the property accessors so we can do validationby using
                  // cdef_property methods.

                  for (std::map<std::string, octave_value>::iterator ait = amap.begin ();
                       ait != amap.end (); ++ait)
                    prop.put (ait->first, ait->second);

                  // Install property access methods, if any. Remove the
                  // accessor methods from the temporary storage map, so we can
                  // detect which ones are invalid and do not correspond to a
                  // defined property.

                  std::map<std::string, octave_value>::iterator git =
                    get_methods.find (prop_name);

                  if (git != get_methods.end ())
                    {
                      make_function_of_class (retval, git->second);
                      prop.put ("GetMethod", git->second);
                      get_methods.erase (git);
                    }

                  std::map<std::string, octave_value>::iterator sit =
                    set_methods.find (prop_name);

                  if (sit != set_methods.end ())
                    {
                      make_function_of_class (retval, sit->second);
                      prop.put ("SetMethod", sit->second);
                      set_methods.erase (sit);
                    }

                  retval.install_property (prop);
                }
            }
        }
    }

  return retval;
}

octave_function*
cdef_class::get_method_function (const std::string& /* nm */)
{
  octave_classdef_meta* p = new octave_classdef_meta (*this);

  return p;
}

octave_value
cdef_property::cdef_property_rep::get_value (const cdef_object& obj,
                                             bool do_check_access,
                                             const std::string& who)
{
  octave_value retval;

  if (do_check_access && ! check_get_access ())
    {
      gripe_property_access (who, wrap (), false);

      return retval;
    }

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        {
          ::error ("cannot reference properties of class `%s' for non-constructed object",
                   cls.get_name ().c_str ());
          return retval;
        }
    }

  octave_value get_fcn = get ("GetMethod");

  // FIXME: should check whether we're already in get accessor method

  if (get_fcn.is_empty () || is_method_executing (get_fcn, obj))
    retval = obj.get (get ("Name").string_value ());
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);

      args = execute_ov (get_fcn, args, 1);

      if (! error_state)
        retval = args(0);
    }

  return retval;
}

octave_value
cdef_property::cdef_property_rep::get_value (bool do_check_access,
                                             const std::string& who)
{
  if (do_check_access && ! check_get_access ())
    {
      gripe_property_access (who, wrap (), false);

      return octave_value ();
    }

  return get ("DefaultValue");
}

bool
cdef_property::cdef_property_rep::is_recursive_set (const cdef_object& /* obj */) const
{
  // FIXME: implement
  return false;
}

void
cdef_property::cdef_property_rep::set_value (cdef_object& obj,
                                             const octave_value& val,
                                             bool do_check_access,
                                             const std::string& who)
{
  if (do_check_access && ! check_set_access ())
    {
      gripe_property_access (who, wrap (), true);

      return;
    }

  if (! obj.is_constructed ())
    {
      cdef_class cls (to_cdef (get ("DefiningClass")));

      if (! obj.is_partially_constructed_for (cls))
        {
          ::error ("cannot reference properties of class `%s' for non-constructed object",
                   cls.get_name ().c_str ());
          return;
        }
    }

  octave_value set_fcn = get ("SetMethod");

  if (set_fcn.is_empty () || is_method_executing (set_fcn, obj))
    obj.put (get ("Name").string_value (), val);
  else
    {
      octave_value_list args;

      args(0) = to_ov (obj);
      args(1) = val;

      args = execute_ov (set_fcn, args, 1);

      if (! error_state)
        {
          if (args.length () > 0 && args(0).is_defined ())
            {
              if (args (0).is_classdef_object ())
                {
                  cdef_object new_obj = to_cdef (args(0));

                  if (! error_state)
                    obj = new_obj;
                }
              else
                ::warning ("set-method of property `%s' returned a non-classdef object",
                           get_name ().c_str ());
            }
        }
    }
}

bool
cdef_property::cdef_property_rep::check_get_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  if (! error_state)
    return ::check_access (cls, get ("GetAccess"), std::string (),
                           get_name (), false);

  return false;
}

bool
cdef_property::cdef_property_rep::check_set_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  if (! error_state)
    return ::check_access (cls, get ("SetAccess"), std::string (),
                           get_name (), true);

  return false;
}

void
cdef_method::cdef_method_rep::check_method (void)
{
  if (is_external ())
    {
      if (is_dummy_method (function))
        {
          std::string name = get_name ();
          std::string cls_name = dispatch_type;
          std::string pack_name;

          size_t pos = cls_name.rfind ('.');

          if (pos != std::string::npos)
            {
              pack_name = cls_name.substr (0, pos);
              cls_name = cls_name.substr (pos + 1);
            }

          std::string dir_name;
          std::string file_name = load_path::find_method (cls_name, name,
                                                          dir_name, pack_name);

          if (! file_name.empty ())
            {
              octave_function *fcn = load_fcn_from_file (file_name, dir_name,
                                                         dispatch_type,
                                                         pack_name);

              if (fcn)
                {
                  function = octave_value (fcn);

                  make_function_of_class (dispatch_type, function);
                }
            }
        }
      else
        {
          // FIXME: check out-of-date status
        }

      if (is_dummy_method (function))
        ::error ("no definition found for method `%s' of class `%s'",
                 get_name ().c_str (), dispatch_type.c_str ());
    }
}

octave_value_list
cdef_method::cdef_method_rep::execute (const octave_value_list& args,
                                       int nargout, bool do_check_access,
                                       const std::string& who)
{
  octave_value_list retval;

  if (do_check_access && ! check_access ())
    {
      gripe_method_access (who, wrap ());

      return retval;
    }

  if (! get ("Abstract").bool_value ())
    {
      check_method ();

      if (! error_state && function.is_defined ())
        {
          retval = execute_ov (function, args, nargout);
        }
    }
  else
    error ("%s: cannot execute abstract method",
           get ("Name").string_value ().c_str ());

  return retval;
}

octave_value_list
cdef_method::cdef_method_rep::execute (const cdef_object& obj,
                                       const octave_value_list& args,
                                       int nargout, bool do_check_access,
                                       const std::string& who)
{
  octave_value_list retval;

  if (do_check_access && ! check_access ())
    {
      gripe_method_access (who, wrap ());

      return retval;
    }

  if (! get ("Abstract").bool_value ())
    {
      check_method ();

      if (! error_state && function.is_defined ())
        {
          octave_value_list new_args;

          new_args.resize (args.length () + 1);

          new_args(0) = to_ov (obj);
          for (int i = 0; i < args.length (); i++)
            new_args(i+1) = args(i);

          retval = execute_ov (function, new_args, nargout);
        }
    }
  else
    error ("%s: cannot execute abstract method",
           get ("Name").string_value ().c_str ());

  return retval;
}

bool
cdef_method::cdef_method_rep::is_constructor (void) const
{
  if (function.is_function())
    return function.function_value ()->is_classdef_constructor ();

  return false;
}

bool
cdef_method::cdef_method_rep::check_access (void) const
{
  cdef_class cls (to_cdef (get ("DefiningClass")));

  if (! error_state)
    return ::check_access (cls, get ("Access"), get_name ());

  return false;
}

octave_value_list
cdef_method::cdef_method_rep::meta_subsref
  (const std::string& type, const std::list<octave_value_list>& idx,
   int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      retval = execute (idx.front (), type.length () > 1 ? 1 : nargout, true);
      break;

    default:
      error ("invalid meta.method indexing");
      break;
    }

  if (! error_state)
    {
      if (type.length () > 1 && idx.size () > 1 && ! retval.empty ())
        retval = retval(0).next_subsref (nargout, type, idx, 1);
    }

  return retval;
}

static cdef_package
lookup_package (const std::string& name)
{
  return cdef_manager::find_package (name);
}

static octave_value_list
package_fromName (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        retval(0) = to_ov (lookup_package (name));
      else
        error ("fromName: invalid package name, expected a string value");
    }
  else
    error ("fromName: invalid number of parameters");

  return retval;
}

static octave_value_list
package_get_classes (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 1 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_classes ();
    }

  return retval;
}

static octave_value_list
package_get_functions (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 0 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_functions ();
    }

  return retval;
}

static octave_value_list
package_get_packages (const octave_value_list& args, int /* nargout */)
{
  octave_value_list retval (1, Matrix ());

  if (args.length () == 0 && args(0).type_name () == "object"
      && args(0).class_name () == "meta.package")
    {
      cdef_package pack (to_cdef (args(0)));

      retval(0) = pack.get_packages ();
    }

  return retval;
}

static octave_value_list
package_getAllPackages (const octave_value_list& /* args */, int /* nargout */)
{
  std::map<std::string, cdef_package> toplevel_packages;

  std::list<std::string> names = load_path::get_all_package_names ();

  toplevel_packages["meta"] = cdef_manager::find_package ("meta", false,
                                                          false);

  for (std::list<std::string>::const_iterator it = names.begin ();
       it != names.end (); ++it)
    toplevel_packages[*it] = cdef_manager::find_package (*it, false, true);

  Cell c (toplevel_packages.size (), 1);

  int i = 0;

  for (std::map<std::string, cdef_package>::const_iterator it =
         toplevel_packages.begin ();
       it != toplevel_packages.end (); ++it)
    c(i++,0) = to_ov (it->second);

  return octave_value_list (octave_value (c));
}

void
cdef_package::cdef_package_rep::install_class (const cdef_class& cls,
                                               const std::string& nm)
{
  class_map[nm] = cls;

  member_count++;
}

void
cdef_package::cdef_package_rep::install_function (const octave_value& fcn,
                                                  const std::string& nm)
{
  function_map[nm] = fcn;
}

void
cdef_package::cdef_package_rep::install_package (const cdef_package& pack,
                                                 const std::string& nm)
{
  package_map[nm] = pack;

  member_count++;
}

template<class T1, class T2>
Cell
map2Cell (const std::map<T1, T2>& m)
{
  Cell retval (1, m.size ());
  int i = 0;

  for (typename std::map<T1, T2>::const_iterator it = m.begin ();
       it != m.end (); ++it, ++i)
    {
      retval(i) = to_ov (it->second);
    }

  return retval;
}

Cell
cdef_package::cdef_package_rep::get_classes (void) const
{ return map2Cell (class_map); }

Cell
cdef_package::cdef_package_rep::get_functions (void) const
{ return map2Cell (function_map); }

Cell
cdef_package::cdef_package_rep::get_packages (void) const
{ return map2Cell (package_map); }

octave_value
cdef_package::cdef_package_rep::find (const std::string& nm)
{
  std::string symbol_name = get_name () + "." + nm;

  return symbol_table::find (symbol_name, octave_value_list (), true, false);
}

octave_value_list
cdef_package::cdef_package_rep::meta_subsref
  (const std::string& type, const std::list<octave_value_list>& idx,
   int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '.':
      if (idx.front ().length () == 1)
        {
          std::string nm = idx.front ()(0).string_value ();

          if (! error_state)
            {
#if DEBUG_TRACE
              std::cerr << "meta.package query: " << nm << std::endl;
#endif

              octave_value o = find (nm);

              if (o.is_defined ())
                {
                  if (o.is_function ())
                    {
                      octave_function* fcn = o.function_value ();

                      if (! error_state)
                        {
                          // NOTE: the case where the package query is the last
                          // part of this subsref index is handled in the parse
                          // tree, because there is some logic to handle magic
                          // "end" that makes it impossible to execute the
                          // function call at this stage.

                          if (type.size () > 1
                              && ! fcn->is_postfix_index_handled (type[1]))
                            {
                              octave_value_list tmp_args;

                              retval = o.do_multi_index_op (nargout,
                                                            tmp_args);
                            }
                          else
                            retval(0) = o;

                          if (type.size () > 1 && idx.size () > 1)
                            retval = retval(0).next_subsref (nargout, type,
                                                             idx, 1);
                        }
                    }
                  else if (type.size () > 1 && idx.size () > 1)
                    retval = o.next_subsref (nargout, type, idx, 1);
                  else
                    retval(0) = o;
                }
              else if (! error_state)
                error ("member `%s' in package `%s' does not exist",
                       nm.c_str (), get_name ().c_str ());
            }
          else
            error ("invalid meta.package indexing, expected a symbol name");
        }
      else
        error ("invalid meta.package indexing");
      break;

    default:
      error ("invalid meta.package indexing");
      break;
    }

  return retval;
}

void
cdef_package::cdef_package_rep::meta_release (void)
{
  // FIXME: Do we really want to unregister the package, as it
  //        could still be referenced by classes or sub-packages?
  //        If the package object is recreated later on, it won't
  //        match the one already referenced by those classes or
  //        sub-packages.

  //cdef_manager::unregister_package (wrap ());
}

cdef_class cdef_class::_meta_class = cdef_class ();
cdef_class cdef_class::_meta_property = cdef_class ();
cdef_class cdef_class::_meta_method = cdef_class ();
cdef_class cdef_class::_meta_package = cdef_class ();

cdef_package cdef_package::_meta = cdef_package ();

void
install_classdef (void)
{
  octave_classdef::register_type ();

  /* bootstrap */
  cdef_class handle = make_class ("handle");
  cdef_class meta_class = cdef_class::_meta_class = make_meta_class ("meta.class", handle);
  handle.set_class (meta_class);
  meta_class.set_class (meta_class);

  /* meta classes */
  cdef_class meta_property = cdef_class::_meta_property = make_meta_class ("meta.property", handle);
  cdef_class meta_method = cdef_class::_meta_method = make_meta_class ("meta.method", handle);
  cdef_class meta_package = cdef_class::_meta_package = make_meta_class ("meta.package", handle);

  cdef_class meta_event = make_meta_class ("meta.event", handle);
  cdef_class meta_dynproperty = make_meta_class ("meta.dynamicproperty", handle);

  /* meta.class properties */
  meta_class.install_property (make_attribute (meta_class, "Abstract"));
  meta_class.install_property (make_attribute (meta_class, "ConstructOnLoad"));
  meta_class.install_property (make_property  (meta_class, "ContainingPackage"));
  meta_class.install_property (make_property  (meta_class, "Description"));
  meta_class.install_property (make_property  (meta_class, "DetailedDescription"));
  meta_class.install_property (make_property  (meta_class, "Events"));
  meta_class.install_property (make_attribute (meta_class, "HandleCompatible"));
  meta_class.install_property (make_attribute (meta_class, "Hidden"));
  meta_class.install_property
      (make_property (meta_class, "InferiorClasses",
                      make_fcn_handle (class_get_inferiorclasses, "meta.class>get.InferiorClasses"),
                      "public", Matrix (), "private"));
  meta_class.install_property
      (make_property  (meta_class, "Methods",
                       make_fcn_handle (class_get_methods, "meta.class>get.Methods"),
                       "public", Matrix (), "private"));
  meta_class.install_property
      (make_property  (meta_class, "MethodList",
                       make_fcn_handle (class_get_methods, "meta.class>get.MethodList"),
                       "public", Matrix (), "private"));
  meta_class.install_property (make_attribute (meta_class, "Name"));
  meta_class.install_property
      (make_property  (meta_class, "Properties",
                       make_fcn_handle (class_get_properties, "meta.class>get.Properties"),
                       "public", Matrix (), "private"));
  meta_class.install_property
      (make_property  (meta_class, "PropertyList",
                       make_fcn_handle (class_get_properties, "meta.class>get.PropertyList"),
                       "public", Matrix (), "private"));
  meta_class.install_property (make_attribute (meta_class, "Sealed"));
  meta_class.install_property
      (make_property (meta_class, "SuperClasses",
                      make_fcn_handle (class_get_superclasses, "meta.class>get.SuperClasses"),
                      "public", Matrix (), "private"));
  meta_class.install_property
      (make_property (meta_class, "SuperClassList",
                      make_fcn_handle (class_get_superclasses, "meta.class>get.SuperClassList"),
                      "public", Matrix (), "private"));
  /* meta.class methods */
  meta_class.install_method (make_method (meta_class, "fromName", class_fromName,
                                          "public", true));
  meta_class.install_method (make_method (meta_class, "fevalStatic", class_fevalStatic,
                                          "public", false));
  meta_class.install_method (make_method (meta_class, "getConstant", class_getConstant,
                                          "public", false));
  meta_class.install_method (make_method (meta_class, "eq", class_eq));
  meta_class.install_method (make_method (meta_class, "ne", class_ne));
  meta_class.install_method (make_method (meta_class, "lt", class_lt));
  meta_class.install_method (make_method (meta_class, "le", class_le));
  meta_class.install_method (make_method (meta_class, "gt", class_gt));
  meta_class.install_method (make_method (meta_class, "ge", class_ge));

  /* meta.method properties */
  meta_method.install_property (make_attribute (meta_method, "Abstract"));
  meta_method.install_property (make_attribute (meta_method, "Access"));
  meta_method.install_property (make_attribute (meta_method, "DefiningClass"));
  meta_method.install_property (make_attribute (meta_method, "Description"));
  meta_method.install_property (make_attribute (meta_method, "DetailedDescription"));
  meta_method.install_property (make_attribute (meta_method, "Hidden"));
  meta_method.install_property (make_attribute (meta_method, "Name"));
  meta_method.install_property (make_attribute (meta_method, "Sealed"));
  meta_method.install_property (make_attribute (meta_method, "Static"));

  /* meta.property properties */
  meta_property.install_property (make_attribute (meta_property, "Name"));
  meta_property.install_property (make_attribute (meta_property, "Description"));
  meta_property.install_property (make_attribute (meta_property, "DetailedDescription"));
  meta_property.install_property (make_attribute (meta_property, "Abstract"));
  meta_property.install_property (make_attribute (meta_property, "Constant"));
  meta_property.install_property (make_attribute (meta_property, "GetAccess"));
  meta_property.install_property (make_attribute (meta_property, "SetAccess"));
  meta_property.install_property (make_attribute (meta_property, "Dependent"));
  meta_property.install_property (make_attribute (meta_property, "Transient"));
  meta_property.install_property (make_attribute (meta_property, "Hidden"));
  meta_property.install_property (make_attribute (meta_property, "GetObservable"));
  meta_property.install_property (make_attribute (meta_property, "SetObservable"));
  meta_property.install_property (make_attribute (meta_property, "GetMethod"));
  meta_property.install_property (make_attribute (meta_property, "SetMethod"));
  meta_property.install_property (make_attribute (meta_property, "DefiningClass"));
  meta_property.install_property
      (make_property (meta_property, "DefaultValue",
                      make_fcn_handle (property_get_defaultvalue, "meta.property>get.DefaultValue"),
                      "public", Matrix (), "private"));
  meta_property.install_property (make_attribute (meta_property, "HasDefault"));
  /* meta.property events */
  // FIXME: add events

  /* handle methods */
  handle.install_method (make_method (handle, "delete", handle_delete));

  /* meta.package properties */
  meta_package.install_property (make_attribute (meta_package, "Name"));
  meta_package.install_property (make_property  (meta_package, "ContainingPackage"));
  meta_package.install_property
      (make_property (meta_package, "ClassList",
                      make_fcn_handle (package_get_classes, "meta.package>get.ClassList"),
                      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "Classes",
                      make_fcn_handle (package_get_classes, "meta.package>get.Classes"),
                      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "FunctionList",
                      make_fcn_handle (package_get_functions, "meta.package>get.FunctionList"),
                      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "Functions",
                      make_fcn_handle (package_get_functions, "meta.package>get.Functions"),
                      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "PackageList",
                      make_fcn_handle (package_get_packages, "meta.package>get.PackageList"),
                      "public", Matrix (), "private"));
  meta_package.install_property
      (make_property (meta_package, "Packages",
                      make_fcn_handle (package_get_packages, "meta.package>get.Packages"),
                      "public", Matrix (), "private"));
  meta_package.install_method (make_method (meta_package, "fromName", package_fromName,
                                            "public", true));
  meta_package.install_method (make_method (meta_package, "getAllPackages", package_getAllPackages,
                                            "public", true));

  /* create "meta" package */
  cdef_package package_meta = cdef_package::_meta = make_package ("meta");
  package_meta.install_class (meta_class,       "class");
  package_meta.install_class (meta_property,    "property");
  package_meta.install_class (meta_method,      "method");
  package_meta.install_class (meta_package,     "package");
  package_meta.install_class (meta_event,       "event");
  package_meta.install_class (meta_dynproperty, "dynproperty");

  /* install built-in classes into the symbol table */
  symbol_table::install_built_in_function
    ("meta.class", octave_value (meta_class.get_constructor_function ()));
  symbol_table::install_built_in_function
    ("meta.method", octave_value (meta_method.get_constructor_function ()));
  symbol_table::install_built_in_function
    ("meta.property", octave_value (meta_property.get_constructor_function ()));
  symbol_table::install_built_in_function
    ("meta.package", octave_value (meta_package.get_constructor_function ()));
  symbol_table::install_built_in_function
    ("meta.event", octave_value (meta_event.get_constructor_function ()));
  symbol_table::install_built_in_function
    ("meta.dynproperty", octave_value (meta_dynproperty.get_constructor_function ()));
}

//----------------------------------------------------------------------------

cdef_manager* cdef_manager::instance = 0;

void
cdef_manager::create_instance (void)
{
  instance = new cdef_manager ();

  if (instance)
    singleton_cleanup_list::add (cleanup_instance);
}

cdef_class
cdef_manager::do_find_class (const std::string& name,
                             bool error_if_not_found, bool load_if_not_found)
{
  std::map<std::string, cdef_class>::iterator it = all_classes.find (name);

  if (it == all_classes.end ())
    {
      if (load_if_not_found)
        {
          octave_value ov_cls;

          size_t pos = name.rfind ('.');

          if (pos == std::string::npos)
            ov_cls = symbol_table::find (name);
          else
            {
              std::string pack_name = name.substr (0, pos);

              cdef_package pack = do_find_package (pack_name, false, true);

              if (pack.ok ())
                ov_cls = pack.find (name.substr (pos+1));
            }

          if (ov_cls.is_defined ())
            it = all_classes.find (name);
        }
    }

  if (it == all_classes.end ())
    {
      if (error_if_not_found)
        error ("class not found: %s", name.c_str ());
    }
  else
    {
      cdef_class cls = it->second;

      if (! cls.is_builtin ())
        cls = lookup_class (cls);

      if (cls.ok ())
        return cls;
      else
        all_classes.erase (it);
    }

  return cdef_class ();
}

octave_function*
cdef_manager::do_find_method_symbol (const std::string& method_name,
                                     const std::string& class_name)
{
  octave_function *retval = 0;

  cdef_class cls = find_class (class_name, false, false);

  if (cls.ok ())
    {
      cdef_method meth = cls.find_method (method_name);

      if (meth.ok ())
        retval = new octave_classdef_meta (meth);
    }

  return retval;
}

cdef_package
cdef_manager::do_find_package (const std::string& name,
                               bool error_if_not_found,
                               bool load_if_not_found)
{
  cdef_package retval;

  std::map<std::string, cdef_package>::const_iterator it
    = all_packages.find (name);

  if (it != all_packages.end ())
    {
      retval = it->second;

      if (! retval.ok ())
        error ("invalid package `%s'", name.c_str ());
    }
  else
    {
      if (load_if_not_found && load_path::find_package (name))
        {
          size_t pos = name.find ('.');

          if (pos == std::string::npos)
            retval = make_package (name, std::string ());
          else
            {
              std::string parent_name = name.substr (0, pos);

              retval = make_package (name, parent_name);
            }
        }
      else if (error_if_not_found)
        error ("unknown package `%s'", name.c_str ());
    }

  return retval;
}

octave_function*
cdef_manager::do_find_package_symbol (const std::string& pack_name)
{
  octave_function* retval = 0;

  cdef_package pack = find_package (pack_name, false);

  if (pack.ok ())
    retval = new octave_classdef_meta (pack);

  return retval;
}

//----------------------------------------------------------------------------

DEFUN (__meta_get_package__, args, , "")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string cname = args(0).string_value ();

      if (! error_state)
        retval = to_ov (lookup_package (cname));
      else
        error ("invalid package name, expected a string value");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__superclass_reference__, args, /* nargout */,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __superclass_reference__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  return octave_value (new octave_classdef_superclass_ref (args));
}

DEFUN (__meta_class_query__, args, /* nargout */,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __meta_class_query__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

#if DEBUG_TRACE
  std::cerr << "__meta_class_query__ ("
            << args(0).string_value () << ")"
            << std::endl;
#endif

  if (args.length () == 1)
    {
      std::string cls = args(0).string_value ();

      if (! error_state)
        retval = to_ov (lookup_class (cls));
      else
        error ("invalid class name, expected a string value");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (metaclass, args, /* nargout */,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} metaclass (obj)\n\
Returns the meta.class object corresponding to the class of @var{obj}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      cdef_object obj = to_cdef (args(0));

      if (! error_state)
        retval = to_ov (obj.get_class ());
      else
        print_usage ();
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
