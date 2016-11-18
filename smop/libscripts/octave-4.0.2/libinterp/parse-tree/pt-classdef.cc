/*

Copyright (C) 2012-2015 John W. Eaton

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

#include "ov-classdef.h"
#include "pt-classdef.h"

// Classdef attribute

void
tree_classdef_attribute::accept (tree_walker& tw)
{
  tw.visit_classdef_attribute (*this);
}

// Classdef attribute_list

tree_classdef_attribute_list::~tree_classdef_attribute_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_classdef_attribute_list::accept (tree_walker& tw)
{
  tw.visit_classdef_attribute_list (*this);
}

// Classdef superclass

void
tree_classdef_superclass::accept (tree_walker& tw)
{
  tw.visit_classdef_superclass (*this);
}

// Classdef superclass_list

tree_classdef_superclass_list::~tree_classdef_superclass_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_classdef_superclass_list::accept (tree_walker& tw)
{
  tw.visit_classdef_superclass_list (*this);
}

// Classdef property

void
tree_classdef_property::accept (tree_walker& tw)
{
  tw.visit_classdef_property (*this);
}

// Classdef property_list

tree_classdef_property_list::~tree_classdef_property_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_classdef_property_list::accept (tree_walker& tw)
{
  tw.visit_classdef_property_list (*this);
}

// Classdef properties_block

void
tree_classdef_properties_block::accept (tree_walker& tw)
{
  tw.visit_classdef_properties_block (*this);
}

// Classdef methods_list

void
tree_classdef_methods_list::accept (tree_walker& tw)
{
  tw.visit_classdef_methods_list (*this);
}

// Classdef methods_block

void
tree_classdef_methods_block::accept (tree_walker& tw)
{
  tw.visit_classdef_methods_block (*this);
}

// Classdef event

void
tree_classdef_event::accept (tree_walker& tw)
{
  tw.visit_classdef_event (*this);
}

// Classdef events_list

tree_classdef_events_list::~tree_classdef_events_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_classdef_events_list::accept (tree_walker& tw)
{
  tw.visit_classdef_events_list (*this);
}

// Classdef events_block

void
tree_classdef_events_block::accept (tree_walker& tw)
{
  tw.visit_classdef_events_block (*this);
}

// Classdef enum

void
tree_classdef_enum::accept (tree_walker& tw)
{
  tw.visit_classdef_enum (*this);
}

// Classdef enum_list

tree_classdef_enum_list::~tree_classdef_enum_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

void
tree_classdef_enum_list::accept (tree_walker& tw)
{
  tw.visit_classdef_enum_list (*this);
}

// Classdef enum_block

void
tree_classdef_enum_block::accept (tree_walker& tw)
{
  tw.visit_classdef_enum_block (*this);
}

// Classdef body

tree_classdef_body::~tree_classdef_body (void)
{
  while (! properties_lst.empty ())
    {
      properties_list_iterator p = properties_lst.begin ();
      delete *p;
      properties_lst.erase (p);
    }

  while (! methods_lst.empty ())
    {
      methods_list_iterator p = methods_lst.begin ();
      delete *p;
      methods_lst.erase (p);
    }

  while (! events_lst.empty ())
    {
      events_list_iterator p = events_lst.begin ();
      delete *p;
      events_lst.erase (p);
    }

  while (! enum_lst.empty ())
    {
      enum_list_iterator p = enum_lst.begin ();
      delete *p;
      enum_lst.erase (p);
    }
}

// Classdef

octave_function*
tree_classdef::make_meta_class (bool is_at_folder)
{
  octave_value retval;
  cdef_class cls = cdef_class::make_meta_class (this, is_at_folder);

  if (cls.ok ())
    return cls.get_constructor_function ();

  return 0;
}

tree_classdef *
tree_classdef::dup (symbol_table::scope_id,
                    symbol_table::context_id) const
{
  // FIXME
  return 0;
}

void
tree_classdef::accept (tree_walker& tw)
{
  tw.visit_classdef (*this);
}
