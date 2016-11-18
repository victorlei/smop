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

#if !defined (octave_tree_classdef_h)
#define octave_tree_classdef_h 1

class octave_value;

class tree_walker;

#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-id.h"

#include "base-list.h"

#include <list>

class tree_classdef_attribute
{
public:

  tree_classdef_attribute (tree_identifier *i = 0, tree_expression *e = 0)
    : id (i), expr (e), neg (false) { }

  tree_classdef_attribute (tree_identifier *i, bool b)
    : id (i), expr (0), neg (b) { }

  ~tree_classdef_attribute (void)
  {
    delete id;
    delete expr;
  }

  tree_identifier *ident (void) { return id; }

  tree_expression *expression (void) { return expr; }

  bool negate (void) { return neg; }

  void accept (tree_walker&);

private:

  tree_identifier *id;
  tree_expression *expr;
  bool neg;

  // No copying!

  tree_classdef_attribute (const tree_classdef_attribute&);

  tree_classdef_attribute& operator = (const tree_classdef_attribute&);
};

class tree_classdef_attribute_list : public octave_base_list<tree_classdef_attribute *>
{
public:

  tree_classdef_attribute_list (void) { }

  tree_classdef_attribute_list (tree_classdef_attribute *a) { append (a); }

  tree_classdef_attribute_list (const octave_base_list<tree_classdef_attribute *>& a)
    : octave_base_list<tree_classdef_attribute *> (a) { }

  ~tree_classdef_attribute_list (void);

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_attribute_list (const tree_classdef_attribute_list&);

  tree_classdef_attribute_list& operator = (const tree_classdef_attribute_list&);
};

class tree_classdef_superclass
{
public:

  tree_classdef_superclass (const std::string& cname)
    : cls_name (cname) { }

  ~tree_classdef_superclass (void) { }

  std::string class_name (void) { return cls_name; }

  void accept (tree_walker&);

private:

  std::string cls_name;

  // No copying!

  tree_classdef_superclass (const tree_classdef_superclass&);

  tree_classdef_superclass& operator = (const tree_classdef_superclass&);
};

class tree_classdef_superclass_list : public octave_base_list<tree_classdef_superclass *>
{
public:

  tree_classdef_superclass_list (void) { }

  tree_classdef_superclass_list (tree_classdef_superclass *sc) { append (sc); }

  tree_classdef_superclass_list (const octave_base_list<tree_classdef_superclass *>& a)
    : octave_base_list<tree_classdef_superclass *> (a) { }

  ~tree_classdef_superclass_list (void);

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_superclass_list (const tree_classdef_superclass_list&);

  tree_classdef_superclass_list& operator = (const tree_classdef_superclass_list&);
};

template <typename T>
class tree_classdef_element : public tree
{
public:

  tree_classdef_element (tree_classdef_attribute_list *a,
                         octave_base_list<T> *elist,
                         octave_comment_list *lc, octave_comment_list *tc,
                         int l = -1, int c = -1)
    : tree (l, c), attr_list (a), elt_list (elist),
      lead_comm (lc), trail_comm (tc)
  { }

  ~tree_classdef_element (void)
  {
    delete attr_list;
    delete elt_list;
    delete lead_comm;
    delete trail_comm;
  }

  tree_classdef_attribute_list *attribute_list (void) { return attr_list; }

  octave_base_list<T> *element_list (void) { return elt_list; }

  octave_comment_list *leading_comment (void) { return lead_comm; }

  octave_comment_list *trailing_comment (void) { return trail_comm; }

  void accept (tree_walker&) { }

private:

  // List of attributes that apply to this class.
  tree_classdef_attribute_list *attr_list;

  // The list of objects contained in this block.
  octave_base_list<T> *elt_list;

  // Comment preceding the token marking the beginning of the block.
  octave_comment_list *lead_comm;

  // Comment preceding END token.
  octave_comment_list *trail_comm;

  // No copying!

  tree_classdef_element (const tree_classdef_element&);

  tree_classdef_element& operator = (const tree_classdef_element&);
};

class tree_classdef_property
{
public:

  tree_classdef_property (tree_identifier *i = 0, tree_expression *e = 0)
    : id (i), expr (e) { }

  ~tree_classdef_property (void)
  {
    delete id;
    delete expr;
  }

  tree_identifier *ident (void) { return id; }

  tree_expression *expression (void) { return expr; }

  void accept (tree_walker&);

private:

  tree_identifier *id;
  tree_expression *expr;

  // No copying!

  tree_classdef_property (const tree_classdef_property&);

  tree_classdef_property& operator = (const tree_classdef_property&);
};

class tree_classdef_property_list : public octave_base_list<tree_classdef_property *>
{
public:

  tree_classdef_property_list (void) { }

  tree_classdef_property_list (tree_classdef_property* p) { append (p); }

  tree_classdef_property_list (const octave_base_list<tree_classdef_property *>& a)
    : octave_base_list<tree_classdef_property *> (a) { }

  ~tree_classdef_property_list (void);

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_property_list (const tree_classdef_property_list&);

  tree_classdef_property_list& operator = (const tree_classdef_property_list&);
};

class tree_classdef_properties_block
  : public tree_classdef_element<tree_classdef_property *>
{
public:

  tree_classdef_properties_block (tree_classdef_attribute_list *a,
                                  tree_classdef_property_list *plist,
                                  octave_comment_list *lc,
                                  octave_comment_list *tc,
                                  int l = -1, int c = -1)
    : tree_classdef_element<tree_classdef_property *> (a, plist, lc, tc, l, c) { }

  ~tree_classdef_properties_block (void) { }

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_properties_block (const tree_classdef_properties_block&);

  tree_classdef_properties_block& operator = (const tree_classdef_properties_block&);
};

class tree_classdef_methods_list : public octave_base_list<octave_value>
{
public:

  tree_classdef_methods_list (void) { }

  tree_classdef_methods_list (const octave_value& f) { append (f); }

  tree_classdef_methods_list (const octave_base_list<octave_value>& a)
    : octave_base_list<octave_value> (a) { }

  ~tree_classdef_methods_list (void) { }

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_methods_list (const tree_classdef_methods_list&);

  tree_classdef_methods_list& operator = (const tree_classdef_methods_list&);
};

class tree_classdef_methods_block : public tree_classdef_element<octave_value>
{
public:

  tree_classdef_methods_block (tree_classdef_attribute_list *a,
                               tree_classdef_methods_list *mlist,
                               octave_comment_list *lc,
                               octave_comment_list *tc, int l = -1, int c = -1)
    : tree_classdef_element<octave_value> (a, mlist, lc, tc, l, c) { }

  ~tree_classdef_methods_block (void) { }

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_methods_block (const tree_classdef_methods_block&);

  tree_classdef_methods_block& operator = (const tree_classdef_methods_block&);
};

class tree_classdef_event
{
public:

  tree_classdef_event (tree_identifier *i = 0) : id (i) { }

  ~tree_classdef_event (void)
  {
    delete id;
  }

  tree_identifier *ident (void) { return id; }

  void accept (tree_walker&);

private:

  tree_identifier *id;

  // No copying!

  tree_classdef_event (const tree_classdef_event&);

  tree_classdef_event& operator = (const tree_classdef_event&);
};

class tree_classdef_events_list : public octave_base_list<tree_classdef_event *>
{
public:

  tree_classdef_events_list (void) { }

  tree_classdef_events_list (tree_classdef_event *e) { append (e); }

  tree_classdef_events_list (const octave_base_list<tree_classdef_event *>& a)
    : octave_base_list<tree_classdef_event *> (a) { }

  ~tree_classdef_events_list (void);

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_events_list (const tree_classdef_events_list&);

  tree_classdef_events_list& operator = (const tree_classdef_events_list&);
};

class tree_classdef_events_block
  : public tree_classdef_element<tree_classdef_event *>
{
public:

  tree_classdef_events_block (tree_classdef_attribute_list *a,
                              tree_classdef_events_list *elist,
                              octave_comment_list *lc,
                              octave_comment_list *tc, int l = -1, int c = -1)
    : tree_classdef_element<tree_classdef_event *> (a, elist, lc, tc, l, c) { }

  ~tree_classdef_events_block (void) { }

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_events_block (const tree_classdef_events_block&);

  tree_classdef_events_block& operator = (const tree_classdef_events_block&);
};

class tree_classdef_enum
{
public:

  tree_classdef_enum (void) : id (0), expr (0) { }

  tree_classdef_enum (tree_identifier *i, tree_expression *e)
    : id (i), expr (e) { }

  ~tree_classdef_enum (void)
  {
    delete id;
    delete expr;
  }

  tree_identifier *ident (void) { return id; }

  tree_expression *expression (void) { return expr; }

  void accept (tree_walker&);

private:

  tree_identifier *id;
  tree_expression *expr;

  // No copying!

  tree_classdef_enum (const tree_classdef_enum&);

  tree_classdef_enum& operator = (const tree_classdef_enum&);
};

class tree_classdef_enum_list : public octave_base_list<tree_classdef_enum *>
{
public:

  tree_classdef_enum_list (void) { }

  tree_classdef_enum_list (tree_classdef_enum *e) { append (e); }

  tree_classdef_enum_list (const octave_base_list<tree_classdef_enum *>& a)
    : octave_base_list<tree_classdef_enum *> (a) { }

  ~tree_classdef_enum_list (void);

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_enum_list (const tree_classdef_enum_list&);

  tree_classdef_enum_list& operator = (const tree_classdef_enum_list&);
};

class tree_classdef_enum_block
  : public tree_classdef_element<tree_classdef_enum *>
{
public:

  tree_classdef_enum_block (tree_classdef_attribute_list *a,
                            tree_classdef_enum_list *elist,
                            octave_comment_list *lc,
                            octave_comment_list *tc, int l = -1, int c = -1)
    : tree_classdef_element<tree_classdef_enum *> (a, elist, lc, tc, l, c) { }

  ~tree_classdef_enum_block (void) { }

  void accept (tree_walker&);

private:

  // No copying!

  tree_classdef_enum_block (const tree_classdef_enum_block&);

  tree_classdef_enum_block& operator = (const tree_classdef_enum_block&);
};

class tree_classdef_body
{
public:

  typedef std::list<tree_classdef_properties_block *>::iterator properties_list_iterator;
  typedef std::list<tree_classdef_properties_block *>::const_iterator properties_list_const_iterator;

  typedef std::list<tree_classdef_methods_block *>::iterator methods_list_iterator;
  typedef std::list<tree_classdef_methods_block *>::const_iterator methods_list_const_iterator;

  typedef std::list<tree_classdef_events_block *>::iterator events_list_iterator;
  typedef std::list<tree_classdef_events_block *>::const_iterator events_list_const_iterator;

  typedef std::list<tree_classdef_enum_block *>::iterator enum_list_iterator;
  typedef std::list<tree_classdef_enum_block *>::const_iterator enum_list_const_iterator;

  tree_classdef_body (void)
    : properties_lst (), methods_lst (), events_lst (), enum_lst () { }

  tree_classdef_body (tree_classdef_properties_block *pb)
    : properties_lst (), methods_lst (), events_lst (), enum_lst ()
  {
    append (pb);
  }

  tree_classdef_body (tree_classdef_methods_block *mb)
    : properties_lst (), methods_lst (), events_lst (), enum_lst ()
  {
    append (mb);
  }

  tree_classdef_body (tree_classdef_events_block *evb)
    : properties_lst (), methods_lst (), events_lst (), enum_lst ()
  {
    append (evb);
  }

  tree_classdef_body (tree_classdef_enum_block *enb)
    : properties_lst (), methods_lst (), events_lst (), enum_lst ()
  {
    append (enb);
  }

  ~tree_classdef_body (void);

  void append (tree_classdef_properties_block *pb)
  {
    properties_lst.push_back (pb);
  }

  void append (tree_classdef_methods_block *mb)
  {
    methods_lst.push_back (mb);
  }

  void append (tree_classdef_events_block *evb)
  {
    events_lst.push_back (evb);
  }

  void append (tree_classdef_enum_block *enb)
  {
    enum_lst.push_back (enb);
  }

  std::list<tree_classdef_properties_block *> properties_list (void)
  {
    return properties_lst;
  }

  std::list<tree_classdef_methods_block *> methods_list (void)
  {
    return methods_lst;
  }

  std::list<tree_classdef_events_block *> events_list (void)
  {
    return events_lst;
  }

  std::list<tree_classdef_enum_block *> enum_list (void)
  {
    return enum_lst;
  }

  void accept (tree_walker&);

private:

  std::list<tree_classdef_properties_block *> properties_lst;

  std::list<tree_classdef_methods_block *> methods_lst;

  std::list<tree_classdef_events_block *> events_lst;

  std::list<tree_classdef_enum_block *> enum_lst;

  // No copying!

  tree_classdef_body (const tree_classdef_body&);

  tree_classdef_body& operator = (const tree_classdef_body&);
};

// Classdef definition.

class tree_classdef : public tree_command
{
public:

  tree_classdef (tree_classdef_attribute_list *a, tree_identifier *i,
                 tree_classdef_superclass_list *sc,
                 tree_classdef_body *b, octave_comment_list *lc,
                 octave_comment_list *tc,
                 const std::string& pn = std::string (), int l = -1,
                 int c = -1)
    : tree_command (l, c), attr_list (a), id (i),
      supclass_list (sc), element_list (b), lead_comm (lc), trail_comm (tc),
      pack_name (pn) { }

  ~tree_classdef (void)
  {
    delete attr_list;
    delete id;
    delete supclass_list;
    delete element_list;
    delete lead_comm;
    delete trail_comm;
  }

  tree_classdef_attribute_list *attribute_list (void) { return attr_list; }

  tree_identifier *ident (void) { return id; }

  tree_classdef_superclass_list *superclass_list (void) { return supclass_list; }

  tree_classdef_body *body (void) { return element_list; }

  octave_comment_list *leading_comment (void) { return lead_comm; }
  octave_comment_list *trailing_comment (void) { return trail_comm; }

  const std::string& package_name (void) const { return pack_name; }

  octave_function* make_meta_class (bool is_at_folder = false);

  tree_classdef *dup (symbol_table::scope_id scope,
                      symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  tree_classdef_attribute_list *attr_list;

  tree_identifier *id;

  tree_classdef_superclass_list *supclass_list;

  tree_classdef_body *element_list;

  octave_comment_list *lead_comm;
  octave_comment_list *trail_comm;

  std::string pack_name;

  // No copying!

  tree_classdef (const tree_classdef&);

  tree_classdef& operator = (const tree_classdef&);
};

#endif
