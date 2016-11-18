/*

Copyright (C) 2007-2015 John W. Eaton

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

#if !defined (octave_graphics_h)
#define octave_graphics_h 1

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include "caseless-str.h"

#include "gripes.h"
#include "oct-handle.h"
#include "oct-map.h"
#include "oct-mutex.h"
#include "oct-refcount.h"
#include "ov.h"
#include "txt-eng-ft.h"

// FIXME: maybe this should be a configure option?
// Matlab defaults to "Helvetica", but that causes problems for many
// gnuplot users.
#if !defined (OCTAVE_DEFAULT_FONTNAME)
#define OCTAVE_DEFAULT_FONTNAME "*"
#endif

typedef octave_handle graphics_handle;

// ---------------------------------------------------------------------

class base_scaler
{
public:
  base_scaler (void) { }

  virtual ~base_scaler (void) { }

  virtual Matrix scale (const Matrix& m) const
  {
    error ("invalid axis scale");
    return m;
  }

  virtual NDArray scale (const NDArray& m) const
  {
    error ("invalid axis scale");
    return m;
  }

  virtual double scale (double d) const
  {
    error ("invalid axis scale");
    return d;
  }

  virtual double unscale (double d) const
  {
    error ("invalid axis scale");
    return d;
  }

  virtual base_scaler* clone () const
  { return new base_scaler (); }

  virtual bool is_linear (void) const
  { return false; }
};

class lin_scaler : public base_scaler
{
public:
  lin_scaler (void) { }

  Matrix scale (const Matrix& m) const { return m; }

  NDArray scale (const NDArray& m) const { return m; }

  double scale (double d) const { return d; }

  double unscale (double d) const { return d; }

  base_scaler* clone (void) const { return new lin_scaler (); }

  bool is_linear (void) const { return true; }
};

class log_scaler : public base_scaler
{
public:
  log_scaler (void) { }

  Matrix scale (const Matrix& m) const
  {
    Matrix retval (m.rows (), m.cols ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  NDArray scale (const NDArray& m) const
  {
    NDArray retval (m.dims ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  double scale (double d) const
  { return log10 (d); }

  double unscale (double d) const
  { return pow (10.0, d); }

  base_scaler* clone (void) const
  { return new log_scaler (); }

private:
  void do_scale (const double *src, double *dest, int n) const
  {
    for (int i = 0; i < n; i++)
      dest[i] = log10 (src[i]);
  }
};

class neg_log_scaler : public base_scaler
{
public:
  neg_log_scaler (void) { }

  Matrix scale (const Matrix& m) const
  {
    Matrix retval (m.rows (), m.cols ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  NDArray scale (const NDArray& m) const
  {
    NDArray retval (m.dims ());

    do_scale (m.data (), retval.fortran_vec (), m.numel ());

    return retval;
  }

  double scale (double d) const
  { return -log10 (-d); }

  double unscale (double d) const
  { return -pow (10.0, -d); }

  base_scaler* clone (void) const
  { return new neg_log_scaler (); }

private:
  void do_scale (const double *src, double *dest, int n) const
  {
    for (int i = 0; i < n; i++)
      dest[i] = -log10 (-src[i]);
  }
};

class scaler
{
public:
  scaler (void) : rep (new base_scaler ()) { }

  scaler (const scaler& s) : rep (s.rep->clone ()) { }

  scaler (const std::string& s)
    : rep (s == "log"
           ? new log_scaler ()
           : (s == "neglog" ? new neg_log_scaler ()
              : (s == "linear" ? new lin_scaler () : new base_scaler ())))
  { }

  ~scaler (void) { delete rep; }

  Matrix scale (const Matrix& m) const
  { return rep->scale (m); }

  NDArray scale (const NDArray& m) const
  { return rep->scale (m); }

  double scale (double d) const
  { return rep->scale (d); }

  double unscale (double d) const
  { return rep->unscale (d); }

  bool is_linear (void) const
  { return rep->is_linear (); }

  scaler& operator = (const scaler& s)
  {
    if (rep)
      {
        delete rep;
        rep = 0;
      }

    rep = s.rep->clone ();

    return *this;
  }

  scaler& operator = (const std::string& s)
  {
    if (rep)
      {
        delete rep;
        rep = 0;
      }

    if (s == "log")
      rep = new log_scaler ();
    else if (s == "neglog")
      rep = new neg_log_scaler ();
    else if (s == "linear")
      rep = new lin_scaler ();
    else
      rep = new base_scaler ();

    return *this;
  }

private:
  base_scaler *rep;
};

// ---------------------------------------------------------------------

class property;

enum listener_mode { POSTSET, PERSISTENT, PREDELETE };

class base_property
{
public:
  friend class property;

public:
  base_property (void)
    : id (-1), count (1), name (), parent (), hidden (), listeners ()
  { }

  base_property (const std::string& s, const graphics_handle& h)
    : id (-1), count (1), name (s), parent (h), hidden (false), listeners ()
  { }

  base_property (const base_property& p)
    : id (-1), count (1), name (p.name), parent (p.parent),
      hidden (p.hidden), listeners ()
  { }

  virtual ~base_property (void) { }

  bool ok (void) const { return parent.ok (); }

  std::string get_name (void) const { return name; }

  void set_name (const std::string& s) { name = s; }

  graphics_handle get_parent (void) const { return parent; }

  void set_parent (const graphics_handle& h) { parent = h; }

  bool is_hidden (void) const { return hidden; }

  void set_hidden (bool flag) { hidden = flag; }

  virtual bool is_radio (void) const { return false; }

  int get_id (void) const { return id; }

  void set_id (int d) { id = d; }

  // Sets property value, notifies graphics toolkit.
  // If do_run is true, runs associated listeners.
  OCTINTERP_API bool set (const octave_value& v, bool do_run = true,
                          bool do_notify_toolkit = true);

  virtual octave_value get (void) const
  {
    error ("get: invalid property \"%s\"", name.c_str ());
    return octave_value ();
  }


  virtual std::string values_as_string (void) const
  {
    error ("values_as_string: invalid property \"%s\"", name.c_str ());
    return std::string ();
  }

  virtual Cell values_as_cell (void) const
  {
    error ("values_as_cell: invalid property \"%s\"", name.c_str ());
    return Cell ();
  }

  base_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  void add_listener (const octave_value& v, listener_mode mode = POSTSET)
  {
    octave_value_list& l = listeners[mode];
    l.resize (l.length () + 1, v);
  }

  void delete_listener (const octave_value& v = octave_value (),
                        listener_mode mode = POSTSET)
  {
    octave_value_list& l = listeners[mode];

    if (v.is_defined ())
      {
        bool found = false;
        int i;

        for (i = 0; i < l.length (); i++)
          {
            if (v.internal_rep () == l(i).internal_rep ())
              {
                found = true;
                break;
              }
          }
        if (found)
          {
            for (int j = i; j < l.length () - 1; j++)
              l(j) = l(j + 1);

            l.resize (l.length () - 1);
          }
      }
    else
      {
        if (mode == PERSISTENT)
          l.resize (0);
        else
          {
            octave_value_list lnew (0);
            octave_value_list& lp = listeners[PERSISTENT];
            for (int i = l.length () - 1; i >= 0 ; i--)
              {
                for (int j = 0; j < lp.length (); j++)
                  {
                    if (l(i).internal_rep () == lp(j).internal_rep ())
                      {
                        lnew.resize (lnew.length () + 1, l(i));
                        break;
                      }
                  }
              }
            l = lnew;
          }
      }

  }

  OCTINTERP_API void run_listeners (listener_mode mode = POSTSET);

  virtual base_property* clone (void) const
  { return new base_property (*this); }

protected:
  virtual bool do_set (const octave_value&)
  {
    error ("set: invalid property \"%s\"", name.c_str ());
    return false;
  }

private:
  typedef std::map<listener_mode, octave_value_list> listener_map;
  typedef std::map<listener_mode, octave_value_list>::iterator
    listener_map_iterator;
  typedef std::map<listener_mode, octave_value_list>::const_iterator
    listener_map_const_iterator;

private:
  int id;
  octave_refcount<int> count;
  std::string name;
  graphics_handle parent;
  bool hidden;
  listener_map listeners;
};

// ---------------------------------------------------------------------

class string_property : public base_property
{
public:
  string_property (const std::string& s, const graphics_handle& h,
                   const std::string& val = "")
    : base_property (s, h), str (val) { }

  string_property (const string_property& p)
    : base_property (p), str (p.str) { }

  octave_value get (void) const
  { return octave_value (str); }

  std::string string_value (void) const { return str; }

  string_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new string_property (*this); }

protected:
  bool do_set (const octave_value& val)
  {
    if (val.is_string ())
      {
        std::string new_str = val.string_value ();

        if (new_str != str)
          {
            str = new_str;
            return true;
          }
      }
    else
      error ("set: invalid string property value for \"%s\"",
             get_name ().c_str ());
    return false;
  }

private:
  std::string str;
};

// ---------------------------------------------------------------------

class string_array_property : public base_property
{
public:
  enum desired_enum { string_t, cell_t };

  string_array_property (const std::string& s, const graphics_handle& h,
                         const std::string& val = "", const char& sep = '|',
                         const desired_enum& typ = string_t)
    : base_property (s, h), desired_type (typ), separator (sep), str ()
  {
    size_t pos = 0;

    while (true)
      {
        size_t new_pos = val.find_first_of (separator, pos);

        if (new_pos == std::string::npos)
          {
            str.append (val.substr (pos));
            break;
          }
        else
          str.append (val.substr (pos, new_pos - pos));

        pos = new_pos + 1;
      }
  }

  string_array_property (const std::string& s, const graphics_handle& h,
                         const Cell& c, const char& sep = '|',
                         const desired_enum& typ = string_t)
    : base_property (s, h), desired_type (typ), separator (sep), str ()
  {
    if (c.is_cellstr ())
      {
        string_vector strings (c.numel ());

        for (octave_idx_type i = 0; i < c.numel (); i++)
          strings[i] = c(i).string_value ();

        str = strings;
      }
    else
      error ("set: invalid order property value for \"%s\"",
             get_name ().c_str ());
  }

  string_array_property (const string_array_property& p)
    : base_property (p), desired_type (p.desired_type),
      separator (p.separator), str (p.str) { }

  octave_value get (void) const
  {
    if (desired_type == string_t)
      return octave_value (string_value ());
    else
      return octave_value (cell_value ());
  }

  std::string string_value (void) const
  {
    std::string s;

    for (octave_idx_type i = 0; i < str.length (); i++)
      {
        s += str[i];
        if (i != str.length () - 1)
          s += separator;
      }

    return s;
  }

  Cell cell_value (void) const {return Cell (str);}

  string_vector string_vector_value (void) const { return str; }

  string_array_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const
  { return new string_array_property (*this); }

protected:
  bool do_set (const octave_value& val)
  {
    if (val.is_string () && val.rows () == 1)
      {
        bool replace = false;
        std::string new_str = val.string_value ();
        string_vector strings;
        size_t pos = 0;

        // Split single string on delimiter (usually '|')
        while (pos != std::string::npos)
          {
            size_t new_pos = new_str.find_first_of (separator, pos);

            if (new_pos == std::string::npos)
              {
                strings.append (new_str.substr (pos));
                break;
              }
            else
              strings.append (new_str.substr (pos, new_pos - pos));

            pos = new_pos + 1;
          }

        if (str.numel () == strings.numel ())
          {
            for (octave_idx_type i = 0; i < str.numel (); i++)
              if (strings[i] != str[i])
                {
                  replace = true;
                  break;
                }
          }
        else
          replace = true;

        desired_type = string_t;

        if (replace)
          {
            str = strings;
            return true;
          }
      }
    else if (val.is_string ())  // multi-row character matrix
      {
        bool replace = false;
        charMatrix chm = val.char_matrix_value ();
        octave_idx_type nel = chm.rows ();
        string_vector strings (nel);

        if (nel != str.numel ())
          replace = true;
        for (octave_idx_type i = 0; i < nel; i++)
          {
            strings[i] = chm.row_as_string (i);
            if (!replace && strings[i] != str[i])
              replace = true;
          }

        desired_type = string_t;

        if (replace)
          {
            str = strings;
            return true;
          }
      }
    else if (val.is_cellstr ())
      {
        bool replace = false;
        Cell new_cell = val.cell_value ();

        string_vector strings = new_cell.cellstr_value ();

        octave_idx_type nel = strings.length ();

        if (nel != str.length ())
          replace = true;
        else
          {
            for (octave_idx_type i = 0; i < nel; i++)
              {
                if (strings[i] != str[i])
                  {
                    replace = true;
                    break;
                  }
              }
          }

        desired_type = cell_t;

        if (replace)
          {
            str = strings;
            return true;
          }
      }
    else
      error ("set: invalid string property value for \"%s\"",
             get_name ().c_str ());
    return false;
  }

private:
  desired_enum desired_type;
  char separator;
  string_vector str;
};

// ---------------------------------------------------------------------

class text_label_property : public base_property
{
public:
  enum type { char_t, cellstr_t };

  text_label_property (const std::string& s, const graphics_handle& h,
                       const std::string& val = "")
    : base_property (s, h), value (val), stored_type (char_t)
  { }

  text_label_property (const std::string& s, const graphics_handle& h,
                       const NDArray& nda)
    : base_property (s, h), stored_type (char_t)
  {
    octave_idx_type nel = nda.numel ();

    value.resize (nel);

    for (octave_idx_type i = 0; i < nel; i++)
      {
        std::ostringstream buf;
        buf << nda(i);
        value[i] = buf.str ();
      }
  }

  text_label_property (const std::string& s, const graphics_handle& h,
                       const Cell& c)
    : base_property (s, h), stored_type (cellstr_t)
  {
    octave_idx_type nel = c.numel ();

    value.resize (nel);

    for (octave_idx_type i = 0; i < nel; i++)
      {
        octave_value tmp = c(i);

        if (tmp.is_string ())
          value[i] = c(i).string_value ();
        else
          {
            double d = c(i).double_value ();

            if (! error_state)
              {
                std::ostringstream buf;
                buf << d;
                value[i] = buf.str ();
              }
            else
              break;
          }
      }
  }

  text_label_property (const text_label_property& p)
    : base_property (p), value (p.value), stored_type (p.stored_type)
  { }

  bool empty (void) const
  {
    octave_value tmp = get ();
    return tmp.is_empty ();
  }

  octave_value get (void) const
  {
    if (stored_type == char_t)
      return octave_value (char_value ());
    else
      return octave_value (cell_value ());
  }

  std::string string_value (void) const
  {
    return value.empty () ? std::string () : value[0];
  }

  string_vector string_vector_value (void) const { return value; }

  charMatrix char_value (void) const { return charMatrix (value, ' '); }

  Cell cell_value (void) const {return Cell (value); }

  text_label_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new text_label_property (*this); }

protected:

  bool do_set (const octave_value& val)
  {
    if (val.is_string ())
      {
        value = val.all_strings ();

        stored_type = char_t;
      }
    else if (val.is_cell ())
      {
        Cell c = val.cell_value ();

        octave_idx_type nel = c.numel ();

        value.resize (nel);

        for (octave_idx_type i = 0; i < nel; i++)
          {
            octave_value tmp = c(i);

            if (tmp.is_string ())
              value[i] = c(i).string_value ();
            else
              {
                double d = c(i).double_value ();

                if (! error_state)
                  {
                    std::ostringstream buf;
                    buf << d;
                    value[i] = buf.str ();
                  }
                else
                  return false;
              }
          }

        stored_type = cellstr_t;
      }
    else
      {
        NDArray nda = val.array_value ();

        if (! error_state)
          {
            octave_idx_type nel = nda.numel ();

            value.resize (nel);

            for (octave_idx_type i = 0; i < nel; i++)
              {
                std::ostringstream buf;
                buf << nda(i);
                value[i] = buf.str ();
              }

            stored_type = char_t;
          }
        else
          {
            error ("set: invalid string property value for \"%s\"",
                   get_name ().c_str ());

            return false;
          }
      }

    return true;
  }

private:
  string_vector value;
  type stored_type;
};

// ---------------------------------------------------------------------

class radio_values
{
public:
  OCTINTERP_API radio_values (const std::string& opt_string = std::string ());

  radio_values (const radio_values& a)
    : default_val (a.default_val), possible_vals (a.possible_vals) { }

  radio_values& operator = (const radio_values& a)
  {
    if (&a != this)
      {
        default_val = a.default_val;
        possible_vals = a.possible_vals;
      }

    return *this;
  }

  std::string default_value (void) const { return default_val; }

  bool validate (const std::string& val, std::string& match)
  {
    bool retval = true;

    if (! contains (val, match))
      {
        error ("invalid value = %s", val.c_str ());
        retval = false;
      }

    return retval;
  }

  bool contains (const std::string& val, std::string& match)
  {
    size_t k = 0;

    size_t len = val.length ();

    std::string first_match;

    for (std::set<caseless_str>::const_iterator p = possible_vals.begin ();
         p != possible_vals.end (); p++)
      {
        if (p->compare (val, len))
          {
            if (len == p->length ())
              {
                // We found a full match (consider the case of val ==
                // "replace" with possible values "replace" and
                // "replacechildren").  Any other matches are
                // irrelevant, so set match and return now.

                match = *p;
                return true;
              }
            else
              {
                if (k == 0)
                  first_match = *p;

                k++;
              }
          }
      }

    if (k == 1)
      {
        match = first_match;
        return true;
      }
    else
      return false;
  }

  std::string values_as_string (void) const;

  Cell values_as_cell (void) const;

  octave_idx_type nelem (void) const { return possible_vals.size (); }

private:
  // Might also want to cache
  std::string default_val;
  std::set<caseless_str> possible_vals;
};

class radio_property : public base_property
{
public:
  radio_property (const std::string& nm, const graphics_handle& h,
                  const radio_values& v = radio_values ())
    : base_property (nm, h),
      vals (v), current_val (v.default_value ()) { }

  radio_property (const std::string& nm, const graphics_handle& h,
                  const std::string& v)
    : base_property (nm, h),
      vals (v), current_val (vals.default_value ()) { }

  radio_property (const std::string& nm, const graphics_handle& h,
                  const radio_values& v, const std::string& def)
    : base_property (nm, h),
      vals (v), current_val (def) { }

  radio_property (const radio_property& p)
    : base_property (p), vals (p.vals), current_val (p.current_val) { }

  octave_value get (void) const { return octave_value (current_val); }

  const std::string& current_value (void) const { return current_val; }

  std::string values_as_string (void) const { return vals.values_as_string (); }

  Cell values_as_cell (void) const { return vals.values_as_cell (); }

  bool is (const caseless_str& v) const
  { return v.compare (current_val); }

  bool is_radio (void) const { return true; }

  radio_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new radio_property (*this); }

protected:
  bool do_set (const octave_value& newval)
  {
    if (newval.is_string ())
      {
        std::string s = newval.string_value ();

        std::string match;

        if (vals.validate (s, match))
          {
            if (match != current_val)
              {
                if (s.length () != match.length ())
                  warning_with_id ("Octave:abbreviated-property-match",
                                   "%s: allowing %s to match %s value %s",
                                   "set", s.c_str (), get_name ().c_str (),
                                   match.c_str ());
                current_val = match;
                return true;
              }
          }
        else
          error ("set: invalid value for radio property \"%s\" (value = %s)",
                 get_name ().c_str (), s.c_str ());
      }
    else
      error ("set: invalid value for radio property \"%s\"",
             get_name ().c_str ());
    return false;
  }

private:
  radio_values vals;
  std::string current_val;
};

// ---------------------------------------------------------------------

class color_values
{
public:
  color_values (double r = 0, double g = 0, double b = 1)
    : xrgb (1, 3)
  {
    xrgb(0) = r;
    xrgb(1) = g;
    xrgb(2) = b;

    validate ();
  }

  color_values (const std::string& str)
    : xrgb (1, 3)
  {
    if (! str2rgb (str))
      error ("invalid color specification: %s", str.c_str ());
  }

  color_values (const color_values& c)
    : xrgb (c.xrgb)
  { }

  color_values& operator = (const color_values& c)
  {
    if (&c != this)
      xrgb = c.xrgb;

    return *this;
  }

  bool operator == (const color_values& c) const
  {
    return (xrgb(0) == c.xrgb(0)
            && xrgb(1) == c.xrgb(1)
            && xrgb(2) == c.xrgb(2));
  }

  bool operator != (const color_values& c) const
  { return ! (*this == c); }

  Matrix rgb (void) const { return xrgb; }

  operator octave_value (void) const { return xrgb; }

  void validate (void) const
  {
    for (int i = 0; i < 3; i++)
      {
        if (xrgb(i) < 0 ||  xrgb(i) > 1)
          {
            error ("invalid RGB color specification");
            break;
          }
      }
  }

private:
  Matrix xrgb;

  OCTINTERP_API bool str2rgb (const std::string& str);
};

class color_property : public base_property
{
public:
  color_property (const color_values& c, const radio_values& v)
    : base_property ("", graphics_handle ()),
      current_type (color_t), color_val (c), radio_val (v),
      current_val (v.default_value ())
  { }

  color_property (const radio_values& v, const color_values& c)
    : base_property ("", graphics_handle ()),
      current_type (radio_t), color_val (c), radio_val (v),
      current_val (v.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const color_values& c = color_values (),
                  const radio_values& v = radio_values ())
    : base_property (nm, h),
      current_type (color_t), color_val (c), radio_val (v),
      current_val (v.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const radio_values& v)
    : base_property (nm, h),
      current_type (radio_t), color_val (color_values ()), radio_val (v),
      current_val (v.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const std::string& v)
    : base_property (nm, h),
      current_type (radio_t), color_val (color_values ()), radio_val (v),
      current_val (radio_val.default_value ())
  { }

  color_property (const std::string& nm, const graphics_handle& h,
                  const color_property& v)
    : base_property (nm, h),
      current_type (v.current_type), color_val (v.color_val),
      radio_val (v.radio_val), current_val (v.current_val)
  { }

  color_property (const color_property& p)
    : base_property (p), current_type (p.current_type),
      color_val (p.color_val), radio_val (p.radio_val),
      current_val (p.current_val) { }

  octave_value get (void) const
  {
    if (current_type == color_t)
      return color_val.rgb ();

    return current_val;
  }

  bool is_rgb (void) const { return (current_type == color_t); }

  bool is_radio (void) const { return (current_type == radio_t); }

  bool is (const std::string& v) const
  { return (is_radio () && current_val == v); }

  Matrix rgb (void) const
  {
    if (current_type != color_t)
      error ("color has no rgb value");

    return color_val.rgb ();
  }

  const std::string& current_value (void) const
  {
    if (current_type != radio_t)
      error ("color has no radio value");

    return current_val;
  }

  color_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  operator octave_value (void) const { return get (); }

  base_property* clone (void) const { return new color_property (*this); }

  std::string values_as_string (void) const
  { return radio_val.values_as_string (); }

  Cell values_as_cell (void) const { return radio_val.values_as_cell (); }

protected:
  OCTINTERP_API bool do_set (const octave_value& newval);

private:
  enum current_enum { color_t, radio_t } current_type;
  color_values color_val;
  radio_values radio_val;
  std::string current_val;
};

// ---------------------------------------------------------------------

class double_property : public base_property
{
public:
  double_property (const std::string& nm, const graphics_handle& h,
                   double d = 0)
    : base_property (nm, h),
      current_val (d) { }

  double_property (const double_property& p)
    : base_property (p), current_val (p.current_val) { }

  octave_value get (void) const { return octave_value (current_val); }

  double double_value (void) const { return current_val; }

  double_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new double_property (*this); }

protected:
  bool do_set (const octave_value& v)
  {
    if (v.is_scalar_type () && v.is_real_type ())
      {
        double new_val = v.double_value ();

        if (new_val != current_val)
          {
            current_val = new_val;
            return true;
          }
      }
    else
      error ("set: invalid value for double property \"%s\"",
             get_name ().c_str ());
    return false;
  }

private:
  double current_val;
};

// ---------------------------------------------------------------------

class double_radio_property : public base_property
{
public:
  double_radio_property (double d, const radio_values& v)
    : base_property ("", graphics_handle ()),
      current_type (double_t), dval (d), radio_val (v),
      current_val (v.default_value ())
  { }

  double_radio_property (const std::string& nm, const graphics_handle& h,
                         const std::string& v)
    : base_property (nm, h),
      current_type (radio_t), dval (0), radio_val (v),
      current_val (radio_val.default_value ())
  { }

  double_radio_property (const std::string& nm, const graphics_handle& h,
                         const double_radio_property& v)
    : base_property (nm, h),
      current_type (v.current_type), dval (v.dval),
      radio_val (v.radio_val), current_val (v.current_val)
  { }

  double_radio_property (const double_radio_property& p)
    : base_property (p), current_type (p.current_type),
      dval (p.dval), radio_val (p.radio_val),
      current_val (p.current_val) { }

  octave_value get (void) const
  {
    if (current_type == double_t)
      return dval;

    return current_val;
  }

  bool is_double (void) const { return (current_type == double_t); }

  bool is_radio (void) const { return (current_type == radio_t); }

  bool is (const std::string& v) const
  { return (is_radio () && current_val == v); }

  double double_value (void) const
  {
    if (current_type != double_t)
      error ("%s: property has no double", get_name ().c_str ());

    return dval;
  }

  const std::string& current_value (void) const
  {
    if (current_type != radio_t)
      error ("%s: property has no radio value");

    return current_val;
  }

  double_radio_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  operator octave_value (void) const { return get (); }

  base_property* clone (void) const
  { return new double_radio_property (*this); }

protected:
  OCTINTERP_API bool do_set (const octave_value& v);

private:
  enum current_enum { double_t, radio_t } current_type;
  double dval;
  radio_values radio_val;
  std::string current_val;
};

// ---------------------------------------------------------------------

class array_property : public base_property
{
public:
  array_property (void)
    : base_property ("", graphics_handle ()), data (Matrix ()),
      xmin (), xmax (), xminp (), xmaxp (),
      type_constraints (), size_constraints ()
  {
    get_data_limits ();
  }

  array_property (const std::string& nm, const graphics_handle& h,
                  const octave_value& m)
    : base_property (nm, h), data (m.is_sparse_type () ? m.full_value () : m),
      xmin (), xmax (), xminp (), xmaxp (),
      type_constraints (), size_constraints ()
  {
    get_data_limits ();
  }

  // This copy constructor is only intended to be used
  // internally to access min/max values; no need to
  // copy constraints.
  array_property (const array_property& p)
    : base_property (p), data (p.data),
      xmin (p.xmin), xmax (p.xmax), xminp (p.xminp), xmaxp (p.xmaxp),
      type_constraints (), size_constraints ()
  { }

  octave_value get (void) const { return data; }

  void add_constraint (const std::string& type)
  { type_constraints.insert (type); }

  void add_constraint (const dim_vector& dims)
  { size_constraints.push_back (dims); }

  double min_val (void) const { return xmin; }
  double max_val (void) const { return xmax; }
  double min_pos (void) const { return xminp; }
  double max_neg (void) const { return xmaxp; }

  Matrix get_limits (void) const
  {
    Matrix m (1, 4);

    m(0) = min_val ();
    m(1) = max_val ();
    m(2) = min_pos ();
    m(3) = max_neg ();

    return m;
  }

  array_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const
  {
    array_property *p = new array_property (*this);

    p->type_constraints = type_constraints;
    p->size_constraints = size_constraints;

    return p;
  }

protected:
  bool do_set (const octave_value& v)
  {
    octave_value tmp = v.is_sparse_type () ? v.full_value () : v;

    if (validate (tmp))
      {
        // FIXME: should we check for actual data change?
        if (! is_equal (tmp))
          {
            data = tmp;

            get_data_limits ();

            return true;
          }
      }
    else
      error ("invalid value for array property \"%s\"",
             get_name ().c_str ());

    return false;
  }

private:
  OCTINTERP_API bool validate (const octave_value& v);

  OCTINTERP_API bool is_equal (const octave_value& v) const;

  OCTINTERP_API void get_data_limits (void);

protected:
  octave_value data;
  double xmin;
  double xmax;
  double xminp;
  double xmaxp;
  std::set<std::string> type_constraints;
  std::list<dim_vector> size_constraints;
};

class row_vector_property : public array_property
{
public:
  row_vector_property (const std::string& nm, const graphics_handle& h,
                       const octave_value& m)
    : array_property (nm, h, m)
  {
    add_constraint (dim_vector (-1, 1));
    add_constraint (dim_vector (1, -1));
    add_constraint (dim_vector (0, 0));
  }

  row_vector_property (const row_vector_property& p)
    : array_property (p)
  {
    add_constraint (dim_vector (-1, 1));
    add_constraint (dim_vector (1, -1));
    add_constraint (dim_vector (0, 0));
  }

  void add_constraint (const std::string& type)
  {
    array_property::add_constraint (type);
  }

  void add_constraint (const dim_vector& dims)
  {
    array_property::add_constraint (dims);
  }

  void add_constraint (octave_idx_type len)
  {
    size_constraints.remove (dim_vector (1, -1));
    size_constraints.remove (dim_vector (-1, 1));
    size_constraints.remove (dim_vector (0, 0));

    add_constraint (dim_vector (1, len));
    add_constraint (dim_vector (len, 1));
  }

  row_vector_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const
  {
    row_vector_property *p = new row_vector_property (*this);

    p->type_constraints = type_constraints;
    p->size_constraints = size_constraints;

    return p;
  }

protected:
  bool do_set (const octave_value& v)
  {
    bool retval = array_property::do_set (v);

    if (! error_state)
      {
        dim_vector dv = data.dims ();

        if (dv(0) > 1 && dv(1) == 1)
          {
            int tmp = dv(0);
            dv(0) = dv(1);
            dv(1) = tmp;

            data = data.reshape (dv);
          }

        return retval;
      }

    return false;
  }

private:
  OCTINTERP_API bool validate (const octave_value& v);
};

// ---------------------------------------------------------------------

class bool_property : public radio_property
{
public:
  bool_property (const std::string& nm, const graphics_handle& h,
                 bool val)
    : radio_property (nm, h, radio_values (val ? "{on}|off" : "on|{off}"))
  { }

  bool_property (const std::string& nm, const graphics_handle& h,
                 const char* val)
    : radio_property (nm, h, radio_values ("on|off"), val)
  { }

  bool_property (const bool_property& p)
    : radio_property (p) { }

  bool is_on (void) const { return is ("on"); }

  bool_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new bool_property (*this); }

protected:
  bool do_set (const octave_value& val)
  {
    if (val.is_bool_scalar ())
      return radio_property::do_set (val.bool_value () ? "on" : "off");
    else
      return radio_property::do_set (val);
  }
};

// ---------------------------------------------------------------------

class handle_property : public base_property
{
public:
  handle_property (const std::string& nm, const graphics_handle& h,
                   const graphics_handle& val = graphics_handle ())
    : base_property (nm, h),
      current_val (val) { }

  handle_property (const handle_property& p)
    : base_property (p), current_val (p.current_val) { }

  octave_value get (void) const { return current_val.as_octave_value (); }

  graphics_handle handle_value (void) const { return current_val; }

  handle_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  handle_property& operator = (const graphics_handle& h)
  {
    set (octave_value (h.value ()));
    return *this;
  }

  void invalidate (void) { current_val = octave_NaN; }

  base_property* clone (void) const { return new handle_property (*this); }

protected:
  OCTINTERP_API bool do_set (const octave_value& v);

private:
  graphics_handle current_val;
};

// ---------------------------------------------------------------------

class any_property : public base_property
{
public:
  any_property (const std::string& nm, const graphics_handle& h,
                const octave_value& m = Matrix ())
    : base_property (nm, h), data (m) { }

  any_property (const any_property& p)
    : base_property (p), data (p.data) { }

  octave_value get (void) const { return data; }

  any_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new any_property (*this); }

protected:
  bool do_set (const octave_value& v)
  {
    data = v;
    return true;
  }

private:
  octave_value data;
};

// ---------------------------------------------------------------------

class children_property : public base_property
{
public:
  children_property (void)
    : base_property ("", graphics_handle ()), children_list ()
  {
    do_init_children (Matrix ());
  }

  children_property (const std::string& nm, const graphics_handle& h,
                     const Matrix& val)
    : base_property (nm, h), children_list ()
  {
    do_init_children (val);
  }

  children_property (const children_property& p)
    : base_property (p), children_list ()
  {
    do_init_children (p.children_list);
  }

  children_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new children_property (*this); }

  bool remove_child (double val)
  {
    return do_remove_child (val);
  }

  void adopt (double val)
  {
    do_adopt_child (val);
  }

  Matrix get_children (void) const
  {
    return do_get_children (false);
  }

  Matrix get_hidden (void) const
  {
    return do_get_children (true);
  }

  Matrix get_all (void) const
  {
    return do_get_all_children ();
  }

  octave_value get (void) const
  {
    return octave_value (get_children ());
  }

  void delete_children (bool clear = false)
  {
    do_delete_children (clear);
  }

  void renumber (graphics_handle old_gh, graphics_handle new_gh)
  {
    for (children_list_iterator p = children_list.begin ();
         p != children_list.end (); p++)
      {
        if (*p == old_gh)
          {
            *p = new_gh.value ();
            return;
          }
      }

    error ("children_list::renumber: child not found!");
  }

private:
  typedef std::list<double>::iterator children_list_iterator;
  typedef std::list<double>::const_iterator const_children_list_iterator;
  std::list<double> children_list;

protected:
  bool do_set (const octave_value& val)
  {
    const Matrix new_kids = val.matrix_value ();

    octave_idx_type nel = new_kids.numel ();

    const Matrix new_kids_column = new_kids.reshape (dim_vector (nel, 1));

    bool is_ok = true;
    bool add_hidden = true;

    if (! error_state)
      {
        const Matrix visible_kids = do_get_children (false);

        if (visible_kids.numel () == new_kids.numel ())
          {
            Matrix t1 = visible_kids.sort ();
            Matrix t2 = new_kids_column.sort ();
            Matrix t3 = get_hidden ().sort ();

            if (t1 != t2)
              is_ok = false;

            if (t1 == t3)
              add_hidden = false;
          }
        else
          is_ok = false;

        if (! is_ok)
          error ("set: new children must be a permutation of existing children");
      }
    else
      {
        is_ok = false;
        error ("set: expecting children to be array of graphics handles");
      }

    if (is_ok)
      {
        Matrix tmp = new_kids_column;

        if (add_hidden)
          tmp.stack (get_hidden ());

        children_list.clear ();

        // Don't use do_init_children here, as that reverses the
        // order of the list, and we don't want to do that if setting
        // the child list directly.

        for (octave_idx_type i = 0; i < tmp.numel (); i++)
          children_list.push_back (tmp.xelem (i));
      }

    return is_ok;
  }

private:
  void do_init_children (const Matrix& val)
  {
    children_list.clear ();
    for (octave_idx_type i = 0; i < val.numel (); i++)
      children_list.push_front (val.xelem (i));
  }

  void do_init_children (const std::list<double>& val)
  {
    children_list.clear ();
    for (const_children_list_iterator p = val.begin (); p != val.end (); p++)
      children_list.push_front (*p);
  }

  Matrix do_get_children (bool return_hidden) const;

  Matrix do_get_all_children (void) const
  {
    Matrix retval (children_list.size (), 1);
    octave_idx_type i  = 0;

    for (const_children_list_iterator p = children_list.begin ();
         p != children_list.end (); p++)
      retval(i++) = *p;
    return retval;
  }

  bool do_remove_child (double child)
  {
    for (children_list_iterator p = children_list.begin ();
         p != children_list.end (); p++)
      {
        if (*p == child)
          {
            children_list.erase (p);
            return true;
          }
      }
    return false;
  }

  void do_adopt_child (double val)
  {
    children_list.push_front (val);
  }

  void do_delete_children (bool clear);
};



// ---------------------------------------------------------------------

class callback_property : public base_property
{
public:
  callback_property (const std::string& nm, const graphics_handle& h,
                     const octave_value& m)
    : base_property (nm, h), callback (m), executing (false) { }

  callback_property (const callback_property& p)
    : base_property (p), callback (p.callback), executing (false) { }

  octave_value get (void) const { return callback; }

  OCTINTERP_API void execute (const octave_value& data = octave_value ()) const;

  bool is_defined (void) const
  {
    return (callback.is_defined () && ! callback.is_empty ());
  }

  callback_property& operator = (const octave_value& val)
  {
    set (val);
    return *this;
  }

  base_property* clone (void) const { return new callback_property (*this); }

protected:
  bool do_set (const octave_value& v)
  {
    if (validate (v))
      {
        callback = v;
        return true;
      }
    else
      error ("invalid value for callback property \"%s\"",
             get_name ().c_str ());
    return false;
  }

private:
  OCTINTERP_API bool validate (const octave_value& v) const;

private:
  octave_value callback;

  // If TRUE, we are executing this callback.
  mutable bool executing;
};

// ---------------------------------------------------------------------

class property
{
public:
  property (void) : rep (new base_property ("", graphics_handle ()))
  { }

  property (base_property *bp, bool persist = false) : rep (bp)
  { if (persist) rep->count++; }

  property (const property& p) : rep (p.rep)
  {
    rep->count++;
  }

  ~property (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  bool ok (void) const
  { return rep->ok (); }

  std::string get_name (void) const
  { return rep->get_name (); }

  void set_name (const std::string& name)
  { rep->set_name (name); }

  graphics_handle get_parent (void) const
  { return rep->get_parent (); }

  void set_parent (const graphics_handle& h)
  { rep->set_parent (h); }

  bool is_hidden (void) const
  { return rep->is_hidden (); }

  void set_hidden (bool flag)
  { rep->set_hidden (flag); }

  bool is_radio (void) const
  { return rep->is_radio (); }

  int get_id (void) const
  { return rep->get_id (); }

  void set_id (int d)
  { rep->set_id (d); }

  octave_value get (void) const
  { return rep->get (); }

  bool set (const octave_value& val, bool do_run = true,
            bool do_notify_toolkit = true)
  { return rep->set (val, do_run, do_notify_toolkit); }

  std::string values_as_string (void) const
  { return rep->values_as_string (); }

  Cell values_as_cell (void) const
  { return rep->values_as_cell (); }

  property& operator = (const octave_value& val)
  {
    *rep = val;
    return *this;
  }

  property& operator = (const property& p)
  {
    if (rep && --rep->count == 0)
      delete rep;

    rep = p.rep;
    rep->count++;

    return *this;
  }

  void add_listener (const octave_value& v, listener_mode mode = POSTSET)
  { rep->add_listener (v, mode); }

  void delete_listener (const octave_value& v = octave_value (),
                        listener_mode mode = POSTSET)
  { rep->delete_listener (v, mode); }

  void run_listeners (listener_mode mode = POSTSET)
  { rep->run_listeners (mode); }

  OCTINTERP_API static
  property create (const std::string& name, const graphics_handle& parent,
                   const caseless_str& type,
                   const octave_value_list& args);

  property clone (void) const
  { return property (rep->clone ()); }

  /*
  const string_property& as_string_property (void) const
    { return *(dynamic_cast<string_property*> (rep)); }

  const radio_property& as_radio_property (void) const
    { return *(dynamic_cast<radio_property*> (rep)); }

  const color_property& as_color_property (void) const
    { return *(dynamic_cast<color_property*> (rep)); }

  const double_property& as_double_property (void) const
    { return *(dynamic_cast<double_property*> (rep)); }

  const bool_property& as_bool_property (void) const
    { return *(dynamic_cast<bool_property*> (rep)); }

  const handle_property& as_handle_property (void) const
    { return *(dynamic_cast<handle_property*> (rep)); }
    */

private:
  base_property *rep;
};

// ---------------------------------------------------------------------

typedef std::pair <std::string, octave_value> pval_pair;

class pval_vector : public std::vector <pval_pair>
{
public:
  const_iterator find (const std::string pname) const
  {
    const_iterator it;

    for (it = (*this).begin (); it != (*this).end (); it++)
      if (pname.compare ((*it).first) == 0)
        return it;

    return (*this).end ();
  }

  iterator find (const std::string pname)
  {
    iterator it;

    for (it = (*this).begin (); it != (*this).end (); it++)
      if (pname.compare ((*it).first) == 0)
        return it;

    return (*this).end ();
  }

  octave_value lookup (const std::string pname) const
  {
    octave_value retval;

    const_iterator it = find (pname);

    if (it != (*this).end ())
      retval = (*it).second;

    return retval;
  }

  octave_value& operator [] (const std::string pname)
  {
    iterator it = find (pname);

    if (it == (*this).end ())
      {
        push_back (pval_pair (pname, octave_value ()));
        return (*this).back ().second;
      }

    return (*it).second;
  }

  void erase (const std::string pname)
  {
    iterator it = find (pname);
    if (it != (*this).end ())
      erase (it);
  }

  void erase (iterator it)
  {
    std::vector <pval_pair>::erase (it);
  }

};

class property_list
{
public:
  typedef pval_vector pval_map_type;
  typedef std::map<std::string, pval_map_type> plist_map_type;

  typedef pval_map_type::iterator pval_map_iterator;
  typedef pval_map_type::const_iterator pval_map_const_iterator;

  typedef plist_map_type::iterator plist_map_iterator;
  typedef plist_map_type::const_iterator plist_map_const_iterator;

  property_list (const plist_map_type& m = plist_map_type ())
    : plist_map (m) { }

  ~property_list (void) { }

  void set (const caseless_str& name, const octave_value& val);

  octave_value lookup (const caseless_str& name) const;

  plist_map_iterator begin (void) { return plist_map.begin (); }
  plist_map_const_iterator begin (void) const { return plist_map.begin (); }

  plist_map_iterator end (void) { return plist_map.end (); }
  plist_map_const_iterator end (void) const { return plist_map.end (); }

  plist_map_iterator find (const std::string& go_name)
  {
    return plist_map.find (go_name);
  }

  plist_map_const_iterator find (const std::string& go_name) const
  {
    return plist_map.find (go_name);
  }

  octave_scalar_map as_struct (const std::string& prefix_arg) const;

private:
  plist_map_type plist_map;
};

// ---------------------------------------------------------------------

class graphics_toolkit;
class graphics_object;

class base_graphics_toolkit
{
public:
  friend class graphics_toolkit;

public:
  base_graphics_toolkit (const std::string& nm)
    : name (nm), count (0) { }

  virtual ~base_graphics_toolkit (void) { }

  std::string get_name (void) const { return name; }

  virtual bool is_valid (void) const { return false; }

  virtual void redraw_figure (const graphics_object&) const
  { gripe_invalid ("redraw_figure"); }

  virtual void print_figure (const graphics_object&, const std::string&,
                             const std::string&, bool,
                             const std::string& = "") const
  { gripe_invalid ("print_figure"); }

  virtual Matrix get_canvas_size (const graphics_handle&) const
  {
    gripe_invalid ("get_canvas_size");
    return Matrix (1, 2, 0.0);
  }

  virtual double get_screen_resolution (void) const
  {
    gripe_invalid ("get_screen_resolution");
    return 72.0;
  }

  virtual Matrix get_screen_size (void) const
  {
    gripe_invalid ("get_screen_size");
    return Matrix (1, 2, 0.0);
  }

  // Callback function executed when the given graphics object
  // changes.  This allows the graphics toolkit to act on property
  // changes if needed.
  virtual void update (const graphics_object&, int)
  { gripe_invalid ("base_graphics_toolkit::update"); }

  void update (const graphics_handle&, int);

  // Callback function executed when the given graphics object is
  // created.  This allows the graphics toolkit to do toolkit-specific
  // initializations for a newly created object.
  virtual bool initialize (const graphics_object&)
  { gripe_invalid ("base_graphics_toolkit::initialize"); return false; }

  bool initialize (const graphics_handle&);

  // Callback function executed just prior to deleting the given
  // graphics object.  This allows the graphics toolkit to perform
  // toolkit-specific cleanup operations before an object is deleted.
  virtual void finalize (const graphics_object&)
  { gripe_invalid ("base_graphics_toolkit::finalize"); }

  void finalize (const graphics_handle&);

  // Close the graphics toolkit.
  virtual void close (void)
  { gripe_invalid ("base_graphics_toolkit::close"); }

private:
  std::string name;
  octave_refcount<int> count;

private:
  void gripe_invalid (const std::string& fname) const
  {
    if (! is_valid ())
      error ("%s: invalid graphics toolkit", fname.c_str ());
  }
};

class graphics_toolkit
{
public:
  graphics_toolkit (void)
    : rep (new base_graphics_toolkit ("unknown"))
  {
    rep->count++;
  }

  graphics_toolkit (base_graphics_toolkit* b)
    : rep (b)
  {
    rep->count++;
  }

  graphics_toolkit (const graphics_toolkit& b)
    : rep (b.rep)
  {
    rep->count++;
  }

  ~graphics_toolkit (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  graphics_toolkit& operator = (const graphics_toolkit& b)
  {
    if (rep != b.rep)
      {
        if (--rep->count == 0)
          delete rep;

        rep = b.rep;
        rep->count++;
      }

    return *this;
  }

  operator bool (void) const { return rep->is_valid (); }

  std::string get_name (void) const { return rep->get_name (); }

  void redraw_figure (const graphics_object& go) const
  { rep->redraw_figure (go); }

  void print_figure (const graphics_object& go, const std::string& term,
                     const std::string& file, bool mono,
                     const std::string& debug_file = "") const
  { rep->print_figure (go, term, file, mono, debug_file); }

  Matrix get_canvas_size (const graphics_handle& fh) const
  { return rep->get_canvas_size (fh); }

  double get_screen_resolution (void) const
  { return rep->get_screen_resolution (); }

  Matrix get_screen_size (void) const
  { return rep->get_screen_size (); }

  // Notifies graphics toolkit that object't property has changed.
  void update (const graphics_object& go, int id)
  { rep->update (go, id); }

  void update (const graphics_handle& h, int id)
  { rep->update (h, id); }

  // Notifies graphics toolkit that new object was created.
  bool initialize (const graphics_object& go)
  { return rep->initialize (go); }

  bool initialize (const graphics_handle& h)
  { return rep->initialize (h); }

  // Notifies graphics toolkit that object was destroyed.
  // This is called only for explicitly deleted object. Children are
  // deleted implicitly and graphics toolkit isn't notified.
  void finalize (const graphics_object& go)
  { rep->finalize (go); }

  void finalize (const graphics_handle& h)
  { rep->finalize (h); }

  // Close the graphics toolkit.
  void close (void) { rep->close (); }

private:

  base_graphics_toolkit *rep;
};

class gtk_manager
{
public:

  static graphics_toolkit get_toolkit (void)
  {
    return instance_ok () ? instance->do_get_toolkit () : graphics_toolkit ();
  }

  static void register_toolkit (const std::string& name)
  {
    if (instance_ok ())
      instance->do_register_toolkit (name);
  }

  static void unregister_toolkit (const std::string& name)
  {
    if (instance_ok ())
      instance->do_unregister_toolkit (name);
  }

  static void load_toolkit (const graphics_toolkit& tk)
  {
    if (instance_ok ())
      instance->do_load_toolkit (tk);
  }

  static void unload_toolkit (const std::string& name)
  {
    if (instance_ok ())
      instance->do_unload_toolkit (name);
  }

  static graphics_toolkit find_toolkit (const std::string& name)
  {
    return instance_ok ()
           ? instance->do_find_toolkit (name) : graphics_toolkit ();
  }

  static Cell available_toolkits_list (void)
  {
    return instance_ok () ? instance->do_available_toolkits_list () : Cell ();
  }

  static Cell loaded_toolkits_list (void)
  {
    return instance_ok () ? instance->do_loaded_toolkits_list () : Cell ();
  }

  static void unload_all_toolkits (void)
  {
    if (instance_ok ())
      instance->do_unload_all_toolkits ();
  }

  static std::string default_toolkit (void)
  {
    return instance_ok () ? instance->do_default_toolkit () : std::string ();
  }

private:

  gtk_manager (void) { }

  ~gtk_manager (void) { }

  OCTINTERP_API static void create_instance (void);

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      create_instance ();

    if (! instance)
      {
        ::error ("unable to create gh_manager!");

        retval = false;
      }

    return retval;
  }

  static void cleanup_instance (void) { delete instance; instance = 0; }

  OCTINTERP_API static gtk_manager *instance;

  // The name of the default toolkit.
  std::string dtk;

  // The list of toolkits that we know about.
  std::set<std::string> available_toolkits;

  // The list of toolkits we have actually loaded.
  std::map<std::string, graphics_toolkit> loaded_toolkits;

  typedef std::set<std::string>::iterator available_toolkits_iterator;

  typedef std::set<std::string>::const_iterator
    const_available_toolkits_iterator;

  typedef std::map<std::string, graphics_toolkit>::iterator
    loaded_toolkits_iterator;

  typedef std::map<std::string, graphics_toolkit>::const_iterator
    const_loaded_toolkits_iterator;

  graphics_toolkit do_get_toolkit (void) const;

  void do_register_toolkit (const std::string& name);

  void do_unregister_toolkit (const std::string& name);

  void do_load_toolkit (const graphics_toolkit& tk)
  {
    loaded_toolkits[tk.get_name ()] = tk;
  }

  void do_unload_toolkit (const std::string& name)
  {
    loaded_toolkits.erase (name);
  }

  graphics_toolkit do_find_toolkit (const std::string& name) const
  {
    const_loaded_toolkits_iterator p = loaded_toolkits.find (name);

    if (p != loaded_toolkits.end ())
      return p->second;
    else
      return graphics_toolkit ();
  }

  Cell do_available_toolkits_list (void) const
  {
    Cell m (1 , available_toolkits.size ());

    octave_idx_type i = 0;
    for (const_available_toolkits_iterator p = available_toolkits.begin ();
         p !=  available_toolkits.end (); p++)
      m(i++) = *p;

    return m;
  }

  Cell do_loaded_toolkits_list (void) const
  {
    Cell m (1 , loaded_toolkits.size ());

    octave_idx_type i = 0;
    for (const_loaded_toolkits_iterator p = loaded_toolkits.begin ();
         p !=  loaded_toolkits.end (); p++)
      m(i++) = p->first;

    return m;
  }

  void do_unload_all_toolkits (void)
  {
    while (! loaded_toolkits.empty ())
      {
        loaded_toolkits_iterator p = loaded_toolkits.begin ();

        std::string name = p->first;

        p->second.close ();

        // The toolkit may have unloaded itself.  If not, we'll do
        // it here.
        if (loaded_toolkits.find (name) != loaded_toolkits.end ())
          unload_toolkit (name);
      }
  }

  std::string do_default_toolkit (void) { return dtk; }
};

// ---------------------------------------------------------------------

class base_graphics_object;
class graphics_object;

class OCTINTERP_API base_properties
{
public:
  base_properties (const std::string& ty = "unknown",
                   const graphics_handle& mh = graphics_handle (),
                   const graphics_handle& p = graphics_handle ());

  virtual ~base_properties (void) { }

  virtual std::string graphics_object_name (void) const { return "unknown"; }

  void mark_modified (void);

  void override_defaults (base_graphics_object& obj);

  virtual void init_integerhandle (const octave_value&)
  {
    panic_impossible ();
  }

  // Look through DEFAULTS for properties with given CLASS_NAME, and
  // apply them to the current object with set (virtual method).

  void set_from_list (base_graphics_object& obj, property_list& defaults);

  void insert_property (const std::string& name, property p)
  {
    p.set_name (name);
    p.set_parent (__myhandle__);
    all_props[name] = p;
  }

  virtual void set (const caseless_str&, const octave_value&);

  virtual octave_value get (const caseless_str& pname) const;

  virtual octave_value get (const std::string& pname) const
  {
    return get (caseless_str (pname));
  }

  virtual octave_value get (const char *pname) const
  {
    return get (caseless_str (pname));
  }

  virtual octave_value get (bool all = false) const;

  virtual property get_property (const caseless_str& pname);

  virtual bool has_property (const caseless_str&) const
  {
    panic_impossible ();
    return false;
  }

  bool is_modified (void) const { return is___modified__ (); }

  virtual void remove_child (const graphics_handle& h)
  {
    if (children.remove_child (h.value ()))
      {
        children.run_listeners ();
        mark_modified ();
      }
  }

  virtual void adopt (const graphics_handle& h)
  {
    children.adopt (h.value ());
    children.run_listeners ();
    mark_modified ();
  }

  virtual graphics_toolkit get_toolkit (void) const;

  virtual Matrix
  get_boundingbox (bool /*internal*/ = false,
                   const Matrix& /*parent_pix_size*/ = Matrix ()) const
  { return Matrix (1, 4, 0.0); }

  virtual void update_boundingbox (void);

  virtual void update_autopos (const std::string& elem_type);

  virtual void add_listener (const caseless_str&, const octave_value&,
                             listener_mode = POSTSET);

  virtual void delete_listener (const caseless_str&, const octave_value&,
                                listener_mode = POSTSET);

  void set_tag (const octave_value& val) { tag = val; }

  void set_parent (const octave_value& val);

  Matrix get_children (void) const
  {
    return children.get_children ();
  }

  Matrix get_all_children (void) const
  {
    return children.get_all ();
  }

  Matrix get_hidden_children (void) const
  {
    return children.get_hidden ();
  }

  void set_modified (const octave_value& val) { set___modified__ (val); }

  void set___modified__ (const octave_value& val) { __modified__ = val; }

  void reparent (const graphics_handle& new_parent) { parent = new_parent; }

  // Update data limits for AXIS_TYPE (xdata, ydata, etc.) in the parent
  // axes object.

  virtual void update_axis_limits (const std::string& axis_type) const;

  virtual void update_axis_limits (const std::string& axis_type,
                                   const graphics_handle& h) const;

  virtual void update_uicontextmenu (void) const;

  virtual void delete_children (bool clear = false)
  {
    children.delete_children (clear);
  }

  void renumber_child (graphics_handle old_gh, graphics_handle new_gh)
  {
    children.renumber (old_gh, new_gh);
  }

  void renumber_parent (graphics_handle new_gh)
  {
    parent = new_gh;
  }

  static property_list::pval_map_type factory_defaults (void);

  // FIXME: these functions should be generated automatically by the
  //        genprops.awk script.
  //
  // EMIT_BASE_PROPERTIES_GET_FUNCTIONS

  virtual octave_value get_alim (void) const { return octave_value (); }
  virtual octave_value get_clim (void) const { return octave_value (); }
  virtual octave_value get_xlim (void) const { return octave_value (); }
  virtual octave_value get_ylim (void) const { return octave_value (); }
  virtual octave_value get_zlim (void) const { return octave_value (); }

  virtual bool is_aliminclude (void) const { return false; }
  virtual bool is_climinclude (void) const { return false; }
  virtual bool is_xliminclude (void) const { return false; }
  virtual bool is_yliminclude (void) const { return false; }
  virtual bool is_zliminclude (void) const { return false; }

  bool is_handle_visible (void) const;

  std::set<std::string> dynamic_property_names (void) const;

  bool has_dynamic_property (const std::string& pname);

protected:
  std::set<std::string> dynamic_properties;

  void set_dynamic (const caseless_str& pname, const octave_value& val);

  octave_value get_dynamic (const caseless_str& pname) const;

  octave_value get_dynamic (bool all = false) const;

  property get_property_dynamic (const caseless_str& pname);

  BEGIN_BASE_PROPERTIES
    // properties common to all objects
    bool_property beingdeleted , "off"
    radio_property busyaction , "{queue}|cancel"
    callback_property buttondownfcn , Matrix ()
    children_property children gf , Matrix ()
    bool_property clipping , "on"
    callback_property createfcn , Matrix ()
    callback_property deletefcn , Matrix ()
    radio_property handlevisibility , "{on}|callback|off"
    bool_property hittest , "on"
    bool_property interruptible , "on"
    handle_property parent fs , p
    bool_property selected , "off"
    bool_property selectionhighlight , "on"
    string_property tag s , ""
    string_property type frs , ty
    handle_property uicontextmenu u , graphics_handle ()
    any_property userdata , Matrix ()
    bool_property visible , "on"
    // additional (Octave-specific) properties
    bool_property __modified__ s , "on"
    graphics_handle __myhandle__ fhrs , mh
  END_PROPERTIES

protected:
  struct cmp_caseless_str
  {
    bool operator () (const caseless_str& a, const caseless_str& b) const
    {
      std::string a1 = a;
      std::transform (a1.begin (), a1.end (), a1.begin (), tolower);
      std::string b1 = b;
      std::transform (b1.begin (), b1.end (), b1.begin (), tolower);

      return a1 < b1;
    }
  };

  std::map<caseless_str, property, cmp_caseless_str> all_props;

protected:
  void insert_static_property (const std::string& name, base_property& p)
  { insert_property (name, property (&p, true)); }

  virtual void init (void) { }
};

class OCTINTERP_API base_graphics_object
{
public:
  friend class graphics_object;

  base_graphics_object (void) : count (1), toolkit_flag (false) { }

  virtual ~base_graphics_object (void) { }

  virtual void mark_modified (void)
  {
    if (valid_object ())
      get_properties ().mark_modified ();
    else
      error ("base_graphics_object::mark_modified: invalid graphics object");
  }

  virtual void override_defaults (base_graphics_object& obj)
  {
    if (valid_object ())
      get_properties ().override_defaults (obj);
    else
      error ("base_graphics_object::override_defaults: invalid graphics object");
  }

  void build_user_defaults_map (property_list::pval_map_type &def,
                                const std::string go_name) const;

  virtual void set_from_list (property_list& plist)
  {
    if (valid_object ())
      get_properties ().set_from_list (*this, plist);
    else
      error ("base_graphics_object::set_from_list: invalid graphics object");
  }

  virtual void set (const caseless_str& pname, const octave_value& pval)
  {
    if (valid_object ())
      get_properties ().set (pname, pval);
    else
      error ("base_graphics_object::set: invalid graphics object");
  }

  virtual void set_defaults (const std::string&)
  {
    error ("base_graphics_object::set_defaults: invalid graphics object");
  }

  virtual octave_value get (bool all = false) const
  {
    if (valid_object ())
      return get_properties ().get (all);
    else
      {
        error ("base_graphics_object::get: invalid graphics object");
        return octave_value ();
      }
  }

  virtual octave_value get (const caseless_str& pname) const
  {
    if (valid_object ())
      return get_properties ().get (pname);
    else
      {
        error ("base_graphics_object::get: invalid graphics object");
        return octave_value ();
      }
  }

  virtual octave_value get_default (const caseless_str&) const;

  virtual octave_value get_factory_default (const caseless_str&) const;

  virtual octave_value get_defaults (void) const
  {
    error ("base_graphics_object::get_defaults: invalid graphics object");
    return octave_value ();
  }

  virtual property_list get_defaults_list (void) const
  {
    if (! valid_object ())
      error ("base_graphics_object::get_defaults_list: invalid graphics object");
    return property_list ();
  }

  virtual octave_value get_factory_defaults (void) const
  {
    error ("base_graphics_object::get_factory_defaults: invalid graphics object");
    return octave_value ();
  }

  virtual property_list get_factory_defaults_list (void) const
  {
    error ("base_graphics_object::get_factory_defaults_list: invalid graphics object");
    return property_list ();
  }

  virtual bool has_readonly_property (const caseless_str& pname) const
  {
    return base_properties::has_readonly_property (pname);
  }

  virtual std::string values_as_string (void);

  virtual std::string value_as_string (const std::string& prop);

  virtual octave_scalar_map values_as_struct (void);

  virtual graphics_handle get_parent (void) const
  {
    if (valid_object ())
      return get_properties ().get_parent ();
    else
      {
        error ("base_graphics_object::get_parent: invalid graphics object");
        return graphics_handle ();
      }
  }

  graphics_handle get_handle (void) const
  {
    if (valid_object ())
      return get_properties ().get___myhandle__ ();
    else
      {
        error ("base_graphics_object::get_handle: invalid graphics object");
        return graphics_handle ();
      }
  }

  virtual void remove_child (const graphics_handle& h)
  {
    if (valid_object ())
      get_properties ().remove_child (h);
    else
      error ("base_graphics_object::remove_child: invalid graphics object");
  }

  virtual void adopt (const graphics_handle& h)
  {
    if (valid_object ())
      get_properties ().adopt (h);
    else
      error ("base_graphics_object::adopt: invalid graphics object");
  }

  virtual void reparent (const graphics_handle& np)
  {
    if (valid_object ())
      get_properties ().reparent (np);
    else
      error ("base_graphics_object::reparent: invalid graphics object");
  }

  virtual void defaults (void) const
  {
    if (valid_object ())
      {
        std::string msg = (type () + "::defaults");
        gripe_not_implemented (msg.c_str ());
      }
    else
      error ("base_graphics_object::default: invalid graphics object");
  }

  virtual base_properties& get_properties (void)
  {
    static base_properties properties;
    error ("base_graphics_object::get_properties: invalid graphics object");
    return properties;
  }

  virtual const base_properties& get_properties (void) const
  {
    static base_properties properties;
    error ("base_graphics_object::get_properties: invalid graphics object");
    return properties;
  }

  virtual void update_axis_limits (const std::string& axis_type);

  virtual void update_axis_limits (const std::string& axis_type,
                                   const graphics_handle& h);

  virtual bool valid_object (void) const { return false; }

  bool valid_toolkit_object (void) const { return toolkit_flag; }

  virtual std::string type (void) const
  {
    return (valid_object () ? get_properties ().graphics_object_name ()
                            : "unknown");
  }

  bool isa (const std::string& go_name) const
  {
    return type () == go_name;
  }

  virtual graphics_toolkit get_toolkit (void) const
  {
    if (valid_object ())
      return get_properties ().get_toolkit ();
    else
      {
        error ("base_graphics_object::get_toolkit: invalid graphics object");
        return graphics_toolkit ();
      }
  }

  virtual void add_property_listener (const std::string& nm,
                                      const octave_value& v,
                                      listener_mode mode = POSTSET)
  {
    if (valid_object ())
      get_properties ().add_listener (nm, v, mode);
  }

  virtual void delete_property_listener (const std::string& nm,
                                         const octave_value& v,
                                         listener_mode mode = POSTSET)
  {
    if (valid_object ())
      get_properties ().delete_listener (nm, v, mode);
  }

  virtual void remove_all_listeners (void);

  virtual void reset_default_properties (void);

protected:
  virtual void initialize (const graphics_object& go)
  {
    if (! toolkit_flag)
      toolkit_flag = get_toolkit ().initialize (go);
  }

  virtual void finalize (const graphics_object& go)
  {
    if (toolkit_flag)
      {
        get_toolkit ().finalize (go);
        toolkit_flag = false;
      }
  }

  virtual void update (const graphics_object& go, int id)
  {
    if (toolkit_flag)
      get_toolkit ().update (go, id);
  }

protected:
  // A reference count.
  octave_refcount<int> count;

  // A flag telling whether this object is a valid object
  // in the backend context.
  bool toolkit_flag;

  // No copying!

  base_graphics_object (const base_graphics_object&) : count (0) { }

  base_graphics_object& operator = (const base_graphics_object&)
  {
    return *this;
  }
};

class OCTINTERP_API graphics_object
{
public:
  graphics_object (void) : rep (new base_graphics_object ()) { }

  graphics_object (base_graphics_object *new_rep)
    : rep (new_rep) { }

  graphics_object (const graphics_object& obj) : rep (obj.rep)
  {
    rep->count++;
  }

  graphics_object& operator = (const graphics_object& obj)
  {
    if (rep != obj.rep)
      {
        if (--rep->count == 0)
          delete rep;

        rep = obj.rep;
        rep->count++;
      }

    return *this;
  }

  ~graphics_object (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  void mark_modified (void) { rep->mark_modified (); }

  void override_defaults (base_graphics_object& obj)
  {
    rep->override_defaults (obj);
  }

  void override_defaults (void)
  {
    rep->override_defaults (*rep);
  }

  void build_user_defaults_map (property_list::pval_map_type &def,
                                const std::string go_name) const
  {
    rep->build_user_defaults_map (def, go_name);
  }

  void set_from_list (property_list& plist) { rep->set_from_list (plist); }

  void set (const caseless_str& name, const octave_value& val)
  {
    rep->set (name, val);
  }

  void set (const octave_value_list& args);

  void set (const Array<std::string>& names, const Cell& values,
            octave_idx_type row);

  void set (const octave_map& m);

  void set_value_or_default (const caseless_str& name,
                             const octave_value& val);

  void set_defaults (const std::string& mode) { rep->set_defaults (mode); }

  octave_value get (bool all = false) const { return rep->get (all); }

  octave_value get (const caseless_str& name) const
  {
    return name.compare ("default")
           ? get_defaults ()
           : (name.compare ("factory")
              ? get_factory_defaults () : rep->get (name));
  }

  octave_value get (const std::string& name) const
  {
    return get (caseless_str (name));
  }

  octave_value get (const char *name) const
  {
    return get (caseless_str (name));
  }

  octave_value get_default (const caseless_str& name) const
  {
    return rep->get_default (name);
  }

  octave_value get_factory_default (const caseless_str& name) const
  {
    return rep->get_factory_default (name);
  }

  octave_value get_defaults (void) const { return rep->get_defaults (); }

  property_list get_defaults_list (void) const
  {
    return rep->get_defaults_list ();
  }

  octave_value get_factory_defaults (void) const
  {
    return rep->get_factory_defaults ();
  }

  property_list get_factory_defaults_list (void) const
  {
    return rep->get_factory_defaults_list ();
  }

  bool has_readonly_property (const caseless_str& pname) const
  {
    return rep->has_readonly_property (pname);
  }

  std::string values_as_string (void) { return rep->values_as_string (); }

  std::string value_as_string (const std::string& prop)
  {
    return rep->value_as_string (prop);
  }

  octave_map values_as_struct (void) { return rep->values_as_struct (); }

  graphics_handle get_parent (void) const { return rep->get_parent (); }

  graphics_handle get_handle (void) const { return rep->get_handle (); }

  graphics_object get_ancestor (const std::string& type) const;

  void remove_child (const graphics_handle& h) { rep->remove_child (h); }

  void adopt (const graphics_handle& h) { rep->adopt (h); }

  void reparent (const graphics_handle& h) { rep->reparent (h); }

  void defaults (void) const { rep->defaults (); }

  bool isa (const std::string& go_name) const { return rep->isa (go_name); }

  base_properties& get_properties (void) { return rep->get_properties (); }

  const base_properties& get_properties (void) const
  {
    return rep->get_properties ();
  }

  void update_axis_limits (const std::string& axis_type)
  {
    rep->update_axis_limits (axis_type);
  }

  void update_axis_limits (const std::string& axis_type,
                           const graphics_handle& h)
  {
    rep->update_axis_limits (axis_type, h);
  }

  bool valid_object (void) const { return rep->valid_object (); }

  std::string type (void) const { return rep->type (); }

  operator bool (void) const { return rep->valid_object (); }

  // FIXME: these functions should be generated automatically by the
  //        genprops.awk script.
  //
  // EMIT_GRAPHICS_OBJECT_GET_FUNCTIONS

  octave_value get_alim (void) const
  { return get_properties ().get_alim (); }

  octave_value get_clim (void) const
  { return get_properties ().get_clim (); }

  octave_value get_xlim (void) const
  { return get_properties ().get_xlim (); }

  octave_value get_ylim (void) const
  { return get_properties ().get_ylim (); }

  octave_value get_zlim (void) const
  { return get_properties ().get_zlim (); }

  bool is_aliminclude (void) const
  { return get_properties ().is_aliminclude (); }

  bool is_climinclude (void) const
  { return get_properties ().is_climinclude (); }

  bool is_xliminclude (void) const
  { return get_properties ().is_xliminclude (); }

  bool is_yliminclude (void) const
  { return get_properties ().is_yliminclude (); }

  bool is_zliminclude (void) const
  { return get_properties ().is_zliminclude (); }

  bool is_handle_visible (void) const
  { return get_properties ().is_handle_visible (); }

  graphics_toolkit get_toolkit (void) const { return rep->get_toolkit (); }

  void add_property_listener (const std::string& nm, const octave_value& v,
                              listener_mode mode = POSTSET)
  { rep->add_property_listener (nm, v, mode); }

  void delete_property_listener (const std::string& nm, const octave_value& v,
                                 listener_mode mode = POSTSET)
  { rep->delete_property_listener (nm, v, mode); }

  void initialize (void) { rep->initialize (*this); }

  void finalize (void) { rep->finalize (*this); }

  void update (int id) { rep->update (*this, id); }

  void reset_default_properties (void)
  { rep->reset_default_properties (); }

private:
  base_graphics_object *rep;
};

// ---------------------------------------------------------------------

class OCTINTERP_API root_figure : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    void remove_child (const graphics_handle& h);

    Matrix get_boundingbox (bool internal = false,
                            const Matrix& parent_pix_size = Matrix ()) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.

    // FIXME: Properties that still dont have callbacks are:
    // language, monitorpositions, pointerlocation, pointerwindow.
    // Note that these properties are not yet used by Octave, so setting
    // them will have no effect.

    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (root_figure, root)
      handle_property callbackobject Sr , graphics_handle ()
      array_property commandwindowsize r , Matrix (1, 2, 0)
      handle_property currentfigure S , graphics_handle ()
      bool_property diary GS , "off"
      string_property diaryfile GS , "diary"
      bool_property echo GS , "off"
      string_property errormessage Gr , ""
      string_property fixedwidthfontname , "Courier"
      radio_property format GS , "+|bank|bit|hex|long|longe|longeng|longg|native-bit|native-hex|none|rat|{short}|shorte|shorteng|shortg"
      radio_property formatspacing GS , "compact|{loose}"
      string_property language , "ascii"
      array_property monitorpositions , Matrix (1, 4, 0)
      array_property pointerlocation , Matrix (1, 2, 0)
      double_property pointerwindow r , 0.0
      double_property recursionlimit GS , 256.0
      double_property screendepth r , default_screendepth ()
      double_property screenpixelsperinch r , default_screenpixelsperinch ()
      array_property screensize r , default_screensize ()
      bool_property showhiddenhandles , "off"
      radio_property units U , "inches|centimeters|normalized|points|{pixels}"
    END_PROPERTIES

  private:
    std::list<graphics_handle> cbo_stack;

  };

private:
  properties xproperties;

public:

  root_figure (void)
    : xproperties (0, graphics_handle ()), default_properties () { }

  ~root_figure (void) { }

  void mark_modified (void) { }

  void override_defaults (base_graphics_object& obj)
  {
    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      xproperties.set (name, value);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      return get_default (name.substr (7));
    else if (name.compare ("factory", 7))
      return get_factory_default (name.substr (7));
    else
      retval = xproperties.get (name);

    return retval;
  }

  octave_value get_default (const caseless_str& name) const
  {
    octave_value retval = default_properties.lookup (name);

    if (retval.is_undefined ())
      {
        // no default property found, use factory default
        retval = factory_properties.lookup (name);

        if (retval.is_undefined ())
          error ("get: invalid default property '%s'", name.c_str ());
      }

    return retval;
  }

  octave_value get_factory_default (const caseless_str& name) const
  {
    octave_value retval = factory_properties.lookup (name);

    if (retval.is_undefined ())
      error ("get: invalid factory default property '%s'", name.c_str ());

    return retval;
  }

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return default_properties;
  }

  octave_value get_factory_defaults (void) const
  {
    return factory_properties.as_struct ("factory");
  }

  property_list get_factory_defaults_list (void) const
  {
    return factory_properties;
  }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

private:
  property_list default_properties;

  static property_list factory_properties;

  static property_list::plist_map_type init_factory_properties (void);
};

// ---------------------------------------------------------------------

class OCTINTERP_API figure : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    void init_integerhandle (const octave_value& val)
    {
      integerhandle = val;
    }

    void remove_child (const graphics_handle& h);

    void set_visible (const octave_value& val);

    graphics_toolkit get_toolkit (void) const
    {
      if (! toolkit)
        toolkit = gtk_manager::get_toolkit ();

      return toolkit;
    }

    void set_toolkit (const graphics_toolkit& b);

    void set___graphics_toolkit__ (const octave_value& val)
    {
      if (! error_state)
        {
          if (val.is_string ())
            {
              std::string nm = val.string_value ();
              graphics_toolkit b = gtk_manager::find_toolkit (nm);
              if (b.get_name () != nm)
                {
                  error ("set___graphics_toolkit__: invalid graphics toolkit");
                }
              else
                {
                  if (nm != get___graphics_toolkit__ ())
                    {
                      set_toolkit (b);
                      mark_modified ();
                    }
                }
            }
          else
            error ("set___graphics_toolkit__ must be a string");
        }
    }

    void adopt (const graphics_handle& h);

    void set_position (const octave_value& val,
                       bool do_notify_toolkit = true);

    void set_outerposition (const octave_value& val,
                            bool do_notify_toolkit = true);

    Matrix get_boundingbox (bool internal = false,
                            const Matrix& parent_pix_size = Matrix ()) const;

    void set_boundingbox (const Matrix& bb, bool internal = false,
                          bool do_notify_toolkit = true);

    Matrix map_from_boundingbox (double x, double y) const;

    Matrix map_to_boundingbox (double x, double y) const;

    void update_units (const caseless_str& old_units);

    void update_paperunits (const caseless_str& old_paperunits);

    std::string get_title (void) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (figure)
      array_property alphamap , Matrix (64, 1, 1)
      callback_property buttondownfcn , Matrix ()
      callback_property closerequestfcn , "closereq"
      color_property color , color_property (color_values (1, 1, 1), radio_values ("none"))
      array_property colormap , jet_colormap ()
      handle_property currentaxes S , graphics_handle ()
      string_property currentcharacter r , ""
      handle_property currentobject r , graphics_handle ()
      array_property currentpoint r , Matrix (2, 1, 0)
      bool_property dockcontrols , "off"
      bool_property doublebuffer , "on"
      string_property filename , ""
      bool_property integerhandle S , "on"
      bool_property inverthardcopy , "off"
      callback_property keypressfcn , Matrix ()
      callback_property keyreleasefcn , Matrix ()
      radio_property menubar , "none|{figure}"
      double_property mincolormap , 64
      string_property name , ""
      radio_property nextplot , "new|{add}|replacechildren|replace"
      bool_property numbertitle , "on"
      array_property outerposition s , Matrix (1, 4, -1.0)
      radio_property paperorientation U , "{portrait}|landscape|rotated"
      array_property paperposition m , default_figure_paperposition ()
      radio_property paperpositionmode au , "auto|{manual}"
      array_property papersize U , default_figure_papersize ()
      radio_property papertype SU , "{usletter}|uslegal|a0|a1|a2|a3|a4|a5|b0|b1|b2|b3|b4|b5|arch-a|arch-b|arch-c|arch-d|arch-e|a|b|c|d|e|tabloid|<custom>"
      radio_property paperunits Su , "{inches}|centimeters|normalized|points"
      radio_property pointer , "crosshair|fullcrosshair|{arrow}|ibeam|watch|topl|topr|botl|botr|left|top|right|bottom|circle|cross|fleur|custom|hand"
      array_property pointershapecdata , Matrix (16, 16, 0)
      array_property pointershapehotspot , Matrix (1, 2, 0)
      array_property position s , default_figure_position ()
      radio_property renderer , "{painters}|zbuffer|opengl|none"
      radio_property renderermode , "{auto}|manual"
      bool_property resize , "on"
      callback_property resizefcn , Matrix ()
      radio_property selectiontype , "{normal}|open|alt|extend"
      radio_property toolbar , "none|{auto}|figure"
      radio_property units Su , "inches|centimeters|normalized|points|{pixels}|characters"
      callback_property windowbuttondownfcn , Matrix ()
      callback_property windowbuttonmotionfcn , Matrix ()
      callback_property windowbuttonupfcn , Matrix ()
      callback_property windowkeypressfcn , Matrix ()
      callback_property windowkeyreleasefcn , Matrix ()
      callback_property windowscrollwheelfcn , Matrix ()
      radio_property windowstyle , "{normal}|modal|docked"
      string_property wvisual , ""
      radio_property wvisualmode , "{auto}|manual"
      string_property xdisplay , ""
      string_property xvisual , ""
      radio_property xvisualmode , "{auto}|manual"
      // Octave-specific properties
      radio_property __mouse_mode__ hS , "{none}|pan|rotate|select|text|zoom"
      any_property __pan_mode__ h , Matrix ()
      any_property __rotate_mode__ h , Matrix ()
      any_property __zoom_mode__ h , Matrix ()
      bool_property __enhanced__ h , "on"
      string_property __graphics_toolkit__ hs , gtk_manager::default_toolkit ()
      any_property __guidata__ h , Matrix ()
      any_property __plot_stream__ h , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    {
      alphamap.add_constraint (dim_vector (-1, 1));
      colormap.add_constraint (dim_vector (-1, 3));
      outerposition.add_constraint (dim_vector (1, 4));
      paperposition.add_constraint (dim_vector (1, 4));
      papersize.add_constraint (dim_vector (1, 2));
      pointershapecdata.add_constraint (dim_vector (16, 16));
      pointershapehotspot.add_constraint (dim_vector (1, 2));
      position.add_constraint (dim_vector (1, 4));
    }

  private:
    Matrix get_auto_paperposition (void);

    void update_paperpositionmode (void)
    {
      if (paperpositionmode.is ("auto"))
        paperposition.set (get_auto_paperposition ());
    }

    mutable graphics_toolkit toolkit;
  };

private:
  properties xproperties;

public:
  figure (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p), default_properties ()
  { }

  ~figure (void) { }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (root figure) to override first (properties knows how
    // to find the parent object).
    xproperties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      xproperties.set (name, value);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = xproperties.get (name);

    return retval;
  }

  octave_value get_default (const caseless_str& name) const;

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return default_properties;
  }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

private:
  property_list default_properties;
};

// ---------------------------------------------------------------------

class OCTINTERP_API graphics_xform
{
public:
  graphics_xform (void)
    : xform (xform_eye ()), xform_inv (xform_eye ()),
      sx ("linear"), sy ("linear"), sz ("linear"),  zlim (1, 2, 0.0)
  {
    zlim(1) = 1.0;
  }

  graphics_xform (const Matrix& xm, const Matrix& xim,
                  const scaler& x, const scaler& y, const scaler& z,
                  const Matrix& zl)
    : xform (xm), xform_inv (xim), sx (x), sy (y), sz (z), zlim (zl) { }

  graphics_xform (const graphics_xform& g)
    : xform (g.xform), xform_inv (g.xform_inv), sx (g.sx),
      sy (g.sy), sz (g.sz), zlim (g.zlim) { }

  ~graphics_xform (void) { }

  graphics_xform& operator = (const graphics_xform& g)
  {
    xform = g.xform;
    xform_inv = g.xform_inv;
    sx = g.sx;
    sy = g.sy;
    sz = g.sz;
    zlim = g.zlim;

    return *this;
  }

  static ColumnVector xform_vector (double x, double y, double z);

  static Matrix xform_eye (void);

  ColumnVector transform (double x, double y, double z,
                          bool use_scale = true) const;

  ColumnVector untransform (double x, double y, double z,
                            bool use_scale = true) const;

  ColumnVector untransform (double x, double y, bool use_scale = true) const
  { return untransform (x, y, (zlim(0)+zlim(1))/2, use_scale); }

  Matrix xscale (const Matrix& m) const { return sx.scale (m); }
  Matrix yscale (const Matrix& m) const { return sy.scale (m); }
  Matrix zscale (const Matrix& m) const { return sz.scale (m); }

  Matrix scale (const Matrix& m) const
  {
    bool has_z = (m.columns () > 2);

    if (sx.is_linear () && sy.is_linear ()
        && (! has_z || sz.is_linear ()))
      return m;

    Matrix retval (m.dims ());

    int r = m.rows ();

    for (int i = 0; i < r; i++)
      {
        retval(i,0) = sx.scale (m(i,0));
        retval(i,1) = sy.scale (m(i,1));
        if (has_z)
          retval(i,2) = sz.scale (m(i,2));
      }

    return retval;
  }

private:
  Matrix xform;
  Matrix xform_inv;
  scaler sx, sy, sz;
  Matrix zlim;
};

enum
{
  AXE_ANY_DIR   = 0,
  AXE_DEPTH_DIR = 1,
  AXE_HORZ_DIR  = 2,
  AXE_VERT_DIR  = 3
};

class OCTINTERP_API axes : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    void set_defaults (base_graphics_object& obj, const std::string& mode);

    void remove_child (const graphics_handle& h);

    const scaler& get_x_scaler (void) const { return sx; }
    const scaler& get_y_scaler (void) const { return sy; }
    const scaler& get_z_scaler (void) const { return sz; }

    Matrix get_boundingbox (bool internal = false,
                            const Matrix& parent_pix_size = Matrix ()) const;
    Matrix get_extent (bool with_text = false,
                       bool only_text_height=false) const;

    double get_fontsize_points (double box_pix_height = 0) const;

    void update_boundingbox (void)
    {
      if (units_is ("normalized"))
        {
          sync_positions ();
          base_properties::update_boundingbox ();
        }
    }

    void update_camera (void);
    void update_axes_layout (void);
    void update_aspectratios (void);
    void update_transform (void)
    {
      update_aspectratios ();
      update_camera ();
      update_axes_layout ();
    }

    void sync_positions (void);

    void update_autopos (const std::string& elem_type);
    void update_xlabel_position (void);
    void update_ylabel_position (void);
    void update_zlabel_position (void);
    void update_title_position (void);

    graphics_xform get_transform (void) const
    { return graphics_xform (x_render, x_render_inv, sx, sy, sz, x_zlim); }

    Matrix get_transform_matrix (void) const { return x_render; }
    Matrix get_inverse_transform_matrix (void) const { return x_render_inv; }
    Matrix get_opengl_matrix_1 (void) const { return x_gl_mat1; }
    Matrix get_opengl_matrix_2 (void) const { return x_gl_mat2; }
    Matrix get_transform_zlim (void) const { return x_zlim; }

    int get_xstate (void) const { return xstate; }
    int get_ystate (void) const { return ystate; }
    int get_zstate (void) const { return zstate; }
    double get_xPlane (void) const { return xPlane; }
    double get_xPlaneN (void) const { return xPlaneN; }
    double get_yPlane (void) const { return yPlane; }
    double get_yPlaneN (void) const { return yPlaneN; }
    double get_zPlane (void) const { return zPlane; }
    double get_zPlaneN (void) const { return zPlaneN; }
    double get_xpTick (void) const { return xpTick; }
    double get_xpTickN (void) const { return xpTickN; }
    double get_ypTick (void) const { return ypTick; }
    double get_ypTickN (void) const { return ypTickN; }
    double get_zpTick (void) const { return zpTick; }
    double get_zpTickN (void) const { return zpTickN; }
    double get_x_min (void) const { return std::min (xPlane, xPlaneN); }
    double get_x_max (void) const { return std::max (xPlane, xPlaneN); }
    double get_y_min (void) const { return std::min (yPlane, yPlaneN); }
    double get_y_max (void) const { return std::max (yPlane, yPlaneN); }
    double get_z_min (void) const { return std::min (zPlane, zPlaneN); }
    double get_z_max (void) const { return std::max (zPlane, zPlaneN); }
    double get_fx (void) const { return fx; }
    double get_fy (void) const { return fy; }
    double get_fz (void) const { return fz; }
    double get_xticklen (void) const { return xticklen; }
    double get_yticklen (void) const { return yticklen; }
    double get_zticklen (void) const { return zticklen; }
    double get_xtickoffset (void) const { return xtickoffset; }
    double get_ytickoffset (void) const { return ytickoffset; }
    double get_ztickoffset (void) const { return ztickoffset; }
    bool get_x2Dtop (void) const { return x2Dtop; }
    bool get_y2Dright (void) const { return y2Dright; }
    bool get_layer2Dtop (void) const { return layer2Dtop; }
    bool get_is2D (void) const { return is2D; }
    bool get_xySym (void) const { return xySym; }
    bool get_xyzSym (void) const { return xyzSym; }
    bool get_zSign (void) const { return zSign; }
    bool get_nearhoriz (void) const { return nearhoriz; }

    ColumnVector pixel2coord (double px, double py) const
    { return get_transform ().untransform (px, py, (x_zlim(0)+x_zlim(1))/2); }

    ColumnVector coord2pixel (double x, double y, double z) const
    { return get_transform ().transform (x, y, z); }

    void zoom_about_point (const std::string& mode, double x, double y,
                           double factor, bool push_to_zoom_stack = true);
    void zoom (const std::string& mode, double factor,
               bool push_to_zoom_stack = true);
    void zoom (const std::string& mode, const Matrix& xl, const Matrix& yl,
               bool push_to_zoom_stack = true);

    void translate_view (const std::string& mode,
                         double x0, double x1, double y0, double y1,
                         bool push_to_zoom_stack = true);

    void pan (const std::string& mode, double factor,
              bool push_to_zoom_stack = true);

    void rotate3d (double x0, double x1, double y0, double y1,
                   bool push_to_zoom_stack = true);

    void rotate_view (double delta_az, double delta_el,
                      bool push_to_zoom_stack = true);

    void unzoom (void);
    void push_zoom_stack (void);
    void clear_zoom_stack (bool do_unzoom = true);

    void update_units (const caseless_str& old_units);

    void update_fontunits (const caseless_str& old_fontunits);

  private:
    scaler sx, sy, sz;
    Matrix x_render, x_render_inv;
    Matrix x_gl_mat1, x_gl_mat2;
    Matrix x_zlim;
    std::list<octave_value> zoom_stack;

    // Axes layout data
    int xstate, ystate, zstate;
    double xPlane, xPlaneN, yPlane, yPlaneN, zPlane, zPlaneN;
    double xpTick, xpTickN, ypTick, ypTickN, zpTick, zpTickN;
    double fx, fy, fz;
    double xticklen, yticklen, zticklen;
    double xtickoffset, ytickoffset, ztickoffset;
    bool x2Dtop, y2Dright, layer2Dtop, is2D;
    bool xySym, xyzSym, zSign, nearhoriz;

#if HAVE_FREETYPE
    // FreeType renderer, used for calculation of text (tick labels) size
    ft_render text_renderer;
#endif

    void set_text_child (handle_property& h, const std::string& who,
                         const octave_value& v);

    void delete_text_child (handle_property& h);

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (axes)
      radio_property activepositionproperty , "{outerposition}|position"
      row_vector_property alim m , default_lim ()
      radio_property alimmode , "{auto}|manual"
      color_property ambientlightcolor , color_values (1, 1, 1)
      bool_property box , "on"
      array_property cameraposition m , Matrix (1, 3, 0.0)
      radio_property camerapositionmode , "{auto}|manual"
      array_property cameratarget m , Matrix (1, 3, 0.0)
      radio_property cameratargetmode , "{auto}|manual"
      array_property cameraupvector m , Matrix (1, 3, 0.0)
      radio_property cameraupvectormode , "{auto}|manual"
      double_property cameraviewangle m , 10.0
      radio_property cameraviewanglemode , "{auto}|manual"
      row_vector_property clim m , default_lim ()
      radio_property climmode al , "{auto}|manual"
      color_property color , color_property (color_values (1, 1, 1), radio_values ("none"))
      array_property colororder , default_colororder ()
      array_property currentpoint , Matrix (2, 3, 0.0)
      array_property dataaspectratio mu , Matrix (1, 3, 1.0)
      radio_property dataaspectratiomode u , "{auto}|manual"
      radio_property drawmode , "{normal}|fast"
      radio_property fontangle u , "{normal}|italic|oblique"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize u , 10
      radio_property fontunits SU , "{points}|normalized|inches|centimeters|pixels"
      radio_property fontweight u , "{normal}|light|demi|bold"
      radio_property gridlinestyle , "-|--|{:}|-.|none"
      // NOTE: interpreter is not a Matlab axis property, but it makes
      //       more sense to have it so that axis ticklabels can use it.
      radio_property interpreter , "tex|{none}|latex"
      radio_property layer u , "{bottom}|top"
      // FIXME: should be kind of string array.
      any_property linestyleorder S , "-"
      double_property linewidth , 0.5
      radio_property minorgridlinestyle , "-|--|{:}|-.|none"
      double_property mousewheelzoom , 0.5
      radio_property nextplot , "add|replacechildren|{replace}"
      array_property outerposition u , default_axes_outerposition ()
      array_property plotboxaspectratio mu , Matrix (1, 3, 1.0)
      radio_property plotboxaspectratiomode u , "{auto}|manual"
      array_property position u , default_axes_position ()
      radio_property projection , "{orthographic}|perspective"
      radio_property tickdir mu , "{in}|out"
      radio_property tickdirmode u , "{auto}|manual"
      array_property ticklength u , default_axes_ticklength ()
      array_property tightinset r , Matrix (1, 4, 0.0)
      handle_property title SOf , gh_manager::make_graphics_handle ("text", __myhandle__, false, false, false)
      // FIXME: uicontextmenu should be moved here.
      radio_property units SU , "{normalized}|inches|centimeters|points|pixels|characters"
      array_property view u , default_axes_view ()
      radio_property xaxislocation u , "{bottom}|top|zero"
      color_property xcolor , color_values (0, 0, 0)
      radio_property xdir u , "{normal}|reverse"
      bool_property xgrid , "off"
      handle_property xlabel SOf , gh_manager::make_graphics_handle ("text", __myhandle__, false, false, false)
      row_vector_property xlim mu , default_lim ()
      radio_property xlimmode al , "{auto}|manual"
      bool_property xminorgrid , "off"
      bool_property xminortick , "off"
      radio_property xscale alu , "{linear}|log"
      row_vector_property xtick mu , default_axes_tick ()
      // FIXME: should be kind of string array.
      any_property xticklabel S , ""
      radio_property xticklabelmode u , "{auto}|manual"
      radio_property xtickmode u , "{auto}|manual"
      radio_property yaxislocation u , "{left}|right|zero"
      color_property ycolor , color_values (0, 0, 0)
      radio_property ydir u , "{normal}|reverse"
      bool_property ygrid , "off"
      handle_property ylabel SOf , gh_manager::make_graphics_handle ("text", __myhandle__, false, false, false)
      row_vector_property ylim mu , default_lim ()
      radio_property ylimmode al , "{auto}|manual"
      bool_property yminorgrid , "off"
      bool_property yminortick , "off"
      radio_property yscale alu , "{linear}|log"
      row_vector_property ytick mu , default_axes_tick ()
      any_property yticklabel S , ""
      radio_property yticklabelmode u , "{auto}|manual"
      radio_property ytickmode u , "{auto}|manual"
      color_property zcolor , color_values (0, 0, 0)
      radio_property zdir u , "{normal}|reverse"
      bool_property zgrid , "off"
      handle_property zlabel SOf , gh_manager::make_graphics_handle ("text", __myhandle__, false, false, false)
      row_vector_property zlim mu , default_lim ()
      radio_property zlimmode al , "{auto}|manual"
      bool_property zminorgrid , "off"
      bool_property zminortick , "off"
      radio_property zscale alu , "{linear}|log"
      row_vector_property ztick mu , default_axes_tick ()
      any_property zticklabel S , ""
      radio_property zticklabelmode u , "{auto}|manual"
      radio_property ztickmode u , "{auto}|manual"
      // Octave-specific properties
      bool_property __hold_all__ h , "off"
      // hidden properties for alignment of subplots
      radio_property autopos_tag h , "{none}|subplot"
      // hidden properties for inset
      array_property looseinset hu , Matrix (1, 4, 0.0)
      // hidden properties for transformation computation
      array_property x_viewtransform h , Matrix (4, 4, 0.0)
      array_property x_projectiontransform h , Matrix (4, 4, 0.0)
      array_property x_viewporttransform h , Matrix (4, 4, 0.0)
      array_property x_normrendertransform h , Matrix (4, 4, 0.0)
      array_property x_rendertransform h , Matrix (4, 4, 0.0)
      // hidden properties for minor ticks
      row_vector_property xmtick h , Matrix ()
      row_vector_property ymtick h , Matrix ()
      row_vector_property zmtick h , Matrix ()
      // hidden property for text rendering
      double_property fontsize_points hgr , 0
   END_PROPERTIES

  protected:
    void init (void);

  private:

    std::string
    get_scale (const std::string& scale, const Matrix& lims)
    {
      std::string retval = scale;

      if (scale == "log" && lims.numel () > 1 && lims(0) < 0 && lims(1) < 0)
        retval = "neglog";

      return retval;
    }

    void update_xscale (void)
    {
      sx = get_scale (get_xscale (), xlim.get ().matrix_value ());
    }

    void update_yscale (void)
    {
      sy = get_scale (get_yscale (), ylim.get ().matrix_value ());
    }

    void update_zscale (void)
    {
      sz = get_scale (get_zscale (), zlim.get ().matrix_value ());
    }

    void update_view (void) { sync_positions (); }
    void update_dataaspectratio (void) { sync_positions (); }
    void update_dataaspectratiomode (void) { sync_positions (); }
    void update_plotboxaspectratio (void) { sync_positions (); }
    void update_plotboxaspectratiomode (void) { sync_positions (); }

    void update_layer (void) { update_axes_layout (); }
    void update_yaxislocation (void)
    {
      sync_positions ();
      update_axes_layout ();
      update_ylabel_position ();
    }
    void update_xaxislocation (void)
    {
      sync_positions ();
      update_axes_layout ();
      update_xlabel_position ();
    }

    void update_xdir (void) { update_camera (); update_axes_layout (); }
    void update_ydir (void) { update_camera (); update_axes_layout (); }
    void update_zdir (void) { update_camera (); update_axes_layout (); }

    void update_ticklength (void);
    void update_tickdir (void) { update_ticklength (); }
    void update_tickdirmode (void) { update_ticklength (); }

    void update_xtick (void)
    {
      if (xticklabelmode.is ("auto"))
        calc_ticklabels (xtick, xticklabel, xscale.is ("log"));
      sync_positions ();
    }
    void update_ytick (void)
    {
      if (yticklabelmode.is ("auto"))
        calc_ticklabels (ytick, yticklabel, yscale.is ("log"));
      sync_positions ();
    }
    void update_ztick (void)
    {
      if (zticklabelmode.is ("auto"))
        calc_ticklabels (ztick, zticklabel, zscale.is ("log"));
      sync_positions ();
    }

    void update_xtickmode (void)
    {
      if (xtickmode.is ("auto"))
        {
          calc_ticks_and_lims (xlim, xtick, xmtick, xlimmode.is ("auto"),
                               xscale.is ("log"));
          update_xtick ();
        }
    }
    void update_ytickmode (void)
    {
      if (ytickmode.is ("auto"))
        {
          calc_ticks_and_lims (ylim, ytick, ymtick, ylimmode.is ("auto"),
                               yscale.is ("log"));
          update_ytick ();
        }
    }
    void update_ztickmode (void)
    {
      if (ztickmode.is ("auto"))
        {
          calc_ticks_and_lims (zlim, ztick, zmtick, zlimmode.is ("auto"),
                               zscale.is ("log"));
          update_ztick ();
        }
    }

    void update_xticklabelmode (void)
    {
      if (xticklabelmode.is ("auto"))
        calc_ticklabels (xtick, xticklabel, xscale.is ("log"));
    }
    void update_yticklabelmode (void)
    {
      if (yticklabelmode.is ("auto"))
        calc_ticklabels (ytick, yticklabel, yscale.is ("log"));
    }
    void update_zticklabelmode (void)
    {
      if (zticklabelmode.is ("auto"))
        calc_ticklabels (ztick, zticklabel, zscale.is ("log"));
    }

    void update_font (void);
    void update_fontname (void)
    {
      update_font ();
      sync_positions ();
    }
    void update_fontsize (void)
    {
      update_font ();
      sync_positions ();
    }
    void update_fontangle (void)
    {
      update_font ();
      sync_positions ();
    }
    void update_fontweight (void)
    {
      update_font ();
      sync_positions ();
    }

    void update_outerposition (void)
    {
      set_activepositionproperty ("outerposition");
      caseless_str old_units = get_units ();
      set_units ("normalized");
      Matrix outerbox = outerposition.get ().matrix_value ();
      Matrix innerbox = position.get ().matrix_value ();
      Matrix linset = looseinset.get ().matrix_value ();
      Matrix tinset = tightinset.get ().matrix_value ();
      outerbox(2) = outerbox(2) + outerbox(0);
      outerbox(3) = outerbox(3) + outerbox(1);
      innerbox(0) = outerbox(0) + std::max (linset(0), tinset(0));
      innerbox(1) = outerbox(1) + std::max (linset(1), tinset(1));
      innerbox(2) = outerbox(2) - std::max (linset(2), tinset(2));
      innerbox(3) = outerbox(3) - std::max (linset(3), tinset(3));
      innerbox(2) = innerbox(2) - innerbox(0);
      innerbox(3) = innerbox(3) - innerbox(1);
      position = innerbox;
      set_units (old_units);
      update_transform ();
    }

    void update_position (void)
    {
      set_activepositionproperty ("position");
      caseless_str old_units = get_units ();
      set_units ("normalized");
      Matrix outerbox = outerposition.get ().matrix_value ();
      Matrix innerbox = position.get ().matrix_value ();
      Matrix linset = looseinset.get ().matrix_value ();
      Matrix tinset = tightinset.get ().matrix_value ();
      innerbox(2) = innerbox(2) + innerbox(0);
      innerbox(3) = innerbox(3) + innerbox(1);
      outerbox(0) = innerbox(0) - std::max (linset(0), tinset(0));
      outerbox(1) = innerbox(1) - std::max (linset(1), tinset(1));
      outerbox(2) = innerbox(2) + std::max (linset(2), tinset(2));
      outerbox(3) = innerbox(3) + std::max (linset(3), tinset(3));
      outerbox(2) = outerbox(2) - outerbox(0);
      outerbox(3) = outerbox(3) - outerbox(1);
      outerposition = outerbox;
      set_units (old_units);
      update_transform ();
    }

    void update_looseinset (void)
    {
      caseless_str old_units = get_units ();
      set_units ("normalized");
      Matrix innerbox = position.get ().matrix_value ();
      innerbox(2) = innerbox(2) + innerbox(0);
      innerbox(3) = innerbox(3) + innerbox(1);
      Matrix outerbox = outerposition.get ().matrix_value ();
      outerbox(2) = outerbox(2) + outerbox(0);
      outerbox(3) = outerbox(3) + outerbox(1);
      Matrix linset = looseinset.get ().matrix_value ();
      Matrix tinset = tightinset.get ().matrix_value ();
      if (activepositionproperty.is ("position"))
        {
          outerbox(0) = innerbox(0) - std::max (linset(0), tinset(0));
          outerbox(1) = innerbox(1) - std::max (linset(1), tinset(1));
          outerbox(2) = innerbox(2) + std::max (linset(2), tinset(2));
          outerbox(3) = innerbox(3) + std::max (linset(3), tinset(3));
          outerbox(2) = outerbox(2) - outerbox(0);
          outerbox(3) = outerbox(3) - outerbox(1);
          outerposition = outerbox;
        }
      else
        {
          innerbox(0) = outerbox(0) + std::max (linset(0), tinset(0));
          innerbox(1) = outerbox(1) + std::max (linset(1), tinset(1));
          innerbox(2) = outerbox(2) - std::max (linset(2), tinset(2));
          innerbox(3) = outerbox(3) - std::max (linset(3), tinset(3));
          innerbox(2) = innerbox(2) - innerbox(0);
          innerbox(3) = innerbox(3) - innerbox(1);
          position = innerbox;
        }
      set_units (old_units);
      update_transform ();
    }

    double calc_tick_sep (double minval, double maxval);
    void calc_ticks_and_lims (array_property& lims, array_property& ticks,
                              array_property& mticks,
                              bool limmode_is_auto, bool is_logscale);
    void calc_ticklabels (const array_property& ticks, any_property& labels,
                          bool is_logscale);
    Matrix get_ticklabel_extents (const Matrix& ticks,
                                  const string_vector& ticklabels,
                                  const Matrix& limits);

    void fix_limits (array_property& lims)
    {
      if (lims.get ().is_empty ())
        return;

      Matrix l = lims.get ().matrix_value ();
      if (l(0) > l(1))
        {
          l(0) = 0;
          l(1) = 1;
          lims = l;
        }
      else if (l(0) == l(1))
        {
          l(0) -= 0.5;
          l(1) += 0.5;
          lims = l;
        }
    }

    Matrix calc_tightbox (const Matrix& init_pos);

  public:
    Matrix get_axis_limits (double xmin, double xmax,
                            double min_pos, double max_neg,
                            bool logscale);

    void update_xlim ()
    {
      if (xtickmode.is ("auto"))
        calc_ticks_and_lims (xlim, xtick, xmtick, xlimmode.is ("auto"),
                             xscale.is ("log"));
      if (xticklabelmode.is ("auto"))
        calc_ticklabels (xtick, xticklabel, xscale.is ("log"));

      fix_limits (xlim);

      update_xscale ();

      update_axes_layout ();
    }

    void update_ylim (void)
    {
      if (ytickmode.is ("auto"))
        calc_ticks_and_lims (ylim, ytick, ymtick, ylimmode.is ("auto"),
                             yscale.is ("log"));
      if (yticklabelmode.is ("auto"))
        calc_ticklabels (ytick, yticklabel, yscale.is ("log"));

      fix_limits (ylim);

      update_yscale ();

      update_axes_layout ();
    }

    void update_zlim (void)
    {
      if (ztickmode.is ("auto"))
        calc_ticks_and_lims (zlim, ztick, zmtick, zlimmode.is ("auto"),
                             zscale.is ("log"));
      if (zticklabelmode.is ("auto"))
        calc_ticklabels (ztick, zticklabel, zscale.is ("log"));

      fix_limits (zlim);

      update_zscale ();

      update_axes_layout ();
    }

  };

private:
  properties xproperties;

public:
  axes (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p), default_properties ()
  {
    xproperties.update_transform ();
  }

  ~axes (void) { }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    xproperties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      xproperties.set (name, value);
  }

  void set_defaults (const std::string& mode)
  {
    xproperties.set_defaults (*this, mode);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    // FIXME: finish this.
    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = xproperties.get (name);

    return retval;
  }

  octave_value get_default (const caseless_str& name) const;

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return default_properties;
  }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  void update_axis_limits (const std::string& axis_type);

  void update_axis_limits (const std::string& axis_type,
                           const graphics_handle& h);

  bool valid_object (void) const { return true; }

  void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

protected:
  void initialize (const graphics_object& go);

private:
  property_list default_properties;
};

// ---------------------------------------------------------------------

class OCTINTERP_API line : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (line)
      color_property color , color_values (0, 0, 0)
      string_property displayname , ""
      radio_property erasemode , "{normal}|none|xor|background"
      // FIXME: interpreter is not a property of Matlab line objects.
      //        Octave uses this for legend() with the string displayname.
      radio_property interpreter , "{tex}|none|latex"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      radio_property marker , "{none}|+|o|*|.|x|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram"
      color_property markeredgecolor , color_property (radio_values ("{auto}|none"), color_values (0, 0, 0))
      color_property markerfacecolor , color_property (radio_values ("auto|{none}"), color_values (0, 0, 0))
      double_property markersize , 6
      row_vector_property xdata u , default_data ()
      string_property xdatasource , ""
      row_vector_property ydata u , default_data ()
      string_property ydatasource , ""
      row_vector_property zdata u , Matrix ()
      string_property zdatasource , ""

      // hidden properties for limit computation
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "off"
    END_PROPERTIES

  private:
    Matrix compute_xlim (void) const;
    Matrix compute_ylim (void) const;

    void update_xdata (void) { set_xlim (compute_xlim ()); }

    void update_ydata (void) { set_ylim (compute_ylim ()); }

    void update_zdata (void)
    {
      set_zlim (zdata.get_limits ());
      set_zliminclude (get_zdata ().numel () > 0);
    }
  };

private:
  properties xproperties;

public:
  line (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~line (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API text : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    double get_fontsize_points (double box_pix_height = 0) const;

    void set_position (const octave_value& val)
    {
      if (! error_state)
        {
          octave_value new_val (val);

          if (new_val.numel () == 2)
            {
              dim_vector dv (1, 3);

              new_val = new_val.resize (dv, true);
            }

          if (position.set (new_val, false))
            {
              set_positionmode ("manual");
              update_position ();
              position.run_listeners (POSTSET);
              mark_modified ();
            }
          else
            set_positionmode ("manual");
        }
    }

    // See the genprops.awk script for an explanation of the
    // properties declarations.

    BEGIN_PROPERTIES (text)
      color_property backgroundcolor , color_property (radio_values ("{none}"), color_values (1, 1, 1))
      color_property color u , color_values (0, 0, 0)
      string_property displayname , ""
      color_property edgecolor , color_property (radio_values ("{none}"), color_values (0, 0, 0))
      bool_property editing , "off"
      radio_property erasemode , "{normal}|none|xor|background"
      array_property extent rG , Matrix (1, 4, 0.0)
      radio_property fontangle u , "{normal}|italic|oblique"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize u , 10
      radio_property fontunits SU , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight u , "light|{normal}|demi|bold"
      radio_property horizontalalignment mu , "{left}|center|right"
      radio_property interpreter u , "{tex}|none|latex"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      double_property margin , 2
      array_property position smu , Matrix (1, 3, 0.0)
      double_property rotation mu , 0
      text_label_property string u , ""
      radio_property units u , "{data}|pixels|normalized|inches|centimeters|points"
      radio_property verticalalignment mu , "top|cap|{middle}|baseline|bottom"

      // hidden properties for limit computation
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property xliminclude hl , "off"
      bool_property yliminclude hl , "off"
      bool_property zliminclude hl , "off"
      // hidden properties for auto-positioning
      radio_property positionmode hu , "{auto}|manual"
      radio_property rotationmode hu , "{auto}|manual"
      radio_property horizontalalignmentmode hu , "{auto}|manual"
      radio_property verticalalignmentmode hu , "{auto}|manual"
      radio_property autopos_tag h , "{none}|xlabel|ylabel|zlabel|title"
      // hidden property for text rendering
      double_property fontsize_points hgr , 0
    END_PROPERTIES

    Matrix get_data_position (void) const;
    Matrix get_extent_matrix (void) const;
    const uint8NDArray& get_pixels (void) const { return pixels; }
#if HAVE_FREETYPE
    // FreeType renderer, used for calculation of text size
    ft_render renderer;
#endif

  protected:
    void init (void)
    {
      position.add_constraint (dim_vector (1, 3));
      cached_units = get_units ();
      update_font ();
    }

  private:
    void update_position (void)
    {
      Matrix pos = get_data_position ();
      Matrix lim;

      lim = Matrix (1, 3, pos(0));
      lim(2) = (lim(2) <= 0 ? octave_Inf : lim(2));
      set_xlim (lim);

      lim = Matrix (1, 3, pos(1));
      lim(2) = (lim(2) <= 0 ? octave_Inf : lim(2));
      set_ylim (lim);

      if (pos.numel () == 3)
        {
          lim = Matrix (1, 3, pos(2));
          lim(2) = (lim(2) <= 0 ? octave_Inf : lim(2));
          set_zliminclude ("on");
          set_zlim (lim);
        }
      else
        set_zliminclude ("off");
    }

    void update_text_extent (void);

    void request_autopos (void);
    void update_positionmode (void) { request_autopos (); }
    void update_rotationmode (void) { request_autopos (); }
    void update_horizontalalignmentmode (void) { request_autopos (); }
    void update_verticalalignmentmode (void) { request_autopos (); }

    void update_font (void);
    void update_string (void) { request_autopos (); update_text_extent (); }
    void update_rotation (void) { update_text_extent (); }
    void update_color (void) { update_font (); update_text_extent (); }
    void update_fontname (void) { update_font (); update_text_extent (); }
    void update_fontsize (void) { update_font (); update_text_extent (); }
    void update_fontangle (void) { update_font (); update_text_extent (); }
    void update_fontweight (void) { update_font (); update_text_extent (); }
    void update_interpreter (void) { update_text_extent (); }
    void update_horizontalalignment (void) { update_text_extent (); }
    void update_verticalalignment (void) { update_text_extent (); }

    void update_units (void);
    void update_fontunits (const caseless_str& old_fontunits);

  private:
    std::string cached_units;
    uint8NDArray pixels;
  };

private:
  properties xproperties;

public:
  text (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  {
    xproperties.set_clipping ("off");
  }

  ~text (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API image : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    bool is_aliminclude (void) const
    { return (aliminclude.is_on () && alphadatamapping.is ("scaled")); }
    std::string get_aliminclude (void) const
    { return aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return (climinclude.is_on () && cdatamapping.is ("scaled")); }
    std::string get_climinclude (void) const
    { return climinclude.current_value (); }

    octave_value get_color_data (void) const;

    void initialize_data (void) { update_cdata (); }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (image)
      array_property alphadata u , Matrix (1, 1, 1.0)
      radio_property alphadatamapping al , "{none}|direct|scaled"
      array_property cdata u , default_image_cdata ()
      radio_property cdatamapping al , "scaled|{direct}"
      string_property displayname , ""
      radio_property erasemode , "{normal}|none|xor|background"
      row_vector_property xdata mu , Matrix ()
      row_vector_property ydata mu , Matrix ()
      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      radio_property xdatamode ha , "{auto}|manual"
      radio_property ydatamode ha , "{auto}|manual"
    END_PROPERTIES

  protected:
    void init (void)
    {
      xdata.add_constraint (2);
      xdata.add_constraint (dim_vector (0, 0));
      ydata.add_constraint (2);
      ydata.add_constraint (dim_vector (0, 0));
      cdata.add_constraint ("double");
      cdata.add_constraint ("single");
      cdata.add_constraint ("logical");
      cdata.add_constraint ("uint8");
      cdata.add_constraint ("uint16");
      cdata.add_constraint ("int16");
      cdata.add_constraint ("real");
      cdata.add_constraint (dim_vector (-1, -1));
      cdata.add_constraint (dim_vector (-1, -1, 3));
      alphadata.add_constraint (dim_vector (-1, -1));
      alphadata.add_constraint ("double");
      alphadata.add_constraint ("uint8");
    }

  private:
    void update_alphadata (void)
    {
      if (alphadatamapping_is ("scaled"))
        set_alim (alphadata.get_limits ());
      else
        alim = alphadata.get_limits ();
    }

    void update_cdata (void)
    {
      if (cdatamapping_is ("scaled"))
        set_clim (cdata.get_limits ());
      else
        clim = cdata.get_limits ();

      if (xdatamode.is ("auto"))
        update_xdata ();

      if (ydatamode.is ("auto"))
        update_ydata ();
    }

    void update_xdata (void)
    {
      if (xdata.get ().is_empty ())
        set_xdatamode ("auto");

      if (xdatamode.is ("auto"))
        {
          set_xdata (get_auto_xdata ());
          set_xdatamode ("auto");
        }

      Matrix limits = xdata.get_limits ();
      float dp = pixel_xsize ();

      limits(0) = limits(0) - dp;
      limits(1) = limits(1) + dp;
      set_xlim (limits);
    }

    void update_ydata (void)
    {
      if (ydata.get ().is_empty ())
        set_ydatamode ("auto");

      if (ydatamode.is ("auto"))
        {
          set_ydata (get_auto_ydata ());
          set_ydatamode ("auto");
        }

      Matrix limits = ydata.get_limits ();
      float dp = pixel_ysize ();

      limits(0) = limits(0) - dp;
      limits(1) = limits(1) + dp;
      set_ylim (limits);
    }

    Matrix get_auto_xdata (void)
    {
      dim_vector dv = get_cdata ().dims ();
      Matrix data;
      if (dv(1) > 0.)
        {
          data = Matrix (1, 2, 1);
          data(1) = dv(1);
        }
      return data;
    }

    Matrix get_auto_ydata (void)
    {
      dim_vector dv = get_cdata ().dims ();
      Matrix data;
      if (dv(0) > 0.)
        {
          data = Matrix (1, 2, 1);
          data(1) = dv(0);
        }
      return data;
    }

    float pixel_size (octave_idx_type dim, const Matrix limits)
    {
      octave_idx_type l = dim - 1;
      float dp;

      if (l > 0 && limits(0) != limits(1))
        dp = (limits(1) - limits(0))/(2*l);
      else
        {
          if (limits(1) == limits(2))
            dp = 0.5;
          else
            dp = (limits(1) - limits(0))/2;
        }
      return dp;
    }

  public:
    float  pixel_xsize (void)
    {
      return pixel_size ((get_cdata ().dims ())(1), xdata.get_limits ());
    }

    float pixel_ysize (void)
    {
      return pixel_size ((get_cdata ().dims ())(0), ydata.get_limits ());
    }
  };

private:
  properties xproperties;

public:
  image (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  {
    xproperties.initialize_data ();
  }

  ~image (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API patch : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    octave_value get_color_data (void) const;

    // Matlab allows incoherent data to be stored into patch properties.
    // The patch should then be ignored by the renderer.
    bool has_bad_data (std::string &msg) const
    {
      msg = bad_data_msg;
      return ! msg.empty ();
    }

    bool is_aliminclude (void) const
    { return (aliminclude.is_on () && alphadatamapping.is ("scaled")); }
    std::string get_aliminclude (void) const
    { return aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return (climinclude.is_on () && cdatamapping.is ("scaled")); }
    std::string get_climinclude (void) const
    { return climinclude.current_value (); }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (patch)
      radio_property alphadatamapping l , "none|{scaled}|direct"
      double_property ambientstrength , 0.3
      radio_property backfacelighting , "unlit|lit|{reverselit}"
      array_property cdata u , Matrix ()
      radio_property cdatamapping l , "{scaled}|direct"
      double_property diffusestrength , 0.6
      string_property displayname , ""
      double_radio_property edgealpha , double_radio_property (1.0, radio_values ("flat|interp"))
      color_property edgecolor , color_property (color_values (0, 0, 0), radio_values ("none|flat|interp"))
      radio_property edgelighting , "{none}|flat|gouraud|phong"
      radio_property erasemode , "{normal}|background|xor|none"
      double_radio_property facealpha , double_radio_property (1.0, radio_values ("flat|interp"))
      color_property facecolor , color_property (color_values (0, 0, 0), radio_values ("none|flat|interp"))
      radio_property facelighting , "{none}|flat|gouraud|phong"
      array_property faces u , default_patch_faces ()
      array_property facevertexalphadata , Matrix ()
      array_property facevertexcdata u , Matrix ()
      // FIXME: interpreter is not a property of a Matlab patch.
      //        Octave uses this for legend() with the string displayname.
      radio_property interpreter , "{tex}|none|latex"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      radio_property marker , "{none}|+|o|*|.|x|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram"
      color_property markeredgecolor , color_property (radio_values ("none|{auto}|flat"), color_values (0, 0, 0))
      color_property markerfacecolor , color_property (radio_values ("{none}|auto|flat"), color_values (0, 0, 0))
      double_property markersize , 6
      radio_property normalmode , "{auto}|manual"
      double_property specularcolorreflectance , 1.0
      double_property specularexponent , 10.0
      double_property specularstrength , 0.9
      array_property vertexnormals , Matrix ()
      array_property vertices u , default_patch_vertices ()
      array_property xdata u , default_patch_xdata ()
      array_property ydata u , default_patch_ydata ()
      array_property zdata u , Matrix ()

      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "on"
    END_PROPERTIES

  protected:
    void init (void)
    {
      xdata.add_constraint (dim_vector (-1, -1));
      ydata.add_constraint (dim_vector (-1, -1));
      zdata.add_constraint (dim_vector (-1, -1));
      faces.add_constraint (dim_vector (-1, -1));
      vertices.add_constraint (dim_vector (-1, 2));
      vertices.add_constraint (dim_vector (-1, 3));
      cdata.add_constraint (dim_vector (-1, -1));
      cdata.add_constraint (dim_vector (-1, -1, 3));
      facevertexcdata.add_constraint (dim_vector (-1, 1));
      facevertexcdata.add_constraint (dim_vector (-1, 3));
      facevertexalphadata.add_constraint (dim_vector (-1, 1));
      vertexnormals.add_constraint (dim_vector (-1, -1));
    }

  private:
    std::string bad_data_msg;

    void update_faces (void) { update_data ();}

    void update_vertices (void)  {  update_data ();}

    void update_facevertexcdata (void) { update_data ();}

    void update_fvc (void);

    void update_xdata (void)
    {
      if (get_xdata ().is_empty ())
        {
          // For compatibility with matlab behavior,
          // if x/ydata are set empty, silently empty other *data and
          // faces properties while vertices remain unchanged.
          set_ydata (Matrix ());
          set_zdata (Matrix ());
          set_cdata (Matrix ());
          set_faces (Matrix ());
        }
      else
        update_fvc ();

      set_xlim (xdata.get_limits ());
    }

    void update_ydata (void)
    {
      if (get_ydata ().is_empty ())
        {
          set_xdata (Matrix ());
          set_zdata (Matrix ());
          set_cdata (Matrix ());
          set_faces (Matrix ());
        }
      else
        update_fvc ();

      set_ylim (ydata.get_limits ());
    }

    void update_zdata (void)
    {
      update_fvc ();
      set_zlim (zdata.get_limits ());
    }

    void update_cdata (void)
    {
      update_fvc ();

      if (cdatamapping_is ("scaled"))
        set_clim (cdata.get_limits ());
      else
        clim = cdata.get_limits ();
    }


    void update_data (void);
  };

private:
  properties xproperties;

public:
  patch (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~patch (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API surface : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    octave_value get_color_data (void) const;

    bool is_aliminclude (void) const
    { return (aliminclude.is_on () && alphadatamapping.is ("scaled")); }
    std::string get_aliminclude (void) const
    { return aliminclude.current_value (); }

    bool is_climinclude (void) const
    { return (climinclude.is_on () && cdatamapping.is ("scaled")); }
    std::string get_climinclude (void) const
    { return climinclude.current_value (); }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (surface)
      array_property alphadata u , Matrix (1, 1, 1.0)
      radio_property alphadatamapping l , "none|direct|{scaled}"
      double_property ambientstrength , 0.3
      radio_property backfacelighting , "unlit|lit|{reverselit}"
      array_property cdata u , default_surface_cdata ()
      radio_property cdatamapping al , "{scaled}|direct"
      string_property cdatasource , ""
      double_property diffusestrength , 0.6
      string_property displayname , ""
      double_radio_property edgealpha , double_radio_property (1.0, radio_values ("flat|interp"))
      color_property edgecolor , color_property (color_values (0, 0, 0), radio_values ("none|flat|interp"))
      radio_property edgelighting , "{none}|flat|gouraud|phong"
      radio_property erasemode , "{normal}|none|xor|background"
      double_radio_property facealpha , double_radio_property (1.0, radio_values ("flat|interp|texturemap"))
      color_property facecolor , color_property (radio_values ("none|{flat}|interp|texturemap"), color_values (0, 0, 0))
      radio_property facelighting , "{none}|flat|gouraud|phong"
      // FIXME: interpreter is not a Matlab surface property
      //        Octave uses this for legend() with the string displayname.
      radio_property interpreter , "{tex}|none|latex"
      radio_property linestyle , "{-}|--|:|-.|none"
      double_property linewidth , 0.5
      radio_property marker , "{none}|+|o|*|.|x|s|square|d|diamond|^|v|>|<|p|pentagram|h|hexagram"
      color_property markeredgecolor , color_property (radio_values ("none|{auto}|flat"), color_values (0, 0, 0))
      color_property markerfacecolor , color_property (radio_values ("{none}|auto|flat"), color_values (0, 0, 0))
      double_property markersize , 6
      radio_property meshstyle , "{both}|row|column"
      radio_property normalmode u , "{auto}|manual"
      double_property specularcolorreflectance , 1
      double_property specularexponent , 10
      double_property specularstrength , 0.9
      array_property vertexnormals u , Matrix ()
      array_property xdata u , default_surface_xdata ()
      string_property xdatasource , ""
      array_property ydata u , default_surface_ydata ()
      string_property ydatasource , ""
      array_property zdata u , default_surface_zdata ()
      string_property zdatasource , ""

      // hidden properties for limit computation
      row_vector_property alim hlr , Matrix ()
      row_vector_property clim hlr , Matrix ()
      row_vector_property xlim hlr , Matrix ()
      row_vector_property ylim hlr , Matrix ()
      row_vector_property zlim hlr , Matrix ()
      bool_property aliminclude hlg , "on"
      bool_property climinclude hlg , "on"
      bool_property xliminclude hl , "on"
      bool_property yliminclude hl , "on"
      bool_property zliminclude hl , "on"
    END_PROPERTIES

  protected:
    void init (void)
    {
      xdata.add_constraint (dim_vector (-1, -1));
      ydata.add_constraint (dim_vector (-1, -1));
      zdata.add_constraint (dim_vector (-1, -1));
      cdata.add_constraint ("double");
      cdata.add_constraint ("single");
      cdata.add_constraint (dim_vector (-1, -1));
      cdata.add_constraint (dim_vector (-1, -1, 3));
      alphadata.add_constraint ("double");
      alphadata.add_constraint ("uint8");
      alphadata.add_constraint (dim_vector (-1, -1));
      vertexnormals.add_constraint (dim_vector (-1, -1, 3));
      vertexnormals.add_constraint (dim_vector (0, 0));
    }

  private:
    void update_alphadata (void)
    {
      if (alphadatamapping_is ("scaled"))
        set_alim (alphadata.get_limits ());
      else
        alim = alphadata.get_limits ();
    }

    void update_cdata (void)
    {
      if (cdatamapping_is ("scaled"))
        set_clim (cdata.get_limits ());
      else
        clim = cdata.get_limits ();
    }

    void update_xdata (void)
    {
      update_normals ();
      set_xlim (xdata.get_limits ());
    }

    void update_ydata (void)
    {
      update_normals ();
      set_ylim (ydata.get_limits ());
    }

    void update_zdata (void)
    {
      update_normals ();
      set_zlim (zdata.get_limits ());
    }

    void update_normals (void);

    void update_normalmode (void)
    { update_normals (); }

    void update_vertexnormals (void)
    { set_normalmode ("manual"); }
  };

private:
  properties xproperties;

public:
  surface (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~surface (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API hggroup : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    void remove_child (const graphics_handle& h)
    {
      base_properties::remove_child (h);
      update_limits ();
    }

    void adopt (const graphics_handle& h)
    {

      base_properties::adopt (h);
      update_limits (h);
    }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (hggroup)
      string_property displayname , ""
      radio_property erasemode , "{normal}|none|xor|background"

      // hidden properties for limit computation
      row_vector_property alim hr , Matrix ()
      row_vector_property clim hr , Matrix ()
      row_vector_property xlim hr , Matrix ()
      row_vector_property ylim hr , Matrix ()
      row_vector_property zlim hr , Matrix ()
      bool_property aliminclude h , "on"
      bool_property climinclude h , "on"
      bool_property xliminclude h , "on"
      bool_property yliminclude h , "on"
      bool_property zliminclude h , "on"
    END_PROPERTIES

  private:
    void update_limits (void) const;

    void update_limits (const graphics_handle& h) const;

  protected:
    void init (void)
    { }

  };

private:
  properties xproperties;

public:
  hggroup (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~hggroup (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  void update_axis_limits (const std::string& axis_type);

  void update_axis_limits (const std::string& axis_type,
                           const graphics_handle& h);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uimenu : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    void remove_child (const graphics_handle& h)
    {
      base_properties::remove_child (h);
    }

    void adopt (const graphics_handle& h)
    {
      base_properties::adopt (h);
    }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uimenu)
      any_property __object__ , Matrix ()
      string_property accelerator , ""
      callback_property callback , Matrix ()
      bool_property checked , "off"
      bool_property enable , "on"
      color_property foregroundcolor , color_values (0, 0, 0)
      string_property label , ""
      double_property position , 0
      bool_property separator , "off"
      // Octave-specific properties
      string_property fltk_label h , ""
    END_PROPERTIES

  protected:
    void init (void)
    { }
  };

private:
  properties xproperties;

public:
  uimenu (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~uimenu (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uicontextmenu : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:

    void add_dependent_obj (graphics_handle gh)
    { dependent_obj_list.push_back (gh); }

    // FIXME: the list may contain duplicates.
    //        Should we return only unique elements?
    const std::list<graphics_handle> get_dependent_obj_list (void)
    { return dependent_obj_list; }

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uicontextmenu)
      any_property __object__ , Matrix ()
      callback_property callback , Matrix ()
      array_property position , Matrix (1, 2, 0.0)
    END_PROPERTIES

  protected:
    void init (void)
    {
      position.add_constraint (dim_vector (1, 2));
      position.add_constraint (dim_vector (2, 1));
      visible.set (octave_value (false));
    }

  private:
    // List of objects that might depend on this uicontextmenu object
    std::list<graphics_handle> dependent_obj_list;
  };

private:
  properties xproperties;

public:
  uicontextmenu (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~uicontextmenu (void);

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uicontrol : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    Matrix get_boundingbox (bool internal = false,
                            const Matrix& parent_pix_size = Matrix ()) const;

    double get_fontsize_points (double box_pix_height = 0) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uicontrol)
      any_property __object__ , Matrix ()
      color_property backgroundcolor , color_values (1, 1, 1)
      callback_property callback , Matrix ()
      array_property cdata , Matrix ()
      bool_property clipping , "on"
      radio_property enable , "{on}|inactive|off"
      array_property extent rG , Matrix (1, 4, 0.0)
      radio_property fontangle u , "{normal}|italic|oblique"
      string_property fontname u , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize u , 10
      radio_property fontunits S , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight u , "light|{normal}|demi|bold"
      color_property foregroundcolor , color_values (0, 0, 0)
      radio_property horizontalalignment , "left|{center}|right"
      callback_property keypressfcn , Matrix ()
      double_property listboxtop , 1
      double_property max , 1
      double_property min , 0
      array_property position , default_control_position ()
      array_property sliderstep , default_control_sliderstep ()
      string_array_property string u , ""
      radio_property style S , "{pushbutton}|togglebutton|radiobutton|checkbox|edit|text|slider|frame|listbox|popupmenu"
      string_property tooltipstring , ""
      radio_property units u , "normalized|inches|centimeters|points|{pixels}|characters"
      row_vector_property value , Matrix (1, 1, 1.0)
      radio_property verticalalignment , "top|{middle}|bottom"
    END_PROPERTIES

  private:
    std::string cached_units;

  protected:
    void init (void)
    {
      cdata.add_constraint ("double");
      cdata.add_constraint ("single");
      cdata.add_constraint (dim_vector (-1, -1, 3));
      position.add_constraint (dim_vector (1, 4));
      sliderstep.add_constraint (dim_vector (1, 2));
      cached_units = get_units ();
    }

    void update_text_extent (void);

    void update_string (void) { update_text_extent (); }
    void update_fontname (void) { update_text_extent (); }
    void update_fontsize (void) { update_text_extent (); }
    void update_fontangle (void) { update_text_extent (); }
    void update_fontweight (void) { update_text_extent (); }
    void update_fontunits (const caseless_str& old_units);

    void update_units (void);

  };

private:
  properties xproperties;

public:
  uicontrol (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~uicontrol (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API uipanel : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    Matrix get_boundingbox (bool internal = false,
                            const Matrix& parent_pix_size = Matrix ()) const;

    double get_fontsize_points (double box_pix_height = 0) const;

    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uipanel)
      any_property __object__ , Matrix ()
      color_property backgroundcolor , color_values (1, 1, 1)
      radio_property bordertype , "none|{etchedin}|etchedout|beveledin|beveledout|line"
      double_property borderwidth , 1
      radio_property fontangle , "{normal}|italic|oblique"
      string_property fontname , OCTAVE_DEFAULT_FONTNAME
      double_property fontsize , 10
      radio_property fontunits S , "inches|centimeters|normalized|{points}|pixels"
      radio_property fontweight , "light|{normal}|demi|bold"
      color_property foregroundcolor , color_values (0, 0, 0)
      color_property highlightcolor , color_values (1, 1, 1)
      array_property position , default_panel_position ()
      callback_property resizefcn , Matrix ()
      color_property shadowcolor , color_values (0, 0, 0)
      string_property title , ""
      radio_property titleposition , "{lefttop}|centertop|righttop|leftbottom|centerbottom|rightbottom"
      radio_property units S , "{normalized}|inches|centimeters|points|pixels|characters"
    END_PROPERTIES

  protected:
    void init (void)
    {
      position.add_constraint (dim_vector (1, 4));
    }

    void update_units (const caseless_str& old_units);
    void update_fontunits (const caseless_str& old_units);

  };

private:
  properties xproperties;

public:
  uipanel (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~uipanel (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }
};

// ---------------------------------------------------------------------

class OCTINTERP_API uitoolbar : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uitoolbar)
      any_property __object__ , Matrix ()
    END_PROPERTIES

  protected:
    void init (void)
    { }
  };

private:
  properties xproperties;

public:
  uitoolbar (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p), default_properties ()
  { }

  ~uitoolbar (void) { }

  void override_defaults (base_graphics_object& obj)
  {
    // Allow parent (figure) to override first (properties knows how
    // to find the parent object).
    xproperties.override_defaults (obj);

    // Now override with our defaults.  If the default_properties
    // list includes the properties for all defaults (line,
    // surface, etc.) then we don't have to know the type of OBJ
    // here, we just call its set function and let it decide which
    // properties from the list to use.
    obj.set_from_list (default_properties);
  }

  void set (const caseless_str& name, const octave_value& value)
  {
    if (name.compare ("default", 7))
      // strip "default", pass rest to function that will
      // parse the remainder and add the element to the
      // default_properties map.
      default_properties.set (name.substr (7), value);
    else
      xproperties.set (name, value);
  }

  octave_value get (const caseless_str& name) const
  {
    octave_value retval;

    if (name.compare ("default", 7))
      retval = get_default (name.substr (7));
    else
      retval = xproperties.get (name);

    return retval;
  }

  octave_value get_default (const caseless_str& name) const;

  octave_value get_defaults (void) const
  {
    return default_properties.as_struct ("default");
  }

  property_list get_defaults_list (void) const
  {
    return default_properties;
  }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  void reset_default_properties (void);

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

private:
  property_list default_properties;
};

// ---------------------------------------------------------------------

class OCTINTERP_API uipushtool : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uipushtool)
      any_property __object__ , Matrix ()
      array_property cdata , Matrix ()
      callback_property clickedcallback , Matrix ()
      bool_property enable , "on"
      bool_property separator , "off"
      string_property tooltipstring , ""
    END_PROPERTIES

  protected:
    void init (void)
    {
      cdata.add_constraint ("double");
      cdata.add_constraint ("single");
      cdata.add_constraint (dim_vector (-1, -1, 3));
    }
  };

private:
  properties xproperties;

public:
  uipushtool (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~uipushtool (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

class OCTINTERP_API uitoggletool : public base_graphics_object
{
public:
  class OCTINTERP_API properties : public base_properties
  {
  public:
    // See the genprops.awk script for an explanation of the
    // properties declarations.
    // Programming note: Keep property list sorted if new ones are added.

    BEGIN_PROPERTIES (uitoggletool)
      any_property __object__ , Matrix ()
      array_property cdata , Matrix ()
      callback_property clickedcallback , Matrix ()
      bool_property enable , "on"
      callback_property offcallback , Matrix ()
      callback_property oncallback , Matrix ()
      bool_property separator , "off"
      bool_property state , "off"
      string_property tooltipstring , ""
    END_PROPERTIES

  protected:
    void init (void)
    {
      cdata.add_constraint ("double");
      cdata.add_constraint ("single");
      cdata.add_constraint (dim_vector (-1, -1, 3));
    }
  };

private:
  properties xproperties;

public:
  uitoggletool (const graphics_handle& mh, const graphics_handle& p)
    : base_graphics_object (), xproperties (mh, p)
  { }

  ~uitoggletool (void) { }

  base_properties& get_properties (void) { return xproperties; }

  const base_properties& get_properties (void) const { return xproperties; }

  bool valid_object (void) const { return true; }

  bool has_readonly_property (const caseless_str& pname) const
  {
    bool retval = xproperties.has_readonly_property (pname);
    if (! retval)
      retval = base_properties::has_readonly_property (pname);
    return retval;
  }

};

// ---------------------------------------------------------------------

octave_value
get_property_from_handle (double handle, const std::string& property,
                          const std::string& func);
bool
set_property_in_handle (double handle, const std::string& property,
                        const octave_value& arg, const std::string& func);

// ---------------------------------------------------------------------

class graphics_event;

class
base_graphics_event
{
public:
  friend class graphics_event;

  base_graphics_event (void) : count (1) { }

  virtual ~base_graphics_event (void) { }

  virtual void execute (void) = 0;

private:
  octave_refcount<int> count;
};

class
graphics_event
{
public:
  typedef void (*event_fcn) (void*);

  graphics_event (void) : rep (0) { }

  graphics_event (const graphics_event& e) : rep (e.rep)
  {
    rep->count++;
  }

  ~graphics_event (void)
  {
    if (rep && --rep->count == 0)
      delete rep;
  }

  graphics_event& operator = (const graphics_event& e)
  {
    if (rep != e.rep)
      {
        if (rep && --rep->count == 0)
          delete rep;

        rep = e.rep;
        if (rep)
          rep->count++;
      }

    return *this;
  }

  void execute (void)
  { if (rep) rep->execute (); }

  bool ok (void) const
  { return (rep != 0); }

  static graphics_event
  create_callback_event (const graphics_handle& h,
                         const std::string& name,
                         const octave_value& data = Matrix ());

  static graphics_event
  create_callback_event (const graphics_handle& h,
                         const octave_value& cb,
                         const octave_value& data = Matrix ());

  static graphics_event
  create_function_event (event_fcn fcn, void *data = 0);

  static graphics_event
  create_set_event (const graphics_handle& h, const std::string& name,
                    const octave_value& value,
                    bool notify_toolkit = true);
private:
  base_graphics_event *rep;
};

class OCTINTERP_API gh_manager
{
protected:

  gh_manager (void);

public:

  static void create_instance (void);

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      create_instance ();

    if (! instance)
      {
        ::error ("unable to create gh_manager!");

        retval = false;
      }

    return retval;
  }

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static graphics_handle get_handle (bool integer_figure_handle)
  {
    return instance_ok ()
           ? instance->do_get_handle (integer_figure_handle)
           : graphics_handle ();
  }

  static void free (const graphics_handle& h)
  {
    if (instance_ok ())
      instance->do_free (h);
  }

  static void renumber_figure (const graphics_handle& old_gh,
                               const graphics_handle& new_gh)
  {
    if (instance_ok ())
      instance->do_renumber_figure (old_gh, new_gh);
  }

  static graphics_handle lookup (double val)
  {
    return instance_ok () ? instance->do_lookup (val) : graphics_handle ();
  }

  static graphics_handle lookup (const octave_value& val)
  {
    return val.is_real_scalar ()
           ? lookup (val.double_value ()) : graphics_handle ();
  }

  static graphics_object get_object (double val)
  {
    return get_object (lookup (val));
  }

  static graphics_object get_object (const graphics_handle& h)
  {
    return instance_ok () ? instance->do_get_object (h) : graphics_object ();
  }

  static graphics_handle
  make_graphics_handle (const std::string& go_name,
                        const graphics_handle& parent,
                        bool integer_figure_handle = false,
                        bool do_createfcn = true,
                        bool do_notify_toolkit = true)
  {
    return instance_ok ()
           ? instance->do_make_graphics_handle (go_name, parent,
               integer_figure_handle,
               do_createfcn, do_notify_toolkit)
           : graphics_handle ();
  }

  static graphics_handle make_figure_handle (double val,
                                             bool do_notify_toolkit = true)
  {
    return instance_ok ()
           ? instance->do_make_figure_handle (val, do_notify_toolkit)
           : graphics_handle ();
  }

  static void push_figure (const graphics_handle& h)
  {
    if (instance_ok ())
      instance->do_push_figure (h);
  }

  static void pop_figure (const graphics_handle& h)
  {
    if (instance_ok ())
      instance->do_pop_figure (h);
  }

  static graphics_handle current_figure (void)
  {
    return instance_ok ()
           ? instance->do_current_figure () : graphics_handle ();
  }

  static Matrix handle_list (bool show_hidden = false)
  {
    return instance_ok ()
           ? instance->do_handle_list (show_hidden) : Matrix ();
  }

  static void lock (void)
  {
    if (instance_ok ())
      instance->do_lock ();
  }

  static bool try_lock (void)
  {
    if (instance_ok ())
      return instance->do_try_lock ();
    else
      return false;
  }

  static void unlock (void)
  {
    if (instance_ok ())
      instance->do_unlock ();
  }

  static Matrix figure_handle_list (bool show_hidden = false)
  {
    return instance_ok ()
           ? instance->do_figure_handle_list (show_hidden) : Matrix ();
  }

  static void execute_listener (const graphics_handle& h,
                                const octave_value& l)
  {
    if (instance_ok ())
      instance->do_execute_listener (h, l);
  }

  static void execute_callback (const graphics_handle& h,
                                const std::string& name,
                                const octave_value& data = Matrix ())
  {
    octave_value cb;

    if (true)
      {
        gh_manager::auto_lock lock;

        graphics_object go = get_object (h);

        if (go.valid_object ())
          cb = go.get (name);
      }

    if (! error_state)
      execute_callback (h, cb, data);
  }

  static void execute_callback (const graphics_handle& h,
                                const octave_value& cb,
                                const octave_value& data = Matrix ())
  {
    if (instance_ok ())
      instance->do_execute_callback (h, cb, data);
  }

  static void post_callback (const graphics_handle& h,
                             const std::string& name,
                             const octave_value& data = Matrix ())
  {
    if (instance_ok ())
      instance->do_post_callback (h, name, data);
  }

  static void post_function (graphics_event::event_fcn fcn, void* data = 0)
  {
    if (instance_ok ())
      instance->do_post_function (fcn, data);
  }

  static void post_set (const graphics_handle& h, const std::string& name,
                        const octave_value& value, bool notify_toolkit = true)
  {
    if (instance_ok ())
      instance->do_post_set (h, name, value, notify_toolkit);
  }

  static int process_events (void)
  {
    return (instance_ok () ?  instance->do_process_events () : 0);
  }

  static int flush_events (void)
  {
    return (instance_ok () ?  instance->do_process_events (true) : 0);
  }

  static void enable_event_processing (bool enable = true)
  {
    if (instance_ok ())
      instance->do_enable_event_processing (enable);
  }

  static bool is_handle_visible (const graphics_handle& h)
  {
    bool retval = false;

    graphics_object go = get_object (h);

    if (go.valid_object ())
      retval = go.is_handle_visible ();

    return retval;
  }

  static void close_all_figures (void)
  {
    if (instance_ok ())
      instance->do_close_all_figures ();
  }

public:
  class auto_lock : public octave_autolock
  {
  public:
    auto_lock (bool wait = true)
      : octave_autolock (instance_ok ()
                         ? instance->graphics_lock
                         : octave_mutex (),
                         wait)
    { }

  private:

    // No copying!
    auto_lock (const auto_lock&);
    auto_lock& operator = (const auto_lock&);
  };

private:

  static gh_manager *instance;

  typedef std::map<graphics_handle, graphics_object>::iterator iterator;
  typedef std::map<graphics_handle, graphics_object>::const_iterator
    const_iterator;

  typedef std::set<graphics_handle>::iterator free_list_iterator;
  typedef std::set<graphics_handle>::const_iterator const_free_list_iterator;

  typedef std::list<graphics_handle>::iterator figure_list_iterator;
  typedef std::list<graphics_handle>::const_iterator const_figure_list_iterator;

  // A map of handles to graphics objects.
  std::map<graphics_handle, graphics_object> handle_map;

  // The available graphics handles.
  std::set<graphics_handle> handle_free_list;

  // The next handle available if handle_free_list is empty.
  double next_handle;

  // The allocated figure handles.  Top of the stack is most recently
  // created.
  std::list<graphics_handle> figure_list;

  // The lock for accessing the graphics sytsem.
  octave_mutex graphics_lock;

  // The list of events queued by graphics toolkits.
  std::list<graphics_event> event_queue;

  // The stack of callback objects.
  std::list<graphics_object> callback_objects;

  // A flag telling whether event processing must be constantly on.
  int event_processing;

  graphics_handle do_get_handle (bool integer_figure_handle);

  void do_free (const graphics_handle& h);

  void do_renumber_figure (const graphics_handle& old_gh,
                           const graphics_handle& new_gh);

  graphics_handle do_lookup (double val)
  {
    iterator p = (xisnan (val) ? handle_map.end () : handle_map.find (val));

    return (p != handle_map.end ()) ? p->first : graphics_handle ();
  }

  graphics_object do_get_object (const graphics_handle& h)
  {
    iterator p = (h.ok () ? handle_map.find (h) : handle_map.end ());

    return (p != handle_map.end ()) ? p->second : graphics_object ();
  }

  graphics_handle do_make_graphics_handle (const std::string& go_name,
                                           const graphics_handle& p,
                                           bool integer_figure_handle,
                                           bool do_createfcn,
                                           bool do_notify_toolkit);

  graphics_handle do_make_figure_handle (double val, bool do_notify_toolkit);

  Matrix do_handle_list (bool show_hidden)
  {
    Matrix retval (1, handle_map.size ());

    octave_idx_type i = 0;
    for (const_iterator p = handle_map.begin (); p != handle_map.end (); p++)
      {
        graphics_handle h = p->first;

        if (show_hidden || is_handle_visible (h))
          retval(i++) = h.value ();
      }

    retval.resize (1, i);

    return retval;
  }

  Matrix do_figure_handle_list (bool show_hidden)
  {
    Matrix retval (1, figure_list.size ());

    octave_idx_type i = 0;
    for (const_figure_list_iterator p = figure_list.begin ();
         p != figure_list.end ();
         p++)
      {
        graphics_handle h = *p;

        if (show_hidden || is_handle_visible (h))
          retval(i++) = h.value ();
      }

    retval.resize (1, i);

    return retval;
  }

  void do_push_figure (const graphics_handle& h);

  void do_pop_figure (const graphics_handle& h);

  graphics_handle do_current_figure (void) const
  {
    graphics_handle retval;

    for (const_figure_list_iterator p = figure_list.begin ();
         p != figure_list.end ();
         p++)
      {
        graphics_handle h = *p;

        if (is_handle_visible (h))
          retval = h;
      }

    return retval;
  }

  void do_lock (void) { graphics_lock.lock (); }

  bool do_try_lock (void) { return graphics_lock.try_lock (); }

  void do_unlock (void) { graphics_lock.unlock (); }

  void do_execute_listener (const graphics_handle& h, const octave_value& l);

  void do_execute_callback (const graphics_handle& h, const octave_value& cb,
                            const octave_value& data);

  void do_post_callback (const graphics_handle& h, const std::string& name,
                         const octave_value& data);

  void do_post_function (graphics_event::event_fcn fcn, void* fcn_data);

  void do_post_set (const graphics_handle& h, const std::string& name,
                    const octave_value& value, bool notify_toolkit = true);

  int do_process_events (bool force = false);

  void do_close_all_figures (void);

  static void restore_gcbo (void)
  {
    if (instance_ok ())
      instance->do_restore_gcbo ();
  }

  void do_restore_gcbo (void);

  void do_post_event (const graphics_event& e);

  void do_enable_event_processing (bool enable = true);
};

void get_children_limits (double& min_val, double& max_val,
                          double& min_pos, double& max_neg,
                          const Matrix& kids, char limit_type);

OCTINTERP_API int calc_dimensions (const graphics_object& gh);

// This function is NOT equivalent to the scripting language function gcf.
OCTINTERP_API graphics_handle gcf (void);

// This function is NOT equivalent to the scripting language function gca.
OCTINTERP_API graphics_handle gca (void);

OCTINTERP_API void close_all_figures (void);

#endif
