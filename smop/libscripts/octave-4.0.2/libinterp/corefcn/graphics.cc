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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <cfloat>
#include <cstdlib>
#include <ctime>

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <string>
#include <sstream>

#include "cmd-edit.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-locbuf.h"
#include "singleton-cleanup.h"

#include "builtins.h"
#include "cutils.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "graphics.h"
#include "input.h"
#include "ov.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-fcn-handle.h"
#include "pager.h"
#include "parse.h"
#include "toplev.h"
#include "txt-eng-ft.h"
#include "unwind-prot.h"
#include "octave-default-image.h"

// forward declarations
static octave_value xget (const graphics_handle& h, const caseless_str& name);

static void
gripe_set_invalid (const std::string& pname)
{
  error ("set: invalid value for %s property", pname.c_str ());
}

// Check to see that PNAME matches just one of PNAMES uniquely.
// Return the full name of the match, or an empty caseless_str object
// if there is no match, or the match is ambiguous.

static caseless_str
validate_property_name (const std::string& who, const std::string& what,
                        const std::set<std::string>& pnames,
                        const caseless_str& pname)
{
  size_t len = pname.length ();
  std::set<std::string> matches;

  for (std::set<std::string>::const_iterator p = pnames.begin ();
       p != pnames.end (); p++)
    {
      if (pname.compare (*p, len))
        {
          if (len == p->length ())
            {
              // Exact match.
              return pname;
            }

          matches.insert (*p);
        }
    }

  size_t num_matches = matches.size ();

  if (num_matches == 0)
    {
      error ("%s: unknown %s property %s",
             who.c_str (), what.c_str (), pname.c_str ());
    }
  else if (num_matches > 1)
    {
      string_vector sv (matches);

      std::ostringstream os;

      sv.list_in_columns (os);

      std::string match_list = os.str ();

      error ("%s: ambiguous %s property name %s; possible matches:\n\n%s",
             who.c_str (), what.c_str (), pname.c_str (), match_list.c_str ());
    }
  else if (num_matches == 1)
    {
      // Exact match was handled above.

      std::string possible_match = *(matches.begin ());

      warning_with_id ("Octave:abbreviated-property-match",
                       "%s: allowing %s to match %s property %s",
                       who.c_str (), pname.c_str (), what.c_str (),
                       possible_match.c_str ());

      return possible_match;
    }

  return caseless_str ();
}

static Matrix
jet_colormap (void)
{
  Matrix cmap (64, 3, 0.0);

  // Produce X in the same manner as linspace so that
  // jet_colormap and jet.m produce *exactly* the same result.
  double delta = 1.0 / 63.0;

  for (octave_idx_type i = 0; i < 64; i++)
    {
      // This is the jet colormap.  It would be nice to be able
      // to feval the jet function but since there is a static
      // property object that includes a colormap_property
      // object, we need to initialize this before main is even
      // called, so calling an interpreted function is not
      // possible.

      double x = i*delta;

      if (x >= 3.0/8.0 && x < 5.0/8.0)
        cmap(i,0) = 4.0 * x - 3.0/2.0;
      else if (x >= 5.0/8.0 && x < 7.0/8.0)
        cmap(i,0) = 1.0;
      else if (x >= 7.0/8.0)
        cmap(i,0) = -4.0 * x + 9.0/2.0;

      if (x >= 1.0/8.0 && x < 3.0/8.0)
        cmap(i,1) = 4.0 * x - 1.0/2.0;
      else if (x >= 3.0/8.0 && x < 5.0/8.0)
        cmap(i,1) = 1.0;
      else if (x >= 5.0/8.0 && x < 7.0/8.0)
        cmap(i,1) = -4.0 * x + 7.0/2.0;

      if (x < 1.0/8.0)
        cmap(i,2) = 4.0 * x + 1.0/2.0;
      else if (x >= 1.0/8.0 && x < 3.0/8.0)
        cmap(i,2) = 1.0;
      else if (x >= 3.0/8.0 && x < 5.0/8.0)
        cmap(i,2) = -4.0 * x + 5.0/2.0;
    }

  return cmap;
}

static double
default_screendepth (void)
{
  return display_info::depth ();
}

static Matrix
default_screensize (void)
{
  Matrix retval (1, 4, 1.0);

  retval(2) = display_info::width ();
  retval(3) = display_info::height ();

  return retval;
}

static double
default_screenpixelsperinch (void)
{
  return (display_info::x_dpi () + display_info::y_dpi ()) / 2;
}

static Matrix
default_colororder (void)
{
  Matrix retval (7, 3, 0.0);

  retval(0,2) = 1.0;

  retval(1,1) = 0.5;

  retval(2,0) = 1.0;

  retval(3,1) = 0.75;
  retval(3,2) = 0.75;

  retval(4,0) = 0.75;
  retval(4,2) = 0.75;

  retval(5,0) = 0.75;
  retval(5,1) = 0.75;

  retval(6,0) = 0.25;
  retval(6,1) = 0.25;
  retval(6,2) = 0.25;

  return retval;
}

static Matrix
default_lim (bool logscale = false)
{
  Matrix m (1, 2, 0);

  if (logscale)
    {
      m(0) = 0.1;
      m(1) = 1.0;
    }
  else
    m(1) = 1;

  return m;
}

static Matrix
default_data (void)
{
  Matrix retval (1, 2);

  retval(0) = 0;
  retval(1) = 1;

  return retval;
}

static Matrix
default_image_cdata (void)
{
  Matrix m (64, 64, 0.0);
  int i = 0;
  for (int col = 0; col < 64; col++)
    for (int row = 0; row < 64; row++)
      {
        m(col,row) = static_cast<double> (default_im_data[i]);
        i++;
      }

  return m;
}

static Matrix
default_surface_xdata (void)
{
  Matrix m (3, 3, 0.0);
  for (int col = 0; col < 3; col++)
    for (int row = 0; row < 3; row++)
      m(row,col) = col+1;

  return m;
}

static Matrix
default_surface_ydata (void)
{
  Matrix m (3, 3, 0.0);
  for (int row = 0; row < 3; row++)
    for (int col = 0; col < 3; col++)
      m(row,col) = row+1;

  return m;
}

static Matrix
default_surface_zdata (void)
{
  Matrix m (3, 3, 0.0);
  for (int row = 0; row < 3; row++)
    m(row,row) = 1.0;
  return m;
}

static Matrix
default_surface_cdata (void)
{
  return default_surface_zdata ();
}

static Matrix
default_patch_faces (void)
{
  Matrix m (1, 3, 1.0);
  m(1) = 2.0;
  m(2) = 3.0;
  return m;
}

static Matrix
default_patch_vertices (void)
{
  Matrix m (3, 2, 0);
  m(1) = 1.0;
  m(3) = 1.0;
  m(4) = 1.0;
  return m;
}

static Matrix
default_patch_xdata (void)
{
  Matrix m (3, 1, 0.0);
  m(1) = 1.0;
  return m;
}

static Matrix
default_patch_ydata (void)
{
  Matrix m (3, 1, 1.0);
  m(2) = 0.0;
  return m;
}

static Matrix
default_axes_position (void)
{
  Matrix m (1, 4, 0.0);
  m(0) = 0.13;
  m(1) = 0.11;
  m(2) = 0.775;
  m(3) = 0.815;
  return m;
}

static Matrix
default_axes_outerposition (void)
{
  Matrix m (1, 4, 0.0);
  m(2) = m(3) = 1.0;
  return m;
}

static Matrix
default_axes_view (void)
{
  Matrix m (1, 2, 0.0);
  m(1) = 90.0;
  return m;
}

static Matrix
default_axes_tick (void)
{
  Matrix m (1, 6, 0.0);
  m(0) = 0.0;
  m(1) = 0.2;
  m(2) = 0.4;
  m(3) = 0.6;
  m(4) = 0.8;
  m(5) = 1.0;
  return m;
}

static Matrix
default_axes_ticklength (void)
{
  Matrix m (1, 2, 0.0);
  m(0) = 0.01;
  m(1) = 0.025;
  return m;
}

static Matrix
default_figure_position (void)
{
  Matrix m (1, 4, 0.0);
  m(0) = 300;
  m(1) = 200;
  m(2) = 560;
  m(3) = 420;
  return m;
}

static Matrix
default_figure_papersize (void)
{
  Matrix m (1, 2, 0.0);
  m(0) = 8.5;
  m(1) = 11.0;
  return m;
}

static Matrix
default_figure_paperposition (void)
{
  Matrix m (1, 4, 0.0);
  m(0) = 0.25;
  m(1) = 2.50;
  m(2) = 8.00;
  m(3) = 6.00;
  return m;
}

static Matrix
default_control_position (void)
{
  Matrix retval (1, 4, 0.0);

  retval(0) = 0;
  retval(1) = 0;
  retval(2) = 80;
  retval(3) = 30;

  return retval;
}

static Matrix
default_control_sliderstep (void)
{
  Matrix retval (1, 2, 0.0);

  retval(0) = 0.01;
  retval(1) = 0.1;

  return retval;
}

static Matrix
default_panel_position (void)
{
  Matrix retval (1, 4, 0.0);

  retval(0) = 0;
  retval(1) = 0;
  retval(2) = 1;
  retval(3) = 1;

  return retval;
}

static double
convert_font_size (double font_size, const caseless_str& from_units,
                   const caseless_str& to_units, double parent_height = 0)
{
  // Simple case where from_units == to_units

  if (from_units.compare (to_units))
    return font_size;

  // Converts the given fontsize using the following transformation:
  // <old_font_size> => points => <new_font_size>

  double points_size = 0;
  double res = 0;

  if (from_units.compare ("points"))
    points_size = font_size;
  else
    {
      res = xget (0, "screenpixelsperinch").double_value ();

      if (from_units.compare ("pixels"))
        points_size = font_size * 72.0 / res;
      else if (from_units.compare ("inches"))
        points_size = font_size * 72.0;
      else if (from_units.compare ("centimeters"))
        points_size = font_size * 72.0 / 2.54;
      else if (from_units.compare ("normalized"))
        points_size = font_size * parent_height * 72.0 / res;
    }

  double new_font_size = 0;

  if (to_units.compare ("points"))
    new_font_size = points_size;
  else
    {
      if (res <= 0)
        res = xget (0, "screenpixelsperinch").double_value ();

      if (to_units.compare ("pixels"))
        new_font_size = points_size * res / 72.0;
      else if (to_units.compare ("inches"))
        new_font_size = points_size / 72.0;
      else if (to_units.compare ("centimeters"))
        new_font_size = points_size * 2.54 / 72.0;
      else if (to_units.compare ("normalized"))
        {
          // Avoid setting font size to (0/0) = NaN

          if (parent_height > 0)
            new_font_size = points_size * res / (parent_height * 72.0);
        }
    }

  return new_font_size;
}

static Matrix
convert_position (const Matrix& pos, const caseless_str& from_units,
                  const caseless_str& to_units, const Matrix& parent_dim)
{
  Matrix retval (1, pos.numel ());
  double res = 0;
  bool is_rectangle = (pos.numel () == 4);
  bool is_2d = (pos.numel () == 2);

  if (from_units.compare ("pixels"))
    retval = pos;
  else if (from_units.compare ("normalized"))
    {
      retval(0) = pos(0) * parent_dim(0) + 1;
      retval(1) = pos(1) * parent_dim(1) + 1;
      if (is_rectangle)
        {
          retval(2) = pos(2) * parent_dim(0);
          retval(3) = pos(3) * parent_dim(1);
        }
      else if (! is_2d)
        retval(2) = 0;
    }
  else if (from_units.compare ("characters"))
    {
      if (res <= 0)
        res = xget (0, "screenpixelsperinch").double_value ();

      double f = 0.0;

      // FIXME: this assumes the system font is Helvetica 10pt
      //          (for which "x" requires 6x12 pixels at 74.951 pixels/inch)
      f = 12.0 * res / 74.951;

      if (f > 0)
        {
          retval(0) = 0.5 * pos(0) * f;
          retval(1) = pos(1) * f;
          if (is_rectangle)
            {
              retval(2) = 0.5 * pos(2) * f;
              retval(3) = pos(3) * f;
            }
          else if (! is_2d)
            retval(2) = 0;
        }
    }
  else
    {
      if (res <= 0)
        res = xget (0, "screenpixelsperinch").double_value ();

      double f = 0.0;

      if (from_units.compare ("points"))
        f = res / 72.0;
      else if (from_units.compare ("inches"))
        f = res;
      else if (from_units.compare ("centimeters"))
        f = res / 2.54;

      if (f > 0)
        {
          retval(0) = pos(0) * f + 1;
          retval(1) = pos(1) * f + 1;
          if (is_rectangle)
            {
              retval(2) = pos(2) * f;
              retval(3) = pos(3) * f;
            }
          else if (! is_2d)
            retval(2) = 0;
        }
    }

  if (! to_units.compare ("pixels"))
    {
      if (to_units.compare ("normalized"))
        {
          retval(0) = (retval(0) - 1) / parent_dim(0);
          retval(1) = (retval(1) - 1) / parent_dim(1);
          if (is_rectangle)
            {
              retval(2) /= parent_dim(0);
              retval(3) /= parent_dim(1);
            }
          else if (! is_2d)
            retval(2) = 0;
        }
      else if (to_units.compare ("characters"))
        {
          if (res <= 0)
            res = xget (0, "screenpixelsperinch").double_value ();

          double f = 0.0;

          f = 12.0 * res / 74.951;

          if (f > 0)
            {
              retval(0) = 2 * retval(0) / f;
              retval(1) = retval(1) / f;
              if (is_rectangle)
                {
                  retval(2) = 2 * retval(2) / f;
                  retval(3) = retval(3) / f;
                }
              else if (! is_2d)
                retval(2) = 0;
            }
        }
      else
        {
          if (res <= 0)
            res = xget (0, "screenpixelsperinch").double_value ();

          double f = 0.0;

          if (to_units.compare ("points"))
            f = res / 72.0;
          else if (to_units.compare ("inches"))
            f = res;
          else if (to_units.compare ("centimeters"))
            f = res / 2.54;

          if (f > 0)
            {
              retval(0) = (retval(0) - 1) / f;
              retval(1) = (retval(1) - 1) / f;
              if (is_rectangle)
                {
                  retval(2) /= f;
                  retval(3) /= f;
                }
              else if (! is_2d)
                retval(2) = 0;
            }
        }
    }
  else if (! is_rectangle && ! is_2d)
    retval(2) = 0;

  return retval;
}

static Matrix
convert_text_position (const Matrix& pos, const text::properties& props,
                       const caseless_str& from_units,
                       const caseless_str& to_units)
{
  graphics_object go = gh_manager::get_object (props.get___myhandle__ ());
  graphics_object ax = go.get_ancestor ("axes");

  Matrix retval;

  if (ax.valid_object ())
    {
      const axes::properties& ax_props =
        dynamic_cast<const axes::properties&> (ax.get_properties ());
      graphics_xform ax_xform = ax_props.get_transform ();
      bool is_rectangle = (pos.numel () == 4);
      Matrix ax_bbox = ax_props.get_boundingbox (true),
             ax_size = ax_bbox.extract_n (0, 2, 1, 2);

      if (from_units.compare ("data"))
        {
          if (is_rectangle)
            {
              ColumnVector v1 = ax_xform.transform (pos(0), pos(1), 0),
                           v2 = ax_xform.transform (pos(0) + pos(2),
                                                    pos(1) + pos(3), 0);

              retval.resize (1, 4);

              retval(0) = v1(0) - ax_bbox(0) + 1;
              retval(1) = ax_bbox(1) + ax_bbox(3) - v1(1) + 1;
              retval(2) = v2(0) - v1(0);
              retval(3) = v1(1) - v2(1);
            }
          else
            {
              ColumnVector v = ax_xform.transform (pos(0), pos(1), pos(2));

              retval.resize (1, 3);

              retval(0) = v(0) - ax_bbox(0) + 1;
              retval(1) = ax_bbox(1) + ax_bbox(3) - v(1) + 1;
              retval(2) = 0;
            }
        }
      else
        retval = convert_position (pos, from_units, "pixels", ax_size);

      if (! to_units.compare ("pixels"))
        {
          if (to_units.compare ("data"))
            {
              if (is_rectangle)
                {
                  ColumnVector v1, v2;
                  v1 = ax_xform.untransform (
                         retval(0)  + ax_bbox(0) - 1,
                         ax_bbox(1) + ax_bbox(3) - retval(1) + 1);
                  v2 = ax_xform.untransform (
                         retval(0)  + retval(2)  + ax_bbox(0) - 1,
                         ax_bbox(1) + ax_bbox(3) - (retval(1) + retval(3)) + 1);

                  retval.resize (1, 4);

                  retval(0) = v1(0);
                  retval(1) = v1(1);
                  retval(2) = v2(0) - v1(0);
                  retval(3) = v2(1) - v1(1);
                }
              else
                {
                  ColumnVector v;
                  v = ax_xform.untransform (
                        retval(0)  + ax_bbox(0) - 1,
                        ax_bbox(1) + ax_bbox(3) - retval(1) + 1);

                  retval.resize (1, 3);

                  retval(0) = v(0);
                  retval(1) = v(1);
                  retval(2) = v(2);
                }
            }
          else
            retval = convert_position (retval, "pixels", to_units, ax_size);
        }
    }

  return retval;
}

// This function always returns the screensize in pixels
static Matrix
screen_size_pixels (void)
{
  graphics_object obj = gh_manager::get_object (0);
  Matrix sz = obj.get ("screensize").matrix_value ();
  return convert_position (sz, obj.get ("units").string_value (), "pixels",
                           sz.extract_n (0, 2, 1, 2)).extract_n (0, 2, 1, 2);
}

static void
convert_cdata_2 (bool is_scaled, bool is_real, double clim_0, double clim_1,
                 const double *cmapv, double x, octave_idx_type lda,
                 octave_idx_type nc, octave_idx_type i, double *av)
{
  if (is_scaled)
    x = xround ((nc - 1) * (x - clim_0) / (clim_1 - clim_0));
  else if (is_real)
    x = xround (x - 1);

  if (xisnan (x))
    {
      av[i]       = x;
      av[i+lda]   = x;
      av[i+2*lda] = x;
    }
  else
    {
      if (x < 0)
        x = 0;
      else if (x >= nc)
        x = (nc - 1);

      octave_idx_type idx = static_cast<octave_idx_type> (x);

      av[i]       = cmapv[idx];
      av[i+lda]   = cmapv[idx+nc];
      av[i+2*lda] = cmapv[idx+2*nc];
    }
}

template <class T>
void
convert_cdata_1 (bool is_scaled, bool is_real, double clim_0, double clim_1,
                 const double *cmapv, const T *cv, octave_idx_type lda,
                 octave_idx_type nc, double *av)
{
  for (octave_idx_type i = 0; i < lda; i++)
    convert_cdata_2 (is_scaled, is_real,
                     clim_0, clim_1, cmapv, cv[i], lda, nc, i, av);
}

static octave_value
convert_cdata (const base_properties& props, const octave_value& cdata,
               bool is_scaled, int cdim)
{
  dim_vector dv (cdata.dims ());

  // TrueColor data doesn't require conversion
  if (dv.length () == cdim && dv(cdim-1) == 3)
    return cdata;

  Matrix cmap (1, 3, 0.0);
  Matrix clim (1, 2, 0.0);

  graphics_object go = gh_manager::get_object (props.get___myhandle__ ());
  graphics_object fig = go.get_ancestor ("figure");

  if (fig.valid_object ())
    {
      Matrix _cmap = fig.get (caseless_str ("colormap")).matrix_value ();

      if (! error_state)
        cmap = _cmap;
    }

  if (is_scaled)
    {
      graphics_object ax = go.get_ancestor ("axes");

      if (ax.valid_object ())
        {
          Matrix _clim = ax.get (caseless_str ("clim")).matrix_value ();

          if (! error_state)
            clim = _clim;
        }
    }

  dv.resize (cdim);
  dv(cdim-1) = 3;

  NDArray a (dv);

  octave_idx_type lda = a.numel () / static_cast<octave_idx_type> (3);
  octave_idx_type nc = cmap.rows ();

  double *av = a.fortran_vec ();
  const double *cmapv = cmap.data ();

  double clim_0 = clim(0);
  double clim_1 = clim(1);

  // FIXME: There is a lot of processing time spent just on data conversion
  //        both here in graphics.cc and again in gl-render.cc.  There must
  //        be room for improvement!  Here a macro expands to a templated
  //        function which in turn calls another function (covert_cdata_2).
  //        And in gl-render.cc (opengl_renderer::draw_image), only GLfloat
  //        is supported anyways so there is another double for loop across
  //        height and width to convert all of the input data to GLfloat.

#define CONVERT_CDATA_1(ARRAY_T, VAL_FN, IS_REAL) \
  do \
    { \
      ARRAY_T tmp = cdata. VAL_FN ## array_value (); \
 \
      convert_cdata_1 (is_scaled, IS_REAL, clim_0, clim_1, cmapv, \
                       tmp.data (), lda, nc, av); \
    } \
  while (0)

  if (cdata.is_uint8_type ())
    CONVERT_CDATA_1 (uint8NDArray, uint8_, false);
  else if (cdata.is_uint16_type ())
    CONVERT_CDATA_1 (uint16NDArray, uint16_, false);
  else if (cdata.is_double_type ())
    CONVERT_CDATA_1 (NDArray, , true);
  else if (cdata.is_single_type ())
    CONVERT_CDATA_1 (FloatNDArray, float_, true);
  else if (cdata.is_bool_type ())
    CONVERT_CDATA_1 (boolNDArray, bool_, false);
  else
    error ("unsupported type for cdata (= %s)", cdata.type_name ().c_str ());

#undef CONVERT_CDATA_1

  return octave_value (a);
}

template<class T>
static void
get_array_limits (const Array<T>& m, double& emin, double& emax,
                  double& eminp, double& emaxp)
{
  const T *data = m.data ();
  octave_idx_type n = m.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double e = double (data[i]);

      // Don't need to test for NaN here as NaN>x and NaN<x is always false
      if (! xisinf (e))
        {
          if (e < emin)
            emin = e;

          if (e > emax)
            emax = e;

          if (e > 0 && e < eminp)
            eminp = e;

          if (e < 0 && e > emaxp)
            emaxp = e;
        }
    }
}

static bool
lookup_object_name (const caseless_str& name, caseless_str& go_name,
                    caseless_str& rest)
{
  int len = name.length ();
  int offset = 0;
  bool result = false;

  if (len >= 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
          || pfx.compare ("text"))
        offset = 4;
      else if (len >= 5)
        {
          pfx = name.substr (0, 5);

          if (pfx.compare ("image") || pfx.compare ("patch"))
            offset = 5;
          else if (len >= 6)
            {
              pfx = name.substr (0, 6);

              if (pfx.compare ("figure") || pfx.compare ("uimenu"))
                offset = 6;
              else if (len >= 7)
                {
                  pfx = name.substr (0, 7);

                  if (pfx.compare ("surface") || pfx.compare ("hggroup")
                      || pfx.compare ("uipanel"))
                    offset = 7;
                  else if (len >= 9)
                    {
                      pfx = name.substr (0, 9);

                      if (pfx.compare ("uicontrol")
                          || pfx.compare ("uitoolbar"))
                        offset = 9;
                      else if (len >= 10)
                        {
                          pfx = name.substr (0, 10);

                          if (pfx.compare ("uipushtool"))
                            offset = 10;
                          else if (len >= 12)
                            {
                              pfx = name.substr (0, 12);

                              if (pfx.compare ("uitoggletool"))
                                offset = 12;
                              else if (len >= 13)
                                {
                                  pfx = name.substr (0, 13);

                                  if (pfx.compare ("uicontextmenu"))
                                    offset = 13;
                                }
                            }
                        }
                    }
                }
            }
        }

      if (offset > 0)
        {
          go_name = pfx;
          rest = name.substr (offset);
          result = true;
        }
    }

  return result;
}

static base_graphics_object*
make_graphics_object_from_type (const caseless_str& type,
                                const graphics_handle& h = graphics_handle (),
                                const graphics_handle& p = graphics_handle ())
{
  base_graphics_object *go = 0;

  if (type.compare ("figure"))
    go = new figure (h, p);
  else if (type.compare ("axes"))
    go = new axes (h, p);
  else if (type.compare ("line"))
    go = new line (h, p);
  else if (type.compare ("text"))
    go = new text (h, p);
  else if (type.compare ("image"))
    go = new image (h, p);
  else if (type.compare ("patch"))
    go = new patch (h, p);
  else if (type.compare ("surface"))
    go = new surface (h, p);
  else if (type.compare ("hggroup"))
    go = new hggroup (h, p);
  else if (type.compare ("uimenu"))
    go = new uimenu (h, p);
  else if (type.compare ("uicontrol"))
    go = new uicontrol (h, p);
  else if (type.compare ("uipanel"))
    go = new uipanel (h, p);
  else if (type.compare ("uicontextmenu"))
    go = new uicontextmenu (h, p);
  else if (type.compare ("uitoolbar"))
    go = new uitoolbar (h, p);
  else if (type.compare ("uipushtool"))
    go = new uipushtool (h, p);
  else if (type.compare ("uitoggletool"))
    go = new uitoggletool (h, p);
  return go;
}

// ---------------------------------------------------------------------

bool
base_property::set (const octave_value& v, bool do_run, bool do_notify_toolkit)
{
  if (do_set (v))
    {

      // Notify graphics toolkit.
      if (id >= 0 && do_notify_toolkit)
        {
          graphics_object go = gh_manager::get_object (parent);
          if (go)
            go.update (id);
        }

      // run listeners
      if (do_run && ! error_state)
        run_listeners (POSTSET);

      return true;
    }

  return false;
}


void
base_property::run_listeners (listener_mode mode)
{
  const octave_value_list& l = listeners[mode];

  for (int i = 0; i < l.length (); i++)
    {
      gh_manager::execute_listener (parent, l(i));

      if (error_state)
        break;
    }
}

radio_values::radio_values (const std::string& opt_string)
  : default_val (), possible_vals ()
{
  size_t beg = 0;
  size_t len = opt_string.length ();
  bool done = len == 0;

  while (! done)
    {
      size_t end = opt_string.find ('|', beg);

      if (end == std::string::npos)
        {
          end = len;
          done = true;
        }

      std::string t = opt_string.substr (beg, end-beg);

      // Might want more error checking here...
      if (t[0] == '{')
        {
          t = t.substr (1, t.length () - 2);
          default_val = t;
        }
      else if (beg == 0) // ensure default value
        default_val = t;

      possible_vals.insert (t);

      beg = end + 1;
    }
}

std::string
radio_values::values_as_string (void) const
{
  std::string retval;
  for (std::set<caseless_str>::const_iterator it = possible_vals.begin ();
       it != possible_vals.end (); it++)
    {
      if (retval.empty ())
        {
          if (*it == default_value ())
            retval = "{" + *it + "}";
          else
            retval = *it;
        }
      else
        {
          if (*it == default_value ())
            retval += " | {" + *it + "}";
          else
            retval += " | " + *it;
        }
    }

  if (! retval.empty ())
    retval = "[ " + retval + " ]";

  return retval;
}

Cell
radio_values::values_as_cell (void) const
{
  octave_idx_type i = 0;
  Cell retval (nelem (), 1);
  for (std::set<caseless_str>::const_iterator it = possible_vals.begin ();
       it != possible_vals.end (); it++)
    retval(i++) = std::string (*it);
  return retval;
}

bool
color_values::str2rgb (const std::string& str_arg)
{
  bool retval = true;

  double tmp_rgb[3] = {0, 0, 0};

  std::string str = str_arg;
  unsigned int len = str.length ();

  std::transform (str.begin (), str.end (), str.begin (), tolower);

  if (str.compare (0, len, "blue", 0, len) == 0)
    tmp_rgb[2] = 1;
  else if (str.compare (0, len, "black", 0, len) == 0
           || str.compare (0, len, "k", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 0;
  else if (str.compare (0, len, "red", 0, len) == 0)
    tmp_rgb[0] = 1;
  else if (str.compare (0, len, "green", 0, len) == 0)
    tmp_rgb[1] = 1;
  else if (str.compare (0, len, "yellow", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = 1;
  else if (str.compare (0, len, "magenta", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[2] = 1;
  else if (str.compare (0, len, "cyan", 0, len) == 0)
    tmp_rgb[1] = tmp_rgb[2] = 1;
  else if (str.compare (0, len, "white", 0, len) == 0
           || str.compare (0, len, "w", 0, len) == 0)
    tmp_rgb[0] = tmp_rgb[1] = tmp_rgb[2] = 1;
  else
    retval = false;

  if (retval)
    {
      for (int i = 0; i < 3; i++)
        xrgb(i) = tmp_rgb[i];
    }

  return retval;
}

bool
color_property::do_set (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();

      if (! s.empty ())
        {
          std::string match;

          if (radio_val.contains (s, match))
            {
              if (current_type != radio_t || match != current_val)
                {
                  if (s.length () != match.length ())
                    warning_with_id ("Octave:abbreviated-property-match",
                                     "%s: allowing %s to match %s value %s",
                                     "set", s.c_str (), get_name ().c_str (),
                                     match.c_str ());
                  current_val = match;
                  current_type = radio_t;
                  return true;
                }
            }
          else
            {
              color_values col (s);
              if (! error_state)
                {
                  if (current_type != color_t || col != color_val)
                    {
                      color_val = col;
                      current_type = color_t;
                      return true;
                    }
                }
              else
                error ("invalid value for color property \"%s\" (value = %s)",
                       get_name ().c_str (), s.c_str ());
            }
        }
      else
        error ("invalid value for color property \"%s\"",
               get_name ().c_str ());
    }
  else if (val.is_numeric_type ())
    {
      Matrix m = val.matrix_value ();

      if (m.numel () == 3)
        {
          color_values col (m(0), m(1), m(2));
          if (! error_state)
            {
              if (current_type != color_t || col != color_val)
                {
                  color_val = col;
                  current_type = color_t;
                  return true;
                }
            }
        }
      else
        error ("invalid value for color property \"%s\"",
               get_name ().c_str ());
    }
  else
    error ("invalid value for color property \"%s\"",
           get_name ().c_str ());

  return false;
}

bool
double_radio_property::do_set (const octave_value& val)
{
  if (val.is_string ())
    {
      std::string s = val.string_value ();
      std::string match;

      if (! s.empty () && radio_val.contains (s, match))
        {
          if (current_type != radio_t || match != current_val)
            {
              if (s.length () != match.length ())
                warning_with_id ("Octave:abbreviated-property-match",
                                 "%s: allowing %s to match %s value %s",
                                 "set", s.c_str (), get_name ().c_str (),
                                 match.c_str ());
              current_val = match;
              current_type = radio_t;
              return true;
            }
        }
      else
        error ("invalid value for double_radio property \"%s\"",
               get_name ().c_str ());
    }
  else if (val.is_scalar_type () && val.is_real_type ())
    {
      double new_dval = val.double_value ();

      if (current_type != double_t || new_dval != dval)
        {
          dval = new_dval;
          current_type = double_t;
          return true;
        }
    }
  else
    error ("invalid value for double_radio property \"%s\"",
           get_name ().c_str ());

  return false;
}

bool
array_property::validate (const octave_value& v)
{
  bool xok = false;

  // check value type
  if (type_constraints.size () > 0)
    {
      if (type_constraints.find (v.class_name ()) != type_constraints.end ())
        xok = true;

      // check if complex is allowed (it's also of class "double", so
      // checking that alone is not enough to ensure real type)
      if (type_constraints.find ("real") != type_constraints.end ()
          && v.is_complex_type ())
        xok = false;
    }
  else
    xok = v.is_numeric_type () || v.is_bool_scalar ();

  if (xok)
    {
      if (size_constraints.size () == 0)
        return true;

      dim_vector vdims = v.dims ();
      int vlen = vdims.length ();

      xok = false;

      // check dimensional size constraints until a match is found
      for (std::list<dim_vector>::const_iterator it = size_constraints.begin ();
           ! xok && it != size_constraints.end (); ++it)
        {
          dim_vector itdims = (*it);

          if (itdims.length () == vlen)
            {
              xok = true;

              for (int i = 0; xok && i < vlen; i++)
                {
                  if (itdims(i) > 0)
                    {
                      if (itdims(i) != vdims(i))
                        xok = false;
                    }
                  else if (v.is_empty ())
                    break;
                }
            }
        }
    }

  return xok;
}

bool
array_property::is_equal (const octave_value& v) const
{
  if (data.type_name () == v.type_name ())
    {
      if (data.dims () == v.dims ())
        {

#define CHECK_ARRAY_EQUAL(T,F,A) \
            { \
              if (data.numel () == 1) \
                return data.F ## scalar_value () == \
                  v.F ## scalar_value (); \
              else  \
                { \
                  /* Keep copy of array_value to allow sparse/bool arrays */ \
                  /* that are converted, to not be deallocated early */ \
                  const A m1 = data.F ## array_value (); \
                  const T* d1 = m1.data (); \
                  const A m2 = v.F ## array_value (); \
                  const T* d2 = m2.data ();\
                  \
                  bool flag = true; \
                  \
                  for (int i = 0; flag && i < data.numel (); i++) \
                    if (d1[i] != d2[i]) \
                      flag = false; \
                  \
                  return flag; \
                } \
            }

          if (data.is_double_type () || data.is_bool_type ())
            CHECK_ARRAY_EQUAL (double, , NDArray)
          else if (data.is_single_type ())
            CHECK_ARRAY_EQUAL (float, float_, FloatNDArray)
          else if (data.is_int8_type ())
            CHECK_ARRAY_EQUAL (octave_int8, int8_, int8NDArray)
          else if (data.is_int16_type ())
            CHECK_ARRAY_EQUAL (octave_int16, int16_, int16NDArray)
          else if (data.is_int32_type ())
            CHECK_ARRAY_EQUAL (octave_int32, int32_, int32NDArray)
          else if (data.is_int64_type ())
            CHECK_ARRAY_EQUAL (octave_int64, int64_, int64NDArray)
          else if (data.is_uint8_type ())
            CHECK_ARRAY_EQUAL (octave_uint8, uint8_, uint8NDArray)
          else if (data.is_uint16_type ())
            CHECK_ARRAY_EQUAL (octave_uint16, uint16_, uint16NDArray)
          else if (data.is_uint32_type ())
            CHECK_ARRAY_EQUAL (octave_uint32, uint32_, uint32NDArray)
          else if (data.is_uint64_type ())
            CHECK_ARRAY_EQUAL (octave_uint64, uint64_, uint64NDArray)
        }
    }

  return false;
}

void
array_property::get_data_limits (void)
{
  xmin = xminp = octave_Inf;
  xmax = xmaxp = -octave_Inf;

  if (! data.is_empty ())
    {
      if (data.is_integer_type ())
        {
          if (data.is_int8_type ())
            get_array_limits (data.int8_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_uint8_type ())
            get_array_limits (data.uint8_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_int16_type ())
            get_array_limits (data.int16_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_uint16_type ())
            get_array_limits (data.uint16_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_int32_type ())
            get_array_limits (data.int32_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_uint32_type ())
            get_array_limits (data.uint32_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_int64_type ())
            get_array_limits (data.int64_array_value (),
                              xmin, xmax, xminp, xmaxp);
          else if (data.is_uint64_type ())
            get_array_limits (data.uint64_array_value (),
                              xmin, xmax, xminp, xmaxp);
        }
      else
        get_array_limits (data.array_value (), xmin, xmax, xminp, xmaxp);
    }
}

bool
handle_property::do_set (const octave_value& v)
{
  // Users may want to use empty matrix to reset a handle property
  if (v.is_empty ())
    {
      if (! get ().is_empty ())
        {
          current_val = graphics_handle ();
          return true;
        }
      else
        return false;
    }

  double dv = v.double_value ();

  if (! error_state)
    {
      graphics_handle gh = gh_manager::lookup (dv);

      if (xisnan (gh.value ()) || gh.ok ())
        {
          if (current_val != gh)
            {
              current_val = gh;
              return true;
            }
        }
      else
        error ("set: invalid graphics handle (= %g) for property \"%s\"",
               dv, get_name ().c_str ());
    }
  else
    error ("set: invalid graphics handle for property \"%s\"",
           get_name ().c_str ());

  return false;
}

Matrix
children_property::do_get_children (bool return_hidden) const
{
  Matrix retval (children_list.size (), 1);
  octave_idx_type k = 0;

  graphics_object go = gh_manager::get_object (0);

  root_figure::properties& props =
    dynamic_cast<root_figure::properties&> (go.get_properties ());

  if (! props.is_showhiddenhandles ())
    {
      for (const_children_list_iterator p = children_list.begin ();
           p != children_list.end (); p++)
        {
          graphics_handle kid = *p;

          if (gh_manager::is_handle_visible (kid))
            {
              if (! return_hidden)
                retval(k++) = *p;
            }
          else if (return_hidden)
            retval(k++) = *p;
        }

      retval.resize (k, 1);
    }
  else
    {
      for (const_children_list_iterator p = children_list.begin ();
           p != children_list.end (); p++)
        retval(k++) = *p;
    }

  return retval;
}

void
children_property::do_delete_children (bool clear)
{
  for (children_list_iterator p = children_list.begin ();
       p != children_list.end (); p++)
    {
      graphics_object go = gh_manager::get_object (*p);

      if (go.valid_object ())
        gh_manager::free (*p);

    }

  if (clear)
    children_list.clear ();
}

bool
callback_property::validate (const octave_value& v) const
{
  // case 1: function handle
  // case 2: cell array with first element being a function handle
  // case 3: string corresponding to known function name
  // case 4: evaluatable string
  // case 5: empty matrix

  if (v.is_function_handle ())
    return true;
  else if (v.is_string ())
    // complete validation will be done at execution-time
    return true;
  else if (v.is_cell () && v.length () > 0
           && (v.rows () == 1 || v.columns () == 1)
           && v.cell_value ()(0).is_function_handle ())
    return true;
  else if (v.is_empty ())
    return true;

  return false;
}

// If TRUE, we are executing any callback function, or the functions it
// calls.  Used to determine handle visibility inside callback
// functions.
static bool executing_callback = false;

void
callback_property::execute (const octave_value& data) const
{
  unwind_protect frame;

  // We are executing the callback function associated with this
  // callback property.  When set to true, we avoid recursive calls to
  // callback routines.
  frame.protect_var (executing);

  // We are executing a callback function, so allow handles that have
  // their handlevisibility property set to "callback" to be visible.
  frame.protect_var (executing_callback);

  if (! executing)
    {
      executing = true;
      executing_callback = true;

      if (callback.is_defined () && ! callback.is_empty ())
        gh_manager::execute_callback (get_parent (), callback, data);
    }
}

// Used to cache dummy graphics objects from which dynamic
// properties can be cloned.
static std::map<caseless_str, graphics_object> dprop_obj_map;

property
property::create (const std::string& name, const graphics_handle& h,
                  const caseless_str& type, const octave_value_list& args)
{
  property retval;

  if (type.compare ("string"))
    {
      std::string val = (args.length () > 0 ? args(0).string_value () : "");

      if (! error_state)
        retval = property (new string_property (name, h, val));
    }
  else if (type.compare ("any"))
    {
      octave_value val = args.length () > 0 ? args(0)
                                            : octave_value (Matrix ());

      retval = property (new any_property (name, h, val));
    }
  else if (type.compare ("radio"))
    {
      if (args.length () > 0)
        {
          std::string vals = args(0).string_value ();

          if (! error_state)
            {
              retval = property (new radio_property (name, h, vals));

              if (args.length () > 1)
                retval.set (args(1));
            }
          else
            error ("addproperty: invalid argument for radio property, expected a string value");
        }
      else
        error ("addproperty: missing possible values for radio property");
    }
  else if (type.compare ("double"))
    {
      double d = (args.length () > 0 ? args(0).double_value () : 0);

      if (! error_state)
        retval = property (new double_property (name, h, d));
    }
  else if (type.compare ("handle"))
    {
      double hh = (args.length () > 0 ? args(0).double_value () : octave_NaN);

      if (! error_state)
        {
          graphics_handle gh (hh);

          retval = property (new handle_property (name, h, gh));
        }
    }
  else if (type.compare ("boolean"))
    {
      retval = property (new bool_property (name, h, false));

      if (args.length () > 0)
        retval.set (args(0));
    }
  else if (type.compare ("data"))
    {
      retval = property (new array_property (name, h, Matrix ()));

      if (args.length () > 0)
        {
          retval.set (args(0));

          // FIXME: additional argument could define constraints,
          //        but is this really useful?
        }
    }
  else if (type.compare ("color"))
    {
      color_values cv (0, 0, 0);
      radio_values rv;

      if (args.length () > 1)
        rv = radio_values (args(1).string_value ());

      if (! error_state)
        {
          retval = property (new color_property (name, h, cv, rv));

          if (! error_state)
            {
              if (args.length () > 0 && ! args(0).is_empty ())
                retval.set (args(0));
              else
                retval.set (rv.default_value ());
            }
        }
    }
  else
    {
      caseless_str go_name, go_rest;

      if (lookup_object_name (type, go_name, go_rest))
        {
          graphics_object go;

          std::map<caseless_str, graphics_object>::const_iterator it =
            dprop_obj_map.find (go_name);

          if (it == dprop_obj_map.end ())
            {
              base_graphics_object *bgo =
                make_graphics_object_from_type (go_name);

              if (bgo)
                {
                  go = graphics_object (bgo);

                  dprop_obj_map[go_name] = go;
                }
            }
          else
            go = it->second;

          if (go.valid_object ())
            {
              property prop = go.get_properties ().get_property (go_rest);

              if (! error_state)
                {
                  retval = prop.clone ();

                  retval.set_parent (h);
                  retval.set_name (name);

                  if (args.length () > 0)
                    retval.set (args(0));
                }
            }
          else
            error ("addproperty: invalid object type (= %s)",
                   go_name.c_str ());
        }
      else
        error ("addproperty: unsupported type for dynamic property (= %s)",
               type.c_str ());
    }

  return retval;
}

static void
finalize_r (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  if (go)
    {
      Matrix children = go.get_properties ().get_all_children ();

      for (int k = 0; k < children.numel (); k++)
        finalize_r (children(k));

      go.finalize ();
    }
}

static void
initialize_r (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  if (go)
    {
      Matrix children = go.get_properties ().get_all_children ();

      go.initialize ();

      for (int k = 0; k < children.numel (); k++)
        initialize_r (children(k));
    }
}

void
figure::properties::set_toolkit (const graphics_toolkit& b)
{
  if (toolkit)
    finalize_r (get___myhandle__ ());

  toolkit = b;
  __graphics_toolkit__ = b.get_name ();
  __plot_stream__ = Matrix ();

  if (toolkit)
    initialize_r (get___myhandle__ ());

  mark_modified ();
}

void
figure::properties::set___mouse_mode__ (const octave_value& val_arg)
{
  if (! error_state)
    {
      std::string direction = "in";

      octave_value val = val_arg;

      if (val.is_string ())
        {
          std::string modestr = val.string_value ();

          if (modestr == "zoom in")
            {
              val = modestr = "zoom";
              direction = "in";
            }
          else if (modestr == "zoom out")
            {
              val = modestr = "zoom";
              direction = "out";
            }

          if (__mouse_mode__.set (val, true))
            {
              std::string mode = __mouse_mode__.current_value ();

              octave_scalar_map pm = get___pan_mode__ ().scalar_map_value ();
              pm.setfield ("Enable", mode == "pan" ? "on" : "off");
              set___pan_mode__ (pm);

              octave_scalar_map rm = get___rotate_mode__ ().scalar_map_value ();
              rm.setfield ("Enable", mode == "rotate" ? "on" : "off");
              set___rotate_mode__ (rm);

              octave_scalar_map zm = get___zoom_mode__ ().scalar_map_value ();
              zm.setfield ("Enable", mode == "zoom" ? "on" : "off");
              zm.setfield ("Direction", direction);
              set___zoom_mode__ (zm);

              mark_modified ();
            }
          else if (modestr == "zoom")
            {
              octave_scalar_map zm = get___zoom_mode__ ().scalar_map_value ();
              std::string curr_direction
                = zm.getfield ("Direction").string_value ();

              if (direction != curr_direction)
                {
                  zm.setfield ("Direction", direction);
                  set___zoom_mode__ (zm);

                  mark_modified ();
                }
            }
        }
    }
}

// ---------------------------------------------------------------------

void
property_list::set (const caseless_str& name, const octave_value& val)
{
  size_t offset = 0;

  size_t len = name.length ();

  if (len > 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
          || pfx.compare ("text"))
        offset = 4;
      else if (len > 5)
        {
          pfx = name.substr (0, 5);

          if (pfx.compare ("image") || pfx.compare ("patch"))
            offset = 5;
          else if (len > 6)
            {
              pfx = name.substr (0, 6);

              if (pfx.compare ("figure") || pfx.compare ("uimenu"))
                offset = 6;
              else if (len > 7)
                {
                  pfx = name.substr (0, 7);

                  if (pfx.compare ("surface") || pfx.compare ("hggroup")
                      || pfx.compare ("uipanel"))
                    offset = 7;
                  else if (len > 9)
                    {
                      pfx = name.substr (0, 9);

                      if (pfx.compare ("uicontrol")
                          || pfx.compare ("uitoolbar"))
                        offset = 9;
                      else if (len > 10)
                        {
                          pfx = name.substr (0, 10);

                          if (pfx.compare ("uipushtool"))
                            offset = 10;
                          else if (len > 12)
                            {
                              pfx = name.substr (0, 12);

                              if (pfx.compare ("uitoogletool"))
                                offset = 12;
                              else if (len > 13)
                                {
                                  pfx = name.substr (0, 13);

                                  if (pfx.compare ("uicontextmenu"))
                                    offset = 13;
                                }
                            }
                        }
                    }
                }
            }
        }

      if (offset > 0)
        {
          // FIXME: should we validate property names and values here?

          std::string pname = name.substr (offset);

          std::transform (pfx.begin (), pfx.end (), pfx.begin (), tolower);
          std::transform (pname.begin (), pname.end (), pname.begin (),
                          tolower);

          bool has_property = false;
          if (pfx == "axes")
            has_property = axes::properties::has_core_property (pname);
          else if (pfx == "line")
            has_property = line::properties::has_core_property (pname);
          else if (pfx == "text")
            has_property = text::properties::has_core_property (pname);
          else if (pfx == "image")
            has_property = image::properties::has_core_property (pname);
          else if (pfx == "patch")
            has_property = patch::properties::has_core_property (pname);
          else if (pfx == "figure")
            has_property = figure::properties::has_core_property (pname);
          else if (pfx == "surface")
            has_property = surface::properties::has_core_property (pname);
          else if (pfx == "hggroup")
            has_property = hggroup::properties::has_core_property (pname);
          else if (pfx == "uimenu")
            has_property = uimenu::properties::has_core_property (pname);
          else if (pfx == "uicontrol")
            has_property = uicontrol::properties::has_core_property (pname);
          else if (pfx == "uipanel")
            has_property = uipanel::properties::has_core_property (pname);
          else if (pfx == "uicontextmenu")
            has_property = uicontextmenu::properties::has_core_property (pname);
          else if (pfx == "uitoolbar")
            has_property = uitoolbar::properties::has_core_property (pname);
          else if (pfx == "uipushtool")
            has_property = uipushtool::properties::has_core_property (pname);

          if (has_property)
            {
              bool remove = false;
              if (val.is_string ())
                {
                  std::string tval = val.string_value ();

                  remove = (tval.compare ("remove") == 0);
                }

              pval_map_type& pval_map = plist_map[pfx];

              if (remove)
                {
                  pval_map_iterator p = pval_map.find (pname);

                  if (p != pval_map.end ())
                    pval_map.erase (p);
                }
              else
                pval_map[pname] = val;
            }
          else
            error ("invalid %s property '%s'", pfx.c_str (), pname.c_str ());
        }
    }

  if (! error_state && offset == 0)
    error ("invalid default property specification");
}

octave_value
property_list::lookup (const caseless_str& name) const
{
  octave_value retval;

  size_t offset = 0;

  size_t len = name.length ();

  if (len > 4)
    {
      caseless_str pfx = name.substr (0, 4);

      if (pfx.compare ("axes") || pfx.compare ("line")
          || pfx.compare ("text"))
        offset = 4;
      else if (len > 5)
        {
          pfx = name.substr (0, 5);

          if (pfx.compare ("image") || pfx.compare ("patch"))
            offset = 5;
          else if (len > 6)
            {
              pfx = name.substr (0, 6);

              if (pfx.compare ("figure") || pfx.compare ("uimenu"))
                offset = 6;
              else if (len > 7)
                {
                  pfx = name.substr (0, 7);

                  if (pfx.compare ("surface") || pfx.compare ("hggroup")
                      || pfx.compare ("uipanel"))
                    offset = 7;
                  else if (len > 9)
                    {
                      pfx = name.substr (0, 9);

                      if (pfx.compare ("uicontrol")
                          || pfx.compare ("uitoolbar"))
                        offset = 9;
                      else if (len > 10)
                        {
                          pfx = name.substr (0, 10);

                          if (pfx.compare ("uipushtool"))
                            offset = 10;
                          else if (len > 12)
                            {
                              pfx = name.substr (0, 12);

                              if (pfx.compare ("uitoggletool"))
                                offset = 12;
                              else if (len > 13)
                                {
                                  pfx = name.substr (0, 13);

                                  if (pfx.compare ("uicontextmenu"))
                                    offset = 13;
                                }
                            }
                        }
                    }
                }
            }
        }

      if (offset > 0)
        {
          std::string pname = name.substr (offset);

          std::transform (pfx.begin (), pfx.end (), pfx.begin (), tolower);
          std::transform (pname.begin (), pname.end (), pname.begin (),
                          tolower);

          plist_map_const_iterator p = find (pfx);

          if (p != end ())
            {
              const pval_map_type& pval_map = p->second;

              pval_map_const_iterator q = pval_map.find (pname);

              if (q != pval_map.end ())
                retval = q->second;
            }
        }
    }

  return retval;
}

octave_scalar_map
property_list::as_struct (const std::string& prefix_arg) const
{
  octave_scalar_map m;

  for (plist_map_const_iterator p = begin (); p != end (); p++)
    {
      std::string prefix = prefix_arg + p->first;

      const pval_map_type pval_map = p->second;

      for (pval_map_const_iterator q = pval_map.begin ();
           q != pval_map.end ();
           q++)
        m.assign (prefix + q->first, q->second);
    }

  return m;
}

// Set properties given as a cs-list of name, value pairs.

void
graphics_object::set (const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin == 0)
    error ("graphics_object::set: Nothing to set");
  else if (nargin % 2 == 0)
    {
      for (int i = 0; i < nargin; i += 2)
        {
          caseless_str name = args(i).string_value ();

          if (! error_state)
            {
              octave_value val = args(i+1);

              set_value_or_default (name, val);

              if (error_state)
                break;
            }
          else
            error ("set: expecting argument %d to be a property name", i);
        }
    }
  else
    error ("set: invalid number of arguments");
}

/*
## test set with name, value pairs
%!test
%! hf = figure ("visible", "off");
%! h = plot (1:10, 10:-1:1);
%! set (h, "linewidth", 10, "marker", "x");
%! lw = get (h, "linewidth");
%! mk = get (h, "marker");
%! close (hf);
%! assert (lw, 10);
%! assert (mk, "x");
*/

// Set properties given in two cell arrays containing names and values.
void
graphics_object::set (const Array<std::string>& names,
                      const Cell& values, octave_idx_type row)
{
  if (names.numel () != values.columns ())
    {
      error ("set: number of names must match number of value columns (%d != %d)",
             names.numel (), values.columns ());
    }

  octave_idx_type k = names.columns ();

  for (octave_idx_type column = 0; column < k; column++)
    {
      caseless_str name = names(column);
      octave_value val  = values(row, column);

      set_value_or_default (name, val);

      if (error_state)
        break;
    }
}

/*
## test set with cell array arguments
%!test
%! hf = figure ("visible", "off");
%! h = plot (1:10, 10:-1:1);
%! set (h, {"linewidth", "marker"}, {10, "x"});
%! lw = get (h, "linewidth");
%! mk = get (h, "marker");
%! close (hf);
%! assert (lw, 10);
%! assert (mk, "x");

## test set with multiple handles and cell array arguments
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, {"linewidth", "marker"}, {10, "x"; 5, "o"});
%!   assert (get (h, "linewidth"), {10; 5});
%!   assert (get (h, "marker"), {"x"; "o"});
%!   set (h, {"linewidth", "marker"}, {10, "x"});
%!   assert (get (h, "linewidth"), {10; 10});
%!   assert (get (h, "marker"), {"x"; "x"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect;

%!error <set: number of graphics handles must match number of value rows>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, {"linewidth", "marker"}, {10, "x"; 5, "o"; 7, "."});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <set: number of names must match number of value columns>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, {"linewidth"}, {10, "x"; 5, "o"});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

// Set properties given in a struct array
void
graphics_object::set (const octave_map& m)
{
  for (octave_idx_type p = 0; p < m.nfields (); p++)
    {
      caseless_str name  = m.keys ()[p];

      octave_value val = octave_value (m.contents (name).elem (m.numel () - 1));

      set_value_or_default (name, val);

      if (error_state)
        break;
    }
}

/*
## test set ticklabels for compatibility
%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", [0, 0.2, 0.4, 0.6, 0.8, 1]);
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "char");
%! assert (size (xticklabel), [6, 3]);

%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", "0|0.2|0.4|0.6|0.8|1");
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "char");
%! assert (size (xticklabel), [6, 3]);

%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", ["0 "; "0.2"; "0.4"; "0.6"; "0.8"; "1 "]);
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "char");
%! assert (size (xticklabel), [6, 3]);

%!test
%! hf = figure ("visible", "off");
%! set (gca (), "xticklabel", {"0", "0.2", "0.4", "0.6", "0.8", "1"});
%! xticklabel = get (gca (), "xticklabel");
%! close (hf);
%! assert (class (xticklabel), "cell");
%! assert (size (xticklabel), [6, 1]);
*/

/*
## test set with struct arguments
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = plot (1:10, 10:-1:1);
%!   set (h, struct ("linewidth", 10, "marker", "x"));
%!   assert (get (h, "linewidth"), 10);
%!   assert (get (h, "marker"), "x");
%!   h = plot (1:10, 10:-1:1, 1:10, 1:10);
%!   set (h, struct ("linewidth", {5, 10}));
%!   assert (get (h, "linewidth"), {10; 10});
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## test ordering
%!test
%! markchanged = @(h, foobar, name) set (h, "userdata", [get(h,"userdata"); {name}]);
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = line ();
%!   set (h, "userdata", {});
%!   addlistener (h, "color", {markchanged, "color"});
%!   addlistener (h, "linewidth", {markchanged, "linewidth"});
%!   ## "linewidth" first
%!   props.linewidth = 2;
%!   props.color = "r";
%!   set (h, props);
%!   assert (get (h, "userdata"), fieldnames (props));
%!   clear props;
%!   clf ();
%!   h = line ();
%!   set (h, "userdata", {});
%!   addlistener (h, "color", {markchanged, "color"});
%!   addlistener (h, "linewidth", {markchanged, "linewidth"});
%!   ## "color" first
%!   props.color = "r";
%!   props.linewidth = 2;
%!   set (h, props);
%!   assert (get (h, "userdata"), fieldnames (props));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

// Set a property to a value or to its (factory) default value.

void
graphics_object::set_value_or_default (const caseless_str& name,
                                       const octave_value& val)
{
  if (val.is_string ())
    {
      std::string tval = val.string_value ();

      octave_value default_val;

      if (tval.compare ("default") == 0)
        {
          default_val = get_default (name);

          if (error_state)
            return;

          rep->set (name, default_val);
        }
      else if (tval.compare ("factory") == 0)
        {
          default_val = get_factory_default (name);

          if (error_state)
            return;

          rep->set (name, default_val);
        }
      else
        {
          // Matlab specifically uses "\default" to escape string setting
          if (tval.compare ("\\default") == 0)
            rep->set (name, "default");
          else if (tval.compare ("\\factory") == 0)
            rep->set (name, "factory");
          else
            rep->set (name, val);
        }
    }
  else
    rep->set (name, val);
}

/*
## test setting of default values
%!test
%! old_lw = get (0, "defaultlinelinewidth");
%! unwind_protect
%!   hf = figure ("visible", "off");
%!   h = plot (1:10, 10:-1:1);
%!   set (0, "defaultlinelinewidth", 20);
%!   set (h, "linewidth", "default");
%!   assert (get (h, "linewidth"), 20);
%!   set (h, "linewidth", "factory");
%!   assert (get (h, "linewidth"), 0.5);
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "defaultlinelinewidth", old_lw);
%! end_unwind_protect
*/

static double
make_handle_fraction (void)
{
  static double maxrand = RAND_MAX + 2.0;

  return (rand () + 1.0) / maxrand;
}

graphics_handle
gh_manager::do_get_handle (bool integer_figure_handle)
{
  graphics_handle retval;

  if (integer_figure_handle)
    {
      // Figure handles are positive integers corresponding to the
      // figure number.

      // We always want the lowest unused figure number.

      retval = 1;

      while (handle_map.find (retval) != handle_map.end ())
        retval++;
    }
  else
    {
      // Other graphics handles are negative integers plus some random
      // fractional part.  To avoid running out of integers, we
      // recycle the integer part but tack on a new random part each
      // time.

      free_list_iterator p = handle_free_list.begin ();

      if (p != handle_free_list.end ())
        {
          retval = *p;
          handle_free_list.erase (p);
        }
      else
        {
          retval = graphics_handle (next_handle);

          next_handle = std::ceil (next_handle) - 1.0 - make_handle_fraction ();
        }
    }

  return retval;
}

void
gh_manager::do_free (const graphics_handle& h)
{
  if (h.ok ())
    {
      if (h.value () != 0)
        {
          iterator p = handle_map.find (h);

          if (p != handle_map.end ())
            {
              base_properties& bp = p->second.get_properties ();

              bp.set_beingdeleted (true);

              bp.delete_children ();

              octave_value val = bp.get_deletefcn ();

              bp.execute_deletefcn ();

              // Notify graphics toolkit.
              p->second.finalize ();

              // Note: this will be valid only for first explicitly
              // deleted object.  All its children will then have an
              // unknown graphics toolkit.

              // Graphics handles for non-figure objects are negative
              // integers plus some random fractional part.  To avoid
              // running out of integers, we recycle the integer part
              // but tack on a new random part each time.

              handle_map.erase (p);

              if (h.value () < 0)
                handle_free_list.insert
                  (std::ceil (h.value ()) - make_handle_fraction ());
            }
          else
            error ("graphics_handle::free: invalid object %g", h.value ());
        }
      else
        error ("graphics_handle::free: can't delete root figure");
    }
}

void
gh_manager::do_renumber_figure (const graphics_handle& old_gh,
                                const graphics_handle& new_gh)
{
  iterator p = handle_map.find (old_gh);

  if (p != handle_map.end ())
    {
      graphics_object go = p->second;

      handle_map.erase (p);

      handle_map[new_gh] = go;

      if (old_gh.value () < 0)
        handle_free_list.insert (std::ceil (old_gh.value ())
                                 - make_handle_fraction ());
    }
  else
    error ("graphics_handle::free: invalid object %g", old_gh.value ());

  for (figure_list_iterator q = figure_list.begin ();
       q != figure_list.end (); q++)
    {
      if (*q == old_gh)
        {
          *q = new_gh;
          break;
        }
    }
}

gh_manager *gh_manager::instance = 0;

static void
xset (const graphics_handle& h, const caseless_str& name,
      const octave_value& val)
{
  graphics_object obj = gh_manager::get_object (h);
  obj.set (name, val);
}

static void
xset (const graphics_handle& h, const octave_value_list& args)
{
  if (args.length () > 0)
    {
      graphics_object obj = gh_manager::get_object (h);
      obj.set (args);
    }
}

static octave_value
xget (const graphics_handle& h, const caseless_str& name)
{
  graphics_object obj = gh_manager::get_object (h);
  return obj.get (name);
}

static graphics_handle
reparent (const octave_value& ov, const std::string& who,
          const std::string& property, const graphics_handle& new_parent,
          bool adopt = true)
{
  graphics_handle h = octave_NaN;

  double val = ov.double_value ();

  if (! error_state)
    {
      h = gh_manager::lookup (val);

      if (h.ok ())
        {
          graphics_object obj = gh_manager::get_object (h);

          graphics_handle parent_h = obj.get_parent ();

          graphics_object parent_obj = gh_manager::get_object (parent_h);

          parent_obj.remove_child (h);

          if (adopt)
            obj.set ("parent", new_parent.value ());
          else
            obj.reparent (new_parent);
        }
      else
        error ("%s: invalid graphics handle (= %g) for %s",
               who.c_str (), val, property.c_str ());
    }
  else
    error ("%s: expecting %s to be a graphics handle",
           who.c_str (), property.c_str ());

  return h;
}

// This function is NOT equivalent to the scripting language function gcf.
graphics_handle
gcf (void)
{
  octave_value val = xget (0, "currentfigure");

  return val.is_empty () ? octave_NaN : val.double_value ();
}

// This function is NOT equivalent to the scripting language function gca.
graphics_handle
gca (void)
{
  octave_value val = xget (gcf (), "currentaxes");

  return val.is_empty () ? octave_NaN : val.double_value ();
}

static void
delete_graphics_object (const graphics_handle& h)
{
  if (h.ok ())
    {
      graphics_object obj = gh_manager::get_object (h);

      // Don't do recursive deleting, due to callbacks
      if (! obj.get_properties ().is_beingdeleted ())
        {
          graphics_handle parent_h = obj.get_parent ();

          graphics_object parent_obj =
            gh_manager::get_object (parent_h);

          // NOTE: free the handle before removing it from its
          //       parent's children, such that the object's
          //       state is correct when the deletefcn callback
          //       is executed

          gh_manager::free (h);

          // A callback function might have already deleted
          // the parent
          if (parent_obj.valid_object ())
            parent_obj.remove_child (h);

          Vdrawnow_requested = true;
        }
    }
}

static void
delete_graphics_object (double val)
{
  delete_graphics_object (gh_manager::lookup (val));
}

static void
delete_graphics_objects (const NDArray vals)
{
  for (octave_idx_type i = 0; i < vals.numel (); i++)
    delete_graphics_object (vals.elem (i));
}

static void
close_figure (const graphics_handle& handle)
{
  octave_value closerequestfcn = xget (handle, "closerequestfcn");

  OCTAVE_SAFE_CALL (gh_manager::execute_callback, (handle, closerequestfcn));
}

static void
force_close_figure (const graphics_handle& handle)
{
  // Remove the deletefcn and closerequestfcn callbacks and delete the
  // object directly.

  xset (handle, "deletefcn", Matrix ());
  xset (handle, "closerequestfcn", Matrix ());

  delete_graphics_object (handle);
}

void
gh_manager::do_close_all_figures (void)
{
  // FIXME: should we process or discard pending events?

  event_queue.clear ();

  // Don't use figure_list_iterator because we'll be removing elements
  // from the list elsewhere.

  Matrix hlist = do_figure_handle_list (true);

  for (octave_idx_type i = 0; i < hlist.numel (); i++)
    {
      graphics_handle h = gh_manager::lookup (hlist(i));

      if (h.ok ())
        close_figure (h);
    }

  // They should all be closed now.  If not, force them to close.

  hlist = do_figure_handle_list (true);

  for (octave_idx_type i = 0; i < hlist.numel (); i++)
    {
      graphics_handle h = gh_manager::lookup (hlist(i));

      if (h.ok ())
        force_close_figure (h);
    }

  // None left now, right?

  hlist = do_figure_handle_list (true);

  assert (hlist.numel () == 0);

  // Clear all callback objects from our list.

  callback_objects.clear ();
}

static void
adopt (const graphics_handle& p, const graphics_handle& h)
{
  graphics_object parent_obj = gh_manager::get_object (p);
  parent_obj.adopt (h);
}

static bool
is_handle (const graphics_handle& h)
{
  return h.ok ();
}

static bool
is_handle (double val)
{
  graphics_handle h = gh_manager::lookup (val);

  return h.ok ();
}

static octave_value
is_handle (const octave_value& val)
{
  octave_value retval = false;

  if (val.is_real_scalar () && is_handle (val.double_value ()))
    retval = true;
  else if (val.is_numeric_type () && val.is_real_type ())
    {
      const NDArray handles = val.array_value ();

      if (! error_state)
        {
          boolNDArray result (handles.dims ());

          for (octave_idx_type i = 0; i < handles.numel (); i++)
            result.xelem (i) = is_handle (handles (i));

          retval = result;
        }
    }

  return retval;
}

static bool
is_figure (double val)
{
  graphics_object obj = gh_manager::get_object (val);

  return obj && obj.isa ("figure");
}

static void
xcreatefcn (const graphics_handle& h)
{
  graphics_object obj = gh_manager::get_object (h);
  obj.get_properties ().execute_createfcn  ();
}

static void
xinitialize (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  if (go)
    go.initialize ();
}

// ---------------------------------------------------------------------

void
base_graphics_toolkit::update (const graphics_handle& h, int id)
{
  graphics_object go = gh_manager::get_object (h);

  update (go, id);
}

bool
base_graphics_toolkit::initialize (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  return initialize (go);
}

void
base_graphics_toolkit::finalize (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (h);

  finalize (go);
}

static void
xreset_default_properties (graphics_handle gh,
                           property_list::pval_map_type factory_pval)
{
  graphics_object obj = gh_manager::get_object (gh);

  // Replace factory defaults by user defined ones
  std::string go_name = obj.get_properties ().graphics_object_name ();
  property_list::pval_map_type pval;
  obj.build_user_defaults_map (pval, go_name);

  for (property_list::pval_map_const_iterator p = pval.begin ();
       p != pval.end (); p++)
    {
      factory_pval[p->first] = p->second;
    }


  // Reset defaults
  for (property_list::pval_map_const_iterator it = factory_pval.begin ();
       it != factory_pval.end (); it++)
    {
      std::string pname = it->first;

      // Don't reset internal properties and handle_properties
      if (! obj.has_readonly_property (pname)
          && pname.find ("__") != 0 && pname.find ("current") != 0
          && pname != "uicontextmenu" && pname != "parent")
        {
          // Store *mode prop/val in order to set them last
          if (pname.find ("mode") == (pname.length () - 4))
            pval[pname] = it->second;
          else
            obj.set (pname, it->second);
        }
    }

  // set *mode properties
  for (property_list::pval_map_const_iterator it = pval.begin ();
       it != pval.end (); it++)
    obj.set (it->first, it->second);
}

// ---------------------------------------------------------------------

void
base_properties::set_from_list (base_graphics_object& obj,
                                property_list& defaults)
{
  std::string go_name = graphics_object_name ();

  property_list::plist_map_const_iterator p = defaults.find (go_name);

  if (p != defaults.end ())
    {
      const property_list::pval_map_type pval_map = p->second;

      for (property_list::pval_map_const_iterator q = pval_map.begin ();
           q != pval_map.end ();
           q++)
        {
          std::string pname = q->first;

          obj.set (pname, q->second);

          if (error_state)
            {
              error ("error setting default property %s", pname.c_str ());
              break;
            }
        }
    }
}

/*
## test defaults are set in the order they were stored
%!test
%! set(0, "defaultfigureunits", "normalized");
%! set(0, "defaultfigureposition", [0.7 0 0.3 0.3]);
%! hf = figure ("visible", "off");
%! tol = 20 * eps;
%! unwind_protect
%!   assert (get (hf, "position"), [0.7 0 0.3 0.3], tol);
%! unwind_protect_cleanup
%!   close (hf);
%!   set(0, "defaultfigureunits", "remove");
%!   set(0, "defaultfigureposition", "remove");
%! end_unwind_protect
*/

octave_value
base_properties::get_dynamic (const caseless_str& name) const
{
  octave_value retval;

  std::map<caseless_str, property, cmp_caseless_str>::const_iterator it =
    all_props.find (name);

  if (it != all_props.end ())
    retval = it->second.get ();
  else
    error ("get: unknown property \"%s\"", name.c_str ());

  return retval;
}

octave_value
base_properties::get_dynamic (bool all) const
{
  octave_scalar_map m;

  for (std::map<caseless_str, property, cmp_caseless_str>::const_iterator
       it = all_props.begin (); it != all_props.end (); ++it)
    if (all || ! it->second.is_hidden ())
      m.assign (it->second.get_name (), it->second.get ());

  return m;
}

std::set<std::string>
base_properties::dynamic_property_names (void) const
{
  return dynamic_properties;
}

bool
base_properties::has_dynamic_property (const std::string& pname)
{
  const std::set<std::string>& dynprops = dynamic_property_names ();

  if (dynprops.find (pname) != dynprops.end ())
    return true;
  else
    return all_props.find (pname) != all_props.end ();
}

void
base_properties::set_dynamic (const caseless_str& pname,
                              const octave_value& val)
{
  std::map<caseless_str, property, cmp_caseless_str>::iterator it =
    all_props.find (pname);

  if (it != all_props.end ())
    it->second.set (val);
  else
    error ("set: unknown property \"%s\"", pname.c_str ());

  if (! error_state)
    {
      dynamic_properties.insert (pname);

      mark_modified ();
    }
}

property
base_properties::get_property_dynamic (const caseless_str& name)
{
  std::map<caseless_str, property, cmp_caseless_str>::const_iterator it =
    all_props.find (name);

  if (it == all_props.end ())
    {
      error ("get_property: unknown property \"%s\"", name.c_str ());
      return property ();
    }
  else
    return it->second;
}

void
base_properties::set_parent (const octave_value& val)
{
  double hnp = val.double_value ();

  graphics_handle new_parent = octave_NaN;

  if (! error_state)
    {
      if (hnp == __myhandle__)
        error ("set: can not set object parent to be object itself");
      else
        {
          new_parent = gh_manager::lookup (hnp);

          if (new_parent.ok ())
            {
              // Remove child from current parent
              graphics_object old_parent_obj;
              old_parent_obj = gh_manager::get_object (get_parent ());


              if (old_parent_obj.get_handle () != hnp)
                old_parent_obj.remove_child (__myhandle__);
              else
                // Do nothing more
                return;

              // Check new parent's parent is not this child to avoid recursion
              graphics_object new_parent_obj;
              new_parent_obj = gh_manager::get_object (new_parent);
              if (new_parent_obj.get_parent () == __myhandle__)
                {
                  // new parent's parent gets child's original parent
                  new_parent_obj.get_properties ().set_parent (get_parent ().as_octave_value ());
                }

              // Set parent property to new_parent and do adoption
              parent = new_parent.as_octave_value ();
              ::adopt (parent.handle_value (), __myhandle__);
            }
          else
            error ("set: invalid graphics handle (= %g) for parent", hnp);
        }
    }
  else
    error ("set: expecting parent to be a graphics handle");
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax = gca ();
%!   set (hax, "parent", gcf ())
%!   assert (gca (), hax)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
base_properties::mark_modified (void)
{
  __modified__ = "on";
  graphics_object parent_obj = gh_manager::get_object (get_parent ());
  if (parent_obj)
    parent_obj.mark_modified ();
}

void
base_properties::override_defaults (base_graphics_object& obj)
{
  graphics_object parent_obj = gh_manager::get_object (get_parent ());

  if (parent_obj)
    parent_obj.override_defaults (obj);
}

void
base_properties::update_axis_limits (const std::string& axis_type) const
{
  graphics_object obj = gh_manager::get_object (__myhandle__);

  if (obj)
    obj.update_axis_limits (axis_type);
}

void
base_properties::update_axis_limits (const std::string& axis_type,
                                     const graphics_handle& h) const
{
  graphics_object obj = gh_manager::get_object (__myhandle__);

  if (obj)
    obj.update_axis_limits (axis_type, h);
}

void
base_properties::update_uicontextmenu (void) const
{
  if (uicontextmenu.get ().is_empty ())
    return;

  graphics_object obj = gh_manager::get_object (uicontextmenu.get ());
  if (obj && obj.isa ("uicontextmenu"))
    {
      uicontextmenu::properties& props =
        reinterpret_cast<uicontextmenu::properties&> (obj.get_properties ());
      props.add_dependent_obj (__myhandle__);
    }
}

bool
base_properties::is_handle_visible (void) const
{
  return (handlevisibility.is ("on")
          || (executing_callback && ! handlevisibility.is ("off")));
}

graphics_toolkit
base_properties::get_toolkit (void) const
{
  graphics_object go = gh_manager::get_object (get_parent ());

  if (go)
    return go.get_toolkit ();
  else
    return graphics_toolkit ();
}

void
base_properties::update_boundingbox (void)
{
  Matrix kids = get_children ();

  for (int i = 0; i < kids.numel (); i++)
    {
      graphics_object go = gh_manager::get_object (kids(i));

      if (go.valid_object ())
        go.get_properties ().update_boundingbox ();
    }
}

void
base_properties::update_autopos (const std::string& elem_type)
{
  graphics_object parent_obj = gh_manager::get_object (get_parent ());

  if (parent_obj.valid_object ())
    parent_obj.get_properties ().update_autopos (elem_type);
}

void
base_properties::add_listener (const caseless_str& nm, const octave_value& v,
                               listener_mode mode)
{
  property p = get_property (nm);

  if (! error_state && p.ok ())
    p.add_listener (v, mode);
}

void
base_properties::delete_listener (const caseless_str& nm,
                                  const octave_value& v, listener_mode mode)
{
  property p = get_property (nm);

  if (! error_state && p.ok ())
    p.delete_listener (v, mode);
}

// ---------------------------------------------------------------------

void
base_graphics_object::update_axis_limits (const std::string& axis_type)
{
  if (valid_object ())
    {
      graphics_object parent_obj = gh_manager::get_object (get_parent ());

      if (parent_obj)
        parent_obj.update_axis_limits (axis_type);
    }
  else
    error ("base_graphics_object::update_axis_limits: invalid graphics object");
}

void
base_graphics_object::update_axis_limits (const std::string& axis_type,
                                          const graphics_handle& h)
{
  if (valid_object ())
    {
      graphics_object parent_obj = gh_manager::get_object (get_parent ());

      if (parent_obj)
        parent_obj.update_axis_limits (axis_type, h);
    }
  else
    error ("base_graphics_object::update_axis_limits: invalid graphics object");
}

void
base_graphics_object::remove_all_listeners (void)
{
  octave_map m = get (true).map_value ();

  for (octave_map::const_iterator pa = m.begin (); pa != m.end (); pa++)
    {
      // FIXME: there has to be a better way.  I think we want to
      // ask whether it is OK to delete the listener for the given
      // property.  How can we know in advance that it will be OK?

      unwind_protect frame;

      frame.protect_var (error_state);
      frame.protect_var (discard_error_messages);
      frame.protect_var (Vdebug_on_error);
      frame.protect_var (Vdebug_on_warning);

      discard_error_messages = true;
      Vdebug_on_error = false;
      Vdebug_on_warning = false;

      property p = get_properties ().get_property (pa->first);

      if (! error_state && p.ok ())
        p.delete_listener ();
    }
}

void
base_graphics_object::build_user_defaults_map (property_list::pval_map_type &def, const std::string go_name) const
{
  property_list local_defaults = get_defaults_list ();
  property_list::plist_map_const_iterator p =
    local_defaults.find (go_name);

  if (p != local_defaults.end ())
    {
      property_list::pval_map_type pval = p->second;
      for (property_list::pval_map_const_iterator q = pval.begin ();
           q != pval.end (); q++)
        {
          std::string pname = q->first;
          if (def.find (pname) == def.end ())
            def[pname] = q->second;
        }
    }

  graphics_object parent_obj = gh_manager::get_object (get_parent ());

  if (parent_obj)
    parent_obj.build_user_defaults_map (def, go_name);

}

void
base_graphics_object::reset_default_properties (void)
{
  if (valid_object ())
    {
      property_list::pval_map_type factory_pval =
        gh_manager::get_object (0).get_factory_defaults_list ()
        .find (type ())->second;

      xreset_default_properties (get_handle (), factory_pval);
    }
}

std::string
base_graphics_object::values_as_string (void)
{
  std::string retval;

  if (valid_object ())
    {
      octave_map m = get ().map_value ();
      graphics_object obj = gh_manager::get_object (get_handle ());

      for (octave_map::const_iterator pa = m.begin (); pa != m.end (); pa++)
        {
          if (pa->first != "children"
              && ! obj.has_readonly_property (pa->first))
            {
              property p = get_properties ().get_property (pa->first);

              if (p.ok () && ! p.is_hidden ())
                {
                  retval += "\n\t" + std::string (pa->first) + ":  ";
                  if (p.is_radio ())
                    retval += p.values_as_string ();
                }
            }
        }

      if (! retval.empty ())
        retval += "\n";
    }
  else
    error ("base_graphics_object::values_as_string: invalid graphics object");

  return retval;
}

std::string
base_graphics_object::value_as_string (const std::string& prop)
{
  std::string retval;

  if (valid_object ())
    {
      graphics_object obj = gh_manager::get_object (get_handle ());

      if (prop != "children" && ! obj.has_readonly_property (prop))
        {
          property p = get_properties ().get_property (prop);

          if (p.ok () && ! p.is_hidden ())
            {
              if (p.is_radio ())
                retval += p.values_as_string ();
            }
        }

      if (! retval.empty ())
        retval += "\n";
    }
  else
    error ("base_graphics_object::value_as_string: invalid graphics object");

  return retval;
}

octave_scalar_map
base_graphics_object::values_as_struct (void)
{
  octave_scalar_map retval;

  if (valid_object ())
    {
      octave_scalar_map m = get ().scalar_map_value ();
      graphics_object obj = gh_manager::get_object (get_handle ());

      for (octave_scalar_map::const_iterator pa = m.begin ();
           pa != m.end (); pa++)
        {
          if (pa->first != "children"
              && ! obj.has_readonly_property (pa->first))
            {
              property p = get_properties ().get_property (pa->first);

              if (p.ok () && ! p.is_hidden ())
                {
                  if (p.is_radio ())
                    retval.assign (p.get_name (), p.values_as_cell ());
                  else
                    retval.assign (p.get_name (), Cell ());
                }
            }
        }
    }
  else
    error ("base_graphics_object::values_as_struct: invalid graphics object");

  return retval;
}

/*
%!test
%! hfig = figure ("visible", "off");
%! unwind_protect
%!   hax = axes ();
%!   ret = set (hax, "tightinset");
%!   assert (isempty (ret));
%!   ret = set (hax, "type");
%!   assert (isempty (ret));
%!   ret = set (hfig, "tag");
%!   assert (isempty (ret));
%!   ret = set (0, "commandwindowsize");
%!   assert (isempty (ret));
%!   ret = set (0);
%!   assert (! isfield (ret, "commandwindowsize"));
%! unwind_protect_cleanup
%!   close (hfig);
%! end_unwind_protect
*/

graphics_object
graphics_object::get_ancestor (const std::string& obj_type) const
{
  if (valid_object ())
    {
      if (isa (obj_type))
        return *this;
      else
        return gh_manager::get_object (get_parent ()).get_ancestor (obj_type);
    }
  else
    return graphics_object ();
}

// ---------------------------------------------------------------------

#include "graphics-props.cc"

// ---------------------------------------------------------------------

void
root_figure::properties::set_callbackobject (const octave_value& v)
{
  graphics_handle val (v);

  if (error_state)
    return;

  if (xisnan (val.value ()))
    {
      if (! cbo_stack.empty ())
        {
          val = cbo_stack.front ();

          cbo_stack.pop_front ();
        }

      callbackobject = val;
    }
  else if (is_handle (val))
    {
      if (get_callbackobject ().ok ())
        cbo_stack.push_front (get_callbackobject ());

      callbackobject = val;
    }
  else
    gripe_set_invalid ("callbackobject");
}

void
root_figure::properties::set_currentfigure (const octave_value& v)
{
  graphics_handle val (v);

  if (error_state)
    return;

  if (xisnan (val.value ()) || is_handle (val))
    {
      currentfigure = val;

      if (val.ok ())
        gh_manager::push_figure (val);
    }
  else
    gripe_set_invalid ("currentfigure");
}

std::string
root_figure::properties::get_diary (void) const
{
  bool is_diary_on = F__diarystate__ ()(0).bool_value ();
  if (is_diary_on)
    return std::string ("on");
  else
    return std::string ("off");
}

void
root_figure::properties::set_diary (const octave_value& val)
{
  if (! error_state)
    {
      // Input checking and abrev. matching
      diary.set (val, false);

      if (! error_state)
        {
          Fdiary (ovl (diary.current_value ()));

          diary.run_listeners ();
        }
    }
}

std::string
root_figure::properties::get_diaryfile (void) const
{
  return F__diaryfile__ ()(0).string_value ();
}

void
root_figure::properties::set_diaryfile (const octave_value& val)
{
  if (! error_state)
    {
      // Input checking and abrev. matching
      diaryfile.set (val, false);

      if (! error_state)
        {
          Fdiary (ovl (diaryfile.string_value ()));

          diaryfile.run_listeners ();
        }
    }
}

std::string
root_figure::properties::get_echo (void) const
{
  bool is_echo_on = F__echostate__ ()(0).bool_value ();
  if (is_echo_on)
    return std::string ("on");
  else
    return std::string ("off");
}

void
root_figure::properties::set_echo (const octave_value& val)
{
  if (! error_state)
    {
      // Input checking and abrev. matching
      echo.set (val, false);

      if (! error_state)
        {
          Fecho (ovl (echo.current_value ()));

          echo.run_listeners ();
        }
    }
}

std::string
root_figure::properties::get_errormessage (void) const
{
  return Flasterr ()(0).string_value ();
}

std::string
root_figure::properties::get_format (void) const
{
  return F__formatstring__ ()(0).string_value ();
}

void
root_figure::properties::set_format (const octave_value& val)
{
  if (! error_state)
    {
      // Input checking and abrev. matching
      format.set (val, false);

      if (! error_state)
        {
          Fformat (ovl (format.current_value ()));

          format.run_listeners ();
        }
    }
}

std::string
root_figure::properties::get_formatspacing (void) const
{
  bool iscompact = F__compactformat__ ()(0).bool_value ();
  if (iscompact)
    return std::string ("compact");
  else
    return std::string ("loose");
}

void
root_figure::properties::set_formatspacing (const octave_value& val)
{
  if (! error_state)
    {
      // Input checking and abrev. matching
      formatspacing.set (val, false);

      if (! error_state)
        {
          std::string strval = formatspacing.current_value ();

          if (strval == "compact")
            F__compactformat__ (ovl (true));
          else
            F__compactformat__ (ovl (false));

          formatspacing.run_listeners ();
        }
    }
}


double
root_figure::properties::get_recursionlimit (void) const
{
  return Fmax_recursion_depth ()(0).double_value ();
}

void
root_figure::properties::set_recursionlimit (const octave_value& val)
{
  if (! error_state)
    {
      // Input checking and abrev. matching
      recursionlimit.set (val, false);

      if (! error_state)
        {
          double dval = recursionlimit.double_value ();

          Fmax_recursion_depth (ovl (dval));

          recursionlimit.run_listeners ();
        }
    }
}

void
figure::properties::set_integerhandle (const octave_value& val)
{
  if (! error_state)
    {
      if (integerhandle.set (val, true))
        {
          bool int_fig_handle = integerhandle.is_on ();

          graphics_object this_go = gh_manager::get_object (__myhandle__);

          graphics_handle old_myhandle = __myhandle__;

          __myhandle__ = gh_manager::get_handle (int_fig_handle);

          gh_manager::renumber_figure (old_myhandle, __myhandle__);

          graphics_object parent_go = gh_manager::get_object (get_parent ());

          base_properties& props = parent_go.get_properties ();

          props.renumber_child (old_myhandle, __myhandle__);

          Matrix kids = get_children ();

          for (octave_idx_type i = 0; i < kids.numel (); i++)
            {
              graphics_object kid = gh_manager::get_object (kids(i));

              kid.get_properties ().renumber_parent (__myhandle__);
            }

          graphics_handle cf = gh_manager::current_figure ();

          if (__myhandle__ == cf)
            xset (0, "currentfigure", __myhandle__.value ());

          this_go.update (integerhandle.get_id ());

          mark_modified ();
        }
    }
}

// FIXME: This should update monitorpositions and pointerlocation, but
// as these properties are yet used, and so it doesn't matter that they
// aren't set yet.
void
root_figure::properties::update_units (void)
{
  caseless_str xunits = get_units ();

  Matrix ss = default_screensize ();

  double dpi = get_screenpixelsperinch ();

  if (xunits.compare ("inches"))
    {
      ss(0) = 0;
      ss(1) = 0;
      ss(2) /= dpi;
      ss(3) /= dpi;
    }
  else if (xunits.compare ("centimeters"))
    {
      ss(0) = 0;
      ss(1) = 0;
      ss(2) *= 2.54 / dpi;
      ss(3) *= 2.54 / dpi;
    }
  else if (xunits.compare ("normalized"))
    {
      ss = Matrix (1, 4, 1.0);
      ss(0) = 0;
      ss(1) = 0;
    }
  else if (xunits.compare ("points"))
    {
      ss(0) = 0;
      ss(1) = 0;
      ss(2) *= 72 / dpi;
      ss(3) *= 72 / dpi;
    }

  set_screensize (ss);
}

Matrix
root_figure::properties::get_boundingbox (bool, const Matrix&) const
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = Matrix (1, 4, 0);
  pos(2) = screen_size(0);
  pos(3) = screen_size(1);
  return pos;
}

/*
%!test
%! old_units = get (0, "units");
%! unwind_protect
%!   set (0, "units", "pixels");
%!   sz = get (0, "screensize") - [1, 1, 0, 0];
%!   dpi = get (0, "screenpixelsperinch");
%!   set (0, "units", "inches");
%!   assert (get (0, "screensize"), sz / dpi, 0.5 / dpi);
%!   set (0, "units", "centimeters");
%!   assert (get (0, "screensize"), sz / dpi * 2.54, 0.5 / dpi * 2.54);
%!   set (0, "units", "points");
%!   assert (get (0, "screensize"), sz / dpi * 72, 0.5 / dpi * 72);
%!   set (0, "units", "normalized");
%!   assert (get (0, "screensize"), [0.0, 0.0, 1.0, 1.0]);
%!   set (0, "units", "pixels");
%!   assert (get (0, "screensize"), sz + [1, 1, 0, 0]);
%! unwind_protect_cleanup
%!   set (0, "units", old_units);
%! end_unwind_protect
*/

void
root_figure::properties::remove_child (const graphics_handle& gh)
{
  gh_manager::pop_figure (gh);

  graphics_handle cf = gh_manager::current_figure ();

  xset (0, "currentfigure", cf.value ());

  base_properties::remove_child (gh);
}

property_list
root_figure::factory_properties = root_figure::init_factory_properties ();

void
root_figure::reset_default_properties (void)
{
  // empty list of local defaults
  default_properties = property_list ();

  xreset_default_properties (get_handle (),
                             xproperties.factory_defaults ());
}

// ---------------------------------------------------------------------

void
figure::properties::set_currentaxes (const octave_value& v)
{
  graphics_handle val (v);

  if (error_state)
    return;

  if (xisnan (val.value ()) || is_handle (val))
    currentaxes = val;
  else
    gripe_set_invalid ("currentaxes");
}

void
figure::properties::remove_child (const graphics_handle& gh)
{
  base_properties::remove_child (gh);

  if (gh == currentaxes.handle_value ())
    {
      graphics_handle new_currentaxes;

      Matrix kids = get_children ();

      for (octave_idx_type i = 0; i < kids.numel (); i++)
        {
          graphics_handle kid = kids(i);

          graphics_object go = gh_manager::get_object (kid);

          if (go.isa ("axes"))
            {
              new_currentaxes = kid;
              break;
            }
        }

      currentaxes = new_currentaxes;
    }
}

void
figure::properties::adopt (const graphics_handle& h)
{
  base_properties::adopt (h);

  if (! get_currentaxes ().ok ())
    {
      graphics_object go = gh_manager::get_object (h);

      if (go.type () == "axes")
        set_currentaxes (h.as_octave_value ());
    }
}

/*
%!test
%! hf1 = figure ("visible", "off");
%! ax1 = subplot (1,2,1);
%! ax2 = subplot (1,2,2);
%! hf2 = figure ("visible", "off");
%! unwind_protect
%!   set (ax2, "parent", hf2);
%!   assert (get (hf2, "currentaxes"), ax2);
%!   assert (get (hf1, "currentaxes"), ax1);
%!   set (ax1, "parent", hf2);
%!   assert (get (hf2, "currentaxes"), ax2);
%! unwind_protect_cleanup
%!   close (hf1);
%!   close (hf2);
%! end_unwind_protect
*/

void
figure::properties::set_visible (const octave_value& val)
{
  std::string s = val.string_value ();

  if (! error_state)
    {
      if (s == "on")
        xset (0, "currentfigure", __myhandle__.value ());

      visible = val;
    }
}

Matrix
figure::properties::get_boundingbox (bool internal, const Matrix&) const
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = (internal ?
                get_position ().matrix_value () :
                get_outerposition ().matrix_value ());

  pos = convert_position (pos, get_units (), "pixels", screen_size);

  pos(0)--;
  pos(1)--;
  pos(1) = screen_size(1) - pos(1) - pos(3);

  return pos;
}

void
figure::properties::set_boundingbox (const Matrix& bb, bool internal,
                                     bool do_notify_toolkit)
{
  Matrix screen_size = screen_size_pixels ();
  Matrix pos = bb;

  pos(1) = screen_size(1) - pos(1) - pos(3);
  pos(1)++;
  pos(0)++;
  pos = convert_position (pos, "pixels", get_units (), screen_size);

  if (internal)
    set_position (pos, do_notify_toolkit);
  else
    set_outerposition (pos, do_notify_toolkit);
}

Matrix
figure::properties::map_from_boundingbox (double x, double y) const
{
  Matrix bb = get_boundingbox (true);
  Matrix pos (1, 2, 0);

  pos(0) = x;
  pos(1) = y;

  pos(1) = bb(3) - pos(1);
  pos(0)++;
  pos = convert_position (pos, "pixels", get_units (),
                          bb.extract_n (0, 2, 1, 2));

  return pos;
}

Matrix
figure::properties::map_to_boundingbox (double x, double y) const
{
  Matrix bb = get_boundingbox (true);
  Matrix pos (1, 2, 0);

  pos(0) = x;
  pos(1) = y;

  pos = convert_position (pos, get_units (), "pixels",
                          bb.extract_n (0, 2, 1, 2));
  pos(0)--;
  pos(1) = bb(3) - pos(1);

  return pos;
}

void
figure::properties::set_position (const octave_value& v,
                                  bool do_notify_toolkit)
{
  if (! error_state)
    {
      Matrix old_bb, new_bb;
      bool modified = false;

      old_bb = get_boundingbox (true);
      modified = position.set (v, false, do_notify_toolkit);
      new_bb = get_boundingbox (true);

      if (old_bb != new_bb)
        {
          if (old_bb(2) != new_bb(2) || old_bb(3) != new_bb(3))
            {
              execute_resizefcn ();
              update_boundingbox ();
            }
        }

      if (modified)
        {
          position.run_listeners (POSTSET);
          mark_modified ();
        }

      if (paperpositionmode.is ("auto"))
        paperposition.set (get_auto_paperposition ());
    }
}

void
figure::properties::set_outerposition (const octave_value& v,
                                       bool do_notify_toolkit)
{
  if (! error_state)
    {
      if (outerposition.set (v, true, do_notify_toolkit))
        {
          mark_modified ();
        }
    }
}

void
figure::properties::set_paperunits (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str typ = get_papertype ();
      caseless_str punits = v.string_value ();
      if (! error_state)
        {
          if (punits.compare ("normalized") && typ.compare ("<custom>"))
            error ("set: can't set the paperunits to normalized when the papertype is custom");
          else
            {
              caseless_str old_paperunits = get_paperunits ();
              if (paperunits.set (v, true))
                {
                  update_paperunits (old_paperunits);
                  mark_modified ();
                }
            }
        }
    }
}

void
figure::properties::set_papertype (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str typ = v.string_value ();
      caseless_str punits = get_paperunits ();
      if (! error_state)
        {
          if (punits.compare ("normalized") && typ.compare ("<custom>"))
            error ("set: can't set the paperunits to normalized when the papertype is custom");
          else
            {
              if (papertype.set (v, true))
                {
                  update_papertype ();
                  mark_modified ();
                }
            }
        }
    }
}

static Matrix
papersize_from_type (const caseless_str punits, const caseless_str typ)
{
  Matrix ret (1, 2, 1.0);

  if (! punits.compare ("normalized"))
    {
      double in2units;
      double mm2units;

      if (punits.compare ("inches"))
        {
          in2units = 1.0;
          mm2units = 1 / 25.4 ;
        }
      else if (punits.compare ("centimeters"))
        {
          in2units = 2.54;
          mm2units = 1 / 10.0;
        }
      else // points
        {
          in2units = 72.0;
          mm2units = 72.0 / 25.4;
        }

      if (typ.compare ("usletter"))
        {
          ret (0) = 8.5 * in2units;
          ret (1) = 11.0 * in2units;
        }
      else if (typ.compare ("uslegal"))
        {
          ret (0) = 8.5 * in2units;
          ret (1) = 14.0 * in2units;
        }
      else if (typ.compare ("tabloid"))
        {
          ret (0) = 11.0 * in2units;
          ret (1) = 17.0 * in2units;
        }
      else if (typ.compare ("a0"))
        {
          ret (0) = 841.0 * mm2units;
          ret (1) = 1189.0 * mm2units;
        }
      else if (typ.compare ("a1"))
        {
          ret (0) = 594.0 * mm2units;
          ret (1) = 841.0 * mm2units;
        }
      else if (typ.compare ("a2"))
        {
          ret (0) = 420.0 * mm2units;
          ret (1) = 594.0 * mm2units;
        }
      else if (typ.compare ("a3"))
        {
          ret (0) = 297.0 * mm2units;
          ret (1) = 420.0 * mm2units;
        }
      else if (typ.compare ("a4"))
        {
          ret (0) = 210.0 * mm2units;
          ret (1) = 297.0 * mm2units;
        }
      else if (typ.compare ("a5"))
        {
          ret (0) = 148.0 * mm2units;
          ret (1) = 210.0 * mm2units;
        }
      else if (typ.compare ("b0"))
        {
          ret (0) = 1029.0 * mm2units;
          ret (1) = 1456.0 * mm2units;
        }
      else if (typ.compare ("b1"))
        {
          ret (0) = 728.0 * mm2units;
          ret (1) = 1028.0 * mm2units;
        }
      else if (typ.compare ("b2"))
        {
          ret (0) = 514.0 * mm2units;
          ret (1) = 728.0 * mm2units;
        }
      else if (typ.compare ("b3"))
        {
          ret (0) = 364.0 * mm2units;
          ret (1) = 514.0 * mm2units;
        }
      else if (typ.compare ("b4"))
        {
          ret (0) = 257.0 * mm2units;
          ret (1) = 364.0 * mm2units;
        }
      else if (typ.compare ("b5"))
        {
          ret (0) = 182.0 * mm2units;
          ret (1) = 257.0 * mm2units;
        }
      else if (typ.compare ("arch-a"))
        {
          ret (0) = 9.0 * in2units;
          ret (1) = 12.0 * in2units;
        }
      else if (typ.compare ("arch-b"))
        {
          ret (0) = 12.0 * in2units;
          ret (1) = 18.0 * in2units;
        }
      else if (typ.compare ("arch-c"))
        {
          ret (0) = 18.0 * in2units;
          ret (1) = 24.0 * in2units;
        }
      else if (typ.compare ("arch-d"))
        {
          ret (0) = 24.0 * in2units;
          ret (1) = 36.0 * in2units;
        }
      else if (typ.compare ("arch-e"))
        {
          ret (0) = 36.0 * in2units;
          ret (1) = 48.0 * in2units;
        }
      else if (typ.compare ("a"))
        {
          ret (0) = 8.5 * in2units;
          ret (1) = 11.0 * in2units;
        }
      else if (typ.compare ("b"))
        {
          ret (0) = 11.0 * in2units;
          ret (1) = 17.0 * in2units;
        }
      else if (typ.compare ("c"))
        {
          ret (0) = 17.0 * in2units;
          ret (1) = 22.0 * in2units;
        }
      else if (typ.compare ("d"))
        {
          ret (0) = 22.0 * in2units;
          ret (1) = 34.0 * in2units;
        }
      else if (typ.compare ("e"))
        {
          ret (0) = 34.0 * in2units;
          ret (1) = 43.0 * in2units;
        }
    }

  return ret;
}


Matrix
figure::properties::get_auto_paperposition (void)
{
  Matrix pos = get_position ().matrix_value ();
  Matrix sz;

  caseless_str funits = get_units ();
  caseless_str punits = get_paperunits ();

  // Convert position from figure units to paperunits
  if (funits == "normalized" || punits == "normalized")
    {
      sz = screen_size_pixels ();
      pos = convert_position (pos, funits, "inches", sz);

      if (punits == "normalized")
        sz = papersize_from_type ("points", get_papertype ());

      pos = convert_position (pos, "inches", punits, sz);
    }
  else
    pos = convert_position (pos, funits, punits, sz);

  // Center the figure on the page
  sz = get_papersize ().matrix_value ();

  pos(0) = sz(0)/2 - pos(2)/2;
  pos(1) = sz(1)/2 - pos(3)/2;

  return pos;
}

/*
%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## paperpositionmode "auto" converts figure size to paper units
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   set (hf, "paperunits", "centimeters");
%!   psz = get (hf, "papersize");
%!   fsz = in_pos(3:4) * 2.54;
%!   pos = [(psz/2 .- fsz/2) fsz];
%!   set (hf, "paperpositionmode", "auto");
%!   assert (get (hf, "paperposition"), pos, tol)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## likewise with normalized units
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   psz = get (hf, "papersize");
%!   set (hf, "paperunits", "normalized");
%!   fsz = in_pos(3:4) ./ psz;
%!   pos = [([0.5 0.5] .- fsz/2) fsz];
%!   assert (get (hf, "paperposition"), pos, tol)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## changing papertype updates paperposition
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   set  (hf, "papertype", "a4");
%!   psz = get (hf, "papersize");
%!   fsz = in_pos(3:4);
%!   pos = [(psz/2 .- fsz/2) fsz];
%!   assert (get (hf, "paperposition"), pos, tol)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! tol = 20 * eps ();
%! unwind_protect
%!   ## lanscape updates paperposition
%!   set (hf, "units", "inches");
%!   set (hf, "position", in_pos);
%!   set (hf, "paperorientation", "landscape");
%!   psz = get (hf, "papersize");
%!   fsz = in_pos(3:4);
%!   pos = [(psz/2 .- fsz/2) fsz];
%!   assert (get (hf, "paperposition"), pos, tol)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off", "paperpositionmode", "auto");
%! in_pos = [0 0 4 5];
%! unwind_protect
%!   ## back to manual mode
%!   set (hf, "paperposition", in_pos * 1.1)
%!   assert (get (hf, "paperpositionmode"), "manual")
%!   assert (get (hf, "paperposition"), in_pos * 1.1)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
figure::properties::update_paperunits (const caseless_str& old_paperunits)
{
  Matrix pos = get_paperposition ().matrix_value ();
  Matrix sz = get_papersize ().matrix_value ();

  pos(0) /= sz(0);
  pos(1) /= sz(1);
  pos(2) /= sz(0);
  pos(3) /= sz(1);

  std::string porient = get_paperorientation ();
  caseless_str punits = get_paperunits ();
  caseless_str typ = get_papertype ();

  if (typ.compare ("<custom>"))
    {
      if (old_paperunits.compare ("centimeters"))
        {
          sz(0) /= 2.54;
          sz(1) /= 2.54;
        }
      else if (old_paperunits.compare ("points"))
        {
          sz(0) /= 72.0;
          sz(1) /= 72.0;
        }

      if (punits.compare ("centimeters"))
        {
          sz(0) *= 2.54;
          sz(1) *= 2.54;
        }
      else if (punits.compare ("points"))
        {
          sz(0) *= 72.0;
          sz(1) *= 72.0;
        }
    }
  else
    {
      sz = papersize_from_type (punits, typ);
      if (porient == "landscape")
        std::swap (sz(0), sz(1));
    }

  pos(0) *= sz(0);
  pos(1) *= sz(1);
  pos(2) *= sz(0);
  pos(3) *= sz(1);

  papersize.set (octave_value (sz));
  paperposition.set (octave_value (pos));
}

void
figure::properties::update_papertype (void)
{
  caseless_str typ = get_papertype ();
  if (! typ.compare ("<custom>"))
    {
      Matrix sz = papersize_from_type (get_paperunits (), typ);
      if (get_paperorientation () == "landscape")
        std::swap (sz(0), sz(1));
      // Call papersize.set rather than set_papersize to avoid loops
      // between update_papersize and update_papertype
      papersize.set (octave_value (sz));
    }

  if (paperpositionmode.is ("auto"))
    paperposition.set (get_auto_paperposition ());
}

void
figure::properties::update_papersize (void)
{
  Matrix sz = get_papersize ().matrix_value ();
  if (sz(0) > sz(1))
    {
      std::swap (sz(0), sz(1));
      papersize.set (octave_value (sz));
      paperorientation.set (octave_value ("landscape"));
    }
  else
    {
      paperorientation.set ("portrait");
    }
  std::string punits = get_paperunits ();
  if (punits == "centimeters")
    {
      sz(0) /= 2.54;
      sz(1) /= 2.54;
    }
  else if (punits == "points")
    {
      sz(0) /= 72.0;
      sz(1) /= 72.0;
    }
  if (punits == "normalized")
    {
      caseless_str typ = get_papertype ();
      if (get_papertype () == "<custom>")
        error ("set: can't set the papertype to <custom> when the paperunits is normalized");
    }
  else
    {
      // TODO - the papersizes info is also in papersize_from_type().
      // Both should be rewritten to avoid the duplication.
      std::string typ = "<custom>";
      const double mm2in = 1.0 / 25.4;
      const double tol = 0.01;

      if (std::abs (sz(0) - 8.5) + std::abs (sz(1) - 11.0) < tol)
        typ = "usletter";
      else if (std::abs (sz(0) - 8.5) + std::abs (sz(1) - 14.0) < tol)
        typ = "uslegal";
      else if (std::abs (sz(0) - 11.0) + std::abs (sz(1) - 17.0) < tol)
        typ = "tabloid";
      else if (std::abs (sz(0) - 841.0 * mm2in)
               + std::abs (sz(1) - 1198.0 * mm2in) < tol)
        typ = "a0";
      else if (std::abs (sz(0) - 594.0 * mm2in)
               + std::abs (sz(1) - 841.0 * mm2in) < tol)
        typ = "a1";
      else if (std::abs (sz(0) - 420.0 * mm2in)
               + std::abs (sz(1) - 594.0 * mm2in) < tol)
        typ = "a2";
      else if (std::abs (sz(0) - 297.0 * mm2in)
               + std::abs (sz(1) - 420.0 * mm2in) < tol)
        typ = "a3";
      else if (std::abs (sz(0) - 210.0 * mm2in)
               + std::abs (sz(1) - 297.0 * mm2in) < tol)
        typ = "a4";
      else if (std::abs (sz(0) - 148.0 * mm2in)
               + std::abs (sz(1) - 210.0 * mm2in) < tol)
        typ = "a5";
      else if (std::abs (sz(0) - 1029.0 * mm2in)
               + std::abs (sz(1) - 1456.0 * mm2in) < tol)
        typ = "b0";
      else if (std::abs (sz(0) - 728.0 * mm2in)
               + std::abs (sz(1) - 1028.0 * mm2in) < tol)
        typ = "b1";
      else if (std::abs (sz(0) - 514.0 * mm2in)
               + std::abs (sz(1) - 728.0 * mm2in) < tol)
        typ = "b2";
      else if (std::abs (sz(0) - 364.0 * mm2in)
               + std::abs (sz(1) - 514.0 * mm2in) < tol)
        typ = "b3";
      else if (std::abs (sz(0) - 257.0 * mm2in)
               + std::abs (sz(1) - 364.0 * mm2in) < tol)
        typ = "b4";
      else if (std::abs (sz(0) - 182.0 * mm2in)
               + std::abs (sz(1) - 257.0 * mm2in) < tol)
        typ = "b5";
      else if (std::abs (sz(0) - 9.0)
               + std::abs (sz(1) - 12.0) < tol)
        typ = "arch-a";
      else if (std::abs (sz(0) - 12.0)
               + std::abs (sz(1) - 18.0) < tol)
        typ = "arch-b";
      else if (std::abs (sz(0) - 18.0)
               + std::abs (sz(1) - 24.0) < tol)
        typ = "arch-c";
      else if (std::abs (sz(0) - 24.0)
               + std::abs (sz(1) - 36.0) < tol)
        typ = "arch-d";
      else if (std::abs (sz(0) - 36.0)
               + std::abs (sz(1) - 48.0) < tol)
        typ = "arch-e";
      else if (std::abs (sz(0) - 8.5)
               + std::abs (sz(1) - 11.0) < tol)
        typ = "a";
      else if (std::abs (sz(0) - 11.0)
               + std::abs (sz(1) - 17.0) < tol)
        typ = "b";
      else if (std::abs (sz(0) - 17.0)
               + std::abs (sz(1) - 22.0) < tol)
        typ = "c";
      else if (std::abs (sz(0) - 22.0)
               + std::abs (sz(1) - 34.0) < tol)
        typ = "d";
      else if (std::abs (sz(0) - 34.0)
               + std::abs (sz(1) - 43.0) < tol)
        typ = "e";
      // Call papertype.set rather than set_papertype to avoid loops between
      // update_papersize and update_papertype
      papertype.set (typ);
    }
  if (punits == "centimeters")
    {
      sz(0) *= 2.54;
      sz(1) *= 2.54;
    }
  else if (punits == "points")
    {
      sz(0) *= 72.0;
      sz(1) *= 72.0;
    }
  if (get_paperorientation () == "landscape")
    {
      std::swap (sz(0), sz(1));
      papersize.set (octave_value (sz));
    }

  if (paperpositionmode.is ("auto"))
    paperposition.set (get_auto_paperposition ());
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (hf, "paperunits", "inches");
%!   set (hf, "papersize", [5, 4]);
%!   set (hf, "paperunits", "points");
%!   assert (get (hf, "papersize"), [5, 4] * 72, 1);
%!   papersize = get (hf, "papersize");
%!   set (hf, "papersize", papersize + 1);
%!   set (hf, "papersize", papersize);
%!   assert (get (hf, "papersize"), [5, 4] * 72, 1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   set (hf, "paperunits", "inches");
%!   set (hf, "papersize", [5, 4]);
%!   set (hf, "paperunits", "centimeters");
%!   assert (get (hf, "papersize"), [5, 4] * 2.54, 2.54/72);
%!   papersize = get (hf, "papersize");
%!   set (hf, "papersize", papersize + 1);
%!   set (hf, "papersize", papersize);
%!   assert (get (hf, "papersize"), [5, 4] * 2.54, 2.54/72);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
figure::properties::update_paperorientation (void)
{
  std::string porient = get_paperorientation ();
  Matrix sz = get_papersize ().matrix_value ();
  Matrix pos = get_paperposition ().matrix_value ();
  if ((sz(0) > sz(1) && porient == "portrait")
      || (sz(0) < sz(1) && porient == "landscape"))
    {
      std::swap (sz(0), sz(1));
      std::swap (pos(0), pos(1));
      std::swap (pos(2), pos(3));
      // Call papertype.set rather than set_papertype to avoid loops
      // between update_papersize and update_papertype
      papersize.set (octave_value (sz));
      paperposition.set (octave_value (pos));
    }

  if (paperpositionmode.is ("auto"))
    paperposition.set (get_auto_paperposition ());
}

/*
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 100 * eps ();
%!   ## UPPER case and MiXed case is part of test and should not be changed.
%!   set (hf, "paperorientation", "PORTRAIT");
%!   set (hf, "paperunits", "inches");
%!   set (hf, "papertype", "USletter");
%!   assert (get (hf, "papersize"), [8.5, 11.0], tol);
%!   set (hf, "paperorientation", "Landscape");
%!   assert (get (hf, "papersize"), [11.0, 8.5], tol);
%!   set (hf, "paperunits", "centimeters");
%!   assert (get (hf, "papersize"), [11.0, 8.5] * 2.54, tol);
%!   set (hf, "papertype", "a4");
%!   assert (get (hf, "papersize"), [29.7, 21.0], tol);
%!   set (hf, "paperunits", "inches", "papersize", [8.5, 11.0]);
%!   assert (get (hf, "papertype"), "usletter");
%!   assert (get (hf, "paperorientation"), "portrait");
%!   set (hf, "papersize", [11.0, 8.5]);
%!   assert (get (hf, "papertype"), "usletter");
%!   assert (get (hf, "paperorientation"), "landscape");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
figure::properties::set_units (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_units = get_units ();
      if (units.set (v, true))
        {
          update_units (old_units);
          mark_modified ();
        }
    }
}

void
figure::properties::update_units (const caseless_str& old_units)
{
  position.set (convert_position (get_position ().matrix_value (), old_units,
                                  get_units (), screen_size_pixels ()), false);
}

/*
%!test
%! hf = figure ("visible", "off");
%! old_units = get (0, "units");
%! unwind_protect
%!   set (0, "units", "pixels");
%!   rsz = get (0, "screensize");
%!   set (gcf (), "units", "pixels");
%!   fsz = get (gcf (), "position");
%!   set (gcf (), "units", "normalized");
%!   pos = get (gcf (), "position");
%!   assert (pos, (fsz - [1, 1, 0, 0]) ./ rsz([3, 4, 3, 4]));
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "units", old_units);
%! end_unwind_protect
*/

std::string
figure::properties::get_title (void) const
{
  if (is_numbertitle ())
    {
      std::ostringstream os;
      std::string nm = get_name ();

      os << "Figure " << __myhandle__.value ();
      if (! nm.empty ())
        os << ": " << get_name ();

      return os.str ();
    }
  else
    return get_name ();
}

octave_value
figure::get_default (const caseless_str& name) const
{
  octave_value retval = default_properties.lookup (name);

  if (retval.is_undefined ())
    {
      graphics_handle parent = get_parent ();
      graphics_object parent_obj = gh_manager::get_object (parent);

      retval = parent_obj.get_default (name);
    }

  return retval;
}

void
figure::reset_default_properties (void)
{
  // empty list of local defaults
  default_properties = property_list ();

  property_list::pval_map_type plist = xproperties.factory_defaults ();
  plist.erase ("units");
  plist.erase ("position");
  plist.erase ("outerposition");
  plist.erase ("paperunits");
  plist.erase ("paperposition");
  plist.erase ("windowstyle");

  xreset_default_properties (get_handle (), plist);
}

// ---------------------------------------------------------------------

void
axes::properties::init (void)
{
  position.add_constraint (dim_vector (1, 4));
  outerposition.add_constraint (dim_vector (1, 4));
  tightinset.add_constraint (dim_vector (1, 4));
  looseinset.add_constraint (dim_vector (1, 4));
  colororder.add_constraint (dim_vector (-1, 3));
  dataaspectratio.add_constraint (dim_vector (1, 3));
  plotboxaspectratio.add_constraint (dim_vector (1, 3));
  alim.add_constraint (2);
  clim.add_constraint (2);
  xlim.add_constraint (2);
  ylim.add_constraint (2);
  zlim.add_constraint (2);
  xtick.add_constraint (dim_vector (1, -1));
  ytick.add_constraint (dim_vector (1, -1));
  ztick.add_constraint (dim_vector (1, -1));
  ticklength.add_constraint (dim_vector (1, 2));
  Matrix vw (1, 2, 0);
  vw(1) = 90;
  view = vw;
  view.add_constraint (dim_vector (1, 2));
  cameraposition.add_constraint (dim_vector (1, 3));
  cameratarget.add_constraint (dim_vector (1, 3));
  Matrix upv (1, 3, 0.0);
  upv(2) = 1.0;
  cameraupvector = upv;
  cameraupvector.add_constraint (dim_vector (1, 3));
  currentpoint.add_constraint (dim_vector (2, 3));
  // No constraints for hidden transform properties
  update_font ();

  x_zlim.resize (1, 2);

  sx = "linear";
  sy = "linear";
  sz = "linear";

  calc_ticklabels (xtick, xticklabel, xscale.is ("log"));
  calc_ticklabels (ytick, yticklabel, yscale.is ("log"));
  calc_ticklabels (ztick, zticklabel, zscale.is ("log"));

  xset (xlabel.handle_value (), "handlevisibility", "off");
  xset (ylabel.handle_value (), "handlevisibility", "off");
  xset (zlabel.handle_value (), "handlevisibility", "off");
  xset (title.handle_value (), "handlevisibility", "off");

  xset (xlabel.handle_value (), "horizontalalignment", "center");
  xset (xlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (ylabel.handle_value (), "horizontalalignment", "center");
  xset (ylabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (zlabel.handle_value (), "horizontalalignment", "right");
  xset (zlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (title.handle_value (), "horizontalalignment", "center");
  xset (title.handle_value (), "horizontalalignmentmode", "auto");

  xset (xlabel.handle_value (), "verticalalignment", "top");
  xset (xlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (ylabel.handle_value (), "verticalalignment", "bottom");
  xset (ylabel.handle_value (), "verticalalignmentmode", "auto");
  xset (title.handle_value (), "verticalalignment", "bottom");
  xset (title.handle_value (), "verticalalignmentmode", "auto");

  xset (ylabel.handle_value (), "rotation", 90.0);
  xset (ylabel.handle_value (), "rotationmode", "auto");

  xset (zlabel.handle_value (), "visible", "off");

  xset (xlabel.handle_value (), "clipping", "off");
  xset (ylabel.handle_value (), "clipping", "off");
  xset (zlabel.handle_value (), "clipping", "off");
  xset (title.handle_value (), "clipping", "off");

  xset (xlabel.handle_value (), "autopos_tag", "xlabel");
  xset (ylabel.handle_value (), "autopos_tag", "ylabel");
  xset (zlabel.handle_value (), "autopos_tag", "zlabel");
  xset (title.handle_value (), "autopos_tag", "title");

  adopt (xlabel.handle_value ());
  adopt (ylabel.handle_value ());
  adopt (zlabel.handle_value ());
  adopt (title.handle_value ());

  Matrix tlooseinset = default_axes_position ();
  tlooseinset(2) = 1-tlooseinset(0)-tlooseinset(2);
  tlooseinset(3) = 1-tlooseinset(1)-tlooseinset(3);
  looseinset = tlooseinset;
}

Matrix
axes::properties::calc_tightbox (const Matrix& init_pos)
{
  Matrix pos = init_pos;
  graphics_object obj = gh_manager::get_object (get_parent ());
  Matrix parent_bb = obj.get_properties ().get_boundingbox (true);
  Matrix ext = get_extent (true, true);
  ext(1) = parent_bb(3) - ext(1) - ext(3);
  ext(0)++;
  ext(1)++;
  ext = convert_position (ext, "pixels", get_units (),
                          parent_bb.extract_n (0, 2, 1, 2));
  if (ext(0) < pos(0))
    {
      pos(2) += pos(0)-ext(0);
      pos(0) = ext(0);
    }
  if (ext(0)+ext(2) > pos(0)+pos(2))
    pos(2) = ext(0)+ext(2)-pos(0);

  if (ext(1) < pos(1))
    {
      pos(3) += pos(1)-ext(1);
      pos(1) = ext(1);
    }
  if (ext(1)+ext(3) > pos(1)+pos(3))
    pos(3) = ext(1)+ext(3)-pos(1);
  return pos;
}

void
axes::properties::sync_positions (void)
{
  // First part is equivalent to `update_tightinset ()'
  if (activepositionproperty.is ("position"))
    update_position ();
  else
    update_outerposition ();
  caseless_str old_units = get_units ();
  set_units ("normalized");
  Matrix pos = position.get ().matrix_value ();
  Matrix outpos = outerposition.get ().matrix_value ();
  Matrix tightpos = calc_tightbox (pos);
  Matrix tinset (1, 4, 1.0);
  tinset(0) = pos(0)-tightpos(0);
  tinset(1) = pos(1)-tightpos(1);
  tinset(2) = tightpos(0)+tightpos(2)-pos(0)-pos(2);
  tinset(3) = tightpos(1)+tightpos(3)-pos(1)-pos(3);
  tightinset = tinset;
  set_units (old_units);
  update_transform ();
  if (activepositionproperty.is ("position"))
    update_position ();
  else
    update_outerposition ();
}

/*
%!testif HAVE_FLTK
%! hf = figure ("visible", "off");
%! graphics_toolkit (hf, "fltk");
%! unwind_protect
%!   subplot(2,1,1); plot(rand(10,1)); subplot(2,1,2); plot(rand(10,1));
%!   hax = findall (gcf (), "type", "axes");
%!   positions = cell2mat (get (hax, "position"));
%!   outerpositions = cell2mat (get (hax, "outerposition"));
%!   looseinsets = cell2mat (get (hax, "looseinset"));
%!   tightinsets = cell2mat (get (hax, "tightinset"));
%!   subplot(2,1,1); plot(rand(10,1)); subplot(2,1,2); plot(rand(10,1));
%!   hax = findall (gcf (), "type", "axes");
%!   assert (cell2mat (get (hax, "position")), positions, 1e-4);
%!   assert (cell2mat (get (hax, "outerposition")), outerpositions, 1e-4);
%!   assert (cell2mat (get (hax, "looseinset")), looseinsets, 1e-4);
%!   assert (cell2mat (get (hax, "tightinset")), tightinsets, 1e-4);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%!testif HAVE_FLTK
%! hf = figure ("visible", "off");
%! graphics_toolkit (hf, "fltk");
%! fpos = get (hf, "position");
%! unwind_protect
%!   plot (rand (3))
%!   position = get (gca, "position");
%!   outerposition = get (gca, "outerposition");
%!   looseinset = get (gca, "looseinset");
%!   tightinset = get (gca, "tightinset");
%!   set (hf, "position", [fpos(1:2), 2*fpos(3:4)])
%!   set (hf, "position", fpos);
%!   assert (get (gca, "outerposition"), outerposition, 0.001)
%!   assert (get (gca, "position"), position, 0.001)
%!   assert (get (gca, "looseinset"), looseinset, 0.001)
%!   assert (get (gca, "tightinset"), tightinset, 0.001)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
%!testif HAVE_FLTK
%! hf = figure ("visible", "off");
%! graphics_toolkit (hf, "fltk");
%! fpos = get (hf, "position");
%! set (gca, "activepositionproperty", "position")
%! unwind_protect
%!   plot (rand (3))
%!   position = get (gca, "position");
%!   outerposition = get (gca, "outerposition");
%!   looseinset = get (gca, "looseinset");
%!   tightinset = get (gca, "tightinset");
%!   set (hf, "position", [fpos(1:2), 2*fpos(3:4)])
%!   set (hf, "position", fpos);
%!   assert (get (gca, "position"), position, 0.001)
%!   assert (get (gca, "outerposition"), outerposition, 0.001)
%!   assert (get (gca, "looseinset"), looseinset, 0.001)
%!   assert (get (gca, "tightinset"), tightinset, 0.001)
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
*/

void
axes::properties::set_text_child (handle_property& hp,
                                  const std::string& who,
                                  const octave_value& v)
{
  if (v.is_string ())
    {
      xset (hp.handle_value (), "string", v);
      return;
    }

  graphics_handle val;
  graphics_object go = gh_manager::get_object (gh_manager::lookup (v));

  if (go.isa ("text"))
    val = ::reparent (v, "set", who, __myhandle__, false);
  else
    {
      std::string cname = v.class_name ();

      error ("set: expecting text graphics object or character string for %s property, found %s",
             who.c_str (), cname.c_str ());
    }

  if (! error_state)
    {
      xset (val, "handlevisibility", "off");

      gh_manager::free (hp.handle_value ());

      base_properties::remove_child (hp.handle_value ());

      hp = val;

      adopt (hp.handle_value ());
    }
}

void
axes::properties::set_xlabel (const octave_value& v)
{
  set_text_child (xlabel, "xlabel", v);
  xset (xlabel.handle_value (), "positionmode", "auto");
  xset (xlabel.handle_value (), "rotationmode", "auto");
  xset (xlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (xlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (xlabel.handle_value (), "clipping", "off");
  xset (xlabel.handle_value (), "color", get_xcolor ());
  xset (xlabel.handle_value (), "autopos_tag", "xlabel");
  update_xlabel_position ();
}

void
axes::properties::set_ylabel (const octave_value& v)
{
  set_text_child (ylabel, "ylabel", v);
  xset (ylabel.handle_value (), "positionmode", "auto");
  xset (ylabel.handle_value (), "rotationmode", "auto");
  xset (ylabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (ylabel.handle_value (), "verticalalignmentmode", "auto");
  xset (ylabel.handle_value (), "clipping", "off");
  xset (ylabel.handle_value (), "color", get_ycolor ());
  xset (ylabel.handle_value (), "autopos_tag", "ylabel");
  update_ylabel_position ();
}

void
axes::properties::set_zlabel (const octave_value& v)
{
  set_text_child (zlabel, "zlabel", v);
  xset (zlabel.handle_value (), "positionmode", "auto");
  xset (zlabel.handle_value (), "rotationmode", "auto");
  xset (zlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (zlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (zlabel.handle_value (), "clipping", "off");
  xset (zlabel.handle_value (), "color", get_zcolor ());
  xset (zlabel.handle_value (), "autopos_tag", "zlabel");
  update_zlabel_position ();
}

void
axes::properties::set_title (const octave_value& v)
{
  set_text_child (title, "title", v);
  xset (title.handle_value (), "positionmode", "auto");
  xset (title.handle_value (), "horizontalalignment", "center");
  xset (title.handle_value (), "horizontalalignmentmode", "auto");
  xset (title.handle_value (), "verticalalignment", "bottom");
  xset (title.handle_value (), "verticalalignmentmode", "auto");
  xset (title.handle_value (), "clipping", "off");
  xset (title.handle_value (), "autopos_tag", "title");
  update_title_position ();
}

void
axes::properties::set_defaults (base_graphics_object& obj,
                                const std::string& mode)
{
  box = "on";
  colororder = default_colororder ();
  // Note: dataspectratio will be set through update_aspectratios
  dataaspectratiomode = "auto";
  layer = "bottom";

  Matrix tlim (1, 2, 0.0);
  tlim(1) = 1;
  xlim = tlim;
  ylim = tlim;
  zlim = tlim;

  Matrix cl (1, 2, 0);
  cl(1) = 1;
  clim = cl;

  alim = tlim;

  xlimmode = "auto";
  ylimmode = "auto";
  zlimmode = "auto";
  climmode = "auto";
  alimmode = "auto";

  xgrid = "off";
  ygrid = "off";
  zgrid = "off";
  xminorgrid = "off";
  yminorgrid = "off";
  zminorgrid = "off";
  xtick = Matrix ();
  ytick = Matrix ();
  ztick = Matrix ();
  xtickmode = "auto";
  ytickmode = "auto";
  ztickmode = "auto";
  xminortick = "off";
  yminortick = "off";
  zminortick = "off";
  xticklabel = "";
  yticklabel = "";
  zticklabel = "";
  xticklabelmode = "auto";
  yticklabelmode = "auto";
  zticklabelmode = "auto";

  interpreter = "none";

  color = color_values ("white");
  xcolor = color_values ("black");
  ycolor = color_values ("black");
  zcolor = color_values ("black");
  xscale = "linear";
  yscale = "linear";
  zscale = "linear";
  xdir = "normal";
  ydir = "normal";
  zdir = "normal";
  yaxislocation = "left";
  xaxislocation = "bottom";

  Matrix tview (1, 2, 0.0);
  tview(1) = 90;
  view = tview;

  __hold_all__ = "off";
  nextplot = "replace";

  ambientlightcolor = Matrix (1, 3, 1.0);

  // Note: camera properties (not mode) will be set in update_transform
  camerapositionmode = "auto";
  cameratargetmode = "auto";
  cameraupvectormode = "auto";
  cameraviewanglemode = "auto";

  drawmode = "normal";

  fontangle = "normal";
  fontname = OCTAVE_DEFAULT_FONTNAME;
  fontsize = 10;
  fontunits = "points";
  fontweight = "normal";

  gridlinestyle = ":";
  linestyleorder = "-";
  linewidth = 0.5;
  minorgridlinestyle = ":";

  // Note: plotboxaspectratio will be set through update_aspectratios
  plotboxaspectratiomode = "auto";
  projection = "orthographic";

  tickdir = "in";
  tickdirmode = "auto";
  ticklength = default_axes_ticklength ();

  tightinset = Matrix (1, 4, 0.0);

  sx = "linear";
  sy = "linear";
  sz = "linear";

  visible = "on";

  // Replace/Reset preserves Position and Units properties
  if (mode != "replace" && mode != "reset")
    {
      outerposition = default_axes_outerposition ();
      position = default_axes_position ();
      activepositionproperty = "outerposition";
    }

  if (mode != "reset")
    {
      delete_children (true);

      xlabel.invalidate ();
      ylabel.invalidate ();
      zlabel.invalidate ();
      title.invalidate ();

      xlabel = gh_manager::make_graphics_handle ("text", __myhandle__,
                                                 false, false, false);
      ylabel = gh_manager::make_graphics_handle ("text", __myhandle__,
                                                 false, false, false);
      zlabel = gh_manager::make_graphics_handle ("text", __myhandle__,
                                                 false, false, false);
      title = gh_manager::make_graphics_handle ("text", __myhandle__,
                                                false, false, false);

      adopt (xlabel.handle_value ());
      adopt (ylabel.handle_value ());
      adopt (zlabel.handle_value ());
      adopt (title.handle_value ());

      update_xlabel_position ();
      update_ylabel_position ();
      update_zlabel_position ();
      update_title_position ();
    }
  else
    {
      graphics_object go = gh_manager::get_object (xlabel.handle_value ());
      go.reset_default_properties ();
      go = gh_manager::get_object (ylabel.handle_value ());
      go.reset_default_properties ();
      go = gh_manager::get_object (zlabel.handle_value ());
      go.reset_default_properties ();
      go = gh_manager::get_object (title.handle_value ());
      go.reset_default_properties ();
    }

  xset (xlabel.handle_value (), "handlevisibility", "off");
  xset (ylabel.handle_value (), "handlevisibility", "off");
  xset (zlabel.handle_value (), "handlevisibility", "off");
  xset (title.handle_value (), "handlevisibility", "off");

  xset (xlabel.handle_value (), "horizontalalignment", "center");
  xset (xlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (ylabel.handle_value (), "horizontalalignment", "center");
  xset (ylabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (zlabel.handle_value (), "horizontalalignment", "right");
  xset (zlabel.handle_value (), "horizontalalignmentmode", "auto");
  xset (title.handle_value (), "horizontalalignment", "center");
  xset (title.handle_value (), "horizontalalignmentmode", "auto");

  xset (xlabel.handle_value (), "verticalalignment", "top");
  xset (xlabel.handle_value (), "verticalalignmentmode", "auto");
  xset (ylabel.handle_value (), "verticalalignment", "bottom");
  xset (ylabel.handle_value (), "verticalalignmentmode", "auto");
  xset (title.handle_value (), "verticalalignment", "bottom");
  xset (title.handle_value (), "verticalalignmentmode", "auto");

  xset (ylabel.handle_value (), "rotation", 90.0);
  xset (ylabel.handle_value (), "rotationmode", "auto");

  xset (zlabel.handle_value (), "visible", "off");

  xset (xlabel.handle_value (), "clipping", "off");
  xset (ylabel.handle_value (), "clipping", "off");
  xset (zlabel.handle_value (), "clipping", "off");
  xset (title.handle_value (), "clipping", "off");

  xset (xlabel.handle_value (), "autopos_tag", "xlabel");
  xset (ylabel.handle_value (), "autopos_tag", "ylabel");
  xset (zlabel.handle_value (), "autopos_tag", "zlabel");
  xset (title.handle_value (), "autopos_tag", "title");

  update_transform ();
  sync_positions ();
  override_defaults (obj);
}

void
axes::properties::delete_text_child (handle_property& hp)
{
  graphics_handle h = hp.handle_value ();

  if (h.ok ())
    {
      graphics_object go = gh_manager::get_object (h);

      if (go.valid_object ())
        gh_manager::free (h);

      base_properties::remove_child (h);
    }

  // FIXME: is it necessary to check whether the axes object is
  // being deleted now?  I think this function is only called when an
  // individual child object is delete and not when the parent axes
  // object is deleted.

  if (! is_beingdeleted ())
    {
      hp = gh_manager::make_graphics_handle ("text", __myhandle__,
                                             false, false);

      xset (hp.handle_value (), "handlevisibility", "off");

      adopt (hp.handle_value ());
    }
}

void
axes::properties::remove_child (const graphics_handle& h)
{
  if (xlabel.handle_value ().ok () && h == xlabel.handle_value ())
    {
      delete_text_child (xlabel);
      update_xlabel_position ();
    }
  else if (ylabel.handle_value ().ok () && h == ylabel.handle_value ())
    {
      delete_text_child (ylabel);
      update_ylabel_position ();
    }
  else if (zlabel.handle_value ().ok () && h == zlabel.handle_value ())
    {
      delete_text_child (zlabel);
      update_zlabel_position ();
    }
  else if (title.handle_value ().ok () && h == title.handle_value ())
    {
      delete_text_child (title);
      update_title_position ();
    }
  else
    base_properties::remove_child (h);
}

inline Matrix
xform_matrix (void)
{
  Matrix m (4, 4, 0.0);
  for (int i = 0; i < 4; i++)
    m(i,i) = 1;
  return m;
}

inline ColumnVector
xform_vector (void)
{
  ColumnVector v (4, 0.0);
  v(3) = 1;
  return v;
}

inline ColumnVector
xform_vector (double x, double y, double z)
{
  ColumnVector v (4, 1.0);
  v(0) = x; v(1) = y; v(2) = z;
  return v;
}

inline ColumnVector
transform (const Matrix& m, double x, double y, double z)
{
  return (m * xform_vector (x, y, z));
}

inline Matrix
xform_scale (double x, double y, double z)
{
  Matrix m (4, 4, 0.0);
  m(0,0) = x; m(1,1) = y; m(2,2) = z; m(3,3) = 1;
  return m;
}

inline Matrix
xform_translate (double x, double y, double z)
{
  Matrix m = xform_matrix ();
  m(0,3) = x; m(1,3) = y; m(2,3) = z; m(3,3) = 1;
  return m;
}

inline void
scale (Matrix& m, double x, double y, double z)
{
  m = m * xform_scale (x, y, z);
}

inline void
translate (Matrix& m, double x, double y, double z)
{
  m = m * xform_translate (x, y, z);
}

inline void
xform (ColumnVector& v, const Matrix& m)
{
  v = m*v;
}

inline void
scale (ColumnVector& v, double x, double y, double z)
{
  v(0) *= x;
  v(1) *= y;
  v(2) *= z;
}

inline void
translate (ColumnVector& v, double x, double y, double z)
{
  v(0) += x;
  v(1) += y;
  v(2) += z;
}

inline void
normalize (ColumnVector& v)
{
  double fact = 1.0 / sqrt (v(0)*v(0)+v(1)*v(1)+v(2)*v(2));
  scale (v, fact, fact, fact);
}

inline double
dot (const ColumnVector& v1, const ColumnVector& v2)
{
  return (v1(0)*v2(0)+v1(1)*v2(1)+v1(2)*v2(2));
}

inline double
norm (const ColumnVector& v)
{
  return sqrt (dot (v, v));
}

inline ColumnVector
cross (const ColumnVector& v1, const ColumnVector& v2)
{
  ColumnVector r = xform_vector ();
  r(0) = v1(1)*v2(2)-v1(2)*v2(1);
  r(1) = v1(2)*v2(0)-v1(0)*v2(2);
  r(2) = v1(0)*v2(1)-v1(1)*v2(0);
  return r;
}

inline Matrix
unit_cube (void)
{
  static double data[32] =
  {
    0,0,0,1,
    1,0,0,1,
    0,1,0,1,
    0,0,1,1,
    1,1,0,1,
    1,0,1,1,
    0,1,1,1,
    1,1,1,1
  };
  Matrix m (4, 8);
  memcpy (m.fortran_vec (), data, sizeof (double)*32);
  return m;
}

inline ColumnVector
cam2xform (const Array<double>& m)
{
  ColumnVector retval (4, 1.0);
  memcpy (retval.fortran_vec (), m.fortran_vec (), sizeof (double)*3);
  return retval;
}

inline RowVector
xform2cam (const ColumnVector& v)
{
  return v.extract_n (0, 3).transpose ();
}

void
axes::properties::update_camera (void)
{
  double xd = (xdir_is ("normal") ? 1 : -1);
  double yd = (ydir_is ("normal") ? 1 : -1);
  double zd = (zdir_is ("normal") ? 1 : -1);

  Matrix xlimits = sx.scale (get_xlim ().matrix_value ());
  Matrix ylimits = sy.scale (get_ylim ().matrix_value ());
  Matrix zlimits = sz.scale (get_zlim ().matrix_value ());

  double xo = xlimits(xd > 0 ? 0 : 1);
  double yo = ylimits(yd > 0 ? 0 : 1);
  double zo = zlimits(zd > 0 ? 0 : 1);

  Matrix pb = get_plotboxaspectratio ().matrix_value ();

  bool autocam = (camerapositionmode_is ("auto")
                  && cameratargetmode_is ("auto")
                  && cameraupvectormode_is ("auto")
                  && cameraviewanglemode_is ("auto"));
  bool dowarp = (autocam && dataaspectratiomode_is ("auto")
                 && plotboxaspectratiomode_is ("auto"));

  ColumnVector c_eye (xform_vector ());
  ColumnVector c_center (xform_vector ());
  ColumnVector c_upv (xform_vector ());

  if (cameratargetmode_is ("auto"))
    {
      c_center(0) = (xlimits(0)+xlimits(1))/2;
      c_center(1) = (ylimits(0)+ylimits(1))/2;
      c_center(2) = (zlimits(0)+zlimits(1))/2;

      cameratarget = xform2cam (c_center);
    }
  else
    c_center = cam2xform (get_cameratarget ().matrix_value ());

  if (camerapositionmode_is ("auto"))
    {
      Matrix tview = get_view ().matrix_value ();
      double az = tview(0);
      double el = tview(1);
      double d = 5 * sqrt (pb(0)*pb(0)+pb(1)*pb(1)+pb(2)*pb(2));

      if (el == 90 || el == -90)
        c_eye(2) = d*signum (el);
      else
        {
          az *= M_PI/180.0;
          el *= M_PI/180.0;
          c_eye(0) = d * cos (el) * sin (az);
          c_eye(1) = -d* cos (el) * cos (az);
          c_eye(2) = d * sin (el);
        }
      c_eye(0) = c_eye(0)*(xlimits(1)-xlimits(0))/(xd*pb(0))+c_center(0);
      c_eye(1) = c_eye(1)*(ylimits(1)-ylimits(0))/(yd*pb(1))+c_center(1);
      c_eye(2) = c_eye(2)*(zlimits(1)-zlimits(0))/(zd*pb(2))+c_center(2);

      cameraposition = xform2cam (c_eye);
    }
  else
    c_eye = cam2xform (get_cameraposition ().matrix_value ());

  if (cameraupvectormode_is ("auto"))
    {
      Matrix tview = get_view ().matrix_value ();
      double az = tview(0);
      double el = tview(1);

      if (el == 90 || el == -90)
        {
          c_upv(0) =
            -signum (el) *sin (az*M_PI/180.0)*(xlimits(1)-xlimits(0))/pb(0);
          c_upv(1) =
            signum (el) * cos (az*M_PI/180.0)*(ylimits(1)-ylimits(0))/pb(1);
        }
      else
        c_upv(2) = 1;

      cameraupvector = xform2cam (c_upv);
    }
  else
    c_upv = cam2xform (get_cameraupvector ().matrix_value ());

  Matrix x_view = xform_matrix ();
  Matrix x_projection = xform_matrix ();
  Matrix x_viewport = xform_matrix ();
  Matrix x_normrender = xform_matrix ();
  Matrix x_pre = xform_matrix ();

  x_render = xform_matrix ();
  x_render_inv = xform_matrix ();

  scale (x_pre, pb(0), pb(1), pb(2));
  translate (x_pre, -0.5, -0.5, -0.5);
  scale (x_pre, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
         zd/(zlimits(1)-zlimits(0)));
  translate (x_pre, -xo, -yo, -zo);

  xform (c_eye, x_pre);
  xform (c_center, x_pre);
  scale (c_upv, pb(0)/(xlimits(1)-xlimits(0)), pb(1)/(ylimits(1)-ylimits(0)),
         pb(2)/(zlimits(1)-zlimits(0)));
  translate (c_center, -c_eye(0), -c_eye(1), -c_eye(2));

  ColumnVector F (c_center), f (F), UP (c_upv);
  normalize (f);
  normalize (UP);

  if (std::abs (dot (f, UP)) > 1e-15)
    {
      double fa = 1 / sqrt(1-f(2)*f(2));
      scale (UP, fa, fa, fa);
    }

  ColumnVector s = cross (f, UP);
  ColumnVector u = cross (s, f);

  scale (x_view, 1, 1, -1);
  Matrix l = xform_matrix ();
  l(0,0) = s(0); l(0,1) = s(1); l(0,2) = s(2);
  l(1,0) = u(0); l(1,1) = u(1); l(1,2) = u(2);
  l(2,0) = -f(0); l(2,1) = -f(1); l(2,2) = -f(2);
  x_view = x_view * l;
  translate (x_view, -c_eye(0), -c_eye(1), -c_eye(2));
  scale (x_view, pb(0), pb(1), pb(2));
  translate (x_view, -0.5, -0.5, -0.5);

  Matrix x_cube = x_view * unit_cube ();
  ColumnVector cmin = x_cube.row_min ();
  ColumnVector cmax = x_cube.row_max ();
  double xM = cmax(0)-cmin(0);
  double yM = cmax(1)-cmin(1);

  Matrix bb = get_boundingbox (true);

  double v_angle;

  if (cameraviewanglemode_is ("auto"))
    {
      double af;

      // FIXME: was this really needed?  When compared to Matlab, it
      // does not seem to be required. Need investigation with concrete
      // graphics toolkit to see results visually.
      if (false && dowarp)
        af = 1.0 / (xM > yM ? xM : yM);
      else
        {
          if ((bb(2)/bb(3)) > (xM/yM))
            af = 1.0 / yM;
          else
            af = 1.0 / xM;
        }
      v_angle = 2 * (180.0 / M_PI) * atan (1 / (2 * af * norm (F)));

      cameraviewangle = v_angle;
    }
  else
    v_angle = get_cameraviewangle ();

  double pf = 1 / (2 * tan ((v_angle / 2) * M_PI / 180.0) * norm (F));
  scale (x_projection, pf, pf, 1);

  if (dowarp)
    {
      xM *= pf;
      yM *= pf;
      translate (x_viewport, bb(0)+bb(2)/2, bb(1)+bb(3)/2, 0);
      scale (x_viewport, bb(2)/xM, -bb(3)/yM, 1);
    }
  else
    {
      double pix = 1;
      if (autocam)
        {
          if ((bb(2)/bb(3)) > (xM/yM))
            pix = bb(3);
          else
            pix = bb(2);
        }
      else
        pix = (bb(2) < bb(3) ? bb(2) : bb(3));
      translate (x_viewport, bb(0)+bb(2)/2, bb(1)+bb(3)/2, 0);
      scale (x_viewport, pix, -pix, 1);
    }

  x_normrender = x_viewport * x_projection * x_view;

  x_cube = x_normrender * unit_cube ();
  cmin = x_cube.row_min ();
  cmax = x_cube.row_max ();
  x_zlim.resize (1, 2);
  x_zlim(0) = cmin(2);
  x_zlim(1) = cmax(2);

  x_render = x_normrender;
  scale (x_render, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
         zd/(zlimits(1)-zlimits(0)));
  translate (x_render, -xo, -yo, -zo);

  x_viewtransform = x_view;
  x_projectiontransform = x_projection;
  x_viewporttransform = x_viewport;
  x_normrendertransform = x_normrender;
  x_rendertransform = x_render;

  x_render_inv = x_render.inverse ();

  // Note: these matrices are a slight modified version of the regular
  // matrices, more suited for OpenGL rendering (x_gl_mat1 => light
  // => x_gl_mat2)
  x_gl_mat1 = x_view;
  scale (x_gl_mat1, xd/(xlimits(1)-xlimits(0)), yd/(ylimits(1)-ylimits(0)),
         zd/(zlimits(1)-zlimits(0)));
  translate (x_gl_mat1, -xo, -yo, -zo);
  x_gl_mat2 = x_viewport * x_projection;
}

static bool updating_axes_layout = false;

void
axes::properties::update_axes_layout (void)
{
  if (updating_axes_layout)
    return;

  graphics_xform xform = get_transform ();

  double xd = (xdir_is ("normal") ? 1 : -1);
  double yd = (ydir_is ("normal") ? 1 : -1);
  double zd = (zdir_is ("normal") ? 1 : -1);

  const Matrix xlims = xform.xscale (get_xlim ().matrix_value ());
  const Matrix ylims = xform.yscale (get_ylim ().matrix_value ());
  const Matrix zlims = xform.zscale (get_zlim ().matrix_value ());

  double x_min, x_max, y_min, y_max, z_min, z_max;
  x_min = xlims(0), x_max = xlims(1);
  y_min = ylims(0), y_max = ylims(1);
  z_min = zlims(0), z_max = zlims(1);

  ColumnVector p1, p2, dir (3);

  xstate = ystate = zstate = AXE_ANY_DIR;

  p1 = xform.transform (x_min, (y_min+y_max)/2, (z_min+z_max)/2, false);
  p2 = xform.transform (x_max, (y_min+y_max)/2, (z_min+z_max)/2, false);
  dir(0) = xround (p2(0)-p1(0));
  dir(1) = xround (p2(1)-p1(1));
  dir(2) = (p2(2)-p1(2));
  if (dir(0) == 0 && dir(1) == 0)
    xstate = AXE_DEPTH_DIR;
  else if (dir(2) == 0)
    {
      if (dir(0) == 0)
        xstate = AXE_VERT_DIR;
      else if (dir(1) == 0)
        xstate = AXE_HORZ_DIR;
    }

  if (dir(2) == 0)
    {
      if (dir(1) == 0)
        xPlane = (dir(0) > 0 ? x_max : x_min);
      else
        xPlane = (dir(1) < 0 ? x_max : x_min);
    }
  else
    xPlane = (dir(2) < 0 ? x_min : x_max);

  xPlaneN = (xPlane == x_min ? x_max : x_min);
  fx = (x_max-x_min) / sqrt (dir(0)*dir(0)+dir(1)*dir(1));

  p1 = xform.transform ((x_min+x_max)/2, y_min, (z_min+z_max)/2, false);
  p2 = xform.transform ((x_min+x_max)/2, y_max, (z_min+z_max)/2, false);
  dir(0) = xround (p2(0)-p1(0));
  dir(1) = xround (p2(1)-p1(1));
  dir(2) = (p2(2)-p1(2));
  if (dir(0) == 0 && dir(1) == 0)
    ystate = AXE_DEPTH_DIR;
  else if (dir(2) == 0)
    {
      if (dir(0) == 0)
        ystate = AXE_VERT_DIR;
      else if (dir(1) == 0)
        ystate = AXE_HORZ_DIR;
    }

  if (dir(2) == 0)
    {
      if (dir(1) == 0)
        yPlane = (dir(0) > 0 ? y_max : y_min);
      else
        yPlane = (dir(1) < 0 ? y_max : y_min);
    }
  else
    yPlane = (dir(2) < 0 ? y_min : y_max);

  yPlaneN = (yPlane == y_min ? y_max : y_min);
  fy = (y_max-y_min) / sqrt (dir(0)*dir(0)+dir(1)*dir(1));

  p1 = xform.transform ((x_min+x_max)/2, (y_min+y_max)/2, z_min, false);
  p2 = xform.transform ((x_min+x_max)/2, (y_min+y_max)/2, z_max, false);
  dir(0) = xround (p2(0)-p1(0));
  dir(1) = xround (p2(1)-p1(1));
  dir(2) = (p2(2)-p1(2));
  if (dir(0) == 0 && dir(1) == 0)
    zstate = AXE_DEPTH_DIR;
  else if (dir(2) == 0)
    {
      if (dir(0) == 0)
        zstate = AXE_VERT_DIR;
      else if (dir(1) == 0)
        zstate = AXE_HORZ_DIR;
    }

  if (dir(2) == 0)
    {
      if (dir(1) == 0)
        zPlane = (dir(0) > 0 ? z_min : z_max);
      else
        zPlane = (dir(1) < 0 ? z_min : z_max);
    }
  else
    zPlane = (dir(2) < 0 ? z_min : z_max);

  zPlaneN = (zPlane == z_min ? z_max : z_min);
  fz = (z_max-z_min) / sqrt (dir(0)*dir(0)+dir(1)*dir(1));

  unwind_protect frame;
  frame.protect_var (updating_axes_layout);
  updating_axes_layout = true;

  xySym = (xd*yd*(xPlane-xPlaneN)*(yPlane-yPlaneN) > 0);
  zSign = (zd*(zPlane-zPlaneN) <= 0);
  xyzSym = zSign ? xySym : !xySym;
  xpTick = (zSign ? xPlaneN : xPlane);
  ypTick = (zSign ? yPlaneN : yPlane);
  zpTick = (zSign ? zPlane : zPlaneN);
  xpTickN = (zSign ? xPlane : xPlaneN);
  ypTickN = (zSign ? yPlane : yPlaneN);
  zpTickN = (zSign ? zPlaneN : zPlane);

  /* 2D mode */
  x2Dtop = false;
  y2Dright = false;
  layer2Dtop = false;
  if (xstate == AXE_HORZ_DIR && ystate == AXE_VERT_DIR)
    {
      if (xaxislocation_is ("top"))
        {
          std::swap (yPlane, yPlaneN);
          x2Dtop = true;
        }
      ypTick = yPlaneN;
      ypTickN = yPlane;
      if (yaxislocation_is ("right"))
        {
          std::swap (xPlane, xPlaneN);
          y2Dright = true;
        }
      xpTick = xPlaneN;
      xpTickN = xPlane;
      if (layer_is ("top"))
        {
          zpTick = zPlaneN;
          layer2Dtop = true;
        }
      else
        zpTick = zPlane;
    }

  Matrix viewmat = get_view ().matrix_value ();
  nearhoriz = std::abs (viewmat(1)) <= 5;
  is2D = viewmat(1) == 90;

  update_ticklength ();
}

void
axes::properties::update_ticklength (void)
{
  bool mode2d = (((xstate > AXE_DEPTH_DIR ? 1 : 0) +
                  (ystate > AXE_DEPTH_DIR ? 1 : 0) +
                  (zstate > AXE_DEPTH_DIR ? 1 : 0)) == 2);

  if (tickdirmode_is ("auto"))
    tickdir.set (mode2d ? "in" : "out", true);

  double ticksign = (tickdir_is ("in") ? -1 : 1);

  Matrix bbox = get_boundingbox (true);
  Matrix ticklen = get_ticklength ().matrix_value ();
  ticklen(0) = ticklen(0) * std::max (bbox(2), bbox(3));
  ticklen(1) = ticklen(1) * std::max (bbox(2), bbox(3));

  xticklen = ticksign * (mode2d ? ticklen(0) : ticklen(1));
  yticklen = ticksign * (mode2d ? ticklen(0) : ticklen(1));
  zticklen = ticksign * (mode2d ? ticklen(0) : ticklen(1));

  xtickoffset = (mode2d ? std::max (0., xticklen) : std::abs (xticklen)) + 5;
  ytickoffset = (mode2d ? std::max (0., yticklen) : std::abs (yticklen)) + 5;
  ztickoffset = (mode2d ? std::max (0., zticklen) : std::abs (zticklen)) + 5;

  update_xlabel_position ();
  update_ylabel_position ();
  update_zlabel_position ();
  update_title_position ();
}

/*
## FIXME: A demo can't be called in a C++ file.  This should be made a test
## or moved to a .m file where it can be called.
%!demo
%! clf;
%! subplot (2,1,1);
%!  plot (rand (3));
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title title;
%! subplot (2,1,2);
%!  plot (rand (3));
%!  set (gca, "ticklength", get (gca, "ticklength") * 2, "tickdir", "out");
%!  xlabel xlabel;
%!  ylabel ylabel;
%!  title title;
*/

static ColumnVector
convert_label_position (const ColumnVector& p,
                        const text::properties& props,
                        const graphics_xform& xform,
                        const Matrix& bbox)
{
  ColumnVector retval;

  caseless_str to_units = props.get_units ();

  if (! to_units.compare ("data"))
    {
      ColumnVector v = xform.transform (p(0), p(1), p(2));

      retval.resize (3);

      retval(0) = v(0) - bbox(0) + 1;
      retval(1) = bbox(1) + bbox(3) - v(1) + 1;
      retval(2) = 0;

      retval = convert_position (retval, "pixels", to_units,
                                 bbox.extract_n (0, 2, 1, 2));
    }
  else
    retval = p;

  return retval;
}

static bool updating_xlabel_position = false;

void
axes::properties::update_xlabel_position (void)
{
  if (updating_xlabel_position)
    return;

  graphics_object obj = gh_manager::get_object (get_xlabel ());

  if (! obj.valid_object ())
    return;

  text::properties& xlabel_props
    = reinterpret_cast<text::properties&> (obj.get_properties ());

  bool is_empty = xlabel_props.get_string ().is_empty ();

  unwind_protect frame;
  frame.protect_var (updating_xlabel_position);
  updating_xlabel_position = true;

  if (! is_empty)
    {
      if (xlabel_props.horizontalalignmentmode_is ("auto"))
        {
          xlabel_props.set_horizontalalignment
            (xstate > AXE_DEPTH_DIR
             ? "center" : (xyzSym ? "left" : "right"));

          xlabel_props.set_horizontalalignmentmode ("auto");
        }

      if (xlabel_props.verticalalignmentmode_is ("auto"))
        {
          xlabel_props.set_verticalalignment
            (xstate == AXE_VERT_DIR || x2Dtop ? "bottom" : "top");

          xlabel_props.set_verticalalignmentmode ("auto");
        }
    }

  if (xlabel_props.positionmode_is ("auto")
      || xlabel_props.rotationmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      Matrix ext (1, 2, 0.0);
      ext = get_ticklabel_extents (get_xtick ().matrix_value (),
                                   get_xticklabel ().all_strings (),
                                   get_xlim ().matrix_value ());

      double wmax = ext(0);
      double hmax = ext(1);
      double angle = 0;
      ColumnVector p =
        graphics_xform::xform_vector ((xpTickN+xpTick)/2, ypTick, zpTick);

      bool tick_along_z = nearhoriz || xisinf (fy);
      if (tick_along_z)
        p(2) += (signum (zpTick-zpTickN)*fz*xtickoffset);
      else
        p(1) += (signum (ypTick-ypTickN)*fy*xtickoffset);

      p = xform.transform (p(0), p(1), p(2), false);

      switch (xstate)
        {
        case AXE_ANY_DIR:
          p(0) += (xyzSym ? wmax : -wmax);
          p(1) += hmax;
          break;

        case AXE_VERT_DIR:
          p(0) -= wmax;
          angle = 90;
          break;

        case AXE_HORZ_DIR:
          p(1) += (x2Dtop ? -hmax : hmax);
          break;
        }

      if (xlabel_props.positionmode_is ("auto"))
        {
          p = xform.untransform (p(0), p(1), p(2), true);

          p = convert_label_position (p, xlabel_props, xform,
                                      get_extent (false));

          xlabel_props.set_position (p.extract_n (0, 3).transpose ());
          xlabel_props.set_positionmode ("auto");
        }

      if (! is_empty && xlabel_props.rotationmode_is ("auto"))
        {
          xlabel_props.set_rotation (angle);
          xlabel_props.set_rotationmode ("auto");
        }
    }
}

static bool updating_ylabel_position = false;

void
axes::properties::update_ylabel_position (void)
{
  if (updating_ylabel_position)
    return;

  graphics_object obj = gh_manager::get_object (get_ylabel ());

  if (! obj.valid_object ())
    return;

  text::properties& ylabel_props
    = reinterpret_cast<text::properties&> (obj.get_properties ());

  bool is_empty = ylabel_props.get_string ().is_empty ();

  unwind_protect frame;
  frame.protect_var (updating_ylabel_position);
  updating_ylabel_position = true;

  if (! is_empty)
    {
      if (ylabel_props.horizontalalignmentmode_is ("auto"))
        {
          ylabel_props.set_horizontalalignment
            (ystate > AXE_DEPTH_DIR
             ? "center" : (!xyzSym ? "left" : "right"));

          ylabel_props.set_horizontalalignmentmode ("auto");
        }

      if (ylabel_props.verticalalignmentmode_is ("auto"))
        {
          ylabel_props.set_verticalalignment
            (ystate == AXE_VERT_DIR && !y2Dright ? "bottom" : "top");

          ylabel_props.set_verticalalignmentmode ("auto");
        }
    }

  if (ylabel_props.positionmode_is ("auto")
      || ylabel_props.rotationmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      Matrix ext (1, 2, 0.0);

      // The underlying get_extents() from FreeType produces mismatched values.
      // x-extent accurately measures the width of the glyphs.
      // y-extent instead measures from baseline-to-baseline.
      // Pad x-extent (+4) so that it approximately matches y-extent.
      // This keeps ylabels about the same distance from y-axis as
      // xlabels are from x-axis.
      // ALWAYS use an even number for padding or horizontal alignment
      // will be off.
      ext = get_ticklabel_extents (get_ytick ().matrix_value (),
                                   get_yticklabel ().all_strings (),
                                   get_ylim ().matrix_value ());

      double wmax = ext(0)+4;
      double hmax = ext(1);
      double angle = 0;
      ColumnVector p =
        graphics_xform::xform_vector (xpTick, (ypTickN+ypTick)/2, zpTick);

      bool tick_along_z = nearhoriz || xisinf (fx);
      if (tick_along_z)
        p(2) += (signum (zpTick-zpTickN)*fz*ytickoffset);
      else
        p(0) += (signum (xpTick-xpTickN)*fx*ytickoffset);

      p = xform.transform (p(0), p(1), p(2), false);

      switch (ystate)
        {
        case AXE_ANY_DIR:
          p(0) += (!xyzSym ? wmax : -wmax);
          p(1) += hmax;
          break;

        case AXE_VERT_DIR:
          p(0) += (y2Dright ? wmax : -wmax);
          angle = 90;
          break;

        case AXE_HORZ_DIR:
          p(1) += hmax;
          break;
        }

      if (ylabel_props.positionmode_is ("auto"))
        {
          p = xform.untransform (p(0), p(1), p(2), true);

          p = convert_label_position (p, ylabel_props, xform,
                                      get_extent (false));

          ylabel_props.set_position (p.extract_n (0, 3).transpose ());
          ylabel_props.set_positionmode ("auto");
        }

      if (! is_empty && ylabel_props.rotationmode_is ("auto"))
        {
          ylabel_props.set_rotation (angle);
          ylabel_props.set_rotationmode ("auto");
        }
    }
}

static bool updating_zlabel_position = false;

void
axes::properties::update_zlabel_position (void)
{
  if (updating_zlabel_position)
    return;

  graphics_object obj = gh_manager::get_object (get_zlabel ());

  if (! obj.valid_object ())
    return;

  text::properties& zlabel_props
    = reinterpret_cast<text::properties&> (obj.get_properties ());

  bool camAuto = cameraupvectormode_is ("auto");
  bool is_empty = zlabel_props.get_string ().is_empty ();

  unwind_protect frame;
  frame.protect_var (updating_zlabel_position);
  updating_zlabel_position = true;

  if (! is_empty)
    {
      if (zlabel_props.horizontalalignmentmode_is ("auto"))
        {
          zlabel_props.set_horizontalalignment
            ((zstate > AXE_DEPTH_DIR || camAuto) ? "center" : "right");

          zlabel_props.set_horizontalalignmentmode ("auto");
        }

      if (zlabel_props.verticalalignmentmode_is ("auto"))
        {
          zlabel_props.set_verticalalignment
            (zstate == AXE_VERT_DIR
             ? "bottom" : ((zSign || camAuto) ? "bottom" : "top"));

          zlabel_props.set_verticalalignmentmode ("auto");
        }
    }

  if (zlabel_props.positionmode_is ("auto")
      || zlabel_props.rotationmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      Matrix ext (1, 2, 0.0);
      ext = get_ticklabel_extents (get_ztick ().matrix_value (),
                                   get_zticklabel ().all_strings (),
                                   get_zlim ().matrix_value ());

      double wmax = ext(0);
      double hmax = ext(1);
      double angle = 0;
      ColumnVector p;

      if (xySym)
        {
          p = graphics_xform::xform_vector (xPlaneN, yPlane,
                                            (zpTickN+zpTick)/2);
          if (xisinf (fy))
            p(0) += (signum (xPlaneN-xPlane)*fx*ztickoffset);
          else
            p(1) += (signum (yPlane-yPlaneN)*fy*ztickoffset);
        }
      else
        {
          p = graphics_xform::xform_vector (xPlane, yPlaneN,
                                            (zpTickN+zpTick)/2);
          if (xisinf (fx))
            p(1) += (signum (yPlaneN-yPlane)*fy*ztickoffset);
          else
            p(0) += (signum (xPlane-xPlaneN)*fx*ztickoffset);
        }

      p = xform.transform (p(0), p(1), p(2), false);

      switch (zstate)
        {
        case AXE_ANY_DIR:
          if (camAuto)
            {
              p(0) -= wmax;
              angle = 90;
            }

          // FIXME: what's the correct offset?
          //
          //   p[0] += (!xySym ? wmax : -wmax);
          //   p[1] += (zSign ? hmax : -hmax);

          break;

        case AXE_VERT_DIR:
          p(0) -= wmax;
          angle = 90;
          break;

        case AXE_HORZ_DIR:
          p(1) += hmax;
          break;
        }

      if (zlabel_props.positionmode_is ("auto"))
        {
          p = xform.untransform (p(0), p(1), p(2), true);

          p = convert_label_position (p, zlabel_props, xform,
                                      get_extent (false));

          zlabel_props.set_position (p.extract_n (0, 3).transpose ());
          zlabel_props.set_positionmode ("auto");
        }

      if (! is_empty && zlabel_props.rotationmode_is ("auto"))
        {
          zlabel_props.set_rotation (angle);
          zlabel_props.set_rotationmode ("auto");
        }
    }
}

static bool updating_title_position = false;

void
axes::properties::update_title_position (void)
{
  if (updating_title_position)
    return;

  graphics_object obj = gh_manager::get_object (get_title ());

  if (! obj.valid_object ())
    return;

  text::properties& title_props
    = reinterpret_cast<text::properties&> (obj.get_properties ());

  unwind_protect frame;
  frame.protect_var (updating_title_position);
  updating_title_position = true;

  if (title_props.positionmode_is ("auto"))
    {
      graphics_xform xform = get_transform ();

      // FIXME: bbox should be stored in axes::properties
      Matrix bbox = get_extent (false);

      ColumnVector p =
        graphics_xform::xform_vector (bbox(0)+bbox(2)/2,
                                      bbox(1)-10,
                                      (x_zlim(0)+x_zlim(1))/2);

      if (x2Dtop)
        {
          Matrix ext (1, 2, 0.0);
          ext = get_ticklabel_extents (get_xtick ().matrix_value (),
                                       get_xticklabel ().all_strings (),
                                       get_xlim ().matrix_value ());
          p(1) -= ext(1);
        }

      p = xform.untransform (p(0), p(1), p(2), true);

      p = convert_label_position (p, title_props, xform, bbox);

      title_props.set_position (p.extract_n (0, 3).transpose ());
      title_props.set_positionmode ("auto");
    }
}

void
axes::properties::update_autopos (const std::string& elem_type)
{
  if (elem_type == "xlabel")
    update_xlabel_position ();
  else if (elem_type == "ylabel")
    update_ylabel_position ();
  else if (elem_type == "zlabel")
    update_zlabel_position ();
  else if (elem_type == "title")
    update_title_position ();
  else if (elem_type == "sync")
    sync_positions ();
}

static void
normalized_aspectratios (Matrix& aspectratios, const Matrix& scalefactors,
                         double xlength, double ylength, double zlength)
{
  double xval = xlength/scalefactors(0);
  double yval = ylength/scalefactors(1);
  double zval = zlength/scalefactors(2);

  double minval = xmin (xmin (xval, yval), zval);

  aspectratios(0) = xval/minval;
  aspectratios(1) = yval/minval;
  aspectratios(2) = zval/minval;
}

static void
max_axes_scale (double& s, Matrix& limits, const Matrix& kids,
                double pbfactor, double dafactor, char limit_type, bool tight)
{
  if (tight)
    {
      double minval = octave_Inf;
      double maxval = -octave_Inf;
      double min_pos = octave_Inf;
      double max_neg = -octave_Inf;
      get_children_limits (minval, maxval, min_pos, max_neg, kids, limit_type);
      if (xfinite (minval) && xfinite (maxval))
        {
          limits(0) = minval;
          limits(1) = maxval;
          s = xmax(s, (maxval - minval) / (pbfactor * dafactor));
        }
    }
  else
    s = xmax(s, (limits(1) - limits(0)) / (pbfactor * dafactor));
}

static std::set<double> updating_aspectratios;

void
axes::properties::update_aspectratios (void)
{
  if (updating_aspectratios.find (get___myhandle__ ().value ())
      != updating_aspectratios.end ())
    return;

  Matrix xlimits = get_xlim ().matrix_value ();
  Matrix ylimits = get_ylim ().matrix_value ();
  Matrix zlimits = get_zlim ().matrix_value ();

  double dx = (xlimits(1)-xlimits(0));
  double dy = (ylimits(1)-ylimits(0));
  double dz = (zlimits(1)-zlimits(0));

  Matrix da = get_dataaspectratio ().matrix_value ();
  Matrix pba = get_plotboxaspectratio ().matrix_value ();

  if (dataaspectratiomode_is ("auto"))
    {
      if (plotboxaspectratiomode_is ("auto"))
        {
          pba = Matrix (1, 3, 1.0);
          plotboxaspectratio.set (pba, false);
        }

      normalized_aspectratios (da, pba, dx, dy, dz);
      dataaspectratio.set (da, false);
    }
  else if (plotboxaspectratiomode_is ("auto"))
    {
      normalized_aspectratios (pba, da, dx, dy, dz);
      plotboxaspectratio.set (pba, false);
    }
  else
    {
      double s = -octave_Inf;
      bool modified_limits = false;
      Matrix kids;

      if (xlimmode_is ("auto") && ylimmode_is ("auto") && zlimmode_is ("auto"))
        {
          modified_limits = true;
          kids = get_children ();
          max_axes_scale (s, xlimits, kids, pba(0), da(0), 'x', true);
          max_axes_scale (s, ylimits, kids, pba(1), da(1), 'y', true);
          max_axes_scale (s, zlimits, kids, pba(2), da(2), 'z', true);
        }
      else if (xlimmode_is ("auto") && ylimmode_is ("auto"))
        {
          modified_limits = true;
          max_axes_scale (s, zlimits, kids, pba(2), da(2), 'z', false);
        }
      else if (ylimmode_is ("auto") && zlimmode_is ("auto"))
        {
          modified_limits = true;
          max_axes_scale (s, xlimits, kids, pba(0), da(0), 'x', false);
        }
      else if (zlimmode_is ("auto") && xlimmode_is ("auto"))
        {
          modified_limits = true;
          max_axes_scale (s, ylimits, kids, pba(1), da(1), 'y', false);
        }

      if (modified_limits)
        {

          unwind_protect frame;
          frame.protect_var (updating_aspectratios);

          updating_aspectratios.insert (get___myhandle__ ().value ());

          dx = pba(0) *da(0);
          dy = pba(1) *da(1);
          dz = pba(2) *da(2);
          if (xisinf (s))
            s = 1 / xmin (xmin (dx, dy), dz);

          if (xlimmode_is ("auto"))
            {
              dx = s * dx;
              xlimits(0) = 0.5 * (xlimits(0) + xlimits(1) - dx);
              xlimits(1) = xlimits(0) + dx;
              set_xlim (xlimits);
              set_xlimmode ("auto");
            }

          if (ylimmode_is ("auto"))
            {
              dy = s * dy;
              ylimits(0) = 0.5 * (ylimits(0) + ylimits(1) - dy);
              ylimits(1) = ylimits(0) + dy;
              set_ylim (ylimits);
              set_ylimmode ("auto");
            }

          if (zlimmode_is ("auto"))
            {
              dz = s * dz;
              zlimits(0) = 0.5 * (zlimits(0) + zlimits(1) - dz);
              zlimits(1) = zlimits(0) + dz;
              set_zlim (zlimits);
              set_zlimmode ("auto");
            }
        }
      else
        {
          normalized_aspectratios (pba, da, dx, dy, dz);
          plotboxaspectratio.set (pba, false);
        }
    }
}

void
axes::properties::update_font (void)
{
#ifdef HAVE_FREETYPE
#ifdef HAVE_FONTCONFIG
  text_renderer.set_font (get ("fontname").string_value (),
                          get ("fontweight").string_value (),
                          get ("fontangle").string_value (),
                          get ("fontsize_points").double_value ());
#endif
#endif
}

// The INTERNAL flag defines whether position or outerposition is used.

Matrix
axes::properties::get_boundingbox (bool internal,
                                   const Matrix& parent_pix_size) const
{
  Matrix pos = internal ? get_position ().matrix_value ()
                        : get_outerposition ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.numel () == 0)
    {
      graphics_object obj = gh_manager::get_object (get_parent ());

      if (obj.valid_object ())
        parent_size =
          obj.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
      else
        parent_size = default_figure_position ();
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  return pos;
}

Matrix
axes::properties::get_extent (bool with_text, bool only_text_height) const
{
  graphics_xform xform = get_transform ();

  Matrix ext (1, 4, 0.0);
  ext(0) = octave_Inf;
  ext(1) = octave_Inf;
  ext(2) = -octave_Inf;
  ext(3) = -octave_Inf;
  for (int i = 0; i <= 1; i++)
    for (int j = 0; j <= 1; j++)
      for (int k = 0; k <= 1; k++)
        {
          ColumnVector p = xform.transform (i ? xPlaneN : xPlane,
                                            j ? yPlaneN : yPlane,
                                            k ? zPlaneN : zPlane, false);
          ext(0) = std::min (ext(0), p(0));
          ext(1) = std::min (ext(1), p(1));
          ext(2) = std::max (ext(2), p(0));
          ext(3) = std::max (ext(3), p(1));
        }

  if (with_text)
    {
      for (int i = 0; i < 4; i++)
        {
          graphics_handle text_handle;
          if (i == 0)
            text_handle = get_title ();
          else if (i == 1)
            text_handle = get_xlabel ();
          else if (i == 2)
            text_handle = get_ylabel ();
          else if (i == 3)
            text_handle = get_zlabel ();

          text::properties& text_props
            = reinterpret_cast<text::properties&>
                (gh_manager::get_object (text_handle).get_properties ());

          Matrix text_pos = text_props.get_data_position ();
          text_pos = xform.transform (text_pos(0), text_pos(1), text_pos(2));
          if (text_props.get_string ().is_empty ())
            {
              ext(0) = std::min (ext(0), text_pos(0));
              ext(1) = std::min (ext(1), text_pos(1));
              ext(2) = std::max (ext(2), text_pos(0));
              ext(3) = std::max (ext(3), text_pos(1));
            }
          else
            {
              Matrix text_ext = text_props.get_extent_matrix ();

              bool ignore_horizontal = false;
              bool ignore_vertical = false;
              if (only_text_height)
                {
                  double text_rotation = text_props.get_rotation ();
                  if (text_rotation == 0. || text_rotation == 180.)
                    ignore_horizontal = true;
                  else if (text_rotation == 90. || text_rotation == 270.)
                    ignore_vertical = true;
                }

              if (! ignore_horizontal)
                {
                  ext(0) = std::min (ext(0), text_pos(0)+text_ext(0));
                  ext(2) = std::max (ext(2),
                                     text_pos(0)+text_ext(0)+text_ext(2));
                }

              if (! ignore_vertical)
                {
                  ext(1) = std::min (ext(1),
                                     text_pos(1)-text_ext(1)-text_ext(3));
                  ext(3) = std::max (ext(3), text_pos(1)-text_ext(1));
                }
            }
        }
    }

  ext(2) = ext(2)-ext(0);
  ext(3) = ext(3)-ext(1);

  return ext;
}

static octave_value
convert_ticklabel_string (const octave_value& val)
{
  octave_value retval = val;

  if (val.is_cellstr ())
    {
      // Always return a column vector for Matlab Compatibility
      if (val.columns () > 1)
        retval = val.reshape (dim_vector (val.numel (), 1));
    }
  else
    {
      string_vector sv;
      if (val.is_numeric_type ())
        {
          NDArray data = val.array_value ();
          std::ostringstream oss;
          oss.precision (5);
          for (octave_idx_type i = 0; i < val.numel (); i++)
            {
              oss.str ("");
              oss << data(i);
              sv.append (oss.str ());
            }
        }
      else if (val.is_string () && val.rows () == 1)
        {
          std::string valstr = val.string_value ();
          std::istringstream iss (valstr);
          std::string tmpstr;

          // Split string with delimiter '|'
          while (std::getline (iss, tmpstr, '|'))
            sv.append (tmpstr);

          // If string ends with '|' Matlab appends a null string
          if (*valstr.rbegin () == '|')
            sv.append (std::string (""));
        }
      else
        return retval;

      charMatrix chmat (sv, ' ');

      retval = octave_value (chmat);
    }

  return retval;
}

void
axes::properties::set_xticklabel (const octave_value& v)
{
  if (!error_state)
    {
      if (xticklabel.set (convert_ticklabel_string (v), false))
        {
          set_xticklabelmode ("manual");
          xticklabel.run_listeners (POSTSET);
          mark_modified ();
        }
      else
        set_xticklabelmode ("manual");

      sync_positions ();
    }
}

void
axes::properties::set_yticklabel (const octave_value& v)
{
  if (!error_state)
    {
      if (yticklabel.set (convert_ticklabel_string (v), false))
        {
          set_yticklabelmode ("manual");
          yticklabel.run_listeners (POSTSET);
          mark_modified ();
        }
      else
        set_yticklabelmode ("manual");

      sync_positions ();
    }
}

void
axes::properties::set_zticklabel (const octave_value& v)
{
  if (!error_state)
    {
      if (zticklabel.set (convert_ticklabel_string (v), false))
        {
          set_zticklabelmode ("manual");
          zticklabel.run_listeners (POSTSET);
          mark_modified ();
        }
      else
        set_zticklabelmode ("manual");

      sync_positions ();
    }
}

// Almost identical to convert_ticklabel_string but it only accepts
// cellstr or string, not numeric input.
static octave_value
convert_linestyleorder_string (const octave_value& val)
{
  octave_value retval = val;

  if (val.is_cellstr ())
    {
      // Always return a column vector for Matlab Compatibility
      if (val.columns () > 1)
        retval = val.reshape (dim_vector (val.numel (), 1));
    }
  else
    {
      string_vector sv;
      if (val.is_string () && val.rows () == 1)
        {
          std::string valstr = val.string_value ();
          std::istringstream iss (valstr);
          std::string tmpstr;

          // Split string with delimiter '|'
          while (std::getline (iss, tmpstr, '|'))
            sv.append (tmpstr);

          // If string ends with '|' Matlab appends a null string
          if (*valstr.rbegin () == '|')
            sv.append (std::string (""));
        }
      else
        return retval;

      charMatrix chmat (sv, ' ');

      retval = octave_value (chmat);
    }

  return retval;
}

void
axes::properties::set_linestyleorder (const octave_value& v)
{
  if (!error_state)
    {
      linestyleorder.set (convert_linestyleorder_string (v), false);
    }
}

void
axes::properties::set_units (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_units = get_units ();
      if (units.set (v, true))
        {
          update_units (old_units);
          mark_modified ();
        }
    }
}

void
axes::properties::update_units (const caseless_str& old_units)
{
  graphics_object obj = gh_manager::get_object (get_parent ());
  Matrix parent_bb
    = obj.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
  caseless_str new_units = get_units ();
  position.set (octave_value (convert_position (get_position ().matrix_value (),
                                                old_units, new_units,
                                                parent_bb)),
                                                false);
  outerposition.set (octave_value (convert_position (get_outerposition ().matrix_value (),
                                                old_units, new_units,
                                                parent_bb)),
                                                false);
  tightinset.set (octave_value (convert_position (get_tightinset ().matrix_value (),
                                                old_units, new_units,
                                                parent_bb)),
                                                false);
  looseinset.set (octave_value (convert_position (get_looseinset ().matrix_value (),
                                                old_units, new_units,
                                                parent_bb)),
                                                false);
}

void
axes::properties::set_fontunits (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_fontunits = get_fontunits ();
      if (fontunits.set (v, true))
        {
          update_fontunits (old_fontunits);
          mark_modified ();
        }
    }
}

void
axes::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (true).elem (3);
  double fsz = get_fontsize ();

  fsz = convert_font_size (fsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fsz));
}

double
axes::properties::get_fontsize_points (double box_pix_height) const
{
  double fs = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (true).elem (3);

  return convert_font_size (fs, get_fontunits (), "points", parent_height);
}

ColumnVector
graphics_xform::xform_vector (double x, double y, double z)
{
  return ::xform_vector (x, y, z);
}

Matrix
graphics_xform::xform_eye (void)
{
  return ::xform_matrix ();
}

ColumnVector
graphics_xform::transform (double x, double y, double z,
                           bool use_scale) const
{
  if (use_scale)
    {
      x = sx.scale (x);
      y = sy.scale (y);
      z = sz.scale (z);
    }

  return ::transform (xform, x, y, z);
}

ColumnVector
graphics_xform::untransform (double x, double y, double z,
                             bool use_scale) const
{
  ColumnVector v = ::transform (xform_inv, x, y, z);

  if (use_scale)
    {
      v(0) = sx.unscale (v(0));
      v(1) = sy.unscale (v(1));
      v(2) = sz.unscale (v(2));
    }

  return v;
}

octave_value
axes::get_default (const caseless_str& name) const
{
  octave_value retval = default_properties.lookup (name);

  if (retval.is_undefined ())
    {
      graphics_handle parent = get_parent ();
      graphics_object parent_obj = gh_manager::get_object (parent);

      retval = parent_obj.get_default (name);
    }

  return retval;
}

// FIXME: remove.
// FIXME: maybe this should go into array_property class?
/*
static void
check_limit_vals (double& min_val, double& max_val,
                  double& min_pos, double& max_neg,
                  const array_property& data)
{
  double val = data.min_val ();
  if (xfinite (val) && val < min_val)
    min_val = val;
  val = data.max_val ();
  if (xfinite (val) && val > max_val)
    max_val = val;
  val = data.min_pos ();
  if (xfinite (val) && val > 0 && val < min_pos)
    min_pos = val;
  val = data.max_neg ();
  if (xfinite (val) && val < 0 && val > max_neg)
    max_neg = val;
}
*/

static void
check_limit_vals (double& min_val, double& max_val,
                  double& min_pos, double& max_neg,
                  const octave_value& data)
{
  if (data.is_matrix_type ())
    {
      Matrix m = data.matrix_value ();

      if (! error_state && m.numel () == 4)
        {
          double val;

          val = m(0);
          if (xfinite (val) && val < min_val)
            min_val = val;

          val = m(1);
          if (xfinite (val) && val > max_val)
            max_val = val;

          val = m(2);
          if (xfinite (val) && val > 0 && val < min_pos)
            min_pos = val;

          val = m(3);
          if (xfinite (val) && val < 0 && val > max_neg)
            max_neg = val;
        }
    }
}

// magform(x) Returns (a, b), where x = a * 10^b, abs (a) >= 1., and b is
// integer.

static void
magform (double x, double& a, int& b)
{
  if (x == 0)
    {
      a = 0;
      b = 0;
    }
  else
    {
      b = static_cast<int> (gnulib::floor (std::log10 (std::abs (x))));
      a = x / std::pow (10.0, b);
    }
}

// A translation from Tom Holoryd's python code at
// http://kurage.nimh.nih.gov/tomh/tics.py
// FIXME: add log ticks

double
axes::properties::calc_tick_sep (double lo, double hi)
{
  int ticint = 5;

  // Reference: Lewart, C. R., "Algorithms SCALE1, SCALE2, and
  // SCALE3 for Determination of Scales on Computer Generated
  // Plots", Communications of the ACM, 10 (1973), 639-640.
  // Also cited as ACM Algorithm 463.

  double a;
  int b, x;

  magform ((hi-lo)/ticint, a, b);

  static const double sqrt_2 = sqrt (2.0);
  static const double sqrt_10 = sqrt (10.0);
  static const double sqrt_50 = sqrt (50.0);

  if (a < sqrt_2)
    x = 1;
  else if (a < sqrt_10)
    x = 2;
  else if (a < sqrt_50)
    x = 5;
  else
    x = 10;

  return x * std::pow (10., b);

}

// Attempt to make "nice" limits from the actual max and min of the
// data.  For log plots, we will also use the smallest strictly positive
// value.

Matrix
axes::properties::get_axis_limits (double xmin, double xmax,
                                   double min_pos, double max_neg,
                                   bool logscale)
{
  Matrix retval;

  double min_val = xmin;
  double max_val = xmax;

  if (xisinf (min_val) && min_val > 0 && xisinf (max_val) && max_val < 0)
    {
      retval = default_lim (logscale);
      return retval;
    }
  else if (! (xisinf (min_val) || xisinf (max_val)))
    {
      if (logscale)
        {
          if (xisinf (min_pos) && xisinf (max_neg))
            {
              // TODO -- max_neg is needed for "loglog ([0 -Inf])"
              //         This is the only place where max_neg is needed.
              //         Is there another way?
              retval = default_lim ();
              retval(0) = pow (10., retval(0));
              retval(1) = pow (10., retval(1));
              return retval;
            }
          if ((min_val <= 0 && max_val > 0))
            {
              warning ("axis: omitting non-positive data in log plot");
              min_val = min_pos;
            }
          // FIXME: maybe this test should also be relative?
          if (std::abs (min_val - max_val)
              < sqrt (std::numeric_limits<double>::epsilon ()))
            {
              // Widen range when too small
              if (min_val >= 0)
                {
                  min_val *= 0.9;
                  max_val *= 1.1;
                }
              else
                {
                  min_val *= 1.1;
                  max_val *= 0.9;
                }
            }
          if (min_val > 0)
            {
              // Log plots with all positive data
              min_val = pow (10, gnulib::floor (log10 (min_val)));
              max_val = pow (10, std::ceil (log10 (max_val)));
            }
          else
            {
              // Log plots with all negative data
              min_val = -pow (10, std::ceil (log10 (-min_val)));
              max_val = -pow (10, gnulib::floor (log10 (-max_val)));
            }
        }
      else
        {
          if (min_val == 0 && max_val == 0)
            {
              min_val = -1;
              max_val = 1;
            }
          // FIXME: maybe this test should also be relative?
          else if (std::abs (min_val - max_val)
                   < sqrt (std::numeric_limits<double>::epsilon ()))
            {
              min_val -= 0.1 * std::abs (min_val);
              max_val += 0.1 * std::abs (max_val);
            }

          double tick_sep = calc_tick_sep (min_val , max_val);
          double min_tick = gnulib::floor (min_val / tick_sep);
          double max_tick = std::ceil (max_val / tick_sep);
          // Prevent round-off from cropping ticks
          min_val = std::min (min_val, tick_sep * min_tick);
          max_val = std::max (max_val, tick_sep * max_tick);
        }
    }

  retval.resize (1, 2);

  retval(1) = max_val;
  retval(0) = min_val;

  return retval;
}

void
axes::properties::calc_ticks_and_lims (array_property& lims,
                                       array_property& ticks,
                                       array_property& mticks,
                                       bool limmode_is_auto, bool is_logscale)
{
  // FIXME: add log ticks and lims

  if (lims.get ().is_empty ())
    return;

  double lo = (lims.get ().matrix_value ()) (0);
  double hi = (lims.get ().matrix_value ()) (1);
  bool is_negative = lo < 0 && hi < 0;
  double tmp;
  // FIXME: should this be checked for somewhere else? (i.e. set{x,y,z}lim)
  if (hi < lo)
    std::swap (hi, lo);

  if (is_logscale)
    {
      if (is_negative)
        {
          tmp = hi;
          hi = std::log10 (-lo);
          lo = std::log10 (-tmp);
        }
      else
        {
          hi = std::log10 (hi);
          lo = std::log10 (lo);
        }
    }

  double tick_sep;

  if (is_logscale)
    {
      if (! (xisinf (hi) || xisinf (lo)))
        tick_sep = 1;  // Tick is every order of magnitude (bug #39449)
      else
        tick_sep = 0;
    }
  else
    tick_sep = calc_tick_sep (lo , hi);

  int i1 = static_cast<int> (gnulib::floor (lo / tick_sep));
  int i2 = static_cast<int> (std::ceil (hi / tick_sep));

  if (limmode_is_auto)
    {
      // adjust limits to include min and max tics
      Matrix tmp_lims (1,2);
      tmp_lims(0) = std::min (tick_sep * i1, lo);
      tmp_lims(1) = std::max (tick_sep * i2, hi);

      if (is_logscale)
        {
          tmp_lims(0) = std::pow (10.,tmp_lims(0));
          tmp_lims(1) = std::pow (10.,tmp_lims(1));
          if (tmp_lims(0) <= 0)
            tmp_lims(0) = std::pow (10., lo);
          if (is_negative)
            {
              tmp = tmp_lims(0);
              tmp_lims(0) = -tmp_lims(1);
              tmp_lims(1) = -tmp;
            }
        }
      lims = tmp_lims;
    }

  Matrix tmp_ticks (1, i2-i1+1);
  for (int i = 0; i <= i2-i1; i++)
    {
      tmp_ticks (i) = tick_sep * (i+i1);
      if (is_logscale)
        tmp_ticks (i) = std::pow (10., tmp_ticks (i));
    }
  if (is_logscale && is_negative)
    {
      Matrix rev_ticks (1, i2-i1+1);
      rev_ticks = -tmp_ticks;
      for (int i = 0; i <= i2-i1; i++)
        tmp_ticks (i) = rev_ticks (i2-i1-i);
    }

  ticks = tmp_ticks;

  int n = is_logscale ? 8 : 4;
  Matrix tmp_mticks (1, n * (tmp_ticks.numel () - 1));

  for (int i = 0; i < tmp_ticks.numel ()-1; i++)
    {
      double d = (tmp_ticks (i+1) - tmp_ticks (i)) / (n+1);
      for (int j = 0; j < n; j++)
        {
          tmp_mticks (n*i+j) = tmp_ticks (i) + d * (j+1);
        }
    }
  mticks = tmp_mticks;
}

void
axes::properties::calc_ticklabels (const array_property& ticks,
                                   any_property& labels, bool logscale)
{
  Matrix values = ticks.get ().matrix_value ();
  Cell c (values.dims ());
  std::ostringstream os;

  if (logscale)
    {
      double significand;
      double exponent;
      double exp_max = 0.;
      double exp_min = 0.;

      for (int i = 0; i < values.numel (); i++)
        {
          exp_max = std::max (exp_max, std::log10 (values(i)));
          exp_min = std::max (exp_min, std::log10 (values(i)));
        }

      for (int i = 0; i < values.numel (); i++)
        {
          if (values(i) < 0.)
            exponent = gnulib::floor (std::log10 (-values(i)));
          else
            exponent = gnulib::floor (std::log10 (values(i)));
          significand = values(i) * std::pow (10., -exponent);
          os.str (std::string ());
          os << significand;
          if (exponent < 0.)
            {
              os << "e-";
              exponent = -exponent;
            }
          else
            os << "e+";
          if (exponent < 10. && (exp_max > 9 || exp_min < -9))
            os << "0";
          os << exponent;
          c(i) = os.str ();
        }
    }
  else
    {
      for (int i = 0; i < values.numel (); i++)
        {
          os.str (std::string ());
          os << values(i);
          c(i) = os.str ();
        }
    }

  labels = c;
}

Matrix
axes::properties::get_ticklabel_extents (const Matrix& ticks,
                                         const string_vector& ticklabels,
                                         const Matrix& limits)
{
#ifndef HAVE_FREETYPE
  double fontsize = get ("fontsize").double_value ();
#endif

  Matrix ext (1, 2, 0.0);
  double wmax, hmax;
  wmax = hmax = 0.;
  int n = std::min (ticklabels.numel (), ticks.numel ());
  for (int i = 0; i < n; i++)
    {
      double val = ticks(i);
      if (limits(0) <= val && val <= limits(1))
        {
          std::string label (ticklabels(i));
          label.erase (0, label.find_first_not_of (" "));
          label = label.substr (0, label.find_last_not_of (" ")+1);
#ifdef HAVE_FREETYPE
          ext = text_renderer.get_extent (label, 0.0, "none");
          wmax = std::max (wmax, ext(0));
          hmax = std::max (hmax, ext(1));
#else
          // FIXME: find a better approximation
          int len = label.length ();
          wmax = std::max (wmax, 0.5*fontsize*len);
          hmax = fontsize;
#endif
        }
    }

  ext(0) = wmax;
  ext(1) = hmax;
  return ext;
}

void
get_children_limits (double& min_val, double& max_val,
                     double& min_pos, double& max_neg,
                     const Matrix& kids, char limit_type)
{
  octave_idx_type n = kids.numel ();

  switch (limit_type)
    {
    case 'x':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object obj = gh_manager::get_object (kids(i));

          if (obj.is_xliminclude ())
            {
              octave_value lim = obj.get_xlim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'y':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object obj = gh_manager::get_object (kids(i));

          if (obj.is_yliminclude ())
            {
              octave_value lim = obj.get_ylim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'z':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object obj = gh_manager::get_object (kids(i));

          if (obj.is_zliminclude ())
            {
              octave_value lim = obj.get_zlim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'c':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object obj = gh_manager::get_object (kids(i));

          if (obj.is_climinclude ())
            {
              octave_value lim = obj.get_clim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    case 'a':
      for (octave_idx_type i = 0; i < n; i++)
        {
          graphics_object obj = gh_manager::get_object (kids(i));

          if (obj.is_aliminclude ())
            {
              octave_value lim = obj.get_alim ();

              check_limit_vals (min_val, max_val, min_pos, max_neg, lim);
            }
        }
      break;

    default:
      break;
    }
}

static std::set<double> updating_axis_limits;

void
axes::update_axis_limits (const std::string& axis_type,
                          const graphics_handle& h)
{
  if (updating_axis_limits.find (get_handle ().value ())
      != updating_axis_limits.end ())
    return;

  Matrix kids = Matrix (1, 1, h.value ());

  double min_val = octave_Inf;
  double max_val = -octave_Inf;
  double min_pos = octave_Inf;
  double max_neg = -octave_Inf;

  char update_type = 0;

  Matrix limits;
  double val;

#define FIX_LIMITS \
  if (limits.numel () == 4) \
    { \
      val = limits(0); \
      if (xfinite (val)) \
        min_val = val; \
      val = limits(1); \
      if (xfinite (val)) \
        max_val = val; \
      val = limits(2); \
      if (xfinite (val)) \
        min_pos = val; \
      val = limits(3); \
      if (xfinite (val)) \
        max_neg = val; \
    } \
  else \
    { \
      limits.resize (4, 1); \
      limits(0) = min_val; \
      limits(1) = max_val; \
      limits(2) = min_pos; \
      limits(3) = max_neg; \
    }

  if (axis_type == "xdata" || axis_type == "xscale"
      || axis_type == "xlimmode" || axis_type == "xliminclude"
      || axis_type == "xlim")
    {
      if (xproperties.xlimmode_is ("auto"))
        {
          limits = xproperties.get_xlim ().matrix_value ();
          FIX_LIMITS ;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'x');

          limits = xproperties.get_axis_limits (min_val, max_val,
                                                min_pos, max_neg,
                                                xproperties.xscale_is ("log"));

          update_type = 'x';
        }
    }
  else if (axis_type == "ydata" || axis_type == "yscale"
           || axis_type == "ylimmode" || axis_type == "yliminclude"
           || axis_type == "ylim")
    {
      if (xproperties.ylimmode_is ("auto"))
        {
          limits = xproperties.get_ylim ().matrix_value ();
          FIX_LIMITS ;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'y');

          limits = xproperties.get_axis_limits (min_val, max_val,
                                                min_pos, max_neg,
                                                xproperties.yscale_is ("log"));

          update_type = 'y';
        }
    }
  else if (axis_type == "zdata" || axis_type == "zscale"
           || axis_type == "zlimmode" || axis_type == "zliminclude"
           || axis_type == "zlim")
    {
      if (xproperties.zlimmode_is ("auto"))
        {
          limits = xproperties.get_zlim ().matrix_value ();
          FIX_LIMITS ;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

          limits = xproperties.get_axis_limits (min_val, max_val,
                                                min_pos, max_neg,
                                                xproperties.zscale_is ("log"));

          update_type = 'z';
        }
    }
  else if (axis_type == "cdata" || axis_type == "climmode"
           || axis_type == "cdatamapping" || axis_type == "climinclude"
           || axis_type == "clim")
    {
      if (xproperties.climmode_is ("auto"))
        {
          limits = xproperties.get_clim ().matrix_value ();
          FIX_LIMITS ;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'c');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            {
              max_val = min_val + 1;
              min_val -= 1;
            }

          limits.resize (1, 2);

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'c';
        }

    }
  else if (axis_type == "alphadata" || axis_type == "alimmode"
           || axis_type == "alphadatamapping" || axis_type == "aliminclude"
           || axis_type == "alim")
    {
      if (xproperties.alimmode_is ("auto"))
        {
          limits = xproperties.get_alim ().matrix_value ();
          FIX_LIMITS ;

          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'a');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            max_val = min_val + 1;

          limits.resize (1, 2);

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'a';
        }

    }

#undef FIX_LIMITS

  unwind_protect frame;
  frame.protect_var (updating_axis_limits);

  updating_axis_limits.insert (get_handle ().value ());

  switch (update_type)
    {
    case 'x':
      xproperties.set_xlim (limits);
      xproperties.set_xlimmode ("auto");
      xproperties.update_xlim ();
      break;

    case 'y':
      xproperties.set_ylim (limits);
      xproperties.set_ylimmode ("auto");
      xproperties.update_ylim ();
      break;

    case 'z':
      xproperties.set_zlim (limits);
      xproperties.set_zlimmode ("auto");
      xproperties.update_zlim ();
      break;

    case 'c':
      xproperties.set_clim (limits);
      xproperties.set_climmode ("auto");
      break;

    case 'a':
      xproperties.set_alim (limits);
      xproperties.set_alimmode ("auto");
      break;

    default:
      break;
    }

  xproperties.update_transform ();

}

void
axes::update_axis_limits (const std::string& axis_type)
{
  if ((updating_axis_limits.find (get_handle ().value ())
       != updating_axis_limits.end ())
      || (updating_aspectratios.find (get_handle ().value ())
          != updating_aspectratios.end ()))
    return;

  Matrix kids = xproperties.get_children ();

  double min_val = octave_Inf;
  double max_val = -octave_Inf;
  double min_pos = octave_Inf;
  double max_neg = -octave_Inf;

  char update_type = 0;

  Matrix limits;

  if (axis_type == "xdata" || axis_type == "xscale"
      || axis_type == "xlimmode" || axis_type == "xliminclude"
      || axis_type == "xlim")
    {
      if (xproperties.xlimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'x');

          limits = xproperties.get_axis_limits (min_val, max_val,
                                                min_pos, max_neg,
                                                xproperties.xscale_is ("log"));

          update_type = 'x';
        }
    }
  else if (axis_type == "ydata" || axis_type == "yscale"
           || axis_type == "ylimmode" || axis_type == "yliminclude"
           || axis_type == "ylim")
    {
      if (xproperties.ylimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'y');

          limits = xproperties.get_axis_limits (min_val, max_val,
                                                min_pos, max_neg,
                                                xproperties.yscale_is ("log"));

          update_type = 'y';
        }
    }
  else if (axis_type == "zdata" || axis_type == "zscale"
           || axis_type == "zlimmode" || axis_type == "zliminclude"
           || axis_type == "zlim")
    {
      if (xproperties.zlimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

          limits = xproperties.get_axis_limits (min_val, max_val,
                                                min_pos, max_neg,
                                                xproperties.zscale_is ("log"));

          update_type = 'z';
        }
    }
  else if (axis_type == "cdata" || axis_type == "climmode"
           || axis_type == "cdatamapping" || axis_type == "climinclude"
           || axis_type == "clim")
    {
      if (xproperties.climmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'c');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            {
              max_val = min_val + 1;
              min_val -= 1;
            }

          limits.resize (1, 2);

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'c';
        }

    }
  else if (axis_type == "alphadata" || axis_type == "alimmode"
           || axis_type == "alphadatamapping" || axis_type == "aliminclude"
           || axis_type == "alim")
    {
      if (xproperties.alimmode_is ("auto"))
        {
          get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'a');

          if (min_val > max_val)
            {
              min_val = min_pos = 0;
              max_val = 1;
            }
          else if (min_val == max_val)
            max_val = min_val + 1;

          limits.resize (1, 2);

          limits(0) = min_val;
          limits(1) = max_val;

          update_type = 'a';
        }

    }

  unwind_protect frame;
  frame.protect_var (updating_axis_limits);

  updating_axis_limits.insert (get_handle ().value ());

  switch (update_type)
    {
    case 'x':
      xproperties.set_xlim (limits);
      xproperties.set_xlimmode ("auto");
      xproperties.update_xlim ();
      break;

    case 'y':
      xproperties.set_ylim (limits);
      xproperties.set_ylimmode ("auto");
      xproperties.update_ylim ();
      break;

    case 'z':
      xproperties.set_zlim (limits);
      xproperties.set_zlimmode ("auto");
      xproperties.update_zlim ();
      break;

    case 'c':
      xproperties.set_clim (limits);
      xproperties.set_climmode ("auto");
      break;

    case 'a':
      xproperties.set_alim (limits);
      xproperties.set_alimmode ("auto");
      break;

    default:
      break;
    }

  xproperties.update_transform ();
}

inline
double force_in_range (double x, double lower, double upper)
{
  if (x < lower)
    return lower;
  else if (x > upper)
    return upper;
  else
    return x;
}

static Matrix
do_zoom (double val, double factor, const Matrix& lims, bool is_logscale)
{
  Matrix new_lims = lims;

  double lo = lims(0);
  double hi = lims(1);

  bool is_negative = lo < 0 && hi < 0;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = hi;
          hi = std::log10 (-lo);
          lo = std::log10 (-tmp);
          val = std::log10 (-val);
        }
      else
        {
          hi = std::log10 (hi);
          lo = std::log10 (lo);
          val = std::log10 (val);
        }
    }

  // Perform the zooming
  lo = val + (lo - val) / factor;
  hi = val + (hi - val) / factor;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = -std::pow (10.0, hi);
          hi = -std::pow (10.0, lo);
          lo = tmp;
        }
      else
        {
          lo = std::pow (10.0, lo);
          hi = std::pow (10.0, hi);
        }
    }

  new_lims(0) = lo;
  new_lims(1) = hi;

  return new_lims;
}

void
axes::properties::zoom_about_point (const std::string& mode,
                                    double x, double y, double factor,
                                    bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  // Get children axes limits
  Matrix kids = get_children ();
  double minx = octave_Inf;
  double maxx = -octave_Inf;
  double min_pos_x = octave_Inf;
  double max_neg_x = -octave_Inf;
  get_children_limits (minx, maxx, min_pos_x, max_neg_x, kids, 'x');

  double miny = octave_Inf;
  double maxy = -octave_Inf;
  double min_pos_y = octave_Inf;
  double max_neg_y = -octave_Inf;
  get_children_limits (miny, maxy, min_pos_y, max_neg_y, kids, 'y');

  xlims = do_zoom (x, factor, xlims, xscale_is ("log"));
  ylims = do_zoom (y, factor, ylims, yscale_is ("log"));

  zoom (mode, xlims, ylims, push_to_zoom_stack);
}

void
axes::properties::zoom (const std::string& mode, double factor,
                        bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  double x = (xlims(0) + xlims(1)) / 2;
  double y = (ylims(0) + ylims(1)) / 2;

  zoom_about_point (mode, x, y, factor, push_to_zoom_stack);
}

void
axes::properties::push_zoom_stack (void)
{
  if (zoom_stack.empty ())
    {
      zoom_stack.push_front (xlimmode.get ());
      zoom_stack.push_front (xlim.get ());
      zoom_stack.push_front (ylimmode.get ());
      zoom_stack.push_front (ylim.get ());
      zoom_stack.push_front (zlimmode.get ());
      zoom_stack.push_front (zlim.get ());
      zoom_stack.push_front (view.get ());
    }
}

void
axes::properties::zoom (const std::string& mode,
                        const Matrix& xl, const Matrix& yl,
                        bool push_to_zoom_stack)
{
  if (push_to_zoom_stack)
    push_zoom_stack ();

  if (mode == "horizontal" || mode == "both")
    {
      xlim = xl;
      xlimmode = "manual";
    }

  if (mode == "vertical" || mode == "both")
    {
      ylim = yl;
      ylimmode = "manual";
    }

  update_transform ();

  if (mode == "horizontal" || mode == "both")
    update_xlim ();

  if (mode == "vertical" || mode == "both")
    update_ylim ();
}

static Matrix
do_translate (double x0, double x1, const Matrix& lims, bool is_logscale)
{
  Matrix new_lims = lims;

  double lo = lims(0);
  double hi = lims(1);

  bool is_negative = lo < 0 && hi < 0;

  double delta;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = hi;
          hi = std::log10 (-lo);
          lo = std::log10 (-tmp);
          x0 = -x0;
          x1 = -x1;
        }
      else
        {
          hi = std::log10 (hi);
          lo = std::log10 (lo);
        }

      delta = std::log10 (x0) - std::log10 (x1);
    }
  else
    {
      delta = x0 - x1;
    }

  // Perform the translation
  lo += delta;
  hi += delta;

  if (is_logscale)
    {
      if (is_negative)
        {
          double tmp = -std::pow (10.0, hi);
          hi = -std::pow (10.0, lo);
          lo = tmp;
        }
      else
        {
          lo = std::pow (10.0, lo);
          hi = std::pow (10.0, hi);
        }
    }

  new_lims(0) = lo;
  new_lims(1) = hi;

  return new_lims;
}

void
axes::properties::translate_view (const std::string& mode,
                                  double x0, double x1, double y0, double y1,
                                  bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  // Get children axes limits
  Matrix kids = get_children ();
  double minx = octave_Inf;
  double maxx = -octave_Inf;
  double min_pos_x = octave_Inf;
  double max_neg_x = -octave_Inf;
  get_children_limits (minx, maxx, min_pos_x, max_neg_x, kids, 'x');

  double miny = octave_Inf;
  double maxy = -octave_Inf;
  double min_pos_y = octave_Inf;
  double max_neg_y = -octave_Inf;
  get_children_limits (miny, maxy, min_pos_y, max_neg_y, kids, 'y');

  xlims = do_translate (x0, x1, xlims, xscale_is ("log"));
  ylims = do_translate (y0, y1, ylims, yscale_is ("log"));

  zoom (mode, xlims, ylims, push_to_zoom_stack);
}

void
axes::properties::pan (const std::string& mode, double factor,
                       bool push_to_zoom_stack)
{
  // FIXME: Do we need error checking here?
  Matrix xlims = get_xlim ().matrix_value ();
  Matrix ylims = get_ylim ().matrix_value ();

  double x0 = (xlims(0) + xlims(1)) / 2;
  double y0 = (ylims(0) + ylims(1)) / 2;

  double x1 = x0 + (xlims(1) - xlims(0)) * factor;
  double y1 = y0 + (ylims(1) - ylims(0)) * factor;

  translate_view (mode, x0, x1, y0, y1, push_to_zoom_stack);
}

void
axes::properties::rotate3d (double x0, double x1, double y0, double y1,
                            bool push_to_zoom_stack)
{
  if (push_to_zoom_stack)
    push_zoom_stack ();

  Matrix bb = get_boundingbox (true);
  Matrix new_view = get_view ().matrix_value ();

  // Compute new view angles
  new_view(0) += ((x0 - x1) * (180.0 / bb(2)));
  new_view(1) += ((y1 - y0) * (180.0 / bb(3)));

  // Clipping
  new_view(1) = std::min (new_view(1), 90.0);
  new_view(1) = std::max (new_view(1), -90.0);
  if (new_view(0) > 180.0)
    new_view(0) -= 360.0;
  else if (new_view(0) < -180.0)
    new_view(0) += 360.0;

  // Snapping
  double snapMargin = 1.0;
  for (int a = -90; a <= 90; a += 90)
    {
      if ((a - snapMargin) < new_view(1)
          && new_view(1) < (a + snapMargin))
        {
          new_view(1) = a;
          break;
        }
    }

  for (int a = -180; a <= 180; a += 180)
    if ((a - snapMargin) < new_view(0)
        && new_view(0) < (a + snapMargin))
      {
        if (a == 180)
          new_view(0) = -180;
        else
          new_view(0) = a;
        break;
      }

  // Update axes properties
  set_view (new_view);
}

void
axes::properties::rotate_view (double delta_el, double delta_az,
                               bool push_to_zoom_stack)
{
  if (push_to_zoom_stack)
    push_zoom_stack ();

  Matrix v = get_view ().matrix_value ();

  v(1) += delta_el;

  if (v(1) > 90)
    v(1) = 90;
  if (v(1) < -90)
    v(1) = -90;

  v(0) = fmod (v(0) - delta_az + 720,360);

  set_view (v);

  update_transform ();
}

void
axes::properties::unzoom (void)
{
  if (zoom_stack.size () >= 7)
    {
      view = zoom_stack.front ();
      zoom_stack.pop_front ();

      zlim = zoom_stack.front ();
      zoom_stack.pop_front ();

      zlimmode = zoom_stack.front ();
      zoom_stack.pop_front ();

      ylim = zoom_stack.front ();
      zoom_stack.pop_front ();

      ylimmode = zoom_stack.front ();
      zoom_stack.pop_front ();

      xlim = zoom_stack.front ();
      zoom_stack.pop_front ();

      xlimmode = zoom_stack.front ();
      zoom_stack.pop_front ();

      update_transform ();

      update_xlim ();
      update_ylim ();
      update_zlim ();

      update_view ();
    }
}

void
axes::properties::clear_zoom_stack (bool do_unzoom)
{
  size_t items_to_leave_on_stack = do_unzoom ? 7 : 0;

  while (zoom_stack.size () > items_to_leave_on_stack)
    zoom_stack.pop_front ();

  if (do_unzoom)
    unzoom ();
}

void
axes::reset_default_properties (void)
{
  // empty list of local defaults
  default_properties = property_list ();

  // reset factory defaults
  set_defaults ("reset");
}

void
axes::initialize (const graphics_object& go)
{
  base_graphics_object::initialize (go);

  xinitialize (xproperties.get_title ());
  xinitialize (xproperties.get_xlabel ());
  xinitialize (xproperties.get_ylabel ());
  xinitialize (xproperties.get_zlabel ());

  xproperties.sync_positions ();
}

// ---------------------------------------------------------------------

Matrix
line::properties::compute_xlim (void) const
{
  Matrix m (1, 4);

  m(0) = xdata.min_val ();
  m(1) = xdata.max_val ();
  m(2) = xdata.min_pos ();
  m(3) = xdata.max_neg ();

  return m;
}

Matrix
line::properties::compute_ylim (void) const
{
  Matrix m (1, 4);

  m(0) = ydata.min_val ();
  m(1) = ydata.max_val ();
  m(2) = ydata.min_pos ();
  m(3) = ydata.max_neg ();

  return m;
}

// ---------------------------------------------------------------------

Matrix
text::properties::get_data_position (void) const
{
  Matrix pos = get_position ().matrix_value ();

  if (! units_is ("data"))
    pos = convert_text_position (pos, *this, get_units (), "data");

  return pos;
}

Matrix
text::properties::get_extent_matrix (void) const
{
  // FIXME: Should this function also add the (x,y) base position?
  return extent.get ().matrix_value ();
}

octave_value
text::properties::get_extent (void) const
{
  // FIXME: This doesn't work right for 3D plots.
  // (It doesn't in Matlab either, at least not in version 6.5.)
  Matrix m = extent.get ().matrix_value ();
  Matrix pos = get_position ().matrix_value ();
  Matrix p = convert_text_position (pos, *this, get_units (), "pixels");

  m(0) += p(0);
  m(1) += p(1);

  return convert_text_position (m, *this, "pixels", get_units ());
}

void
text::properties::set_fontunits (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_fontunits = get_fontunits ();
      if (fontunits.set (v, true))
        {
          update_fontunits (old_fontunits);
          mark_modified ();
        }
    }
}

void
text::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = 0;
  double fsz = get_fontsize ();

  if (new_units == "normalized")
    {
      graphics_object go (gh_manager::get_object (get___myhandle__ ()));
      graphics_object ax (go.get_ancestor ("axes"));

      parent_height = ax.get_properties ().get_boundingbox (true).elem (3);
    }

  fsz = convert_font_size (fsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fsz));
}

void
text::properties::update_font (void)
{
#ifdef HAVE_FREETYPE
#ifdef HAVE_FONTCONFIG
  renderer.set_font (get ("fontname").string_value (),
                     get ("fontweight").string_value (),
                     get ("fontangle").string_value (),
                     get ("fontsize_points").double_value ());
#endif
  renderer.set_color (get_color_rgb ());
#endif
}

void
text::properties::update_text_extent (void)
{
#ifdef HAVE_FREETYPE

  int halign = 0;
  int valign = 0;

  if (horizontalalignment_is ("center"))
    halign = 1;
  else if (horizontalalignment_is ("right"))
    halign = 2;

  if (verticalalignment_is ("middle"))
    valign = 1;
  else if (verticalalignment_is ("top"))
    valign = 2;
  else if (verticalalignment_is ("baseline"))
    valign = 3;
  else if (verticalalignment_is ("cap"))
    valign = 4;

  Matrix bbox;

  // FIXME: string should be parsed only when modified, for efficiency

  octave_value string_prop = get_string ();

  string_vector sv = string_prop.all_strings ();

  renderer.text_to_pixels (sv.join ("\n"), pixels, bbox,
                           halign, valign, get_rotation (),
                           get_interpreter ());
  /* The bbox is relative to the text's position.
     We'll leave it that way, because get_position () does not return
     valid results when the text is first constructed.
     Conversion to proper coordinates is performed in get_extent. */
  set_extent (bbox);

#endif

  if (autopos_tag_is ("xlabel") || autopos_tag_is ("ylabel")
      || autopos_tag_is ("zlabel") || autopos_tag_is ("title"))
    update_autopos ("sync");
}

void
text::properties::request_autopos (void)
{
  if (autopos_tag_is ("xlabel") || autopos_tag_is ("ylabel")
      || autopos_tag_is ("zlabel") || autopos_tag_is ("title"))
    update_autopos (get_autopos_tag ());
}

void
text::properties::update_units (void)
{
  if (! units_is ("data"))
    {
      set_xliminclude ("off");
      set_yliminclude ("off");
      set_zliminclude ("off");
    }

  Matrix pos = get_position ().matrix_value ();

  pos = convert_text_position (pos, *this, cached_units, get_units ());

  // FIXME: if the current axes view is 2D, then one should
  // probably drop the z-component of "pos" and leave "zliminclude"
  // to "off".

  bool autopos = positionmode_is ("auto");

  set_position (pos);

  if (autopos)
    set_positionmode ("auto");

  if (units_is ("data"))
    {
      set_xliminclude ("on");
      set_yliminclude ("on");
      // FIXME: see above
      set_zliminclude ("off");
    }

  cached_units = get_units ();
}

double
text::properties::get_fontsize_points (double box_pix_height) const
{
  double fs = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    {
      graphics_object go (gh_manager::get_object (get___myhandle__ ()));
      graphics_object ax (go.get_ancestor ("axes"));

      parent_height = ax.get_properties ().get_boundingbox (true).elem (3);
    }

  return convert_font_size (fs, get_fontunits (), "points", parent_height);
}

// ---------------------------------------------------------------------

octave_value
image::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_cdata (),
                        cdatamapping_is ("scaled"), 3);
}

// ---------------------------------------------------------------------

octave_value
patch::properties::get_color_data (void) const
{
  octave_value fvc = get_facevertexcdata ();
  if (fvc.is_undefined () || fvc.is_empty ())
    return Matrix ();
  else
    return convert_cdata (*this, fvc,cdatamapping_is ("scaled"), 2);
}

static bool updating_patch_data = false;

void
patch::properties::update_fvc (void)
{
  if (updating_patch_data)
    return;

  Matrix xd = get_xdata ().matrix_value ();
  Matrix yd = get_ydata ().matrix_value ();
  Matrix zd = get_zdata ().matrix_value ();
  NDArray cd = get_cdata ().array_value ();

  bad_data_msg = std::string ();
  if (xd.dims () != yd.dims ()
      || (xd.dims () != zd.dims () && ! zd.is_empty ()))
    {
      bad_data_msg = "x/y/zdata should have the same dimensions";
      return;
    }

  // Faces and Vertices
  dim_vector dv;
  bool is3D = false;
  octave_idx_type nr = xd.rows ();
  octave_idx_type nc = xd.columns ();
  if (nr == 1 && nc > 1)
    {
      nr = nc;
      nc = 1;
      xd = xd.as_column ();
      yd = yd.as_column ();
      zd = zd.as_column ();
    }

  dv(0) = nr * nc;
  if (zd.is_empty ())
    dv(1) = 2;
  else
    {
      dv(1) = 3;
      is3D = true;
    }

  Matrix vert (dv);
  Matrix idx (nc, nr);

  octave_idx_type kk = 0;
  for (octave_idx_type jj = 0; jj < nc; jj++)
    {
      for (octave_idx_type ii = 0; ii < nr; ii++)
        {
          vert(kk,0) = xd(ii,jj);
          vert(kk,1) = yd(ii,jj);
          if (is3D)
            vert(kk,2) = zd(ii,jj);

          idx(jj,ii) = static_cast<double> (kk+1);

          kk++;
        }
    }

  // facevertexcdata
  Matrix fvc;
  if (cd.ndims () == 3)
    {
      dv(0) = cd.rows () * cd.columns ();
      dv(1) = cd.dims ()(2);
      fvc = cd.reshape (dv);
    }
  else
    fvc = cd.as_column ();

  // FIXME: shouldn't we update facevertexalphadata here ?

  unwind_protect frame;
  frame.protect_var (updating_patch_data);
  updating_patch_data = true;

  faces.set (idx);
  vertices.set (vert);
  facevertexcdata.set (fvc);
}


void
patch::properties::update_data (void)
{
  if (updating_patch_data)
    return;

  Matrix idx = get_faces ().matrix_value ().transpose ();
  Matrix vert = get_vertices ().matrix_value ();
  NDArray fvc = get_facevertexcdata ().array_value ();

  octave_idx_type nfaces = idx.columns ();
  octave_idx_type nvert = vert.rows ();

  // Check all vertices in faces are defined
  bad_data_msg = std::string ();
  if (static_cast<double> (nvert) < idx.row_max ().max ())
    {
      bad_data_msg = "some vertices in \"faces\" property are undefined";
      return;
    }

  // Replace NaNs
  if (idx.any_element_is_inf_or_nan ())
    {
      for (octave_idx_type jj = 0; jj < idx.columns (); jj++)
        {
          double valid_vert = idx(0,jj);
          bool turn_valid = false;
          for (octave_idx_type ii = 0; ii < idx.rows (); ii++)
            {
              if (xisnan (idx(ii,jj)) || turn_valid)
                {
                  idx(ii,jj) = valid_vert;
                  turn_valid = true;
                }
              else
                valid_vert = idx(ii,jj);
            }
        }
    }

  // Build cdata
  dim_vector dv = dim_vector::alloc (3);
  NDArray cd;
  bool pervertex = false;

  if (fvc.rows () == nfaces || fvc.rows () == 1)
    {
      dv(0) = 1;
      dv(1) = fvc.rows ();
      dv(2) = fvc.columns ();
      cd = fvc.reshape (dv);
    }
  else
    {
      if (! fvc.is_empty ())
        {
          dv(0) = idx.rows ();
          dv(1) = nfaces;
          dv(2) = fvc.columns ();
          cd.resize (dv);
          pervertex = true;
        }
    }

  // Build x,y,zdata and eventually per vertex cdata
  Matrix xd (idx.dims ());
  Matrix yd (idx.dims ());
  Matrix zd;
  bool has_zd = false;
  if (vert.columns () > 2)
    {
      zd = Matrix (idx.dims ());
      has_zd = true;
    }


  for (octave_idx_type jj = 0; jj < nfaces; jj++)
    {
      for (octave_idx_type ii = 0; ii < idx.rows (); ii++)
        {
          octave_idx_type row = static_cast<octave_idx_type> (idx(ii,jj)-1);
          xd(ii,jj) = vert(row,0);
          yd(ii,jj) = vert(row,1);

          if (has_zd)
            zd(ii,jj) = vert(row,2);

          if (pervertex)
            for (int kk = 0; kk < fvc.columns (); kk++)
              cd(ii,jj,kk) = fvc(row,kk);
        }
    }


  unwind_protect frame;
  frame.protect_var (updating_patch_data);
  updating_patch_data = true;

  set_xdata (xd);
  set_ydata (yd);
  set_zdata (zd);
  set_cdata (cd);
}

// ---------------------------------------------------------------------

octave_value
surface::properties::get_color_data (void) const
{
  return convert_cdata (*this, get_cdata (), cdatamapping_is ("scaled"), 3);
}

inline void
cross_product (double x1, double y1, double z1,
               double x2, double y2, double z2,
               double& x, double& y, double& z)
{
  x += (y1 * z2 - z1 * y2);
  y += (z1 * x2 - x1 * z2);
  z += (x1 * y2 - y1 * x2);
}

void
surface::properties::update_normals (void)
{
  if (normalmode_is ("auto"))
    {
      Matrix x = get_xdata ().matrix_value ();
      Matrix y = get_ydata ().matrix_value ();
      Matrix z = get_zdata ().matrix_value ();

      int p = z.columns ();
      int q = z.rows ();

      // FIXME: There might be a cleaner way to do this.  When data is changed
      // the update_xdata, update_ydata, update_zdata routines are called in a
      // serial fashion.  Until the final call to update_zdata the matrices
      // will be of mismatched dimensions which can cause an out-of-bound
      // indexing in the code below.  This one-liner prevents calculating
      // normals until dimensions match.
      if (x.columns () != p || y.rows () != q)
        return;

      NDArray n (dim_vector (q, p, 3), 0.0);

      bool x_mat = (x.rows () == q);
      bool y_mat = (y.columns () == p);

      int i1, i2, i3, j1, j2, j3;
      i1 = i2 = i3 = 0;
      j1 = j2 = j3 = 0;

      for (int i = 0; i < p; i++)
        {
          if (y_mat)
            {
              i1 = i - 1;
              i2 = i;
              i3 = i + 1;
            }

          for (int j = 0; j < q; j++)
            {
              if (x_mat)
                {
                  j1 = j - 1;
                  j2 = j;
                  j3 = j + 1;
                }

              double& nx = n(j, i, 0);
              double& ny = n(j, i, 1);
              double& nz = n(j, i, 2);

              if ((j > 0) && (i > 0))
                // upper left quadrangle
                cross_product
                  (x(j1,i-1)-x(j2,i), y(j-1,i1)-y(j,i2), z(j-1,i-1)-z(j,i),
                   x(j2,i-1)-x(j1,i), y(j,i1)-y(j-1,i2), z(j,i-1)-z(j-1,i),
                   nx, ny, nz);

              if ((j > 0) && (i < (p -1)))
                // upper right quadrangle
                cross_product
                  (x(j1,i+1)-x(j2,i), y(j-1,i3)-y(j,i2), z(j-1,i+1)-z(j,i),
                   x(j1,i)-x(j2,i+1), y(j-1,i2)-y(j,i3), z(j-1,i)-z(j,i+1),
                   nx, ny, nz);

              if ((j < (q - 1)) && (i > 0))
                // lower left quadrangle
                cross_product
                  (x(j2,i-1)-x(j3,i), y(j,i1)-y(j+1,i2), z(j,i-1)-z(j+1,i),
                   x(j3,i-1)-x(j2,i), y(j+1,i1)-y(j,i2), z(j+1,i-1)-z(j,i),
                   nx, ny, nz);

              if ((j < (q - 1)) && (i < (p -1)))
                // lower right quadrangle
                cross_product
                  (x(j3,i)-x(j2,i+1), y(j+1,i2)-y(j,i3), z(j+1,i)-z(j,i+1),
                   x(j3,i+1)-x(j2,i), y(j+1,i3)-y(j,i2), z(j+1,i+1)-z(j,i),
                   nx, ny, nz);

              double d = -std::max (std::max (fabs (nx), fabs (ny)), fabs (nz));

              nx /= d;
              ny /= d;
              nz /= d;
            }
        }
      vertexnormals = n;
    }
}

// ---------------------------------------------------------------------

void
hggroup::properties::update_limits (void) const
{
  graphics_object obj = gh_manager::get_object (__myhandle__);

  if (obj)
    {
      obj.update_axis_limits ("xlim");
      obj.update_axis_limits ("ylim");
      obj.update_axis_limits ("zlim");
      obj.update_axis_limits ("clim");
      obj.update_axis_limits ("alim");
    }
}

void
hggroup::properties::update_limits (const graphics_handle& h) const
{
  graphics_object obj = gh_manager::get_object (__myhandle__);

  if (obj)
    {
      obj.update_axis_limits ("xlim", h);
      obj.update_axis_limits ("ylim", h);
      obj.update_axis_limits ("zlim", h);
      obj.update_axis_limits ("clim", h);
      obj.update_axis_limits ("alim", h);
    }
}

static bool updating_hggroup_limits = false;

void
hggroup::update_axis_limits (const std::string& axis_type,
                             const graphics_handle& h)
{
  if (updating_hggroup_limits)
    return;

  Matrix kids = Matrix (1, 1, h.value ());

  double min_val = octave_Inf;
  double max_val = -octave_Inf;
  double min_pos = octave_Inf;
  double max_neg = -octave_Inf;

  Matrix limits;
  double val;

  char update_type = 0;

  if (axis_type == "xlim" || axis_type == "xliminclude")
    {
      limits = xproperties.get_xlim ().matrix_value ();
      update_type = 'x';
    }
  else if (axis_type == "ylim" || axis_type == "yliminclude")
    {
      limits = xproperties.get_ylim ().matrix_value ();
      update_type = 'y';
    }
  else if (axis_type == "zlim" || axis_type == "zliminclude")
    {
      limits = xproperties.get_zlim ().matrix_value ();
      update_type = 'z';
    }
  else if (axis_type == "clim" || axis_type == "climinclude")
    {
      limits = xproperties.get_clim ().matrix_value ();
      update_type = 'c';
    }
  else if (axis_type == "alim" || axis_type == "aliminclude")
    {
      limits = xproperties.get_alim ().matrix_value ();
      update_type = 'a';
    }

  if (limits.numel () == 4)
    {
      val = limits(0);
      if (xfinite (val))
        min_val = val;
      val = limits(1);
      if (xfinite (val))
        max_val = val;
      val = limits(2);
      if (xfinite (val))
        min_pos = val;
      val = limits(3);
      if (xfinite (val))
        max_neg = val;
    }
  else
    {
      limits.resize (4,1);
      limits(0) = min_val;
      limits(1) = max_val;
      limits(2) = min_pos;
      limits(3) = max_neg;
    }

  get_children_limits (min_val, max_val, min_pos, max_neg, kids, update_type);

  unwind_protect frame;
  frame.protect_var (updating_hggroup_limits);

  updating_hggroup_limits = true;

  if (limits(0) != min_val || limits(1) != max_val
      || limits(2) != min_pos || limits(3) != max_neg)
    {
      limits(0) = min_val;
      limits(1) = max_val;
      limits(2) = min_pos;
      limits(3) = max_neg;

      switch (update_type)
        {
        case 'x':
          xproperties.set_xlim (limits);
          break;

        case 'y':
          xproperties.set_ylim (limits);
          break;

        case 'z':
          xproperties.set_zlim (limits);
          break;

        case 'c':
          xproperties.set_clim (limits);
          break;

        case 'a':
          xproperties.set_alim (limits);
          break;

        default:
          break;
        }

      base_graphics_object::update_axis_limits (axis_type, h);
    }
}

void
hggroup::update_axis_limits (const std::string& axis_type)
{
  if (updating_hggroup_limits)
    return;

  Matrix kids = xproperties.get_children ();

  double min_val = octave_Inf;
  double max_val = -octave_Inf;
  double min_pos = octave_Inf;
  double max_neg = -octave_Inf;

  char update_type = 0;

  if (axis_type == "xlim" || axis_type == "xliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'x');

      update_type = 'x';
    }
  else if (axis_type == "ylim" || axis_type == "yliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'y');

      update_type = 'y';
    }
  else if (axis_type == "zlim" || axis_type == "zliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'z');

      update_type = 'z';
    }
  else if (axis_type == "clim" || axis_type == "climinclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'c');

      update_type = 'c';
    }
  else if (axis_type == "alim" || axis_type == "aliminclude")
    {
      get_children_limits (min_val, max_val, min_pos, max_neg, kids, 'a');

      update_type = 'a';
    }

  unwind_protect frame;
  frame.protect_var (updating_hggroup_limits);

  updating_hggroup_limits = true;

  Matrix limits (1, 4, 0.0);

  limits(0) = min_val;
  limits(1) = max_val;
  limits(2) = min_pos;
  limits(3) = max_neg;

  switch (update_type)
    {
    case 'x':
      xproperties.set_xlim (limits);
      break;

    case 'y':
      xproperties.set_ylim (limits);
      break;

    case 'z':
      xproperties.set_zlim (limits);
      break;

    case 'c':
      xproperties.set_clim (limits);
      break;

    case 'a':
      xproperties.set_alim (limits);
      break;

    default:
      break;
    }

  base_graphics_object::update_axis_limits (axis_type);
}

// ---------------------------------------------------------------------

uicontextmenu::~uicontextmenu (void)
{
  std::list<graphics_handle> lst = xproperties.get_dependent_obj_list ();
  std::list<graphics_handle>::const_iterator it;

  for (it = lst.begin (); it != lst.end (); it++)
    {
      graphics_object go = gh_manager::get_object (*it);

      if (go.valid_object () &&
          go.get ("uicontextmenu") == xproperties.get___myhandle__ ())
        go.set ("uicontextmenu", Matrix ());
    }
}


/*
## Test deletion/reset of uicontextmenu
%!test
%! hf = figure ("visible", "off");
%! hax = axes ("parent", hf);
%! unwind_protect
%!   hctx1 = uicontextmenu ("parent", hf);
%!   hctx2 = uicontextmenu ("parent", hf);
%!   set (hf, "uicontextmenu", hctx2);
%!   set (hax, "uicontextmenu", hctx2);
%!   assert (get (hf, "uicontextmenu"), hctx2);
%!   assert (get (hax, "uicontextmenu"), hctx2);
%!   assert (get (hf, "children"), [hctx2; hctx1; hax]);
%!   delete (hctx2);
%!   assert (get (hf, "uicontextmenu"), []);
%!   assert (get (hax, "uicontextmenu"), []);
%!   assert (get (hf, "children"), [hctx1; hax]);
%!   set (hf, "uicontextmenu", hctx1);
%!   assert (get (hf, "uicontextmenu"), hctx1);
%!   set (hf, "uicontextmenu", []);
%!   assert (get (hf, "uicontextmenu"), []);
%!   assert (get (hf, "children"), [hctx1; hax]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect;
 */

// ---------------------------------------------------------------------

octave_value
uicontrol::properties::get_extent (void) const
{
  Matrix m = extent.get ().matrix_value ();

  graphics_object parent_obj =
    gh_manager::get_object (get_parent ());
  Matrix parent_bbox = parent_obj.get_properties ().get_boundingbox (true),
         parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  return convert_position (m, "pixels", get_units (), parent_size);
}

void
uicontrol::properties::update_text_extent (void)
{
#ifdef HAVE_FREETYPE

  text_element *elt;
  ft_render text_renderer;
  Matrix box;

  // FIXME: parsed content should be cached for efficiency
  // FIXME: support multiline text

  elt = text_parser::parse (get_string_string (), "none");
#ifdef HAVE_FONTCONFIG
  text_renderer.set_font (get_fontname (),
                          get_fontweight (),
                          get_fontangle (),
                          get_fontsize ());
#endif
  box = text_renderer.get_extent (elt, 0);
  delete elt;

  Matrix ext (1, 4, 0.0);

  // FIXME: also handle left and bottom components

  ext(0) = ext(1) = 1;
  ext(2) = box(0);
  ext(3) = box(1);

  set_extent (ext);

#endif
}

void
uicontrol::properties::update_units (void)
{
  Matrix pos = get_position ().matrix_value ();

  graphics_object parent_obj = gh_manager::get_object (get_parent ());
  Matrix parent_bbox = parent_obj.get_properties ().get_boundingbox (true),
         parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  pos = convert_position (pos, cached_units, get_units (), parent_size);
  set_position (pos);

  cached_units = get_units ();
}

void
uicontrol::properties::set_style (const octave_value& st)
{
  if (get___object__ ().is_empty ())
    style = st;
  else
    error ("set: cannot change the style of a uicontrol object after creation.");
}

Matrix
uicontrol::properties::get_boundingbox (bool,
                                        const Matrix& parent_pix_size) const
{
  Matrix pos = get_position ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.numel () == 0)
    {
      graphics_object obj = gh_manager::get_object (get_parent ());

      if (obj.valid_object ())
        parent_size =
          obj.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
      else
        parent_size = default_figure_position ();
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  return pos;
}

void
uicontrol::properties::set_fontunits (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_fontunits = get_fontunits ();
      if (fontunits.set (v, true))
        {
          update_fontunits (old_fontunits);
          mark_modified ();
        }
    }
}

void
uicontrol::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (false).elem (3);
  double fsz = get_fontsize ();

  fsz = convert_font_size (fsz, old_units, new_units, parent_height);

  fontsize.set (octave_value (fsz), true);
}

double
uicontrol::properties::get_fontsize_points (double box_pix_height) const
{
  double fs = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fs, get_fontunits (), "points", parent_height);
}

// ---------------------------------------------------------------------

Matrix
uipanel::properties::get_boundingbox (bool internal,
                                      const Matrix& parent_pix_size) const
{
  Matrix pos = get_position ().matrix_value ();
  Matrix parent_size (parent_pix_size);

  if (parent_size.numel () == 0)
    {
      graphics_object obj = gh_manager::get_object (get_parent ());

      parent_size =
        obj.get_properties ().get_boundingbox (true).extract_n (0, 2, 1, 2);
    }

  pos = convert_position (pos, get_units (), "pixels", parent_size);

  pos(0)--;
  pos(1)--;
  pos(1) = parent_size(1) - pos(1) - pos(3);

  if (internal)
    {
      double outer_height = pos(3);

      pos(0) = pos(1) = 0;

      if (! bordertype_is ("none"))
        {
          double bw = get_borderwidth ();
          double mul = 1.0;

          if (bordertype_is ("etchedin") || bordertype_is ("etchedout"))
            mul = 2.0;

          pos(0) += mul * bw;
          pos(1) += mul * bw;
          pos(2) -= 2 * mul * bw;
          pos(3) -= 2 * mul * bw;
        }

      if (! get_title ().empty ())
        {
          double fs = get_fontsize ();

          if (! fontunits_is ("pixels"))
            {
              double res = xget (0, "screenpixelsperinch").double_value ();

              if (fontunits_is ("points"))
                fs *= (res / 72.0);
              else if (fontunits_is ("inches"))
                fs *= res;
              else if (fontunits_is ("centimeters"))
                fs *= (res / 2.54);
              else if (fontunits_is ("normalized"))
                fs *= outer_height;
            }

          if (titleposition_is ("lefttop") || titleposition_is ("centertop")
              || titleposition_is ("righttop"))
            pos(1) += (fs / 2);
          pos(3) -= (fs / 2);
        }
    }

  return pos;
}

void
uipanel::properties::set_units (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_units = get_units ();
      if (units.set (v, true))
        {
          update_units (old_units);
          mark_modified ();
        }
    }
}

void
uipanel::properties::update_units (const caseless_str& old_units)
{
  Matrix pos = get_position ().matrix_value ();

  graphics_object parent_obj = gh_manager::get_object (get_parent ());
  Matrix parent_bbox = parent_obj.get_properties ().get_boundingbox (true),
         parent_size = parent_bbox.extract_n (0, 2, 1, 2);

  pos = convert_position (pos, old_units, get_units (), parent_size);
  set_position (pos);
}

void
uipanel::properties::set_fontunits (const octave_value& v)
{
  if (! error_state)
    {
      caseless_str old_fontunits = get_fontunits ();
      if (fontunits.set (v, true))
        {
          update_fontunits (old_fontunits);
          mark_modified ();
        }
    }
}

void
uipanel::properties::update_fontunits (const caseless_str& old_units)
{
  caseless_str new_units = get_fontunits ();
  double parent_height = get_boundingbox (false).elem (3);
  double fsz = get_fontsize ();

  fsz = convert_font_size (fsz, old_units, new_units, parent_height);

  set_fontsize (octave_value (fsz));
}

double
uipanel::properties::get_fontsize_points (double box_pix_height) const
{
  double fs = get_fontsize ();
  double parent_height = box_pix_height;

  if (fontunits_is ("normalized") && parent_height <= 0)
    parent_height = get_boundingbox (false).elem (3);

  return convert_font_size (fs, get_fontunits (), "points", parent_height);
}

// ---------------------------------------------------------------------

octave_value
uitoolbar::get_default (const caseless_str& name) const
{
  octave_value retval = default_properties.lookup (name);

  if (retval.is_undefined ())
    {
      graphics_handle parent = get_parent ();
      graphics_object parent_obj = gh_manager::get_object (parent);

      retval = parent_obj.get_default (name);
    }

  return retval;
}

void
uitoolbar::reset_default_properties (void)
{
  // empty list of local defaults
  default_properties = property_list ();

  xreset_default_properties (get_handle (),
                             xproperties.factory_defaults ());
}

// ---------------------------------------------------------------------

octave_value
base_graphics_object::get_default (const caseless_str& name) const
{
  graphics_handle parent = get_parent ();
  graphics_object parent_obj = gh_manager::get_object (parent);

  return parent_obj.get_default (type () + name);
}

octave_value
base_graphics_object::get_factory_default (const caseless_str& name) const
{
  graphics_object parent_obj = gh_manager::get_object (0);

  return parent_obj.get_factory_default (type () + name);
}

// We use a random value for the handle to avoid issues with plots and
// scalar values for the first argument.
gh_manager::gh_manager (void)
  : handle_map (), handle_free_list (),
    next_handle (-1.0 - (rand () + 1.0) / (RAND_MAX + 2.0)),
    figure_list (), graphics_lock (),  event_queue (),
    callback_objects (), event_processing (0)
{
  handle_map[0] = graphics_object (new root_figure ());

  // Make sure the default graphics toolkit is registered.
  gtk_manager::default_toolkit ();
}

void
gh_manager::create_instance (void)
{
  instance = new gh_manager ();

  if (instance)
    singleton_cleanup_list::add (cleanup_instance);
}

graphics_handle
gh_manager::do_make_graphics_handle (const std::string& go_name,
                                     const graphics_handle& p,
                                     bool integer_figure_handle,
                                     bool do_createfcn,
                                     bool do_notify_toolkit)
{
  graphics_handle h = get_handle (integer_figure_handle);

  base_graphics_object *go = 0;

  go = make_graphics_object_from_type (go_name, h, p);

  if (go)
    {
      graphics_object obj (go);

      handle_map[h] = obj;

      // Overriding defaults will work now because the handle is valid
      // and we can find parent objects (not just handles).
      obj.override_defaults ();

      if (go_name == "axes")
        {
          // Handle defaults for labels since overriding defaults for
          // them can't work before the axes object is fully
          // constructed.

          axes::properties& props =
            dynamic_cast<axes::properties&> (obj.get_properties ());

          graphics_object tgo;

          tgo = gh_manager::get_object (props.get_xlabel ());
          tgo.override_defaults ();

          tgo = gh_manager::get_object (props.get_ylabel ());
          tgo.override_defaults ();

          tgo = gh_manager::get_object (props.get_zlabel ());
          tgo.override_defaults ();

          tgo = gh_manager::get_object (props.get_title ());
          tgo.override_defaults ();
        }

      if (do_createfcn)
        go->get_properties ().execute_createfcn ();

      // Notify graphics toolkit.
      if (do_notify_toolkit)
        obj.initialize ();
    }
  else
    error ("gh_manager::do_make_graphics_handle: invalid object type '%s'",
           go_name.c_str ());

  return h;
}

graphics_handle
gh_manager::do_make_figure_handle (double val, bool do_notify_toolkit)
{
  graphics_handle h = val;

  base_graphics_object* go = new figure (h, 0);
  graphics_object obj (go);

  handle_map[h] = obj;

  // Notify graphics toolkit.
  if (do_notify_toolkit)
    obj.initialize ();

  obj.override_defaults ();

  return h;
}

void
gh_manager::do_push_figure (const graphics_handle& h)
{
  do_pop_figure (h);

  figure_list.push_front (h);
}

void
gh_manager::do_pop_figure (const graphics_handle& h)
{
  for (figure_list_iterator p = figure_list.begin ();
       p != figure_list.end ();
       p++)
    {
      if (*p == h)
        {
          figure_list.erase (p);
          break;
        }
    }
}

class
callback_event : public base_graphics_event
{
public:
  callback_event (const graphics_handle& h, const std::string& name,
                  const octave_value& data = Matrix ())
    : base_graphics_event (), handle (h), callback_name (name),
      callback (), callback_data (data) { }

  callback_event (const graphics_handle& h, const octave_value& cb,
                  const octave_value& data = Matrix ())
    : base_graphics_event (), handle (h), callback_name (),
      callback (cb), callback_data (data) { }

  void execute (void)
  {
    if (callback.is_defined ())
      gh_manager::execute_callback (handle, callback, callback_data);
    else
      gh_manager::execute_callback (handle, callback_name, callback_data);
  }

private:
  callback_event (void)
    : base_graphics_event (), handle (), callback_name (), callback_data ()
  { }

private:
  graphics_handle handle;
  std::string callback_name;
  octave_value callback;
  octave_value callback_data;
};

class
function_event : public base_graphics_event
{
public:
  function_event (graphics_event::event_fcn fcn, void* data = 0)
    : base_graphics_event (), function (fcn), function_data (data)
  { }

  void execute (void)
  {
    function (function_data);
  }

private:

  graphics_event::event_fcn function;

  void* function_data;

  // function_event objects must be created with at least a function.
  function_event (void);

  // No copying!

  function_event (const function_event&);

  function_event & operator = (const function_event&);
};

class
set_event : public base_graphics_event
{
public:
  set_event (const graphics_handle& h, const std::string& name,
             const octave_value& value, bool do_notify_toolkit = true)
    : base_graphics_event (), handle (h), property_name (name),
      property_value (value), notify_toolkit (do_notify_toolkit) { }

  void execute (void)
  {
    gh_manager::auto_lock guard;

    graphics_object go = gh_manager::get_object (handle);

    if (go)
      {
        property p = go.get_properties ().get_property (property_name);

        if (p.ok ())
          p.set (property_value, true, notify_toolkit);
      }
  }

private:
  set_event (void)
    : base_graphics_event (), handle (), property_name (), property_value ()
  { }

private:
  graphics_handle handle;
  std::string property_name;
  octave_value property_value;
  bool notify_toolkit;
};

graphics_event
graphics_event::create_callback_event (const graphics_handle& h,
                                       const std::string& name,
                                       const octave_value& data)
{
  graphics_event e;

  e.rep = new callback_event (h, name, data);

  return e;
}

graphics_event
graphics_event::create_callback_event (const graphics_handle& h,
                                       const octave_value& cb,
                                       const octave_value& data)
{
  graphics_event e;

  e.rep = new callback_event (h, cb, data);

  return e;
}

graphics_event
graphics_event::create_function_event (graphics_event::event_fcn fcn,
                                       void *data)
{
  graphics_event e;

  e.rep = new function_event (fcn, data);

  return e;
}

graphics_event
graphics_event::create_set_event (const graphics_handle& h,
                                  const std::string& name,
                                  const octave_value& data,
                                  bool notify_toolkit)
{
  graphics_event e;

  e.rep = new set_event (h, name, data, notify_toolkit);

  return e;
}

static void
xset_gcbo (const graphics_handle& h)
{
  graphics_object go = gh_manager::get_object (0);
  root_figure::properties& props =
    dynamic_cast<root_figure::properties&> (go.get_properties ());

  props.set_callbackobject (h.as_octave_value ());
}

void
gh_manager::do_restore_gcbo (void)
{
  gh_manager::auto_lock guard;

  callback_objects.pop_front ();

  xset_gcbo (callback_objects.empty ()
             ? graphics_handle ()
             : callback_objects.front ().get_handle ());
}

void
gh_manager::do_execute_listener (const graphics_handle& h,
                                 const octave_value& l)
{
  if (octave_thread::is_octave_thread ())
    gh_manager::execute_callback (h, l, octave_value ());
  else
    {
      gh_manager::auto_lock guard;

      do_post_event (graphics_event::create_callback_event (h, l));
    }
}

void
gh_manager::do_execute_callback (const graphics_handle& h,
                                 const octave_value& cb_arg,
                                 const octave_value& data)
{
  if (cb_arg.is_defined () && ! cb_arg.is_empty ())
    {
      octave_value_list args;
      octave_function *fcn = 0;

      args(0) = h.as_octave_value ();
      if (data.is_defined ())
        args(1) = data;
      else
        args(1) = Matrix ();

      unwind_protect_safe frame;
      frame.add_fcn (gh_manager::restore_gcbo);

      if (true)
        {
          gh_manager::auto_lock guard;

          callback_objects.push_front (get_object (h));
          xset_gcbo (h);
        }

      BEGIN_INTERRUPT_WITH_EXCEPTIONS;

      // Copy CB because "function_value" method is non-const.

      octave_value cb = cb_arg;

      if (cb.is_function () || cb.is_function_handle ())
        fcn = cb.function_value ();
      else if (cb.is_string ())
        {
          int status;
          std::string s = cb.string_value ();

          eval_string (s, false, status, 0);
        }
      else if (cb.is_cell () && cb.length () > 0
               && (cb.rows () == 1 || cb.columns () == 1)
               && (cb.cell_value ()(0).is_function ()
                   || cb.cell_value ()(0).is_function_handle ()))
        {
          Cell c = cb.cell_value ();

          fcn = c(0).function_value ();
          if (! error_state)
            {
              for (int i = 1; i < c.length () ; i++)
                args(1+i) = c(i);
            }
        }
      else
        {
          std::string nm = cb.class_name ();
          error ("trying to execute non-executable object (class = %s)",
                 nm.c_str ());
        }

      if (fcn && ! error_state)
        feval (fcn, args);

      END_INTERRUPT_WITH_EXCEPTIONS;
    }
}

void
gh_manager::do_post_event (const graphics_event& e)
{
  event_queue.push_back (e);

  command_editor::add_event_hook (gh_manager::process_events);
}

void
gh_manager::do_post_callback (const graphics_handle& h, const std::string& name,
                              const octave_value& data)
{
  gh_manager::auto_lock guard;

  graphics_object go = get_object (h);

  if (go.valid_object ())
    {
      if (callback_objects.empty ())
        do_post_event (graphics_event::create_callback_event (h, name, data));
      else
        {
          const graphics_object& current = callback_objects.front ();

          if (current.get_properties ().is_interruptible ())
            do_post_event (graphics_event::create_callback_event (h, name,
                                                                  data));
          else
            {
              caseless_str busy_action (go.get_properties ().get_busyaction ());

              if (busy_action.compare ("queue"))
                do_post_event (graphics_event::create_callback_event (h, name,
                                                                      data));
              else
                {
                  caseless_str cname (name);

                  if (cname.compare ("deletefcn") || cname.compare ("createfcn")
                      || (go.isa ("figure")
                          && (cname.compare ("closerequestfcn")
                              || cname.compare ("resizefcn"))))
                    do_post_event (
                      graphics_event::create_callback_event (h, name, data));
                }
            }
        }
    }
}

void
gh_manager::do_post_function (graphics_event::event_fcn fcn, void* fcn_data)
{
  gh_manager::auto_lock guard;

  do_post_event (graphics_event::create_function_event (fcn, fcn_data));
}

void
gh_manager::do_post_set (const graphics_handle& h, const std::string& name,
                         const octave_value& value, bool notify_toolkit)
{
  gh_manager::auto_lock guard;

  do_post_event (graphics_event::create_set_event (h, name, value,
                                                   notify_toolkit));
}

int
gh_manager::do_process_events (bool force)
{
  graphics_event e;
  bool old_Vdrawnow_requested = Vdrawnow_requested;
  bool events_executed = false;

  do
    {
      e = graphics_event ();

      gh_manager::lock ();

      if (! event_queue.empty ())
        {
          if (callback_objects.empty () || force)
            {
              e = event_queue.front ();

              event_queue.pop_front ();
            }
          else
            {
              const graphics_object& go = callback_objects.front ();

              if (go.get_properties ().is_interruptible ())
                {
                  e = event_queue.front ();

                  event_queue.pop_front ();
                }
            }
        }

      gh_manager::unlock ();

      if (e.ok ())
        {
          e.execute ();
          events_executed = true;
        }
    }
  while (e.ok ());

  gh_manager::lock ();

  if (event_queue.empty () && event_processing == 0)
    command_editor::remove_event_hook (gh_manager::process_events);

  gh_manager::unlock ();

  if (events_executed)
    flush_octave_stdout ();

  if (Vdrawnow_requested && ! old_Vdrawnow_requested)
    {
      Fdrawnow ();

      Vdrawnow_requested = false;
    }

  return 0;
}

void
gh_manager::do_enable_event_processing (bool enable)
{
  gh_manager::auto_lock guard;

  if (enable)
    {
      event_processing++;

      command_editor::add_event_hook (gh_manager::process_events);
    }
  else
    {
      event_processing--;

      if (event_queue.empty () && event_processing == 0)
        command_editor::remove_event_hook (gh_manager::process_events);
    }
}

property_list::plist_map_type
root_figure::init_factory_properties (void)
{
  property_list::plist_map_type plist_map;

  plist_map["figure"] = figure::properties::factory_defaults ();
  plist_map["axes"] = axes::properties::factory_defaults ();
  plist_map["line"] = line::properties::factory_defaults ();
  plist_map["text"] = text::properties::factory_defaults ();
  plist_map["image"] = image::properties::factory_defaults ();
  plist_map["patch"] = patch::properties::factory_defaults ();
  plist_map["surface"] = surface::properties::factory_defaults ();
  plist_map["hggroup"] = hggroup::properties::factory_defaults ();
  plist_map["uimenu"] = uimenu::properties::factory_defaults ();
  plist_map["uicontrol"] = uicontrol::properties::factory_defaults ();
  plist_map["uipanel"] = uipanel::properties::factory_defaults ();
  plist_map["uicontextmenu"] = uicontextmenu::properties::factory_defaults ();
  plist_map["uitoolbar"] = uitoolbar::properties::factory_defaults ();
  plist_map["uipushtool"] = uipushtool::properties::factory_defaults ();
  plist_map["uitoggletool"] = uitoggletool::properties::factory_defaults ();

  return plist_map;
}

// ---------------------------------------------------------------------

DEFUN (ishandle, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} ishandle (@var{h})\n\
Return true if @var{h} is a graphics handle and false otherwise.\n\
\n\
@var{h} may also be a matrix of handles in which case a logical array is\n\
returned that is true where the elements of @var{h} are graphics handles and\n\
false where they are not.\n\
@seealso{isaxes, isfigure}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  if (args.length () == 1)
    retval = is_handle (args(0));
  else
    print_usage ();

  return retval;
}

static bool
is_handle_visible (const graphics_handle& h)
{
  return h.ok () && gh_manager::is_handle_visible (h);
}

static bool
is_handle_visible (double val)
{
  return is_handle_visible (gh_manager::lookup (val));
}

static octave_value
is_handle_visible (const octave_value& val)
{
  octave_value retval = false;

  if (val.is_real_scalar () && is_handle_visible (val.double_value ()))
    retval = true;
  else if (val.is_numeric_type () && val.is_real_type ())
    {
      const NDArray handles = val.array_value ();

      if (! error_state)
        {
          boolNDArray result (handles.dims ());

          for (octave_idx_type i = 0; i < handles.numel (); i++)
            result.xelem (i) = is_handle_visible (handles (i));

          retval = result;
        }
    }

  return retval;
}

DEFUN (__is_handle_visible__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __is_handle_visible__ (@var{h})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = is_handle_visible (args(0));
  else
    print_usage ();

  return retval;
}

DEFUN (reset, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} reset (@var{h})\n\
Reset the properties of the graphic object @var{h} to their default values.\n\
\n\
For figures, the properties @qcode{\"position\"}, @qcode{\"units\"},\n\
@qcode{\"windowstyle\"}, and @qcode{\"paperunits\"} are not affected.\n\
For axes, the properties @qcode{\"position\"} and @qcode{\"units\"} are\n\
not affected.\n\
\n\
The input @var{h} may also be a vector of graphic handles in which case\n\
each individual object will be reset.\n\
@seealso{cla, clf, newplot}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else
    {
      // get vector of graphics handles
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
          // loop over graphics objects
          for (octave_idx_type n = 0; n < hcv.length (); n++)
            gh_manager::get_object (hcv(n)).reset_default_properties ();

          if (! error_state)
            Fdrawnow ();
        }
    }

  return octave_value ();
}

/*

%!test  # line object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   hax = axes ("defaultlinelinewidth", 3);
%!
%!   hli = line (1:10, 1:10, 1:10, "marker", "o",
%!               "markerfacecolor", "b", "linestyle", ":");
%!
%!   reset (hli);
%!   assert (get (hli, "marker"), get (0, "defaultlinemarker"));
%!   assert (get (hli, "markerfacecolor"),
%!           get (0, "defaultlinemarkerfacecolor"));
%!   assert (get (hli, "linestyle"),
%!           get (0, "defaultlinelinestyle"));
%!   assert (get (hli, "linewidth"), 3, tol);  # parent axes defaults
%!
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # patch object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   t1 = (1/16:1/8:1)' * 2*pi;
%!   t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%!   x1 = sin (t1) - 0.8;
%!   y1 = cos (t1);
%!   x2 = sin (t2) + 0.8;
%!   y2 = cos (t2);
%!   vert = [x1, y1; x2, y2];
%!   fac = [1:8,NaN(1,8);9:24];
%!   hpa = patch ("Faces",fac, "Vertices",vert, "FaceColor","r");
%!
%!   reset (hpa);
%!   assert (get (hpa, "faces"), get (0, "defaultpatchfaces"), tol);
%!   assert (get (hpa, "vertices"), get (0, "defaultpatchvertices"), tol);
%!   assert (get (hpa, "facevertexcdata"),
%!           get (0, "defaultpatchfacevertexcdata"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # surface object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   hsu = surface (peaks, "edgecolor", "none");
%!
%!   reset (hsu);
%!   assert (get (hsu, "xdata"), get (0, "defaultsurfacexdata"), tol);
%!   assert (get (hsu, "ydata"), get (0, "defaultsurfaceydata"), tol);
%!   assert (get (hsu, "zdata"), get (0, "defaultsurfacezdata"), tol);
%!   assert (get (hsu, "edgecolor"),
%!           get (0, "defaultsurfaceedgecolor"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # image object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   him = image (rand (10,10), "cdatamapping", "scaled");
%!
%!   reset (him);
%!   assert (get (him, "cdata"), get (0, "defaultimagecdata"), tol);
%!   assert (get (him, "cdatamapping"),
%!           get (0, "defaultimagecdatamapping"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # text object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   hte = text (5, 5, "Hi!", "fontsize", 20 ,"color", "r");
%!
%!   reset (hte);
%!   assert (get (hte, "position"), get (0, "defaulttextposition"), tol);
%!   assert (get (hte, "fontsize"), get (0, "defaulttextfontsize"), tol);
%!   assert (get (hte, "color"), get (0, "defaulttextcolor"), tol);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # axes object
%! hf = figure ("visible", "off");
%! unwind_protect
%!   tol = 20 * eps;
%!   pos = get (0, "defaultaxesposition") * .5;
%!   hax = axes ("linewidth", 2, "position", pos);
%!   title ("Reset me, please!");
%!
%!   reset (hax);
%!   assert (get (hax, "linewidth"), get (0, "defaultaxeslinewidth"), tol);
%!   assert (get (hax, "position"), pos, tol); # axes position is unchanged
%!   assert (get (hax, "default"), struct ()); # no more axes' defaults
%!   assert (get (get (hax, "title"), "string"), "");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test  # root figure object
%! set (0, "defaultfigurevisible", "off");
%! hf = figure ("visible", "off", "paperunits", "centimeters",
%!              "papertype", "a4");
%! unwind_protect
%!   reset (hf);
%!   assert (get (hf, "papertype"), get (0, "defaultfigurepapertype"));
%!   assert (get (hf, "paperunits"), "centimeters"); # paperunits is unchanged
%!   assert (get (hf, "visible"), get (0, "defaultfigurevisible"));
%! unwind_protect_cleanup
%!   close (hf);
%!   set (0, "defaultfigurevisible", "remove");
%! end_unwind_protect

*/

DEFUN (set, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} set (@var{h}, @var{property}, @var{value}, @dots{})\n\
@deftypefnx {Built-in Function} {} set (@var{h}, @var{properties}, @var{values})\n\
@deftypefnx {Built-in Function} {} set (@var{h}, @var{pv})\n\
@deftypefnx {Built-in Function} {@var{value_list} =} set (@var{h}, @var{property})\n\
@deftypefnx {Built-in Function} {@var{all_value_list} =} set (@var{h})\n\
Set named property values for the graphics handle (or vector of graphics\n\
handles) @var{h}.\n\
\n\
There are three ways to give the property names and values:\n\
\n\
@itemize\n\
@item as a comma separated list of @var{property}, @var{value} pairs\n\
\n\
Here, each @var{property} is a string containing the property name, each\n\
@var{value} is a value of the appropriate type for the property.\n\
\n\
@item as a cell array of strings @var{properties} containing property names\n\
and a cell array @var{values} containing property values.\n\
\n\
In this case, the number of columns of @var{values} must match the number of\n\
elements in @var{properties}.  The first column of @var{values} contains\n\
values for the first entry in @var{properties}, etc.  The number of rows of\n\
@var{values} must be 1 or match the number of elements of @var{h}.  In the\n\
first case, each handle in @var{h} will be assigned the same values.  In the\n\
latter case, the first handle in @var{h} will be assigned the values from\n\
the first row of @var{values} and so on.\n\
\n\
@item as a structure array @var{pv}\n\
\n\
Here, the field names of @var{pv} represent the property names, and the field\n\
values give the property values.  In contrast to the previous case, all\n\
elements of @var{pv} will be set in all handles in @var{h} independent of\n\
the dimensions of @var{pv}.\n\
@end itemize\n\
\n\
@code{set} is also used to query the list of values a named property will\n\
take.  @code{@var{clist} = set (@var{h}, \"property\")} will return the list\n\
of possible values for @qcode{\"property\"} in the cell list @var{clist}.\n\
If no output variable is used then the list is formatted and printed to the\n\
screen.\n\
\n\
If no property is specified (@code{@var{slist} = set (@var{h})}) then a\n\
structure @var{slist} is returned where the fieldnames are the properties of\n\
the object @var{h} and the fields are the list of possible values for each\n\
property.  If no output variable is used then the list is formatted and\n\
printed to the screen.\n\
\n\
For example,\n\
\n\
@example\n\
@group\n\
hf = figure ();\n\
set (hf, \"paperorientation\")\n\
@result{}  paperorientation:  [ landscape | @{portrait@} | rotated ]\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
shows the paperorientation property can take three values with the default\n\
being @qcode{\"portrait\"}.\n\
@seealso{get}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      // get vector of graphics handles
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
          bool request_drawnow = false;

          // loop over graphics objects
          for (octave_idx_type n = 0; n < hcv.length (); n++)
            {
              graphics_object obj = gh_manager::get_object (hcv(n));

              if (obj)
                {
                  if (nargin == 3 && args(1).is_cellstr ()
                      && args(2).is_cell ())
                    {
                      if (args(2).cell_value ().rows () == 1)
                        {
                          obj.set (args(1).cellstr_value (),
                                   args(2).cell_value (), 0);
                        }
                      else if (hcv.length () == args(2).cell_value ().rows ())
                        {
                          obj.set (args(1).cellstr_value (),
                                   args(2).cell_value (), n);
                        }
                      else
                        {
                          error ("set: number of graphics handles must match number of value rows (%d != %d)",
                                 hcv.length (), args(2).cell_value ().rows ());
                          break;

                        }
                    }
                  else if (nargin == 2 && args(1).is_map ())
                    {
                      obj.set (args(1).map_value ());
                    }
                  else if (nargin == 2 && args(1).is_string ())
                    {
                      std::string property = args(1).string_value ();

                      octave_map pmap = obj.values_as_struct ();

                      if (obj.has_readonly_property (property))
                        if (nargout != 0)
                          retval = Matrix ();
                        else
                          octave_stdout << "set: " << property
                                        <<" is read-only" << std::endl;
                      else if (pmap.isfield (property))
                        {
                          if (nargout != 0)
                            retval = pmap.getfield (property)(0);
                          else
                            {
                              std::string s = obj.value_as_string (property);
                              if (! error_state)
                                octave_stdout << s;
                            }
                        }
                      else
                        {
                          error ("set: unknown property");
                          break;
                        }
                    }
                  else if (nargin == 1)
                    {
                      if (nargout != 0)
                        retval = obj.values_as_struct ();
                      else
                        {
                          std::string s = obj.values_as_string ();
                          if (! error_state)
                            octave_stdout << s;
                        }
                    }
                  else
                    {
                      obj.set (args.splice (0, 1));
                      request_drawnow = true;
                    }
                }
              else
                {
                  error ("set: invalid handle (= %g)", hcv(n));
                  break;
                }

              if (error_state)
                break;

              request_drawnow = true;
            }

          if (! error_state && request_drawnow)
            Vdrawnow_requested = true;
        }
      else
        error ("set: expecting graphics handle as first argument");
    }
  else
    print_usage ();

  return retval;
}

static std::string
get_graphics_object_type (double val)
{
  std::string retval;

  graphics_object obj = gh_manager::get_object (val);

  if (obj)
    retval = obj.type ();
  else
    error ("get: invalid handle (= %g)", val);

  return retval;
}

DEFUN (get, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} get (@var{h})\n\
@deftypefnx {Built-in Function} {@var{val} =} get (@var{h}, @var{p})\n\
Return the value of the named property @var{p} from the graphics handle\n\
@var{h}.\n\
\n\
If @var{p} is omitted, return the complete property list for @var{h}.\n\
\n\
If @var{h} is a vector, return a cell array including the property values or\n\
lists respectively.\n\
@seealso{set}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  Cell vals;

  int nargin = args.length ();

  bool use_cell_format = false;

  if (nargin == 1 || nargin == 2)
    {
      if (args(0).is_empty ())
        {
          retval = Matrix ();
          return retval;
        }

      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
          octave_idx_type len = hcv.length ();

          if (nargin == 1 && len > 1)
            {
              std::string t0 = get_graphics_object_type (hcv(0));

              if (! error_state)
                {
                  for (octave_idx_type n = 1; n < len; n++)
                    {
                      std::string t = get_graphics_object_type (hcv(n));

                      if (error_state)
                        break;

                      if (t != t0)
                        {
                          error ("get: vector of handles must all have same type");
                          break;
                        }
                    }

                }
            }

          if (! error_state)
            {
              if (nargin > 1 && args(1).is_cellstr ())
                {
                  Array<std::string> plist = args(1).cellstr_value ();

                  if (! error_state)
                    {
                      octave_idx_type plen = plist.numel ();

                      use_cell_format = true;

                      vals.resize (dim_vector (len, plen));

                      for (octave_idx_type n = 0; ! error_state && n < len; n++)
                        {
                          graphics_object obj = gh_manager::get_object (hcv(n));

                          if (obj)
                            {
                              for (octave_idx_type m = 0;
                                   ! error_state && m < plen;
                                   m++)
                                {
                                  caseless_str property = plist(m);

                                  vals(n, m) = obj.get (property);
                                }
                            }
                          else
                            {
                              error ("get: invalid handle (= %g)", hcv(n));
                              break;
                            }
                        }
                    }
                  else
                    error ("get: expecting property name or cell array of property names as second argument");
                }
              else
                {
                  caseless_str property;

                  if (nargin > 1)
                    {
                      property = args(1).string_value ();

                      if (error_state)
                        error ("get: expecting property name or cell array of property names as second argument");
                    }

                  vals.resize (dim_vector (len, 1));

                  if (! error_state)
                    {
                      for (octave_idx_type n = 0; ! error_state && n < len; n++)
                        {
                          graphics_object obj = gh_manager::get_object (hcv(n));

                          if (obj)
                            {
                              if (nargin == 1)
                                vals(n) = obj.get ();
                              else
                                vals(n) = obj.get (property);
                            }
                          else
                            {
                              error ("get: invalid handle (= %g)", hcv(n));
                              break;
                            }
                        }
                    }
                }
            }
        }
      else
        error ("get: expecting graphics handle as first argument");
    }
  else
    print_usage ();

  if (! error_state)
    {
      if (use_cell_format)
        retval = vals;
      else
        {
          octave_idx_type len = vals.numel ();

          if (len == 0)
            retval = Matrix ();
          else if (len == 1)
            retval = vals(0);
          else if (len > 1 && nargin == 1)
            {
              OCTAVE_LOCAL_BUFFER (octave_scalar_map, tmp, len);

              for (octave_idx_type n = 0; n < len; n++)
                tmp[n] = vals(n).scalar_map_value ();

              retval = octave_map::cat (0, len, tmp);
            }
          else
            retval = vals;
        }
    }

  return retval;
}

/*
%!assert (get (findobj (0, "Tag", "nonexistenttag"), "nonexistentproperty"), [])
*/

// Return all properties from the graphics handle @var{h}.
// If @var{h} is a vector, return a cell array including the
// property values or lists respectively.

DEFUN (__get__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __get__ (@var{h})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  Cell vals;

  int nargin = args.length ();

  if (nargin == 1)
    {
      ColumnVector hcv (args(0).vector_value ());

      if (! error_state)
        {
          octave_idx_type len = hcv.length ();

          vals.resize (dim_vector (len, 1));

          for (octave_idx_type n = 0; n < len; n++)
            {
              graphics_object obj = gh_manager::get_object (hcv(n));

              if (obj)
                vals(n) = obj.get (true);
              else
                {
                  error ("get: invalid handle (= %g)", hcv(n));
                  break;
                }
            }
        }
      else
        error ("get: expecting graphics handle as first argument");
    }
  else
    print_usage ();

  if (! error_state)
    {
      octave_idx_type len = vals.numel ();

      if (len > 1)
        retval = vals;
      else if (len == 1)
        retval = vals(0);
    }

  return retval;
}

static octave_value
make_graphics_object (const std::string& go_name,
                      bool integer_figure_handle,
                      const octave_value_list& args)
{
  octave_value retval;

  double val = octave_NaN;

  octave_value_list xargs = args.splice (0, 1);

  caseless_str p ("parent");

  for (int i = 0; i < xargs.length (); i++)
    if (xargs(i).is_string ()
        && p.compare (xargs(i).string_value ()))
      {
        if (i < (xargs.length () - 1))
          {
            val = xargs(i+1).double_value ();

            if (! error_state)
              {
                xargs = xargs.splice (i, 2);
                break;
              }
          }
        else
          error ("__go_%s__: missing value for parent property",
                 go_name.c_str ());
      }

  if (! error_state && xisnan (val))
    val = args(0).double_value ();

  if (! error_state)
    {
      graphics_handle parent = gh_manager::lookup (val);

      if (parent.ok ())
        {
          graphics_handle h
            = gh_manager::make_graphics_handle (go_name, parent,
                                                integer_figure_handle,
                                                false, false);

          if (! error_state)
            {
              adopt (parent, h);

              xset (h, xargs);
              xcreatefcn (h);
              xinitialize (h);

              retval = h.value ();

              if (! error_state)
                Vdrawnow_requested = true;
            }
          else
            error ("__go%s__: unable to create graphics handle",
                   go_name.c_str ());
        }
      else
        error ("__go_%s__: invalid parent", go_name.c_str ());
    }
  else
    error ("__go_%s__: invalid parent", go_name.c_str ());

  return retval;
}

DEFUN (__go_figure__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_figure__ (@var{fignum})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  if (args.length () > 0)
    {
      double val = args(0).double_value ();

      if (! error_state)
        {
          if (is_figure (val))
            {
              graphics_handle h = gh_manager::lookup (val);

              xset (h, args.splice (0, 1));

              retval = h.value ();
            }
          else
            {
              bool int_fig_handle = true;

              octave_value_list xargs = args.splice (0, 1);

              graphics_handle h = octave_NaN;

              if (xisnan (val))
                {
                  caseless_str p ("integerhandle");

                  for (int i = 0; i < xargs.length (); i++)
                    {
                      if (xargs(i).is_string ()
                          && p.compare (xargs(i).string_value ()))
                        {
                          if (i < (xargs.length () - 1))
                            {
                              std::string pval = xargs(i+1).string_value ();

                              if (! error_state)
                                {
                                  caseless_str on ("on");
                                  int_fig_handle = on.compare (pval);
                                  xargs = xargs.splice (i, 2);
                                  break;
                                }
                            }
                        }
                    }

                  h = gh_manager::make_graphics_handle ("figure", 0,
                                                        int_fig_handle,
                                                        false, false);

                  if (! int_fig_handle)
                    {
                      // We need to intiailize the integerhandle
                      // property without calling the set_integerhandle
                      // method, because doing that will generate a new
                      // handle value...

                      graphics_object go = gh_manager::get_object (h);
                      go.get_properties ().init_integerhandle ("off");
                    }
                }
              else if (val > 0 && D_NINT (val) == val)
                h = gh_manager::make_figure_handle (val, false);

              if (! error_state && h.ok ())
                {
                  adopt (0, h);

                  gh_manager::push_figure (h);

                  xset (h, xargs);
                  xcreatefcn (h);
                  xinitialize (h);

                  retval = h.value ();
                }
              else
                error ("__go_figure__: failed to create figure handle");
            }
        }
      else
        error ("__go_figure__: expecting figure number to be double value");
    }
  else
    print_usage ();

  return retval;
}

#define GO_BODY(TYPE) \
  gh_manager::auto_lock guard; \
 \
  octave_value retval; \
 \
  if (args.length () > 0) \
    retval = make_graphics_object (#TYPE, false, args);  \
  else \
    print_usage (); \
 \
  return retval

int
calc_dimensions (const graphics_object& go)
{

  int nd = 2;

  if (go.isa ("surface"))
    nd = 3;
  else if ((go.isa ("line") || go.isa ("patch"))
           && ! go.get ("zdata").is_empty ())
    nd = 3;
  else
    {
      Matrix kids = go.get_properties ().get_children ();

      for (octave_idx_type i = 0; i < kids.length (); i++)
        {
          graphics_handle hnd = gh_manager::lookup (kids(i));

          if (hnd.ok ())
            {
              const graphics_object& kid = gh_manager::get_object (hnd);

              if (kid.valid_object ())
                nd = calc_dimensions (kid);

              if (nd == 3)
                break;
            }
        }
    }

  return nd;
}

DEFUN (__calc_dimensions__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __calc_dimensions__ (@var{axes})\n\
Internal function.\n\
\n\
Determine the number of dimensions in a graphics object, either 2 or 3.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      double h = args(0).double_value ();
      if (! error_state)
        retval = calc_dimensions (gh_manager::get_object (h));
      else
        error ("__calc_dimensions__: expecting graphics handle as only argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__go_axes__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_axes__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (axes);
}

DEFUN (__go_line__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_line__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (line);
}

DEFUN (__go_text__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_text__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (text);
}

DEFUN (__go_image__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_image__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (image);
}

DEFUN (__go_surface__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_surface__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (surface);
}

DEFUN (__go_patch__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_patch__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (patch);
}

DEFUN (__go_hggroup__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_hggroup__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (hggroup);
}

DEFUN (__go_uimenu__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uimenu__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uimenu);
}

DEFUN (__go_uicontrol__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uicontrol__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uicontrol);
}

DEFUN (__go_uipanel__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uipanel__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uipanel);
}

DEFUN (__go_uicontextmenu__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uicontextmenu__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uicontextmenu);
}

DEFUN (__go_uitoolbar__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uitoolbar__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uitoolbar);
}

DEFUN (__go_uipushtool__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uipushtool__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uipushtool);
}

DEFUN (__go_uitoggletool__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_uitoggletool__ (@var{parent})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  GO_BODY (uitoggletool);
}

DEFUN (__go_delete__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_delete__ (@var{h})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value_list retval;

  if (args.length () == 1)
    {
      graphics_handle h = octave_NaN;

      const NDArray vals = args(0).array_value ();

      if (! error_state)
        {
          // Check is all the handles to delete are valid first
          // as callbacks might delete one of the handles we
          // later want to delete
          for (octave_idx_type i = 0; i < vals.numel (); i++)
            {
              h = gh_manager::lookup (vals.elem (i));

              if (! h.ok ())
                {
                  error ("delete: invalid graphics object (= %g)",
                         vals.elem (i));
                  break;
                }
            }

          if (! error_state)
            delete_graphics_objects (vals);
        }
      else
        error ("delete: invalid graphics object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__go_axes_init__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_axes_init__ (@var{h}, @var{mode})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  int nargin = args.length ();

  std::string mode = "";

  if (nargin == 2)
    {
      mode = args(1).string_value ();

      if (error_state)
        return retval;
    }

  if (nargin == 1 || nargin == 2)
    {
      graphics_handle h = octave_NaN;

      double val = args(0).double_value ();

      if (! error_state)
        {
          h = gh_manager::lookup (val);

          if (h.ok ())
            {
              graphics_object obj = gh_manager::get_object (h);

              obj.set_defaults (mode);

              h = gh_manager::lookup (val);
              if (! h.ok ())
                error ("__go_axes_init__: axis deleted during initialization (= %g)",
                       val);
            }
          else
            error ("__go_axes_init__: invalid graphics object (= %g)", val);
        }
      else
        error ("__go_axes_init__: invalid graphics object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__go_handles__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_handles__ (@var{show_hidden})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  bool show_hidden = false;

  if (args.length () > 0)
    show_hidden = args(0).bool_value ();

  return octave_value (gh_manager::handle_list (show_hidden));
}

DEFUN (__go_figure_handles__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __go_figure_handles__ (@var{show_hidden})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  bool show_hidden = false;

  if (args.length () > 0)
    show_hidden = args(0).bool_value ();

  return octave_value (gh_manager::figure_handle_list (show_hidden));
}

DEFUN (__go_execute_callback__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} __go_execute_callback__ (@var{h}, @var{name})\n\
@deftypefnx {Built-in Function} {} __go_execute_callback__ (@var{h}, @var{name}, @var{param})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 3)
    {
      double val = args(0).double_value ();

      if (! error_state)
        {
          graphics_handle h = gh_manager::lookup (val);

          if (h.ok ())
            {
              std::string name = args(1).string_value ();

              if (! error_state)
                {
                  if (nargin == 2)
                    gh_manager::execute_callback (h, name);
                  else
                    gh_manager::execute_callback (h, name, args(2));
                }
              else
                error ("__go_execute_callback__: invalid callback name");
            }
          else
            error ("__go_execute_callback__: invalid graphics object (= %g)",
                   val);
        }
      else
        error ("__go_execute_callback__: invalid graphics object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (__image_pixel_size__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{px}, @var{py}} __image_pixel_size__ (@var{h})\n\
Internal function: returns the pixel size of the image in normalized units.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      double h = args(0).double_value ();

      if (! error_state)
        {
          graphics_object fobj = gh_manager::get_object (h);
          if (fobj &&  fobj.isa ("image"))
            {
              image::properties& ip =
                dynamic_cast<image::properties&> (fobj.get_properties ());

              Matrix dp =  Matrix (1, 2, 0);
              dp(0, 0) = ip.pixel_xsize ();
              dp(0, 1) = ip.pixel_ysize ();
              retval = dp;
            }
          else
            error ("__image_pixel_size__: object is not an image");
        }
      else
        error ("__image_pixel_size__: argument is not a handle");
    }
  else
    print_usage ();

  return retval;
}

gtk_manager *gtk_manager::instance = 0;

void
gtk_manager::create_instance (void)
{
  instance = new gtk_manager ();

  if (instance)
    singleton_cleanup_list::add (cleanup_instance);
}

graphics_toolkit
gtk_manager::do_get_toolkit (void) const
{
  graphics_toolkit retval;

  if (! dtk.empty ())
    {
      const_loaded_toolkits_iterator pl = loaded_toolkits.find (dtk);

      if (pl == loaded_toolkits.end ())
        {
          const_available_toolkits_iterator pa = available_toolkits.find (dtk);

          if (pa != available_toolkits.end ())
            {
              octave_value_list args;
              args(0) = dtk;
              feval ("graphics_toolkit", args);

              if (! error_state)
                pl = loaded_toolkits.find (dtk);

              if (error_state || pl == loaded_toolkits.end ())
                error ("failed to load %s graphics toolkit", dtk.c_str ());
              else
                retval = pl->second;
            }
          else
            error ("default graphics toolkit '%s' is not available!",
                   dtk.c_str ());
        }
      else
        retval = pl->second;
    }
  else
    error ("no graphics toolkits are available!");

  return retval;
}

void
gtk_manager::do_register_toolkit (const std::string& name)
{
  if (dtk.empty () || name == "qt"
      || (name == "fltk"
          && available_toolkits.find ("qt") == available_toolkits.end ()))
    dtk = name;

  available_toolkits.insert (name);
}

void
gtk_manager::do_unregister_toolkit (const std::string& name)
{
  available_toolkits.erase (name);

  if (dtk == name)
    {
      if (available_toolkits.empty ())
        dtk.clear ();
      else
        {
          const_available_toolkits_iterator pa = available_toolkits.begin ();

          dtk = *pa++;

          while (pa != available_toolkits.end ())
            {
              std::string tk_name = *pa++;

              if (tk_name == "qt"
                  || (tk_name == "fltk"
                      && (available_toolkits.find ("qt")
                          == available_toolkits.end ())))
                dtk = tk_name;
            }
        }
    }
}

DEFUN (available_graphics_toolkits, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} available_graphics_toolkits ()\n\
Return a cell array of registered graphics toolkits.\n\
@seealso{graphics_toolkit, register_graphics_toolkit}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  return octave_value (gtk_manager::available_toolkits_list ());
}

DEFUN (register_graphics_toolkit, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} register_graphics_toolkit (@var{toolkit})\n\
List @var{toolkit} as an available graphics toolkit.\n\
@seealso{available_graphics_toolkits}\n\
@end deftypefn")
{
  octave_value retval;

  gh_manager::auto_lock guard;

  if (args.length () == 1)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();
          gtk_manager::register_toolkit (name);
        }
      else
        error ("register_graphics_toolkit: TOOLKIT must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (loaded_graphics_toolkits, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} loaded_graphics_toolkits ()\n\
Return a cell array of the currently loaded graphics toolkits.\n\
@seealso{available_graphics_toolkits}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  return octave_value (gtk_manager::loaded_toolkits_list ());
}

DEFUN (drawnow, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} drawnow ()\n\
@deftypefnx {Built-in Function} {} drawnow (\"expose\")\n\
@deftypefnx {Built-in Function} {} drawnow (@var{term}, @var{file}, @var{mono}, @var{debug_file})\n\
Update figure windows and their children.\n\
\n\
The event queue is flushed and any callbacks generated are executed.\n\
\n\
With the optional argument @qcode{\"expose\"}, only graphic objects are\n\
updated and no other events or callbacks are processed.\n\
\n\
The third calling form of @code{drawnow} is for debugging and is\n\
undocumented.\n\
@seealso{refresh}\n\
@end deftypefn")
{
  static int drawnow_executing = 0;

  octave_value retval;

  gh_manager::lock ();

  unwind_protect frame;
  frame.protect_var (Vdrawnow_requested, false);

  frame.protect_var (drawnow_executing);

  if (++drawnow_executing <= 1)
    {
      if (args.length () == 0 || args.length () == 1)
        {
          Matrix hlist = gh_manager::figure_handle_list (true);

          for (int i = 0; ! error_state && i < hlist.length (); i++)
            {
              graphics_handle h = gh_manager::lookup (hlist(i));

              if (h.ok () && h != 0)
                {
                  graphics_object go = gh_manager::get_object (h);
                  figure::properties& fprops
                    = dynamic_cast <figure::properties&> (go.get_properties ());

                  if (fprops.is_modified ())
                    {
                      if (fprops.is_visible ())
                        {
                          gh_manager::unlock ();

                          fprops.get_toolkit ().redraw_figure (go);

                          gh_manager::lock ();
                        }

                      fprops.set_modified (false);
                    }
                }
            }

          bool do_events = true;

          if (args.length () == 1)
            {
              caseless_str val (args(0).string_value ());

              if (! error_state && val.compare ("expose"))
                do_events = false;
              else
                {
                  error ("drawnow: invalid argument, expected 'expose' as argument");

                  gh_manager::unlock ();

                  return retval;
                }
            }

          if (do_events)
            {
              gh_manager::unlock ();

              gh_manager::process_events ();

              gh_manager::lock ();
            }
        }
      else if (args.length () >= 2 && args.length () <= 4)
        {
          std::string term, file, debug_file;
          bool mono;

          term = args(0).string_value ();

          if (! error_state)
            {
              file = args(1).string_value ();

              if (! error_state)
                {
                  size_t pos_p = file.find_first_of ("|");
                  size_t pos_c = file.find_first_not_of ("| ");

                  if (pos_p == std::string::npos &&
                      pos_c == std::string::npos)
                    {
                      error ("drawnow: empty output ''");

                      gh_manager::unlock ();

                      return retval;
                    }
                  else if (pos_c == std::string::npos)
                    {
                      error ("drawnow: empty pipe '|'");

                      gh_manager::unlock ();

                      return retval;
                    }
                  else if (pos_p != std::string::npos && pos_p < pos_c)
                    {
                      // Strip leading pipe character
                      file = file.substr (pos_c);
                    }
                  else
                    {
                      size_t pos = file.find_last_of (file_ops::dir_sep_chars ());

                      if (pos != std::string::npos)
                        {
                          std::string dirname = file.substr (pos_c, pos+1);

                          file_stat fs (dirname);

                          if (! (fs && fs.is_dir ()))
                            {
                              error ("drawnow: nonexistent directory '%s'",
                                     dirname.c_str ());

                              gh_manager::unlock ();

                              return retval;
                            }
                        }
                    }

                  mono = (args.length () >= 3 ? args(2).bool_value () : false);

                  if (! error_state)
                    {
                      debug_file = (args.length () > 3 ? args(3).string_value ()
                                    : "");

                      if (! error_state)
                        {
                          graphics_handle h = gcf ();

                          if (h.ok ())
                            {
                              graphics_object go = gh_manager::get_object (h);

                              // FIXME: when using qt toolkit the print_figure
                              // method returns immediately and Canvas::print 
                              // doesn't have enough time to lock the mutex 
                              // before we lock it here again. We thus wait 
                              // 50 ms (which may not be enough) to give it 
                              // a chance: see
                              // http://octave.1599824.n4.nabble.com/Printing-issues-with-Qt-toolkit-tp4673270.html
                              gh_manager::unlock ();

                              go.get_toolkit ().print_figure (go, term, file,
                                                              mono, debug_file);
                              Fsleep (octave_value (0.05));
                              gh_manager::lock ();
                            }
                          else
                            error ("drawnow: nothing to draw");
                        }
                      else
                        error ("drawnow: invalid DEBUG_FILE, expected a string value");
                    }
                  else
                    error ("drawnow: invalid colormode MONO, expected a boolean value");
                }
              else
                error ("drawnow: invalid FILE, expected a string value");
            }
          else
            error ("drawnow: invalid terminal TERM, expected a string value");
        }
      else
        print_usage ();
    }

  gh_manager::unlock ();

  return retval;
}

DEFUN (addlistener, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} addlistener (@var{h}, @var{prop}, @var{fcn})\n\
Register @var{fcn} as listener for the property @var{prop} of the graphics\n\
object @var{h}.\n\
\n\
Property listeners are executed (in order of registration) when the property\n\
is set.  The new value is already available when the listeners are executed.\n\
\n\
@var{prop} must be a string naming a valid property in @var{h}.\n\
\n\
@var{fcn} can be a function handle, a string or a cell array whose first\n\
element is a function handle.  If @var{fcn} is a function handle, the\n\
corresponding function should accept at least 2 arguments, that will be\n\
set to the object handle and the empty matrix respectively.  If @var{fcn}\n\
is a string, it must be any valid octave expression.  If @var{fcn} is a cell\n\
array, the first element must be a function handle with the same signature\n\
as described above.  The next elements of the cell array are passed\n\
as additional arguments to the function.\n\
\n\
Example:\n\
\n\
@example\n\
@group\n\
function my_listener (h, dummy, p1)\n\
  fprintf (\"my_listener called with p1=%s\\n\", p1);\n\
endfunction\n\
\n\
addlistener (gcf, \"position\", @{@@my_listener, \"my string\"@})\n\
@end group\n\
@end example\n\
\n\
@seealso{addproperty, hggroup}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  if (args.length () >= 3 && args.length () <= 4)
    {
      double h = args(0).double_value ();

      if (! error_state)
        {
          std::string pname = args(1).string_value ();

          if (! error_state)
            {
              graphics_handle gh = gh_manager::lookup (h);

              if (gh.ok ())
                {
                  graphics_object go = gh_manager::get_object (gh);

                  go.add_property_listener (pname, args(2), POSTSET);

                  if (args.length () == 4)
                    {
                      caseless_str persistent = args(3).string_value ();
                      if (persistent.compare ("persistent"))
                        go.add_property_listener (pname, args(2), PERSISTENT);
                    }
                }
              else
                error ("addlistener: invalid graphics object (= %g)",
                       h);
            }
          else
            error ("addlistener: invalid property name, expected a string value");
        }
      else
        error ("addlistener: invalid handle");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (dellistener, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} dellistener (@var{h}, @var{prop}, @var{fcn})\n\
Remove the registration of @var{fcn} as a listener for the property\n\
@var{prop} of the graphics object @var{h}.\n\
\n\
The function @var{fcn} must be the same variable (not just the same value),\n\
as was passed to the original call to @code{addlistener}.\n\
\n\
If @var{fcn} is not defined then all listener functions of @var{prop}\n\
are removed.\n\
\n\
Example:\n\
\n\
@example\n\
@group\n\
function my_listener (h, dummy, p1)\n\
  fprintf (\"my_listener called with p1=%s\\n\", p1);\n\
endfunction\n\
\n\
c = @{@@my_listener, \"my string\"@};\n\
addlistener (gcf, \"position\", c);\n\
dellistener (gcf, \"position\", c);\n\
@end group\n\
@end example\n\
\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  if (args.length () == 3 || args.length () == 2)
    {
      double h = args(0).double_value ();

      if (! error_state)
        {
          std::string pname = args(1).string_value ();

          if (! error_state)
            {
              graphics_handle gh = gh_manager::lookup (h);

              if (gh.ok ())
                {
                  graphics_object go = gh_manager::get_object (gh);

                  if (args.length () == 2)
                    go.delete_property_listener (pname, octave_value (),
                                                 POSTSET);
                  else
                    {
                      if (args(2).is_string ()
                          && args(2).string_value () == "persistent")
                        {
                          go.delete_property_listener (pname, octave_value (),
                                                       PERSISTENT);
                          go.delete_property_listener (pname, octave_value (),
                                                       POSTSET);
                        }
                      else
                        go.delete_property_listener (pname, args(2), POSTSET);
                    }
                }
              else
                error ("dellistener: invalid graphics object (= %g)",
                       h);
            }
          else
            error ("dellistener: invalid property name, expected a string value");
        }
      else
        error ("dellistener: invalid handle");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (addproperty, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} addproperty (@var{name}, @var{h}, @var{type})\n\
@deftypefnx {Built-in Function} {} addproperty (@var{name}, @var{h}, @var{type}, @var{arg}, @dots{})\n\
Create a new property named @var{name} in graphics object @var{h}.\n\
\n\
@var{type} determines the type of the property to create.  @var{args}\n\
usually contains the default value of the property, but additional\n\
arguments might be given, depending on the type of the property.\n\
\n\
The supported property types are:\n\
\n\
@table @code\n\
@item string\n\
A string property.  @var{arg} contains the default string value.\n\
\n\
@item any\n\
An @nospell{un-typed} property.  This kind of property can hold any octave\n\
value.  @var{args} contains the default value.\n\
\n\
@item radio\n\
A string property with a limited set of accepted values.  The first\n\
argument must be a string with all accepted values separated by\n\
a vertical bar ('|').  The default value can be marked by enclosing\n\
it with a '@{' '@}' pair.  The default value may also be given as\n\
an optional second string argument.\n\
\n\
@item boolean\n\
A boolean property.  This property type is equivalent to a radio\n\
property with \"on|off\" as accepted values.  @var{arg} contains\n\
the default property value.\n\
\n\
@item double\n\
A scalar double property.  @var{arg} contains the default value.\n\
\n\
@item handle\n\
A handle property.  This kind of property holds the handle of a\n\
graphics object.  @var{arg} contains the default handle value.\n\
When no default value is given, the property is initialized to\n\
the empty matrix.\n\
\n\
@item data\n\
A data (matrix) property.  @var{arg} contains the default data\n\
value.  When no default value is given, the data is initialized to\n\
the empty matrix.\n\
\n\
@item color\n\
A color property.  @var{arg} contains the default color value.\n\
When no default color is given, the property is set to black.\n\
An optional second string argument may be given to specify an\n\
additional set of accepted string values (like a radio property).\n\
@end table\n\
\n\
@var{type} may also be the concatenation of a core object type and\n\
a valid property name for that object type.  The property created\n\
then has the same characteristics as the referenced property (type,\n\
possible values, hidden state@dots{}).  This allows one to clone an\n\
existing property into the graphics object @var{h}.\n\
\n\
Examples:\n\
\n\
@example\n\
@group\n\
addproperty (\"my_property\", gcf, \"string\", \"a string value\");\n\
addproperty (\"my_radio\", gcf, \"radio\", \"val_1|val_2|@{val_3@}\");\n\
addproperty (\"my_style\", gcf, \"linelinestyle\", \"--\");\n\
@end group\n\
@end example\n\
\n\
@seealso{addlistener, hggroup}\n\
@end deftypefn")
{
  gh_manager::auto_lock guard;

  octave_value retval;

  if (args.length () >= 3)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        {
          double h = args(1).double_value ();

          if (! error_state)
            {
              graphics_handle gh = gh_manager::lookup (h);

              if (gh.ok ())
                {
                  graphics_object go = gh_manager::get_object (gh);

                  std::string type = args(2).string_value ();

                  if (! error_state)
                    {
                      if (! go.get_properties ().has_property (name))
                        {
                          property p = property::create (name, gh, type,
                                                         args.splice (0, 3));

                          if (! error_state)
                            go.get_properties ().insert_property (name, p);
                        }
                      else
                        error ("addproperty: a '%s' property already exists in the graphics object",
                               name.c_str ());
                    }
                  else
                    error ("addproperty: invalid property TYPE, expected a string value");
                }
              else
                error ("addproperty: invalid graphics object (= %g)", h);
            }
          else
            error ("addproperty: invalid handle value");
        }
      else
        error ("addproperty: invalid property NAME, expected a string value");
    }
  else
    print_usage ();

  return retval;
}

octave_value
get_property_from_handle (double handle, const std::string& property,
                          const std::string& func)
{
  gh_manager::auto_lock guard;

  graphics_object obj = gh_manager::get_object (handle);
  octave_value retval;

  if (obj)
    retval = obj.get (caseless_str (property));
  else
    error ("%s: invalid handle (= %g)", func.c_str (), handle);

  return retval;
}

bool
set_property_in_handle (double handle, const std::string& property,
                        const octave_value& arg, const std::string& func)
{
  gh_manager::auto_lock guard;

  graphics_object obj = gh_manager::get_object (handle);
  int ret = false;

  if (obj)
    {
      obj.set (caseless_str (property), arg);

      if (! error_state)
        ret = true;
    }
  else
    error ("%s: invalid handle (= %g)", func.c_str (), handle);

  return ret;
}

static bool
compare_property_values (const octave_value& o1, const octave_value& o2)
{
  octave_value_list args(2);

  args(0) = o1;
  args(1) = o2;

  octave_value_list result = feval ("isequal", args, 1);

  if (! error_state && result.length () > 0)
    return result(0).bool_value ();

  return false;
}

static std::map<uint32_t, bool> waitfor_results;

static void
cleanup_waitfor_id (uint32_t id)
{
  waitfor_results.erase (id);
}

static void
do_cleanup_waitfor_listener (const octave_value& listener,
                             listener_mode mode = POSTSET)
{
  Cell c = listener.cell_value ();

  if (c.numel () >= 4)
    {
      double h = c(2).double_value ();

      if (! error_state)
        {
          caseless_str pname = c(3).string_value ();

          if (! error_state)
            {
              gh_manager::auto_lock guard;

              graphics_handle handle = gh_manager::lookup (h);

              if (handle.ok ())
                {
                  graphics_object go = gh_manager::get_object (handle);

                  if (go.get_properties ().has_property (pname))
                    {
                      go.get_properties ().delete_listener (pname, listener,
                                                            mode);
                      if (mode == POSTSET)
                        go.get_properties ().delete_listener (pname, listener,
                                                              PERSISTENT);
                    }
                }
            }
        }
    }
}

static void
cleanup_waitfor_postset_listener (const octave_value& listener)
{ do_cleanup_waitfor_listener (listener, POSTSET); }

static void
cleanup_waitfor_predelete_listener (const octave_value& listener)
{ do_cleanup_waitfor_listener (listener, PREDELETE); }

static octave_value_list
waitfor_listener (const octave_value_list& args, int)
{
  if (args.length () > 3)
    {
      uint32_t id = args(2).uint32_scalar_value ().value ();

      if (! error_state)
        {
          if (args.length () > 5)
            {
              double h = args(0).double_value ();

              if (! error_state)
                {
                  caseless_str pname = args(4).string_value ();

                  if (! error_state)
                    {
                      gh_manager::auto_lock guard;

                      graphics_handle handle = gh_manager::lookup (h);

                      if (handle.ok ())
                        {
                          graphics_object go = gh_manager::get_object (handle);
                          octave_value pvalue = go.get (pname);

                          if (compare_property_values (pvalue, args(5)))
                            waitfor_results[id] = true;
                        }
                    }
                }
            }
          else
            waitfor_results[id] = true;
        }
    }

  return octave_value_list ();
}

static octave_value_list
waitfor_del_listener (const octave_value_list& args, int)
{
  if (args.length () > 2)
    {
      uint32_t id = args(2).uint32_scalar_value ().value ();

      if (! error_state)
        waitfor_results[id] = true;
    }

  return octave_value_list ();
}

DEFUN (waitfor, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} waitfor (@var{h})\n\
@deftypefnx {Built-in Function} {} waitfor (@var{h}, @var{prop})\n\
@deftypefnx {Built-in Function} {} waitfor (@var{h}, @var{prop}, @var{value})\n\
@deftypefnx {Built-in Function} {} waitfor (@dots{}, \"timeout\", @var{timeout})\n\
Suspend the execution of the current program until a condition is\n\
satisfied on the graphics handle @var{h}.\n\
\n\
While the program is suspended graphics events are still processed normally,\n\
allowing callbacks to modify the state of graphics objects.  This function\n\
is reentrant and can be called from a callback, while another @code{waitfor}\n\
call is pending at the top-level.\n\
\n\
In the first form, program execution is suspended until the graphics object\n\
@var{h} is destroyed.  If the graphics handle is invalid, the function\n\
returns immediately.\n\
\n\
In the second form, execution is suspended until the graphics object is\n\
destroyed or the property named @var{prop} is modified.  If the graphics\n\
handle is invalid or the property does not exist, the function returns\n\
immediately.\n\
\n\
In the third form, execution is suspended until the graphics object is\n\
destroyed or the property named @var{prop} is set to @var{value}.  The\n\
function @code{isequal} is used to compare property values.  If the graphics\n\
handle is invalid, the property does not exist or the property is already\n\
set to @var{value}, the function returns immediately.\n\
\n\
An optional timeout can be specified using the property @code{timeout}.\n\
This timeout value is the number of seconds to wait for the condition to be\n\
true.  @var{timeout} must be at least 1. If a smaller value is specified, a\n\
warning is issued and a value of 1 is used instead.  If the timeout value is\n\
not an integer, it is truncated towards 0.\n\
\n\
To define a condition on a property named @code{timeout}, use the string\n\
@code{\\timeout} instead.\n\
\n\
In all cases, typing CTRL-C stops program execution immediately.\n\
@seealso{waitforbuttonpress, isequal}\n\
@end deftypefn")
{
  if (args.length () > 0)
    {
      double h = args(0).double_value ();

      if (! error_state)
        {
          caseless_str pname;

          unwind_protect frame;

          static uint32_t id_counter = 0;
          uint32_t id = 0;

          int max_arg_index = 0;
          int timeout_index = -1;

          int timeout = 0;

          if (args.length () > 1)
            {
              pname = args(1).string_value ();
              if (! error_state
                  && ! pname.empty ()
                  && ! pname.compare ("timeout"))
                {
                  if (pname.compare ("\\timeout"))
                    pname = "timeout";

                  static octave_value wf_listener;

                  if (! wf_listener.is_defined ())
                    wf_listener =
                      octave_value (new octave_builtin (waitfor_listener,
                                                        "waitfor_listener"));

                  max_arg_index++;
                  if (args.length () > 2)
                    {
                      if (args(2).is_string ())
                        {
                          caseless_str s = args(2).string_value ();

                          if (s.compare ("timeout"))
                            timeout_index = 2;
                          else
                            max_arg_index++;
                        }
                      else
                        max_arg_index++;
                    }

                  Cell listener (1, max_arg_index >= 2 ? 5 : 4);

                  id = id_counter++;
                  frame.add_fcn (cleanup_waitfor_id, id);
                  waitfor_results[id] = false;

                  listener(0) = wf_listener;
                  listener(1) = octave_uint32 (id);
                  listener(2) = h;
                  listener(3) = pname;

                  if (max_arg_index >= 2)
                    listener(4) = args(2);

                  octave_value ov_listener (listener);

                  gh_manager::auto_lock guard;

                  graphics_handle handle = gh_manager::lookup (h);

                  if (handle.ok ())
                    {
                      graphics_object go = gh_manager::get_object (handle);

                      if (max_arg_index >= 2
                          && compare_property_values (go.get (pname),
                                                      args(2)))
                        waitfor_results[id] = true;
                      else
                        {

                          frame.add_fcn (cleanup_waitfor_postset_listener,
                                         ov_listener);
                          go.add_property_listener (pname, ov_listener,
                                                    POSTSET);
                          go.add_property_listener (pname, ov_listener,
                                                    PERSISTENT);

                          if (go.get_properties ()
                              .has_dynamic_property (pname))
                            {
                              static octave_value wf_del_listener;

                              if (! wf_del_listener.is_defined ())
                                wf_del_listener =
                                  octave_value (new octave_builtin
                                                (waitfor_del_listener,
                                                 "waitfor_del_listener"));

                              Cell del_listener (1, 4);

                              del_listener(0) = wf_del_listener;
                              del_listener(1) = octave_uint32 (id);
                              del_listener(2) = h;
                              del_listener(3) = pname;

                              octave_value ov_del_listener (del_listener);

                              frame.add_fcn (cleanup_waitfor_predelete_listener,
                                             ov_del_listener);
                              go.add_property_listener (pname, ov_del_listener,
                                                        PREDELETE);
                            }
                        }
                    }
                }
              else if (error_state || pname.empty ())
                error ("waitfor: invalid property name, expected a non-empty string value");
            }

          if (! error_state
              && timeout_index < 0
              && args.length () > (max_arg_index + 1))
            {
              caseless_str s = args(max_arg_index + 1).string_value ();

              if (! error_state)
                {
                  if (s.compare ("timeout"))
                    timeout_index = max_arg_index + 1;
                  else
                    error ("waitfor: invalid parameter '%s'", s.c_str ());
                }
              else
                error ("waitfor: invalid parameter, expected 'timeout'");
            }

          if (! error_state && timeout_index >= 0)
            {
              if (args.length () > (timeout_index + 1))
                {
                  timeout = static_cast<int>
                            (args(timeout_index + 1).scalar_value ());

                  if (! error_state)
                    {
                      if (timeout < 1)
                        {
                          warning ("waitfor: the timeout value must be >= 1, using 1 instead");
                          timeout = 1;
                        }
                    }
                  else
                    error ("waitfor: invalid timeout value, expected a value >= 1");
                }
              else
                error ("waitfor: missing timeout value");
            }

          // FIXME: There is still a "hole" in the following loop. The code
          //        assumes that an object handle is unique, which is a fair
          //        assumption, except for figures. If a figure is destroyed
          //        then recreated with the same figure ID, within the same
          //        run of event hooks, then the figure destruction won't be
          //        caught and the loop will not stop. This is an unlikely
          //        possibility in practice, though.
          //
          //        Using deletefcn callback is also unreliable as it could be
          //        modified during a callback execution and the waitfor loop
          //        would not stop.
          //
          //        The only "good" implementation would require object
          //        listeners, similar to property listeners.

          time_t start = 0;

          if (timeout > 0)
            start = time (0);

          while (! error_state)
            {
              if (true)
                {
                  gh_manager::auto_lock guard;

                  graphics_handle handle = gh_manager::lookup (h);

                  if (handle.ok ())
                    {
                      if (! pname.empty () && waitfor_results[id])
                        break;
                    }
                  else
                    break;
                }

              octave_usleep (100000);

              OCTAVE_QUIT;

              command_editor::run_event_hooks ();

              if (timeout > 0)
                {
                  if (start + timeout < time (0))
                    break;
                }
            }
        }
      else
        error ("waitfor: invalid handle value.");
    }
  else
    print_usage ();

  return octave_value ();
}

DEFUN (__zoom__, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} __zoom__ (@var{axes}, @var{mode}, @var{factor})\n\
@deftypefnx {Built-in Function} {} __zoom__ (@var{axes}, \"out\")\n\
@deftypefnx {Built-in Function} {} __zoom__ (@var{axes}, \"reset\")\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 2 && nargin != 3)
    {
      print_usage ();
      return retval;
    }

  double h = args(0).double_value ();

  if (error_state)
    return retval;

  gh_manager::auto_lock guard;

  graphics_handle handle = gh_manager::lookup (h);

  if (! handle.ok ())
    {
      error ("__zoom__: invalid handle");
      return retval;
    }

  graphics_object ax = gh_manager::get_object (handle);

  axes::properties& ax_props =
    dynamic_cast<axes::properties&> (ax.get_properties ());

  if (nargin == 2)
    {
      std::string opt = args(1).string_value ();

      if (error_state)
        return retval;

      if (opt == "out" || opt == "reset")
        {
          if (opt == "out")
            {
              ax_props.clear_zoom_stack ();
              Vdrawnow_requested = true;
            }
          else
            ax_props.clear_zoom_stack (false);

        }
    }
  else
    {
      std::string mode = args(1).string_value ();
      double factor = args(2).scalar_value ();

      if (error_state)
        return retval;

      ax_props.zoom (mode, factor);
      Vdrawnow_requested = true;
    }

  return retval;
}
