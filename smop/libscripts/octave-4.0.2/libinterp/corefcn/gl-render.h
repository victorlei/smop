/*

Copyright (C) 2008-2015 Michael Goffioul

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

#if !defined (octave_gl_render_h)
#define octave_gl_render_h 1

#ifdef HAVE_WINDOWS_H
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef HAVE_GL_GL_H
#include <GL/gl.h>
#elif defined HAVE_OPENGL_GL_H || defined HAVE_FRAMEWORK_OPENGL
#include <OpenGL/gl.h>
#endif

#ifdef HAVE_GL_GLU_H
#include <GL/glu.h>
#elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
#include <OpenGL/glu.h>
#endif

#ifdef HAVE_GL_GLEXT_H
#include <GL/glext.h>
#elif defined HAVE_OPENGL_GLEXT_H || defined HAVE_FRAMEWORK_OPENGL
#include <OpenGL/glext.h>
#endif

#include "graphics.h"
#include "txt-eng-ft.h"

#if defined (HAVE_OPENGL)

class
OCTINTERP_API
opengl_renderer
{
public:
  opengl_renderer (void)
    : toolkit (), xform (), xmin (), xmax (), ymin (), ymax (),
    zmin (), zmax (), xZ1 (), xZ2 (), marker_id (), filled_marker_id (),
    camera_pos (), camera_dir ()
#if HAVE_FREETYPE
    , text_renderer ()
#endif
  { }

  virtual ~opengl_renderer (void) { }

  virtual void draw (const graphics_object& go, bool toplevel = true);

  virtual void draw (const Matrix& hlist, bool toplevel = false)
  {
    int len = hlist.length ();

    for (int i = len-1; i >= 0; i--)
      {
        graphics_object obj = gh_manager::get_object (hlist(i));

        if (obj)
          draw (obj, toplevel);
      }
  }

  virtual void set_viewport (int w, int h);
  virtual graphics_xform get_transform (void) const { return xform; }

protected:
  virtual void draw_figure (const figure::properties& props);
  virtual void draw_axes (const axes::properties& props);
  virtual void draw_line (const line::properties& props);
  virtual void draw_surface (const surface::properties& props);
  virtual void draw_patch (const patch::properties& props);
  virtual void draw_hggroup (const hggroup::properties& props);
  virtual void draw_text (const text::properties& props);
  virtual void draw_image (const image::properties& props);
  virtual void draw_uipanel (const uipanel::properties& props,
                             const graphics_object& go);

  virtual void init_gl_context (bool enhanced, const Matrix& backgroundColor);
  virtual void setup_opengl_transformation (const axes::properties& props);

  virtual void set_color (const Matrix& c);
  virtual void set_polygon_offset (bool on, float offset = 0.0f);
  virtual void set_linewidth (float w);
  virtual void set_linestyle (const std::string& s, bool stipple = false);
  virtual void set_clipbox (double x1, double x2, double y1, double y2,
                            double z1, double z2);
  virtual void set_clipping (bool on);
  virtual void set_font (const base_properties& props);

  virtual void init_marker (const std::string& m, double size, float width);
  virtual void end_marker (void);
  virtual void draw_marker (double x, double y, double z,
                            const Matrix& lc, const Matrix& fc);

  virtual void text_to_pixels (const std::string& txt,
                               uint8NDArray& pixels,
                               Matrix& bbox,
                               int halign = 0, int valign = 0,
                               double rotation = 0.0);

  virtual Matrix render_text (const std::string& txt,
                              double x, double y, double z,
                              int halign, int valign, double rotation = 0.0);

  virtual void draw_pixels (GLsizei w, GLsizei h, GLenum format,
                            GLenum type, const GLvoid *data);

  virtual void render_grid (const std::string& gridstyle, const Matrix& ticks,
                            double lim1, double lim2,
                            double p1, double p1N, double p2, double p2N,
                            int xyz, bool is_3D);

  virtual void render_tickmarks (const Matrix& ticks, double lim1, double lim2,
                                 double p1, double p1N, double p2, double p2N,
                                 double dx, double dy, double dz,
                                 int xyz, bool doubleside);

  virtual void render_ticktexts (const Matrix& ticks,
                                 const string_vector& ticklabels,
                                 double lim1, double lim2,
                                 double p1, double p2,
                                 int xyz, int ha, int va,
                                 int& wmax, int& hmax);

private:
  opengl_renderer (const opengl_renderer&)
    : toolkit (), xform (), xmin (), xmax (), ymin (), ymax (),
    zmin (), zmax (), xZ1 (), xZ2 (), marker_id (), filled_marker_id (),
    camera_pos (), camera_dir ()
#if HAVE_FREETYPE
    , text_renderer ()
#endif
  { }

  opengl_renderer& operator = (const opengl_renderer&)
  { return *this; }

  bool is_nan_or_inf (double x, double y, double z) const
  {
    return (xisnan (x) || xisnan (y) || xisnan (z)
            || xisinf (x) || xisinf (y) || xisinf (z));
  }

  octave_uint8 clip_code (double x, double y, double z) const
  {
    return ((x < xmin ? 1 : 0)
            | (x > xmax ? 1 : 0) << 1
            | (y < ymin ? 1 : 0) << 2
            | (y > ymax ? 1 : 0) << 3
            | (z < zmin ? 1 : 0) << 4
            | (z > zmax ? 1 : 0) << 5
            | (is_nan_or_inf (x, y, z) ? 0 : 1) << 6);
  }

  unsigned int make_marker_list (const std::string& m, double size,
                                 bool filled) const;

  void draw_axes_planes (const axes::properties& props);
  void draw_axes_boxes (const axes::properties& props);

  void draw_axes_x_grid (const axes::properties& props);
  void draw_axes_y_grid (const axes::properties& props);
  void draw_axes_z_grid (const axes::properties& props);

  void draw_axes_children (const axes::properties& props);

private:
  // The graphics toolkit associated with the figure being rendered.
  graphics_toolkit toolkit;

  // axes transformation data
  graphics_xform xform;

  // axis limits in model scaled coordinate
  double xmin, xmax;
  double ymin, ymax;
  double zmin, zmax;

  // Z projection limits in windows coordinate
  double xZ1, xZ2;

  // call lists identifiers for markers
  unsigned int marker_id, filled_marker_id;

  // camera information for primitive sorting
  ColumnVector camera_pos, camera_dir;

#if HAVE_FREETYPE
  // FreeType render, used for text rendering
  ft_render text_renderer;
#endif

private:
  class patch_tesselator;
};

#endif

#endif
