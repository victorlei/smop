/*

Copyright (C) 2009-2015 Shai Ayal

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

#include "error.h"
#include "gl2ps-renderer.h"

#ifdef HAVE_GL2PS_H

#include <cstdio>

#include "lo-mappers.h"
#include "oct-locbuf.h"
#include "unwind-prot.h"

#include "gl2ps.h"
#include "sysdep.h"

void
glps_renderer::draw (const graphics_object& go, const std::string& print_cmd)
{
  static bool in_draw = false;
  static std::string old_print_cmd;

  if (!in_draw)
    {
      unwind_protect frame;

      frame.protect_var (in_draw);

      in_draw = true;

      GLint gl2ps_term;
      if (term.find ("eps") != std::string::npos)
        gl2ps_term = GL2PS_EPS;
      else if (term.find ("pdf") != std::string::npos)
        gl2ps_term = GL2PS_PDF;
      else if (term.find ("ps") != std::string::npos)
        gl2ps_term = GL2PS_PS;
      else if (term.find ("svg") != std::string::npos)
        gl2ps_term = GL2PS_SVG;
      else if (term.find ("pgf") != std::string::npos)
        gl2ps_term = GL2PS_PGF;
      else if (term.find ("tex") != std::string::npos)
        gl2ps_term = GL2PS_TEX;
      else
        {
          error ("gl2ps-renderer::draw: Unknown terminal %s", term.c_str ());
          return;
        }

      GLint gl2ps_text = 0;
      if (term.find ("notxt") != std::string::npos)
        gl2ps_text = GL2PS_NO_TEXT;

      // Default sort order optimizes for 3D plots
      GLint gl2ps_sort = GL2PS_BSP_SORT;

      // For 2D plots we can use a simpler Z-depth sorting algorithm
      if (term.find ("is2D") != std::string::npos)
        gl2ps_sort = GL2PS_SIMPLE_SORT;

      GLint state = GL2PS_OVERFLOW;
      GLint buffsize = 0;

      while (state == GL2PS_OVERFLOW)
        {
          // For LaTeX output the fltk print process uses 2 drawnow() commands.
          // The first one is for the pdf/ps/eps graph to be included.  The
          // print_cmd is saved as old_print_cmd.  Then the second drawnow()
          // outputs the tex-file and the graphic filename to be included is
          // extracted from old_print_cmd.

          std::string include_graph;

          size_t found_redirect = old_print_cmd.find (">");

          if (found_redirect != std::string::npos)
            include_graph = old_print_cmd.substr (found_redirect + 1);
          else
            include_graph = old_print_cmd;

          size_t n_begin = include_graph.find_first_not_of (" ");

          if (n_begin != std::string::npos)
            {
              size_t n_end = include_graph.find_last_not_of (" ");
              include_graph = include_graph.substr (n_begin,
                                                    n_end - n_begin + 1);
            }
          else
            include_graph = "foobar-inc";

          buffsize += 1024*1024;

          // GL2PS_SILENT was removed to allow gl2ps printing errors on stderr
          GLint ret = gl2psBeginPage ("glps_renderer figure", "Octave", 0,
                                      gl2ps_term, gl2ps_sort,
                                      (GL2PS_NO_BLENDING
                                       | GL2PS_OCCLUSION_CULL
                                       | GL2PS_BEST_ROOT
                                       | gl2ps_text
                                       | GL2PS_NO_PS3_SHADING
                                       | GL2PS_USE_CURRENT_VIEWPORT),
                                      GL_RGBA, 0, 0, 0, 0, 0,
                                      buffsize, fp, include_graph.c_str ());
          if (ret == GL2PS_ERROR)
            {
              old_print_cmd.clear ();
              error ("gl2ps-renderer::draw: gl2psBeginPage returned GL2PS_ERROR");
              return;
            }

          old_print_cmd = print_cmd;

          opengl_renderer::draw (go);

          // Without glFinish () there may primitives be missing in the
          // gl2ps output.
          glFinish ();

          state = gl2psEndPage ();

          if (state == GL2PS_NO_FEEDBACK)
            {
              warning ("gl2ps-renderer::draw: empty feedback buffer and/or nothing else to print");
            }
          else if (state == GL2PS_ERROR)
            {
              old_print_cmd.clear ();
              error ("gl2ps-renderer::draw: gl2psEndPage returned GL2PS_ERROR");
              return;
            }

          // Don't check state for GL2PS_UNINITIALIZED (should never happen)
          // GL2PS_OVERFLOW (see while loop) or GL2PS_SUCCESS
        }
    }
  else
    opengl_renderer::draw (go);
}

int
glps_renderer::alignment_to_mode (int ha, int va) const
{
  int gl2psa = GL2PS_TEXT_BL;

  if (ha == 0)
    {
      if (va == 0 || va == 3)
        gl2psa=GL2PS_TEXT_BL;
      else if (va == 2)
        gl2psa=GL2PS_TEXT_TL;
      else if (va == 1)
        gl2psa=GL2PS_TEXT_CL;
    }
  else if (ha == 2)
    {
      if (va == 0 || va == 3)
        gl2psa=GL2PS_TEXT_BR;
      else if (va == 2)
        gl2psa=GL2PS_TEXT_TR;
      else if (va == 1)
        gl2psa=GL2PS_TEXT_CR;
    }
  else if (ha == 1)
    {
      if (va == 0 || va == 3)
        gl2psa=GL2PS_TEXT_B;
      else if (va == 2)
        gl2psa=GL2PS_TEXT_T;
      else if (va == 1)
        gl2psa=GL2PS_TEXT_C;
    }

  return gl2psa;
}

Matrix
glps_renderer::render_text (const std::string& txt,
                            double x, double y, double z,
                            int ha, int va, double rotation)
{
  if (txt.empty ())
    return Matrix (1, 4, 0.0);

  glRasterPos3d (x, y, z);

  gl2psTextOpt (txt.c_str (), fontname.c_str (), fontsize,
                alignment_to_mode (ha, va), rotation);

  // FIXME?
  // We have no way of getting a bounding box from gl2ps, so we use FreeType.
  Matrix bbox;
  uint8NDArray pixels;
  text_to_pixels (txt, pixels, bbox, 0, 0, rotation);

  return bbox;
}

void
glps_renderer::set_font (const base_properties& props)
{
  opengl_renderer::set_font (props);

  fontsize = props.get ("fontsize_points").double_value ();

  caseless_str fn = props.get ("fontname").xtolower ().string_value ();
  bool isbold =
    (props.get ("fontweight").xtolower ().string_value () == "bold");
  bool isitalic =
    (props.get ("fontangle").xtolower ().string_value () == "italic");

  fontname = "";
  if (fn == "times" || fn == "times-roman")
    {
      if (isitalic && isbold)
        fontname = "Times-BoldItalic";
      else if (isitalic)
        fontname = "Times-Italic";
      else if (isbold)
        fontname = "Times-Bold";
      else
        fontname = "Times-Roman";
    }
  else if (fn == "courier")
    {
      if (isitalic && isbold)
        fontname = "Courier-BoldOblique";
      else if (isitalic)
        fontname = "Courier-Oblique";
      else if (isbold)
        fontname = "Courier-Bold";
      else
        fontname = "Courier";
    }
  else if (fn == "symbol")
    fontname = "Symbol";
  else if (fn == "zapfdingbats")
    fontname = "ZapfDingbats";
  else
    {
      if (isitalic && isbold)
        fontname = "Helvetica-BoldOblique";
      else if (isitalic)
        fontname = "Helvetica-Oblique";
      else if (isbold)
        fontname = "Helvetica-Bold";
      else
        fontname = "Helvetica";
    }
}

template <typename T>
static void
draw_pixels (GLsizei w, GLsizei h, GLenum format, const T *data, float maxval)
{
  OCTAVE_LOCAL_BUFFER (GLfloat, a, 3*w*h);

  // Convert to GL_FLOAT as it is the only type gl2ps accepts.
  for (int i = 0; i < 3*w*h; i++)
    a[i] = data[i] / maxval;

  gl2psDrawPixels (w, h, 0, 0, format, GL_FLOAT, a);
}

void
glps_renderer::draw_pixels (GLsizei w, GLsizei h, GLenum format,
                            GLenum type, const GLvoid *data)
{
  // gl2psDrawPixels only supports the GL_FLOAT type.
  // Other formats, such as uint8, must be converted first.
  if (type == GL_UNSIGNED_BYTE)
    ::draw_pixels (w, h, format, static_cast<const GLubyte *> (data), 255.0f);
  else if (type == GL_UNSIGNED_SHORT)
    ::draw_pixels (w, h, format, static_cast<const GLushort *> (data), 65535.0f);
  else
    gl2psDrawPixels (w, h, 0, 0, format, type, data);
}

void
glps_renderer::draw_text (const text::properties& props)
{
  if (props.get_string ().is_empty ())
    return;

  set_font (props);
  set_color (props.get_color_rgb ());

  const Matrix pos = get_transform ().scale (props.get_data_position ());
  int halign = 0;
  int valign = 0;

  if (props.horizontalalignment_is ("center"))
    halign = 1;
  else if (props.horizontalalignment_is ("right"))
    halign = 2;

  if (props.verticalalignment_is ("top"))
    valign = 2;
  else if (props.verticalalignment_is ("baseline"))
    valign = 3;
  else if (props.verticalalignment_is ("middle"))
    valign = 1;

  // FIXME: handle margin and surrounding box

  glRasterPos3d (pos(0), pos(1), pos.numel () > 2 ? pos(2) : 0.0);

  octave_value string_prop = props.get_string ();

  string_vector sv = string_prop.all_strings ();

  std::string s = sv.join ("\n");

  gl2psTextOpt (s.c_str (), fontname.c_str (), fontsize,
                alignment_to_mode (halign, valign), props.get_rotation ());
}

static void
safe_pclose (FILE *f)
{
  if (f)
    octave_pclose (f);
}

#endif

void
gl2ps_print (const graphics_object& fig, const std::string& cmd,
             const std::string& term)
{
#ifdef HAVE_GL2PS_H

  FILE *fp = octave_popen (cmd.c_str (), "w");

  if (fp)
    {
      unwind_protect frame;

      frame.add_fcn (safe_pclose, fp);

      glps_renderer rend (fp, term);

      rend.draw (fig, cmd);
    }
  else
    error ("print: failed to open pipe for gl2ps renderer");

#else

  error ("print: printing not available without gl2ps library");

#endif
}
