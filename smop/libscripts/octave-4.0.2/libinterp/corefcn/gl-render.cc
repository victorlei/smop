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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_OPENGL)

#include <iostream>

#include <lo-mappers.h>
#include "oct-locbuf.h"
#include "oct-refcount.h"
#include "gl-render.h"
#include "txt-eng.h"
#include "txt-eng-ft.h"

#define LIGHT_MODE GL_FRONT_AND_BACK

// Use symbolic names for axes
enum
{
  X_AXIS,
  Y_AXIS,
  Z_AXIS
};

// Use symbolic names for color mode
enum
{
  UNIFORM,
  FLAT,
  INTERP,
  TEXTURE
};

// Use symbolic names for lighting
enum
{
  NONE,
  //FLAT,  // Already declared in anonymous enum for color mode
  GOURAUD = 2
};

// Win32 API requires the CALLBACK attributes for
// GLU callback functions. Define it to empty on
// other platforms.
#ifndef CALLBACK
#define CALLBACK
#endif

class
opengl_texture
{
protected:
  class texture_rep
  {
  public:
    texture_rep (void)
      : id (), w (), h (), tw (), th (), tx (), ty (),
        valid (false), count (1)
    { }

    texture_rep (GLuint id_arg, int w_arg, int h_arg, int tw_arg, int th_arg)
      : id (id_arg), w (w_arg), h (h_arg), tw (tw_arg), th (th_arg),
        tx (double(w)/tw), ty (double(h)/th), valid (true),
        count (1) { }

    ~texture_rep (void)
    {
      if (valid)
        glDeleteTextures (1, &id);
    }

    void bind (int mode) const
    { if (valid) glBindTexture (mode, id); }

    void tex_coord (double q, double r) const
    { if (valid) glTexCoord2d (q*tx, r*ty); }

    GLuint id;
    int w, h;
    int tw, th;
    double tx, ty;
    bool valid;
    octave_refcount<int> count;
  };

  texture_rep *rep;

private:
  opengl_texture (texture_rep *_rep) : rep (_rep) { }

public:
  opengl_texture (void) : rep (new texture_rep ()) { }

  opengl_texture (const opengl_texture& tx)
    : rep (tx.rep)
  {
    rep->count++;
  }

  ~opengl_texture (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  opengl_texture& operator = (const opengl_texture& tx)
  {
    if (--rep->count == 0)
      delete rep;

    rep = tx.rep;
    rep->count++;

    return *this;
  }

  static opengl_texture create (const octave_value& data);

  void bind (int mode = GL_TEXTURE_2D) const
  { rep->bind (mode); }

  void tex_coord (double q, double r) const
  { rep->tex_coord (q, r); }

  bool is_valid (void) const
  { return rep->valid; }
};

static int
next_power_of_2 (int n)
{
  int m = 1;

  while (m < n && m < std::numeric_limits<int>::max ())
    m <<= 1;

  return m;
}

opengl_texture
opengl_texture::create (const octave_value& data)
{
  opengl_texture retval;

  dim_vector dv (data.dims ());

  // Expect RGB data
  if (dv.length () == 3 && dv(2) == 3)
    {
      // FIXME: dim_vectors hold octave_idx_type values.
      //        Should we check for dimensions larger than intmax?
      int h, w, tw, th;
      h = dv(0), w = dv(1);
      GLuint id;
      bool ok = true;

      tw = next_power_of_2 (w);
      th = next_power_of_2 (h);

      glGenTextures (1, &id);
      glBindTexture (GL_TEXTURE_2D, id);

      if (data.is_double_type ())
        {
          const NDArray xdata = data.array_value ();

          OCTAVE_LOCAL_BUFFER (float, a, (3*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
                {
                  a[idx]   = xdata(i,j,0);
                  a[idx+1] = xdata(i,j,1);
                  a[idx+2] = xdata(i,j,2);
                }
            }

          glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0, GL_RGB, GL_FLOAT, a);
        }
      else if (data.is_uint8_type ())
        {
          const uint8NDArray xdata = data.uint8_array_value ();

          OCTAVE_LOCAL_BUFFER (octave_uint8, a, (3*tw*th));

          for (int i = 0; i < h; i++)
            {
              for (int j = 0, idx = i*tw*3; j < w; j++, idx += 3)
                {
                  a[idx]   = xdata(i,j,0);
                  a[idx+1] = xdata(i,j,1);
                  a[idx+2] = xdata(i,j,2);
                }
            }

          glTexImage2D (GL_TEXTURE_2D, 0, 3, tw, th, 0,
                        GL_RGB, GL_UNSIGNED_BYTE, a);
        }
      else
        {
          ok = false;
          warning ("opengl_texture::create: invalid texture data type (expected double or uint8)");
        }

      if (ok)
        {
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

          if (glGetError () != GL_NO_ERROR)
            warning ("opengl_texture::create: OpenGL error while generating texture data");
          else
            retval = opengl_texture (new texture_rep (id, w, h, tw, th));
        }
    }
  else
    warning ("opengl_texture::create: invalid texture data size");

  return retval;
}

class
opengl_tesselator
{
public:
#if defined (HAVE_FRAMEWORK_OPENGL) && defined (HAVE_GLUTESSCALLBACK_THREEDOTS)
  typedef GLvoid (CALLBACK *fcn) (...);
#else
  typedef void (CALLBACK *fcn) (void);
#endif

public:

  opengl_tesselator (void) : glu_tess (0), fill () { init (); }

  virtual ~opengl_tesselator (void)
  { if (glu_tess) gluDeleteTess (glu_tess); }

  void begin_polygon (bool filled = true)
  {
    gluTessProperty (glu_tess, GLU_TESS_BOUNDARY_ONLY,
                     (filled ? GL_FALSE : GL_TRUE));
    fill = filled;
    gluTessBeginPolygon (glu_tess, this);
  }

  void end_polygon (void) const
  { gluTessEndPolygon (glu_tess); }

  void begin_contour (void) const
  { gluTessBeginContour (glu_tess); }

  void end_contour (void) const
  { gluTessEndContour (glu_tess); }

  void add_vertex (double *loc, void *data) const
  { gluTessVertex (glu_tess, loc, data); }

protected:
  virtual void begin (GLenum /*type*/) { }

  virtual void end (void) { }

  virtual void vertex (void * /*data*/) { }

  virtual void combine (GLdouble [3] /*c*/, void * [4] /*data*/,
                        GLfloat  [4] /*w*/, void ** /*out_data*/) { }

  virtual void edge_flag (GLboolean /*flag*/) { }

  virtual void error (GLenum err)
  { ::error ("OpenGL tesselation error (%d)", err); }

  virtual void init (void)
  {
    glu_tess = gluNewTess ();

    gluTessCallback (glu_tess, GLU_TESS_BEGIN_DATA,
                     reinterpret_cast<fcn> (tess_begin));
    gluTessCallback (glu_tess, GLU_TESS_END_DATA,
                     reinterpret_cast<fcn> (tess_end));
    gluTessCallback (glu_tess, GLU_TESS_VERTEX_DATA,
                     reinterpret_cast<fcn> (tess_vertex));
    gluTessCallback (glu_tess, GLU_TESS_COMBINE_DATA,
                     reinterpret_cast<fcn> (tess_combine));
    gluTessCallback (glu_tess, GLU_TESS_EDGE_FLAG_DATA,
                     reinterpret_cast<fcn> (tess_edge_flag));
    gluTessCallback (glu_tess, GLU_TESS_ERROR_DATA,
                     reinterpret_cast<fcn> (tess_error));
  }

  bool is_filled (void) const { return fill; }

private:
  static void CALLBACK tess_begin (GLenum type, void *t)
  { reinterpret_cast<opengl_tesselator *> (t)->begin (type); }

  static void CALLBACK tess_end (void *t)
  { reinterpret_cast<opengl_tesselator *> (t)->end (); }

  static void CALLBACK tess_vertex (void *v, void *t)
  { reinterpret_cast<opengl_tesselator *> (t)->vertex (v); }

  static void CALLBACK tess_combine (GLdouble c[3], void *v[4], GLfloat w[4],
                                     void **out,  void *t)
  { reinterpret_cast<opengl_tesselator *> (t)->combine (c, v, w, out); }

  static void CALLBACK tess_edge_flag (GLboolean flag, void *t)
  { reinterpret_cast<opengl_tesselator *> (t)->edge_flag (flag); }

  static void CALLBACK tess_error (GLenum err, void *t)
  { reinterpret_cast<opengl_tesselator *> (t)->error (err); }

private:

  // No copying!

  opengl_tesselator (const opengl_tesselator&);

  opengl_tesselator operator = (const opengl_tesselator&);

  GLUtesselator *glu_tess;
  bool fill;
};

class
vertex_data
{
public:
  class vertex_data_rep
  {
  public:
    Matrix coords;
    Matrix color;
    Matrix normal;
    double alpha;
    float ambient;
    float diffuse;
    float specular;
    float specular_exp;

    // reference counter
    octave_refcount<int> count;

    vertex_data_rep (void)
      : coords (), color (), normal (), alpha (),
        ambient (), diffuse (), specular (), specular_exp (),count (1) { }

    vertex_data_rep (const Matrix& c, const Matrix& col, const Matrix& n,
                     double a, float as, float ds, float ss, float se)
      : coords (c), color (col), normal (n), alpha (a),
        ambient (as), diffuse (ds), specular (ss), specular_exp (se),
        count (1) { }
  };

private:
  vertex_data_rep *rep;

  vertex_data_rep *nil_rep (void) const
  {
    static vertex_data_rep *nr = new vertex_data_rep ();

    return nr;
  }

public:
  vertex_data (void) : rep (nil_rep ())
  { rep->count++; }

  vertex_data (const vertex_data& v) : rep (v.rep)
  { rep->count++; }

  vertex_data (const Matrix& c, const Matrix& col, const Matrix& n,
               double a, float as, float ds, float ss, float se)
    : rep (new vertex_data_rep (c, col, n, a, as, ds, ss, se))
  { }

  vertex_data (vertex_data_rep *new_rep)
    : rep (new_rep) { }

  ~vertex_data (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  vertex_data& operator = (const vertex_data& v)
  {
    if (--rep->count == 0)
      delete rep;

    rep = v.rep;
    rep->count++;

    return *this;
  }

  vertex_data_rep *get_rep (void) const { return rep; }
};

class
opengl_renderer::patch_tesselator : public opengl_tesselator
{
public:
  patch_tesselator (opengl_renderer *r, int cmode, int lmode, float idx = 0.0)
    : opengl_tesselator (), renderer (r),
      color_mode (cmode), light_mode (lmode), index (idx),
      first (true), tmp_vdata ()
  { }

protected:
  void begin (GLenum type)
  {
    //printf ("patch_tesselator::begin (%d)\n", type);
    first = true;

    if (color_mode == INTERP || light_mode == GOURAUD)
      glShadeModel (GL_SMOOTH);
    else
      glShadeModel (GL_FLAT);

    if (is_filled ())
      renderer->set_polygon_offset (true, index);

    glBegin (type);
  }

  void end (void)
  {
    //printf ("patch_tesselator::end\n");
    glEnd ();
    renderer->set_polygon_offset (false);
  }

  void vertex (void *data)
  {
    vertex_data::vertex_data_rep *v
      = reinterpret_cast<vertex_data::vertex_data_rep *> (data);
    //printf ("patch_tesselator::vertex (%g, %g, %g)\n", v->coords(0), v->coords(1), v->coords(2));

    // NOTE: OpenGL can re-order vertices.  For "flat" coloring of FaceColor
    // the first vertex must be identified in the draw_patch routine.

    if (color_mode == INTERP || (color_mode == FLAT && ! is_filled ()))
      {
        Matrix col = v->color;

        if (col.numel () == 3)
          {
            glColor3dv (col.data ());
            if (light_mode > 0)
              {
                float buf[4] = { 0, 0, 0, 1 };

                for (int k = 0; k < 3; k++)
                  buf[k] = (v->ambient * col(k));
                glMaterialfv (LIGHT_MODE, GL_AMBIENT, buf);

                for (int k = 0; k < 3; k++)
                  buf[k] = (v->diffuse * col(k));
                glMaterialfv (LIGHT_MODE, GL_DIFFUSE, buf);
              }
          }
      }

    if (light_mode > 0 && (first || light_mode == GOURAUD))
      glNormal3dv (v->normal.data ());

    glVertex3dv (v->coords.data ());

    first = false;
  }

  void combine (GLdouble xyz[3], void *data[4], GLfloat w[4], void **out_data)
  {
    //printf ("patch_tesselator::combine\n");

    vertex_data::vertex_data_rep *v[4];
    int vmax = 4;

    for (int i = 0; i < 4; i++)
      {
        v[i] = reinterpret_cast<vertex_data::vertex_data_rep *> (data[i]);

        if (vmax == 4 && ! v[i])
          vmax = i;
      }

    Matrix vv (1, 3, 0.0);
    Matrix cc;
    Matrix nn (1, 3, 0.0);
    double aa = 0.0;

    vv(0) = xyz[0];
    vv(1) = xyz[1];
    vv(2) = xyz[2];

    if (v[0]->color.numel ())
      {
        cc.resize (1, 3, 0.0);
        for (int ic = 0; ic < 3; ic++)
          for (int iv = 0; iv < vmax; iv++)
            cc(ic) += (w[iv] * v[iv]->color (ic));
      }

    if (v[0]->normal.numel () > 0)
      {
        for (int in = 0; in < 3; in++)
          for (int iv = 0; iv < vmax; iv++)
            nn(in) += (w[iv] * v[iv]->normal (in));
      }

    for (int iv = 0; iv < vmax; iv++)
      aa += (w[iv] * v[iv]->alpha);

    vertex_data new_v (vv, cc, nn, aa, v[0]->ambient, v[0]->diffuse,
                       v[0]->specular, v[0]->specular_exp);
    tmp_vdata.push_back (new_v);

    *out_data = new_v.get_rep ();
  }

private:

  // No copying!

  patch_tesselator (const patch_tesselator&);

  patch_tesselator& operator = (const patch_tesselator&);

  opengl_renderer *renderer;
  int color_mode;
  int light_mode;
  int index;
  bool first;
  std::list<vertex_data> tmp_vdata;
};

void
opengl_renderer::draw (const graphics_object& go, bool toplevel)
{
  if (! go.valid_object ())
    return;

  const base_properties& props = go.get_properties ();

  if (! toolkit)
    toolkit = props.get_toolkit ();

  if (go.isa ("figure"))
    draw_figure (dynamic_cast<const figure::properties&> (props));
  else if (go.isa ("axes"))
    draw_axes (dynamic_cast<const axes::properties&> (props));
  else if (go.isa ("line"))
    draw_line (dynamic_cast<const line::properties&> (props));
  else if (go.isa ("surface"))
    draw_surface (dynamic_cast<const surface::properties&> (props));
  else if (go.isa ("patch"))
    draw_patch (dynamic_cast<const patch::properties&> (props));
  else if (go.isa ("hggroup"))
    draw_hggroup (dynamic_cast<const hggroup::properties&> (props));
  else if (go.isa ("text"))
    draw_text (dynamic_cast<const text::properties&> (props));
  else if (go.isa ("image"))
    draw_image (dynamic_cast<const image::properties&> (props));
  else if (go.isa ("uimenu") || go.isa ("uicontrol")
           || go.isa ("uicontextmenu") || go.isa ("uitoolbar")
           || go.isa ("uipushtool") || go.isa ("uitoggletool"))
    /* SKIP */;
  else if (go.isa ("uipanel"))
    {
      if (toplevel)
        draw_uipanel (dynamic_cast<const uipanel::properties&> (props), go);
    }
  else
    {
      warning ("opengl_renderer: cannot render object of type '%s'",
               props.graphics_object_name ().c_str ());
    }
}

void
opengl_renderer::draw_figure (const figure::properties& props)
{
  // Initialize OpenGL context

  init_gl_context (props.is___enhanced__ (), props.get_color_rgb ());

  // Draw children

  draw (props.get_all_children (), false);
}

void
opengl_renderer::draw_uipanel (const uipanel::properties& props,
                               const graphics_object& go)
{
  graphics_object fig = go.get_ancestor ("figure");
  const figure::properties& figProps =
    dynamic_cast<const figure::properties&> (fig.get_properties ());

  // Initialize OpenGL context

  init_gl_context (figProps.is___enhanced__ (),
                   props.get_backgroundcolor_rgb ());

  // Draw children

  draw (props.get_all_children (), false);
}

void
opengl_renderer::init_gl_context (bool enhanced, const Matrix& c)
{
  // Initialize OpenGL context

  glEnable (GL_DEPTH_TEST);
  glDepthFunc (GL_LEQUAL);
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glAlphaFunc (GL_GREATER, 0.0f);
  glEnable (GL_NORMALIZE);

  if (enhanced)
    {
      glEnable (GL_BLEND);
      glEnable (GL_MULTISAMPLE);
      GLint iMultiSample, iNumSamples;
      glGetIntegerv (GL_SAMPLE_BUFFERS, &iMultiSample);
      glGetIntegerv (GL_SAMPLES, &iNumSamples);
      if (iMultiSample != GL_TRUE || iNumSamples == 0)
        {
          // MultiSample not implemented.  Use old-style anti-aliasing
          glDisable (GL_MULTISAMPLE);
          glEnable (GL_LINE_SMOOTH);
          glHint (GL_LINE_SMOOTH_HINT, GL_NICEST);
        }
    }
  else
    {
      glDisable (GL_BLEND);
      glDisable (GL_LINE_SMOOTH);
    }

  // Clear background

  if (c.length () >= 3)
    {
      glClearColor (c(0), c(1), c(2), 1);
      glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }
}

void
opengl_renderer::render_grid (const std::string& gridstyle,
                              const Matrix& ticks, double lim1, double lim2,
                              double p1, double p1N, double p2, double p2N,
                              int xyz, bool is_3D)
{
  set_linestyle (gridstyle, true);
  glBegin (GL_LINES);
  for (int i = 0; i < ticks.numel (); i++)
    {
      double val = ticks(i);
      if (lim1 <= val && val <= lim2)
        {
          if (xyz == X_AXIS)
            {
              glVertex3d (val, p1N, p2);
              glVertex3d (val, p1, p2);
              if (is_3D)
                {
                  glVertex3d (val, p1, p2N);
                  glVertex3d (val, p1, p2);
                }
            }
          else if (xyz == Y_AXIS)
            {
              glVertex3d (p1N, val, p2);
              glVertex3d (p1, val, p2);
              if (is_3D)
                {
                  glVertex3d (p1, val, p2N);
                  glVertex3d (p1, val, p2);
                }
            }
          else if (xyz == Z_AXIS)
            {
              glVertex3d (p1N, p2, val);
              glVertex3d (p1, p2, val);
              glVertex3d (p1, p2N, val);
              glVertex3d (p1, p2, val);
            }
        }
    }
  glEnd ();
  set_linestyle ("-", true);
}

void
opengl_renderer::render_tickmarks (const Matrix& ticks,
                                   double lim1, double lim2,
                                   double p1, double p1N,
                                   double p2, double p2N,
                                   double dx, double dy, double dz,
                                   int xyz, bool mirror)
{
  glBegin (GL_LINES);

  for (int i = 0; i < ticks.numel (); i++)
    {
      double val = ticks(i);

      if (lim1 <= val && val <= lim2)
        {
          if (xyz == X_AXIS)
            {
              glVertex3d (val, p1, p2);
              glVertex3d (val, p1+dy, p2+dz);
              if (mirror)
                {
                  glVertex3d (val, p1N, p2N);
                  glVertex3d (val, p1N-dy, p2N-dz);
                }
            }
          else if (xyz == Y_AXIS)
            {
              glVertex3d (p1, val, p2);
              glVertex3d (p1+dx, val, p2+dz);
              if (mirror)
                {
                  glVertex3d (p1N, val, p2N);
                  glVertex3d (p1N-dx, val, p2N-dz);
                }
            }
          else if (xyz == Z_AXIS)
            {
              glVertex3d (p1, p2, val);
              glVertex3d (p1+dx, p2+dy, val);
              if (mirror)
                {
                  glVertex3d (p1N, p2N, val);
                  glVertex3d (p1N-dx, p2N-dy, val);
                }
            }
        }
    }

  glEnd ();
}

void
opengl_renderer::render_ticktexts (const Matrix& ticks,
                                   const string_vector& ticklabels,
                                   double lim1, double lim2,
                                   double p1, double p2,
                                   int xyz, int ha, int va,
                                   int& wmax, int& hmax)
{
  int nticks  = ticks.numel ();
  int nlabels = ticklabels.numel ();

  if (nlabels == 0)
    return;

  for (int i = 0; i < nticks; i++)
    {
      double val = ticks(i);

      if (lim1 <= val && val <= lim2)
        {
          Matrix b;

          std::string label (ticklabels(i % nlabels));
          label.erase (0, label.find_first_not_of (" "));
          label = label.substr (0, label.find_last_not_of (" ")+1);

          // FIXME: As tick text is transparent, shouldn't it be
          //        drawn after axes object, for correct rendering?
          if (xyz == X_AXIS)
            {
              b = render_text (label, val, p1, p2, ha, va);
            }
          else if (xyz == Y_AXIS)
            {
              b = render_text (label, p1, val, p2, ha, va);
            }
          else if (xyz == Z_AXIS)
            {
              b = render_text (label, p1, p2, val, ha, va);
            }

          wmax = std::max (wmax, static_cast<int> (b(2)));
          hmax = std::max (hmax, static_cast<int> (b(3)));
        }
    }
}

void
opengl_renderer::setup_opengl_transformation (const axes::properties& props)
{
  // setup OpenGL transformation

  Matrix x_zlim = props.get_transform_zlim ();

  xZ1 = x_zlim(0)-(x_zlim(1)-x_zlim(0))/2;
  xZ2 = x_zlim(1)+(x_zlim(1)-x_zlim(0))/2;

  Matrix x_mat1 = props.get_opengl_matrix_1 ();
  Matrix x_mat2 = props.get_opengl_matrix_2 ();

#if defined (HAVE_FRAMEWORK_OPENGL)
  GLint vw[4];
#else
  int vw[4];
#endif

  glGetIntegerv (GL_VIEWPORT, vw);

  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  glScaled (1, 1, -1);
  glMultMatrixd (x_mat1.data ());
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glOrtho (0, vw[2], vw[3], 0, xZ1, xZ2);
  glMultMatrixd (x_mat2.data ());
  glMatrixMode (GL_MODELVIEW);

  glClear (GL_DEPTH_BUFFER_BIT);

  // store axes transformation data

  xform = props.get_transform ();
}

void
opengl_renderer::draw_axes_planes (const axes::properties& props)
{
  Matrix axe_color = props.get_color_rgb ();
  if (axe_color.numel () == 0 || ! props.is_visible ())
    return;

  double xPlane = props.get_xPlane ();
  double yPlane = props.get_yPlane ();
  double zPlane = props.get_zPlane ();
  double xPlaneN = props.get_xPlaneN ();
  double yPlaneN = props.get_yPlaneN ();
  double zPlaneN = props.get_zPlaneN ();
  bool is2d = props.get_is2D ();

  // Axes planes
  set_color (axe_color);
  set_polygon_offset (true, 2.5);

  glBegin (GL_QUADS);

  if (! is2d)
    {
      // X plane
      glVertex3d (xPlane, yPlaneN, zPlaneN);
      glVertex3d (xPlane, yPlane, zPlaneN);
      glVertex3d (xPlane, yPlane, zPlane);
      glVertex3d (xPlane, yPlaneN, zPlane);

      // Y plane
      glVertex3d (xPlaneN, yPlane, zPlaneN);
      glVertex3d (xPlane, yPlane, zPlaneN);
      glVertex3d (xPlane, yPlane, zPlane);
      glVertex3d (xPlaneN, yPlane, zPlane);
    }

  // Z plane
  glVertex3d (xPlaneN, yPlaneN, zPlane);
  glVertex3d (xPlane, yPlaneN, zPlane);
  glVertex3d (xPlane, yPlane, zPlane);
  glVertex3d (xPlaneN, yPlane, zPlane);

  glEnd ();

  set_polygon_offset (false);
}

void
opengl_renderer::draw_axes_boxes (const axes::properties& props)
{
  if (! props.is_visible ())
    return;

  bool xySym = props.get_xySym ();
  bool layer2Dtop = props.get_layer2Dtop ();
  bool is2d = props.get_is2D ();
  double xPlane = props.get_xPlane ();
  double yPlane = props.get_yPlane ();
  double zPlane = props.get_zPlane ();
  double xPlaneN = props.get_xPlaneN ();
  double yPlaneN = props.get_yPlaneN ();
  double zPlaneN = props.get_zPlaneN ();
  double xpTick = props.get_xpTick ();
  double ypTick = props.get_ypTick ();
  double zpTick = props.get_zpTick ();
  double xpTickN = props.get_xpTickN ();
  double ypTickN = props.get_ypTickN ();
  double zpTickN = props.get_zpTickN ();

  bool plotyy = (props.has_property ("__plotyy_axes__"));

  // Axes box

  set_linestyle ("-", true);
  set_linewidth (props.get_linewidth ());

  glBegin (GL_LINES);

  if (layer2Dtop)
    std::swap (zpTick, zpTickN);

  // X box
  set_color (props.get_xcolor_rgb ());
  glVertex3d (xPlaneN, ypTick, zpTick);
  glVertex3d (xPlane, ypTick, zpTick);

  if (props.is_box ())
    {
      glVertex3d (xPlaneN, ypTickN, zpTick);
      glVertex3d (xPlane, ypTickN, zpTick);
      if (! is2d)
        {
          glVertex3d (xPlaneN, ypTickN, zpTickN);
          glVertex3d (xPlane, ypTickN, zpTickN);
          glVertex3d (xPlaneN, ypTick, zpTickN);
          glVertex3d (xPlane, ypTick, zpTickN);
        }
    }

  // Y box
  set_color (props.get_ycolor_rgb ());
  glVertex3d (xpTick, yPlaneN, zpTick);
  glVertex3d (xpTick, yPlane, zpTick);

  if (props.is_box () && ! plotyy)
    {
      glVertex3d (xpTickN, yPlaneN, zpTick);
      glVertex3d (xpTickN, yPlane, zpTick);

      if (! is2d)
        {
          glVertex3d (xpTickN, yPlaneN, zpTickN);
          glVertex3d (xpTickN, yPlane, zpTickN);
          glVertex3d (xpTick, yPlaneN, zpTickN);
          glVertex3d (xpTick, yPlane, zpTickN);
        }
    }

  // Z box
  if (! is2d)
    {
      set_color (props.get_zcolor_rgb ());

      if (xySym)
        {
          glVertex3d (xPlaneN, yPlane, zPlaneN);
          glVertex3d (xPlaneN, yPlane, zPlane);
        }
      else
        {
          glVertex3d (xPlane, yPlaneN, zPlaneN);
          glVertex3d (xPlane, yPlaneN, zPlane);
        }

      if (props.is_box ())
        {
          glVertex3d (xPlane, yPlane, zPlaneN);
          glVertex3d (xPlane, yPlane, zPlane);

          if (xySym)
            {
              glVertex3d (xPlane, yPlaneN, zPlaneN);
              glVertex3d (xPlane, yPlaneN, zPlane);
            }
          else
            {
              glVertex3d (xPlaneN, yPlane, zPlaneN);
              glVertex3d (xPlaneN, yPlane, zPlane);
            }

          glVertex3d (xPlaneN, yPlaneN, zPlaneN);
          glVertex3d (xPlaneN, yPlaneN, zPlane);
        }
    }

  glEnd ();
}

void
opengl_renderer::draw_axes_x_grid (const axes::properties& props)
{
  int xstate = props.get_xstate ();

  if (props.is_visible () && xstate != AXE_DEPTH_DIR)
    {
      int zstate = props.get_zstate ();
      bool x2Dtop = props.get_x2Dtop ();
      bool layer2Dtop = props.get_layer2Dtop ();
      bool xyzSym = props.get_xyzSym ();
      bool nearhoriz = props.get_nearhoriz ();
      double xticklen = props.get_xticklen ();
      double xtickoffset = props.get_xtickoffset ();
      double fy = props.get_fy ();
      double fz = props.get_fz ();
      double x_min = props.get_x_min ();
      double x_max = props.get_x_max ();
      double yPlane = props.get_yPlane ();
      double yPlaneN = props.get_yPlaneN ();
      double ypTick = props.get_ypTick ();
      double ypTickN = props.get_ypTickN ();
      double zPlane = props.get_zPlane ();
      double zPlaneN = props.get_zPlaneN ();
      double zpTick = props.get_zpTick ();
      double zpTickN = props.get_zpTickN ();

      // X grid

      std::string gridstyle = props.get_gridlinestyle ();
      std::string minorgridstyle = props.get_minorgridlinestyle ();
      bool do_xgrid = (props.is_xgrid () && (gridstyle != "none"));
      bool do_xminorgrid = (props.is_xminorgrid ()
                            && (minorgridstyle != "none"));
      bool do_xminortick = props.is_xminortick ();
      Matrix xticks = xform.xscale (props.get_xtick ().matrix_value ());
      Matrix xmticks = xform.xscale (props.get_xmtick ().matrix_value ());
      string_vector xticklabels = props.get_xticklabel ().all_strings ();
      int wmax = 0;
      int hmax = 0;
      bool tick_along_z = nearhoriz || xisinf (fy);
      bool mirror = props.is_box () && xstate != AXE_ANY_DIR;

      set_color (props.get_xcolor_rgb ());

      // grid lines
      if (do_xgrid)
        render_grid (gridstyle, xticks, x_min, x_max,
                     yPlane, yPlaneN, layer2Dtop ? zPlaneN : zPlane,
                     zPlaneN, 0, (zstate != AXE_DEPTH_DIR));

      // tick marks
      if (tick_along_z)
        {
          render_tickmarks (xticks, x_min, x_max, ypTick, ypTick,
                            zpTick, zpTickN, 0., 0.,
                            signum (zpTick-zpTickN)*fz*xticklen,
                            0, mirror);
        }
      else
        {
          render_tickmarks (xticks, x_min, x_max, ypTick, ypTickN,
                            zpTick, zpTick, 0.,
                            signum (ypTick-ypTickN)*fy*xticklen,
                            0., 0, mirror);
        }

      // tick texts
      if (xticklabels.numel () > 0)
        {
          int halign = (xstate == AXE_HORZ_DIR ? 1 : (xyzSym ? 0 : 2));
          int valign = (xstate == AXE_VERT_DIR ? 1 : (x2Dtop ? 0 : 2));

          if (tick_along_z)
            render_ticktexts (xticks, xticklabels, x_min, x_max, ypTick,
                              zpTick+signum (zpTick-zpTickN)*fz*xtickoffset,
                              0, halign, valign, wmax, hmax);
          else
            render_ticktexts (xticks, xticklabels, x_min, x_max,
                              ypTick+signum (ypTick-ypTickN)*fy*xtickoffset,
                              zpTick, 0, halign, valign, wmax, hmax);
        }

      // minor grid lines
      if (do_xminorgrid)
        render_grid (minorgridstyle, xmticks, x_min, x_max,
                     yPlane, yPlaneN, layer2Dtop ? zPlaneN : zPlane,
                     zPlaneN, 0, (zstate != AXE_DEPTH_DIR));

      // minor tick marks
      if (do_xminortick)
        {
          if (tick_along_z)
            render_tickmarks (xmticks, x_min, x_max, ypTick, ypTick,
                              zpTick, zpTickN, 0., 0.,
                              signum (zpTick-zpTickN)*fz*xticklen/2,
                              0, mirror);
          else
            render_tickmarks (xmticks, x_min, x_max, ypTick, ypTickN,
                              zpTick, zpTick, 0.,
                              signum (ypTick-ypTickN)*fy*xticklen/2,
                              0., 0, mirror);
        }

      gh_manager::get_object (props.get_xlabel ()).set ("visible", "on");
    }
  else
    gh_manager::get_object (props.get_xlabel ()).set ("visible", "off");
}

void
opengl_renderer::draw_axes_y_grid (const axes::properties& props)
{
  int ystate = props.get_ystate ();

  if (ystate != AXE_DEPTH_DIR && props.is_visible ())
    {
      int zstate = props.get_zstate ();
      bool y2Dright = props.get_y2Dright ();
      bool layer2Dtop = props.get_layer2Dtop ();
      bool xyzSym = props.get_xyzSym ();
      bool nearhoriz = props.get_nearhoriz ();
      double yticklen = props.get_yticklen ();
      double ytickoffset = props.get_ytickoffset ();
      double fx = props.get_fx ();
      double fz = props.get_fz ();
      double xPlane = props.get_xPlane ();
      double xPlaneN = props.get_xPlaneN ();
      double xpTick = props.get_xpTick ();
      double xpTickN = props.get_xpTickN ();
      double y_min = props.get_y_min ();
      double y_max = props.get_y_max ();
      double zPlane = props.get_zPlane ();
      double zPlaneN = props.get_zPlaneN ();
      double zpTick = props.get_zpTick ();
      double zpTickN = props.get_zpTickN ();

      // Y grid

      std::string gridstyle = props.get_gridlinestyle ();
      std::string minorgridstyle = props.get_minorgridlinestyle ();
      bool do_ygrid = (props.is_ygrid () && (gridstyle != "none"));
      bool do_yminorgrid = (props.is_yminorgrid ()
                            && (minorgridstyle != "none"));
      bool do_yminortick = props.is_yminortick ();
      Matrix yticks = xform.yscale (props.get_ytick ().matrix_value ());
      Matrix ymticks = xform.yscale (props.get_ymtick ().matrix_value ());
      string_vector yticklabels = props.get_yticklabel ().all_strings ();
      int wmax = 0;
      int hmax = 0;
      bool tick_along_z = nearhoriz || xisinf (fx);
      bool mirror = props.is_box () && ystate != AXE_ANY_DIR
                    && (! props.has_property ("__plotyy_axes__"));

      set_color (props.get_ycolor_rgb ());

      // grid lines
      if (do_ygrid)
        render_grid (gridstyle, yticks, y_min, y_max,
                     xPlane, xPlaneN, layer2Dtop ? zPlaneN : zPlane,
                     zPlaneN, 1, (zstate != AXE_DEPTH_DIR));

      // tick marks
      if (tick_along_z)
        render_tickmarks (yticks, y_min, y_max, xpTick, xpTick,
                          zpTick, zpTickN, 0., 0.,
                          signum (zpTick-zpTickN)*fz*yticklen,
                          1, mirror);
      else
        render_tickmarks (yticks, y_min, y_max, xpTick, xpTickN,
                          zpTick, zpTick,
                          signum (xPlaneN-xPlane)*fx*yticklen,
                          0., 0., 1, mirror);

      // tick texts
      if (yticklabels.numel () > 0)
        {
          int halign = (ystate == AXE_HORZ_DIR
                        ? 1 : (!xyzSym || y2Dright ? 0 : 2));
          int valign = (ystate == AXE_VERT_DIR ? 1 : 2);

          if (tick_along_z)
            render_ticktexts (yticks, yticklabels, y_min, y_max, xpTick,
                              zpTick+signum (zpTick-zpTickN)*fz*ytickoffset,
                              1, halign, valign, wmax, hmax);
          else
            render_ticktexts (yticks, yticklabels, y_min, y_max,
                              xpTick+signum (xpTick-xpTickN)*fx*ytickoffset,
                              zpTick, 1, halign, valign, wmax, hmax);
        }

      // minor grid lines
      if (do_yminorgrid)
        render_grid (minorgridstyle, ymticks, y_min, y_max,
                     xPlane, xPlaneN, layer2Dtop ? zPlaneN : zPlane,
                     zPlaneN, 1, (zstate != AXE_DEPTH_DIR));

      // minor tick marks
      if (do_yminortick)
        {
          if (tick_along_z)
            render_tickmarks (ymticks, y_min, y_max, xpTick, xpTick,
                              zpTick, zpTickN, 0., 0.,
                              signum (zpTick-zpTickN)*fz*yticklen/2,
                              1, mirror);
          else
            render_tickmarks (ymticks, y_min, y_max, xpTick, xpTickN,
                              zpTick, zpTick,
                              signum (xpTick-xpTickN)*fx*yticklen/2,
                              0., 0., 1, mirror);
        }

      gh_manager::get_object (props.get_ylabel ()).set ("visible", "on");
    }
  else
    gh_manager::get_object (props.get_ylabel ()).set ("visible", "off");
}

void
opengl_renderer::draw_axes_z_grid (const axes::properties& props)
{
  int zstate = props.get_zstate ();

  if (zstate != AXE_DEPTH_DIR && props.is_visible ())
    {
      bool xySym = props.get_xySym ();
      bool zSign = props.get_zSign ();
      double zticklen = props.get_zticklen ();
      double ztickoffset = props.get_ztickoffset ();
      double fx = props.get_fx ();
      double fy = props.get_fy ();
      double xPlane = props.get_xPlane ();
      double xPlaneN = props.get_xPlaneN ();
      double yPlane = props.get_yPlane ();
      double yPlaneN = props.get_yPlaneN ();
      double z_min = props.get_z_min ();
      double z_max = props.get_z_max ();

      // Z Grid

      std::string gridstyle = props.get_gridlinestyle ();
      std::string minorgridstyle = props.get_minorgridlinestyle ();
      bool do_zgrid = (props.is_zgrid () && (gridstyle != "none"));
      bool do_zminorgrid = (props.is_zminorgrid ()
                            && (minorgridstyle != "none"));
      bool do_zminortick = props.is_zminortick ();
      Matrix zticks = xform.zscale (props.get_ztick ().matrix_value ());
      Matrix zmticks = xform.zscale (props.get_zmtick ().matrix_value ());
      string_vector zticklabels = props.get_zticklabel ().all_strings ();
      int wmax = 0;
      int hmax = 0;
      bool mirror = props.is_box () && zstate != AXE_ANY_DIR;

      set_color (props.get_zcolor_rgb ());

      // grid lines
      if (do_zgrid)
        render_grid (gridstyle, zticks, z_min, z_max,
                     xPlane, xPlaneN, yPlane, yPlaneN, 2, true);

      // tick marks
      if (xySym)
        {
          if (xisinf (fy))
            render_tickmarks (zticks, z_min, z_max, xPlaneN, xPlane,
                              yPlane, yPlane,
                              signum (xPlaneN-xPlane)*fx*zticklen,
                              0., 0., 2, mirror);
          else
            render_tickmarks (zticks, z_min, z_max, xPlaneN, xPlaneN,
                              yPlane, yPlane, 0.,
                              signum (yPlane-yPlaneN)*fy*zticklen,
                              0., 2, false);
        }
      else
        {
          if (xisinf (fx))
            render_tickmarks (zticks, z_min, z_max, xPlaneN, xPlane,
                              yPlaneN, yPlane, 0.,
                              signum (yPlaneN-yPlane)*fy*zticklen,
                              0., 2, mirror);
          else
            render_tickmarks (zticks, z_min, z_max, xPlane, xPlane,
                              yPlaneN, yPlane,
                              signum (xPlane-xPlaneN)*fx*zticklen,
                              0., 0., 2, false);
        }

      // FIXME: tick texts
      if (zticklabels.numel () > 0)
        {
          int halign = 2;
          int valign = (zstate == AXE_VERT_DIR ? 1 : (zSign ? 3 : 2));

          if (xySym)
            {
              if (xisinf (fy))
                render_ticktexts (zticks, zticklabels, z_min, z_max,
                                  xPlaneN+signum (xPlaneN-xPlane)*fx*ztickoffset,
                                  yPlane, 2, halign, valign, wmax, hmax);
              else
                render_ticktexts (zticks, zticklabels, z_min, z_max, xPlaneN,
                                  yPlane+signum (yPlane-yPlaneN)*fy*ztickoffset,
                                  2, halign, valign, wmax, hmax);
            }
          else
            {
              if (xisinf (fx))
                render_ticktexts (zticks, zticklabels, z_min, z_max, xPlane,
                                  yPlaneN+signum (yPlaneN-yPlane)*fy*ztickoffset,
                                  2, halign, valign, wmax, hmax);
              else
                render_ticktexts (zticks, zticklabels, z_min, z_max,
                                  xPlane+signum (xPlane-xPlaneN)*fx*ztickoffset,
                                  yPlaneN, 2, halign, valign, wmax, hmax);
            }
        }

      // minor grid lines
      if (do_zminorgrid)
        render_grid (minorgridstyle, zmticks, z_min, z_max,
                     xPlane, xPlaneN, yPlane, yPlaneN, 2, true);

      // minor tick marks
      if (do_zminortick)
        {
          if (xySym)
            {
              if (xisinf (fy))
                render_tickmarks (zmticks, z_min, z_max, xPlaneN, xPlane,
                                  yPlane, yPlane,
                                  signum (xPlaneN-xPlane)*fx*zticklen/2,
                                  0., 0., 2, mirror);
              else
                render_tickmarks (zmticks, z_min, z_max, xPlaneN, xPlaneN,
                                  yPlane, yPlane, 0.,
                                  signum (yPlane-yPlaneN)*fy*zticklen/2,
                                  0., 2, false);
            }
          else
            {
              if (xisinf (fx))
                render_tickmarks (zmticks, z_min, z_max, xPlane, xPlane,
                                  yPlaneN, yPlane, 0.,
                                  signum (yPlaneN-yPlane)*fy*zticklen/2,
                                  0., 2, mirror);
              else
                render_tickmarks (zmticks, z_min, z_max, xPlane, xPlane,
                                  yPlaneN, yPlaneN,
                                  signum (xPlane-xPlaneN)*fx*zticklen/2,
                                  0., 0., 2, false);
            }
        }

      gh_manager::get_object (props.get_zlabel ()).set ("visible", "on");
    }
  else
    gh_manager::get_object (props.get_zlabel ()).set ("visible", "off");
}

void
opengl_renderer::draw_axes_children (const axes::properties& props)
{
  // Children

  Matrix children = props.get_all_children ();
  std::list<graphics_object> obj_list;
  std::list<graphics_object>::iterator it;

  // 1st pass: draw light objects

  // Start with the last element of the array of child objects to
  // display them in the order they were added to the array.

  for (octave_idx_type i = children.numel () - 1; i >= 0; i--)
    {
      graphics_object go = gh_manager::get_object (children(i));

      if (go.get_properties ().is_visible ())
        {
          if (go.isa ("light"))
            draw (go);
          else
            obj_list.push_back (go);
        }
    }

  // 2nd pass: draw other objects (with units set to "data")

  it = obj_list.begin ();
  while (it != obj_list.end ())
    {
      graphics_object go = (*it);

      // FIXME: check whether object has "units" property and it is set
      // to "data"
      if (! go.isa ("text") || go.get ("units").string_value () == "data")
        {
          set_clipping (go.get_properties ().is_clipping ());
          draw (go);

          it = obj_list.erase (it);
        }
      else
        it++;
    }

  // 3rd pass: draw remaining objects

  glDisable (GL_DEPTH_TEST);

  for (it = obj_list.begin (); it != obj_list.end (); it++)
    {
      graphics_object go = (*it);

      set_clipping (go.get_properties ().is_clipping ());
      draw (go);
    }

  glEnable (GL_DEPTH_TEST);

  set_clipping (false);

  // FIXME: finalize rendering (transparency processing)
  // FIXME: draw zoom box, if needed
}

void
opengl_renderer::draw_axes (const axes::properties& props)
{
  static double floatmax = std::numeric_limits<float>::max ();

  double x_min = props.get_x_min ();
  double x_max = props.get_x_max ();
  double y_min = props.get_y_min ();
  double y_max = props.get_y_max ();
  double z_min = props.get_z_min ();
  double z_max = props.get_z_max ();

  if (x_max > floatmax || y_max > floatmax || z_max > floatmax
      || x_min < -floatmax || y_min < -floatmax || z_min < -floatmax)
    {
      warning ("opengl_renderer: data values greater than float capacity.  (1) Scale data, or (2) Use gnuplot");
      return;
    }

  setup_opengl_transformation (props);

  // Disable line smoothing for axes
  GLboolean antialias;
  glGetBooleanv (GL_LINE_SMOOTH, &antialias);
  if (antialias == GL_TRUE)
    glDisable (GL_LINE_SMOOTH);

  // draw axes object

  draw_axes_planes (props);
  draw_axes_boxes (props);

  set_font (props);

  draw_axes_x_grid (props);
  draw_axes_y_grid (props);
  draw_axes_z_grid (props);

  set_linestyle ("-");

  set_clipbox (x_min, x_max, y_min, y_max, z_min, z_max);

  // Re-enable line smoothing for children
  if (antialias == GL_TRUE)
    glEnable (GL_LINE_SMOOTH);

  draw_axes_children (props);
}

void
opengl_renderer::draw_line (const line::properties& props)
{
  Matrix x = xform.xscale (props.get_xdata ().matrix_value ());
  Matrix y = xform.yscale (props.get_ydata ().matrix_value ());
  Matrix z = xform.zscale (props.get_zdata ().matrix_value ());

  bool has_z = (z.numel () > 0);
  int n = static_cast<int> (std::min (std::min (x.numel (), y.numel ()),
                                      (has_z ? z.numel ()
                                             : std::numeric_limits<int>::max ())));
  octave_uint8 clip_mask = (props.is_clipping () ? 0x7F : 0x40), clip_ok (0x40);

  std::vector<octave_uint8> clip (n);

  if (has_z)
    for (int i = 0; i < n; i++)
      clip[i] = (clip_code (x(i), y(i), z(i)) & clip_mask);
  else
    {
      double z_mid = (zmin+zmax)/2;

      for (int i = 0; i < n; i++)
        clip[i] = (clip_code (x(i), y(i), z_mid) & clip_mask);
    }

  if (! props.linestyle_is ("none"))
    {
      set_color (props.get_color_rgb ());
      set_linestyle (props.get_linestyle (), false);
      set_linewidth (props.get_linewidth ());

      if (has_z)
        {
          bool flag = false;

          for (int i = 1; i < n; i++)
            {
              if ((clip[i-1] & clip[i]) == clip_ok)
                {
                  if (! flag)
                    {
                      flag = true;
                      glBegin (GL_LINE_STRIP);
                      glVertex3d (x(i-1), y(i-1), z(i-1));
                    }
                  glVertex3d (x(i), y(i), z(i));
                }
              else if (flag)
                {
                  flag = false;
                  glEnd ();
                }
            }

          if (flag)
            glEnd ();
        }
      else
        {
          bool flag = false;

          for (int i = 1; i < n; i++)
            {
              if ((clip[i-1] & clip[i]) == clip_ok)
                {
                  if (! flag)
                    {
                      flag = true;
                      glBegin (GL_LINE_STRIP);
                      glVertex2d (x(i-1), y(i-1));
                    }
                  glVertex2d (x(i), y(i));
                }
              else if (flag)
                {
                  flag = false;
                  glEnd ();
                }
            }

          if (flag)
            glEnd ();
        }

      set_linewidth (0.5);
      set_linestyle ("-");
    }

  set_clipping (false);

  if (! props.marker_is ("none")
      && ! (props.markeredgecolor_is ("none")
            && props.markerfacecolor_is ("none")))
    {
      Matrix lc, fc;

      if (props.markeredgecolor_is ("auto"))
        lc = props.get_color_rgb ();
      else if (! props.markeredgecolor_is ("none"))
        lc = props.get_markeredgecolor_rgb ();

      if (props.markerfacecolor_is ("auto"))
        fc = props.get_color_rgb ();
      else if (! props.markerfacecolor_is ("none"))
        fc = props.get_markerfacecolor_rgb ();

      init_marker (props.get_marker (), props.get_markersize (),
                   props.get_linewidth ());

      for (int i = 0; i < n; i++)
        {
          if (clip[i] == clip_ok)
            draw_marker (x(i), y(i),
                         has_z ? z(i) : 0.0,
                         lc, fc);
        }

      end_marker ();
    }

  set_clipping (props.is_clipping ());
}

void
opengl_renderer::draw_surface (const surface::properties& props)
{
  const Matrix x = xform.xscale (props.get_xdata ().matrix_value ());
  const Matrix y = xform.yscale (props.get_ydata ().matrix_value ());
  const Matrix z = xform.zscale (props.get_zdata ().matrix_value ());

  int zr = z.rows ();
  int zc = z.columns ();

  NDArray c;
  const NDArray n = props.get_vertexnormals ().array_value ();

  // FIXME: handle transparency
  Matrix a;

  if (props.facelighting_is ("phong") || props.edgelighting_is ("phong"))
    warning ("opengl_renderer: phong light model not supported");

  int fc_mode = (props.facecolor_is_rgb () ? 0 :
                 (props.facecolor_is ("flat") ? 1 :
                  (props.facecolor_is ("interp") ? 2 :
                   (props.facecolor_is ("texturemap") ? 3 : -1))));
  int fl_mode = (props.facelighting_is ("none") ? 0 :
                 (props.facelighting_is ("flat") ? 1 : 2));
  int fa_mode = (props.facealpha_is_double () ? 0 :
                 (props.facealpha_is ("flat") ? 1 : 2));
  int ec_mode = (props.edgecolor_is_rgb () ? 0 :
                 (props.edgecolor_is ("flat") ? 1 :
                  (props.edgecolor_is ("interp") ? 2 : -1)));
  int el_mode = (props.edgelighting_is ("none") ? 0 :
                 (props.edgelighting_is ("flat") ? 1 : 2));
  int ea_mode = (props.edgealpha_is_double () ? 0 :
                 (props.edgealpha_is ("flat") ? 1 : 2));

  Matrix fcolor = (fc_mode == TEXTURE ? Matrix (1, 3, 1.0)
                                      : props.get_facecolor_rgb ());
  Matrix ecolor = props.get_edgecolor_rgb ();

  float as = props.get_ambientstrength ();
  float ds = props.get_diffusestrength ();
  float ss = props.get_specularstrength ();
  float se = props.get_specularexponent ();
  float cb[4] = { 0.0, 0.0, 0.0, 1.0 };
  double d = 1.0;

  opengl_texture tex;

  int i1, i2, j1, j2;
  bool x_mat = (x.rows () == z.rows ());
  bool y_mat = (y.columns () == z.columns ());

  i1 = i2 = j1 = j2 = 0;

  if ((fc_mode > 0 && fc_mode < 3) || ec_mode > 0)
    c = props.get_color_data ().array_value ();

  boolMatrix clip (z.dims (), false);

  for (int i = 0; i < zr; i++)
    {
      if (x_mat)
        i1 = i;

      for (int j = 0; j < zc; j++)
        {
          if (y_mat)
            j1 = j;

          clip(i,j) = is_nan_or_inf (x(i1,j), y(i,j1), z(i,j));
        }
    }

  if (fa_mode > 0 || ea_mode > 0)
    {
      // FIXME: implement alphadata conversion
      //a = props.get_alpha_data ();
    }

  if (fl_mode > 0 || el_mode > 0)
    {
      float buf[4] = { ss, ss, ss, 1 };

      glMaterialfv (LIGHT_MODE, GL_SPECULAR, buf);
      glMaterialf (LIGHT_MODE, GL_SHININESS, se);
    }

  // FIXME: good candidate for caching,
  //        transferring pixel data to OpenGL is time consuming.
  if (fc_mode == TEXTURE)
    tex = opengl_texture::create (props.get_color_data ());

  if (! props.facecolor_is ("none"))
    {
      if (props.get_facealpha_double () == 1)
        {
          if (fc_mode == UNIFORM || fc_mode == TEXTURE)
            {
              glColor3dv (fcolor.data ());
              if (fl_mode > 0)
                {
                  for (int i = 0; i < 3; i++)
                    cb[i] = as * fcolor(i);
                  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * fcolor(i);
                  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                }
            }

          if (fl_mode > 0)
            glEnable (GL_LIGHTING);
          glShadeModel ((fc_mode == INTERP || fl_mode == GOURAUD) ? GL_SMOOTH
                                                                  : GL_FLAT);
          set_polygon_offset (true, 1);
          if (fc_mode == TEXTURE)
            glEnable (GL_TEXTURE_2D);

          for (int i = 1; i < zc; i++)
            {
              if (y_mat)
                {
                  i1 = i-1;
                  i2 = i;
                }

              for (int j = 1; j < zr; j++)
                {

                  if (clip(j-1, i-1) || clip(j, i-1)
                      || clip(j-1, i) || clip(j, i))
                    continue;

                  if (fc_mode == FLAT)
                    {
                      // "flat" only needs color at lower-left vertex
                      if (! xfinite (c(j-1,i-1)))
                        continue;
                    }
                  else if (fc_mode == INTERP)
                    {
                      // "interp" needs valid color at all 4 vertices
                      if (! (xfinite (c(j-1, i-1)) && xfinite (c(j, i-1))
                             && xfinite (c(j-1, i)) && xfinite (c(j, i))))
                        continue;
                    }

                  if (x_mat)
                    {
                      j1 = j-1;
                      j2 = j;
                    }

                  glBegin (GL_QUADS);

                  // Vertex 1
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i-1) / (zc-1),
                                   double (j-1) / (zr-1));
                  else if (fc_mode > 0)
                    {
                      // FIXME: is there a smarter way to do this?
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j-1, i-1, k);
                      glColor3fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j-1, i-1, k);
                          glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                        }
                    }
                  if (fl_mode > 0)
                    {
                      d = sqrt (n(j-1,i-1,0) * n(j-1,i-1,0)
                                + n(j-1,i-1,1) * n(j-1,i-1,1)
                                + n(j-1,i-1,2) * n(j-1,i-1,2));
                      glNormal3d (n(j-1,i-1,0)/d,
                                  n(j-1,i-1,1)/d,
                                  n(j-1,i-1,2)/d);
                    }
                  glVertex3d (x(j1,i-1), y(j-1,i1), z(j-1,i-1));

                  // Vertex 2
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i) / (zc-1), double (j-1) / (zr-1));
                  else if (fc_mode == INTERP)
                    {
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j-1, i, k);
                      glColor3fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j-1, i, k);
                          glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                        }
                    }

                  if (fl_mode == GOURAUD)
                    {
                      d = sqrt (n(j-1,i,0) * n(j-1,i,0)
                                + n(j-1,i,1) * n(j-1,i,1)
                                + n(j-1,i,2) * n(j-1,i,2));
                      glNormal3d (n(j-1,i,0)/d, n(j-1,i,1)/d, n(j-1,i,2)/d);
                    }

                  glVertex3d (x(j1,i), y(j-1,i2), z(j-1,i));

                  // Vertex 3
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i) / (zc-1), double (j) / (zr-1));
                  else if (fc_mode == INTERP)
                    {
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j, i, k);
                      glColor3fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j, i, k);
                          glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                        }
                    }
                  if (fl_mode == GOURAUD)
                    {
                      d = sqrt (n(j,i,0) * n(j,i,0)
                                + n(j,i,1) * n(j,i,1)
                                + n(j,i,2) * n(j,i,2));
                      glNormal3d (n(j,i,0)/d, n(j,i,1)/d, n(j,i,2)/d);
                    }
                  glVertex3d (x(j2,i), y(j,i2), z(j,i));

                  // Vertex 4
                  if (fc_mode == TEXTURE)
                    tex.tex_coord (double (i-1) / (zc-1), double (j) / (zr-1));
                  else if (fc_mode == INTERP)
                    {
                      for (int k = 0; k < 3; k++)
                        cb[k] = c(j, i-1, k);
                      glColor3fv (cb);

                      if (fl_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] *= as;
                          glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                          for (int k = 0; k < 3; k++)
                            cb[k] = ds * c(j, i-1, k);
                          glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                        }
                    }
                  if (fl_mode == GOURAUD)
                    {
                      d = sqrt (n(j,i-1,0) * n(j,i-1,0)
                                + n(j,i-1,1) * n(j,i-1,1)
                                + n(j,i-1,2) * n(j,i-1,2));
                      glNormal3d (n(j,i-1,0)/d, n(j,i-1,1)/d, n(j,i-1,2)/d);
                    }
                  glVertex3d (x(j2,i-1), y(j,i1), z(j,i-1));

                  glEnd ();
                }
            }

          set_polygon_offset (false);
          if (fc_mode == TEXTURE)
            glDisable (GL_TEXTURE_2D);

          if (fl_mode > 0)
            glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement transparency
        }
    }

  if (! props.edgecolor_is ("none"))
    {
      if (props.get_edgealpha_double () == 1)
        {
          if (ec_mode == UNIFORM)
            {
              glColor3dv (ecolor.data ());
              if (fl_mode > 0)
                {
                  for (int i = 0; i < 3; i++)
                    cb[i] = as * ecolor(i);
                  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * ecolor(i);
                  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                }
            }

          if (el_mode > 0)
            glEnable (GL_LIGHTING);
          glShadeModel ((ec_mode == INTERP || el_mode == GOURAUD) ? GL_SMOOTH
                                                                  : GL_FLAT);

          set_linestyle (props.get_linestyle (), false);
          set_linewidth (props.get_linewidth ());

          // Mesh along Y-axis

          if (props.meshstyle_is ("both") || props.meshstyle_is ("column"))
            {
              for (int i = 0; i < zc; i++)
                {
                  if (y_mat)
                    {
                      i1 = i-1;
                      i2 = i;
                    }

                  for (int j = 1; j < zr; j++)
                    {
                      if (clip(j-1,i) || clip(j,i))
                        continue;

                      if (ec_mode == FLAT)
                        {
                          // "flat" only needs color at lower-left vertex
                          if (! xfinite (c(j-1,i)))
                            continue;
                        }
                      else if (ec_mode == INTERP)
                        {
                          // "interp" needs valid color at both vertices
                          if (! (xfinite (c(j-1, i)) && xfinite (c(j, i))))
                            continue;
                        }

                      if (x_mat)
                        {
                          j1 = j-1;
                          j2 = j;
                        }

                      glBegin (GL_LINES);

                      // Vertex 1
                      if (ec_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j-1, i, k);
                          glColor3fv (cb);

                          if (fl_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j-1, i, k);
                              glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                            }
                        }
                      if (el_mode > 0)
                        {
                          d = sqrt (n(j-1,i,0) * n(j-1,i,0)
                                    + n(j-1,i,1) * n(j-1,i,1)
                                    + n(j-1,i,2) * n(j-1,i,2));
                          glNormal3d (n(j-1,i,0)/d, n(j-1,i,1)/d, n(j-1,i,2)/d);
                        }
                      glVertex3d (x(j1,i), y(j-1,i2), z(j-1,i));

                      // Vertex 2
                      if (ec_mode == INTERP)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j, i, k);
                          glColor3fv (cb);

                          if (fl_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j, i, k);
                              glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                            }
                        }
                      if (el_mode == GOURAUD)
                        {
                          d = sqrt (n(j,i,0) * n(j,i,0)
                                    + n(j,i,1) * n(j,i,1)
                                    + n(j,i,2) * n(j,i,2));
                          glNormal3d (n(j,i,0)/d, n(j,i,1)/d, n(j,i,2)/d);
                        }
                      glVertex3d (x(j2,i), y(j,i2), z(j,i));

                      glEnd ();
                    }
                }
            }

          // Mesh along X-axis

          if (props.meshstyle_is ("both") || props.meshstyle_is ("row"))
            {
              for (int j = 0; j < zr; j++)
                {
                  if (x_mat)
                    {
                      j1 = j-1;
                      j2 = j;
                    }

                  for (int i = 1; i < zc; i++)
                    {
                      if (clip(j,i-1) || clip(j,i))
                        continue;

                      if (ec_mode == FLAT)
                        {
                          // "flat" only needs color at lower-left vertex
                          if (! xfinite (c(j,i-1)))
                            continue;
                        }
                      else if (ec_mode == INTERP)
                        {
                          // "interp" needs valid color at both vertices
                          if (! (xfinite (c(j, i-1)) && xfinite (c(j, i))))
                            continue;
                        }

                      if (y_mat)
                        {
                          i1 = i-1;
                          i2 = i;
                        }

                      glBegin (GL_LINES);

                      // Vertex 1
                      if (ec_mode > 0)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j, i-1, k);
                          glColor3fv (cb);

                          if (fl_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j, i-1, k);
                              glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                            }
                        }
                      if (el_mode > 0)
                        {
                          d = sqrt (n(j,i-1,0) * n(j,i-1,0)
                                    + n(j,i-1,1) * n(j,i-1,1)
                                    + n(j,i-1,2) * n(j,i-1,2));
                          glNormal3d (n(j,i-1,0)/d, n(j,i-1,1)/d, n(j,i-1,2)/d);
                        }
                      glVertex3d (x(j2,i-1), y(j,i1), z(j,i-1));

                      // Vertex 2
                      if (ec_mode == INTERP)
                        {
                          for (int k = 0; k < 3; k++)
                            cb[k] = c(j, i, k);
                          glColor3fv (cb);

                          if (fl_mode > 0)
                            {
                              for (int k = 0; k < 3; k++)
                                cb[k] *= as;
                              glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = ds * c(j, i, k);
                              glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                            }
                        }
                      if (el_mode == GOURAUD)
                        {
                          d = sqrt (n(j,i,0) * n(j,i,0)
                                    + n(j,i,1) * n(j,i,1)
                                    + n(j,i,2) * n(j,i,2));
                          glNormal3d (n(j,i,0)/d, n(j,i,1)/d, n(j,i,2)/d);
                        }
                      glVertex3d (x(j2,i), y(j,i2), z(j,i));

                      glEnd ();
                    }
                }
            }

          set_linestyle ("-");
          set_linewidth (0.5);

          if (el_mode > 0)
            glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement transparency
        }
    }

  if (! props.marker_is ("none")
      && ! (props.markeredgecolor_is ("none")
            && props.markerfacecolor_is ("none")))
    {
      // FIXME: check how transparency should be handled in markers
      // FIXME: check what to do with marker facecolor set to auto
      //        and facecolor set to none.

      bool do_edge = ! props.markeredgecolor_is ("none");
      bool do_face = ! props.markerfacecolor_is ("none");

      Matrix mecolor = props.get_markeredgecolor_rgb ();
      Matrix mfcolor = props.get_markerfacecolor_rgb ();
      Matrix cc (1, 3, 0.0);

      if (mecolor.numel () == 0 && props.markeredgecolor_is ("auto"))
        {
          mecolor = props.get_edgecolor_rgb ();
          do_edge = ! props.edgecolor_is ("none");
        }

      if (mfcolor.numel () == 0 && props.markerfacecolor_is ("auto"))
        {
          mfcolor = props.get_facecolor_rgb ();
          do_face = ! props.facecolor_is ("none");
        }

      if ((mecolor.numel () == 0 || mfcolor.numel () == 0)
          && c.numel () == 0)
        c = props.get_color_data ().array_value ();

      init_marker (props.get_marker (), props.get_markersize (),
                   props.get_linewidth ());

      for (int i = 0; i < zc; i++)
        {
          if (y_mat)
            i1 = i;

          for (int j = 0; j < zr; j++)
            {
              if (clip(j,i))
                continue;

              if (x_mat)
                j1 = j;

              if ((do_edge && mecolor.numel () == 0)
                  || (do_face && mfcolor.numel () == 0))
                {
                  if (! xfinite (c(j,i)))
                    continue;  // Skip NaNs in color data

                  for (int k = 0; k < 3; k++)
                    cc(k) = c(j,i,k);
                }

              Matrix lc = (do_edge ? (mecolor.numel () == 0 ? cc : mecolor)
                                   : Matrix ());
              Matrix fc = (do_face ? (mfcolor.numel () == 0 ? cc : mfcolor)
                                   : Matrix ());

              draw_marker (x(j1,i), y(j,i1), z(j,i), lc, fc);
            }
        }

      end_marker ();
    }
}

// FIXME: global optimization (rendering, data structures...),
// there is probably a smarter/faster/less-memory-consuming way to do this.
void
opengl_renderer::draw_patch (const patch::properties &props)
{
  // Do not render if the patch has incoherent data
  std::string msg;
  if (props.has_bad_data (msg))
    {
      warning ("opengl_renderer: %s. Not rendering.", msg.c_str ());
      return;
    }

  const Matrix f = props.get_faces ().matrix_value ();
  const Matrix v = xform.scale (props.get_vertices ().matrix_value ());
  Matrix c;
  const Matrix n = props.get_vertexnormals ().matrix_value ();
  Matrix a;

  int nv = v.rows ();
  int nf = f.rows ();
  int fcmax = f.columns ();

  bool has_z = (v.columns () > 2);
  bool has_facecolor = false;
  bool has_facealpha = false;

  int fc_mode = ((props.facecolor_is ("none")
                  || props.facecolor_is_rgb ()) ? 0 :
                 (props.facecolor_is ("flat") ? 1 : 2));
  int fl_mode = (props.facelighting_is ("none") ? 0 :
                 (props.facelighting_is ("flat") ? 1 : 2));
  int fa_mode = (props.facealpha_is_double () ? 0 :
                 (props.facealpha_is ("flat") ? 1 : 2));
  int ec_mode = ((props.edgecolor_is ("none")
                  || props.edgecolor_is_rgb ()) ? 0 :
                 (props.edgecolor_is ("flat") ? 1 : 2));
  int el_mode = (props.edgelighting_is ("none") ? 0 :
                 (props.edgelighting_is ("flat") ? 1 : 2));
  int ea_mode = (props.edgealpha_is_double () ? 0 :
                 (props.edgealpha_is ("flat") ? 1 : 2));

  Matrix fcolor = props.get_facecolor_rgb ();
  Matrix ecolor = props.get_edgecolor_rgb ();

  float as = props.get_ambientstrength ();
  float ds = props.get_diffusestrength ();
  float ss = props.get_specularstrength ();
  float se = props.get_specularexponent ();

  boolMatrix clip (1, nv, false);

  if (has_z)
    for (int i = 0; i < nv; i++)
      clip(i) = is_nan_or_inf (v(i,0), v(i,1), v(i,2));
  else
    for (int i = 0; i < nv; i++)
      clip(i) = is_nan_or_inf (v(i,0), v(i,1), 0);

  boolMatrix clip_f (1, nf, false);
  Array<int> count_f (dim_vector (nf, 1), 0);

  for (int i = 0; i < nf; i++)
    {
      bool fclip = false;
      int count = 0;

      for (int j = 0; j < fcmax && ! xisnan (f(i,j)); j++, count++)
        fclip = (fclip || clip(int (f(i,j) - 1)));

      clip_f(i) = fclip;
      count_f(i) = count;
    }

  if (fc_mode > 0 || ec_mode > 0)
    {
      c = props.get_color_data ().matrix_value ();

      if (c.rows () == 1)
        {
          // Single color specifications, we can simplify a little bit

          if (fc_mode > 0)
            {
              fcolor = c;
              fc_mode = UNIFORM;
            }

          if (ec_mode > 0)
            {
              ecolor = c;
              ec_mode = UNIFORM;
            }

          c = Matrix ();
        }
      else
        has_facecolor = ((c.numel () > 0) && (c.rows () == f.rows ()));
    }

  if (fa_mode > 0 || ea_mode > 0)
    {
      // FIXME: retrieve alpha data from patch object
      //a = props.get_alpha_data ();
      has_facealpha = ((a.numel () > 0) && (a.rows () == f.rows ()));
    }

  octave_idx_type fr = f.rows ();
  std::vector<vertex_data> vdata (f.numel ());

  for (int i = 0; i < nf; i++)
    for (int j = 0; j < count_f(i); j++)
      {
        int idx = int (f(i,j) - 1);

        Matrix vv (1, 3, 0.0);
        Matrix cc;
        Matrix nn (1, 3, 0.0);
        double aa = 1.0;

        vv(0) = v(idx,0); vv(1) = v(idx,1);
        if (has_z)
          vv(2) = v(idx,2);
        // FIXME: uncomment when patch object has normal computation
        //nn(0) = n(idx,0); nn(1) = n(idx,1); nn(2) = n(idx,2);
        if (c.numel () > 0)
          {
            cc.resize (1, 3);
            if (has_facecolor)
              cc(0) = c(i,0), cc(1) = c(i,1), cc(2) = c(i,2);
            else
              cc(0) = c(idx,0), cc(1) = c(idx,1), cc(2) = c(idx,2);
          }
        if (a.numel () > 0)
          {
            if (has_facealpha)
              aa = a(i);
            else
              aa = a(idx);
          }

        vdata[i+j*fr] = vertex_data (vv, cc, nn, aa, as, ds, ss, se);
      }

  if (fl_mode > 0 || el_mode > 0)
    {
      float buf[4] = { ss, ss, ss, 1 };

      glMaterialfv (LIGHT_MODE, GL_SPECULAR, buf);
      glMaterialf (LIGHT_MODE, GL_SHININESS, se);
    }

  if (! props.facecolor_is ("none"))
    {
      // FIXME: adapt to double-radio property
      if (props.get_facealpha_double () == 1)
        {
          if (fc_mode == UNIFORM)
            {
              glColor3dv (fcolor.data ());
              if (fl_mode > 0)
                {
                  float cb[4] = { 0, 0, 0, 1 };

                  for (int i = 0; i < 3; i++)
                    cb[i] = (as * fcolor(i));
                  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * fcolor(i);
                  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                }
            }

          if (fl_mode > 0)
            glEnable (GL_LIGHTING);

          // NOTE: Push filled part of patch backwards to avoid Z-fighting with
          // tesselator outline.  A value of 1.0 seems to work fine.  Value
          // can't be too large or the patch will be pushed below the axes
          // planes at +2.5.
          patch_tesselator tess (this, fc_mode, fl_mode, 1.0);

          for (int i = 0; i < nf; i++)
            {
              if (clip_f(i))
                continue;

              tess.begin_polygon (true);
              tess.begin_contour ();

              // Add vertices in reverse order for Matlab compatibility
              for (int j = count_f(i)-1; j > 0; j--)
                {
                  vertex_data::vertex_data_rep *vv = vdata[i+j*fr].get_rep ();

                  tess.add_vertex (vv->coords.fortran_vec (), vv);
                }

              if (count_f(i) > 0)
                {
                  vertex_data::vertex_data_rep *vv = vdata[i].get_rep ();

                  if (fc_mode == FLAT)
                    {
                      // For "flat" shading, use color of 1st vertex.
                      Matrix col = vv->color;

                      if (col.numel () == 3)
                        {
                          glColor3dv (col.data ());
                          if (fl_mode > 0)
                            {
                              float cb[4] = { 0, 0, 0, 1 };

                              for (int k = 0; k < 3; k++)
                                cb[k] = (vv->ambient * col(k));
                              glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                              for (int k = 0; k < 3; k++)
                                cb[k] = (vv->diffuse * col(k));
                              glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                            }
                        }
                    }

                  tess.add_vertex (vv->coords.fortran_vec (), vv);
                }

              tess.end_contour ();
              tess.end_polygon ();
            }

          if (fl_mode > 0)
            glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement transparency
        }
    }

  if (! props.edgecolor_is ("none"))
    {
      // FIXME: adapt to double-radio property
      if (props.get_edgealpha_double () == 1)
        {
          if (ec_mode == UNIFORM)
            {
              glColor3dv (ecolor.data ());
              if (el_mode > 0)
                {
                  float cb[4] = { 0, 0, 0, 1 };

                  for (int i = 0; i < 3; i++)
                    cb[i] = (as * ecolor(i));
                  glMaterialfv (LIGHT_MODE, GL_AMBIENT, cb);

                  for (int i = 0; i < 3; i++)
                    cb[i] = ds * ecolor(i);
                  glMaterialfv (LIGHT_MODE, GL_DIFFUSE, cb);
                }
            }

          if (el_mode > 0)
            glEnable (GL_LIGHTING);

          set_linestyle (props.get_linestyle (), false);
          set_linewidth (props.get_linewidth ());

          // NOTE: patch contour cannot be offset.  Offset must occur with the
          // filled portion of the patch above.  The tesselator uses
          // GLU_TESS_BOUNDARY_ONLY to get the outline of the patch and OpenGL
          // automatically sets the glType to GL_LINE_LOOP.  This primitive is
          // not supported by glPolygonOffset which is used to do Z offsets.
          patch_tesselator tess (this, ec_mode, el_mode);

          for (int i = 0; i < nf; i++)
            {
              if (clip_f(i))
                {
                  // This is an unclosed contour.  Draw it as a line.
                  bool flag = false;

                  glShadeModel ((ec_mode == INTERP || el_mode == GOURAUD)
                                ? GL_SMOOTH : GL_FLAT);

                  // Add vertices in reverse order for Matlab compatibility
                  for (int j = count_f(i)-1; j >= 0; j--)
                    {
                      if (! clip(int (f(i,j) - 1)))
                        {
                          vertex_data::vertex_data_rep *vv
                            = vdata[i+j*fr].get_rep ();
                          const Matrix m = vv->coords;
                          if (! flag)
                            {
                              flag = true;
                              glBegin (GL_LINE_STRIP);
                            }
                          if (ec_mode != UNIFORM)
                            {
                              Matrix col = vv->color;

                              if (col.numel () == 3)
                                glColor3dv (col.data ());
                            }
                          glVertex3d (m(0), m(1), m(2));
                        }
                      else if (flag)
                        {
                          flag = false;
                          glEnd ();
                        }
                    }
                  // Do loop body with vertex N to "close" GL_LINE_STRIP
                  // from vertex 0 to vertex N.
                  int j = count_f(i)-1;
                  if (flag && ! clip(int (f(i,j) - 1)))
                    {
                      vertex_data::vertex_data_rep *vv
                        = vdata[i+j*fr].get_rep ();
                      const Matrix m = vv->coords;
                      if (ec_mode != UNIFORM)
                        {
                          Matrix col = vv->color;

                          if (col.numel () == 3)
                            glColor3dv (col.data ());
                        }
                      glVertex3d (m(0), m(1), m(2));
                    }

                  if (flag)
                    glEnd ();
                }
              else  // Normal edge contour drawn with tesselator
                {
                  tess.begin_polygon (false);
                  tess.begin_contour ();

                  for (int j = count_f(i)-1; j >= 0; j--)
                    {
                      vertex_data::vertex_data_rep *vv
                        = vdata[i+j*fr].get_rep ();
                      tess.add_vertex (vv->coords.fortran_vec (), vv);
                    }

                  tess.end_contour ();
                  tess.end_polygon ();
                }
            }

          set_linestyle ("-");
          set_linewidth (0.5);

          if (el_mode > 0)
            glDisable (GL_LIGHTING);
        }
      else
        {
          // FIXME: implement transparency
        }
    }

  if (! props.marker_is ("none")
      && ! (props.markeredgecolor_is ("none")
            && props.markerfacecolor_is ("none")))
    {
      bool do_edge = ! props.markeredgecolor_is ("none");
      bool do_face = ! props.markerfacecolor_is ("none");

      Matrix mecolor = props.get_markeredgecolor_rgb ();
      Matrix mfcolor = props.get_markerfacecolor_rgb ();

      bool has_markerfacecolor = false;

      if ((mecolor.numel () == 0 && ! props.markeredgecolor_is ("none"))
          || (mfcolor.numel () == 0 && ! props.markerfacecolor_is ("none")))
        {
          Matrix mc = props.get_color_data ().matrix_value ();

          if (mc.rows () == 1)
            {
              // Single color specifications, we can simplify a little bit

              if (mfcolor.numel () == 0
                  && ! props.markerfacecolor_is ("none"))
                mfcolor = mc;

              if (mecolor.numel () == 0
                  && ! props.markeredgecolor_is ("none"))
                mecolor = mc;
            }
          else
            {
              if (c.numel () == 0)
                c = props.get_color_data ().matrix_value ();
              has_markerfacecolor = ((c.numel () > 0)
                                     && (c.rows () == f.rows ()));
            }
        }


      init_marker (props.get_marker (), props.get_markersize (),
                   props.get_linewidth ());

      for (int i = 0; i < nf; i++)
        for (int j = 0; j < count_f(i); j++)
          {
            int idx = int (f(i,j) - 1);

            if (clip(idx))
              continue;

            Matrix cc;
            if (c.numel () > 0)
              {
                cc.resize (1, 3);
                if (has_markerfacecolor)
                  cc(0) = c(i,0), cc(1) = c(i,1), cc(2) = c(i,2);
                else
                  cc(0) = c(idx,0), cc(1) = c(idx,1), cc(2) = c(idx,2);
              }

            Matrix lc = (do_edge ? (mecolor.numel () == 0 ? cc : mecolor)
                                 : Matrix ());
            Matrix fc = (do_face ? (mfcolor.numel () == 0 ? cc : mfcolor)
                                 : Matrix ());

            draw_marker (v(idx,0), v(idx,1), (has_z ? v(idx,2) : 0), lc, fc);
          }

      end_marker ();
    }
}

void
opengl_renderer::draw_hggroup (const hggroup::properties &props)
{
  draw (props.get_children ());
}

void
opengl_renderer::draw_text (const text::properties& props)
{
  if (props.get_string ().is_empty ())
    return;

  set_font (props);

  Matrix pos = xform.scale (props.get_data_position ());
  const Matrix bbox = props.get_extent_matrix ();

  // FIXME: handle margin and surrounding box
  bool blend = glIsEnabled (GL_BLEND);

  glEnable (GL_BLEND);
  glEnable (GL_ALPHA_TEST);
  glRasterPos3d (pos(0), pos(1), pos.numel () > 2 ? pos(2) : 0.0);
  glBitmap (0, 0, 0, 0, bbox(0), bbox(1), 0);
  glDrawPixels (bbox(2), bbox(3),
                GL_RGBA, GL_UNSIGNED_BYTE, props.get_pixels ().data ());
  glDisable (GL_ALPHA_TEST);
  if (! blend)
    glDisable (GL_BLEND);

}

void
opengl_renderer::draw_image (const image::properties& props)
{
  octave_value cdata = props.get_color_data ();
  dim_vector dv (cdata.dims ());
  int h = dv(0);
  int w = dv(1);

  Matrix x = props.get_xdata ().matrix_value ();
  Matrix y = props.get_ydata ().matrix_value ();

  // Someone wants us to draw an empty image? No way.
  if (x.is_empty () || y.is_empty ())
    return;

  if (w > 1 && x(1) == x(0))
    x(1) = x(1) + (w-1);

  if (h > 1 && y(1) == y(0))
    y(1) = y(1) + (h-1);

  const ColumnVector p0 = xform.transform (x(0), y(0), 0);
  const ColumnVector p1 = xform.transform (x(1), y(1), 0);

  if (xisnan (p0(0)) || xisnan (p0(1)) || xisnan (p1(0)) || xisnan (p1(1)))
    {
      warning ("opengl_renderer: image X,Y data too large to draw");
      return;
    }

  // image pixel size in screen pixel units
  float pix_dx, pix_dy;
  // image pixel size in normalized units
  float nor_dx, nor_dy;

  if (w > 1)
    {
      pix_dx = (p1(0) - p0(0))/(w-1);
      nor_dx = (x(1) - x(0))/(w-1);
    }
  else
    {
      const ColumnVector p1w = xform.transform (x(1) + 1, y(1), 0);
      pix_dx = p1w(0) - p0(0);
      nor_dx = 1;
    }

  if (h > 1)
    {
      pix_dy = (p1(1) - p0(1))/(h-1);
      nor_dy = (y(1) - y(0))/(h-1);
    }
  else
    {
      const ColumnVector p1h = xform.transform (x(1), y(1) + 1, 0);
      pix_dy = p1h(1) - p0(1);
      nor_dy = 1;
    }

  // OpenGL won't draw any of the image if it's origin is outside the
  // viewport/clipping plane so we must do the clipping ourselves.

  int j0, j1, i0, i1;
  j0 = 0, j1 = w;
  i0 = 0, i1 = h;

  float im_xmin = x(0) - nor_dx/2;
  float im_xmax = x(1) + nor_dx/2;
  float im_ymin = y(0) - nor_dy/2;
  float im_ymax = y(1) + nor_dy/2;
  if (props.is_clipping ()) // clip to axes
    {
      if (im_xmin < xmin)
        j0 += (xmin - im_xmin)/nor_dx + 1;
      if (im_xmax > xmax)
        j1 -= (im_xmax - xmax)/nor_dx ;

      if (im_ymin < ymin)
        i0 += (ymin - im_ymin)/nor_dy + 1;
      if (im_ymax > ymax)
        i1 -= (im_ymax - ymax)/nor_dy;
    }
  else // clip to viewport
    {
      GLfloat vp[4];
      glGetFloatv (GL_VIEWPORT, vp);
      // FIXME: actually add the code to do it!

    }

  if (i0 >= i1 || j0 >= j1)
    return;

  glPixelZoom (pix_dx, -pix_dy);
  glRasterPos3d (im_xmin + nor_dx*j0, im_ymin + nor_dy*i0, 0);

  // by default this is 4
  glPixelStorei (GL_UNPACK_ALIGNMENT, 1);

  // Expect RGB data
  if (dv.length () == 3 && dv(2) == 3)
    {
      if (cdata.is_double_type ())
        {
          const NDArray xcdata = cdata.array_value ();

          OCTAVE_LOCAL_BUFFER (GLfloat, a, 3*(j1-j0)*(i1-i0));

          for (int i = i0; i < i1; i++)
            {
              for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
                {
                  a[idx]   = xcdata(i,j,0);
                  a[idx+1] = xcdata(i,j,1);
                  a[idx+2] = xcdata(i,j,2);
                }
            }

          draw_pixels (j1-j0, i1-i0, GL_RGB, GL_FLOAT, a);

        }
      else if (cdata.is_single_type ())
        {
          const FloatNDArray xcdata = cdata.float_array_value ();

          OCTAVE_LOCAL_BUFFER (GLfloat, a, 3*(j1-j0)*(i1-i0));

          for (int i = i0; i < i1; i++)
            {
              for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
                {
                  a[idx]   = xcdata(i,j,0);
                  a[idx+1] = xcdata(i,j,1);
                  a[idx+2] = xcdata(i,j,2);
                }
            }

          draw_pixels (j1-j0, i1-i0, GL_RGB, GL_FLOAT, a);

        }
      else if (cdata.is_uint8_type ())
        {
          const uint8NDArray xcdata = cdata.uint8_array_value ();

          OCTAVE_LOCAL_BUFFER (GLubyte, a, 3*(j1-j0)*(i1-i0));

          for (int i = i0; i < i1; i++)
            {
              for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
                {
                  a[idx]   = xcdata(i,j,0);
                  a[idx+1] = xcdata(i,j,1);
                  a[idx+2] = xcdata(i,j,2);
                }
            }

          draw_pixels (j1-j0, i1-i0, GL_RGB, GL_UNSIGNED_BYTE, a);

        }
      else if (cdata.is_uint16_type ())
        {
          const uint16NDArray xcdata = cdata.uint16_array_value ();

          OCTAVE_LOCAL_BUFFER (GLushort, a, 3*(j1-j0)*(i1-i0));

          for (int i = i0; i < i1; i++)
            {
              for (int j = j0, idx = (i-i0)*(j1-j0)*3; j < j1; j++, idx += 3)
                {
                  a[idx]   = xcdata(i,j,0);
                  a[idx+1] = xcdata(i,j,1);
                  a[idx+2] = xcdata(i,j,2);
                }
            }

          draw_pixels (j1-j0, i1-i0, GL_RGB, GL_UNSIGNED_SHORT, a);

        }
      else
        warning ("opengl_renderer: invalid image data type (expected double, single, uint8, or uint16)");
    }
  else
    warning ("opengl_renderer: invalid image size (expected MxNx3 or MxN)");

  glPixelZoom (1, 1);
}

void
opengl_renderer::set_viewport (int w, int h)
{
  glViewport (0, 0, w, h);
}

void
opengl_renderer::draw_pixels (GLsizei width, GLsizei height, GLenum format,
                              GLenum type, const GLvoid *data)
{
  glDrawPixels (width, height, format, type, data);
}

void
opengl_renderer::set_color (const Matrix& c)
{
  glColor3dv (c.data ());
#if HAVE_FREETYPE
  text_renderer.set_color (c);
#endif
}

void
opengl_renderer::set_font (const base_properties& props)
{
#if HAVE_FREETYPE
  text_renderer.set_font (props.get ("fontname").string_value (),
                          props.get ("fontweight").string_value (),
                          props.get ("fontangle").string_value (),
                          props.get ("fontsize_points").double_value ());
#endif
}

void
opengl_renderer::set_polygon_offset (bool on, float offset)
{
  if (on)
    {
      glEnable (GL_POLYGON_OFFSET_FILL);
      glEnable (GL_POLYGON_OFFSET_LINE);
      glPolygonOffset (offset, offset);
    }
  else
    {
      glDisable (GL_POLYGON_OFFSET_FILL);
      glDisable (GL_POLYGON_OFFSET_LINE);
    }
}

void
opengl_renderer::set_linewidth (float w)
{
  glLineWidth (w);
}

void
opengl_renderer::set_linestyle (const std::string& s, bool use_stipple)
{
  bool solid = false;

  if (s == "-")
    {
      glLineStipple (1, static_cast<unsigned short> (0xFFFF));
      solid = true;
    }
  else if (s == ":")
    glLineStipple (1, static_cast<unsigned short> (0x8888));
  else if (s == "--")
    glLineStipple (1, static_cast<unsigned short> (0xF0F0));
  else if (s == "-.")
    glLineStipple (1, static_cast<unsigned short> (0x020F));
  else
    glLineStipple (1, static_cast<unsigned short> (0x0000));

  if (solid && ! use_stipple)
    glDisable (GL_LINE_STIPPLE);
  else
    glEnable (GL_LINE_STIPPLE);
}

void
opengl_renderer::set_clipbox (double x1, double x2, double y1, double y2,
                              double z1, double z2)
{
  double dx = (x2-x1);
  double dy = (y2-y1);
  double dz = (z2-z1);

  x1 -= 0.001*dx; x2 += 0.001*dx;
  y1 -= 0.001*dy; y2 += 0.001*dy;
  z1 -= 0.001*dz; z2 += 0.001*dz;

  ColumnVector p (4, 0.0);

  p(0) = -1; p(3) = x2;
  glClipPlane (GL_CLIP_PLANE0, p.data ());
  p(0) = 1; p(3) = -x1;
  glClipPlane (GL_CLIP_PLANE1, p.data ());
  p(0) = 0; p(1) = -1; p(3) = y2;
  glClipPlane (GL_CLIP_PLANE2, p.data ());
  p(1) = 1; p(3) = -y1;
  glClipPlane (GL_CLIP_PLANE3, p.data ());
  p(1) = 0; p(2) = -1; p(3) = z2;
  glClipPlane (GL_CLIP_PLANE4, p.data ());
  p(2) = 1; p(3) = -z1;
  glClipPlane (GL_CLIP_PLANE5, p.data ());

  xmin = x1; xmax = x2;
  ymin = y1; ymax = y2;
  zmin = z1; zmax = z2;
}

void
opengl_renderer::set_clipping (bool enable)
{
  bool has_clipping = (glIsEnabled (GL_CLIP_PLANE0) == GL_TRUE);

  if (enable != has_clipping)
    {
      if (enable)
        for (int i = 0; i < 6; i++)
          glEnable (GL_CLIP_PLANE0+i);
      else
        for (int i = 0; i < 6; i++)
          glDisable (GL_CLIP_PLANE0+i);
    }
}

void
opengl_renderer::init_marker (const std::string& m, double size, float width)
{
#if defined (HAVE_FRAMEWORK_OPENGL)
  GLint vw[4];
#else
  int vw[4];
#endif

  glGetIntegerv (GL_VIEWPORT, vw);

  glMatrixMode (GL_PROJECTION);
  glPushMatrix ();
  glLoadIdentity ();
  glOrtho (0, vw[2], vw[3], 0, xZ1, xZ2);
  glMatrixMode (GL_MODELVIEW);
  glPushMatrix ();

  set_clipping (false);
  set_linewidth (width);

  marker_id = make_marker_list (m, size, false);
  filled_marker_id = make_marker_list (m, size, true);
}

void
opengl_renderer::end_marker (void)
{
  glDeleteLists (marker_id, 1);
  glDeleteLists (filled_marker_id, 1);

  glMatrixMode (GL_MODELVIEW);
  glPopMatrix ();
  glMatrixMode (GL_PROJECTION);
  glPopMatrix ();
  set_linewidth (0.5f);
}

void
opengl_renderer::draw_marker (double x, double y, double z,
                              const Matrix& lc, const Matrix& fc)
{
  ColumnVector tmp = xform.transform (x, y, z, false);

  glLoadIdentity ();
  glTranslated (tmp(0), tmp(1), -tmp(2));

  if (filled_marker_id > 0 && fc.numel () > 0)
    {
      glColor3dv (fc.data ());
      set_polygon_offset (true, -1.0);
      glCallList (filled_marker_id);
      if (lc.numel () > 0)
        {
          glColor3dv (lc.data ());
          glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
          glEdgeFlag (GL_TRUE);
          set_polygon_offset (true, -2.0);
          glCallList (filled_marker_id);
          glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
        }
      set_polygon_offset (false);
    }
  else if (marker_id > 0 && lc.numel () > 0)
    {
      glColor3dv (lc.data ());
      glCallList (marker_id);
    }
}

unsigned int
opengl_renderer::make_marker_list (const std::string& marker, double size,
                                   bool filled) const
{
  char c = marker[0];

  if (filled && (c == '+' || c == 'x' || c == '*' || c == '.'))
    return 0;

  unsigned int ID = glGenLists (1);
  double sz = size * toolkit.get_screen_resolution () / 72.0;

  // constants for the * marker
  const double sqrt2d4 = 0.35355339059327;
  double tt = sz*sqrt2d4;

  glNewList (ID, GL_COMPILE);

  switch (marker[0])
    {
    case '+':
      glBegin (GL_LINES);
      glVertex2d (-sz/2, 0);
      glVertex2d (sz/2, 0);
      glVertex2d (0, -sz/2);
      glVertex2d (0, sz/2);
      glEnd ();
      break;
    case 'x':
      glBegin (GL_LINES);
      glVertex2d (-sz/2, -sz/2);
      glVertex2d (sz/2, sz/2);
      glVertex2d (-sz/2, sz/2);
      glVertex2d (sz/2, -sz/2);
      glEnd ();
      break;
    case '*':
      glBegin (GL_LINES);
      glVertex2d (-sz/2, 0);
      glVertex2d (sz/2, 0);
      glVertex2d (0, -sz/2);
      glVertex2d (0, sz/2);
      glVertex2d (-tt, -tt);
      glVertex2d (+tt, +tt);
      glVertex2d (-tt, +tt);
      glVertex2d (+tt, -tt);
      glEnd ();
      break;
    case '.':
      {
        double ang_step = M_PI / 5;

        glBegin (GL_POLYGON);
        for (double ang = 0; ang < (2*M_PI); ang += ang_step)
          glVertex2d (sz*cos (ang)/3, sz*sin (ang)/3);
        glEnd ();
      }
      break;
    case 's':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (-sz/2, -sz/2);
      glVertex2d (-sz/2, sz/2);
      glVertex2d (sz/2, sz/2);
      glVertex2d (sz/2, -sz/2);
      glEnd ();
      break;
    case 'o':
      {
        double ang_step = M_PI / 5;

        glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
        for (double ang = 0; ang < (2*M_PI); ang += ang_step)
          glVertex2d (sz*cos (ang)/2, sz*sin (ang)/2);
        glEnd ();
      }
      break;
    case 'd':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (0, -sz/2);
      glVertex2d (sz/2, 0);
      glVertex2d (0, sz/2);
      glVertex2d (-sz/2, 0);
      glEnd ();
      break;
    case 'v':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (0, sz/2);
      glVertex2d (sz/2, -sz/2);
      glVertex2d (-sz/2, -sz/2);
      glEnd ();
      break;
    case '^':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (0, -sz/2);
      glVertex2d (-sz/2, sz/2);
      glVertex2d (sz/2, sz/2);
      glEnd ();
      break;
    case '>':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (sz/2, 0);
      glVertex2d (-sz/2, sz/2);
      glVertex2d (-sz/2, -sz/2);
      glEnd ();
      break;
    case '<':
      glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
      glVertex2d (-sz/2, 0);
      glVertex2d (sz/2, -sz/2);
      glVertex2d (sz/2, sz/2);
      glEnd ();
      break;
    case 'p':
      {
        double ang;
        double r;
        double dr = 1.0 - sin (M_PI/10)/sin (3*M_PI/10)*1.02;

        glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
        for (int i = 0; i < 2*5; i++)
          {
            ang = (-0.5 + double(i+1)/5) * M_PI;
            r = 1.0 - (dr * fmod (double(i+1), 2.0));
            glVertex2d (sz*r*cos (ang)/2, sz*r*sin (ang)/2);
          }
        glEnd ();
      }
      break;
    case 'h':
      {
        double ang;
        double r;
        double dr = 1.0 - 0.5/sin (M_PI/3)*1.02;

        glBegin ((filled ? GL_POLYGON : GL_LINE_LOOP));
        for (int i = 0; i < 2*6; i++)
          {
            ang = (0.5 + double(i+1)/6.0) * M_PI;
            r = 1.0 - (dr * fmod (double(i+1), 2.0));
            glVertex2d (sz*r*cos (ang)/2, sz*r*sin (ang)/2);
          }
        glEnd ();
      }
      break;
    default:
      warning ("opengl_renderer: unsupported marker '%s'", marker.c_str ());
      break;
    }

  glEndList ();

  return ID;
}

void
opengl_renderer::text_to_pixels (const std::string& txt,
                                 uint8NDArray& pixels,
                                 Matrix& bbox,
                                 int halign, int valign, double rotation)
{
#if HAVE_FREETYPE
  text_renderer.text_to_pixels (txt, pixels, bbox,
                                halign, valign, rotation, "none");
#endif
}

Matrix
opengl_renderer::render_text (const std::string& txt,
                              double x, double y, double z,
                              int halign, int valign, double rotation)
{
#if HAVE_FREETYPE
  if (txt.empty ())
    return Matrix (1, 4, 0.0);

  uint8NDArray pixels;
  Matrix bbox;
  text_to_pixels (txt, pixels, bbox, halign, valign, rotation);

  bool blend = glIsEnabled (GL_BLEND);

  glEnable (GL_BLEND);
  glEnable (GL_ALPHA_TEST);
  glRasterPos3d (x, y, z);
  glBitmap(0, 0, 0, 0, bbox(0), bbox(1), 0);
  glDrawPixels (bbox(2), bbox(3),
                GL_RGBA, GL_UNSIGNED_BYTE, pixels.data ());
  glDisable (GL_ALPHA_TEST);
  if (! blend)
    glDisable (GL_BLEND);

  return bbox;
#else
  warning ("opengl_renderer: cannot render text, FreeType library not available");
  return Matrix (1, 4, 0.0);
#endif
}

#endif
