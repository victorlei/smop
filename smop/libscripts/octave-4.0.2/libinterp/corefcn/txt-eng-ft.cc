/*

Copyright (C) 2009-2015 Michael Goffioul

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

#if defined (HAVE_FREETYPE)

#if defined (HAVE_FONTCONFIG)
#include <fontconfig/fontconfig.h>
#endif

#include <clocale>
#include <cwchar>
#include <iostream>
#include <map>
#include <utility>

#include "singleton-cleanup.h"

#include "error.h"
#include "pr-output.h"
#include "txt-eng-ft.h"

// FIXME: maybe issue at most one warning per glyph/font/size/weight
//        combination.

static void
gripe_missing_glyph (FT_ULong c)
{
  warning_with_id ("Octave:missing-glyph",
                   "ft_render: skipping missing glyph for character '%x'",
                   c);
}

static void
gripe_glyph_render (FT_ULong c)
{
  warning_with_id ("Octave:glyph-render",
                   "ft_render: unable to render glyph for character '%x'",
                   c);
}

#ifdef _MSC_VER
// This is just a trick to avoid multiple symbol definitions.
// PermMatrix.h contains a dllexport'ed Array<octave_idx_type>
// that will cause MSVC not to generate a new instantiation and
// use the imported one instead.
#include "PermMatrix.h"
#endif

// Forward declaration
static void ft_face_destroyed (void* object);

class
ft_manager
{
public:
  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      {
        instance = new ft_manager ();

        if (instance)
          singleton_cleanup_list::add (cleanup_instance);
      }

    if (! instance)
      {
        ::error ("unable to create ft_manager!");

        retval = false;
      }

    return retval;
  }

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static FT_Face get_font (const std::string& name, const std::string& weight,
                           const std::string& angle, double size)
  {
    return (instance_ok ()
            ? instance->do_get_font (name, weight, angle, size)
            : 0);
  }

  static void font_destroyed (FT_Face face)
  {
    if (instance_ok ())
      instance->do_font_destroyed (face);
  }

private:

  static ft_manager *instance;

  typedef std::pair<std::string, double> ft_key;
  typedef std::map<ft_key, FT_Face> ft_cache;

  // Cache the fonts loaded by FreeType. This cache only contains
  // weak references to the fonts, strong references are only present
  // in class ft_render.
  ft_cache cache;

private:

  // No copying!

  ft_manager (const ft_manager&);

  ft_manager& operator = (const ft_manager&);

  ft_manager (void)
    : library (), freetype_initialized (false), fontconfig_initialized (false)
  {
    if (FT_Init_FreeType (&library))
      ::error ("unable to initialize FreeType library");
    else
      freetype_initialized = true;

#if defined (HAVE_FONTCONFIG)
    if (! FcInit ())
      ::error ("unable to initialize fontconfig library");
    else
      fontconfig_initialized = true;
#endif
  }

  ~ft_manager (void)
  {
    if (freetype_initialized)
      FT_Done_FreeType (library);

#if defined (HAVE_FONTCONFIG)
    // FIXME: Skip the call to FcFini because it can trigger the assertion
    //
    //   octave: fccache.c:507: FcCacheFini: Assertion 'fcCacheChains[i] == ((void *)0)' failed.
    //
    // if (fontconfig_initialized)
    //   FcFini ();
#endif
  }


  FT_Face do_get_font (const std::string& name, const std::string& weight,
                       const std::string& angle, double size)
  {
    FT_Face retval = 0;

#if HAVE_FT_REFERENCE_FACE
    // Look first into the font cache, then use fontconfig. If the font
    // is present in the cache, simply add a reference and return it.

    ft_key key (name + ":" + weight + ":" + angle, size);
    ft_cache::const_iterator it = cache.find (key);

    if (it != cache.end ())
      {
        FT_Reference_Face (it->second);
        return it->second;
      }
#endif

    std::string file;

#if defined (HAVE_FONTCONFIG)
    if (fontconfig_initialized)
      {
        int fc_weight, fc_angle;

        if (weight == "bold")
          fc_weight = FC_WEIGHT_BOLD;
        else if (weight == "light")
          fc_weight = FC_WEIGHT_LIGHT;
        else if (weight == "demi")
          fc_weight = FC_WEIGHT_DEMIBOLD;
        else
          fc_weight = FC_WEIGHT_NORMAL;

        if (angle == "italic")
          fc_angle = FC_SLANT_ITALIC;
        else if (angle == "oblique")
          fc_angle = FC_SLANT_OBLIQUE;
        else
          fc_angle = FC_SLANT_ROMAN;

        FcPattern *pat = FcPatternCreate ();

        FcPatternAddString (pat, FC_FAMILY,
                            (reinterpret_cast<const FcChar8*>
                             (name == "*" ? "sans" : name.c_str ())));

        FcPatternAddInteger (pat, FC_WEIGHT, fc_weight);
        FcPatternAddInteger (pat, FC_SLANT, fc_angle);
        FcPatternAddDouble (pat, FC_PIXEL_SIZE, size);

        if (FcConfigSubstitute (0, pat, FcMatchPattern))
          {
            FcResult res;
            FcPattern *match;

            FcDefaultSubstitute (pat);
            match = FcFontMatch (0, pat, &res);

            // FIXME: originally, this test also required that
            // res != FcResultNoMatch.  Is that really needed?
            if (match)
              {
                unsigned char *tmp;

                FcPatternGetString (match, FC_FILE, 0, &tmp);
                file = reinterpret_cast<char*> (tmp);
              }
            else
              ::warning ("could not match any font: %s-%s-%s-%g",
                         name.c_str (), weight.c_str (), angle.c_str (),
                         size);

            if (match)
              FcPatternDestroy (match);
          }

        FcPatternDestroy (pat);
      }
#endif

    if (file.empty ())
      {
#ifdef __WIN32__
        file = "C:/WINDOWS/Fonts/verdana.ttf";
#else
        // FIXME: find a "standard" font for UNIX platforms
#endif
      }

    if (! file.empty ())
      {
        if (FT_New_Face (library, file.c_str (), 0, &retval))
          ::warning ("ft_manager: unable to load font: %s", file.c_str ());
#if HAVE_FT_REFERENCE_FACE
        else
          {
            // Install a finalizer to notify ft_manager that the font is
            // being destroyed. The class ft_manager only keeps weak
            // references to font objects.

            retval->generic.data = new ft_key (key);
            retval->generic.finalizer = ft_face_destroyed;

            // Insert loaded font into the cache.

            cache[key] = retval;
          }
#endif
      }

    return retval;
  }

  void do_font_destroyed (FT_Face face)
  {
    if (face->generic.data)
      {
        ft_key* pkey = reinterpret_cast<ft_key*> (face->generic.data);

        cache.erase (*pkey);
        delete pkey;
        face->generic.data = 0;
      }
  }

private:
  FT_Library library;
  bool freetype_initialized;
  bool fontconfig_initialized;
};

ft_manager* ft_manager::instance = 0;

static void
ft_face_destroyed (void* object)
{ ft_manager::font_destroyed (reinterpret_cast<FT_Face> (object)); }

// ---------------------------------------------------------------------------

ft_render::ft_render (void)
  : text_processor (), font (), bbox (1, 4, 0.0), halign (0), xoffset (0),
    line_yoffset (0), yoffset (0), mode (MODE_BBOX),
    color (dim_vector (1, 3), 0)
{
}

ft_render::~ft_render (void)
{
}

void
ft_render::set_font (const std::string& name, const std::string& weight,
                     const std::string& angle, double size)
{
  // FIXME: take "fontunits" into account

  font = ft_font (name, weight, angle, size, 0);
}

void
ft_render::push_new_line (void)
{
  switch (mode)
    {
    case MODE_BBOX:
      {
        // Create a new bbox entry based on the current font.

        FT_Face face = font.get_face ();

        if (face)
          {
            int asc = face->size->metrics.ascender >> 6;
            int desc = face->size->metrics.descender >> 6;
            int h = face->size->metrics.height >> 6;

            Matrix bb (1, 5, 0.0);

            bb(1) = desc;
            bb(3) = asc - desc;
            bb(4) = h;

            line_bbox.push_back (bb);

            xoffset = yoffset = 0;
          }
      }
      break;

    case MODE_RENDER:
      {
        // Move to the next line bbox, adjust xoffset based on alignment
        // and yoffset based on the old and new line bbox.

        Matrix old_bbox = line_bbox.front ();
        line_bbox.pop_front ();
        Matrix new_bbox = line_bbox.front ();

        xoffset = compute_line_xoffset (new_bbox);
        line_yoffset += (old_bbox(1) - (new_bbox(1) + new_bbox(3)));
        yoffset = 0;
      }
      break;
    }
}

int
ft_render::compute_line_xoffset (const Matrix& lb) const
{
  if (! bbox.is_empty ())
    {
      switch (halign)
        {
        case 0:
          return 0;
        case 1:
          return (bbox(2) - lb(2)) / 2;
        case 2:
          return (bbox(2) - lb(2));
        }
    }

  return 0;
}

void
ft_render::compute_bbox (void)
{
  // Stack the various line bbox together and compute the final
  // bounding box for the entire text string.

  bbox = Matrix ();

  switch (line_bbox.size ())
    {
    case 0:
      break;
    case 1:
      bbox = line_bbox.front ().extract (0, 0, 0, 3);
      break;
    default:
      for (std::list<Matrix>::const_iterator it = line_bbox.begin ();
           it != line_bbox.end (); ++it)
        {
          if (bbox.is_empty ())
            bbox = it->extract (0, 0, 0, 3);
          else
            {
              bbox(1) -= (*it)(3);
              bbox(3) += (*it)(3);
              bbox(2) = xmax (bbox(2), (*it)(2));
            }
        }
      break;
    }
}

void
ft_render::update_line_bbox (void)
{
  // Called after a font change, when in MODE_BBOX mode, to update the
  // current line bbox with the new font metrics. This also includes the
  // current yoffset, that is the offset of the current glyph's baseline
  // the line's baseline.

  if (mode == MODE_BBOX)
    {
      int asc = font.get_face ()->size->metrics.ascender >> 6;
      int desc = font.get_face ()->size->metrics.descender >> 6;

      Matrix& bb = line_bbox.front ();

      if ((yoffset + desc) < bb(1))
        {
          // The new font goes below the bottom of the current bbox.

          int delta = bb(1) - (yoffset + desc);

          bb(1) -= delta;
          bb(3) += delta;
        }

      if ((yoffset + asc) > (bb(1) + bb(3)))
        {
          // The new font goes above the top of the current bbox.

          int delta = (yoffset + asc) - (bb(1) + bb(3));

          bb(3) += delta;
        }
    }
}

void
ft_render::set_mode (int m)
{
  mode = m;

  switch (mode)
    {
    case MODE_BBOX:
      xoffset = line_yoffset = yoffset = 0;
      bbox = Matrix (1, 4, 0.0);
      line_bbox.clear ();
      push_new_line ();
      break;
    case MODE_RENDER:
      if (bbox.numel () != 4)
        {
          ::warning ("ft_render: invalid bounding box, cannot render");

          xoffset = line_yoffset = yoffset = 0;
          pixels = uint8NDArray ();
        }
      else
        {
          pixels = uint8NDArray (dim_vector (4, bbox(2), bbox(3)),
                                 static_cast<uint8_t> (0));
          xoffset = compute_line_xoffset (line_bbox.front ());
          line_yoffset = -bbox(1)-1;
          yoffset = 0;
        }
      break;
    default:
      ::error ("ft_render: invalid mode '%d'", mode);
      break;
    }
}

FT_UInt
ft_render::process_character (FT_ULong code, FT_UInt previous)
{
  FT_Face face = font.get_face ();
  FT_UInt glyph_index = 0;

  if (face)
    {
      glyph_index = FT_Get_Char_Index (face, code);

      if (code != '\n'
          && (! glyph_index
              || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT)))
        {
          glyph_index = 0;
          gripe_missing_glyph (code);
        }
      else
        {
          switch (mode)
            {
            case MODE_RENDER:
              if (code == '\n')
                {
                  glyph_index = FT_Get_Char_Index (face, ' ');
                  if (! glyph_index
                      || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT))
                    {
                      glyph_index = 0;
                      gripe_missing_glyph (' ');
                    }
                  else
                    push_new_line ();
                }
              else if (FT_Render_Glyph (face->glyph, FT_RENDER_MODE_NORMAL))
                {
                  glyph_index = 0;
                  gripe_glyph_render (code);
                }
              else
                {
                  FT_Bitmap& bitmap = face->glyph->bitmap;
                  int x0, y0;

                  if (previous)
                    {
                      FT_Vector delta;

                      FT_Get_Kerning (face, previous, glyph_index,
                                      FT_KERNING_DEFAULT, &delta);
                      xoffset += (delta.x >> 6);
                    }

                  x0 = xoffset + face->glyph->bitmap_left;
                  y0 = line_yoffset + yoffset + face->glyph->bitmap_top;

                  // 'w' seems to have a negative -1
                  // face->glyph->bitmap_left, this is so we don't
                  // index out of bound, and assumes we we allocated
                  // the right amount of horizontal space in the bbox.
                  if (x0 < 0)
                    x0 = 0;

                  for (int r = 0; r < bitmap.rows; r++)
                    for (int c = 0; c < bitmap.width; c++)
                      {
                        unsigned char pix = bitmap.buffer[r*bitmap.width+c];
                        if (x0+c < 0 || x0+c >= pixels.dim2 ()
                            || y0-r < 0 || y0-r >= pixels.dim3 ())
                          {
                            //::warning ("ft_render: pixel out of bound (char=%d, (x,y)=(%d,%d), (w,h)=(%d,%d)",
                            //           str[i], x0+c, y0-r, pixels.dim2 (), pixels.dim3 ());
                          }
                        else if (pixels(3, x0+c, y0-r).value () == 0)
                          {
                            pixels(0, x0+c, y0-r) = color(0);
                            pixels(1, x0+c, y0-r) = color(1);
                            pixels(2, x0+c, y0-r) = color(2);
                            pixels(3, x0+c, y0-r) = pix;
                          }
                      }

                  xoffset += (face->glyph->advance.x >> 6);
                }
              break;

            case MODE_BBOX:
              if (code == '\n')
                {
                  glyph_index = FT_Get_Char_Index (face, ' ');
                  if (! glyph_index
                      || FT_Load_Glyph (face, glyph_index, FT_LOAD_DEFAULT))
                    {
                      glyph_index = 0;
                      gripe_missing_glyph (' ');
                    }
                  else
                    push_new_line ();
                }
              else
                {
                  Matrix& bb = line_bbox.back ();

                  // If we have a previous glyph, use kerning information.
                  // This usually means moving a bit backward before adding
                  // the next glyph. That is, "delta.x" is usually < 0.
                  if (previous)
                    {
                      FT_Vector delta;

                      FT_Get_Kerning (face, previous, glyph_index,
                                      FT_KERNING_DEFAULT, &delta);

                      xoffset += (delta.x >> 6);
                    }

                  // Extend current X offset box by the width of the current
                  // glyph. Then extend the line bounding box if necessary.

                  xoffset += (face->glyph->advance.x >> 6);
                  bb(2) = xmax (bb(2), xoffset);
                }
              break;
            }
        }
    }

  return glyph_index;
}

void
ft_render::visit (text_element_string& e)
{
  if (font.is_valid ())
    {
      FT_UInt glyph_index, previous = 0;

      std::string str = e.string_value ();
      size_t n = str.length ();
      size_t curr = 0;
      mbstate_t ps;
      memset (&ps, 0, sizeof (ps));  // Initialize state to 0.
      wchar_t wc;

      while (n > 0)
        {
          size_t r = gnulib::mbrtowc (&wc, str.data () + curr, n, &ps);

          if (r > 0
              && r != static_cast<size_t> (-1)
              && r != static_cast<size_t> (-2))
            {
              n -= r;
              curr += r;

              glyph_index = process_character (wc, previous);

              if (wc == L'\n')
                previous = 0;
              else
                previous = glyph_index;
            }
          else
            {
              if (r != 0)
                ::warning ("ft_render: failed to decode string `%s' with "
                           "locale `%s'", str.c_str (),
                           std::setlocale (LC_CTYPE, 0));
              break;
            }
        }
    }
}

void
ft_render::visit (text_element_list& e)
{
  // Save and restore (after processing the list) the current font and color.

  ft_font saved_font (font);
  uint8NDArray saved_color (color);

  text_processor::visit (e);

  font = saved_font;
  color = saved_color;
}

void
ft_render::visit (text_element_subscript& e)
{
  ft_font saved_font (font);
  int saved_line_yoffset = line_yoffset;
  int saved_yoffset = yoffset;

  set_font (font.get_name (), font.get_weight (), font.get_angle (),
            font.get_size () - 2);

  if (font.is_valid ())
    {
      int h = font.get_face ()->size->metrics.height >> 6;

      // Shifting the baseline by 2/3 the font height seems to produce
      // decent result.
      yoffset -= (h * 2) / 3;

      if (mode == MODE_BBOX)
        update_line_bbox ();
    }

  text_processor::visit (e);

  font = saved_font;
  // If line_yoffset changed, this means we moved to a new line; hence yoffset
  // cannot be restored, because the saved value is not relevant anymore.
  if (line_yoffset == saved_line_yoffset)
    yoffset = saved_yoffset;
}

void
ft_render::visit (text_element_superscript& e)
{
  ft_font saved_font (font);
  int saved_line_yoffset = line_yoffset;
  int saved_yoffset = yoffset;

  set_font (font.get_name (), font.get_weight (), font.get_angle (),
            font.get_size () - 2);

  if (saved_font.is_valid ())
    {
      int s_asc = saved_font.get_face ()->size->metrics.ascender >> 6;

      // Shifting the baseline by 2/3 base font ascender seems to produce
      // decent result.
      yoffset += (s_asc * 2) / 3;

      if (mode == MODE_BBOX)
        update_line_bbox ();
    }

  text_processor::visit (e);

  font = saved_font;
  // If line_yoffset changed, this means we moved to a new line; hence yoffset
  // cannot be restored, because the saved value is not relevant anymore.
  if (line_yoffset == saved_line_yoffset)
    yoffset = saved_yoffset;
}

void
ft_render::visit (text_element_color& e)
{
  if (mode == MODE_RENDER)
    set_color (e.get_color ());
}

void
ft_render::visit (text_element_fontsize& e)
{
  double sz = e.get_fontsize ();

  // FIXME: Matlab documentation says that the font size is expressed
  //        in the text object FontUnit.

  set_font (font.get_name (), font.get_weight (), font.get_angle (), sz);

  if (mode == MODE_BBOX)
    update_line_bbox ();
}

void
ft_render::visit (text_element_fontname& e)
{
  set_font (e.get_fontname (), font.get_weight (), font.get_angle (),
            font.get_size ());

  if (mode == MODE_BBOX)
    update_line_bbox ();
}

void
ft_render::visit (text_element_fontstyle& e)
{
  switch (e.get_fontstyle ())
    {
    case text_element_fontstyle::normal:
      set_font (font.get_name (), "normal", "normal", font.get_size ());
      break;
    case text_element_fontstyle::bold:
      set_font (font.get_name (), "bold", "normal", font.get_size ());
      break;
    case text_element_fontstyle::italic:
      set_font (font.get_name (), "normal", "italic", font.get_size ());
      break;
    case text_element_fontstyle::oblique:
      set_font (font.get_name (), "normal", "oblique", font.get_size ());
      break;
    }

  if (mode == MODE_BBOX)
    update_line_bbox ();
}

void
ft_render::visit (text_element_symbol& e)
{
  uint32_t code = e.get_symbol_code ();

  if (code != text_element_symbol::invalid_code && font.is_valid ())
    process_character (code);
  else if (font.is_valid ())
    ::warning ("ignoring unknown symbol: %d", e.get_symbol ());
}

void
ft_render::visit (text_element_combined& e)
{
  int saved_xoffset = xoffset;
  int max_xoffset = xoffset;

  for (text_element_combined::iterator it = e.begin (); it != e.end (); ++it)
    {
      xoffset = saved_xoffset;
      (*it)->accept (*this);
      max_xoffset = xmax (xoffset, max_xoffset);
    }

  xoffset = max_xoffset;
}

void
ft_render::reset (void)
{
  set_mode (MODE_BBOX);
  set_color (Matrix (1, 3, 0.0));
}

void
ft_render::set_color (Matrix c)
{
  if (c.numel () == 3)
    {
      color(0) = static_cast<uint8_t> (c(0)*255);
      color(1) = static_cast<uint8_t> (c(1)*255);
      color(2) = static_cast<uint8_t> (c(2)*255);
    }
  else
    ::warning ("ft_render::set_color: invalid color");
}

uint8NDArray
ft_render::render (text_element* elt, Matrix& box, int rotation)
{
  set_mode (MODE_BBOX);
  elt->accept (*this);
  compute_bbox ();
  box = bbox;

  set_mode (MODE_RENDER);
  if (pixels.numel () > 0)
    {
      elt->accept (*this);

      switch (rotation)
        {
        case ROTATION_0:
          break;
        case ROTATION_90:
          {
            Array<octave_idx_type> perm (dim_vector (3, 1));
            perm(0) = 0;
            perm(1) = 2;
            perm(2) = 1;
            pixels = pixels.permute (perm);

            Array<idx_vector> idx (dim_vector (3, 1));
            idx(0) = idx_vector (':');
            idx(1) = idx_vector (pixels.dim2 ()-1, -1, -1);
            idx(2) = idx_vector (':');
            pixels = uint8NDArray (pixels.index (idx));
          }
          break;
        case ROTATION_180:
          {
            Array<idx_vector> idx (dim_vector (3, 1));
            idx(0) = idx_vector (':');
            idx(1) = idx_vector (pixels.dim2 ()-1, -1, -1);
            idx(2)=  idx_vector (pixels.dim3 ()-1, -1, -1);
            pixels = uint8NDArray (pixels.index (idx));
          }
          break;
        case ROTATION_270:
          {
            Array<octave_idx_type> perm (dim_vector (3, 1));
            perm(0) = 0;
            perm(1) = 2;
            perm(2) = 1;
            pixels = pixels.permute (perm);

            Array<idx_vector> idx (dim_vector (3, 1));
            idx(0) = idx_vector (':');
            idx(1) = idx_vector (':');
            idx(2) = idx_vector (pixels.dim3 ()-1, -1, -1);
            pixels = uint8NDArray (pixels.index (idx));
          }
          break;
        }
    }

  return pixels;
}

// Note:
// x-extent accurately measures width of glyphs.
// y-extent is overly large because it is measured from baseline-to-baseline.
// Calling routines, such as ylabel, may need to account for this mismatch.

Matrix
ft_render::get_extent (text_element *elt, double rotation)
{
  set_mode (MODE_BBOX);
  elt->accept (*this);
  compute_bbox ();

  Matrix extent (1, 2, 0.0);

  switch (rotation_to_mode (rotation))
    {
    case ROTATION_0:
    case ROTATION_180:
      extent(0) = bbox(2);
      extent(1) = bbox(3);
      break;
    case ROTATION_90:
    case ROTATION_270:
      extent(0) = bbox(3);
      extent(1) = bbox(2);
    }

  return extent;
}

Matrix
ft_render::get_extent (const std::string& txt, double rotation,
                       const caseless_str& interpreter)
{
  text_element *elt = text_parser::parse (txt, interpreter);
  Matrix extent = get_extent (elt, rotation);
  delete elt;

  return extent;
}

int
ft_render::rotation_to_mode (double rotation) const
{
  // Clip rotation to range [0, 360]
  while (rotation < 0)
    rotation += 360.0;
  while (rotation > 360.0)
    rotation -= 360.0;

  if (rotation == 0.0)
    return ROTATION_0;
  else if (rotation == 90.0)
    return ROTATION_90;
  else if (rotation == 180.0)
    return ROTATION_180;
  else if (rotation == 270.0)
    return ROTATION_270;
  else
    return ROTATION_0;
}

void
ft_render::text_to_pixels (const std::string& txt,
                           uint8NDArray& pixels_, Matrix& box,
                           int _halign, int valign, double rotation,
                           const caseless_str& interpreter)
{
  int rot_mode = rotation_to_mode (rotation);

  halign = _halign;

  text_element *elt = text_parser::parse (txt, interpreter);
  pixels_ = render (elt, box, rot_mode);
  delete elt;

  if (pixels_.numel () == 0)
    {
      // nothing to render
      return;
    }

  switch (halign)
    {
    default: box(0) = 0; break;
    case 1: box(0) = -box(2)/2; break;
    case 2: box(0) = -box(2); break;
    }
  switch (valign)
    {
    default: box(1) = 0; break;
    case 1: box(1) = -box(3)/2; break;
    case 2: box(1) = -box(3); break;
    case 3: break;
    case 4: box(1) = -box(3)-box(1); break;
    }

  switch (rot_mode)
    {
    case ROTATION_90:
      std::swap (box(0), box(1));
      std::swap (box(2), box(3));
      box(0) = -box(0)-box(2);
      break;
    case ROTATION_180:
      box(0) = -box(0)-box(2);
      box(1) = -box(1)-box(3);
      break;
    case ROTATION_270:
      std::swap (box(0), box(1));
      std::swap (box(2), box(3));
      box(1) = -box(1)-box(3);
      break;
    }
}

ft_render::ft_font::ft_font (const ft_font& ft)
  : name (ft.name), weight (ft.weight), angle (ft.angle), size (ft.size),
    face (0)
{
#if HAVE_FT_REFERENCE_FACE
  FT_Face ft_face = ft.get_face ();

  if (ft_face && FT_Reference_Face (ft_face) == 0)
    face = ft_face;
#endif
}

ft_render::ft_font&
ft_render::ft_font::operator = (const ft_font& ft)
{
  if (&ft != this)
    {
      name = ft.name;
      weight = ft.weight;
      angle = ft.angle;
      size = ft.size;
      if (face)
        {
          FT_Done_Face (face);
          face = 0;
        }

#if HAVE_FT_REFERENCE_FACE
      FT_Face ft_face = ft.get_face ();

      if (ft_face && FT_Reference_Face (ft_face) == 0)
        face = ft_face;
#endif
    }

  return *this;
}

FT_Face
ft_render::ft_font::get_face (void) const
{
  if (! face && ! name.empty ())
    {
      face = ft_manager::get_font (name, weight, angle, size);

      if (face)
        {
          if (FT_Set_Char_Size (face, 0, size*64, 0, 0))
            ::warning ("ft_render: unable to set font size to %g", size);
        }
      else
        ::warning ("ft_render: unable to load appropriate font");
    }

  return face;
}

#endif // HAVE_FREETYPE
