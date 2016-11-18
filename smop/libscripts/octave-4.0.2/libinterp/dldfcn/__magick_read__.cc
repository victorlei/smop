/*

Copyright (C) 2013-2015 Carnë Draug
Copyright (C) 2002-2015 Andy Adler
Copyright (C) 2008 Thomas L. Scofield
Copyright (C) 2010 David Grundberg

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

#include "file-stat.h"
#include "oct-env.h"
#include "oct-time.h"

#include "defun-dld.h"
#include "error.h"
#include "ov-struct.h"

#include "gripes.h"

#ifdef HAVE_MAGICK

#include <Magick++.h>
#include <clocale>

// In theory, it should be enough to check the class:
// Magick::ClassType
// PseudoClass:
// Image is composed of pixels which specify an index in a color palette.
// DirectClass:
// Image is composed of pixels which represent literal color values.
//
//  GraphicsMagick does not really distinguishes between indexed and
//  normal images. After reading a file, it decides itself the optimal
//  way to store the image in memory, independently of the how the
//  image was stored in the file. That's what ClassType returns. While
//  it seems to match the original file most of the times, this is
//  not necessarily true all the times. See
//    https://sourceforge.net/mailarchive/message.php?msg_id=31180507
//  In addition to the ClassType, there is also ImageType which has a
//  type for indexed images (PaletteType and PaletteMatteType). However,
//  they also don't represent the original image. Not only does DirectClass
//  can have a PaletteType, but also does a PseudoClass have non Palette
//  types.
//
//        We can't do better without having format specific code which is
//        what we are trying to avoid by using a library such as GM. We at
//        least create workarounds for the most common problems.
//
// 1) A grayscale jpeg image can report being indexed even though the
//    JPEG format has no support for indexed images. We can at least
//    fix this one.
// 2) A PNG file is only an indexed image if color type orig is 3 (value comes
//    from libpng)
static bool
is_indexed (const Magick::Image& img)
{
  bool indexed = (img.classType () == Magick::PseudoClass);
  // Our problem until now is non-indexed images, being represented as indexed
  // by GM. The following attempts educated guesses to undo this optimization.
  if (indexed)
    {
      const std::string fmt = img.magick ();
      if (fmt == "JPEG")
        // The JPEG format does not support indexed images, but GM sometimes
        // reports grayscale JPEG as indexed. Always false for JPEG.
        indexed = false;
      else if (fmt == "PNG")
        {
          // Newer versions of GM (at least does not happens with 1.3.16) will
          // store values from the underlying library as image attributes. In
          // the case of PNG files, this is libpng where an indexed image will
          // always have a value of 3 for "color-type-orig". This property
          // always has a value in libpng so if we get nothing, we assume this
          // GM version does not store them and we have to go with whatever
          // GM PseudoClass says.
          const std::string color_type =
            const_cast<Magick::Image&> (img).attribute ("PNG:IHDR.color-type-orig");
          if (! color_type.empty() && color_type != "3")
            indexed = false;
        }
    }
  return indexed;
}

//  The depth from depth() is not always correct for us but seems to be the
//  best value we can get. For example, a grayscale png image with 1 bit
//  per channel should return a depth of 1 but instead we get 8.
//  We could check channelDepth() but then, which channel has the data
//  is not straightforward. So we'd have to check all
//  the channels and select the highest value. But then, I also
//  have a 16bit TIFF whose depth returns 16 (correct), but all of the
//  channels gives 8 (wrong). No idea why, maybe a bug in GM?
//  Anyway, using depth() seems that only causes problems for binary
//  images, and the problem with channelDepth() is not making set them
//  all to 1. So we will guess that if all channels have depth of 1,
//  then we must have a binary image.
//  Note that we can't use AllChannels it doesn't work for this.
//  Instead of checking all of the individual channels, we check one
//  from RGB, CMYK, grayscale, and transparency.
static octave_idx_type
get_depth (Magick::Image& img)
{
  octave_idx_type depth = img.depth ();
  if (depth == 8
      && img.channelDepth (Magick::RedChannel)     == 1
      && img.channelDepth (Magick::CyanChannel)    == 1
      && img.channelDepth (Magick::OpacityChannel) == 1
      && img.channelDepth (Magick::GrayChannel)    == 1)
    depth = 1;

  return depth;
}

// We need this in case one of the sides of the image being read has
// width 1. In those cases, the type will come as scalar instead of range
// since that's the behaviour of the colon operator (1:1:1 will be a scalar,
// not a range).
static Range
get_region_range (const octave_value& region)
{
  Range output;
  if (region.is_range ())
    output = region.range_value ();
  else if (region.is_scalar_type ())
    {
      double value = region.scalar_value ();
      output = Range (value, value);
    }
  else
    error ("__magick_read__: unknown datatype for Region option");

  return output;
}

static std::map<std::string, octave_idx_type>
calculate_region (const octave_scalar_map& options)
{
  std::map<std::string, octave_idx_type> region;
  const Cell pixel_region = options.getfield ("region").cell_value ();

  // Subtract 1 to account for 0 indexing.
  const Range rows     = get_region_range (pixel_region (0));
  const Range cols     = get_region_range (pixel_region (1));
  region["row_start"]  = rows.base () -1;
  region["col_start"]  = cols.base () -1;
  region["row_end"]    = rows.max ()  -1;
  region["col_end"]    = cols.max ()  -1;

  // Length of the area to load into the Image Pixel Cache.  We use max and
  // min to account for cases where last element of range is the range limit.
  region["row_cache"] = region["row_end"] - region["row_start"] +1;
  region["col_cache"] = region["col_end"] - region["col_start"] +1;

  // How much we have to shift in the memory when doing the loops.
  region["row_shift"] = region["col_cache"] * rows.inc ();
  region["col_shift"] = region["col_cache"] *
                        (region["row_cache"] + rows.inc () -1) - cols.inc ();

  // The actual height and width of the output image
  region["row_out"] = rows.nelem ();
  region["col_out"] = cols.nelem ();

  return region;
}

static octave_value_list
read_maps (Magick::Image& img)
{
  // can't call colorMapSize on const Magick::Image
  const octave_idx_type mapsize = img.colorMapSize ();
  Matrix cmap                   = Matrix (mapsize, 3); // colormap
  ColumnVector amap             = ColumnVector (mapsize); // alpha map
  for (octave_idx_type i = 0; i < mapsize; i++)
    {
      const Magick::ColorRGB c = img.colorMap (i);
      cmap(i,0) = c.red   ();
      cmap(i,1) = c.green ();
      cmap(i,2) = c.blue  ();
      amap(i)   = c.alpha ();
    }
  octave_value_list maps;
  maps(0) = cmap;
  maps(1) = amap;
  return maps;
}

template <class T>
static octave_value_list
read_indexed_images (const std::vector<Magick::Image>& imvec,
                     const Array<octave_idx_type>& frameidx,
                     const octave_idx_type& nargout,
                     const octave_scalar_map& options)
{
  typedef typename T::element_type P;

  octave_value_list retval (3, Matrix ());

  std::map<std::string, octave_idx_type> region = calculate_region (options);
  const octave_idx_type nFrames = frameidx.length ();
  const octave_idx_type nRows = region["row_out"];
  const octave_idx_type nCols = region["col_out"];

  // imvec has all of the pages of a file, even the ones we are not
  // interested in. We will use the first image that we will be actually
  // reading to get information about the image.
  const octave_idx_type def_elem = frameidx(0);

  T img       = T (dim_vector (nRows, nCols, 1, nFrames));
  P* img_fvec = img.fortran_vec ();

  const octave_idx_type row_start = region["row_start"];
  const octave_idx_type col_start = region["col_start"];
  const octave_idx_type row_shift = region["row_shift"];
  const octave_idx_type col_shift = region["col_shift"];
  const octave_idx_type row_cache = region["row_cache"];
  const octave_idx_type col_cache = region["col_cache"];

  // When reading PixelPackets from the Image Pixel Cache, they come in
  // row major order. So we keep moving back and forth there so we can
  // write the image in column major order.
  octave_idx_type idx = 0;
  for (octave_idx_type frame = 0; frame < nFrames; frame++)
    {
      OCTAVE_QUIT;
      imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                             col_cache, row_cache);

      const Magick::IndexPacket *pix
        = imvec[frameidx(frame)].getConstIndexes ();

      for (octave_idx_type col = 0; col < nCols; col++)
        {
          for (octave_idx_type row = 0; row < nRows; row++)
            {
              img_fvec[idx++] = static_cast<P> (*pix);
              pix += row_shift;
            }
          pix -= col_shift;
        }
    }
  retval(0) = octave_value (img);

  // Only bother reading the colormap if it was requested as output.
  if (nargout > 1)
    {
      // In theory, it should be possible for each frame of an image to
      // have different colormaps but for Matlab compatibility, we only
      // return the colormap of the first frame.  To obtain the colormaps
      // of different frames, one needs can either use imfinfo or a for
      // loop around imread.
      const octave_value_list maps =
        read_maps (const_cast<Magick::Image&> (imvec[frameidx(def_elem)]));

      retval(1) = maps(0);

      // only interpret alpha channel if it exists and was requested as output
      if (imvec[def_elem].matte () && nargout >= 3)
        {
          const Matrix amap = maps(1).matrix_value ();
          const double* amap_fvec = amap.fortran_vec ();

          NDArray alpha (dim_vector (nRows, nCols, 1, nFrames));
          double* alpha_fvec = alpha.fortran_vec ();

          // GraphicsMagick stores the alpha values inverted, i.e.,
          // 1 for transparent and 0 for opaque so we fix that here.
          const octave_idx_type nPixels = alpha.numel ();
          for (octave_idx_type pix = 0; pix < nPixels; pix++)
            alpha_fvec[pix] = 1 - amap_fvec[static_cast<int> (img_fvec[3])];

          retval(2) = alpha;
        }
    }

  return retval;
}

// This function is highly repetitive, a bunch of for loops that are
// very similar to account for different image types. They are different
// enough that trying to reduce the copy and paste would decrease its
// readability too much.
template <class T>
octave_value_list
read_images (std::vector<Magick::Image>& imvec,
             const Array<octave_idx_type>& frameidx,
             const octave_idx_type& nargout,
             const octave_scalar_map& options)
{
  typedef typename T::element_type P;

  octave_value_list retval (3, Matrix ());

  std::map<std::string, octave_idx_type> region = calculate_region (options);
  const octave_idx_type nFrames = frameidx.length ();
  const octave_idx_type nRows = region["row_out"];
  const octave_idx_type nCols = region["col_out"];
  T img;

  // imvec has all of the pages of a file, even the ones we are not
  // interested in. We will use the first image that we will be actually
  // reading to get information about the image.
  const octave_idx_type def_elem = frameidx(0);

  const octave_idx_type row_start = region["row_start"];
  const octave_idx_type col_start = region["col_start"];
  const octave_idx_type row_shift = region["row_shift"];
  const octave_idx_type col_shift = region["col_shift"];
  const octave_idx_type row_cache = region["row_cache"];
  const octave_idx_type col_cache = region["col_cache"];

  // GraphicsMagick (GM) keeps the image values in memory using whatever
  // QuantumDepth it was built with independently of the original image
  // bitdepth. Basically this means that if GM was built with quantum 16
  // all values are scaled in the uint16 range. If the original image
  // had an 8 bit depth, we need to rescale it for that range.
  // However, if the image had a bitdepth of 32, then we will be returning
  // a floating point image. In this case, the values need to be rescaled
  // for the range [0 1] (this is what Matlab has documented on the page
  // about image types but in some cases seems to be doing something else.
  // See bug #39249).
  // Finally, we must do the division ourselves (set a divisor) instead of
  // using quantumOperator for the cases where we will be returning floating
  // point and want things in the range [0 1]. This is the same reason why
  // the divisor is of type double.
  // uint64_t is used in expression because default 32-bit value overflows
  // when depth() is 32.
  // TODO in the next release of GraphicsMagick, MaxRGB should be replaced
  //      with QuantumRange since MaxRGB is already deprecated in ImageMagick.
  double divisor;
  if (imvec[def_elem].depth () == 32)
    divisor = std::numeric_limits<uint32_t>::max ();
  else
    divisor = MaxRGB / ((uint64_t (1) << imvec[def_elem].depth ()) - 1);

  // FIXME: this workaround should probably be fixed in GM by creating a
  //        new ImageType BilevelMatteType
  // Despite what GM documentation claims, opacity is not only on the types
  // with Matte on the name. It is possible that an image is completely
  // black (1 color), and have a second channel set for transparency (2nd
  // color). Its type will be bilevel since there is no BilevelMatte. The
  // only way to check for this seems to be by checking matte ().
  Magick::ImageType type = imvec[def_elem].type ();
  if (type == Magick::BilevelType && imvec[def_elem].matte ())
    type = Magick::GrayscaleMatteType;

  // FIXME: ImageType is the type being used to represent the image in memory
  // by GM. The real type may be different (see among others bug #36820). For
  // example, a png file where all channels are equal may report being
  // grayscale or even bilevel. But we must always return the real image in
  // file. In some cases, the original image attributes are stored in the
  // attributes but this is undocumented. This should be fixed in GM so that
  // a method such as original_type returns an actual Magick::ImageType
  if (imvec[0].magick () == "PNG")
    {
      // These values come from libpng, not GM:
      //      Grayscale         = 0
      //      Palette           = 2 + 1
      //      RGB               = 2
      //      RGB + Alpha       = 2 + 4
      //      Grayscale + Alpha = 4
      // We won't bother with case 3 (palette) since those should be
      // read by the function to read indexed images
      const std::string type_str
        = imvec[0].attribute ("PNG:IHDR.color-type-orig");

      if (type_str == "0")
        type = Magick::GrayscaleType;
      else if (type_str == "2")
        type = Magick::TrueColorType;
      else if (type_str == "6")
        type = Magick::TrueColorMatteType;
      else if (type_str == "4")
        type = Magick::GrayscaleMatteType;
      // Color types 0, 2, and 3 can also have alpha channel, conveyed
      // via the "tRNS" chunk.  For 0 and 2, it's limited to GIF-style
      // binary transparency, while 3 can have any level of alpha per
      // palette entry. We thus must check matte() to see if the image
      // really doesn't have an alpha channel.
      if (imvec[0].matte ())
        {
          if (type == Magick::GrayscaleType)
            type = Magick::GrayscaleMatteType;
          else if (type == Magick::TrueColorType)
            type = Magick::TrueColorMatteType;
        }
    }

  // If the alpha channel was not requested, treat images as if
  // it doesn't exist.
  if (nargout < 3)
    {
      switch (type)
        {
        case Magick::GrayscaleMatteType:
          type = Magick::GrayscaleType;
          break;

        case Magick::PaletteMatteType:
          type = Magick::PaletteType;
          break;

        case Magick::TrueColorMatteType:
          type = Magick::TrueColorType;
          break;

        case Magick::ColorSeparationMatteType:
          type = Magick::ColorSeparationType;
          break;

        default:
          // Do nothing other than silencing warnings about enumeration
          // values not being handled in switch.
          ;
        }
    }

  const octave_idx_type colour_stride = nRows * nCols;
  switch (type)
    {
    case Magick::BilevelType:           // Monochrome bi-level image
    case Magick::GrayscaleType:         // Grayscale image
      {
        img = T (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();

        octave_idx_type idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                                       col_cache, row_cache);

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    img_fvec[idx++] = pix->red / divisor;
                    pix += row_shift;
                  }
                pix -= col_shift;
              }
          }
        break;
      }

    case Magick::GrayscaleMatteType:    // Grayscale image with opacity
      {
        img   = T (dim_vector (nRows, nCols, 1, nFrames));
        T alpha   (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();
        P *a_fvec   = alpha.fortran_vec ();

        octave_idx_type idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                                       col_cache, row_cache);

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    img_fvec[idx] = pix->red / divisor;
                    a_fvec[idx]   = (MaxRGB - pix->opacity) / divisor;
                    pix += row_shift;
                    idx++;
                  }
                pix -= col_shift;
              }
          }
        retval(2) = alpha;
        break;
      }

    case Magick::PaletteType:           // Indexed color (palette) image
    case Magick::TrueColorType:         // Truecolor image
      {
        img = T (dim_vector (nRows, nCols, 3, nFrames));
        P *img_fvec = img.fortran_vec ();

        const octave_idx_type frame_stride  = colour_stride * 3;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                                       col_cache, row_cache);

            octave_idx_type idx = 0;
            P *rbuf   = img_fvec;
            P *gbuf   = img_fvec + colour_stride;
            P *bbuf   = img_fvec + colour_stride * 2;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    rbuf[idx] = pix->red   / divisor;
                    gbuf[idx] = pix->green / divisor;
                    bbuf[idx] = pix->blue  / divisor;
                    pix += row_shift;
                    idx++;
                  }
                pix -= col_shift;
              }
            img_fvec += frame_stride;
          }
        break;
      }

    case Magick::PaletteMatteType:    // Indexed color image with opacity
    case Magick::TrueColorMatteType:  // Truecolor image with opacity
      {
        img   = T (dim_vector (nRows, nCols, 3, nFrames));
        T alpha   (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();
        P *a_fvec   = alpha.fortran_vec ();

        const octave_idx_type frame_stride  = colour_stride * 3;

        // Unlike the index for the other channels, this one won't need
        // to be reset on each frame since it's a separate matrix.
        octave_idx_type a_idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                                       col_cache, row_cache);

            octave_idx_type idx = 0;
            P *rbuf   = img_fvec;
            P *gbuf   = img_fvec + colour_stride;
            P *bbuf   = img_fvec + colour_stride * 2;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    rbuf[idx]     = pix->red     / divisor;
                    gbuf[idx]     = pix->green   / divisor;
                    bbuf[idx]     = pix->blue    / divisor;
                    a_fvec[a_idx++] = (MaxRGB - pix->opacity) / divisor;
                    pix += row_shift;
                    idx++;
                  }
                pix -= col_shift;
              }
            img_fvec += frame_stride;
          }
        retval(2) = alpha;
        break;
      }

    case Magick::ColorSeparationType:  // Cyan/Magenta/Yellow/Black (CMYK) image
      {
        img   = T (dim_vector (nRows, nCols, 4, nFrames));
        P *img_fvec = img.fortran_vec ();

        const octave_idx_type frame_stride  = colour_stride * 4;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                                       col_cache, row_cache);

            octave_idx_type idx = 0;
            P *cbuf   = img_fvec;
            P *mbuf   = img_fvec + colour_stride;
            P *ybuf   = img_fvec + colour_stride * 2;
            P *kbuf   = img_fvec + colour_stride * 3;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    cbuf[idx] = pix->red     / divisor;
                    mbuf[idx] = pix->green   / divisor;
                    ybuf[idx] = pix->blue    / divisor;
                    kbuf[idx] = pix->opacity / divisor;
                    pix += row_shift;
                    idx++;
                  }
                pix -= col_shift;
              }
            img_fvec += frame_stride;
          }
        break;
      }

    // Cyan, magenta, yellow, and black with alpha (opacity) channel
    case Magick::ColorSeparationMatteType:
      {
        img   = T (dim_vector (nRows, nCols, 4, nFrames));
        T alpha   (dim_vector (nRows, nCols, 1, nFrames));
        P *img_fvec = img.fortran_vec ();
        P *a_fvec   = alpha.fortran_vec ();

        const octave_idx_type frame_stride  = colour_stride * 4;

        // Unlike the index for the other channels, this one won't need
        // to be reset on each frame since it's a separate matrix.
        octave_idx_type a_idx = 0;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            const Magick::PixelPacket *pix
              = imvec[frameidx(frame)].getConstPixels (col_start, row_start,
                                                       col_cache, row_cache);
            // Note that for CMYKColorspace + matte (CMYKA), the opacity is
            // stored in the assocated IndexPacket.
            const Magick::IndexPacket *apix
              = imvec[frameidx(frame)].getConstIndexes ();

            octave_idx_type idx = 0;
            P *cbuf   = img_fvec;
            P *mbuf   = img_fvec + colour_stride;
            P *ybuf   = img_fvec + colour_stride * 2;
            P *kbuf   = img_fvec + colour_stride * 3;

            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    cbuf[idx]     = pix->red     / divisor;
                    mbuf[idx]     = pix->green   / divisor;
                    ybuf[idx]     = pix->blue    / divisor;
                    kbuf[idx]     = pix->opacity / divisor;
                    a_fvec[a_idx++] = (MaxRGB - *apix) / divisor;
                    pix += row_shift;
                    idx++;
                  }
                pix -= col_shift;
              }
            img_fvec += frame_stride;
          }
        retval(2) = alpha;
        break;
      }

    default:
      error ("__magick_read__: unknown Magick++ image type");
      return retval;
    }

  retval(0) = img;
  return retval;
}

// Read a file into vector of image objects.
void static
read_file (const std::string& filename, std::vector<Magick::Image>& imvec)
{
  try
    {
      Magick::readImages (&imvec, filename);
    }
  catch (Magick::Warning& w)
    {
      warning ("Magick++ warning: %s", w.what ());
    }
  catch (Magick::Exception& e)
    {
      error ("Magick++ exception: %s", e.what ());
      error_state = 1;
    }
}

static void
maybe_initialize_magick (void)
{
  static bool initialized = false;

  if (! initialized)
    {
      // Save locale as GraphicsMagick might change this (fixed in
      // GraphicsMagick since version 1.3.13 released on December 24, 2011)
      const char *static_locale = setlocale (LC_ALL, NULL);
      const std::string locale (static_locale);

      const std::string program_name
        = octave_env::get_program_invocation_name ();
      Magick::InitializeMagick (program_name.c_str ());

      // Restore locale from before GraphicsMagick initialisation
      setlocale (LC_ALL, locale.c_str ());

      if (QuantumDepth < 32)
        warning_with_id ("Octave:GraphicsMagic-Quantum-Depth",
                         "your version of %s limits images to %d bits per pixel",
                         MagickPackageName, QuantumDepth);

      initialized = true;
    }
}
#endif

DEFUN_DLD (__magick_read__, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{img}, @var{map}, @var{alpha}] =} __magick_read__ (@var{fname}, @var{options})\n\
Read image with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.\n\
Use @code{imread} instead.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
@end deftypefn")
{
  octave_value_list output;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imread", "Image IO");
#else

  maybe_initialize_magick ();

  if (args.length () != 2 || ! args(0).is_string ())
    {
      print_usage ();
      return output;
    }

  const octave_scalar_map options = args(1).scalar_map_value ();
  if (error_state)
    {
      error ("__magick_read__: OPTIONS must be a struct");
      return output;
    }

  std::vector<Magick::Image> imvec;
  read_file (args(0).string_value (), imvec);
  if (error_state)
    return output;

  // Prepare an Array with the indexes for the requested frames.
  const octave_idx_type nFrames = imvec.size ();
  Array<octave_idx_type> frameidx;
  const octave_value indexes = options.getfield ("index");
  if (indexes.is_string () && indexes.string_value () == "all")
    {
      frameidx.resize (dim_vector (1, nFrames));
      for (octave_idx_type i = 0; i < nFrames; i++)
        frameidx(i) = i;
    }
  else
    {
      frameidx = indexes.int_vector_value ();
      if (error_state)
        {
          error ("__magick_read__: invalid value for Index/Frame");
          return output;
        }
      // Fix indexes from base 1 to base 0, and at the same time, make
      // sure none of the indexes is outside the range of image number.
      const octave_idx_type n = frameidx.nelem ();
      for (octave_idx_type i = 0; i < n; i++)
        {
          frameidx(i)--;
          if (frameidx(i) < 0 || frameidx(i) > nFrames - 1)
            {
              // We do this check inside the loop because frameidx does not
              // need to be ordered (this is a feature and even allows for
              // some frames to be read multiple times).
              error ("imread: index/frames specified are outside the number of images");
              return output;
            }
        }
    }

  // Check that all frames have the same size. We don't do this at the same
  // time we decode the image because that's done in many different places,
  // to cover the different types of images which would lead to a lot of
  // copy and paste.
  {
    const unsigned int nRows = imvec[frameidx(0)].rows ();
    const unsigned int nCols = imvec[frameidx(0)].columns ();
    const octave_idx_type n = frameidx.nelem ();
    for (octave_idx_type frame = 0; frame < n; frame++)
      {
        if (nRows != imvec[frameidx(frame)].rows ()
            || nCols != imvec[frameidx(frame)].columns ())
          {
            error ("imread: all frames must have the same size but frame %i is different",
                   frameidx(frame) +1);
            return output;
          }
      }
  }

  const octave_idx_type depth = get_depth (imvec[frameidx(0)]);
  if (is_indexed (imvec[frameidx(0)]))
    {
      if (depth <= 1)
        output = read_indexed_images<boolNDArray>   (imvec, frameidx,
                                                     nargout, options);
      else if (depth <= 8)
        output = read_indexed_images<uint8NDArray>  (imvec, frameidx,
                                                     nargout, options);
      else if (depth <= 16)
        output = read_indexed_images<uint16NDArray> (imvec, frameidx,
                                                     nargout, options);
      else
        {
          error ("imread: indexed images with depths greater than 16-bit are not supported");
          return output;
        }
    }

  else
    {
      if (depth <= 1)
        output = read_images<boolNDArray>   (imvec, frameidx, nargout, options);
      else if (depth <= 8)
        output = read_images<uint8NDArray>  (imvec, frameidx, nargout, options);
      else if (depth <= 16)
        output = read_images<uint16NDArray> (imvec, frameidx, nargout, options);
      else if (depth <= 32)
        output = read_images<FloatNDArray>  (imvec, frameidx, nargout, options);
      else
        {
          error ("imread: reading of images with %i-bit depth is not supported",
                 depth);
        }
    }

#endif
  return output;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

#ifdef HAVE_MAGICK

template <class T>
static uint32NDArray
img_float2uint (const T& img)
{
  typedef typename T::element_type P;
  uint32NDArray out (img.dims ());

  octave_uint32* out_fvec = out.fortran_vec ();
  const P*       img_fvec = img.fortran_vec ();

  const octave_uint32 max = octave_uint32::max ();
  const octave_idx_type numel = img.numel ();
  for (octave_idx_type idx = 0; idx < numel; idx++)
    out_fvec[idx] = img_fvec[idx] * max;

  return out;
}

// Gets the bitdepth to be used for an Octave class, i.e, returns 8 for
// uint8, 16 for uint16, and 32 for uint32
template <class T>
static octave_idx_type
bitdepth_from_class ()
{
  typedef typename T::element_type P;
  const octave_idx_type bitdepth =
    sizeof (P) * std::numeric_limits<unsigned char>::digits;
  return bitdepth;
}

static Magick::Image
init_enconde_image (const octave_idx_type& nCols, const octave_idx_type& nRows,
                    const octave_idx_type& bitdepth,
                    const Magick::ImageType& type,
                    const Magick::ClassType& klass)
{
  Magick::Image img (Magick::Geometry (nCols, nRows), "black");
  // Ensure that there are no other references to this image.
  img.modifyImage ();

  img.classType (klass);
  img.type (type);
  // FIXME: for some reason, setting bitdepth doesn't seem to work for
  //        indexed images.
  img.depth (bitdepth);
  switch (type)
    {
    case Magick::GrayscaleMatteType:
    case Magick::TrueColorMatteType:
    case Magick::ColorSeparationMatteType:
    case Magick::PaletteMatteType:
      img.matte (true);
      break;

    default:
      img.matte (false);
    }

  return img;
}

template <class T>
static void
encode_indexed_images (std::vector<Magick::Image>& imvec,
                       const T& img,
                       const Matrix& cmap)
{
  typedef typename T::element_type P;
  const octave_idx_type nFrames   = img.ndims () < 4 ? 1 : img.dims ()(3);
  const octave_idx_type nRows     = img.rows ();
  const octave_idx_type nCols     = img.columns ();
  const octave_idx_type cmap_size = cmap.rows ();
  const octave_idx_type bitdepth  = bitdepth_from_class<T> ();

  // There is no colormap object, we need to build a new one for each frame,
  // even if it's always the same. We can least get a vector for the Colors.
  std::vector<Magick::ColorRGB> colormap;
  {
    const double* cmap_fvec = cmap.fortran_vec ();
    const octave_idx_type G_offset = cmap_size;
    const octave_idx_type B_offset = cmap_size * 2;
    for (octave_idx_type map_idx = 0; map_idx < cmap_size; map_idx++)
      colormap.push_back (Magick::ColorRGB (cmap_fvec[map_idx],
                                            cmap_fvec[map_idx + G_offset],
                                            cmap_fvec[map_idx + B_offset]));
  }

  for (octave_idx_type frame = 0; frame < nFrames; frame++)
    {
      OCTAVE_QUIT;
      Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                Magick::PaletteType,
                                                Magick::PseudoClass);

      // Insert colormap.
      m_img.colorMapSize (cmap_size);
      for (octave_idx_type map_idx = 0; map_idx < cmap_size; map_idx++)
        m_img.colorMap (map_idx, colormap[map_idx]);

      // Why are we also setting the pixel values instead of only the
      // index values? We don't know if a file format supports indexed
      // images. If we only set the indexes and then try to save the
      // image as JPEG for example, the indexed values get discarded,
      // there is no conversion from the indexes, it's the initial values
      // that get used. An alternative would be to only set the pixel
      // values (no indexes), then set the image as PseudoClass and GM
      // would create a colormap for us. However, we wouldn't have control
      // over the order of that colormap. And that's why we set both.
      Magick::PixelPacket* pix = m_img.getPixels (0, 0, nCols, nRows);
      Magick::IndexPacket* ind = m_img.getIndexes ();
      const P* img_fvec        = img.fortran_vec ();

      octave_idx_type GM_idx = 0;
      for (octave_idx_type column = 0; column < nCols; column++)
        {
          for (octave_idx_type row = 0; row < nRows; row++)
            {
              ind[GM_idx] = double (*img_fvec);
              pix[GM_idx] = m_img.colorMap (double (*img_fvec));
              img_fvec++;
              GM_idx += nCols;
            }
          GM_idx -= nCols * nRows - 1;
        }

      // Save changes to underlying image.
      m_img.syncPixels ();
      imvec.push_back (m_img);
    }
}

static void
encode_bool_image (std::vector<Magick::Image>& imvec, const boolNDArray& img)
{
  const octave_idx_type nFrames   = img.ndims () < 4 ? 1 : img.dims ()(3);
  const octave_idx_type nRows     = img.rows ();
  const octave_idx_type nCols     = img.columns ();

  // The initialized image will be black, this is for the other pixels
  const Magick::Color white ("white");

  const bool *img_fvec = img.fortran_vec ();
  octave_idx_type img_idx = 0;
  for (octave_idx_type frame = 0; frame < nFrames; frame++)
    {
      OCTAVE_QUIT;
      // For some reason, we can't set the type to Magick::BilevelType or
      // the output image will be black, changing to white has no effect.
      // However, this will still work fine and a binary image will be
      // saved because we are setting the bitdepth to 1.
      Magick::Image m_img = init_enconde_image (nCols, nRows, 1,
                                                Magick::GrayscaleType,
                                                Magick::DirectClass);

      Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
      octave_idx_type GM_idx = 0;
      for (octave_idx_type col = 0; col < nCols; col++)
        {
          for (octave_idx_type row = 0; row < nRows; row++)
            {
              if (img_fvec[img_idx])
                pix[GM_idx] = white;

              img_idx++;
              GM_idx += nCols;
            }
          GM_idx -= nCols * nRows - 1;
        }
      // Save changes to underlying image.
      m_img.syncPixels ();
      // While we could not set it to Bilevel at the start, we can do it
      // here otherwise some coders won't save it as binary.
      m_img.type (Magick::BilevelType);
      imvec.push_back (m_img);
    }
}

template <class T>
static void
encode_uint_image (std::vector<Magick::Image>& imvec,
                   const T& img, const T& alpha)
{
  typedef typename T::element_type P;
  const octave_idx_type channels = img.ndims () < 3 ? 1 : img.dims ()(2);
  const octave_idx_type nFrames  = img.ndims () < 4 ? 1 : img.dims ()(3);
  const octave_idx_type nRows    = img.rows ();
  const octave_idx_type nCols    = img.columns ();
  const octave_idx_type bitdepth = bitdepth_from_class<T> ();

  Magick::ImageType type;
  const bool has_alpha = ! alpha.is_empty ();
  switch (channels)
    {
    case 1:
      if (has_alpha)
        type = Magick::GrayscaleMatteType;
      else
        type = Magick::GrayscaleType;
      break;

    case 3:
      if (has_alpha)
        type = Magick::TrueColorMatteType;
      else
        type = Magick::TrueColorType;
      break;

    case 4:
      if (has_alpha)
        type = Magick::ColorSeparationMatteType;
      else
        type = Magick::ColorSeparationType;
      break;

    default:
      {
        // __imwrite should have already filtered this cases
        error ("__magick_write__: wrong size on 3rd dimension");
        return;
      }
    }

  // We will be passing the values as integers with depth as specified
  // by QuantumDepth (maximum value specified by MaxRGB). This is independent
  // of the actual depth of the image. GM will then convert the values but
  // while in memory, it always keeps the values as specified by QuantumDepth.
  // From GM documentation:
  //  Color arguments are must be scaled to fit the Quantum size according to
  //  the range of MaxRGB
  const double divisor = static_cast<double>((uint64_t (1) << bitdepth) - 1)
                         / MaxRGB;

  const P *img_fvec = img.fortran_vec ();
  const P *a_fvec   = alpha.fortran_vec ();
  switch (type)
    {
    case Magick::GrayscaleType:
      {
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                      type,
                                                      Magick::DirectClass);

            Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
            octave_idx_type GM_idx = 0;
            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    const double grey = double (*img_fvec) / divisor;
                    Magick::Color c (grey, grey, grey);
                    pix[GM_idx] = c;
                    img_fvec++;
                    GM_idx += nCols;
                  }
                GM_idx -= nCols * nRows - 1;
              }
            // Save changes to underlying image.
            m_img.syncPixels ();
            imvec.push_back (m_img);
          }
        break;
      }

    case Magick::GrayscaleMatteType:
      {
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                      type,
                                                      Magick::DirectClass);

            Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
            octave_idx_type GM_idx = 0;
            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    double grey = double (*img_fvec) / divisor;
                    Magick::Color c (grey, grey, grey,
                                     MaxRGB - (double (*a_fvec) / divisor));
                    pix[GM_idx] = c;
                    img_fvec++;
                    a_fvec++;
                    GM_idx += nCols;
                  }
                GM_idx -= nCols * nRows - 1;
              }
            // Save changes to underlying image.
            m_img.syncPixels ();
            imvec.push_back (m_img);
          }
        break;
      }

    case Magick::TrueColorType:
      {
        // The fortran_vec offset for the green and blue channels
        const octave_idx_type G_offset = nCols * nRows;
        const octave_idx_type B_offset = nCols * nRows * 2;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                      type,
                                                      Magick::DirectClass);

            Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
            octave_idx_type GM_idx = 0;
            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    Magick::Color c (double (*img_fvec)          / divisor,
                                     double (img_fvec[G_offset]) / divisor,
                                     double (img_fvec[B_offset]) / divisor);
                    pix[GM_idx] = c;
                    img_fvec++;
                    GM_idx += nCols;
                  }
                GM_idx -= nCols * nRows - 1;
              }
            // Save changes to underlying image.
            m_img.syncPixels ();
            imvec.push_back (m_img);
            img_fvec += B_offset;
          }
        break;
      }

    case Magick::TrueColorMatteType:
      {
        // The fortran_vec offset for the green and blue channels
        const octave_idx_type G_offset = nCols * nRows;
        const octave_idx_type B_offset = nCols * nRows * 2;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                      type,
                                                      Magick::DirectClass);

            Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
            octave_idx_type GM_idx = 0;
            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    Magick::Color c (double (*img_fvec)          / divisor,
                                     double (img_fvec[G_offset]) / divisor,
                                     double (img_fvec[B_offset]) / divisor,
                                     MaxRGB - (double (*a_fvec) / divisor));
                    pix[GM_idx] = c;
                    img_fvec++;
                    a_fvec++;
                    GM_idx += nCols;
                  }
                GM_idx -= nCols * nRows - 1;
              }
            // Save changes to underlying image.
            m_img.syncPixels ();
            imvec.push_back (m_img);
            img_fvec += B_offset;
          }
        break;
      }

    case Magick::ColorSeparationType:
      {
        // The fortran_vec offset for the Magenta, Yellow, and blacK channels
        const octave_idx_type M_offset = nCols * nRows;
        const octave_idx_type Y_offset = nCols * nRows * 2;
        const octave_idx_type K_offset = nCols * nRows * 3;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                      type,
                                                      Magick::DirectClass);

            Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
            octave_idx_type GM_idx = 0;
            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    Magick::Color c (double (*img_fvec)          / divisor,
                                     double (img_fvec[M_offset]) / divisor,
                                     double (img_fvec[Y_offset]) / divisor,
                                     double (img_fvec[K_offset]) / divisor);
                    pix[GM_idx] = c;
                    img_fvec++;
                    GM_idx += nCols;
                  }
                GM_idx -= nCols * nRows - 1;
              }
            // Save changes to underlying image.
            m_img.syncPixels ();
            imvec.push_back (m_img);
            img_fvec += K_offset;
          }
        break;
      }

    case Magick::ColorSeparationMatteType:
      {
        // The fortran_vec offset for the Magenta, Yellow, and blacK channels
        const octave_idx_type M_offset = nCols * nRows;
        const octave_idx_type Y_offset = nCols * nRows * 2;
        const octave_idx_type K_offset = nCols * nRows * 3;
        for (octave_idx_type frame = 0; frame < nFrames; frame++)
          {
            OCTAVE_QUIT;
            Magick::Image m_img = init_enconde_image (nCols, nRows, bitdepth,
                                                      type,
                                                      Magick::DirectClass);

            Magick::PixelPacket *pix = m_img.getPixels (0, 0, nCols, nRows);
            Magick::IndexPacket *ind = m_img.getIndexes ();
            octave_idx_type GM_idx = 0;
            for (octave_idx_type col = 0; col < nCols; col++)
              {
                for (octave_idx_type row = 0; row < nRows; row++)
                  {
                    Magick::Color c (double (*img_fvec)          / divisor,
                                     double (img_fvec[M_offset]) / divisor,
                                     double (img_fvec[Y_offset]) / divisor,
                                     double (img_fvec[K_offset]) / divisor);
                    pix[GM_idx] = c;
                    ind[GM_idx] = MaxRGB - (double (*a_fvec) / divisor);
                    img_fvec++;
                    a_fvec++;
                    GM_idx += nCols;
                  }
                GM_idx -= nCols * nRows - 1;
              }
            // Save changes to underlying image.
            m_img.syncPixels ();
            imvec.push_back (m_img);
            img_fvec += K_offset;
          }
        break;
      }

    default:
      {
        error ("__magick_write__: unrecognized Magick::ImageType");
        return;
      }
    }
  return;
}

// Meant to be shared with both imfinfo and imwrite.
static std::map<octave_idx_type, std::string>
init_disposal_methods ()
{
  //  GIF Specifications:
  //
  // Disposal Method - Indicates the way in which the graphic is to
  //                    be treated after being displayed.
  //
  //  0 -   No disposal specified. The decoder is
  //        not required to take any action.
  //  1 -   Do not dispose. The graphic is to be left
  //        in place.
  //  2 -   Restore to background color. The area used by the
  //        graphic must be restored to the background color.
  //  3 -   Restore to previous. The decoder is required to
  //        restore the area overwritten by the graphic with
  //        what was there prior to rendering the graphic.
  //  4-7 - To be defined.
  static std::map<octave_idx_type, std::string> methods;
  if (methods.empty ())
    {
      methods[0] = "doNotSpecify";
      methods[1] = "leaveInPlace";
      methods[2] = "restoreBG";
      methods[3] = "restorePrevious";
    }
  return methods;
}
static std::map<std::string, octave_idx_type>
init_reverse_disposal_methods ()
{
  static std::map<std::string, octave_idx_type> methods;
  if (methods.empty ())
    {
      methods["donotspecify"]     = 0;
      methods["leaveinplace"]     = 1;
      methods["restorebg"]        = 2;
      methods["restoreprevious"]  = 3;
    }
  return methods;
}

void static
write_file (const std::string& filename,
            const std::string& ext,
            std::vector<Magick::Image>& imvec)
{
  try
    {
      Magick::writeImages (imvec.begin (), imvec.end (), ext + ":" + filename);
    }
  catch (Magick::Warning& w)
    {
      warning ("Magick++ warning: %s", w.what ());
    }
  catch (Magick::ErrorCoder& e)
    {
      warning ("Magick++ coder error: %s", e.what ());
    }
  catch (Magick::Exception& e)
    {
      error ("Magick++ exception: %s", e.what ());
      error_state = 1;
    }
}

#endif

DEFUN_DLD (__magick_write__, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_write__ (@var{fname}, @var{fmt}, @var{img}, @var{map}, @var{options})\n\
Write image with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.\n\
Use @code{imwrite} instead.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
@end deftypefn")
{
  octave_value_list retval;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imwrite", "Image IO");
#else

  maybe_initialize_magick ();

  if (args.length () != 5 || ! args(0).is_string () || ! args(1).is_string ())
    {
      print_usage ();
      return retval;
    }
  const std::string filename = args(0).string_value ();
  const std::string ext      = args(1).string_value ();

  const octave_scalar_map options = args(4).scalar_map_value ();
  if (error_state)
    {
      error ("__magick_write__: OPTIONS must be a struct");
      return retval;
    }

  const octave_value img  = args(2);
  const Matrix       cmap = args(3).matrix_value ();
  if (error_state)
    {
      error ("__magick_write__: invalid IMG or MAP");
      return retval;
    }

  std::vector<Magick::Image> imvec;

  if (cmap.is_empty ())
    {
      const octave_value alpha = options.getfield ("alpha");
      if (img.is_bool_type ())
        encode_bool_image (imvec, img.bool_array_value ());
      else if (img.is_uint8_type ())
        encode_uint_image<uint8NDArray>  (imvec, img.uint8_array_value (),
                                          alpha.uint8_array_value ());
      else if (img.is_uint16_type ())
        encode_uint_image<uint16NDArray> (imvec, img.uint16_array_value (),
                                          alpha.uint16_array_value ());
      else if (img.is_uint32_type ())
        encode_uint_image<uint32NDArray> (imvec, img.uint32_array_value (),
                                          alpha.uint32_array_value ());
      else if (img.is_float_type ())
        {
          // For image formats that support floating point values, we write
          // the actual values. For those who don't, we only use the values
          // on the range [0 1] and save integer values.
          // But here, even for formats that would support floating point
          // values, GM seems unable to do that so we at least make them uint32.
          uint32NDArray clip_img;
          uint32NDArray clip_alpha;
          if (img.is_single_type ())
            {
              clip_img   = img_float2uint<FloatNDArray>
                             (img.float_array_value ());
              clip_alpha = img_float2uint<FloatNDArray>
                             (alpha.float_array_value ());
            }
          else
            {
              clip_img   = img_float2uint<NDArray> (img.array_value ());
              clip_alpha = img_float2uint<NDArray> (alpha.array_value ());
            }
          encode_uint_image<uint32NDArray> (imvec, clip_img, clip_alpha);
        }
      else
        {
          error ("__magick_write__: image type not supported");
          return retval;
        }
    }
  else
    {
      // We should not get floating point indexed images here because we
      // converted them in __imwrite__.m. We should probably do it here
      // but it would look much messier.
      if (img.is_uint8_type ())
        encode_indexed_images<uint8NDArray>  (imvec, img.uint8_array_value (),
                                              cmap);
      else if (img.is_uint16_type ())
        encode_indexed_images<uint16NDArray> (imvec, img.uint16_array_value (),
                                              cmap);
      else
        {
          error ("__magick_write__: indexed image must be uint8, uint16 or float.");
          return retval;
        }
    }
  static std::map<std::string, octave_idx_type> disposal_methods
    = init_reverse_disposal_methods ();

  const octave_idx_type nFrames = imvec.size ();

  const octave_idx_type quality = options.getfield ("quality").int_value ();
  const ColumnVector delaytime =
    options.getfield ("delaytime").column_vector_value ();
  const Array<std::string> disposalmethod =
    options.getfield ("disposalmethod").cellstr_value ();
  for (octave_idx_type i = 0; i < nFrames; i++)
    {
      imvec[i].quality (quality);
      imvec[i].animationDelay (delaytime(i));
      imvec[i].gifDisposeMethod (disposal_methods[disposalmethod(i)]);
    }

  // If writemode is set to append, read the image and append to it. Even
  // if set to append, make sure that something was read at all.
  const std::string writemode = options.getfield ("writemode").string_value ();
  if (writemode == "append" && file_stat (filename).exists ())
    {
      std::vector<Magick::Image> ini_imvec;
      read_file (filename, ini_imvec);
      if (error_state)
        return retval;
      if (ini_imvec.size () > 0)
        {
          ini_imvec.insert (ini_imvec.end (), imvec.begin (), imvec.end ());
          ini_imvec.swap (imvec);
        }
    }

  // FIXME: LoopCount or animationIterations
  //  How it should work:
  //
  // This value is only set for the first image in the sequence. Trying
  // to set this value with the append mode should have no effect, the
  // value used with the first image is the one that counts (that would
  // also be Matlab compatible). Thus, the right way to do this would be
  // to have an else block on the condition above, and set this only
  // when creating a new file. Since Matlab does not interpret a 4D
  // matrix as sequence of images to write, its users need to use a for
  // loop and set LoopCount only on the first iteration (it actually
  // throws warnings otherwise)
  //
  //  Why is this not done the right way:
  //
  // When GM saves a single image, it discards the value if there is only
  // a single image and sets it to "no loop".  Since our default is an
  // infinite loop, if the user tries to do it the Matlab way (setting
  // LoopCount only on the first image) that value will go nowhere.
  // See https://sourceforge.net/p/graphicsmagick/bugs/248/
  // Because of this, we document to set LoopCount on every iteration
  // (in Matlab will cause a lot of warnings), or pass a 4D matrix with
  // all frames (won't work in Matlab at all).
  // Note that this only needs to be set on the first frame
  imvec[0].animationIterations (options.getfield ("loopcount").uint_value ());

  write_file (filename, ext, imvec);
  if (error_state)
    return retval;

#endif
  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

// Gets the minimum information from images such as its size and format. Much
// faster than using imfinfo, which slows down a lot since. Note than without
// this, we need to read the image once for imfinfo to set defaults (which is
// done in Octave language), and then again for the actual reading.
DEFUN_DLD (__magick_ping__, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_ping__ (@var{fname}, @var{idx})\n\
Ping image information with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.\n\
\n\
@seealso{imfinfo}\n\
@end deftypefn")
{
  octave_value retval;
#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imfinfo", "Image IO");
#else
  maybe_initialize_magick ();

  if (args.length () < 1 || ! args(0).is_string ())
    {
      print_usage ();
      return retval;
    }
  const std::string filename = args(0).string_value ();
  int idx;
  if (args.length () > 1)
    idx = args(1).int_value () -1;
  else
    idx = 0;

  Magick::Image img;
  img.subImage (idx); // start ping from this image (in case of multi-page)
  img.subRange (1);   // ping only one of them
  try
    {
      img.ping (filename);
    }
  catch (Magick::Warning& w)
    {
      warning ("Magick++ warning: %s", w.what ());
    }
  catch (Magick::Exception& e)
    {
      error ("Magick++ exception: %s", e.what ());
      return retval;
    }

  static const char *fields[] = {"rows", "columns", "format", 0};
  octave_scalar_map ping = octave_scalar_map (string_vector (fields));
  ping.setfield ("rows",    octave_value (img.rows ()));
  ping.setfield ("columns", octave_value (img.columns ()));
  ping.setfield ("format",  octave_value (img.magick ()));
  retval = octave_value (ping);
#endif
  return retval;
}

#ifdef HAVE_MAGICK
static octave_value
magick_to_octave_value (const Magick::CompressionType& magick)
{
  switch (magick)
    {
    case Magick::NoCompression:
      return octave_value ("none");
    case Magick::BZipCompression:
      return octave_value ("bzip");
    case Magick::FaxCompression:
      return octave_value ("fax3");
    case Magick::Group4Compression:
      return octave_value ("fax4");
    case Magick::JPEGCompression:
      return octave_value ("jpeg");
    case Magick::LZWCompression:
      return octave_value ("lzw");
    case Magick::RLECompression:
      // This is named "rle" for the HDF, but the same thing is named
      // "ccitt" and "PackBits" for binary and non-binary images in TIFF.
      return octave_value ("rle");
    case Magick::ZipCompression:
      return octave_value ("deflate");

      // The following are present only in recent versions of GraphicsMagick.
      // At the moment the only use of this would be to have imfinfo report
      // the compression method. In the future, someone could implement
      // the Compression option for imwrite in which case a macro in
      // configure.ac will have to check for their presence of this.
      // See bug #39913
      //      case Magick::LZMACompression:
      //        return octave_value ("lzma");
      //      case Magick::JPEG2000Compression:
      //        return octave_value ("jpeg2000");
      //      case Magick::JBIG1Compression:
      //        return octave_value ("jbig1");
      //      case Magick::JBIG2Compression:
      //        return octave_value ("jbig2");
    default:
      return octave_value ("undefined");
    }
}

static octave_value
magick_to_octave_value (const Magick::EndianType& magick)
{
  switch (magick)
    {
    case Magick::LSBEndian:
      return octave_value ("little-endian");
    case Magick::MSBEndian:
      return octave_value ("big-endian");
    default:
      return octave_value ("undefined");
    }
}

static octave_value
magick_to_octave_value (const Magick::OrientationType& magick)
{
  switch (magick)
    {
      // Values come from the TIFF6 spec
    case Magick::TopLeftOrientation:
      return octave_value (1);
    case Magick::TopRightOrientation:
      return octave_value (2);
    case Magick::BottomRightOrientation:
      return octave_value (3);
    case Magick::BottomLeftOrientation:
      return octave_value (4);
    case Magick::LeftTopOrientation:
      return octave_value (5);
    case Magick::RightTopOrientation:
      return octave_value (6);
    case Magick::RightBottomOrientation:
      return octave_value (7);
    case Magick::LeftBottomOrientation:
      return octave_value (8);
    default:
      return octave_value (1);
    }
}

static octave_value
magick_to_octave_value (const Magick::ResolutionType& magick)
{
  switch (magick)
    {
    case Magick::PixelsPerInchResolution:
      return octave_value ("Inch");
    case Magick::PixelsPerCentimeterResolution:
      return octave_value ("Centimeter");
    default:
      return octave_value ("undefined");
    }
}

static bool
is_valid_exif (const std::string& val)
{
  // Sometimes GM will return the string "unknown" instead of empty
  // for an empty value.
  return (! val.empty () && val != "unknown");
}

static void
fill_exif (octave_scalar_map& map, Magick::Image& img,
           const std::string& key)
{
  const std::string attr = img.attribute ("EXIF:" + key);
  if (is_valid_exif (attr))
    map.setfield (key, octave_value (attr));
  return;
}

static void
fill_exif_ints (octave_scalar_map& map, Magick::Image& img,
                const std::string& key)
{
  const std::string attr = img.attribute ("EXIF:" + key);
  if (is_valid_exif (attr))
    {
      // string of the type "float,float,float....."
      float number;
      ColumnVector values (std::count (attr.begin (), attr.end (), ',') +1);
      std::string sub;
      std::istringstream sstream (attr);
      octave_idx_type n = 0;
      while (std::getline (sstream, sub, char (',')))
        {
          sscanf (sub.c_str (), "%f", &number);
          values(n++) = number;
        }
      map.setfield (key, octave_value (values));
    }
  return;
}

static void
fill_exif_floats (octave_scalar_map& map, Magick::Image& img,
                  const std::string& key)
{
  const std::string attr = img.attribute ("EXIF:" + key);
  if (is_valid_exif (attr))
    {
      // string of the type "int/int,int/int,int/int....."
      int numerator;
      int denominator;
      ColumnVector values (std::count (attr.begin (), attr.end (), ',') +1);
      std::string sub;
      std::istringstream sstream (attr);
      octave_idx_type n = 0;
      while (std::getline (sstream, sub, ','))
        {
          sscanf (sub.c_str (), "%i/%i", &numerator, &denominator);
          values(n++) = double (numerator) / double (denominator);
        }
      map.setfield (key, octave_value (values));
    }
  return;
}

#endif

DEFUN_DLD (__magick_finfo__, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_finfo__ (@var{fname})\n\
Read image information with GraphicsMagick or ImageMagick.\n\
\n\
This is a private internal function not intended for direct use.\n\
Use @code{imfinfo} instead.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
@end deftypefn")
{
  octave_value retval;

#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imfinfo", "Image IO");
#else
  maybe_initialize_magick ();

  if (args.length () < 1 || ! args(0).is_string ())
    {
      print_usage ();
      return retval;
    }
  const std::string filename = args(0).string_value ();

  std::vector<Magick::Image> imvec;
  read_file (filename, imvec);
  if (error_state)
    return retval;
  const octave_idx_type nFrames = imvec.size ();
  const std::string format = imvec[0].magick ();

  // Here's how this function works. We need to return a struct array, one
  // struct for each image in the file (remember, there are image
  // that allow for multiple images in the same file). Now, Matlab seems
  // to have format specific code so the fields on the struct are different
  // for each format. It only has a small subset that is common to all
  // of them, the others are undocumented. Because we try to abstract from
  // the formats we always return the same list of fields (note that with
  // GM we support more than 88 formats. That's way more than Matlab, and
  // I don't want to write specific code for each of them).
  //
  // So what we do is we create an octave_scalar_map, fill it with the
  // information for that image, and then insert it into an octave_map.
  // Because in the same file, different images may have values for
  // different fields, we can't create a field only if there's a value.
  // Bad things happen if we merge octave_scalar_maps with different
  // fields from the others (suppose for example a TIFF file with 4 images,
  // where only the third image has a colormap.

  static const char *fields[] =
  {
    // These are fields that must always appear for Matlab.
    "Filename",
    "FileModDate",
    "FileSize",
    "Format",
    "FormatVersion",
    "Width",
    "Height",
    "BitDepth",
    "ColorType",

    // These are format specific or not existent in Matlab. The most
    // annoying thing is that Matlab may have different names for the
    // same thing in different formats.
    "DelayTime",
    "DisposalMethod",
    "LoopCount",
    "ByteOrder",
    "Gamma",
    "Chromaticities",
    "Comment",
    "Quality",
    "Compression",        // same as CompressionType
    "Colormap",           // same as ColorTable (in PNG)
    "Orientation",
    "ResolutionUnit",
    "XResolution",
    "YResolution",
    "Software",           // sometimes is an Exif tag
    "Make",               // actually an Exif tag
    "Model",              // actually an Exif tag
    "DateTime",           // actually an Exif tag
    "ImageDescription",   // actually an Exif tag
    "Artist",             // actually an Exif tag
    "Copyright",          // actually an Exif tag
    "DigitalCamera",
    "GPSInfo",
    // Notes for the future: GM allows one to get many attributes, and even has
    // attribute() to obtain arbitrary ones, that may exist in only some
    // cases. The following is a list of some methods and into what possible
    // Matlab compatible values they may be converted.
    //
    //  colorSpace()      -> PhotometricInterpretation
    //  backgroundColor() -> BackgroundColor
    //  interlaceType()   -> Interlaced, InterlaceType, and PlanarConfiguration
    //  label()           -> Title
    0
  };

  // The one we will return at the end
  octave_map info (dim_vector (nFrames, 1), string_vector (fields));

  // Some of the fields in the struct are about file information and will be
  // the same for all images in the file. So we create a template, fill in
  // those values, and make a copy of the template for each image.
  octave_scalar_map template_info = (string_vector (fields));

  template_info.setfield ("Format", octave_value (format));
  // We can't actually get FormatVersion but even Matlab sometimes can't.
  template_info.setfield ("FormatVersion", octave_value (""));

  const file_stat fs (filename);
  if (fs)
    {
      const octave_localtime mtime (fs.mtime ());
      const std::string filetime = mtime.strftime ("%e-%b-%Y %H:%M:%S");
      template_info.setfield ("Filename",    octave_value (filename));
      template_info.setfield ("FileModDate", octave_value (filetime));
      template_info.setfield ("FileSize",    octave_value (fs.size ()));
    }
  else
    {
      error ("imfinfo: error reading '%s': %s",
             filename.c_str (), fs.error ().c_str ());
      return retval;
    }

  for (octave_idx_type frame = 0; frame < nFrames; frame++)
    {
      OCTAVE_QUIT;
      octave_scalar_map info_frame (template_info);
      const Magick::Image img = imvec[frame];

      info_frame.setfield ("Width",  octave_value (img.columns ()));
      info_frame.setfield ("Height", octave_value (img.rows ()));
      info_frame.setfield ("BitDepth",
                           octave_value (get_depth (const_cast<Magick::Image&> (img))));

      // Stuff related to colormap, image class and type
      // Because GM is too smart for us... Read the comments in is_indexed()
      {
        std::string color_type;
        Matrix cmap;
        if (is_indexed (img))
          {
            color_type = "indexed";
            cmap =
              read_maps (const_cast<Magick::Image&> (img))(0).matrix_value ();
          }
        else
          {
            switch (img.type ())
              {
              case Magick::BilevelType:
              case Magick::GrayscaleType:
              case Magick::GrayscaleMatteType:
                color_type = "grayscale";
                break;

              case Magick::TrueColorType:
              case Magick::TrueColorMatteType:
                color_type = "truecolor";
                break;

              case Magick::PaletteType:
              case Magick::PaletteMatteType:
                // we should never get here or is_indexed needs to be fixed
                color_type = "indexed";
                break;

              case Magick::ColorSeparationType:
              case Magick::ColorSeparationMatteType:
                color_type = "CMYK";
                break;

              default:
                color_type = "undefined";
              }
          }
        info_frame.setfield ("ColorType", octave_value (color_type));
        info_frame.setfield ("Colormap",  octave_value (cmap));
      }

      {
        // Not all images have chroma values. In such cases, they'll
        // be all zeros. So rather than send a matrix of zeros, we will
        // check for that, and send an empty vector instead.
        RowVector chromaticities (8);
        double* chroma_fvec = chromaticities.fortran_vec ();
        img.chromaWhitePoint    (&chroma_fvec[0], &chroma_fvec[1]);
        img.chromaRedPrimary    (&chroma_fvec[2], &chroma_fvec[3]);
        img.chromaGreenPrimary  (&chroma_fvec[4], &chroma_fvec[5]);
        img.chromaBluePrimary   (&chroma_fvec[6], &chroma_fvec[7]);
        if (chromaticities.nnz () == 0)
          chromaticities = RowVector (0);
        info_frame.setfield ("Chromaticities", octave_value (chromaticities));
      }

      info_frame.setfield ("Gamma",       octave_value (img.gamma ()));
      info_frame.setfield ("XResolution", octave_value (img.xResolution ()));
      info_frame.setfield ("YResolution", octave_value (img.yResolution ()));
      info_frame.setfield ("DelayTime",   octave_value (img.animationDelay ()));
      info_frame.setfield ("LoopCount",
                           octave_value (img.animationIterations ()));
      info_frame.setfield ("Quality",     octave_value (img.quality ()));
      info_frame.setfield ("Comment",     octave_value (img.comment ()));

      info_frame.setfield ("Compression",
                           magick_to_octave_value (img.compressType ()));
      info_frame.setfield ("Orientation",
                           magick_to_octave_value (img.orientation ()));
      info_frame.setfield ("ResolutionUnit",
                           magick_to_octave_value (img.resolutionUnits ()));
      info_frame.setfield ("ByteOrder",
                           magick_to_octave_value (img.endian ()));

      // It is not possible to know if there's an Exif field so we just
      // check for the Exif Version value. If it does exists, then we
      // bother about looking for specific fields.
      {
        Magick::Image& cimg = const_cast<Magick::Image&> (img);

        // These will be in Exif tags but must appear as fields in the
        // base struct array, not as another struct in one of its fields.
        // This is likely because they belong to the Baseline TIFF specs
        // and may appear out of the Exif tag. So first we check if it
        // exists outside the Exif tag.
        // See Section 4.6.4, table 4, page 28 of Exif specs version 2.3
        // (CIPA DC- 008-Translation- 2010)
        static const char *base_exif_str_fields[] =
        {
          "DateTime",
          "ImageDescription",
          "Make",
          "Model",
          "Software",
          "Artist",
          "Copyright",
          0,
        };
        static const string_vector base_exif_str (base_exif_str_fields);
        static const octave_idx_type n_base_exif_str = base_exif_str.numel ();
        for (octave_idx_type field = 0; field < n_base_exif_str; field++)
          {
            info_frame.setfield (base_exif_str[field],
                                 octave_value (cimg.attribute (base_exif_str[field])));
            fill_exif (info_frame, cimg, base_exif_str[field]);
          }

        octave_scalar_map camera;
        octave_scalar_map gps;
        if (! cimg.attribute ("EXIF:ExifVersion").empty ())
          {
            // See Section 4.6.5, table 7 and 8, over pages page 42 to 43
            // of Exif specs version 2.3 (CIPA DC- 008-Translation- 2010)

            // Listed on the Exif specs as being of type ASCII.
            static const char *exif_str_fields[] =
            {
              "RelatedSoundFile",
              "DateTimeOriginal",
              "DateTimeDigitized",
              "SubSecTime",
              "DateTimeOriginal",
              "SubSecTimeOriginal",
              "SubSecTimeDigitized",
              "ImageUniqueID",
              "CameraOwnerName",
              "BodySerialNumber",
              "LensMake",
              "LensModel",
              "LensSerialNumber",
              "SpectralSensitivity",
              // These last two are of type undefined but most likely will
              // be strings. Even if they're not GM returns a string anyway.
              "UserComment",
              "MakerComment",
              0
            };
            static const string_vector exif_str (exif_str_fields);
            static const octave_idx_type n_exif_str = exif_str.numel ();
            for (octave_idx_type field = 0; field < n_exif_str; field++)
              fill_exif (camera, cimg, exif_str[field]);

            // Listed on the Exif specs as being of type SHORT or LONG.
            static const char *exif_int_fields[] =
            {
              "ColorSpace",
              "ExifImageWidth",  // PixelXDimension (CPixelXDimension in Matlab)
              "ExifImageHeight", // PixelYDimension (CPixelYDimension in Matlab)
              "PhotographicSensitivity",
              "StandardOutputSensitivity",
              "RecommendedExposureIndex",
              "ISOSpeed",
              "ISOSpeedLatitudeyyy",
              "ISOSpeedLatitudezzz",
              "FocalPlaneResolutionUnit",
              "FocalLengthIn35mmFilm",
              // Listed as SHORT or LONG but with more than 1 count.
              "SubjectArea",
              "SubjectLocation",
              // While the following are an integer, their value have a meaning
              // that must be represented as a string for Matlab compatibility.
              // For example, a 3 on ExposureProgram, would return
              // "Aperture priority" as defined on the Exif specs.
              "ExposureProgram",
              "SensitivityType",
              "MeteringMode",
              "LightSource",
              "Flash",
              "SensingMethod",
              "FileSource",
              "CustomRendered",
              "ExposureMode",
              "WhiteBalance",
              "SceneCaptureType",
              "GainControl",
              "Contrast",
              "Saturation",
              "Sharpness",
              "SubjectDistanceRange",
              0
            };
            static const string_vector exif_int (exif_int_fields);
            static const octave_idx_type n_exif_int = exif_int.numel ();
            for (octave_idx_type field = 0; field < n_exif_int; field++)
              fill_exif_ints (camera, cimg, exif_int[field]);

            // Listed as RATIONAL or SRATIONAL
            static const char *exif_float_fields[] =
            {
              "Gamma",
              "CompressedBitsPerPixel",
              "ExposureTime",
              "FNumber",
              "ShutterSpeedValue",  // SRATIONAL
              "ApertureValue",
              "BrightnessValue",    // SRATIONAL
              "ExposureBiasValue",  // SRATIONAL
              "MaxApertureValue",
              "SubjectDistance",
              "FocalLength",
              "FlashEnergy",
              "FocalPlaneXResolution",
              "FocalPlaneYResolution",
              "ExposureIndex",
              "DigitalZoomRatio",
              // Listed as RATIONAL or SRATIONAL with more than 1 count.
              "LensSpecification",
              0
            };
            static const string_vector exif_float (exif_float_fields);
            static const octave_idx_type n_exif_float = exif_float.numel ();
            for (octave_idx_type field = 0; field < n_exif_float; field++)
              fill_exif_floats (camera, cimg, exif_float[field]);

            // Inside a Exif field, it is possible that there is also a
            // GPS field. This is not the same as ExifVersion but seems
            // to be how we have to check for it.
            if (cimg.attribute ("EXIF:GPSInfo") != "unknown")
              {
                // The story here is the same as with Exif.
                // See Section 4.6.6, table 15 on page 68 of Exif specs
                // version 2.3 (CIPA DC- 008-Translation- 2010)

                static const char *gps_str_fields[] =
                {
                  "GPSLatitudeRef",
                  "GPSLongitudeRef",
                  "GPSAltitudeRef",
                  "GPSSatellites",
                  "GPSStatus",
                  "GPSMeasureMode",
                  "GPSSpeedRef",
                  "GPSTrackRef",
                  "GPSImgDirectionRef",
                  "GPSMapDatum",
                  "GPSDestLatitudeRef",
                  "GPSDestLongitudeRef",
                  "GPSDestBearingRef",
                  "GPSDestDistanceRef",
                  "GPSDateStamp",
                  0
                };
                static const string_vector gps_str (gps_str_fields);
                static const octave_idx_type n_gps_str = gps_str.numel ();
                for (octave_idx_type field = 0; field < n_gps_str; field++)
                  fill_exif (gps, cimg, gps_str[field]);

                static const char *gps_int_fields[] =
                {
                  "GPSDifferential",
                  0
                };
                static const string_vector gps_int (gps_int_fields);
                static const octave_idx_type n_gps_int = gps_int.numel ();
                for (octave_idx_type field = 0; field < n_gps_int; field++)
                  fill_exif_ints (gps, cimg, gps_int[field]);

                static const char *gps_float_fields[] =
                {
                  "GPSAltitude",
                  "GPSDOP",
                  "GPSSpeed",
                  "GPSTrack",
                  "GPSImgDirection",
                  "GPSDestBearing",
                  "GPSDestDistance",
                  "GPSHPositioningError",
                  // Listed as RATIONAL or SRATIONAL with more than 1 count.
                  "GPSLatitude",
                  "GPSLongitude",
                  "GPSTimeStamp",
                  "GPSDestLatitude",
                  "GPSDestLongitude",
                  0
                };
                static const string_vector gps_float (gps_float_fields);
                static const octave_idx_type n_gps_float = gps_float.numel ();
                for (octave_idx_type field = 0; field < n_gps_float; field++)
                  fill_exif_floats (gps, cimg, gps_float[field]);

              }
          }
        info_frame.setfield ("DigitalCamera", octave_value (camera));
        info_frame.setfield ("GPSInfo",       octave_value (gps));
      }

      info.fast_elem_insert (frame, info_frame);
    }

  if (format == "GIF")
    {
      static std::map<octave_idx_type, std::string> disposal_methods
        = init_disposal_methods ();
      string_vector methods (nFrames);
      for (octave_idx_type frame = 0; frame < nFrames; frame++)
        methods[frame] = disposal_methods[imvec[frame].gifDisposeMethod ()];
      info.setfield ("DisposalMethod", Cell (methods));
    }
  else
    info.setfield ("DisposalMethod",
                   Cell (dim_vector (nFrames, 1), octave_value ("")));

  retval = octave_value (info);
#endif
  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

DEFUN_DLD (__magick_formats__, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __magick_imformats__ (@var{formats})\n\
Fill formats info with GraphicsMagick CoderInfo.\n\
\n\
@seealso{imfinfo, imformats, imread, imwrite}\n\
@end deftypefn")
{
  octave_value retval;
#ifndef HAVE_MAGICK
  gripe_disabled_feature ("imformats", "Image IO");
#else
  if (args.length () != 1 || ! args(0).is_map ())
    {
      print_usage ();
      return retval;
    }
  octave_map formats = args(0).map_value ();

  maybe_initialize_magick ();
  for (octave_idx_type idx = 0; idx < formats.numel (); idx++)
    {
      try
        {
          octave_scalar_map fmt = formats.checkelem (idx);
          Magick::CoderInfo coder (fmt.getfield ("coder").string_value ());

          fmt.setfield ("description", octave_value (coder.description ()));
          fmt.setfield ("multipage", coder.isMultiFrame () ? true : false);
          // default for read and write is a function handle. If we can't
          // read or write them, them set it to an empty value
          if (! coder.isReadable ())
            fmt.setfield ("read",  Matrix ());
          if (! coder.isWritable ())
            fmt.setfield ("write", Matrix ());
          formats.fast_elem_insert (idx, fmt);
        }
      catch (Magick::Exception& e)
        {
          // Exception here are missing formats. So we remove the format
          // from the structure and reduce idx.
          formats.delete_elements (idx);
          idx--;
        }
    }
  retval = formats;
#endif
  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
