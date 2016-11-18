/*

Copyright (C) 2013-2015 Vytautas Jančauskas

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

#include <string>
#include <map>

#include "oct-locbuf.h"
#include "unwind-prot.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-struct.h"

#ifdef HAVE_SNDFILE
#include <sndfile.h>
#endif

#ifdef HAVE_SNDFILE
static void
safe_close (SNDFILE *file)
{
  sf_close (file);
}
#endif

DEFUN_DLD (audioread, args, ,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{samples})\n\
\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{datatype})\n\
@deftypefnx {Loadable Function} {[@var{y}, @var{fs}] =} audioread (@var{filename}, @var{samples}, @var{datatype})\n\
Read the audio file @var{filename} and return the audio data @var{y} and\n\
sampling rate @var{fs}.\n\
\n\
The audio data is stored as matrix with rows corresponding to audio frames\n\
and columns corresponding to channels.\n\
\n\
The optional two-element vector argument @var{samples} specifies starting\n\
and ending frames.\n\
\n\
The optional argument @var{datatype} specifies the datatype to return.\n\
If it is @qcode{\"native\"}, then the type of data depends on how the data\n\
is stored in the audio file.\n\
@end deftypefn")
{
  octave_value_list retval;

#ifdef HAVE_SNDFILE

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  std::string filename = args(0).string_value ();

  if (error_state)
    return retval;

  SF_INFO info;
  info.format = 0;
  SNDFILE *file = sf_open (filename.c_str (), SFM_READ, &info);

  if (! file)
    {
      error ("audioread: failed to open input file %s", filename.c_str ());
      return retval;
    }

  unwind_protect frame;

  frame.add_fcn (safe_close, file);

  OCTAVE_LOCAL_BUFFER (float, data, info.frames * info.channels);

  sf_read_float (file, data, info.frames * info.channels);

  sf_count_t start = 0;
  sf_count_t end = info.frames;

  if ((nargin == 2 && ! args(1).is_string ()) || nargin == 3)
    {
      RowVector range = args(1).row_vector_value ();

      if (error_state)
        return retval;

      if (range.nelem () != 2)
        {
          error ("audioread: invalid specification for range of frames");
          return retval;
        }

      double dstart = xisinf (range(0)) ? info.frames : range(0);
      double dend = xisinf (range(1)) ? info.frames : range(1);

      if (dstart < 1 || dstart > dend || dend > info.frames
          || D_NINT (dstart) != dstart || D_NINT (dend) != dend)
        {
          error ("audioread: invalid specification for range of frames");
          return retval;
        }

      start = dstart - 1;
      end = dend;
    }

  sf_count_t items = end - start;

  Matrix audio (items, info.channels);

  double *paudio = audio.fortran_vec ();

  data += start * info.channels;

  for (int i = 0; i < items; i++)
    {
      for (int channel = 0; channel < info.channels; channel++)
        paudio[items*channel+i] = *data++;
    }

  octave_value ret_audio;

  if ((nargin == 2 && args(1).is_string ()) || nargin == 3)
    {
      std::string type;
      if (nargin == 3)
        type = args(2).string_value ();
      else
        type = args(1).string_value ();

      if (error_state)
        return retval;

      if (type == "native")
        {
          switch (info.format & SF_FORMAT_SUBMASK)
            {
            case SF_FORMAT_PCM_S8:
              ret_audio = int8NDArray (audio * 127);
              break;
            case SF_FORMAT_PCM_U8:
              ret_audio = uint8NDArray (audio * 127 + 127);
              break;
            case SF_FORMAT_PCM_16:
              ret_audio = int16NDArray (audio * 32767);
              break;
            case SF_FORMAT_PCM_24:
              ret_audio = int32NDArray (audio * 8388608);
              break;
            case SF_FORMAT_PCM_32:
              ret_audio = int32NDArray (audio * 2147483648);
              break;
            default:
              ret_audio = audio;
              break;
            }
        }
      else
        ret_audio = audio;
    }
  else
    ret_audio = audio;

  retval(1) = info.samplerate;
  retval(0) = ret_audio;

#else

  error ("sndfile not found on your system and thus audioread is not functional");

#endif

  return retval;
}

#ifdef HAVE_SNDFILE

static int
extension_to_format (const std::string& ext)
{
  static bool initialized = false;

  static std::map<std::string, int> table;

  if (! initialized)
    {
      table["wav"] = SF_FORMAT_WAV;
      table["aiff"] = SF_FORMAT_AIFF;
      table["au"] = SF_FORMAT_AU;
      table["raw"] = SF_FORMAT_RAW;
      table["paf"] = SF_FORMAT_PAF;
      table["svx"] = SF_FORMAT_SVX;
      table["nist"] = SF_FORMAT_NIST;
      table["voc"] = SF_FORMAT_VOC;
      table["ircam"] = SF_FORMAT_IRCAM;
      table["w64"] = SF_FORMAT_W64;
      table["mat4"] = SF_FORMAT_MAT4;
      table["mat5"] = SF_FORMAT_MAT5;
      table["pvf"] = SF_FORMAT_PVF;
      table["xi"] = SF_FORMAT_XI;
      table["htk"] = SF_FORMAT_HTK;
      table["sds"] = SF_FORMAT_SDS;
      table["avr"] = SF_FORMAT_AVR;
      table["wavex"] = SF_FORMAT_WAVEX;
      table["sd2"] = SF_FORMAT_SD2;
      table["flac"] = SF_FORMAT_FLAC;
      table["caf"] = SF_FORMAT_CAF;
      table["wve"] = SF_FORMAT_WVE;
      table["ogg"] = SF_FORMAT_OGG;
      table["mpc2k"] = SF_FORMAT_MPC2K;
      table["rf64"] = SF_FORMAT_RF64;

      initialized = true;
    }

  std::map<std::string, int>::const_iterator it = table.find (ext);

  return (it != table.end ()) ? it->second : 0;
}

#endif

DEFUN_DLD (audiowrite, args, ,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} audiowrite (@var{filename}, @var{y}, @var{fs})\n\
@deftypefnx {Loadable Function} {} audiowrite (@var{filename}, @var{y}, @var{fs}, @var{name}, @var{value}, @dots{})\n\
\n\
Write audio data from the matrix @var{y} to @var{filename} at sampling rate\n\
@var{fs} with the file format determined by the file extension.\n\
\n\
Additional name/value argument pairs may be used to specify the\n\
following options:\n\
\n\
@table @samp\n\
@item BitsPerSample\n\
Number of bits per sample, valid values are 8, 16, 24 and 32.  Default is 16.\n\
\n\
@item BitRate\n\
Valid argument name, but ignored.  Left for compatibility with @sc{matlab}.\n\
\n\
@item Quality\n\
Quality setting for the Ogg Vorbis compressor.  Values can range between 0\n\
and 100 with 100 being the highest quality setting.  Default is 75.\n\
\n\
@item Title\n\
Title for the audio file.\n\
\n\
@item Artist\n\
Artist name.\n\
\n\
@item Comment\n\
Comment.\n\
@end table\n\
@end deftypefn")
{
  // FIXME: shouldn't we return something to indicate whether the file
  // was written successfully?

  octave_value retval;

#ifdef HAVE_SNDFILE

  int nargin = args.length ();

  if (nargin < 3)
    {
      print_usage ();
      return retval;
    }

  std::string filename = args(0).string_value ();

  if (error_state)
    return retval;

  Matrix audio = args(1).matrix_value ();

  if (error_state)
    return retval;

  double bias = 0.0;
  double scale = 1.0;

  if (args(1).is_uint8_type ())
    bias = scale = std::pow (2.0, 7);
  else if (args(1).is_int16_type ())
    scale = std::pow (2.0, 15);
  else if (args(1).is_int32_type ())
    scale = std::pow (2.0, 31);
  else if (args(1).is_integer_type ())
    {
      gripe_wrong_type_arg ("audiowrite", args(1));
      return retval;
    }

  int samplerate = args(2).int_value ();

  if (error_state)
    return retval;

  std::string ext;
  size_t dotpos = filename.find_last_of (".");
  if (dotpos != std::string::npos)
    ext = filename.substr (dotpos + 1);
  std::transform (ext.begin (), ext.end (), ext.begin (), ::tolower);

  sf_count_t items_to_write = audio.rows () * audio.columns ();

  if (audio.rows () == 1)
    audio = audio.transpose ();

  OCTAVE_LOCAL_BUFFER (float, data, items_to_write);

  sf_count_t idx = 0;
  for (int i = 0; i < audio.rows (); i++)
    {
      for (int j = 0; j < audio.columns (); j++)
        {
          double elem = (audio.xelem (i, j) - bias) / scale;
          data[idx++] = std::min (std::max (elem, -1.0), 1.0);
        }
    }

  SF_INFO info;

  memset (&info, 0, sizeof (info)) ;

  sf_count_t chunk_size = 0;

  if (ext == "ogg")
    {
      info.format = SF_FORMAT_VORBIS;

      // FIXME: there seems to be a bug writing ogg files in one shot
      // that causes a segfault.  Breaking it up into a series of
      // smaller chunks seems to avoid the problem and produce valid
      // files.
      chunk_size = 0x1FFFFE;
    }
  else
    info.format = SF_FORMAT_PCM_16;

  info.channels = audio.columns ();
  info.samplerate = samplerate;
  info.channels = audio.cols ();
  info.format |= extension_to_format (ext);

  std::string title = "";
  std::string artist = "";
  std::string comment = "";
  // Quality is currently unused?
  //
  // float quality = 0.75;
  for (int i = 3; i < nargin; i += 2)
    {
      if (args(i).string_value () == "BitsPerSample")
        {
          info.format &= ~SF_FORMAT_SUBMASK;
          int bits = args(i + 1).int_value ();
          if (bits == 8)
            {
              if ((info.format & SF_FORMAT_TYPEMASK) == SF_FORMAT_WAV)
                info.format |= SF_FORMAT_PCM_U8;
              else
                info.format |= SF_FORMAT_PCM_S8;
            }
          else if (bits == 16)
            info.format |= SF_FORMAT_PCM_16;
          else if (bits == 24)
            info.format |= SF_FORMAT_PCM_24;
          else if (bits == 32)
            info.format |= SF_FORMAT_PCM_32;
          else
            {
              error ("audiowrite: wrong number of bits specified");
              return retval;
            }
        }
      else if (args(i).string_value () == "BitRate")
        ;
      // Quality is currently unused?
      //
      // else if (args(i).string_value () == "Quality")
      //   quality = args(i + 1).int_value () * 0.01;
      else if (args(i).string_value () == "Title")
        title = args(i + 1).string_value ();
      else if (args(i).string_value () == "Artist")
        artist = args(i + 1).string_value ();
      else if (args(i).string_value () == "Comment")
        comment = args(i + 1).string_value ();
      else
        {
          error ("audiowrite: wrong argument name");
          return retval;
        }
    }

  SNDFILE *file = sf_open (filename.c_str (), SFM_WRITE, &info);

  if (! file)
    {
      error ("audiowrite: failed to open output file %s", filename.c_str ());
      return retval;
    }

  unwind_protect frame;

  frame.add_fcn (safe_close, file);

  if (title != "")
    sf_set_string (file, SF_STR_TITLE, title.c_str ());

  if (artist != "")
    sf_set_string (file, SF_STR_ARTIST, artist.c_str ());

  if (comment != "")
    sf_set_string (file, SF_STR_COMMENT, comment.c_str ());

  sf_count_t total_items_written = 0;
  sf_count_t offset = 0;

  if (chunk_size == 0)
    chunk_size = items_to_write;

  while (total_items_written < items_to_write)
    {
      if (items_to_write - offset < chunk_size)
        chunk_size = items_to_write - offset;

      sf_count_t items_written = sf_write_float (file, data+offset, chunk_size);

      if (items_written != chunk_size)
        {
          error ("audiowrite: write failed, wrote %ld of %ld items\n",
                 items_written, chunk_size);
          return retval;
        }

      total_items_written += items_written;
      offset += chunk_size;
    }

#else

  error ("sndfile not found on your system and thus audiowrite is not functional");

#endif

  return retval;
}

DEFUN_DLD (audioinfo, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{info} =} audioinfo (@var{filename})\n\
Return information about an audio file specified by @var{filename}.\n\
@end deftypefn")
{
  octave_value retval;

#ifdef HAVE_SNDFILE

  if (args.length () != 1)
    {
      print_usage ();
      return retval;
    }

  std::string filename = args(0).string_value ();

  if (error_state)
    return retval;

  SF_INFO info;
  info.format = 0;
  SNDFILE *file = sf_open (filename.c_str (), SFM_READ, &info);

  if (! file)
    {
      error ("audioinfo: failed to open file %s", filename.c_str ());
      return retval;
    }

  unwind_protect frame;

  frame.add_fcn (safe_close, file);

  octave_scalar_map result;

  result.assign ("Filename", filename);
  result.assign ("CompressionMethod", "");
  result.assign ("NumChannels", info.channels);
  result.assign ("SampleRate", info.samplerate);
  result.assign ("TotalSamples", info.frames);

  double dframes = info.frames;
  double drate = info.samplerate;
  result.assign ("Duration", dframes / drate);

  int bits;
  switch (info.format & SF_FORMAT_SUBMASK)
    {
    case SF_FORMAT_PCM_S8:
      bits = 8;
      break;
    case SF_FORMAT_PCM_U8:
      bits = 8;
      break;
    case SF_FORMAT_PCM_16:
      bits = 16;
      break;
    case SF_FORMAT_PCM_24:
      bits = 24;
      break;
    case SF_FORMAT_PCM_32:
      bits = 32;
      break;
    default:
      bits = -1;
      break;
    }

  result.assign ("BitsPerSample", bits);
  result.assign ("BitRate", -1);
  result.assign ("Title", sf_get_string (file, SF_STR_TITLE));
  result.assign ("Artist", sf_get_string (file, SF_STR_ARTIST));
  result.assign ("Comment", sf_get_string (file, SF_STR_COMMENT));

  retval = result;

#else

  error ("sndfile not found on your system and thus audioinfo is not functional");

#endif

  return retval;
}
