/*

Copyright (C) 2000-2015 John W. Eaton

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

#include <iostream>

#include "c-file-ptr-stream.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

c_file_ptr_buf::~c_file_ptr_buf (void)
{
  buf_close ();
}

// FIXME: I'm sure there is room for improvement here...

c_file_ptr_buf::int_type
c_file_ptr_buf::overflow (int_type c)
{
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
  if (f)
    return (c != traits_type::eof ()) ? gnulib::fputc (c, f) : flush ();
  else
    return traits_type::not_eof (c);
#else
  if (f)
    return (c != EOF) ? gnulib::fputc (c, f) : flush ();
  else
    return EOF;
#endif
}

c_file_ptr_buf::int_type
c_file_ptr_buf::underflow_common (bool bump)
{
  if (f)
    {
      int_type c = gnulib::fgetc (f);

      if (! bump
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
          && c != traits_type::eof ())
#else
          && c != EOF)
#endif
        ungetc (c, f);

      return c;
    }
  else
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
    return traits_type::eof ();
#else
    return EOF;
#endif
}

c_file_ptr_buf::int_type
c_file_ptr_buf::pbackfail (int_type c)
{
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
  return (c != traits_type::eof () && f) ? ungetc (c, f)
                                         : traits_type::not_eof (c);
#else
  return (c != EOF && f) ? ungetc (c, f) : EOF;
#endif
}

std::streamsize
c_file_ptr_buf::xsputn (const char* s, std::streamsize n)
{
  if (f)
    return gnulib::fwrite (s, 1, n, f);
  else
    return 0;
}

std::streamsize
c_file_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (f)
    return gnulib::fread (s, 1, n, f);
  else
    return 0;
}

static inline int
seekdir_to_whence (std::ios::seekdir dir)
{
  return ((dir == std::ios::beg) ? SEEK_SET :
          (dir == std::ios::cur) ? SEEK_CUR :
          (dir == std::ios::end) ? SEEK_END :
          dir);
}

std::streampos
c_file_ptr_buf::seekoff (std::streamoff /* offset */,
                         std::ios::seekdir /* dir */,
                         std::ios::openmode)
{
  // FIXME
#if 0
  if (f)
    {
      fseek (f, offset, seekdir_to_whence (dir));

      return ftell (f);
    }
  else
    return 0;
#endif
  return -1;
}

std::streampos
c_file_ptr_buf::seekpos (std::streampos /* offset */, std::ios::openmode)
{
  // FIXME
#if 0
  if (f)
    {
      fseek (f, offset, SEEK_SET);

      return ftell (f);
    }
  else
    return 0;
#endif
  return -1;
}

int
c_file_ptr_buf::sync (void)
{
  flush ();

  return 0;
}

int
c_file_ptr_buf::flush (void)
{
  return f ? gnulib::fflush (f) : EOF;
}

int
c_file_ptr_buf::buf_close (void)
{
  int retval = -1;

  flush ();

  if (f)
    {
      retval = cf (f);
      f = 0;
    }

  return retval;
}

int
c_file_ptr_buf::seek (off_t offset, int origin)
{
  return f ? gnulib::fseeko (f, offset, origin) : -1;
}

off_t
c_file_ptr_buf::tell (void)
{
  return f ? gnulib::ftello (f) : -1;
}

int
c_file_ptr_buf::file_close (FILE *f)
{
  return gnulib::fclose (f);
}

#ifdef HAVE_ZLIB

c_zfile_ptr_buf::~c_zfile_ptr_buf (void)
{
  buf_close ();
}

// FIXME: I'm sure there is room for improvement here...

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::overflow (int_type c)
{
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
  if (f)
    return (c != traits_type::eof ()) ? gzputc (f, c) : flush ();
  else
    return traits_type::not_eof (c);
#else
  if (f)
    return (c != EOF) ? gzputc (f, c) : flush ();
  else
    return EOF;
#endif
}

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::underflow_common (bool bump)
{
  if (f)
    {
      int_type c = gzgetc (f);

      if (! bump
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
          && c != traits_type::eof ())
#else
          && c != EOF)
#endif
        gzungetc (c, f);

      return c;
    }
  else
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
    return traits_type::eof ();
#else
    return EOF;
#endif
}

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::pbackfail (int_type c)
{
#if defined (CXX_ISO_COMPLIANT_LIBRARY)
  return (c != traits_type::eof () && f) ? gzungetc (c, f)
                                         : traits_type::not_eof (c);
#else
  return (c != EOF && f) ? gzungetc (c, f) : EOF;
#endif
}

std::streamsize
c_zfile_ptr_buf::xsputn (const char* s, std::streamsize n)
{
  if (f)
    return gzwrite (f, s, n);
  else
    return 0;
}

std::streamsize
c_zfile_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (f)
    return gzread (f, s, n);
  else
    return 0;
}

std::streampos
c_zfile_ptr_buf::seekoff (std::streamoff /* offset */,
                          std::ios::seekdir /* dir */,
                          std::ios::openmode)
{
  // FIXME
#if 0
  if (f)
    {
      gzseek (f, offset, seekdir_to_whence (dir));

      return gztell (f);
    }
  else
    return 0;
#endif
  return -1;
}

std::streampos
c_zfile_ptr_buf::seekpos (std::streampos /* offset */, std::ios::openmode)
{
  // FIXME
#if 0
  if (f)
    {
      gzseek (f, offset, SEEK_SET);

      return gztell (f);
    }
  else
    return 0;
#endif
  return -1;
}

int
c_zfile_ptr_buf::sync (void)
{
  flush ();

  return 0;
}

int
c_zfile_ptr_buf::flush (void)
{
  // FIXME: do we need something more complex here, passing
  // something other than 0 for the second argument to gzflush and
  // checking the return value, etc.?

  return f ? gzflush (f, 0) : EOF;
}

int
c_zfile_ptr_buf::buf_close (void)
{
  int retval = -1;

  flush ();

  if (f)
    {
      retval = cf (f);
      f = 0;
    }

  return retval;
}

#endif
