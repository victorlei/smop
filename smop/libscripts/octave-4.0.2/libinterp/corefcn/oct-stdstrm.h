/*

Copyright (C) 1996-2015 John W. Eaton

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

#if !defined (octave_oct_stdstrm_h)
#define octave_oct_stdstrm_h 1

#include "oct-stream.h"
#include "c-file-ptr-stream.h"

template <typename BUF_T, typename STREAM_T, typename FILE_T>
class
octave_tstdiostream : public octave_base_stream
{
public:

  octave_tstdiostream (const std::string& n, FILE_T f = 0, int fid = 0,
                       std::ios::openmode m = std::ios::in|std::ios::out,
                       oct_mach_info::float_format ff
                         = oct_mach_info::native_float_format (),
                       typename BUF_T::close_fcn cf = BUF_T::file_close)
    : octave_base_stream (m, ff), nm (n), md (m),
      s (f ? new STREAM_T (f, cf) : 0), fnum (fid)
  { }

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t offset, int origin)
  { return s ? s->seek (offset, origin) : -1; }

  // Return current stream position.

  off_t tell (void) { return s ? s->tell () : -1; }

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const { return s ? s->eof () : true; }

  // The name of the file.

  std::string name (void) const { return nm; }

  std::istream *input_stream (void) { return (md & std::ios::in) ? s : 0; }

  std::ostream *output_stream (void) { return (md & std::ios::out) ? s : 0; }

  // FIXME: should not have to cast away const here.
  BUF_T *rdbuf (void) const
  { return s ? (const_cast<STREAM_T *> (s))->rdbuf () : 0; }

  int file_number (void) const { return fnum; }

  bool bad (void) const { return s ? s->bad () : true; }

  void clear (void) { if (s) s->clear (); }

  void do_close (void) { if (s) s->stream_close (); }

protected:

  std::string nm;

  std::ios::openmode md;

  STREAM_T *s;

  // The file number associated with this file.
  int fnum;

  ~octave_tstdiostream (void) { delete s; }

private:

  // No copying!

  octave_tstdiostream (const octave_tstdiostream&);

  octave_tstdiostream& operator = (const octave_tstdiostream&);
};

class
octave_stdiostream
  : public octave_tstdiostream<c_file_ptr_buf, io_c_file_ptr_stream, FILE *>
{
public:

  octave_stdiostream (const std::string& n, FILE *f = 0,
                      std::ios::openmode m = std::ios::in|std::ios::out,
                      oct_mach_info::float_format ff
                        = oct_mach_info::native_float_format (),
                      c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::file_close)
    : octave_tstdiostream<c_file_ptr_buf, io_c_file_ptr_stream, FILE *>
       (n, f, f ? fileno (f) : -1, m, ff, cf) { }

  static octave_stream
  create (const std::string& n, FILE *f = 0,
          std::ios::openmode m = std::ios::in|std::ios::out,
          oct_mach_info::float_format ff
            = oct_mach_info::native_float_format (),
          c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::file_close)
  {
    return octave_stream (new octave_stdiostream (n, f, m, ff, cf));
  }

protected:

  ~octave_stdiostream (void) { }

private:

  // No copying!

  octave_stdiostream (const octave_stdiostream&);

  octave_stdiostream& operator = (const octave_stdiostream&);
};

#ifdef HAVE_ZLIB

class
octave_zstdiostream
  : public octave_tstdiostream<c_zfile_ptr_buf, io_c_zfile_ptr_stream, gzFile>
{
public:

  octave_zstdiostream (const std::string& n, gzFile f = 0, int fid = 0,
                       std::ios::openmode m = std::ios::in|std::ios::out,
                       oct_mach_info::float_format ff
                         = oct_mach_info::native_float_format (),
                       c_zfile_ptr_buf::close_fcn cf
                         = c_zfile_ptr_buf::file_close)
    : octave_tstdiostream<c_zfile_ptr_buf, io_c_zfile_ptr_stream, gzFile>
       (n, f, fid, m, ff, cf) { }

  static octave_stream
  create (const std::string& n, gzFile f = 0, int fid = 0,
          std::ios::openmode m = std::ios::in|std::ios::out,
          oct_mach_info::float_format ff
            = oct_mach_info::native_float_format (),
          c_zfile_ptr_buf::close_fcn cf = c_zfile_ptr_buf::file_close)
  {
    return octave_stream (new octave_zstdiostream (n, f, fid, m, ff, cf));
  }

protected:

  ~octave_zstdiostream (void) { }

private:

  // No copying!

  octave_zstdiostream (const octave_zstdiostream&);

  octave_zstdiostream& operator = (const octave_zstdiostream&);
};

#endif

#endif
