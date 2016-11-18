/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_pager_h)
#define octave_pager_h 1

#include <iosfwd>
#include <sstream>
#include <string>

#include <sys/types.h>

class
OCTINTERP_API
octave_pager_buf : public std::stringbuf
{
public:

  octave_pager_buf (void) : std::stringbuf (), diary_skip (0) { }

  void flush_current_contents_to_diary (void);

  void set_diary_skip (void);

protected:

  int sync (void);

private:

  size_t diary_skip;
};

class
OCTINTERP_API
octave_pager_stream : public std::ostream
{
protected:

  octave_pager_stream (void);

public:

  ~octave_pager_stream (void);

  static void flush_current_contents_to_diary (void);

  static void set_diary_skip (void);

  static std::ostream& stream (void);

  static void reset (void);

private:

  void do_flush_current_contents_to_diary (void);

  void do_set_diary_skip (void);

  void do_reset (void);

  static octave_pager_stream *instance;

  static bool instance_ok (void);

  static void cleanup_instance (void) { delete instance; instance = 0; }

  octave_pager_buf *pb;

  // No copying!

  octave_pager_stream (const octave_pager_stream&);

  octave_pager_stream& operator = (const octave_pager_stream&);
};

class
OCTINTERP_API
octave_diary_buf : public std::stringbuf
{
public:

  octave_diary_buf (void) : std::stringbuf () { }

protected:

  int sync (void);
};

class
OCTINTERP_API
octave_diary_stream : public std::ostream
{
protected:

  octave_diary_stream (void);

public:

  ~octave_diary_stream (void);

  static std::ostream& stream (void);

  static void reset (void);

private:

  void do_reset (void);

  static octave_diary_stream *instance;

  static bool instance_ok (void);

  static void cleanup_instance (void) { delete instance; instance = 0; }

  octave_diary_buf *db;

  // No copying!

  octave_diary_stream (const octave_diary_stream&);

  octave_diary_stream& operator = (const octave_diary_stream&);
};

#define octave_stdout (octave_pager_stream::stream ())

#define octave_diary (octave_diary_stream::stream ())

extern OCTINTERP_API void flush_octave_stdout (void);

#endif
