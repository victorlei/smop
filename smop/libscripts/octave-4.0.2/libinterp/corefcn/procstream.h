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

#if !defined (octave_procstream_h)
#define octave_procstream_h 1

#include <istream>
#include <ostream>
#include <string>

#include <sys/types.h>

#include "oct-procbuf.h"

class
OCTINTERP_API
procstreambase : virtual public std::ios
{
public:

  procstreambase (void) : pb () { pb_init (); }

  procstreambase (const std::string& name, int mode);

  procstreambase (const char *name, int mode);

  ~procstreambase (void) { close (); }

  void open (const std::string& name, int mode)
  { open (name.c_str (), mode); }

  void open (const char *name, int mode);

  int is_open (void) const { return pb.is_open (); }

  int close (void);

  pid_t pid (void) const { return pb.pid (); }

  int file_number (void) const { return pb.file_number (); }

private:

  octave_procbuf pb;

  void pb_init (void) { init (&pb); }

  procstreambase (const procstreambase&);

  procstreambase& operator = (const procstreambase&);
};

class
OCTINTERP_API
iprocstream : public std::istream, public procstreambase
// iprocstream : public procstreambase, public std::istream
{
public:

  iprocstream (void) : std::istream (0), procstreambase () { }

  iprocstream (const std::string& name, int mode = std::ios::in)
    : std::istream (0), procstreambase (name, mode) { }

  iprocstream (const char *name, int mode = std::ios::in)
    : std::istream (0), procstreambase (name, mode) { }

  ~iprocstream (void) { }

  void open (const std::string& name, int mode = std::ios::in)
  { procstreambase::open (name, mode); }

  void open (const char *name, int mode = std::ios::in)
  { procstreambase::open (name, mode); }

private:

  iprocstream (const iprocstream&);

  iprocstream& operator = (const iprocstream&);
};

class
OCTINTERP_API
oprocstream : public std::ostream, public procstreambase
// oprocstream : public procstreambase, public std::ostream
{
public:

  oprocstream (void) : std::ostream (0), procstreambase () { }

  oprocstream (const std::string& name, int mode = std::ios::out)
    : std::ostream (0), procstreambase (name, mode) { }

  oprocstream (const char *name, int mode = std::ios::out)
    : std::ostream (0), procstreambase (name, mode) { }

  ~oprocstream (void) { }

  void open (const std::string& name, int mode = std::ios::out)
  { procstreambase::open (name, mode); }

  void open (const char *name, int mode = std::ios::out)
  { procstreambase::open (name, mode); }

private:

  oprocstream (const oprocstream&);

  oprocstream& operator = (const oprocstream&);
};

class
OCTINTERP_API
procstream : public std::iostream, public procstreambase
// procstream : public procstreambase, public std::iostream
{
public:

  procstream (void) : std::iostream (0), procstreambase () { }

  procstream (const std::string& name, int mode)
    : std::iostream (0), procstreambase (name, mode) { }

  procstream (const char *name, int mode)
    : std::iostream (0), procstreambase (name, mode) { }

  ~procstream (void) { }

  void open (const std::string& name, int mode)
  { procstreambase::open (name, mode); }

  void open (const char *name, int mode)
  { procstreambase::open (name, mode); }

private:

  procstream (const procstream&);

  procstream& operator = (const procstream&);
};

#endif
