/*

Copyright (C) 2005-2015 John W. Eaton

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

#if !defined (octave_oct_uname_h)
#define octave_uname_h 1

#include <string>

class
OCTAVE_API
octave_uname
{
public:

  octave_uname (void)
    : utsname_sysname ("unknown"), utsname_nodename ("unknown"),
      utsname_release ("unknown"), utsname_version ("unknown"),
      utsname_machine ("unknown"),
      msg ("uname not supported on this system"), err (-1)
  { init (); }

  octave_uname (const octave_uname& unm)
    : utsname_sysname (unm.utsname_sysname),
      utsname_nodename (unm.utsname_nodename),
      utsname_release (unm.utsname_release),
      utsname_version (unm.utsname_version),
      utsname_machine (unm.utsname_machine),
      msg (unm.msg), err (unm.err)
  { }

  octave_uname& operator = (const octave_uname& unm)
  {
    if (this != &unm)
      {
        utsname_sysname = unm.utsname_sysname;
        utsname_nodename = unm.utsname_nodename;
        utsname_release = unm.utsname_release;
        utsname_version = unm.utsname_version;
        utsname_machine = unm.utsname_machine;

        msg = unm.msg;
        err = unm.err;
      }

    return *this;
  }

  ~octave_uname (void) { }

  std::string sysname (void) const { return utsname_sysname; }
  std::string nodename (void) const { return utsname_nodename; }
  std::string release (void) const { return utsname_release; }
  std::string version (void) const { return utsname_version; }
  std::string machine (void) const { return utsname_machine; }

  std::string message (void) const { return msg; }
  int error (void) const { return err; }

private:

  std::string utsname_sysname;
  std::string utsname_nodename;
  std::string utsname_release;
  std::string utsname_version;
  std::string utsname_machine;

  std::string msg;
  int err;

  void init (void);
};

#endif
