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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <sys/types.h>

#include "oct-passwd.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

// Password file functions.  (Why not?)

static octave_value
mk_pw_map (const octave_passwd& pw)
{
  octave_value retval;

  if (pw)
    {
      octave_scalar_map m;

      m.assign ("name", pw.name ());
      m.assign ("passwd", pw.passwd ());
      m.assign ("uid", static_cast<double> (pw.uid ()));
      m.assign ("gid", static_cast<double> (pw.gid ()));
      m.assign ("gecos", pw.gecos ());
      m.assign ("dir", pw.dir ());
      m.assign ("shell", pw.shell ());

      retval = m;
    }
  else
    retval = 0;

  return retval;
}

DEFUN (getpwent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{pw_struct} =} getpwent ()\n\
Return a structure containing an entry from the password database,\n\
opening it if necessary.\n\
\n\
Once the end of the data has been reached, @code{getpwent} returns 0.\n\
@seealso{setpwent, endpwent}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = 0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      retval(1) = msg;
      retval(0) = mk_pw_map (octave_passwd::getpwent (msg));
    }
  else
    print_usage ();

  return retval;
}

DEFUN (getpwuid, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{pw_struct} =} getpwuid (@var{uid}).\n\
Return a structure containing the first entry from the password database\n\
with the user ID @var{uid}.\n\
\n\
If the user ID does not exist in the database, @code{getpwuid} returns 0.\n\
@seealso{getpwnam}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = 0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
        {
          if (D_NINT (dval) == dval)
            {
              uid_t uid = static_cast<uid_t> (dval);

              std::string msg;

              retval(1) = msg;
              retval(0) = mk_pw_map (octave_passwd::getpwuid (uid, msg));
            }
          else
            error ("getpwuid: UID must be an integer");
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (getpwnam, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{pw_struct} =} getpwnam (@var{name})\n\
Return a structure containing the first entry from the password database\n\
with the user name @var{name}.\n\
\n\
If the user name does not exist in the database, @code{getpwname} returns 0.\n\
@seealso{getpwuid}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = 0.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string s = args(0).string_value ();

      if (! error_state)
        {
          std::string msg;

          retval(1) = msg;
          retval(0) = mk_pw_map (octave_passwd::getpwnam (s, msg));
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (setpwent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} setpwent ()\n\
Return the internal pointer to the beginning of the password database.\n\
@seealso{getpwent, endpwent}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      retval(1) = msg;
      retval(0) = static_cast<double> (octave_passwd::setpwent (msg));
    }
  else
    print_usage ();

  return retval;
}

DEFUN (endpwent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} endpwent ()\n\
Close the password database.\n\
@seealso{getpwent, setpwent}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      retval(1) = msg;
      retval(0) = static_cast<double> (octave_passwd::endpwent (msg));
    }
  else
    print_usage ();

  return retval;
}
