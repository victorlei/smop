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

#include <sys/types.h>

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#include "lo-error.h"
#include "oct-group.h"
#include "str-vec.h"

#define NOT_SUPPORTED(nm) \
  nm ": not supported on this system"

std::string
octave_group::name (void) const
{
  if (! ok ())
    gripe_invalid ();

  return gr_name;
}

std::string
octave_group::passwd (void) const
{
  if (! ok ())
    gripe_invalid ();

  return gr_passwd;
}

gid_t
octave_group::gid (void) const
{
  if (! ok ())
    gripe_invalid ();

  return gr_gid;
}

string_vector
octave_group::mem (void) const
{
  if (! ok ())
    gripe_invalid ();

  return gr_mem;
}

octave_group
octave_group::getgrent (void)
{
  std::string msg;
  return getgrent (msg);
}

octave_group
octave_group::getgrent (std::string& msg)
{
#if defined (HAVE_GETGRENT)
  msg = std::string ();
  return octave_group (::getgrent (), msg);
#else
  msg = NOT_SUPPORTED ("getgrent");
  return octave_group ();
#endif
}

octave_group
octave_group::getgrgid (gid_t gid)
{
  std::string msg;
  return getgrgid (gid, msg);
}

octave_group
octave_group::getgrgid (gid_t gid, std::string& msg)
{
#if defined (HAVE_GETGRGID)
  msg = std::string ();
  return octave_group (::getgrgid (gid), msg);
#else
  msg = NOT_SUPPORTED ("getgruid");
  return octave_group ();
#endif
}

octave_group
octave_group::getgrnam (const std::string& nm)
{
  std::string msg;
  return getgrnam (nm, msg);
}

octave_group
octave_group::getgrnam (const std::string& nm, std::string& msg)
{
#if defined (HAVE_GETGRNAM)
  msg = std::string ();
  return octave_group (::getgrnam (nm.c_str ()), msg);
#else
  msg = NOT_SUPPORTED ("getgrnam");
  return octave_group ();
#endif
}

int
octave_group::setgrent (void)
{
  std::string msg;
  return setgrent (msg);
}

int
octave_group::setgrent (std::string& msg)
{
#if defined (HAVE_SETGRENT)
  msg = std::string ();
  ::setgrent ();
  return 0;
#else
  msg = NOT_SUPPORTED ("setgrent");
  return -1;
#endif
}

int
octave_group::endgrent (void)
{
  std::string msg;
  return endgrent (msg);
}

int
octave_group::endgrent (std::string& msg)
{
#if defined (HAVE_ENDGRENT)
  msg = std::string ();
  ::endgrent ();
  return 0;
#else
  msg = NOT_SUPPORTED ("endgrent");
  return -1;
#endif
}

octave_group::octave_group (void *p, std::string& msg)
  : gr_name (), gr_passwd (), gr_gid (0), gr_mem (), valid (false)
{
#if defined (HAVE_GRP_H)
  msg = std::string ();

  if (p)
    {
      struct group *gr = static_cast<struct group *> (p);

      gr_name = gr->gr_name;

#if defined (HAVE_GR_PASSWD)
      gr_passwd = gr->gr_passwd;
#endif

      gr_gid = gr->gr_gid;

      // FIXME: Maybe there should be a string_vector constructor
      //        that takes a NUL terminated list of C strings?

      const char * const *tmp = gr->gr_mem;

      int k = 0;
      while (*tmp++)
        k++;

      if (k > 0)
        {
          tmp = gr->gr_mem;

          gr_mem.resize (k);

          for (int i = 0; i < k; i++)
            gr_mem[i] = tmp[i];
        }

      valid = true;
    }
#else
  msg = NOT_SUPPORTED ("group functions");
#endif
}

void
octave_group::gripe_invalid (void) const
{
  (*current_liboctave_error_handler) ("invalid group object");
}
