// DO NOT EDIT!  Generated automatically from oct-errno.in.cc by configure
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>

#include "singleton-cleanup.h"

#include "oct-errno.h"
#include "oct-map.h"
#include "error.h"

octave_errno *octave_errno::instance = 0;

octave_errno::octave_errno (void)
{
  struct errno_struct
  {
    const char *name;
    int value;
  };

  static errno_struct errno_codes[] =
  {
    // POSIX.

#if defined (E2BIG)
    { "E2BIG", E2BIG, },
#endif
#if defined (EACCES)
    { "EACCES", EACCES, },
#endif
#if defined (EADDRINUSE)
    { "EADDRINUSE", EADDRINUSE, },
#endif
#if defined (EADDRNOTAVAIL)
    { "EADDRNOTAVAIL", EADDRNOTAVAIL, },
#endif
#if defined (EAFNOSUPPORT)
    { "EAFNOSUPPORT", EAFNOSUPPORT, },
#endif
#if defined (EAGAIN)
    { "EAGAIN", EAGAIN, },
#endif
#if defined (EALREADY)
    { "EALREADY", EALREADY, },
#endif
#if defined (EBADF)
    { "EBADF", EBADF, },
#endif
#if defined (EBUSY)
    { "EBUSY", EBUSY, },
#endif
#if defined (ECHILD)
    { "ECHILD", ECHILD, },
#endif
#if defined (ECONNABORTED)
    { "ECONNABORTED", ECONNABORTED, },
#endif
#if defined (ECONNREFUSED)
    { "ECONNREFUSED", ECONNREFUSED, },
#endif
#if defined (ECONNRESET)
    { "ECONNRESET", ECONNRESET, },
#endif
#if defined (EDEADLK)
    { "EDEADLK", EDEADLK, },
#endif
#if defined (EDESTADDRREQ)
    { "EDESTADDRREQ", EDESTADDRREQ, },
#endif
#if defined (EDOM)
    { "EDOM", EDOM, },
#endif
#if defined (EDQUOT)
    { "EDQUOT", EDQUOT, },
#endif
#if defined (EEXIST)
    { "EEXIST", EEXIST, },
#endif
#if defined (EFAULT)
    { "EFAULT", EFAULT, },
#endif
#if defined (EFBIG)
    { "EFBIG", EFBIG, },
#endif
#if defined (EHOSTDOWN)
    { "EHOSTDOWN", EHOSTDOWN, },
#endif
#if defined (EHOSTUNREACH)
    { "EHOSTUNREACH", EHOSTUNREACH, },
#endif
#if defined (EINPROGRESS)
    { "EINPROGRESS", EINPROGRESS, },
#endif
#if defined (EINTR)
    { "EINTR", EINTR, },
#endif
#if defined (EINVAL)
    { "EINVAL", EINVAL, },
#endif
#if defined (EIO)
    { "EIO", EIO, },
#endif
#if defined (EISCONN)
    { "EISCONN", EISCONN, },
#endif
#if defined (EISDIR)
    { "EISDIR", EISDIR, },
#endif
#if defined (ELOOP)
    { "ELOOP", ELOOP, },
#endif
#if defined (EMFILE)
    { "EMFILE", EMFILE, },
#endif
#if defined (EMLINK)
    { "EMLINK", EMLINK, },
#endif
#if defined (EMSGSIZE)
    { "EMSGSIZE", EMSGSIZE, },
#endif
#if defined (ENAMETOOLONG)
    { "ENAMETOOLONG", ENAMETOOLONG, },
#endif
#if defined (ENETDOWN)
    { "ENETDOWN", ENETDOWN, },
#endif
#if defined (ENETRESET)
    { "ENETRESET", ENETRESET, },
#endif
#if defined (ENETUNREACH)
    { "ENETUNREACH", ENETUNREACH, },
#endif
#if defined (ENFILE)
    { "ENFILE", ENFILE, },
#endif
#if defined (ENOBUFS)
    { "ENOBUFS", ENOBUFS, },
#endif
#if defined (ENODEV)
    { "ENODEV", ENODEV, },
#endif
#if defined (ENOENT)
    { "ENOENT", ENOENT, },
#endif
#if defined (ENOEXEC)
    { "ENOEXEC", ENOEXEC, },
#endif
#if defined (ENOLCK)
    { "ENOLCK", ENOLCK, },
#endif
#if defined (ENOMEM)
    { "ENOMEM", ENOMEM, },
#endif
#if defined (ENOPROTOOPT)
    { "ENOPROTOOPT", ENOPROTOOPT, },
#endif
#if defined (ENOSPC)
    { "ENOSPC", ENOSPC, },
#endif
#if defined (ENOSYS)
    { "ENOSYS", ENOSYS, },
#endif
#if defined (ENOTBLK)
    { "ENOTBLK", ENOTBLK, },
#endif
#if defined (ENOTCONN)
    { "ENOTCONN", ENOTCONN, },
#endif
#if defined (ENOTDIR)
    { "ENOTDIR", ENOTDIR, },
#endif
#if defined (ENOTEMPTY)
    { "ENOTEMPTY", ENOTEMPTY, },
#endif
#if defined (ENOTSOCK)
    { "ENOTSOCK", ENOTSOCK, },
#endif
#if defined (ENOTTY)
    { "ENOTTY", ENOTTY, },
#endif
#if defined (ENXIO)
    { "ENXIO", ENXIO, },
#endif
#if defined (EOPNOTSUPP)
    { "EOPNOTSUPP", EOPNOTSUPP, },
#endif
#if defined (EPERM)
    { "EPERM", EPERM, },
#endif
#if defined (EPFNOSUPPORT)
    { "EPFNOSUPPORT", EPFNOSUPPORT, },
#endif
#if defined (EPIPE)
    { "EPIPE", EPIPE, },
#endif
#if defined (EPROTONOSUPPORT)
    { "EPROTONOSUPPORT", EPROTONOSUPPORT, },
#endif
#if defined (EPROTOTYPE)
    { "EPROTOTYPE", EPROTOTYPE, },
#endif
#if defined (ERANGE)
    { "ERANGE", ERANGE, },
#endif
#if defined (EREMOTE)
    { "EREMOTE", EREMOTE, },
#endif
#if defined (ERESTART)
    { "ERESTART", ERESTART, },
#endif
#if defined (EROFS)
    { "EROFS", EROFS, },
#endif
#if defined (ESHUTDOWN)
    { "ESHUTDOWN", ESHUTDOWN, },
#endif
#if defined (ESOCKTNOSUPPORT)
    { "ESOCKTNOSUPPORT", ESOCKTNOSUPPORT, },
#endif
#if defined (ESPIPE)
    { "ESPIPE", ESPIPE, },
#endif
#if defined (ESRCH)
    { "ESRCH", ESRCH, },
#endif
#if defined (ESTALE)
    { "ESTALE", ESTALE, },
#endif
#if defined (ETIMEDOUT)
    { "ETIMEDOUT", ETIMEDOUT, },
#endif
#if defined (ETOOMANYREFS)
    { "ETOOMANYREFS", ETOOMANYREFS, },
#endif
#if defined (ETXTBSY)
    { "ETXTBSY", ETXTBSY, },
#endif
#if defined (EUSERS)
    { "EUSERS", EUSERS, },
#endif
#if defined (EWOULDBLOCK)
    { "EWOULDBLOCK", EWOULDBLOCK, },
#endif
#if defined (EXDEV)
    { "EXDEV", EXDEV, },
#endif

    // Others (duplicates are OK).

@SYSDEP_ERRNO_LIST@

    { 0, 0, },
  };

  // Stuff them all in a map for fast access.

  errno_struct *ptr = errno_codes;

  while (ptr->name)
    {
      errno_tbl[ptr->name] = ptr->value;
      ptr++;
    }
}

bool
octave_errno::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_errno ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create errno object!");

      retval = false;
    }

  return retval;
}

int
octave_errno::lookup (const std::string& name)
{
  return (instance_ok ()) ? instance->do_lookup (name) : -1;
}

octave_scalar_map
octave_errno::list (void)
{
  return (instance_ok ()) ? instance->do_list () : octave_scalar_map ();
}

int
octave_errno::do_lookup (const std::string& name)
{
  return (errno_tbl.find (name) != errno_tbl.end ()) ? errno_tbl[name] : -1;
}

octave_scalar_map
octave_errno::do_list (void)
{
  octave_scalar_map retval;

  for (std::map<std::string, int>::const_iterator p = errno_tbl.begin ();
       p != errno_tbl.end ();
       p++)
    {
      retval.assign (p->first, p->second);
    }

  return retval;
}
