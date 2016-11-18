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

#if !defined (octave_oct_passwd_h)
#define octave_oct_passwd_h 1

#include <string>

#include <sys/types.h>

class
OCTAVE_API
octave_passwd
{
public:

  octave_passwd (void)
    : pw_name (), pw_passwd (), pw_uid (0), pw_gid (0), pw_gecos (),
      pw_dir (), pw_shell (), valid (false)
  { }

  octave_passwd (const octave_passwd& pw)
    : pw_name (pw.pw_name), pw_passwd (pw.pw_passwd),
      pw_uid (pw.pw_uid), pw_gid (pw.pw_gid), pw_gecos (pw.pw_gecos),
      pw_dir (pw.pw_dir), pw_shell (pw.pw_shell), valid (pw.valid)
  { }

  octave_passwd& operator = (const octave_passwd& pw)
  {
    if (this != &pw)
      {
        pw_name = pw.pw_name;
        pw_passwd = pw.pw_passwd;
        pw_uid = pw.pw_uid;
        pw_gid = pw.pw_gid;
        pw_gecos = pw.pw_gecos;
        pw_dir = pw.pw_dir;
        pw_shell = pw.pw_shell;
        valid = pw.valid;
      }

    return *this;
  }

  ~octave_passwd (void) { }

  std::string name (void) const;

  std::string passwd (void) const;

  uid_t uid (void) const;

  gid_t gid (void) const;

  std::string gecos (void) const;

  std::string dir (void) const;

  std::string shell (void) const;

  bool ok (void) const { return valid; }

  operator bool () const { return ok (); }

  static octave_passwd getpwent (void);
  static octave_passwd getpwent (std::string& msg);

  static octave_passwd getpwuid (uid_t uid);
  static octave_passwd getpwuid (uid_t uid, std::string& msg);

  static octave_passwd getpwnam (const std::string& nm);
  static octave_passwd getpwnam (const std::string& nm, std::string& msg);

  static int setpwent (void);
  static int setpwent (std::string& msg);

  static int endpwent (void);
  static int endpwent (std::string& msg);

private:

  // User name.
  std::string pw_name;

  // Encrypted password.
  std::string pw_passwd;

  // Numeric user id.
  uid_t pw_uid;

  // Numeric group id.
  gid_t pw_gid;

  // Miscellaneous junk.
  std::string pw_gecos;

  // Home directory.
  std::string pw_dir;

  // Login shell.
  std::string pw_shell;

  // Flag that says whether we have been properly initialized.
  bool valid;

  // This is how we will create an octave_passwd object from a pointer
  // to a struct passwd.
  octave_passwd (void *p, std::string& msg);

  void gripe_invalid (void) const;
};

#endif
