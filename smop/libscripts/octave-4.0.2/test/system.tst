## Copyright (C) 2006-2015 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

%!test
%! [t1, u1, s1] = cputime ();
%! for i = 1:200
%!   sin (i);
%! endfor
%! [t2, u2, s2] = cputime ();
%! assert (t1, u1 + s1);
%! assert (t2 == u2 + s2);
%! assert (t2 >= t1);
%! assert (u2 >= u2);
%! assert (s2 >= s2);
%!#assert (t1 == u1 + s1 && t2 == u2 + s2 && t2 >= t1 && u2 >= u2 && s2 >= s2);

%!test
%! tic ();
%! sleep (2);
%! assert (toc () > 0);

%!test
%! pause (0);
%! __printf_assert__ ("ok\n");
%! assert (__prog_output_assert__ ("ok"));

%!error <Invalid call to pause> pause (1, 2)

%!test
%! sleep (0);
%! __printf_assert__ ("ok\n");
%! assert (__prog_output_assert__ ("ok"));

%!error <Invalid call to sleep> sleep ()
%!error <Invalid call to sleep> sleep (1, 2)

%!test
%! usleep (0);
%! __printf_assert__ ("ok\n");
%! assert (__prog_output_assert__ ("ok"));

%!error <Invalid call to usleep> usleep ()
%!error <Invalid call to usleep> usleep (1, 2)

%!test
%! from = tempname ();
%! to = tempname ();
%! id = fopen (from, "wb");
%! if (id > 0 && fclose (id) == 0)
%!   [s, e] = stat (from);
%!   if (! e)
%!     if (rename (from, to) == 0)
%!       [s, e] = stat (from);
%!       if (e < 0)
%!         [s, e] = stat (to);
%!         assert (e == 0);
%!         unlink (to);
%!       endif
%!     endif
%!   endif
%! endif

%!error <Invalid call to rename> rename ()
%!error <Invalid call to rename> rename ("foo", "bar", 1)

%!test
%! nm = tempname ();
%! if ((id = fopen (nm, "wb")) > 0)
%!   [s, err] = stat (nm);
%!   if (! err && fclose (id) == 0 && unlink (nm) == 0)
%!     [s, err] = stat (nm);
%!     assert (err < 0);
%!   endif
%! endif

%!error <Invalid call to unlink> unlink ()
%!error <Invalid call to unlink> unlink ("foo", 1)
%!error <FILE must be a string> unlink ({})

%!test
%! [files, status, msg] = readdir (filesep);
%! assert (iscell (files) && status == 0 && strcmp (msg, ""));

%!error <Invalid call to readdir> readdir ()
%!error <Invalid call to readdir> readdir ("foo", 1)

%!test
%! nm = tempname ();
%! e1 = mkdir (nm);
%! [s2, e2] = stat (nm);
%! e3 = rmdir (nm);
%! [s4, e4] = stat (nm);
%! assert ((e1 && strcmp (s2.modestr(1), "d") && e3 && e4 < 0));

%!error <Invalid call to mkdir> mkdir ()
%!error <Invalid call to mkdir> mkdir ("foo", 1, 2)
%!error <Invalid call to rmdir> rmdir ()

%!test
%! crr = confirm_recursive_rmdir ();
%! confirm_recursive_rmdir (0);
%! assert (!rmdir ("foo", "s"));
%! confirm_recursive_rmdir (crr);

%!test
%! ## Test makes no sense on Windows systems
%! if (isunix () || ismac ())
%!   orig_umask = umask (0);
%!   nm = tempname ();
%!   id = fopen (nm, "wb");
%!   s1 = stat (nm);
%!   fclose (id);
%!   unlink (nm);
%!
%!   umask (777);
%!   nm = tempname ();
%!   id = fopen (nm, "wb");
%!   s2 = stat (nm);
%!   fclose (id);
%!   unlink (nm);
%!
%!   assert (deblank (s1.modestr), "-rw-rw-rw-");
%!   assert (deblank (s2.modestr), "----------");
%!   ## Restore original umask value
%!   umask (orig_umask);
%! endif

%!error <Invalid call to umask> umask ()
%!error <Invalid call to umask> umask (1, 2)

%!test
%! [s, err, msg] = stat (filesep);
%! assert ((err == 0
%! && isstruct (s)
%! && isfield (s, "dev")
%! && isfield (s, "ino")
%! && isfield (s, "modestr")
%! && isfield (s, "nlink")
%! && isfield (s, "uid")
%! && isfield (s, "gid")
%! && isfield (s, "size")
%! && isfield (s, "atime")
%! && isfield (s, "mtime")
%! && isfield (s, "ctime")
%! && ischar (msg)));

%!error <Invalid call to stat> stat ()
%!error <Invalid call to stat> stat ("foo", 1)

%!test
%! [s, err, msg] = lstat (filesep);
%! assert ((err == 0
%! && isstruct (s)
%! && isfield (s, "dev")
%! && isfield (s, "ino")
%! && isfield (s, "modestr")
%! && isfield (s, "nlink")
%! && isfield (s, "uid")
%! && isfield (s, "gid")
%! && isfield (s, "size")
%! && isfield (s, "atime")
%! && isfield (s, "mtime")
%! && isfield (s, "ctime")
%! && ischar (msg)));

%!error <Invalid call to lstat> lstat ()
%!error <Invalid call to lstat> lstat ("foo", 1)

%!test
%! if (isunix ())
%!   assert (S_ISCHR (stat ("/dev/null").mode));
%!   if (exist ("/dev/initctl"))
%!     assert (S_ISFIFO (stat ("/dev/initctl").mode));
%!   endif
%!   assert (S_ISLNK (lstat ("/dev/core").mode));
%! endif
%! nm = tempname ();
%! fid = fopen (nm, "wb");
%! fclose (fid);
%! r = [ S_ISREG(stat(nm).mode)
%!       S_ISDIR(stat(nm).mode)
%!       S_ISCHR(stat(nm).mode)
%!       S_ISBLK(stat(nm).mode)
%!       S_ISFIFO(stat(nm).mode)
%!       S_ISLNK(lstat(nm).mode)
%!       S_ISSOCK(stat(nm).mode) ];
%! unlink (nm);
%! assert (r(:), [true; false; false; false; false; false; false]);

%!error <octave_base_value::double_value> S_ISREG ({})
%!error <octave_base_value::double_value> S_ISDIR ({})
%!error <octave_base_value::double_value> S_ISCHR ({})
%!error <octave_base_value::double_value> S_ISBLK ({})
%!error <octave_base_value::double_value> S_ISFIFO ({})
%!error <octave_base_value::double_value> S_ISLNK ({})
%!error <octave_base_value::double_value> S_ISSOCK ({})

%!error <Invalid call to S_ISREG> S_ISREG ()
%!error <Invalid call to S_ISDIR> S_ISDIR ()
%!error <Invalid call to S_ISCHR> S_ISCHR ()
%!error <Invalid call to S_ISBLK> S_ISBLK ()
%!error <Invalid call to S_ISFIFO> S_ISFIFO ()
%!error <Invalid call to S_ISLNK> S_ISLNK ()
%!error <Invalid call to S_ISSOCK> S_ISSOCK ()

%!assert (iscell (glob ([filesep "*"])))

%!error <Invalid call to glob> glob ()
%!error <Invalid call to glob> glob ("foo", 1)

%!assert (ischar (file_in_path (path (), "date.m")))

%!error <invalid option> file_in_path ("foo", "bar", 1)
%!error <Invalid call to file_in_path> file_in_path ()
%!error <Invalid call to file_in_path> file_in_path ("foo", "bar", "baz", "ooka")

%!testif HAVE_GETPWUID
%! x = getpwuid (getuid ());
%! assert (x.dir, tilde_expand ("~"));
%! assert (x.dir, tilde_expand (sprintf ("~%s", x.name)));
%! assert ("foobar", tilde_expand ("foobar"));

%!error <Invalid call to tilde_expand> tilde_expand ()
%!error <Invalid call to tilde_expand> tilde_expand ("str", 2)

%!testif HAVE_GETPGRP
%! assert (getpgrp () > 0);

%!error <... getpgrp> getpgrp (1)

%!assert (getpid () > 0)

%!error <... getpid> getpid (1)

%!testif HAVE_GETPPID
%! assert (getppid () > 0);

%!error <... getppid> getppid (1)

%!assert (geteuid () >= 0)

%!error <... geteuid> geteuid (1)

%!assert (getuid () >= 0)

%!error <... getuid> getuid (1)

%!assert (getegid () >= 0)

%!error <... getegid> getegid (1)

%!assert (getgid () >= 0)

%!error <... getgid> getgid (1)

%!assert (get_home_directory (), tilde_expand ("~"))

%!error <Invalid call to getenv> getenv ()
%!error <Invalid call to getenv> getenv ("foo", 1)

%!test
%! wns = warning ("query", "Octave:num-to-str");
%! warning ("on", "Octave:num-to-str");
%! fail ("getenv (1)", "warning");
%! warning (wns.state, "Octave:num-to-str");

%!test
%! setenv ("foobar", "baz");
%! assert (getenv ("foobar"), "baz");

%!error <Invalid call to setenv> setenv ()
%!error <Invalid call to setenv> setenv ("foo", "bar", 1)

%!error <VAR must be a string> setenv (1, 2)

%!test
%! xdir = pwd ();
%! cd /
%! d1 = pwd ();
%! cd (xdir);
%! if (ispc () && ! isunix ())
%!   ## should be a drive letter
%!   assert (length (d1), 3);
%!   assert (d1(2), ":");
%!   assert (d1(3), "\\");
%! else
%!   assert ("/", d1);
%! endif
%! assert (pwd (), xdir);

%!error cd (1)

%!assert (ischar (pwd ()))

%!testif HAVE_GETPWENT
%! s = getpwent ();
%! endpwent ();
%! assert ((isstruct (s)
%! && isfield (s, "name")
%! && isfield (s, "passwd")
%! && isfield (s, "uid")
%! && isfield (s, "gid")
%! && isfield (s, "gecos")
%! && isfield (s, "dir")
%! && isfield (s, "shell")));

%!error <Invalid call to getpwent> getpwent (1)

%!testif HAVE_GETPWUID
%! x = getpwent ();
%! y = getpwuid (x.uid);
%! endpwent ();
%! assert (strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid);

%!error <Invalid call to getpwuid> getpwuid ()
%!error <Invalid call to getpwuid> getpwuid (1, 2)

%!testif HAVE_GETPWNAM
%! x = getpwent ();
%! y = getpwnam (x.name);
%! endpwent ();
%! assert (strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid);

%!error <Invalid call to getpwnam> getpwnam ()
%!error <Invalid call to getpwnam> getpwnam ("foo", 1)

%!testif HAVE_SETPWENT
%! x = getpwent ();
%! setpwent ();
%! y = getpwent ();
%! endpwent ();
%! assert (strcmp (x.name, y.name) && x.uid == y.uid && x.gid == y.gid);

%!error <Invalid call to setpwent> setpwent (1)
%!error <Invalid call to endpwent> endpwent (1)

%!testif HAVE_GETGRENT
%! x = getgrent ();
%! endgrent ();
%! assert ((isstruct (x)
%! && isfield (x, "name")
%! && isfield (x, "passwd")
%! && isfield (x, "gid")
%! && isfield (x, "mem")));

%!error <Invalid call to getgrent> getgrent (1)

%!testif HAVE_GETGRGID
%! x = getgrent ();
%! y = getgrgid (x.gid);
%! endgrent ();
%! assert (strcmp (x.name, y.name) && x.gid == y.gid);

%!error <Invalid call to getgrgid> getgrgid ()
%!error <Invalid call to getgrgid> getgrgid (1, 2)

%!testif HAVE_GETGRNAM
%! x = getgrent ();
%! y = getgrnam (x.name);
%! endgrent ();
%! assert (strcmp (x.name, y.name) && x.gid == y.gid);

%!error <Invalid call to getgrnam> getgrnam ()
%!error <Invalid call to getgrnam> getgrnam ("foo", 1)

%!testif HAVE_SETGRENT
%! x = getgrent ();
%! setgrent ();
%! y = getgrent ();
%! endgrent ();
%! assert (strcmp (x.name, y.name) && x.gid == y.gid);

%!error <Invalid call to setgrent> setgrent (1)
%!error <Invalid call to endgrent> endgrent (1)

%!assert (isieee () == 1 || isieee () == 0)

%!assert (isstruct (octave_config_info ()))

%!assert (isstruct (getrusage ()))
