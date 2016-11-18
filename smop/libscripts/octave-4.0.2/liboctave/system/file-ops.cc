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

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <iostream>
#include <vector>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "pathmax.h"
#include "canonicalize.h"

extern "C" {
#include <tempname.h>
}

#include "dir-ops.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-locbuf.h"
#include "oct-passwd.h"
#include "pathlen.h"
#include "quit.h"
#include "singleton-cleanup.h"
#include "str-vec.h"

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
#include <algorithm>
#endif

file_ops *file_ops::instance = 0;

bool
file_ops::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
      char system_dir_sep_char = '\\';
      std::string system_dir_sep_str = "\\";
#else
      char system_dir_sep_char = '/';
      std::string system_dir_sep_str = "/";
#endif
#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM)
      std::string system_dir_sep_chars = "/\\";
#else
      std::string system_dir_sep_chars = system_dir_sep_str;
#endif

      instance = new file_ops (system_dir_sep_char, system_dir_sep_str,
                               system_dir_sep_chars);

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create file_ops object!");

      retval = false;
    }

  return retval;
}

// The following tilde-expansion code was stolen and adapted from
// readline.

// The default value of tilde_additional_prefixes.  This is set to
// whitespace preceding a tilde so that simple programs which do not
// perform any word separation get desired behaviour.
static const char *default_prefixes[] = { " ~", "\t~", ":~", 0 };

// The default value of tilde_additional_suffixes.  This is set to
// whitespace or newline so that simple programs which do not perform
// any word separation get desired behaviour.
static const char *default_suffixes[] = { " ", "\n", ":", 0 };

// If non-null, this contains the address of a function that the
// application wants called before trying the standard tilde
// expansions.  The function is called with the text sans tilde, and
// returns a malloc()'ed string which is the expansion, or a NULL
// pointer if the expansion fails.
file_ops::tilde_expansion_hook file_ops::tilde_expansion_preexpansion_hook = 0;

// If non-null, this contains the address of a function to call if the
// standard meaning for expanding a tilde fails.  The function is
// called with the text (sans tilde, as in "foo"), and returns a
// malloc()'ed string which is the expansion, or a NULL pointer if
// there is no expansion.
file_ops::tilde_expansion_hook file_ops::tilde_expansion_failure_hook = 0;

// When non-null, this is a NULL terminated array of strings which are
// duplicates for a tilde prefix.  Bash uses this to expand '=~' and
// ':~'.
string_vector file_ops::tilde_additional_prefixes = default_prefixes;

// When non-null, this is a NULL terminated array of strings which
// match the end of a username, instead of just "/".  Bash sets this
// to ':' and '=~'.
string_vector file_ops::tilde_additional_suffixes = default_suffixes;

// Find the start of a tilde expansion in S, and return the index
// of the tilde which starts the expansion.  Place the length of the
// text which identified this tilde starter in LEN, excluding the
// tilde itself.

static size_t
tilde_find_prefix (const std::string& s, size_t& len)
{
  len = 0;

  size_t s_len = s.length ();

  if (s_len == 0 || s[0] == '~')
    return 0;

  string_vector prefixes = file_ops::tilde_additional_prefixes;

  if (! prefixes.empty ())
    {
      for (size_t i = 0; i < s_len; i++)
        {
          for (int j = 0; j < prefixes.length (); j++)
            {
              size_t pfx_len = prefixes[j].length ();

              if (prefixes[j].compare (s.substr (i, pfx_len)) == 0)
                {
                  len = pfx_len - 1;
                  return i + len;
                }
            }
        }
    }

  return s_len;
}

// Find the end of a tilde expansion in S, and return the index
// of the character which ends the tilde definition.

static size_t
tilde_find_suffix (const std::string& s)
{
  size_t s_len = s.length ();

  string_vector suffixes = file_ops::tilde_additional_suffixes;

  size_t i = 0;

  for ( ; i < s_len; i++)
    {
      if (file_ops::is_dir_sep (s[i]))
        break;

      if (! suffixes.empty ())
        {
          for (int j = 0; j < suffixes.length (); j++)
            {
              size_t sfx_len = suffixes[j].length ();

              if (suffixes[j].compare (s.substr (i, sfx_len)) == 0)
                return i;
            }
        }
    }

  return i;
}

// Take FNAME and return the tilde prefix we want expanded.

static std::string
isolate_tilde_prefix (const std::string& fname)
{
  size_t f_len = fname.length ();

  size_t len = 1;

  while (len < f_len && ! file_ops::is_dir_sep (fname[len]))
    len++;

  return fname.substr (1, len);
}

// Do the work of tilde expansion on FILENAME.  FILENAME starts with a
// tilde.

static std::string
tilde_expand_word (const std::string& filename)
{
  size_t f_len = filename.length ();

  if (f_len == 0 || filename[0] != '~')
    return filename;

  // A leading '~/' or a bare '~' is *always* translated to the value
  // of $HOME or the home directory of the current user, regardless of
  // any preexpansion hook.

  if (f_len == 1 || file_ops::is_dir_sep (filename[1]))
    return octave_env::get_home_directory () + filename.substr (1);

  std::string username = isolate_tilde_prefix (filename);

  size_t user_len = username.length ();

  std::string dirname;

  if (file_ops::tilde_expansion_preexpansion_hook)
    {
      std::string expansion
        = file_ops::tilde_expansion_preexpansion_hook (username);

      if (! expansion.empty ())
        return expansion + filename.substr (user_len+1);
    }

  // No preexpansion hook, or the preexpansion hook failed.  Look in the
  // password database.

  octave_passwd pw = octave_passwd::getpwnam (username);

  if (! pw)
    {
      // If the calling program has a special syntax for expanding tildes,
      // and we couldn't find a standard expansion, then let them try.

      if (file_ops::tilde_expansion_failure_hook)
        {
          std::string expansion
            = file_ops::tilde_expansion_failure_hook (username);

          if (! expansion.empty ())
            dirname = expansion + filename.substr (user_len+1);
        }

      // If we don't have a failure hook, or if the failure hook did not
      // expand the tilde, return a copy of what we were passed.

      if (dirname.length () == 0)
        dirname = filename;
    }
  else
    dirname = pw.dir () + filename.substr (user_len+1);

  return dirname;
}

// If NAME has a leading ~ or ~user, Unix-style, expand it to the
// user's home directory.  If no ~, or no <pwd.h>, just return NAME.

std::string
file_ops::tilde_expand (const std::string& name)
{
  if (name.find ('~') == std::string::npos)
    return name;
  else
    {
      std::string result;

      size_t name_len = name.length ();

      // Scan through S expanding tildes as we come to them.

      size_t pos = 0;

      while (1)
        {
          if (pos > name_len)
            break;

          size_t len;

          // Make START point to the tilde which starts the expansion.

          size_t start = tilde_find_prefix (name.substr (pos), len);

          result.append (name.substr (pos, start));

          // Advance STRING to the starting tilde.

          pos += start;

          // Make FINI be the index of one after the last character of the
          // username.

          size_t fini = tilde_find_suffix (name.substr (pos));

          // If both START and FINI are zero, we are all done.

          if (! (start || fini))
            break;

          // Expand the entire tilde word, and copy it into RESULT.

          std::string tilde_word = name.substr (pos, fini);

          pos += fini;

          std::string expansion = tilde_expand_word (tilde_word);

          result.append (expansion);
        }

      return result;
    }
}

// A vector version of the above.

string_vector
file_ops::tilde_expand (const string_vector& names)
{
  string_vector retval;

  int n = names.length ();

  retval.resize (n);

  for (int i = 0; i < n; i++)
    retval[i] = tilde_expand (names[i]);

  return retval;
}

std::string
file_ops::concat (const std::string& dir, const std::string& file)
{
  return dir.empty ()
         ? file
         : (is_dir_sep (dir[dir.length ()-1])
            ? dir + file
            : dir + dir_sep_char () + file);
}

std::string
file_ops::native_separator_path (const std::string& path)
{
  std::string retval;

  if (dir_sep_char () == '/')
    retval = path;
  else
    {
      size_t n = path.length ();
      for (size_t i = 0; i < n; i++)
        {
          if (path[i] == '/')
            retval += dir_sep_char();
          else
            retval += path[i];
        }
    }

  return retval;
}

int
octave_mkdir (const std::string& nm, mode_t md)
{
  std::string msg;
  return octave_mkdir (nm, md, msg);
}

int
octave_mkdir (const std::string& name, mode_t mode, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::mkdir (name.c_str (), mode);

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

int
octave_mkfifo (const std::string& nm, mode_t md)
{
  std::string msg;
  return octave_mkfifo (nm, md, msg);
}

int
octave_mkfifo (const std::string& name, mode_t mode, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  // With gnulib we will always have mkfifo, but some systems like MinGW
  // don't have working mkfifo functions.  On those systems, mkfifo will
  // always return -1 and set errno.

  status = gnulib::mkfifo (name.c_str (), mode);

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

int
octave_link (const std::string& old_name, const std::string& new_name)
{
  std::string msg;
  return octave_link (old_name, new_name, msg);
}

int
octave_link (const std::string& old_name,
             const std::string& new_name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::link (old_name.c_str (), new_name.c_str ());

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

int
octave_symlink (const std::string& old_name, const std::string& new_name)
{
  std::string msg;
  return octave_symlink (old_name, new_name, msg);
}

int
octave_symlink (const std::string& old_name,
                const std::string& new_name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::symlink (old_name.c_str (), new_name.c_str ());

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

int
octave_readlink (const std::string& path, std::string& result)
{
  std::string msg;
  return octave_readlink (path, result, msg);
}

int
octave_readlink (const std::string& path, std::string& result,
                 std::string& msg)
{
  int status = -1;

  msg = std::string ();

  char buf[MAXPATHLEN+1];

  status = gnulib::readlink (path.c_str (), buf, MAXPATHLEN);

  if (status < 0)
    msg = gnulib::strerror (errno);
  else
    {
      buf[status] = '\0';
      result = std::string (buf);
      status = 0;
    }

  return status;
}

int
octave_rename (const std::string& from, const std::string& to)
{
  std::string msg;
  return octave_rename (from, to, msg);
}

int
octave_rename (const std::string& from, const std::string& to,
               std::string& msg)
{
  int status = -1;

  msg = std::string ();

  status = gnulib::rename (from.c_str (), to.c_str ());

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

int
octave_rmdir (const std::string& name)
{
  std::string msg;
  return octave_rmdir (name, msg);
}

int
octave_rmdir (const std::string& name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::rmdir (name.c_str ());

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

// And a version that works recursively.

int
octave_recursive_rmdir (const std::string& name)
{
  std::string msg;
  return octave_recursive_rmdir (name, msg);
}

int
octave_recursive_rmdir (const std::string& name, std::string& msg)
{
  msg = std::string ();

  int status = 0;

  dir_entry dir (name);

  if (dir)
    {
      string_vector dirlist = dir.read ();

      for (octave_idx_type i = 0; i < dirlist.length (); i++)
        {
          octave_quit ();

          std::string nm = dirlist[i];

          // Skip current directory and parent.
          if (nm == "." || nm == "..")
            continue;

          std::string fullnm = name + file_ops::dir_sep_str () + nm;

          // Get info about the file.  Don't follow links.
          file_stat fs (fullnm, false);

          if (fs)
            {
              if (fs.is_dir ())
                {
                  status = octave_recursive_rmdir (fullnm, msg);

                  if (status < 0)
                    break;
                }
              else
                {
                  status = octave_unlink (fullnm, msg);

                  if (status < 0)
                    break;
                }
            }
          else
            {
              msg = fs.error ();
              break;
            }
        }

      if (status >= 0)
        {
          dir.close ();
          status = octave_rmdir (name, msg);
        }
    }
  else
    {
      status = -1;

      msg = dir.error ();
    }

  return status;
}

int
octave_umask (mode_t mode)
{
#if defined (HAVE_UMASK)
  return umask (mode);
#else
  return 0;
#endif
}

int
octave_unlink (const std::string& name)
{
  std::string msg;
  return octave_unlink (name, msg);
}

int
octave_unlink (const std::string& name, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::unlink (name.c_str ());

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

std::string
octave_tempnam (const std::string& dir, const std::string& pfx)
{
  std::string msg;
  return octave_tempnam (dir, pfx, msg);
}

std::string
octave_tempnam (const std::string& dir, const std::string& pfx,
                std::string& msg)
{
  msg = std::string ();

  std::string retval;

  // get dir path to use for template
  std::string templatename;
  if (dir.empty ())
    templatename = octave_env::get_temp_directory ();
  else if (! file_stat (dir, false).is_dir ())
    templatename = octave_env::get_temp_directory ();
  else
    templatename = dir;

  // add dir sep char if it is not there
  if (*templatename.rbegin () != file_ops::dir_sep_char ())
    templatename += file_ops::dir_sep_char ();

  if (pfx.empty ())
    templatename += "file";
  else
    templatename += pfx;

  // add the required XXXXXX for the template
  templatename += "XXXXXX";

  // create and copy template to char array for call to gen_tempname
  char tname [templatename.length () + 1];

  strcpy (tname, templatename.c_str ());

  if (gen_tempname (tname, 0, 0, GT_NOCREATE) == -1)
    msg = gnulib::strerror (errno);
  else
    retval = tname;

  return retval;
}

std::string
octave_canonicalize_file_name (const std::string& name)
{
  std::string msg;
  return octave_canonicalize_file_name (name, msg);
}

std::string
octave_canonicalize_file_name (const std::string& name, std::string& msg)
{
  msg = std::string ();

  std::string retval;

  char *tmp = gnulib::canonicalize_file_name (name.c_str ());

  if (tmp)
    {
      retval = tmp;
      free (tmp);
    }

#if (defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM))
  // Canonical Windows file separator is backslash.
  std::replace (retval.begin (), retval.end (), '/', '\\');
#endif

  if (retval.empty ())
    msg = gnulib::strerror (errno);

  return retval;
}

