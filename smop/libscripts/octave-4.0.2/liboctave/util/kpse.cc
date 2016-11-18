// This file is not compiled to a separate object file.  It is
// included in pathsearch.cc.

/* Look up a filename in a path.

Copyright (C) 2003-2015 John W. Eaton
Copyright (C) 1993, 94, 95, 96, 97, 98 Karl Berry.
Copyright (C) 1993, 94, 95, 96, 97 Karl Berry & O. Weber.
Copyright (C) 1992, 93, 94, 95, 96, 97 Free Software Foundation, Inc.

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

#if defined (HAVE_CONFIG_H)
#include <config.h>
#endif

#include <map>
#include <string>

/* System defines are for non-Unix systems only.  (Testing for all Unix
   variations should be done in configure.)  Presently the defines used
   are: DOS OS2 WIN32.  I do not use any of these systems
   myself; if you do, I'd be grateful for any changes. --kb@mail.tug.org */

/* If we have either DOS or OS2, we are DOSISH.  */
#if defined (DOS) || defined (OS2) || defined (WIN32) || defined (__MSDOS__)
#define DOSISH
#endif

#if defined (DOSISH)
#define MONOCASE_FILENAMES      /* case-insensitive filename comparisons */
#endif

extern "C" {
#if defined (__MINGW32__)
#include <windows.h>
#include <fcntl.h>
#include <dirent.h>
#elif defined (WIN32)
#ifndef _MSC_VER
#define __STDC__ 1
#include "win32lib.h"
#endif
#endif /* not WIN32 */

#ifdef __DJGPP__
#include <fcntl.h>      /* for long filenames' stuff */
#include <dir.h>        /* for 'getdisk' */
#include <io.h>         /* for 'setmode' */
#endif
}

/* Some drivers have partially integrated kpathsea changes.  */
#ifndef KPATHSEA
#define KPATHSEA 32
#endif

/* System dependencies that are figured out by 'configure'.  If we are
   compiling standalone, we get our c-auto.h.  Otherwise, the package
   containing us must provide this (unless it can somehow generate ours
   from c-auto.in).  We use <...> instead of "..." so that the current
   cpp directory (i.e., kpathsea/) won't be searched. */

/* If you want to find subdirectories in a directory with non-Unix
   semantics (specifically, if a directory with no subdirectories does
   not have exactly two links), define this.  */
#if defined (__DJGPP__) || ! defined (DOSISH)
/* Surprise!  DJGPP returns st_nlink exactly like on Unix.  */
#define ST_NLINK_TRICK
#endif /* either not DOSISH or __DJGPP__ */

#ifdef OS2
#define access ln_access
#define fopen ln_fopen
#define rename ln_rename
#define stat ln_stat
#endif /* OS2 */

/* Define the characters which separate components of
   filenames and environment variable paths.  */

/* What separates filename components?  */
#ifndef DIR_SEP
#ifdef DOSISH
/* Either \'s or 's work.  Wayne Sullivan's web2pc prefers /, so we'll
   go with that.  */
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#define IS_DEVICE_SEP(ch) ((ch) == ':')
#define NAME_BEGINS_WITH_DEVICE(name) ((name.length ()>0) && IS_DEVICE_SEP((name)[1]))
/* On DOS, it's good to allow both \ and / between directories.  */
#define IS_DIR_SEP(ch) ((ch) == '/' || (ch) == '\\')
#else
#define DIR_SEP '/'
#define DIR_SEP_STRING "/"
#endif /* not DOSISH */
#endif /* not DIR_SEP */

#ifndef IS_DIR_SEP
#define IS_DIR_SEP(ch) ((ch) == DIR_SEP)
#endif
#ifndef IS_DEVICE_SEP /* No 'devices' on, e.g., Unix.  */
#define IS_DEVICE_SEP(ch) 0
#endif
#ifndef NAME_BEGINS_WITH_DEVICE
#define NAME_BEGINS_WITH_DEVICE(name) 0
#endif

#include "lo-error.h"
#include "oct-env.h"
#include "oct-passwd.h"
#include "str-vec.h"

/* Header files that essentially all of our sources need, and
   that all implementations have.  We include these first, to help with
   NULL being defined multiple times.  */
#include <cstdio>
#include <cstdarg>
#include <cstdlib>
#include <cerrno>
#include <cassert>

#include <sys/types.h>
#include <unistd.h>

#include "sysdir.h"
#include "statdefs.h"

/* define NAME_MAX, the maximum length of a single
   component in a filename.  No such limit may exist, or may vary
   depending on the filesystem.  */

/* Most likely the system will truncate filenames if it is not POSIX,
   and so we can use the BSD value here.  */
#ifndef _POSIX_NAME_MAX
#define _POSIX_NAME_MAX 255
#endif

#ifndef NAME_MAX
#define NAME_MAX _POSIX_NAME_MAX
#endif

#include <cctype>

/* What separates elements in environment variable path lists?  */
#ifndef ENV_SEP
#if defined (SEPCHAR) && defined (SEPCHAR_STR)
#define ENV_SEP SEPCHAR
#define ENV_SEP_STRING SEPCHAR_STR
#elif defined (DOSISH)
#define ENV_SEP ';'
#define ENV_SEP_STRING ";"
#else
#define ENV_SEP ':'
#define ENV_SEP_STRING ":"
#endif /* not DOS */
#endif /* not ENV_SEP */

#ifndef IS_ENV_SEP
#define IS_ENV_SEP(ch) ((ch) == ENV_SEP)
#endif

/* define PATH_MAX, the maximum length of a filename.  Since no such
   limit may exist, it's preferable to dynamically grow filenames as
   needed.  */

/* Cheat and define this as a manifest constant no matter what, instead
   of using pathconf.  I forget why we want to do this.  */

#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 255
#endif

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX _POSIX_PATH_MAX
#endif
#endif /* not PATH_MAX */

/* If NO_DEBUG is defined (not recommended), skip all this.  */
#ifndef NO_DEBUG

/* OK, we'll have tracing support.  */
#define KPSE_DEBUG

/* Test if a bit is on.  */
#define KPSE_DEBUG_P(bit) (kpathsea_debug & (1 << (bit)))

#define KPSE_DEBUG_STAT 0               /* stat calls */
#define KPSE_DEBUG_HASH 1               /* hash lookups */
#define KPSE_DEBUG_FOPEN 2              /* fopen/fclose calls */
#define KPSE_DEBUG_PATHS 3              /* search path initializations */
#define KPSE_DEBUG_EXPAND 4             /* path element expansion */
#define KPSE_DEBUG_SEARCH 5             /* searches */
#define KPSE_DEBUG_VARS 6               /* variable values */
#define KPSE_LAST_DEBUG KPSE_DEBUG_VARS

/* A printf for the debugging.  */
#define DEBUGF_START() do { gnulib::fputs ("kdebug:", stderr)
#define DEBUGF_END()        gnulib::fflush (stderr); } while (0)

#define DEBUGF(str)                                                     \
  DEBUGF_START (); gnulib::fputs (str, stderr); DEBUGF_END ()
#define DEBUGF1(str, e1)                                                \
  DEBUGF_START (); gnulib::fprintf (stderr, str, e1); DEBUGF_END ()
#define DEBUGF2(str, e1, e2)                                            \
  DEBUGF_START (); gnulib::fprintf (stderr, str, e1, e2); DEBUGF_END ()
#define DEBUGF3(str, e1, e2, e3)                                        \
  DEBUGF_START (); gnulib::fprintf (stderr, str, e1, e2, e3); DEBUGF_END ()
#define DEBUGF4(str, e1, e2, e3, e4)                                    \
  DEBUGF_START (); gnulib::fprintf (stderr, str, e1, e2, e3, e4); DEBUGF_END ()

#endif /* not NO_DEBUG */

#ifdef KPSE_DEBUG
static unsigned int kpathsea_debug = 0;
#endif

#if defined (WIN32) && !defined (__MINGW32__)

/* System description file for Windows NT.  */

/*
 *      Define symbols to identify the version of Unix this is.
 *      Define all the symbols that apply correctly.
 */

#ifndef DOSISH
#define DOSISH
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif

/* These have to be defined because our compilers treat __STDC__ as being
   defined (most of them anyway). */

#define access  _access
#define stat    _stat
#define strdup  _strdup

#define S_IFMT   _S_IFMT
#define S_IFDIR  _S_IFDIR

/* Define this so that winsock.h definitions don't get included when
   windows.h is...  For this to have proper effect, config.h must
   always be included before windows.h.  */
#define _WINSOCKAPI_    1

#include <windows.h>

/* For proper declaration of environ.  */
#include <io.h>
#include <fcntl.h>
#include <process.h>

/* ============================================================ */

#endif /* WIN32 */

/* Define common sorts of messages.  */

/* This should be called only after a system call fails.  Don't exit
   with status 'errno', because that might be 256, which would mean
   success (exit statuses are truncated to eight bits).  */
#define FATAL_PERROR(str) \
  do \
    { \
      gnulib::fputs ("pathsearch: ", stderr); \
      perror (str); exit (EXIT_FAILURE); \
    } \
  while (0)

#define FATAL(str) \
  do \
    { \
      gnulib::fputs ("pathsearch: fatal: ", stderr); \
      gnulib::fputs (str, stderr); \
      gnulib::fputs (".\n", stderr); \
      exit (1); \
    } \
  while (0)

#ifndef WIN32
static void xclosedir (DIR *d);
#endif

/* It's a little bizarre to be using the same type for the list and the
   elements of the list, but no reason not to in this case, I think --
   we never need a NULL string in the middle of the list, and an extra
   NULL/NULL element always at the end is inconsequential.  */

struct str_llist_elt
{
  str_llist_elt (void) : str (), moved (0), next (0) { }

  ~str_llist_elt (void) { }

  std::string str;
  int moved;
  struct str_llist_elt *next;
};

typedef str_llist_elt str_llist_elt_type;
typedef str_llist_elt *str_llist_type;

#define STR_LLIST(sl) ((sl).str)
#define STR_LLIST_MOVED(sl) ((sl).moved)
#define STR_LLIST_NEXT(sl) ((sl).next)

static void str_llist_add (str_llist_type *l, const std::string& str);

static void str_llist_float (str_llist_type *l, str_llist_elt_type *mover);

static std::string kpse_var_expand (const std::string& src);

static str_llist_type *kpse_element_dirs (const std::string& elt);

static std::string kpse_expand (const std::string& s);

static std::string kpse_expand_default (const std::string& path,
                                        const std::string& dflt);

static string_vector kpse_db_search (const std::string& name,
                                     const std::string& path_elt, bool all);

#include <ctime> /* for 'time' */

static bool
kpse_is_env_sep (char c)
{
  return IS_ENV_SEP (c);
}

/* These routines just check the return status from standard library
   routines and abort if an error happens.  */

static FILE *
xfopen (const std::string& filename, const char *mode)
{
  FILE *f;

  assert (! filename.empty () && mode);

  f = gnulib::fopen (filename.c_str (), mode);

  if (! f)
    FATAL_PERROR (filename.c_str ());

  if (KPSE_DEBUG_P (KPSE_DEBUG_FOPEN))
    DEBUGF3 ("fopen (%s, %s) => 0x%lx\n", filename.c_str (), mode,
             reinterpret_cast<intptr_t> (f));

  return f;
}

/* A single (key,value) pair.  */

struct hash_element_type
{
  std::string key;
  std::string value;
  struct hash_element_type *next;
};

/* The usual arrangement of buckets initialized to null.  */

struct hash_table_type
{
  hash_element_type **buckets;
  unsigned size;
};

static unsigned
kpse_hash (hash_table_type table, const std::string& key)
{
  unsigned n = 0;

  /* Our keys aren't often anagrams of each other, so no point in
     weighting the characters.  */
  size_t len = key.length ();
  for (size_t i = 0; i < len; i++)
    n = (n + n + key[i]) % table.size;

  return n;
}

/* Look up STR in MAP.  Return a (dynamically-allocated) list of the
   corresponding strings or NULL if no match.  */

static string_vector
hash_lookup (hash_table_type table, const std::string& key)
{
  hash_element_type *p;
  string_vector ret;
  unsigned n = kpse_hash (table, key);

  /* Look at everything in this bucket.  */
  for (p = table.buckets[n]; p; p = p->next)
    if (key == p->key)
      ret.append (p->value);

#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_HASH))
    {
      DEBUGF1 ("hash_lookup (%s) =>", key.c_str ());
      if (ret.empty ())
        gnulib::fputs (" (nil)\n", stderr);
      else
        {
          int len = ret.length ();
          for (int i = 0; i < len; i++)
            {
              gnulib::putc (' ', stderr);
              gnulib::fputs (ret[i].c_str (), stderr);
            }
          gnulib::putc ('\n', stderr);
        }
      gnulib::fflush (stderr);
    }
#endif

  return ret;
}

/* A way to step through a path, extracting one directory name at a
   time.  */

class kpse_path_iterator
{
public:

  kpse_path_iterator (const std::string& p)
    : path (p), b (0), e (0), len (path.length ()) { set_end (); }

  kpse_path_iterator (const kpse_path_iterator& pi)
    : path (pi.path), b (pi.b), e (pi.e), len (pi.len) { }

  kpse_path_iterator operator ++ (int)
  {
    kpse_path_iterator retval (*this);
    next ();
    return retval;
  }

  std::string operator * (void) { return path.substr (b, e-b); }

  bool operator != (const size_t sz) { return b != sz; }

private:

  const std::string& path;
  size_t b;
  size_t e;
  size_t len;

  void set_end (void)
  {
    e = b + 1;

    if (e == len)
      ; /* OK, we have found the last element.  */
    else if (e > len)
      b = e = std::string::npos;
    else
      {
        /* Find the next colon not enclosed by braces (or the end of
           the path).  */

        int brace_level = 0;
        while (e < len && ! (brace_level == 0 && kpse_is_env_sep (path[e])))
          e++;
      }
  }

  void next (void)
  {
    b = e + 1;

    /* Skip any consecutive colons.  */
    while (b < len && kpse_is_env_sep (path[b]))
      b++;

    if (b >= len)
      b = e = std::string::npos;
    else
      set_end ();
  }

  // No assignment.
  kpse_path_iterator& operator = (const kpse_path_iterator&);
};

/* Here's the simple one, when a program just wants a value.  */

static std::string
kpse_var_value (const std::string& var)
{
  std::string ret;

  std::string tmp = octave_env::getenv (var);

  if (! tmp.empty ())
    ret = kpse_var_expand (tmp);

#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_VARS))
    DEBUGF2 ("variable: %s = %s\n", var.c_str (),
             tmp.empty () ? "(nil)" :  tmp.c_str ());
#endif

  return ret;
}

/* Truncate any too-long components in NAME, returning the result.  It's
   too bad this is necessary.  See comments in readable.c for why.  */

static std::string
kpse_truncate_filename (const std::string& name)
{
  unsigned c_len = 0;        /* Length of current component.  */
  unsigned ret_len = 0;      /* Length of constructed result.  */

  std::string ret = name;

  size_t len = name.length ();

  for (size_t i = 0; i < len; i++)
    {
      if (IS_DIR_SEP (name[i]) || IS_DEVICE_SEP (name[i]))
        {
          /* At a directory delimiter, reset component length.  */
          c_len = 0;
        }
      else if (c_len > NAME_MAX)
        {
          /* If past the max for a component, ignore this character.  */
          continue;
        }

      /* Copy this character.  */
      ret[ret_len++] = name[i];
      c_len++;
    }

  ret.resize (ret_len);

  return ret;
}

/* If access can read FN, run stat (assigning to stat buffer ST) and
   check that fn is not a directory.  Don't check for just being a
   regular file, as it is potentially useful to read fifo's or some
   kinds of devices.  */

#ifdef WIN32
static inline bool
READABLE (const std::string& fn, struct stat&)
{
  const char *t = fn.c_str ();
  return (GetFileAttributes (t) != 0xFFFFFFFF
          && ! (GetFileAttributes (t) & FILE_ATTRIBUTE_DIRECTORY));
}
#else
static inline bool
READABLE (const std::string& fn, struct stat& st)
{
  const char *t = fn.c_str ();
  return (access (t, R_OK) == 0
          && stat (t, &(st)) == 0 && ! S_ISDIR (st.st_mode));
}
#endif

/* POSIX invented the brain-damage of not necessarily truncating
   filename components; the system's behavior is defined by the value of
   the symbol _POSIX_NO_TRUNC, but you can't change it dynamically!

   Generic const return warning.  See extend-fname.c.  */

static std::string
kpse_readable_file (const std::string& name)
{
  struct stat st;
  std::string ret;

  if (READABLE (name, st))
    {
      ret = name;

#ifdef ENAMETOOLONG
    }
  else if (errno == ENAMETOOLONG)
    {
      ret = kpse_truncate_filename (name);

      /* Perhaps some other error will occur with the truncated name,
         so let's call access again.  */

      if (! READABLE (ret, st))
        {
          /* Failed.  */
          ret = std::string ();
        }
#endif /* ENAMETOOLONG */

    }
  else
    {
      /* Some other error.  */
      if (errno == EACCES)
        {
          /* Maybe warn them if permissions are bad.  */
          perror (name.c_str ());
        }

      ret = std::string ();
    }

  return ret;
}

/* Sorry this is such a system-dependent mess, but I can't see any way
   to usefully generalize.  */

static bool
kpse_absolute_p (const std::string& filename, int relative_ok)
{
  size_t len = filename.length ();

  int absolute = (len > 0 && IS_DIR_SEP (filename[0]))
#ifdef DOSISH
                 /* Novell allows non-alphanumeric drive letters. */
                 || (len > 0 && IS_DEVICE_SEP (filename[1]))
#endif /* DOSISH */
#ifdef WIN32
                 /* UNC names */
                 || (len > 1 && filename[0] == '\\' && filename[1] == '\\')
#endif
                 ;

  int explicit_relative
    = relative_ok
      && (len > 1
          && filename[0] == '.'
          && (IS_DIR_SEP (filename[1])
              || (len > 2 && filename[1] == '.' && IS_DIR_SEP (filename[2]))));

  return absolute || explicit_relative;
}

/* The very first search is for texmf.cnf, called when someone tries to
   initialize the TFM path or whatever.  init_path calls kpse_cnf_get
   which calls kpse_all_path_search to find all the texmf.cnf's.  We
   need to do various special things in this case, since we obviously
   don't yet have the configuration files when we're searching for the
   configuration files.  */
static bool first_search = true;

/* This function is called after every search (except the first, since
   we definitely want to allow enabling the logging in texmf.cnf) to
   record the filename(s) found in $TEXMFLOG.  */

static void
log_search (const string_vector& filenames)
{
  static FILE *log_file = 0;
  static bool first_time = true; /* Need to open the log file?  */

  if (first_time)
    {
      first_time = false;

      /* Get name from either envvar or config file.  */
      std::string log_name = kpse_var_value ("TEXMFLOG");

      if (! log_name.empty ())
        {
          log_file = xfopen (log_name.c_str (), "a");

          if (! log_file)
            perror (log_name.c_str ());
        }
    }

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH) || log_file)
    {
      /* FILENAMES should never be null, but safety doesn't hurt.  */
      for (int e = 0; e < filenames.length () && ! filenames[e].empty (); e++)
        {
          std::string filename = filenames[e];

          /* Only record absolute filenames, for privacy.  */
          if (log_file && kpse_absolute_p (filename.c_str (), false))
            gnulib::fprintf (log_file, "%lu %s\n",
                             static_cast<unsigned long> (time (0)),
                             filename.c_str ());

          /* And show them online, if debugging.  We've already started
             the debugging line in 'search', where this is called, so
             just print the filename here, don't use DEBUGF.  */
          if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
            gnulib::fputs (filename.c_str (), stderr);
        }
    }
}

/* Concatenate each element in DIRS with NAME (assume each ends with a
   /, to save time).  If SEARCH_ALL is false, return the first readable
   regular file.  Else continue to search for more.  In any case, if
   none, return a list containing just NULL.

   We keep a single buffer for the potential filenames and reallocate
   only when necessary.  I'm not sure it's noticeably faster, but it
   does seem cleaner.  (We do waste a bit of space in the return
   value, though, since we don't shrink it to the final size returned.)  */

static string_vector
dir_list_search (str_llist_type *dirs, const std::string& name,
                 bool search_all)
{
  str_llist_elt_type *elt;
  string_vector ret;

  for (elt = *dirs; elt; elt = STR_LLIST_NEXT (*elt))
    {
      const std::string dir = STR_LLIST (*elt);

      std::string potential = dir + name;

      std::string tmp = kpse_readable_file (potential);

      if (! tmp.empty ())
        {
          ret.append (potential);

          /* Move this element towards the top of the list.  */
          str_llist_float (dirs, elt);

          if (! search_all)
            return ret;
        }
    }

  return ret;
}

/* This is called when NAME is absolute or explicitly relative; if it's
   readable, return (a list containing) it; otherwise, return NULL.  */

static string_vector
absolute_search (const std::string& name)
{
  string_vector ret_list;
  std::string found = kpse_readable_file (name);

  /* Add 'found' to the return list even if it's null; that tells
     the caller we didn't find anything.  */
  ret_list.append (found);

  return ret_list;
}

/* This is the hard case -- look for NAME in PATH.  If ALL is false,
   return the first file found.  Otherwise, search all elements of PATH.  */

static string_vector
path_search (const std::string& path, const std::string& name,
             bool /* must_exist */, bool all)
{
  string_vector ret_list;
  bool done = false;

  for (kpse_path_iterator pi (path); ! done && pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      string_vector found;
      bool allow_disk_search = true;

      if (elt.length () > 1 && elt[0] == '!' && elt[1] == '!')
        {
          /* Those magic leading chars in a path element means don't
             search the disk for this elt.  And move past the magic to
             get to the name.  */
          allow_disk_search = false;
          elt = elt.substr (2);
        }

      /* Do not touch the device if present */
      if (NAME_BEGINS_WITH_DEVICE (elt))
        {
          while (elt.length () > 3
                 && IS_DIR_SEP (elt[2]) && IS_DIR_SEP (elt[3]))
            {
              elt[2] = elt[1];
              elt[1] = elt[0];
              elt = elt.substr (1);
            }
        }
      else
        {
          /* We never want to search the whole disk.  */
          while (elt.length () > 1
                 && IS_DIR_SEP (elt[0]) && IS_DIR_SEP (elt[1]))
            elt = elt.substr (1);
        }

      /* Try ls-R, unless we're searching for texmf.cnf.  Our caller
         (search), also tests first_search, and does the resetting.  */
      found = first_search ? string_vector () : kpse_db_search (name, elt, all);

      /* Search the filesystem if (1) the path spec allows it, and either
         (2a) we are searching for texmf.cnf ; or
         (2b) no db exists; or
         (2c) no db's are relevant to this elt; or
         (3) MUST_EXIST && NAME was not in the db.
         In (2*), 'found' will be NULL.
         In (3),  'found' will be an empty list. */

      if (allow_disk_search && found.empty ())
        {
          str_llist_type *dirs = kpse_element_dirs (elt);

          if (dirs && *dirs)
            found = dir_list_search (dirs, name, all);
        }

      /* Did we find anything anywhere?  */
      if (! found.empty ())
        {
          if (all)
            ret_list.append (found);
          else
            {
              ret_list.append (found[0]);
              done = true;
            }
        }
    }

  return ret_list;
}

/* Search PATH for ORIGINAL_NAME.  If ALL is false, or ORIGINAL_NAME is
   absolute_p, check ORIGINAL_NAME itself.  Otherwise, look at each
   element of PATH for the first readable ORIGINAL_NAME.

   Always return a list; if no files are found, the list will
   contain just NULL.  If ALL is true, the list will be
   terminated with NULL.  */

static string_vector
search (const std::string& path, const std::string& original_name,
        bool must_exist, bool all)
{
  string_vector ret_list;
  bool absolute_p;

  /* Make a leading ~ count as an absolute filename, and expand $FOO's.  */
  std::string name = kpse_expand (original_name);

  /* If the first name is absolute or explicitly relative, no need to
     consider PATH at all.  */
  absolute_p = kpse_absolute_p (name, true);

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    DEBUGF4 ("start search (file=%s, must_exist=%d, find_all=%d, path=%s).\n",
             name.c_str (), must_exist, all, path.c_str ());

  /* Find the file(s). */
  ret_list = absolute_p ? absolute_search (name)
                        : path_search (path, name, must_exist, all);

  /* The very first search is for texmf.cnf.  We can't log that, since
     we want to allow setting TEXMFLOG in texmf.cnf.  */
  if (first_search)
    {
      first_search = false;
    }
  else
    {
      /* Record the filenames we found, if desired.  And wrap them in a
         debugging line if we're doing that.  */

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        DEBUGF1 ("search (%s) =>", original_name.c_str ());

      log_search (ret_list);

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        gnulib::putc ('\n', stderr);
    }

  return ret_list;
}

/* Search PATH for the first NAME.  */

/* Call 'kpse_expand' on NAME.  If the result is an absolute or
   explicitly relative filename, check whether it is a readable
   (regular) file.

   Otherwise, look in each of the directories specified in PATH (also do
   tilde and variable expansion on elements in PATH), using a prebuilt
   db (see db.h) if it's relevant for a given path element.

   If the prebuilt db doesn't exist, or if MUST_EXIST is true and NAME
   isn't found in the prebuilt db, look on the filesystem.  (I.e., if
   MUST_EXIST is false, and NAME isn't found in the db, do *not* look on
   the filesystem.)

   The caller must expand PATH. This is because it makes more sense to
   do this once, in advance, instead of for every search using it.

   In any case, return the complete filename if found, otherwise NULL.  */

static std::string
kpse_path_search (const std::string& path, const std::string& name,
                  bool must_exist)
{
  string_vector ret_list = search (path, name, must_exist, false);

  return ret_list.empty () ? std::string () : ret_list[0];
}

/* Search all elements of PATH for files named NAME.  Not sure if it's
   right to assert 'must_exist' here, but it suffices now.  */

/* Like 'kpse_path_search' with MUST_EXIST true, but return a list of
   all the filenames (or NULL if none), instead of taking the first.  */

static string_vector
kpse_all_path_search (const std::string& path, const std::string& name)
{
  return search (path, name, true, true);
}

/* This is the hard case -- look in each element of PATH for each
   element of NAMES.  If ALL is false, return the first file found.
   Otherwise, search all elements of PATH.  */

static string_vector
path_find_first_of (const std::string& path, const string_vector& names,
                    bool /* must_exist */, bool all)
{
  string_vector ret_list;
  bool done = false;

  for (kpse_path_iterator pi (path); ! done && pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      str_llist_type *dirs;
      str_llist_elt_type *dirs_elt;
      string_vector found;
      bool allow_disk_search = true;

      if (elt.length () > 1 && elt[0] == '!' && elt[1] == '!')
        {
          /* Those magic leading chars in a path element means don't
             search the disk for this elt.  And move past the magic to
             get to the name.  */

          allow_disk_search = false;
          elt = elt.substr (2);
        }

      /* Do not touch the device if present */

      if (NAME_BEGINS_WITH_DEVICE (elt))
        {
          while (elt.length () > 3
                 && IS_DIR_SEP (elt[2]) && IS_DIR_SEP (elt[3]))
            {
              elt[2] = elt[1];
              elt[1] = elt[0];
              elt = elt.substr (1);
            }
        }
      else
        {
          /* We never want to search the whole disk.  */
          while (elt.length () > 1
                 && IS_DIR_SEP (elt[0]) && IS_DIR_SEP (elt[1]))
            elt = elt.substr (1);
        }

      /* We have to search one directory at a time.  */
      dirs = kpse_element_dirs (elt);
      for (dirs_elt = *dirs; dirs_elt; dirs_elt = STR_LLIST_NEXT (*dirs_elt))
        {
          const std::string dir = STR_LLIST (*dirs_elt);

          int len = names.length ();
          for (int i = 0; i < len && !done; i++)
            {
              std::string name = names[i];

              /* Try ls-R, unless we're searching for texmf.cnf.  Our caller
                 (find_first_of), also tests first_search, and does the
                 resetting.  */
              found = first_search ? string_vector ()
                                   : kpse_db_search (name, dir.c_str (), all);

              /* Search the filesystem if (1) the path spec allows it,
                 and either

                   (2a) we are searching for texmf.cnf ; or
                   (2b) no db exists; or
                   (2c) no db's are relevant to this elt; or
                   (3) MUST_EXIST && NAME was not in the db.

                 In (2*), 'found' will be NULL.
                 In (3),  'found' will be an empty list. */

              if (allow_disk_search && found.empty ())
                {
                  static str_llist_type *tmp = 0;

                  if (! tmp)
                    {
                      tmp = new str_llist_type;
                      *tmp = 0;
                      str_llist_add (tmp, "");
                    }

                  STR_LLIST (*(*tmp)) = dir;

                  found = dir_list_search (tmp, name, all);
                }

              /* Did we find anything anywhere?  */
              if (! found.empty ())
                {
                  if (all)
                    ret_list.append (found);
                  else
                    {
                      ret_list.append (found[0]);
                      done = true;
                    }
                }
            }
        }
    }

  return ret_list;
}

static string_vector
find_first_of (const std::string& path, const string_vector& names,
               bool must_exist, bool all)
{
  string_vector ret_list;

  if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
    {
      gnulib::fputs ("start find_first_of ((", stderr);

      int len = names.length ();

      for (int i = 0; i < len; i++)
        {
          if (i == 0)
            gnulib::fputs (names[i].c_str (), stderr);
          else
            gnulib::fprintf (stderr, ", %s", names[i].c_str ());
        }

      gnulib::fprintf (stderr, "), path=%s, must_exist=%d).\n",
                       path.c_str (), must_exist);
    }

  for (int i = 0; i < names.length (); i++)
    {
      std::string name = names[i];

      if (kpse_absolute_p (name, true))
        {
          /* If the name is absolute or explicitly relative, no need
             to consider PATH at all.  If we find something, then we
             are done.  */

          ret_list = absolute_search (name);

          if (! ret_list.empty ())
            return ret_list;
        }
    }

  /* Find the file. */
  ret_list = path_find_first_of (path, names, must_exist, all);

  /* The very first search is for texmf.cnf.  We can't log that, since
     we want to allow setting TEXMFLOG in texmf.cnf.  */
  if (first_search)
    {
      first_search = false;
    }
  else
    {
      /* Record the filenames we found, if desired.  And wrap them in a
         debugging line if we're doing that.  */

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        {
          gnulib::fputs ("find_first_of (", stderr);

          int len = names.length ();

          for (int i = 0; i < len; i++)
            {
              if (i == 0)
                gnulib::fputs (names[i].c_str (), stderr);
              else
                gnulib::fprintf (stderr, ", %s", names[i].c_str ());
            }

          gnulib::fputs (") =>", stderr);
        }

      log_search (ret_list);

      if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
        gnulib::putc ('\n', stderr);
    }

  return ret_list;
}

/* Search each element of PATH for each element of NAMES.  Return the
   first one found.  */

/* Search each element of PATH for each element in the list of NAMES.
   Return the first one found.  */

static std::string
kpse_path_find_first_of (const std::string& path, const string_vector& names,
                         bool must_exist)
{
  string_vector ret_list = find_first_of (path, names, must_exist, false);

  return ret_list.empty () ? std::string () : ret_list[0];
}

/* Search each element of PATH for each element of NAMES and return a
   list containing everything found, in the order found.  */

/* Like 'kpse_path_find_first_of' with MUST_EXIST true, but return a
   list of all the filenames (or NULL if none), instead of taking the
   first.  */

static string_vector
kpse_all_path_find_first_of (const std::string& path,
                             const string_vector& names)
{
  return find_first_of (path, names, true, true);
}

/* General expansion.  Some of this file (the brace-expansion
   code from bash) is covered by the GPL; this is the only GPL-covered
   code in kpathsea.  The part of the file that I wrote (the first
   couple of functions) is covered by the LGPL.  */

/* If NAME has a leading ~ or ~user, Unix-style, expand it to the user's
   home directory, and return a new malloced string.  If no ~, or no
   <pwd.h>, just return NAME.  */

static std::string
kpse_tilde_expand (const std::string& name)
{
  std::string expansion;

  /* If no leading tilde, do nothing.  */
  if (name.empty () || name[0] != '~')
    {
      expansion = name;

      /* If a bare tilde, return the home directory or '.'.  (Very
         unlikely that the directory name will do anyone any good, but
         ...  */
    }
  else if (name.length () == 1)
    {
      expansion = octave_env::get_home_directory ();

      if (expansion.empty ())
        expansion = ".";

      /* If '~/', remove any trailing / or replace leading // in $HOME.
         Should really check for doubled intermediate slashes, too.  */
    }
  else if (IS_DIR_SEP (name[1]))
    {
      unsigned c = 1;
      std::string home = octave_env::get_home_directory ();

      if (home.empty ())
        home = ".";

      size_t home_len = home.length ();

      /* handle leading // */
      if (home_len > 1 && IS_DIR_SEP (home[0]) && IS_DIR_SEP (home[1]))
        home = home.substr (1);

      /* omit / after ~ */
      if (IS_DIR_SEP (home[home_len - 1]))
        c++;

      expansion = home + name.substr (c);

      /* If '~user' or '~user/', look up user in the passwd database (but
         OS/2 doesn't have this concept.  */
    }
  else
#ifdef HAVE_PWD_H
    {
      unsigned c = 2;

      /* find user name */
      while (name.length () > c && ! IS_DIR_SEP (name[c]))
        c++;

      std::string user = name.substr (1, c-1);

      /* We only need the cast here for (deficient) systems
         which do not declare 'getpwnam' in <pwd.h>.  */
      octave_passwd p = octave_passwd::getpwnam (user);

      /* If no such user, just use '.'.  */
      std::string home = p ? p.dir () : std::string (".");

      if (home.empty ())
        home = ".";

      /* handle leading // */
      if (home.length () > 1 && IS_DIR_SEP (home[0]) && IS_DIR_SEP (home[1]))
        home = home.substr (1);

      /* If HOME ends in /, omit the / after ~user. */
      if (name.length () > c && IS_DIR_SEP (home[home.length () - 1]))
        c++;

      expansion = name.length () > c ? home : home + name.substr (c);
    }
#else /* not HAVE_PWD_H */
  expansion = name;
#endif /* not HAVE_PWD_H */

  return expansion;
}

/* Do variable expansion first so ~${USER} works.  (Besides, it's what the
   shells do.)  */

/* Call kpse_var_expand and kpse_tilde_expand (in that order).  Result
   is always in fresh memory, even if no expansions were done.  */

static std::string
kpse_expand (const std::string& s)
{
  std::string var_expansion = kpse_var_expand (s);
  return kpse_tilde_expand (var_expansion);
}

/* Forward declarations of functions from the original expand.c  */
static string_vector brace_expand (const std::string&);

/* If $KPSE_DOT is defined in the environment, prepend it to any relative
   path components. */

static std::string
kpse_expand_kpse_dot (const std::string& path)
{
  std::string ret;
  std::string kpse_dot = octave_env::getenv ("KPSE_DOT");

  if (kpse_dot.empty ())
    return path;

  for (kpse_path_iterator pi (path); pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      /* We assume that the !! magic is only used on absolute components.
         Single "." get special treatment, as does "./" or its  equivalent.  */

      size_t elt_len = elt.length ();

      if (kpse_absolute_p (elt, false)
          || (elt_len > 1 && elt[0] == '!' && elt[1] == '!'))
        ret += elt + ENV_SEP_STRING;
      else if (elt_len == 1 && elt[0] == '.')
        ret += kpse_dot + ENV_SEP_STRING;
      else if (elt_len > 1 && elt[0] == '.' && IS_DIR_SEP (elt[1]))
        ret += kpse_dot + elt.substr (1) + ENV_SEP_STRING;
      else
        ret += kpse_dot + DIR_SEP_STRING + elt + ENV_SEP_STRING;
    }

  int len = ret.length ();
  if (len > 0)
    ret.resize (len-1);

  return ret;
}

/* Do brace expansion on ELT; then do variable and ~ expansion on each
   element of the result; then do brace expansion again, in case a
   variable definition contained braces (e.g., $TEXMF).  Return a
   string comprising all of the results separated by ENV_SEP_STRING.  */

static std::string
kpse_brace_expand_element (const std::string& elt)
{
  std::string ret;

  string_vector expansions = brace_expand (elt);

  for (int i = 0; i < expansions.length (); i++)
    {
      /* Do $ and ~ expansion on each element.  */
      std::string x = kpse_expand (expansions[i]);

      if (x != expansions[i])
        {
          /* If we did any expansions, do brace expansion again.  Since
             recursive variable definitions are not allowed, this recursion
             must terminate.  (In practice, it's unlikely there will ever be
             more than one level of recursion.)  */
          x = kpse_brace_expand_element (x);
        }

      ret += x + ENV_SEP_STRING;
    }

  ret.resize (ret.length () - 1);

  return ret;
}

/* Do brace expansion and call 'kpse_expand' on each element of the
   result; return the final expansion (always in fresh memory, even if
   no expansions were done).  We don't call 'kpse_expand_default'
   because there is a whole sequence of defaults to run through; see
   'kpse_init_format'.  */

static std::string
kpse_brace_expand (const std::string& path)
{
  /* Must do variable expansion first because if we have
       foo = .:~
       TEXINPUTS = $foo
     we want to end up with TEXINPUTS = .:/home/karl.
     Since kpse_path_element is not reentrant, we must get all
     the path elements before we start the loop.  */
  std::string tmp = kpse_var_expand (path);

  std::string ret;

  for (kpse_path_iterator pi (tmp); pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      /* Do brace expansion first, so tilde expansion happens in {~ka,~kb}.  */
      std::string expansion = kpse_brace_expand_element (elt);
      ret += expansion + ENV_SEP_STRING;
    }

  size_t len = ret.length ();
  if (len > 0)
    ret.resize (len-1);

  return kpse_expand_kpse_dot (ret);
}

/* Expand all special constructs in a path, and include only the actually
   existing directories in the result. */

/* Do brace expansion and call 'kpse_expand' on each argument of the
   result, then expand any '//' constructs.  The final expansion (always
   in fresh memory) is a path of all the existing directories that match
   the pattern. */

static std::string
kpse_path_expand (const std::string& path)
{
  std::string ret;
  unsigned len;

  len = 0;

  /* Expand variables and braces first.  */
  std::string tmp = kpse_brace_expand (path);

  /* Now expand each of the path elements, printing the results */
  for (kpse_path_iterator pi (tmp); pi != std::string::npos; pi++)
    {
      std::string elt = *pi;

      str_llist_type *dirs;

      /* Skip and ignore magic leading chars.  */
      if (elt.length () > 1 && elt[0] == '!' && elt[1] == '!')
        elt = elt.substr (2);

      /* Do not touch the device if present */
      if (NAME_BEGINS_WITH_DEVICE (elt))
        {
          while (elt.length () > 3
                 && IS_DIR_SEP (elt[2]) && IS_DIR_SEP (elt[3]))
            {
              elt[2] = elt[1];
              elt[1] = elt[0];
              elt = elt.substr (1);
            }
        }
      else
        {
          /* We never want to search the whole disk.  */
          while (elt.length () > 1
                 && IS_DIR_SEP (elt[0]) && IS_DIR_SEP (elt[1]))
            elt = elt.substr (1);
        }

      /* Search the disk for all dirs in the component specified.
         Be faster to check the database, but this is more reliable.  */
      dirs = kpse_element_dirs (elt);

      if (dirs && *dirs)
        {
          str_llist_elt_type *dir;

          for (dir = *dirs; dir; dir = STR_LLIST_NEXT (*dir))
            {
              const std::string thedir = STR_LLIST (*dir);
              unsigned dirlen = thedir.length ();

              ret += thedir;
              len += dirlen;

              /* Retain trailing slash if that's the root directory.  */
              if (dirlen == 1
                  || (dirlen == 3 && NAME_BEGINS_WITH_DEVICE (thedir)
                      && IS_DIR_SEP (thedir[2])))
                {
                  ret += ENV_SEP_STRING;
                  len++;
                }

              ret[len-1] = ENV_SEP;
            }
        }
    }

  if (len > 0)
    ret.resize (len-1);

  return ret;
}

/* braces.c -- code for doing word expansion in curly braces. Taken from
   bash 1.14.5.  [And subsequently modified for kpatshea.]

   Copyright (C) 1987,1991 Free Software Foundation, Inc.  */

#define brace_whitespace(c) (! (c) || (c) == ' ' || (c) == '\t' || (c) == '\n')

/* Basic idea:

   Segregate the text into 3 sections: preamble (stuff before an open brace),
   postamble (stuff after the matching close brace) and amble (stuff after
   preamble, and before postamble).  Expand amble, and then tack on the
   expansions to preamble.  Expand postamble, and tack on the expansions to
   the result so far.  */

/* Return a new array of strings which is the result of appending each
   string in ARR2 to each string in ARR1.  The resultant array is
   len (arr1) * len (arr2) long.  For convenience, ARR1 (and its contents)
   are free ()'ed.  ARR1 can be NULL, in that case, a new version of ARR2
   is returned. */

static string_vector
array_concat (const string_vector& arr1, const string_vector& arr2)
{
  string_vector result;

  if (arr1.empty ())
    result = arr2;
  else if (arr2.empty ())
    result = arr1;
  else
    {
      int len1 = arr1.length ();
      int len2 = arr2.length ();

      result = string_vector (len1 * len2);

      int k = 0;
      for (int i = 0; i < len2; i++)
        for (int j = 0; j < len1; j++)
          result[k++] = arr1[j] + arr2[i];
    }

  return result;
}

static int brace_gobbler (const std::string&, int&, int);
static string_vector expand_amble (const std::string&);

/* Return an array of strings; the brace expansion of TEXT. */
static string_vector
brace_expand (const std::string& text)
{
  /* Find the text of the preamble. */
  int i = 0;
  int c = brace_gobbler (text, i, '{');

  std::string preamble = text.substr (0, i);

  string_vector result = string_vector (preamble);

  if (c == '{')
    {
      /* Find the amble.  This is the stuff inside this set of braces. */
      int start = ++i;
      c = brace_gobbler (text, i, '}');

      /* What if there isn't a matching close brace? */
      if (! c)
        {
          (*current_liboctave_warning_with_id_handler)
            ("Octave:pathsearch-syntax",
             "%s: Unmatched {", text.c_str ());

          result = string_vector (text);
        }
      else
        {
          std::string amble = text.substr (start, i-start);
          result = array_concat (result, expand_amble (amble));

          std::string postamble = text.substr (i+1);
          result = array_concat (result, brace_expand (postamble));
        }
    }

  return result;
}

/* The character which is used to separate arguments. */
static int brace_arg_separator = ',';

/* Expand the text found inside of braces.  We simply try to split the
   text at BRACE_ARG_SEPARATORs into separate strings.  We then brace
   expand each slot which needs it, until there are no more slots which
   need it. */
static string_vector
expand_amble (const std::string& text)
{
  string_vector result;

  size_t text_len = text.length ();
  size_t start;
  int i, c;

  for (start = 0, i = 0, c = 1; c && start < text_len; start = ++i)
    {
      int i0 = i;
      int c0 = brace_gobbler (text, i0, brace_arg_separator);
      int i1 = i;
      int c1 = brace_gobbler (text, i1, ENV_SEP);
      c = c0 | c1;
      i = (i0 < i1 ? i0 : i1);

      std::string tem = text.substr (start, i-start);

      string_vector partial = brace_expand (tem);

      if (result.empty ())
        result = partial;
      else
        result.append (partial);
    }

  return result;
}

/* Start at INDEX, and skip characters in TEXT. Set INDEX to the
   index of the character matching SATISFY.  This understands about
   quoting.  Return the character that caused us to stop searching;
   this is either the same as SATISFY, or 0. */
static int
brace_gobbler (const std::string& text, int& indx, int satisfy)
{
  int c = 0;
  int level = 0;
  int quoted = 0;
  int pass_next = 0;

  size_t text_len = text.length ();

  size_t i = indx;

  for (; i < text_len; i++)
    {
      c = text[i];

      if (pass_next)
        {
          pass_next = 0;
          continue;
        }

      /* A backslash escapes the next character.  This allows backslash to
         escape the quote character in a double-quoted string. */
      if (c == '\\' && (quoted == 0 || quoted == '"' || quoted == '`'))
        {
          pass_next = 1;
          continue;
        }

      if (quoted)
        {
          if (c == quoted)
            quoted = 0;
          continue;
        }

      if (c == '"' || c == '\'' || c == '`')
        {
          quoted = c;
          continue;
        }

      if (c == satisfy && !level && !quoted)
        {
          /* We ignore an open brace surrounded by whitespace, and also
             an open brace followed immediately by a close brace, that
             was preceded with whitespace.  */
          if (c == '{'
              && ((i == 0 || brace_whitespace (text[i-1]))
                  && (i+1 < text_len
                      && (brace_whitespace (text[i+1]) || text[i+1] == '}'))))
            continue;
          /* If this is being compiled as part of bash, ignore the '{'
             in a '${ }' construct */
          if ((c != '{') || i == 0 || (text[i-1] != '$'))
            break;
        }

      if (c == '{')
        level++;
      else if (c == '}' && level)
        level--;
    }

  indx = i;
  return c;
}

/* For each file format, we record the following information.  The main
   thing that is not part of this structure is the environment variable
   lists. They are used directly in tex-file.c. We could incorporate
   them here, but it would complicate the code a bit. We could also do
   it via variable expansion, but not now, maybe not ever:
   ${PKFONTS-${TEXFONTS-/usr/local/lib/texmf/fonts//}}.  */

struct kpse_format_info_type
{
  kpse_format_info_type (void)
    : type (), path (), raw_path (), path_source (), override_path (),
      client_path (), cnf_path (), default_path (), suffix ()
  { }

  ~kpse_format_info_type (void) { }

  std::string type;          /* Human-readable description.  */
  std::string path;          /* The search path to use.  */
  std::string raw_path;      /* Pre-$~ (but post-default) expansion.  */
  std::string path_source;   /* Where the path started from.  */
  std::string override_path; /* From client environment variable.  */
  std::string client_path;   /* E.g., from dvips's config.ps.  */
  std::string cnf_path;      /* From texmf.cnf.  */
  std::string default_path;  /* If all else fails.  */
  string_vector suffix;      /* For kpse_find_file to check for/append.  */
};

/* The sole variable of that type, indexed by 'kpse_file_format_type'.
   Initialized by calls to 'kpse_find_file' for 'kpse_init_format'.  */
static kpse_format_info_type kpse_format_info;

/* And EXPAND_DEFAULT calls kpse_expand_default on try_path and the
   present info->path.  */
#define EXPAND_DEFAULT(try_path, source_string) \
  do \
    { \
      if (! try_path.empty ()) \
        { \
          info.raw_path = try_path;     \
          info.path = kpse_expand_default (try_path, info.path); \
          info.path_source = source_string;     \
        } \
    } \
  while (0)

static hash_table_type db; /* The hash table for all the ls-R's.  */

static hash_table_type alias_db;

static string_vector db_dir_list;

/* Return true if FILENAME could be in PATH_ELT, i.e., if the directory
   part of FILENAME matches PATH_ELT.  Have to consider // wildcards, but
   $ and ~ expansion have already been done.  */

static bool
match (const std::string& filename_arg, const std::string& path_elt_arg)
{
  const char *filename = filename_arg.c_str ();
  const char *path_elt = path_elt_arg.c_str ();

  const char *original_filename = filename;
  bool matched = false;

  for (; *filename && *path_elt; filename++, path_elt++)
    {
      if (*filename == *path_elt) /* normal character match */
        ;

      else if (IS_DIR_SEP (*path_elt)  /* at // */
               && original_filename < filename && IS_DIR_SEP (path_elt[-1]))
        {
          while (IS_DIR_SEP (*path_elt))
            path_elt++; /* get past second and any subsequent /'s */

          if (*path_elt == 0)
            {
              /* Trailing //, matches anything. We could make this
                 part of the other case, but it seems pointless to do
                 the extra work.  */
              matched = true;
              break;
            }
          else
            {
              /* Intermediate //, have to match rest of PATH_ELT.  */
              for (; !matched && *filename; filename++)
                {
                  /* Try matching at each possible character.  */
                  if (IS_DIR_SEP (filename[-1]) && *filename == *path_elt)
                    matched = match (filename, path_elt);
                }

              /* Prevent filename++ when *filename='\0'. */
              break;
            }
        }
      else
        /* normal character nonmatch, quit */
        break;
    }

  /* If we've reached the end of PATH_ELT, check that we're at the last
     component of FILENAME, we've matched.  */
  if (! matched && *path_elt == 0)
    {
      /* Probably PATH_ELT ended with 'vf' or some such, and FILENAME
         ends with 'vf/ptmr.vf'.  In that case, we'll be at a
         directory separator.  On the other hand, if PATH_ELT ended
         with a / (as in 'vf/'), FILENAME being the same 'vf/ptmr.vf',
         we'll be at the 'p'.  Upshot: if we're at a dir separator in
         FILENAME, skip it.  But if not, that's ok, as long as there
         are no more dir separators.  */

      if (IS_DIR_SEP (*filename))
        filename++;

      while (*filename && !IS_DIR_SEP (*filename))
        filename++;

      matched = *filename == 0;
    }

  return matched;
}

/* If DB_DIR is a prefix of PATH_ELT, return true; otherwise false.
   That is, the question is whether to try the db for a file looked up
   in PATH_ELT.  If PATH_ELT == ".", for example, the answer is no. If
   PATH_ELT == "/usr/local/lib/texmf/fonts//tfm", the answer is yes.

   In practice, ls-R is only needed for lengthy subdirectory
   comparisons, but there's no gain to checking PATH_ELT to see if it is
   a subdir match, since the only way to do that is to do a string
   search in it, which is all we do anyway.  */

static bool
elt_in_db (const std::string& db_dir, const std::string& path_elt)
{
  bool found = false;

  size_t db_dir_len = db_dir.length ();
  size_t path_elt_len = path_elt.length ();

  size_t i = 0;

  while (! found && db_dir[i] == path_elt[i])
    {
      i++;
      /* If we've matched the entire db directory, it's good.  */
      if (i == db_dir_len)
        found = true;

      /* If we've reached the end of PATH_ELT, but not the end of the db
         directory, it's no good.  */
      else if (i == path_elt_len)
        break;
    }

  return found;
}

/* Avoid doing anything if this PATH_ELT is irrelevant to the databases. */

/* Return list of matches for NAME in the ls-R file matching PATH_ELT.  If
   ALL is set, return (null-terminated list) of all matches, else just
   the first.  If no matches, return a pointer to an empty list.  If no
   databases can be read, or PATH_ELT is not in any of the databases,
   return NULL.  */

static string_vector
kpse_db_search (const std::string& name_arg,
                const std::string& orig_path_elt, bool all)
{
  bool done;
  string_vector ret;
  string_vector aliases;
  bool relevant = false;

  std::string name = name_arg;

  /* If we failed to build the database (or if this is the recursive
     call to build the db path), quit.  */
  if (! db.buckets)
    return ret;

  /* When tex-glyph.c calls us looking for, e.g., dpi600/cmr10.pk, we
     won't find it unless we change NAME to just 'cmr10.pk' and append
     '/dpi600' to PATH_ELT.  We are justified in using a literal '/'
     here, since that's what tex-glyph.c unconditionally uses in
     DPI_BITMAP_SPEC.  But don't do anything if the / begins NAME; that
     should never happen.  */
  std::string path_elt;
  size_t last_slash = name.rfind ('/');
  if (last_slash != std::string::npos && last_slash != 0)
    {
      std::string dir_part = name.substr (0, last_slash);
      name = name.substr (last_slash + 1);
    }
  else
    path_elt = orig_path_elt;

  /* Don't bother doing any lookups if this 'path_elt' isn't covered by
     any of database directories.  We do this not so much because the
     extra couple of hash lookups matter -- they don't -- but rather
     because we want to return NULL in this case, so path_search can
     know to do a disk search.  */
  for (int e = 0; ! relevant && e < db_dir_list.length (); e++)
    relevant = elt_in_db (db_dir_list[e], path_elt);

  if (! relevant)
    return ret;

  /* If we have aliases for this name, use them.  */
  if (alias_db.buckets)
    aliases = hash_lookup (alias_db, name);

  /* Push aliases up by one and insert the original name at the front.  */
  int len = aliases.length ();
  aliases.resize (len+1);
  for (int i = len; i > 0; i--)
    aliases[i] = aliases[i - 1];
  aliases[0] = name;

  done = false;
  len = aliases.length ();
  for (int i = 0; i < len && !done; i++)
    {
      std::string atry = aliases[i];

      /* We have an ls-R db.  Look up 'atry'.  */
      string_vector db_dirs = hash_lookup (db, atry);

      /* For each filename found, see if it matches the path element.  For
         example, if we have .../cx/cmr10.300pk and .../ricoh/cmr10.300pk,
         and the path looks like .../cx, we don't want the ricoh file.  */

      int db_dirs_len = db_dirs.length ();
      for (int j = 0; j < db_dirs_len && !done; j++)
        {
          std::string db_file = db_dirs[j] + atry;
          bool matched = match (db_file, path_elt);

#ifdef KPSE_DEBUG
          if (KPSE_DEBUG_P (KPSE_DEBUG_SEARCH))
            DEBUGF3 ("db:match (%s,%s) = %d\n",
                     db_file.c_str (), path_elt.c_str (), matched);
#endif

          /* We got a hit in the database.  Now see if the file actually
             exists, possibly under an alias.  */
          if (matched)
            {
              std::string found;
              std::string tmp = kpse_readable_file (db_file);
              if (! tmp.empty ())
                found = db_file;
              else
                {
                  /* The hit in the DB doesn't exist in disk.  Now try
                     all its aliases.  For example, suppose we have a
                     hierarchy on CD, thus 'mf.bas', but ls-R contains
                     'mf.base'.  Find it anyway.  Could probably work
                     around this with aliases, but this is pretty easy
                     and shouldn't hurt.  The upshot is that if one of
                     the aliases actually exists, we use that.  */

                  int aliases_len = aliases.length ();

                  for (int k = 1; k < aliases_len && found.empty (); k++)
                    {
                      std::string aatry = db_dirs[j] + aliases[k];
                      tmp = kpse_readable_file (aatry);
                      if (! tmp.empty ())
                        found = aatry;
                    }
                }

              /* If we have a real file, add it to the list, maybe done.  */
              if (! found.empty ())
                {
                  ret.append (found);

                  if (! (all || found.empty ()))
                    done = true;
                }
            }
        }
    }

  return ret;
}

/* Expand extra colons.  */

/* Check for leading colon first, then trailing, then doubled, since
   that is fastest.  Usually it will be leading or trailing.  */

/* Replace a leading or trailing or doubled : in PATH with DFLT.  If
   no extra colons, return PATH.  Only one extra colon is replaced.
   DFLT may not be NULL.  */

static std::string
kpse_expand_default (const std::string& path, const std::string& fallback)
{
  std::string expansion;

  size_t path_len = path.length ();

  if (path_len == 0)
    expansion = fallback;

  /* Solitary or leading :?  */
  else if (IS_ENV_SEP (path[0]))
    {
      expansion = path_len == 1 ? fallback : fallback + path;
    }

  /* Sorry about the assignment in the middle of the expression, but
     conventions were made to be flouted and all that.  I don't see the
     point of calling strlen twice or complicating the logic just to
     avoid the assignment (especially now that I've pointed it out at
     such great length).  */
  else if (IS_ENV_SEP (path[path_len-1]))
    expansion = path + fallback;

  /* OK, not leading or trailing.  Check for doubled.  */
  else
    {
      /* What we'll return if we find none.  */
      expansion = path;

      for (size_t i = 0; i < path_len; i++)
        {
          if (i + 1 < path_len
              && IS_ENV_SEP (path[i]) && IS_ENV_SEP (path[i+1]))
            {
              /* We have a doubled colon.  */

              /* Copy stuff up to and including the first colon.  */
              /* Copy in FALLBACK, and then the rest of PATH.  */
              expansion = path.substr (0, i+1) + fallback + path.substr (i+1);

              break;
            }
        }
    }

  return expansion;
}

/* Translate a path element to its corresponding director{y,ies}.  */

/* To avoid giving prototypes for all the routines and then their real
   definitions, we give all the subroutines first.  The entry point is
   the last routine in the file.  */

/* Make a copy of DIR (unless it's null) and save it in L.  Ensure that
   DIR ends with a DIR_SEP for the benefit of later searches.  */

static void
dir_list_add (str_llist_type *l, const std::string& dir)
{
  char last_char = dir[dir.length () - 1];

  std::string saved_dir = dir;

  if (! (IS_DIR_SEP (last_char) || IS_DEVICE_SEP (last_char)))
    saved_dir += DIR_SEP_STRING;

  str_llist_add (l, saved_dir);
}

/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

static bool
dir_p (const std::string& fn)
{
#ifdef WIN32
  unsigned int fa = GetFileAttributes (fn.c_str ());
  return (fa != 0xFFFFFFFF && (fa & FILE_ATTRIBUTE_DIRECTORY));
#else
  struct stat stats;
  return stat (fn.c_str (), &stats) == 0 && S_ISDIR (stats.st_mode);
#endif
}

/* If DIR is a directory, add it to the list L.  */

static void
checked_dir_list_add (str_llist_type *l, const std::string& dir)
{
  if (dir_p (dir))
    dir_list_add (l, dir);
}

/* The cache.  Typically, several paths have the same element; for
   example, /usr/local/lib/texmf/fonts//.  We don't want to compute the
   expansion of such a thing more than once.  Even though we also cache
   the dir_links call, that's not enough -- without this path element
   caching as well, the execution time doubles.  */

struct cache_entry
{
  cache_entry (void) : key (), value (0) { }

  ~cache_entry (void) { }

  std::string key;
  str_llist_type *value;
};

static cache_entry *the_cache = 0;
static unsigned cache_length = 0;

/* Associate KEY with VALUE.  We implement the cache as a simple linear
   list, since it's unlikely to ever be more than a dozen or so elements
   long.  We don't bother to check here if PATH has already been saved;
   we always add it to our list.  We copy KEY but not VALUE; not sure
   that's right, but it seems to be all that's needed.  */

static void
cache (const std::string key, str_llist_type *value)
{
  cache_entry *new_cache = new cache_entry [cache_length+1];

  for (unsigned i = 0; i < cache_length; i++)
    {
      new_cache[i].key = the_cache[i].key;
      new_cache[i].value = the_cache[i].value;
    }

  delete [] the_cache;

  the_cache = new_cache;

  the_cache[cache_length].key = key;
  the_cache[cache_length].value = value;

  cache_length++;
}

/* To retrieve, just check the list in order.  */

static str_llist_type *
cached (const std::string& key)
{
  unsigned p;

  for (p = 0; p < cache_length; p++)
    {
      if (key == the_cache[p].key)
        return the_cache[p].value;
    }

  return 0;
}

/* Handle the magic path constructs.  */

/* Declare recursively called routine.  */
static void expand_elt (str_llist_type *, const std::string&, unsigned);

/* POST is a pointer into the original element (which may no longer be
   ELT) to just after the doubled DIR_SEP, perhaps to the null.  Append
   subdirectories of ELT (up to ELT_LENGTH, which must be a /) to
   STR_LIST_PTR.  */

#ifdef WIN32

/* Shared across recursive calls, it acts like a stack. */
static std::string dirname;

#else /* WIN32 */

/* Return -1 if FN isn't a directory, else its number of links.
   Duplicate the call to stat; no need to incur overhead of a function
   call for that little bit of cleanliness. */

static int
dir_links (const std::string& fn)
{
  std::map<std::string, long> link_table;

  long ret;

  if (link_table.find (fn) != link_table.end ())
    ret = link_table[fn];
  else
    {
      struct stat stats;

      ret = stat (fn.c_str (), &stats) == 0 && S_ISDIR (stats.st_mode)
            ? stats.st_nlink : static_cast<unsigned> (-1);

      link_table[fn] = ret;

#ifdef KPSE_DEBUG
      if (KPSE_DEBUG_P (KPSE_DEBUG_STAT))
        DEBUGF2 ("dir_links (%s) => %ld\n", fn.c_str (), ret);
#endif
    }

  return ret;
}

#endif /* WIN32 */

static void
do_subdir (str_llist_type *str_list_ptr, const std::string& elt,
           unsigned elt_length, const std::string& post)
{
#ifdef WIN32
  WIN32_FIND_DATA find_file_data;
  HANDLE hnd;
  int proceed;
#else
  DIR *dir;
  struct dirent *e;
#endif /* not WIN32 */

  std::string name = elt.substr (0, elt_length);

  assert (IS_DIR_SEP (elt[elt_length - 1])
          || IS_DEVICE_SEP (elt[elt_length - 1]));

#if defined (WIN32)

  dirname = name + "/*.*";         /* "*.*" or "*" -- seems equivalent. */

  hnd = FindFirstFile (dirname.c_str (), &find_file_data);

  if (hnd == INVALID_HANDLE_VALUE)
    return;

  /* Include top level before subdirectories, if nothing to match.  */
  if (post.empty ())
    dir_list_add (str_list_ptr, name);
  else
    {
      /* If we do have something to match, see if it exists.  For
         example, POST might be 'pk/ljfour', and they might have a
         directory '$TEXMF/fonts/pk/ljfour' that we should find.  */
      name += post;
      expand_elt (str_list_ptr, name, elt_length);
      name.resize (elt_length);
    }

  proceed = 1;

  while (proceed)
    {
      if (find_file_data.cFileName[0] != '.')
        {
          /* Construct the potential subdirectory name.  */
          name += find_file_data.cFileName;

          if (find_file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            {
              /* It's a directory, so append the separator.  */
              name += DIR_SEP_STRING;
              unsigned potential_len = name.length ();

              do_subdir (str_list_ptr, name, potential_len, post);
            }
          name.resize (elt_length);
        }

      proceed = FindNextFile (hnd, &find_file_data);
    }

  FindClose (hnd);

#else /* not WIN32 */

  /* If we can't open it, quit.  */
  dir = gnulib::opendir (name.c_str ());

  if (! dir)
    return;

  /* Include top level before subdirectories, if nothing to match.  */
  if (post.empty ())
    dir_list_add (str_list_ptr, name);
  else
    {
      /* If we do have something to match, see if it exists.  For
         example, POST might be 'pk/ljfour', and they might have a
         directory '$TEXMF/fonts/pk/ljfour' that we should find.  */
      name += post;
      expand_elt (str_list_ptr, name, elt_length);
      name.resize (elt_length);
    }

  while ((e = gnulib::readdir (dir)))
    {
      /* If it begins with a '.', never mind.  (This allows "hidden"
         directories that the algorithm won't find.)  */

      if (e->d_name[0] != '.')
        {
          int links;

          /* Construct the potential subdirectory name.  */
          name += e->d_name;

          /* If we can't stat it, or if it isn't a directory, continue.  */
          links = dir_links (name);

          if (links >= 0)
            {
              /* It's a directory, so append the separator.  */
              name += DIR_SEP_STRING;
              unsigned potential_len = name.length ();

              /* Should we recurse?  To see if the subdirectory is a
                 leaf, check if it has two links (one for . and one for
                 ..).  This means that symbolic links to directories do
                 not affect the leaf-ness.  This is arguably wrong, but
                 the only alternative I know of is to stat every entry
                 in the directory, and that is unacceptably slow.

                 The #ifdef here makes all this configurable at
                 compile-time, so that if we're using VMS directories or
                 some such, we can still find subdirectories, even if it
                 is much slower.  */
#ifdef ST_NLINK_TRICK
              if (links != 2)
#endif /* not ST_NLINK_TRICK */
                /* All criteria are met; find subdirectories.  */
                do_subdir (str_list_ptr, name, potential_len, post);
#ifdef ST_NLINK_TRICK
              else if (post.empty ())
                /* Nothing to match, no recursive subdirectories to
                   look for: we're done with this branch.  Add it.  */
                dir_list_add (str_list_ptr, name);
#endif
            }

          /* Remove the directory entry we just checked from 'name'.  */
          name.resize (elt_length);
        }
    }

  xclosedir (dir);
#endif /* not WIN32 */
}

/* Assume ELT is non-empty and non-NULL.  Return list of corresponding
   directories (with no terminating NULL entry) in STR_LIST_PTR.  Start
   looking for magic constructs at START.  */

static void
expand_elt (str_llist_type *str_list_ptr, const std::string& elt,
            unsigned /* start */)
{
#if 0
  // We don't want magic constructs.

  size_t elt_len = elt.length ();

  size_t dir = start;


  while (dir < elt_len)
    {
      if (IS_DIR_SEP (elt[dir]))
        {
          /* If two or more consecutive /'s, find subdirectories.  */
          if (++dir < elt_len && IS_DIR_SEP (elt[dir]))
            {
              size_t i = dir;
              while (i < elt_len && IS_DIR_SEP (elt[i]))
                i++;

              std::string post = elt.substr (i);

              do_subdir (str_list_ptr, elt, dir, post);

              return;
            }

          /* No special stuff at this slash.  Keep going.  */
        }
      else
        dir++;
    }
#endif

  /* When we reach the end of ELT, it will be a normal filename.  */
  checked_dir_list_add (str_list_ptr, elt);
}

/* Here is the entry point.  Returns directory list for ELT.  */

/* Given a path element ELT, return a pointer to a NULL-terminated list
   of the corresponding (existing) directory or directories, with
   trailing slashes, or NULL.  If ELT is the empty string, check the
   current working directory.

   It's up to the caller to expand ELT.  This is because this routine is
   most likely only useful to be called from 'kpse_path_search', which
   has already assumed expansion has been done.  */

static str_llist_type *
kpse_element_dirs (const std::string& elt)
{
  str_llist_type *ret;

  /* If given nothing, return nothing.  */
  if (elt.empty ())
    return 0;

  /* If we've already cached the answer for ELT, return it.  */
  ret = cached (elt);
  if (ret)
    return ret;

  /* We're going to have a real directory list to return.  */
  ret = new str_llist_type;
  *ret = 0;

  /* We handle the hard case in a subroutine.  */
  expand_elt (ret, elt, 0);

  /* Remember the directory list we just found, in case future calls are
     made with the same ELT.  */
  cache (elt, ret);

#ifdef KPSE_DEBUG
  if (KPSE_DEBUG_P (KPSE_DEBUG_EXPAND))
    {
      DEBUGF1 ("path element %s =>", elt.c_str ());
      if (ret)
        {
          str_llist_elt_type *e;
          for (e = *ret; e; e = STR_LLIST_NEXT (*e))
            gnulib::fprintf (stderr, " %s", (STR_LLIST (*e)).c_str ());
        }
      gnulib::putc ('\n', stderr);
      gnulib::fflush (stderr);
    }
#endif /* KPSE_DEBUG */

  return ret;
}

#ifndef WIN32
void
xclosedir (DIR *d)
{
  int ret = gnulib::closedir (d);

  if (ret != 0)
    FATAL ("closedir failed");
}
#endif

/* Implementation of a linked list of strings.  */

/* Add the new string STR to the end of the list L.  */

static void
str_llist_add (str_llist_type *l, const std::string& str)
{
  str_llist_elt_type *e;
  str_llist_elt_type *new_elt = new str_llist_elt_type;

  /* The new element will be at the end of the list.  */
  STR_LLIST (*new_elt) = str;
  STR_LLIST_MOVED (*new_elt) = 0;
  STR_LLIST_NEXT (*new_elt) = 0;

  /* Find the current end of the list.  */
  for (e = *l; e && STR_LLIST_NEXT (*e); e = STR_LLIST_NEXT (*e))
    ;

  if (! e)
    *l = new_elt;
  else
    STR_LLIST_NEXT (*e) = new_elt;
}

/* Move an element towards the top. The idea is that when a file is
   found in a given directory, later files will likely be in that same
   directory, and looking for the file in all the directories in between
   is thus a waste.  */

static void
str_llist_float (str_llist_type *l, str_llist_elt_type *mover)
{
  str_llist_elt_type *last_moved, *unmoved;

  /* If we've already moved this element, never mind.  */
  if (STR_LLIST_MOVED (*mover))
    return;

  /* Find the first unmoved element (to insert before).  We're
     guaranteed this will terminate, since MOVER itself is currently
     unmoved, and it must be in L (by hypothesis).  */
  for (last_moved = 0, unmoved = *l; STR_LLIST_MOVED (*unmoved);
       last_moved = unmoved, unmoved = STR_LLIST_NEXT (*unmoved))
    ;

  /* If we are the first unmoved element, nothing to relink.  */
  if (unmoved != mover)
    {
      /* Remember 'mover's current successor, so we can relink 'mover's
         predecessor to it.  */
      str_llist_elt_type *before_mover;
      str_llist_elt_type *after_mover = STR_LLIST_NEXT (*mover);

      /* Find 'mover's predecessor.  */
      for (before_mover = unmoved; STR_LLIST_NEXT (*before_mover) != mover;
           before_mover = STR_LLIST_NEXT (*before_mover))
        ;

      /* 'before_mover' now links to 'after_mover'.  */
      STR_LLIST_NEXT (*before_mover) = after_mover;

      /* Insert 'mover' before 'unmoved' and after 'last_moved' (or at
         the head of the list).  */
      STR_LLIST_NEXT (*mover) = unmoved;
      if (! last_moved)
        *l = mover;
      else
        STR_LLIST_NEXT (*last_moved) = mover;
    }

  /* We've moved it.  */
  STR_LLIST_MOVED (*mover) = 1;
}

/* Variable expansion.  */

/* We have to keep track of variables being expanded, otherwise
   constructs like TEXINPUTS = $TEXINPUTS result in an infinite loop.
   (Or indirectly recursive variables, etc.)  Our simple solution is to
   add to a list each time an expansion is started, and check the list
   before expanding.  */

static std::map <std::string, bool> expansions;

static void
expanding (const std::string& var, bool xp)
{
  expansions[var] = xp;
}

/* Return whether VAR is currently being expanding.  */

static bool
expanding_p (const std::string& var)
{
  return (expansions.find (var) != expansions.end ()) ? expansions[var] : false;
}

/* Append the result of value of 'var' to EXPANSION, where 'var' begins
   at START and ends at END.  If 'var' is not set, do not complain.
   This is a subroutine for the more complicated expansion function.  */

static void
expand (std::string &expansion, const std::string& var)
{
  if (expanding_p (var))
    {
      (*current_liboctave_warning_with_id_handler)
        ("Octave:pathsearch-syntax",
         "kpathsea: variable '%s' references itself (eventually)",
         var.c_str ());
    }
  else
    {
      /* Check for an environment variable.  */
      std::string value = octave_env::getenv (var);

      if (! value.empty ())
        {
          expanding (var, true);
          std::string tmp = kpse_var_expand (value);
          expanding (var, false);
          expansion += tmp;
        }
    }
}

/* Can't think of when it would be useful to change these (and the
   diagnostic messages assume them), but ... */
#ifndef IS_VAR_START /* starts all variable references */
#define IS_VAR_START(c) ((c) == '$')
#endif
#ifndef IS_VAR_CHAR  /* variable name constituent */
#define IS_VAR_CHAR(c) (isalnum (c) || (c) == '_')
#endif
#ifndef IS_VAR_BEGIN_DELIMITER /* start delimited variable name (after $) */
#define IS_VAR_BEGIN_DELIMITER(c) ((c) == '{')
#endif
#ifndef IS_VAR_END_DELIMITER
#define IS_VAR_END_DELIMITER(c) ((c) == '}')
#endif

/* Maybe we should support some or all of the various shell ${...}
   constructs, especially ${var-value}.  */

static std::string
kpse_var_expand (const std::string& src)
{
  std::string expansion;

  size_t src_len = src.length ();

  /* Copy everything but variable constructs.  */
  for (size_t i = 0; i < src_len; i++)
    {
      if (IS_VAR_START (src[i]))
        {
          i++;

          /* Three cases: '$VAR', '${VAR}', '$<anything-else>'.  */
          if (IS_VAR_CHAR (src[i]))
            {
              /* $V: collect name constituents, then expand.  */
              size_t var_end = i;

              do
                {
                  var_end++;
                }
              while (IS_VAR_CHAR (src[var_end]));

              var_end--; /* had to go one past */
              expand (expansion, src.substr (i, var_end - i + 1));
              i = var_end;

            }
          else if (IS_VAR_BEGIN_DELIMITER (src[i]))
            {
              /* ${: scan ahead for matching delimiter, then expand.  */
              size_t var_end = ++i;

              while (var_end < src_len && !IS_VAR_END_DELIMITER (src[var_end]))
                var_end++;

              if (var_end == src_len)
                {
                  (*current_liboctave_warning_with_id_handler)
                    ("Octave:pathsearch-syntax",
                     "%s: No matching } for ${", src.c_str ());
                  i = var_end - 1; /* will incr to eos at top of loop */
                }
              else
                {
                  expand (expansion, src.substr (i, var_end - i));
                  i = var_end; /* will incr past } at top of loop*/
                }
            }
          else
            {
              /* $<something-else>: error.  */
              (*current_liboctave_warning_with_id_handler)
                ("Octave:pathsearch-syntax",
                 "%s: Unrecognized variable construct '$%c'",
                 src.c_str (), src[i]);

              /* Just ignore those chars and keep going.  */
            }
        }
      else
        expansion += src[i];
    }

  return expansion;
}
