// %NO_EDIT_WARNING%
/*

Copyright (C) 2008-2015 Michael Goffioul

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

#include <string>
#include <cstring>
#include <map>
#include <list>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>

#include <unistd.h>

// This mess suggested by the autoconf manual.

#include <sys/types.h>

#if defined HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifndef WEXITSTATUS
#define WEXITSTATUS(stat_val) (static_cast<unsigned> (stat_val) >> 8)
#endif

static std::map<std::string, std::string> vars;

#ifndef OCTAVE_VERSION
#define OCTAVE_VERSION %OCTAVE_CONF_VERSION%
#endif

#ifndef OCTAVE_PREFIX
#define OCTAVE_PREFIX %OCTAVE_CONF_PREFIX%
#endif

#include "shared-fcns.h"

static std::string
get_line (FILE *fp)
{
  static std::vector<char> buf (100);
  unsigned int idx = 0;
  int c;

  while (true)
    {
      c = gnulib::fgetc (fp);
      if (c == '\n' || c == EOF)
        break;
      if (buf.size () <= idx)
        buf.resize (buf.size () + 100);
      buf[idx++] = c;
    }
  if (idx == 0)
    return std::string ("");
  else
    return std::string (&buf[0], idx);
}


static std::string
get_variable (const char *name, const std::string& defval)
{
  const char *val = getenv (name);
  if (val && *val)
    return std::string (val);
  else
    return defval;
}

static std::string
quote_path (const std::string& s)
{
  if (s.find (' ') != std::string::npos && s[0] != '"')
    return "\"" + s + "\"";
  else
    return s;
}

static void
initialize (void)
{
  vars["OCTAVE_HOME"] = get_octave_home ();
  vars["OCTAVE_PREFIX"] = OCTAVE_PREFIX;

  vars["SED"] = get_variable ("SED", %OCTAVE_CONF_SED%);

  std::string DEFAULT_OCTINCLUDEDIR = %OCTAVE_CONF_OCTINCLUDEDIR%;
  std::string DEFAULT_INCLUDEDIR = %OCTAVE_CONF_INCLUDEDIR%;
  std::string DEFAULT_LIBDIR = %OCTAVE_CONF_LIBDIR%;
  std::string DEFAULT_OCTLIBDIR = %OCTAVE_CONF_OCTLIBDIR%;

  DEFAULT_OCTINCLUDEDIR = subst_octave_home (DEFAULT_OCTINCLUDEDIR);
  DEFAULT_INCLUDEDIR = subst_octave_home (DEFAULT_INCLUDEDIR);
  DEFAULT_LIBDIR = subst_octave_home (DEFAULT_LIBDIR);
  DEFAULT_OCTLIBDIR = subst_octave_home (DEFAULT_OCTLIBDIR);

  vars["OCTINCLUDEDIR"] = get_variable ("OCTINCLUDEDIR", DEFAULT_OCTINCLUDEDIR);
  vars["INCLUDEDIR"] = get_variable ("INCLUDEDIR", DEFAULT_INCLUDEDIR);
  vars["LIBDIR"] = get_variable ("LIBDIR", DEFAULT_LIBDIR);
  vars["OCTLIBDIR"] = get_variable ("OCTLIBDIR", DEFAULT_OCTLIBDIR);

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
  std::string DEFAULT_INCFLAGS
    = "-I" + quote_path (vars["OCTINCLUDEDIR"] + "\\..")
      + " -I" + quote_path (vars["OCTINCLUDEDIR"]);
#else
  std::string DEFAULT_INCFLAGS
    = "-I" + quote_path (vars["OCTINCLUDEDIR"] + "/..")
      + " -I" + quote_path (vars["OCTINCLUDEDIR"]);
#endif
  if (vars["INCLUDEDIR"] != "/usr/include")
    DEFAULT_INCFLAGS += " -I" + quote_path (vars["INCLUDEDIR"]);

  std::string DEFAULT_LFLAGS = "-L" + quote_path (vars["OCTLIBDIR"]);
  if (vars["LIBDIR"] != "/usr/lib")
    DEFAULT_LFLAGS += " -L" + quote_path (vars["LIBDIR"]);

  vars["CPPFLAGS"] = get_variable ("CPPFLAGS", %OCTAVE_CONF_CPPFLAGS%);
  vars["INCFLAGS"] = get_variable ("INCFLAGS", DEFAULT_INCFLAGS);
  vars["F77"] = get_variable ("F77", %OCTAVE_CONF_MKOCTFILE_F77%);
  vars["FFLAGS"] = get_variable ("FFLAGS", %OCTAVE_CONF_FFLAGS%);
  vars["FPICFLAG"] = get_variable ("FPICFLAG", %OCTAVE_CONF_FPICFLAG%);
  vars["CC"] = get_variable ("CC", %OCTAVE_CONF_MKOCTFILE_CC%);
  vars["CFLAGS"] = get_variable ("CFLAGS", %OCTAVE_CONF_CFLAGS%);
  vars["CPICFLAG"] = get_variable ("CPICFLAG", %OCTAVE_CONF_CPICFLAG%);
  vars["CXX"] = get_variable ("CXX", %OCTAVE_CONF_MKOCTFILE_CXX%);
  vars["CXXFLAGS"] = get_variable ("CXXFLAGS", %OCTAVE_CONF_CXXFLAGS%);
  vars["CXXPICFLAG"] = get_variable ("CXXPICFLAG", %OCTAVE_CONF_CXXPICFLAG%);
  vars["XTRA_CFLAGS"] = get_variable ("XTRA_CFLAGS", %OCTAVE_CONF_XTRA_CFLAGS%);
  vars["XTRA_CXXFLAGS"] = get_variable ("XTRA_CXXFLAGS",
                                        %OCTAVE_CONF_XTRA_CXXFLAGS%);

  vars["AR"] = get_variable ("AR", %OCTAVE_CONF_MKOCTFILE_AR%);
  vars["RANLIB"] = get_variable ("RANLIB", %OCTAVE_CONF_MKOCTFILE_RANLIB%);

  vars["DEPEND_FLAGS"] = get_variable ("DEPEND_FLAGS",
                                       %OCTAVE_CONF_DEPEND_FLAGS%);
  vars["DEPEND_EXTRA_SED_PATTERN"] = get_variable ("DEPEND_EXTRA_SED_PATTERN",
                                                   %OCTAVE_CONF_DEPEND_EXTRA_SED_PATTERN%);

  vars["DL_LD"] = get_variable ("DL_LD", %OCTAVE_CONF_MKOCTFILE_DL_LD%);
  vars["DL_LDFLAGS"] = get_variable ("DL_LDFLAGS",
                                     %OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%);

  vars["RDYNAMIC_FLAG"] = get_variable ("RDYNAMIC_FLAG",
                                        %OCTAVE_CONF_RDYNAMIC_FLAG%);
  vars["LIBOCTAVE"] = "-loctave";
  vars["LIBOCTINTERP"] = "-loctinterp";
  vars["READLINE_LIBS"] = "-lreadline";
  vars["LAPACK_LIBS"] = get_variable ("LAPACK_LIBS", %OCTAVE_CONF_LAPACK_LIBS%);
  vars["BLAS_LIBS"] = get_variable ("BLAS_LIBS", %OCTAVE_CONF_BLAS_LIBS%);
  vars["FFTW3_LDFLAGS"] = get_variable ("FFTW3_LDFLAGS",
                                        %OCTAVE_CONF_FFTW3_LDFLAGS%);
  vars["FFTW3_LIBS"] = get_variable ("FFTW3_LIBS", %OCTAVE_CONF_FFTW3_LIBS%);
  vars["FFTW3F_LDFLAGS"] = get_variable ("FFTW3F_LDFLAGS",
                                         %OCTAVE_CONF_FFTW3F_LDFLAGS%);
  vars["FFTW3F_LIBS"] = get_variable ("FFTW3F_LIBS", %OCTAVE_CONF_FFTW3F_LIBS%);
  vars["LIBS"] = get_variable ("LIBS", %OCTAVE_CONF_LIBS%);
  vars["FLIBS"] = get_variable ("FLIBS", %OCTAVE_CONF_FLIBS%);
  vars["OCTAVE_LINK_DEPS"] = get_variable ("OCTAVE_LINK_DEPS",
                                           %OCTAVE_CONF_OCTAVE_LINK_DEPS%);
  vars["OCTAVE_LINK_OPTS"] = get_variable ("OCTAVE_LINK_OPTS",
                                           %OCTAVE_CONF_OCTAVE_LINK_OPTS%);
  vars["OCT_LINK_DEPS"] = get_variable ("OCT_LINK_DEPS",
                                        %OCTAVE_CONF_OCT_LINK_DEPS%);
  vars["OCT_LINK_OPTS"] = get_variable ("OCT_LINK_OPTS",
                                        %OCTAVE_CONF_OCT_LINK_OPTS%);
  vars["LD_CXX"] = get_variable ("LD_CXX", %OCTAVE_CONF_MKOCTFILE_LD_CXX%);
  vars["LDFLAGS"] = get_variable ("LDFLAGS", %OCTAVE_CONF_LDFLAGS%);
  vars["LD_STATIC_FLAG"] = get_variable ("LD_STATIC_FLAG",
                                         %OCTAVE_CONF_LD_STATIC_FLAG%);
  vars["LFLAGS"] = get_variable ("LFLAGS", DEFAULT_LFLAGS);
  vars["F77_INTEGER8_FLAG"] = get_variable ("F77_INTEGER8_FLAG",
                                            %OCTAVE_CONF_F77_INTEGER_8_FLAG%);

  vars["ALL_FFLAGS"] = vars["FFLAGS"] + " " + vars["F77_INTEGER8_FLAG"];

  vars["ALL_CFLAGS"] = vars["INCFLAGS"] + " " + vars["XTRA_CFLAGS"] + " "
                       + vars["CFLAGS"];

  vars["ALL_CXXFLAGS"] = vars["INCFLAGS"] + " " + vars["XTRA_CXXFLAGS"] + " "
                         + vars["CXXFLAGS"];

  vars["ALL_LDFLAGS"] = vars["LD_STATIC_FLAG"] + " " + vars["CPICFLAG"] + " "
                        + vars["LDFLAGS"];

  vars["OCTAVE_LIBS"] = vars["LIBOCTINTERP"] + " " + vars["LIBOCTAVE"] + " "
                        + vars["SPECIAL_MATH_LIB"];

  vars["FFTW_LIBS"] = vars["FFTW3_LDFLAGS"] + " " + vars["FFTW3_LIBS"] + " "
                      + vars["FFTW3F_LDFLAGS"] + " " + vars["FFTW3F_LIBS"];
}

static std::string usage_msg = "usage: mkoctfile [options] file ...";
static std::string version_msg = "mkoctfile, version " OCTAVE_VERSION;
static bool debug = false;
static std::string help_msg =
"\n"
"Options:\n"
"\n"
"  -h, -?, --help          Print this message.\n"
"\n"
"  -IDIR                   Add -IDIR to compile commands.\n"
"\n"
"  -idirafter DIR          Add -idirafter DIR to compile commands.\n"
"\n"
"  -DDEF                   Add -DDEF to compile commands.\n"
"\n"
"  -lLIB                   Add library LIB to link command.\n"
"\n"
"  -LDIR                   Add -LDIR to link command.\n"
"\n"
"  -M, --depend            Generate dependency files (.d) for C and C++\n"
"                          source files.\n"
#if ! defined (__WIN32__) || defined (_POSIX_VERSION)
"\n"
"  -pthread                Add -pthread to link command.\n"
#endif
"\n"
"  -RDIR                   Add -RDIR to link command.\n"
"\n"
"  -Wl,...                 Pass flags though the linker like -Wl,-rpath=...\n"
"\n"
"  -W...                   Pass flags though the compiler like -Wa,OPTION.\n"
"\n"
"  -c, --compile           Compile, but do not link.\n"
"\n"
"  -o FILE, --output FILE  Output file name.  Default extension is .oct\n"
"                          (or .mex if --mex is specified) unless linking\n"
"                          a stand-alone executable.\n"
"\n"
"  -g                      Enable debugging options for compilers.\n"
"\n"
"  -p VAR, --print VAR     Print configuration variable VAR.  Recognized\n"
"                          variables are:\n"
"\n"
"                            ALL_CFLAGS                  INCFLAGS\n"
"                            ALL_CXXFLAGS                INCLUDEDIR\n"
"                            ALL_FFLAGS                  LAPACK_LIBS\n"
"                            ALL_LDFLAGS                 LD_CXX\n"
"                            AR                          LDFLAGS\n"
"                            BLAS_LIBS                   LD_STATIC_FLAG\n"
"                            CC                          LFLAGS\n"
"                            CFLAGS                      LIBDIR\n"
"                            CPICFLAG                    LIBOCTAVE\n"
"                            CPPFLAGS                    LIBOCTINTERP\n"
"                            CXX                         LIBS\n"
"                            CXXFLAGS                    OCTAVE_HOME\n"
"                            CXXPICFLAG                  OCTAVE_LIBS\n"
"                            DEPEND_EXTRA_SED_PATTERN    OCTAVE_LINK_DEPS\n"
"                            DEPEND_FLAGS                OCTAVE_LINK_OPTS\n"
"                            DL_LD                       OCTAVE_PREFIX\n"
"                            DL_LDFLAGS                  OCTINCLUDEDIR\n"
"                            F77                         OCTLIBDIR\n"
"                            F77_INTEGER8_FLAG           OCT_LINK_DEPS\n"
"                            FFLAGS                      OCT_LINK_OPTS\n"
"                            FFTW3F_LDFLAGS              RANLIB\n"
"                            FFTW3F_LIBS                 RDYNAMIC_FLAG\n"
"                            FFTW3_LDFLAGS               READLINE_LIBS\n"
"                            FFTW3_LIBS                  SED\n"
"                            FFTW_LIBS                   SPECIAL_MATH_LIB\n"
"                            FLIBS                       XTRA_CFLAGS\n"
"                            FPICFLAG                    XTRA_CXXFLAGS\n"
"\n"
"  --link-stand-alone      Link a stand-alone executable file.\n"
"\n"
"  --mex                   Assume we are creating a MEX file.  Set the\n"
"                          default output extension to \".mex\".\n"
"\n"
"  -s, --strip             Strip output file.\n"
"\n"
"  -v, --verbose           Echo commands as they are executed.\n"
"\n"
"  FILE                    Compile or link FILE.  Recognized file types are:\n"
"\n"
"                            .c    C source\n"
"                            .cc   C++ source\n"
"                            .C    C++ source\n"
"                            .cpp  C++ source\n"
"                            .f    Fortran source (fixed form)\n"
"                            .F    Fortran source (fixed form)\n"
"                            .f90  Fortran source (free form)\n"
"                            .F90  Fortran source (free form)\n"
"                            .o    object file\n"
"                            .a    library file\n"
#ifdef _MSC_VER
"                            .lib  library file\n"
#endif
"\n";

static std::string
basename (const std::string& s, bool strip_path = false)
{
  std::string retval;
  size_t pos = s.rfind ('.');

  if (pos == std::string::npos)
    retval = s;
  else
    retval = s.substr (0, pos);

  if (strip_path)
    {
      size_t p1 = retval.rfind ('/'), p2 = retval.rfind ('\\');
      pos = (p1 != std::string::npos && p2 != std::string::npos
             ? std::max (p1, p2) : (p2 != std::string::npos ? p2 : p1));
      if (pos != std::string::npos)
        retval = retval.substr (++pos, std::string::npos);
    }

  return retval;
}

inline bool
starts_with (const std::string& s, const std::string& prefix)
{
  return (s.length () >= prefix.length () && s.find (prefix) == 0);
}

inline bool
ends_with (const std::string& s, const std::string& suffix)
{
  return (s.length () >= suffix.length ()
          && s.rfind (suffix) == s.length () - suffix.length ());
}

static int
run_command (const std::string& cmd)
{
  if (debug)
    std::cout << cmd << std::endl;

  int result = system (cmd.c_str ());

  if (WIFEXITED (result))
    result = WEXITSTATUS (result);

  return result;
}

bool
is_true (const std::string& s)
{
  return (s == "yes" || s == "true");
}

int
main (int argc, char **argv)
{
  initialize ();

  std::string file, output_option;
  std::list<std::string> cfiles, ccfiles, f77files;
  int result = 0;

  std::string objfiles = "";
  std::string libfiles = "";
  std::string octfile = "";
  std::string outputfile = "";
  std::string incflags = "";
  std::string defs = "";
  std::string ldflags = "";
  std::string pass_on_options = "";
  bool strip = false;
  bool no_oct_file_strip_on_this_platform = is_true ("%NO_OCT_FILE_STRIP%");
  bool link = true;
  bool link_stand_alone = false;
  std::string output_ext = ".oct";
  bool depend = false;

  if (argc == 1)
    {
      std::cout << usage_msg << std::endl;
      return 1;
    }

  if (argc == 2 && (!strcmp (argv[1], "-v")
                    || !strcmp (argv[1], "-version")
                    || !strcmp (argv[1], "--version")))
    {
      std::cout << version_msg << std::endl;
      return 0;
    }

  for (int i = 1; i < argc; i++)
    {
      std::string arg = argv[i];

      if (ends_with (arg, ".c"))
        {
          file = arg;
          cfiles.push_back (file);
        }
      else if (ends_with (arg, ".cc") || ends_with (arg, ".C")
               || ends_with (arg, ".cpp"))
        {
          file = arg;
          ccfiles.push_back (file);
        }
      else if (ends_with (arg, ".f") || ends_with (arg, ".F")
               || ends_with (arg, "f90") || ends_with (arg, ".F90"))
        {
          file = arg;
          f77files.push_back (file);
        }
      else if (ends_with (arg, ".o") || ends_with (arg, ".obj"))
        {
          file = arg;
          objfiles += (" " + quote_path (arg));
        }
      else if (ends_with (arg, ".lib") || ends_with (arg, ".a"))
        {
          file = arg;
          libfiles += (" " + quote_path (arg));
        }
      else if (arg == "-d" || arg == "-debug" || arg == "--debug"
               || arg == "-v" || arg == "-verbose" ||  arg == "--verbose")
        {
          debug = true;
          if (vars["CC"] == "cc-msvc")
            vars["CC"] += " -d";
          if (vars["CXX"] == "cc-msvc")
            vars["CXX"] += " -d";
          if (vars["DL_LD"] == "cc-msvc")
            vars["DL_LD"] += " -d";
        }
      else if (arg == "-h" || arg == "-?" || arg == "-help" || arg == "--help")
        {
          std::cout << usage_msg << std::endl;
          std::cout << help_msg << std::endl;
          return 0;
        }
      else if (starts_with (arg, "-I"))
        {
          incflags += (" " + quote_path (arg));
        }
      else if (arg == "-idirafter")
        {
          if (i < argc-1)
            {
              arg = argv[++i];
              incflags += (" -idirafter " + arg);
            }
          else
            std::cerr << "mkoctfile: include directory name missing"
                      << std::endl;
        }
      else if (starts_with (arg, "-D"))
        {
          defs += (" " + arg);
        }
      else if (starts_with (arg, "-Wl,") || starts_with (arg, "-l")
               || starts_with (arg, "-L") || starts_with (arg, "-R"))
        {
          ldflags += (" " + arg);
        }
#if ! defined (__WIN32__) || defined (_POSIX_VERSION)
      else if (arg == "-pthread")
        {
          ldflags += (" " + arg);
        }
#endif
      else if (arg == "-M" || arg == "-depend" || arg == "--depend")
        {
          depend = true;
        }
      else if (arg == "-o" || arg == "-output" || arg == "--output")
        {
          if (i < argc-1)
            {
              arg = argv[++i];
              outputfile = arg;
            }
          else
            std::cerr << "mkoctfile: output file name missing" << std::endl;
        }
      else if (arg == "-p" || arg == "-print" || arg == "--print")
        {
          if (i < argc-1)
            {
              arg = argv[++i];
              std::cout << vars[arg] << std::endl;
              return 0;
            }
          else
            std::cerr << "mkoctfile: --print requires argument" << std::endl;
        }
      else if (arg == "-s" || arg == "-strip" || arg == "--strip")
        {
          if (no_oct_file_strip_on_this_platform)
            std::cerr << "mkoctfile: stripping disabled on this platform"
                      << std::endl;
          else
            strip = true;
        }
      else if (arg == "-c" || arg == "-compile" || arg == "--compile")
        {
          link = false;
        }
      else if (arg == "-g")
        {
          vars["ALL_CFLAGS"] += " -g";
          vars["ALL_CXXFLAGS"] += " -g";
          vars["ALL_FFLAGS"] += " -g";
        }
      else if (arg == "-link-stand-alone" || arg == "--link-stand-alone")
        {
          link_stand_alone = true;
        }
      else if (arg == "-mex" || arg == "--mex")
        {
          incflags += " -I.";
#ifdef _MSC_VER
          ldflags += " -Wl,-export:mexFunction";
#endif
          output_ext = ".mex";
        }
      else if (starts_with (arg, "-W"))
        {
          pass_on_options += (" " + arg);
        }
      else if (starts_with (arg, "-"))
        {
          // Pass through any unrecognized options
          pass_on_options += (" " + arg);
        }
      else
        {
          std::cerr << "mkoctfile: unrecognized argument " << arg << std::endl;
          return 1;
        }

      if (!file.empty () && octfile.empty ())
        octfile = file;
    }

  if (link_stand_alone)
    {
      if (!outputfile.empty ())
        output_option = "-o " + outputfile;
    }
  else
    {
      if (!outputfile.empty ())
        {
          octfile = outputfile;
          size_t len = octfile.length ();
          size_t len_ext = output_ext.length ();
          if (octfile.substr (len-len_ext) != output_ext)
            octfile += output_ext;
        }
      else
        octfile = basename (octfile, true) + output_ext;
    }

  std::list<std::string>::const_iterator it;

  if (depend)
    {
      for (it = cfiles.begin (); it != cfiles.end (); ++it)
        {
          std::string f = *it, dfile = basename (f, true) + ".d", line;

          gnulib::unlink (dfile.c_str ());
          std::string cmd = vars["CC"] + " "
                            + vars["DEPEND_FLAGS"] + " "
                            + vars["CPPFLAGS"] + " "
                            + vars["ALL_CFLAGS"] + " "
                            + incflags  + " " + defs + " " + quote_path (f);

          FILE *fd = popen (cmd.c_str (), "r");
          std::ofstream fo (dfile.c_str ());
          size_t pos;
          while (!feof (fd))
            {
              line = get_line (fd);
              if ((pos = line.rfind (".o:")) != std::string::npos)
                {
                  size_t spos = line.rfind ('/', pos);
                  std::string ofile =
                    (spos == std::string::npos
                      ? line.substr (0, pos+2)
                      : line.substr (spos+1, pos-spos+1));
                  fo << "pic/" << ofile << " " << ofile << " "
                     << dfile << line.substr (pos) << std::endl;
                }
              else
                fo << line << std::endl;
            }
          pclose (fd);
          fo.close ();
        }

      for (it = ccfiles.begin (); it != ccfiles.end (); ++it)
        {
          std::string f = *it, dfile = basename (f, true) + ".d", line;

          gnulib::unlink (dfile.c_str ());
          std::string cmd = vars["CC"] + " "
                            + vars["DEPEND_FLAGS"] + " "
                            + vars["CPPFLAGS"] + " "
                            + vars["ALL_CXXFLAGS"] + " "
                            + incflags  + " " + defs + " " + quote_path (f);

          FILE *fd = popen (cmd.c_str (), "r");
          std::ofstream fo (dfile.c_str ());
          size_t pos;
          while (!feof (fd))
            {
              line = get_line (fd);
              if ((pos = line.rfind (".o:")) != std::string::npos)
                {
                  size_t spos = line.rfind ('/', pos);
                  std::string ofile =
                    (spos == std::string::npos
                      ? line.substr (0, pos+2)
                      : line.substr (spos+1, pos-spos+1));
                  fo << "pic/" << ofile << " " << ofile << " "
                     << dfile << line.substr (pos+2) << std::endl;
                }
              else
                fo << line << std::endl;
            }
          pclose (fd);
          fo.close ();
        }

      return 0;
    }

  for (it = f77files.begin (); it != f77files.end () && !result; ++it)
    {
      std::string f = *it, b = basename (f, true);
      if (!vars["F77"].empty ())
        {
          std::string o;
          if (!outputfile.empty ())
            {
              if (link)
                o = b + ".o";
              else
                o = outputfile;
            }
          else
            o = b + ".o";
          objfiles += (" " + o);
          std::string cmd = vars["F77"] + " -c "
                            + vars["FPICFLAG"] + " "
                            + vars["ALL_FFLAGS"] + " "
                            + incflags + " " + defs + " " + pass_on_options
                            + " " + f + " -o " + o;
          result = run_command (cmd);
        }
      else
        {
          std::cerr << "mkoctfile: no way to compile Fortran file " << f
                    << std::endl;
          return 1;
        }
    }

  for (it = cfiles.begin (); it != cfiles.end () && !result; ++it)
    {
      std::string f = *it;
      if (!vars["CC"].empty ())
        {
          std::string b = basename (f, true), o;
          if (!outputfile.empty ())
            {
              if (link)
                o = b + ".o";
              else
                o = outputfile;
            }
          else
            o = b + ".o";
          objfiles += (" " + o);
          std::string cmd = vars["CC"] + " -c "
                            + vars["CPPFLAGS"] + " "
                            + vars["CPICFLAG"] + " " + vars["ALL_CFLAGS"] + " "
                            + pass_on_options + " "
                            + incflags + " " + defs + " "
                            + quote_path (f) + " -o " + quote_path (o);
          result = run_command (cmd);
        }
      else
        {
          std::cerr << "mkoctfile: no way to compile C file " << f
                    << std::endl;
          return 1;
        }
    }

  for (it = ccfiles.begin (); it != ccfiles.end () && !result; ++it)
    {
      std::string f = *it;
      if (!vars["CXX"].empty ())
        {
          std::string b = basename (f, true), o;
          if (!outputfile.empty ())
            {
              if (link)
                o = b + ".o";
              else
                o = outputfile;
            }
          else
            o = b + ".o";
          objfiles += (" " + o);
          std::string cmd = vars["CXX"] + " -c "
                            + vars["CPPFLAGS"] + " "
                            + vars["CXXPICFLAG"] + " "
                            + vars["ALL_CXXFLAGS"] + " "
                            + pass_on_options + " "
                            + incflags + " " + defs + " "
                            + quote_path (f) + " -o " + quote_path (o);
          result = run_command (cmd);
        }
      else
        {
          std::cerr << "mkoctfile: no way to compile C++ file " << f
                    << std::endl;
          return 1;
        }
    }

  if (link && !objfiles.empty () && !result)
    {
      if (link_stand_alone)
        {
          if (!vars["LD_CXX"].empty ())
            {
              std::string cmd = vars["LD_CXX"] + " "
                                + vars["CPPFLAGS"] + " "
                                + vars["ALL_CXXFLAGS"] + " "
                                + vars["RDYNAMIC_FLAG"] + " "
                                + vars["ALL_LDFLAGS"] + " "
                                + pass_on_options + " " + output_option + " "
                                + objfiles + " " + libfiles + " "
                                + ldflags + " " + vars["LFLAGS"]
                                + " -loctinterp -loctave "
                                + " " + vars["OCTAVE_LINK_OPTS"]
                                + " " + vars["OCTAVE_LINK_DEPS"];
              result = run_command (cmd);
            }
          else
            {
              std::cerr
                << "mkoctfile: no way to link stand-alone executable file"
                << std::endl;
              return 1;
            }
        }
      else
        {
          std::string cmd = vars["DL_LD"] + " "
                            + vars["ALL_CXXFLAGS"] + " "
                            + vars["DL_LDFLAGS"] + " "
                            + pass_on_options
                            + " -o " + octfile + " "
                            + objfiles + " " + libfiles + " "
                            + ldflags + " "
                            + vars["LFLAGS"] + " -loctinterp -loctave "
                            + vars["OCT_LINK_OPTS"] + " "
                            + vars["OCT_LINK_DEPS"];
          result = run_command (cmd);
        }

      if (strip)
        {
          std::string cmd = "strip " + octfile;
          result = run_command (cmd);
        }
    }

  return result;
}
