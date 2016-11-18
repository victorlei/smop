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
#include <map>
#include <iostream>
#include <algorithm>
#include <cstdlib>

#ifndef OCTAVE_PREFIX
#define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

#include "shared-fcns.h"

static std::map<std::string,std::string> vars;

static std::string usage_msg = "usage: octave-config [options]";

static std::string help_msg =
"\n"
"Options:\n"
"\n"
"  -h, -?, --help        Print this message.\n"
"\n"
"  --m-site-dir          Print the name of the directory where Octave\n"
"                        expects to find locally installed .m files.\n"
"\n"
"  --oct-site-dir        Print the name of the directory where Octave\n"
"                        expects to find locally installed .oct files.\n"
"\n"
"  -p VAR, --print VAR   Print the value of the given configuration\n"
"                        variable VAR.  Recognized variables are:\n"
"\n"
"                          API_VERSION            LOCALARCHLIBDIR\n"
"                          ARCHLIBDIR             LOCALFCNFILEDIR\n"
"                          BINDIR                 LOCALOCTFILEDIR\n"
"                          CANONICAL_HOST_TYPE    LOCALSTARTUPFILEDIR\n"
"                          DATADIR                LOCALVERARCHLIBDIR\n"
"                          DATAROOTDIR            LOCALVERFCNFILEDIR\n"
"                          DEFAULT_PAGER          LOCALVEROCTFILEDIR\n"
"                          EXEC_PREFIX            MAN1DIR\n"
"                          EXEEXT                 MAN1EXT\n"
"                          FCNFILEDIR             MANDIR\n"
"                          IMAGEDIR               OCTDATADIR\n"
"                          INCLUDEDIR             OCTFILEDIR\n"
"                          INFODIR                OCTINCLUDEDIR\n"
"                          INFOFILE               OCTLIBDIR\n"
"                          LIBDIR                 PREFIX\n"
"                          LIBEXECDIR             SHLEXT\n"
"                          LOCALAPIARCHLIBDIR     STARTUPFILEDIR\n"
"                          LOCALAPIFCNFILEDIR     VERSION\n"
"                          LOCALAPIOCTFILEDIR\n"
"\n"
"  -v, --version         Print the Octave version number.\n"
"\n";

static void
initialize (void)
{
  vars["OCTAVE_HOME"] = get_octave_home ();
  vars["PREFIX"] = OCTAVE_PREFIX;

  vars["API_VERSION"] = %OCTAVE_API_VERSION%;
  vars["CANONICAL_HOST_TYPE"] = %OCTAVE_CANONICAL_HOST_TYPE%;
  vars["DEFAULT_PAGER"] = %OCTAVE_DEFAULT_PAGER%;
  vars["MAN1EXT"] = %OCTAVE_MAN1EXT%;
  vars["VERSION"] = %OCTAVE_VERSION%;

  vars["ARCHLIBDIR"] = subst_octave_home (%OCTAVE_ARCHLIBDIR%);
  vars["BINDIR"] = subst_octave_home (%OCTAVE_BINDIR%);
  vars["DATADIR"] = subst_octave_home (%OCTAVE_DATADIR%);
  vars["DATAROOTDIR"] = subst_octave_home (%OCTAVE_DATAROOTDIR%);
  vars["EXEC_PREFIX"] = subst_octave_home (%OCTAVE_EXEC_PREFIX%);
  vars["EXEEXT"] = subst_octave_home (%OCTAVE_EXEEXT%);
  vars["FCNFILEDIR"] = subst_octave_home (%OCTAVE_FCNFILEDIR%);
  vars["IMAGEDIR"] = subst_octave_home (%OCTAVE_IMAGEDIR%);
  vars["INCLUDEDIR"] = subst_octave_home (%OCTAVE_INCLUDEDIR%);
  vars["INFODIR"] = subst_octave_home (%OCTAVE_INFODIR%);
  vars["INFOFILE"] = subst_octave_home (%OCTAVE_INFOFILE%);
  vars["LIBDIR"] = subst_octave_home (%OCTAVE_LIBDIR%);
  vars["LIBEXECDIR"] = subst_octave_home (%OCTAVE_LIBEXECDIR%);
  vars["LOCALAPIARCHLIBDIR"] = subst_octave_home (%OCTAVE_LOCALAPIARCHLIBDIR%);
  vars["LOCALAPIFCNFILEDIR"] = subst_octave_home (%OCTAVE_LOCALAPIFCNFILEDIR%);
  vars["LOCALAPIOCTFILEDIR"] = subst_octave_home (%OCTAVE_LOCALAPIOCTFILEDIR%);
  vars["LOCALARCHLIBDIR"] = subst_octave_home (%OCTAVE_LOCALARCHLIBDIR%);
  vars["LOCALFCNFILEDIR"] = subst_octave_home (%OCTAVE_LOCALFCNFILEDIR%);
  vars["LOCALOCTFILEDIR"] = subst_octave_home (%OCTAVE_LOCALOCTFILEDIR%);
  vars["LOCALSTARTUPFILEDIR"] = subst_octave_home (%OCTAVE_LOCALSTARTUPFILEDIR%);
  vars["LOCALVERARCHLIBDIR"] = subst_octave_home (%OCTAVE_LOCALVERARCHLIBDIR%);
  vars["LOCALVERFCNFILEDIR"] = subst_octave_home (%OCTAVE_LOCALVERFCNFILEDIR%);
  vars["LOCALVEROCTFILEDIR"] = subst_octave_home (%OCTAVE_LOCALVEROCTFILEDIR%);
  vars["MAN1DIR"] = subst_octave_home (%OCTAVE_MAN1DIR%);
  vars["MANDIR"] = subst_octave_home (%OCTAVE_MANDIR%);
  vars["OCTDATADIR"] = subst_octave_home (%OCTAVE_OCTDATADIR%);
  vars["OCTFILEDIR"] = subst_octave_home (%OCTAVE_OCTFILEDIR%);
  vars["OCTINCLUDEDIR"] = subst_octave_home (%OCTAVE_OCTINCLUDEDIR%);
  vars["OCTLIBDIR"] = subst_octave_home (%OCTAVE_OCTLIBDIR%);
  vars["SHLEXT"] = subst_octave_home (%OCTAVE_SHLEXT%);
  vars["STARTUPFILEDIR"] = subst_octave_home (%OCTAVE_STARTUPFILEDIR%);
}

int
main (int argc, char **argv)
{
  initialize ();

  if (argc == 1)
    {
      std::cout << usage_msg << std::endl;
      return 1;
    }

  for (int i = 1; i < argc; i++)
    {
      std::string arg (argv[i]);

      if (arg == "-h" || arg == "-?" || arg == "--help")
        {
          std::cout << usage_msg << std::endl;
          std::cout << help_msg;
          return 0;
        }
      else if (arg == "--m-site-dir")
        std::cout << vars["LOCALVERFCNFILEDIR"] << std::endl;
      else if (arg == "--oct-site-dir")
        std::cout << vars["LOCALVEROCTFILEDIR"] << std::endl;
      else if (arg == "-v" || arg == "--version")
        std::cout << vars["VERSION"] << std::endl;
      else if (arg == "-p" || arg == "--print")
        {
          if (i < argc-1)
            {
              arg = argv[++i];
              std::cout << vars[arg] << std::endl;
            }
          else
            {
              std::cerr << "octave-config: " << arg
                        << " options requires argument" << std::endl;
              return 1;
            }
        }
      else
        {
          std::cerr << "octave-config: unrecognized argument " << arg
                    << std::endl;
          return 1;
        }
    }

  return 0;
}
