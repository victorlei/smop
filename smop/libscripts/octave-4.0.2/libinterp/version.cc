/*

Copyright (C) 2013-2015 John W. Eaton

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

#include "defaults.h"
#include "version.h"

static std::string
octave_warranty_statement (const std::string& extra_info = std::string ())
{
  return "There is ABSOLUTELY NO WARRANTY; not even for MERCHANTABILITY or\n\
FITNESS FOR A PARTICULAR PURPOSE."
         + extra_info;
}

static std::string
format_url (bool html, const std::string& url)
{
  return html ? "<a href=\"" + url + "\">" + url + "</a>" : url;
}

std::string
octave_www_statement (bool html)
{
  return "Additional information about Octave is available at "
         + format_url (html, "http://www.octave.org.");
}

std::string
octave_contrib_statement (bool html)
{
  return "Please contribute if you find this software useful.\n\
For more information, visit "
         + format_url (html, "http://www.octave.org/get-involved.html");
}

std::string
octave_bugs_statement (bool html)
{
  return "Read " + format_url (html, "http://www.octave.org/bugs.html")
         + " to learn how to submit bug reports.";
}

std::string
octave_name_version_and_copyright (void)
{
  // The GNU coding standards say that on the first line printed by
  // --version, the version number should follow the last space on the
  // line.

  return "GNU Octave, version " OCTAVE_VERSION "\n" OCTAVE_COPYRIGHT;
}

std::string
octave_name_version_copyright_copying_and_warranty
 (bool html, const std::string& extra_info)
{
  std::string br = html ? "<br>\n" : "\n";
  std::string sep = html ? "\n</p>\n<p>\n" : "\n\n";

  return octave_name_version_and_copyright ()
         + br
         + "This is free software; see the source code for copying conditions."
         + br
         + octave_warranty_statement (extra_info)
         + sep
         + "Octave was configured for \"" OCTAVE_CANONICAL_HOST_TYPE "\".";
}

std::string
octave_name_version_copyright_copying_warranty_and_bugs
  (bool html, const std::string& extra_info)
{
  std::string sep = html ? "\n</p>\n<p>\n" : "\n\n";

  std::string msg;

  if (html)
    msg = "<p>\n";

  msg += octave_name_version_copyright_copying_and_warranty (html, extra_info)
         + sep
         + octave_www_statement (html)
         + sep
         + octave_contrib_statement (html)
         + sep
         + octave_bugs_statement (html)
         + (html ? "\n</p>" : "");

  return msg;
}

std::string
octave_startup_message (bool html)
{
  std::string msg
    = octave_name_version_copyright_copying_warranty_and_bugs
        (html, "  For details, type 'warranty'.");

  msg += (html ? "<p>\n" : "\n");

  msg += "For information about changes from previous versions, type 'news'.";

  msg += (html ? "\n</p>" : "");

  return msg;
}
