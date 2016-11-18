/*

Copyright (C) 2009-2015 Benjamin Lindner

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

#include "ls-ascii-helper.h"

#include <iostream>
#include <sstream>

// Helper functions when reading from ascii files.

// These function take care of CR/LF issues when files are opened in
// text-mode for reading.

// Skip characters from stream IS until a newline is reached.
// Depending on KEEP_NEWLINE, either eat newline from stream or
// keep it unread.

void
skip_until_newline (std::istream& is, bool keep_newline)
{
  if (! is)
    return;

  while (is)
    {
      char c = is.peek ();

      if (c == '\n' || c == '\r')
        {
          // Reached newline.
          if (! keep_newline)
            {
              // Eat the CR or LF character.
              char d;
              is.get (d);

              // Make sure that for binary-mode opened ascii files
              // containing CRLF line endings we skip the LF after CR.
              if (c == '\r' && is.peek () == '\n')
                {
                  // Yes, LF following CR, eat it.
                  is.get (d);
                }
            }

          // Newline was found, and read from stream if
          // keep_newline == true, so exit loop.
          break;
        }
      else
        {
          // No newline charater peeked, so read it and proceed to next
          // character.
          char d;
          is.get (d);
        }
    }
}


// If stream IS currently points to a newline (a leftover from a
// previous read) then eat newline(s) until a non-newline character is
// found.

void
skip_preceeding_newline (std::istream& is)
{
  if (! is)
    return;

  // Check whether IS currently points to newline character.
  char c = is.peek ();

  if (c == '\n' || c == '\r')
    {
      // Yes, at newline.
      do
        {
          // Eat the CR or LF character.
          char d;
          is.get (d);

          // Make sure that for binary-mode opened ascii files
          // containing CRLF line endings we skip the LF after CR.
          if (c == '\r' && is.peek () == '\n')
            {
              // Yes, LF following CR, eat it.
              is.get (d);
            }

          // Peek into next character.
          c = is.peek ();

          // Loop while still a newline ahead.
        }
      while (c == '\n' || c == '\r');
    }
}

// Read charaters from stream IS until a newline is reached.
// Depending on KEEP_NEWLINE, either eat newline from stream or keep
// it unread.  Characters read are stored and returned as
// std::string.

std::string
read_until_newline (std::istream& is, bool keep_newline)
{
  if (! is)
    return std::string ();

  std::ostringstream buf;

  while (is)
    {
      char c = is.peek ();

      if (c == '\n' || c == '\r')
        {
          // Reached newline.
          if (! keep_newline)
            {
              // Eat the CR or LF character.
              char d;
              is.get (d);

              // Make sure that for binary-mode opened ascii files
              // containing CRLF line endings we skip the LF after
              // CR.

              if (c == '\r' && is.peek () == '\n')
                {
                  // Yes, LF following CR, eat it.
                  is.get (d);
                }
            }

          // Newline was found, and read from stream if
          // keep_newline == true, so exit loop.
          break;
        }
      else
        {
          // No newline charater peeked, so read it, store it, and
          // proceed to next.
          char d;
          is.get (d);
          buf << d;
        }
    }

  return buf.str ();
}
