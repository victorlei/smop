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

#include <cassert>
#include <cctype>
#include <cstring>

#include <iomanip>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include "Array.h"
#include "byte-swap.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-locbuf.h"
#include "quit.h"
#include "singleton-cleanup.h"
#include "str-vec.h"

#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-stdstrm.h"
#include "oct-stream.h"
#include "oct-obj.h"
#include "utils.h"

// Possible values for conv_err:
//
//   1 : not a real scalar
//   2 : value is NaN
//   3 : value is not an integer

static int
convert_to_valid_int (const octave_value& tc, int& conv_err)
{
  int retval = 0;

  conv_err = 0;

  double dval = tc.double_value ();

  if (! error_state)
    {
      if (! lo_ieee_isnan (dval))
        {
          int ival = NINT (dval);

          if (ival == dval)
            retval = ival;
          else
            conv_err = 3;
        }
      else
        conv_err = 2;
    }
  else
    conv_err = 1;

  return retval;
}

static int
get_size (double d, const std::string& who)
{
  int retval = -1;

  if (! lo_ieee_isnan (d))
    {
      if (! xisinf (d))
        {
          if (d >= 0.0)
            retval = NINT (d);
          else
            ::error ("%s: negative value invalid as size specification",
                     who.c_str ());
        }
      else
        retval = -1;
    }
  else
    ::error ("%s: NaN is invalid as size specification", who.c_str ());

  return retval;
}

static void
get_size (const Array<double>& size, octave_idx_type& nr, octave_idx_type& nc,
          bool& one_elt_size_spec, const std::string& who)
{
  nr = -1;
  nc = -1;

  one_elt_size_spec = false;

  double dnr = -1.0;
  double dnc = -1.0;

  octave_idx_type sz_len = size.length ();

  if (sz_len == 1)
    {
      one_elt_size_spec = true;

      dnr = size (0);

      dnc = (dnr == 0.0) ? 0.0 : 1.0;
    }
  else if (sz_len == 2)
    {
      dnr = size (0);

      if (! xisinf (dnr))
        dnc = size (1);
      else
        ::error ("%s: invalid size specification", who.c_str ());
    }
  else
    ::error ("%s: invalid size specification", who.c_str ());

  if (! error_state)
    {
      nr = get_size (dnr, who);

      if (! error_state && dnc >= 0.0)
        nc = get_size (dnc, who);
    }
}

scanf_format_list::scanf_format_list (const std::string& s)
  : nconv (0), curr_idx (0), list (dim_vector (16, 1)), buf (0)
{
  octave_idx_type num_elts = 0;

  size_t n = s.length ();

  size_t i = 0;

  int width = 0;
  bool discard = false;
  char modifier = '\0';
  char type = '\0';

  bool have_more = true;

  while (i < n)
    {
      have_more = true;

      if (! buf)
        buf = new std::ostringstream ();

      if (s[i] == '%')
        {
          // Process percent-escape conversion type.

          process_conversion (s, i, n, width, discard, type, modifier,
                              num_elts);

          have_more = (buf != 0);
        }
      else if (isspace (s[i]))
        {
          type = scanf_format_elt::whitespace_conversion;

          width = 0;
          discard = false;
          modifier = '\0';
          *buf << " ";

          while (++i < n && isspace (s[i]))
            /* skip whitespace */;

          add_elt_to_list (width, discard, type, modifier, num_elts);

          have_more = false;
        }
      else
        {
          type = scanf_format_elt::literal_conversion;

          width = 0;
          discard = false;
          modifier = '\0';

          while (i < n && ! isspace (s[i]) && s[i] != '%')
            *buf << s[i++];

          add_elt_to_list (width, discard, type, modifier, num_elts);

          have_more = false;
        }

      if (nconv < 0)
        {
          have_more = false;
          break;
        }
    }

  if (have_more)
    add_elt_to_list (width, discard, type, modifier, num_elts);

  list.resize (dim_vector (num_elts, 1));

  delete buf;
}

scanf_format_list::~scanf_format_list (void)
{
  octave_idx_type n = list.length ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      scanf_format_elt *elt = list(i);
      delete elt;
    }
}

void
scanf_format_list::add_elt_to_list (int width, bool discard, char type,
                                    char modifier, octave_idx_type& num_elts,
                                    const std::string& char_class)
{
  if (buf)
    {
      std::string text = buf->str ();

      if (! text.empty ())
        {
          scanf_format_elt *elt
            = new scanf_format_elt (text.c_str (), width, discard, type,
                                    modifier, char_class);

          if (num_elts == list.length ())
            list.resize (dim_vector (2 * num_elts, 1));

          list(num_elts++) = elt;
        }

      delete buf;
      buf = 0;
    }
}

static std::string
expand_char_class (const std::string& s)
{
  std::string retval;

  size_t len = s.length ();

  size_t i = 0;

  while (i < len)
    {
      unsigned char c = s[i++];

      if (c == '-' && i > 1 && i < len
          && (   static_cast<unsigned char> (s[i-2])
              <= static_cast<unsigned char> (s[i])))
        {
          // Add all characters from the range except the first (we
          // already added it below).

          for (c = s[i-2]+1; c < s[i]; c++)
            retval += c;
        }
      else
        {
          // Add the character to the class.  Only add '-' if it is
          // the last character in the class.

          if (c != '-' || i == len)
            retval += c;
        }
    }

  return retval;
}

void
scanf_format_list::process_conversion (const std::string& s, size_t& i,
                                       size_t n, int& width, bool& discard,
                                       char& type, char& modifier,
                                       octave_idx_type& num_elts)
{
  width = 0;
  discard = false;
  modifier = '\0';
  type = '\0';

  *buf << s[i++];

  bool have_width = false;

  while (i < n)
    {
      switch (s[i])
        {
        case '*':
          if (discard)
            nconv = -1;
          else
            {
              discard = true;
              *buf << s[i++];
            }
          break;

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          if (have_width)
            nconv = -1;
          else
            {
              char c = s[i++];
              width = width * 10 + c - '0';
              have_width = true;
              *buf << c;
              while (i < n && isdigit (s[i]))
                {
                  c = s[i++];
                  width = width * 10 + c - '0';
                  *buf << c;
                }
            }
          break;

        case 'h': case 'l': case 'L':
          if (modifier != '\0')
            nconv = -1;
          else
            modifier = s[i++];
          break;

        case 'd': case 'i': case 'o': case 'u': case 'x':
          if (modifier == 'L')
            {
              nconv = -1;
              break;
            }
          goto fini;

        case 'e': case 'f': case 'g':
          if (modifier == 'h')
            {
              nconv = -1;
              break;
            }

          // No float or long double conversions, thanks.
          *buf << 'l';

          goto fini;

        case 'c': case 's': case 'p': case '%': case '[':
          if (modifier != '\0')
            {
              nconv = -1;
              break;
            }
          goto fini;

        fini:
          {
            if (finish_conversion (s, i, n, width, discard, type,
                                   modifier, num_elts) == 0)
              return;
          }
          break;

        default:
          nconv = -1;
          break;
        }

      if (nconv < 0)
        break;
    }

  nconv = -1;
}

int
scanf_format_list::finish_conversion (const std::string& s, size_t& i,
                                      size_t n, int& width, bool discard,
                                      char& type, char modifier,
                                      octave_idx_type& num_elts)
{
  int retval = 0;

  std::string char_class;

  size_t beg_idx = std::string::npos;
  size_t end_idx = std::string::npos;

  if (s[i] == '%')
    {
      type = '%';
      *buf << s[i++];
    }
  else
    {
      type = s[i];

      if (s[i] == '[')
        {
          *buf << s[i++];

          if (i < n)
            {
              beg_idx = i;

              if (s[i] == '^')
                {
                  type = '^';
                  *buf << s[i++];

                  if (i < n)
                    {
                      beg_idx = i;

                      if (s[i] == ']')
                        *buf << s[i++];
                    }
                }
              else if (s[i] == ']')
                *buf << s[i++];
            }

          while (i < n && s[i] != ']')
            *buf << s[i++];

          if (i < n && s[i] == ']')
            {
              end_idx = i-1;
              *buf << s[i++];
            }

          if (s[i-1] != ']')
            retval = nconv = -1;
        }
      else
        *buf << s[i++];

      nconv++;
    }

  if (nconv >= 0)
    {
      if (beg_idx != std::string::npos && end_idx != std::string::npos)
        char_class = expand_char_class (s.substr (beg_idx,
                                        end_idx - beg_idx + 1));

      add_elt_to_list (width, discard, type, modifier, num_elts, char_class);
    }

  return retval;
}

void
scanf_format_list::printme (void) const
{
  octave_idx_type n = list.length ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      scanf_format_elt *elt = list(i);

      std::cerr
        << "width:      " << elt->width << "\n"
        << "discard:    " << elt->discard << "\n"
        << "type:       ";

      if (elt->type == scanf_format_elt::literal_conversion)
        std::cerr << "literal text\n";
      else if (elt->type == scanf_format_elt::whitespace_conversion)
        std::cerr << "whitespace\n";
      else
        std::cerr << elt->type << "\n";

      std::cerr
        << "modifier:   " << elt->modifier << "\n"
        << "char_class: '" << undo_string_escapes (elt->char_class) << "'\n"
        << "text:       '" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

bool
scanf_format_list::all_character_conversions (void)
{
  octave_idx_type n = list.length ();

  if (n > 0)
    {
      for (octave_idx_type i = 0; i < n; i++)
        {
          scanf_format_elt *elt = list(i);

          switch (elt->type)
            {
            case 'c': case 's': case '%': case '[': case '^':
            case scanf_format_elt::literal_conversion:
            case scanf_format_elt::whitespace_conversion:
              break;

            default:
              return false;
              break;
            }
        }

      return true;
    }
  else
    return false;
}

bool
scanf_format_list::all_numeric_conversions (void)
{
  octave_idx_type n = list.length ();

  if (n > 0)
    {
      for (octave_idx_type i = 0; i < n; i++)
        {
          scanf_format_elt *elt = list(i);

          switch (elt->type)
            {
            case 'd': case 'i': case 'o': case 'u': case 'x':
            case 'e': case 'f': case 'g':
              break;

            default:
              return false;
              break;
            }
        }

      return true;
    }
  else
    return false;
}

// Ugh again.

printf_format_list::printf_format_list (const std::string& s)
  : nconv (0), curr_idx (0), list (dim_vector (16, 1)), buf (0)
{
  octave_idx_type num_elts = 0;

  size_t n = s.length ();

  size_t i = 0;

  int args = 0;
  std::string flags;
  int fw = -1;
  int prec = -1;
  char modifier = '\0';
  char type = '\0';

  bool have_more = true;
  bool empty_buf = true;

  if (n == 0)
    {
      printf_format_elt *elt
        = new printf_format_elt ("", args, fw, prec, flags, type, modifier);

      list(num_elts++) = elt;

      list.resize (dim_vector (num_elts, 1));
    }
  else
    {
      while (i < n)
        {
          have_more = true;

          if (! buf)
            {
              buf = new std::ostringstream ();
              empty_buf = true;
            }

          switch (s[i])
            {
            case '%':
              {
                if (empty_buf)
                  {
                    process_conversion (s, i, n, args, flags, fw, prec,
                                        type, modifier, num_elts);

                    have_more = (buf != 0);
                  }
                else
                  add_elt_to_list (args, flags, fw, prec, type, modifier,
                                   num_elts);
              }
              break;

            default:
              {
                args = 0;
                flags = "";
                fw = -1;
                prec = -1;
                modifier = '\0';
                type = '\0';
                *buf << s[i++];
                empty_buf = false;
              }
              break;
            }

          if (nconv < 0)
            {
              have_more = false;
              break;
            }
        }

      if (have_more)
        add_elt_to_list (args, flags, fw, prec, type, modifier, num_elts);

      list.resize (dim_vector (num_elts, 1));

      delete buf;
    }
}

printf_format_list::~printf_format_list (void)
{
  octave_idx_type n = list.length ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      printf_format_elt *elt = list(i);
      delete elt;
    }
}

void
printf_format_list::add_elt_to_list (int args, const std::string& flags,
                                     int fw, int prec, char type,
                                     char modifier, octave_idx_type& num_elts)
{
  if (buf)
    {
      std::string text = buf->str ();

      if (! text.empty ())
        {
          printf_format_elt *elt
            = new printf_format_elt (text.c_str (), args, fw, prec, flags,
                                     type, modifier);

          if (num_elts == list.length ())
            list.resize (dim_vector (2 * num_elts, 1));

          list(num_elts++) = elt;
        }

      delete buf;
      buf = 0;
    }
}

void
printf_format_list::process_conversion (const std::string& s, size_t& i,
                                        size_t n, int& args, std::string& flags,
                                        int& fw, int& prec, char& modifier,
                                        char& type, octave_idx_type& num_elts)
{
  args = 0;
  flags = "";
  fw = -1;
  prec = -1;
  modifier = '\0';
  type = '\0';

  *buf << s[i++];

  bool nxt = false;

  while (i < n)
    {
      switch (s[i])
        {
        case '-': case '+': case ' ': case '0': case '#':
          flags += s[i];
          *buf << s[i++];
          break;

        default:
          nxt = true;
          break;
        }

      if (nxt)
        break;
    }

  if (i < n)
    {
      if (s[i] == '*')
        {
          fw = -2;
          args++;
          *buf << s[i++];
        }
      else
        {
          if (isdigit (s[i]))
            {
              int nn = 0;
              std::string tmp = s.substr (i);
              sscanf (tmp.c_str (), "%d%n", &fw, &nn);
            }

          while (i < n && isdigit (s[i]))
            *buf << s[i++];
        }
    }

  if (i < n && s[i] == '.')
    {
      // nothing before the . means 0.
      if (fw == -1)
        fw = 0;

      // . followed by nothing is 0.
      prec = 0;

      *buf << s[i++];

      if (i < n)
        {
          if (s[i] == '*')
            {
              prec = -2;
              args++;
              *buf << s[i++];
            }
          else
            {
              if (isdigit (s[i]))
                {
                  int nn = 0;
                  std::string tmp = s.substr (i);
                  sscanf (tmp.c_str (), "%d%n", &prec, &nn);
                }

              while (i < n && isdigit (s[i]))
                *buf << s[i++];
            }
        }
    }

  if (i < n)
    {
      // Accept and record modifier, but don't place it in the format
      // item text.  All integer conversions are handled as 64-bit
      // integers.

      switch (s[i])
        {
        case 'h': case 'l': case 'L':
          modifier = s[i++];
          break;

        default:
          break;
        }
    }

  if (i < n)
    finish_conversion (s, i, args, flags, fw, prec, modifier, type, num_elts);
  else
    nconv = -1;
}

void
printf_format_list::finish_conversion (const std::string& s, size_t& i,
                                       int args, const std::string& flags,
                                       int fw, int prec, char modifier,
                                       char& type, octave_idx_type& num_elts)
{
  switch (s[i])
    {
    case 'd': case 'i': case 'o': case 'x': case 'X':
    case 'u': case 'c':
      if (modifier == 'L')
        {
          nconv = -1;
          break;
        }
      goto fini;

    case 'f': case 'e': case 'E': case 'g': case 'G':
      if (modifier == 'h' || modifier == 'l')
        {
          nconv = -1;
          break;
        }
      goto fini;

    case 's': case 'p': case '%':
      if (modifier != '\0')
        {
          nconv = -1;
          break;
        }
      goto fini;

    fini:

      type = s[i];

      *buf << s[i++];

      if (type != '%' || args != 0)
        nconv++;

      if (type != '%')
        args++;

      add_elt_to_list (args, flags, fw, prec, type, modifier, num_elts);

      break;

    default:
      nconv = -1;
      break;
    }
}

void
printf_format_list::printme (void) const
{
  int n = list.length ();

  for (int i = 0; i < n; i++)
    {
      printf_format_elt *elt = list(i);

      std::cerr
        << "args:     " << elt->args << "\n"
        << "flags:    '" << elt->flags << "'\n"
        << "width:    " << elt->fw << "\n"
        << "prec:     " << elt->prec << "\n"
        << "type:     '" << elt->type << "'\n"
        << "modifier: '" << elt->modifier << "'\n"
        << "text:     '" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

void
octave_base_stream::error (const std::string& msg)
{
  fail = true;
  errmsg = msg;
}

void
octave_base_stream::error (const std::string& who, const std::string& msg)
{
  fail = true;
  errmsg = who + ": " + msg;
}

void
octave_base_stream::clear (void)
{
  fail = false;
  errmsg = "";
}

void
octave_base_stream::clearerr (void)
{
  std::istream *is = input_stream ();
  std::ostream *os = output_stream ();

  if (is)
    is->clear ();

  if (os)
    os->clear ();
}

// Functions that are defined for all input streams (input streams
// are those that define is).

std::string
octave_base_stream::do_gets (octave_idx_type max_len, bool& err,
                             bool strip_newline, const std::string& who)
{
  std::string retval;

  if (interactive && file_number () == 0)
    {
      ::error ("%s: unable to read from stdin while running interactively",
               who.c_str ());

      return retval;
    }

  err = false;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      std::ostringstream buf;

      int c = 0;
      int char_count = 0;

      if (max_len != 0)
        {
          while (is && (c = is.get ()) != EOF)
            {
              char_count++;

              // Handle CRLF, CR, or LF as line ending.

              if (c == '\r')
                {
                  if (! strip_newline)
                    buf << static_cast<char> (c);

                  c = is.get ();

                  if (c != EOF)
                    {
                      if (c == '\n')
                        {
                          char_count++;

                          if (! strip_newline)
                            buf << static_cast<char> (c);
                        }
                      else
                        is.putback (c);
                    }

                  break;
                }
              else if (c == '\n')
                {
                  if (! strip_newline)
                    buf << static_cast<char> (c);

                  break;
                }
              else
                buf << static_cast<char> (c);

              if (max_len > 0 && char_count == max_len)
                break;
            }
        }

      if (! is.eof () && char_count > 0)
        {
          // GAGME.  Matlab seems to check for EOF even if the last
          // character in a file is a newline character.  This is NOT
          // what the corresponding C-library functions do.
          int disgusting_compatibility_hack = is.get ();
          if (! is.eof ())
            is.putback (disgusting_compatibility_hack);
        }

      if (is.good () || (is.eof () && char_count > 0))
        retval = buf.str ();
      else
        {
          err = true;

          if (is.eof () && char_count == 0)
            error (who, "at end of file");
          else
            error (who, "read error");
        }
    }
  else
    {
      err = true;
      invalid_operation (who, "reading");
    }

  return retval;
}

std::string
octave_base_stream::getl (octave_idx_type max_len, bool& err,
                          const std::string& who)
{
  return do_gets (max_len, err, true, who);
}

std::string
octave_base_stream::gets (octave_idx_type max_len, bool& err,
                          const std::string& who)
{
  return do_gets (max_len, err, false, who);
}

off_t
octave_base_stream::skipl (off_t num, bool& err, const std::string& who)
{
  off_t cnt = -1;

  if (interactive && file_number () == 0)
    {
      ::error ("%s: unable to read from stdin while running interactively",
               who.c_str ());

      return count;
    }

  err = false;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      int c = 0;
      int lastc = -1;
      cnt = 0;

      while (is && (c = is.get ()) != EOF)
        {
          // Handle CRLF, CR, or LF as line ending.

          if (c == '\r' || (c == '\n' && lastc != '\r'))
            {
              if (++cnt == num)
                break;
            }

          lastc = c;
        }

      // Maybe eat the following \n if \r was just met.
      if (c == '\r' && is.peek () == '\n')
        is.get ();

      if (is.bad ())
        {
          err = true;
          error (who, "read error");
        }

      if (err)
        cnt = -1;
    }
  else
    {
      err = true;
      invalid_operation (who, "reading");
    }

  return cnt;
}

#define OCTAVE_SCAN(is, fmt, arg) octave_scan (is, fmt, arg)

template <class T>
std::istream&
octave_scan_1 (std::istream& is, const scanf_format_elt& fmt, T* valptr)
{
  T& ref = *valptr;

  switch (fmt.type)
    {
    case 'o':
      is >> std::oct >> ref >> std::dec;
      break;

    case 'x':
      is >> std::hex >> ref >> std::dec;
      break;

    case 'i':
      {
        int c1 = EOF;

        while (is && (c1 = is.get ()) != EOF && isspace (c1))
          /* skip whitespace */;

        if (c1 != EOF)
          {
            if (c1 == '0')
              {
                int c2 = is.peek ();

                if (c2 == 'x' || c2 == 'X')
                  {
                    is.ignore ();
                    if (std::isxdigit (is.peek ()))
                      is >> std::hex >> ref >> std::dec;
                    else
                      ref = 0;
                  }
                else
                  {
                    if (c2 == '0' || c2 == '1' || c2 == '2'
                        || c2 == '3' || c2 == '4' || c2 == '5'
                        || c2 == '6' || c2 == '7')
                      is >> std::oct >> ref >> std::dec;
                    else
                      ref = 0;
                  }
              }
            else
              {
                is.putback (c1);

                is >> ref;
              }
          }
      }
      break;

    default:
      is >> ref;
      break;
    }

  return is;
}

template <class T>
std::istream&
octave_scan (std::istream& is, const scanf_format_elt& fmt, T* valptr)
{
  if (fmt.width)
    {
      // Limit input to fmt.width characters by reading into a
      // temporary stringstream buffer.

      std::string tmp;

      is.width (fmt.width);
      is >> tmp;

      std::istringstream ss (tmp);

      octave_scan_1 (ss, fmt, valptr);
    }
  else
    octave_scan_1 (is, fmt, valptr);

  return is;
}

// Note that this specialization is only used for reading characters, not
// character strings. See BEGIN_S_CONVERSION for details.

template<>
std::istream&
octave_scan<> (std::istream& is, const scanf_format_elt& /* fmt */,
               char* valptr)
{
  return is >> valptr;
}

template<>
std::istream&
octave_scan<> (std::istream& is, const scanf_format_elt& fmt, double* valptr)
{
  double& ref = *valptr;

  switch (fmt.type)
    {
    case 'e':
    case 'f':
    case 'g':
      {
        int c1 = EOF;

        while (is && (c1 = is.get ()) != EOF && isspace (c1))
          /* skip whitespace */;

        if (c1 != EOF)
          {
            is.putback (c1);

            ref = octave_read_value<double> (is);
          }
      }
      break;

    default:
      panic_impossible ();
      break;
    }

  return is;
}

template <class T>
void
do_scanf_conv (std::istream& is, const scanf_format_elt& fmt,
               T valptr, Matrix& mval, double *data, octave_idx_type& idx,
               octave_idx_type& conversion_count, octave_idx_type nr,
               octave_idx_type max_size, bool discard)
{
  OCTAVE_SCAN (is, fmt, valptr);

  if (is)
    {
      if (idx == max_size && ! discard)
        {
          max_size *= 2;

          if (nr > 0)
            mval.resize (nr, max_size / nr, 0.0);
          else
            mval.resize (max_size, 1, 0.0);

          data = mval.fortran_vec ();
        }

      if (! discard)
        {
          conversion_count++;
          data[idx++] = *(valptr);
        }
    }
}

template void
do_scanf_conv (std::istream&, const scanf_format_elt&, double*,
               Matrix&, double*, octave_idx_type&, octave_idx_type&,
               octave_idx_type, octave_idx_type, bool);

#define DO_WHITESPACE_CONVERSION() \
  do \
    { \
      int c = EOF; \
 \
      while (is && (c = is.get ()) != EOF && isspace (c)) \
        /* skip whitespace */; \
 \
      if (c != EOF) \
        is.putback (c); \
    } \
  while (0)

#define DO_LITERAL_CONVERSION() \
  do \
    { \
      int c = EOF; \
 \
      int n = strlen (fmt); \
      int i = 0; \
 \
      while (i < n && is && (c = is.get ()) != EOF) \
        { \
          if (c == static_cast<unsigned char> (fmt[i])) \
            { \
              i++; \
              continue; \
            } \
          else \
            { \
              is.putback (c); \
              break; \
            } \
        } \
 \
      if (i != n) \
        is.setstate (std::ios::failbit); \
    } \
  while (0)

#define DO_PCT_CONVERSION() \
  do \
    { \
      int c = is.get (); \
 \
      if (c != EOF) \
        { \
          if (c != '%') \
            { \
              is.putback (c); \
              is.setstate (std::ios::failbit); \
            } \
        } \
      else \
        is.setstate (std::ios::failbit); \
    } \
  while (0)

#define BEGIN_C_CONVERSION() \
  is.unsetf (std::ios::skipws); \
 \
  int width = elt->width ? elt->width : 1; \
 \
  std::string tmp (width, '\0'); \
 \
  int c = EOF; \
  int n = 0; \
 \
  while (is && n < width && (c = is.get ()) != EOF) \
    tmp[n++] = static_cast<char> (c); \
 \
  if (n > 0 && c == EOF) \
    is.clear (); \
 \
  tmp.resize (n)

// For a '%s' format, skip initial whitespace and then read until the
// next whitespace character or until WIDTH characters have been read.
#define BEGIN_S_CONVERSION() \
  int width = elt->width; \
 \
  std::string tmp; \
 \
  do \
    { \
      if (width) \
        { \
          tmp = std::string (width, '\0'); \
 \
          int c = EOF; \
 \
          int n = 0; \
 \
          while (is && (c = is.get ()) != EOF) \
            { \
              if (! isspace (c)) \
                { \
                  tmp[n++] = static_cast<char> (c); \
                  break; \
                } \
            } \
 \
          while (is && n < width && (c = is.get ()) != EOF) \
            { \
              if (isspace (c)) \
                { \
                  is.putback (c); \
                  break; \
                } \
              else \
                tmp[n++] = static_cast<char> (c); \
            } \
 \
          if (n > 0 && c == EOF) \
            is.clear (); \
 \
          tmp.resize (n); \
        } \
      else \
        { \
          is >> std::ws >> tmp; \
        } \
    } \
  while (0)

// This format must match a nonempty sequence of characters.
#define BEGIN_CHAR_CLASS_CONVERSION() \
  int width = elt->width; \
 \
  std::string tmp; \
 \
  do \
    { \
      if (! width) \
        width = std::numeric_limits<int>::max (); \
 \
      std::ostringstream buf; \
 \
      std::string char_class = elt->char_class; \
 \
      int c = EOF; \
 \
      if (elt->type == '[') \
        { \
          int chars_read = 0; \
          while (is && chars_read++ < width && (c = is.get ()) != EOF \
                 && char_class.find (c) != std::string::npos) \
            buf << static_cast<char> (c); \
        } \
      else \
        { \
          int chars_read = 0; \
          while (is && chars_read++ < width && (c = is.get ()) != EOF \
                 && char_class.find (c) == std::string::npos) \
            buf << static_cast<char> (c); \
        } \
 \
      if (width == std::numeric_limits<int>::max () && c != EOF) \
        is.putback (c); \
 \
      tmp = buf.str (); \
 \
      if (tmp.empty ()) \
        is.setstate (std::ios::failbit); \
      else if (c == EOF) \
        is.clear (); \
 \
    } \
  while (0)

#define FINISH_CHARACTER_CONVERSION() \
  do \
    { \
      width = tmp.length (); \
 \
      if (is) \
        { \
          int i = 0; \
 \
          if (! discard) \
            { \
              conversion_count++; \
 \
              while (i < width) \
                { \
                  if (data_index == max_size) \
                    { \
                      max_size *= 2; \
 \
                      if (all_char_conv) \
                        { \
                          if (one_elt_size_spec) \
                            mval.resize (1, max_size, 0.0); \
                          else if (nr > 0) \
                            mval.resize (nr, max_size / nr, 0.0); \
                          else \
                            panic_impossible (); \
                        } \
                      else if (nr > 0) \
                        mval.resize (nr, max_size / nr, 0.0); \
                      else \
                        mval.resize (max_size, 1, 0.0); \
 \
                      data = mval.fortran_vec (); \
                    } \
 \
                  data[data_index++] = tmp[i++]; \
                } \
            } \
        } \
    } \
  while (0)

octave_value
octave_base_stream::do_scanf (scanf_format_list& fmt_list,
                              octave_idx_type nr, octave_idx_type nc,
                              bool one_elt_size_spec,
                              octave_idx_type& conversion_count,
                              const std::string& who)
{
  octave_value retval = Matrix ();

  if (interactive && file_number () == 0)
    {
      ::error ("%s: unable to read from stdin while running interactively",
               who.c_str ());

      return retval;
    }

  conversion_count = 0;

  octave_idx_type nconv = fmt_list.num_conversions ();

  octave_idx_type data_index = 0;

  if (nr == 0 || nc == 0)
    {
      if (one_elt_size_spec)
        nc = 0;

      return Matrix (nr, nc, 0.0);
    }

  std::istream *isp = input_stream ();

  bool all_char_conv = fmt_list.all_character_conversions ();

  Matrix mval;
  double *data = 0;
  octave_idx_type max_size = 0;
  octave_idx_type max_conv = 0;

  octave_idx_type final_nr = 0;
  octave_idx_type final_nc = 0;

  if (all_char_conv)
    {
      // Any of these could be resized later (if we have %s
      // conversions, we may read more than one element for each
      // conversion).

      if (one_elt_size_spec)
        {
          max_size = 512;
          mval.resize (1, max_size, 0.0);

          if (nr > 0)
            max_conv = nr;
        }
      else if (nr > 0)
        {
          if (nc > 0)
            {
              mval.resize (nr, nc, 0.0);
              max_size = max_conv = nr * nc;
            }
          else
            {
              mval.resize (nr, 32, 0.0);
              max_size = nr * 32;
            }
        }
      else
        panic_impossible ();
    }
  else if (nr > 0)
    {
      if (nc > 0)
        {
          // Will not resize later.
          mval.resize (nr, nc, 0.0);
          max_size = nr * nc;
          max_conv = max_size;
        }
      else
        {
          // Maybe resize later.
          mval.resize (nr, 32, 0.0);
          max_size = nr * 32;
        }
    }
  else
    {
      // Maybe resize later.
      mval.resize (32, 1, 0.0);
      max_size = 32;
    }

  data = mval.fortran_vec ();

  if (isp)
    {
      std::istream& is = *isp;

      const scanf_format_elt *elt = fmt_list.first ();

      std::ios::fmtflags flags = is.flags ();

      octave_idx_type trips = 0;

      octave_idx_type num_fmt_elts = fmt_list.length ();

      for (;;)
        {
          octave_quit ();

          if (elt)
            {
              if (! (elt->type == scanf_format_elt::whitespace_conversion
                     || elt->type == scanf_format_elt::literal_conversion
                     || elt->type == '%')
                  && max_conv > 0 && conversion_count == max_conv)
                {
                  if (all_char_conv && one_elt_size_spec)
                    {
                      final_nr = 1;
                      final_nc = data_index;
                    }
                  else
                    {
                      final_nr = nr;
                      final_nc = (data_index - 1) / nr + 1;
                    }

                  break;
                }
              else if (data_index == max_size)
                {
                  max_size *= 2;

                  if (all_char_conv)
                    {
                      if (one_elt_size_spec)
                        mval.resize (1, max_size, 0.0);
                      else if (nr > 0)
                        mval.resize (nr, max_size / nr, 0.0);
                      else
                        panic_impossible ();
                    }
                  else if (nr > 0)
                    mval.resize (nr, max_size / nr, 0.0);
                  else
                    mval.resize (max_size, 1, 0.0);

                  data = mval.fortran_vec ();
                }

              const char *fmt = elt->text;

              bool discard = elt->discard;

              switch (elt->type)
                {
                case scanf_format_elt::whitespace_conversion:
                  DO_WHITESPACE_CONVERSION ();
                  break;

                case scanf_format_elt::literal_conversion:
                  DO_LITERAL_CONVERSION ();
                  break;

                case '%':
                  DO_PCT_CONVERSION ();
                  break;

                case 'd': case 'i':
                  {
                    switch (elt->modifier)
                      {
                      case 'h':
                        {
                          short int tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      case 'l':
                        {
                          long int tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      default:
                        {
                          int tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;
                      }
                  }
                  break;

                case 'o': case 'u': case 'x':
                  {
                    switch (elt->modifier)
                      {
                      case 'h':
                        {
                          unsigned short int tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      case 'l':
                        {
                          unsigned long int tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;

                      default:
                        {
                          unsigned int tmp;
                          do_scanf_conv (is, *elt, &tmp, mval, data,
                                         data_index, conversion_count,
                                         nr, max_size, discard);
                        }
                        break;
                      }
                  }
                  break;

                case 'e': case 'f': case 'g':
                  {
                    double tmp;

                    do_scanf_conv (is, *elt, &tmp, mval, data,
                                   data_index, conversion_count,
                                   nr, max_size, discard);
                  }
                  break;

                case 'c':
                  {
                    BEGIN_C_CONVERSION ();

                    FINISH_CHARACTER_CONVERSION ();

                    is.setf (flags);
                  }
                  break;

                case 's':
                  {
                    BEGIN_S_CONVERSION ();

                    FINISH_CHARACTER_CONVERSION ();
                  }
                  break;

                case '[': case '^':
                  {
                    BEGIN_CHAR_CLASS_CONVERSION ();

                    FINISH_CHARACTER_CONVERSION ();
                  }
                  break;

                case 'p':
                  error ("%s: unsupported format specifier", who.c_str ());
                  break;

                default:
                  error ("%s: internal format error", who.c_str ());
                  break;
                }

              if (! ok ())
                {
                  break;
                }
              else if (! is)
                {
                  if (all_char_conv)
                    {
                      if (one_elt_size_spec)
                        {
                          final_nr = 1;
                          final_nc = data_index;
                        }
                      else if (data_index > nr)
                        {
                          final_nr = nr;
                          final_nc = (data_index - 1) / nr + 1;
                        }
                      else
                        {
                          final_nr = data_index;
                          final_nc = 1;
                        }
                    }
                  else if (nr > 0)
                    {
                      if (data_index > nr)
                        {
                          final_nr = nr;
                          final_nc = (data_index - 1) / nr + 1;
                        }
                      else
                        {
                          final_nr = data_index;
                          final_nc = 1;
                        }
                    }
                  else
                    {
                      final_nr = data_index;
                      final_nc = 1;
                    }

                  // If it looks like we have a matching failure, then
                  // reset the failbit in the stream state.

                  if (is.rdstate () & std::ios::failbit)
                    is.clear (is.rdstate () & (~std::ios::failbit));

                  // FIXME: is this the right thing to do?

                  if (interactive && ! forced_interactive && name () == "stdin")
                    {
                      is.clear ();

                      // Skip to end of line.

                      bool err;
                      do_gets (-1, err, false, who);
                    }

                  break;
                }
            }
          else
            {
              error ("%s: internal format error", who.c_str ());
              break;
            }

          if (nconv == 0 && ++trips == num_fmt_elts)
            {
              if (all_char_conv && one_elt_size_spec)
                {
                  final_nr = 1;
                  final_nc = data_index;
                }
              else
                {
                  final_nr = nr;
                  final_nc = (data_index - 1) / nr + 1;
                }

              break;
            }
          else
            elt = fmt_list.next (nconv > 0);
        }
    }

  if (ok ())
    {
      mval.resize (final_nr, final_nc, 0.0);

      retval = mval;

      if (all_char_conv)
        retval = retval.convert_to_str (false, true);
    }

  return retval;
}

octave_value
octave_base_stream::scanf (const std::string& fmt, const Array<double>& size,
                           octave_idx_type& conversion_count,
                           const std::string& who)
{
  octave_value retval = Matrix ();

  conversion_count = 0;

  std::istream *isp = input_stream ();

  if (isp)
    {
      scanf_format_list fmt_list (fmt);

      if (fmt_list.num_conversions () == -1)
        ::error ("%s: invalid format specified", who.c_str ());
      else
        {
          octave_idx_type nr = -1;
          octave_idx_type nc = -1;

          bool one_elt_size_spec;

          get_size (size, nr, nc, one_elt_size_spec, who);

          if (! error_state)
            retval = do_scanf (fmt_list, nr, nc, one_elt_size_spec,
                               conversion_count, who);
        }
    }
  else
    invalid_operation (who, "reading");

  return retval;
}

bool
octave_base_stream::do_oscanf (const scanf_format_elt *elt,
                               octave_value& retval, const std::string& who)
{
  bool quit = false;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      std::ios::fmtflags flags = is.flags ();

      if (elt)
        {
          const char *fmt = elt->text;

          bool discard = elt->discard;

          switch (elt->type)
            {
            case scanf_format_elt::whitespace_conversion:
              DO_WHITESPACE_CONVERSION ();
              break;

            case scanf_format_elt::literal_conversion:
              DO_LITERAL_CONVERSION ();
              break;

            case '%':
              {
                DO_PCT_CONVERSION ();

                if (! is)
                  quit = true;

              }
              break;

            case 'd': case 'i':
              {
                int tmp;

                if (OCTAVE_SCAN (is, *elt, &tmp))
                  {
                    if (! discard)
                      retval = tmp;
                  }
                else
                  quit = true;
              }
              break;

            case 'o': case 'u': case 'x':
              {
                long int tmp;

                if (OCTAVE_SCAN (is, *elt, &tmp))
                  {
                    if (! discard)
                      retval = tmp;
                  }
                else
                  quit = true;
              }
              break;

            case 'e': case 'f': case 'g':
              {
                double tmp;

                if (OCTAVE_SCAN (is, *elt, &tmp))
                  {
                    if (! discard)
                      retval = tmp;
                  }
                else
                  quit = true;
              }
              break;

            case 'c':
              {
                BEGIN_C_CONVERSION ();

                if (! discard)
                  retval = tmp;

                if (! is)
                  quit = true;

                is.setf (flags);
              }
              break;

            case 's':
              {
                BEGIN_S_CONVERSION ();

                if (! discard)
                  retval = tmp;

                if (! is)
                  quit = true;
              }
              break;

            case '[': case '^':
              {
                BEGIN_CHAR_CLASS_CONVERSION ();

                if (! discard)
                  retval = tmp;

                if (! is)
                  quit = true;
              }
              break;

            case 'p':
              error ("%s: unsupported format specifier", who.c_str ());
              break;

            default:
              error ("%s: internal format error", who.c_str ());
              break;
            }
        }

      if (ok () && is.fail ())
        {
          error ("%s: read error", who.c_str ());

          // FIXME: is this the right thing to do?

          if (interactive && ! forced_interactive && name () == "stdin")
            {
              // Skip to end of line.

              bool err;
              do_gets (-1, err, false, who);
            }
        }
    }

  return quit;
}

octave_value_list
octave_base_stream::oscanf (const std::string& fmt, const std::string& who)
{
  octave_value_list retval;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      scanf_format_list fmt_list (fmt);

      octave_idx_type nconv = fmt_list.num_conversions ();

      if (nconv == -1)
        ::error ("%s: invalid format specified", who.c_str ());
      else
        {
          is.clear ();

          octave_idx_type len = fmt_list.length ();

          retval.resize (nconv+2, Matrix ());

          const scanf_format_elt *elt = fmt_list.first ();

          int num_values = 0;

          bool quit = false;

          for (octave_idx_type i = 0; i < len; i++)
            {
              octave_value tmp;

              quit = do_oscanf (elt, tmp, who);

              if (quit)
                break;
              else
                {
                  if (tmp.is_defined ())
                    retval(num_values++) = tmp;

                  if (! ok ())
                    break;

                  elt = fmt_list.next (nconv > 0);
                }
            }

          retval(nconv) = num_values;

          int err_num;
          retval(nconv+1) = error (false, err_num);

          if (! quit)
            {
              // Pick up any trailing stuff.
              if (ok () && len > nconv)
                {
                  octave_value tmp;

                  elt = fmt_list.next ();

                  do_oscanf (elt, tmp, who);
                }
            }
        }
    }
  else
    invalid_operation (who, "reading");

  return retval;
}

// Functions that are defined for all output streams (output streams
// are those that define os).

int
octave_base_stream::flush (void)
{
  int retval = -1;

  std::ostream *os = output_stream ();

  if (os)
    {
      os->flush ();

      if (os->good ())
        retval = 0;
    }
  else
    invalid_operation ("fflush", "writing");

  return retval;
}

class
printf_value_cache
{
public:

  enum state { ok, conversion_error };

  printf_value_cache (const octave_value_list& args, const std::string& who)
    : values (args), val_idx (0), elt_idx (0),
      n_vals (values.length ()), n_elts (0), have_data (false),
      curr_state (ok)
  {
    for (octave_idx_type i = 0; i < values.length (); i++)
      {
        octave_value val = values(i);

        if (val.is_map () || val.is_cell () || val.is_object ())
          {
            gripe_wrong_type_arg (who, val);
            break;
          }
      }
  }

  ~printf_value_cache (void) { }

  // Get the current value as a double and advance the internal pointer.
  octave_value get_next_value (char type = 0);

  // Get the current value as an int and advance the internal pointer.
  int int_value (void);

  operator bool () const { return (curr_state == ok); }

  bool exhausted (void) { return (val_idx >= n_vals); }

private:

  const octave_value_list values;
  int val_idx;
  int elt_idx;
  int n_vals;
  int n_elts;
  bool have_data;
  octave_value curr_val;
  state curr_state;

  // Must create value cache with values!

  printf_value_cache (void);

  // No copying!

  printf_value_cache (const printf_value_cache&);

  printf_value_cache& operator = (const printf_value_cache&);
};

octave_value
printf_value_cache::get_next_value (char type)
{
  octave_value retval;

  if (exhausted ())
    curr_state = conversion_error;

  while (! exhausted ())
    {
      if (! have_data)
        {
          curr_val = values (val_idx);

          // Force string conversion here for compatibility.

          if (! error_state)
            {
              elt_idx = 0;
              n_elts = curr_val.numel ();
              have_data = true;
            }
          else
            {
              curr_state = conversion_error;
              break;
            }
        }

      if (elt_idx < n_elts)
        {
          if (type == 's')
            {
              if (curr_val.is_string ())
                {
                  dim_vector dv (1, curr_val.numel ());
                  octave_value tmp = curr_val.reshape (dv);

                  std::string sval = tmp.string_value ();

                  retval = sval.substr (elt_idx);

                  // We've consumed the rest of the value.
                  elt_idx = n_elts;
                }
              else
                {
                  // Convert to character string while values are
                  // integers in the range [0 : char max]

                  const NDArray val = curr_val.array_value ();

                  octave_idx_type idx = elt_idx;

                  for (; idx < n_elts; idx++)
                    {
                      double dval = val(idx);

                      if (D_NINT (dval) != dval || dval < 0 || dval > 255)
                        break;
                    }

                  octave_idx_type n = idx - elt_idx;

                  if (n > 0)
                    {
                      std::string sval (n, '\0');

                      for (octave_idx_type i = 0; i < n; i++)
                        sval[i] = val(elt_idx++);

                      retval = sval;
                    }
                  else
                    retval = curr_val.fast_elem_extract (elt_idx++);
                }
            }
          else
            {
              retval = curr_val.fast_elem_extract (elt_idx++);

              if (type == 'c' && ! retval.is_string ())
                {
                  double dval = retval.double_value ();

                  if (D_NINT (dval) == dval && dval >= 0 && dval < 256)
                    retval = static_cast<char> (dval);
                }
            }

          if (elt_idx >= n_elts)
            {
              elt_idx = 0;
              val_idx++;
              have_data = false;
            }

          break;
        }
      else
        {
          val_idx++;
          have_data = false;

          if (n_elts == 0)
            {
              if (elt_idx == 0 && (type == 's' || type == 'c'))
                {
                  retval = "";
                  break;
                }

              if (exhausted ())
                curr_state = conversion_error;
            }
        }
    }

  return retval;
}

int
printf_value_cache::int_value (void)
{
  int retval = 0;

  octave_value val = get_next_value ();

  if (! error_state)
    {
      double dval = val.double_value (true);

      if (! error_state)
        {
          if (D_NINT (dval) == dval)
            retval = NINT (dval);
          else
            curr_state = conversion_error;
        }
    }

  return retval;
}

// Ugh again and again.

template <class T>
int
do_printf_conv (std::ostream& os, const char *fmt, int nsa, int sa_1,
                int sa_2, T arg, const std::string& who)
{
  int retval = 0;

  switch (nsa)
    {
    case 2:
      retval = octave_format (os, fmt, sa_1, sa_2, arg);
      break;

    case 1:
      retval = octave_format (os, fmt, sa_1, arg);
      break;

    case 0:
      retval = octave_format (os, fmt, arg);
      break;

    default:
      ::error ("%s: internal error handling format", who.c_str ());
      break;
    }

  return retval;
}

static size_t
do_printf_string (std::ostream& os, const printf_format_elt *elt,
                  int nsa, int sa_1, int sa_2, const std::string& arg,
                  const std::string& who)
{
  size_t retval = 0;

  if (nsa > 2)
    {
      ::error ("%s: internal error handling format", who.c_str ());
      return retval;
    }

  std::string flags = elt->flags;

  bool left = flags.find ('-') != std::string::npos;

  size_t len = arg.length ();

  size_t fw = nsa > 0 ? sa_1 : (elt->fw == -1 ? len : elt->fw);
  size_t prec = nsa > 1 ? sa_2 : (elt->prec == -1 ? len : elt->prec);

  os << std::setw (fw)
     << (left ? std::left : std::right)
     << (prec < len ? arg.substr (0, prec) : arg);

  return len > fw ? len : fw;
}

static bool
is_nan_or_inf (const octave_value& val)
{
  octave_value ov_isnan = val.isnan ();
  octave_value ov_isinf = val.isinf ();

  return (ov_isnan.is_true () || ov_isinf.is_true ());
}

static bool
ok_for_signed_int_conv (const octave_value& val)
{
  uint64_t limit = std::numeric_limits<int64_t>::max ();

  if (val.is_string ())
    return true;
  else if (val.is_integer_type ())
    {
      if (val.is_uint64_type ())
        {
          octave_uint64 ival = val.uint64_scalar_value ();

          if (ival.value () <= limit)
            return true;
        }
      else
        return true;
    }
  else
    {
      double dval = val.double_value (true);

      if (dval == xround (dval) && dval <= limit)
        return true;
    }

  return false;
}

static bool
ok_for_unsigned_int_conv (const octave_value& val)
{
  if (val.is_string ())
    return true;
  else if (val.is_integer_type ())
    {
      // Easier than dispatching here...

      octave_value ov_is_ge_zero
        = do_binary_op (octave_value::op_ge, val, octave_value (0.0));

      return ov_is_ge_zero.is_true ();
    }
  else
    {
      double dval = val.double_value (true);

      uint64_t limit = std::numeric_limits<uint64_t>::max ();

      if (dval == xround (dval) && dval >= 0 && dval <= limit)
        return true;
    }

  return false;
}

static std::string
switch_to_g_format (const printf_format_elt *elt)
{
  std::string tfmt = elt->text;

  tfmt.replace (tfmt.rfind (elt->type), 1, "g");

  return tfmt;
}

int
octave_base_stream::do_numeric_printf_conv (std::ostream& os,
                                            const printf_format_elt *elt,
                                            int nsa, int sa_1, int sa_2,
                                            const octave_value& val,
                                            const std::string& who)
{
  int retval = 0;

  const char *fmt = elt->text;

  if (is_nan_or_inf (val))
    {
      double dval = val.double_value ();

      std::string tfmt = fmt;
      std::string::size_type i1, i2;

      tfmt.replace ((i1 = tfmt.rfind (elt->type)),
                    1, 1, 's');

      if ((i2 = tfmt.rfind ('.')) != std::string::npos
          && i2 < i1)
        {
          tfmt.erase (i2, i1-i2);
          if (elt->prec == -2)
            nsa--;
        }

      const char *tval;
      if (lo_ieee_isinf (dval))
        {
          if (elt->flags.find ('+') != std::string::npos)
            tval = (dval < 0 ? "-Inf" : "+Inf");
          else
            tval = (dval < 0 ? "-Inf" : "Inf");
        }
      else
        {
          if (elt->flags.find ('+') != std::string::npos)
            tval = (lo_ieee_is_NA (dval) ? "+NA" : "+NaN");
          else
            tval = (lo_ieee_is_NA (dval) ? "NA" : "NaN");
        }

      retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2, tval, who);
    }
  else
    {
      static std::string llmod
        = sizeof (long) == sizeof (int64_t) ? "l" : "ll";

      char type = elt->type;

      switch (type)
        {
        case 'd': case 'i': case 'c':
          if (ok_for_signed_int_conv (val))
            {
              octave_int64 tval = val.int64_scalar_value ();

              // Insert "long" modifier.
              std::string tfmt = fmt;
              tfmt.replace (tfmt.rfind (type), 1, llmod + type);

              retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                        tval.value (), who);
            }
          else
            {
              std::string tfmt = switch_to_g_format (elt);

              double dval = val.double_value (true);

              if (! error_state)
                retval += do_printf_conv (os, tfmt.c_str (), nsa,
                                          sa_1, sa_2, dval, who);
            }
          break;

        case 'o': case 'x': case 'X': case 'u':
          if (ok_for_unsigned_int_conv (val))
            {
              octave_uint64 tval = val.uint64_scalar_value ();

              // Insert "long" modifier.
              std::string tfmt = fmt;
              tfmt.replace (tfmt.rfind (type), 1, llmod + type);

              retval += do_printf_conv (os, tfmt.c_str (), nsa, sa_1, sa_2,
                                        tval.value (), who);
            }
          else
            {
              std::string tfmt = switch_to_g_format (elt);

              double dval = val.double_value (true);

              if (! error_state)
                retval += do_printf_conv (os, tfmt.c_str (), nsa,
                                          sa_1, sa_2, dval, who);
            }
          break;

        case 'f': case 'e': case 'E':
        case 'g': case 'G':
          {
            double dval = val.double_value (true);

            if (! error_state)
              retval += do_printf_conv (os, fmt, nsa, sa_1, sa_2, dval, who);
          }
          break;

        default:
          error ("%s: invalid format specifier",
                 who.c_str ());
          return -1;
          break;
        }
    }

  return retval;
}

int
octave_base_stream::do_printf (printf_format_list& fmt_list,
                               const octave_value_list& args,
                               const std::string& who)
{
  int retval = 0;

  octave_idx_type nconv = fmt_list.num_conversions ();

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      const printf_format_elt *elt = fmt_list.first ();

      printf_value_cache val_cache (args, who);

      if (error_state)
        return retval;

      for (;;)
        {
          octave_quit ();

          if (elt)
            {
              // NSA is the number of 'star' args to convert.

              int nsa = (elt->fw == -2) + (elt->prec == -2);

              int sa_1 = 0;
              int sa_2 = 0;

              if (nsa > 0)
                {
                  sa_1 = val_cache.int_value ();

                  if (! val_cache)
                    break;
                  else
                    {
                      if (nsa > 1)
                        {
                          sa_2 = val_cache.int_value ();

                          if (! val_cache)
                            break;
                        }
                    }
                }

              if (elt->type == '%')
                {
                  os << "%";
                  retval++;
                }
              else if (elt->args == 0 && elt->text)
                {
                  os << elt->text;
                  retval += strlen (elt->text);
                }
              else if (elt->type == 's' || elt->type == 'c')
                {
                  octave_value val = val_cache.get_next_value (elt->type);

                  if (val_cache)
                    {
                      if (val.is_string ())
                        {
                          std::string sval = val.string_value ();

                          retval += do_printf_string (os, elt, nsa, sa_1,
                                                      sa_2, sval, who);
                        }
                      else
                        retval += do_numeric_printf_conv (os, elt, nsa, sa_1,
                                                          sa_2, val, who);
                    }
                  else
                    break;
                }
              else
                {
                  octave_value val = val_cache.get_next_value ();

                  if (val_cache)
                    retval += do_numeric_printf_conv (os, elt, nsa, sa_1,
                                                      sa_2, val, who);
                  else
                    break;
                }

              if (! os)
                {
                  error ("%s: write error", who.c_str ());
                  break;
                }
            }
          else
            {
              ::error ("%s: internal error handling format", who.c_str ());
              retval = -1;
              break;
            }

          elt = fmt_list.next (nconv > 0 && ! val_cache.exhausted ());

          if (! elt || (val_cache.exhausted () && elt->args > 0))
            break;
        }
    }
  else
    invalid_operation (who, "writing");

  return retval;
}

int
octave_base_stream::printf (const std::string& fmt,
                            const octave_value_list& args,
                            const std::string& who)
{
  int retval = 0;

  printf_format_list fmt_list (fmt);

  if (fmt_list.num_conversions () == -1)
    ::error ("%s: invalid format specified", who.c_str ());
  else
    retval = do_printf (fmt_list, args, who);

  return retval;
}

int
octave_base_stream::puts (const std::string& s, const std::string& who)
{
  int retval = -1;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      os << s;

      if (os)
        {
          // FIXME: why does this seem to be necessary?
          // Without it, output from a loop like
          //
          //   for i = 1:100, fputs (stdout, "foo\n"); endfor
          //
          // doesn't seem to go to the pager immediately.

          os.flush ();

          if (os)
            retval = 0;
          else
            error ("%s: write error", who.c_str ());
        }
      else
        error ("%s: write error", who.c_str ());
    }
  else
    invalid_operation (who, "writing");

  return retval;
}

// Return current error message for this stream.

std::string
octave_base_stream::error (bool clear_err, int& err_num)
{
  err_num = fail ? -1 : 0;

  std::string tmp = errmsg;

  if (clear_err)
    clear ();

  return tmp;
}

void
octave_base_stream::invalid_operation (const std::string& who, const char *rw)
{
  // Note that this is not ::error () !

  error (who, std::string ("stream not open for ") + rw);
}

octave_stream::octave_stream (octave_base_stream *bs)
  : rep (bs)
{
  if (rep)
    rep->count = 1;
}

octave_stream::~octave_stream (void)
{
  if (rep && --rep->count == 0)
    delete rep;
}

octave_stream::octave_stream (const octave_stream& s)
  : rep (s.rep)
{
  if (rep)
    rep->count++;
}

octave_stream&
octave_stream::operator = (const octave_stream& s)
{
  if (rep != s.rep)
    {
      if (rep && --rep->count == 0)
        delete rep;

      rep = s.rep;

      if (rep)
        rep->count++;
    }

  return *this;
}

int
octave_stream::flush (void)
{
  int retval = -1;

  if (stream_ok ())
    retval = rep->flush ();

  return retval;
}

std::string
octave_stream::getl (octave_idx_type max_len, bool& err, const std::string& who)
{
  std::string retval;

  if (stream_ok ())
    retval = rep->getl (max_len, err, who);

  return retval;
}

std::string
octave_stream::getl (const octave_value& tc_max_len, bool& err,
                     const std::string& who)
{
  std::string retval;

  err = false;

  int conv_err = 0;

  int max_len = -1;

  if (tc_max_len.is_defined ())
    {
      max_len = convert_to_valid_int (tc_max_len, conv_err);

      if (conv_err || max_len < 0)
        {
          err = true;
          ::error ("%s: invalid maximum length specified", who.c_str ());
        }
    }

  if (! error_state)
    retval = getl (max_len, err, who);

  return retval;
}

std::string
octave_stream::gets (octave_idx_type max_len, bool& err, const std::string& who)
{
  std::string retval;

  if (stream_ok ())
    retval = rep->gets (max_len, err, who);

  return retval;
}

std::string
octave_stream::gets (const octave_value& tc_max_len, bool& err,
                     const std::string& who)
{
  std::string retval;

  err = false;

  int conv_err = 0;

  int max_len = -1;

  if (tc_max_len.is_defined ())
    {
      max_len = convert_to_valid_int (tc_max_len, conv_err);

      if (conv_err || max_len < 0)
        {
          err = true;
          ::error ("%s: invalid maximum length specified", who.c_str ());
        }
    }

  if (! error_state)
    retval = gets (max_len, err, who);

  return retval;
}

off_t
octave_stream::skipl (off_t count, bool& err, const std::string& who)
{
  off_t retval = -1;

  if (stream_ok ())
    retval = rep->skipl (count, err, who);

  return retval;
}

off_t
octave_stream::skipl (const octave_value& tc_count, bool& err,
                      const std::string& who)
{
  off_t retval = -1;

  err = false;

  int conv_err = 0;

  int count = 1;

  if (tc_count.is_defined ())
    {
      if (tc_count.is_scalar_type () && xisinf (tc_count.scalar_value ()))
        count = -1;
      else
        {
          count = convert_to_valid_int (tc_count, conv_err);

          if (conv_err || count < 0)
            {
              err = true;
              ::error ("%s: invalid number of lines specified", who.c_str ());
            }
        }
    }

  if (! error_state)
    retval = skipl (count, err, who);

  return retval;
}

int
octave_stream::seek (off_t offset, int origin)
{
  int status = -1;

  if (stream_ok ())
    {
      clearerr ();

      // Find current position so we can return to it if needed.

      off_t orig_pos = rep->tell ();

      // Move to end of file.  If successful, find the offset of the end.

      status = rep->seek (0, SEEK_END);

      if (status == 0)
        {
          off_t eof_pos = rep->tell ();

          if (origin == SEEK_CUR)
            {
              // Move back to original position, otherwise we will be
              // seeking from the end of file which is probably not the
              // original location.

              rep->seek (orig_pos, SEEK_SET);
            }

          // Attempt to move to desired position; may be outside bounds
          // of existing file.

          status = rep->seek (offset, origin);

          if (status == 0)
            {
              // Where are we after moving to desired position?

              off_t desired_pos = rep->tell ();

              // I don't think save_pos can be less than zero, but we'll
              // check anyway...

              if (desired_pos > eof_pos || desired_pos < 0)
                {
                  // Seek outside bounds of file.  Failure should leave
                  // position unchanged.

                  rep->seek (orig_pos, SEEK_SET);

                  status = -1;
                }
            }
          else
            {
              // Seeking to the desired position failed.  Move back to
              // original position and return failure status.

              rep->seek (orig_pos, SEEK_SET);

              status = -1;
            }
        }
    }

  return status;
}

int
octave_stream::seek (const octave_value& tc_offset,
                     const octave_value& tc_origin)
{
  int retval = -1;

  // FIXME: should we have octave_value methods that handle off_t explicitly?
  octave_int64 val = tc_offset.int64_scalar_value ();
  off_t xoffset = val.value ();

  if (! error_state)
    {
      int conv_err = 0;

      int origin = SEEK_SET;

      if (tc_origin.is_string ())
        {
          std::string xorigin = tc_origin.string_value ();

          if (xorigin == "bof")
            origin = SEEK_SET;
          else if (xorigin == "cof")
            origin = SEEK_CUR;
          else if (xorigin == "eof")
            origin = SEEK_END;
          else
            conv_err = -1;
        }
      else
        {
          int xorigin = convert_to_valid_int (tc_origin, conv_err);

          if (! conv_err)
            {
              if (xorigin == -1)
                origin = SEEK_SET;
              else if (xorigin == 0)
                origin = SEEK_CUR;
              else if (xorigin == 1)
                origin = SEEK_END;
              else
                conv_err = -1;
            }
        }

      if (! conv_err)
        {
          retval = seek (xoffset, origin);

          if (retval != 0)
            error ("fseek: failed to seek to requested position");
        }
      else
        error ("fseek: invalid value for origin");
    }
  else
    error ("fseek: invalid value for offset");

  return retval;
}

off_t
octave_stream::tell (void)
{
  off_t retval = -1;

  if (stream_ok ())
    retval = rep->tell ();

  return retval;
}

int
octave_stream::rewind (void)
{
  return seek (0, SEEK_SET);
}

bool
octave_stream::is_open (void) const
{
  bool retval = false;

  if (stream_ok ())
    retval = rep->is_open ();

  return retval;
}

void
octave_stream::close (void)
{
  if (stream_ok ())
    rep->close ();
}

template <class SRC_T, class DST_T>
static octave_value
convert_and_copy (std::list<void *>& input_buf_list,
                  octave_idx_type input_buf_elts,
                  octave_idx_type elts_read,
                  octave_idx_type nr, octave_idx_type nc, bool swap,
                  bool do_float_fmt_conv, bool do_NA_conv,
                  oct_mach_info::float_format from_flt_fmt)
{
  typedef typename DST_T::element_type dst_elt_type;

  DST_T conv (dim_vector (nr, nc));

  dst_elt_type *conv_data = conv.fortran_vec ();

  octave_idx_type j = 0;

  for (std::list<void *>::const_iterator it = input_buf_list.begin ();
       it != input_buf_list.end (); it++)
    {
      SRC_T *data = static_cast<SRC_T *> (*it);

      if (swap || do_float_fmt_conv)
        {
          for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
               i++, j++)
            {
              if (swap)
                swap_bytes<sizeof (SRC_T)> (&data[i]);
              else if (do_float_fmt_conv)
                do_float_format_conversion (&data[i], sizeof (SRC_T),
                                            1, from_flt_fmt,
                                            oct_mach_info::float_format ());

              dst_elt_type tmp (data[i]);

              if (do_NA_conv && __lo_ieee_is_old_NA (tmp))
                tmp = __lo_ieee_replace_old_NA (tmp);

              conv_data[j] = tmp;
            }
        }
      else
        {
          if (do_NA_conv)
            {
              for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
                   i++, j++)
                {
                  dst_elt_type tmp (data[i]);

                  if (__lo_ieee_is_old_NA (tmp))
                    tmp = __lo_ieee_replace_old_NA (tmp);

                  conv_data[j] = tmp;
                }
            }
          else
            {
              for (octave_idx_type i = 0; i < input_buf_elts && j < elts_read;
                   i++, j++)
                conv_data[j] = data[i];
            }
        }

      delete [] data;
    }

  input_buf_list.clear ();

  for (octave_idx_type i = elts_read; i < nr * nc; i++)
    conv_data[i] = dst_elt_type (0);

  return conv;
}

typedef octave_value (*conv_fptr)
  (std::list<void *>& input_buf_list, octave_idx_type input_buf_elts,
   octave_idx_type elts_read, octave_idx_type nr, octave_idx_type nc,
   bool swap, bool do_float_fmt_conv, bool do_NA_conv,
   oct_mach_info::float_format from_flt_fmt);

#define TABLE_ELT(T, U, V, W) \
  conv_fptr_table[oct_data_conv::T][oct_data_conv::U] = convert_and_copy<V, W>

#define FILL_TABLE_ROW(T, V) \
  TABLE_ELT (T, dt_int8, V, int8NDArray); \
  TABLE_ELT (T, dt_uint8, V, uint8NDArray); \
  TABLE_ELT (T, dt_int16, V, int16NDArray); \
  TABLE_ELT (T, dt_uint16, V, uint16NDArray); \
  TABLE_ELT (T, dt_int32, V, int32NDArray); \
  TABLE_ELT (T, dt_uint32, V, uint32NDArray); \
  TABLE_ELT (T, dt_int64, V, int64NDArray); \
  TABLE_ELT (T, dt_uint64, V, uint64NDArray); \
  TABLE_ELT (T, dt_single, V, FloatNDArray); \
  TABLE_ELT (T, dt_double, V, NDArray); \
  TABLE_ELT (T, dt_char, V, charNDArray); \
  TABLE_ELT (T, dt_schar, V, charNDArray); \
  TABLE_ELT (T, dt_uchar, V, charNDArray); \
  TABLE_ELT (T, dt_logical, V, boolNDArray);

octave_value
octave_stream::finalize_read (std::list<void *>& input_buf_list,
                              octave_idx_type input_buf_elts,
                              octave_idx_type elts_read,
                              octave_idx_type nr, octave_idx_type nc,
                              oct_data_conv::data_type input_type,
                              oct_data_conv::data_type output_type,
                              oct_mach_info::float_format ffmt)
{
  octave_value retval;

  static bool initialized = false;

  // Table function pointers for return types x read types.

  static conv_fptr conv_fptr_table[oct_data_conv::dt_unknown][14];

  if (! initialized)
    {
      for (int i = 0; i < oct_data_conv::dt_unknown; i++)
        for (int j = 0; j < 14; j++)
          conv_fptr_table[i][j] = 0;

      FILL_TABLE_ROW (dt_int8, int8_t);
      FILL_TABLE_ROW (dt_uint8, uint8_t);
      FILL_TABLE_ROW (dt_int16, int16_t);
      FILL_TABLE_ROW (dt_uint16, uint16_t);
      FILL_TABLE_ROW (dt_int32, int32_t);
      FILL_TABLE_ROW (dt_uint32, uint32_t);
      FILL_TABLE_ROW (dt_int64, int64_t);
      FILL_TABLE_ROW (dt_uint64, uint64_t);
      FILL_TABLE_ROW (dt_single, float);
      FILL_TABLE_ROW (dt_double, double);
      FILL_TABLE_ROW (dt_char, char);
      FILL_TABLE_ROW (dt_schar, signed char);
      FILL_TABLE_ROW (dt_uchar, unsigned char);
      FILL_TABLE_ROW (dt_logical, bool);

      initialized = true;
    }

  bool swap = false;

  if (ffmt == oct_mach_info::flt_fmt_unknown)
    ffmt = float_format ();

  if (oct_mach_info::words_big_endian ())
    swap = (ffmt == oct_mach_info::flt_fmt_ieee_little_endian);
  else
    swap = (ffmt == oct_mach_info::flt_fmt_ieee_big_endian);

  bool do_float_fmt_conv = ((input_type == oct_data_conv::dt_double
                             || input_type == oct_data_conv::dt_single)
                            && ffmt != float_format ());

  bool do_NA_conv = (output_type == oct_data_conv::dt_double);

  switch (output_type)
    {
    case oct_data_conv::dt_int8:
    case oct_data_conv::dt_uint8:
    case oct_data_conv::dt_int16:
    case oct_data_conv::dt_uint16:
    case oct_data_conv::dt_int32:
    case oct_data_conv::dt_uint32:
    case oct_data_conv::dt_int64:
    case oct_data_conv::dt_uint64:
    case oct_data_conv::dt_single:
    case oct_data_conv::dt_double:
    case oct_data_conv::dt_char:
    case oct_data_conv::dt_schar:
    case oct_data_conv::dt_uchar:
    case oct_data_conv::dt_logical:
      {
        conv_fptr fptr = conv_fptr_table[input_type][output_type];

        retval = fptr (input_buf_list, input_buf_elts, elts_read,
                       nr, nc, swap, do_float_fmt_conv, do_NA_conv, ffmt);
      }
      break;

    default:
      retval = false;
      (*current_liboctave_error_handler)
        ("read: invalid type specification");
      break;
    }


  return retval;
}

octave_value
octave_stream::read (const Array<double>& size, octave_idx_type block_size,
                     oct_data_conv::data_type input_type,
                     oct_data_conv::data_type output_type,
                     octave_idx_type skip, oct_mach_info::float_format ffmt,
                     octave_idx_type& count)
{
  octave_value retval;

  octave_idx_type nr = -1;
  octave_idx_type nc = -1;

  bool one_elt_size_spec = false;

  if (stream_ok ())
    {
      // FIXME: we may eventually want to make this extensible.

      // FIXME: we need a better way to ensure that this
      // numbering stays consistent with the order of the elements in the
      // data_type enum in the oct_data_conv class.

      // Expose this in a future version?
      octave_idx_type char_count = 0;

      count = 0;

      get_size (size, nr, nc, one_elt_size_spec, "fread");

      if (! error_state)
        {

          octave_idx_type elts_to_read;

          if (one_elt_size_spec)
            {
              // If NR == 0, Matlab returns [](0x0).

              // If NR > 0, the result will be a column vector with the given
              // number of rows.

              // If NR < 0, then we have Inf and the result will be a column
              // vector but we have to wait to see how big NR will be.

              if (nr == 0)
                nr = nc = 0;
              else
                nc = 1;
            }
          else
            {
              // Matlab returns [] even if there are two elements in the size
              // specification and one is nonzero.

              // If NC < 0 we have [NR, Inf] and we'll wait to decide how big NC
              // should be.

              if (nr == 0 || nc == 0)
                nr = nc = 0;
            }

          // FIXME: Ensure that this does not overflow.
          //        Maybe try comparing nr * nc computed in double with
          //        std::numeric_limits<octave_idx_type>::max ();

          elts_to_read = nr * nc;

          bool read_to_eof = elts_to_read < 0;

          octave_idx_type input_buf_elts = -1;

          if (skip == 0)
            {
              if (read_to_eof)
                input_buf_elts = 1024 * 1024;
              else
                input_buf_elts = elts_to_read;
            }
          else
            input_buf_elts = block_size;

          octave_idx_type input_elt_size
            = oct_data_conv::data_type_size (input_type);

          octave_idx_type input_buf_size = input_buf_elts * input_elt_size;

          assert (input_buf_size >= 0);

          // Must also work and return correct type object
          // for 0 elements to read.

          std::istream *isp = input_stream ();

          if (isp)
            {
              std::istream& is = *isp;

              std::list <void *> input_buf_list;

              while (is && ! is.eof ()
                     && (read_to_eof || count < elts_to_read))
                {
                  if (! read_to_eof)
                    {
                      octave_idx_type remaining_elts = elts_to_read - count;

                      if (remaining_elts < input_buf_elts)
                        input_buf_size = remaining_elts * input_elt_size;
                    }

                  char *input_buf = new char [input_buf_size];

                  is.read (input_buf, input_buf_size);

                  size_t gcount = is.gcount ();

                  char_count += gcount;

                  octave_idx_type nel = gcount / input_elt_size;

                  count += nel;

                  input_buf_list.push_back (input_buf);

                  if (is && skip != 0 && nel == block_size)
                    {
                      // Seek to skip.  If skip would move past EOF,
                      // position at EOF.

                      off_t orig_pos = tell ();

                      seek (0, SEEK_END);

                      off_t eof_pos = tell ();

                      // Is it possible for this to fail to return us to
                      // the original position?
                      seek (orig_pos, SEEK_SET);

                      off_t remaining = eof_pos - orig_pos;

                      if (remaining < skip)
                        seek (0, SEEK_END);
                      else
                        seek (skip, SEEK_CUR);

                      if (! is)
                        break;
                    }
                }

              if (read_to_eof)
                {
                  if (nc < 0)
                    {
                      nc = count / nr;

                      if (count % nr != 0)
                        nc ++;
                    }
                  else
                    nr = count;
                }
              else if (count == 0)
                {
                  nr = 0;
                  nc = 0;
                }
              else if (count != nr * nc)
                {
                  if (count % nr != 0)
                    nc = count / nr + 1;
                  else
                    nc = count / nr;

                  if (count < nr)
                    nr = count;
                }

              retval = finalize_read (input_buf_list, input_buf_elts, count,
                                      nr, nc, input_type, output_type, ffmt);
            }
          else
            error ("fread: invalid input stream");
        }
      else
        invalid_operation ("fread", "reading");
    }

  return retval;
}

octave_idx_type
octave_stream::write (const octave_value& data, octave_idx_type block_size,
                      oct_data_conv::data_type output_type,
                      octave_idx_type skip, oct_mach_info::float_format flt_fmt)
{
  octave_idx_type retval = -1;

  if (stream_ok ())
    {
      if (! error_state)
        {
          if (flt_fmt == oct_mach_info::flt_fmt_unknown)
            flt_fmt = float_format ();

          octave_idx_type status = data.write (*this, block_size, output_type,
                                               skip, flt_fmt);

          if (status < 0)
            error ("fwrite: write error");
          else
            retval = status;
        }
      else
        invalid_operation ("fwrite", "writing");
    }

  return retval;
}

template <class T, class V>
static void
convert_chars (const void *data, void *conv_data, octave_idx_type n_elts)
{
  const T *tt_data = static_cast<const T *> (data);

  V *vt_data = static_cast<V *> (conv_data);

  for (octave_idx_type i = 0; i < n_elts; i++)
    vt_data[i] = tt_data[i];
}

template <class T, class V>
static void
convert_ints (const T *data, void *conv_data, octave_idx_type n_elts,
              bool swap)
{
  typedef typename V::val_type val_type;

  val_type *vt_data = static_cast<val_type *> (conv_data);

  for (octave_idx_type i = 0; i < n_elts; i++)
    {
      // Yes, we want saturation semantics when converting to an integer
      // type.

      V val (data[i]);

      vt_data[i] = val.value ();

      if (swap)
        swap_bytes<sizeof (val_type)> (&vt_data[i]);
    }
}

template <class T>
class ultimate_element_type
{
public:
  typedef T type;
};

template <class T>
class ultimate_element_type<octave_int<T> >
{
public:
  typedef T type;
};

template <class T>
static bool
convert_data (const T *data, void *conv_data, octave_idx_type n_elts,
              oct_data_conv::data_type output_type,
              oct_mach_info::float_format flt_fmt)
{
  bool retval = true;

  bool swap
    = ((oct_mach_info::words_big_endian ()
        && flt_fmt == oct_mach_info::flt_fmt_ieee_little_endian)
       || flt_fmt == oct_mach_info::flt_fmt_ieee_big_endian);

  bool do_float_conversion =  flt_fmt != oct_mach_info::float_format ();

  typedef typename ultimate_element_type<T>::type ult_elt_type;

  switch (output_type)
    {
    case oct_data_conv::dt_char:
      convert_chars<ult_elt_type, char> (data, conv_data, n_elts);
      break;

    case oct_data_conv::dt_schar:
      convert_chars<ult_elt_type, signed char> (data, conv_data, n_elts);
      break;

    case oct_data_conv::dt_uchar:
      convert_chars<ult_elt_type, unsigned char> (data, conv_data, n_elts);
      break;

    case oct_data_conv::dt_int8:
      convert_ints<T, octave_int8> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint8:
      convert_ints<T, octave_uint8> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_int16:
      convert_ints<T, octave_int16> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint16:
      convert_ints<T, octave_uint16> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_int32:
      convert_ints<T, octave_int32> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint32:
      convert_ints<T, octave_uint32> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_int64:
      convert_ints<T, octave_int64> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_uint64:
      convert_ints<T, octave_uint64> (data, conv_data, n_elts, swap);
      break;

    case oct_data_conv::dt_single:
      {
        float *vt_data = static_cast<float *> (conv_data);

        for (octave_idx_type i = 0; i < n_elts; i++)
          {
            vt_data[i] = data[i];

            if (do_float_conversion)
              do_float_format_conversion (&vt_data[i], 1, flt_fmt);
          }
      }
      break;

    case oct_data_conv::dt_double:
      {
        double *vt_data = static_cast<double *> (conv_data);

        for (octave_idx_type i = 0; i < n_elts; i++)
          {
            vt_data[i] = data[i];

            if (do_float_conversion)
              do_double_format_conversion (&vt_data[i], 1, flt_fmt);
          }
      }
      break;

    default:
      retval = false;
      (*current_liboctave_error_handler)
        ("write: invalid type specification");
      break;
    }

  return retval;
}

bool
octave_stream::write_bytes (const void *data, size_t nbytes)
{
  bool status = false;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      if (os)
        {
          os.write (static_cast<const char *> (data), nbytes);

          if (os)
            status = true;
        }
    }

  return status;
}

bool
octave_stream::skip_bytes (size_t skip)
{
  bool status = false;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      // Seek to skip when inside bounds of existing file.
      // Otherwise, write NUL to skip.

      off_t orig_pos = tell ();

      seek (0, SEEK_END);

      off_t eof_pos = tell ();

      // Is it possible for this to fail to return us to the
      // original position?
      seek (orig_pos, SEEK_SET);

      size_t remaining = eof_pos - orig_pos;

      if (remaining < skip)
        {
          seek (0, SEEK_END);

          // FIXME: probably should try to write larger blocks...

          unsigned char zero = 0;
          for (size_t j = 0; j < skip - remaining; j++)
            os.write (reinterpret_cast<const char *> (&zero), 1);
        }
      else
        seek (skip, SEEK_CUR);

      if (os)
        status = true;
    }

  return status;
}

template <class T>
octave_idx_type
octave_stream::write (const Array<T>& data, octave_idx_type block_size,
                      oct_data_conv::data_type output_type,
                      octave_idx_type skip,
                      oct_mach_info::float_format flt_fmt)
{
  bool swap = ((oct_mach_info::words_big_endian ()
                && flt_fmt == oct_mach_info::flt_fmt_ieee_little_endian)
               || flt_fmt == oct_mach_info::flt_fmt_ieee_big_endian);

  bool do_data_conversion = (swap || ! is_equivalent_type<T> (output_type)
                             || flt_fmt != oct_mach_info::float_format ());

  octave_idx_type nel = data.numel ();

  octave_idx_type chunk_size;

  if (skip != 0)
    chunk_size = block_size;
  else if (do_data_conversion)
    chunk_size = 1024 * 1024;
  else
    chunk_size = nel;

  octave_idx_type i = 0;

  const T *pdata = data.data ();

  while (i < nel)
    {
      if (skip != 0)
        {
          if (! skip_bytes (skip))
            return -1;
        }

      octave_idx_type remaining_nel = nel - i;

      if (chunk_size > remaining_nel)
        chunk_size = remaining_nel;

      bool status = false;

      if (do_data_conversion)
        {
          size_t output_size
            = chunk_size * oct_data_conv::data_type_size (output_type);

          OCTAVE_LOCAL_BUFFER (unsigned char, conv_data, output_size);

          status = convert_data (&pdata[i], conv_data, chunk_size,
                                 output_type, flt_fmt);

          if (status)
            status = write_bytes (conv_data, output_size);
        }
      else
        status = write_bytes (pdata, sizeof (T) * chunk_size);

      if (! status)
        return -1;

      i += chunk_size;
    }

  return nel;
}

#define INSTANTIATE_WRITE(T) \
  template \
  octave_idx_type \
  octave_stream::write (const Array<T>& data, octave_idx_type block_size, \
                        oct_data_conv::data_type output_type, \
                        octave_idx_type skip, \
                        oct_mach_info::float_format flt_fmt)

INSTANTIATE_WRITE (octave_int8);
INSTANTIATE_WRITE (octave_uint8);
INSTANTIATE_WRITE (octave_int16);
INSTANTIATE_WRITE (octave_uint16);
INSTANTIATE_WRITE (octave_int32);
INSTANTIATE_WRITE (octave_uint32);
INSTANTIATE_WRITE (octave_int64);
INSTANTIATE_WRITE (octave_uint64);
INSTANTIATE_WRITE (int8_t);
INSTANTIATE_WRITE (uint8_t);
INSTANTIATE_WRITE (int16_t);
INSTANTIATE_WRITE (uint16_t);
INSTANTIATE_WRITE (int32_t);
INSTANTIATE_WRITE (uint32_t);
INSTANTIATE_WRITE (int64_t);
INSTANTIATE_WRITE (uint64_t);
INSTANTIATE_WRITE (bool);
#if defined (HAVE_OVERLOAD_CHAR_INT8_TYPES)
INSTANTIATE_WRITE (char);
#endif
INSTANTIATE_WRITE (float);
INSTANTIATE_WRITE (double);

octave_value
octave_stream::scanf (const std::string& fmt, const Array<double>& size,
                      octave_idx_type& count, const std::string& who)
{
  octave_value retval;

  if (stream_ok ())
    retval = rep->scanf (fmt, size, count, who);

  return retval;
}

octave_value
octave_stream::scanf (const octave_value& fmt, const Array<double>& size,
                      octave_idx_type& count, const std::string& who)
{
  octave_value retval = Matrix ();

  if (fmt.is_string ())
    {
      std::string sfmt = fmt.string_value ();

      if (fmt.is_sq_string ())
        sfmt = do_string_escapes (sfmt);

      retval = scanf (sfmt, size, count, who);
    }
  else
    {
      // Note that this is not ::error () !

      error (who + ": format must be a string");
    }

  return retval;
}

octave_value_list
octave_stream::oscanf (const std::string& fmt, const std::string& who)
{
  octave_value_list retval;

  if (stream_ok ())
    retval = rep->oscanf (fmt, who);

  return retval;
}

octave_value_list
octave_stream::oscanf (const octave_value& fmt, const std::string& who)
{
  octave_value_list retval;

  if (fmt.is_string ())
    {
      std::string sfmt = fmt.string_value ();

      if (fmt.is_sq_string ())
        sfmt = do_string_escapes (sfmt);

      retval = oscanf (sfmt, who);
    }
  else
    {
      // Note that this is not ::error () !

      error (who + ": format must be a string");
    }

  return retval;
}

int
octave_stream::printf (const std::string& fmt, const octave_value_list& args,
                       const std::string& who)
{
  int retval = -1;

  if (stream_ok ())
    retval = rep->printf (fmt, args, who);

  return retval;
}

int
octave_stream::printf (const octave_value& fmt, const octave_value_list& args,
                       const std::string& who)
{
  int retval = 0;

  if (fmt.is_string ())
    {
      std::string sfmt = fmt.string_value ();

      if (fmt.is_sq_string ())
        sfmt = do_string_escapes (sfmt);

      retval = printf (sfmt, args, who);
    }
  else
    {
      // Note that this is not ::error () !

      error (who + ": format must be a string");
    }

  return retval;
}

int
octave_stream::puts (const std::string& s, const std::string& who)
{
  int retval = -1;

  if (stream_ok ())
    retval = rep->puts (s, who);

  return retval;
}

// FIXME: maybe this should work for string arrays too.

int
octave_stream::puts (const octave_value& tc_s, const std::string& who)
{
  int retval = -1;

  if (tc_s.is_string ())
    {
      std::string s = tc_s.string_value ();
      retval = puts (s, who);
    }
  else
    {
      // Note that this is not ::error () !

      error (who + ": argument must be a string");
    }

  return retval;
}

bool
octave_stream::eof (void) const
{
  int retval = -1;

  if (stream_ok ())
    retval = rep->eof ();

  return retval;
}

std::string
octave_stream::error (bool clear, int& err_num)
{
  std::string retval = "invalid stream object";

  if (stream_ok (false))
    retval = rep->error (clear, err_num);

  return retval;
}

std::string
octave_stream::name (void) const
{
  std::string retval;

  if (stream_ok ())
    retval = rep->name ();

  return retval;
}

int
octave_stream::mode (void) const
{
  int retval = 0;

  if (stream_ok ())
    retval = rep->mode ();

  return retval;
}

oct_mach_info::float_format
octave_stream::float_format (void) const
{
  oct_mach_info::float_format retval = oct_mach_info::flt_fmt_unknown;

  if (stream_ok ())
    retval = rep->float_format ();

  return retval;
}

std::string
octave_stream::mode_as_string (int mode)
{
  std::string retval = "???";
  std::ios::openmode in_mode = static_cast<std::ios::openmode> (mode);

  if (in_mode == std::ios::in)
    retval = "r";
  else if (in_mode == std::ios::out
           || in_mode == (std::ios::out | std::ios::trunc))
    retval = "w";
  else if (in_mode == (std::ios::out | std::ios::app))
    retval = "a";
  else if (in_mode == (std::ios::in | std::ios::out))
    retval = "r+";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::trunc))
    retval = "w+";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::ate))
    retval = "a+";
  else if (in_mode == (std::ios::in | std::ios::binary))
    retval = "rb";
  else if (in_mode == (std::ios::out | std::ios::binary)
           || in_mode == (std::ios::out | std::ios::trunc | std::ios::binary))
    retval = "wb";
  else if (in_mode == (std::ios::out | std::ios::app | std::ios::binary))
    retval = "ab";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::binary))
    retval = "r+b";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::trunc
                       | std::ios::binary))
    retval = "w+b";
  else if (in_mode == (std::ios::in | std::ios::out | std::ios::ate
                       | std::ios::binary))
    retval = "a+b";

  return retval;
}

octave_stream_list *octave_stream_list::instance = 0;

bool
octave_stream_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_stream_list ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create stream list object!");

      retval = false;
    }

  return retval;
}

int
octave_stream_list::insert (octave_stream& os)
{
  return (instance_ok ()) ? instance->do_insert (os) : -1;
}

octave_stream
octave_stream_list::lookup (int fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_lookup (fid, who) : octave_stream ();
}

octave_stream
octave_stream_list::lookup (const octave_value& fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_lookup (fid, who) : octave_stream ();
}

int
octave_stream_list::remove (int fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_remove (fid, who) : -1;
}

int
octave_stream_list::remove (const octave_value& fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_remove (fid, who) : -1;
}

void
octave_stream_list::clear (bool flush)
{
  if (instance)
    instance->do_clear (flush);
}

string_vector
octave_stream_list::get_info (int fid)
{
  return (instance_ok ()) ? instance->do_get_info (fid) : string_vector ();
}

string_vector
octave_stream_list::get_info (const octave_value& fid)
{
  return (instance_ok ()) ? instance->do_get_info (fid) : string_vector ();
}

std::string
octave_stream_list::list_open_files (void)
{
  return (instance_ok ()) ? instance->do_list_open_files () : std::string ();
}

octave_value
octave_stream_list::open_file_numbers (void)
{
  return (instance_ok ())
         ? instance->do_open_file_numbers () : octave_value ();
}

int
octave_stream_list::get_file_number (const octave_value& fid)
{
  return (instance_ok ()) ? instance->do_get_file_number (fid) : -1;
}

int
octave_stream_list::do_insert (octave_stream& os)
{
  // Insert item with key corresponding to file-descriptor.

  int stream_number;

  if ((stream_number = os.file_number ()) == -1)
    return stream_number;

  // Should we test for
  //
  //  (list.find (stream_number) != list.end ()
  //   && list[stream_number].is_open ())
  //
  // and respond with "error ("internal error: ...")"?  It should not
  // happen except for some bug or if the user has opened a stream with
  // an interpreted command, but closed it directly with a system call
  // in an oct-file; then the kernel knows the fd is free, but Octave
  // does not know.  If it happens, it should not do harm here to simply
  // overwrite this entry, although the wrong entry might have done harm
  // before.

  if (list.size () < list.max_size ())
    list[stream_number] = os;
  else
    {
      stream_number = -1;
      error ("could not create file id");
    }

  return stream_number;

}

static void
gripe_invalid_file_id (int fid, const std::string& who)
{
  if (who.empty ())
    ::error ("invalid stream number = %d", fid);
  else
    ::error ("%s: invalid stream number = %d", who.c_str (), fid);
}

octave_stream
octave_stream_list::do_lookup (int fid, const std::string& who) const
{
  octave_stream retval;

  if (fid >= 0)
    {
      if (lookup_cache != list.end () && lookup_cache->first == fid)
        retval = lookup_cache->second;
      else
        {
          ostrl_map::const_iterator iter = list.find (fid);

          if (iter != list.end ())
            {
              retval = iter->second;
              lookup_cache = iter;
            }
          else
            gripe_invalid_file_id (fid, who);
        }
    }
  else
    gripe_invalid_file_id (fid, who);

  return retval;
}

octave_stream
octave_stream_list::do_lookup (const octave_value& fid,
                               const std::string& who) const
{
  octave_stream retval;

  int i = get_file_number (fid);

  if (! error_state)
    retval = do_lookup (i, who);

  return retval;
}

int
octave_stream_list::do_remove (int fid, const std::string& who)
{
  int retval = -1;

  // Can't remove stdin (std::cin), stdout (std::cout), or stderr
  // (std::cerr).

  if (fid > 2)
    {
      ostrl_map::iterator iter = list.find (fid);

      if (iter != list.end ())
        {
          octave_stream os = iter->second;
          list.erase (iter);
          lookup_cache = list.end ();

          // FIXME: is this check redundant?
          if (os.is_valid ())
            {
              os.close ();
              retval = 0;
            }
          else
            gripe_invalid_file_id (fid, who);
        }
      else
        gripe_invalid_file_id (fid, who);
    }
  else
    gripe_invalid_file_id (fid, who);

  return retval;
}

int
octave_stream_list::do_remove (const octave_value& fid, const std::string& who)
{
  int retval = -1;

  if (fid.is_string () && fid.string_value () == "all")
    {
      do_clear (false);

      retval = 0;
    }
  else
    {
      int i = get_file_number (fid);

      if (! error_state)
        retval = do_remove (i, who);
    }

  return retval;
}

void
octave_stream_list::do_clear (bool flush)
{
  if (flush)
    {
      // Do flush stdout and stderr.

      list[0].flush ();
      list[1].flush ();
    }

  octave_stream saved_os[3];
  // But don't delete them or stdin.
  for (ostrl_map::iterator iter = list.begin (); iter != list.end (); iter++)
    {
      int fid = iter->first;
      octave_stream os = iter->second;
      if (fid < 3)
        saved_os[fid] = os;
      else if (os.is_valid ())
        os.close ();
    }
  list.clear ();
  for (int fid = 0; fid < 3; fid++) list[fid] = saved_os[fid];
  lookup_cache = list.end ();
}

string_vector
octave_stream_list::do_get_info (int fid) const
{
  string_vector retval;

  octave_stream os = do_lookup (fid);

  if (os.is_valid ())
    {
      retval.resize (3);

      retval(2) = oct_mach_info::float_format_as_string (os.float_format ());
      retval(1) = octave_stream::mode_as_string (os.mode ());
      retval(0) = os.name ();
    }
  else
    ::error ("invalid file id = %d", fid);

  return retval;
}

string_vector
octave_stream_list::do_get_info (const octave_value& fid) const
{
  string_vector retval;

  int conv_err = 0;

  int int_fid = convert_to_valid_int (fid, conv_err);

  if (! conv_err)
    retval = do_get_info (int_fid);
  else
    ::error ("file id must be a file object or integer value");

  return retval;
}

std::string
octave_stream_list::do_list_open_files (void) const
{
  std::string retval;

  std::ostringstream buf;

  buf << "\n"
      << "  number  mode  arch       name\n"
      << "  ------  ----  ----       ----\n";

  for (ostrl_map::const_iterator p = list.begin (); p != list.end (); p++)
    {
      octave_stream os = p->second;

      buf << "  "
          << std::setiosflags (std::ios::right)
          << std::setw (4) << p->first << "     "
          // reset necessary in addition to setiosflags since this is one stmt.
          << std::resetiosflags (std::ios::adjustfield)
          << std::setiosflags (std::ios::left)
          << std::setw (3)
          << octave_stream::mode_as_string (os.mode ())
          << "  "
          << std::setw (9)
          << oct_mach_info::float_format_as_string (os.float_format ())
          << "  "
          << os.name () << "\n";
    }

  buf << "\n";

  retval = buf.str ();

  return retval;
}

octave_value
octave_stream_list::do_open_file_numbers (void) const
{
  Matrix retval (1, list.size (), 0.0);

  int num_open = 0;

  for (ostrl_map::const_iterator p = list.begin (); p != list.end (); p++)
    {
      // Skip stdin, stdout, and stderr.

      if (p->first > 2 && p->second)
        retval(0,num_open++) = p->first;
    }

  retval.resize ((num_open > 0), num_open);

  return retval;
}

int
octave_stream_list::do_get_file_number (const octave_value& fid) const
{
  int retval = -1;

  if (fid.is_string ())
    {
      std::string nm = fid.string_value ();

      for (ostrl_map::const_iterator p = list.begin (); p != list.end (); p++)
        {
          // stdin (std::cin), stdout (std::cout), and stderr (std::cerr)
          // are unnamed.

          if (p->first > 2)
            {
              octave_stream os = p->second;

              if (os && os.name () == nm)
                {
                  retval = p->first;
                  break;
                }
            }
        }
    }
  else
    {
      int conv_err = 0;

      int int_fid = convert_to_valid_int (fid, conv_err);

      if (conv_err)
        ::error ("file id must be a file object, std::string, or integer value");
      else
        retval = int_fid;
    }

  return retval;
}
