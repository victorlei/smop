/*

Copyright (C) 1994-2015 John W. Eaton

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

// Author: John W. Eaton.
// HDF5 support by Steven G. Johnson <stevenj@alum.mit.edu>
// Matlab v5 support by James R. Van Zandt <jrv@vanzandt.mv.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "strftime.h"

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "file-stat.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"
#include "oct-locbuf.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-path.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

#include "ls-hdf5.h"
#include "ls-mat-ascii.h"
#include "ls-mat4.h"
#include "ls-mat5.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"

// Remove gnulib definitions, if any.
#ifdef close
#undef close
#endif
#ifdef open
#undef open
#endif

#ifdef HAVE_ZLIB
#include "zfstream.h"
#endif

// Write octave-workspace file if Octave crashes or is killed by a signal.
static bool Vcrash_dumps_octave_core = true;

// The maximum amount of memory (in kilobytes) that we will attempt to
// write to the Octave core file.
static double Voctave_core_file_limit = -1.0;

// The name of the Octave core file.
static std::string Voctave_core_file_name = "octave-workspace";

// The default output format.  May be one of "binary", "text",
// "mat-binary", or "hdf5".
static std::string Vsave_default_options = "-text";

// The output format for Octave core files.
static std::string Voctave_core_file_options = "-binary";

static std::string
default_save_header_format (void)
{
  return
    std::string ("# Created by Octave " OCTAVE_VERSION
                 ", %a %b %d %H:%M:%S %Y %Z <")
    + octave_env::get_user_name ()
    + std::string ("@")
    + octave_env::get_host_name ()
    + std::string (">");
}

// The format string for the comment line at the top of text-format
// save files.  Passed to strftime.  Should begin with '#' and contain
// no newline characters.
static std::string Vsave_header_format_string = default_save_header_format ();

static void
gripe_file_open (const std::string& fcn, const std::string& file)
{
  if (fcn == "load")
    error ("%s: unable to open input file '%s'", fcn.c_str (), file.c_str ());
  else if (fcn == "save")
    error ("%s: unable to open output file '%s'", fcn.c_str (), file.c_str ());
  else
    error ("%s: unable to open file '%s'", fcn.c_str (), file.c_str ());
}

// Install a variable with name NAME and the value VAL in the
// symbol table.  If GLOBAL is TRUE, make the variable global.

static void
install_loaded_variable (const std::string& name,
                         const octave_value& val,
                         bool global, const std::string& /*doc*/)
{
  if (global)
    {
      symbol_table::clear (name);
      symbol_table::mark_global (name);
      symbol_table::global_assign (name, val);
    }
  else
    symbol_table::assign (name, val);
}

// Return TRUE if NAME matches one of the given globbing PATTERNS.

static bool
matches_patterns (const string_vector& patterns, int pat_idx,
                  int num_pat, const std::string& name)
{
  for (int i = pat_idx; i < num_pat; i++)
    {
      glob_match pattern (patterns[i]);

      if (pattern.match (name))
        return true;
    }

  return false;
}

int
read_binary_file_header (std::istream& is, bool& swap,
                         oct_mach_info::float_format& flt_fmt, bool quiet)
{
  const int magic_len = 10;
  char magic[magic_len+1];
  is.read (magic, magic_len);
  magic[magic_len] = '\0';

  if (strncmp (magic, "Octave-1-L", magic_len) == 0)
    swap = oct_mach_info::words_big_endian ();
  else if (strncmp (magic, "Octave-1-B", magic_len) == 0)
    swap = ! oct_mach_info::words_big_endian ();
  else
    {
      if (! quiet)
        error ("load: unable to read read binary file");
      return -1;
    }

  char tmp = 0;
  is.read (&tmp, 1);

  flt_fmt = mopt_digit_to_float_format (tmp);

  if (flt_fmt == oct_mach_info::flt_fmt_unknown)
    {
      if (! quiet)
        error ("load: unrecognized binary format!");

      return -1;
    }

  return 0;
}

#ifdef HAVE_ZLIB
static bool
check_gzip_magic (const std::string& fname)
{
  bool retval = false;
  std::ifstream file (fname.c_str ());
  OCTAVE_LOCAL_BUFFER (unsigned char, magic, 2);

  if (file.read (reinterpret_cast<char *> (magic), 2) && magic[0] == 0x1f
      && magic[1] == 0x8b)
    retval = true;

  file.close ();
  return retval;
}
#endif

static load_save_format
get_file_format (std::istream& file, const std::string& filename)
{
  load_save_format retval = LS_UNKNOWN;

  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;

  bool swap = false;

  if (read_binary_file_header (file, swap, flt_fmt, true) == 0)
    retval = LS_BINARY;
  else
    {
      file.clear ();
      file.seekg (0, std::ios::beg);

      int32_t mopt, nr, nc, imag, len;

      int err = read_mat_file_header (file, swap, mopt, nr, nc, imag, len,
                                      true);

      if (! err)
        retval = LS_MAT_BINARY;
      else
        {
          file.clear ();
          file.seekg (0, std::ios::beg);

          err = read_mat5_binary_file_header (file, swap, true, filename);

          if (! err)
            {
              file.clear ();
              file.seekg (0, std::ios::beg);
              retval = LS_MAT5_BINARY;
            }
          else
            {
              file.clear ();
              file.seekg (0, std::ios::beg);

              std::string name_val = extract_keyword (file, "name");
              std::string type_val = extract_keyword (file, "type");

              if (name_val.empty () != true && type_val.empty () != true)
                retval = LS_ASCII;
              else
                {
                  file.clear ();
                  file.seekg (0, std::ios::beg);

                  // FIXME: looks_like_mat_ascii_file does not check to see
                  // whether the file contains numbers.  It just skips comments
                  // and checks for the same number of words on each line.  We
                  // may need a better check here.  The best way to do that
                  // might be just to try to read the file and see if it works.

                  if (looks_like_mat_ascii_file (file, filename))
                    retval = LS_MAT_ASCII;
                }
            }
        }
    }

  return retval;
}

static load_save_format
get_file_format (const std::string& fname, const std::string& orig_fname,
                 bool &use_zlib, bool quiet = false)
{
  load_save_format retval = LS_UNKNOWN;

#ifdef HAVE_HDF5
  // check this before we open the file
  if (H5Fis_hdf5 (fname.c_str ()) > 0)
    return LS_HDF5;
#endif /* HAVE_HDF5 */

#ifdef HAVE_ZLIB
  use_zlib = check_gzip_magic (fname);
#else
  use_zlib = false;
#endif

  if (! use_zlib)
    {
      std::ifstream file (fname.c_str ());
      if (file)
        {
          retval = get_file_format (file, orig_fname);
          file.close ();
        }
      else if (! quiet)
        gripe_file_open ("load", orig_fname);
    }
#ifdef HAVE_ZLIB
  else
    {
      gzifstream gzfile (fname.c_str ());
      if (gzfile)
        {
          retval = get_file_format (gzfile, orig_fname);
          gzfile.close ();
        }
      else if (! quiet)
        gripe_file_open ("load", orig_fname);
    }
#endif

  return retval;
}

octave_value
do_load (std::istream& stream, const std::string& orig_fname,
         load_save_format format, oct_mach_info::float_format flt_fmt,
         bool list_only, bool swap, bool verbose,
         const string_vector& argv, int argv_idx, int argc, int nargout)
{
  octave_value retval;

  octave_scalar_map retstruct;

  std::ostringstream output_buf;
  std::list<std::string> symbol_names;

  octave_idx_type count = 0;

  for (;;)
    {
      bool global = false;
      octave_value tc;

      std::string name;
      std::string doc;

      switch (format.type)
        {
        case LS_ASCII:
          name = read_ascii_data (stream, orig_fname, global, tc, count);
          break;

        case LS_BINARY:
          name = read_binary_data (stream, swap, flt_fmt, orig_fname,
                                   global, tc, doc);
          break;

        case LS_MAT_ASCII:
          name = read_mat_ascii_data (stream, orig_fname, tc);
          break;

        case LS_MAT_BINARY:
          name = read_mat_binary_data (stream, orig_fname, tc);
          break;

#ifdef HAVE_HDF5
        case LS_HDF5:
          name = read_hdf5_data (stream, orig_fname, global, tc, doc,
                                 argv, argv_idx, argc);
          break;
#endif /* HAVE_HDF5 */

        case LS_MAT5_BINARY:
        case LS_MAT7_BINARY:
          name = read_mat5_binary_element (stream, orig_fname, swap,
                                           global, tc);
          break;

        default:
          gripe_unrecognized_data_fmt ("load");
          break;
        }

      if (error_state || stream.eof () || name.empty ())
        break;
      else if (! error_state && ! name.empty ())
        {
          if (tc.is_defined ())
            {
              if (format == LS_MAT_ASCII && argv_idx < argc)
                warning ("load: loaded ASCII file '%s' -- ignoring extra args",
                         orig_fname.c_str ());

              if (format == LS_MAT_ASCII
                  || argv_idx == argc
                  || matches_patterns (argv, argv_idx, argc, name))
                {
                  count++;
                  if (list_only)
                    {
                      if (verbose)
                        {
                          if (count == 1)
                            output_buf
                              << "type               rows   cols   name\n"
                              << "====               ====   ====   ====\n";

                          output_buf
                            << std::setiosflags (std::ios::left)
                            << std::setw (16) << tc.type_name () . c_str ()
                            << std::setiosflags (std::ios::right)
                            << std::setw (7) << tc.rows ()
                            << std::setw (7) << tc.columns ()
                            << "   " << name << "\n";
                        }
                      else
                        symbol_names.push_back (name);
                    }
                  else
                    {
                      if (nargout == 1)
                        {
                          if (format == LS_MAT_ASCII)
                            retval = tc;
                          else
                            retstruct.assign (name, tc);
                        }
                      else
                        install_loaded_variable (name, tc, global, doc);
                    }
                }

              // Only attempt to read one item from a headless text file.

              if (format == LS_MAT_ASCII)
                break;
            }
          else
            error ("load: unable to load variable '%s'", name.c_str ());
        }
      else
        {
          if (count == 0)
            error ("load: are you sure '%s' is an Octave data file?",
                   orig_fname.c_str ());

          break;
        }
    }

  if (list_only && count)
    {
      if (verbose)
        {
          std::string msg = output_buf.str ();

          if (nargout > 0)
            retval = msg;
          else
            octave_stdout << msg;
        }
      else
        {
          if (nargout  > 0)
            retval = Cell (string_vector (symbol_names));
          else
            {
              string_vector names (symbol_names);

              names.list_in_columns (octave_stdout);

              octave_stdout << "\n";
            }
        }
    }
  else if (retstruct.nfields () != 0)
    retval = retstruct;

  return retval;
}

std::string
find_file_to_load (const std::string& name, const std::string& orig_name)
{
  std::string fname = find_data_file_in_load_path ("load", name, true);

  size_t dot_pos = fname.rfind (".");
  size_t sep_pos = fname.find_last_of (file_ops::dir_sep_chars ());

  if (dot_pos == std::string::npos
      || (sep_pos != std::string::npos && dot_pos < sep_pos))
    {
      // Either no '.' in name or no '.' appears after last directory
      // separator.

      file_stat fs (fname);

      if (! (fs.exists () && fs.is_reg ()))
        fname = find_file_to_load (fname + ".mat", orig_name);
    }
  else
    {
      file_stat fs (fname);

      if (! (fs.exists () && fs.is_reg ()))
        {
          fname = "";

          error ("load: unable to find file %s", orig_name.c_str ());
        }
    }

  return fname;
}

bool
is_octave_data_file (const std::string& fname)
{
  bool use_zlib = false;
  return get_file_format (fname, fname, use_zlib, true) != LS_UNKNOWN;
}

DEFUN (load, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} load file\n\
@deftypefnx {Command} {} load options file\n\
@deftypefnx {Command} {} load options file v1 v2 @dots{}\n\
@deftypefnx {Command} {S =} load (\"options\", \"file\", \"v1\", \"v2\", @dots{})\n\
@deftypefnx {Command} {} load file options\n\
@deftypefnx {Command} {} load file options v1 v2 @dots{}\n\
@deftypefnx {Command} {S =} load (\"file\", \"options\", \"v1\", \"v2\", @dots{})\n\
Load the named variables @var{v1}, @var{v2}, @dots{}, from the file\n\
@var{file}.\n\
\n\
If no variables are specified then all variables found in the\n\
file will be loaded.  As with @code{save}, the list of variables to extract\n\
can be full names or use a pattern syntax.  The format of the file is\n\
automatically detected but may be overridden by supplying the appropriate\n\
option.\n\
\n\
If load is invoked using the functional form\n\
\n\
@example\n\
load (\"-option1\", @dots{}, \"file\", \"v1\", @dots{})\n\
@end example\n\
\n\
@noindent\n\
then the @var{options}, @var{file}, and variable name arguments\n\
(@var{v1}, @dots{}) must be specified as character strings.\n\
\n\
If a variable that is not marked as global is loaded from a file when a\n\
global symbol with the same name already exists, it is loaded in the\n\
global symbol table.  Also, if a variable is marked as global in a file\n\
and a local symbol exists, the local symbol is moved to the global\n\
symbol table and given the value from the file.\n\
\n\
If invoked with a single output argument, Octave returns data instead\n\
of inserting variables in the symbol table.  If the data file contains\n\
only numbers (TAB- or space-delimited columns), a matrix of values is\n\
returned.  Otherwise, @code{load} returns a structure with members\n\
 corresponding to the names of the variables in the file.\n\
\n\
The @code{load} command can read data stored in Octave's text and\n\
binary formats, and @sc{matlab}'s binary format.  If compiled with zlib\n\
support, it can also load gzip-compressed files.  It will automatically\n\
detect the type of file and do conversion from different floating point\n\
formats (currently only IEEE big and little endian, though other formats\n\
may be added in the future).\n\
\n\
Valid options for @code{load} are listed in the following table.\n\
\n\
@table @code\n\
@item -force\n\
This option is accepted for backward compatibility but is ignored.\n\
Octave now overwrites variables currently in memory with\n\
those of the same name found in the file.\n\
\n\
@item -ascii\n\
Force Octave to assume the file contains columns of numbers in text format\n\
without any header or other information.  Data in the file will be loaded\n\
as a single numeric matrix with the name of the variable derived from the\n\
name of the file.\n\
\n\
@item -binary\n\
Force Octave to assume the file is in Octave's binary format.\n\
\n\
@item -hdf5\n\
Force Octave to assume the file is in @sc{hdf5} format.\n\
(@sc{hdf5} is a free, portable binary format developed by the National\n\
Center for Supercomputing Applications at the University of Illinois.)\n\
Note that Octave can read @sc{hdf5} files not created by itself, but may\n\
skip some datasets in formats that it cannot support.  This format is\n\
only available if Octave was built with a link to the @sc{hdf5} libraries.\n\
\n\
@item -import\n\
This option is accepted for backward compatibility but is ignored.\n\
Octave can now support multi-dimensional HDF data and automatically\n\
modifies variable names if they are invalid Octave identifiers.\n\
\n\
@item  -mat\n\
@itemx -mat-binary\n\
@itemx -6\n\
@itemx -v6\n\
@itemx -7\n\
@itemx -v7\n\
Force Octave to assume the file is in @sc{matlab}'s version 6 or 7 binary\n\
format.\n\
\n\
@item  -mat4-binary\n\
@itemx -4\n\
@itemx -v4\n\
@itemx -V4\n\
Force Octave to assume the file is in the binary format written by\n\
@sc{matlab} version 4.\n\
\n\
@item -text\n\
Force Octave to assume the file is in Octave's text format.\n\
@end table\n\
@seealso{save, dlmwrite, csvwrite, fwrite}\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("load");

  if (error_state)
    return retval;

  int i = 1;
  std::string orig_fname = "";

  // Function called with Matlab-style ["filename", options] syntax
  if (argc > 1 && ! argv[1].empty () && argv[1].at (0) != '-')
    {
      orig_fname = argv[1];
      i++;
    }

  // It isn't necessary to have the default load format stored in a
  // user preference variable since we can determine the type of file
  // as we are reading.

  load_save_format format = LS_UNKNOWN;

  bool list_only = false;
  bool verbose = false;

  //for (i; i < argc; i++)
  for (; i < argc; i++)
    {
      if (argv[i] == "-force" || argv[i] == "-f")
        {
          // Silently ignore this
          // warning ("load: -force ignored");
        }
      else if (argv[i] == "-list" || argv[i] == "-l")
        {
          list_only = true;
        }
      else if (argv[i] == "-verbose" || argv[i] == "-v")
        {
          verbose = true;
        }
      else if (argv[i] == "-ascii" || argv[i] == "-a")
        {
          format = LS_MAT_ASCII;
        }
      else if (argv[i] == "-binary" || argv[i] == "-b")
        {
          format = LS_BINARY;
        }
      else if (argv[i] == "-mat-binary" || argv[i] == "-mat" || argv[i] == "-m"
               || argv[i] == "-6" || argv[i] == "-v6")
        {
          format = LS_MAT5_BINARY;
        }
      else if (argv[i] == "-7" || argv[i] == "-v7")
        {
          format = LS_MAT7_BINARY;
        }
      else if (argv[i] == "-mat4-binary" || argv[i] == "-V4"
               || argv[i] == "-v4" || argv[i] == "-4")
        {
          format = LS_MAT_BINARY;
        }
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
        {
#ifdef HAVE_HDF5
          format = LS_HDF5;
#else /* ! HAVE_HDF5 */
          error ("load: octave executable was not linked with HDF5 library");
          return retval;
#endif /* ! HAVE_HDF5 */
        }
      else if (argv[i] == "-import" || argv[i] == "-i")
        {
          warning ("load: -import ignored");
        }
      else if (argv[i] == "-text" || argv[i] == "-t")
        {
          format = LS_ASCII;
        }
      else
        break;
    }

  if (orig_fname == "")
    {
      if (i == argc)
        {
          print_usage ();
          return retval;
        }
      else
        orig_fname = argv[i];
    }
  else
    i--;

  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_unknown;

  bool swap = false;

  if (orig_fname == "-")
    {
      i++;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        error ("load: cannot read HDF5 format from stdin");
      else
#endif /* HAVE_HDF5 */
      if (format != LS_UNKNOWN)
        {
          // FIXME: if we have already seen EOF on a previous call,
          // how do we fix up the state of std::cin so that we can get
          // additional input?  I'm afraid that we can't fix this
          // using std::cin only.

          retval = do_load (std::cin, orig_fname, format, flt_fmt,
                            list_only, swap, verbose, argv, i, argc,
                            nargout);
        }
      else
        error ("load: must specify file format if reading from stdin");
    }
  else
    {
      std::string fname = file_ops::tilde_expand (orig_fname);

      fname = find_file_to_load (fname, orig_fname);

      if (error_state)
        return retval;

      bool use_zlib = false;

      if (format == LS_UNKNOWN)
        format = get_file_format (fname, orig_fname, use_zlib);

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        {
          i++;

          hdf5_ifstream hdf5_file (fname.c_str ());

          if (hdf5_file.file_id >= 0)
            {
              retval = do_load (hdf5_file, orig_fname, format,
                                flt_fmt, list_only, swap, verbose,
                                argv, i, argc, nargout);

              hdf5_file.close ();
            }
          else
            gripe_file_open ("load", orig_fname);
        }
      else
#endif /* HAVE_HDF5 */
        // don't insert any statements here; the "else" above has to
        // go with the "if" below!!!!!
      if (format != LS_UNKNOWN)
        {
          i++;

          // Always open in binary mode and handle various
          // line-endings explicitly.
          std::ios::openmode mode = std::ios::in | std::ios::binary;

#ifdef HAVE_ZLIB
          if (use_zlib)
            {
              gzifstream file (fname.c_str (), mode);

              if (file)
                {
                  if (format == LS_BINARY)
                    {
                      if (read_binary_file_header (file, swap, flt_fmt) < 0)
                        {
                          if (file) file.close ();
                          return retval;
                        }
                    }
                  else if (format == LS_MAT5_BINARY
                           || format == LS_MAT7_BINARY)
                    {
                      if (read_mat5_binary_file_header (file, swap, false,
                                                        orig_fname) < 0)
                        {
                          if (file) file.close ();
                          return retval;
                        }
                    }

                  retval = do_load (file, orig_fname, format,
                                    flt_fmt, list_only, swap, verbose,
                                argv, i, argc, nargout);

                  file.close ();
                }
              else
                gripe_file_open ("load", orig_fname);
            }
          else
#endif
            {
              std::ifstream file (fname.c_str (), mode);

              if (file)
                {
                  if (format == LS_BINARY)
                    {
                      if (read_binary_file_header (file, swap, flt_fmt) < 0)
                        {
                          if (file) file.close ();
                          return retval;
                        }
                    }
                  else if (format == LS_MAT5_BINARY
                           || format == LS_MAT7_BINARY)
                    {
                      if (read_mat5_binary_file_header (file, swap, false,
                                                        orig_fname) < 0)
                        {
                          if (file) file.close ();
                          return retval;
                        }
                    }

                  retval = do_load (file, orig_fname, format,
                                    flt_fmt, list_only, swap, verbose,
                                    argv, i, argc, nargout);

                  file.close ();
                }
              else
                error ("load: unable to open input file '%s'",
                       orig_fname.c_str ());
            }
        }
      else
        {
          error ("load: unable to determine file format of '%s'",
                 orig_fname.c_str ());
        }
    }

  return retval;
}

// Return TRUE if PATTERN has any special globbing chars in it.

static bool
glob_pattern_p (const std::string& pattern)
{
  int open = 0;

  int len = pattern.length ();

  for (int i = 0; i < len; i++)
    {
      char c = pattern[i];

      switch (c)
        {
        case '?':
        case '*':
          return true;

        case '[':       // Only accept an open brace if there is a close
          open++;       // brace to match it.  Bracket expressions must be
          continue;     // complete, according to Posix.2

        case ']':
          if (open)
            return true;
          continue;

        case '\\':
          if (i == len - 1)
            return false;

        default:
          continue;
        }
    }

  return false;
}

static void
do_save (std::ostream& os, const octave_value& tc,
         const std::string& name, const std::string& help,
         bool global, load_save_format fmt, bool save_as_floats)
{
  switch (fmt.type)
    {
    case LS_ASCII:
      save_ascii_data (os, tc, name, global, 0);
      break;

    case LS_BINARY:
      save_binary_data (os, tc, name, help, global, save_as_floats);
      break;

    case LS_MAT_ASCII:
      if (! save_mat_ascii_data (os, tc, fmt.opts & LS_MAT_ASCII_LONG ? 16 : 8,
                                 fmt.opts & LS_MAT_ASCII_TABS))
        warning ("save: unable to save %s in ASCII format", name.c_str ());
      break;

    case LS_MAT_BINARY:
      save_mat_binary_data (os, tc, name);
      break;

#ifdef HAVE_HDF5
    case LS_HDF5:
      save_hdf5_data (os, tc, name, help, global, save_as_floats);
      break;
#endif /* HAVE_HDF5 */

    case LS_MAT5_BINARY:
      save_mat5_binary_element (os, tc, name, global, false, save_as_floats);
      break;

    case LS_MAT7_BINARY:
      save_mat5_binary_element (os, tc, name, global, true, save_as_floats);
      break;

    default:
      gripe_unrecognized_data_fmt ("save");
      break;
    }
}

// Save the info from SR on stream OS in the format specified by FMT.

void
do_save (std::ostream& os, const symbol_table::symbol_record& sr,
         load_save_format fmt, bool save_as_floats)
{
  octave_value val = sr.varval ();

  if (val.is_defined ())
    {
      std::string name = sr.name ();
      std::string help;
      bool global = sr.is_global ();

      do_save (os, val, name, help, global, fmt, save_as_floats);
    }
}

// save fields of a scalar structure STR matching PATTERN on stream OS
// in the format specified by FMT.

static size_t
save_fields (std::ostream& os, const octave_scalar_map& m,
             const std::string& pattern,
             load_save_format fmt, bool save_as_floats)
{
  glob_match pat (pattern);

  size_t saved = 0;

  for (octave_scalar_map::const_iterator p = m.begin (); p != m.end (); p++)
    {
      std::string empty_str;

      if (pat.match (m.key (p)))
        {
          do_save (os, m.contents (p), m.key (p), empty_str,
                   0, fmt, save_as_floats);

          saved++;
        }
    }

  return saved;
}

// Save variables with names matching PATTERN on stream OS in the
// format specified by FMT.

static size_t
save_vars (std::ostream& os, const std::string& pattern,
           load_save_format fmt, bool save_as_floats)
{
  std::list<symbol_table::symbol_record> vars = symbol_table::glob (pattern);

  size_t saved = 0;

  typedef std::list<symbol_table::symbol_record>::const_iterator
    const_vars_iterator;

  for (const_vars_iterator p = vars.begin (); p != vars.end (); p++)
    {
      do_save (os, *p, fmt, save_as_floats);

      if (error_state)
        break;

      saved++;
    }

  return saved;
}

static string_vector
parse_save_options (const string_vector &argv,
                    load_save_format &format, bool &append,
                    bool &save_as_floats, bool &use_zlib)
{
  string_vector retval;
  int argc = argv.length ();

  bool do_double = false;
  bool do_tabs = false;

  for (int i = 0; i < argc; i++)
    {
      if (argv[i] == "-append")
        {
          append = true;
        }
      else if (argv[i] == "-ascii" || argv[i] == "-a")
        {
          format = LS_MAT_ASCII;
        }
      else if (argv[i] == "-double")
        {
          do_double = true;
        }
      else if (argv[i] == "-tabs")
        {
          do_tabs = true;
        }
      else if (argv[i] == "-text" || argv[i] == "-t")
        {
          format = LS_ASCII;
        }
      else if (argv[i] == "-binary" || argv[i] == "-b")
        {
          format = LS_BINARY;
        }
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
        {
#ifdef HAVE_HDF5
          format = LS_HDF5;
#else /* ! HAVE_HDF5 */
          error ("save: octave executable was not linked with HDF5 library");
#endif /* ! HAVE_HDF5 */
        }
      else if (argv[i] == "-mat-binary" || argv[i] == "-mat"
               || argv[i] == "-m" || argv[i] == "-6" || argv[i] == "-v6"
               || argv[i] == "-V6")
        {
          format = LS_MAT5_BINARY;
        }
#ifdef HAVE_ZLIB
      else if (argv[i] == "-mat7-binary" || argv[i] == "-7"
               || argv[i] == "-v7" || argv[i] == "-V7")
        {
          format = LS_MAT7_BINARY;
        }
#endif
      else if (argv[i] == "-mat4-binary" || argv[i] == "-V4"
               || argv[i] == "-v4" || argv[i] == "-4")
        {
          format = LS_MAT_BINARY;
        }
      else if (argv[i] == "-float-binary" || argv[i] == "-f")
        {
          format = LS_BINARY;
          save_as_floats = true;
        }
      else if (argv[i] == "-float-hdf5")
        {
#ifdef HAVE_HDF5
          format = LS_HDF5;
          save_as_floats = true;
#else /* ! HAVE_HDF5 */
          error ("save: octave executable was not linked with HDF5 library");
#endif /* ! HAVE_HDF5 */
        }
#ifdef HAVE_ZLIB
      else if (argv[i] == "-zip" || argv[i] == "-z")
        {
          use_zlib  = true;
        }
#endif
      else if (argv[i] == "-struct")
        {
          retval.append (argv[i]);
        }
      else if (argv[i][0] == '-' && argv[i] != "-")
        {
          error ("save: Unrecognized option '%s'", argv[i].c_str ());
        }
      else
        retval.append (argv[i]);
    }

  if (do_double)
    {
      if (format == LS_MAT_ASCII)
        format.opts |= LS_MAT_ASCII_LONG;
      else
        warning ("save: \"-double\" option only has an effect with \"-ascii\"");
    }

  if (do_tabs)
    {
      if (format == LS_MAT_ASCII)
        format.opts |= LS_MAT_ASCII_TABS;
      else
        warning ("save: \"-tabs\" option only has an effect with \"-ascii\"");
    }

  return retval;
}

static string_vector
parse_save_options (const std::string &arg, load_save_format &format,
                    bool &append, bool &save_as_floats,
                    bool &use_zlib)
{
  std::istringstream is (arg);
  std::string str;
  string_vector argv;

  while (! is.eof ())
    {
      is >> str;
      argv.append (str);
    }

  return parse_save_options (argv, format, append, save_as_floats,
                             use_zlib);
}

void
write_header (std::ostream& os, load_save_format format)
{
  switch (format.type)
    {
    case LS_BINARY:
      {
        os << (oct_mach_info::words_big_endian ()
               ? "Octave-1-B" : "Octave-1-L");

        oct_mach_info::float_format flt_fmt =
          oct_mach_info::native_float_format ();

        char tmp = static_cast<char> (float_format_to_mopt_digit (flt_fmt));

        os.write (&tmp, 1);
      }
      break;

    case LS_MAT5_BINARY:
    case LS_MAT7_BINARY:
      {
        char const * versionmagic;
        int16_t number = *(reinterpret_cast<const int16_t *>("\x00\x01"));
        struct tm bdt;
        time_t now;
        char headertext[128];

        time (&now);
        bdt = *gnulib::gmtime (&now);
        memset (headertext, ' ', 124);
        // ISO 8601 format date
        nstrftime (headertext, 124, "MATLAB 5.0 MAT-file, written by Octave "
                   OCTAVE_VERSION ", %Y-%m-%d %T UTC", &bdt, 1, 0);

        // The first pair of bytes give the version of the MAT file
        // format.  The second pair of bytes form a magic number which
        // signals a MAT file.  MAT file data are always written in
        // native byte order.  The order of the bytes in the second
        // pair indicates whether the file was written by a big- or
        // little-endian machine.  However, the version number is
        // written in the *opposite* byte order from everything else!
        if (number == 1)
          versionmagic = "\x01\x00\x4d\x49"; // this machine is big endian
        else
          versionmagic = "\x00\x01\x49\x4d"; // this machine is little endian

        memcpy (headertext+124, versionmagic, 4);
        os.write (headertext, 128);
      }

      break;

#ifdef HAVE_HDF5
    case LS_HDF5:
#endif /* HAVE_HDF5 */
    case LS_ASCII:
      {
        octave_localtime now;

        std::string comment_string = now.strftime (Vsave_header_format_string);

        if (! comment_string.empty ())
          {
#ifdef HAVE_HDF5
            if (format == LS_HDF5)
              {
                hdf5_ofstream& hs = dynamic_cast<hdf5_ofstream&> (os);
                H5Gset_comment (hs.file_id, "/", comment_string.c_str ());
              }
            else
#endif /* HAVE_HDF5 */
              os << comment_string << "\n";
          }
      }
      break;

    default:
      break;
    }
}

void
octave_prepare_hdf5 (void)
{
#ifdef HAVE_HDF5
  H5dont_atexit ();
#endif
}

void
octave_finalize_hdf5 (void)
{
#ifdef HAVE_HDF5
  H5close ();
#endif
}

static void
save_vars (const string_vector& argv, int argv_idx, int argc,
           std::ostream& os, load_save_format fmt,
           bool save_as_floats, bool write_header_info)
{
  if (write_header_info)
    write_header (os, fmt);

  if (argv_idx == argc)
    {
      save_vars (os, "*", fmt, save_as_floats);
    }
  else if (argv[argv_idx] == "-struct")
    {
      if (++argv_idx >= argc)
        {
          error ("save: missing struct name");
          return;
        }

      std::string struct_name = argv[argv_idx];

      if (! symbol_table::is_variable (struct_name))
        {
          error ("save: no such variable: '%s'", struct_name.c_str ());
          return;
        }

      octave_value struct_var = symbol_table::varval (struct_name);

      if (! struct_var.is_map () || struct_var.numel () != 1)
        {
          error ("save: '%s' is not a scalar structure",
                 struct_name.c_str ());
          return;
        }
      octave_scalar_map struct_var_map = struct_var.scalar_map_value ();

      ++argv_idx;

      if (argv_idx < argc)
        {
          for (int i = argv_idx; i < argc; i++)
            {
              if (! save_fields (os, struct_var_map, argv[i], fmt,
                                 save_as_floats))
                {
                  warning ("save: no such field '%s.%s'",
                           struct_name.c_str (), argv[i].c_str ());
                }
            }
        }
      else
        save_fields (os, struct_var_map, "*", fmt, save_as_floats);
    }
  else
    {
      for (int i = argv_idx; i < argc; i++)
        {
          if (argv[i] == "")
            continue;  // Skip empty vars for Matlab compatibility
          if (! save_vars (os, argv[i], fmt, save_as_floats))
            warning ("save: no such variable '%s'", argv[i].c_str ());
        }
    }
}

static void
dump_octave_core (std::ostream& os, const char *fname, load_save_format fmt,
                  bool save_as_floats)
{
  write_header (os, fmt);

  std::list<symbol_table::symbol_record> vars
    = symbol_table::all_variables (symbol_table::top_scope (), 0);

  double save_mem_size = 0;

  typedef std::list<symbol_table::symbol_record>::const_iterator
    const_vars_iterator;

  for (const_vars_iterator p = vars.begin (); p != vars.end (); p++)
    {
      octave_value val = p->varval ();

      if (val.is_defined ())
        {
          std::string name = p->name ();
          std::string help;
          bool global = p->is_global ();

          double val_size = val.byte_size () / 1024;

          // FIXME: maybe we should try to throw out the largest first...

          if (Voctave_core_file_limit < 0
              || save_mem_size + val_size < Voctave_core_file_limit)
            {
              save_mem_size += val_size;

              do_save (os, val, name, help, global, fmt, save_as_floats);

              if (error_state)
                break;
            }
        }
    }

  message (0, "save to '%s' complete", fname);
}

void
dump_octave_core (void)
{
  if (Vcrash_dumps_octave_core)
    {
      // FIXME: should choose better file name?

      const char *fname = Voctave_core_file_name.c_str ();

      message (0, "attempting to save variables to '%s'...", fname);

      load_save_format format = LS_BINARY;

      bool save_as_floats = false;

      bool append = false;

      bool use_zlib = false;

      parse_save_options (Voctave_core_file_options, format, append,
                          save_as_floats, use_zlib);

      std::ios::openmode mode = std::ios::out;

      // Matlab v7 files are always compressed
      if (format == LS_MAT7_BINARY)
        use_zlib = false;

      if (format == LS_BINARY
#ifdef HAVE_HDF5
          || format == LS_HDF5
#endif
          || format == LS_MAT_BINARY
          || format == LS_MAT5_BINARY
          || format == LS_MAT7_BINARY)
        mode |= std::ios::binary;

      mode |= append ? std::ios::ate : std::ios::trunc;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        {
          hdf5_ofstream file (fname, mode);

          if (file.file_id >= 0)
            {
              dump_octave_core (file, fname, format, save_as_floats);

              file.close ();
            }
          else
            warning ("unable to open '%s' for writing...", fname);
        }
      else
#endif /* HAVE_HDF5 */
        // don't insert any commands here!  The open brace below must
        // go with the else above!
        {
#ifdef HAVE_ZLIB
          if (use_zlib)
            {
              gzofstream file (fname, mode);

              if (file)
                {
                  dump_octave_core (file, fname, format, save_as_floats);

                  file.close ();
                }
              else
                warning ("unable to open '%s' for writing...", fname);
            }
          else
#endif
            {
              std::ofstream file (fname, mode);

              if (file)
                {
                  dump_octave_core (file, fname, format, save_as_floats);

                  file.close ();
                }
              else
                warning ("unable to open '%s' for writing...", fname);
            }
        }
    }
}

DEFUN (save, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} save file\n\
@deftypefnx {Command} {} save options file\n\
@deftypefnx {Command} {} save options file @var{v1} @var{v2} @dots{}\n\
@deftypefnx {Command} {} save options file -struct @var{STRUCT} @var{f1} @var{f2} @dots{}\n\
@deftypefnx {Command} {} save @code{\"-\"} @var{v1} @var{v2} @dots{}\n\
@deftypefnx {Built-in Function} {@var{s} =} save (@code{\"-\"} @var{v1} @var{v2} @dots{})\n\
Save the named variables @var{v1}, @var{v2}, @dots{}, in the file\n\
@var{file}.\n\
\n\
The special filename @samp{-} may be used to return the\n\
content of the variables as a string.  If no variable names are listed,\n\
Octave saves all the variables in the current scope.  Otherwise, full\n\
variable names or pattern syntax can be used to specify the variables to\n\
save.  If the @option{-struct} modifier is used, fields @var{f1} @var{f2}\n\
@dots{} of the scalar structure @var{STRUCT} are saved as if they were\n\
variables with corresponding names.  Valid options for the @code{save}\n\
command are listed in the following table.  Options that modify the output\n\
format override the format specified by @code{save_default_options}.\n\
\n\
If save is invoked using the functional form\n\
\n\
@example\n\
save (\"-option1\", @dots{}, \"file\", \"v1\", @dots{})\n\
@end example\n\
\n\
@noindent\n\
then the @var{options}, @var{file}, and variable name arguments\n\
(@var{v1}, @dots{}) must be specified as character strings.\n\
\n\
If called with a filename of @qcode{\"-\"}, write the output to stdout\n\
if nargout is 0, otherwise return the output in a character string.\n\
\n\
@table @code\n\
@item -append\n\
Append to the destination instead of overwriting.\n\
\n\
@item -ascii\n\
Save a single matrix in a text file without header or any other information.\n\
\n\
@item -binary\n\
Save the data in Octave's binary data format.\n\
\n\
@item -float-binary\n\
Save the data in Octave's binary data format but only using single\n\
precision.  Only use this format if you know that all the\n\
values to be saved can be represented in single precision.\n\
\n\
@item -hdf5\n\
Save the data in @sc{hdf5} format.\n\
(HDF5 is a free, portable binary format developed by the National\n\
Center for Supercomputing Applications at the University of Illinois.)\n\
This format is only available if Octave was built with a link to the\n\
@sc{hdf5} libraries.\n\
\n\
@item -float-hdf5\n\
Save the data in @sc{hdf5} format but only using single precision.\n\
Only use this format if you know that all the\n\
values to be saved can be represented in single precision.\n\
\n\
@item  -V7\n\
@itemx -v7\n\
@itemx -7\n\
@itemx -mat7-binary\n\
Save the data in @sc{matlab}'s v7 binary data format.\n\
\n\
@item  -V6\n\
@itemx -v6\n\
@itemx -6\n\
@itemx -mat\n\
@itemx -mat-binary\n\
Save the data in @sc{matlab}'s v6 binary data format.\n\
\n\
@item  -V4\n\
@itemx -v4\n\
@itemx -4\n\
@itemx -mat4-binary\n\
Save the data in the binary format written by @sc{matlab} version 4.\n\
\n\
@item -text\n\
Save the data in Octave's text data format.  (default).\n\
\n\
@item  -zip\n\
@itemx -z\n\
Use the gzip algorithm to compress the file.  This works equally on files\n\
that are compressed with gzip outside of octave, and gzip can equally be\n\
used to convert the files for backward compatibility.\n\
This option is only available if Octave was built with a link to the zlib\n\
libraries.\n\
@end table\n\
\n\
The list of variables to save may use wildcard patterns containing\n\
the following special characters:\n\
\n\
@table @code\n\
@item ?\n\
Match any single character.\n\
\n\
@item *\n\
Match zero or more characters.\n\
\n\
@item [ @var{list} ]\n\
Match the list of characters specified by @var{list}.  If the first\n\
character is @code{!} or @code{^}, match all characters except those\n\
specified by @var{list}.  For example, the pattern @code{[a-zA-Z]} will\n\
match all lower and uppercase alphabetic characters.\n\
\n\
Wildcards may also be used in the field name specifications when using\n\
the @option{-struct} modifier (but not in the struct name itself).\n\
\n\
@end table\n\
\n\
Except when using the @sc{matlab} binary data file format or the\n\
@samp{-ascii} format, saving global\n\
variables also saves the global status of the variable.  If the variable\n\
is restored at a later time using @samp{load}, it will be restored as a\n\
global variable.\n\
\n\
The command\n\
\n\
@example\n\
save -binary data a b*\n\
@end example\n\
\n\
@noindent\n\
saves the variable @samp{a} and all variables beginning with @samp{b} to\n\
the file @file{data} in Octave's binary format.\n\
@seealso{load, save_default_options, save_header_format_string, dlmread, csvread, fread}\n\
@end deftypefn")
{
  octave_value_list retval;

  string_vector argv = args.make_argv ();

  if (error_state)
    return retval;

  // Here is where we would get the default save format if it were
  // stored in a user preference variable.

  bool save_as_floats = false;

  load_save_format format = LS_ASCII;

  bool append = false;

  bool use_zlib = false;

  // get default options
  parse_save_options (Vsave_default_options, format, append, save_as_floats,
                      use_zlib);

  // override from command line
  argv = parse_save_options (argv, format, append, save_as_floats,
                             use_zlib);
  int argc = argv.length ();
  int i = 0;

  if (error_state)
    return retval;

  if (i == argc)
    {
      print_usage ();
      return retval;
    }

  if (save_as_floats && format == LS_ASCII)
    {
      error ("save: cannot specify both -ascii and -float-binary");
      return retval;
    }

  if (argv[i] == "-")
    {
      i++;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        error ("save: cannot write HDF5 format to stdout");
      else
#endif /* HAVE_HDF5 */
        // don't insert any commands here!  the brace below must go
        // with the "else" above!
        {
          if (append)
            warning ("save: ignoring -append option for output to stdout");

          if (nargout == 0)
            save_vars (argv, i, argc, std::cout, format, save_as_floats, true);
          else
            {
              std::ostringstream output_buf;
              save_vars (argv, i, argc, output_buf, format, save_as_floats, true);
              retval = octave_value (output_buf.str());
            }
        }
    }

  // Guard against things like 'save a*', which are probably mistakes...

  else if (i == argc - 1 && glob_pattern_p (argv[i]))
    {
      print_usage ();
      return retval;
    }
  else
    {
      std::string fname = file_ops::tilde_expand (argv[i]);

      i++;

      // Matlab v7 files are always compressed
      if (format == LS_MAT7_BINARY)
        use_zlib = false;

      std::ios::openmode mode
        = append ? (std::ios::app | std::ios::ate) : std::ios::out;

      if (format == LS_BINARY
#ifdef HAVE_HDF5
          || format == LS_HDF5
#endif
          || format == LS_MAT_BINARY
          || format == LS_MAT5_BINARY
          || format == LS_MAT7_BINARY)
        mode |= std::ios::binary;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        {
          // FIXME: It should be possible to append to HDF5 files.
          if (append)
            {
              error ("save: appending to HDF5 files is not implemented");
              return retval;
            }

          bool write_header_info
            = ! (append && H5Fis_hdf5 (fname.c_str ()) > 0);

          hdf5_ofstream hdf5_file (fname.c_str (), mode);

          if (hdf5_file.file_id != -1)
            {
              save_vars (argv, i, argc, hdf5_file, format,
                         save_as_floats, write_header_info);

              hdf5_file.close ();
            }
          else
            {
              gripe_file_open ("save", fname);
              return retval;
            }
        }
      else
#endif /* HAVE_HDF5 */
        // don't insert any statements here!  The brace below must go
        // with the "else" above!
        {
#ifdef HAVE_ZLIB
          if (use_zlib)
            {
              gzofstream file (fname.c_str (), mode);

              if (file)
                {
                  bool write_header_info = ! file.tellp ();

                  save_vars (argv, i, argc, file, format,
                             save_as_floats, write_header_info);

                  file.close ();
                }
              else
                {
                  gripe_file_open ("save", fname);
                  return retval;
                }
            }
          else
#endif
            {
              std::ofstream file (fname.c_str (), mode);

              if (file)
                {
                  bool write_header_info = ! file.tellp ();

                  save_vars (argv, i, argc, file, format,
                             save_as_floats, write_header_info);

                  file.close ();
                }
              else
                {
                  gripe_file_open ("save", fname);
                  return retval;
                }
            }
        }
    }

  return retval;
}

DEFUN (crash_dumps_octave_core, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} crash_dumps_octave_core ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} crash_dumps_octave_core (@var{new_val})\n\
@deftypefnx {Built-in Function} {} crash_dumps_octave_core (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave tries\n\
to save all current variables to the file @file{octave-workspace} if it\n\
crashes or receives a hangup, terminate or similar signal.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{octave_core_file_limit, octave_core_file_name, octave_core_file_options}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (crash_dumps_octave_core);
}

DEFUN (save_default_options, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} save_default_options ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} save_default_options (@var{new_val})\n\
@deftypefnx {Built-in Function} {} save_default_options (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the default options\n\
for the @code{save} command, and defines the default format.\n\
\n\
Typical values include @qcode{\"-ascii\"}, @qcode{\"-text -zip\"}.\n\
The default value is @option{-text}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{save}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (save_default_options);
}

DEFUN (octave_core_file_limit, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} octave_core_file_limit ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} octave_core_file_limit (@var{new_val})\n\
@deftypefnx {Built-in Function} {} octave_core_file_limit (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the maximum amount\n\
of memory (in kilobytes) of the top-level workspace that Octave will\n\
attempt to save when writing data to the crash dump file (the name of\n\
the file is specified by @var{octave_core_file_name}).\n\
\n\
If @var{octave_core_file_options} flags specify a binary format,\n\
then @var{octave_core_file_limit} will be approximately the maximum\n\
size of the file.  If a text file format is used, then the file could\n\
be much larger than the limit.  The default value is -1 (unlimited)\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{crash_dumps_octave_core, octave_core_file_name, octave_core_file_options}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (octave_core_file_limit);
}

DEFUN (octave_core_file_name, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} octave_core_file_name ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} octave_core_file_name (@var{new_val})\n\
@deftypefnx {Built-in Function} {} octave_core_file_name (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the name of the file\n\
used for saving data from the top-level workspace if Octave aborts.\n\
\n\
The default value is @qcode{\"octave-workspace\"}\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{crash_dumps_octave_core, octave_core_file_name, octave_core_file_options}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (octave_core_file_name);
}

DEFUN (octave_core_file_options, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} octave_core_file_options ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} octave_core_file_options (@var{new_val})\n\
@deftypefnx {Built-in Function} {} octave_core_file_options (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the options used for\n\
saving the workspace data if Octave aborts.\n\
\n\
The value of @code{octave_core_file_options} should follow the same format\n\
as the options for the @code{save} function.  The default value is Octave's\n\
binary format.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{crash_dumps_octave_core, octave_core_file_name, octave_core_file_limit}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (octave_core_file_options);
}

DEFUN (save_header_format_string, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} save_header_format_string ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} save_header_format_string (@var{new_val})\n\
@deftypefnx {Built-in Function} {} save_header_format_string (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the format\n\
string used for the comment line written at the beginning of\n\
text-format data files saved by Octave.\n\
\n\
The format string is passed to @code{strftime} and should begin with the\n\
character @samp{#} and contain no newline characters.  If the value of\n\
@code{save_header_format_string} is the empty string, the header comment is\n\
omitted from text-format data files.  The default value is\n\
@c Set example in small font to prevent overfull line\n\
\n\
@smallexample\n\
\"# Created by Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>\"\n\
@end smallexample\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{strftime, save}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (save_header_format_string);
}
