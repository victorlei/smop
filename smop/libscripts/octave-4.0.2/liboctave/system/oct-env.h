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

#if !defined (octave_oct_env_h)
#define octave_oct_env_h 1

#include <string>

class
OCTAVE_API
octave_env
{
protected:

  octave_env (void);

public:

  static std::string polite_directory_format (const std::string& name);

  static bool absolute_pathname (const std::string& s);

  static bool rooted_relative_pathname (const std::string& s);

  static std::string base_pathname (const std::string& s);

  static std::string
  make_absolute (const std::string& s,
                 const std::string& dot_path = get_current_directory ());

  static std::string get_current_directory (void);

  static std::string get_home_directory (void);

  static std::string get_temp_directory (void);

  static std::string get_program_name (void);

  static std::string get_program_invocation_name (void);

  static std::string get_user_name (void);

  static std::string get_host_name (void);

  static std::string getenv (const std::string& name);

  static void putenv (const std::string& name, const std::string& value);

  static bool have_x11_display (void);

  static bool chdir (const std::string& newdir);

  static void set_program_name (const std::string& s);

private:

  static bool instance_ok (void);

  std::string do_polite_directory_format (const std::string& name) const;

  bool do_absolute_pathname (const std::string& s) const;

  bool do_rooted_relative_pathname (const std::string& s) const;

  std::string do_base_pathname (const std::string& s) const;

  std::string do_make_absolute (const std::string& s,
                                const std::string& dot_path) const;

  std::string do_getcwd (void) const;

  std::string do_get_home_directory (void) const;

  std::string do_get_temp_directory (void) const;

  std::string do_get_user_name (void) const;

  std::string do_get_host_name (void) const;

  std::string do_getenv (const std::string& name) const;

  void do_putenv (const std::string& name, const std::string& value) const;

  bool do_chdir (const std::string& newdir);

  void do_set_program_name (const std::string& s) const;

  void pathname_backup (std::string& path, int n) const;

  void error (int) const;

  void error (const std::string&) const;

  // No copying!

  octave_env (const octave_env&);

  octave_env& operator = (const octave_env&);

  // The real thing.
  static octave_env *instance;


  static void cleanup_instance (void) { delete instance; instance = 0; }

  // TRUE means follow symbolic links that point to directories just
  // as if they are real directories.
  bool follow_symbolic_links;

  // TRUE means that pwd always give verbatim directory, regardless
  // of symbolic link following.
  bool verbatim_pwd;

  // Where are we?
  mutable std::string current_directory;

  // Etc.
  mutable std::string prog_name;

  mutable std::string prog_invocation_name;

  mutable std::string user_name;

  mutable std::string host_name;
};

#endif
