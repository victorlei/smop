/*

Copyright (C) 2006-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if !defined (octave_load_path_h)
#define octave_load_path_h 1

#include <iosfwd>
#include <list>
#include <map>
#include <string>

#include "pathsearch.h"
#include "str-vec.h"

class
OCTINTERP_API
load_path
{
protected:

  load_path (void)
    : loader_map (), default_loader (), dir_info_list (), init_dirs () { }

public:

  typedef void (*hook_fcn_ptr) (const std::string& dir);

  ~load_path (void) { }

  static void initialize (bool set_initial_path = false)
  {
    if (instance_ok ())
      instance->do_initialize (set_initial_path);
  }

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }

  static void set (const std::string& p, bool warn = false)
  {
    if (instance_ok ())
      instance->do_set (p, warn);
  }

  static void append (const std::string& dir, bool warn = false)
  {
    if (instance_ok ())
      instance->do_append (dir, warn);
  }

  static void prepend (const std::string& dir, bool warn = false)
  {
    if (instance_ok ())
      instance->do_prepend (dir, warn);
  }

  static bool remove (const std::string& dir)
  {
    return instance_ok () ? instance->do_remove (dir) : false;
  }

  static void update (void)
  {
    if (instance_ok ())
      instance->do_update ();
  }

  static bool contains_canonical (const std::string& dir_name)
  {
    return instance_ok () ? instance->do_contains_canonical (dir_name) : false;
  }

  static std::string find_method (const std::string& class_name,
                                  const std::string& meth,
                                  std::string& dir_name,
                                  const std::string& pack_name = std::string ())
  {
    return instance_ok ()
      ? instance->get_loader (pack_name).find_method (class_name, meth,
                                                      dir_name)
      : std::string ();
  }

  static std::string find_method (const std::string& class_name,
                                  const std::string& meth,
                                  const std::string& pack_name = std::string ())
  {
    std::string dir_name;
    return find_method (class_name, meth, dir_name, pack_name);
  }

  static std::list<std::string> methods (const std::string& class_name,
                                         const std::string& pack_name = std::string ())
  {
    return instance_ok ()
      ? instance->get_loader(pack_name).methods (class_name)
      : std::list<std::string> ();
  }

  static std::list<std::string> overloads (const std::string& meth)
  {
    return instance_ok ()
           ? instance->do_overloads (meth) : std::list<std::string> ();
  }

  static bool find_package (const std::string& package_name)
  {
    return instance_ok ()
      ? instance->do_find_package (package_name) : false;
  }

  static std::list<std::string>
  get_all_package_names (bool only_top_level = true)
  {
    return instance_ok ()
      ? instance->do_get_all_package_names (only_top_level)
      : std::list<std::string> ();
  }

  static std::string find_fcn (const std::string& fcn, std::string& dir_name,
                               const std::string& pack_name = std::string ())
  {
    return instance_ok ()
      ? instance->get_loader (pack_name).find_fcn (fcn, dir_name)
      : std::string ();
  }

  static std::string find_fcn (const std::string& fcn,
                               const std::string& pack_name = std::string ())
  {
    std::string dir_name;
    return find_fcn (fcn, dir_name, pack_name);
  }

  static std::string find_private_fcn (const std::string& dir,
                                       const std::string& fcn,
                                       const std::string& pack_name = std::string ())
  {
    return instance_ok ()
      ? instance->get_loader (pack_name).find_private_fcn (dir, fcn)
      : std::string ();
  }

  static std::string find_fcn_file (const std::string& fcn,
                                    const std::string& pack_name = std::string ())
  {
    std::string dir_name;

    return instance_ok ()
      ? instance->get_loader (pack_name).find_fcn (fcn, dir_name, M_FILE)
      : std::string ();
  }

  static std::string find_oct_file (const std::string& fcn,
                                    const std::string& pack_name = std::string ())
  {
    std::string dir_name;

    return instance_ok ()
      ? instance->get_loader (pack_name).find_fcn (fcn, dir_name, M_FILE)
      : std::string ();
  }

  static std::string find_mex_file (const std::string& fcn,
                                    const std::string& pack_name = std::string ())
  {
    std::string dir_name;

    return instance_ok ()
      ? instance->get_loader (pack_name).find_fcn (fcn, dir_name, M_FILE)
      : std::string ();
  }

  static std::string find_file (const std::string& file)
  {
    return instance_ok ()
           ? instance->do_find_file (file) : std::string ();
  }

  static std::string find_dir (const std::string& dir)
  {
    return instance_ok ()
           ? instance->do_find_dir (dir) : std::string ();
  }

  static string_vector find_matching_dirs (const std::string& dir)
  {
    return instance_ok ()
           ? instance->do_find_matching_dirs (dir) : string_vector ();
  }

  static std::string find_first_of (const string_vector& files)
  {
    return instance_ok () ?
           instance->do_find_first_of (files) : std::string ();
  }

  static string_vector find_all_first_of (const string_vector& files)
  {
    return instance_ok () ?
           instance->do_find_all_first_of (files) : string_vector ();
  }

  static string_vector dirs (void)
  {
    return instance_ok () ? instance->do_dirs () : string_vector ();
  }

  static std::list<std::string> dir_list (void)
  {
    return instance_ok ()
           ? instance->do_dir_list () : std::list<std::string> ();
  }

  static string_vector files (const std::string& dir, bool omit_exts = false)
  {
    return instance_ok ()
           ? instance->do_files (dir, omit_exts) : string_vector ();
  }

  static string_vector fcn_names (void)
  {
    return instance_ok () ? instance->do_fcn_names () : string_vector ();
  }

  static std::string path (void)
  {
    return instance_ok () ? instance->do_path () : std::string ();
  }

  static void display (std::ostream& os)
  {
    if (instance_ok ())
      instance->do_display (os);
  }

  static void set_add_hook (hook_fcn_ptr f) { add_hook = f; }

  static void set_remove_hook (hook_fcn_ptr f) { remove_hook = f; }

  static void set_command_line_path (const std::string& p)
  {
    if (command_line_path.empty ())
      command_line_path = p;
    else
      command_line_path += dir_path::path_sep_str () + p;
  }

  static std::string get_command_line_path (void)
  {
    return instance_ok () ? instance->do_get_command_line_path ()
                          : std::string ();
  }

  static std::string system_path (void)
  {
    return instance_ok () ? instance->do_system_path () : std::string ();
  }

private:

  static const int M_FILE = 1;
  static const int OCT_FILE = 2;
  static const int MEX_FILE = 4;

  class dir_info
  {
  public:

    // <FCN_NAME, TYPE>
    typedef std::map<std::string, int> fcn_file_map_type;

    typedef fcn_file_map_type::const_iterator const_fcn_file_map_iterator;
    typedef fcn_file_map_type::iterator fcn_file_map_iterator;

    struct class_info
    {
      class_info (void) : method_file_map (), private_file_map () { }

      class_info (const class_info& ci)
        : method_file_map (ci.method_file_map),
          private_file_map (ci.private_file_map) { }

      class_info& operator = (const class_info& ci)
      {
        if (this != &ci)
          {
            method_file_map = ci.method_file_map;
            private_file_map = ci.private_file_map;
          }
        return *this;
      }

      ~class_info (void) { }

      fcn_file_map_type method_file_map;
      fcn_file_map_type private_file_map;
    };

    // <CLASS_NAME, CLASS_INFO>
    typedef std::map<std::string, class_info> method_file_map_type;

    typedef method_file_map_type::const_iterator const_method_file_map_iterator;
    typedef method_file_map_type::iterator method_file_map_iterator;

    // <PACKAGE_NAME, DIR_INFO>
    typedef std::map<std::string, dir_info> package_dir_map_type;

    typedef package_dir_map_type::const_iterator const_package_dir_map_iterator;
    typedef package_dir_map_type::iterator package_dir_map_iterator;

    // This default constructor is only provided so we can create a
    // std::map of dir_info objects.  You should not use this
    // constructor for any other purpose.
    dir_info (void)
      : dir_name (), abs_dir_name (), is_relative (false),
        dir_mtime (), dir_time_last_checked (),
        all_files (), fcn_files (), private_file_map (), method_file_map (),
        package_dir_map ()
    { }

    dir_info (const std::string& d)
      : dir_name (d), abs_dir_name (), is_relative (false),
        dir_mtime (), dir_time_last_checked (),
        all_files (), fcn_files (), private_file_map (), method_file_map (),
        package_dir_map ()
    {
      initialize ();
    }

    dir_info (const dir_info& di)
      : dir_name (di.dir_name), abs_dir_name (di.abs_dir_name),
        is_relative (di.is_relative),
        dir_mtime (di.dir_mtime),
        dir_time_last_checked (di.dir_time_last_checked),
        all_files (di.all_files), fcn_files (di.fcn_files),
        private_file_map (di.private_file_map),
        method_file_map (di.method_file_map),
        package_dir_map (di.package_dir_map) { }

    ~dir_info (void) { }

    dir_info& operator = (const dir_info& di)
    {
      if (&di != this)
        {
          dir_name = di.dir_name;
          abs_dir_name = di.abs_dir_name;
          is_relative = di.is_relative;
          dir_mtime = di.dir_mtime;
          dir_time_last_checked = di.dir_time_last_checked;
          all_files = di.all_files;
          fcn_files = di.fcn_files;
          private_file_map = di.private_file_map;
          method_file_map = di.method_file_map;
          package_dir_map = di.package_dir_map;
        }

      return *this;
    }

    void update (void);

    std::string dir_name;
    std::string abs_dir_name;
    bool is_relative;
    octave_time dir_mtime;
    octave_time dir_time_last_checked;
    string_vector all_files;
    string_vector fcn_files;
    fcn_file_map_type private_file_map;
    method_file_map_type method_file_map;
    package_dir_map_type package_dir_map;

    bool is_package (const std::string& name) const;

  private:

    void initialize (void);

    void get_file_list (const std::string& d);

    void get_private_file_map (const std::string& d);

    void get_method_file_map (const std::string& d,
                              const std::string& class_name);

    void get_package_dir (const std::string& d,
                          const std::string& package_name);

    friend fcn_file_map_type get_fcn_files (const std::string& d);
  };

  class file_info
  {
  public:

    file_info (const std::string& d, int t) : dir_name (d), types (t) { }

    file_info (const file_info& fi)
      : dir_name (fi.dir_name), types (fi.types) { }

    ~file_info (void) { }

    file_info& operator = (const file_info& fi)
    {
      if (&fi != this)
        {
          dir_name = fi.dir_name;
          types = fi.types;
        }

      return *this;
    }

    std::string dir_name;
    int types;
  };

  // We maintain two ways of looking at the same information.
  //
  // First, a list of directories and the set of "public" files and
  // private files (those found in the special "private" subdirectory)
  // in each directory.
  //
  // Second, a map from file names (the union of all "public" files for all
  // directories, but without filename extensions) to a list of
  // corresponding information (directory name and file types).  This
  // way, we can quickly find shadowed file names and look up all
  // overloaded functions (in the "@" directories used to implement
  // classes).

  typedef std::list<dir_info> dir_info_list_type;

  typedef dir_info_list_type::const_iterator const_dir_info_list_iterator;
  typedef dir_info_list_type::iterator dir_info_list_iterator;

  typedef std::map<std::string, dir_info> abs_dir_cache_type;

  typedef abs_dir_cache_type::const_iterator const_abs_dir_cache_iterator;
  typedef abs_dir_cache_type::iterator abs_dir_cache_iterator;

  typedef std::list<file_info> file_info_list_type;

  typedef file_info_list_type::const_iterator const_file_info_list_iterator;
  typedef file_info_list_type::iterator file_info_list_iterator;

  // <FCN_NAME, FILE_INFO_LIST>
  typedef std::map<std::string, file_info_list_type> fcn_map_type;

  typedef fcn_map_type::const_iterator const_fcn_map_iterator;
  typedef fcn_map_type::iterator fcn_map_iterator;

  // <DIR_NAME, <FCN_NAME, TYPE>>
  typedef std::map<std::string, dir_info::fcn_file_map_type>
    private_fcn_map_type;

  typedef private_fcn_map_type::const_iterator const_private_fcn_map_iterator;
  typedef private_fcn_map_type::iterator private_fcn_map_iterator;

  // <CLASS_NAME, <FCN_NAME, FILE_INFO_LIST>>
  typedef std::map<std::string, fcn_map_type> method_map_type;

  typedef method_map_type::const_iterator const_method_map_iterator;
  typedef method_map_type::iterator method_map_iterator;

  class loader
  {
  public:
    loader (const std::string& pfx = std::string ())
      : prefix (pfx), dir_list (), fcn_map (), private_fcn_map (),
        method_map () { }

    loader (const loader& l)
      : prefix (l.prefix), dir_list (l.dir_list),
        private_fcn_map (l.private_fcn_map), method_map (l.method_map) { }

    ~loader (void) { }

    loader& operator = (const loader& l)
    {
      if (&l != this)
        {
          prefix = l.prefix;
          dir_list = l.dir_list;
          fcn_map = l.fcn_map;
          private_fcn_map = l.private_fcn_map;
          method_map = l.method_map;
        }

      return *this;
    }

    void add (const dir_info& di, bool at_end)
    {
      if (at_end)
        dir_list.push_back (di.dir_name);
      else
        dir_list.push_front (di.dir_name);

      add_to_fcn_map (di, at_end);

      add_to_private_fcn_map (di);

      add_to_method_map (di, at_end);
    }

    void move (const dir_info& di, bool at_end);

    void remove (const dir_info& di);

    void clear (void)
    {
      dir_list.clear ();

      fcn_map.clear ();

      private_fcn_map.clear ();

      method_map.clear ();
    }

    void display (std::ostream& out) const;

    std::string find_fcn (const std::string& fcn,
                          std::string& dir_name,
                          int type = M_FILE | OCT_FILE | MEX_FILE) const;

    std::string find_private_fcn (const std::string& dir,
                                  const std::string& fcn,
                                  int type = M_FILE | OCT_FILE | MEX_FILE) const;

    std::string find_method (const std::string& class_name,
                             const std::string& meth,
                             std::string& dir_name,
                             int type = M_FILE | OCT_FILE | MEX_FILE) const;

    std::list<std::string> methods (const std::string& class_name) const;

    void overloads (const std::string& meth, std::list<std::string>& l) const;

    string_vector fcn_names (void) const;

  private:
    void add_to_fcn_map (const dir_info& di, bool at_end);

    void add_to_private_fcn_map (const dir_info& di);

    void add_to_method_map (const dir_info& di, bool at_end);

    void move_fcn_map (const std::string& dir,
                       const string_vector& fcn_files, bool at_end);

    void move_method_map (const std::string& dir, bool at_end);

    void remove_fcn_map (const std::string& dir,
                         const string_vector& fcn_files);

    void remove_private_fcn_map (const std::string& dir);

    void remove_method_map (const std::string& dir);

  private:
    std::string prefix;

    std::list<std::string> dir_list;

    fcn_map_type fcn_map;

    private_fcn_map_type private_fcn_map;

    method_map_type method_map;
  };

  // <PACKAGE_NAME, LOADER>
  typedef std::map<std::string, loader> loader_map_type;

  typedef loader_map_type::const_iterator const_loader_map_iterator;
  typedef loader_map_type::iterator loader_map_iterator;

  mutable loader_map_type loader_map;

  mutable loader default_loader;

  mutable dir_info_list_type dir_info_list;

  mutable std::set<std::string> init_dirs;

  static load_path *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  static hook_fcn_ptr add_hook;

  static hook_fcn_ptr remove_hook;

  static std::string command_line_path;

  static std::string sys_path;

  static abs_dir_cache_type abs_dir_cache;

  static bool instance_ok (void);

  const_dir_info_list_iterator find_dir_info (const std::string& dir) const;
  dir_info_list_iterator find_dir_info (const std::string& dir);

  bool contains (const std::string& dir) const;

  bool do_contains_canonical (const std::string& dir) const;

  void do_move (dir_info_list_iterator i, bool at_end);

  void move (const dir_info& di, bool at_end,
             const std::string& pname = std::string ());

  void remove (const dir_info& di,
               const std::string& pname = std::string ());

  void do_initialize (bool set_initial_path);

  void do_clear (void);

  void do_set (const std::string& p, bool warn, bool is_init = false);

  void do_append (const std::string& dir, bool warn);

  void do_prepend (const std::string& dir, bool warn);

  void do_add (const std::string& dir, bool at_end, bool warn);

  bool do_remove (const std::string& dir);

  void do_update (void) const;

  static bool
  check_file_type (std::string& fname, int type, int possible_types,
                   const std::string& fcn, const char *who);

  bool is_package (const std::string& name) const;

  loader& get_loader (const std::string& name) const
  {
    if (! name.empty () && is_package (name))
      {
        loader_map_iterator l = loader_map.find (name);

        if (l == loader_map.end ())
          l = loader_map.insert (loader_map.end (),
                                 loader_map_type::value_type (name, loader (name)));

        return l->second;
      }

    return default_loader;
  }

  std::list<std::string> do_overloads (const std::string& meth) const;

  bool do_find_package (const std::string& package_name) const
  {
    return (loader_map.find (package_name) != loader_map.end ());
  }

  std::list<std::string> do_get_all_package_names (bool only_top_level) const;

  std::string do_find_file (const std::string& file) const;

  std::string do_find_dir (const std::string& dir) const;

  string_vector do_find_matching_dirs (const std::string& dir) const;

  std::string do_find_first_of (const string_vector& files) const;

  string_vector do_find_all_first_of (const string_vector& files) const;

  string_vector do_dirs (void) const;

  std::list<std::string> do_dir_list (void) const;

  string_vector do_files (const std::string& dir, bool omit_exts) const;

  string_vector do_fcn_names (void) const;

  std::string do_path (void) const;

  friend void print_types (std::ostream& os, int types);

  friend string_vector get_file_list (const dir_info::fcn_file_map_type& lst);

  friend void
  print_fcn_list (std::ostream& os, const dir_info::fcn_file_map_type& lst);

  void do_display (std::ostream& os) const;

  std::string do_system_path (void) const { return sys_path; }

  std::string do_get_command_line_path (void) const
  { return command_line_path; }

  void add (const dir_info& di, bool at_end,
            const std::string& pname = std::string ()) const;

  friend dir_info::fcn_file_map_type get_fcn_files (const std::string& d);
};

extern std::string
genpath (const std::string& dir, const string_vector& skip = "private");

extern void execute_pkg_add (const std::string& dir);
extern void execute_pkg_del (const std::string& dir);

#endif
