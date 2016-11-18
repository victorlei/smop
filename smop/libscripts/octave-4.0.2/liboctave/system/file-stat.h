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

#if !defined (octave_file_stat_h)
#define octave_file_stat_h 1

#include <string>

#include "oct-time.h"

#include <sys/types.h>

class
OCTAVE_API
base_file_stat
{
public:

  base_file_stat (void)
    : initialized (false), fail (false), errmsg (), fs_mode (),
      fs_ino (), fs_dev (), fs_nlink (), fs_uid (), fs_gid (),
      fs_size (), fs_atime (), fs_mtime (), fs_ctime (), fs_rdev (),
      fs_blksize (), fs_blocks () { }

  base_file_stat (const base_file_stat& fs)
    : initialized (fs.initialized), fail (fs.fail), errmsg (fs.errmsg),
      fs_mode (fs.fs_mode), fs_ino (fs.fs_ino), fs_dev (fs.fs_dev),
      fs_nlink (fs.fs_nlink), fs_uid (fs.fs_uid), fs_gid (fs.fs_gid),
      fs_size (fs.fs_size), fs_atime (fs.fs_atime), fs_mtime (fs.fs_mtime),
      fs_ctime (fs.fs_ctime), fs_rdev (fs.fs_rdev),
      fs_blksize (fs.fs_blksize), fs_blocks (fs.fs_blocks) { }

  base_file_stat& operator = (const base_file_stat& fs)
  {
    if (this != &fs)
      {
        initialized = fs.initialized;
        fail = fs.fail;
        errmsg = fs.errmsg;
        fs_mode = fs.fs_mode;
        fs_ino = fs.fs_ino;
        fs_dev = fs.fs_dev;
        fs_nlink = fs.fs_nlink;
        fs_uid = fs.fs_uid;
        fs_gid = fs.fs_gid;
        fs_size = fs.fs_size;
        fs_atime = fs.fs_atime;
        fs_mtime = fs.fs_mtime;
        fs_ctime = fs.fs_ctime;
        fs_rdev = fs.fs_rdev;
        fs_blksize = fs.fs_blksize;
        fs_blocks = fs.fs_blocks;
      }

    return *this;
  }

  // The minimum difference in file time stamp values.
  // FIXME: This value should come from the filesystem itself.
  //        How can we get that info?
  octave_time time_resolution (void) const
  {
    static octave_time resolution (1.0);
    return resolution;
  }

  // File status and info.  The is_XXX functions will return false for
  // file_stat objects that are not properly initialized.  The others
  // should all return 0 (or the equivalent, for the given object)
  // which is likely not meaningful.

  bool is_blk (void) const;
  bool is_chr (void) const;
  bool is_dir (void) const;
  bool is_fifo (void) const;
  bool is_lnk (void) const;
  bool is_reg (void) const;
  bool is_sock (void) const;

  static bool is_blk (mode_t mode);
  static bool is_chr (mode_t mode);
  static bool is_dir (mode_t mode);
  static bool is_fifo (mode_t mode);
  static bool is_lnk (mode_t mode);
  static bool is_reg (mode_t mode);
  static bool is_sock (mode_t mode);

  ino_t ino (void) const { return fs_ino; }
  dev_t dev (void) const { return fs_dev; }

  nlink_t nlink (void) const { return fs_nlink; }

  uid_t uid (void) const { return fs_uid; }
  gid_t gid (void) const { return fs_gid; }

  off_t size (void) const { return fs_size; }

  octave_time atime (void) const { return fs_atime; }
  octave_time mtime (void) const { return fs_mtime; }
  octave_time ctime (void) const { return fs_ctime; }

  dev_t rdev (void) const { return fs_rdev; }

  long blksize (void) const { return fs_blksize; }
  long blocks (void) const { return fs_blocks; }

  mode_t mode (void) const { return fs_mode; }

  std::string mode_as_string (void) const;

  bool ok (void) const { return initialized && ! fail; }

  operator bool () const { return ok (); }

  bool exists (void) const { return ok (); }

  std::string error (void) const { return ok () ? std::string () : errmsg; }

  // Has the file referenced by this object been modified since TIME?
  bool is_newer (const octave_time& time) const { return fs_mtime > time; }

  // It's nice to be able to hide the file_stat object if we don't
  // really care about it.
  static int is_newer (const std::string&, const octave_time&);

protected:

  virtual ~base_file_stat (void) { }

  // TRUE means we have already called stat.
  bool initialized;

  // TRUE means the stat for this file failed.
  bool fail;

  // If a failure occurs, this contains the system error text.
  std::string errmsg;

  // file type and permissions
  mode_t fs_mode;

  // serial number
  ino_t fs_ino;

  // device number
  dev_t fs_dev;

  // number of links
  nlink_t fs_nlink;

  // user ID of owner
  uid_t fs_uid;

  // group ID of owner
  gid_t fs_gid;

  // size in bytes, for regular files
  off_t fs_size;

  // time of last access
  octave_time fs_atime;

  // time of last modification
  octave_time fs_mtime;

  // time of last file status change
  octave_time fs_ctime;

  // device number for special files
  dev_t fs_rdev;

  // best I/O block size
  long fs_blksize;

  // number of 512-byte blocks allocated
  long fs_blocks;
};

class
OCTAVE_API
file_stat : public base_file_stat
{
public:

  file_stat (const std::string& n = std::string (), bool fl = true)
    : base_file_stat (), file_name (n), follow_links (fl)
  {
    if (! file_name.empty ())
      update_internal ();
  }

  file_stat (const file_stat& fs)
    : base_file_stat (fs), file_name (fs.file_name),
      follow_links (fs.follow_links) { }

  file_stat& operator = (const file_stat& fs)
  {
    if (this != &fs)
      {
        base_file_stat::operator = (fs);

        file_name = fs.file_name;
        follow_links = fs.follow_links;
      }

    return *this;
  }

  ~file_stat (void) { }

  void get_stats (bool force = false)
  {
    if (! initialized || force)
      update_internal (force);
  }

  void get_stats (const std::string& n, bool force = false)
  {
    if (n != file_name || ! initialized  || force)
      {
        initialized = false;

        file_name = n;

        update_internal (force);
      }
  }

private:

  // Name of the file.
  std::string file_name;

  // TRUE means follow symbolic links to the ultimate file (stat).
  // FALSE means get information about the link itself (lstat).
  bool follow_links;

  void update_internal (bool force = false);
};

class
OCTAVE_API
file_fstat : public base_file_stat
{
public:

  file_fstat (int n) : base_file_stat (), fid (n)
  {
    update_internal ();
  }

  file_fstat (const file_fstat& fs)
    : base_file_stat (fs), fid (fs.fid) { }

  file_fstat& operator = (const file_fstat& fs)
  {
    if (this != &fs)
      {
        base_file_stat::operator = (fs);

        fid = fs.fid;
      }

    return *this;
  }

  ~file_fstat (void) { }

  void get_stats (bool force = false)
  {
    if (! initialized || force)
      update_internal (force);
  }

  void get_stats (int n, bool force = false)
  {
    if (n != fid || ! initialized  || force)
      {
        initialized = false;

        fid = n;

        update_internal (force);
      }
  }

private:

  // Open file descriptor.
  int fid;

  void update_internal (bool force = false);
};

#endif
