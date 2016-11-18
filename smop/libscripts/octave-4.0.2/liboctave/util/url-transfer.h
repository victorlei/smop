/*

Copyright (C) 2013-2015 John W. Eaton
Copyright (C) 2006-2015 Alexander Barth
Copyright (C) 2009 David Bateman

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

// Author: Alexander Barth <abarth@marine.usf.edu>
// Author: jwe

#if !defined (octave_url_transfer_h)
#define octave_url_transfer_h 1

#include <iosfwd>
#include <string>

class
OCTAVE_API
base_url_transfer
{
private:

  static void delete_file (const std::string& file);

  static void reset_path (base_url_transfer *curl_xfer)
  {
    curl_xfer->cwd ("..");
  }

public:

  friend class url_transfer;

  base_url_transfer (void)
    : count (1), host_or_url (), valid (false), ftp (false),
      ascii_mode (false), ok (true), errmsg (),
      curr_istream (&std::cin), curr_ostream (&std::cout)
  { }

  base_url_transfer (const std::string& host,
                     const std::string& /* user_arg */,
                     const std::string& /* passwd */,
                     std::ostream& os)
    : count (1), host_or_url (host), valid (false), ftp (true),
      ascii_mode (false), ok (true), errmsg (), curr_istream (&std::cin),
      curr_ostream (&os) { }

  base_url_transfer (const std::string& url, std::ostream& os)
    : count (1), host_or_url (url), valid (false), ftp (false),
      ascii_mode (false), ok (true), errmsg (),
      curr_istream (&std::cin), curr_ostream (&os) { }

  virtual ~base_url_transfer (void) { }

  bool is_valid (void) const { return valid; }

  bool good (void) const { return valid && ok; }

  virtual void perform (void) { }

  virtual std::string lasterror (void) const { return errmsg; }

  virtual std::ostream& set_ostream (std::ostream& /* os */)
  {
    return *curr_ostream;
  }

  virtual std::istream& set_istream (std::istream& /* is */)
  {
    return *curr_istream;
  }

  virtual void ascii (void) { }

  virtual void binary (void) { }

  bool is_ascii (void) const { return ascii_mode; }

  bool is_binary (void) const { return !ascii_mode; }

  virtual void cwd (const std::string& /* path */) { }

  virtual void del (const std::string& /* file */) { }

  virtual void rmdir (const std::string& /* path */) { }

  virtual void mkdir (const std::string& /* path */) { }

  virtual void rename (const std::string& /* oldname */,
                       const std::string& /* newname */) { }

  virtual void put (const std::string& /* file */,
                    std::istream& /* is */) { }

  virtual void get (const std::string& /* file */,
                    std::ostream& /* os */) { }

  void mget_directory (const std::string& directory,
                       const std::string& target);

  string_vector mput_directory (const std::string& base,
                                const std::string& directory);

  virtual void dir (void) { }

  virtual string_vector list (void) { return string_vector (); }

  virtual void get_fileinfo (const std::string& /* filename */,
                             double& /* filesize */,
                             time_t& /* filetime */,
                             bool& /* fileisdir */) { }

  virtual std::string pwd (void) { return std::string (); }

  virtual void http_get (const Array<std::string>& /* param */) { }

  virtual void http_post (const Array<std::string>& /* param */) { }

  virtual void http_action (const Array<std::string>& /* param */,
                            const std::string& /* action */) { }

protected:

  // Reference count.
  octave_refcount<size_t> count;

  // Host for ftp transfers or full URL for http requests.
  std::string host_or_url;
  bool valid;
  bool ftp;
  bool ascii_mode;
  bool ok;
  std::string errmsg;
  std::istream *curr_istream;
  std::ostream *curr_ostream;

private:

  // No copying!

  base_url_transfer (const base_url_transfer&);

  base_url_transfer& operator = (const base_url_transfer&);
};

class
OCTAVE_API
url_transfer
{
public:

  url_transfer (void);

  url_transfer (const std::string& host, const std::string& user,
                const std::string& passwd, std::ostream& os);

  url_transfer (const std::string& url, std::ostream& os);

  url_transfer (const url_transfer& h) : rep (h.rep)
  {
    rep->count++;
  }

  ~url_transfer (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  url_transfer& operator = (const url_transfer& h)
  {
    if (this != &h)
      {
        if (--rep->count == 0)
          delete rep;

        rep = h.rep;
        rep->count++;
      }

    return *this;
  }

  bool is_valid (void) const { return rep->is_valid (); }

  bool good (void) const { return rep->good (); }

  std::string lasterror (void) const { return rep->lasterror (); }

  std::ostream& set_ostream (std::ostream& os)
  {
    return rep->set_ostream (os);
  }

  std::istream& set_istream (std::istream& is)
  {
    return rep->set_istream (is);
  }

  void ascii (void) { rep->ascii (); }

  void binary (void) { rep->binary (); }

  bool is_ascii (void) const { return rep->is_ascii (); }

  bool is_binary (void) const { return rep->is_binary (); }

  void cwd (const std::string& path) { rep->cwd (path); }

  void del (const std::string& file) { rep->del (file); }

  void rmdir (const std::string& path) { rep->rmdir (path); }

  void mkdir (const std::string& path) { rep->mkdir (path); }

  void rename (const std::string& oldname, const std::string& newname)
  {
    rep->rename (oldname, newname);
  }

  void put (const std::string& file, std::istream& is)
  {
    rep->put (file, is);
  }

  void get (const std::string& file, std::ostream& os)
  {
    rep->get (file, os);
  }

  void mget_directory (const std::string& directory,
                       const std::string& target)
  {
    rep->mget_directory (directory, target);
  }

  string_vector mput_directory (const std::string& base,
                                const std::string& directory)
  {
    return rep->mput_directory (base, directory);
  }

  void dir (void) { rep->dir (); }

  string_vector list (void) { return rep->list (); }

  void get_fileinfo (const std::string& filename, double& filesize,
                     time_t& filetime, bool& fileisdir)
  {
    rep->get_fileinfo (filename, filesize, filetime, fileisdir);
  }

  std::string pwd (void) { return rep->pwd (); }

  void http_get (const Array<std::string>& param) { rep->http_get (param); }

  void http_post (const Array<std::string>& param) { rep->http_post (param); }

  void http_action (const Array<std::string>& param,
                    const std::string& action)
  {
    rep->http_action (param, action);
  }

private:

  base_url_transfer *rep;
};

#endif
