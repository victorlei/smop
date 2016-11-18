/*

Copyright (C) 2013-2015 John W. Eaton
Copyright (C) 2011-2015 Jacob Dawid
Copyright (C) 2011-2015 John P. Swensen

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

#include "cmd-edit.h"
#include "defun.h"
#include "oct-env.h"
#include "oct-mutex.h"
#include "singleton-cleanup.h"
#include "toplev.h"

#include "octave-link.h"

static int
octave_readline_hook (void)
{
  octave_link::entered_readline_hook ();
  octave_link::generate_events ();
  octave_link::process_events ();
  octave_link::finished_readline_hook ();

  return 0;
}

octave_link *octave_link::instance = 0;

octave_link::octave_link (void)
  : event_queue_mutex (new octave_mutex ()), gui_event_queue (),
    debugging (false), link_enabled (true)
{
  command_editor::add_event_hook (octave_readline_hook);
}

void
octave_link::set_workspace (void)
{
  if (enabled ())
    instance->do_set_workspace ((symbol_table::current_scope ()
                                 == symbol_table::top_scope ()),
                                instance->debugging,
                                symbol_table::workspace_info ());
}

// OBJ should be an object of a class that is derived from the base
// class octave_link, or 0 to disconnect the link.  It is the
// responsibility of the caller to delete obj.

void
octave_link::connect_link (octave_link* obj)
{
  if (obj && instance)
    ::error ("octave_link is already linked!");
  else
    instance = obj;
}

void
octave_link::do_generate_events (void)
{
}

void
octave_link::do_process_events (void)
{
  event_queue_mutex->lock ();

  gui_event_queue.run ();

  event_queue_mutex->unlock ();
}

void
octave_link::do_discard_events (void)
{
  event_queue_mutex->lock ();

  gui_event_queue.discard ();

  event_queue_mutex->unlock ();
}

DEFUN (__octave_link_enabled__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_enabled__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  return octave_value (octave_link::enabled ());
}

DEFUN (__octave_link_edit_file__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_edit_file__ (@var{file})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string file = args(0).string_value ();

      if (! error_state)
        {
          flush_octave_stdout ();

          retval = octave_link::edit_file (file);
        }
      else
        error ("expecting file name as argument");
    }
  else if (args.length () == 2)
    {
      std::string file = args(0).string_value ();

      if (! error_state)
        {
          flush_octave_stdout ();

          retval = octave_link::prompt_new_edit_file (file);
        }
      else
        error ("expecting file name as first argument");
    }

  return retval;
}

DEFUN (__octave_link_message_dialog__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_message_dialog__ (@var{dlg}, @var{msg}, @var{title})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 3)
    {
      std::string dlg   = args(0).string_value ();
      std::string msg   = args(1).string_value ();
      std::string title = args(2).string_value ();

      if (! error_state)
        {
          flush_octave_stdout ();

          retval = octave_link::message_dialog (dlg, msg, title);
        }
      else
        error ("invalid arguments");
    }

  return retval;
}

DEFUN (__octave_link_question_dialog__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_question_dialog__ (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{btn3}, @var{default})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 6)
    {
      std::string msg    = args(0).string_value ();
      std::string title  = args(1).string_value ();
      std::string btn1   = args(2).string_value ();
      std::string btn2   = args(3).string_value ();
      std::string btn3   = args(4).string_value ();
      std::string btndef = args(5).string_value ();

      if (! error_state)
        {
          flush_octave_stdout ();

          retval = octave_link::question_dialog (msg, title, btn1, btn2, btn3,
                                                 btndef);
        }
      else
        error ("invalid arguments");
    }

  return retval;
}

DEFUN (__octave_link_file_dialog__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_file_dialog__ (@var{filterlist}, @var{title}, @var{filename}, @var{size} @var{multiselect}, @var{pathname})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 6)
    {

      const Array<std::string> flist = args(0).cellstr_value ();
      std::string title = args(1).string_value ();
      std::string filename = args(2).string_value ();
      Matrix pos = args(3).matrix_value ();
      std::string multi_on = args(4).string_value (); // on, off, create
      std::string pathname = args(5).string_value ();

      octave_idx_type nel;
      octave_link::filter_list filter_lst;

      for (octave_idx_type i = 0; i < flist.rows (); i++)
        filter_lst.push_back (std::make_pair (flist(i,0),
                                              (flist.columns () > 1
                                               ? flist(i,1) : "")));

      if (! error_state)
        {
          flush_octave_stdout ();

          std::list<std::string> items_lst
            = octave_link::file_dialog (filter_lst, title, filename, pathname,
                                        multi_on);

          nel = items_lst.size ();

          retval.resize (3);

          // If 3, then retval is filename, directory, and selected index.
          if (nel <= 3)
            {
              int idx = 0;
              for (std::list<std::string>::iterator it = items_lst.begin ();
                   it != items_lst.end (); it++)
                {
                  retval(idx++) = *it;

                  if (idx == 1 && retval(0).string_value ().length () == 0)
                    retval(0) = 0;

                  if (idx == 3)
                    retval(2) = atoi (retval(2).string_value ().c_str ());
                }
            }
          else
            {
              // Multiple files.
              nel = items_lst.size () - 2;
              Cell items (dim_vector (1, nel));

              std::list<std::string>::iterator it = items_lst.begin ();

              for (int idx = 0; idx < nel; idx++)
                {
                  items.xelem (idx) = *it;
                  it++;
                }

              retval(0) = items;
              retval(1) = *it++;
              retval(2) = atoi (it->c_str ());
            }
        }
      else
        error ("invalid arguments");
    }

  return retval;
}

DEFUN (__octave_link_list_dialog__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_list_dialog__ (@var{list}, @var{mode}, @var{size}, @var{intial}, @var{name}, @var{prompt}, @var{ok_string}, @var{cancel_string})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 8)
    {
      Cell list = args(0).cell_value ();
      const Array<std::string> tlist = list.cellstr_value ();
      octave_idx_type nel = tlist.numel ();
      std::list<std::string> list_lst;
      for (octave_idx_type i = 0; i < nel; i++)
        list_lst.push_back (tlist(i));

      std::string mode = args(1).string_value ();

      Matrix size_matrix = args(2).matrix_value ();
      int width = size_matrix(0);
      int height = size_matrix(1);

      Matrix initial_matrix = args(3).matrix_value ();
      nel = initial_matrix.numel ();
      std::list<int> initial_lst;
      for (octave_idx_type i = 0; i < nel; i++)
        initial_lst.push_back (initial_matrix(i));

      std::string name = args(4).string_value ();
      list = args(5).cell_value ();
      const Array<std::string> plist = list.cellstr_value ();
      nel = plist.numel ();
      std::list<std::string> prompt_lst;
      for (octave_idx_type i = 0; i < nel; i++)
        prompt_lst.push_back (plist(i));
      std::string ok_string = args(6).string_value ();
      std::string cancel_string = args(7).string_value ();

      if (! error_state)
        {
          flush_octave_stdout ();

          std::pair<std::list<int>, int> result
            = octave_link::list_dialog (list_lst, mode, width, height,
                                        initial_lst, name, prompt_lst,
                                        ok_string, cancel_string);

          std::list<int> items_lst = result.first;
          nel = items_lst.size ();
          Matrix items (dim_vector (1, nel));
          octave_idx_type i = 0;
          for (std::list<int>::iterator it = items_lst.begin ();
               it != items_lst.end (); it++)
            {
              items.xelem(i++) = *it;
            }

          retval(1) = result.second;
          retval(0) = items;
        }
      else
        error ("invalid arguments");
    }

  return retval;
}

DEFUN (__octave_link_input_dialog__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_input_dialog__ (@var{prompt}, @var{title}, @var{rowscols}, @var{defaults})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 4)
    {
      Cell prompt = args(0).cell_value ();
      Array<std::string> tmp = prompt.cellstr_value ();
      octave_idx_type nel = tmp.numel ();
      std::list<std::string> prompt_lst;
      for (octave_idx_type i = 0; i < nel; i++)
        prompt_lst.push_back (tmp(i));

      std::string title = args(1).string_value ();

      Matrix rc = args(2).matrix_value ();
      nel = rc.rows ();
      std::list<float> nr;
      std::list<float> nc;
      for (octave_idx_type i = 0; i < nel; i++)
        {
          nr.push_back (rc(i,0));
          nc.push_back (rc(i,1));
        }

      Cell defaults = args(3).cell_value ();
      tmp = defaults.cellstr_value ();
      nel = tmp.numel ();
      std::list<std::string> defaults_lst;
      for (octave_idx_type i = 0; i < nel; i++)
        defaults_lst.push_back (tmp(i));

      if (! error_state)
        {
          flush_octave_stdout ();

          std::list<std::string> items_lst
            = octave_link::input_dialog (prompt_lst, title, nr, nc,
                                         defaults_lst);

          nel = items_lst.size ();
          Cell items (dim_vector (nel, 1));
          octave_idx_type i = 0;
          for (std::list<std::string>::iterator it = items_lst.begin ();
               it != items_lst.end (); it++)
            {
              items.xelem(i++) = *it;
            }

          retval = items;
        }
      else
        error ("invalid arguments");
    }

  return retval;
}

DEFUN (__octave_link_show_preferences__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_show_preferences__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  retval = octave_link::show_preferences ();

  return retval;
}

DEFUN (__octave_link_show_doc__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __octave_link_show_doc__ (@var{filename})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;
  std::string file;

  if (args.length () >= 1)
    file = args(0).string_value();

  retval = octave_link::show_doc (file);

  return retval;
}



