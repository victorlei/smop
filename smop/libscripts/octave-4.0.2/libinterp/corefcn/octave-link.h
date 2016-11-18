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

#if !defined (octave_octave_link_h)
#define octave_octave_link_h 1

#include <string>

#include "event-queue.h"

class octave_mutex;
class string_vector;
class workspace_element;

// \class OctaveLink
// \brief Provides threadsafe access to octave.
// \author Jacob Dawid
//
// This class is a wrapper around octave and provides thread safety by
// buffering access operations to octave and executing them in the
// readline event hook, which lives in the octave thread.

class
OCTINTERP_API
octave_link
{
protected:

  octave_link (void);

public:

  virtual ~octave_link (void) { }

  static void generate_events (void)
  {
    if (enabled ())
      instance->do_generate_events ();
  }

  // If disable is TRUE, then no additional events will be processed
  // other than exit.

  static void process_events (bool disable = false)
  {
    if (enabled ())
      {
        if (disable)
          instance->link_enabled = false;

        instance->do_process_events ();
      }
  }

  static void discard_events (void)
  {
    if (enabled ())
      instance->do_discard_events ();
  }

  static bool confirm_shutdown (void)
  {
    bool retval = true;

    if (instance_ok ())
      retval = instance->do_confirm_shutdown ();

    return retval;
  }

  static bool exit (int status)
  {
    bool retval = false;

    if (instance_ok ())
      retval = instance->do_exit (status);

    return retval;
  }

  template <class T>
  static void post_event (T *obj, void (T::*method) (void))
  {
    if (enabled ())
      instance->do_post_event (obj, method);
  }

  template <class T, class A>
  static void post_event (T *obj, void (T::*method) (A), A arg)
  {
    if (enabled ())
      instance->do_post_event (obj, method, arg);
  }

  template <class T, class A>
  static void post_event (T *obj, void (T::*method) (const A&), const A& arg)
  {
    if (enabled ())
      instance->do_post_event (obj, method, arg);
  }

  static void entered_readline_hook (void)
  {
    if (enabled ())
      instance->do_entered_readline_hook ();
  }

  static void finished_readline_hook (void)
  {
    if (enabled ())
      instance->do_finished_readline_hook ();
  }

  static bool
  copy_image_to_clipboard (const std::string& file)
  {
    return enabled () ? instance->do_copy_image_to_clipboard (file) : false;
  }

  static bool
  edit_file (const std::string& file)
  {
    return enabled () ? instance->do_edit_file (file) : false;
  }

  static bool
  prompt_new_edit_file (const std::string& file)
  {
    return enabled () ? instance->do_prompt_new_edit_file (file) : false;
  }

  static int
  message_dialog (const std::string& dlg, const std::string& msg,
                  const std::string& title)
  {
    return enabled () ? instance->do_message_dialog (dlg, msg, title) : 0;
  }

  static std::string
  question_dialog (const std::string& msg, const std::string& title,
                   const std::string& btn1, const std::string& btn2,
                   const std::string& btn3, const std::string& btndef)
  {
    return enabled () ? instance->do_question_dialog (msg, title, btn1,
                                                      btn2, btn3, btndef) : 0;
  }

  static std::pair<std::list<int>, int>
  list_dialog (const std::list<std::string>& list,
               const std::string& mode,
               int width, int height,
               const std::list<int>& initial_value,
               const std::string& name,
               const std::list<std::string>& prompt,
               const std::string& ok_string,
               const std::string& cancel_string)
  {
    return enabled ()
           ? instance->do_list_dialog (list, mode, width, height,
                                       initial_value, name, prompt,
                                       ok_string, cancel_string)
           : std::pair<std::list<int>, int> ();
  }

  static std::list<std::string>
  input_dialog (const std::list<std::string>& prompt,
                const std::string& title,
                const std::list<float>& nr,
                const std::list<float>& nc,
                const std::list<std::string>& defaults)
  {
    return enabled ()
           ? instance->do_input_dialog (prompt, title, nr, nc, defaults)
           : std::list<std::string> ();
  }

  typedef std::list<std::pair<std::string, std::string> > filter_list;

  static std::list<std::string>
  file_dialog (const filter_list& filter, const std::string& title,
               const std::string& filename, const std::string& dirname,
               const std::string& multimode)
  {
    return enabled ()
           ? instance->do_file_dialog (filter, title, filename, dirname,
                                       multimode)
           : std::list<std::string> ();
  }


  static int debug_cd_or_addpath_error (const std::string& file,
                                        const std::string& dir,
                                        bool addpath_option)
  {
    return enabled ()
           ? instance->do_debug_cd_or_addpath_error (file, dir, addpath_option)
           : 0;
  }

  static void change_directory (const std::string& dir)
  {
    if (enabled ())
      instance->do_change_directory (dir);
  }

  // Preserves pending input.
  static void execute_command_in_terminal (const std::string& command)
  {
    if (enabled ())
      instance->do_execute_command_in_terminal (command);
  }

  static void set_workspace (void);

  static void set_workspace (bool top_level,
                             const std::list<workspace_element>& ws)
  {
    if (enabled ())
      instance->do_set_workspace (top_level, instance->debugging, ws);
  }

  static void clear_workspace (void)
  {
    if (enabled ())
      instance->do_clear_workspace ();
  }

  static void set_history (const string_vector& hist)
  {
    if (enabled ())
      instance->do_set_history (hist);
  }

  static void append_history (const std::string& hist_entry)
  {
    if (enabled ())
      instance->do_append_history (hist_entry);
  }

  static void clear_history (void)
  {
    if (enabled ())
      instance->do_clear_history ();
  }

  static void pre_input_event (void)
  {
    if (enabled ())
      instance->do_pre_input_event ();
  }

  static void post_input_event (void)
  {
    if (enabled ())
      instance->do_post_input_event ();
  }

  static void enter_debugger_event (const std::string& file, int line)
  {
    if (enabled ())
      {
        instance->debugging = true;

        instance->do_enter_debugger_event (file, line);
      }
  }

  static void execute_in_debugger_event (const std::string& file, int line)
  {
    if (enabled ())
      instance->do_execute_in_debugger_event (file, line);
  }

  static void exit_debugger_event (void)
  {
    if (enabled () && instance->debugging)
      {
        instance->debugging = false;

        instance->do_exit_debugger_event ();
      }
  }

  static void
  update_breakpoint (bool insert, const std::string& file, int line)
  {
    if (enabled ())
      instance->do_update_breakpoint (insert, file, line);
  }

  static void connect_link (octave_link *);

  static void set_default_prompts (std::string& ps1, std::string& ps2,
                                   std::string& ps4)
  {
    if (enabled ())
      instance->do_set_default_prompts (ps1, ps2, ps4);
  }

  static bool enabled (void)
  {
    return instance_ok () ? instance->link_enabled : false;
  }

  static bool
  show_preferences ()
  {
    if (enabled ())
      {
        instance->do_show_preferences ();
        return true;
      }
    else
      return false;
  }

  static bool
  show_doc (const std::string & file)
  {
    if (enabled ())
      {
        instance->do_show_doc (file);
        return true;
      }
    else
      return false;

  }

private:

  static octave_link *instance;

  // No copying!

  octave_link (const octave_link&);

  octave_link& operator = (const octave_link&);

  static bool instance_ok (void) { return instance != 0; }

protected:

  // Semaphore to lock access to the event queue.
  octave_mutex *event_queue_mutex;

  // Event Queue.
  event_queue gui_event_queue;

  bool debugging;
  bool link_enabled;

  void do_generate_events (void);
  void do_process_events (void);
  void do_discard_events (void);

  template <class T>
  void do_post_event (T *obj, void (T::*method) (void))
  {
    gui_event_queue.add_method (obj, method);
  }

  template <class T, class A>
  void do_post_event (T *obj, void (T::*method) (A), A arg)
  {
    gui_event_queue.add_method (obj, method, arg);
  }

  template <class T, class A>
  void do_post_event (T *obj, void (T::*method) (const A&), const A& arg)
  {
    gui_event_queue.add_method (obj, method, arg);
  }

  void do_entered_readline_hook (void) { }
  void do_finished_readline_hook (void) { }

  virtual bool do_confirm_shutdown (void) = 0;
  virtual bool do_exit (int status) = 0;

  virtual bool do_copy_image_to_clipboard (const std::string& file) = 0;

  virtual bool do_edit_file (const std::string& file) = 0;
  virtual bool do_prompt_new_edit_file (const std::string& file) = 0;

  virtual int
  do_message_dialog (const std::string& dlg, const std::string& msg,
                     const std::string& title) = 0;

  virtual std::string
  do_question_dialog (const std::string& msg, const std::string& title,
                      const std::string& btn1, const std::string& btn2,
                      const std::string& btn3, const std::string& btndef) = 0;

  virtual std::pair<std::list<int>, int>
  do_list_dialog (const std::list<std::string>& list,
                  const std::string& mode,
                  int width, int height,
                  const std::list<int>& initial_value,
                  const std::string& name,
                  const std::list<std::string>& prompt,
                  const std::string& ok_string,
                  const std::string& cancel_string) = 0;

  virtual std::list<std::string>
  do_input_dialog (const std::list<std::string>& prompt,
                   const std::string& title,
                   const std::list<float>& nr,
                   const std::list<float>& nc,
                   const std::list<std::string>& defaults) = 0;

  virtual std::list<std::string>
  do_file_dialog (const filter_list& filter, const std::string& title,
                  const std::string& filename, const std::string& dirname,
                  const std::string& multimode) = 0;

  virtual int
  do_debug_cd_or_addpath_error (const std::string& file,
                                const std::string& dir,
                                bool addpath_option) = 0;

  virtual void do_change_directory (const std::string& dir) = 0;

  virtual void do_execute_command_in_terminal (const std::string& command) = 0;

  virtual void
  do_set_workspace (bool top_level, bool debug,
                    const std::list<workspace_element>& ws) = 0;

  virtual void do_clear_workspace (void) = 0;

  virtual void do_set_history (const string_vector& hist) = 0;
  virtual void do_append_history (const std::string& hist_entry) = 0;
  virtual void do_clear_history (void) = 0;

  virtual void do_pre_input_event (void) = 0;
  virtual void do_post_input_event (void) = 0;

  virtual void
  do_enter_debugger_event (const std::string& file, int line) = 0;

  virtual void
  do_execute_in_debugger_event (const std::string& file, int line) = 0;

  virtual void do_exit_debugger_event (void) = 0;

  virtual void do_update_breakpoint (bool insert,
                                     const std::string& file, int line) = 0;

  virtual void do_set_default_prompts (std::string& ps1, std::string& ps2,
                                       std::string& ps4) = 0;

  virtual void do_show_preferences (void) = 0;

  virtual void do_show_doc (const std::string &file) = 0;
};

#endif // OCTAVELINK_H
