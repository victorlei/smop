/*

Copyright (C) 2014-2015 Julien Bect
Copyright (C) 2012-2015 Daniel Kraft

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

#if !defined (octave_profiler_h)
#define octave_profiler_h 1

#include <cstddef>
#include <map>
#include <set>
#include <string>
#include <vector>

class octave_value;

class
OCTINTERP_API
profile_data_accumulator
{
public:

  // This is a utility class that can be used to call the enter/exit
  // functions in a manner protected from stack unwinding.
  template<class T> class enter
  {
  private:

    profile_data_accumulator& acc;
    std::string fcn;
    bool is_active;

  public:

    enter (profile_data_accumulator& a, const T& t) : acc (a)
    {
      // A profiling block cannot be active if the profiler is not
      is_active = acc.is_active ();

      if (is_active)
        {
          fcn = t.profiler_name ();

          // NOTE: The test f != "" must be kept to prevent a blank line showing
          //  up in profiler statistics.  See bug #39524.  The root cause is that
          //  the function name is not set for the recurring readline hook function.
          if (fcn == "")
            is_active = false;  // Inactive profiling block
          else
            acc.enter_function (fcn);
        }
    }

    ~enter ()
    {
      if (is_active)
        acc.exit_function (fcn);
    }

  private:

    // No copying!
    enter (const enter&);
    enter& operator = (const enter&);
  };

  profile_data_accumulator (void);
  virtual ~profile_data_accumulator ();

  bool is_active (void) const { return enabled; }
  void set_active (bool);

  void reset (void);

  octave_value get_flat (void) const;
  octave_value get_hierarchical (void) const;

private:

  // One entry in the flat profile (i.e., a collection of data for a single
  // function).  This is filled in when building the flat profile from the
  // hierarchical call tree.
  struct stats
  {
    stats ();

    double time;
    unsigned calls;

    bool recursive;

    typedef std::set<octave_idx_type> function_set;
    function_set parents;
    function_set children;

    // Convert a function_set list to an Octave array of indices.
    static octave_value function_set_value (const function_set&);
  };

  typedef std::vector<stats> flat_profile;

  // Store data for one node in the call-tree of the hierarchical profiler
  // data we collect.
  class tree_node
  {
  public:

    tree_node (tree_node*, octave_idx_type);
    virtual ~tree_node ();

    void add_time (double dt) { time += dt; }

    // Enter a child function.  It is created in the list of children if it
    // wasn't already there.  The now-active child node is returned.
    tree_node* enter (octave_idx_type);

    // Exit function.  As a sanity-check, it is verified that the currently
    // active function actually is the one handed in here.  Returned is the
    // then-active node, which is our parent.
    tree_node* exit (octave_idx_type);

    void build_flat (flat_profile&) const;

    // Get the hierarchical profile for this node and its children.  If total
    // is set, accumulate total time of the subtree in that variable as
    // additional return value.
    octave_value get_hierarchical (double* total = 0) const;

  private:

    tree_node* parent;
    octave_idx_type fcn_id;

    typedef std::map<octave_idx_type, tree_node*> child_map;
    child_map children;

    // This is only time spent *directly* on this level, excluding children!
    double time;

    unsigned calls;

    // No copying!
    tree_node (const tree_node&);
    tree_node& operator = (const tree_node&);
  };

  // Each function we see in the profiler is given a unique index (which
  // simply counts starting from 1).  We thus have to map profiler-names to
  // those indices.  For all other stuff, we identify functions by their index.

  typedef std::vector<std::string> function_set;
  typedef std::map<std::string, octave_idx_type> fcn_index_map;

  function_set known_functions;
  fcn_index_map fcn_index;

  bool enabled;

  tree_node* call_tree;
  tree_node* active_fcn;

  // Store last timestamp we had, when the currently active function was called.
  double last_time;

  // These are private as only the unwind-protecting inner class enter
  // should be allowed to call them.
  void enter_function (const std::string&);
  void exit_function (const std::string&);

  // Query a timestamp, used for timing calls (obviously).
  // This is not static because in the future, maybe we want a flag
  // in the profiler or something to choose between cputime, wall-time,
  // user-time, system-time, ...
  double query_time () const;

  // Add the time elapsed since last_time to the function we're currently in.
  // This is called from two different positions, thus it is useful to have
  // it as a seperate function.
  void add_current_time (void);

  // No copying!
  profile_data_accumulator (const profile_data_accumulator&);
  profile_data_accumulator& operator = (const profile_data_accumulator&);
};

// The instance used.
extern OCTINTERP_API profile_data_accumulator profiler;

// Helper macro to profile a block of code.

#define BEGIN_PROFILER_BLOCK(classname) \
  { \
    profile_data_accumulator::enter<classname> pe (profiler, *this);

#define END_PROFILER_BLOCK \
  }  // end of block => call pe's destructor

#endif
