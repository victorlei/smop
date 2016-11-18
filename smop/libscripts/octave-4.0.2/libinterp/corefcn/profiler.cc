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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "defun.h"
#include "oct-time.h"
#include "ov-struct.h"
#include "pager.h"
#include "profiler.h"

profile_data_accumulator::stats::stats ()
  : time (0.0), calls (0), recursive (false),
    parents (), children ()
{}

octave_value
profile_data_accumulator::stats::function_set_value (const function_set& list)
{
  const octave_idx_type n = list.size ();

  RowVector retval (n);
  octave_idx_type i = 0;
  for (function_set::const_iterator p = list.begin (); p != list.end (); ++p)
    {
      retval(i) = *p;
      ++i;
    }
  assert (i == n);

  return retval;
}

profile_data_accumulator::tree_node::tree_node (tree_node* p, octave_idx_type f)
  : parent (p), fcn_id (f), children (), time (0.0), calls (0)
{}

profile_data_accumulator::tree_node::~tree_node ()
{
  for (child_map::iterator i = children.begin (); i != children.end (); ++i)
    delete i->second;
}

profile_data_accumulator::tree_node*
profile_data_accumulator::tree_node::enter (octave_idx_type fcn)
{
  tree_node* retval;

  child_map::iterator pos = children.find (fcn);
  if (pos == children.end ())
    {
      retval = new tree_node (this, fcn);
      children[fcn] = retval;
    }
  else
    retval = pos->second;

  ++retval->calls;
  return retval;
}

profile_data_accumulator::tree_node*
profile_data_accumulator::tree_node::exit (octave_idx_type /* fcn */)
{
  // FIXME: These assert statements don't make sense if profile() is called
  //        from within a function hierarchy to begin with.  See bug #39587.
  //  assert (parent);
  //  assert (fcn_id == fcn);

  return parent;
}

void
profile_data_accumulator::tree_node::build_flat (flat_profile& data) const
{
  // If this is not the top-level node, update profile entry for this function.
  if (fcn_id != 0)
    {
      stats& entry = data[fcn_id - 1];

      entry.time += time;
      entry.calls += calls;

      assert (parent);
      if (parent->fcn_id != 0)
        {
          entry.parents.insert (parent->fcn_id);
          data[parent->fcn_id - 1].children.insert (fcn_id);
        }

      if (! entry.recursive)
        for (const tree_node* i = parent; i; i = i->parent)
          if (i->fcn_id == fcn_id)
            {
              entry.recursive = true;
              break;
            }
    }

  // Recurse on children.
  for (child_map::const_iterator i = children.begin ();
       i != children.end (); ++i)
    i->second->build_flat (data);
}

octave_value
profile_data_accumulator::tree_node::get_hierarchical (double* total) const
{
  /* Note that we don't generate the entry just for this node, but rather
     a struct-array with entries for all children.  This way, the top-node
     (for which we don't want a real entry) generates already the final
     hierarchical profile data.  */

  const octave_idx_type n = children.size ();

  Cell rv_indices (n, 1);
  Cell rv_times (n, 1);
  Cell rv_totals (n, 1);
  Cell rv_calls (n, 1);
  Cell rv_children (n, 1);

  octave_idx_type i = 0;
  for (child_map::const_iterator p = children.begin ();
       p != children.end (); ++p)
    {
      const tree_node& entry = *p->second;
      double child_total = entry.time;

      rv_indices(i) = octave_value (p->first);
      rv_times(i) = octave_value (entry.time);
      rv_calls(i) = octave_value (entry.calls);
      rv_children(i) = entry.get_hierarchical (&child_total);
      rv_totals(i) = octave_value (child_total);

      if (total)
        *total += child_total;

      ++i;
    }
  assert (i == n);

  octave_map retval;

  retval.assign ("Index", rv_indices);
  retval.assign ("SelfTime", rv_times);
  retval.assign ("TotalTime", rv_totals);
  retval.assign ("NumCalls", rv_calls);
  retval.assign ("Children", rv_children);

  return retval;
}

profile_data_accumulator::profile_data_accumulator ()
  : known_functions (), fcn_index (),
    enabled (false), call_tree (0), last_time (-1.0)
{}

profile_data_accumulator::~profile_data_accumulator ()
{
  if (call_tree)
    delete call_tree;
}

void
profile_data_accumulator::set_active (bool value)
{
  if (value)
    {
      // Create a call-tree top-node if there isn't yet one.
      if (! call_tree)
        call_tree = new tree_node (0, 0);

      // Let the top-node be the active one.  This ensures we have a clean
      // fresh start collecting times.
      active_fcn = call_tree;
    }
  else
    {
      // Make sure we start with fresh timing if we're re-enabled later.
      last_time = -1.0;
    }

  enabled = value;
}

void
profile_data_accumulator::enter_function (const std::string& fcn)
{
  // The enter class will check and only call us if the profiler is active.
  assert (is_active ());
  assert (call_tree);

  // If there is already an active function, add to its time before
  // pushing the new one.
  if (active_fcn != call_tree)
    add_current_time ();

  // Map the function's name to its index.
  octave_idx_type fcn_idx;
  fcn_index_map::iterator pos = fcn_index.find (fcn);
  if (pos == fcn_index.end ())
    {
      known_functions.push_back (fcn);
      fcn_idx = known_functions.size ();
      fcn_index[fcn] = fcn_idx;
    }
  else
    fcn_idx = pos->second;

  active_fcn = active_fcn->enter (fcn_idx);
  last_time = query_time ();

}

void
profile_data_accumulator::exit_function (const std::string& fcn)
{
  assert (call_tree);
  // FIXME: This assert statements doesn't make sense if profile() is called
  //        from within a function hierarchy to begin with.  See bug #39587.
  //assert (active_fcn != call_tree);

  // Usually, if we are disabled this function is not even called.  But the
  // call disabling the profiler is an exception.  So also check here
  // and only record the time if enabled.
  if (is_active ())
    add_current_time ();

  fcn_index_map::iterator pos = fcn_index.find (fcn);
  // FIXME: This assert statements doesn't make sense if profile() is called
  //        from within a function hierarchy to begin with.  See bug #39587.
  //assert (pos != fcn_index.end ());
  active_fcn = active_fcn->exit (pos->second);

  // If this was an "inner call", we resume executing the parent function
  // up the stack.  So note the start-time for this!
  last_time = query_time ();
}

void
profile_data_accumulator::reset (void)
{
  if (is_active ())
    {
      error ("Can't reset active profiler.");
      return;
    }

  known_functions.clear ();
  fcn_index.clear ();

  if (call_tree)
    {
      delete call_tree;
      call_tree = 0;
    }

  last_time = -1.0;
}

octave_value
profile_data_accumulator::get_flat (void) const
{
  octave_value retval;

  const octave_idx_type n = known_functions.size ();

  flat_profile flat (n);

  if (call_tree)
    {
      call_tree->build_flat (flat);

      Cell rv_names (n, 1);
      Cell rv_times (n, 1);
      Cell rv_calls (n, 1);
      Cell rv_recursive (n, 1);
      Cell rv_parents (n, 1);
      Cell rv_children (n, 1);

      for (octave_idx_type i = 0; i != n; ++i)
        {
          rv_names(i) = octave_value (known_functions[i]);
          rv_times(i) = octave_value (flat[i].time);
          rv_calls(i) = octave_value (flat[i].calls);
          rv_recursive(i) = octave_value (flat[i].recursive);
          rv_parents(i) = stats::function_set_value (flat[i].parents);
          rv_children(i) = stats::function_set_value (flat[i].children);
        }

      octave_map m;

      m.assign ("FunctionName", rv_names);
      m.assign ("TotalTime", rv_times);
      m.assign ("NumCalls", rv_calls);
      m.assign ("IsRecursive", rv_recursive);
      m.assign ("Parents", rv_parents);
      m.assign ("Children", rv_children);

      retval = m;
    }
  else
    {
      static const char *fn[] =
      {
        "FunctionName",
        "TotalTime",
        "NumCalls",
        "IsRecursive",
        "Parents",
        "Children",
        0
      };

      static octave_map m (dim_vector (0, 1), string_vector (fn));

      retval = m;
    }

  return retval;
}

octave_value
profile_data_accumulator::get_hierarchical (void) const
{
  octave_value retval;

  if (call_tree)
    retval = call_tree->get_hierarchical ();
  else
    {
      static const char *fn[] =
      {
        "Index",
        "SelfTime",
        "NumCalls",
        "Children",
        0
      };

      static octave_map m (dim_vector (0, 1), string_vector (fn));

      retval = m;
    }

  return retval;
}

double
profile_data_accumulator::query_time (void) const
{
  octave_time now;

  // FIXME: is this volatile declaration really needed?
  // See bug #34210 for additional details.
  volatile double dnow = now.double_value ();

  return dnow;
}

void
profile_data_accumulator::add_current_time (void)
{
  const double t = query_time ();
  assert (last_time >= 0.0 && last_time <= t);

  assert (call_tree && active_fcn != call_tree);
  active_fcn->add_time (t - last_time);
}

profile_data_accumulator profiler;

// Enable or disable the profiler data collection.
DEFUN (__profiler_enable__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} __profiler_enable__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  const int nargin = args.length ();
  if (nargin > 0)
    {
      if (nargin > 1)
        {
          print_usage ();
          return retval;
        }

      profiler.set_active (args(0).bool_value ());
    }

  retval(0) = profiler.is_active ();

  return retval;
}

// Clear all collected profiling data.
DEFUN (__profiler_reset__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} __profiler_reset__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;
  const int nargin = args.length ();

  if (nargin > 0)
    warning ("profiler_reset: ignoring extra arguments");

  profiler.reset ();

  return retval;
}

// Query the timings collected by the profiler.
DEFUN (__profiler_data__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Function File} {} __profiler_data__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;
  const int nargin = args.length ();

  if (nargin > 0)
    warning ("profiler_data: ignoring extra arguments");

  if (nargout > 1)
    retval(1) = profiler.get_hierarchical ();
  retval(0) = profiler.get_flat ();

  return retval;
}

