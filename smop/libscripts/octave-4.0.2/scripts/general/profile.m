## Copyright (C) 2012-2015 Daniel Kraft
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Command} {} profile on
## @deftypefnx {Command} {} profile off
## @deftypefnx {Command} {} profile resume
## @deftypefnx {Command} {} profile clear
## @deftypefnx {Function File} {@var{S} =} profile ("status")
## @deftypefnx {Function File} {@var{T} =} profile ("info")
## Control the built-in profiler.
##
## @table @code
## @item profile on
## Start the profiler, clearing all previously collected data if there is any.
##
## @item profile off
## Stop profiling.  The collected data can later be retrieved and examined
## with @code{T = profile ("info")}.
##
## @item profile clear
## Clear all collected profiler data.
##
## @item profile resume
## Restart profiling without clearing the old data.  All newly collected
## statistics are added to the existing ones.
##
## @item @var{S} = profile ("status")
## Return a structure with information about the current status of the profiler.
## At the moment, the only field is @code{ProfilerStatus} which is either
## @qcode{"on"} or @qcode{"off"}.
##
## @item @var{T} = profile ("info")
## Return the collected profiling statistics in the structure @var{T}.  The
## flat profile is returned in the field @code{FunctionTable} which is an
## array of structures, each entry corresponding to a function which was called
## and for which profiling statistics are present.  In addition, the field
## @code{Hierarchical} contains the hierarchical call tree.  Each node has an
## index into the @code{FunctionTable} identifying the function it corresponds
## to as well as data fields for number of calls and time spent at this level
## in the call tree.
## @seealso{profshow, profexplore}
## @end table
## @end deftypefn

## Built-in profiler.
## Author: Daniel Kraft <d@domob.eu>

function retval = profile (option)

  if (nargin != 1)
    print_usage ();
  endif

  switch (option)
    case "on"
      __profiler_reset__ ();
      __profiler_enable__ (true);

    case "off"
      __profiler_enable__ (false);

    case "clear"
      __profiler_reset__ ();

    case "resume"
      __profiler_enable__ (true);

    case "status"
      enabled = __profiler_enable__ ();
      if (enabled)
        enabled = "on";
      else
        enabled = "off";
      endif
      retval = struct ("ProfilerStatus", enabled);

    case "info"
      [flat, tree] = __profiler_data__ ();
      retval = struct ("FunctionTable", flat, "Hierarchical", tree);

    otherwise
      warning ("profile: Unrecognized option '%s'", option);
      print_usage ();

  endswitch

endfunction


%!demo
%! profile on;
%! A = rand (100);
%! B = expm (A);
%! profile off;
%! profile resume;
%! C = sqrtm (A);
%! profile off;
%! T = profile ("info");
%! profshow (T);

%!test
%! on_struct.ProfilerStatus = "on";
%! off_struct.ProfilerStatus = "off";
%! profile ("on");
%! result = logm (rand (200) + 10 * eye (200));
%! assert (profile ("status"), on_struct);
%! profile ("off");
%! assert (profile ("status"), off_struct);
%! profile ("resume");
%! result = logm (rand (200) + 10 * eye (200));
%! profile ("off");
%! assert (profile ("status"), off_struct);
%! info = profile ("info");
%! assert (isstruct (info));
%! assert (size (info), [1, 1]);
%! assert (fieldnames (info), {"FunctionTable"; "Hierarchical"});
%! ftbl = info.FunctionTable;
%! assert (fieldnames (ftbl), {"FunctionName"; "TotalTime"; "NumCalls"; "IsRecursive"; "Parents"; "Children"});
%! hier = info.Hierarchical;
%! assert (fieldnames (hier), {"Index"; "SelfTime"; "TotalTime"; "NumCalls"; "Children"});
%! profile ("clear");
%! info = profile ("info");
%! assert (isstruct (info));
%! assert (size (info), [1, 1]);
%! assert (fieldnames (info), {"FunctionTable"; "Hierarchical"});
%! ftbl = info.FunctionTable;
%! assert (size (ftbl), [0, 1]);
%! assert (fieldnames (ftbl), {"FunctionName"; "TotalTime"; "NumCalls"; "IsRecursive"; "Parents"; "Children"});
%! hier = info.Hierarchical;
%! assert (size (hier), [0, 1]);
%! assert (fieldnames (hier), {"Index"; "SelfTime"; "NumCalls"; "Children"});

## Test input validation
%!error profile ()
%!error profile ("on", 2)
%!error profile ("INVALID_OPTION")

