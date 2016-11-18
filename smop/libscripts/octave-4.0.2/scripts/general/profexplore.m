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
## @deftypefn  {Function File} {} profexplore ()
## @deftypefnx {Function File} {} profexplore (@var{data})
## Interactively explore hierarchical profiler output.
##
## Assuming @var{data} is the structure with profile data returned by
## @code{profile (@qcode{"info"})}, this command opens an interactive prompt
## that can be used to explore the call-tree.  Type @kbd{help} to get a list
## of possible commands.  If @var{data} is omitted, @code{profile ("info")}
## is called and used in its place.
## @seealso{profile, profshow}
## @end deftypefn

## Built-in profiler.
## Author: Daniel Kraft <d@domob.eu>

function profexplore (data)

  if (nargin == 0)
    data = profile ("info");
  elseif (nargin != 1)
    print_usage ();
  endif

  ## The actual work is done by a recursive worker function, since that
  ## is an easy way to traverse the tree datastructure.  Here, we just check
  ## the arguments (already done) and give over to it.

  __profexplore_worker (data.FunctionTable, data.Hierarchical, "Top\n", "  ");

endfunction

## This is the worker function.  tree is the current subtree we want to
## display / explore.  parents is a string containing the already "rendered"
## data for the parents which is displayed on top of the list of current
## children.  prefix is the prefix to add to each line rendered; this
## is just a string of spaces to get indentation right.
##
## Returning 0 indicates that the user requested to totally exit the
## explorer, thus also all higher levels should exit immediately.  An integer
## greater zero indicates to exit that many levels since the user wants to go
## up (but not necessarily quit).

function rv = __profexplore_worker (fcn_table, tree, parents, prefix)

  ## Sort children by total time.
  times = -[ tree.TotalTime ];
  [~, p] = sort (times);
  tree = tree(p);

  while (true)

    printf ("\n%s", parents);
    strings = cell (length (tree), 1);
    for i = 1 : length (tree)
      strings{i} = sprintf ("%s: %d calls, %.3f total, %.3f self", ...
                            fcn_table(tree(i).Index).FunctionName, ...
                            tree(i).NumCalls, ...
                            tree(i).TotalTime, tree(i).SelfTime);
      printf ("%s%d) %s\n", prefix, i, strings{i});
    endfor
    printf ("\n");

    cmd = input ("profexplore> ", "s");
    option = fix (str2double (cmd));

    if (strcmp (cmd, "exit") || strcmp (cmd, "quit"))
      rv = 0;
      return;
    elseif (strcmp (cmd, "help"))
      printf ("\nCommands for profile explorer:\n\n");
      printf ("exit   Return to Octave prompt.\n");
      printf ("quit   Return to Octave prompt.\n");
      printf ("help   Display this help message.\n");
      printf ("up [N] Go up N levels, where N is an integer.  Default is 1.\n");
      printf ("N      Go down a level into option N.\n");
    elseif (! isnan (option))
      if (option < 1 || option > length (tree))
        printf ("The chosen option is out of range!\n");
      else
        newParents = sprintf ("%s%s%s\n", parents, prefix, strings{option});
        newPrefix = sprintf ("%s  ", prefix);

        rv = __profexplore_worker (fcn_table, tree(option).Children, ...
                                   newParents, newPrefix);

        if (rv == 0)
          return;
        elseif (rv > 1)
          rv = rv - 1;
          return;
        else
          assert (rv == 1);
          ## It was requested to return to this level, so just stay.
        endif
      endif
    elseif (length (cmd) >= 2 && strcmp (substr (cmd, 1, 2), "up"))
      if (length (cmd) == 2)
        rv = 1;
        return;
      endif

      if (length (cmd) > 3 && cmd(3) == ' ')
        opt = fix (str2double (substr (cmd, 3)));
        if (! isnan (opt) && opt > 0)
          rv = opt;
          return;
        endif
      endif

      printf ("Invalid 'up' command.  Type 'help' for further");
      printf (" information.\n");
    else
      printf ("Unrecognized input.  Type 'help' to get a list of possible");
      printf (" commands.\n");
    endif

  endwhile
endfunction

