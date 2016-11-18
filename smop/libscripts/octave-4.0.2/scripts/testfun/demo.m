## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Command} {} demo @var{name}
## @deftypefnx {Command} {} demo @var{name} @var{n}
## @deftypefnx {Function File} {} demo ("@var{name}")
## @deftypefnx {Function File} {} demo ("@var{name}", @var{n})
##
## Run example code block @var{n} associated with the function @var{name}.
##
## If @var{n} is not specified, all examples are run.
##
## The preferred location for example code blocks is embedded within the script
## m-file immediately following the code that it exercises.  Alternatively,
## the examples may be stored in a file with the same name but no extension
## located on Octave's load path.  To separate examples from regular script
## code all lines are prefixed by @code{%!}.  Each example must also be
## introduced by the keyword @qcode{"demo"} flush left to the prefix with no
## intervening spaces.  The remainder of the example can contain arbitrary
## Octave code.  For example:
##
## @example
## @group
## %!demo
## %! t = 0:0.01:2*pi;
## %! x = sin (t);
## %! plot (t, x);
## %! title ("one cycle of a sine wave");
## %! #-------------------------------------------------
## %! # the figure window shows one cycle of a sine wave
## @end group
## @end example
##
## Note that the code is displayed before it is executed so that a simple
## comment at the end suffices for labeling what is being shown.  For plots,
## labeling can also be done with @code{title} or @code{text}.  It is generally
## @strong{not} necessary to use @code{disp} or @code{printf} within the demo.
##
## Demos are run in a stand-alone function environment with no access to
## external variables.  This means that every demo must have separate
## initialization code.  Alternatively, all demos can be combined into a single
## large demo with the code
##
## @example
## %! input ("Press <enter> to continue: ", "s");
## @end example
##
## @noindent
## between the sections, but this usage is discouraged.  Other techniques to
## avoid multiple initialization blocks include using multiple plots with a new
## @code{figure} command between each plot, or using @code{subplot} to put
## multiple plots in the same window.
##
## Finally, because @code{demo} evaluates within a function context it is not
## possible to define new functions within the code.  Anonymous functions make
## a good substitute in most instances.  If function blocks @strong{must} be
## used then the code @code{eval (example ("function", n))} will allow Octave
## to see them.  This has its own problems, however, as @code{eval} only
## evaluates one line or statement at a time.  In this case the function
## declaration must be wrapped with @qcode{"if 1 <demo stuff> endif"} where
## @qcode{"if"} is on the same line as @qcode{"demo"}.  For example:
##
## @example
## @group
## %!demo if 1
## %!  function y = f(x)
## %!    y = x;
## %!  endfunction
## %!  f(3)
## %! endif
## @end group
## @end example
##
## @seealso{rundemos, example, test}
## @end deftypefn

## FIXME: modify subplot so that gnuplot_has_multiplot == 0 causes it to
## use the current figure window but pause if not plotting in the
## first subplot.

function demo (name, n = 0)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (ischar (n))
    n = str2double (n);
  endif

  if (! (isreal (n) && isscalar (n) && n == fix (n)))
    error ("demo: N must be a scalar integer");
  endif

  [code, idx] = test (name, "grabdemo");

  if (idx == -1)
    warning ("demo: no function %s found", name);
    return;
  elseif (isempty (idx))
    warning ("demo: no demo available for %s", name);
    return;
  elseif (n >= length (idx))
    warning ("demo: only %d demos available for %s", length (idx) - 1, name);
    return;
  endif

  if (n > 0)
    doidx = n;
  else
    doidx = 1:(length (idx) - 1);
  endif
  for i = 1:length (doidx)
    ## Pause between demos
    if (i > 1)
      input ("Press <enter> to continue: ", "s");
    endif

    ## Process each demo without failing
    try
      block = code(idx(doidx(i)):idx(doidx(i)+1)-1);
      ## Use an environment without variables
      eval (["function __demo__ ()\n" block "\nendfunction"]);
      ## Display the code that will be executed before executing it
      printf ("%s example %d:%s\n\n", name, doidx(i), block);
      __demo__;
    catch
      ## Let the programmer know which demo failed.
      printf ("%s example %d: failed\n%s\n", name, doidx(i), lasterr ());
    end_try_catch
    clear __demo__;
  endfor

endfunction


%!demo
%! t = 0:0.01:2*pi;
%! x = sin (t);
%! plot (t, x);
%! title ("one cycle of a sine wave");
%! #-------------------------------------------------
%! # the figure window shows one cycle of a sine wave

%!error demo ()
%!error demo (1, 2, 3)
%!error <N must be a scalar integer> demo ("demo", {1})
%!error <N must be a scalar integer> demo ("demo", ones (2,2))
%!error <N must be a scalar integer> demo ("demo", 1.5)
%!warning <no function .* found> demo ("_%NOT_A_FUNCTION%_");
%!warning <only 1 demos available for demo> demo ("demo", 10);

