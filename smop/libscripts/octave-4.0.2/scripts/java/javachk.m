## Copyright (C) 2014-2015 Philip Nienhuis
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
## @deftypefn  {Function File} {} javachk (@var{feature})
## @deftypefnx {Function File} {} javachk (@var{feature}, @var{component})
## @deftypefnx {Function File} {@var{msg} =} javachk (@dots{})
## Check for the presence of the Java @var{feature} in the current session
## and print or return an error message if it is not.
##
## Possible features are:
##
## @table @asis
## @item @qcode{"awt"}
## Abstract Window Toolkit for GUIs.
##
## @item @qcode{"desktop"}
## Interactive desktop is running.
##
## @item @qcode{"jvm"}
## Java Virtual Machine.
##
## @item @qcode{"swing"}
## Swing components for lightweight GUIs.
## @end table
##
## If @var{feature} is supported and
##
## @itemize @bullet
## @item
## no output argument is requested:
##
## Return an empty string
##
## @item
## an output argument is requested:
##
## Return a struct with fields @qcode{"feature"} and @qcode{"identifier"}
## both empty
## @end itemize
##
## If @var{feature} is not supported and
##
## @itemize @bullet
## @item
## no output argument is requested:
##
## Emit an error message
##
## @item
## an output argument is requested:
##
## Return a struct with field @qcode{"feature"} set to @var{feature} and field
## @qcode{"identifier"} set to @var{component}
## @end itemize
##
## The optional input @var{component} will be used in place of @var{feature}
## in any error messages for greater specificity.
##
## @code{javachk} determines if specific Java features are available in an
## Octave session.  This function is provided for scripts which may alter
## their behavior based on the availability of Java.  The feature
## @qcode{"desktop"} is never available as Octave has no Java-based desktop.
## Other features may be available if Octave was compiled with the Java
## Interface and Java is installed.
##
## @seealso{usejava, error}
## @end deftypefn

## Author: Philip Nienhuis <prnienhuis at users.sf.net>
## Created: 2014-04-19

function msg = javachk (feature, component = "")

  msg = "";
  chk = false;
  switch (feature)
    ## For each feature, try methods() on a Java class of a feature
    case "awt"
      try
        dum = methods ("java.awt.Frame");
        chk = true;
      end_try_catch
    case "desktop"
      ## Octave has no Java based GUI/desktop, leave chk = false
    case "jvm"
      try
        dum = methods ("java.lang.Runtime");
        chk = true;
      end_try_catch
    case "swing"
      try
        dum = methods ("javax.swing.Popup");
        chk = true;
      end_try_catch
    otherwise
      error ("javachk: unrecognized feature '%s', can be one of 'awt'|'desktop'|'jvm'|'swing'\n", feature);
  endswitch

  if (! chk)
    ## Desired feature not present
    if (nargout >= 1)
      msg.message = sprintf ("javachk: %s is not supported", feature);
      msg.identifier = component;
    else
      if (! isempty (component))
        err = sprintf ("javachk: %s is not supported\n", component);
      else
        err = sprintf ("javachk: %s is not supported\n", feature);
      endif
      error (err);
    endif
  endif

endfunction


%!error <javachk: desktop is not supported> javachk ("desktop")
%!error <Java DESKTOP is not supported> javachk ("desktop", "Java DESKTOP")
%!test
%! msg = javachk ("desktop");
%! assert (msg.message, "javachk: desktop is not supported");
%! assert (msg.identifier, "");
%!test
%! msg = javachk ("desktop", "Java DESKTOP");
%! assert (msg.message, "javachk: desktop is not supported");
%! assert (msg.identifier, "Java DESKTOP");

%!testif HAVE_JAVA
%! assert (javachk ("jvm"), "");

## Test input validation
%!error <javachk: unrecognized feature 'foobar'> javachk ("foobar")

