## Copyright (C) 2006-2015 John W. Eaton
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

## Test %!error usage

%!function g ()
%!  error ("foo");
%!endfunction
%!function f ()
%!  g ();
%!endfunction
%!error <foo> f ()

%!function g ()
%!  error ("foo\n");
%!endfunction
%!function f ()
%!  g
%!endfunction
%!error <foo> f ()

%!error error ()

%!error <foo> error ("foo\n")

## Test %!warning usage

%!function g ()
%!  warning ("foo");
%!endfunction
%!function f ()
%!  g;
%!endfunction
%!warning <foo> f ()

%!test
%! st.identifier = "backtrace";
%! ws = warning ("query", "backtrace");
%! warning ("on", "backtrace");
%! st.state = "on";
%! assert (warning ("query", "backtrace"), st);
%! warning ("off", "backtrace");
%! st.state = "off";
%! assert (warning ("query", "backtrace"), st);
%! warning (ws.state, "backtrace");

%!shared t1_opts, t2_opts, t1_id, t1_state, saved_opts, saved_id, saved_state
%! saved_opts = warning ();
%! saved_id = {saved_opts.identifier};
%! saved_state = {saved_opts.state};
%! warning ("off", "all");
%! assert (warning (), struct ("identifier", {"all"}, "state", {"off"}));
%! warning ("off", "all");
%! warning (saved_opts);
%! t1_opts = struct ("identifier", {"foo:bar"}, "state", {"off"});
%! t1_id = {t1_opts.identifier};
%! t1_state = {t1_opts.state};
%! warning (t1_opts);
%! t2_opts = struct ("identifier", [saved_id, t1_id], "state", [saved_state, t1_state]);
%! assert (warning (), t2_opts);
%! warning ("off", "all");
%! warning (saved_opts);

## Bug 36393

%!test
%! w0 = warning;
%! warnoffId = "MATLAB:singularMatrix";
%! warnstat = warning ("query", warnoffId);
%! warnoff = warnstat;
%! warnoff.state = "off";
%! warning (warnoff); %update warning status
%! warning (warnstat); %reset warning status
%! w = warning;
%! assert (w, w0);

## Test usage() function

%!function g ()
%!  usage ("foo");
%!endfunction
%!function f ()
%!  g ();
%!endfunction
%!error <foo> f ()

%!function g ()
%!  usage ("foo");
%!endfunction
%!function f ()
%!  g
%!endfunction
%!error <foo> f ()

%!error usage ()

%!error <foo> usage ("foo\n")

