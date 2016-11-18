## Copyright (C) 2005-2015 Paul Kienzle
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
##
## Original version by Paul Kienzle distributed as free software in the
## public domain.

## -*- texinfo -*-
## @deftypefn  {Function File} {} fail (@var{code})
## @deftypefnx {Function File} {} fail (@var{code}, @var{pattern})
## @deftypefnx {Function File} {} fail (@var{code}, "warning")
## @deftypefnx {Function File} {} fail (@var{code}, "warning", @var{pattern})
##
## Return true if @var{code} fails with an error message matching
## @var{pattern}, otherwise produce an error.
##
## @var{code} must be in the form of a string that is passed to the Octave
## interpreter via the @code{evalin} function, i.e., a (quoted) string constant
## or a string variable.
##
## Note that if @var{code} runs successfully, rather than failing, the error
## printed is:
##
## @example
##           expected error <.> but got none
## @end example
##
## If called with two arguments, the return value will be true only if
## @var{code} fails with an error message containing @var{pattern}
## (case sensitive).  If the code fails with a different error than the one
## specified in @var{pattern} then the message produced is:
##
## @example
## @group
##           expected <@var{pattern}>
##           but got <text of actual error>
## @end group
## @end example
##
## The angle brackets are not part of the output.
##
## When called with the @qcode{"warning"} option @code{fail} will produce an
## error if executing the code produces no warning.
## @seealso{assert, error}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function retval = fail (code, pattern, warning_pattern)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  ## Parse input arguments
  test_warning = false;
  if (nargin == 1)
    pattern = "";
  elseif (nargin == 2 && ! strcmp (pattern, "warning"))
    ## Normal error test
  elseif (nargin >= 2 && strcmp (pattern, "warning"))
    test_warning = true;
    if (nargin == 2)
      pattern = "";
    else
      pattern = warning_pattern;
    endif
  else
    print_usage ();
  endif

  ## Match any nonempty message
  if (isempty (pattern))
    pattern = ".";
  endif

  ## Allow assert (fail ())
  if (nargout)
    retval = true;
  endif

  if (test_warning)
    ## Perform the warning test.
    ## Clear old warnings.
    lastwarn ("");
    ## Make sure warnings are turned on.
    wstate = warning ("query", "quiet");
    warning ("on", "quiet");
    try
      evalin ("caller", [code ";"]);
      ## Retrieve new warnings.
      warn = lastwarn ();
      warning (wstate.state, "quiet");
      if (isempty (warn))
        msg = sprintf ("expected warning <%s> but got none", pattern);
      else
        if (! isempty (regexp (warn, pattern, "once")))
          return;
        endif
        msg = sprintf ("expected warning <%s>\nbut got <%s>", pattern, warn);
      endif
    catch
      warning (wstate.state, "quiet");
      err = lasterr ();
      msg = sprintf ("expected warning <%s>\nbut got error <%s>", pattern, err);
    end_try_catch

  else
    ## Perform the error test.
    try
      evalin ("caller", [code ";"]);
      msg = sprintf ("expected error <%s> but got none", pattern);
    catch
      err = lasterr ();
      if (! isempty (regexp (err, pattern, "once")))
        return;
      endif
      msg = sprintf ("expected error <%s>\nbut got <%s>", pattern, err);
    end_try_catch
  endif

  ## If we get here, then code didn't fail or error didn't match.
  error (msg);

endfunction


%!fail ("[1,2]*[2,3]", "nonconformant")
%!fail ("fail ('[1,2]*[2;3]', 'nonconformant')", "expected error <nonconformant> but got none")
%!fail ("fail ('[1,2]*[2,3]', 'usage:')", "expected error <usage:>\nbut got.*nonconformant")
%!fail ("warning ('test warning')", "warning", "test warning");

#%!fail ("warning ('next test')",'warning','next test');  # only allowed one warning test?!?

## Test that fail() itself will generate an error
%!error <expected error> fail ("1")
%!error <'__a__' undefined> fail ("__a__*[2;3]", "nonconformant")
%!error <expected error .usage:>  fail ("__a__*[2,3]", "usage:")
%!error <warning failure> fail ("warning ('warning failure')", "warning", "success")

## Test input validation
%!error fail ()
%!error fail (1,2,3,4)
%!error fail (1, "nowarning", "foo")

