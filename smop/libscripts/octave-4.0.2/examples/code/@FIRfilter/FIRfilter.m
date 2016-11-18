## -*- texinfo -*-
## @deftypefn  {Function File} {} FIRfilter ()
## @deftypefnx {Function File} {} FIRfilter (@var{p})
## Create a FIR filter with polynomial @var{p} as coefficient vector.
## @end deftypefn

function f = FIRfilter (p)

  f.polynomial = [];
  if (nargin == 0)
    p = @polynomial ([1]);
  elseif (nargin == 1)
    if (!isa (p, "polynomial"))
      error ("FIRfilter: expecting polynomial as input argument");
    endif
  else
    print_usage ();
  endif
  f = class (f, "FIRfilter", p);
endfunction
