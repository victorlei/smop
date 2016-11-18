## -*- texinfo -*-
## @deftypefn  {Function File} {} FIRfilter ()
## @deftypefnx {Function File} {} FIRfilter (@var{p})
## Create a FIR filter with polynomial @var{p} as coefficient vector.
## @end deftypefn

function f = FIRfilter (p)

  if (nargin == 0)
    f.polynomial = @polynomial ([1]);
  elseif (nargin == 1)
    if (isa (p, "polynomial"))
      f.polynomial = p;
    else
      error ("FIRfilter: expecting polynomial as input argument");
    endif
  else
    print_usage ();
  endif
  f = class (f, "FIRfilter");
endfunction
