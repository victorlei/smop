#include <octave/oct.h>

DEFUN_DLD (oregonator, args, ,
           "The `oregonator'.\n\
\n\
Reference:\n\
\n\
  Oscillations in chemical systems.  IV.  Limit cycle behavior in a\n\
  model of a real chemical reaction. Richard J. Field and Richard\n\
  M. Noyes, The Journal of Chemical Physics, Volume 60 Number 5,\n\
  March 1974.")
{
  ColumnVector dx (3);

  ColumnVector x (args(0).vector_value ());

  dx(0) = 77.27 * (x(1) - x(0)*x(1) + x(0) - 8.375e-06*pow (x(0), 2.0));
  dx(1) = (x(2) - x(0)*x(1) - x(1)) / 77.27;
  dx(2) = 0.161*(x(0) - x(2));

  return octave_value (dx);
}
