#include <octave/oct.h>

DEFUN_DLD (addtwomatrices, args, , "Add A to B")
{
  int nargin = args.length ();

  if (nargin != 2)
    print_usage ();
  else
    {
      NDArray A = args(0).array_value ();
      NDArray B = args(1).array_value ();
      if (! error_state)
        return octave_value (A + B);
    }

  return octave_value_list ();
}
