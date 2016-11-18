#include <octave/oct.h>

DEFUN_DLD (paramdemo, args, nargout, "Parameter Check Demo")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else if (nargout != 0)
    error ("paramdemo: OUTPUT argument required");
  else
    {
      NDArray m = args(0).array_value ();
      double min_val = -10.0;
      double max_val = 10.0;
      octave_stdout << "Properties of input array:\n";
      if (m.any_element_is_negative ())
        octave_stdout << "  includes negative values\n";
      if (m.any_element_is_inf_or_nan ())
        octave_stdout << "  includes Inf or NaN values\n";
      if (m.any_element_not_one_or_zero ())
        octave_stdout << "  includes other values than 1 and 0\n";
      if (m.all_elements_are_int_or_inf_or_nan ())
        octave_stdout << "  includes only int, Inf or NaN values\n";
      if (m.all_integers (min_val, max_val))
        octave_stdout << "  includes only integers in [-10,10]\n";
    }
  return retval;
}
