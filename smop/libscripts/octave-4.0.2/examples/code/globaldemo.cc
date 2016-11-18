#include <octave/oct.h>

DEFUN_DLD (globaldemo, args, , "Global Demo")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else
    {
      std::string s = args(0).string_value ();
      if (! error_state)
        {
          octave_value tmp = get_global_value (s, true);
          if (tmp.is_defined ())
            retval = tmp;
          else
            retval = "Global variable not found";

          set_global_value ("a", 42.0);
        }
    }
  return retval;
}
