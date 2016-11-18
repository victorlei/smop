#include <octave/oct.h>
#include <octave/Cell.h>

DEFUN_DLD (celldemo, args, , "Cell Demo")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else
    {
      Cell c = args(0).cell_value ();
      if (! error_state)
        for (octave_idx_type i = 0; i < c.numel (); i++)
          {
            retval(i) = c(i);          // using operator syntax
            //retval(i) = c.elem (i);  // using method syntax
          }
    }

  return retval;
}
