#include <octave/oct.h>

DEFUN_DLD (stringdemo, args, , "String Demo")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();
  else
    {
      charMatrix ch = args(0).char_matrix_value ();

      if (! error_state)
        {
          retval(1) = octave_value (ch, '\'');  // Single Quote String

          octave_idx_type nr = ch.rows ();
          for (octave_idx_type i = 0; i < nr / 2; i++)
            {
              std::string tmp = ch.row_as_string (i);
              ch.insert (ch.row_as_string (nr-i-1).c_str (), i, 0);
              ch.insert (tmp.c_str (), nr-i-1, 0);
            }
          retval(0) = octave_value (ch, '"');  // Double Quote String
        }
    }
  return retval;
}
