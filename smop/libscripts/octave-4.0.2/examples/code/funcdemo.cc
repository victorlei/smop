#include <octave/oct.h>
#include <octave/parse.h>

DEFUN_DLD (funcdemo, args, nargout, "Function Demo")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();
  else
    {
      octave_value_list newargs;
      for (octave_idx_type i = nargin - 1; i > 0; i--)
        newargs(i-1) = args(i);
      if (args(0).is_function_handle () || args(0).is_inline_function ())
        {
          octave_function *fcn = args(0).function_value ();
          if (! error_state)
            retval = feval (fcn, newargs, nargout);
        }
      else if (args(0).is_string ())
        {
          std::string fcn = args(0).string_value ();
          if (! error_state)
            retval = feval (fcn, newargs, nargout);
        }
      else
        error ("funcdemo: INPUT must be string, inline, or function handle");
    }
  return retval;
}
