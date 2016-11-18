#include <octave/oct.h>
#include <octave/ov-struct.h>

DEFUN_DLD (structdemo, args, , "Struct Demo")
{
  octave_value retval;
  int nargin = args.length ();

  if (args.length () == 2)
    {
      octave_scalar_map arg0 = args(0).scalar_map_value ();
      //octave_map arg0 = args(0).map_value ();

      if (! error_state)
        {
          std::string arg1 = args(1).string_value ();

          if (! error_state)
            {
              octave_value tmp = arg0.contents (arg1);
              //octave_value tmp = arg0.contents (arg1)(0);

              if (tmp.is_defined ())
                {
                  octave_scalar_map st;

                  st.assign ("selected", tmp);

                  retval = octave_value (st);
                }
              else
                error ("structdemo: struct does not have a field named '%s'\n",
                       arg1.c_str ());
            }
          else
            error ("structdemo: ARG2 must be a character string");
        }
      else
        error ("structdemo: ARG1 must be a struct");
    }
  else
    print_usage ();

  return retval;
}
