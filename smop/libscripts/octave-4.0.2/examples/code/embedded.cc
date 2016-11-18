#include <iostream>
#include <octave/oct.h>
#include <octave/octave.h>
#include <octave/parse.h>
#include <octave/toplev.h>

int
main (void)
{
  string_vector argv (2);
  argv(0) = "embedded";
  argv(1) = "-q";

  octave_main (2, argv.c_str_vec (), 1);

  octave_idx_type n = 2;
  octave_value_list in;

  for (octave_idx_type i = 0; i < n; i++)
    in(i) = octave_value (5 * (i + 2));

  octave_value_list out = feval ("gcd", in, 1);

  if (! error_state && out.length () > 0)
    std::cout << "GCD of ["
              << in(0).int_value ()
              << ", "
              << in(1).int_value ()
              << "] is " << out(0).int_value ()
              << std::endl;
  else
    std::cout << "invalid\n";

  clean_up_and_exit (0);
}
