#include <iostream>
#include <octave/oct.h>
#include <octave/builtin-defun-decls.h>

int
main (void)
{
  int n = 2;
  Matrix a_matrix = Matrix (n, n);

  for (octave_idx_type i = 0; i < n; i++)
    for (octave_idx_type j = 0; j < n; j++)
      a_matrix(i,j) = (i + 1) * 10 + (j + 1);

  std::cout << "This is a matrix:" << std::endl
            << a_matrix            << std::endl;

  octave_value_list in;
  in(0) = a_matrix;

  octave_value_list out = Fnorm (in, 1);
  double norm_of_the_matrix = out(0).double_value ();

  std::cout << "This is the norm of the matrix:" << std::endl
            << norm_of_the_matrix                << std::endl;

  return 0;
}
