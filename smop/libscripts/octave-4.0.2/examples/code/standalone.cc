#include <iostream>
#include <octave/oct.h>

int
main (void)
{
  std::cout << "Hello Octave world!\n";

  int n = 2;
  Matrix a_matrix = Matrix (n, n);

  for (octave_idx_type i = 0; i < n; i++)
    for (octave_idx_type j = 0; j < n; j++)
      a_matrix(i,j) = (i + 1) * 10 + (j + 1);

  std::cout << a_matrix;

  return 0;
}
