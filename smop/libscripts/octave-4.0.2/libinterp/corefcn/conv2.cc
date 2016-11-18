/*

Copyright (C) 1999-2015 Andy Adler
Copyright (C) 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-convn.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"
#include "utils.h"

enum Shape { SHAPE_FULL, SHAPE_SAME, SHAPE_VALID };

DEFUN (conv2, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} conv2 (@var{A}, @var{B})\n\
@deftypefnx {Built-in Function} {} conv2 (@var{v1}, @var{v2}, @var{m})\n\
@deftypefnx {Built-in Function} {} conv2 (@dots{}, @var{shape})\n\
Return the 2-D convolution of @var{A} and @var{B}.\n\
\n\
The size of the result is determined by the optional @var{shape} argument\n\
which takes the following values\n\
\n\
@table @asis\n\
@item @var{shape} = @qcode{\"full\"}\n\
Return the full convolution.  (default)\n\
\n\
@item @var{shape} = @qcode{\"same\"}\n\
Return the central part of the convolution with the same size as @var{A}.\n\
The central part of the convolution begins at the indices\n\
@code{floor ([size(@var{B})/2] + 1)}.\n\
\n\
@item @var{shape} = @qcode{\"valid\"}\n\
Return only the parts which do not include zero-padded edges.\n\
The size of the result is @code{max (size (A) - size (B) + 1, 0)}.\n\
@end table\n\
\n\
When the third argument is a matrix, return the convolution of the matrix\n\
@var{m} by the vector @var{v1} in the column direction and by the vector\n\
@var{v2} in the row direction.\n\
@seealso{conv, convn}\n\
@end deftypefn")
{
  octave_value retval;
  octave_value tmp;
  int nargin = args.length ();
  std::string shape = "full";   // default
  bool separable = false;
  convn_type ct;

  if (nargin < 2)
    {
      print_usage ();
      return retval;
    }
  else if (nargin == 3)
    {
      if (args(2).is_string ())
        shape = args(2).string_value ();
      else
        separable = true;
    }
  else if (nargin >= 4)
    {
      separable = true;
      shape = args(3).string_value ();
    }

  if (args(0).ndims () > 2 || args(1).ndims () > 2)
    {
      error ("conv2: A and B must be 1-D vectors or 2-D matrices");
      return retval;
    }

  if (shape == "full")
    ct = convn_full;
  else if (shape == "same")
    ct = convn_same;
  else if (shape == "valid")
    ct = convn_valid;
  else
    {
      error ("conv2: SHAPE type not valid");
      print_usage ();
      return retval;
    }

  if (separable)
    {
      // If user requests separable, check first two params are vectors

      if (! (1 == args(0).rows () || 1 == args(0).columns ())
          || ! (1 == args(1).rows () || 1 == args(1).columns ()))
        {
          print_usage ();
          return retval;
        }

      if (args(0).is_single_type () || args(1).is_single_type ()
          || args(2).is_single_type ())
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ()
              || args(2).is_complex_type ())
            {
              FloatComplexMatrix a (args(2).float_complex_matrix_value ());
              if (args(1).is_real_type () && args(2).is_real_type ())
                {
                  FloatColumnVector v1 (args(0).float_vector_value ());
                  FloatRowVector v2 (args(1).float_vector_value ());
                  retval = convn (a, v1, v2, ct);
                }
              else
                {
                  FloatComplexColumnVector v1 (args(0).float_complex_vector_value ());
                  FloatComplexRowVector v2 (args(1).float_complex_vector_value ());
                  retval = convn (a, v1, v2, ct);
                }
            }
          else
            {
              FloatColumnVector v1 (args(0).float_vector_value ());
              FloatRowVector v2 (args(1).float_vector_value ());
              FloatMatrix a (args(2).float_matrix_value ());
              retval = convn (a, v1, v2, ct);
            }
        }
      else
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ()
              || args(2).is_complex_type ())
            {
              ComplexMatrix a (args(2).complex_matrix_value ());
              if (args(1).is_real_type () && args(2).is_real_type ())
                {
                  ColumnVector v1 (args(0).vector_value ());
                  RowVector v2 (args(1).vector_value ());
                  retval = convn (a, v1, v2, ct);
                }
              else
                {
                  ComplexColumnVector v1 (args(0).complex_vector_value ());
                  ComplexRowVector v2 (args(1).complex_vector_value ());
                  retval = convn (a, v1, v2, ct);
                }
            }
          else
            {
              ColumnVector v1 (args(0).vector_value ());
              RowVector v2 (args(1).vector_value ());
              Matrix a (args(2).matrix_value ());
              retval = convn (a, v1, v2, ct);
            }
        }
    } // if (separable)
  else
    {
      if (args(0).is_single_type () || args(1).is_single_type ())
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ())
            {
              FloatComplexMatrix a (args(0).float_complex_matrix_value ());
              if (args(1).is_real_type ())
                {
                  FloatMatrix b (args(1).float_matrix_value ());
                  retval = convn (a, b, ct);
                }
              else
                {
                  FloatComplexMatrix b (args(1).float_complex_matrix_value ());
                  retval = convn (a, b, ct);
                }
            }
          else
            {
              FloatMatrix a (args(0).float_matrix_value ());
              FloatMatrix b (args(1).float_matrix_value ());
              retval = convn (a, b, ct);
            }
        }
      else
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ())
            {
              ComplexMatrix a (args(0).complex_matrix_value ());
              if (args(1).is_real_type ())
                {
                  Matrix b (args(1).matrix_value ());
                  retval = convn (a, b, ct);
                }
              else
                {
                  ComplexMatrix b (args(1).complex_matrix_value ());
                  retval = convn (a, b, ct);
                }
            }
          else
            {
              Matrix a (args(0).matrix_value ());
              Matrix b (args(1).matrix_value ());
              retval = convn (a, b, ct);
            }
        }

    } // if (separable)

  return retval;
}

/*
%!test
%! c = [0,1,2,3;1,8,12,12;4,20,24,21;7,22,25,18];
%! assert (conv2 ([0,1;1,2], [1,2,3;4,5,6;7,8,9]), c);

%!test
%! c = single ([0,1,2,3;1,8,12,12;4,20,24,21;7,22,25,18]);
%! assert (conv2 (single ([0,1;1,2]), single ([1,2,3;4,5,6;7,8,9])), c);

%!test
%! c = [1,4,4;5,18,16;14,48,40;19,62,48;15,48,36];
%! assert (conv2 (1:3, 1:2, [1,2;3,4;5,6]), c);

%!assert (conv2 (1:3, 1:2, [1,2;3,4;5,6], "full"),
%!        conv2 (1:3, 1:2, [1,2;3,4;5,6]));

%% Test shapes
%!shared A, B, C
%! A = rand (3, 4);
%! B = rand (4);
%! C = conv2 (A, B);
%!assert (conv2 (A,B, "full"), C)
%!assert (conv2 (A,B, "same"), C(3:5,3:6))
%!assert (conv2 (A,B, "valid"), zeros (0, 1))
%!assert (size (conv2 (B,A, "valid")), [2 1])

%!test
%! B = rand (5);
%! C = conv2 (A, B);
%!assert (conv2 (A,B, "full"), C)
%!assert (conv2 (A,B, "same"), C(3:5,3:6))
%!assert (conv2 (A,B, "valid"), zeros (0, 0))
%!assert (size (conv2 (B,A, "valid")), [3 2])

%% Clear shared variables so they are not reported for tests below
%!shared

%% Test cases from Bug #34893
%!assert (conv2 ([1:5;1:5], [1:2], "same"), [4 7 10 13 10; 4 7 10 13 10])
%!assert (conv2 ([1:5;1:5]', [1:2]', "same"), [4 7 10 13 10; 4 7 10 13 10]')
%!assert (conv2 ([1:5;1:5], [1:2], "valid"), [4 7 10 13; 4 7 10 13])
%!assert (conv2 ([1:5;1:5]', [1:2]', "valid"), [4 7 10 13; 4 7 10 13]')

%!test
%! rand ("seed", 42);
%! x = rand (100);
%! y = ones (5);
%! A = conv2 (x, y)(5:end-4,5:end-4);
%! B = conv2 (x, y, "valid");
%! assert (B, A);   # Yes, this test is for *exact* equivalence.


%% Test input validation
%!error conv2 ()
%!error conv2 (1)
%!error <must be 1-D vectors or 2-D matrices> conv2 (ones (2), ones (2,2,2))
%!error <SHAPE type not valid> conv2 (1,2, "NOT_A_SHAPE")
%% Test alternate calling form which should be 2 vectors and a matrix
%!error conv2 (ones (2), 1, 1)
%!error conv2 (1, ones (2), 1)
*/

DEFUN (convn, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{C} =} convn (@var{A}, @var{B})\n\
@deftypefnx {Built-in Function} {@var{C} =} convn (@var{A}, @var{B}, @var{shape})\n\
Return the n-D convolution of @var{A} and @var{B}.\n\
\n\
The size of the result is determined by the optional @var{shape} argument\n\
which takes the following values\n\
\n\
@table @asis\n\
@item @var{shape} = @qcode{\"full\"}\n\
Return the full convolution.  (default)\n\
\n\
@item @var{shape} = @qcode{\"same\"}\n\
Return central part of the convolution with the same size as @var{A}.\n\
The central part of the convolution begins at the indices\n\
@code{floor ([size(@var{B})/2] + 1)}.\n\
\n\
@item @var{shape} = @qcode{\"valid\"}\n\
Return only the parts which do not include zero-padded edges.\n\
The size of the result is @code{max (size (A) - size (B) + 1, 0)}.\n\
@end table\n\
\n\
@seealso{conv2, conv}\n\
@end deftypefn")
{
  octave_value retval;
  octave_value tmp;
  int nargin = args.length ();
  std::string shape = "full";   // default
  convn_type ct;

  if (nargin < 2 || nargin > 3)
    {
      print_usage ();
      return retval;
    }
  else if (nargin == 3)
    {
      if (args(2).is_string ())
        shape = args(2).string_value ();
      else
        {
          error ("convn: SHAPE must be a string");
          return retval;
        }
    }

  if (shape == "full")
    ct = convn_full;
  else if (shape == "same")
    ct = convn_same;
  else if (shape == "valid")
    ct = convn_valid;
  else
    {
      error ("convn: SHAPE type not valid");
      print_usage ();
      return retval;
    }

  if (args(0).is_single_type () || args(1).is_single_type ())
    {
      if (args(0).is_complex_type () || args(1).is_complex_type ())
        {
          FloatComplexNDArray a (args(0).float_complex_array_value ());
          if (args(1).is_real_type ())
            {
              FloatNDArray b (args(1).float_array_value ());
              retval = convn (a, b, ct);
            }
          else
            {
              FloatComplexNDArray b (args(1).float_complex_array_value ());
              retval = convn (a, b, ct);
            }
        }
      else
        {
          FloatNDArray a (args(0).float_array_value ());
          FloatNDArray b (args(1).float_array_value ());
          retval = convn (a, b, ct);
        }
    }
  else
    {
      if (args(0).is_complex_type () || args(1).is_complex_type ())
        {
          ComplexNDArray a (args(0).complex_array_value ());
          if (args(1).is_real_type ())
            {
              NDArray b (args(1).array_value ());
              retval = convn (a, b, ct);
            }
          else
            {
              ComplexNDArray b (args(1).complex_array_value ());
              retval = convn (a, b, ct);
            }
        }
      else
        {
          NDArray a (args(0).array_value ());
          NDArray b (args(1).array_value ());
          retval = convn (a, b, ct);
        }
    }

  return retval;
}

/*
## Check for bug #39314
%!test
%! v = reshape ([1 2], [1 1 2]);
%! assert (convn (v, v), reshape ([1 4 4], [1 1 3]));
%! assert (convn (v, v, "same"), reshape ([4 4], [1 1 2]));
%! assert (convn (v, v, "valid"), 4);

## The following test may look weird since we are using the output
## of convn to test itself.  However, because calculations are done
## differently based on the shape option, it will help to catch some
## bugs.  See also bug #39314.
## FIXME: The "valid" option uses an entirely different code path
##        through C++ and Fortran to calculate inner convolution.
##        The terms in the convolution added in reverse order compared
##        to the "full" option.  This produces differences on the order
##        of tens of eps.  This should be fixed, but in the meantime
##        the tests will be marked as xtests.
%!shared a, b, c
%! ## test 3D by 3D
%! a = rand (10, 10, 10);
%! b = rand (3, 3, 3);
%! c = convn (a, b, "full");
%!assert (convn (a, b, "same"), c(2:11,2:11,2:11))
%!xtest
%! assert (convn (a, b, "valid"), c(3:10,3:10,3:10));
%!
%!test
%! ## test 3D by 2D
%! a = rand (10, 10, 10);
%! b = rand (3, 3);
%! c = convn (a, b, "full");
%!assert (convn (a, b, "same"), c(2:11,2:11,:))
%!xtest
%! assert (convn (a, b, "valid"), c(3:10,3:10,:))
%!
%!test
%! ## test 2D by 3D
%! a = rand (10, 10);
%! b = rand (3, 3, 3);
%! c = convn (a, b, "full");
%!assert (convn (a, b, "same"), c(2:11,2:11,2))
%!assert (convn (a, b, "valid"), c(3:10,3:10,3:2))  # a 7x7x0 matrix
%!
%!test
%! ## test multiple different number of dimensions, with odd and even numbers
%! a = rand (10, 15, 7, 8, 10);
%! b = rand (4, 3, 2, 3);
%! c = convn (a, b, "full");
%!assert (convn (a, b, "same"), c(3:12,2:16,2:8,2:9,:))
%!xtest
%! assert (convn (a, b, "valid"), c(4:10,3:15,2:7,3:8,:))

%!test
%! a = reshape (floor (magic (16) /10), [4 8 4 2]);
%! b = reshape (magic (6), [4 3 3]);
%! c = zeros (7, 10, 6, 2);
%! c(:,:,1,1) = [
%!    875  1415  1215   741   288   264   635  1109   687   171
%!    110   467  1551  1790  1891  1651  1165   900   659   568
%!    883  1047  1475  1964  2181  2302  2117  1674   579   234
%!    940  2330  3099  2573  2306  2207  2442  2918  2272  1004
%!    161   500  1564  2066  2355  2270  2099  1621  1144   831
%!    644   622   886  1121  1652  1967  1907  1668   529   228
%!    160   752  1232   768   360   284   668  1132  1380   864];
%! c(:,:,2,1) = [
%!    150  1174  1903  1971  2030  1719  1467  1420  1220   472
%!    986  2243  2603  2385  2308  2530  2971  3181  2266   768
%!    914  2443  3750  3782  3976  3821  3723  3709  2599  1178
%!   1922  3374  5198  5472  5563  5853  5794  5543  3578  1820
%!   1060  2471  3846  3724  3682  3803  3812  3927  2876  1390
%!    470  2078  3283  3225  2701  2265  2165  2261  2324  1124
%!    700  1130  1486  1515  1830  2097  2081  2028  1009   348];
%! c(:,:,3,1) = [
%!   1350  2127  2461  2082  1694  1909  2230  2621  1681   683
%!    877  2473  4362  4556  4543  4314  3879  3703  2863  1497
%!   1934  4219  5874  6117  5966  6051  5984  5714  3891  1562
%!   1927  5997  8573  8456  8517  8025  7957  8101  6121  2500
%!   1558  3533  5595  6064  6453  6491  6275  5743  3794  1832
%!   1950  2762  3455  3423  4019  4578  4807  4857  2304   907
%!    525  1860  2731  2392  1872  1724  1961  2312  2315  1141];
%! c(:,:,4,1) = [
%!    150  1317  2230  2621  2996  2767  2472  2049  1514   583
%!   1429  3056  3879  3703  3756  3964  4394  4570  3111  1250
%!   1833  4037  5984  5714  5846  5788  5883  6129  4157  2011
%!   3143  5469  7957  8101  8063  8475  8564  8439  5306  2538
%!   2001  4514  6275  5743  5391  5389  5578  6110  4473  1953
%!    817  3196  4807  4857  4229  3659  3477  3375  3208  1400
%!    750  1365  1961  2312  2840  2993  2722  2344  1092   323];
%! c(:,:,5,1) = [
%!    475   734  1296  1352  1400  1595  1557  1517   960   490
%!    751  1977  2831  2746  2607  2665  2733  2833  2186   912
%!   1065  3142  4344  4150  3768  3734  3876  4086  3366  1327
%!    976  3712  5530  5921  6158  5802  5481  5071  3821  1491
%!   1397  2996  3971  4003  4088  4180  4199  4146  2649   985
%!   1273  2121  2555  2247  2378  2624  2908  3229  1788   705
%!    365  1108  1530  1652  1550  1407  1274  1127   889   264];
%! c(:,:,6,1) = [
%!      0   133   345   683   982  1058   960   623   310   100
%!    437   806  1313  1332  1383  1391  1397  1370   864   495
%!    928  1573  2201  1928  1864  1932  2183  2445  1557   855
%!   1199  2083  2739  2573  2507  2656  2786  2928  1795   736
%!    912  1997  2404  2028  1692  1591  1803  2159  1603   599
%!    345  1092  1526  1666  1593  1437  1275  1116   863   253
%!     50   235   510   811   998   894   615   318    77     0];
%! c(:,:,1,2) = [
%!    840  1350  1176   697   293   320   674  1153   717   180
%!    142   490  1563  1824  1929  1604  1132   857   624   587
%!    890  1084  1539  1979  2238  2333  2072  1610   509   202
%!    966  2263  3034  2518  2250  2235  2512  2992  2305  1016
%!    200   561  1607  2107  2361  2277  2030  1548  1102   818
%!    652   631   922  1128  1670  1997  1895  1665   467   197
%!    160   744  1192   692   292   256   708  1208  1448   900];
%! c(:,:,2,2) = [
%!    179  1199  1886  1987  1997  1716  1479  1383  1215   485
%!    988  2213  2552  2358  2304  2615  3011  3210  2246   744
%!    921  2483  3747  3768  3960  3835  3712  3698  2588  1183
%!   1903  3416  5254  5490  5572  5826  5761  5505  3502  1814
%!   1064  2507  3825  3666  3680  3748  3821  3958  2892  1395
%!    495  2129  3277  3228  2566  2216  2154  2250  2390  1154
%!    700  1105  1472  1524  1856  2113  2059  2019   975   325];
%! c(:,:,3,2) = [
%!   1302  2104  2439  2006  1723  1931  2280  2685  1678   690
%!    877  2507  4408  4580  4523  4233  3852  3647  2850  1516
%!   1949  4238  5895  6143  6018  6063  5930  5656  3847  1538
%!   1953  5975  8547  8433  8407  8060  7955  8069  6170  2506
%!   1621  3536  5624  6117  6459  6456  6180  5666  3735  1815
%!   1904  2751  3429  3366  4122  4622  4840  4864  2242   882
%!    517  1843  2674  2337  1777  1686  2005  2367  2385  1175];
%! c(:,:,4,2) = [
%!    198  1346  2280  2685  2980  2759  2396  1982  1497   576
%!   1413  2994  3852  3647  3756  4035  4418  4595  3109  1231
%!   1873  4025  5930  5656  5792  5772  5909  6152  4185  2035
%!   3110  5510  7955  8069  8139  8456  8541  8439  5276  2541
%!   1964  4462  6180  5666  5315  5409  5631  6178  4536  1998
%!    869  3215  4840  4864  4121  3579  3420  3386  3271  1430
%!    725  1361  2005  2367  2925  3006  2667  2297  1054   325];
%! c(:,:,5,2) = [
%!    462   754  1285  1359  1441  1605  1556  1488   938   488
%!    729  1967  2788  2732  2608  2683  2744  2830  2195   912
%!   1052  3139  4302  4101  3742  3730  3895  4103  3403  1335
%!   1007  3725  5577  5964  6165  5754  5407  5006  3846  1507
%!   1375  2969  3951  3990  4144  4183  4200  4150  2661   998
%!   1258  2090  2495  2188  2403  2664  2954  3279  1814   723
%!    388  1127  1551  1673  1525  1390  1253  1139   912   275];
%! c(:,:,6,2) = [
%!     19   147   384   716  1016  1059   927   570   276    80
%!    441   791  1298  1320  1401  1396  1409  1367   865   500
%!    932  1537  2155  1870  1860  1946  2221  2487  1584   874
%!   1201  2067  2705  2538  2512  2687  2806  2971  1812   756
%!    925  1976  2363  1971  1636  1600  1844  2239  1664   626
%!    372  1133  1558  1687  1570  1401  1243  1122   883   264
%!     60   270   556   857  1024   870   569   282    66     0];
%!assert (convn(a, b, "full"), c)
%!assert (convn(a, b, "same"), c(3:6,2:9,2:5,:))
%!assert (convn(a, b, "valid"), c(4,3:8,3:4,:))

## test correct class
%!assert (class (convn (rand(5), rand(3))), "double")
%!assert (class (convn (rand(5, "single"), rand(3))), "single")
%!assert (class (convn (rand(5), rand(3, "single"))), "single")
%!assert (class (convn (true (5), rand(3))), "double")
%!assert (class (convn (true (5), rand(3, "single"))), "single")
%!assert (class (convn (ones(5, "uint8"), rand(3))), "double")
%!assert (class (convn (rand (3, "single"), ones(5, "uint8"))), "single")

%!error convn ()
%!error convn (1)
%!error <SHAPE type not valid> convn (1,2, "NOT_A_SHAPE")
%!error convn (rand (3), 1, 1)
*/
