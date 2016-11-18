/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#if !defined (octave_sparse_xpow_h)
#define octave_sparse_xpow_h 1

#include "oct-cmplx.h"

class SparseMatrix;
class SparseComplexMatrix;
class octave_value;

extern octave_value xpow (const SparseMatrix& a, double b);
extern octave_value xpow (const SparseComplexMatrix& a, double b);

extern octave_value elem_xpow (double a, const SparseMatrix& b);
extern octave_value elem_xpow (double a, const SparseComplexMatrix& b);

extern octave_value elem_xpow (const SparseMatrix& a, double b);
extern octave_value elem_xpow (const SparseMatrix& a, const SparseMatrix& b);
extern octave_value elem_xpow (const SparseMatrix& a, const Complex& b);
extern octave_value elem_xpow (const SparseMatrix& a,
                               const SparseComplexMatrix& b);

extern octave_value elem_xpow (const Complex& a, const SparseMatrix& b);
extern octave_value elem_xpow (const Complex& a,
                               const SparseComplexMatrix& b);

extern octave_value elem_xpow (const SparseComplexMatrix& a, double b);
extern octave_value elem_xpow (const SparseComplexMatrix& a,
                               const SparseMatrix& b);
extern octave_value elem_xpow (const SparseComplexMatrix& a,
                               const Complex& b);
extern octave_value elem_xpow (const SparseComplexMatrix& a,
                               const SparseComplexMatrix& b);

#endif
