/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_pr_output_h)
#define octave_pr_output_h 1

#include <iosfwd>

#include "oct-cmplx.h"

template <typename T> class Array;
class ComplexMatrix;
class FloatComplexMatrix;
class ComplexDiagMatrix;
class FloatComplexDiagMatrix;
class ComplexNDArray;
class FloatComplexNDArray;
class Matrix;
class FloatMatrix;
class DiagMatrix;
class FloatDiagMatrix;
class NDArray;
class FloatNDArray;
class Range;
class boolMatrix;
class boolNDArray;
class charMatrix;
class charNDArray;
class PermMatrix;
class Cell;
class octave_value;

#include "intNDArray.h"
#include "oct-inttypes.h"


extern OCTINTERP_API void
octave_print_internal (std::ostream& os, bool d,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, char c,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, double d,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, float d,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Matrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const DiagMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatDiagMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const NDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Complex& c,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplex& c,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const ComplexMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const ComplexDiagMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplexMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplexDiagMatrix& cm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const ComplexNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const FloatComplexNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const PermMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Range& r,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const boolMatrix& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const boolNDArray& m,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const charMatrix& chm,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0,
                       bool pr_as_string = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const charNDArray& nda,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0,
                       bool pr_as_string = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const std::string& s,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Array<std::string>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int8>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint8>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int16>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint16>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int32>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint32>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_int64>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const intNDArray<octave_uint64>& sa,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<int8_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<uint8_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<int16_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<uint16_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<int32_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<uint32_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<int64_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_int<uint64_t>& sa,
                       bool pr_as_read_syntax = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const Cell& cell,
                       bool pr_as_read_syntax = false,
                       int extra_indent = 0,
                       bool pr_as_string = false);

extern OCTINTERP_API void
octave_print_internal (std::ostream& os, const octave_value& ov,
                       bool pr_as_read_syntax = false);

// TRUE means that the dimensions of empty objects should be printed
// like this: x = [](2x0).
extern bool Vprint_empty_dimensions;

// TRUE means don't put empty lines in output
extern bool Vcompact_format;

#endif
