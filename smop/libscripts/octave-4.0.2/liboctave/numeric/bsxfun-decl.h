/*

Copyright (C) 2009-2015 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague

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

#if !defined (octave_bsxfun_decl_h)
#define octave_bsxfun_decl_h 1

#define BSXFUN_OP_DECL(OP, ARRAY, API) \
extern API ARRAY bsxfun_ ## OP (const ARRAY&, const ARRAY&);

#define BSXFUN_OP2_DECL(OP, ARRAY, ARRAY1, ARRAY2, API) \
extern API ARRAY bsxfun_ ## OP (const ARRAY1&, const ARRAY2&);

#define BSXFUN_REL_DECL(OP, ARRAY, API) \
extern API boolNDArray bsxfun_ ## OP (const ARRAY&, const ARRAY&);

#define BSXFUN_STDOP_DECLS(ARRAY, API) \
  BSXFUN_OP_DECL (add, ARRAY, API) \
  BSXFUN_OP_DECL (sub, ARRAY, API) \
  BSXFUN_OP_DECL (mul, ARRAY, API) \
  BSXFUN_OP_DECL (div, ARRAY, API) \
  BSXFUN_OP_DECL (pow, ARRAY, API) \
  BSXFUN_OP_DECL (min, ARRAY, API) \
  BSXFUN_OP_DECL (max, ARRAY, API)

#define BSXFUN_MIXED_INT_DECLS(INT_TYPE, API)         \
  BSXFUN_OP2_DECL (pow, INT_TYPE, INT_TYPE, NDArray, API)       \
  BSXFUN_OP2_DECL (pow, INT_TYPE, INT_TYPE, FloatNDArray, API)  \
  BSXFUN_OP2_DECL (pow, INT_TYPE, NDArray, INT_TYPE, API)       \
  BSXFUN_OP2_DECL (pow, INT_TYPE, FloatNDArray, INT_TYPE, API)

#define BSXFUN_STDREL_DECLS(ARRAY, API) \
  BSXFUN_REL_DECL (eq, ARRAY, API) \
  BSXFUN_REL_DECL (ne, ARRAY, API) \
  BSXFUN_REL_DECL (lt, ARRAY, API) \
  BSXFUN_REL_DECL (le, ARRAY, API) \
  BSXFUN_REL_DECL (gt, ARRAY, API) \
  BSXFUN_REL_DECL (ge, ARRAY, API)

#endif
