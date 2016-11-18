/*

Copyright (C) 2005-2015 David Bateman
Copyright (C) 1998-2005 Andy Adler

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

#if !defined (octave_sparse_util_h)
#define octave_sparse_util_h 1

#ifdef HAVE_CHOLMOD

// FIXME this overload is here due to API change in SuiteSparse (3.1 -> 3.2)
extern OCTAVE_API void SparseCholError (int status, char *file,
                                        int line, char *message);
extern OCTAVE_API void SparseCholError (int status, const char *file,
                                        int line, const char *message);
extern OCTAVE_API int SparseCholPrint (const char *fmt, ...);

#endif //HAVE_CHOLMOD

extern OCTAVE_API bool
sparse_indices_ok (octave_idx_type *r, octave_idx_type *c,
                   octave_idx_type nrows, octave_idx_type ncols,
                   octave_idx_type nnz);

#endif
