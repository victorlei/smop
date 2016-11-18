/*

Copyright (C) 2006-2015 John W. Eaton

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

/*
   A C-program for MT19937, with initialization improved 2002/2/10.
   Coded by Takuji Nishimura and Makoto Matsumoto.
   This is a faster version by taking Shawn Cokus's optimization,
   Matthe Bellew's simplification, Isaku Wada's real version.
   David Bateman added normal and exponential distributions following
   Marsaglia and Tang's Ziggurat algorithm.

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   Copyright (C) 2004, David Bateman
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote
        products derived from this software without specific prior written
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#ifndef _RANDMTZIG_H
#define _RANDMTZIG_H

#define MT_N 624

#ifdef  __cplusplus
extern "C" {
#endif

/* === Mersenne Twister === */
extern OCTAVE_API void oct_init_by_int (uint32_t s);
extern OCTAVE_API void oct_init_by_array (uint32_t init_key[], int key_length);
extern OCTAVE_API void oct_init_by_entropy (void);
extern OCTAVE_API void oct_set_state (uint32_t save[]);
extern OCTAVE_API void oct_get_state (uint32_t save[]);

/* === Array generators === */
extern OCTAVE_API double oct_randu (void);
extern OCTAVE_API double oct_randn (void);
extern OCTAVE_API double oct_rande (void);

extern OCTAVE_API float oct_float_randu (void);
extern OCTAVE_API float oct_float_randn (void);
extern OCTAVE_API float oct_float_rande (void);

/* === Array generators === */
extern OCTAVE_API void oct_fill_randu (octave_idx_type n, double *p);
extern OCTAVE_API void oct_fill_randn (octave_idx_type n, double *p);
extern OCTAVE_API void oct_fill_rande (octave_idx_type n, double *p);

extern OCTAVE_API void oct_fill_float_randu (octave_idx_type n, float *p);
extern OCTAVE_API void oct_fill_float_randn (octave_idx_type n, float *p);
extern OCTAVE_API void oct_fill_float_rande (octave_idx_type n, float *p);

#ifdef  __cplusplus
}
#endif
#endif
