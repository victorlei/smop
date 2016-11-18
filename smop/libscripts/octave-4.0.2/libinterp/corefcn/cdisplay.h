/*

Copyright (C) 2014-2015 John W. Eaton

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

#if !defined (octave_cdisplay_h)
#define octave_cdisplay_h 1

#ifdef __cplusplus
extern "C" {
#endif

OCTINTERP_API extern const char *
octave_get_display_info (int *ht, int *wd, int *dp, double *rx, double *ry,
                         int *dpy_avail);

#ifdef __cplusplus
}
#endif

#endif
