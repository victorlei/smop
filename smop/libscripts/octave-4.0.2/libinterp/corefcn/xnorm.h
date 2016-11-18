/*

Copyright (C) 2008-2015 VZLU Prague, a.s.

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

// author: Jaroslav Hajek <highegg@gmail.com>

#if !defined (octave_xnorm_h)
#define octave_xnorm_h 1

#include "oct-norm.h"

class octave_value;

extern OCTINTERP_API octave_value
xnorm (const octave_value& x, const octave_value& p);

extern OCTINTERP_API octave_value
xcolnorms (const octave_value& x, const octave_value& p);

extern OCTINTERP_API octave_value
xrownorms (const octave_value& x, const octave_value& p);

extern OCTINTERP_API octave_value
xfrobnorm (const octave_value& x);

#endif
