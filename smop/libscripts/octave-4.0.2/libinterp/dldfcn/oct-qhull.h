/*

Copyright (C) 2012-2015 John W. Eaton

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

#if !defined (octave_oct_qhull_h)
#define octave_oct_qhull_h 1

#include <cstdio>

extern "C" {

#if defined (HAVE_LIBQHULL_LIBQHULL_H)
# include <libqhull/libqhull.h>
# include <libqhull/qset.h>
# include <libqhull/geom.h>
# include <libqhull/poly.h>
# include <libqhull/io.h>
#elif defined (HAVE_QHULL_LIBQHULL_H) || defined (HAVE_QHULL_QHULL_H)
# if defined (HAVE_QHULL_LIBQHULL_H)
#  include <qhull/libqhull.h>
# else
#  include <qhull/qhull.h>
# endif
# include <qhull/qset.h>
# include <qhull/geom.h>
# include <qhull/poly.h>
# include <qhull/io.h>
#elif defined (HAVE_LIBQHULL_H) || defined (HAVE_QHULL_H)
# if defined (HAVE_LIBQHULL_H)
#  include <libqhull.h>
# else
#  include <qhull.h>
# endif
# include <qset.h>
# include <geom.h>
# include <poly.h>
# include <io.h>
#endif

}

#endif
