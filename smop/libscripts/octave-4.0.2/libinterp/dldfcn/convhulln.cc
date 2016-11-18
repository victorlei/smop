/*

Copyright (C) 2000-2015 Kai Habel

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
29. July 2000 - Kai Habel: first release
2002-04-22 Paul Kienzle
* Use warning(...) function rather than writing to cerr
2006-05-01 Tom Holroyd
* add support for consistent winding in all dimensions; output is
* guaranteed to be simplicial.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-locbuf.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "parse.h"
#include "unwind-prot.h"

#if defined (HAVE_QHULL)
# include "oct-qhull.h"
# if defined (NEED_QHULL_VERSION)
char qh_version[] = "convhulln.oct 2007-07-24";
# endif
#endif

static void
close_fcn (FILE *f)
{
  gnulib::fclose (f);
}

static bool
octave_qhull_dims_ok (octave_idx_type dim, octave_idx_type n, const char *who)
{
  if (sizeof (octave_idx_type) > sizeof (int))
    {
      int maxval = std::numeric_limits<int>::max ();

      if (dim > maxval || n > maxval)
        {
          error ("%s: dimension too large for Qhull", who);
          return false;
        }
    }

  return true;
}

DEFUN_DLD (convhulln, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{h} =} convhulln (@var{pts})\n\
@deftypefnx {Loadable Function} {@var{h} =} convhulln (@var{pts}, @var{options})\n\
@deftypefnx {Loadable Function} {[@var{h}, @var{v}] =} convhulln (@dots{})\n\
Compute the convex hull of the set of points @var{pts}.\n\
\n\
@var{pts} is a matrix of size [n, dim] containing n points in a space of\n\
dimension dim.\n\
\n\
The hull @var{h} is an index vector into the set of points and specifies\n\
which points form the enclosing hull.\n\
\n\
An optional second argument, which must be a string or cell array of strings,\n\
contains options passed to the underlying qhull command.\n\
See the documentation for the Qhull library for details\n\
@url{http://www.qhull.org/html/qh-quick.htm#options}.\n\
The default options depend on the dimension of the input:\n\
\n\
@itemize\n\
@item 2D, 3D, 4D: @var{options} = @code{@{\"Qt\"@}}\n\
\n\
@item 5D and higher: @var{options} = @code{@{\"Qt\", \"Qx\"@}}\n\
@end itemize\n\
\n\
If @var{options} is not present or @code{[]} then the default arguments are\n\
used.  Otherwise, @var{options} replaces the default argument list.\n\
To append user options to the defaults it is necessary to repeat the\n\
default arguments in @var{options}.  Use a null string to pass no arguments.\n\
\n\
If the second output @var{v} is requested the volume of the enclosing\n\
convex hull is calculated.\n\n\
@seealso{convhull, delaunayn, voronoin}\n\
@end deftypefn")
{
  octave_value_list retval;

#if defined (HAVE_QHULL)

  int nargin = args.length ();
  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  Matrix points (args(0).matrix_value ());
  const octave_idx_type dim = points.columns ();
  const octave_idx_type num_points = points.rows ();

  if (! octave_qhull_dims_ok (dim, num_points, "convhulln"))
    return retval;

  points = points.transpose ();

  std::string options;

  if (dim <= 4)
    options = " Qt";
  else
    options = " Qt Qx";

  if (nargin == 2)
    {
      if (args(1).is_string ())
        options = " " + args(1).string_value ();
      else if (args(1).is_empty ())
        ; // Use default options.
      else if (args(1).is_cellstr ())
        {
          options = "";

          Array<std::string> tmp = args(1).cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += " " + tmp(i);
        }
      else
        {
          error ("convhulln: OPTIONS must be a string, cell array of strings, or empty");
          return retval;
        }
    }

  boolT ismalloc = false;

  unwind_protect frame;

  // Replace the outfile pointer with stdout for debugging information.
#if defined (OCTAVE_HAVE_WINDOWS_FILESYSTEM) && ! defined (OCTAVE_HAVE_POSIX_FILESYSTEM)
  FILE *outfile = gnulib::fopen ("NUL", "w");
#else
  FILE *outfile = gnulib::fopen ("/dev/null", "w");
#endif
  FILE *errfile = stderr;

  if (outfile)
    frame.add_fcn (close_fcn, outfile);
  else
    {
      error ("convhulln: unable to create temporary file for output");
      return retval;
    }

  // qh_new_qhull command and points arguments are not const...

  std::string cmd = "qhull" + options;

  OCTAVE_LOCAL_BUFFER (char, cmd_str, cmd.length () + 1);

  strcpy (cmd_str, cmd.c_str ());

  int exitcode = qh_new_qhull (dim, num_points, points.fortran_vec (),
                               ismalloc, cmd_str, outfile, errfile);
  if (! exitcode)
    {
      bool nonsimp_seen = false;

      octave_idx_type nf = qh num_facets;

      Matrix idx (nf, dim + 1);

      facetT *facet;

      octave_idx_type i = 0;

      FORALLfacets
        {
          octave_idx_type j = 0;

          if (! (nonsimp_seen || facet->simplicial || qh hull_dim == 2))
            {
              nonsimp_seen = true;

              if (cmd.find ("QJ") != std::string::npos)
                {
                  // Should never happen with QJ.
                  error ("convhulln: qhull failed: option 'QJ' returned non-simplicial facet");
                  return retval;
                }
            }

          if (dim == 3)
            {
              setT *vertices = qh_facet3vertex (facet);

              vertexT *vertex, **vertexp;

              FOREACHvertex_ (vertices)
                idx(i, j++) = 1 + qh_pointid(vertex->point);

              qh_settempfree (&vertices);
            }
          else
            {
              if (facet->toporient ^ qh_ORIENTclock)
                {
                  vertexT *vertex, **vertexp;

                  FOREACHvertex_ (facet->vertices)
                    idx(i, j++) = 1 + qh_pointid(vertex->point);
                }
              else
                {
                  vertexT *vertex, **vertexp;

                  FOREACHvertexreverse12_ (facet->vertices)
                    idx(i, j++) = 1 + qh_pointid(vertex->point);
                }
            }
          if (j < dim)
            warning ("convhulln: facet %d only has %d vertices", i, j);

          i++;
        }

      // Remove extra dimension if all facets were simplicial.

      if (! nonsimp_seen)
        idx.resize (nf, dim, 0.0);

      if (nargout == 2)
        {
          // Calculate volume of convex hull, taken from qhull src/geom2.c.

          realT area;
          realT dist;

          FORALLfacets
            {
              if (! facet->normal)
                continue;

              if (facet->upperdelaunay && qh ATinfinity)
                continue;

              facet->f.area = area = qh_facetarea (facet);
              facet->isarea = True;

              if (qh DELAUNAY)
                {
                  if (facet->upperdelaunay == qh UPPERdelaunay)
                    qh totarea += area;
                }
              else
                {
                  qh totarea += area;
                  qh_distplane (qh interior_point, facet, &dist);
                  qh totvol += -dist * area/ qh hull_dim;
                }
            }

          retval(1) = octave_value (qh totvol);
        }

      retval(0) = idx;
    }
  else
    error ("convhulln: qhull failed");

  // Free memory from Qhull
  qh_freeqhull (! qh_ALL);

  int curlong, totlong;
  qh_memfreeshort (&curlong, &totlong);

  if (curlong || totlong)
    warning ("convhulln: did not free %d bytes of long memory (%d pieces)",
             totlong, curlong);

#else
  error ("convhulln: not available in this version of Octave");
#endif

  return retval;
}

/*
%!testif HAVE_QHULL
%! cube = [0 0 0;1 0 0;1 1 0;0 1 0;0 0 1;1 0 1;1 1 1;0 1 1];
%! [h, v] = convhulln (cube, "Qt");
%! assert (size (h), [12 3]);
%! h = sortrows (sort (h, 2), [1:3]);
%! assert (h, [1 2 4; 1 2 6; 1 4 8; 1 5 6; 1 5 8; 2 3 4; 2 3 7; 2 6 7; 3 4 7; 4 7 8; 5 6 7; 5 7 8]);
%! assert (v, 1, 10*eps);
%! [h2, v2] = convhulln (cube); % Test defaut option = "Qt"
%! assert (size (h2), size (h));
%! h2 = sortrows (sort (h2, 2), [1:3]);
%! assert (h2, h);
%! assert (v2, v, 10*eps);

%!testif HAVE_QHULL
%! cube = [0 0 0;1 0 0;1 1 0;0 1 0;0 0 1;1 0 1;1 1 1;0 1 1];
%! [h, v] = convhulln (cube, "QJ");
%! assert (size (h), [12 3]);
%! assert (sortrows (sort (h, 2), [1:3]), [1 2 4; 1 2 5; 1 4 5; 2 3 4; 2 3 6; 2 5 6; 3 4 8; 3 6 7; 3 7 8; 4 5 8; 5 6 8; 6 7 8]);
%! assert (v, 1.0, 1e6*eps);

%!testif HAVE_QHULL
%! tetrahedron = [1 1 1;-1 -1 1;-1 1 -1;1 -1 -1];
%! [h, v] = convhulln (tetrahedron);
%! h = sortrows (sort (h, 2), [1 2 3]);
%! assert (h, [1 2 3;1 2 4; 1 3 4; 2 3 4]);
%! assert (v, 8/3, 10*eps);

%!testif HAVE_QHULL
%! triangle=[0 0; 1 1; 1 0; 1 2];
%! h = convhulln (triangle);
%! assert (size (h), [3 2]);
*/
