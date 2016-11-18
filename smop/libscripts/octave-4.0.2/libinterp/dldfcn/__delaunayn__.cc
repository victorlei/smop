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
  16. July 2000 - Kai Habel: first release

  25. September 2002 - Changes by Rafael Laboissiere <rafael@laboissiere.net>

  * Added Qbb option to normalize the input and avoid crashes in Octave.
  * delaunayn accepts now a second (optional) argument that must be a string
  containing extra options to the qhull command.
  * Fixed doc string.  The dimension of the result matrix is [m, dim+1], and
  not [n, dim-1].

  6. June 2006: Changes by Alexander Barth <abarth@marine.usf.edu>

  * triangulate non-simplicial facets
  * allow options to be specified as cell array of strings
  * change the default options (for compatibility with matlab)
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>

#include "oct-locbuf.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "unwind-prot.h"

#if defined (HAVE_QHULL)
# include "oct-qhull.h"
# if defined (NEED_QHULL_VERSION)
char qh_version[] = "__delaunayn__.oct 2007-08-21";
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

DEFUN_DLD (__delaunayn__, args, ,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{T} =} __delaunayn__ (@var{pts})\n\
@deftypefnx {Loadable Function} {@var{T} =} __delaunayn__ (@var{pts}, @var{options})\n\
Undocumented internal function.\n\
@end deftypefn")

{
  octave_value_list retval;

#if defined (HAVE_QHULL)

  retval(0) = 0.0;

  int nargin = args.length ();
  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  Matrix p (args(0).matrix_value ());
  const octave_idx_type dim = p.columns ();
  const octave_idx_type n = p.rows ();

  if (! octave_qhull_dims_ok (dim, n, "__delaynayn__"))
    return retval;

  // Default options
  std::string options;
  if (dim <= 3)
    options = "Qt Qbb Qc Qz";
  else
    options = "Qt Qbb Qc Qx";

  if (nargin == 2)
    {
      if (args(1).is_string ())
        options = args(1).string_value ();
      else if (args(1).is_empty ())
        ;  // Use default options
      else if (args(1).is_cellstr ())
        {
          options = "";
          Array<std::string> tmp = args(1).cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += tmp(i) + " ";
        }
      else
        {
          error ("__delaunayn__: OPTIONS argument must be a string, cell array of strings, or empty");
          return retval;
        }
    }

  if (n > dim + 1)
    {
      p = p.transpose ();
      double *pt_array = p.fortran_vec ();
      boolT ismalloc = false;

      // Qhull flags argument is not const char*
      OCTAVE_LOCAL_BUFFER (char, flags, 9 + options.length ());

      sprintf (flags, "qhull d %s", options.c_str ());

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
          error ("__delaunayn__: unable to create temporary file for output");
          return retval;
        }

      int exitcode = qh_new_qhull (dim, n, pt_array,
                                   ismalloc, flags, outfile, errfile);
      if (! exitcode)
        {
          // triangulate non-simplicial facets
          qh_triangulate ();

          facetT *facet;
          vertexT *vertex, **vertexp;
          octave_idx_type nf = 0;
          octave_idx_type i = 0;

          FORALLfacets
            {
              if (! facet->upperdelaunay)
                nf++;

              // Double check.  Non-simplicial facets will cause segfault below
              if (! facet->simplicial)
                {
                  error ("__delaunayn__: Qhull returned non-simplicial facets -- try delaunayn with different options");
                  exitcode = 1;
                  break;
                }
            }

          if (! exitcode)
            {
              Matrix simpl (nf, dim+1);

              FORALLfacets
                {
                  if (! facet->upperdelaunay)
                    {
                      octave_idx_type j = 0;

                      FOREACHvertex_ (facet->vertices)
                        {
                          simpl(i, j++) = 1 + qh_pointid(vertex->point);
                        }
                      i++;
                    }
                }

              retval(0) = simpl;
            }
        }
      else
        error ("__delaunayn__: qhull failed");

      // Free memory from Qhull
      qh_freeqhull (! qh_ALL);

      int curlong, totlong;
      qh_memfreeshort (&curlong, &totlong);

      if (curlong || totlong)
        warning ("__delaunay__: did not free %d bytes of long memory (%d pieces)",
                 totlong, curlong);
    }
  else if (n == dim + 1)
    {
      // one should check if nx points span a simplex
      // I will look at this later.
      RowVector vec (n);
      for (octave_idx_type i = 0; i < n; i++)
        vec(i) = i + 1.0;

      retval(0) = vec;
    }

#else
  error ("__delaunayn__: not available in this version of Octave");
#endif

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
