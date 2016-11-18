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
20. Augiust 2000 - Kai Habel: first release
*/

/*
2003-12-14 Rafael Laboissiere <rafael@laboissiere.net>
Added optional second argument to pass options to the underlying
qhull command
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>

#include <list>

#include "oct-locbuf.h"
#include "lo-ieee.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "unwind-prot.h"

#if defined (HAVE_QHULL)
# include "oct-qhull.h"
# if defined (NEED_QHULL_VERSION)
char qh_version[] = "__voronoi__.oct 2007-07-24";
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

DEFUN_DLD (__voronoi__, args, ,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{C}, @var{F} =} __voronoi__ (@var{caller}, @var{pts})\n\
@deftypefnx {Loadable Function} {@var{C}, @var{F} =} __voronoi__ (@var{caller}, @var{pts}, @var{options})\n\
@deftypefnx {Loadable Function} {@var{C}, @var{F}, @var{Inf_Pts} =} __voronoi__ (@dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  std::string caller = args(0).string_value ();

#if defined (HAVE_QHULL)

  retval(0) = 0.0;

  int nargin = args.length ();
  if (nargin < 2 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  Matrix points = args(1).matrix_value ();
  const octave_idx_type dim = points.columns ();
  const octave_idx_type num_points = points.rows ();

  if (! octave_qhull_dims_ok (dim, num_points, "__voronoi__"))
    return retval;

  points = points.transpose ();

  std::string options;

  if (dim <= 3)
    options = " Qbb";
  else
    options = " Qbb Qx";

  if (nargin == 3)
    {
      octave_value opt_arg = args(2);

      if (opt_arg.is_string ())
        options = " " + opt_arg.string_value ();
      else if (opt_arg.is_empty ())
        ; // Use default options.
      else if (opt_arg.is_cellstr ())
        {
          options = "";

          Array<std::string> tmp = opt_arg.cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += " " + tmp(i);
        }
      else
        {
          error ("%s: OPTIONS must be a string, cell array of strings, or empty",
                 caller.c_str ());
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
      error ("__voronoi__: unable to create temporary file for output");
      return retval;
    }

  // qh_new_qhull command and points arguments are not const...

  std::string cmd = "qhull v" + options;

  OCTAVE_LOCAL_BUFFER (char, cmd_str, cmd.length () + 1);

  strcpy (cmd_str, cmd.c_str ());

  int exitcode = qh_new_qhull (dim, num_points, points.fortran_vec (),
                               ismalloc, cmd_str, outfile, errfile);
  if (! exitcode)
    {
      // Calling findgood_all provides the number of Voronoi vertices
      // (sets qh num_good).

      qh_findgood_all (qh facet_list);

      octave_idx_type num_voronoi_regions
        = qh num_vertices - qh_setsize (qh del_vertices);

      octave_idx_type num_voronoi_vertices = qh num_good;

      // Find the voronoi centers for all facets.

      qh_setvoronoi_all ();

      facetT *facet;
      vertexT *vertex;
      octave_idx_type k;

      // Find the number of Voronoi vertices for each Voronoi cell and
      // store them in NI so we can use them later to set the dimensions
      // of the RowVector objects used to collect them.

      FORALLfacets
        {
          facet->seen = false;
        }

      OCTAVE_LOCAL_BUFFER (octave_idx_type, ni, num_voronoi_regions);
      for (octave_idx_type i = 0; i < num_voronoi_regions; i++)
        ni[i] = 0;

      k = 0;

      FORALLvertices
        {
          if (qh hull_dim == 3)
            qh_order_vertexneighbors (vertex);

          bool infinity_seen = false;

          facetT *neighbor, **neighborp;

          FOREACHneighbor_ (vertex)
            {
              if (neighbor->upperdelaunay)
                {
                  if (! infinity_seen)
                    {
                      infinity_seen = true;
                      ni[k]++;
                    }
                }
              else
                {
                  neighbor->seen = true;
                  ni[k]++;
                }
            }

          k++;
        }

      // If Qhull finds fewer regions than points, we will pad the end
      // of the at_inf and C arrays so that they always contain at least
      // as many elements as the given points array.

      // FIXME: is it possible (or does it make sense) for
      // num_voronoi_regions to ever be larger than num_points?

      octave_idx_type nr = (num_points > num_voronoi_regions
                            ? num_points : num_voronoi_regions);

      boolMatrix at_inf (nr, 1, false);

      // The list of Voronoi vertices.  The first element is always
      // Inf.
      Matrix F (num_voronoi_vertices+1, dim);

      for (octave_idx_type d = 0; d < dim; d++)
        F(0,d) = octave_Inf;

      // The cell array of vectors of indices into F that represent the
      // vertices of the Voronoi regions (cells).

      Cell C (nr, 1);

      // Now loop through the list of vertices again and store the
      // coordinates of the Voronoi vertices and the lists of indices
      // for the cells.

      FORALLfacets
        {
          facet->seen = false;
        }

      octave_idx_type i = 0;
      k = 0;

      FORALLvertices
        {
          if (qh hull_dim == 3)
            qh_order_vertexneighbors (vertex);

          bool infinity_seen = false;

          octave_idx_type idx = qh_pointid (vertex->point);

          octave_idx_type num_vertices = ni[k++];

          // Qhull seems to sometimes produces regions with a single
          // vertex.  Is that a bug?  How can a region have just one
          // vertex?  Let's skip it.

          if (num_vertices == 1)
            continue;

          RowVector facet_list (num_vertices);

          octave_idx_type m = 0;

          facetT *neighbor, **neighborp;

          FOREACHneighbor_(vertex)
            {
              if (neighbor->upperdelaunay)
                {
                  if (! infinity_seen)
                    {
                      infinity_seen = true;
                      facet_list(m++) = 1;
                      at_inf(idx) = true;
                    }
                }
              else
                {
                  if (! neighbor->seen)
                    {
                      i++;
                      for (octave_idx_type d = 0; d < dim; d++)
                        F(i,d) = neighbor->center[d];

                      neighbor->seen = true;
                      neighbor->visitid = i;
                    }

                  facet_list(m++) = neighbor->visitid + 1;
                }
            }

          C(idx) = facet_list;
        }

      retval(2) = at_inf;
      retval(1) = C;
      retval(0) = F;
    }
  else
    error ("%s: qhull failed", caller.c_str ());

  // Free memory from Qhull
  qh_freeqhull (! qh_ALL);

  int curlong, totlong;
  qh_memfreeshort (&curlong, &totlong);

  if (curlong || totlong)
    warning ("%s: qhull did not free %d bytes of long memory (%d pieces)",
             caller.c_str (), totlong, curlong);

#else
  error ("%s: not available in this version of Octave", caller.c_str ());
#endif

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
