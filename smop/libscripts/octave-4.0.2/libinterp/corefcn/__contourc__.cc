/* Contour lines for function evaluated on a grid.

Copyright (C) 2007-2015 Kai Habel
Copyright (C) 2004, 2007 Shai Ayal

Adapted to an oct file from the stand alone contourl by Victro Munoz
Copyright (C) 2004 Victor Munoz

Based on contour plot routine (plcont.c) in PLPlot package
http://plplot.org/

Copyright (C) 1995, 2000, 2001 Maurice LeBrun
Copyright (C) 2000, 2002 Joao Cardoso
Copyright (C) 2000, 2001, 2002, 2004  Alan W. Irwin
Copyright (C) 2004  Andrew Ross

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include "quit.h"

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

static Matrix this_contour;
static Matrix contourc;
static int elem;

// This is the quanta in which we increase this_contour.
#define CONTOUR_QUANT 50

// Add a coordinate point (x,y) to this_contour.

static void
add_point (double x, double y)
{
  if (elem % CONTOUR_QUANT == 0)
    this_contour = this_contour.append (Matrix (2, CONTOUR_QUANT, 0));

  this_contour (0, elem) = x;
  this_contour (1, elem) = y;
  elem++;
}

// Add contents of current contour to contourc.
// this_contour.cols () - 1;

static void
end_contour (void)
{
  if (elem > 2)
    {
      this_contour (1, 0) = elem - 1;
      contourc = contourc.append (this_contour.extract_n (0, 0, 2, elem));
    }

  this_contour = Matrix ();
  elem = 0;
}

// Start a new contour, and add contents of current one to contourc.

static void
start_contour (double lvl, double x, double y)
{
  end_contour ();
  this_contour.resize (2, 0);
  add_point (lvl, 0);
  add_point (x, y);
}

static void
drawcn (const RowVector& X, const RowVector& Y, const Matrix& Z,
        double lvl, int r, int c, double ct_x, double ct_y,
        unsigned int start_edge, bool first, charMatrix& mark)
{
  double px[4], py[4], pz[4], tmp;
  unsigned int stop_edge, pt[2];

  // Continue while next facet is not done yet.
  while (r >= 0 && c >= 0 && r < mark.rows () && c < mark.cols ()
         && mark(r, c) > 0)
    {

      //get x, y, and z - lvl for current facet
      px[0] = px[3] = X(c);
      px[1] = px[2] = X(c+1);

      py[0] = py[1] = Y(r);
      py[2] = py[3] = Y(r+1);

      pz[3] = Z(r+1, c) - lvl;
      pz[2] = Z(r+1, c + 1) - lvl;
      pz[1] = Z(r, c+1) - lvl;
      pz[0] = Z(r, c) - lvl;

      // Facet edge and point naming assignment.
      //
      //  0-----1   .-0-.
      //  |     |   |   |
      //  |     |   3   1
      //  |     |   |   |
      //  3-----2   .-2-.

      // Get mark value of current facet.
      char id = static_cast<char> (mark(r, c));

      // Check startedge s.
      if (start_edge == 255)
        {
          // Find start edge.
          for (unsigned int k = 0; k < 4; k++)
            if (static_cast<char> (1 << k) & id)
              start_edge = k;
        }

      if (start_edge == 255)
        break;

      // Decrease mark value of current facet for start edge.
      mark(r, c) -= static_cast<char> (1 << start_edge);

      // Next point (clockwise).
      pt[0] = start_edge;
      pt[1] = (pt[0] + 1) % 4;

      // Calculate contour segment start if first of contour.
      if (first)
        {
          tmp = fabs (pz[pt[1]]) / fabs (pz[pt[0]]);

          if (xisnan (tmp))
            ct_x = ct_y = 0.5;
          else
            {
              ct_x = px[pt[0]] + (px[pt[1]] - px[pt[0]])/(1 + tmp);
              ct_y = py[pt[0]] + (py[pt[1]] - py[pt[0]])/(1 + tmp);
            }

          start_contour (lvl, ct_x, ct_y);
          first = false;
        }

      // Find stop edge.
      // FIXME: perhaps this should use a while loop?
      for (unsigned int k = 1; k <= 4; k++)
        {
          if (start_edge == 0 || start_edge == 2)
            stop_edge = (start_edge + k) % 4;
          else
            stop_edge = (start_edge - k) % 4;

          if (static_cast<char> (1 << stop_edge) & id)
            break;
        }

      pt[0] = stop_edge;
      pt[1] = (pt[0] + 1) % 4;
      tmp = fabs (pz[pt[1]]) / fabs (pz[pt[0]]);

      if (xisnan (tmp))
        ct_x = ct_y = 0.5;
      else
        {
          ct_x = px[pt[0]] + (px[pt[1]] - px[pt[0]])/(1 + tmp);
          ct_y = py[pt[0]] + (py[pt[1]] - py[pt[0]])/(1 + tmp);
        }

      // Add point to contour.
      add_point (ct_x, ct_y);

      // Decrease id value of current facet for start edge.
      mark(r, c) -= static_cast<char> (1 << stop_edge);

      // Find next facet.
      if (stop_edge == 0)
        r--;
      else if (stop_edge == 1)
        c++;
      else if (stop_edge == 2)
        r++;
      else if (stop_edge == 3)
        c--;

      // Go to next facet.
      start_edge = (stop_edge + 2) % 4;

    }
}

static void
mark_facets (const Matrix& Z, charMatrix& mark, double lvl)
{
  unsigned int nr = mark.rows ();
  unsigned int nc = mark.cols ();

  double f[4];

  for (unsigned int c = 0; c < nc; c++)
    for (unsigned int r = 0; r < nr; r++)
      {
        f[0] = Z(r, c) - lvl;
        f[1] = Z(r, c+1) - lvl;
        f[3] = Z(r+1, c) - lvl;
        f[2] = Z(r+1, c+1) - lvl;

        for (unsigned int i = 0; i < 4; i++)
          if (fabs(f[i]) < std::numeric_limits<double>::epsilon ())
            f[i] = std::numeric_limits<double>::epsilon ();

        if (f[1] * f[2] < 0)
          mark(r, c) += 2;

        if (f[0] * f[3] < 0)
          mark(r, c) += 8;
      }

  for (unsigned int r = 0; r < nr; r++)
    for (unsigned int c = 0; c < nc; c++)
      {
        f[0] = Z(r, c) - lvl;
        f[1] = Z(r, c+1) - lvl;
        f[3] = Z(r+1, c) - lvl;
        f[2] = Z(r+1, c+1) - lvl;

        for (unsigned int i = 0; i < 4; i++)
          if (fabs(f[i]) < std::numeric_limits<double>::epsilon ())
            f[i] = std::numeric_limits<double>::epsilon ();

        if (f[0] * f[1] < 0)
          mark(r, c) += 1;

        if (f[2] * f[3] < 0)
          mark(r, c) += 4;
      }
}

static void
cntr (const RowVector& X, const RowVector& Y, const Matrix& Z, double lvl)
{
  unsigned int nr = Z.rows ();
  unsigned int nc = Z.cols ();

  charMatrix mark (nr - 1, nc - 1, 0);

  mark_facets (Z, mark, lvl);

  // Find contours that start at a domain edge.

  for (unsigned int c = 0; c < nc - 1; c++)
    {
      // Top.
      if (mark(0, c) & 1)
        drawcn (X, Y, Z, lvl, 0, c, 0.0, 0.0, 0, true, mark);

      // Bottom.
      if (mark(nr - 2, c) & 4)
        drawcn (X, Y, Z, lvl, nr - 2, c, 0.0, 0.0, 2, true, mark);
    }

  for (unsigned int r = 0; r < nr - 1; r++)
    {
      // Left.
      if (mark(r, 0) & 8)
        drawcn (X, Y, Z, lvl, r, 0, 0.0, 0.0, 3, true, mark);

      // Right.
      if (mark(r, nc - 2) & 2)
        drawcn (X, Y, Z, lvl, r, nc - 2, 0.0, 0.0, 1, true, mark);
    }

  for (unsigned int r = 0; r < nr - 1; r++)
    for (unsigned int c = 0; c < nc - 1; c++)
      if (mark (r, c) > 0)
        drawcn (X, Y, Z, lvl, r, c, 0.0, 0.0, 255, true, mark);
}

DEFUN (__contourc__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __contourc__ (@var{x}, @var{y}, @var{z}, @var{levels})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 4)
    {
      RowVector X = args(0).row_vector_value ();
      RowVector Y = args(1).row_vector_value ();
      Matrix Z = args(2).matrix_value ();
      RowVector L = args(3).row_vector_value ();

      if (! error_state)
        {
          contourc.resize (2, 0);

          for (int i = 0; i < L.length (); i++)
            cntr (X, Y, Z, L (i));

          end_contour ();

          retval = contourc;
        }
      else
        error ("__contourc__: invalid argument values");
    }
  else
    print_usage ();

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/
