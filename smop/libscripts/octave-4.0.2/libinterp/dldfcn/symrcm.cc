/*

Copyright (C) 2007-2015 Michael Weitzel

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
An implementation of the Reverse Cuthill-McKee algorithm (symrcm)

The implementation of this algorithm is based in the descriptions found in

@INPROCEEDINGS{,
        author = {E. Cuthill and J. McKee},
        title = {Reducing the Bandwidth of Sparse Symmetric Matrices},
        booktitle = {Proceedings of the 24th ACM National Conference},
        publisher = {Brandon Press},
        pages = {157 -- 172},
        location = {New Jersey},
        year = {1969}
}

@BOOK{,
        author = {Alan George and Joseph W. H. Liu},
        title = {Computer Solution of Large Sparse Positive Definite Systems},
        publisher = {Prentice Hall Series in Computational Mathematics},
        ISBN = {0-13-165274-5},
        year = {1981}
}

The algorithm represents a heuristic approach to the NP-complete minimum
bandwidth problem.

Written by Michael Weitzel <michael.weitzel@@uni-siegen.de>
                           <weitzel@@ldknet.org>
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "ov.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "utils.h"
#include "oct-locbuf.h"

#include "ov-re-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "oct-sparse.h"

// A node struct for the Cuthill-McKee algorithm
struct CMK_Node
{
  // the node's id (matrix row index)
  octave_idx_type id;
  // the node's degree
  octave_idx_type deg;
  // minimal distance to the root of the spanning tree
  octave_idx_type dist;
};

// A simple queue.
// Queues Q have a fixed maximum size N (rows,cols of the matrix) and are
// stored in an array. qh and qt point to queue head and tail.

// Enqueue operation (adds a node "o" at the tail)

inline static void
Q_enq (CMK_Node *Q, octave_idx_type N, octave_idx_type& qt, const CMK_Node& o)
{
  Q[qt] = o;
  qt = (qt + 1) % (N + 1);
}

// Dequeue operation (removes a node from the head)

inline static CMK_Node
Q_deq (CMK_Node * Q, octave_idx_type N, octave_idx_type& qh)
{
  CMK_Node r = Q[qh];
  qh = (qh + 1) % (N + 1);
  return r;
}

// Predicate (queue empty)
#define Q_empty(Q, N, qh, qt)   ((qh) == (qt))

// A simple, array-based binary heap (used as a priority queue for nodes)

// the left descendant of entry i
#define LEFT(i)         (((i) << 1) + 1)        // = (2*(i)+1)
// the right descendant of entry i
#define RIGHT(i)        (((i) << 1) + 2)        // = (2*(i)+2)
// the parent of entry i
#define PARENT(i)       (((i) - 1) >> 1)        // = floor(((i)-1)/2)

// Builds a min-heap (the root contains the smallest element). A is an array
// with the graph's nodes, i is a starting position, size is the length of A.

static void
H_heapify_min (CMK_Node *A, octave_idx_type i, octave_idx_type size)
{
  octave_idx_type j = i;
  for (;;)
    {
      octave_idx_type l = LEFT(j);
      octave_idx_type r = RIGHT(j);

      octave_idx_type smallest;
      if (l < size && A[l].deg < A[j].deg)
        smallest = l;
      else
        smallest = j;

      if (r < size && A[r].deg < A[smallest].deg)
        smallest = r;

      if (smallest != j)
        {
          std::swap (A[j], A[smallest]);
          j = smallest;
        }
      else
        break;
    }
}

// Heap operation insert. Running time is O(log(n))

static void
H_insert (CMK_Node *H, octave_idx_type& h, const CMK_Node& o)
{
  octave_idx_type i = h++;

  H[i] = o;

  if (i == 0)
    return;
  do
    {
      octave_idx_type p = PARENT(i);
      if (H[i].deg < H[p].deg)
        {
          std::swap (H[i], H[p]);

          i = p;
        }
      else
        break;
    }
  while (i > 0);
}

// Heap operation remove-min. Removes the smalles element in O(1) and
// reorganizes the heap optionally in O(log(n))

inline static CMK_Node
H_remove_min (CMK_Node *H, octave_idx_type& h, int reorg/*=1*/)
{
  CMK_Node r = H[0];
  H[0] = H[--h];
  if (reorg)
    H_heapify_min (H, 0, h);
  return r;
}

// Predicate (heap empty)
#define H_empty(H, h)   ((h) == 0)

// Helper function for the Cuthill-McKee algorithm. Tries to determine a
// pseudo-peripheral node of the graph as starting node.

static octave_idx_type
find_starting_node (octave_idx_type N, const octave_idx_type *ridx,
                    const octave_idx_type *cidx, const octave_idx_type *ridx2,
                    const octave_idx_type *cidx2, octave_idx_type *D,
                    octave_idx_type start)
{
  CMK_Node w;

  OCTAVE_LOCAL_BUFFER (CMK_Node, Q, N+1);
  boolNDArray btmp (dim_vector (1, N), false);
  bool *visit = btmp.fortran_vec ();

  octave_idx_type qh = 0;
  octave_idx_type qt = 0;
  CMK_Node x;
  x.id = start;
  x.deg = D[start];
  x.dist = 0;
  Q_enq (Q, N, qt, x);
  visit[start] = true;

  // distance level
  octave_idx_type level = 0;
  // current largest "eccentricity"
  octave_idx_type max_dist = 0;

  for (;;)
    {
      while (! Q_empty (Q, N, qh, qt))
        {
          CMK_Node v = Q_deq (Q, N, qh);

          if (v.dist > x.dist || (v.id != x.id && v.deg > x.deg))
            x = v;

          octave_idx_type i = v.id;

          // add all unvisited neighbors to the queue
          octave_idx_type j1 = cidx[i];
          octave_idx_type j2 = cidx2[i];
          while (j1 < cidx[i+1] || j2 < cidx2[i+1])
            {
              OCTAVE_QUIT;

              if (j1 == cidx[i+1])
                {
                  octave_idx_type r2 = ridx2[j2++];
                  if (! visit[r2])
                    {
                      // the distance of node j is dist(i)+1
                      w.id = r2;
                      w.deg = D[r2];
                      w.dist = v.dist+1;
                      Q_enq (Q, N, qt, w);
                      visit[r2] = true;

                      if (w.dist > level)
                        level = w.dist;
                    }
                }
              else if (j2 == cidx2[i+1])
                {
                  octave_idx_type r1 = ridx[j1++];
                  if (! visit[r1])
                    {
                      // the distance of node j is dist(i)+1
                      w.id = r1;
                      w.deg = D[r1];
                      w.dist = v.dist+1;
                      Q_enq (Q, N, qt, w);
                      visit[r1] = true;

                      if (w.dist > level)
                        level = w.dist;
                    }
                }
              else
                {
                  octave_idx_type r1 = ridx[j1];
                  octave_idx_type r2 = ridx2[j2];
                  if (r1 <= r2)
                    {
                      if (! visit[r1])
                        {
                          w.id = r1;
                          w.deg = D[r1];
                          w.dist = v.dist+1;
                          Q_enq (Q, N, qt, w);
                          visit[r1] = true;

                          if (w.dist > level)
                            level = w.dist;
                        }
                      j1++;
                      if (r1 == r2)
                        j2++;
                    }
                  else
                    {
                      if (! visit[r2])
                        {
                          w.id = r2;
                          w.deg = D[r2];
                          w.dist = v.dist+1;
                          Q_enq (Q, N, qt, w);
                          visit[r2] = true;

                          if (w.dist > level)
                            level = w.dist;
                        }
                      j2++;
                    }
                }
            }
        } // finish of BFS

      if (max_dist < x.dist)
        {
          max_dist = x.dist;

          for (octave_idx_type i = 0; i < N; i++)
            visit[i] = false;

          visit[x.id] = true;
          x.dist = 0;
          qt = qh = 0;
          Q_enq (Q, N, qt, x);
        }
      else
        break;
    }
  return x.id;
}

// Calculates the node's degrees. This means counting the nonzero elements
// in the symmetric matrix' rows. This works for non-symmetric matrices
// as well.

static octave_idx_type
calc_degrees (octave_idx_type N, const octave_idx_type *ridx,
              const octave_idx_type *cidx, octave_idx_type *D)
{
  octave_idx_type max_deg = 0;

  for (octave_idx_type i = 0; i < N; i++)
    D[i] = 0;

  for (octave_idx_type j = 0; j < N; j++)
    {
      for (octave_idx_type i = cidx[j]; i < cidx[j+1]; i++)
        {
          OCTAVE_QUIT;
          octave_idx_type k = ridx[i];
          // there is a nonzero element (k,j)
          D[k]++;
          if (D[k] > max_deg)
            max_deg = D[k];
          // if there is no element (j,k) there is one in
          // the symmetric matrix:
          if (k != j)
            {
              bool found = false;
              for (octave_idx_type l = cidx[k]; l < cidx[k + 1]; l++)
                {
                  OCTAVE_QUIT;

                  if (ridx[l] == j)
                    {
                      found = true;
                      break;
                    }
                  else if (ridx[l] > j)
                    break;
                }

              if (! found)
                {
                  // A(j,k) == 0
                  D[j]++;
                  if (D[j] > max_deg)
                    max_deg = D[j];
                }
            }
        }
    }
  return max_deg;
}

// Transpose of the structure of a square sparse matrix

static void
transpose (octave_idx_type N, const octave_idx_type *ridx,
           const octave_idx_type *cidx, octave_idx_type *ridx2,
           octave_idx_type *cidx2)
{
  octave_idx_type nz = cidx[N];

  OCTAVE_LOCAL_BUFFER (octave_idx_type, w, N + 1);
  for (octave_idx_type i = 0; i < N; i++)
    w[i] = 0;
  for (octave_idx_type i = 0; i < nz; i++)
    w[ridx[i]]++;
  nz = 0;
  for (octave_idx_type i = 0; i < N; i++)
    {
      OCTAVE_QUIT;
      cidx2[i] = nz;
      nz += w[i];
      w[i] = cidx2[i];
    }
  cidx2[N] = nz;
  w[N] = nz;

  for (octave_idx_type j = 0; j < N; j++)
    for (octave_idx_type k = cidx[j]; k < cidx[j + 1]; k++)
      {
        OCTAVE_QUIT;
        octave_idx_type q = w[ridx[k]]++;
        ridx2[q] = j;
      }
}

// An implementation of the Cuthill-McKee algorithm.
DEFUN_DLD (symrcm, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} symrcm (@var{S})\n\
Return the symmetric reverse @nospell{Cuthill-McKee} permutation of @var{S}.\n\
\n\
@var{p} is a permutation vector such that\n\
@code{@var{S}(@var{p}, @var{p})} tends to have its diagonal elements closer\n\
to the diagonal than @var{S}.  This is a good preordering for LU or\n\
Cholesky@tie{}factorization of matrices that come from ``long, skinny''\n\
problems.  It works for both symmetric and asymmetric @var{S}.\n\
\n\
The algorithm represents a heuristic approach to the NP-complete bandwidth\n\
minimization problem.  The implementation is based in the descriptions found\n\
in\n\
\n\
@nospell{E. Cuthill, J. McKee}. @cite{Reducing the Bandwidth of Sparse\n\
Symmetric Matrices}. Proceedings of the 24th ACM National Conference,\n\
157--172 1969, Brandon Press, New Jersey.\n\
\n\
@nospell{A. George, J.W.H. Liu}. @cite{Computer Solution of Large Sparse\n\
Positive Definite Systems}, Prentice Hall Series in Computational\n\
Mathematics, ISBN 0-13-165274-5, 1981.\n\
\n\
@seealso{colperm, colamd, symamd}\n\
@end deftypefn")
{
  octave_value retval;
  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  // the parameter of the matrix is converted into a sparse matrix
  //(if necessary)
  octave_idx_type *cidx;
  octave_idx_type *ridx;
  SparseMatrix Ar;
  SparseComplexMatrix Ac;

  if (arg.is_real_type ())
    {
      Ar = arg.sparse_matrix_value ();
      // Note cidx/ridx are const, so use xridx and xcidx...
      cidx = Ar.xcidx ();
      ridx = Ar.xridx ();
    }
  else
    {
      Ac = arg.sparse_complex_matrix_value ();
      cidx = Ac.xcidx ();
      ridx = Ac.xridx ();
    }

  if (error_state)
    return retval;

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (nr != nc)
    {
      gripe_square_matrix_required ("symrcm");
      return retval;
    }

  if (nr == 0 && nc == 0)
    return octave_value (NDArray (dim_vector (1, 0)));

  // sizes of the heaps
  octave_idx_type s = 0;

  // head- and tail-indices for the queue
  octave_idx_type qt = 0;
  octave_idx_type qh = 0;
  CMK_Node v, w;
  // dimension of the matrix
  octave_idx_type N = nr;

  OCTAVE_LOCAL_BUFFER (octave_idx_type, cidx2, N + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, ridx2, cidx[N]);
  transpose (N, ridx, cidx, ridx2, cidx2);

  // the permutation vector
  NDArray P (dim_vector (1, N));

  // compute the node degrees
  OCTAVE_LOCAL_BUFFER (octave_idx_type, D, N);
  octave_idx_type max_deg = calc_degrees (N, ridx, cidx, D);

  // if none of the nodes has a degree > 0 (a matrix of zeros)
  // the return value corresponds to the identity permutation
  if (max_deg == 0)
    {
      for (octave_idx_type i = 0; i < N; i++)
        P(i) = i;
      return octave_value (P);
    }

  // a heap for the a node's neighbors. The number of neighbors is
  // limited by the maximum degree max_deg:
  OCTAVE_LOCAL_BUFFER (CMK_Node, S, max_deg);

  // a queue for the BFS. The array is always one element larger than
  // the number of entries that are stored.
  OCTAVE_LOCAL_BUFFER (CMK_Node, Q, N+1);

  // a counter (for building the permutation)
  octave_idx_type c = -1;

  // upper bound for the bandwidth (=quality of solution)
  // initialize the bandwidth of the graph with 0. B contains the
  // the maximum of the theoretical lower limits of the subgraphs
  // bandwidths.
  octave_idx_type B = 0;

  // mark all nodes as unvisited; with the exception of the nodes
  // that have degree==0 and build a CC of the graph.

  boolNDArray btmp (dim_vector (1, N), false);
  bool *visit = btmp.fortran_vec ();

  do
    {
      // locate an unvisited starting node of the graph
      octave_idx_type i;
      for (i = 0; i < N; i++)
        if (! visit[i])
          break;

      // locate a probably better starting node
      v.id = find_starting_node (N, ridx, cidx, ridx2, cidx2, D, i);

      // mark the node as visited and enqueue it (a starting node
      // for the BFS). Since the node will be a root of a spanning
      // tree, its dist is 0.
      v.deg = D[v.id];
      v.dist = 0;
      visit[v.id] = true;
      Q_enq (Q, N, qt, v);

      // lower bound for the bandwidth of a subgraph
      // keep a "level" in the spanning tree (= min. distance to the
      // root) for determining the bandwidth of the computed
      // permutation P
      octave_idx_type Bsub = 0;
      // min. dist. to the root is 0
      octave_idx_type level = 0;
      // the root is the first/only node on level 0
      octave_idx_type level_N = 1;

      while (! Q_empty (Q, N, qh, qt))
        {
          v = Q_deq (Q, N, qh);
          i = v.id;

          c++;

          // for computing the inverse permutation P where
          // A(inv(P),inv(P)) or P'*A*P is banded
          //         P(i) = c;

          // for computing permutation P where
          // A(P(i),P(j)) or P*A*P' is banded
          P(c) = i;

          // put all unvisited neighbors j of node i on the heap
          s = 0;
          octave_idx_type j1 = cidx[i];
          octave_idx_type j2 = cidx2[i];

          OCTAVE_QUIT;
          while (j1 < cidx[i+1] || j2 < cidx2[i+1])
            {
              OCTAVE_QUIT;
              if (j1 == cidx[i+1])
                {
                  octave_idx_type r2 = ridx2[j2++];
                  if (! visit[r2])
                    {
                      // the distance of node j is dist(i)+1
                      w.id = r2;
                      w.deg = D[r2];
                      w.dist = v.dist+1;
                      H_insert (S, s, w);
                      visit[r2] = true;
                    }
                }
              else if (j2 == cidx2[i+1])
                {
                  octave_idx_type r1 = ridx[j1++];
                  if (! visit[r1])
                    {
                      w.id = r1;
                      w.deg = D[r1];
                      w.dist = v.dist+1;
                      H_insert (S, s, w);
                      visit[r1] = true;
                    }
                }
              else
                {
                  octave_idx_type r1 = ridx[j1];
                  octave_idx_type r2 = ridx2[j2];
                  if (r1 <= r2)
                    {
                      if (! visit[r1])
                        {
                          w.id = r1;
                          w.deg = D[r1];
                          w.dist = v.dist+1;
                          H_insert (S, s, w);
                          visit[r1] = true;
                        }
                      j1++;
                      if (r1 == r2)
                        j2++;
                    }
                  else
                    {
                      if (! visit[r2])
                        {
                          w.id = r2;
                          w.deg = D[r2];
                          w.dist = v.dist+1;
                          H_insert (S, s, w);
                          visit[r2] = true;
                        }
                      j2++;
                    }
                }
            }

          // add the neighbors to the queue (sorted by node degree)
          while (! H_empty (S, s))
            {
              OCTAVE_QUIT;

              // locate a neighbor of i with minimal degree in O(log(N))
              v = H_remove_min (S, s, 1);

              // entered the BFS a new level?
              if (v.dist > level)
                {
                  // adjustment of bandwith:
                  // "[...] the minimum bandwidth that
                  // can be obtained [...] is the
                  //  maximum number of nodes per level"
                  if (Bsub < level_N)
                    Bsub = level_N;

                  level = v.dist;
                  // v is the first node on the new level
                  level_N = 1;
                }
              else
                {
                  // there is no new level but another node on
                  // this level:
                  level_N++;
                }

              // enqueue v in O(1)
              Q_enq (Q, N, qt, v);
            }

          // synchronize the bandwidth with level_N once again:
          if (Bsub < level_N)
            Bsub = level_N;
        }
      // finish of BFS. If there are still unvisited nodes in the graph
      // then it is split into CCs. The computed bandwidth is the maximum
      // of all subgraphs. Update:
      if (Bsub > B)
        B = Bsub;
    }
  // are there any nodes left?
  while (c+1 < N);

  // compute the reverse-ordering
  s = N / 2 - 1;
  for (octave_idx_type i = 0, j = N - 1; i <= s; i++, j--)
    std::swap (P.elem (i), P.elem (j));

  // increment all indices, since Octave is not C
  return octave_value (P+1);
}
