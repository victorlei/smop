## Copyright (C) 2008-2015 Ivana Varekova & Radek Salac
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} treelayout (@var{tree})
## @deftypefnx {Function File} {} treelayout (@var{tree}, @var{permutation})
## treelayout lays out a tree or a forest.
##
## The first argument @var{tree} is a vector of predecessors.
##
## The parameter @var{permutation} is an optional postorder permutation.
##
## The complexity of the algorithm is O(n) in terms of time and memory
## requirements.
## @seealso{etreeplot, gplot, treeplot}
## @end deftypefn

function [x_coordinate, y_coordinate, height, s] = treelayout (tree, permutation)
  if (nargin < 1 || nargin > 2 || nargout > 4)
    print_usage ();
  elseif (! isvector (tree) || rows (tree) != 1 || ! isnumeric (tree)
          || any (tree > length (tree)) || any (tree < 0))
    error ("treelayout: the first input argument must be a vector of predecessors");
  else
    ## Make it a row vector.
    tree = tree(:)';

    ## The count of nodes of the graph.
    num_nodes = length (tree);
    ## The number of children.
    num_children = zeros (1, num_nodes + 1);

    ## Checking vector of predecessors.
    for i = 1 : num_nodes
      if (tree(i) < i)
        ## This part of graph was checked before.
        continue;
      endif

      ## Try to find cicle in this part of graph using modified Floyd's
      ## cycle-finding algorithm.
      tortoise = tree(i);
      hare = tree(tortoise);

      while (tortoise != hare)
        ## End after finding a cicle or reaching a checked part of graph.

        if (hare < i)
          ## This part of graph was checked before.
          break
        endif

        tortoise = tree(tortoise);
        ## Hare will move faster than tortoise so in cicle hare must
        ## reach tortoise.
        hare = tree(tree(hare));

      endwhile

      if (tortoise == hare)
        ## If hare reach tortoise we found circle.
        error ("treelayout: vector of predecessors has bad format");
      endif

    endfor
    ## Vector of predecessors has right format.

    for i = 1:num_nodes
      ## vec_of_child is helping vector which is used to speed up the
      ## choice of descendant nodes.

      num_children(tree(i)+1) = num_children(tree(i)+1) + 1;
    endfor

    pos = 1;
    start = zeros (1, num_nodes+1);
    xhelp = zeros (1, num_nodes+1);
    stop = zeros (1, num_nodes+1);
    for i = 1 : num_nodes + 1
      start(i) = pos;
      xhelp(i) = pos;
      pos += num_children(i);
      stop(i) = pos;
    endfor

    if (nargin == 1)
      for i = 1:num_nodes
        vec_of_child(xhelp(tree(i)+1)) = i;
        xhelp(tree(i)+1) = xhelp(tree(i)+1) + 1;
      endfor
    else
      vec_of_child = permutation;
    endif

    ## The number of "parent" (actual) node (it's descendants will be
    ## browse in the next iteration).
    par_number = 0;

    ## The x-coordinate of the left most descendant of "parent node"
    ## this value is increased in each leaf.
    left_most = 0;

    ## The level of "parent" node (root level is num_nodes).
    level = num_nodes;

    ## num_nodes - max_ht is the height of this graph.
    max_ht = num_nodes;

    ## Main stack - each item consists of two numbers - the number of
    ## node and the number it's of parent node on the top of stack
    ## there is "parent node".
    stk = [-1, 0];

    ## Number of vertices s in the top-level separator.
    s = 0;
    ## Flag which says if we are in top level separator.
    top_level = 1;
    ## The top of the stack.
    while (par_number != -1)
      if (start(par_number+1) < stop(par_number+1))
        idx = vec_of_child(start(par_number+1) : stop(par_number+1) - 1);
      else
        idx = zeros (1, 0);
      endif

      ## Add to idx the vector of parent descendants.
      stk = [stk; [idx', ones(fliplr(size(idx))) * par_number]];

      ## We are in top level separator when we have one child and the
      ## flag is 1
      if (columns (idx) == 1 && top_level == 1)
        s++;
      else
        ## We aren't in top level separator now.
        top_level = 0;
      endif
      ## If there is not any descendant of "parent node":
      if (stk(end,2) != par_number)
       left_most++;
       x_coordinate_r(par_number) = left_most;
       max_ht = min (max_ht, level);
       if (length (stk) > 1 && find ((shift (stk,1) - stk) == 0) > 1
           && stk(end,2) != stk(end-1,2))
          ## Return to the nearest branching the position to return
          ## position is the position on the stack, where should be
          ## started further search (there are two nodes which has the
          ## same parent node).

          position = (find ((shift (stk(:,2), 1) - stk(:,2)) == 0))(end) + 1;
          par_number_vec = stk(position:end,2);

          ## The vector of removed nodes (the content of stack form
          ## position to end).

          level += length (par_number_vec);

          ## The level have to be decreased.

          x_coordinate_r(par_number_vec) = left_most;
          stk(position:end,:) = [];
        endif

        ## Remove the next node from "searched branch".

        stk(end,:) = [];
        ## Choose new "parent node".
        par_number = stk(end,1);
        ## If there is another branch start to search it.
        if (par_number != -1)
          y_coordinate(par_number) = level;
          x_coordinate_l(par_number) = left_most + 1;
        endif
      else

        ## There were descendants of "parent nod" choose the last of
        ## them and go on through it.
        level--;
        par_number = stk(end,1);
        y_coordinate(par_number) = level;
        x_coordinate_l(par_number) = left_most + 1;
      endif
    endwhile

    ## Calculate the x coordinates (the known values are the position
    ## of most left and most right descendants).
    x_coordinate = (x_coordinate_l + x_coordinate_r) / 2;

    height = num_nodes - max_ht - 1;
  endif
endfunction


%!test
%! % Compute a simple tree layout
%! [x, y, h, s] = treelayout ([0, 1, 2, 2]);
%! assert (x, [1.5, 1.5, 2, 1]);
%! assert (y, [3, 2, 1, 1]);
%! assert (h, 2);
%! assert (s, 2);

%!test
%! % Compute a simple tree layout with defined postorder permutation
%! [x, y, h, s] = treelayout ([0, 1, 2, 2], [1, 2, 4, 3]);
%! assert (x, [1.5, 1.5, 1, 2]);
%! assert (y, [3, 2, 1, 1]);
%! assert (h, 2);
%! assert (s, 2);

%!test
%! % Compute a simple tree layout with defined postorder permutation
%! [x, y, h, s] = treelayout ([0, 1, 2, 2], [4, 2, 3, 1]);
%! assert (x, [0, 0, 0, 1]);
%! assert (y, [0, 0, 0, 3]);
%! assert (h, 0);
%! assert (s, 1);

