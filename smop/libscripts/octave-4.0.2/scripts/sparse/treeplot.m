## Copyright (C) 2005-2015 Ivana Varekova
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
## @deftypefn  {Function File} {} treeplot (@var{tree})
## @deftypefnx {Function File} {} treeplot (@var{tree}, @var{node_style}, @var{edge_style})
## Produce a graph of tree or forest.
##
## The first argument is vector of predecessors.
##
## The optional parameters @var{node_style} and @var{edge_style} define the
## output plot style.
##
## The complexity of the algorithm is O(n) in terms of is time and memory
## requirements.
## @seealso{etreeplot, gplot}
## @end deftypefn

function treeplot (tree, node_style = "ko", edge_style = "r")

  if (nargin < 1 || nargin > 3 || nargout > 0)
    print_usage ();
  endif

  if (! isnumeric (tree) || ! isrow (tree) || any (tree > length (tree)))
    error ("treeplot: TREE must be a vector of predecessors");
  endif

  ##  Verify node_style
  if (nargin > 1)
    if (isempty (regexp (node_style, '[ox+*]', 'once')))
      node_style = [node_style, "o"];
    endif
  endif

  ## Make it a row vector.
  tree = tree(:)';

  ## The count of nodes of the graph.
  num_nodes = length (tree);

  ## The number of children.
  num_children = zeros (1, num_nodes+1);

  for i = 1:num_nodes
    ## VEC_OF_CHILD is helping vector which is used to speed up the
    ## choose of descendant nodes.

    num_children(tree(i)+1) = num_children(tree(i)+1) + 1;
  endfor
  pos = 1;
  start = zeros (1, num_nodes+1);
  xhelp = zeros (1, num_nodes+1);
  stop = zeros (1, num_nodes+1);
  for i = 1:num_nodes+1
    start(i) = pos;
    xhelp(i) = pos;
    pos += num_children(i);
    stop(i) = pos;
  endfor
  for i = 1:num_nodes
    vec_of_child(xhelp(tree(i)+1)) = i;
    xhelp(tree(i)+1) = xhelp(tree(i)+1)+1;
  endfor

  ## The number of "parent" (actual) node (it's descendants will be
  ## browse in the next iteration).
  par_number = 0;

  ## The x-coordinate of the left most descendant of "parent node"
  ## this value is increased in each leaf.
  left_most = 0;

  ## The level of "parent" node (root level is num_nodes).
  level = num_nodes;

  ## Num_nodes - max_ht is the height of this graph.
  max_ht = num_nodes;

  ## Main stack - each item consists of two numbers - the number of
  ## node and the number it's of parent node on the top of stack
  ## there is "parent node".
  stk = [-1, 0];

  ## Stack which is use to draw the graph edge (it have to be
  ## uninterupted line).
  skelet = 0;

  ## The top of the stack.
  while (par_number != -1)
    if (start(par_number+1) < stop(par_number+1))
      idx = vec_of_child(start(par_number+1):stop(par_number+1)-1);
    else
      idx = zeros (1, 0);
    endif
    ## Add to idx the vector of parent descendants.
    stk = [stk; [idx', ones(fliplr(size(idx)))*par_number]];
    ## Add to stack the records relevant to parent descandant s.
    if (par_number != 0)
      skelet = [skelet; ([ones(size(idx))*par_number; idx])(:)];
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
        position = (find ((shift (stk(:,2),1) - stk(:,2)) == 0))(end) + 1;
        par_number_vec = stk(position:end,2);
        ## The vector of removed nodes (the content of stack form
        ## position to end).
        skelet = [skelet; flipud(par_number_vec)];
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
        skelet = [skelet; stk(end,2); par_number];
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

  ## FIXME: We should probably stuff all the arguments into a cell
  ##        array and make a single call to plot here so we can avoid
  ##        setting the hold state...

  hold_is_on = ishold ();
  unwind_protect
    ## Plot graph nodes.
    plot (x_coordinate, y_coordinate, node_style);

    ## Helping command - usable for plotting edges
    skelet = [skelet; 0];

    ## Draw graph edges.
    idx = find (skelet == 0);

    hold ("on");
    ## Plot each tree component in one loop.
    for i = 2:length (idx)
      ## Tree component start.
      istart = idx(i-1) + 1;
      ## Tree component end.
      istop = idx(i) - 1;
      if (istop - istart < 1)
        continue;
      endif
      plot (x_coordinate(skelet(istart:istop)),
            y_coordinate(skelet(istart:istop)), edge_style);
    endfor

    ## Set axis and graph size.
    axis ([0.5, left_most+0.5, max_ht-0.5, num_nodes-0.5], "nolabel");

  unwind_protect_cleanup
    if (! hold_is_on)
      hold ("off");
    endif
  end_unwind_protect

endfunction


%!demo
%! clf;
%! treeplot ([2 4 2 0 6 4 6]);
%! % Plot a simple tree plot

%!demo
%! clf;
%! treeplot ([2 4 2 0 6 4 6], "b+", "g");
%! % Plot a simple tree plot defining the edge and node styles

