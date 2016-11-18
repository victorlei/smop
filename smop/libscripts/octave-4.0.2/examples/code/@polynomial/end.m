function r = end (obj, index_pos, num_indices)

  if (num_indices != 1)
    error ("polynomial object may only have one index")
  endif

  r = length (obj.poly) - 1;

endfunction
