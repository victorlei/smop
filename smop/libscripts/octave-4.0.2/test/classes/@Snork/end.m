function r = end (snk, index_pos, num_indices)

  if (num_indices != 1)
    error ('Snork object may only have one index')
  end

  r = length (snk.cack);

end
