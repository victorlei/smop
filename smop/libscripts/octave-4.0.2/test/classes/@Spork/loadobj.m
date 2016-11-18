function out = loadobj (in)

  out = in;
  if (!isa (in, 'Spork'))
    out.cack = [];
  end

end
