function x = subsref (g, s)

  switch s.type
  case '.'
    switch s.subs
    case 'gyrk'
      x = g.gyrk;
  end

end
