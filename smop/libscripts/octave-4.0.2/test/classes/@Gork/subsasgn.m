function g = subsasgn (g, s, x)

  switch s.type
  case '.'
    switch s.subs
    case 'gyrk'
      g.gyrk = x;
  end

end
