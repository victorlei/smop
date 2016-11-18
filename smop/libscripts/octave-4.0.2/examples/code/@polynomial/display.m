function display (p)
  a = p.poly;
  first = true;
  fprintf ("%s =", inputname (1));
  for i = 1 : length (a);
    if (a(i) != 0)
      if (first)
        first = false;
      elseif (a(i) > 0)
        fprintf (" +");
      endif
      if (a(i) < 0)
        fprintf (" -");
      endif
      if (i == 1)
        fprintf (" %g", abs (a(i)));
      elseif (abs(a(i)) != 1)
        fprintf (" %g *", abs (a(i)));
      endif
      if (i > 1)
        fprintf (" X");
      endif
      if (i > 2)
        fprintf (" ^ %d", i - 1);
      endif
    endif
  endfor
  if (first)
    fprintf (" 0");
  endif
  fprintf ("\n");
endfunction
