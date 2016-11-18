function b = subsref (a, s)
  if (isempty (s))
    error ("polynomial: missing index");
  endif
  switch (s(1).type)
    case "()"
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ("polynomial: need exactly one index");
      else
        b = polyval (fliplr (a.poly), ind{1});
      endif
    case "{}"
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ("polynomial: need exactly one index");
      else
        if (isnumeric (ind{1}))
          b = a.poly(ind{1}+1);
        else
          b = a.poly(ind{1});
        endif
      endif
    case "."
      fld = s.subs;
      if (strcmp (fld, "poly"))
        b = a.poly;
      else
        error ("@polynomial/subsref: invalid property \"%s\"", fld);
      endif
    otherwise
      error ("invalid subscript type");
  endswitch
  if (numel (s) > 1)
    b = subsref (b, s(2:end));
  endif
endfunction
