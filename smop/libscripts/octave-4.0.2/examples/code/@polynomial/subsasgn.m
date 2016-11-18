function p = subsasgn (p, s, val)
  if (length (s) < 1)
    error ("polynomial: needs index");
  endif
  switch (s(1).type)
    case "{}"
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ("polynomial: need exactly one index");
      else
        if (length (s) == 1)
          if (isnumeric (ind{1}))
            p.poly(ind{1}+1) = val;
          else
            p.poly(ind{1}) = val;
          endif
        else
          error ("polynomial: chained subscripts not allowed for {}");
        endif
      endif
    case "."
      fld = s(1).subs;
      if (strcmp (fld, "poly"))
        if (length (s) == 1)
          p.poly = val;
        else
          p.poly = subsasgn (p.poly, s(2:end), val);
        endif
      else
        error ("@polynomial/subsref: invalid property \"%s\"", fld);
      endif
    otherwise
      error ("invalid subscript type");
  endswitch
endfunction
