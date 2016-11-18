function b = subsref (snk, s)

  if (isempty (s))
    error ('Snork: missing index');
  end
  switch (s(1).type)
    case '()'
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ('Snork: need exactly one index');
      else
        b = snk.cack(ind{1});
      end
    case '{}'
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ('Snork: need exactly one index');
      else
        b = snk.cack(ind{1});
      end
    case '.'
      fld = s.subs;
      if (strcmp (fld, 'gick'))
        b = snk.gick;
      else
        error ('Snork/subsref: invalid property \"%s\"', fld);
      end
    otherwise
      error ('invalid subscript type');
  end

end
