function snk = subsasgn (snk, s, val)

  if (length (s) < 1)
    error ('Snork: needs index');
  end
  switch (s(1).type)
    case '()'
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ('Snork: need exactly one index');
      else
        if (length (s) == 1)
          snk.cack(ind{1}) = val;
        else
          error ('Snork: chained subscripts not allowed for {}');
        end
      end
    case '{}'
      ind = s(1).subs;
      if (numel (ind) != 1)
        error ('Snork: need exactly one index');
      else
        if (length (s) == 1)
          snk.cack(ind{1}) = val;
        else
          error ('Snork: chained subscripts not allowed for {}');
        end
      end
    case '.'
      fld = s(1).subs;
      if (strcmp (fld, 'gick'))
        snk.gick = val;
      else
        error ('Snork/subsasgn: invalid property \"%s\"', fld);
      end
    otherwise
      error ('invalid subscript type');
  end

end
