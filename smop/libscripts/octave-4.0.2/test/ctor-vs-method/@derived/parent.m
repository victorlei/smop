function r = parent (a)
  __trace__ ('begin derived/parent');
  if (isa (a, 'parent'))
    r = parent (a.parent);
  else
    error ('foo');
  end
  __trace__ ('end derived/parent');
end
