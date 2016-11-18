function r = fhdr_derived (n)
  s.a = n;
  p = fhdr_parent (n);
  r = class (s, 'fhdr_derived', p);
end
