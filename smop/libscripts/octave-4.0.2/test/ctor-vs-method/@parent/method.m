function r = method (a)
  __trace__ ('begin parent/method');
  r = parent (a);
  __trace__ ('end parent/method');
end
