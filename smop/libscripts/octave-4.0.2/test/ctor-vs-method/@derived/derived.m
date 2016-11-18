function r = derived (varargin)
  __trace__ ('begin derived/derived');
  r = class (struct (), 'derived', parent ());
  __trace__ ('end derived/derived');
end
