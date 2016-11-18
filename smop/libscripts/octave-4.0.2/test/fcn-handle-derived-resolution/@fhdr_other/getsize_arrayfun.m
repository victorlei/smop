function r = getsize_arrayfun (x)
  r = arrayfun (@(i) numel (x(i).d), 1:numel (x), 'uniformoutput', true);
end
