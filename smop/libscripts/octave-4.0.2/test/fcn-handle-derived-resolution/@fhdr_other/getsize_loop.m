function r = getsize_loop (x)
  n = numel (x);
  r = zeros (1, n);
  for i = 1:n
    r(i) = numel (x(i).d);
  end
end
