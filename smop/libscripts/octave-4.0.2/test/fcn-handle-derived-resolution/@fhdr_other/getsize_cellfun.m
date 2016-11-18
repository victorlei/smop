function r = getsize_cellfun (x)
  r = cellfun (@numel, {x.d});
end
