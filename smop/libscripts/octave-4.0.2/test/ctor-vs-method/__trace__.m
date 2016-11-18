function r = __trace__ (t)
  persistent history

  if (isempty (history))
    history = {};
  end
  if (nargin == 0)
    if (nargout == 0)
      history = {};
    else
      r = history;
    end
  elseif (nargin == 1);
    history = [history; t];
  else
    error ('incorrect call to __trace__');
  end

end
