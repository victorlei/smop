function s = Cork (click)
% Test class.

  if (nargin == 1 && isa (click, 'Cork'))
    s = click;
    return;
  end

  if (nargin < 1)
    s.click = 1;
  else
    s.click = click;
  end
  s = class (s, 'Cork');

end
