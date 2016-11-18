function s = Sneetch (mcbean)
% Test class: should produce error.

  if (nargin == 1 && isa (mcbean, 'Sneetch'))
    s = mcbean;
  else
    if (nargin == 0)
      s.mcbean = 1;
    else
      s.mcbean = mcbean;
    end
    s = class (s, 'Sneetch');
    s.sylvester = 1;
  end

end
