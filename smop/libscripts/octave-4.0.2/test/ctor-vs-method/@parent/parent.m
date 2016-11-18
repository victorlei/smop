function rot = parent (a)
  __trace__ ('begin parent/parent');
  if (nargin == 0)
    rot = class (struct (), 'parent');
  else
    switch class (a)
      case 'parent'
        %% copy constructor
        rot = a;
      otherwise
        error ('type mismatch in parent constructor')
    end
  end
  __trace__ ('end parent/parent');
end
