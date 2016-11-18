classdef foo_value_class
  properties
    rate;
    term;
    principle;
  end
  methods
    function obj = foo_value_class (r, t, p)
      if (nargin == 3)
        obj.rate = r;
        obj.term = t;
        obj.principle = p;
      elseif (nargin ~= 0)
        error ('foo_value_class:SyntaxError', ...
               'foo_value_class: Invalid syntax')
      end
    end
    function amt = amount (obj)
      i = obj.rate / (12 * 100);
      if (i == 0 && obj.term == 0)
        amt = obj.principle;
      else
        amt = (obj.principle * i) / (1 - (1 + i)^(-obj.term));
      end
    end
  end
end

