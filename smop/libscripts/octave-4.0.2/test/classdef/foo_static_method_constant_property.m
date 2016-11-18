classdef foo_static_method_constant_property
  properties
    frequency;
  end
  properties (Constant = true)
    pie = pi;
  end
  methods
    function obj = foo_static_method_constant_property (f)
      if (nargin == 1)
        obj.frequency = f;
      elseif (nargin ~= 0)
        error ('foo_static_method_constant_property:SyntaxError', ...
               'foo_static_method_constant_property: Invalid syntax')
      end
    end
    function res = cosine (obj, t)
      res = cos (obj.radians_per_cycle () * obj.frequency * t);
    end
    function res = sine (obj, t)
      res = sin (obj.radians_per_cycle () * obj.frequency * t);
    end
  end
  methods (Static)
    function res = radians_per_cycle ()
      res = 2 * foo_static_method_constant_property.pie;
    end
  end
end

