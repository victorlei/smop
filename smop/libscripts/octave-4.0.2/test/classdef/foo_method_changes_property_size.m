classdef foo_method_changes_property_size
  properties
    element;
  end
  methods
    function obj = foo_method_changes_property_size (n)
      obj.element = 1:n;
    end
    function obj = move_element_to_end (obj, n)
      obj.element(end+1) = obj.element(n);
      obj.element(n) = [];
    end
  end
end
