function s = set (s, varargin)

  propArgs = varargin;
  while (length (propArgs) >= 2)
    propName  = propArgs{1};
    propValue = propArgs{2};
    propArgs  = propArgs(3:end);
    switch propName
      case 'gack'
        s.gack = propValue;
      otherwise
        s.Snork = set (s.Snork, propName, propValue);
    end
  end

end
