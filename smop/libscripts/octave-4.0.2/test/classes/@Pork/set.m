function s = set (s, varargin)

  propArgs = varargin;
  while (length (propArgs) >= 2)
    propName  = propArgs{1};
    propValue = propArgs{2};
    propArgs  = propArgs(3:end);
    switch propName
      case 'gurk'
        s.gurk = propValue;
      otherwise
        s.Spork = set (s.Spork, propName, propValue);
    end
  end

end
