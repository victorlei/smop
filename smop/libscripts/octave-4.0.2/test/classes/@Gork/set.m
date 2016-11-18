function s = set (s, varargin)

  propArgs = varargin;
  while (length (propArgs) >= 2)
    propName  = propArgs{1};
    propValue = propArgs{2};
    propArgs  = propArgs(3:end);
    switch propName
      case 'cork'
        if (isa (propValue, 'Cork'))
          s.Cork = propValue;
        else
          s.Cork = set (s.Cork, 'click', propValue);
        end
      case 'gark'
        s.gark = propValue;
      otherwise
        % Note that get/set for multiple parents is hard.  We only do one
        % branch of the parent tree just to test this stuff out.
        s.Dork = set (s.Dork, propName, propValue);
    end
  end

end
