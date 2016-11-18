function v = get (s, propName)

  switch propName
    case 'cork'
      v = s.Cork;
    case 'gark'
      v = s.gark;
    otherwise
      % Note that get/set for multiple parents is hard.  We only do one
      % branch of the parent tree just to test this stuff out.
      v = get (s.Dork,propName);
  end

end
