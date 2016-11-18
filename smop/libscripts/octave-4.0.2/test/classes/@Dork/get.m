function v = get (s, propName)

  switch propName
    case 'gack'
      v = s.gack;
    otherwise
      v = get (s.Snork,propName);
  end

end
