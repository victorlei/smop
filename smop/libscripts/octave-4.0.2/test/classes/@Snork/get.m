function v = get (s, propName)

  switch propName
    case 'gick'
      v = s.gick;
    otherwise
      error ([propName, ' is not a valid Snork property']);
  end

end
