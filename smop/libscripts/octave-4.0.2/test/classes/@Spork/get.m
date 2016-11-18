function v = get (s, propName)

  switch propName
    case 'geek'
      v = s.geek;
    otherwise
      error ([propName, ' is not a valid Spork property']);
  end

end
