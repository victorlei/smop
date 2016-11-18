function v = get (s, propName)

  switch propName
    case 'bleek'
      v = s.bleek;
    otherwise
      error ([propName, ' is not a valid Blork property']);
  end

end
