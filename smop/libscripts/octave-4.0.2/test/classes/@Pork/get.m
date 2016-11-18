function v = get (s, propName)

  switch propName
    case 'gurk'
      v = s.gurk;
    otherwise
      v = get (s.Spork, propName);
  end

end
