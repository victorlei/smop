function v = get (s, propName)

  switch propName
    case 'click'
      v = s.click;
    otherwise
      error ([propName, ' is not a valid Cork property']);
  end

end
