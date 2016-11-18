function x = CPrecedenceTester3 (flag)

  x = struct ('useless_data', pi^3);
  x = class (x, 'CPrecedenceTester3');

  switch flag
    case 1  % CPrecedencetester3 > Snork
      superiorto ('Snork');
    case 2  % CPrecedencetester3 < Snork
      inferiorto ('Snork');
    otherwise
      error ('Incorrect value for argument flag: %d', flag);
  end

end
