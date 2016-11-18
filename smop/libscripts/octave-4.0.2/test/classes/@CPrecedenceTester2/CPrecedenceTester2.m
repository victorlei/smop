function x = CPrecedenceTester2 (flag)

  x = struct ('useless_data', pi^2);
  x = class (x, 'CPrecedenceTester2');

  switch flag
    case 1  % CPrecedencetester2 > Snork
      superiorto ('Snork');
    case 2  % CPrecedencetester2 < Snork
      inferiorto ('Snork');
    otherwise
      error ('Incorrect value for argument flag: %d', flag);
  end

end
