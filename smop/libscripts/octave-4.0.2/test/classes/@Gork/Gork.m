function s = Gork (g)

  if (nargin == 1 && isa (g, 'Gork'))
    s = sprk;
    return;
  end

  drk  = Dork ();
  prk  = Pork ();
  blrk = Blork ();
  s.Cork = Cork (17);  % Aggregation.
  s.gark = -2;
  s.gyrk = -3;
  s = class (s,'Gork',drk,prk,blrk);

end
