function display (s)
%  Display the critical info for a Gork.

   dork_base = s.Dork
   %pork_base = s.Pork
   %sprk = s.sprk;
   gark = get (s, 'gark');
   disp ([inputname(1),'.gark = ']);
   disp (' ');
   disp (gark);
   disp ([inputname(1),'.Cork= ']);
   disp (' ');
   disp (click(s.Cork));

end
