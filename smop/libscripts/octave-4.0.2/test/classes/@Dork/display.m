function display (s)
%  Display the critical info for an amplifier

   gick = get (s, 'gick');
   disp ([inputname(1),'.gick = ']);
   disp (' ');
   disp (gick);
   disp (' ');
   disp ([inputname(1),'.gack = ']);
   disp (' ');
   disp (s.gack);

end
