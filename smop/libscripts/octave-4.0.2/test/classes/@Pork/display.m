function display (s)
%  Display the critical info for an amplifier

   geek = get (s, 'geek');
   disp ([inputname(1),'.geek = ']);
   disp (' ');
   disp (geek);
   disp (' ');
   disp ([inputname(1),'.gurk = ']);
   disp (' ');
   disp (s.gurk);

end
