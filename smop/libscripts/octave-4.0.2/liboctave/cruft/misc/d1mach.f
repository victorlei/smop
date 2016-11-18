      double precision function d1mach (i)
      integer i
      logical init
      double precision dmach(5)
      double precision dlamch
      external dlamch
      save init, dmach
      data init /.false./
      if (.not. init) then
        dmach(1) = dlamch ('u')
        dmach(2) = dlamch ('o')
        dmach(3) = dlamch ('e')
        dmach(4) = dlamch ('p')
        dmach(5) = log10 (dlamch ('b'))
        init = .true.
      endif
      if (i .lt. 1 .or. i .gt. 5) goto 999
      d1mach = dmach(i)
      return
  999 write (*, 1999) i
 1999 format (' d1mach - i out of bounds', i10)
      call xstopx (' ')
      d1mach = 0
      end
