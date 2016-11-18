      real function r1mach (i)
      integer i
      logical init
      real rmach(5)
      real slamch
      external slamch
      save init, rmach
      data init /.false./
      if (.not. init) then
        rmach(1) = slamch ('u')
        rmach(2) = slamch ('o')
        rmach(3) = slamch ('e')
        rmach(4) = slamch ('p')
        rmach(5) = log10 (slamch ('b'))
        init = .true.
      endif
      if (i .lt. 1 .or. i .gt. 5) goto 999
      r1mach = rmach(i)
      return
  999 write (*, 1999) i
 1999 format (' r1mach - i out of bounds', i10)
      call xstopx (' ')
      r1mach = 0
      end
