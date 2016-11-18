      integer function i1mach (i)
      integer i, imach(16)
      logical init
      double precision dlamch
      real slamch
      external dlamch, slamch
      save imach, init
      data imach / 5, 6, 0, 6, 32, 4, 2, 31, 2147483647,
     $     2, 0, 0, 0, 0, 0, 0 /
      data init /.false./
      if (.not. init) then
        imach(11) = slamch ('n')
        imach(12) = slamch ('m')
        imach(13) = slamch ('l')
        imach(14) = dlamch ('n')
        imach(15) = dlamch ('m')
        imach(16) = dlamch ('l')
        init = .true.
      endif
      if (i .lt. 1 .or. i .gt. 16) goto 999
      i1mach = imach(i)
      return
  999 write (*, 1999) i
 1999 format (' i1mach - i out of bounds', i10)
      call xstopx (' ')
      i1mach = 0
      end
