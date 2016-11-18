      subroutine fortransub (n, a, s)
      implicit none
      character*(*) s
      real*8 a(*)
      integer*4 i, n, ioerr
      do i = 1, n
        if (a(i) .eq. 0d0) then
          call xstopx ('fortransub: divide by zero')
        else
          a(i) = 1d0 / a(i)
        endif
      enddo
      write (unit = s, fmt = '(a,i3,a,a)', iostat = ioerr)
     $       'There are ', n,
     $       ' values in the input vector', char(0)
      if (ioerr .ne. 0) then
        call xstopx ('fortransub: error writing string')
      endif
      return
      end

