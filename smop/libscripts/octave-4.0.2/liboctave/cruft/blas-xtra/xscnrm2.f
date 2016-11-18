      subroutine xscnrm2 (n, x, incx, retval)
      real scnrm2, retval
      complex x(*)
      integer n, incx
      retval = scnrm2 (n, x, incx)
      return
      end
