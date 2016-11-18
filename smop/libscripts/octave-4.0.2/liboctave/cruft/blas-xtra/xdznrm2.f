      subroutine xdznrm2 (n, x, incx, retval)
      double precision dznrm2, retval
      double complex x(*)
      integer n, incx
      retval = dznrm2 (n, x, incx)
      return
      end
