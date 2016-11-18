      subroutine xzdotc (n, zx, incx, zy, incy, retval)
      double complex zdotc, zx(*), zy(*), retval
      integer n, incx, incy
      external zdotc
      retval = zdotc (n, zx, incx, zy, incy)
      return
      end
