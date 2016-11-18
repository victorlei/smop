      subroutine xzdotu (n, zx, incx, zy, incy, retval)
      double complex zdotu, zx(*), zy(*), retval
      integer n, incx, incy
      external zdotu
      retval = zdotu (n, zx, incx, zy, incy)
      return
      end
