      subroutine xcdotu (n, zx, incx, zy, incy, retval)
      complex cdotu, zx(*), zy(*), retval
      integer n, incx, incy
      external cdotu
      retval = cdotu (n, zx, incx, zy, incy)
      return
      end
