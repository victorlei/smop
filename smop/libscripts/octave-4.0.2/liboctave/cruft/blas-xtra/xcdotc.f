      subroutine xcdotc (n, zx, incx, zy, incy, retval)
      complex cdotc, zx(*), zy(*), retval
      integer n, incx, incy
      external cdotc
      retval = cdotc (n, zx, incx, zy, incy)
      return
      end
