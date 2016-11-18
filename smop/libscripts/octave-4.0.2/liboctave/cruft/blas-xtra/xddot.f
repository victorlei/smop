      subroutine xddot (n, dx, incx, dy, incy, retval)
      double precision ddot, dx(*), dy(*), retval
      integer n, incx, incy
      retval = ddot (n, dx, incx, dy, incy)
      return
      end
