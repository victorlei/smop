      subroutine xsdot (n, dx, incx, dy, incy, retval)
      real ddot, dx(*), dy(*), retval, sdot
      integer n, incx, incy
      retval = sdot (n, dx, incx, dy, incy)
      return
      end
