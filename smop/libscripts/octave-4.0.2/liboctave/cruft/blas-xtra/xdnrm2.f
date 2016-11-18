      subroutine xdnrm2 (n, x, incx, retval)
      double precision dnrm2, x(*), retval
      integer n, incx
      retval = dnrm2 (n, x, incx)
      return
      end
