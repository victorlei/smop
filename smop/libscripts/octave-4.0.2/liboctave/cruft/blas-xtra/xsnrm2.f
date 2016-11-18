      subroutine xsnrm2 (n, x, incx, retval)
      real snrm2, x(*), retval
      integer n, incx
      retval = snrm2 (n, x, incx)
      return
      end
