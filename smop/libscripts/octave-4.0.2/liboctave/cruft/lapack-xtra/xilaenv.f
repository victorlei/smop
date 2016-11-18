      subroutine xilaenv (ispec, name, opts, n1, n2, n3, n4, retval)
      character*(*) name, opts
      integer ilaenv, ispec, n1, n2, n3, n4, retval
      retval = ilaenv (ispec, name, opts, n1, n2, n3, n4)
      return
      end
