      subroutine xslange (norm, m, n, a, lda, work, retval)
      character norm
      integer lda, m, n
      real a (lda, *), work (*), slange, retval
      retval = slange (norm, m, n, a, lda, work)
      return
      end
