      subroutine xdlange (norm, m, n, a, lda, work, retval)
      character norm
      integer lda, m, n
      double precision a (lda, *), work (*), dlange, retval
      retval = dlange (norm, m, n, a, lda, work)
      return
      end
