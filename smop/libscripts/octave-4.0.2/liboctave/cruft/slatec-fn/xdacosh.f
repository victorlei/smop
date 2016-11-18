      subroutine xdacosh (x, result)
      external dacosh
      double precision x, result, dacosh
      result = dacosh (x)
      return
      end
