      subroutine xdgamit (a, x, result)
      external dgamit
      double precision a, x, result, dgamit
      result = dgamit (a, x)
      return
      end
