      subroutine xdgamma (x, result)
      external dgamma
      double precision x, result, dgamma
      result = dgamma (x)
      return
      end
