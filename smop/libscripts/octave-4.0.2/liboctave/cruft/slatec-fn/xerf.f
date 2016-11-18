      subroutine xerf (x, result)
      external erf
      real x, result, erf
      result = erf (x)
      return
      end
