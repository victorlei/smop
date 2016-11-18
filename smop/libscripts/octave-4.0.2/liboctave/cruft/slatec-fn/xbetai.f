      subroutine xbetai (x, a, b, result)
      external betai
      real x, a, b, result, betai
      result = betai (x, a, b)
      return
      end
