      subroutine xerfc (x, result)
      external erfc
      real x, result, erfc
      result = erfc (x)
      return
      end
