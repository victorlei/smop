      subroutine xderf (x, result)
      external derf
      double precision x, result, derf
      result = derf (x)
      return
      end
