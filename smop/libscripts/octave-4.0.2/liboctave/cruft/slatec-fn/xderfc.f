      subroutine xderfc (x, result)
      external derfc
      double precision x, result, derfc
      result = derfc (x)
      return
      end
