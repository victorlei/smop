      subroutine xdgami (a, x, result)
      external dgami
      double precision a, x, result, dgami
      result = dgami (a, x)
      return
      end
