      subroutine xdbetai (x, a, b, result)
      external dbetai
      double precision x, a, b, result, dbetai
      result = dbetai (x, a, b)
      return
      end
