      subroutine xdatanh (x, result)
      external datanh
      double precision x, result, datanh
      result = datanh (x)
      return
      end
