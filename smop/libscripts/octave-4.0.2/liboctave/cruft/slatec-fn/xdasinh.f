      subroutine xdasinh (x, result)
      external dasinh
      double precision x, result, dasinh
      result = dasinh (x)
      return
      end
