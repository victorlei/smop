      subroutine dgennor (av, sd, result)
      double precision av, sd, result
      result = gennor (real (av), real (sd))
      return
      end
      subroutine dgenunf (low, high, result)
      double precision low, high, result
      result = genunf (real (low), real (high))
      return
      end
      subroutine dgenexp (av, result)
      double precision av, result
      result = genexp (real (av))
      return
      end
      subroutine dgengam (a, r, result)
      double precision a, r, result
      result = gengam (real (a), real (r))
      return
      end
      subroutine dignpoi (mu, result)
      double precision mu, result
      result = ignpoi (real (mu))
      return
      end
