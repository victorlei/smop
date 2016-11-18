      subroutine cfftb (n,c,wsave)
      dimension       c(*)       ,wsave(*)
      if (n .eq. 1) return
      iw1 = n+n+1
      iw2 = iw1+n+n
      call cfftb1 (n,c,wsave,wsave(iw1),wsave(iw2))
      return
      end
