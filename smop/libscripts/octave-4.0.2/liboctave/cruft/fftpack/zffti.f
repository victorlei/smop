      subroutine zffti (n,wsave)
      implicit double precision (a-h,o-z)
      dimension       wsave(*)
      if (n .eq. 1) return
      iw1 = n+n+1
      iw2 = iw1+n+n
      call zffti1 (n,wsave(iw1),wsave(iw2))
      return
      end
