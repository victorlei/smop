      subroutine qk15i(f,boun,inf,a,b,result,abserr,resabs,resasc,ierr)
c***begin prologue  qk15i
c***date written   800101   (yymmdd)
c***revision date  830518   (yymmdd)
c***category no.  h2a3a2,h2a4a2
c***keywords  15-point transformed gauss-kronrod rules
c***author  piessens,robert,appl. math. & progr. div. - k.u.leuven
c           de doncker,elise,appl. math. & progr. div. - k.u.leuven
c***purpose  the original (infinite integration range is mapped
c            onto the interval (0,1) and (a,b) is a part of (0,1).
c            it is the purpose to compute
c            i = integral of transformed integrand over (a,b),
c            j = integral of abs(transformed integrand) over (a,b).
c***description
c
c           integration rule
c           standard fortran subroutine
c           real version
c
c           parameters
c            on entry
c              f      - subroutine f(x,ierr,result) defining the integrand
c                       function f(x). the actual name for f needs to be
c                       declared e x t e r n a l in the calling program.
c
c              boun   - real
c                       finite bound of original integration
c                       range (set to zero if inf = +2)
c
c              inf    - integer
c                       if inf = -1, the original interval is
c                                   (-infinity,bound),
c                       if inf = +1, the original interval is
c                                   (bound,+infinity),
c                       if inf = +2, the original interval is
c                                   (-infinity,+infinity) and
c                       the integral is computed as the sum of two
c                       integrals, one over (-infinity,0) and one over
c                       (0,+infinity).
c
c              a      - real
c                       lower limit for integration over subrange
c                       of (0,1)
c
c              b      - real
c                       upper limit for integration over subrange
c                       of (0,1)
c
c            on return
c              result - real
c                       approximation to the integral i
c                       result is computed by applying the 15-point
c                       kronrod rule(resk) obtained by optimal addition
c                       of abscissae to the 7-point gauss rule(resg).
c
c              abserr - real
c                       estimate of the modulus of the absolute error,
c                       which should equal or exceed abs(i-result)
c
c              resabs - real
c                       approximation to the integral j
c
c              resasc - real
c                       approximation to the integral of
c                       abs((transformed integrand)-i/(b-a)) over (a,b)
c
c***references  (none)
c***routines called  r1mach
c***end prologue  qk15i
c
      real a,absc,absc1,absc2,abserr,b,boun,centr,
     *  dinf,r1mach,epmach,fc,fsum,fval1,fval2,fvalt,fv1,
     *  fv2,hlgth,resabs,resasc,resg,resk,reskh,result,tabsc1,tabsc2,
     *  uflow,wg,wgk,xgk
      integer inf,j,min0
      external f
c
      dimension fv1(7),fv2(7),xgk(8),wgk(8),wg(8)
c
c           the abscissae and weights are supplied for the interval
c           (-1,1).  because of symmetry only the positive abscissae and
c           their corresponding weights are given.
c
c           xgk    - abscissae of the 15-point kronrod rule
c                    xgk(2), xgk(4), ... abscissae of the 7-point
c                    gauss rule
c                    xgk(1), xgk(3), ...  abscissae which are optimally
c                    added to the 7-point gauss rule
c
c           wgk    - weights of the 15-point kronrod rule
c
c           wg     - weights of the 7-point gauss rule, corresponding
c                    to the abscissae xgk(2), xgk(4), ...
c                    wg(1), wg(3), ... are set to zero.
c
      data xgk(1),xgk(2),xgk(3),xgk(4),xgk(5),xgk(6),xgk(7),
     *  xgk(8)/
     *     0.9914553711208126e+00,     0.9491079123427585e+00,
     *     0.8648644233597691e+00,     0.7415311855993944e+00,
     *     0.5860872354676911e+00,     0.4058451513773972e+00,
     *     0.2077849550078985e+00,     0.0000000000000000e+00/
c
      data wgk(1),wgk(2),wgk(3),wgk(4),wgk(5),wgk(6),wgk(7),
     *  wgk(8)/
     *     0.2293532201052922e-01,     0.6309209262997855e-01,
     *     0.1047900103222502e+00,     0.1406532597155259e+00,
     *     0.1690047266392679e+00,     0.1903505780647854e+00,
     *     0.2044329400752989e+00,     0.2094821410847278e+00/
c
      data wg(1),wg(2),wg(3),wg(4),wg(5),wg(6),wg(7),wg(8)/
     *     0.0000000000000000e+00,     0.1294849661688697e+00,
     *     0.0000000000000000e+00,     0.2797053914892767e+00,
     *     0.0000000000000000e+00,     0.3818300505051189e+00,
     *     0.0000000000000000e+00,     0.4179591836734694e+00/
c
c
c           list of major variables
c           -----------------------
c
c           centr  - mid point of the interval
c           hlgth  - half-length of the interval
c           absc*  - abscissa
c           tabsc* - transformed abscissa
c           fval*  - function value
c           resg   - result of the 7-point gauss formula
c           resk   - result of the 15-point kronrod formula
c           reskh  - approximation to the mean value of the transformed
c                    integrand over (a,b), i.e. to i/(b-a)
c
c           machine dependent constants
c           ---------------------------
c
c           epmach is the largest relative spacing.
c           uflow is the smallest positive magnitude.
c
c***first executable statement  qk15i
      epmach = r1mach(4)
      uflow = r1mach(1)
      dinf = min0(1,inf)
c
      centr = 0.5e+00*(a+b)
      hlgth = 0.5e+00*(b-a)
      tabsc1 = boun+dinf*(0.1e+01-centr)/centr
      call f(tabsc1, ierr, fval1)
      if (ierr.lt.0) return
      if(inf.eq.2) then
         call f(-tabsc1, ierr, fval1)
         if (ierr.lt.0) return
         fval1 = fval1 + fvalt
      endif
      fc = (fval1/centr)/centr
c
c           compute the 15-point kronrod approximation to
c           the integral, and estimate the error.
c
      resg = wg(8)*fc
      resk = wgk(8)*fc
      resabs = abs(resk)
      do 10 j=1,7
        absc = hlgth*xgk(j)
        absc1 = centr-absc
        absc2 = centr+absc
        tabsc1 = boun+dinf*(0.1e+01-absc1)/absc1
        tabsc2 = boun+dinf*(0.1e+01-absc2)/absc2
        call f(tabsc1, ierr, fval1)
        if (ierr.lt.0) return
        call f(tabsc2, ierr, fval2)
        if (ierr.lt.0) return
        if(inf.eq.2) then
           call f(-tabsc1,ierr,fvalt)
           if (ierr.lt.0) return
           fval1 = fval1 + fvalt
        endif
        if(inf.eq.2) then
           call f(-tabsc2,ierr,fvalt)
           if (ierr.lt.0) return
           fval2 = fval2 + fvalt
        endif
        fval1 = (fval1/absc1)/absc1
        fval2 = (fval2/absc2)/absc2
        fv1(j) = fval1
        fv2(j) = fval2
        fsum = fval1+fval2
        resg = resg+wg(j)*fsum
        resk = resk+wgk(j)*fsum
        resabs = resabs+wgk(j)*(abs(fval1)+abs(fval2))
   10 continue
      reskh = resk*0.5e+00
      resasc = wgk(8)*abs(fc-reskh)
      do 20 j=1,7
        resasc = resasc+wgk(j)*(abs(fv1(j)-reskh)+abs(fv2(j)-reskh))
   20 continue
      result = resk*hlgth
      resasc = resasc*hlgth
      resabs = resabs*hlgth
      abserr = abs((resk-resg)*hlgth)
      if(resasc.ne.0.0e+00.and.abserr.ne.0.e0) abserr = resasc*
     * amin1(0.1e+01,(0.2e+03*abserr/resasc)**1.5e+00)
      if(resabs.gt.uflow/(0.5e+02*epmach)) abserr = amax1
     * ((epmach*0.5e+02)*resabs,abserr)
      return
      end
