      subroutine mexFunction (nlhs, plhs, nrhs, prhs)

      implicit none

      integer*4 nlhs, nrhs

* The following will need to be integer*8 on 64-bit systems, otherwise
* these variables won't be large enough to hold pointers...
      integer*4 plhs(*), prhs(*)

      integer*4 mxIsString, mxGetString, mxGetN, mexCallMATLAB
      integer*4 status, len
      character*100 str

      call mexPrintf ('Hello, World!')

      if (nrhs .lt. 1 .or. mxIsString (prhs(1)) .ne. 1) then
        call mexErrMsgTxt ('function name expected')
      endif

      len = mxGetN (prhs(1))

      status = mxGetString (prhs(1), str, 100)

      call mexPrintf ('FORTRAN will call the interpreter now')

      status = mexCallMATLAB (nlhs, plhs, nrhs-1, prhs(2), str(1:len))

      return
      end
