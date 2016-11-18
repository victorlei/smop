#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[],
             int nrhs, const mxArray* prhs[])
{
  char *str;

  mexPrintf ("Starting file myfeval.mex\n");

  mexPrintf ("I have %d inputs and %d outputs\n", nrhs, nlhs);

  if (nrhs < 1 || ! mxIsChar (prhs[0]))
    mexErrMsgTxt ("ARG1 must be a function name");

  str = mxArrayToString (prhs[0]);

  mexPrintf ("I'm going to call the function %s\n", str);

  if (nlhs == 0)
    nlhs = 1;  // Octave's automatic 'ans' variable

  /* Cast prhs just to get rid of 'const' qualifier and stop compile warning */
  mexCallMATLAB (nlhs, plhs, nrhs-1, (mxArray**)prhs+1, str);

  mxFree (str);
}
