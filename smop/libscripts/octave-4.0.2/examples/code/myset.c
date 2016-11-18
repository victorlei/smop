#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[],
             int nrhs, const mxArray* prhs[])
{
  char *str;
  mxArray *v;
  int found = 0;

  if (nrhs != 2 || ! mxIsChar (prhs[0]))
    mexErrMsgTxt ("Arguments must be a symbol name and a value");

  str = mxArrayToString (prhs[0]);

  // FIXME: If variable does not exist, error is reported which prevents
  //        subsequent mexGetArray function from working.
  v = mexGetArray (str, "global");
  if (v)
    {
      mexPrintf ("%s is a global variable with the following value:\n", str);
      mexCallMATLAB (0, NULL, 1, &v, "disp");
      found = 1;
    }

  if (! found)
    v = mexGetArray (str, "caller");

  if (! found && v)
    {
      mexPrintf ("%s is a caller variable with the following value:\n", str);
      mexCallMATLAB (0, NULL, 1, &v, "disp");
    }

  // WARNING!! Can't do this in MATLAB!  Must copy variable first.
  mexPutVariable ("caller", str, prhs[1]);
}
