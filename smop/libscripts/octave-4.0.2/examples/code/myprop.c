#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[],
             int nrhs, const mxArray* prhs[])
{
  double handle;
  char property[256];

  if (nrhs < 2 || nrhs > 3)
    mexErrMsgTxt ("incorrect number of arguments");
  if (! mxIsDouble (prhs[0]))
    mexErrMsgTxt ("handle must be a double scalar");
  if (! mxIsChar (prhs[1]))
    mexErrMsgTxt ("property must be a string");

  handle = mxGetScalar (prhs[0]);
  mxGetString (prhs[1], property, 256);
  plhs[0] = mxDuplicateArray (mexGet (handle, property));

  if (nrhs == 3)
    if (mexSet (handle, property, mxDuplicateArray (prhs[2])))
      mexErrMsgTxt ("failed to set property");
}
