#include "mex.h"

void
mexFunction (int nlhs, mxArray *plhs[],
             int nrhs, const mxArray *prhs[])
{
  const char *nm;

  nm = mexFunctionName ();
  mexPrintf ("You called function: %s\n", nm);
  if (strcmp (nm, "myfunc") == 0)
    mexPrintf ("This is the principal function\n", nm);

  return;
}
