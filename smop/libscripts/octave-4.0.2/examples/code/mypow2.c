#include "mex.h"

void
mexFunction (int nlhs, mxArray* plhs[],
             int nrhs, const mxArray* prhs[])
{
  mwSize n;
  mwIndex i;
  double *vri, *vro;

  if (nrhs != 1 || ! mxIsDouble (prhs[0]))
    mexErrMsgTxt ("ARG1 must be a double matrix");

  n = mxGetNumberOfElements (prhs[0]);
  plhs[0] = mxCreateNumericArray (mxGetNumberOfDimensions (prhs[0]),
                                  mxGetDimensions (prhs[0]),
                                  mxGetClassID (prhs[0]),
                                  mxIsComplex (prhs[0]));
  vri = mxGetPr (prhs[0]);
  vro = mxGetPr (plhs[0]);

  if (mxIsComplex (prhs[0]))
    {
      double *vii, *vio;
      vii = mxGetPi (prhs[0]);
      vio = mxGetPi (plhs[0]);

      for (i = 0; i < n; i++)
        {
          vro[i] = vri[i] * vri[i] - vii[i] * vii[i];
          vio[i] = 2 * vri[i] * vii[i];
        }
    }
  else
    {
      for (i = 0; i < n; i++)
        vro[i] = vri[i] * vri[i];
    }
}
