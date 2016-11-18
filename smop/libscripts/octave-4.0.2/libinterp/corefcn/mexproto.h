/*

Copyright (C) 2006-2015 Paul Kienzle

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

/*

This code was originally distributed as part of Octave Forge under
the following terms:

Author: Paul Kienzle
I grant this code to the public domain.
2001-03-22

THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

*/

/* mex.h is for use in C-programs only; do NOT include it in mex.cc */

#if ! defined (MEXPROTO_H)
#define MEXPROTO_H

#if defined (__cplusplus)
#include <cstdlib>
extern "C" {
#else
#include <stdlib.h>
#endif

/* The definition of OCTINTERP_API is normally provided by Octave's
   config.h file.  This is provided for the case of mex.h included by
   user programs that don't use Octave's config.h.  */
#if ! defined (OCTINTERP_API)
#if defined (_MSC_VER)
#define OCTINTERP_API __declspec(dllimport)
#else
/* All other compilers, at least for now.  */
#define OCTINTERP_API
#endif
#endif

#define MXARRAY_TYPEDEFS_ONLY
#include "mxarray.h"
#undef MXARRAY_TYPEDEFS_ONLY

/* Interface to the interpreter.  */
extern OCTINTERP_API const char *mexFunctionName (void);

extern OCTINTERP_API int mexCallMATLAB (int nargout, mxArray *argout[],
                                        int nargin, mxArray *argin[],
                                        const char *fname);

extern OCTINTERP_API void mexSetTrapFlag (int flag);
extern OCTINTERP_API int mexEvalString (const char *s);
extern OCTINTERP_API void mexErrMsgTxt (const char *s);
extern OCTINTERP_API void mexErrMsgIdAndTxt (const char *id, const char *s,
                                             ...);
extern OCTINTERP_API void mexWarnMsgTxt (const char *s);
extern OCTINTERP_API void mexWarnMsgIdAndTxt (const char *id, const char *s,
                                              ...);
extern OCTINTERP_API int mexPrintf (const char *fmt, ...);

extern OCTINTERP_API mxArray *mexGetVariable (const char *space,
                                              const char *name);
extern OCTINTERP_API const mxArray *mexGetVariablePtr (const char *space,
                                                       const char *name);

extern OCTINTERP_API int mexPutVariable (const char *space, const char *name,
                                         const mxArray *ptr);

extern OCTINTERP_API void mexMakeArrayPersistent (mxArray *ptr);
extern OCTINTERP_API void mexMakeMemoryPersistent (void *ptr);

extern OCTINTERP_API int mexAtExit (void (*f) (void));
extern OCTINTERP_API const mxArray *mexGet (double handle,
                                            const char *property);
extern OCTINTERP_API int mexIsGlobal (const mxArray *ptr);
extern OCTINTERP_API int mexIsLocked (void);
extern OCTINTERP_API void mexLock (void);
extern OCTINTERP_API int mexSet (double handle, const char *property,
                                 mxArray *val);
extern OCTINTERP_API void mexUnlock (void);

/* Floating point predicates.  */
extern OCTINTERP_API int mxIsFinite (double v);
extern OCTINTERP_API int mxIsInf (double v);
extern OCTINTERP_API int mxIsNaN (double v);

/* Floating point values.  */
extern OCTINTERP_API double mxGetEps (void);
extern OCTINTERP_API double mxGetInf (void);
extern OCTINTERP_API double mxGetNaN (void);

/* Memory management.  */
extern OCTINTERP_API void *mxCalloc (size_t n, size_t size);
extern OCTINTERP_API void *mxMalloc (size_t n);
extern OCTINTERP_API void *mxRealloc (void *ptr, size_t size);
extern OCTINTERP_API void mxFree (void *ptr);

/* Constructors.  */
extern OCTINTERP_API mxArray *mxCreateCellArray (mwSize ndims,
                                                 const mwSize *dims);
extern OCTINTERP_API mxArray *mxCreateCellMatrix (mwSize m, mwSize n);
extern OCTINTERP_API mxArray *mxCreateCharArray (mwSize ndims,
                                                 const mwSize *dims);
extern OCTINTERP_API mxArray *mxCreateCharMatrixFromStrings (mwSize m,
                                                             const char **str);
extern OCTINTERP_API mxArray *mxCreateDoubleMatrix (mwSize nr, mwSize nc,
                                                    mxComplexity flag);
extern OCTINTERP_API mxArray *mxCreateDoubleScalar (double val);
extern OCTINTERP_API mxArray *mxCreateLogicalArray (mwSize ndims,
                                                    const mwSize *dims);
extern OCTINTERP_API mxArray *mxCreateLogicalMatrix (mwSize m, mwSize n);
extern OCTINTERP_API mxArray *mxCreateLogicalScalar (mxLogical val);
extern OCTINTERP_API mxArray *mxCreateNumericArray (mwSize ndims,
                                                    const mwSize *dims,
                                                    mxClassID class_id,
                                                    mxComplexity flag);
extern OCTINTERP_API mxArray *mxCreateNumericMatrix (mwSize m, mwSize n,
                                                     mxClassID class_id,
                                                     mxComplexity flag);
extern OCTINTERP_API mxArray *mxCreateSparse (mwSize m, mwSize n, mwSize nzmax,
                                              mxComplexity flag);
extern OCTINTERP_API mxArray *mxCreateSparseLogicalMatrix (mwSize m, mwSize n,
                                                           mwSize nzmax);
extern OCTINTERP_API mxArray *mxCreateString (const char *str);
extern OCTINTERP_API mxArray *mxCreateStructArray (mwSize ndims,
                                                   const mwSize *dims,
                                                   int num_keys,
                                                   const char **keys);
extern OCTINTERP_API mxArray *mxCreateStructMatrix (mwSize rows, mwSize cols,
                                                    int num_keys,
                                                    const char **keys);

/* Copy constructor.  */
extern OCTINTERP_API mxArray *mxDuplicateArray (const mxArray *v);

/* Destructor.  */
extern OCTINTERP_API void mxDestroyArray (mxArray *v);

/* Type Predicates.  */
extern OCTINTERP_API int mxIsCell (const mxArray *ptr);
extern OCTINTERP_API int mxIsChar (const mxArray *ptr);
extern OCTINTERP_API int mxIsClass (const mxArray *ptr, const char *name);
extern OCTINTERP_API int mxIsComplex (const mxArray *ptr);
extern OCTINTERP_API int mxIsDouble (const mxArray *ptr);
extern OCTINTERP_API int mxIsFunctionHandle (const mxArray *ptr);
extern OCTINTERP_API int mxIsInt16 (const mxArray *ptr);
extern OCTINTERP_API int mxIsInt32 (const mxArray *ptr);
extern OCTINTERP_API int mxIsInt64 (const mxArray *ptr);
extern OCTINTERP_API int mxIsInt8 (const mxArray *ptr);
extern OCTINTERP_API int mxIsLogical (const mxArray *ptr);
extern OCTINTERP_API int mxIsNumeric (const mxArray *ptr);
extern OCTINTERP_API int mxIsSingle (const mxArray *ptr);
extern OCTINTERP_API int mxIsSparse (const mxArray *ptr);
extern OCTINTERP_API int mxIsStruct (const mxArray *ptr);
extern OCTINTERP_API int mxIsUint16 (const mxArray *ptr);
extern OCTINTERP_API int mxIsUint32 (const mxArray *ptr);
extern OCTINTERP_API int mxIsUint64 (const mxArray *ptr);
extern OCTINTERP_API int mxIsUint8 (const mxArray *ptr);

/* Odd type+size predicate.  */
extern OCTINTERP_API int mxIsLogicalScalar (const mxArray *ptr);

/* Odd type+size+value predicate.  */
extern OCTINTERP_API int mxIsLogicalScalarTrue (const mxArray *ptr);

/* Size predicate.  */
extern OCTINTERP_API int mxIsEmpty (const mxArray *ptr);

/* Just plain odd thing to ask of a value.  */
extern OCTINTERP_API int mxIsFromGlobalWS (const mxArray *ptr);

/* Dimension extractors.  */
extern OCTINTERP_API size_t mxGetM (const mxArray *ptr);
extern OCTINTERP_API size_t mxGetN (const mxArray *ptr);
extern OCTINTERP_API mwSize *mxGetDimensions (const mxArray *ptr);
extern OCTINTERP_API mwSize mxGetNumberOfDimensions (const mxArray *ptr);
extern OCTINTERP_API size_t mxGetNumberOfElements (const mxArray *ptr);

/* Dimension setters.  */
extern OCTINTERP_API void mxSetM (mxArray *ptr, mwSize M);
extern OCTINTERP_API void mxSetN (mxArray *ptr, mwSize N);
extern OCTINTERP_API void mxSetDimensions (mxArray *ptr, const mwSize *dims,
                                           mwSize ndims);

/* Data extractors.  */
extern OCTINTERP_API double *mxGetPi (const mxArray *ptr);
extern OCTINTERP_API double *mxGetPr (const mxArray *ptr);
extern OCTINTERP_API double mxGetScalar (const mxArray *ptr);
extern OCTINTERP_API mxChar *mxGetChars (const mxArray *ptr);
extern OCTINTERP_API mxLogical *mxGetLogicals (const mxArray *ptr);
extern OCTINTERP_API void *mxGetData (const mxArray *ptr);
extern OCTINTERP_API void *mxGetImagData (const mxArray *ptr);

/* Data setters.  */
extern OCTINTERP_API void mxSetPr (mxArray *ptr, double *pr);
extern OCTINTERP_API void mxSetPi (mxArray *ptr, double *pi);
extern OCTINTERP_API void mxSetData (mxArray *ptr, void *data);
extern OCTINTERP_API void mxSetImagData (mxArray *ptr, void *pi);

/* Classes.  */
extern OCTINTERP_API mxClassID mxGetClassID (const mxArray *ptr);
extern OCTINTERP_API const char *mxGetClassName (const mxArray *ptr);

extern OCTINTERP_API void mxSetClassName (mxArray *ptr, const char *name);

/* Cell support.  */
extern OCTINTERP_API mxArray *mxGetCell (const mxArray *ptr, mwIndex idx);

extern OCTINTERP_API void mxSetCell (mxArray *ptr, mwIndex idx, mxArray *val);

/* Sparse support.  */
extern OCTINTERP_API mwIndex *mxGetIr (const mxArray *ptr);
extern OCTINTERP_API mwIndex *mxGetJc (const mxArray *ptr);
extern OCTINTERP_API mwSize mxGetNzmax (const mxArray *ptr);

extern OCTINTERP_API void mxSetIr (mxArray *ptr, mwIndex *ir);
extern OCTINTERP_API void mxSetJc (mxArray *ptr, mwIndex *jc);
extern OCTINTERP_API void mxSetNzmax (mxArray *ptr, mwSize nzmax);

/* Structure support.  */
extern OCTINTERP_API int mxAddField (mxArray *ptr, const char *key);

extern OCTINTERP_API void mxRemoveField (mxArray *ptr, int key_num);

extern OCTINTERP_API mxArray *mxGetField (const mxArray *ptr, mwIndex index,
                                          const char *key);
extern OCTINTERP_API mxArray *mxGetFieldByNumber (const mxArray *ptr,
                                                  mwIndex index, int key_num);

extern OCTINTERP_API void mxSetField (mxArray *ptr, mwIndex index,
                                      const char *key, mxArray *val);
extern OCTINTERP_API void mxSetFieldByNumber (mxArray *ptr, mwIndex index,
                                              int key_num, mxArray *val);

extern OCTINTERP_API int mxGetNumberOfFields (const mxArray *ptr);

extern OCTINTERP_API const char *mxGetFieldNameByNumber (const mxArray *ptr,
                                                         int key_num);
extern OCTINTERP_API int mxGetFieldNumber (const mxArray *ptr, const char *key);

extern OCTINTERP_API int mxGetString (const mxArray *ptr, char *buf,
                                      mwSize buflen);
extern OCTINTERP_API char *mxArrayToString (const mxArray *ptr);

/* Miscellaneous.  */
#ifdef NDEBUG
#define mxAssert(expr, msg) \
  do \
    { \
      if (! expr) \
        { \
          mexPrintf ("Assertion failed: %s, at line %d of file \"%s\".\n%s\n", \
                     #expr, __LINE__, __FILE__, msg); \
        } \
    } \
  while (0)

#define mxAssertS(expr, msg) \
  do \
    { \
      if (! expr) \
        { \
          mexPrintf ("Assertion failed at line %d of file \"%s\".\n%s\n", \
                     __LINE__, __FILE__, msg); \
          abort (); \
        } \
    } \
  while (0)
#else
#define mxAssert(expr, msg)
#define mxAssertS(expr, msg)
#endif

extern OCTINTERP_API mwIndex mxCalcSingleSubscript (const mxArray *ptr,
                                                    mwSize nsubs, mwIndex *subs);

extern OCTINTERP_API size_t mxGetElementSize (const mxArray *ptr);

#if defined (__cplusplus)
}
#endif

#endif
