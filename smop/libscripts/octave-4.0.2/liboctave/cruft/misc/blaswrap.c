/*

Copyright (C) 2012-2015 Jarno Rajahalme

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

Wrapper for Apple libBLAS.dylib and libLAPACK.dylib

At least on the versions of OSX 10.6 so far (up and including 10.6.6)
these libraries are incompatible with 64 bit builds, as some functions
in libBLAS.dylib are not conforming to F2C calling conventions, as
they should.  This breaks them in 64-bit builds on the x86_64
architecture.

Newer gfortran compoilers no longer default to the F2C calling
convention.  These wrappers map the F2C conformant functions in
libBLAS and libLAPACK to the native gfortran calling convention, so
that the libraries can be used with software built for x86_64
architecture.

*/

#ifdef HAVE_CONFIG_H
#include <config.h> /* USE_BLASWRAP ? */
#endif

#ifdef USE_BLASWRAP

/*
 * vecLib is an Apple framework (collection of libraries) containing
 * libBLAS and libLAPACK.  The fortran stubs in these libraries are
 * (mostly, but not completely) in the F2C calling convention.
 * We access the libraries via the vecLib framework to make sure we
 * get the Apple versions, rather than some other blas/lapack with the
 * same name.
 */
#ifndef VECLIB_FILE
#define VECLIB_FILE "/System/Library/Frameworks/vecLib.framework/Versions/A/vecLib"
#endif

/*
 * Since this is a wrapper for fortran functions,
 * we do not have prototypes for them.
 */
#pragma GCC diagnostic ignored "-Wmissing-prototypes"

#include <dlfcn.h>
#include <stdlib.h>

/*
 * Apple LAPACK follows F2C calling convention,
 * Convert to normal gfortran calling convention
 */

static void (*f2c_blas_func[]) (void);   /* forward declaration for wrapper */
static void (*f2c_lapack_func[]) (void); /* forward declaration for wrapper */

/*
 * LAPACK Wrappers, only need to convert the return value from double to float
 */

typedef double (*F2C_CALL_0) (void);
typedef double (*F2C_CALL_1) (void *a1);
typedef double (*F2C_CALL_2) (void *a1, void *a2);
typedef double (*F2C_CALL_3) (void *a1, void *a2, void *a3);
typedef double (*F2C_CALL_4) (void *a1, void *a2, void *a3, void *a4);
typedef double (*F2C_CALL_5) (void *a1, void *a2, void *a3, void *a4, void *a5);
typedef double (*F2C_CALL_6) (void *a1, void *a2, void *a3, void *a4, void *a5,
                              void *a6);
typedef double (*F2C_CALL_7) (void *a1, void *a2, void *a3, void *a4, void *a5,
                              void *a6, void *a7);
typedef double (*F2C_CALL_8) (void *a1, void *a2, void *a3, void *a4, void *a5,
                              void *a6, void *a7, void *a8);

#define F2C_LAPACK_CALL_8(name) \
  float name (void *a1, void *a2, void *a3, void *a4, void *a5, void *a6, void *a7, void *a8) \
  {                                                                     \
    return ((F2C_CALL_8)f2c_lapack_func[f2c_ ## name]) (a1, a2, a3, a4, a5, a6, a7, a8); \
  }

#define F2C_LAPACK_CALL_7(name) \
  float name (void *a1, void *a2, void *a3, void *a4, void *a5, void *a6, void *a7) \
  {                                                                     \
    return ((F2C_CALL_7)f2c_lapack_func[f2c_ ## name]) (a1, a2, a3, a4, a5, a6, a7); \
  }

#define F2C_LAPACK_CALL_6(name) \
  float name (void *a1, void *a2, void *a3, void *a4, void *a5, void *a6) \
  {                                                                     \
    return ((F2C_CALL_6)f2c_lapack_func[f2c_ ## name]) (a1, a2, a3, a4, a5, a6); \
  }

#define F2C_LAPACK_CALL_5(name) \
  float name (void *a1, void *a2, void *a3, void *a4, void *a5)         \
  {                                                                     \
    return ((F2C_CALL_5)f2c_lapack_func[f2c_ ## name]) (a1, a2, a3, a4, a5); \
  }

#define F2C_LAPACK_CALL_4(name) \
  float name (void *a1, void *a2, void *a3, void *a4)                   \
  {                                                                     \
    return ((F2C_CALL_4)f2c_lapack_func[f2c_ ## name]) (a1, a2, a3, a4); \
  }

#define F2C_LAPACK_CALL_3(name) \
  float name (void *a1, void *a2, void *a3)                          \
  {                                                                  \
    return ((F2C_CALL_3)f2c_lapack_func[f2c_ ## name]) (a1, a2, a3); \
  }

#define F2C_LAPACK_CALL_2(name) \
  float name (void *a1, void *a2)                                \
  {                                                              \
    return ((F2C_CALL_2)f2c_lapack_func[f2c_ ## name]) (a1, a2); \
  }

#define F2C_LAPACK_CALL_1(name) \
  float name (void *a1)                                      \
  {                                                          \
    return ((F2C_CALL_1)f2c_lapack_func[f2c_ ## name]) (a1); \
  }

#define F2C_LAPACK_CALL_0(name) \
  float name (void)                                        \
  {                                                        \
    return ((F2C_CALL_0)f2c_lapack_func[f2c_ ## name]) (); \
  }

#define F2C_LAPACK_CALL_NONE(name)

#define F2C_LAPACK_CALL(name, args) F2C_LAPACK_CALL_ ## args (name)

#define ENUM_ITEM(name, args)                   \
  f2c_ ## name,

#define NAME_TO_STRING_CASE(name, args)         \
  case f2c_ ## name: return #name;

#define DEFINE_LAPACK_ENUM(name, list)  \
  typedef enum {                        \
    list(ENUM_ITEM)                     \
  } name;                               \
  static const char*                    \
  f2c_ ## name ## _name (name n) {      \
    switch (n) {                        \
      list(NAME_TO_STRING_CASE)         \
    default: return "";                 \
    }                                   \
  }                                     \
  list(F2C_LAPACK_CALL)

#define DEFINE_BLAS_ENUM(name, list)    \
  typedef enum {                        \
    list(ENUM_ITEM)                     \
  } name;                               \
  static const char*                    \
  f2c_ ## name ## _name(name n) {       \
    switch (n) {                        \
      list(NAME_TO_STRING_CASE)         \
    default: return "";                 \
    }                                   \
  }

/*
 * Lapack functions (with argument count) that need the return value
 * converted from double to float
 */
#define LAPACK_LIST(_)  \
  _(clangb_,7)          \
  _(clange_,6)          \
  _(clangt_,5)          \
  _(clanhb_,7)          \
  _(clanhe_,6)          \
  _(clanhp_,5)          \
  _(clanhs_,5)          \
  _(clanht_,4)          \
  _(clansb_,7)          \
  _(clansp_,5)          \
  _(clansy_,6)          \
  _(clantb_,8)          \
  _(clantp_,6)          \
  _(clantr_,8)          \
  _(scsum1_,3)          \
  _(second_,0)          \
  _(slamc3_,2)          \
  _(slamch_,1)          \
  _(slangb_,7)          \
  _(slange_,6)          \
  _(slangt_,5)          \
  _(slanhs_,5)          \
  _(slansb_,7)          \
  _(slansp_,5)          \
  _(slanst_,4)          \
  _(slansy_,6)          \
  _(slantb_,8)          \
  _(slantp_,6)          \
  _(slantr_,8)          \
  _(slapy2_,2)          \
  _(slapy3_,3)          \
  _(LAPACK_COUNT,NONE)

/*
 * These need a bit more complex wrappers
 */
#define BLAS_LIST(_)    \
  _(cdotu_,6)           \
  _(zdotu_,6)           \
  _(cdotc_,6)           \
  _(zdotc_,6)           \
  _(BLAS_COUNT,NONE)

DEFINE_BLAS_ENUM(blas, BLAS_LIST)

DEFINE_LAPACK_ENUM(lapack, LAPACK_LIST)

/*
 * BLAS wrappers, F2C convention passes retuned complex as an extra first
 * argument
 */
typedef struct { float r, i; } complex;
typedef struct { double r, i; } doublecomplex;

typedef void (*F2C_BLAS_CALL_6) (void *c, void *a1, void *a2, void *a3,
                                 void *a4, void *a5);

#define F2C_BLAS_CALL(type, name) \
type name (void *a1, void *a2, void *a3, void *a4, void *a5) \
{ \
  type cplx; \
  ((F2C_BLAS_CALL_6)f2c_blas_func[f2c_ ## name]) (&cplx, a1, a2, a3, a4, a5); \
  return cplx; \
}

F2C_BLAS_CALL(complex, cdotu_)
F2C_BLAS_CALL(doublecomplex, zdotu_)
F2C_BLAS_CALL(complex, cdotc_)
F2C_BLAS_CALL(doublecomplex, zdotc_)


/*
 * Function pointer arrays, indexed by the enums
 */
static void (*f2c_blas_func[f2c_BLAS_COUNT]) (void) = { 0 };
static void (*f2c_lapack_func[f2c_LAPACK_COUNT]) (void) = { 0 };

/*
 * Initialization: This is called before main ().
 * Get the function pointers to the wrapped functions in Apple vecLib
 */

static void * apple_vecLib = 0;

__attribute__((constructor))
static void initVecLibWrappers (void)
{
  apple_vecLib = dlopen (VECLIB_FILE, RTLD_LOCAL | RTLD_NOLOAD | RTLD_FIRST);
  if (0 == apple_vecLib)
    abort ();

  int i;
  for (i = 0; i < f2c_LAPACK_COUNT; i++)
    if (0 == (f2c_lapack_func[i] = dlsym (apple_vecLib, f2c_lapack_name(i))))
      abort ();
  for (i = 0; i < f2c_BLAS_COUNT; i++)
    if (0 == (f2c_blas_func[i] = dlsym (apple_vecLib, f2c_blas_name(i))))
      abort ();
}

__attribute__((destructor))
static void finiVecLibWrappers (void)
{
  if (apple_vecLib)
    dlclose (apple_vecLib);
  apple_vecLib = 0;
}

#endif /* USE_BLASWRAP */
