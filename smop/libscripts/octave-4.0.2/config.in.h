/* config.in.h.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
#undef AC_APPLE_UNIVERSAL_BUILD

/* Define to the number of bits in type 'ptrdiff_t'. */
#undef BITSIZEOF_PTRDIFF_T

/* Define to the number of bits in type 'sig_atomic_t'. */
#undef BITSIZEOF_SIG_ATOMIC_T

/* Define to the number of bits in type 'size_t'. */
#undef BITSIZEOF_SIZE_T

/* Define to the number of bits in type 'wchar_t'. */
#undef BITSIZEOF_WCHAR_T

/* Define to the number of bits in type 'wint_t'. */
#undef BITSIZEOF_WINT_T

/* Define to 1 to use internal bounds checking. */
#undef BOUNDS_CHECKING

/* Define to 1 if llvm::CallInst:addAttribute arg type is llvm::Attributes. */
#undef CALLINST_ADDATTRIBUTE_ARG_IS_ATTRIBUTES

/* Define to 1 if the `closedir' function returns void instead of `int'. */
#undef CLOSEDIR_VOID

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
#undef CRAY_STACKSEG_END

/* Define to the legacy option name if using an older version of cURL. */
#undef CURLOPT_DIRLISTONLY

/* Define to 1 if C++ reinterpret_cast fails for function pointers. */
#undef CXX_BROKEN_REINTERPRET_CAST

/* Define to 1 if your C++ runtime library is ISO compliant. */
#undef CXX_ISO_COMPLIANT_LIBRARY

/* Define to 1 if your compiler supports `<>' stuff for template friends. */
#undef CXX_NEW_FRIEND_TEMPLATE_DECL

/* Define to 1 if using `alloca.c'. */
#undef C_ALLOCA

/* Define as the bit index in the word where to find bit 0 of the exponent of
   'double'. */
#undef DBL_EXPBIT0_BIT

/* Define as the word index where to find the exponent of 'double'. */
#undef DBL_EXPBIT0_WORD

/* Define as the bit index in the word where to find the sign of 'double'. */
#undef DBL_SIGNBIT_BIT

/* Define as the word index where to find the sign of 'double'. */
#undef DBL_SIGNBIT_WORD

/* the name of the file descriptor member of DIR */
#undef DIR_FD_MEMBER_NAME

#ifdef DIR_FD_MEMBER_NAME
# define DIR_TO_FD(Dir_p) ((Dir_p)->DIR_FD_MEMBER_NAME)
#else
# define DIR_TO_FD(Dir_p) -1
#endif


/* Define to 1 if // is a file system root distinct from /. */
#undef DOUBLE_SLASH_IS_DISTINCT_ROOT

/* Define if struct dirent has a member d_ino that actually works. */
#undef D_INO_IN_DIRENT

/* Define to 1 if using dynamic linking. */
#undef ENABLE_DYNAMIC_LINKING

/* Define to 1 if math.h declares struct exception for matherr(). */
#undef EXCEPTION_IN_MATH

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
#undef F77_DUMMY_MAIN

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#undef F77_FUNC

/* As F77_FUNC, but for C identifiers containing underscores. */
#undef F77_FUNC_

/* Define this to 1 if F_DUPFD behavior does not match POSIX */
#undef FCNTL_DUPFD_BUGGY

/* Define if F77 and FC dummy `main' functions are identical. */
#undef FC_DUMMY_MAIN_EQ_F77

/* Define to volatile if you need to truncate intermediate FP results. */
#undef FLOAT_TRUNCATE

/* Define as the bit index in the word where to find bit 0 of the exponent of
   'float'. */
#undef FLT_EXPBIT0_BIT

/* Define as the word index where to find the exponent of 'float'. */
#undef FLT_EXPBIT0_WORD

/* Define as the bit index in the word where to find the sign of 'float'. */
#undef FLT_SIGNBIT_BIT

/* Define as the word index where to find the sign of 'float'. */
#undef FLT_SIGNBIT_WORD

/* Define to 1 if fopen() fails to recognize a trailing slash. */
#undef FOPEN_TRAILING_SLASH_BUG

/* Define to 1 if the system's ftello function has the Solaris bug. */
#undef FTELLO_BROKEN_AFTER_SWITCHING_FROM_READ_TO_WRITE

/* Define to 1 if llvm::Function:addAttribute arg type is llvm::Attributes. */
#undef FUNCTION_ADDATTRIBUTE_ARG_IS_ATTRIBUTES

/* Define to 1 if llvm::Function:addFnAttr arg type is llvm::Attributes. */
#undef FUNCTION_ADDFNATTR_ARG_IS_ATTRIBUTES

/* Define to 1 if fflush is known to work on stdin as per POSIX.1-2008, 0 if
   fflush is known to not work, -1 if unknown. */
#undef FUNC_FFLUSH_STDIN

/* Define to 1 if mkdir mistakenly creates a directory given with a trailing
   dot component. */
#undef FUNC_MKDIR_DOT_BUG

/* Define to 1 if realpath() can malloc memory, always gives an absolute path,
   and handles trailing slash correctly. */
#undef FUNC_REALPATH_WORKS

/* Define if gettimeofday clobbers the localtime buffer. */
#undef GETTIMEOFDAY_CLOBBERS_LOCALTIME

/* Define this to 'void' or 'struct timezone' to match the system's
   declaration of the second argument to gettimeofday. */
#undef GETTIMEOFDAY_TIMEZONE

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module canonicalize shall be considered present. */
#undef GNULIB_CANONICALIZE

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module canonicalize-lgpl shall be considered present. */
#undef GNULIB_CANONICALIZE_LGPL

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module dirname shall be considered present. */
#undef GNULIB_DIRNAME

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module fdopendir shall be considered present. */
#undef GNULIB_FDOPENDIR

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module fflush shall be considered present. */
#undef GNULIB_FFLUSH

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module fscanf shall be considered present. */
#undef GNULIB_FSCANF

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module getcwd shall be considered present. */
#undef GNULIB_GETCWD

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module malloc-gnu shall be considered present. */
#undef GNULIB_MALLOC_GNU

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module mkostemp shall be considered present. */
#undef GNULIB_MKOSTEMP

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module openat shall be considered present. */
#undef GNULIB_OPENAT

/* Define to 1 if printf and friends should be labeled with attribute
   "__gnu_printf__" instead of "__printf__" */
#undef GNULIB_PRINTF_ATTRIBUTE_FLAVOR_GNU

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module realloc-gnu shall be considered present. */
#undef GNULIB_REALLOC_GNU

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module scanf shall be considered present. */
#undef GNULIB_SCANF

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module strerror shall be considered present. */
#undef GNULIB_STRERROR

/* Define to 1 when the gnulib module canonicalize should be tested. */
#undef GNULIB_TEST_CANONICALIZE

/* Define to 1 when the gnulib module canonicalize_file_name should be tested.
   */
#undef GNULIB_TEST_CANONICALIZE_FILE_NAME

/* Define to 1 when the gnulib module chdir should be tested. */
#undef GNULIB_TEST_CHDIR

/* Define to 1 when the gnulib module cloexec should be tested. */
#undef GNULIB_TEST_CLOEXEC

/* Define to 1 when the gnulib module close should be tested. */
#undef GNULIB_TEST_CLOSE

/* Define to 1 when the gnulib module closedir should be tested. */
#undef GNULIB_TEST_CLOSEDIR

/* Define to 1 when the gnulib module copysign should be tested. */
#undef GNULIB_TEST_COPYSIGN

/* Define to 1 when the gnulib module copysignf should be tested. */
#undef GNULIB_TEST_COPYSIGNF

/* Define to 1 when the gnulib module dirfd should be tested. */
#undef GNULIB_TEST_DIRFD

/* Define to 1 when the gnulib module dup should be tested. */
#undef GNULIB_TEST_DUP

/* Define to 1 when the gnulib module dup2 should be tested. */
#undef GNULIB_TEST_DUP2

/* Define to 1 when the gnulib module environ should be tested. */
#undef GNULIB_TEST_ENVIRON

/* Define to 1 when the gnulib module fchdir should be tested. */
#undef GNULIB_TEST_FCHDIR

/* Define to 1 when the gnulib module fclose should be tested. */
#undef GNULIB_TEST_FCLOSE

/* Define to 1 when the gnulib module fcntl should be tested. */
#undef GNULIB_TEST_FCNTL

/* Define to 1 when the gnulib module fdopendir should be tested. */
#undef GNULIB_TEST_FDOPENDIR

/* Define to 1 when the gnulib module fflush should be tested. */
#undef GNULIB_TEST_FFLUSH

/* Define to 1 when the gnulib module floor should be tested. */
#undef GNULIB_TEST_FLOOR

/* Define to 1 when the gnulib module floorf should be tested. */
#undef GNULIB_TEST_FLOORF

/* Define to 1 when the gnulib module fopen should be tested. */
#undef GNULIB_TEST_FOPEN

/* Define to 1 when the gnulib module fpurge should be tested. */
#undef GNULIB_TEST_FPURGE

/* Define to 1 when the gnulib module frexp should be tested. */
#undef GNULIB_TEST_FREXP

/* Define to 1 when the gnulib module frexpf should be tested. */
#undef GNULIB_TEST_FREXPF

/* Define to 1 when the gnulib module fseek should be tested. */
#undef GNULIB_TEST_FSEEK

/* Define to 1 when the gnulib module fseeko should be tested. */
#undef GNULIB_TEST_FSEEKO

/* Define to 1 when the gnulib module fstat should be tested. */
#undef GNULIB_TEST_FSTAT

/* Define to 1 when the gnulib module fstatat should be tested. */
#undef GNULIB_TEST_FSTATAT

/* Define to 1 when the gnulib module ftell should be tested. */
#undef GNULIB_TEST_FTELL

/* Define to 1 when the gnulib module ftello should be tested. */
#undef GNULIB_TEST_FTELLO

/* Define to 1 when the gnulib module getcwd should be tested. */
#undef GNULIB_TEST_GETCWD

/* Define to 1 when the gnulib module getdtablesize should be tested. */
#undef GNULIB_TEST_GETDTABLESIZE

/* Define to 1 when the gnulib module gethostname should be tested. */
#undef GNULIB_TEST_GETHOSTNAME

/* Define to 1 when the gnulib module getlogin_r should be tested. */
#undef GNULIB_TEST_GETLOGIN_R

/* Define to 1 when the gnulib module getopt-gnu should be tested. */
#undef GNULIB_TEST_GETOPT_GNU

/* Define to 1 when the gnulib module gettimeofday should be tested. */
#undef GNULIB_TEST_GETTIMEOFDAY

/* Define to 1 when the gnulib module isatty should be tested. */
#undef GNULIB_TEST_ISATTY

/* Define to 1 when the gnulib module isnand should be tested. */
#undef GNULIB_TEST_ISNAND

/* Define to 1 when the gnulib module isnanf should be tested. */
#undef GNULIB_TEST_ISNANF

/* Define to 1 when the gnulib module link should be tested. */
#undef GNULIB_TEST_LINK

/* Define to 1 when the gnulib module log should be tested. */
#undef GNULIB_TEST_LOG

/* Define to 1 when the gnulib module log2 should be tested. */
#undef GNULIB_TEST_LOG2

/* Define to 1 when the gnulib module log2f should be tested. */
#undef GNULIB_TEST_LOG2F

/* Define to 1 when the gnulib module logf should be tested. */
#undef GNULIB_TEST_LOGF

/* Define to 1 when the gnulib module lseek should be tested. */
#undef GNULIB_TEST_LSEEK

/* Define to 1 when the gnulib module lstat should be tested. */
#undef GNULIB_TEST_LSTAT

/* Define to 1 when the gnulib module malloc-posix should be tested. */
#undef GNULIB_TEST_MALLOC_POSIX

/* Define to 1 when the gnulib module mbrtowc should be tested. */
#undef GNULIB_TEST_MBRTOWC

/* Define to 1 when the gnulib module mbsinit should be tested. */
#undef GNULIB_TEST_MBSINIT

/* Define to 1 when the gnulib module mbsrtowcs should be tested. */
#undef GNULIB_TEST_MBSRTOWCS

/* Define to 1 when the gnulib module memchr should be tested. */
#undef GNULIB_TEST_MEMCHR

/* Define to 1 when the gnulib module mempcpy should be tested. */
#undef GNULIB_TEST_MEMPCPY

/* Define to 1 when the gnulib module memrchr should be tested. */
#undef GNULIB_TEST_MEMRCHR

/* Define to 1 when the gnulib module mkfifo should be tested. */
#undef GNULIB_TEST_MKFIFO

/* Define to 1 when the gnulib module mkostemp should be tested. */
#undef GNULIB_TEST_MKOSTEMP

/* Define to 1 when the gnulib module mktime should be tested. */
#undef GNULIB_TEST_MKTIME

/* Define to 1 when the gnulib module nanosleep should be tested. */
#undef GNULIB_TEST_NANOSLEEP

/* Define to 1 when the gnulib module open should be tested. */
#undef GNULIB_TEST_OPEN

/* Define to 1 when the gnulib module openat should be tested. */
#undef GNULIB_TEST_OPENAT

/* Define to 1 when the gnulib module opendir should be tested. */
#undef GNULIB_TEST_OPENDIR

/* Define to 1 when the gnulib module pipe should be tested. */
#undef GNULIB_TEST_PIPE

/* Define to 1 when the gnulib module putenv should be tested. */
#undef GNULIB_TEST_PUTENV

/* Define to 1 when the gnulib module raise should be tested. */
#undef GNULIB_TEST_RAISE

/* Define to 1 when the gnulib module readdir should be tested. */
#undef GNULIB_TEST_READDIR

/* Define to 1 when the gnulib module readlink should be tested. */
#undef GNULIB_TEST_READLINK

/* Define to 1 when the gnulib module realloc-posix should be tested. */
#undef GNULIB_TEST_REALLOC_POSIX

/* Define to 1 when the gnulib module realpath should be tested. */
#undef GNULIB_TEST_REALPATH

/* Define to 1 when the gnulib module rename should be tested. */
#undef GNULIB_TEST_RENAME

/* Define to 1 when the gnulib module rewinddir should be tested. */
#undef GNULIB_TEST_REWINDDIR

/* Define to 1 when the gnulib module rmdir should be tested. */
#undef GNULIB_TEST_RMDIR

/* Define to 1 when the gnulib module round should be tested. */
#undef GNULIB_TEST_ROUND

/* Define to 1 when the gnulib module roundf should be tested. */
#undef GNULIB_TEST_ROUNDF

/* Define to 1 when the gnulib module secure_getenv should be tested. */
#undef GNULIB_TEST_SECURE_GETENV

/* Define to 1 when the gnulib module select should be tested. */
#undef GNULIB_TEST_SELECT

/* Define to 1 when the gnulib module sigaction should be tested. */
#undef GNULIB_TEST_SIGACTION

/* Define to 1 when the gnulib module signbit should be tested. */
#undef GNULIB_TEST_SIGNBIT

/* Define to 1 when the gnulib module sigprocmask should be tested. */
#undef GNULIB_TEST_SIGPROCMASK

/* Define to 1 when the gnulib module sleep should be tested. */
#undef GNULIB_TEST_SLEEP

/* Define to 1 when the gnulib module stat should be tested. */
#undef GNULIB_TEST_STAT

/* Define to 1 when the gnulib module strdup should be tested. */
#undef GNULIB_TEST_STRDUP

/* Define to 1 when the gnulib module strerror should be tested. */
#undef GNULIB_TEST_STRERROR

/* Define to 1 when the gnulib module strndup should be tested. */
#undef GNULIB_TEST_STRNDUP

/* Define to 1 when the gnulib module strnlen should be tested. */
#undef GNULIB_TEST_STRNLEN

/* Define to 1 when the gnulib module strptime should be tested. */
#undef GNULIB_TEST_STRPTIME

/* Define to 1 when the gnulib module symlink should be tested. */
#undef GNULIB_TEST_SYMLINK

/* Define to 1 when the gnulib module time_r should be tested. */
#undef GNULIB_TEST_TIME_R

/* Define to 1 when the gnulib module tmpfile should be tested. */
#undef GNULIB_TEST_TMPFILE

/* Define to 1 when the gnulib module trunc should be tested. */
#undef GNULIB_TEST_TRUNC

/* Define to 1 when the gnulib module truncf should be tested. */
#undef GNULIB_TEST_TRUNCF

/* Define to 1 when the gnulib module unlink should be tested. */
#undef GNULIB_TEST_UNLINK

/* Define to 1 when the gnulib module unsetenv should be tested. */
#undef GNULIB_TEST_UNSETENV

/* Define to 1 when the gnulib module vasprintf should be tested. */
#undef GNULIB_TEST_VASPRINTF

/* Define to 1 if you have the `acosh' function. */
#undef HAVE_ACOSH

/* Define to 1 if you have the `acoshf' function. */
#undef HAVE_ACOSHF

/* Define to 1 if you have 'alloca' after including <alloca.h>, a header that
   may be supplied by this distribution. */
#undef HAVE_ALLOCA

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#undef HAVE_ALLOCA_H

/* Define to 1 if AMD is available. */
#undef HAVE_AMD

/* Define to 1 if you have the <amd/amd.h> header file. */
#undef HAVE_AMD_AMD_H

/* Define to 1 if you have the <amd.h> header file. */
#undef HAVE_AMD_H

/* Define to 1 if ARPACK is available. */
#undef HAVE_ARPACK

/* Define to 1 if you have the `asinh' function. */
#undef HAVE_ASINH

/* Define to 1 if you have the `asinhf' function. */
#undef HAVE_ASINHF

/* Define to 1 if you have the `atanh' function. */
#undef HAVE_ATANH

/* Define to 1 if you have the `atanhf' function. */
#undef HAVE_ATANHF

/* Define if you have a BLAS library. */
#undef HAVE_BLAS

/* Define to 1 if you have the <bp-sym.h> header file. */
#undef HAVE_BP_SYM_H

/* Define to 1 if you have the `btowc' function. */
#undef HAVE_BTOWC

/* Define to 1 if nanosleep mishandles large arguments. */
#undef HAVE_BUG_BIG_NANOSLEEP

/* Define to 1 if CAMD is available. */
#undef HAVE_CAMD

/* Define to 1 if you have the <camd/camd.h> header file. */
#undef HAVE_CAMD_CAMD_H

/* Define to 1 if you have the <camd.h> header file. */
#undef HAVE_CAMD_H

/* Define to 1 if you have the `canonicalize_file_name' function. */
#undef HAVE_CANONICALIZE_FILE_NAME

/* Define to 1 if Carbon Framework has CGDisplayBitsPerPixel. */
#undef HAVE_CARBON_CGDISPLAYBITSPERPIXEL

/* Define to 1 if you have the `cbrt' function. */
#undef HAVE_CBRT

/* Define to 1 if you have the `cbrtf' function. */
#undef HAVE_CBRTF

/* Define to 1 if CCOLAMD is available. */
#undef HAVE_CCOLAMD

/* Define to 1 if you have the <ccolamd/ccolamd.h> header file. */
#undef HAVE_CCOLAMD_CCOLAMD_H

/* Define to 1 if you have the <ccolamd.h> header file. */
#undef HAVE_CCOLAMD_H

/* Define to 1 if you have the `chmod' function. */
#undef HAVE_CHMOD

/* Define to 1 if CHOLMOD is available. */
#undef HAVE_CHOLMOD

/* Define to 1 if you have the <cholmod/cholmod.h> header file. */
#undef HAVE_CHOLMOD_CHOLMOD_H

/* Define to 1 if you have the <cholmod.h> header file. */
#undef HAVE_CHOLMOD_H

/* Define to 1 if you have the `chown' function. */
#undef HAVE_CHOWN

/* Define to 1 if you have the `closedir' function. */
#undef HAVE_CLOSEDIR

/* Define to 1 if <cmath> provides isfinite. */
#undef HAVE_CMATH_ISFINITE

/* Define to 1 if <cmath> provides float variant of isfinite. */
#undef HAVE_CMATH_ISFINITEF

/* Define to 1 if <cmath> provides isinf. */
#undef HAVE_CMATH_ISINF

/* Define to 1 if <cmath> provides float variant of isinf. */
#undef HAVE_CMATH_ISINFF

/* Define to 1 if <cmath> provides isnan. */
#undef HAVE_CMATH_ISNAN

/* Define to 1 if <cmath> provides float variant of isnan. */
#undef HAVE_CMATH_ISNANF

/* Define to 1 if <cmath> provides signbit. */
#undef HAVE_CMATH_SIGNBIT

/* Define to 1 if <cmath> provides float variant of signbit. */
#undef HAVE_CMATH_SIGNBITF

/* Define to 1 if COLAMD is available. */
#undef HAVE_COLAMD

/* Define to 1 if you have the <colamd/colamd.h> header file. */
#undef HAVE_COLAMD_COLAMD_H

/* Define to 1 if you have the <colamd.h> header file. */
#undef HAVE_COLAMD_H

/* Define to 1 if you have the <conio.h> header file. */
#undef HAVE_CONIO_H

/* Define if the copysignf function is declared in <math.h> and available in
   libc. */
#undef HAVE_COPYSIGNF_IN_LIBC

/* Define if the copysignl function is declared in <math.h> and available in
   libc. */
#undef HAVE_COPYSIGNL_IN_LIBC

/* Define if the copysign function is declared in <math.h> and available in
   libc. */
#undef HAVE_COPYSIGN_IN_LIBC

/* Define to 1 if you have the <cs.h> header file. */
#undef HAVE_CS_H

/* Define to 1 if you have the `ctermid' function. */
#undef HAVE_CTERMID

/* Define to 1 if cURL is available. */
#undef HAVE_CURL

/* Define to 1 if you have the <curl/curl.h> header file. */
#undef HAVE_CURL_CURL_H

/* Define to 1 if you have the <curses.h> header file. */
#undef HAVE_CURSES_H

/* Define to 1 if CXSparse is available. */
#undef HAVE_CXSPARSE

/* Define to 1 if you have the <cxsparse/cs.h> header file. */
#undef HAVE_CXSPARSE_CS_H

/* Define to 1 if C++ library has templated bitwise operators. */
#undef HAVE_CXX_BITWISE_OP_TEMPLATES

/* Define to 1 if C++ complex class has T& real (void) and T& imag (void)
   methods. */
#undef HAVE_CXX_COMPLEX_REFERENCE_ACCESSORS

/* Define to 1 if C++ complex class has void real (T) and void imag (T)
   methods. */
#undef HAVE_CXX_COMPLEX_SETTERS

/* Define to 1 if you have the declaration of `alarm', and to 0 if you don't.
   */
#undef HAVE_DECL_ALARM

/* Define to 1 if you have the declaration of `ceilf', and to 0 if you don't.
   */
#undef HAVE_DECL_CEILF

/* Define to 1 if you have the declaration of `copysign', and to 0 if you
   don't. */
#undef HAVE_DECL_COPYSIGN

/* Define to 1 if you have the declaration of `copysignf', and to 0 if you
   don't. */
#undef HAVE_DECL_COPYSIGNF

/* Define to 1 if you have the declaration of `copysignl', and to 0 if you
   don't. */
#undef HAVE_DECL_COPYSIGNL

/* Define to 1 if you have the declaration of `dirfd', and to 0 if you don't.
   */
#undef HAVE_DECL_DIRFD

/* Define to 1 if you have the declaration of `exp2', and to 0 if you don't.
   */
#undef HAVE_DECL_EXP2

/* Define to 1 if you have the declaration of `fchdir', and to 0 if you don't.
   */
#undef HAVE_DECL_FCHDIR

/* Define to 1 if you have the declaration of `fdopendir', and to 0 if you
   don't. */
#undef HAVE_DECL_FDOPENDIR

/* Define to 1 if you have the declaration of `floorf', and to 0 if you don't.
   */
#undef HAVE_DECL_FLOORF

/* Define to 1 if you have the declaration of `fpurge', and to 0 if you don't.
   */
#undef HAVE_DECL_FPURGE

/* Define to 1 if you have the declaration of `fseeko', and to 0 if you don't.
   */
#undef HAVE_DECL_FSEEKO

/* Define to 1 if you have the declaration of `ftello', and to 0 if you don't.
   */
#undef HAVE_DECL_FTELLO

/* Define to 1 if you have the declaration of `getcwd', and to 0 if you don't.
   */
#undef HAVE_DECL_GETCWD

/* Define to 1 if you have the declaration of `getc_unlocked', and to 0 if you
   don't. */
#undef HAVE_DECL_GETC_UNLOCKED

/* Define to 1 if you have the declaration of `getdtablesize', and to 0 if you
   don't. */
#undef HAVE_DECL_GETDTABLESIZE

/* Define to 1 if you have the declaration of `getenv', and to 0 if you don't.
   */
#undef HAVE_DECL_GETENV

/* Define to 1 if you have the declaration of `getlogin', and to 0 if you
   don't. */
#undef HAVE_DECL_GETLOGIN

/* Define to 1 if you have the declaration of `getlogin_r', and to 0 if you
   don't. */
#undef HAVE_DECL_GETLOGIN_R

/* Define to 1 if you have the declaration of `isblank', and to 0 if you
   don't. */
#undef HAVE_DECL_ISBLANK

/* Define to 1 if you have the declaration of `localtime_r', and to 0 if you
   don't. */
#undef HAVE_DECL_LOCALTIME_R

/* Define to 1 if you have the declaration of `mbrtowc', and to 0 if you
   don't. */
#undef HAVE_DECL_MBRTOWC

/* Define to 1 if you have the declaration of `mbsinit', and to 0 if you
   don't. */
#undef HAVE_DECL_MBSINIT

/* Define to 1 if you have the declaration of `mbsrtowcs', and to 0 if you
   don't. */
#undef HAVE_DECL_MBSRTOWCS

/* Define to 1 if you have the declaration of `memrchr', and to 0 if you
   don't. */
#undef HAVE_DECL_MEMRCHR

/* Define to 1 if you have the declaration of `program_invocation_name', and
   to 0 if you don't. */
#undef HAVE_DECL_PROGRAM_INVOCATION_NAME

/* Define to 1 if you have the declaration of `program_invocation_short_name',
   and to 0 if you don't. */
#undef HAVE_DECL_PROGRAM_INVOCATION_SHORT_NAME

/* Define to 1 if you have the declaration of `round', and to 0 if you don't.
   */
#undef HAVE_DECL_ROUND

/* Define to 1 if you have the declaration of `roundf', and to 0 if you don't.
   */
#undef HAVE_DECL_ROUNDF

/* Define to 1 if you have the declaration of `signbit', and to 0 if you
   don't. */
#undef HAVE_DECL_SIGNBIT

/* Define to 1 if you have the declaration of `sleep', and to 0 if you don't.
   */
#undef HAVE_DECL_SLEEP

/* Define to 1 if you have the declaration of `strdup', and to 0 if you don't.
   */
#undef HAVE_DECL_STRDUP

/* Define to 1 if you have the declaration of `strerror_r', and to 0 if you
   don't. */
#undef HAVE_DECL_STRERROR_R

/* Define to 1 if you have the declaration of `strmode', and to 0 if you
   don't. */
#undef HAVE_DECL_STRMODE

/* Define to 1 if you have the declaration of `strncasecmp', and to 0 if you
   don't. */
#undef HAVE_DECL_STRNCASECMP

/* Define to 1 if you have the declaration of `strndup', and to 0 if you
   don't. */
#undef HAVE_DECL_STRNDUP

/* Define to 1 if you have the declaration of `strnlen', and to 0 if you
   don't. */
#undef HAVE_DECL_STRNLEN

/* Define to 1 if you have the declaration of `sys_siglist', and to 0 if you
   don't. */
#undef HAVE_DECL_SYS_SIGLIST

/* Define to 1 if you have the declaration of `tgamma', and to 0 if you don't.
   */
#undef HAVE_DECL_TGAMMA

/* Define to 1 if you have the declaration of `towlower', and to 0 if you
   don't. */
#undef HAVE_DECL_TOWLOWER

/* Define to 1 if you have the declaration of `trunc', and to 0 if you don't.
   */
#undef HAVE_DECL_TRUNC

/* Define to 1 if you have the declaration of `truncf', and to 0 if you don't.
   */
#undef HAVE_DECL_TRUNCF

/* Define to 1 if you have the declaration of `tzname', and to 0 if you don't.
   */
#undef HAVE_DECL_TZNAME

/* Define to 1 if you have the declaration of `unsetenv', and to 0 if you
   don't. */
#undef HAVE_DECL_UNSETENV

/* Define to 1 if you have the declaration of `_putenv', and to 0 if you
   don't. */
#undef HAVE_DECL__PUTENV

/* Define to 1 if you have the declaration of `_snprintf', and to 0 if you
   don't. */
#undef HAVE_DECL__SNPRINTF

/* Define to 1 if the system has the type `dev_t'. */
#undef HAVE_DEV_T

/* Define to 1 if you have the <direct.h> header file. */
#undef HAVE_DIRECT_H

/* Define to 1 if you have the <dirent.h> header file. */
#undef HAVE_DIRENT_H

/* Define to 1 if you have the `dirfd' function. */
#undef HAVE_DIRFD

/* Define to 1 if you have the <dlfcn.h> header file. */
#undef HAVE_DLFCN_H

/* Define to 1 if your system has dlopen, dlsym, dlerror, and dlclose for
   dynamic linking. */
#undef HAVE_DLOPEN_API

/* Define to 1 if you have the `dup2' function. */
#undef HAVE_DUP2

/* Define to 1 if your system has dyld for dynamic linking. */
#undef HAVE_DYLD_API

/* Define to 1 if C++ supports dynamic auto arrays. */
#undef HAVE_DYNAMIC_AUTO_ARRAYS

/* Define to 1 if you have the `endgrent' function. */
#undef HAVE_ENDGRENT

/* Define to 1 if you have the `endpwent' function. */
#undef HAVE_ENDPWENT

/* Define if you have the declaration of environ. */
#undef HAVE_ENVIRON_DECL

/* Define to 1 if you have the `erf' function. */
#undef HAVE_ERF

/* Define to 1 if you have the `erfc' function. */
#undef HAVE_ERFC

/* Define to 1 if you have the `erfcf' function. */
#undef HAVE_ERFCF

/* Define to 1 if you have the `erff' function. */
#undef HAVE_ERFF

/* Define to 1 if you have the `execvp' function. */
#undef HAVE_EXECVP

/* Define to 1 if you have the `exp2' function. */
#undef HAVE_EXP2

/* Define to 1 if you have the `exp2f' function. */
#undef HAVE_EXP2F

/* Define to 1 if you have the `expm1' function. */
#undef HAVE_EXPM1

/* Define to 1 if you have the `expm1f' function. */
#undef HAVE_EXPM1F

/* Define to 1 if signed integers use two's complement. */
#undef HAVE_FAST_INT_OPS

/* Define to 1 if you have the `fchdir' function. */
#undef HAVE_FCHDIR

/* Define to 1 if you have the `fcntl' function. */
#undef HAVE_FCNTL

/* Define to 1 if you have the `fdopendir' function. */
#undef HAVE_FDOPENDIR

/* Define to 1 if you have the <features.h> header file. */
#undef HAVE_FEATURES_H

/* Define if you have both FFTW3 and FFTW3F libraries. */
#undef HAVE_FFTW

/* Define to 1 if FFTW3 is available. */
#undef HAVE_FFTW3

/* Define to 1 if FFTW3F is available. */
#undef HAVE_FFTW3F

/* Define to 1 if FFTW3F has multi-threading support. */
#undef HAVE_FFTW3F_THREADS

/* Define to 1 if you have the <fftw3.h> header file. */
#undef HAVE_FFTW3_H

/* Define to 1 if FFTW3 has multi-threading support. */
#undef HAVE_FFTW3_THREADS

/* Define to 1 if you have the `finite' function. */
#undef HAVE_FINITE

/* Define to 1 if you have the <floatingpoint.h> header file. */
#undef HAVE_FLOATINGPOINT_H

/* Define if the both the floorf() and ceilf() functions exist. */
#undef HAVE_FLOORF_AND_CEILF

/* Define to 1 if FLTK is available. */
#undef HAVE_FLTK

/* Define to 1 if you have the <fnmatch.h> header file. */
#undef HAVE_FNMATCH_H

/* Define to 1 if fontconfig is present. */
#undef HAVE_FONTCONFIG

/* Define to 1 if you have the `fork' function. */
#undef HAVE_FORK

/* Define to 1 if you have the `fpurge' function. */
#undef HAVE_FPURGE

/* Define to 1 if you have the <fpu_control.h> header file. */
#undef HAVE_FPU_CONTROL_H

/* Define to 1 if framework CARBON is available. */
#undef HAVE_FRAMEWORK_CARBON

/* Define to 1 if framework OPENGL is available. */
#undef HAVE_FRAMEWORK_OPENGL

/* Define to 1 if you have Freetype library. */
#undef HAVE_FREETYPE

/* Define if the frexp() function is available and works. */
#undef HAVE_FREXP

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
#undef HAVE_FSEEKO

/* Define to 1 if you have the `fstatat' function. */
#undef HAVE_FSTATAT

/* Define to 1 if you have the `ftruncate' function. */
#undef HAVE_FTRUNCATE

/* Define to 1 if you have the `FT_Reference_Face' function. */
#undef HAVE_FT_REFERENCE_FACE

/* Define to 1 if you have the `getcwd' function. */
#undef HAVE_GETCWD

/* Define to 1 if getcwd works, but with shorter paths than is generally
   tested with the replacement. */
#undef HAVE_GETCWD_SHORTER

/* Define to 1 if you have the `getdtablesize' function. */
#undef HAVE_GETDTABLESIZE

/* Define to 1 if you have the `getegid' function. */
#undef HAVE_GETEGID

/* Define to 1 if you have the `geteuid' function. */
#undef HAVE_GETEUID

/* Define to 1 if you have the `getgid' function. */
#undef HAVE_GETGID

/* Define to 1 if you have the `getgrent' function. */
#undef HAVE_GETGRENT

/* Define to 1 if you have the `getgrgid' function. */
#undef HAVE_GETGRGID

/* Define to 1 if you have the `getgrnam' function. */
#undef HAVE_GETGRNAM

/* Define to 1 if you have the `gethostname' function. */
#undef HAVE_GETHOSTNAME

/* Define to 1 if you have the `getlogin_r' function. */
#undef HAVE_GETLOGIN_R

/* Define to 1 if you have the <getopt.h> header file. */
#undef HAVE_GETOPT_H

/* Define to 1 if you have the `getopt_long_only' function. */
#undef HAVE_GETOPT_LONG_ONLY

/* Define to 1 if you have the `getpagesize' function. */
#undef HAVE_GETPAGESIZE

/* Define to 1 if you have the `getpgrp' function. */
#undef HAVE_GETPGRP

/* Define to 1 if you have the `getpid' function. */
#undef HAVE_GETPID

/* Define to 1 if you have the `getppid' function. */
#undef HAVE_GETPPID

/* Define to 1 if you have the `getpwent' function. */
#undef HAVE_GETPWENT

/* Define to 1 if you have the `getpwnam' function. */
#undef HAVE_GETPWNAM

/* Define to 1 if you have the `getpwnam_r' function. */
#undef HAVE_GETPWNAM_R

/* Define to 1 if you have the `getpwuid' function. */
#undef HAVE_GETPWUID

/* Define to 1 if you have the `getrusage' function. */
#undef HAVE_GETRUSAGE

/* Define to 1 if you have the `gettimeofday' function. */
#undef HAVE_GETTIMEOFDAY

/* Define to 1 if you have the `getuid' function. */
#undef HAVE_GETUID

/* Define to 1 if you have the <gl2ps.h> header file. */
#undef HAVE_GL2PS_H

/* Define to 1 if you have the <glob.h> header file. */
#undef HAVE_GLOB_H

/* Define to 1 if GLPK is available. */
#undef HAVE_GLPK

/* Define to 1 if you have the <glpk/glpk.h> header file. */
#undef HAVE_GLPK_GLPK_H

/* Define to 1 if you have the <glpk.h> header file. */
#undef HAVE_GLPK_H

/* Define to 1 if gluTessCallback is called with (...). */
#undef HAVE_GLUTESSCALLBACK_THREEDOTS

/* Define to 1 if you have the <GL/glext.h> header file. */
#undef HAVE_GL_GLEXT_H

/* Define to 1 if you have the <GL/glu.h> header file. */
#undef HAVE_GL_GLU_H

/* Define to 1 if you have the <GL/gl.h> header file. */
#undef HAVE_GL_GL_H

/* Define to 1 if you have the <GL/osmesa.h> header file. */
#undef HAVE_GL_OSMESA_H

/* Define to 1 if you have the <grp.h> header file. */
#undef HAVE_GRP_H

/* Define to 1 if HDF5 is available and newer than version 1.6. */
#undef HAVE_HDF5

/* Define to 1 if >=HDF5-1.8 is available. */
#undef HAVE_HDF5_18

/* Define to 1 if you have the <hdf5.h> header file. */
#undef HAVE_HDF5_H

/* Define to 1 if you have the `hypotf' function. */
#undef HAVE_HYPOTF

/* Define to 1 if your system uses IEEE 754 data format. */
#undef HAVE_IEEE754_DATA_FORMAT

/* Define to 1 if you have the <ieeefp.h> header file. */
#undef HAVE_IEEEFP_H

/* Define to 1 if the system has the type `ino_t'. */
#undef HAVE_INO_T

/* Define if you have the 'intmax_t' type in <stdint.h> or <inttypes.h>. */
#undef HAVE_INTMAX_T

/* Define to 1 if you have the <inttypes.h> header file. */
#undef HAVE_INTTYPES_H

/* Define if <inttypes.h> exists, doesn't clash with <sys/types.h>, and
   declares uintmax_t. */
#undef HAVE_INTTYPES_H_WITH_UINTMAX

/* Define to 1 if you have the `isascii' function. */
#undef HAVE_ISASCII

/* Define to 1 if you have the `isblank' function. */
#undef HAVE_ISBLANK

/* Define to 1 if you have the `isinf' function. */
#undef HAVE_ISINF

/* Define to 1 if you have the `isnan' function. */
#undef HAVE_ISNAN

/* Define if the isnan(double) function is available in libc. */
#undef HAVE_ISNAND_IN_LIBC

/* Define if the isnan(float) function is available in libc. */
#undef HAVE_ISNANF_IN_LIBC

/* Define if the isnan(long double) function is available in libc. */
#undef HAVE_ISNANL_IN_LIBC

/* Define to 1 if you have the `issetugid' function. */
#undef HAVE_ISSETUGID

/* Define to 1 if you have the `iswcntrl' function. */
#undef HAVE_ISWCNTRL

/* Define to 1 if you have the `iswctype' function. */
#undef HAVE_ISWCTYPE

/* Define to 1 if Java is available and is at least version 1.5 */
#undef HAVE_JAVA

/* Define to 1 if you have the `kill' function. */
#undef HAVE_KILL

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
#undef HAVE_LANGINFO_CODESET

/* Define if you have LAPACK library. */
#undef HAVE_LAPACK

/* Define to 1 if you have the `lgamma' function. */
#undef HAVE_LGAMMA

/* Define to 1 if you have the `lgammaf' function. */
#undef HAVE_LGAMMAF

/* Define to 1 if you have the `lgammaf_r' function. */
#undef HAVE_LGAMMAF_R

/* Define to 1 if you have the `lgamma_r' function. */
#undef HAVE_LGAMMA_R

/* Define to 1 if you have the `dirent' library (-ldirent). */
#undef HAVE_LIBDIRENT

/* Define to 1 if you have the `m' library (-lm). */
#undef HAVE_LIBM

/* Define to 1 if you have the <libqhull.h> header file. */
#undef HAVE_LIBQHULL_H

/* Define to 1 if you have the <libqhull/libqhull.h> header file. */
#undef HAVE_LIBQHULL_LIBQHULL_H

/* Define to 1 if you have the `sun' library (-lsun). */
#undef HAVE_LIBSUN

/* Define to 1 if you have the <libutil.h> header file. */
#undef HAVE_LIBUTIL_H

/* Define to 1 if you have the `link' function. */
#undef HAVE_LINK

/* Define to 1 if LLVM is available. */
#undef HAVE_LLVM

/* Define to 1 if you have the <llvm/DataLayout.h> header file. */
#undef HAVE_LLVM_DATALAYOUT_H

/* Define to 1 if you have the <llvm/Function.h> header file. */
#undef HAVE_LLVM_FUNCTION_H

/* Define to 1 if you have the <llvm/IRBuilder.h> header file. */
#undef HAVE_LLVM_IRBUILDER_H

/* Define to 1 if you have the <llvm/IR/DataLayout.h> header file. */
#undef HAVE_LLVM_IR_DATALAYOUT_H

/* Define to 1 if you have the <llvm/IR/Function.h> header file. */
#undef HAVE_LLVM_IR_FUNCTION_H

/* Define to 1 if you have the <llvm/IR/IRBuilder.h> header file. */
#undef HAVE_LLVM_IR_IRBUILDER_H

/* Define to 1 if you have the <llvm/IR/Verifier.h> header file. */
#undef HAVE_LLVM_IR_VERIFIER_H

/* Define to 1 if you have the <llvm/Support/IRBuilder.h> header file. */
#undef HAVE_LLVM_SUPPORT_IRBUILDER_H

/* Define to 1 if you have the <llvm/Target/TargetData.h> header file. */
#undef HAVE_LLVM_TARGET_TARGETDATA_H

/* Define to 1 if your system has LoadLibrary for dynamic linking. */
#undef HAVE_LOADLIBRARY_API

/* Define to 1 if you have the <locale.h> header file. */
#undef HAVE_LOCALE_H

/* Define to 1 if you have the `localtime_r' function. */
#undef HAVE_LOCALTIME_R

/* Define to 1 if you have the `log1p' function. */
#undef HAVE_LOG1P

/* Define to 1 if you have the `log1pf' function. */
#undef HAVE_LOG1PF

/* Define to 1 if you have the `log2' function. */
#undef HAVE_LOG2

/* Define to 1 if you have the `log2f' function. */
#undef HAVE_LOG2F

/* Define to 1 if you have the `logf' function. */
#undef HAVE_LOGF

/* Define to 1 if you support file names longer than 14 characters. */
#undef HAVE_LONG_FILE_NAMES

/* Define to 1 if the system has the type 'long long int'. */
#undef HAVE_LONG_LONG_INT

/* Define to 1 if you have the `lstat' function. */
#undef HAVE_LSTAT

/* Define to 1 if Graphics/ImageMagick++ is available. */
#undef HAVE_MAGICK

/* Define to 1 if your system has a GNU libc compatible 'malloc' function, and
   to 0 otherwise. */
#undef HAVE_MALLOC_GNU

/* Define if the 'malloc' function is POSIX compliant. */
#undef HAVE_MALLOC_POSIX

/* Define to 1 if mmap()'s MAP_ANONYMOUS flag is available after including
   config.h and <sys/mman.h>. */
#undef HAVE_MAP_ANONYMOUS

/* Define to 1 if defines such as M_PI are available in math.h */
#undef HAVE_MATH_DEFINES

/* Define to 1 if you have the <math.h> header file. */
#undef HAVE_MATH_H

/* Define to 1 if you have the `mbrtowc' function. */
#undef HAVE_MBRTOWC

/* Define to 1 if you have the `mbsinit' function. */
#undef HAVE_MBSINIT

/* Define to 1 if you have the `mbsrtowcs' function. */
#undef HAVE_MBSRTOWCS

/* Define to 1 if <wchar.h> declares mbstate_t. */
#undef HAVE_MBSTATE_T

/* Define to 1 if you have the <memory.h> header file. */
#undef HAVE_MEMORY_H

/* Define to 1 if you have the `mempcpy' function. */
#undef HAVE_MEMPCPY

/* Define to 1 if you have the `memrchr' function. */
#undef HAVE_MEMRCHR

/* Define to 1 if getcwd minimally works, that is, its result can be trusted
   when it succeeds. */
#undef HAVE_MINIMALLY_WORKING_GETCWD

/* Define to 1 if you have the `mkfifo' function. */
#undef HAVE_MKFIFO

/* Define to 1 if you have the `mkostemp' function. */
#undef HAVE_MKOSTEMP

/* Define to 1 if you have the `mmap' function. */
#undef HAVE_MMAP

/* Define to 1 if you have the `modf' function. */
#undef HAVE_MODF

/* Define to 1 if you have the `mprotect' function. */
#undef HAVE_MPROTECT

/* Define to 1 on MSVC platforms that have the "invalid parameter handler"
   concept. */
#undef HAVE_MSVC_INVALID_PARAMETER_HANDLER

/* Define to 1 if you have the `munmap' function. */
#undef HAVE_MUNMAP

/* Define to 1 if you have the <ncurses.h> header file. */
#undef HAVE_NCURSES_H

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
#undef HAVE_NDIR_H

/* Define to 1 if you have the <netdb.h> header file. */
#undef HAVE_NETDB_H

/* Define to 1 if you have the `omp_get_num_threads' function. */
#undef HAVE_OMP_GET_NUM_THREADS

/* Define to 1 if you have the <omp.h> header file. */
#undef HAVE_OMP_H

/* Define to 1 if you have the `openat' function. */
#undef HAVE_OPENAT

/* Define to 1 if you have the `opendir' function. */
#undef HAVE_OPENDIR

/* Define to 1 if OpenGL is available. */
#undef HAVE_OPENGL

/* Define to 1 if you have the <OpenGL/glext.h> header file. */
#undef HAVE_OPENGL_GLEXT_H

/* Define to 1 if you have the <OpenGL/glu.h> header file. */
#undef HAVE_OPENGL_GLU_H

/* Define to 1 if you have the <OpenGL/gl.h> header file. */
#undef HAVE_OPENGL_GL_H

/* Define if OpenMP is enabled */
#undef HAVE_OPENMP

/* Define whether openpty exists */
#undef HAVE_OPENPTY

/* Define to 1 if libcrypto is used for MD5. */
#undef HAVE_OPENSSL_MD5

/* Define to 1 if OSMesa is available. */
#undef HAVE_OSMESA

/* Define to 1 if you have the <osmesa.h> header file. */
#undef HAVE_OSMESA_H

/* Define to 1 if C++ allows overload of char, int8_t, and uint8_t types. */
#undef HAVE_OVERLOAD_CHAR_INT8_TYPES

/* Define to 1 if getcwd works, except it sometimes fails when it shouldn't,
   setting errno to ERANGE, ENAMETOOLONG, or ENOENT. */
#undef HAVE_PARTLY_WORKING_GETCWD

/* Define to 1 if you have the `pathconf' function. */
#undef HAVE_PATHCONF

/* Define to 1 if you have the `pcre_compile' function. */
#undef HAVE_PCRE_COMPILE

/* Define to 1 if you have the <pcre.h> header file. */
#undef HAVE_PCRE_H

/* Define to 1 if you have the <pcre/pcre.h> header file. */
#undef HAVE_PCRE_PCRE_H

/* Define to 1 if you have the `pipe' function. */
#undef HAVE_PIPE

/* Define to 1 if C++ supports operator delete(void *, void *). */
#undef HAVE_PLACEMENT_DELETE

/* Define to 1 if you have the <poll.h> header file. */
#undef HAVE_POLL_H

/* Define to 1 if PortAudio is available. */
#undef HAVE_PORTAUDIO

/* Define to 1 if you have the `pow' function. */
#undef HAVE_POW

/* Define to 1 if you have the `pstat_getdynamic' function. */
#undef HAVE_PSTAT_GETDYNAMIC

/* Define if you have POSIX threads libraries and header files. */
#undef HAVE_PTHREAD

/* Define to 1 if you have the <pthread.h> header file. */
#undef HAVE_PTHREAD_H

/* Have PTHREAD_PRIO_INHERIT. */
#undef HAVE_PTHREAD_PRIO_INHERIT

/* Define to 1 if the system has the type `ptrdiff_t'. */
#undef HAVE_PTRDIFF_T

/* Define to 1 if you have the <pty.h> header file. */
#undef HAVE_PTY_H

/* Define to 1 if you have the <pwd.h> header file. */
#undef HAVE_PWD_H

/* Define to 1 if Qt has the QAbstractItemModel::beginResetModel() function.
   */
#undef HAVE_QABSTRACTITEMMODEL_BEGINRESETMODEL

/* Define to 1 if Qt provides QFont::ForceIntegerMetrics. */
#undef HAVE_QFONT_FORCE_INTEGER_METRICS

/* Define to 1 if Qt provides QFont::Monospace. */
#undef HAVE_QFONT_MONOSPACE

/* Define to 1 if Qhull is available. */
#undef HAVE_QHULL

/* Define to 1 if you have the <qhull.h> header file. */
#undef HAVE_QHULL_H

/* Define to 1 if you have the <qhull/libqhull.h> header file. */
#undef HAVE_QHULL_LIBQHULL_H

/* Define to 1 if you have the <qhull/qhull.h> header file. */
#undef HAVE_QHULL_QHULL_H

/* Define to 1 if qrupdate is available. */
#undef HAVE_QRUPDATE

/* Define to 1 if qrupdate supports LU updates. */
#undef HAVE_QRUPDATE_LUU

/* Define to 1 if the QScintilla library and header files are available */
#undef HAVE_QSCINTILLA

/* Define to 1 if Qsci has the QsciScintilla::findFirstInSelection ()
   function. */
#undef HAVE_QSCI_FINDSELECTION

/* Define to 1 if you have the <Qsci/qscilexermatlab.h> header file. */
#undef HAVE_QSCI_QSCILEXERMATLAB_H

/* Define to 1 if you have the <Qsci/qscilexeroctave.h> header file. */
#undef HAVE_QSCI_QSCILEXEROCTAVE_H

/* Define to 1 if Qscintilla is of Version 2.6.0 or later. */
#undef HAVE_QSCI_VERSION_2_6_0

/* Define to 1 if Qt is available (libraries, developer header files, utility
   programs (moc, uic, rcc, and lrelease)) */
#undef HAVE_QT

/* Define to 1 if Qt has the QTabWidget::setMovable() function. */
#undef HAVE_QTABWIDGET_SETMOVABLE

/* Define to 1 if you have the `raise' function. */
#undef HAVE_RAISE

/* Define to 1 if accept is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ACCEPT

/* Define to 1 if accept4 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ACCEPT4

/* Define to 1 if acosf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ACOSF

/* Define to 1 if acosl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ACOSL

/* Define to 1 if alphasort is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ALPHASORT

/* Define to 1 if asinf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ASINF

/* Define to 1 if asinl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ASINL

/* Define to 1 if atanf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ATANF

/* Define to 1 if atanl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ATANL

/* Define to 1 if atoll is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ATOLL

/* Define to 1 if bind is declared even after undefining macros. */
#undef HAVE_RAW_DECL_BIND

/* Define to 1 if btowc is declared even after undefining macros. */
#undef HAVE_RAW_DECL_BTOWC

/* Define to 1 if canonicalize_file_name is declared even after undefining
   macros. */
#undef HAVE_RAW_DECL_CANONICALIZE_FILE_NAME

/* Define to 1 if cbrt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CBRT

/* Define to 1 if cbrtf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CBRTF

/* Define to 1 if cbrtl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CBRTL

/* Define to 1 if ceilf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CEILF

/* Define to 1 if ceill is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CEILL

/* Define to 1 if chdir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CHDIR

/* Define to 1 if chown is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CHOWN

/* Define to 1 if closedir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CLOSEDIR

/* Define to 1 if connect is declared even after undefining macros. */
#undef HAVE_RAW_DECL_CONNECT

/* Define to 1 if copysign is declared even after undefining macros. */
#undef HAVE_RAW_DECL_COPYSIGN

/* Define to 1 if copysignf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_COPYSIGNF

/* Define to 1 if copysignl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_COPYSIGNL

/* Define to 1 if cosf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_COSF

/* Define to 1 if coshf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_COSHF

/* Define to 1 if cosl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_COSL

/* Define to 1 if dirfd is declared even after undefining macros. */
#undef HAVE_RAW_DECL_DIRFD

/* Define to 1 if dprintf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_DPRINTF

/* Define to 1 if dup is declared even after undefining macros. */
#undef HAVE_RAW_DECL_DUP

/* Define to 1 if dup2 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_DUP2

/* Define to 1 if dup3 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_DUP3

/* Define to 1 if endusershell is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ENDUSERSHELL

/* Define to 1 if environ is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ENVIRON

/* Define to 1 if euidaccess is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EUIDACCESS

/* Define to 1 if exp2 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXP2

/* Define to 1 if exp2f is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXP2F

/* Define to 1 if exp2l is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXP2L

/* Define to 1 if expf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXPF

/* Define to 1 if expl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXPL

/* Define to 1 if expm1 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXPM1

/* Define to 1 if expm1f is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXPM1F

/* Define to 1 if expm1l is declared even after undefining macros. */
#undef HAVE_RAW_DECL_EXPM1L

/* Define to 1 if fabsf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FABSF

/* Define to 1 if fabsl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FABSL

/* Define to 1 if faccessat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FACCESSAT

/* Define to 1 if fchdir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FCHDIR

/* Define to 1 if fchmodat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FCHMODAT

/* Define to 1 if fchownat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FCHOWNAT

/* Define to 1 if fcntl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FCNTL

/* Define to 1 if fdatasync is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FDATASYNC

/* Define to 1 if fdopendir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FDOPENDIR

/* Define to 1 if ffs is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FFS

/* Define to 1 if ffsl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FFSL

/* Define to 1 if ffsll is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FFSLL

/* Define to 1 if floorf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FLOORF

/* Define to 1 if floorl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FLOORL

/* Define to 1 if fma is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FMA

/* Define to 1 if fmaf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FMAF

/* Define to 1 if fmal is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FMAL

/* Define to 1 if fmod is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FMOD

/* Define to 1 if fmodf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FMODF

/* Define to 1 if fmodl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FMODL

/* Define to 1 if fpurge is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FPURGE

/* Define to 1 if frexpf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FREXPF

/* Define to 1 if frexpl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FREXPL

/* Define to 1 if fseeko is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FSEEKO

/* Define to 1 if fstat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FSTAT

/* Define to 1 if fstatat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FSTATAT

/* Define to 1 if fsync is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FSYNC

/* Define to 1 if ftello is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FTELLO

/* Define to 1 if ftruncate is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FTRUNCATE

/* Define to 1 if futimens is declared even after undefining macros. */
#undef HAVE_RAW_DECL_FUTIMENS

/* Define to 1 if getcwd is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETCWD

/* Define to 1 if getdelim is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETDELIM

/* Define to 1 if getdomainname is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETDOMAINNAME

/* Define to 1 if getdtablesize is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETDTABLESIZE

/* Define to 1 if getgroups is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETGROUPS

/* Define to 1 if gethostname is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETHOSTNAME

/* Define to 1 if getline is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETLINE

/* Define to 1 if getloadavg is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETLOADAVG

/* Define to 1 if getlogin is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETLOGIN

/* Define to 1 if getlogin_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETLOGIN_R

/* Define to 1 if getpagesize is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETPAGESIZE

/* Define to 1 if getpeername is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETPEERNAME

/* Define to 1 if gets is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETS

/* Define to 1 if getsockname is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETSOCKNAME

/* Define to 1 if getsockopt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETSOCKOPT

/* Define to 1 if getsubopt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETSUBOPT

/* Define to 1 if gettimeofday is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETTIMEOFDAY

/* Define to 1 if getusershell is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GETUSERSHELL

/* Define to 1 if grantpt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GRANTPT

/* Define to 1 if group_member is declared even after undefining macros. */
#undef HAVE_RAW_DECL_GROUP_MEMBER

/* Define to 1 if hypotf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_HYPOTF

/* Define to 1 if hypotl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_HYPOTL

/* Define to 1 if ilogb is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ILOGB

/* Define to 1 if ilogbf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ILOGBF

/* Define to 1 if ilogbl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ILOGBL

/* Define to 1 if initstate is declared even after undefining macros. */
#undef HAVE_RAW_DECL_INITSTATE

/* Define to 1 if initstate_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_INITSTATE_R

/* Define to 1 if isatty is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ISATTY

/* Define to 1 if iswctype is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ISWCTYPE

/* Define to 1 if lchmod is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LCHMOD

/* Define to 1 if lchown is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LCHOWN

/* Define to 1 if ldexpf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LDEXPF

/* Define to 1 if ldexpl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LDEXPL

/* Define to 1 if link is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LINK

/* Define to 1 if linkat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LINKAT

/* Define to 1 if listen is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LISTEN

/* Define to 1 if log is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG

/* Define to 1 if log10 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG10

/* Define to 1 if log10f is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG10F

/* Define to 1 if log10l is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG10L

/* Define to 1 if log1p is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG1P

/* Define to 1 if log1pf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG1PF

/* Define to 1 if log1pl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG1PL

/* Define to 1 if log2 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG2

/* Define to 1 if log2f is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG2F

/* Define to 1 if log2l is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOG2L

/* Define to 1 if logb is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOGB

/* Define to 1 if logbf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOGBF

/* Define to 1 if logbl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOGBL

/* Define to 1 if logf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOGF

/* Define to 1 if logl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LOGL

/* Define to 1 if lseek is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LSEEK

/* Define to 1 if lstat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_LSTAT

/* Define to 1 if mbrlen is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MBRLEN

/* Define to 1 if mbrtowc is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MBRTOWC

/* Define to 1 if mbsinit is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MBSINIT

/* Define to 1 if mbsnrtowcs is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MBSNRTOWCS

/* Define to 1 if mbsrtowcs is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MBSRTOWCS

/* Define to 1 if memmem is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MEMMEM

/* Define to 1 if mempcpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MEMPCPY

/* Define to 1 if memrchr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MEMRCHR

/* Define to 1 if mkdirat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKDIRAT

/* Define to 1 if mkdtemp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKDTEMP

/* Define to 1 if mkfifo is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKFIFO

/* Define to 1 if mkfifoat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKFIFOAT

/* Define to 1 if mknod is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKNOD

/* Define to 1 if mknodat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKNODAT

/* Define to 1 if mkostemp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKOSTEMP

/* Define to 1 if mkostemps is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKOSTEMPS

/* Define to 1 if mkstemp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKSTEMP

/* Define to 1 if mkstemps is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MKSTEMPS

/* Define to 1 if modf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MODF

/* Define to 1 if modff is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MODFF

/* Define to 1 if modfl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_MODFL

/* Define to 1 if openat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_OPENAT

/* Define to 1 if opendir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_OPENDIR

/* Define to 1 if pclose is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PCLOSE

/* Define to 1 if pipe is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PIPE

/* Define to 1 if pipe2 is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PIPE2

/* Define to 1 if popen is declared even after undefining macros. */
#undef HAVE_RAW_DECL_POPEN

/* Define to 1 if posix_openpt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_POSIX_OPENPT

/* Define to 1 if powf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_POWF

/* Define to 1 if pread is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PREAD

/* Define to 1 if pselect is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PSELECT

/* Define to 1 if pthread_sigmask is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PTHREAD_SIGMASK

/* Define to 1 if ptsname is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PTSNAME

/* Define to 1 if ptsname_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PTSNAME_R

/* Define to 1 if pwrite is declared even after undefining macros. */
#undef HAVE_RAW_DECL_PWRITE

/* Define to 1 if random is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RANDOM

/* Define to 1 if random_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RANDOM_R

/* Define to 1 if rawmemchr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RAWMEMCHR

/* Define to 1 if readdir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_READDIR

/* Define to 1 if readlink is declared even after undefining macros. */
#undef HAVE_RAW_DECL_READLINK

/* Define to 1 if readlinkat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_READLINKAT

/* Define to 1 if realpath is declared even after undefining macros. */
#undef HAVE_RAW_DECL_REALPATH

/* Define to 1 if recv is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RECV

/* Define to 1 if recvfrom is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RECVFROM

/* Define to 1 if remainder is declared even after undefining macros. */
#undef HAVE_RAW_DECL_REMAINDER

/* Define to 1 if remainderf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_REMAINDERF

/* Define to 1 if remainderl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_REMAINDERL

/* Define to 1 if renameat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RENAMEAT

/* Define to 1 if rewinddir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_REWINDDIR

/* Define to 1 if rint is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RINT

/* Define to 1 if rintf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RINTF

/* Define to 1 if rintl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RINTL

/* Define to 1 if rmdir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RMDIR

/* Define to 1 if round is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ROUND

/* Define to 1 if roundf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ROUNDF

/* Define to 1 if roundl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_ROUNDL

/* Define to 1 if rpmatch is declared even after undefining macros. */
#undef HAVE_RAW_DECL_RPMATCH

/* Define to 1 if scandir is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SCANDIR

/* Define to 1 if secure_getenv is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SECURE_GETENV

/* Define to 1 if select is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SELECT

/* Define to 1 if send is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SEND

/* Define to 1 if sendto is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SENDTO

/* Define to 1 if setenv is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SETENV

/* Define to 1 if sethostname is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SETHOSTNAME

/* Define to 1 if setsockopt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SETSOCKOPT

/* Define to 1 if setstate is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SETSTATE

/* Define to 1 if setstate_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SETSTATE_R

/* Define to 1 if setusershell is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SETUSERSHELL

/* Define to 1 if shutdown is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SHUTDOWN

/* Define to 1 if sigaction is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGACTION

/* Define to 1 if sigaddset is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGADDSET

/* Define to 1 if sigdelset is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGDELSET

/* Define to 1 if sigemptyset is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGEMPTYSET

/* Define to 1 if sigfillset is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGFILLSET

/* Define to 1 if sigismember is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGISMEMBER

/* Define to 1 if sigpending is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGPENDING

/* Define to 1 if sigprocmask is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SIGPROCMASK

/* Define to 1 if sinf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SINF

/* Define to 1 if sinhf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SINHF

/* Define to 1 if sinl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SINL

/* Define to 1 if sleep is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SLEEP

/* Define to 1 if snprintf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SNPRINTF

/* Define to 1 if socket is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SOCKET

/* Define to 1 if sqrtf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SQRTF

/* Define to 1 if sqrtl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SQRTL

/* Define to 1 if srandom is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SRANDOM

/* Define to 1 if srandom_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SRANDOM_R

/* Define to 1 if stat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STAT

/* Define to 1 if stpcpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STPCPY

/* Define to 1 if stpncpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STPNCPY

/* Define to 1 if strcasecmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRCASECMP

/* Define to 1 if strcasestr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRCASESTR

/* Define to 1 if strchrnul is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRCHRNUL

/* Define to 1 if strdup is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRDUP

/* Define to 1 if strerror_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRERROR_R

/* Define to 1 if strncasecmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRNCASECMP

/* Define to 1 if strncat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRNCAT

/* Define to 1 if strndup is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRNDUP

/* Define to 1 if strnlen is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRNLEN

/* Define to 1 if strpbrk is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRPBRK

/* Define to 1 if strsep is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRSEP

/* Define to 1 if strsignal is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRSIGNAL

/* Define to 1 if strtod is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRTOD

/* Define to 1 if strtok_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRTOK_R

/* Define to 1 if strtoll is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRTOLL

/* Define to 1 if strtoull is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRTOULL

/* Define to 1 if strverscmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_STRVERSCMP

/* Define to 1 if symlink is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SYMLINK

/* Define to 1 if symlinkat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_SYMLINKAT

/* Define to 1 if tanf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TANF

/* Define to 1 if tanhf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TANHF

/* Define to 1 if tanl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TANL

/* Define to 1 if times is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TIMES

/* Define to 1 if tmpfile is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TMPFILE

/* Define to 1 if towctrans is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TOWCTRANS

/* Define to 1 if trunc is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TRUNC

/* Define to 1 if truncf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TRUNCF

/* Define to 1 if truncl is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TRUNCL

/* Define to 1 if ttyname_r is declared even after undefining macros. */
#undef HAVE_RAW_DECL_TTYNAME_R

/* Define to 1 if uname is declared even after undefining macros. */
#undef HAVE_RAW_DECL_UNAME

/* Define to 1 if unlink is declared even after undefining macros. */
#undef HAVE_RAW_DECL_UNLINK

/* Define to 1 if unlinkat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_UNLINKAT

/* Define to 1 if unlockpt is declared even after undefining macros. */
#undef HAVE_RAW_DECL_UNLOCKPT

/* Define to 1 if unsetenv is declared even after undefining macros. */
#undef HAVE_RAW_DECL_UNSETENV

/* Define to 1 if usleep is declared even after undefining macros. */
#undef HAVE_RAW_DECL_USLEEP

/* Define to 1 if utimensat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_UTIMENSAT

/* Define to 1 if vdprintf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_VDPRINTF

/* Define to 1 if vsnprintf is declared even after undefining macros. */
#undef HAVE_RAW_DECL_VSNPRINTF

/* Define to 1 if wcpcpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCPCPY

/* Define to 1 if wcpncpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCPNCPY

/* Define to 1 if wcrtomb is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCRTOMB

/* Define to 1 if wcscasecmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCASECMP

/* Define to 1 if wcscat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCAT

/* Define to 1 if wcschr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCHR

/* Define to 1 if wcscmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCMP

/* Define to 1 if wcscoll is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCOLL

/* Define to 1 if wcscpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCPY

/* Define to 1 if wcscspn is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSCSPN

/* Define to 1 if wcsdup is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSDUP

/* Define to 1 if wcslen is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSLEN

/* Define to 1 if wcsncasecmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSNCASECMP

/* Define to 1 if wcsncat is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSNCAT

/* Define to 1 if wcsncmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSNCMP

/* Define to 1 if wcsncpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSNCPY

/* Define to 1 if wcsnlen is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSNLEN

/* Define to 1 if wcsnrtombs is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSNRTOMBS

/* Define to 1 if wcspbrk is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSPBRK

/* Define to 1 if wcsrchr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSRCHR

/* Define to 1 if wcsrtombs is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSRTOMBS

/* Define to 1 if wcsspn is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSSPN

/* Define to 1 if wcsstr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSSTR

/* Define to 1 if wcstok is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSTOK

/* Define to 1 if wcswidth is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSWIDTH

/* Define to 1 if wcsxfrm is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCSXFRM

/* Define to 1 if wctob is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCTOB

/* Define to 1 if wctrans is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCTRANS

/* Define to 1 if wctype is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCTYPE

/* Define to 1 if wcwidth is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WCWIDTH

/* Define to 1 if wmemchr is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WMEMCHR

/* Define to 1 if wmemcmp is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WMEMCMP

/* Define to 1 if wmemcpy is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WMEMCPY

/* Define to 1 if wmemmove is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WMEMMOVE

/* Define to 1 if wmemset is declared even after undefining macros. */
#undef HAVE_RAW_DECL_WMEMSET

/* Define to 1 if _Exit is declared even after undefining macros. */
#undef HAVE_RAW_DECL__EXIT

/* Define to 1 if you have the `readdir' function. */
#undef HAVE_READDIR

/* Define to 1 if you have the `readlink' function. */
#undef HAVE_READLINK

/* Define to 1 if your system has a GNU libc compatible 'realloc' function,
   and to 0 otherwise. */
#undef HAVE_REALLOC_GNU

/* Define if the 'realloc' function is POSIX compliant. */
#undef HAVE_REALLOC_POSIX

/* Define to 1 if you have the `realpath' function. */
#undef HAVE_REALPATH

/* Define to 1 if you have the `resolvepath' function. */
#undef HAVE_RESOLVEPATH

/* Define to 1 if you have the `rewinddir' function. */
#undef HAVE_REWINDDIR

/* Define to 1 if you have the `round' function. */
#undef HAVE_ROUND

/* Define to 1 if you have the `roundl' function. */
#undef HAVE_ROUNDL

/* Define to 1 if 'long double' and 'double' have the same representation. */
#undef HAVE_SAME_LONG_DOUBLE_AS_DOUBLE

/* Define to 1 if the system has the type `sa_family_t'. */
#undef HAVE_SA_FAMILY_T

/* Define to 1 if you have the `sched_getaffinity' function. */
#undef HAVE_SCHED_GETAFFINITY

/* Define to 1 if sched_getaffinity has a glibc compatible declaration. */
#undef HAVE_SCHED_GETAFFINITY_LIKE_GLIBC

/* Define to 1 if you have the `sched_getaffinity_np' function. */
#undef HAVE_SCHED_GETAFFINITY_NP

/* Define to 1 if you have the `secure_getenv' function. */
#undef HAVE_SECURE_GETENV

/* Define to 1 if you have the `select' function. */
#undef HAVE_SELECT

/* Define to 1 if you have the `setdtablesize' function. */
#undef HAVE_SETDTABLESIZE

/* Define to 1 if you have the `setgrent' function. */
#undef HAVE_SETGRENT

/* Define to 1 if you have the `setlocale' function. */
#undef HAVE_SETLOCALE

/* Define to 1 if you have the Qt SetPlaceholderText function. */
#undef HAVE_SETPLACEHOLDERTEXT

/* Define to 1 if you have the `setpwent' function. */
#undef HAVE_SETPWENT

/* Define to 1 if you have the `setvbuf' function. */
#undef HAVE_SETVBUF

/* Define to 1 if you have the <sgtty.h> header file. */
#undef HAVE_SGTTY_H

/* Define to 1 if your system has shl_load and shl_findsym for dynamic
   linking. */
#undef HAVE_SHL_LOAD_API

/* Define to 1 if you have the `shutdown' function. */
#undef HAVE_SHUTDOWN

/* Define to 1 if you have the `sigaction' function. */
#undef HAVE_SIGACTION

/* Define to 1 if you have the `sigaltstack' function. */
#undef HAVE_SIGALTSTACK

/* Define to 1 if the system has the type `siginfo_t'. */
#undef HAVE_SIGINFO_T

/* Define to 1 if you have the `siginterrupt' function. */
#undef HAVE_SIGINTERRUPT

/* Define to 1 if you have the `siglongjmp' function. */
#undef HAVE_SIGLONGJMP

/* Define to 1 if you have the `signbit' function. */
#undef HAVE_SIGNBIT

/* Define to 1 if 'sig_atomic_t' is a signed integer type. */
#undef HAVE_SIGNED_SIG_ATOMIC_T

/* Define to 1 if 'wchar_t' is a signed integer type. */
#undef HAVE_SIGNED_WCHAR_T

/* Define to 1 if 'wint_t' is a signed integer type. */
#undef HAVE_SIGNED_WINT_T

/* Define to 1 if the system has the type `sigset_t'. */
#undef HAVE_SIGSET_T

/* Define to 1 if you have the `sleep' function. */
#undef HAVE_SLEEP

/* Define to 1 if sndfile is available. */
#undef HAVE_SNDFILE

/* Define to 1 if you have the `snprintf' function. */
#undef HAVE_SNPRINTF

/* Define if the return value of the snprintf function is the number of of
   bytes (excluding the terminating NUL) that would have been produced if the
   buffer had been large enough. */
#undef HAVE_SNPRINTF_RETVAL_C99

/* Define to 1 if you have the `sqrt' function. */
#undef HAVE_SQRT

/* Define to 1 if you have the `sqrtf' function. */
#undef HAVE_SQRTF

/* Define to 1 if you have the <sstream> header file. */
#undef HAVE_SSTREAM

/* Define to 1 if you have the <stdint.h> header file. */
#undef HAVE_STDINT_H

/* Define if <stdint.h> exists, doesn't clash with <sys/types.h>, and declares
   uintmax_t. */
#undef HAVE_STDINT_H_WITH_UINTMAX

/* Define to 1 if you have the <stdlib.h> header file. */
#undef HAVE_STDLIB_H

/* Define to 1 if you have the `strcasecmp' function. */
#undef HAVE_STRCASECMP

/* Define to 1 if you have the `strdup' function. */
#undef HAVE_STRDUP

/* Define to 1 if you have the `strerror_r' function. */
#undef HAVE_STRERROR_R

/* Define to 1 if you have the <strings.h> header file. */
#undef HAVE_STRINGS_H

/* Define to 1 if you have the <string.h> header file. */
#undef HAVE_STRING_H

/* Define to 1 if you have the `strncasecmp' function. */
#undef HAVE_STRNCASECMP

/* Define to 1 if you have the `strndup' function. */
#undef HAVE_STRNDUP

/* Define to 1 if you have the `strnlen' function. */
#undef HAVE_STRNLEN

/* Define to 1 if you have the `strptime' function. */
#undef HAVE_STRPTIME

/* Define to 1 if you have the `strsignal' function. */
#undef HAVE_STRSIGNAL

/* Define if there is a member named d_type in the struct describing directory
   headers. */
#undef HAVE_STRUCT_DIRENT_D_TYPE

/* Define to 1 if `gr_passwd' is a member of `struct group'. */
#undef HAVE_STRUCT_GROUP_GR_PASSWD

/* Define to 1 if `sa_sigaction' is a member of `struct sigaction'. */
#undef HAVE_STRUCT_SIGACTION_SA_SIGACTION

/* Define to 1 if the system has the type `struct sockaddr_storage'. */
#undef HAVE_STRUCT_SOCKADDR_STORAGE

/* Define to 1 if `ss_family' is a member of `struct sockaddr_storage'. */
#undef HAVE_STRUCT_SOCKADDR_STORAGE_SS_FAMILY

/* Define to 1 if `st_blksize' is a member of `struct stat'. */
#undef HAVE_STRUCT_STAT_ST_BLKSIZE

/* Define to 1 if `st_blocks' is a member of `struct stat'. */
#undef HAVE_STRUCT_STAT_ST_BLOCKS

/* Define to 1 if `st_rdev' is a member of `struct stat'. */
#undef HAVE_STRUCT_STAT_ST_RDEV

/* Define to 1 if the system has the type `struct tms'. */
#undef HAVE_STRUCT_TMS

/* Define to 1 if `tm_zone' is a member of `struct tm'. */
#undef HAVE_STRUCT_TM_TM_ZONE

/* Define to 1 if the system has the type `struct utsname'. */
#undef HAVE_STRUCT_UTSNAME

/* Define if struct stat has an st_dm_mode member. */
#undef HAVE_ST_DM_MODE

/* Define to 1 if you have the <suitesparse/amd.h> header file. */
#undef HAVE_SUITESPARSE_AMD_H

/* Define to 1 if you have the <suitesparse/camd.h> header file. */
#undef HAVE_SUITESPARSE_CAMD_H

/* Define to 1 if you have the <suitesparse/ccolamd.h> header file. */
#undef HAVE_SUITESPARSE_CCOLAMD_H

/* Define to 1 if you have the <suitesparse/cholmod.h> header file. */
#undef HAVE_SUITESPARSE_CHOLMOD_H

/* Define to 1 if you have the <suitesparse/colamd.h> header file. */
#undef HAVE_SUITESPARSE_COLAMD_H

/* Define to 1 if you have the <suitesparse/cs.h> header file. */
#undef HAVE_SUITESPARSE_CS_H

/* Define to 1 if you have the <suitesparse/umfpack.h> header file. */
#undef HAVE_SUITESPARSE_UMFPACK_H

/* Define to 1 if you have the <sunmath.h> header file. */
#undef HAVE_SUNMATH_H

/* Define to 1 if you have the `symlink' function. */
#undef HAVE_SYMLINK

/* Define to 1 if you have the `sysctl' function. */
#undef HAVE_SYSCTL

/* Define to 1 if you have the `sysmp' function. */
#undef HAVE_SYSMP

/* Define to 1 if you have the <sys/bitypes.h> header file. */
#undef HAVE_SYS_BITYPES_H

/* Define to 1 if you have the <sys/cdefs.h> header file. */
#undef HAVE_SYS_CDEFS_H

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
#undef HAVE_SYS_DIR_H

/* Define to 1 if you have the <sys/inttypes.h> header file. */
#undef HAVE_SYS_INTTYPES_H

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#undef HAVE_SYS_IOCTL_H

/* Define to 1 if you have the <sys/mman.h> header file. */
#undef HAVE_SYS_MMAN_H

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
#undef HAVE_SYS_NDIR_H

/* Define to 1 if you have the <sys/param.h> header file. */
#undef HAVE_SYS_PARAM_H

/* Define to 1 if you have the <sys/poll.h> header file. */
#undef HAVE_SYS_POLL_H

/* Define to 1 if you have the <sys/pstat.h> header file. */
#undef HAVE_SYS_PSTAT_H

/* Define to 1 if you have the <sys/resource.h> header file. */
#undef HAVE_SYS_RESOURCE_H

/* Define to 1 if you have the <sys/select.h> header file. */
#undef HAVE_SYS_SELECT_H

/* Define to 1 if you have the <sys/socket.h> header file. */
#undef HAVE_SYS_SOCKET_H

/* Define to 1 if you have the <sys/stat.h> header file. */
#undef HAVE_SYS_STAT_H

/* Define to 1 if you have the <sys/stropts.h> header file. */
#undef HAVE_SYS_STROPTS_H

/* Define to 1 if you have the <sys/sysctl.h> header file. */
#undef HAVE_SYS_SYSCTL_H

/* Define to 1 if you have the <sys/sysmp.h> header file. */
#undef HAVE_SYS_SYSMP_H

/* Define to 1 if you have the <sys/timeb.h> header file. */
#undef HAVE_SYS_TIMEB_H

/* Define to 1 if you have the <sys/times.h> header file. */
#undef HAVE_SYS_TIMES_H

/* Define to 1 if you have the <sys/time.h> header file. */
#undef HAVE_SYS_TIME_H

/* Define to 1 if you have the <sys/types.h> header file. */
#undef HAVE_SYS_TYPES_H

/* Define to 1 if you have the <sys/uio.h> header file. */
#undef HAVE_SYS_UIO_H

/* Define to 1 if you have the <sys/utsname.h> header file. */
#undef HAVE_SYS_UTSNAME_H

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#undef HAVE_SYS_WAIT_H

/* Define to 1 if you have the `tcgetattr' function. */
#undef HAVE_TCGETATTR

/* Define to 1 if you have the `tcsetattr' function. */
#undef HAVE_TCSETATTR

/* Define to 1 if you have the <termcap.h> header file. */
#undef HAVE_TERMCAP_H

/* Define to 1 if you have the <termios.h> header file. */
#undef HAVE_TERMIOS_H

/* Define to 1 if you have the <termio.h> header file. */
#undef HAVE_TERMIO_H

/* Define to 1 if you have the `tgamma' function. */
#undef HAVE_TGAMMA

/* Define to 1 if you have the `tgammaf' function. */
#undef HAVE_TGAMMAF

/* Define to 1 if you have the `times' function. */
#undef HAVE_TIMES

/* Define if struct tm has the tm_gmtoff member. */
#undef HAVE_TM_GMTOFF

/* Define to 1 if your `struct tm' has `tm_zone'. Deprecated, use
   `HAVE_STRUCT_TM_TM_ZONE' instead. */
#undef HAVE_TM_ZONE

/* Define to 1 if you have the `toascii' function. */
#undef HAVE_TOASCII

/* Define to 1 if you have the `towlower' function. */
#undef HAVE_TOWLOWER

/* Define to 1 if you have the <tr1/unordered_map> header file. */
#undef HAVE_TR1_UNORDERED_MAP

/* Define to 1 if you don't have `tm_zone' but do have the external array
   `tzname'. */
#undef HAVE_TZNAME

/* Define to 1 if you have the `tzset' function. */
#undef HAVE_TZSET

/* Define to 1 if you have the <ufsparse/amd.h> header file. */
#undef HAVE_UFSPARSE_AMD_H

/* Define to 1 if you have the <ufsparse/camd.h> header file. */
#undef HAVE_UFSPARSE_CAMD_H

/* Define to 1 if you have the <ufsparse/ccolamd.h> header file. */
#undef HAVE_UFSPARSE_CCOLAMD_H

/* Define to 1 if you have the <ufsparse/cholmod.h> header file. */
#undef HAVE_UFSPARSE_CHOLMOD_H

/* Define to 1 if you have the <ufsparse/colamd.h> header file. */
#undef HAVE_UFSPARSE_COLAMD_H

/* Define to 1 if you have the <ufsparse/cs.h> header file. */
#undef HAVE_UFSPARSE_CS_H

/* Define to 1 if you have the <ufsparse/umfpack.h> header file. */
#undef HAVE_UFSPARSE_UMFPACK_H

/* Define to 1 if you have the `umask' function. */
#undef HAVE_UMASK

/* Define to 1 if UMFPACK is available. */
#undef HAVE_UMFPACK

/* Define to 1 if you have the <umfpack.h> header file. */
#undef HAVE_UMFPACK_H

/* Define to 1 if you have the <umfpack/umfpack.h> header file. */
#undef HAVE_UMFPACK_UMFPACK_H

/* Define to 1 if you have the `uname' function. */
#undef HAVE_UNAME

/* Define to 1 if you have the <unistd.h> header file. */
#undef HAVE_UNISTD_H

/* Define to 1 if you have the <unordered_map> header file. */
#undef HAVE_UNORDERED_MAP

/* Define to 1 if you have the `unsetenv' function. */
#undef HAVE_UNSETENV

/* Define to 1 if the system has the type 'unsigned long long int'. */
#undef HAVE_UNSIGNED_LONG_LONG_INT

/* Define to 1 if you have the <util.h> header file. */
#undef HAVE_UTIL_H

/* Define to 1 if you have the `vasnprintf' function. */
#undef HAVE_VASNPRINTF

/* Define to 1 if you have the `vasprintf' function. */
#undef HAVE_VASPRINTF

/* Define to 1 if you have the `waitpid' function. */
#undef HAVE_WAITPID

/* Define to 1 if you have the <wchar.h> header file. */
#undef HAVE_WCHAR_H

/* Define if you have the 'wchar_t' type. */
#undef HAVE_WCHAR_T

/* Define to 1 if you have the `wcrtomb' function. */
#undef HAVE_WCRTOMB

/* Define to 1 if you have the `wcslen' function. */
#undef HAVE_WCSLEN

/* Define to 1 if you have the `wcsnlen' function. */
#undef HAVE_WCSNLEN

/* Define to 1 if you have the <wctype.h> header file. */
#undef HAVE_WCTYPE_H

/* Define to 1 if you have the <windows.h> header file. */
#undef HAVE_WINDOWS_H

/* Define to 1 if you have the <winsock2.h> header file. */
#undef HAVE_WINSOCK2_H

/* Define if you have the 'wint_t' type. */
#undef HAVE_WINT_T

/* Define to 1 if you have the `wmemchr' function. */
#undef HAVE_WMEMCHR

/* Define to 1 if you have the `wmemcpy' function. */
#undef HAVE_WMEMCPY

/* Define to 1 if you have the `wmempcpy' function. */
#undef HAVE_WMEMPCPY

/* Define to 1 if fstatat (..., 0) works. For example, it does not work in AIX
   7.1. */
#undef HAVE_WORKING_FSTATAT_ZERO_FLAG

/* Define to 1 if O_NOATIME works. */
#undef HAVE_WORKING_O_NOATIME

/* Define to 1 if O_NOFOLLOW works. */
#undef HAVE_WORKING_O_NOFOLLOW

/* Define to 1 if you have the <ws2tcpip.h> header file. */
#undef HAVE_WS2TCPIP_H

/* Define to 1 if Xft is present. */
#undef HAVE_XFT

/* Define to 1 if you have X11. */
#undef HAVE_X_WINDOWS

/* Define to 1 if ZLIB is available. */
#undef HAVE_Z

/* Define to 1 if ZLIB is available. */
#undef HAVE_ZLIB

/* Define to 1 if you have the <zlib.h> header file. */
#undef HAVE_ZLIB_H

/* Define to 1 if the system has the type `_Bool'. */
#undef HAVE__BOOL

/* Define to 1 if you have the `_finite' function. */
#undef HAVE__FINITE

/* Define to 1 if you have the `_fseeki64' function. */
#undef HAVE__FSEEKI64

/* Define to 1 if you have the `_ftelli64' function. */
#undef HAVE__FTELLI64

/* Define to 1 if you have the `_ftime' function. */
#undef HAVE__FTIME

/* Define to 1 if you have the `_getch' function. */
#undef HAVE__GETCH

/* Define to 1 if you have the `_hypotf' function. */
#undef HAVE__HYPOTF

/* Define to 1 if you have the `_isnan' function. */
#undef HAVE__ISNAN

/* Define to 1 if you have the `_kbhit' function. */
#undef HAVE__KBHIT

/* Define to 1 if you have the `_set_invalid_parameter_handler' function. */
#undef HAVE__SET_INVALID_PARAMETER_HANDLER

/* Define to 1 if you have the `__fpurge' function. */
#undef HAVE___FPURGE

/* Define to 1 if you have the `__freading' function. */
#undef HAVE___FREADING

/* Define to 1 if you have the `__secure_getenv' function. */
#undef HAVE___SECURE_GETENV

/* Define HOST_NAME_MAX when <limits.h> does not define it. */
#undef HOST_NAME_MAX

/* Java home (top-level installation dir) */
#undef JAVA_HOME

/* Java library path (libjvm) */
#undef JAVA_LDPATH

/* Define as the bit index in the word where to find bit 0 of the exponent of
   'long double'. */
#undef LDBL_EXPBIT0_BIT

/* Define as the word index where to find the exponent of 'long double'. */
#undef LDBL_EXPBIT0_WORD

/* Define as the bit index in the word where to find the sign of 'long
   double'. */
#undef LDBL_SIGNBIT_BIT

/* Define as the word index where to find the sign of 'long double'. */
#undef LDBL_SIGNBIT_WORD

/* Define to 1 if LLVM::legacy::PassManager exists. */
#undef LEGACY_PASSMANAGER

/* Define to 1 if lseek does not detect pipes. */
#undef LSEEK_PIPE_BROKEN

/* Define to 1 if 'lstat' dereferences a symlink specified with a trailing
   slash. */
#undef LSTAT_FOLLOWS_SLASHED_SYMLINK

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#undef LT_OBJDIR

/* If malloc(0) is != NULL, define this to 1. Otherwise define this to 0. */
#undef MALLOC_0_IS_NONNULL

/* Define to a substitute value for mmap()'s MAP_ANONYMOUS flag. */
#undef MAP_ANONYMOUS

/* Define if the mbrtowc function does not return (size_t) -2 for empty input.
   */
#undef MBRTOWC_EMPTY_INPUT_BUG

/* Define if the mbrtowc function has the NULL pwc argument bug. */
#undef MBRTOWC_NULL_ARG1_BUG

/* Define if the mbrtowc function has the NULL string argument bug. */
#undef MBRTOWC_NULL_ARG2_BUG

/* Define if the mbrtowc function does not return 0 for a NUL character. */
#undef MBRTOWC_NUL_RETVAL_BUG

/* Define if the mbrtowc function returns a wrong return value. */
#undef MBRTOWC_RETVAL_BUG

/* Define to 1 if mkfifo does not reject trailing slash */
#undef MKFIFO_TRAILING_SLASH_BUG

/* Define to 1 if the Qhull library needs a qh_version variable defined. */
#undef NEED_QHULL_VERSION

/* Define to 1 if you want to avoid min/max macro definition in Windows
   headers. */
#undef NOMINMAX

/* Define to the type of octave_idx_type (64 or 32 bit signed integer). */
#undef OCTAVE_IDX_TYPE

/* Define to 1 if this is Octave. */
#undef OCTAVE_SOURCE

/* Define to 1 if open() fails to recognize a trailing slash. */
#undef OPEN_TRAILING_SLASH_BUG

/* Name of package */
#undef PACKAGE

/* Define to the address where bug reports for this package should be sent. */
#undef PACKAGE_BUGREPORT

/* Define to the full name of this package. */
#undef PACKAGE_NAME

/* Define to the full name and version of this package. */
#undef PACKAGE_STRING

/* Define to the one symbol short name of this package. */
#undef PACKAGE_TARNAME

/* Define to the home page for this package. */
#undef PACKAGE_URL

/* Define to the version of this package. */
#undef PACKAGE_VERSION

/* Define to the type that is the result of default argument promotions of
   type mode_t. */
#undef PROMOTED_MODE_T

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
#undef PTHREAD_CREATE_JOINABLE

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'ptrdiff_t'. */
#undef PTRDIFF_T_SUFFIX

/* Define to 1 if LLVM::raw_fd_ostream arg type is llvm::sys:fs. */
#undef RAW_FD_OSTREAM_ARG_IS_LLVM_SYS_FS

/* Define to 1 if readlink fails to recognize a trailing slash. */
#undef READLINK_TRAILING_SLASH_BUG

/* Define if rename does not work when the destination file exists, as on
   Cygwin 1.5 or Windows. */
#undef RENAME_DEST_EXISTS_BUG

/* Define if rename fails to leave hard links alone, as on NetBSD 1.6 or
   Cygwin 1.5. */
#undef RENAME_HARD_LINK_BUG

/* Define if rename does not correctly handle slashes on the destination
   argument, such as on Solaris 10 or NetBSD 1.6. */
#undef RENAME_TRAILING_SLASH_DEST_BUG

/* Define if rename does not correctly handle slashes on the source argument,
   such as on Solaris 9 or cygwin 1.5. */
#undef RENAME_TRAILING_SLASH_SOURCE_BUG

/* Define to 1 if gnulib's fchdir() replacement is used. */
#undef REPLACE_FCHDIR

/* Define to 1 if stat needs help when passed a directory name with a trailing
   slash */
#undef REPLACE_FUNC_STAT_DIR

/* Define to 1 if stat needs help when passed a file name with a trailing
   slash */
#undef REPLACE_FUNC_STAT_FILE

/* Define to 1 if open() should work around the inability to open a directory.
   */
#undef REPLACE_OPEN_DIRECTORY

/* Define to 1 if strerror(0) does not return a message implying success. */
#undef REPLACE_STRERROR_0

/* Define if vasnprintf exists but is overridden by gnulib. */
#undef REPLACE_VASNPRINTF

/* Define to 1 if your struct rusage only has time information. */
#undef RUSAGE_TIMES_ONLY

/* Define this to be the path separator for your system, as a character
   constant. */
#undef SEPCHAR

/* Define this to be the path separator for your system, as a string. */
#undef SEPCHAR_STR

/* Define this to be the path to the shell command interpreter. */
#undef SHELL_PATH

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'sig_atomic_t'. */
#undef SIG_ATOMIC_T_SUFFIX

/* The size of `int', as computed by sizeof. */
#undef SIZEOF_INT

/* The size of `int64_t', as computed by sizeof. */
#undef SIZEOF_INT64_T

/* The size of `long', as computed by sizeof. */
#undef SIZEOF_LONG

/* The size of `long double', as computed by sizeof. */
#undef SIZEOF_LONG_DOUBLE

/* The size of `long long', as computed by sizeof. */
#undef SIZEOF_LONG_LONG

/* The size of `short', as computed by sizeof. */
#undef SIZEOF_SHORT

/* The size of `void *', as computed by sizeof. */
#undef SIZEOF_VOID_P

/* Define as the maximum value of type 'size_t', if the system doesn't define
   it. */
#ifndef SIZE_MAX
# undef SIZE_MAX
#endif

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'size_t'. */
#undef SIZE_T_SUFFIX

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
#undef STACK_DIRECTION

/* Define to 1 if the `S_IS*' macros in <sys/stat.h> do not work properly. */
#undef STAT_MACROS_BROKEN

/* Define to 1 if you have the ANSI C header files. */
#undef STDC_HEADERS

/* Define to 1 if strerror_r returns char *. */
#undef STRERROR_R_CHAR_P

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
#undef TM_IN_SYS_TIME

/* Define to 1 if the UMFPACK Complex solver allows matrix and RHS to be split
   independently. */
#undef UMFPACK_SEPARATE_SPLIT

/* Define to 1 if unlink() on a parent directory may succeed */
#undef UNLINK_PARENT_BUG

/* Define to 1 if using 64-bit integers for array dimensions and indexing. */
#undef USE_64_BIT_IDX_T

/* Define to 1 to use atomic operations for reference counting. */
#undef USE_ATOMIC_REFCOUNT

/* Define to 1 if BLAS functions need to be wrapped (potentially needed for
   64-bit OSX only). */
#undef USE_BLASWRAP

/* Define to 1 to use the readline library. */
#undef USE_READLINE

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# undef _ALL_SOURCE
#endif
/* Enable general extensions on OS X.  */
#ifndef _DARWIN_C_SOURCE
# undef _DARWIN_C_SOURCE
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# undef _GNU_SOURCE
#endif
/* Use GNU style printf and scanf.  */
#ifndef __USE_MINGW_ANSI_STDIO
# undef __USE_MINGW_ANSI_STDIO
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# undef _POSIX_PTHREAD_SEMANTICS
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# undef _TANDEM_SOURCE
#endif
/* Enable X/Open extensions if necessary.  HP-UX 11.11 defines
   mbstate_t only if _XOPEN_SOURCE is defined to 500, regardless of
   whether compiling with -Ae or -D_HPUX_SOURCE=1.  */
#ifndef _XOPEN_SOURCE
# undef _XOPEN_SOURCE
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# undef __EXTENSIONS__
#endif


/* Define to 1 if unordered_map requires the use of tr1 namespace. */
#undef USE_UNORDERED_MAP_WITH_TR1

/* Version number of package */
#undef VERSION

/* Define to 1 if unsetenv returns void instead of int. */
#undef VOID_UNSETENV

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wchar_t'. */
#undef WCHAR_T_SUFFIX

/* Define if WSAStartup is needed. */
#undef WINDOWS_SOCKETS

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wint_t'. */
#undef WINT_T_SUFFIX

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
#  undef WORDS_BIGENDIAN
# endif
#endif

/* Define to 1 if `lex' declares `yytext' as a `char *' by default, not a
   `char[]'. */
#undef YYTEXT_POINTER


#if defined (__cplusplus)
extern "C" {
#endif
#if HAVE_EXP2 && ! HAVE_DECL_EXP2
double exp2 (double);
#endif
#if HAVE_ROUND && ! HAVE_DECL_ROUND
double round (double);
#endif
#if HAVE_TGAMMA && ! HAVE_DECL_TGAMMA
double tgamma (double);
#endif
#if defined (__cplusplus)
}
#endif


/* Enable large inode numbers on Mac OS X 10.5. */
#undef _DARWIN_USE_64_BIT_INODE

/* Number of bits in a file offset, on hosts where this is settable. */
#undef _FILE_OFFSET_BITS

/* Define to 1 if Gnulib overrides 'struct stat' on Windows so that struct
   stat.st_size becomes 64-bit. */
#undef _GL_WINDOWS_64_BIT_ST_SIZE

/* Define to 1 if using HDF5 dll (Win32). */
#undef _HDF5USEDLL_

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
#undef _LARGEFILE_SOURCE

/* Define for large files, on AIX-style hosts. */
#undef _LARGE_FILES

/* Define to 1 if on MINIX. */
#undef _MINIX

/* Define to 1 to make NetBSD features available. MINIX 3 needs this. */
#undef _NETBSD_SOURCE

/* The _Noreturn keyword of C11.  */
#if ! (defined _Noreturn \
       || (defined __STDC_VERSION__ && 201112 <= __STDC_VERSION__))
# if (3 <= __GNUC__ || (__GNUC__ == 2 && 8 <= __GNUC_MINOR__) \
      || 0x5110 <= __SUNPRO_C)
#  define _Noreturn __attribute__ ((__noreturn__))
# elif defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn
# endif
#endif


/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
#undef _POSIX_1_SOURCE

/* Define to 1 in order to get the POSIX compatible declarations of socket
   functions. */
#undef _POSIX_PII_SOCKET

/* Define to 1 if you need to in order for 'stat' and other things to work. */
#undef _POSIX_SOURCE

/* Define for Solaris 2.5.1 so the uint64_t typedef from <sys/synch.h>,
   <pthread.h>, or <semaphore.h> is not used. If the typedef were allowed, the
   #define below would cause a syntax error. */
#undef _UINT64_T

/* Define to 1 if _USE_MATH_DEFINES is required to get math constants like
   M_LN2. */
#undef _USE_MATH_DEFINES

/* Define to 0x0403 to access InitializeCriticalSectionAndSpinCount. */
#undef _WIN32_WINNT

/* Define to rpl_ if the getopt replacement functions and variables should be
   used. */
#undef __GETOPT_PREFIX

/* Define to 1 if your version of GNU libc has buggy inline assembly code for
   math functions like exp. */
#undef __NO_MATH_INLINES

/* Please see the Gnulib manual for how to use these macros.

   Suppress extern inline with HP-UX cc, as it appears to be broken; see
   <http://lists.gnu.org/archive/html/bug-texinfo/2013-02/msg00030.html>.

   Suppress extern inline with Sun C in standards-conformance mode, as it
   mishandles inline functions that call each other.  E.g., for 'inline void f
   (void) { } inline void g (void) { f (); }', c99 incorrectly complains
   'reference to static identifier "f" in extern inline function'.
   This bug was observed with Sun C 5.12 SunOS_i386 2011/11/16.

   Suppress extern inline (with or without __attribute__ ((__gnu_inline__)))
   on configurations that mistakenly use 'static inline' to implement
   functions or macros in standard C headers like <ctype.h>.  For example,
   if isdigit is mistakenly implemented via a static inline function,
   a program containing an extern inline function that calls isdigit
   may not work since the C standard prohibits extern inline functions
   from calling static functions.  This bug is known to occur on:

     OS X 10.8 and earlier; see:
     http://lists.gnu.org/archive/html/bug-gnulib/2012-12/msg00023.html

     DragonFly; see
     http://muscles.dragonflybsd.org/bulk/bleeding-edge-potential/latest-per-pkg/ah-tty-0.3.12.log

     FreeBSD; see:
     http://lists.gnu.org/archive/html/bug-gnulib/2014-07/msg00104.html

   OS X 10.9 has a macro __header_inline indicating the bug is fixed for C and
   for clang but remains for g++; see <http://trac.macports.org/ticket/41033>.
   Assume DragonFly and FreeBSD will be similar.  */
#if (((defined __APPLE__ && defined __MACH__) \
      || defined __DragonFly__ || defined __FreeBSD__) \
     && (defined __header_inline \
         ? (defined __cplusplus && defined __GNUC_STDC_INLINE__ \
            && ! defined __clang__) \
         : ((! defined _DONT_USE_CTYPE_INLINE_ \
             && (defined __GNUC__ || defined __cplusplus)) \
            || (defined _FORTIFY_SOURCE && 0 < _FORTIFY_SOURCE \
                && defined __GNUC__ && ! defined __cplusplus))))
# define _GL_EXTERN_INLINE_STDHEADER_BUG
#endif
#if ((__GNUC__ \
      ? defined __GNUC_STDC_INLINE__ && __GNUC_STDC_INLINE__ \
      : (199901L <= __STDC_VERSION__ \
         && !defined __HP_cc \
         && !(defined __SUNPRO_C && __STDC__))) \
     && !defined _GL_EXTERN_INLINE_STDHEADER_BUG)
# define _GL_INLINE inline
# define _GL_EXTERN_INLINE extern inline
# define _GL_EXTERN_INLINE_IN_USE
#elif (2 < __GNUC__ + (7 <= __GNUC_MINOR__) && !defined __STRICT_ANSI__ \
       && !defined _GL_EXTERN_INLINE_STDHEADER_BUG)
# if defined __GNUC_GNU_INLINE__ && __GNUC_GNU_INLINE__
   /* __gnu_inline__ suppresses a GCC 4.2 diagnostic.  */
#  define _GL_INLINE extern inline __attribute__ ((__gnu_inline__))
# else
#  define _GL_INLINE extern inline
# endif
# define _GL_EXTERN_INLINE extern
# define _GL_EXTERN_INLINE_IN_USE
#else
# define _GL_INLINE static _GL_UNUSED
# define _GL_EXTERN_INLINE static _GL_UNUSED
#endif

/* In GCC, suppress bogus "no previous prototype for 'FOO'"
   and "no previous declaration for 'FOO'" diagnostics,
   when FOO is an inline function in the header; see
   <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=54113> and
   <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63877>.  */
#if 4 < __GNUC__ + (6 <= __GNUC_MINOR__)
# if defined __GNUC_STDC_INLINE__ && __GNUC_STDC_INLINE__
#  define _GL_INLINE_HEADER_CONST_PRAGMA
# else
#  define _GL_INLINE_HEADER_CONST_PRAGMA \
     _Pragma ("GCC diagnostic ignored \"-Wsuggest-attribute=const\"")
# endif
# define _GL_INLINE_HEADER_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wmissing-prototypes\"") \
    _Pragma ("GCC diagnostic ignored \"-Wmissing-declarations\"") \
    _GL_INLINE_HEADER_CONST_PRAGMA
# define _GL_INLINE_HEADER_END \
    _Pragma ("GCC diagnostic pop")
#else
# define _GL_INLINE_HEADER_BEGIN
# define _GL_INLINE_HEADER_END
#endif

/* Define to a replacement function name for fnmatch(). */
#undef fnmatch

/* Define to `int' if <sys/types.h> doesn't define. */
#undef gid_t

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
#undef inline
#endif

/* Define to the type of a signed integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
#undef int64_t

/* Define to long or long long if <stdint.h> and <inttypes.h> don't define. */
#undef intmax_t

/* Work around a bug in Apple GCC 4.0.1 build 5465: In C99 mode, it supports
   the ISO C 99 semantics of 'extern inline' (unlike the GNU C semantics of
   earlier versions), but does not display it by setting __GNUC_STDC_INLINE__.
   __APPLE__ && __MACH__ test for Mac OS X.
   __APPLE_CC__ tests for the Apple compiler and its version.
   __STDC_VERSION__ tests for the C99 mode.  */
#if defined __APPLE__ && defined __MACH__ && __APPLE_CC__ >= 5465 && !defined __cplusplus && __STDC_VERSION__ >= 199901L && !defined __GNUC_STDC_INLINE__
# define __GNUC_STDC_INLINE__ 1
#endif

/* Define to a type if <wchar.h> does not define. */
#undef mbstate_t

/* Define to `int' if <sys/types.h> does not define. */
#undef mode_t

/* Define to the name of the strftime replacement function. */
#undef my_strftime

/* Define to the type of st_nlink in struct stat, or a supertype. */
#undef nlink_t

/* Define to `long int' if <sys/types.h> does not define. */
#undef off_t

/* Define to `int' if <sys/types.h> does not define. */
#undef pid_t

/* Define as the type of the result of subtracting two pointers, if the system
   doesn't define it. */
#undef ptrdiff_t

/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#undef restrict
/* Work around a bug in Sun C++: it does not support _Restrict or
   __restrict__, even though the corresponding Sun C compiler ends up with
   "#define restrict _Restrict" or "#define restrict __restrict__" in the
   previous line.  Perhaps some future version of Sun C++ will work with
   restrict; if so, hopefully it defines __RESTRICT like Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
# define __restrict__
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
#undef size_t

/* type to use in place of socklen_t if not defined */
#undef socklen_t

/* Define as a signed type of the same size as size_t. */
#undef ssize_t

/* Define to `int' if <sys/types.h> doesn't define. */
#undef uid_t

/* Define to the type of an unsigned integer type of width exactly 64 bits if
   such a type exists and the standard includes do not define it. */
#undef uint64_t

/* Define as a marker that can be attached to declarations that might not
    be used.  This helps to reduce warnings, such as from
    GCC -Wunused-parameter.  */
#if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
# define _GL_UNUSED __attribute__ ((__unused__))
#else
# define _GL_UNUSED
#endif
/* The name _UNUSED_PARAMETER_ is an earlier spelling, although the name
   is a misnomer outside of parameter lists.  */
#define _UNUSED_PARAMETER_ _GL_UNUSED

/* gcc supports the "unused" attribute on possibly unused labels, and
   g++ has since version 4.5.  Note to support C++ as well as C,
   _GL_UNUSED_LABEL should be used with a trailing ;  */
#if !defined __cplusplus || __GNUC__ > 4 \
    || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
# define _GL_UNUSED_LABEL _GL_UNUSED
#else
# define _GL_UNUSED_LABEL
#endif

/* The __pure__ attribute was added in gcc 2.96.  */
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 96)
# define _GL_ATTRIBUTE_PURE __attribute__ ((__pure__))
#else
# define _GL_ATTRIBUTE_PURE /* empty */
#endif

/* The __const__ attribute was added in gcc 2.95.  */
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95)
# define _GL_ATTRIBUTE_CONST __attribute__ ((__const__))
#else
# define _GL_ATTRIBUTE_CONST /* empty */
#endif


#include "oct-conf-post.h"
