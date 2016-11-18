EXTRA_DIST += \
  array/module.mk

ARRAY_INC = \
  array/Array.h \
  array/Array-util.h \
  array/boolMatrix.h \
  array/boolNDArray.h \
  array/boolSparse.h \
  array/CColVector.h \
  array/CDiagMatrix.h \
  array/chMatrix.h \
  array/chNDArray.h \
  array/CMatrix.h \
  array/CNDArray.h \
  array/CRowVector.h \
  array/CSparse.h \
  array/dColVector.h \
  array/dDiagMatrix.h \
  array/DiagArray2.h \
  array/dim-vector.h \
  array/dMatrix.h \
  array/dNDArray.h \
  array/dRowVector.h \
  array/dSparse.h \
  array/fCColVector.h \
  array/fCDiagMatrix.h \
  array/fCMatrix.h \
  array/fCNDArray.h \
  array/fColVector.h \
  array/fCRowVector.h \
  array/fDiagMatrix.h \
  array/fMatrix.h \
  array/fNDArray.h \
  array/fRowVector.h \
  array/idx-vector.h \
  array/int16NDArray.h \
  array/int32NDArray.h \
  array/int64NDArray.h \
  array/int8NDArray.h \
  array/intNDArray.h \
  array/MArray-decl.h \
  array/MArray-defs.h \
  array/MArray.h \
  array/Matrix.h \
  array/MatrixType.h \
  array/MDiagArray2.h \
  array/MSparse-defs.h \
  array/MSparse.h \
  array/PermMatrix.h \
  array/Range.h \
  array/Sparse.h \
  array/uint16NDArray.h \
  array/uint32NDArray.h \
  array/uint64NDArray.h \
  array/uint8NDArray.h

ARRAY_SRC = \
  array/Array-b.cc \
  array/Array-C.cc \
  array/Array-ch.cc \
  array/Array-d.cc \
  array/Array-f.cc \
  array/Array-fC.cc \
  array/Array-i.cc \
  array/Array-idx-vec.cc \
  array/Array-s.cc \
  array/Array-str.cc \
  array/Array-util.cc \
  array/Array-voidp.cc \
  array/boolMatrix.cc \
  array/boolNDArray.cc \
  array/boolSparse.cc \
  array/CColVector.cc \
  array/CDiagMatrix.cc \
  array/chMatrix.cc \
  array/chNDArray.cc \
  array/CMatrix.cc \
  array/CNDArray.cc \
  array/CRowVector.cc \
  array/CSparse.cc \
  array/dColVector.cc \
  array/dDiagMatrix.cc \
  array/dim-vector.cc \
  array/dMatrix.cc \
  array/dNDArray.cc \
  array/dRowVector.cc \
  array/dSparse.cc \
  array/fCColVector.cc \
  array/fCDiagMatrix.cc \
  array/fCMatrix.cc \
  array/fCNDArray.cc \
  array/fColVector.cc \
  array/fCRowVector.cc \
  array/fDiagMatrix.cc \
  array/fMatrix.cc \
  array/fNDArray.cc \
  array/fRowVector.cc \
  array/idx-vector.cc \
  array/int16NDArray.cc \
  array/int32NDArray.cc \
  array/int64NDArray.cc \
  array/int8NDArray.cc \
  array/MArray-C.cc \
  array/MArray-d.cc \
  array/MArray-f.cc \
  array/MArray-fC.cc \
  array/MArray-i.cc \
  array/MArray-s.cc \
  array/MatrixType.cc \
  array/MSparse-C.cc \
  array/MSparse-d.cc \
  array/PermMatrix.cc \
  array/Range.cc \
  array/Sparse-b.cc \
  array/Sparse-C.cc \
  array/Sparse-d.cc \
  array/uint16NDArray.cc \
  array/uint32NDArray.cc \
  array/uint64NDArray.cc \
  array/uint8NDArray.cc

TEMPLATE_SRC += \
  array/Array.cc \
  array/DiagArray2.cc \
  array/intNDArray.cc \
  array/MArray.cc \
  array/MDiagArray2.cc \
  array/MSparse.cc \
  array/Sparse.cc

noinst_LTLIBRARIES += array/libarray.la

array_libarray_la_SOURCES = $(ARRAY_SRC)
array_libarray_la_CPPFLAGS = \
  $(liboctave_la_CPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_la_LIBADD += array/libarray.la
