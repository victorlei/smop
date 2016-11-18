/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#if !defined (octave_oct_locbuf_h)
#define octave_oct_locbuf_h 1

#include <cstddef>
#include "oct-cmplx.h"

// The default local buffer simply encapsulates an *array* pointer
// that gets deleted automatically.  For common POD types, we provide
// specializations.

template <class T>
class octave_local_buffer
{
public:
  octave_local_buffer (size_t size)
    : data (0)
  {
    if (size)
      data = new T [size];
  }
  ~octave_local_buffer (void) { delete [] data; }
  operator T *() const { return data; }

private:
  T *data;

  // No copying!
  octave_local_buffer (const octave_local_buffer&);
  octave_local_buffer& operator = (const octave_local_buffer&);
};

// For buffers of POD types, we'll be smarter.  There is one thing
// that differentiates a local buffer from a dynamic array - the local
// buffers, if not manipulated improperly, have a FIFO semantics,
// meaning that if buffer B is allocated after buffer A, B *must* be
// deallocated before A.  This is *guaranteed* if you use local buffer
// exclusively through the OCTAVE_LOCAL_BUFFER macro, because the C++
// standard requires that explicit local objects be destroyed in
// reverse order of declaration.  Therefore, we can avoid memory
// fragmentation by allocating fairly large chunks of memory and
// serving local buffers from them in a stack-like manner.  The first
// returning buffer in previous chunk will be responsible for
// deallocating the chunk.

class octave_chunk_buffer
{
public:

  OCTAVE_API octave_chunk_buffer (size_t size);

  OCTAVE_API virtual ~octave_chunk_buffer (void);

  char *data (void) const { return dat; }

  static OCTAVE_API void clear (void);

private:

  // The number of bytes we allocate for each large chunk of memory we
  // manage.
  static const size_t chunk_size;

  // Pointer to the end end of the last allocation.
  static char *top;

  // Pointer to the current active chunk.
  static char *chunk;

  // The number of bytes remaining in the active chunk.
  static size_t left;

  // The number of active allocations.
  static size_t active;

  // Pointer to the current chunk.
  char *cnk;

  // Pointer to the beginning of the most recent allocation.
  char *dat;

  // No copying!
  octave_chunk_buffer (const octave_chunk_buffer&);
  octave_chunk_buffer& operator = (const octave_chunk_buffer&);
};

// This specializes octave_local_buffer to use the chunked buffer
// mechanism for POD types.
#define SPECIALIZE_POD_BUFFER(TYPE) \
template <> \
class octave_local_buffer<TYPE> : private octave_chunk_buffer \
{ \
public: \
  octave_local_buffer (size_t size) \
    : octave_chunk_buffer (size * sizeof (TYPE)) { } \
 \
  operator TYPE *() const \
  { \
    return reinterpret_cast<TYPE *> (this->data ()); \
  } \
}

SPECIALIZE_POD_BUFFER (bool);
SPECIALIZE_POD_BUFFER (char);
SPECIALIZE_POD_BUFFER (unsigned short);
SPECIALIZE_POD_BUFFER (short);
SPECIALIZE_POD_BUFFER (int);
SPECIALIZE_POD_BUFFER (unsigned int);
SPECIALIZE_POD_BUFFER (long);
SPECIALIZE_POD_BUFFER (unsigned long);
SPECIALIZE_POD_BUFFER (float);
SPECIALIZE_POD_BUFFER (double);
// FIXME: Are these guaranteed to be POD and satisfy alignment?
SPECIALIZE_POD_BUFFER (Complex);
SPECIALIZE_POD_BUFFER (FloatComplex);
// MORE ?

// All pointers and const pointers are also POD types.
template <class T>
class octave_local_buffer<T *> : private octave_chunk_buffer
{
public:
  octave_local_buffer (size_t size)
    : octave_chunk_buffer (size * sizeof (T *))
  { }

  operator T **() const { return reinterpret_cast<T **> (this->data ()); }
};

template <class T>
class octave_local_buffer<const T *> : private octave_chunk_buffer
{
public:
  octave_local_buffer (size_t size)
    : octave_chunk_buffer (size * sizeof (const T *))
  { }

  operator const T **() const
  {
    return reinterpret_cast<const T **> (this->data ());
  }
};

// If the compiler supports dynamic stack arrays, we can use the
// attached hack to place small buffer arrays on the stack. It may be
// even faster than our obstack-like optimization, but is dangerous
// because stack is a very limited resource, so we disable it.

#if 0 // defined (HAVE_DYNAMIC_AUTO_ARRAYS)

// Maximum buffer size (in bytes) to be placed on the stack.

#define OCTAVE_LOCAL_BUFFER_MAX_STACK_SIZE 8192

// If we have automatic arrays, we use an automatic array if the size
// is small enough.  To avoid possibly evaluating 'size' multiple
// times, we first cache it.  Note that we always construct both the
// stack array and the octave_local_buffer object, but only one of
// them will be nonempty.

#define OCTAVE_LOCAL_BUFFER(T, buf, size) \
  const size_t _bufsize_ ## buf = size; \
  const bool _lbufaut_ ## buf = _bufsize_ ## buf * sizeof (T) \
     <= OCTAVE_LOCAL_BUFFER_MAX_STACK_SIZE; \
  T _bufaut_ ## buf [_lbufaut_ ## buf ? _bufsize_ ## buf : 0]; \
  octave_local_buffer<T> _bufheap_ ## buf \
    (!_lbufaut_ ## buf ? _bufsize_ ## buf : 0); \
  T *buf = _lbufaut_ ## buf \
    ? _bufaut_ ## buf : static_cast<T *> (_bufheap_ ## buf)

#else

// If we don't have automatic arrays, we simply always use
// octave_local_buffer.

#define OCTAVE_LOCAL_BUFFER(T, buf, size) \
  octave_local_buffer<T> _buffer_ ## buf (size); \
  T *buf = _buffer_ ## buf

#endif

// Note: we use weird variables in the for loop to avoid warnings
// about shadowed parameters.

#define OCTAVE_LOCAL_BUFFER_INIT(T, buf, size, value) \
  OCTAVE_LOCAL_BUFFER (T, buf, size); \
  for (size_t _buf_iter = 0, _buf_size = size; \
        _buf_iter < _buf_size; _buf_iter++) \
    buf[_buf_iter] = value

#endif

