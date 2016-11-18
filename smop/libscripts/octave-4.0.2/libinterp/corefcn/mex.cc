/*

Copyright (C) 2006-2015 John W. Eaton

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

#include <config.h>

#include <cfloat>
#include <csetjmp>
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cctype>

#include <set>

#include "f77-fcn.h"
#include "lo-ieee.h"
#include "oct-locbuf.h"

#include "Cell.h"
// mxArray must be declared as a class before including mexproto.h.
#include "mxarray.h"
#include "mexproto.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-mex-fcn.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "graphics.h"

// #define DEBUG 1

static void
xfree (void *ptr)
{
  ::free (ptr);
}

static mwSize
max_str_len (mwSize m, const char **str)
{
  int max_len = 0;

  for (mwSize i = 0; i < m; i++)
    {
      mwSize tmp = strlen (str[i]);

      if (tmp > max_len)
        max_len = tmp;
    }

  return max_len;
}

static int
valid_key (const char *key)
{
  int retval = 0;

  int nel = strlen (key);

  if (nel > 0)
    {
      if (isalpha (key[0]))
        {
          for (int i = 1; i < nel; i++)
            {
              if (! (isalnum (key[i]) || key[i] == '_'))
                goto done;
            }

          retval = 1;
        }
    }

done:

  return retval;
}

// ------------------------------------------------------------------

void
mxArray_base::error (const char *msg) const
{
  // FIXME
  ::error ("%s", msg);
}

static mwIndex
calc_single_subscript_internal (mwSize ndims, const mwSize *dims,
                                mwSize nsubs, const mwIndex *subs)
{
  mwIndex retval = 0;

  switch (nsubs)
    {
    case 0:
      break;

    case 1:
      retval = subs[0];
      break;

    default:
      {
        // Both nsubs and ndims should be at least 2 here.

        mwSize n = nsubs <= ndims ? nsubs : ndims;

        retval = subs[--n];

        while (--n >= 0)
          retval = dims[n] * retval + subs[n];
      }
      break;
    }

  return retval;
}

// The object that handles values pass to MEX files from Octave.  Some
// methods in this class may set mutate_flag to TRUE to tell the
// mxArray class to convert to the Matlab-style representation and
// then invoke the method on that object instead (for example, getting
// a pointer to real or imaginary data from a complex object requires
// a mutation but getting a pointer to real data from a real object
// does not).  Changing the representation causes a copy so we try to
// avoid it unless it is really necessary.  Once the conversion
// happens, we delete this representation, so the conversion can only
// happen once per call to a MEX file.

static inline void *maybe_mark_foreign (void *ptr);

class mxArray_octave_value : public mxArray_base
{
public:

  mxArray_octave_value (const octave_value& ov)
    : mxArray_base (), val (ov), mutate_flag (false),
      id (mxUNKNOWN_CLASS), class_name (0), ndims (-1), dims (0) { }

  mxArray_base *dup (void) const { return new mxArray_octave_value (*this); }

  mxArray *as_mxArray (void) const
  {
    mxArray *retval = val.as_mxArray ();

    // RETVAL is assumed to be an mxArray_matlab object.  Should we
    // assert that condition here?

    if (retval)
      {
        // Preserve cached values of class name and dimensions in case
        // they will be used after we mutate.

        // set_class_name will handle deleting class name that comes
        // from as_mxArray conversion function.

        if (class_name)
          {
            retval->set_class_name (class_name);

            class_name = 0;
          }

        if (dims)
          {
            mwSize *xdims = retval->get_dimensions ();

            mxFree (xdims);

            retval->set_dimensions (dims, ndims);

            dims = 0;
          }
      }

    return retval;
  }

  ~mxArray_octave_value (void)
  {
    mxFree (class_name);
    mxFree (dims);
  }

  bool is_octave_value (void) const { return true; }

  int is_cell (void) const { return val.is_cell (); }

  int is_char (void) const { return val.is_string (); }

  int is_complex (void) const { return val.is_complex_type (); }

  int is_double (void) const { return val.is_double_type (); }

  int is_function_handle (void) const { return val.is_function_handle (); }

  int is_int16 (void) const { return val.is_int16_type (); }

  int is_int32 (void) const { return val.is_int32_type (); }

  int is_int64 (void) const { return val.is_int64_type (); }

  int is_int8 (void) const { return val.is_int8_type (); }

  int is_logical (void) const { return val.is_bool_type (); }

  int is_numeric (void) const { return val.is_numeric_type (); }

  int is_single (void) const { return val.is_single_type (); }

  int is_sparse (void) const { return val.is_sparse_type (); }

  int is_struct (void) const { return val.is_map (); }

  int is_uint16 (void) const { return val.is_uint16_type (); }

  int is_uint32 (void) const { return val.is_uint32_type (); }

  int is_uint64 (void) const { return val.is_uint64_type (); }

  int is_uint8 (void) const { return val.is_uint8_type (); }

  int is_range (void) const { return val.is_range (); }

  int is_real_type (void) const { return val.is_real_type (); }

  int is_logical_scalar_true (void) const
  {
    return (is_logical_scalar () && val.is_true ());
  }

  mwSize get_m (void) const { return val.rows (); }

  mwSize get_n (void) const
  {
    mwSize n = 1;

    // Force dims and ndims to be cached.
    get_dimensions ();

    for (mwIndex i = ndims - 1; i > 0; i--)
      n *= dims[i];

    return n;
  }

  mwSize *get_dimensions (void) const
  {
    if (! dims)
      {
        ndims = val.ndims ();

        dims = static_cast<mwSize *> (mxArray::malloc (ndims
                                                       * sizeof (mwSize)));

        dim_vector dv = val.dims ();

        for (mwIndex i = 0; i < ndims; i++)
          dims[i] = dv(i);
      }

    return dims;
  }

  mwSize get_number_of_dimensions (void) const
  {
    // Force dims and ndims to be cached.
    get_dimensions ();

    return ndims;
  }

  void set_m (mwSize /*m*/) { request_mutation (); }

  void set_n (mwSize /*n*/) { request_mutation (); }

  void set_dimensions (mwSize */*dims_arg*/, mwSize /*ndims_arg*/)
  {
    request_mutation ();
  }

  mwSize get_number_of_elements (void) const { return val.numel (); }

  int is_empty (void) const { return val.is_empty (); }

  mxClassID get_class_id (void) const
  {
    id = mxUNKNOWN_CLASS;

    std::string cn = val.class_name ();

    if (cn == "cell")
      id = mxCELL_CLASS;
    else if (cn == "struct")
      id = mxSTRUCT_CLASS;
    else if (cn == "logical")
      id = mxLOGICAL_CLASS;
    else if (cn == "char")
      id = mxCHAR_CLASS;
    else if (cn == "double")
      id = mxDOUBLE_CLASS;
    else if (cn == "single")
      id = mxSINGLE_CLASS;
    else if (cn == "int8")
      id = mxINT8_CLASS;
    else if (cn == "uint8")
      id = mxUINT8_CLASS;
    else if (cn == "int16")
      id = mxINT16_CLASS;
    else if (cn == "uint16")
      id = mxUINT16_CLASS;
    else if (cn == "int32")
      id = mxINT32_CLASS;
    else if (cn == "uint32")
      id = mxUINT32_CLASS;
    else if (cn == "int64")
      id = mxINT64_CLASS;
    else if (cn == "uint64")
      id = mxUINT64_CLASS;
    else if (cn == "function_handle")
      id = mxFUNCTION_CLASS;

    return id;
  }

  const char *get_class_name (void) const
  {
    if (! class_name)
      {
        std::string s = val.class_name ();
        class_name = mxArray::strsave (s.c_str ());
      }

    return class_name;
  }

  // Not allowed.
  void set_class_name (const char */*name_arg*/) { request_mutation (); }

  mxArray *get_cell (mwIndex /*idx*/) const
  {
    request_mutation ();
    return 0;
  }

  // Not allowed.
  void set_cell (mwIndex /*idx*/, mxArray */*val*/) { request_mutation (); }

  double get_scalar (void) const { return val.scalar_value (true); }

  void *get_data (void) const
  {
    void *retval = val.mex_get_data ();

    if (retval)
      maybe_mark_foreign (retval);
    else
      request_mutation ();

    return retval;
  }

  void *get_imag_data (void) const
  {
    void *retval = 0;

    if (is_numeric () && is_real_type ())
      retval = 0;
    else
      request_mutation ();

    return retval;
  }

  // Not allowed.
  void set_data (void */*pr*/) { request_mutation (); }

  // Not allowed.
  void set_imag_data (void */*pi*/) { request_mutation (); }

  mwIndex *get_ir (void) const
  {
    return static_cast<mwIndex *> (maybe_mark_foreign (val.mex_get_ir ()));
  }

  mwIndex *get_jc (void) const
  {
    return static_cast<mwIndex *> (maybe_mark_foreign (val.mex_get_jc ()));
  }

  mwSize get_nzmax (void) const { return val.nzmax (); }

  // Not allowed.
  void set_ir (mwIndex */*ir*/) { request_mutation (); }

  // Not allowed.
  void set_jc (mwIndex */*jc*/) { request_mutation (); }

  // Not allowed.
  void set_nzmax (mwSize /*nzmax*/) { request_mutation (); }

  // Not allowed.
  int add_field (const char */*key*/)
  {
    request_mutation ();
    return 0;
  }

  // Not allowed.
  void remove_field (int /*key_num*/) { request_mutation (); }

  mxArray *get_field_by_number (mwIndex /*index*/, int /*key_num*/) const
  {
    request_mutation ();
    return 0;
  }

  // Not allowed.
  void set_field_by_number (mwIndex /*index*/, int /*key_num*/,
                            mxArray */*val*/)
  {
    request_mutation ();
  }

  int get_number_of_fields (void) const { return val.nfields (); }

  const char *get_field_name_by_number (int /*key_num*/) const
  {
    request_mutation ();
    return 0;
  }

  int get_field_number (const char */*key*/) const
  {
    request_mutation ();
    return 0;
  }

  int get_string (char *buf, mwSize buflen) const
  {
    int retval = 1;

    mwSize nel = get_number_of_elements ();

    if (val.is_string () && nel < buflen)
      {
        charNDArray tmp = val.char_array_value ();

        const char *p = tmp.data ();

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = p[i];

        buf[nel] = 0;

        retval = 0;
      }

    return retval;
  }

  char *array_to_string (void) const
  {
    // FIXME: this is suposed to handle multi-byte character strings.

    char *buf = 0;

    if (val.is_string ())
      {
        mwSize nel = get_number_of_elements ();

        buf = static_cast<char *> (mxArray::malloc (nel + 1));

        if (buf)
          {
            charNDArray tmp = val.char_array_value ();

            const char *p = tmp.data ();

            for (mwIndex i = 0; i < nel; i++)
              buf[i] = p[i];

            buf[nel] = '\0';
          }
      }

    return buf;
  }

  mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const
  {
    // Force ndims, dims to be cached.
    get_dimensions ();

    return calc_single_subscript_internal (ndims, dims, nsubs, subs);
  }

  size_t get_element_size (void) const
  {
    // Force id to be cached.
    get_class_id ();

    switch (id)
      {
      case mxCELL_CLASS: return sizeof (mxArray *);
      case mxSTRUCT_CLASS: return sizeof (mxArray *);
      case mxLOGICAL_CLASS: return sizeof (mxLogical);
      case mxCHAR_CLASS: return sizeof (mxChar);
      case mxDOUBLE_CLASS: return sizeof (double);
      case mxSINGLE_CLASS: return sizeof (float);
      case mxINT8_CLASS: return 1;
      case mxUINT8_CLASS: return 1;
      case mxINT16_CLASS: return 2;
      case mxUINT16_CLASS: return 2;
      case mxINT32_CLASS: return 4;
      case mxUINT32_CLASS: return 4;
      case mxINT64_CLASS: return 8;
      case mxUINT64_CLASS: return 8;
      case mxFUNCTION_CLASS: return 0;
      default: return 0;
      }
  }

  bool mutation_needed (void) const { return mutate_flag; }

  void request_mutation (void) const
  {
    if (mutate_flag)
      panic_impossible ();

    mutate_flag = true;
  }

  mxArray *mutate (void) const { return as_mxArray (); }

  octave_value as_octave_value (void) const { return val; }

protected:

  mxArray_octave_value (const mxArray_octave_value& arg)
    : mxArray_base (arg), val (arg.val), mutate_flag (arg.mutate_flag),
      id (arg.id), class_name (mxArray::strsave (arg.class_name)),
      ndims (arg.ndims),
      dims (ndims > 0 ? static_cast<mwSize *>
                         (mxArray::malloc (ndims * sizeof (mwSize)))
                      : 0)
  {
    if (dims)
      {
        for (mwIndex i = 0; i < ndims; i++)
          dims[i] = arg.dims[i];
      }
  }

private:

  octave_value val;

  mutable bool mutate_flag;

  // Caching these does not cost much or lead to much duplicated
  // code.  For other things, we just request mutation to a
  // Matlab-style mxArray object.

  mutable mxClassID id;
  mutable char *class_name;
  mutable mwSize ndims;
  mutable mwSize *dims;

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_octave_value& operator = (const mxArray_octave_value&);
};

// The base class for the Matlab-style representation, used to handle
// things that are common to all Matlab-style objects.

class mxArray_matlab : public mxArray_base
{
protected:

  mxArray_matlab (mxClassID id_arg = mxUNKNOWN_CLASS)
    : mxArray_base (), class_name (0), id (id_arg), ndims (0), dims (0) { }

  mxArray_matlab (mxClassID id_arg, mwSize ndims_arg, const mwSize *dims_arg)
    : mxArray_base (), class_name (0), id (id_arg),
      ndims (ndims_arg < 2 ? 2 : ndims_arg),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    if (ndims_arg < 2)
      {
        dims[0] = 0;
        dims[1] = 0;
      }

    for (mwIndex i = 0; i < ndims_arg; i++)
      dims[i] = dims_arg[i];

    for (mwIndex i = ndims - 1; i > 1; i--)
      {
        if (dims[i] == 1)
          ndims--;
        else
          break;
      }
  }

  mxArray_matlab (mxClassID id_arg, const dim_vector& dv)
    : mxArray_base (), class_name (0), id (id_arg),
      ndims (dv.length ()),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    for (mwIndex i = 0; i < ndims; i++)
      dims[i] = dv(i);

    for (mwIndex i = ndims - 1; i > 1; i--)
      {
        if (dims[i] == 1)
          ndims--;
        else
          break;
      }
  }

  mxArray_matlab (mxClassID id_arg, mwSize m, mwSize n)
    : mxArray_base (), class_name (0), id (id_arg), ndims (2),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    dims[0] = m;
    dims[1] = n;
  }

public:

  ~mxArray_matlab (void)
  {
    mxFree (class_name);
    mxFree (dims);
  }

  int is_cell (void) const { return id == mxCELL_CLASS; }

  int is_char (void) const { return id == mxCHAR_CLASS; }

  int is_complex (void) const { return 0; }

  int is_double (void) const { return id == mxDOUBLE_CLASS; }

  int is_function_handle (void) const { return id == mxFUNCTION_CLASS; }

  int is_int16 (void) const { return id == mxINT16_CLASS; }

  int is_int32 (void) const { return id == mxINT32_CLASS; }

  int is_int64 (void) const { return id == mxINT64_CLASS; }

  int is_int8 (void) const { return id == mxINT8_CLASS; }

  int is_logical (void) const { return id == mxLOGICAL_CLASS; }

  int is_numeric (void) const
  {
    return (id == mxDOUBLE_CLASS || id == mxSINGLE_CLASS
            || id == mxINT8_CLASS || id == mxUINT8_CLASS
            || id == mxINT16_CLASS || id == mxUINT16_CLASS
            || id == mxINT32_CLASS || id == mxUINT32_CLASS
            || id == mxINT64_CLASS || id == mxUINT64_CLASS);
  }

  int is_single (void) const { return id == mxSINGLE_CLASS; }

  int is_sparse (void) const { return 0; }

  int is_struct (void) const { return id == mxSTRUCT_CLASS; }

  int is_uint16 (void) const { return id == mxUINT16_CLASS; }

  int is_uint32 (void) const { return id == mxUINT32_CLASS; }

  int is_uint64 (void) const { return id == mxUINT64_CLASS; }

  int is_uint8 (void) const { return id == mxUINT8_CLASS; }

  int is_logical_scalar_true (void) const
  {
    return (is_logical_scalar ()
            && static_cast<mxLogical *> (get_data ())[0] != 0);
  }

  mwSize get_m (void) const { return dims[0]; }

  mwSize get_n (void) const
  {
    mwSize n = 1;

    for (mwSize i = ndims - 1 ; i > 0 ; i--)
      n *= dims[i];

    return n;
  }

  mwSize *get_dimensions (void) const { return dims; }

  mwSize get_number_of_dimensions (void) const { return ndims; }

  void set_m (mwSize m) { dims[0] = m; }

  void set_n (mwSize n) { dims[1] = n; }

  void set_dimensions (mwSize *dims_arg, mwSize ndims_arg)
  {
    dims = dims_arg;
    ndims = ndims_arg;
  }

  mwSize get_number_of_elements (void) const
  {
    mwSize retval = dims[0];

    for (mwIndex i = 1; i < ndims; i++)
      retval *= dims[i];

    return retval;
  }

  int is_empty (void) const { return get_number_of_elements () == 0; }

  mxClassID get_class_id (void) const { return id; }

  const char *get_class_name (void) const
  {
    switch (id)
      {
      case mxCELL_CLASS: return "cell";
      case mxSTRUCT_CLASS: return "struct";
      case mxLOGICAL_CLASS: return "logical";
      case mxCHAR_CLASS: return "char";
      case mxDOUBLE_CLASS: return "double";
      case mxSINGLE_CLASS: return "single";
      case mxINT8_CLASS: return "int8";
      case mxUINT8_CLASS: return "uint8";
      case mxINT16_CLASS: return "int16";
      case mxUINT16_CLASS: return "uint16";
      case mxINT32_CLASS: return "int32";
      case mxUINT32_CLASS: return "uint32";
      case mxINT64_CLASS: return "int64";
      case mxUINT64_CLASS: return "uint64";
      case mxFUNCTION_CLASS: return "function_handle";
      default: return "unknown";
      }
  }

  void set_class_name (const char *name_arg)
  {
    mxFree (class_name);
    class_name = static_cast<char *> (mxArray::malloc (strlen (name_arg) + 1));
    strcpy (class_name, name_arg);
  }

  mxArray *get_cell (mwIndex /*idx*/) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_cell (mwIndex /*idx*/, mxArray */*val*/)
  {
    invalid_type_error ();
  }

  double get_scalar (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void *get_data (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void *get_imag_data (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_data (void */*pr*/)
  {
    invalid_type_error ();
  }

  void set_imag_data (void */*pi*/)
  {
    invalid_type_error ();
  }

  mwIndex *get_ir (void) const
  {
    invalid_type_error ();
    return 0;
  }

  mwIndex *get_jc (void) const
  {
    invalid_type_error ();
    return 0;
  }

  mwSize get_nzmax (void) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_ir (mwIndex */*ir*/)
  {
    invalid_type_error ();
  }

  void set_jc (mwIndex */*jc*/)
  {
    invalid_type_error ();
  }

  void set_nzmax (mwSize /*nzmax*/)
  {
    invalid_type_error ();
  }

  int add_field (const char */*key*/)
  {
    invalid_type_error ();
    return -1;
  }

  void remove_field (int /*key_num*/)
  {
    invalid_type_error ();
  }

  mxArray *get_field_by_number (mwIndex /*index*/, int /*key_num*/) const
  {
    invalid_type_error ();
    return 0;
  }

  void set_field_by_number (mwIndex /*index*/, int /*key_num*/,
                            mxArray */*val*/)
  {
    invalid_type_error ();
  }

  int get_number_of_fields (void) const
  {
    invalid_type_error ();
    return 0;
  }

  const char *get_field_name_by_number (int /*key_num*/) const
  {
    invalid_type_error ();
    return 0;
  }

  int get_field_number (const char */*key*/) const
  {
    return -1;
  }

  int get_string (char */*buf*/, mwSize /*buflen*/) const
  {
    invalid_type_error ();
    return 0;
  }

  char *array_to_string (void) const
  {
    invalid_type_error ();
    return 0;
  }

  mwIndex calc_single_subscript (mwSize nsubs, mwIndex *subs) const
  {
    return calc_single_subscript_internal (ndims, dims, nsubs, subs);
  }

  size_t get_element_size (void) const
  {
    switch (id)
      {
      case mxCELL_CLASS: return sizeof (mxArray *);
      case mxSTRUCT_CLASS: return sizeof (mxArray *);
      case mxLOGICAL_CLASS: return sizeof (mxLogical);
      case mxCHAR_CLASS: return sizeof (mxChar);
      case mxDOUBLE_CLASS: return sizeof (double);
      case mxSINGLE_CLASS: return sizeof (float);
      case mxINT8_CLASS: return 1;
      case mxUINT8_CLASS: return 1;
      case mxINT16_CLASS: return 2;
      case mxUINT16_CLASS: return 2;
      case mxINT32_CLASS: return 4;
      case mxUINT32_CLASS: return 4;
      case mxINT64_CLASS: return 8;
      case mxUINT64_CLASS: return 8;
      case mxFUNCTION_CLASS: return 0;
      default: return 0;
      }
  }

protected:

  mxArray_matlab (const mxArray_matlab& val)
    : mxArray_base (val), class_name (mxArray::strsave (val.class_name)),
      id (val.id), ndims (val.ndims),
      dims (static_cast<mwSize *> (mxArray::malloc (ndims * sizeof (mwSize))))
  {
    for (mwIndex i = 0; i < ndims; i++)
      dims[i] = val.dims[i];
  }

  dim_vector
  dims_to_dim_vector (void) const
  {
    mwSize nd = get_number_of_dimensions ();

    mwSize *d = get_dimensions ();

    dim_vector dv;
    dv.resize (nd);

    for (mwIndex i = 0; i < nd; i++)
      dv(i) = d[i];

    return dv;
  }

private:

  char *class_name;

  mxClassID id;

  mwSize ndims;
  mwSize *dims;

  void invalid_type_error (void) const
  {
    error ("invalid type for operation");
  }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_matlab& operator = (const mxArray_matlab&);
};

// Matlab-style numeric, character, and logical data.

class mxArray_number : public mxArray_matlab
{
public:

  mxArray_number (mxClassID id_arg, mwSize ndims_arg, const mwSize *dims_arg,
                  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, ndims_arg, dims_arg),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (flag == mxCOMPLEX ? mxArray::calloc (get_number_of_elements (),
                                               get_element_size ())
                            : 0) { }

  mxArray_number (mxClassID id_arg, const dim_vector& dv,
                  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, dv),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (flag == mxCOMPLEX ? mxArray::calloc (get_number_of_elements (),
                                               get_element_size ())
                            : 0) { }

  mxArray_number (mxClassID id_arg, mwSize m, mwSize n,
                  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, m, n),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (flag == mxCOMPLEX ? mxArray::calloc (get_number_of_elements (),
                                               get_element_size ())
                            : 0) { }

  mxArray_number (mxClassID id_arg, double val)
    : mxArray_matlab (id_arg, 1, 1),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    double *dpr = static_cast<double *> (pr);
    dpr[0] = val;
  }

  mxArray_number (mxClassID id_arg, mxLogical val)
    : mxArray_matlab (id_arg, 1, 1),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    mxLogical *lpr = static_cast<mxLogical *> (pr);
    lpr[0] = val;
  }

  mxArray_number (const char *str)
    : mxArray_matlab (mxCHAR_CLASS,
                      str ? (strlen (str) ? 1 : 0) : 0,
                      str ? strlen (str) : 0),
    pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
    pi (0)
  {
    mxChar *cpr = static_cast<mxChar *> (pr);
    mwSize nel = get_number_of_elements ();
    for (mwIndex i = 0; i < nel; i++)
      cpr[i] = str[i];
  }

  // FIXME: ???
  mxArray_number (mwSize m, const char **str)
    : mxArray_matlab (mxCHAR_CLASS, m, max_str_len (m, str)),
      pr (mxArray::calloc (get_number_of_elements (), get_element_size ())),
      pi (0)
  {
    mxChar *cpr = static_cast<mxChar *> (pr);

    mwSize *dv = get_dimensions ();

    mwSize nc = dv[1];

    for (mwIndex j = 0; j < m; j++)
      {
        const char *ptr = str[j];

        size_t tmp_len = strlen (ptr);

        for (size_t i = 0; i < tmp_len; i++)
          cpr[m*i+j] = static_cast<mxChar> (ptr[i]);

        for (size_t i = tmp_len; i < static_cast<size_t>(nc); i++)
          cpr[m*i+j] = static_cast<mxChar> (' ');
      }
  }

  mxArray_base *dup (void) const { return new mxArray_number (*this); }

  ~mxArray_number (void)
  {
    mxFree (pr);
    mxFree (pi);
  }

  int is_complex (void) const { return pi != 0; }

  double get_scalar (void) const
  {
    double retval = 0;

    switch (get_class_id ())
      {
      case mxLOGICAL_CLASS:
        retval = *(static_cast<bool *> (pr));
        break;

      case mxCHAR_CLASS:
        retval = *(static_cast<mxChar *> (pr));
        break;

      case mxSINGLE_CLASS:
        retval = *(static_cast<float *> (pr));
        break;

      case mxDOUBLE_CLASS:
        retval = *(static_cast<double *> (pr));
        break;

      case mxINT8_CLASS:
        retval = *(static_cast<int8_t *> (pr));
        break;

      case mxUINT8_CLASS:
        retval = *(static_cast<uint8_t *> (pr));
        break;

      case mxINT16_CLASS:
        retval = *(static_cast<int16_t *> (pr));
        break;

      case mxUINT16_CLASS:
        retval = *(static_cast<uint16_t *> (pr));
        break;

      case mxINT32_CLASS:
        retval = *(static_cast<int32_t *> (pr));
        break;

      case mxUINT32_CLASS:
        retval = *(static_cast<uint32_t *> (pr));
        break;

      case mxINT64_CLASS:
        retval = *(static_cast<int64_t *> (pr));
        break;

      case mxUINT64_CLASS:
        retval = *(static_cast<uint64_t *> (pr));
        break;

      default:
        panic_impossible ();
      }

    return retval;
  }

  void *get_data (void) const { return pr; }

  void *get_imag_data (void) const { return pi; }

  void set_data (void *pr_arg) { pr = pr_arg; }

  void set_imag_data (void *pi_arg) { pi = pi_arg; }

  int get_string (char *buf, mwSize buflen) const
  {
    int retval = 0;

    mwSize nel = get_number_of_elements ();

    if (! (nel < buflen))
      {
        retval = 1;
        if (buflen > 0)
          nel = buflen-1;
      }

    if (nel < buflen)
      {
        mxChar *ptr = static_cast<mxChar *> (pr);

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = static_cast<char> (ptr[i]);

        buf[nel] = 0;
      }

    return retval;
  }

  char *array_to_string (void) const
  {
    // FIXME: this is suposed to handle multi-byte character strings.

    mwSize nel = get_number_of_elements ();

    char *buf = static_cast<char *> (mxArray::malloc (nel + 1));

    if (buf)
      {
        mxChar *ptr = static_cast<mxChar *> (pr);

        for (mwIndex i = 0; i < nel; i++)
          buf[i] = static_cast<char> (ptr[i]);

        buf[nel] = '\0';
      }

    return buf;
  }

  octave_value as_octave_value (void) const
  {
    octave_value retval;

    dim_vector dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxLOGICAL_CLASS:
        retval = int_to_ov<mxLogical, boolNDArray, bool> (dv);
        break;

      case mxCHAR_CLASS:
        {
          mwSize nel = get_number_of_elements ();

          mxChar *ppr = static_cast<mxChar *> (pr);

          charNDArray val (dv);

          char *ptr = val.fortran_vec ();

          for (mwIndex i = 0; i < nel; i++)
            ptr[i] = static_cast<char> (ppr[i]);

          retval = val;
        }
        break;

      case mxSINGLE_CLASS:
        {
          mwSize nel = get_number_of_elements ();

          float *ppr = static_cast<float *> (pr);

          if (pi)
            {
              FloatComplexNDArray val (dv);

              FloatComplex *ptr = val.fortran_vec ();

              float *ppi = static_cast<float *> (pi);

              for (mwIndex i = 0; i < nel; i++)
                ptr[i] = FloatComplex (ppr[i], ppi[i]);

              retval = val;
            }
          else
            {
              FloatNDArray val (dv);

              float *ptr = val.fortran_vec ();

              for (mwIndex i = 0; i < nel; i++)
                ptr[i] = ppr[i];

              retval = val;
            }
        }
        break;

      case mxDOUBLE_CLASS:
        {
          mwSize nel = get_number_of_elements ();

          double *ppr = static_cast<double *> (pr);

          if (pi)
            {
              ComplexNDArray val (dv);

              Complex *ptr = val.fortran_vec ();

              double *ppi = static_cast<double *> (pi);

              for (mwIndex i = 0; i < nel; i++)
                ptr[i] = Complex (ppr[i], ppi[i]);

              retval = val;
            }
          else
            {
              NDArray val (dv);

              double *ptr = val.fortran_vec ();

              for (mwIndex i = 0; i < nel; i++)
                ptr[i] = ppr[i];

              retval = val;
            }
        }
        break;

      case mxINT8_CLASS:
        retval = int_to_ov<int8_t, int8NDArray, octave_int8> (dv);
        break;

      case mxUINT8_CLASS:
        retval = int_to_ov<uint8_t, uint8NDArray, octave_uint8> (dv);
        break;

      case mxINT16_CLASS:
        retval = int_to_ov<int16_t, int16NDArray, octave_int16> (dv);
        break;

      case mxUINT16_CLASS:
        retval = int_to_ov<uint16_t, uint16NDArray, octave_uint16> (dv);
        break;

      case mxINT32_CLASS:
        retval = int_to_ov<int32_t, int32NDArray, octave_int32> (dv);
        break;

      case mxUINT32_CLASS:
        retval = int_to_ov<uint32_t, uint32NDArray, octave_uint32> (dv);
        break;

      case mxINT64_CLASS:
        retval = int_to_ov<int64_t, int64NDArray, octave_int64> (dv);
        break;

      case mxUINT64_CLASS:
        retval = int_to_ov<uint64_t, uint64NDArray, octave_uint64> (dv);
        break;

      default:
        panic_impossible ();
      }

    return retval;
  }

protected:

  template <typename ELT_T, typename ARRAY_T, typename ARRAY_ELT_T>
  octave_value
  int_to_ov (const dim_vector& dv) const
  {
    octave_value retval;

    mwSize nel = get_number_of_elements ();

    ELT_T *ppr = static_cast<ELT_T *> (pr);

    if (pi)
      error ("complex integer types are not supported");
    else
      {
        ARRAY_T val (dv);

        ARRAY_ELT_T *ptr = val.fortran_vec ();

        for (mwIndex i = 0; i < nel; i++)
          ptr[i] = ppr[i];

        retval = val;
      }

    return retval;
  }

  mxArray_number (const mxArray_number& val)
    : mxArray_matlab (val),
      pr (mxArray::malloc (get_number_of_elements () * get_element_size ())),
      pi (val.pi ? mxArray::malloc (get_number_of_elements ()
                                    * get_element_size ())
                 : 0)
  {
    size_t nbytes = get_number_of_elements () * get_element_size ();

    if (pr)
      memcpy (pr, val.pr, nbytes);

    if (pi)
      memcpy (pi, val.pi, nbytes);
  }

private:

  void *pr;
  void *pi;

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_number& operator = (const mxArray_number&);
};

// Matlab-style sparse arrays.

class mxArray_sparse : public mxArray_matlab
{
public:

  mxArray_sparse (mxClassID id_arg, mwSize m, mwSize n, mwSize nzmax_arg,
                  mxComplexity flag = mxREAL)
    : mxArray_matlab (id_arg, m, n), nzmax (nzmax_arg),
      pr (mxArray::calloc (nzmax, get_element_size ())),
      pi (flag == mxCOMPLEX ? mxArray::calloc (nzmax, get_element_size ()) : 0),
      ir (static_cast<mwIndex *> (mxArray::calloc (nzmax, sizeof (mwIndex)))),
      jc (static_cast<mwIndex *> (mxArray::calloc (n + 1, sizeof (mwIndex))))
  { }

  mxArray_base *dup (void) const { return new mxArray_sparse (*this); }

  ~mxArray_sparse (void)
  {
    mxFree (pr);
    mxFree (pi);
    mxFree (ir);
    mxFree (jc);
  }

  int is_complex (void) const { return pi != 0; }

  int is_sparse (void) const { return 1; }

  void *get_data (void) const { return pr; }

  void *get_imag_data (void) const { return pi; }

  void set_data (void *pr_arg) { pr = pr_arg; }

  void set_imag_data (void *pi_arg) { pi = pi_arg; }

  mwIndex *get_ir (void) const { return ir; }

  mwIndex *get_jc (void) const { return jc; }

  mwSize get_nzmax (void) const { return nzmax; }

  void set_ir (mwIndex *ir_arg) { ir = ir_arg; }

  void set_jc (mwIndex *jc_arg) { jc = jc_arg; }

  void set_nzmax (mwSize nzmax_arg) { nzmax = nzmax_arg; }

  octave_value as_octave_value (void) const
  {
    octave_value retval;

    dim_vector dv = dims_to_dim_vector ();

    switch (get_class_id ())
      {
      case mxLOGICAL_CLASS:
        {
          bool *ppr = static_cast<bool *> (pr);

          SparseBoolMatrix val (get_m (), get_n (),
                                static_cast<octave_idx_type> (nzmax));

          for (mwIndex i = 0; i < nzmax; i++)
            {
              val.xdata (i) = ppr[i];
              val.xridx (i) = ir[i];
            }

          for (mwIndex i = 0; i < get_n () + 1; i++)
            val.xcidx (i) = jc[i];

          retval = val;
        }
        break;

      case mxSINGLE_CLASS:
        error ("single precision sparse data type not supported");
        break;

      case mxDOUBLE_CLASS:
        {
          if (pi)
            {
              double *ppr = static_cast<double *> (pr);
              double *ppi = static_cast<double *> (pi);

              SparseComplexMatrix val (get_m (), get_n (),
                                       static_cast<octave_idx_type> (nzmax));

              for (mwIndex i = 0; i < nzmax; i++)
                {
                  val.xdata (i) = Complex (ppr[i], ppi[i]);
                  val.xridx (i) = ir[i];
                }

              for (mwIndex i = 0; i < get_n () + 1; i++)
                val.xcidx (i) = jc[i];

              retval = val;
            }
          else
            {
              double *ppr = static_cast<double *> (pr);

              SparseMatrix val (get_m (), get_n (),
                                static_cast<octave_idx_type> (nzmax));

              for (mwIndex i = 0; i < nzmax; i++)
                {
                  val.xdata (i) = ppr[i];
                  val.xridx (i) = ir[i];
                }

              for (mwIndex i = 0; i < get_n () + 1; i++)
                val.xcidx (i) = jc[i];

              retval = val;
            }
        }
        break;

      default:
        panic_impossible ();
      }

    return retval;
  }

private:

  mwSize nzmax;

  void *pr;
  void *pi;
  mwIndex *ir;
  mwIndex *jc;

  mxArray_sparse (const mxArray_sparse& val)
    : mxArray_matlab (val), nzmax (val.nzmax),
      pr (mxArray::malloc (nzmax * get_element_size ())),
      pi (val.pi ? mxArray::malloc (nzmax * get_element_size ()) : 0),
      ir (static_cast<mwIndex *> (mxArray::malloc (nzmax * sizeof (mwIndex)))),
      jc (static_cast<mwIndex *> (mxArray::malloc (nzmax * sizeof (mwIndex))))
  {
    size_t nbytes = nzmax * get_element_size ();

    if (pr)
      memcpy (pr, val.pr, nbytes);

    if (pi)
      memcpy (pi, val.pi, nbytes);

    if (ir)
      memcpy (ir, val.ir, nzmax * sizeof (mwIndex));

    if (jc)
      memcpy (jc, val.jc, (val.get_n () + 1) * sizeof (mwIndex));
  }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_sparse& operator = (const mxArray_sparse&);
};

// Matlab-style struct arrays.

class mxArray_struct : public mxArray_matlab
{
public:

  mxArray_struct (mwSize ndims_arg, const mwSize *dims_arg, int num_keys_arg,
                  const char **keys)
    : mxArray_matlab (mxSTRUCT_CLASS, ndims_arg, dims_arg),
      nfields (num_keys_arg),
      fields (static_cast<char **> (mxArray::calloc (nfields,
                                                     sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::calloc (nfields *
                                                      get_number_of_elements (),
                                                      sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (const dim_vector& dv, int num_keys_arg, const char **keys)
    : mxArray_matlab (mxSTRUCT_CLASS, dv), nfields (num_keys_arg),
      fields (static_cast<char **> (mxArray::calloc (nfields,
                                                     sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::calloc (nfields *
                                                      get_number_of_elements (),
                                                      sizeof (mxArray *))))
  {
    init (keys);
  }

  mxArray_struct (mwSize m, mwSize n, int num_keys_arg, const char **keys)
    : mxArray_matlab (mxSTRUCT_CLASS, m, n), nfields (num_keys_arg),
      fields (static_cast<char **> (mxArray::calloc (nfields,
                                                     sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::calloc (nfields *
                                                      get_number_of_elements (),
                                                      sizeof (mxArray *))))
  {
    init (keys);
  }

  void init (const char **keys)
  {
    for (int i = 0; i < nfields; i++)
      fields[i] = mxArray::strsave (keys[i]);
  }

  mxArray_base *dup (void) const { return new mxArray_struct (*this); }

  ~mxArray_struct (void)
  {
    for (int i = 0; i < nfields; i++)
      mxFree (fields[i]);

    mxFree (fields);

    mwSize ntot = nfields * get_number_of_elements ();

    for  (mwIndex i = 0; i < ntot; i++)
      delete data[i];

    mxFree (data);
  }

  int add_field (const char *key)
  {
    int retval = -1;

    if (valid_key (key))
      {
        nfields++;

        fields = static_cast<char **>
                  (mxRealloc (fields, nfields * sizeof (char *)));

        if (fields)
          {
            fields[nfields-1] = mxArray::strsave (key);

            mwSize nel = get_number_of_elements ();

            mwSize ntot = nfields * nel;

            mxArray **new_data;
            new_data = static_cast<mxArray **>
                        (mxArray::malloc (ntot * sizeof (mxArray *)));

            if (new_data)
              {
                mwIndex j = 0;
                mwIndex k = 0;
                mwIndex n = 0;

                for (mwIndex i = 0; i < ntot; i++)
                  {
                    if (++n == nfields)
                      {
                        new_data[j++] = 0;
                        n = 0;
                      }
                    else
                      new_data[j++] = data[k++];
                  }

                mxFree (data);

                data = new_data;

                retval = nfields - 1;
              }
          }
      }

    return retval;
  }

  void remove_field (int key_num)
  {
    if (key_num >= 0 && key_num < nfields)
      {
        mwSize nel = get_number_of_elements ();

        mwSize ntot = nfields * nel;

        int new_nfields = nfields - 1;

        char **new_fields = static_cast<char **>
                             (mxArray::malloc (new_nfields * sizeof (char *)));

        mxArray **new_data = static_cast<mxArray **>
                              (mxArray::malloc (new_nfields * nel
                                                * sizeof (mxArray *)));

        for (int i = 0; i < key_num; i++)
          new_fields[i] = fields[i];

        for (int i = key_num + 1; i < nfields; i++)
          new_fields[i-1] = fields[i];

        if (new_nfields > 0)
          {
            mwIndex j = 0;
            mwIndex k = 0;
            mwIndex n = 0;

            for (mwIndex i = 0; i < ntot; i++)
              {
                if (n == key_num)
                  k++;
                else
                  new_data[j++] = data[k++];

                if (++n == nfields)
                  n = 0;
              }
          }

        nfields = new_nfields;

        mxFree (fields);
        mxFree (data);

        fields = new_fields;
        data = new_data;
      }
  }

  mxArray *get_field_by_number (mwIndex index, int key_num) const
  {
    return key_num >= 0 && key_num < nfields
           ? data[nfields * index + key_num] : 0;
  }

  void set_field_by_number (mwIndex index, int key_num, mxArray *val);

  int get_number_of_fields (void) const { return nfields; }

  const char *get_field_name_by_number (int key_num) const
  {
    return key_num >= 0 && key_num < nfields ? fields[key_num] : 0;
  }

  int get_field_number (const char *key) const
  {
    int retval = -1;

    for (int i = 0; i < nfields; i++)
      {
        if (! strcmp (key, fields[i]))
          {
            retval = i;
            break;
          }
      }

    return retval;
  }

  void *get_data (void) const { return data; }

  void set_data (void *data_arg) { data = static_cast<mxArray **> (data_arg); }

  octave_value as_octave_value (void) const
  {
    dim_vector dv = dims_to_dim_vector ();

    string_vector keys (fields, nfields);

    octave_map m;

    mwSize ntot = nfields * get_number_of_elements ();

    for (int i = 0; i < nfields; i++)
      {
        Cell c (dv);

        octave_value *p = c.fortran_vec ();

        mwIndex k = 0;
        for (mwIndex j = i; j < ntot; j += nfields)
          p[k++] = mxArray::as_octave_value (data[j]);

        m.assign (keys[i], c);
      }

    return m;
  }

private:

  int nfields;

  char **fields;

  mxArray **data;

  mxArray_struct (const mxArray_struct& val)
    : mxArray_matlab (val), nfields (val.nfields),
      fields (static_cast<char **> (mxArray::malloc (nfields
                                                     * sizeof (char *)))),
      data (static_cast<mxArray **> (mxArray::malloc (nfields *
                                                      get_number_of_elements ()
                                                      * sizeof (mxArray *))))
  {
    for (int i = 0; i < nfields; i++)
      fields[i] = mxArray::strsave (val.fields[i]);

    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel * nfields; i++)
      {
        mxArray *ptr = val.data[i];
        data[i] = ptr ? ptr->dup () : 0;
      }
  }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_struct& operator = (const mxArray_struct& val);
};

// Matlab-style cell arrays.

class mxArray_cell : public mxArray_matlab
{
public:

  mxArray_cell (mwSize ndims_arg, const mwSize *dims_arg)
    : mxArray_matlab (mxCELL_CLASS, ndims_arg, dims_arg),
      data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (),
                                     sizeof (mxArray *)))) { }

  mxArray_cell (const dim_vector& dv)
    : mxArray_matlab (mxCELL_CLASS, dv),
      data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (),
                                     sizeof (mxArray *)))) { }

  mxArray_cell (mwSize m, mwSize n)
    : mxArray_matlab (mxCELL_CLASS, m, n),
      data (static_cast<mxArray **> (mxArray::calloc (get_number_of_elements (),
                                     sizeof (mxArray *)))) { }

  mxArray_base *dup (void) const { return new mxArray_cell (*this); }

  ~mxArray_cell (void)
  {
    mwSize nel = get_number_of_elements ();

    for  (mwIndex i = 0; i < nel; i++)
      delete data[i];

    mxFree (data);
  }

  mxArray *get_cell (mwIndex idx) const
  {
    return idx >= 0 && idx < get_number_of_elements () ? data[idx] : 0;
  }

  void set_cell (mwIndex idx, mxArray *val);

  void *get_data (void) const { return data; }

  void set_data (void *data_arg) { data = static_cast<mxArray **> (data_arg); }

  octave_value as_octave_value (void) const
  {
    dim_vector dv = dims_to_dim_vector ();

    Cell c (dv);

    mwSize nel = get_number_of_elements ();

    octave_value *p = c.fortran_vec ();

    for (mwIndex i = 0; i < nel; i++)
      p[i] = mxArray::as_octave_value (data[i]);

    return c;
  }

private:

  mxArray **data;

  mxArray_cell (const mxArray_cell& val)
    : mxArray_matlab (val),
      data (static_cast<mxArray **> (mxArray::malloc (get_number_of_elements ()
                                                      * sizeof (mxArray *))))
  {
    mwSize nel = get_number_of_elements ();

    for (mwIndex i = 0; i < nel; i++)
      {
        mxArray *ptr = val.data[i];
        data[i] = ptr ? ptr->dup () : 0;
      }
  }

  // No assignment!  FIXME: should this be implemented?  Note that we
  // do have a copy constructor.

  mxArray_cell& operator = (const mxArray_cell&);
};

// ------------------------------------------------------------------

mxArray::mxArray (const octave_value& ov)
  : rep (new mxArray_octave_value (ov)), name (0) { }

mxArray::mxArray (mxClassID id, mwSize ndims, const mwSize *dims,
                  mxComplexity flag)
  : rep (new mxArray_number (id, ndims, dims, flag)), name (0) { }

mxArray::mxArray (mxClassID id, const dim_vector& dv, mxComplexity flag)
  : rep (new mxArray_number (id, dv, flag)), name (0) { }

mxArray::mxArray (mxClassID id, mwSize m, mwSize n, mxComplexity flag)
  : rep (new mxArray_number (id, m, n, flag)), name (0) { }

mxArray::mxArray (mxClassID id, double val)
  : rep (new mxArray_number (id, val)), name (0) { }

mxArray::mxArray (mxClassID id, mxLogical val)
  : rep (new mxArray_number (id, val)), name (0) { }

mxArray::mxArray (const char *str)
  : rep (new mxArray_number (str)), name (0) { }

mxArray::mxArray (mwSize m, const char **str)
  : rep (new mxArray_number (m, str)), name (0) { }

mxArray::mxArray (mxClassID id, mwSize m, mwSize n, mwSize nzmax,
                  mxComplexity flag)
  : rep (new mxArray_sparse (id, m, n, nzmax, flag)), name (0) { }

mxArray::mxArray (mwSize ndims, const mwSize *dims, int num_keys,
                  const char **keys)
  : rep (new mxArray_struct (ndims, dims, num_keys, keys)), name (0) { }

mxArray::mxArray (const dim_vector& dv, int num_keys, const char **keys)
  : rep (new mxArray_struct (dv, num_keys, keys)), name (0) { }

mxArray::mxArray (mwSize m, mwSize n, int num_keys, const char **keys)
  : rep (new mxArray_struct (m, n, num_keys, keys)), name (0) { }

mxArray::mxArray (mwSize ndims, const mwSize *dims)
  : rep (new mxArray_cell (ndims, dims)), name (0) { }

mxArray::mxArray (const dim_vector& dv)
  : rep (new mxArray_cell (dv)), name (0) { }

mxArray::mxArray (mwSize m, mwSize n)
  : rep (new mxArray_cell (m, n)), name (0) { }

mxArray::~mxArray (void)
{
  mxFree (name);

  delete rep;
}

void
mxArray::set_name (const char *name_arg)
{
  mxFree (name);
  name = mxArray::strsave (name_arg);
}

octave_value
mxArray::as_octave_value (const mxArray *ptr)
{
  return ptr ? ptr->as_octave_value () : octave_value (Matrix ());
}

octave_value
mxArray::as_octave_value (void) const
{
  return rep->as_octave_value ();
}

void
mxArray::maybe_mutate (void) const
{
  if (rep->is_octave_value ())
    {
      // The mutate function returns a pointer to a complete new
      // mxArray object (or 0, if no mutation happened).  We just want
      // to replace the existing rep with the rep from the new object.

      mxArray *new_val = rep->mutate ();

      if (new_val)
        {
          delete rep;
          rep = new_val->rep;
          new_val->rep = 0;
          delete new_val;
        }
    }
}

// ------------------------------------------------------------------

// A class to manage calls to MEX functions.  Mostly deals with memory
// management.

class mex
{
public:

  mex (octave_mex_function *f)
    : curr_mex_fcn (f), memlist (), arraylist (), fname (0) { }

  ~mex (void)
  {
    if (! memlist.empty ())
      error ("mex: %s: cleanup failed", function_name ());

    mxFree (fname);
  }

  const char *function_name (void) const
  {
    if (! fname)
      {
        octave_function *fcn = octave_call_stack::current ();

        if (fcn)
          {
            std::string nm = fcn->name ();
            fname = mxArray::strsave (nm.c_str ());
          }
        else
          fname = mxArray::strsave ("unknown");
      }

    return fname;
  }

  // Free all unmarked pointers obtained from malloc and calloc.
  static void cleanup (void *ptr)
  {
    mex *context = static_cast<mex *> (ptr);

    // We can't use mex::free here because it modifies memlist.
    for (std::set<void *>::iterator p = context->memlist.begin ();
         p != context->memlist.end (); p++)
      xfree (*p);

    context->memlist.clear ();

    // We can't use mex::free_value here because it modifies arraylist.
    for (std::set<mxArray *>::iterator p = context->arraylist.begin ();
         p != context->arraylist.end (); p++)
      delete *p;

    context->arraylist.clear ();
  }

  // Allocate memory.
  void *malloc_unmarked (size_t n)
  {
    void *ptr = gnulib::malloc (n);

    if (! ptr)
      {
        // FIXME: could use "octave_new_handler();" instead

        error ("%s: failed to allocate %d bytes of memory",
               function_name (), n);

        abort ();
      }

    global_mark (ptr);

    return ptr;
  }

  // Allocate memory to be freed on exit.
  void *malloc (size_t n)
  {
    void *ptr = malloc_unmarked (n);

    mark (ptr);

    return ptr;
  }

  // Allocate memory and initialize to 0.
  void *calloc_unmarked (size_t n, size_t t)
  {
    void *ptr = malloc_unmarked (n*t);

    memset (ptr, 0, n*t);

    return ptr;
  }

  // Allocate memory to be freed on exit and initialize to 0.
  void *calloc (size_t n, size_t t)
  {
    void *ptr = calloc_unmarked (n, t);

    mark (ptr);

    return ptr;
  }

  // Reallocate a pointer obtained from malloc or calloc. If the
  // pointer is NULL, allocate using malloc.  We don't need an
  // "unmarked" version of this.
  void *realloc (void *ptr, size_t n)
  {
    void *v;

    if (ptr)
      {
        v = gnulib::realloc (ptr, n);

        std::set<void *>::iterator p = memlist.find (ptr);

        if (v && p != memlist.end ())
          {
            memlist.erase (p);
            memlist.insert (v);
          }

        p = global_memlist.find (ptr);

        if (v && p != global_memlist.end ())
          {
            global_memlist.erase (p);
            global_memlist.insert (v);
          }
      }
    else
      v = malloc (n);

    return v;
  }

  // Free a pointer obtained from malloc or calloc.
  void free (void *ptr)
  {
    if (ptr)
      {
        unmark (ptr);

        std::set<void *>::iterator p = global_memlist.find (ptr);

        if (p != global_memlist.end ())
          {
            global_memlist.erase (p);

            xfree (ptr);
          }
        else
          {
            p = foreign_memlist.find (ptr);

            if (p != foreign_memlist.end ())
              foreign_memlist.erase (p);
#ifdef DEBUG
            else
              warning ("mxFree: skipping memory not allocated by mxMalloc, mxCalloc, or mxRealloc");
#endif
          }
      }
  }

  // Mark a pointer to be freed on exit.
  void mark (void *ptr)
  {
#ifdef DEBUG
    if (memlist.find (ptr) != memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    memlist.insert (ptr);
  }

  // Unmark a pointer to be freed on exit, either because it was
  // made persistent, or because it was already freed.
  void unmark (void *ptr)
  {
    std::set<void *>::iterator p = memlist.find (ptr);

    if (p != memlist.end ())
      memlist.erase (p);
#ifdef DEBUG
    else
      warning ("%s: value not marked", function_name ());
#endif
  }

  mxArray *mark_array (mxArray *ptr)
  {
    arraylist.insert (ptr);
    return ptr;
  }

  void unmark_array (mxArray *ptr)
  {
    std::set<mxArray *>::iterator p = arraylist.find (ptr);

    if (p != arraylist.end ())
      arraylist.erase (p);
  }

  // Mark a pointer as one we allocated.
  void mark_foreign (void *ptr)
  {
#ifdef DEBUG
    if (foreign_memlist.find (ptr) != foreign_memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    foreign_memlist.insert (ptr);
  }

  // Unmark a pointer as one we allocated.
  void unmark_foreign (void *ptr)
  {
    std::set<void *>::iterator p = foreign_memlist.find (ptr);

    if (p != foreign_memlist.end ())
      foreign_memlist.erase (p);
#ifdef DEBUG
    else
      warning ("%s: value not marked", function_name ());
#endif

  }

  // Make a new array value and initialize from an octave value; it will be
  // freed on exit unless marked as persistent.
  mxArray *make_value (const octave_value& ov)
  {
    return mark_array (new mxArray (ov));
  }

  // Free an array and its contents.
  bool free_value (mxArray *ptr)
  {
    bool inlist = false;

    std::set<mxArray *>::iterator p = arraylist.find (ptr);

    if (p != arraylist.end ())
      {
        inlist = true;
        arraylist.erase (p);
        delete ptr;
      }
#ifdef DEBUG
    else
      warning ("mex::free_value: skipping memory not allocated by mex::make_value");
#endif

    return inlist;
  }

  octave_mex_function *current_mex_function (void) const
  {
    return curr_mex_fcn;
  }

  // 1 if error should be returned to MEX file, 0 if abort.
  int trap_feval_error;

  // longjmp return point if mexErrMsgTxt or error.
  jmp_buf jump;

  // Trigger a long jump back to the mex calling function.
  void abort (void) { longjmp (jump, 1); }

private:

  // Pointer to the mex function that corresponds to this mex context.
  octave_mex_function *curr_mex_fcn;

  // List of memory resources that need to be freed upon exit.
  std::set<void *> memlist;

  // List of mxArray objects that need to be freed upon exit.
  std::set<mxArray *> arraylist;

  // List of memory resources we know about, but that were allocated
  // elsewhere.
  std::set<void *> foreign_memlist;

  // The name of the currently executing function.
  mutable char *fname;

  // List of memory resources we allocated.
  static std::set<void *> global_memlist;

  // Mark a pointer as one we allocated.
  void global_mark (void *ptr)
  {
#ifdef DEBUG
    if (global_memlist.find (ptr) != global_memlist.end ())
      warning ("%s: double registration ignored", function_name ());
#endif

    global_memlist.insert (ptr);
  }

  // Unmark a pointer as one we allocated.
  void global_unmark (void *ptr)
  {
    std::set<void *>::iterator p = global_memlist.find (ptr);

    if (p != global_memlist.end ())
      global_memlist.erase (p);
#ifdef DEBUG
    else
      warning ("%s: value not marked", function_name ());
#endif

  }

  // No copying!

  mex (const mex&);

  mex& operator = (const mex&);
};

// List of memory resources we allocated.
std::set<void *> mex::global_memlist;

// Current context.
mex *mex_context = 0;

void *
mxArray::malloc (size_t n)
{
  return mex_context ? mex_context->malloc_unmarked (n) : gnulib::malloc (n);
}

void *
mxArray::calloc (size_t n, size_t t)
{
  return mex_context ? mex_context->calloc_unmarked (n, t) : ::calloc (n, t);
}

static inline void *
maybe_mark_foreign (void *ptr)
{
  if (mex_context)
    mex_context->mark_foreign (ptr);

  return ptr;
}

static inline mxArray *
maybe_unmark_array (mxArray *ptr)
{
  if (mex_context)
    mex_context->unmark_array (ptr);

  return ptr;
}

static inline void *
maybe_unmark (void *ptr)
{
  if (mex_context)
    mex_context->unmark (ptr);

  return ptr;
}

void
mxArray_struct::set_field_by_number (mwIndex index, int key_num, mxArray *val)
{
  if (key_num >= 0 && key_num < nfields)
    data[nfields * index + key_num] = maybe_unmark_array (val);
}

void
mxArray_cell::set_cell (mwIndex idx, mxArray *val)
{
  if (idx >= 0 && idx < get_number_of_elements ())
    data[idx] = maybe_unmark_array (val);
}

// ------------------------------------------------------------------

// C interface to mxArray objects:

// Floating point predicates.

int
mxIsFinite (const double v)
{
  return lo_ieee_finite (v) != 0;
}

int
mxIsInf (const double v)
{
  return lo_ieee_isinf (v) != 0;
}

int
mxIsNaN (const double v)
{
  return lo_ieee_isnan (v) != 0;
}

double
mxGetEps (void)
{
  return std::numeric_limits<double>::epsilon ();
}

double
mxGetInf (void)
{
  return lo_ieee_inf_value ();
}

double
mxGetNaN (void)
{
  return lo_ieee_nan_value ();
}

// Memory management.
void *
mxCalloc (size_t n, size_t size)
{
  return mex_context ? mex_context->calloc (n, size) : ::calloc (n, size);
}

void *
mxMalloc (size_t n)
{
  return mex_context ? mex_context->malloc (n) : gnulib::malloc (n);
}

void *
mxRealloc (void *ptr, size_t size)
{
  return mex_context ? mex_context->realloc (ptr, size)
                     : gnulib::realloc (ptr, size);
}

void
mxFree (void *ptr)
{
  if (mex_context)
    mex_context->free (ptr);
  else
    xfree (ptr);
}

static inline mxArray *
maybe_mark_array (mxArray *ptr)
{
  return mex_context ? mex_context->mark_array (ptr) : ptr;
}

// Constructors.
mxArray *
mxCreateCellArray (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (ndims, dims));
}

mxArray *
mxCreateCellMatrix (mwSize m, mwSize n)
{
  return maybe_mark_array (new mxArray (m, n));
}

mxArray *
mxCreateCharArray (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (mxCHAR_CLASS, ndims, dims));
}

mxArray *
mxCreateCharMatrixFromStrings (mwSize m, const char **str)
{
  return maybe_mark_array (new mxArray (m, str));
}

mxArray *
mxCreateDoubleMatrix (mwSize m, mwSize n, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (mxDOUBLE_CLASS, m, n, flag));
}

mxArray *
mxCreateDoubleScalar (double val)
{
  return maybe_mark_array (new mxArray (mxDOUBLE_CLASS, val));
}

mxArray *
mxCreateLogicalArray (mwSize ndims, const mwSize *dims)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, ndims, dims));
}

mxArray *
mxCreateLogicalMatrix (mwSize m, mwSize n)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, m, n));
}

mxArray *
mxCreateLogicalScalar (mxLogical val)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, val));
}

mxArray *
mxCreateNumericArray (mwSize ndims, const mwSize *dims, mxClassID class_id,
                      mxComplexity flag)
{
  return maybe_mark_array (new mxArray (class_id, ndims, dims, flag));
}

mxArray *
mxCreateNumericMatrix (mwSize m, mwSize n, mxClassID class_id,
                       mxComplexity flag)
{
  return maybe_mark_array (new mxArray (class_id, m, n, flag));
}

mxArray *
mxCreateSparse (mwSize m, mwSize n, mwSize nzmax, mxComplexity flag)
{
  return maybe_mark_array (new mxArray (mxDOUBLE_CLASS, m, n, nzmax, flag));
}

mxArray *
mxCreateSparseLogicalMatrix (mwSize m, mwSize n, mwSize nzmax)
{
  return maybe_mark_array (new mxArray (mxLOGICAL_CLASS, m, n, nzmax));
}

mxArray *
mxCreateString (const char *str)
{
  return maybe_mark_array (new mxArray (str));
}

mxArray *
mxCreateStructArray (mwSize ndims, const mwSize *dims, int num_keys,
                     const char **keys)
{
  return maybe_mark_array (new mxArray (ndims, dims, num_keys, keys));
}

mxArray *
mxCreateStructMatrix (mwSize m, mwSize n, int num_keys, const char **keys)
{
  return maybe_mark_array (new mxArray (m, n, num_keys, keys));
}

// Copy constructor.
mxArray *
mxDuplicateArray (const mxArray *ptr)
{
  return maybe_mark_array (ptr->dup ());
}

// Destructor.
void
mxDestroyArray (mxArray *ptr)
{
  if (! (mex_context && mex_context->free_value (ptr)))
    delete ptr;
}

// Type Predicates.
int
mxIsCell (const mxArray *ptr)
{
  return ptr->is_cell ();
}

int
mxIsChar (const mxArray *ptr)
{
  return ptr->is_char ();
}

int
mxIsClass (const mxArray *ptr, const char *name)
{
  return ptr->is_class (name);
}

int
mxIsComplex (const mxArray *ptr)
{
  return ptr->is_complex ();
}

int
mxIsDouble (const mxArray *ptr)
{
  return ptr->is_double ();
}

int
mxIsFunctionHandle (const mxArray *ptr)
{
  return ptr->is_function_handle ();
}

int
mxIsInt16 (const mxArray *ptr)
{
  return ptr->is_int16 ();
}

int
mxIsInt32 (const mxArray *ptr)
{
  return ptr->is_int32 ();
}

int
mxIsInt64 (const mxArray *ptr)
{
  return ptr->is_int64 ();
}

int
mxIsInt8 (const mxArray *ptr)
{
  return ptr->is_int8 ();
}

int
mxIsLogical (const mxArray *ptr)
{
  return ptr->is_logical ();
}

int
mxIsNumeric (const mxArray *ptr)
{
  return ptr->is_numeric ();
}

int
mxIsSingle (const mxArray *ptr)
{
  return ptr->is_single ();
}

int
mxIsSparse (const mxArray *ptr)
{
  return ptr->is_sparse ();
}

int
mxIsStruct (const mxArray *ptr)
{
  return ptr->is_struct ();
}

int
mxIsUint16 (const mxArray *ptr)
{
  return ptr->is_uint16 ();
}

int
mxIsUint32 (const mxArray *ptr)
{
  return ptr->is_uint32 ();
}

int
mxIsUint64 (const mxArray *ptr)
{
  return ptr->is_uint64 ();
}

int
mxIsUint8 (const mxArray *ptr)
{
  return ptr->is_uint8 ();
}

// Odd type+size predicate.
int
mxIsLogicalScalar (const mxArray *ptr)
{
  return ptr->is_logical_scalar ();
}

// Odd type+size+value predicate.
int
mxIsLogicalScalarTrue (const mxArray *ptr)
{
  return ptr->is_logical_scalar_true ();
}

// Size predicate.
int
mxIsEmpty (const mxArray *ptr)
{
  return ptr->is_empty ();
}

// Just plain odd thing to ask of a value.
int
mxIsFromGlobalWS (const mxArray */*ptr*/)
{
  // FIXME
  abort ();
  return 0;
}

// Dimension extractors.
size_t
mxGetM (const mxArray *ptr)
{
  return ptr->get_m ();
}

size_t
mxGetN (const mxArray *ptr)
{
  return ptr->get_n ();
}

mwSize *
mxGetDimensions (const mxArray *ptr)
{
  return ptr->get_dimensions ();
}

mwSize
mxGetNumberOfDimensions (const mxArray *ptr)
{
  return ptr->get_number_of_dimensions ();
}

size_t
mxGetNumberOfElements (const mxArray *ptr)
{
  return ptr->get_number_of_elements ();
}

// Dimension setters.
void
mxSetM (mxArray *ptr, mwSize m)
{
  ptr->set_m (m);
}

void
mxSetN (mxArray *ptr, mwSize n)
{
  ptr->set_n (n);
}

void
mxSetDimensions (mxArray *ptr, const mwSize *dims, mwSize ndims)
{
  ptr->set_dimensions (static_cast<mwSize *>
                       (maybe_unmark (const_cast<mwSize *> (dims))),
                       ndims);
}

// Data extractors.
double *
mxGetPr (const mxArray *ptr)
{
  return static_cast<double *> (ptr->get_data ());
}

double *
mxGetPi (const mxArray *ptr)
{
  return static_cast<double *> (ptr->get_imag_data ());
}

double
mxGetScalar (const mxArray *ptr)
{
  return ptr->get_scalar ();
}

mxChar *
mxGetChars (const mxArray *ptr)
{
  return static_cast<mxChar *> (ptr->get_data ());
}

mxLogical *
mxGetLogicals (const mxArray *ptr)
{
  return static_cast<mxLogical *> (ptr->get_data ());
}

void *
mxGetData (const mxArray *ptr)
{
  return ptr->get_data ();
}

void *
mxGetImagData (const mxArray *ptr)
{
  return ptr->get_imag_data ();
}

// Data setters.
void
mxSetPr (mxArray *ptr, double *pr)
{
  ptr->set_data (maybe_unmark (pr));
}

void
mxSetPi (mxArray *ptr, double *pi)
{
  ptr->set_imag_data (maybe_unmark (pi));
}

void
mxSetData (mxArray *ptr, void *pr)
{
  ptr->set_data (maybe_unmark (pr));
}

void
mxSetImagData (mxArray *ptr, void *pi)
{
  ptr->set_imag_data (maybe_unmark (pi));
}

// Classes.
mxClassID
mxGetClassID (const mxArray *ptr)
{
  return ptr->get_class_id ();
}

const char *
mxGetClassName (const mxArray *ptr)
{
  return ptr->get_class_name ();
}

void
mxSetClassName (mxArray *ptr, const char *name)
{
  ptr->set_class_name (name);
}

// Cell support.
mxArray *
mxGetCell (const mxArray *ptr, mwIndex idx)
{
  return ptr->get_cell (idx);
}

void
mxSetCell (mxArray *ptr, mwIndex idx, mxArray *val)
{
  ptr->set_cell (idx, val);
}

// Sparse support.
mwIndex *
mxGetIr (const mxArray *ptr)
{
  return ptr->get_ir ();
}

mwIndex *
mxGetJc (const mxArray *ptr)
{
  return ptr->get_jc ();
}

mwSize
mxGetNzmax (const mxArray *ptr)
{
  return ptr->get_nzmax ();
}

void
mxSetIr (mxArray *ptr, mwIndex *ir)
{
  ptr->set_ir (static_cast<mwIndex *> (maybe_unmark (ir)));
}

void
mxSetJc (mxArray *ptr, mwIndex *jc)
{
  ptr->set_jc (static_cast<mwIndex *> (maybe_unmark (jc)));
}

void
mxSetNzmax (mxArray *ptr, mwSize nzmax)
{
  ptr->set_nzmax (nzmax);
}

// Structure support.
int
mxAddField (mxArray *ptr, const char *key)
{
  return ptr->add_field (key);
}

void
mxRemoveField (mxArray *ptr, int key_num)
{
  ptr->remove_field (key_num);
}

mxArray *
mxGetField (const mxArray *ptr, mwIndex index, const char *key)
{
  int key_num = mxGetFieldNumber (ptr, key);
  return mxGetFieldByNumber (ptr, index, key_num);
}

mxArray *
mxGetFieldByNumber (const mxArray *ptr, mwIndex index, int key_num)
{
  return ptr->get_field_by_number (index, key_num);
}

void
mxSetField (mxArray *ptr, mwIndex index, const char *key, mxArray *val)
{
  int key_num = mxGetFieldNumber (ptr, key);
  mxSetFieldByNumber (ptr, index, key_num, val);
}

void
mxSetFieldByNumber (mxArray *ptr, mwIndex index, int key_num, mxArray *val)
{
  ptr->set_field_by_number (index, key_num, val);
}

int
mxGetNumberOfFields (const mxArray *ptr)
{
  return ptr->get_number_of_fields ();
}

const char *
mxGetFieldNameByNumber (const mxArray *ptr, int key_num)
{
  return ptr->get_field_name_by_number (key_num);
}

int
mxGetFieldNumber (const mxArray *ptr, const char *key)
{
  return ptr->get_field_number (key);
}

int
mxGetString (const mxArray *ptr, char *buf, mwSize buflen)
{
  return ptr->get_string (buf, buflen);
}

char *
mxArrayToString (const mxArray *ptr)
{
  return ptr->array_to_string ();
}

mwIndex
mxCalcSingleSubscript (const mxArray *ptr, mwSize nsubs, mwIndex *subs)
{
  return ptr->calc_single_subscript (nsubs, subs);
}

size_t
mxGetElementSize (const mxArray *ptr)
{
  return ptr->get_element_size ();
}

// ------------------------------------------------------------------

typedef void (*cmex_fptr) (int nlhs, mxArray **plhs, int nrhs, mxArray **prhs);
typedef F77_RET_T (*fmex_fptr) (int& nlhs, mxArray **plhs,
                                int& nrhs, mxArray **prhs);

octave_value_list
call_mex (bool have_fmex, void *f, const octave_value_list& args,
          int nargout_arg, octave_mex_function *curr_mex_fcn)
{
  // Use at least 1 for nargout since even for zero specified args,
  // still want to be able to return an ans.

  volatile int nargout = nargout_arg;

  int nargin = args.length ();
  OCTAVE_LOCAL_BUFFER (mxArray *, argin, nargin);
  for (int i = 0; i < nargin; i++)
    argin[i] = 0;

  int nout = nargout == 0 ? 1 : nargout;
  OCTAVE_LOCAL_BUFFER (mxArray *, argout, nout);
  for (int i = 0; i < nout; i++)
    argout[i] = 0;

  unwind_protect_safe frame;

  // Save old mex pointer.
  frame.protect_var (mex_context);

  mex context (curr_mex_fcn);

  frame.add_fcn (mex::cleanup, static_cast<void *> (&context));

  for (int i = 0; i < nargin; i++)
    argin[i] = context.make_value (args(i));

  if (setjmp (context.jump) == 0)
    {
      mex_context = &context;

      if (have_fmex)
        {
          fmex_fptr fcn = FCN_PTR_CAST (fmex_fptr, f);

          int tmp_nargout = nargout;
          int tmp_nargin = nargin;

          fcn (tmp_nargout, argout, tmp_nargin, argin);
        }
      else
        {
          cmex_fptr fcn = FCN_PTR_CAST (cmex_fptr, f);

          fcn (nargout, argout, nargin, argin);
        }
    }

  // Convert returned array entries back into octave values.

  octave_value_list retval;

  if (! error_state)
    {
      if (nargout == 0 && argout[0])
        {
          // We have something for ans.
          nargout = 1;
        }

      retval.resize (nargout);

      for (int i = 0; i < nargout; i++)
        retval(i) = mxArray::as_octave_value (argout[i]);
    }

  // Clean up mex resources.
  frame.run ();

  return retval;
}

// C interface to mex functions:

const char *
mexFunctionName (void)
{
  return mex_context ? mex_context->function_name () : "unknown";
}

int
mexCallMATLAB (int nargout, mxArray *argout[], int nargin,
               mxArray *argin[], const char *fname)
{
  octave_value_list args;

  // FIXME: do we need unwind protect to clean up args?  Off hand, I
  // would say that this problem is endemic to Octave and we will
  // continue to have memory leaks after Ctrl-C until proper exception
  // handling is implemented.  longjmp() only clears the stack, so any
  // class which allocates data on the heap is going to leak.

  args.resize (nargin);

  for (int i = 0; i < nargin; i++)
    args(i) = mxArray::as_octave_value (argin[i]);

  octave_value_list retval = feval (fname, args, nargout);

  if (error_state && mex_context->trap_feval_error == 0)
    {
      // FIXME: is this the correct way to clean up?  abort() is
      // going to trigger a long jump, so the normal class destructors
      // will not be called.  Hopefully this will reduce things to a
      // tiny leak.  Maybe create a new octave memory tracer type
      // which prints a friendly message every time it is
      // created/copied/deleted to check this.

      args.resize (0);
      retval.resize (0);
      mex_context->abort ();
    }

  int num_to_copy = retval.length ();

  if (nargout < retval.length ())
    num_to_copy = nargout;

  for (int i = 0; i < num_to_copy; i++)
    {
      // FIXME: it would be nice to avoid copying the value here,
      // but there is no way to steal memory from a matrix, never mind
      // that matrix memory is allocated by new[] and mxArray memory
      // is allocated by malloc().
      argout[i] = mex_context->make_value (retval (i));
    }

  while (num_to_copy < nargout)
    argout[num_to_copy++] = 0;

  if (error_state)
    {
      error_state = 0;
      return 1;
    }
  else
    return 0;
}

void
mexSetTrapFlag (int flag)
{
  if (mex_context)
    mex_context->trap_feval_error = flag;
}

int
mexEvalString (const char *s)
{
  int retval = 0;

  int parse_status;

  octave_value_list ret;

  ret = eval_string (s, false, parse_status, 0);

  if (parse_status || error_state)
    {
      error_state = 0;

      retval = 1;
    }

  return retval;
}

void
mexErrMsgTxt (const char *s)
{
  if (s && strlen (s) > 0)
    error ("%s: %s", mexFunctionName (), s);
  else
    {
      // For compatibility with Matlab, print an empty message.
      // Octave's error routine requires a non-null input so use a SPACE.
      error (" ");
    }

  mex_context->abort ();
}

void
mexErrMsgIdAndTxt (const char *id, const char *fmt, ...)
{
  if (fmt && strlen (fmt) > 0)
    {
      const char *fname = mexFunctionName ();
      size_t len = strlen (fname) + 2 + strlen (fmt) + 1;
      OCTAVE_LOCAL_BUFFER (char, tmpfmt, len);
      sprintf (tmpfmt, "%s: %s", fname, fmt);
      va_list args;
      va_start (args, fmt);
      verror_with_id (id, tmpfmt, args);
      va_end (args);
    }
  else
    {
      // For compatibility with Matlab, print an empty message.
      // Octave's error routine requires a non-null input so use a SPACE.
      error (" ");
    }

  mex_context->abort ();
}

void
mexWarnMsgTxt (const char *s)
{
  warning ("%s", s);
}

void
mexWarnMsgIdAndTxt (const char *id, const char *fmt, ...)
{
  // FIXME: is this right?  What does Matlab do if fmt is NULL or
  //        an empty string?

  if (fmt && strlen (fmt) > 0)
    {
      const char *fname = mexFunctionName ();
      size_t len = strlen (fname) + 2 + strlen (fmt) + 1;
      OCTAVE_LOCAL_BUFFER (char, tmpfmt, len);
      sprintf (tmpfmt, "%s: %s", fname, fmt);
      va_list args;
      va_start (args, fmt);
      vwarning_with_id (id, tmpfmt, args);
      va_end (args);
    }
}

int
mexPrintf (const char *fmt, ...)
{
  int retval;
  va_list args;
  va_start (args, fmt);
  retval = octave_vformat (octave_stdout, fmt, args);
  va_end (args);
  return retval;
}

mxArray *
mexGetVariable (const char *space, const char *name)
{
  mxArray *retval = 0;

  octave_value val;

  if (! strcmp (space, "global"))
    val = get_global_value (name);
  else
    {
      // FIXME: should this be in variables.cc?

      unwind_protect frame;

      bool caller = ! strcmp (space, "caller");
      bool base = ! strcmp (space, "base");

      if (caller || base)
        {
          // MEX files don't create a separate frame in the call stack,
          // so we are already in the "caller" frame.

          if (base)
            {
              octave_call_stack::goto_base_frame ();

              if (error_state)
                return retval;

              frame.add_fcn (octave_call_stack::pop);
            }

          val = symbol_table::varval (name);
        }
      else
        mexErrMsgTxt ("mexGetVariable: symbol table does not exist");
    }

  if (val.is_defined ())
    {
      retval = mex_context->make_value (val);

      retval->set_name (name);
    }

  return retval;
}

const mxArray *
mexGetVariablePtr (const char *space, const char *name)
{
  return mexGetVariable (space, name);
}

int
mexPutVariable (const char *space, const char *name, const mxArray *ptr)
{
  if (! ptr)
    return 1;

  if (! name)
    return 1;

  if (name[0] == '\0')
    name = ptr->get_name ();

  if (! name || name[0] == '\0')
    return 1;

  if (! strcmp (space, "global"))
    set_global_value (name, mxArray::as_octave_value (ptr));
  else
    {
      // FIXME: should this be in variables.cc?

      unwind_protect frame;

      bool caller = ! strcmp (space, "caller");
      bool base = ! strcmp (space, "base");

      if (caller || base)
        {
          // MEX files don't create a separate frame in the call stack,
          // so we are already in the "caller" frame.

          if (base)
            {
              octave_call_stack::goto_base_frame ();

              if (error_state)
                return 1;

              frame.add_fcn (octave_call_stack::pop);
            }

          symbol_table::assign (name, mxArray::as_octave_value (ptr));
        }
      else
        mexErrMsgTxt ("mexPutVariable: symbol table does not exist");
    }

  return 0;
}

void
mexMakeArrayPersistent (mxArray *ptr)
{
  maybe_unmark_array (ptr);
}

void
mexMakeMemoryPersistent (void *ptr)
{
  maybe_unmark (ptr);
}

int
mexAtExit (void (*f) (void))
{
  if (mex_context)
    {
      octave_mex_function *curr_mex_fcn = mex_context->current_mex_function ();

      assert (curr_mex_fcn);

      curr_mex_fcn->atexit (f);
    }

  return 0;
}

const mxArray *
mexGet (double handle, const char *property)
{
  mxArray *m = 0;
  octave_value ret = get_property_from_handle (handle, property, "mexGet");

  if (!error_state && ret.is_defined ())
    m = ret.as_mxArray ();
  return m;
}

int
mexIsGlobal (const mxArray *ptr)
{
  return mxIsFromGlobalWS (ptr);
}

int
mexIsLocked (void)
{
  int retval = 0;

  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      retval = mislocked (fname);
    }

  return retval;
}

std::map<std::string,int> mex_lock_count;

void
mexLock (void)
{
  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      if (mex_lock_count.find (fname) == mex_lock_count.end ())
        mex_lock_count[fname] = 1;
      else
        mex_lock_count[fname]++;

      mlock ();
    }
}

int
mexSet (double handle, const char *property, mxArray *val)
{
  bool ret =
    set_property_in_handle (handle, property, mxArray::as_octave_value (val),
                            "mexSet");
  return (ret ? 0 : 1);
}

void
mexUnlock (void)
{
  if (mex_context)
    {
      const char *fname = mexFunctionName ();

      std::map<std::string,int>::iterator p = mex_lock_count.find (fname);

      if (p != mex_lock_count.end ())
        {
          int count = --mex_lock_count[fname];

          if (count == 0)
            {
              munlock (fname);

              mex_lock_count.erase (p);
            }
        }
    }
}
