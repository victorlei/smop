/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#if !defined (octave_ov_base_h)
#define octave_ov_base_h 1

#include <cstdlib>

#include <iosfwd>
#include <list>
#include <string>

#include "Range.h"
#include "data-conv.h"
#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-hdf5-id.h"

class Cell;
class mxArray;
class octave_map;
class octave_scalar_map;
class octave_value;
class octave_value_list;
class octave_stream;
class octave_function;
class octave_user_function;
class octave_user_script;
class octave_user_code;
class octave_fcn_handle;
class octave_fcn_inline;
class octave_value_list;
class octave_lvalue;

class tree_walker;

enum builtin_type_t
{
  btyp_double,
  btyp_float,
  btyp_complex,
  btyp_float_complex,
  btyp_int8,
  btyp_int16,
  btyp_int32,
  btyp_int64,
  btyp_uint8,
  btyp_uint16,
  btyp_uint32,
  btyp_uint64,
  btyp_bool,
  btyp_char,
  btyp_struct,
  btyp_cell,
  btyp_func_handle,
  btyp_unknown,
  btyp_num_types = btyp_unknown
};

extern OCTINTERP_API std::string
btyp_class_name [btyp_num_types];

extern OCTINTERP_API string_vector
get_builtin_classes (void);

inline bool btyp_isnumeric (builtin_type_t btyp)
{ return btyp <= btyp_uint64; }

inline bool btyp_isinteger (builtin_type_t btyp)
{ return btyp >= btyp_int8 && btyp <= btyp_uint64; }

inline bool btyp_isfloat (builtin_type_t btyp)
{ return btyp <= btyp_float_complex; }

inline bool btyp_isarray (builtin_type_t btyp)
{ return btyp <= btyp_char; }

// Compute numeric type for a possible mixed-type operation, using these rules:
// bool -> double
// single + double -> single
// real + complex -> complex
// integer + real -> integer
// uint + uint -> uint (the bigger one)
// sint + sint -> sint (the bigger one)
//
// failing otherwise.

extern OCTINTERP_API
builtin_type_t btyp_mixed_numeric (builtin_type_t x, builtin_type_t y);

template <class T>
struct class_to_btyp
{
  static const builtin_type_t btyp = btyp_unknown;
};

#define DEF_CLASS_TO_BTYP(CLASS,BTYP) \
template <> \
struct class_to_btyp<CLASS> \
{ static const builtin_type_t btyp = BTYP; }

DEF_CLASS_TO_BTYP (double, btyp_double);
DEF_CLASS_TO_BTYP (float, btyp_float);
DEF_CLASS_TO_BTYP (Complex, btyp_complex);
DEF_CLASS_TO_BTYP (FloatComplex, btyp_float_complex);
DEF_CLASS_TO_BTYP (octave_int8, btyp_int8);
DEF_CLASS_TO_BTYP (octave_int16, btyp_int16);
DEF_CLASS_TO_BTYP (octave_int32, btyp_int32);
DEF_CLASS_TO_BTYP (octave_int64, btyp_int64);
DEF_CLASS_TO_BTYP (octave_uint8, btyp_uint8);
DEF_CLASS_TO_BTYP (octave_uint16, btyp_uint16);
DEF_CLASS_TO_BTYP (octave_uint32, btyp_uint32);
DEF_CLASS_TO_BTYP (octave_uint64, btyp_uint64);
DEF_CLASS_TO_BTYP (bool, btyp_bool);
DEF_CLASS_TO_BTYP (char, btyp_char);

// T_ID is the type id of struct objects, set by register_type().
// T_NAME is the type name of struct objects.

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2 (OCTAVE_EMPTY_CPP_ARG)

#define DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA \
  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(virtual)

#define DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA2(VIRTUAL) \
  public: \
    VIRTUAL int type_id (void) const { return t_id; } \
    VIRTUAL std::string type_name (void) const { return t_name; } \
    VIRTUAL std::string class_name (void) const { return c_name; } \
    static int static_type_id (void) { return t_id; } \
    static std::string static_type_name (void) { return t_name; } \
    static std::string static_class_name (void) { return c_name; } \
    static void register_type (void); \
 \
  private: \
    static int t_id; \
    static const std::string t_name; \
    static const std::string c_name;


#define DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(t, n, c) \
  int t::t_id (-1); \
  const std::string t::t_name (n); \
  const std::string t::c_name (c); \
  void t::register_type (void) \
    { \
      static t exemplar; \
      octave_value v (&exemplar, true); \
      t_id = octave_value_typeinfo::register_type (t::t_name, t::c_name, v); \
    }

// A base value type, so that derived types only have to redefine what
// they need (if they are derived from octave_base_value instead of
// octave_value).

class
OCTINTERP_API
octave_base_value
{
public:

  typedef octave_base_value * (*type_conv_fcn) (const octave_base_value&);

  // type conversion, including result type information
  class type_conv_info
  {
  public:
    type_conv_info (type_conv_fcn f = 0, int t = -1)
      : _fcn (f), _type_id (t) { }

    operator type_conv_fcn (void) const { return _fcn; }

    octave_base_value * operator () (const octave_base_value &v) const
    { return (*_fcn) (v); }

    int type_id (void) const { return _type_id; }

  private:
    type_conv_fcn _fcn;
    int _type_id;
  };

  friend class octave_value;

  octave_base_value (void) : count (1) { }

  octave_base_value (const octave_base_value&) : count (1) { }

  virtual ~octave_base_value (void) { }

  // Unconditional clone. Always clones.
  virtual octave_base_value *
  clone (void) const { return new octave_base_value (*this); }

  // Empty clone.
  virtual octave_base_value *
  empty_clone (void) const;

  // Unique clone. Usually clones, but may be overridden to fake the
  // cloning when sharing copies is to be controlled from within an
  // instance (see octave_class).
  virtual octave_base_value *
  unique_clone (void) { return clone (); }

  virtual type_conv_info
  numeric_conversion_function (void) const
  { return type_conv_info (); }

  virtual type_conv_info
  numeric_demotion_function (void) const
  { return type_conv_info (); }

  virtual octave_value squeeze (void) const;

  virtual octave_value full_value (void) const;

  virtual octave_base_value *try_narrowing_conversion (void) { return 0; }

  virtual void maybe_economize (void) { }

  virtual Matrix size (void);

  virtual octave_idx_type numel (const octave_value_list&);

  virtual octave_value
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx);

  virtual octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout);

  virtual octave_value
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           bool auto_add);

  virtual octave_value_list
  subsref (const std::string& type,
           const std::list<octave_value_list>& idx,
           int nargout,
           const std::list<octave_lvalue> *lvalue_list);

  virtual octave_value
  do_index_op (const octave_value_list& idx, bool resize_ok = false);

  virtual octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx);

  virtual octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& idx,
                     const std::list<octave_lvalue> *lvalue_list);

  virtual void assign (const std::string&, const octave_value&) { }

  virtual octave_value
  subsasgn (const std::string& type,
            const std::list<octave_value_list>& idx,
            const octave_value& rhs);

  virtual octave_value
  undef_subsasgn (const std::string& type,
                  const std::list<octave_value_list>& idx,
                  const octave_value& rhs);

  virtual idx_vector index_vector (bool require_integers = false) const;

  virtual dim_vector dims (void) const { return dim_vector (); }

  octave_idx_type rows (void) const
  {
    const dim_vector dv = dims ();

    return dv(0);
  }

  octave_idx_type columns (void) const
  {
    const dim_vector dv = dims ();

    return dv(1);
  }

  virtual int ndims (void) const
  { return dims ().length (); }

  virtual octave_idx_type numel (void) const { return dims ().numel (); }

  virtual octave_idx_type capacity (void) const { return numel (); }

  virtual size_t byte_size (void) const { return 0; }

  virtual octave_idx_type nnz (void) const;

  virtual octave_idx_type nzmax (void) const;

  virtual octave_idx_type nfields (void) const;

  virtual octave_value reshape (const dim_vector&) const;

  virtual octave_value permute (const Array<int>& vec, bool = false) const;

  virtual octave_value resize (const dim_vector&, bool fill = false) const;

  virtual MatrixType matrix_type (void) const;

  virtual MatrixType matrix_type (const MatrixType& typ) const;

  virtual bool is_defined (void) const { return false; }

  bool is_empty (void) const { return (dims ().any_zero ()); }

  virtual bool is_cell (void) const { return false; }

  virtual bool is_cellstr (void) const { return false; }

  virtual bool is_real_scalar (void) const { return false; }

  virtual bool is_real_matrix (void) const { return false; }

  virtual bool is_complex_scalar (void) const { return false; }

  virtual bool is_complex_matrix (void) const { return false; }

  virtual bool is_bool_scalar (void) const { return false; }

  virtual bool is_bool_matrix (void) const { return false; }

  virtual bool is_char_matrix (void) const { return false; }

  virtual bool is_diag_matrix (void) const { return false; }

  virtual bool is_perm_matrix (void) const { return false; }

  virtual bool is_string (void) const { return false; }

  virtual bool is_sq_string (void) const { return false; }

  virtual bool is_range (void) const { return false; }

  virtual bool is_map (void) const { return false; }

  virtual bool is_object (void) const { return false; }

  virtual bool is_classdef_object (void) const { return false; }

  virtual bool is_java (void) const { return false; }

  virtual bool is_cs_list (void) const { return false; }

  virtual bool is_magic_colon (void) const { return false; }

  virtual bool is_all_va_args (void) const { return false; }

  virtual octave_value all (int = 0) const;

  virtual octave_value any (int = 0) const;

  virtual builtin_type_t builtin_type (void) const { return btyp_unknown; }

  virtual bool is_double_type (void) const { return false; }

  virtual bool is_single_type (void) const { return false; }

  virtual bool is_float_type (void) const { return false; }

  virtual bool is_int8_type (void) const { return false; }

  virtual bool is_int16_type (void) const { return false; }

  virtual bool is_int32_type (void) const { return false; }

  virtual bool is_int64_type (void) const { return false; }

  virtual bool is_uint8_type (void) const { return false; }

  virtual bool is_uint16_type (void) const { return false; }

  virtual bool is_uint32_type (void) const { return false; }

  virtual bool is_uint64_type (void) const { return false; }

  virtual bool is_bool_type (void) const { return false; }

  virtual bool is_integer_type (void) const { return false; }

  virtual bool is_real_type (void) const { return false; }

  virtual bool is_complex_type (void) const { return false; }

  // Would be nice to get rid of the next four functions:

  virtual bool is_scalar_type (void) const { return false; }

  virtual bool is_matrix_type (void) const { return false; }

  virtual bool is_numeric_type (void) const { return false; }

  virtual bool is_sparse_type (void) const { return false; }

  virtual bool is_true (void) const { return false; }

  virtual bool is_null_value (void) const { return false; }

  virtual bool is_constant (void) const { return false; }

  virtual bool is_function_handle (void) const { return false; }

  virtual bool is_anonymous_function (void) const { return false; }

  virtual bool is_inline_function (void) const { return false; }

  virtual bool is_function (void) const { return false; }

  virtual bool is_user_script (void) const { return false; }

  virtual bool is_user_function (void) const { return false; }

  virtual bool is_user_code (void) const { return false; }

  virtual bool is_builtin_function (void) const { return false; }

  virtual bool is_dld_function (void) const { return false; }

  virtual bool is_mex_function (void) const { return false; }

  virtual void erase_subfunctions (void) { }

  virtual short int short_value (bool = false, bool = false) const;

  virtual unsigned short int ushort_value (bool = false, bool = false) const;

  virtual int int_value (bool = false, bool = false) const;

  virtual unsigned int uint_value (bool = false, bool = false) const;

  virtual int nint_value (bool = false) const;

  virtual long int long_value (bool = false, bool = false) const;

  virtual unsigned long int ulong_value (bool = false, bool = false) const;

  virtual int64_t int64_value (bool = false, bool = false) const;

  virtual uint64_t uint64_value (bool = false, bool = false) const;

  virtual double double_value (bool = false) const;

  virtual float float_value (bool = false) const;

  virtual double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  virtual float float_scalar_value (bool frc_str_conv = false) const
  { return float_value (frc_str_conv); }

  virtual Cell cell_value (void) const;

  virtual Matrix matrix_value (bool = false) const;

  virtual FloatMatrix float_matrix_value (bool = false) const;

  virtual NDArray array_value (bool = false) const;

  virtual FloatNDArray float_array_value (bool = false) const;

  virtual Complex complex_value (bool = false) const;

  virtual FloatComplex float_complex_value (bool = false) const;

  virtual ComplexMatrix complex_matrix_value (bool = false) const;

  virtual FloatComplexMatrix float_complex_matrix_value (bool = false) const;

  virtual ComplexNDArray complex_array_value (bool = false) const;

  virtual FloatComplexNDArray float_complex_array_value (bool = false) const;

  virtual bool bool_value (bool = false) const;

  virtual boolMatrix bool_matrix_value (bool = false) const;

  virtual boolNDArray bool_array_value (bool = false) const;

  virtual charMatrix char_matrix_value (bool force = false) const;

  virtual charNDArray char_array_value (bool = false) const;

  virtual SparseMatrix sparse_matrix_value (bool = false) const;

  virtual SparseComplexMatrix sparse_complex_matrix_value (bool = false) const;

  virtual SparseBoolMatrix sparse_bool_matrix_value (bool = false) const;

  virtual DiagMatrix diag_matrix_value (bool = false) const;

  virtual FloatDiagMatrix float_diag_matrix_value (bool = false) const;

  virtual ComplexDiagMatrix complex_diag_matrix_value (bool = false) const;

  virtual FloatComplexDiagMatrix
  float_complex_diag_matrix_value (bool = false) const;

  virtual PermMatrix perm_matrix_value (void) const;

  virtual octave_int8 int8_scalar_value (void) const;

  virtual octave_int16 int16_scalar_value (void) const;

  virtual octave_int32 int32_scalar_value (void) const;

  virtual octave_int64 int64_scalar_value (void) const;

  virtual octave_uint8 uint8_scalar_value (void) const;

  virtual octave_uint16 uint16_scalar_value (void) const;

  virtual octave_uint32 uint32_scalar_value (void) const;

  virtual octave_uint64 uint64_scalar_value (void) const;

  virtual int8NDArray int8_array_value (void) const;

  virtual int16NDArray int16_array_value (void) const;

  virtual int32NDArray int32_array_value (void) const;

  virtual int64NDArray int64_array_value (void) const;

  virtual uint8NDArray uint8_array_value (void) const;

  virtual uint16NDArray uint16_array_value (void) const;

  virtual uint32NDArray uint32_array_value (void) const;

  virtual uint64NDArray uint64_array_value (void) const;

  virtual string_vector all_strings (bool pad = false) const;

  virtual std::string string_value (bool force = false) const;

  virtual Array<std::string> cellstr_value (void) const;

  virtual Range range_value (void) const;

  virtual octave_map map_value (void) const;

  virtual octave_scalar_map scalar_map_value (void) const;

  virtual string_vector map_keys (void) const;

  virtual size_t nparents (void) const;

  virtual std::list<std::string> parent_class_name_list (void) const;

  virtual string_vector parent_class_names (void) const;

  virtual octave_base_value *find_parent_class (const std::string&)
  { return 0; }

  virtual octave_base_value *unique_parent_class (const std::string&)
  { return 0; }

  virtual bool is_instance_of (const std::string&) const
  { return false; }

  virtual octave_function *function_value (bool silent = false);

  virtual octave_user_function *user_function_value (bool silent = false);

  virtual octave_user_script *user_script_value (bool silent = false);

  virtual octave_user_code *user_code_value (bool silent = false);

  virtual octave_fcn_handle *fcn_handle_value (bool silent = false);

  virtual octave_fcn_inline *fcn_inline_value (bool silent = false);

  virtual octave_value_list list_value (void) const;

  virtual octave_value convert_to_str (bool pad = false, bool force = false,
                                       char type = '\'') const;
  virtual octave_value
  convert_to_str_internal (bool pad, bool force, char type) const;

  virtual void convert_to_row_or_column_vector (void);

  virtual bool print_as_scalar (void) const { return false; }

  virtual void print (std::ostream& os, bool pr_as_read_syntax = false);

  virtual void
  print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  virtual bool
  print_name_tag (std::ostream& os, const std::string& name) const;

  virtual void
  print_with_name (std::ostream& output_buf, const std::string& name,
                   bool print_padding = true);

  virtual void short_disp (std::ostream& os) const { os << "..."; }

  virtual void print_info (std::ostream& os, const std::string& prefix) const;

  virtual bool save_ascii (std::ostream& os);

  virtual bool load_ascii (std::istream& is);

  virtual bool save_binary (std::ostream& os, bool& save_as_floats);

  virtual bool load_binary (std::istream& is, bool swap,
                            oct_mach_info::float_format fmt);

  virtual bool
  save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  virtual bool
  load_hdf5 (octave_hdf5_id loc_id, const char *name);

  virtual int
  write (octave_stream& os, int block_size,
         oct_data_conv::data_type output_type, int skip,
         oct_mach_info::float_format flt_fmt) const;

  virtual void *mex_get_data (void) const { return 0; }

  virtual octave_idx_type *mex_get_ir (void) const { return 0; }

  virtual octave_idx_type *mex_get_jc (void) const { return 0; }

  virtual mxArray *as_mxArray (void) const;

  virtual octave_value diag (octave_idx_type k = 0) const;

  virtual octave_value diag (octave_idx_type m, octave_idx_type n) const;

  virtual octave_value sort (octave_idx_type dim = 0,
                             sortmode mode = ASCENDING) const;
  virtual octave_value sort (Array<octave_idx_type> &sidx,
                             octave_idx_type dim = 0,
                             sortmode mode = ASCENDING) const;

  virtual sortmode is_sorted (sortmode mode = UNSORTED) const;

  virtual Array<octave_idx_type>
  sort_rows_idx (sortmode mode = ASCENDING) const;

  virtual sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  virtual void lock (void);

  virtual void unlock (void);

  virtual bool islocked (void) const { return false; }

  virtual void dump (std::ostream& os) const;

  // Standard mappers. Register new ones here.
  enum unary_mapper_t
  {
    umap_abs,
    umap_acos,
    umap_acosh,
    umap_angle,
    umap_arg,
    umap_asin,
    umap_asinh,
    umap_atan,
    umap_atanh,
    umap_cbrt,
    umap_ceil,
    umap_conj,
    umap_cos,
    umap_cosh,
    umap_erf,
    umap_erfinv,
    umap_erfcinv,
    umap_erfc,
    umap_erfcx,
    umap_erfi,
    umap_dawson,
    umap_exp,
    umap_expm1,
    umap_finite,
    umap_fix,
    umap_floor,
    umap_gamma,
    umap_imag,
    umap_isinf,
    umap_isna,
    umap_isnan,
    umap_lgamma,
    umap_log,
    umap_log2,
    umap_log10,
    umap_log1p,
    umap_real,
    umap_round,
    umap_roundb,
    umap_signum,
    umap_sin,
    umap_sinh,
    umap_sqrt,
    umap_tan,
    umap_tanh,
    umap_xisalnum,
    umap_xisalpha,
    umap_xisascii,
    umap_xiscntrl,
    umap_xisdigit,
    umap_xisgraph,
    umap_xislower,
    umap_xisprint,
    umap_xispunct,
    umap_xisspace,
    umap_xisupper,
    umap_xisxdigit,
    umap_xsignbit,
    umap_xtoascii,
    umap_xtolower,
    umap_xtoupper,
    umap_unknown,
    num_unary_mappers = umap_unknown
  };

  virtual octave_value map (unary_mapper_t) const;

  // These are fast indexing & assignment shortcuts for extracting
  // or inserting a single scalar from/to an array.

  // Extract the n-th element, aka val(n). Result is undefined if val is not an
  // array type or n is out of range. Never error.
  virtual octave_value
  fast_elem_extract (octave_idx_type n) const;

  // Assign the n-th element, aka val(n) = x. Returns false if val is not an
  // array type, x is not a matching scalar type, or n is out of range.
  // Never error.
  virtual bool
  fast_elem_insert (octave_idx_type n, const octave_value& x);

  // This is a helper for the above, to be overridden in scalar types.  The
  // whole point is to handle the insertion efficiently with just *two* VM
  // calls, which is basically the theoretical minimum.
  virtual bool
  fast_elem_insert_self (void *where, builtin_type_t btyp) const;

  // Grab the reference count. For use by jit.
  void
  grab (void)
  {
    ++count;
  }

  // Release the reference count. For use by jit.
  void
  release (void)
  {
    if (--count == 0)
      delete this;
  }

protected:

  // This should only be called for derived types.

  octave_value numeric_assign (const std::string& type,
                               const std::list<octave_value_list>& idx,
                               const octave_value& rhs);

  void reset_indent_level (void) const
  { curr_print_indent_level = 0; }

  void increment_indent_level (void) const
  { curr_print_indent_level += 2; }

  void decrement_indent_level (void) const
  { curr_print_indent_level -= 2; }

  int current_print_indent_level (void) const
  { return curr_print_indent_level; }

  void indent (std::ostream& os) const;

  void newline (std::ostream& os) const;

  void reset (void) const;

  // A reference count.
  // NOTE: the declaration is octave_idx_type because with 64-bit indexing,
  // it is well possible to have more than MAX_INT copies of a single value
  // (think of an empty cell array with >2G elements).
  octave_refcount<octave_idx_type> count;

  static const char *get_umap_name (unary_mapper_t);

  void gripe_load (const char *type) const;
  void gripe_save (const char *type) const;

private:

  static int curr_print_indent_level;
  static bool beginning_of_line;

  DECLARE_OV_BASE_TYPEID_FUNCTIONS_AND_DATA
};

// TRUE means to perform automatic sparse to real mutation if there
// is memory to be saved
extern OCTINTERP_API bool Vsparse_auto_mutate;

// Utility function to convert C++ arguments used in subsref/subsasgn into an
// octave_value_list object that can be used to call a function/method in the
// interpreter.
extern OCTINTERP_API octave_value
make_idx_args (const std::string& type,
               const std::list<octave_value_list>& idx,
               const std::string& who);

// Tells whether some regular octave_value_base methods are being called from
// within the "builtin" function.
extern OCTINTERP_API bool called_from_builtin (void);

#endif
