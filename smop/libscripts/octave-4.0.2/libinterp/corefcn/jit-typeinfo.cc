/*

Copyright (C) 2012-2015 Max Brister

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

// Author: Max Brister <max@2bass.com>

// defines required by llvm
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_LLVM

#include "jit-typeinfo.h"

#ifdef HAVE_LLVM_IR_VERIFIER_H
#include <llvm/IR/Verifier.h>
#else
#include <llvm/Analysis/Verifier.h>
#endif

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#ifdef HAVE_LLVM_IR_FUNCTION_H
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#else
#include <llvm/GlobalVariable.h>
#include <llvm/LLVMContext.h>
#include <llvm/Function.h>
#include <llvm/Instructions.h>
#include <llvm/Intrinsics.h>
#endif

#ifdef HAVE_LLVM_SUPPORT_IRBUILDER_H
#include <llvm/Support/IRBuilder.h>
#elif defined(HAVE_LLVM_IR_IRBUILDER_H)
#include <llvm/IR/IRBuilder.h>
#else
#include <llvm/IRBuilder.h>
#endif

#include <llvm/Support/raw_os_ostream.h>

#include "jit-ir.h"
#include "ov.h"
#include "ov-builtin.h"
#include "ov-complex.h"
#include "ov-scalar.h"
#include "pager.h"

static llvm::LLVMContext& context = llvm::getGlobalContext ();

jit_typeinfo *jit_typeinfo::instance = 0;

std::ostream& jit_print (std::ostream& os, jit_type *atype)
{
  if (! atype)
    return os << "null";
  return os << atype->name ();
}

// function that jit code calls
extern "C" void
octave_jit_print_any (const char *name, octave_base_value *obv)
{
  obv->print_with_name (octave_stdout, name, true);
}

extern "C" void
octave_jit_print_scalar (const char *name, double value)
{
  // FIXME: We should avoid allocating a new octave_scalar each time
  octave_value ov (value);
  ov.print_with_name (octave_stdout, name);
}

extern "C" octave_base_value*
octave_jit_binary_any_any (octave_value::binary_op op, octave_base_value *lhs,
                           octave_base_value *rhs)
{
  octave_value olhs (lhs, true);
  octave_value orhs (rhs, true);
  octave_value result = do_binary_op (op, olhs, orhs);
  octave_base_value *rep = result.internal_rep ();
  rep->grab ();
  return rep;
}

extern "C" octave_idx_type
octave_jit_compute_nelem (double base, double limit, double inc)
{
  Range rng = Range (base, limit, inc);
  return rng.nelem ();
}

extern "C" void
octave_jit_release_any (octave_base_value *obv)
{
  obv->release ();
}

extern "C" void
octave_jit_release_matrix (jit_matrix *m)
{
  delete m->array;
}

extern "C" octave_base_value *
octave_jit_grab_any (octave_base_value *obv)
{
  obv->grab ();
  return obv;
}

extern "C" jit_matrix
octave_jit_grab_matrix (jit_matrix *m)
{
  return *m->array;
}

extern "C" octave_base_value *
octave_jit_cast_any_matrix (jit_matrix *m)
{
  octave_value ret (*m->array);
  octave_base_value *rep = ret.internal_rep ();
  rep->grab ();
  delete m->array;

  return rep;
}

extern "C" jit_matrix
octave_jit_cast_matrix_any (octave_base_value *obv)
{
  NDArray m = obv->array_value ();
  obv->release ();
  return m;
}

extern "C" octave_base_value *
octave_jit_cast_any_range (jit_range *rng)
{
  Range temp (*rng);
  octave_value ret (temp);
  octave_base_value *rep = ret.internal_rep ();
  rep->grab ();

  return rep;
}
extern "C" jit_range
octave_jit_cast_range_any (octave_base_value *obv)
{

  jit_range r (obv->range_value ());
  obv->release ();
  return r;
}

extern "C" double
octave_jit_cast_scalar_any (octave_base_value *obv)
{
  double ret = obv->double_value ();
  obv->release ();
  return ret;
}

extern "C" octave_base_value *
octave_jit_cast_any_scalar (double value)
{
  return new octave_scalar (value);
}

extern "C" Complex
octave_jit_cast_complex_any (octave_base_value *obv)
{
  Complex ret = obv->complex_value ();
  obv->release ();
  return ret;
}

extern "C" octave_base_value *
octave_jit_cast_any_complex (Complex c)
{
  if (c.imag () == 0)
    return new octave_scalar (c.real ());
  else
    return new octave_complex (c);
}

extern "C" void
octave_jit_gripe_nan_to_logical_conversion (void)
{
  try
    {
      gripe_nan_to_logical_conversion ();
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }
}

extern "C" void
octave_jit_ginvalid_index (void)
{
  try
    {
      gripe_invalid_index ();
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }
}

extern "C" void
octave_jit_gindex_range (int nd, int dim, octave_idx_type iext,
                         octave_idx_type ext)
{
  try
    {
      gripe_index_out_of_range (nd, dim, iext, ext);
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }
}

extern "C" jit_matrix
octave_jit_paren_subsasgn_impl (jit_matrix *mat, octave_idx_type index,
                                double value)
{
  NDArray *array = mat->array;
  if (array->nelem () < index)
    array->resize1 (index);

  double *data = array->fortran_vec ();
  data[index - 1] = value;

  mat->update ();
  return *mat;
}

static void
make_indices (double *indices, octave_idx_type idx_count,
              Array<idx_vector>& result)
{
  result.resize (dim_vector (1, idx_count));
  for (octave_idx_type i = 0; i < idx_count; ++i)
    result(i) = idx_vector (indices[i]);
}

extern "C" double
octave_jit_paren_scalar (jit_matrix *mat, double *indicies,
                         octave_idx_type idx_count)
{
  // FIXME: Replace this with a more optimal version
  try
    {
      Array<idx_vector> idx;
      make_indices (indicies, idx_count, idx);

      Array<double> ret = mat->array->index (idx);
      return ret.xelem (0);
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
      return 0;
    }
}

extern "C" jit_matrix
octave_jit_paren_scalar_subsasgn (jit_matrix *mat, double *indices,
                                  octave_idx_type idx_count, double value)
{
  // FIXME: Replace this with a more optimal version
  jit_matrix ret;
  try
    {
      Array<idx_vector> idx;
      make_indices (indices, idx_count, idx);

      Matrix temp (1, 1);
      temp.xelem(0) = value;
      mat->array->assign (idx, temp);
      ret.update (mat->array);
    }
  catch (const octave_execution_exception&)
    {
      gripe_library_execution_error ();
    }

  return ret;
}

extern "C" jit_matrix
octave_jit_paren_subsasgn_matrix_range (jit_matrix *mat, jit_range *index,
                                        double value)
{
  NDArray *array = mat->array;
  bool done = false;

  // optimize for the simple case (no resizing and no errors)
  if (*array->jit_ref_count () == 1
      && index->all_elements_are_ints ())
    {
      // this code is similar to idx_vector::fill, but we avoid allocating an
      // idx_vector and its associated rep
      octave_idx_type start = static_cast<octave_idx_type> (index->base) - 1;
      octave_idx_type step = static_cast<octave_idx_type> (index->inc);
      octave_idx_type nelem = index->nelem;
      octave_idx_type final = start + nelem * step;
      if (step < 0)
        {
          step = -step;
          std::swap (final, start);
        }

      if (start >= 0 && final < mat->slice_len)
        {
          done = true;

          double *data = array->jit_slice_data ();
          if (step == 1)
            std::fill (data + start, data + start + nelem, value);
          else
            {
              for (octave_idx_type i = start; i < final; i += step)
                data[i] = value;
            }
        }
    }

  if (! done)
    {
      idx_vector idx (*index);
      NDArray avalue (dim_vector (1, 1));
      avalue.xelem (0) = value;
      array->assign (idx, avalue);
    }

  jit_matrix ret;
  ret.update (array);
  return ret;
}

extern "C" double
octave_jit_end_matrix (jit_matrix *mat, octave_idx_type idx,
                       octave_idx_type count)
{
  octave_idx_type ndim = mat->dimensions[-1];
  if (ndim == count)
    return mat->dimensions[idx];
  else if (ndim > count)
    {
      if (idx == count - 1)
        {
          double ret = mat->dimensions[idx];
          for (octave_idx_type i = idx + 1; i < ndim; ++i)
            ret *= mat->dimensions[idx];
          return ret;
        }

      return mat->dimensions[idx];
    }
  else // ndim < count
    return idx < ndim ? mat->dimensions[idx] : 1;
}

extern "C" octave_base_value *
octave_jit_create_undef (void)
{
  octave_value undef;
  octave_base_value *ret = undef.internal_rep ();
  ret->grab ();

  return ret;
}

extern "C" Complex
octave_jit_complex_mul (Complex lhs, Complex rhs)
{
  if (lhs.imag () == 0 && rhs.imag() == 0)
    return Complex (lhs.real () * rhs.real (), 0);

  return lhs * rhs;
}

extern "C" Complex
octave_jit_complex_div (Complex lhs, Complex rhs)
{
  // see src/OPERATORS/op-cs-cs.cc
  if (rhs == 0.0)
    gripe_divide_by_zero ();

  return lhs / rhs;
}

// FIXME: CP form src/xpow.cc
static inline int
xisint (double x)
{
  return (D_NINT (x) == x
          && ((x >= 0 && x < std::numeric_limits<int>::max ())
              || (x <= 0 && x > std::numeric_limits<int>::min ())));
}

extern "C" Complex
octave_jit_pow_scalar_scalar (double lhs, double rhs)
{
  // FIXME: almost CP from src/xpow.cc
  if (lhs < 0.0 && ! xisint (rhs))
    return std::pow (Complex (lhs), rhs);
  return std::pow (lhs, rhs);
}

extern "C" Complex
octave_jit_pow_complex_complex (Complex lhs, Complex rhs)
{
  if (lhs.imag () == 0 && rhs.imag () == 0)
    return octave_jit_pow_scalar_scalar (lhs.real (), rhs.real ());
  return std::pow (lhs, rhs);
}

extern "C" Complex
octave_jit_pow_complex_scalar (Complex lhs, double rhs)
{
  if (lhs.imag () == 0)
    return octave_jit_pow_scalar_scalar (lhs.real (), rhs);
  return std::pow (lhs, rhs);
}

extern "C" Complex
octave_jit_pow_scalar_complex (double lhs, Complex rhs)
{
  if (rhs.imag () == 0)
    return octave_jit_pow_scalar_scalar (lhs, rhs.real ());
  return std::pow (lhs, rhs);
}

extern "C" void
octave_jit_print_matrix (jit_matrix *m)
{
  std::cout << *m << std::endl;
}

static void
gripe_bad_result (void)
{
  error ("incorrect type information given to the JIT compiler");
}

// FIXME: Add support for multiple outputs
extern "C" octave_base_value *
octave_jit_call (octave_builtin::fcn fn, size_t nargin,
                 octave_base_value **argin, jit_type *result_type)
{
  octave_value_list ovl (nargin);
  for (size_t i = 0; i < nargin; ++i)
    ovl.xelem (i) = octave_value (argin[i]);

  ovl = fn (ovl, 1);

  // FIXME: Check result_type somehow
  if (result_type)
    {
      if (ovl.length () < 1)
        {
          gripe_bad_result ();
          return 0;
        }

      octave_value result = ovl.xelem(0);
      octave_base_value *ret = result.internal_rep ();
      ret->grab ();
      return ret;
    }

  if (! (ovl.length () == 0
         || (ovl.length () == 1 && ovl.xelem (0).is_undefined ())))
    gripe_bad_result ();

  return 0;
}

// -------------------- jit_range --------------------
bool
jit_range::all_elements_are_ints () const
{
  Range r (*this);
  return r.all_elements_are_ints ();
}

std::ostream&
operator<< (std::ostream& os, const jit_range& rng)
{
  return os << "Range[" << rng.base << ", " << rng.limit << ", " << rng.inc
         << ", " << rng.nelem << "]";
}

// -------------------- jit_matrix --------------------

std::ostream&
operator<< (std::ostream& os, const jit_matrix& mat)
{
  return os << "Matrix[" << mat.ref_count << ", " << mat.slice_data << ", "
         << mat.slice_len << ", " << mat.dimensions << ", "
         << mat.array << "]";
}

// -------------------- jit_type --------------------
jit_type::jit_type (const std::string& aname, jit_type *aparent,
                    llvm::Type *allvm_type, bool askip_paren, int aid) :
  mname (aname), mparent (aparent), llvm_type (allvm_type), mid (aid),
  mdepth (aparent ? aparent->mdepth + 1 : 0), mskip_paren (askip_paren)
{
  std::memset (msret, 0, sizeof (msret));
  std::memset (mpointer_arg, 0, sizeof (mpointer_arg));
  std::memset (mpack, 0, sizeof (mpack));
  std::memset (munpack, 0, sizeof (munpack));

  for (size_t i = 0; i < jit_convention::length; ++i)
    mpacked_type[i] = llvm_type;
}

llvm::Type *
jit_type::to_llvm_arg (void) const
{
  return llvm_type ? llvm_type->getPointerTo () : 0;
}

// -------------------- jit_function --------------------
jit_function::jit_function () : module (0), llvm_function (0), mresult (0),
                                call_conv (jit_convention::length),
                                mcan_error (false)
{}

jit_function::jit_function (llvm::Module *amodule,
                            jit_convention::type acall_conv,
                            const llvm::Twine& aname, jit_type *aresult,
                            const std::vector<jit_type *>& aargs)
  : module (amodule), mresult (aresult), args (aargs), call_conv (acall_conv),
    mcan_error (false)
{
  llvm::SmallVector<llvm::Type *, 15> llvm_args;

  llvm::Type *rtype = llvm::Type::getVoidTy (context);
  if (mresult)
    {
      rtype = mresult->packed_type (call_conv);
      if (sret ())
        {
          llvm_args.push_back (rtype->getPointerTo ());
          rtype = llvm::Type::getVoidTy (context);
        }
    }

  for (std::vector<jit_type *>::const_iterator iter = args.begin ();
       iter != args.end (); ++iter)
    {
      jit_type *ty = *iter;
      assert (ty);
      llvm::Type *argty = ty->packed_type (call_conv);
      if (ty->pointer_arg (call_conv))
        argty = argty->getPointerTo ();

      llvm_args.push_back (argty);
    }

  // we mark all functinos as external linkage because this prevents llvm
  // from getting rid of always inline functions
  llvm::FunctionType *ft = llvm::FunctionType::get (rtype, llvm_args, false);
  llvm_function = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                                          aname, module);

  if (sret ())
    {
#ifdef FUNCTION_ADDATTRIBUTE_ARG_IS_ATTRIBUTES
      llvm::AttrBuilder attr_builder;
      attr_builder.addAttribute (llvm::Attributes::StructRet);
      llvm::Attributes attrs = llvm::Attributes::get(context, attr_builder);
      llvm_function->addAttribute (1, attrs);
#else
      llvm_function->addAttribute (1, llvm::Attribute::StructRet);
#endif
    }

  if (call_conv == jit_convention::internal)
#ifdef FUNCTION_ADDFNATTR_ARG_IS_ATTRIBUTES
    llvm_function->addFnAttr (llvm::Attributes::AlwaysInline);
#else
    llvm_function->addFnAttr (llvm::Attribute::AlwaysInline);
#endif
}

jit_function::jit_function (const jit_function& fn, jit_type *aresult,
                            const std::vector<jit_type *>& aargs)
  : module (fn.module), llvm_function (fn.llvm_function), mresult (aresult),
    args (aargs), call_conv (fn.call_conv), mcan_error (fn.mcan_error)
{
}

jit_function::jit_function (const jit_function& fn)
  : module (fn.module), llvm_function (fn.llvm_function), mresult (fn.mresult),
    args (fn.args), call_conv (fn.call_conv), mcan_error (fn.mcan_error)
{}

void
jit_function::erase (void)
{
  if (! llvm_function)
    return;

  llvm_function->eraseFromParent ();
  llvm_function = 0;
}

std::string
jit_function::name (void) const
{
  return llvm_function->getName ();
}

llvm::BasicBlock *
jit_function::new_block (const std::string& aname,
                         llvm::BasicBlock *insert_before)
{
  return llvm::BasicBlock::Create (context, aname, llvm_function,
                                   insert_before);
}

llvm::Value *
jit_function::call (llvm::IRBuilderD& builder,
                    const std::vector<jit_value *>& in_args) const
{
  if (! valid ())
    throw jit_fail_exception ("Call not implemented");

  assert (in_args.size () == args.size ());
  std::vector<llvm::Value *> llvm_args (args.size ());
  for (size_t i = 0; i < in_args.size (); ++i)
    llvm_args[i] = in_args[i]->to_llvm ();

  return call (builder, llvm_args);
}

llvm::Value *
jit_function::call (llvm::IRBuilderD& builder,
                    const std::vector<llvm::Value *>& in_args) const
{
  if (! valid ())
    throw jit_fail_exception ("Call not implemented");

  assert (in_args.size () == args.size ());
  llvm::SmallVector<llvm::Value *, 10> llvm_args;
  llvm_args.reserve (in_args.size () + sret ());

  llvm::BasicBlock *insert_block = builder.GetInsertBlock ();
  llvm::Function *parent = insert_block->getParent ();
  assert (parent);

  // we insert allocas inside the prelude block to prevent stack overflows
  llvm::BasicBlock& prelude = parent->getEntryBlock ();
  llvm::IRBuilder<> pre_builder (&prelude, prelude.begin ());

  llvm::AllocaInst *sret_mem = 0;
  if (sret ())
    {
      sret_mem = pre_builder.CreateAlloca (mresult->packed_type (call_conv));
      llvm_args.push_back (sret_mem);
    }

  for (size_t i = 0; i < in_args.size (); ++i)
    {
      llvm::Value *arg = in_args[i];
      jit_type::convert_fn convert = args[i]->pack (call_conv);
      if (convert)
        arg = convert (builder, arg);

      if (args[i]->pointer_arg (call_conv))
        {
          llvm::Type *ty = args[i]->packed_type (call_conv);
          llvm::Value *alloca = pre_builder.CreateAlloca (ty);
          builder.CreateStore (arg, alloca);
          arg = alloca;
        }

      llvm_args.push_back (arg);
    }

  llvm::CallInst *callinst = builder.CreateCall (llvm_function, llvm_args);
  llvm::Value *ret = callinst;

  if (sret ())
    {
#ifdef CALLINST_ADDATTRIBUTE_ARG_IS_ATTRIBUTES
      llvm::AttrBuilder attr_builder;
      attr_builder.addAttribute(llvm::Attributes::StructRet);
      llvm::Attributes attrs = llvm::Attributes::get(context, attr_builder);
      callinst->addAttribute (1, attrs);
#else
      callinst->addAttribute (1, llvm::Attribute::StructRet);
#endif
      ret = builder.CreateLoad (sret_mem);
    }

  if (mresult)
    {
      jit_type::convert_fn unpack = mresult->unpack (call_conv);
      if (unpack)
        ret = unpack (builder, ret);
    }

  return ret;
}

llvm::Value *
jit_function::argument (llvm::IRBuilderD& builder, size_t idx) const
{
  assert (idx < args.size ());

  // FIXME: We should be treating arguments like a list, not a vector. Shouldn't
  // matter much for now, as the number of arguments shouldn't be much bigger
  // than 4
  llvm::Function::arg_iterator iter = llvm_function->arg_begin ();
  if (sret ())
    ++iter;

  for (size_t i = 0; i < idx; ++i, ++iter);

  if (args[idx]->pointer_arg (call_conv))
    return builder.CreateLoad (iter);

  return iter;
}

void
jit_function::do_return (llvm::IRBuilderD& builder, llvm::Value *rval,
                         bool verify)
{
  assert (! rval == ! mresult);

  if (rval)
    {
      jit_type::convert_fn convert = mresult->pack (call_conv);
      if (convert)
        rval = convert (builder, rval);

      if (sret ())
        {
          builder.CreateStore (rval, llvm_function->arg_begin ());
          builder.CreateRetVoid ();
        }
      else
        builder.CreateRet (rval);
    }
  else
    builder.CreateRetVoid ();

  if (verify)
    llvm::verifyFunction (*llvm_function);
}

void
jit_function::do_add_mapping (llvm::ExecutionEngine *engine, void *fn)
{
  assert (valid ());
  engine->addGlobalMapping (llvm_function, fn);
}

std::ostream&
operator<< (std::ostream& os, const jit_function& fn)
{
  llvm::Function *lfn = fn.to_llvm ();
  os << "jit_function: cc=" << fn.call_conv;
  llvm::raw_os_ostream llvm_out (os);
  lfn->print (llvm_out);
  llvm_out.flush ();
  return os;
}

// -------------------- jit_operation --------------------
jit_operation::~jit_operation (void)
{
  for (generated_map::iterator iter = generated.begin ();
       iter != generated.end (); ++iter)
    {
      delete iter->first;
      delete iter->second;
    }
}

void
jit_operation::add_overload (const jit_function& func,
                             const std::vector<jit_type*>& args)
{
  if (args.size () >= overloads.size ())
    overloads.resize (args.size () + 1);

  Array<jit_function>& over = overloads[args.size ()];
  dim_vector dv (over.dims ());
  Array<octave_idx_type> idx = to_idx (args);
  bool must_resize = false;

  if (dv.length () != idx.numel ())
    {
      dv.resize (idx.numel ());
      must_resize = true;
    }

  for (octave_idx_type i = 0; i < dv.length (); ++i)
    if (dv(i) <= idx(i))
      {
        must_resize = true;
        dv(i) = idx(i) + 1;
      }

  if (must_resize)
    over.resize (dv);

  over(idx) = func;
}

const jit_function&
jit_operation::overload (const std::vector<jit_type*>& types) const
{
  static jit_function null_overload;
  for (size_t i  =0; i < types.size (); ++i)
    if (! types[i])
      return null_overload;

  if (types.size () >= overloads.size ())
    return do_generate (types);

  const Array<jit_function>& over = overloads[types.size ()];
  dim_vector dv (over.dims ());
  Array<octave_idx_type> idx = to_idx (types);
  for (octave_idx_type i = 0; i < dv.length (); ++i)
    if (idx(i) >= dv(i))
      return do_generate (types);

  const jit_function& ret = over(idx);
  if (! ret.valid ())
    return do_generate (types);

  return ret;
}

Array<octave_idx_type>
jit_operation::to_idx (const std::vector<jit_type*>& types) const
{
  octave_idx_type numel = types.size ();
  numel = std::max (numel, static_cast<octave_idx_type>(2));

  Array<octave_idx_type> idx (dim_vector (1, numel));
  for (octave_idx_type i = 0; i < static_cast<octave_idx_type> (types.size ());
       ++i)
    idx(i) = types[i]->type_id ();

  if (types.size () == 0)
    idx(0) = idx(1) = 0;
  if (types.size () == 1)
    {
      idx(1) = idx(0);
      idx(0) = 0;
    }

  return idx;
}

const jit_function&
jit_operation::do_generate (const signature_vec& types) const
{
  static jit_function null_overload;
  generated_map::const_iterator find = generated.find (&types);
  if (find != generated.end ())
    {
      if (find->second)
        return *find->second;
      else
        return null_overload;
    }

  jit_function *ret = generate (types);
  generated[new signature_vec (types)] = ret;
  return ret ? *ret : null_overload;
}

jit_function *
jit_operation::generate (const signature_vec&) const
{
  return 0;
}

bool
jit_operation::signature_cmp
::operator() (const signature_vec *lhs, const signature_vec *rhs) const
{
  const signature_vec& l = *lhs;
  const signature_vec& r = *rhs;

  if (l.size () < r.size ())
    return true;
  else if (l.size () > r.size ())
    return false;

  for (size_t i = 0; i < l.size (); ++i)
    {
      if (l[i]->type_id () < r[i]->type_id ())
        return true;
      else if (l[i]->type_id () > r[i]->type_id ())
        return false;
    }

  return false;
}

// -------------------- jit_index_operation --------------------
jit_function *
jit_index_operation::generate (const signature_vec& types) const
{
  if (types.size () > 2 && types[0] == jit_typeinfo::get_matrix ())
    {
      // indexing a matrix with scalars
      jit_type *scalar = jit_typeinfo::get_scalar ();
      for (size_t i = 1; i < types.size (); ++i)
        if (types[i] != scalar)
          return 0;

      return generate_matrix (types);
    }

  return 0;
}

llvm::Value *
jit_index_operation::create_arg_array (llvm::IRBuilderD& builder,
                                       const jit_function &fn, size_t start_idx,
                                       size_t end_idx) const
{
  size_t n = end_idx - start_idx;
  llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
  llvm::ArrayType *array_t = llvm::ArrayType::get (scalar_t, n);
  llvm::Value *array = llvm::UndefValue::get (array_t);
  for (size_t i = start_idx; i < end_idx; ++i)
    {
      llvm::Value *idx = fn.argument (builder, i);
      array = builder.CreateInsertValue (array, idx, i - start_idx);
    }

  llvm::Value *array_mem = builder.CreateAlloca (array_t);
  builder.CreateStore (array, array_mem);
  return builder.CreateBitCast (array_mem, scalar_t->getPointerTo ());
}

// -------------------- jit_paren_subsref --------------------
jit_function *
jit_paren_subsref::generate_matrix (const signature_vec& types) const
{
  std::stringstream ss;
  ss << "jit_paren_subsref_matrix_scalar" << (types.size () - 1);

  jit_type *scalar = jit_typeinfo::get_scalar ();
  jit_function *fn = new jit_function (module, jit_convention::internal,
                                       ss.str (), scalar, types);
  fn->mark_can_error ();
  llvm::BasicBlock *body = fn->new_block ();
  llvm::IRBuilder<> builder (body);

  llvm::Value *array = create_arg_array (builder, *fn, 1, types.size ());
  jit_type *index = jit_typeinfo::get_index ();
  llvm::Value *nelem = llvm::ConstantInt::get (index->to_llvm (),
                                               types.size () - 1);
  llvm::Value *mat = fn->argument (builder, 0);
  llvm::Value *ret = paren_scalar.call (builder, mat, array, nelem);
  fn->do_return (builder, ret);
  return fn;
}

void
jit_paren_subsref::do_initialize (void)
{
  std::vector<jit_type *> types (3);
  types[0] = jit_typeinfo::get_matrix ();
  types[1] = jit_typeinfo::get_scalar_ptr ();
  types[2] = jit_typeinfo::get_index ();

  jit_type *scalar = jit_typeinfo::get_scalar ();
  paren_scalar = jit_function (module, jit_convention::external,
                               "octave_jit_paren_scalar", scalar, types);
  paren_scalar.add_mapping (engine, &octave_jit_paren_scalar);
  paren_scalar.mark_can_error ();
}

// -------------------- jit_paren_subsasgn --------------------
jit_function *
jit_paren_subsasgn::generate_matrix (const signature_vec& types) const
{
  std::stringstream ss;
  ss << "jit_paren_subsasgn_matrix_scalar" << (types.size () - 2);

  jit_type *matrix = jit_typeinfo::get_matrix ();
  jit_function *fn = new jit_function (module, jit_convention::internal,
                                       ss.str (), matrix, types);
  fn->mark_can_error ();
  llvm::BasicBlock *body = fn->new_block ();
  llvm::IRBuilder<> builder (body);

  llvm::Value *array = create_arg_array (builder, *fn, 1, types.size () - 1);
  jit_type *index = jit_typeinfo::get_index ();
  llvm::Value *nelem = llvm::ConstantInt::get (index->to_llvm (),
                                               types.size () - 2);

  llvm::Value *mat = fn->argument (builder, 0);
  llvm::Value *value = fn->argument (builder, types.size () - 1);
  llvm::Value *ret = paren_scalar.call (builder, mat, array, nelem, value);
  fn->do_return (builder, ret);
  return fn;
}

void
jit_paren_subsasgn::do_initialize (void)
{
  if (paren_scalar.valid ())
    return;

  jit_type *matrix = jit_typeinfo::get_matrix ();
  std::vector<jit_type *> types (4);
  types[0] = matrix;
  types[1] = jit_typeinfo::get_scalar_ptr ();
  types[2] = jit_typeinfo::get_index ();
  types[3] = jit_typeinfo::get_scalar ();

  paren_scalar = jit_function (module, jit_convention::external,
                               "octave_jit_paren_scalar", matrix, types);
  paren_scalar.add_mapping (engine, &octave_jit_paren_scalar_subsasgn);
  paren_scalar.mark_can_error ();
}

// -------------------- jit_typeinfo --------------------
void
jit_typeinfo::initialize (llvm::Module *m, llvm::ExecutionEngine *e)
{
  new jit_typeinfo (m, e);
}

// wrap function names to simplify jit_typeinfo::create_external
#define JIT_FN(fn) engine, &fn, #fn

jit_typeinfo::jit_typeinfo (llvm::Module *m, llvm::ExecutionEngine *e)
  : module (m), engine (e), next_id (0),
    builder (*new llvm::IRBuilderD (context))
{
  instance = this;

  // FIXME: We should be registering types like in octave_value_typeinfo
  llvm::Type *any_t = llvm::StructType::create (context, "octave_base_value");
  any_t = any_t->getPointerTo ();

  llvm::Type *scalar_t = llvm::Type::getDoubleTy (context);
  llvm::Type *bool_t = llvm::Type::getInt1Ty (context);
  llvm::Type *string_t = llvm::Type::getInt8Ty (context);
  string_t = string_t->getPointerTo ();
  llvm::Type *index_t = llvm::Type::getIntNTy (context,
                                               sizeof(octave_idx_type) * 8);

  llvm::StructType *range_t = llvm::StructType::create (context, "range");
  std::vector<llvm::Type *> range_contents (4, scalar_t);
  range_contents[3] = index_t;
  range_t->setBody (range_contents);

  llvm::Type *refcount_t = llvm::Type::getIntNTy (context, sizeof(int) * 8);

  llvm::StructType *matrix_t = llvm::StructType::create (context, "matrix");
  llvm::Type *matrix_contents[5];
  matrix_contents[0] = refcount_t->getPointerTo ();
  matrix_contents[1] = scalar_t->getPointerTo ();
  matrix_contents[2] = index_t;
  matrix_contents[3] = index_t->getPointerTo ();
  matrix_contents[4] = string_t;
  matrix_t->setBody (llvm::makeArrayRef (matrix_contents, 5));

  llvm::Type *complex_t = llvm::ArrayType::get (scalar_t, 2);

  // complex_ret is what is passed to C functions in order to get calling
  // convention right
  llvm::Type *cmplx_inner_cont[] = {scalar_t, scalar_t};
  llvm::StructType *cmplx_inner = llvm::StructType::create (cmplx_inner_cont);

  complex_ret = llvm::StructType::create (context, "complex_ret");
  {
    llvm::Type *contents[] = {cmplx_inner};
    complex_ret->setBody (contents);
  }

  // create types
  any = new_type ("any", 0, any_t);
  matrix = new_type ("matrix", any, matrix_t);
  complex = new_type ("complex", any, complex_t);
  scalar = new_type ("scalar", complex, scalar_t);
  scalar_ptr = new_type ("scalar_ptr", 0, scalar_t->getPointerTo ());
  any_ptr = new_type ("any_ptr", 0, any_t->getPointerTo ());
  range = new_type ("range", any, range_t);
  string = new_type ("string", any, string_t);
  boolean = new_type ("bool", any, bool_t);
  index = new_type ("index", any, index_t);

  create_int (8);
  create_int (16);
  create_int (32);
  create_int (64);

  casts.resize (next_id + 1);
  identities.resize (next_id + 1);

  // specify calling conventions
  // FIXME: We should detect architecture and do something sane based on that
  // here we assume x86 or x86_64
  matrix->mark_sret (jit_convention::external);
  matrix->mark_pointer_arg (jit_convention::external);

  range->mark_sret (jit_convention::external);
  range->mark_pointer_arg (jit_convention::external);

  complex->set_pack (jit_convention::external, &jit_typeinfo::pack_complex);
  complex->set_unpack (jit_convention::external, &jit_typeinfo::unpack_complex);
  complex->set_packed_type (jit_convention::external, complex_ret);

  if (sizeof (void *) == 4)
    complex->mark_sret (jit_convention::external);

  paren_subsref_fn.initialize (module, engine);
  paren_subsasgn_fn.initialize (module, engine);

  // bind global variables
  lerror_state = new llvm::GlobalVariable (*module, bool_t, false,
                                           llvm::GlobalValue::ExternalLinkage,
                                           0, "error_state");
  engine->addGlobalMapping (lerror_state,
                            reinterpret_cast<void *> (&error_state));

  // sig_atomic_type is going to be some sort of integer
  sig_atomic_type = llvm::Type::getIntNTy (context, sizeof(sig_atomic_t) * 8);
  loctave_interrupt_state
    = new llvm::GlobalVariable (*module, sig_atomic_type, false,
                                llvm::GlobalValue::ExternalLinkage, 0,
                                "octave_interrupt_state");
  engine->addGlobalMapping (loctave_interrupt_state,
                            reinterpret_cast<void *> (&octave_interrupt_state));

  // generic call function
  {
    jit_type *int_t = intN (sizeof (octave_builtin::fcn) * 8);
    any_call = create_external (JIT_FN (octave_jit_call), any, int_t, int_t,
                                any_ptr, int_t);
  }

  // any with anything is an any op
  jit_function fn;
  jit_type *binary_op_type = intN (sizeof (octave_value::binary_op) * 8);
  llvm::Type *llvm_bo_type = binary_op_type->to_llvm ();
  jit_function any_binary = create_external (JIT_FN (octave_jit_binary_any_any),
                                             any, binary_op_type, any, any);
  any_binary.mark_can_error ();
  binary_ops.resize (octave_value::num_binary_ops);
  for (size_t i = 0; i < octave_value::num_binary_ops; ++i)
    {
      octave_value::binary_op op = static_cast<octave_value::binary_op> (i);
      std::string op_name = octave_value::binary_op_as_string (op);
      binary_ops[i].stash_name ("binary" + op_name);
    }

  unary_ops.resize (octave_value::num_unary_ops);
  for (size_t i = 0; i < octave_value::num_unary_ops; ++i)
    {
      octave_value::unary_op op = static_cast<octave_value::unary_op> (i);
      std::string op_name = octave_value::unary_op_as_string (op);
      unary_ops[i].stash_name ("unary" + op_name);
    }

  for (int op = 0; op < octave_value::num_binary_ops; ++op)
    {
      llvm::Twine fn_name ("octave_jit_binary_any_any_");
      fn_name = fn_name + llvm::Twine (op);

      fn = create_internal (fn_name, any, any, any);
      fn.mark_can_error ();
      llvm::BasicBlock *block = fn.new_block ();
      builder.SetInsertPoint (block);
      llvm::APInt op_int(sizeof (octave_value::binary_op) * 8, op,
                         std::numeric_limits<octave_value::binary_op>::is_signed);
      llvm::Value *op_as_llvm = llvm::ConstantInt::get (llvm_bo_type, op_int);
      llvm::Value *ret = any_binary.call (builder, op_as_llvm,
                                          fn.argument (builder, 0),
                                          fn.argument (builder, 1));
      fn.do_return (builder, ret);
      binary_ops[op].add_overload (fn);
    }

  // grab matrix
  fn = create_external (JIT_FN (octave_jit_grab_matrix), matrix, matrix);
  grab_fn.add_overload (fn);

  grab_fn.add_overload (create_identity (scalar));
  grab_fn.add_overload (create_identity (scalar_ptr));
  grab_fn.add_overload (create_identity (any_ptr));
  grab_fn.add_overload (create_identity (boolean));
  grab_fn.add_overload (create_identity (complex));
  grab_fn.add_overload (create_identity (index));

  // release any
  fn = create_external (JIT_FN (octave_jit_release_any), 0, any);
  release_fn.add_overload (fn);
  release_fn.stash_name ("release");

  // release matrix
  fn = create_external (JIT_FN (octave_jit_release_matrix), 0, matrix);
  release_fn.add_overload (fn);

  // destroy
  destroy_fn = release_fn;
  destroy_fn.stash_name ("destroy");
  destroy_fn.add_overload (create_identity(scalar));
  destroy_fn.add_overload (create_identity(boolean));
  destroy_fn.add_overload (create_identity(index));
  destroy_fn.add_overload (create_identity(complex));

  // -------------------- scalar related operations --------------------

  // now for binary scalar operations
  add_binary_op (scalar, octave_value::op_add, llvm::Instruction::FAdd);
  add_binary_op (scalar, octave_value::op_sub, llvm::Instruction::FSub);
  add_binary_op (scalar, octave_value::op_mul, llvm::Instruction::FMul);
  add_binary_op (scalar, octave_value::op_el_mul, llvm::Instruction::FMul);

  add_binary_fcmp (scalar, octave_value::op_lt, llvm::CmpInst::FCMP_ULT);
  add_binary_fcmp (scalar, octave_value::op_le, llvm::CmpInst::FCMP_ULE);
  add_binary_fcmp (scalar, octave_value::op_eq, llvm::CmpInst::FCMP_UEQ);
  add_binary_fcmp (scalar, octave_value::op_ge, llvm::CmpInst::FCMP_UGE);
  add_binary_fcmp (scalar, octave_value::op_gt, llvm::CmpInst::FCMP_UGT);
  add_binary_fcmp (scalar, octave_value::op_ne, llvm::CmpInst::FCMP_UNE);

  jit_function gripe_div0 = create_external (JIT_FN (gripe_divide_by_zero), 0);
  gripe_div0.mark_can_error ();

  // divide is annoying because it might error
  fn = create_internal ("octave_jit_div_scalar_scalar", scalar, scalar, scalar);
  fn.mark_can_error ();

  llvm::BasicBlock *body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::BasicBlock *warn_block = fn.new_block ("warn");
    llvm::BasicBlock *normal_block = fn.new_block ("normal");

    llvm::Value *zero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *check = builder.CreateFCmpUEQ (zero, fn.argument (builder, 1));
    builder.CreateCondBr (check, warn_block, normal_block);

    builder.SetInsertPoint (warn_block);
    gripe_div0.call (builder);
    builder.CreateBr (normal_block);

    builder.SetInsertPoint (normal_block);
    llvm::Value *ret = builder.CreateFDiv (fn.argument (builder, 0),
                                           fn.argument (builder, 1));
    fn.do_return (builder, ret);
  }
  binary_ops[octave_value::op_div].add_overload (fn);
  binary_ops[octave_value::op_el_div].add_overload (fn);

  // ldiv is the same as div with the operators reversed
  fn = mirror_binary (fn);
  binary_ops[octave_value::op_ldiv].add_overload (fn);
  binary_ops[octave_value::op_el_ldiv].add_overload (fn);

  // In general, the result of scalar ^ scalar is a complex number. We might be
  // able to improve on this if we keep track of the range of values varaibles
  // can take on.
  fn = create_external (JIT_FN (octave_jit_pow_scalar_scalar), complex, scalar,
                        scalar);
  binary_ops[octave_value::op_pow].add_overload (fn);
  binary_ops[octave_value::op_el_pow].add_overload (fn);

  // now for unary scalar operations
  // FIXME: Impelment not
  fn = create_internal ("octave_jit_++", scalar, scalar);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *one = llvm::ConstantFP::get (scalar_t, 1);
    llvm::Value *val = fn.argument (builder, 0);
    val = builder.CreateFAdd (val, one);
    fn.do_return (builder, val);
  }
  unary_ops[octave_value::op_incr].add_overload (fn);

  fn = create_internal ("octave_jit_--", scalar, scalar);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *one = llvm::ConstantFP::get (scalar_t, 1);
    llvm::Value *val = fn.argument (builder, 0);
    val = builder.CreateFSub (val, one);
    fn.do_return (builder, val);
  }
  unary_ops[octave_value::op_decr].add_overload (fn);

  fn = create_internal ("octave_jit_uminus", scalar, scalar);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *mone = llvm::ConstantFP::get (scalar_t, -1);
    llvm::Value *val = fn.argument (builder, 0);
    val = builder.CreateFMul (val, mone);
    fn.do_return (builder, val);
  }
  unary_ops[octave_value::op_uminus].add_overload (fn);

  fn = create_identity (scalar);
  unary_ops[octave_value::op_uplus].add_overload (fn);
  unary_ops[octave_value::op_transpose].add_overload (fn);
  unary_ops[octave_value::op_hermitian].add_overload (fn);

  // now for binary complex operations
  fn = create_internal ("octave_jit_+_complex_complex", complex, complex,
                        complex);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *lhs = fn.argument (builder, 0);
    llvm::Value *rhs = fn.argument (builder, 1);
    llvm::Value *real = builder.CreateFAdd (complex_real (lhs),
                                            complex_real (rhs));
    llvm::Value *imag = builder.CreateFAdd (complex_imag (lhs),
                                            complex_imag (rhs));
    fn.do_return (builder, complex_new (real, imag));
  }
  binary_ops[octave_value::op_add].add_overload (fn);

  fn = create_internal ("octave_jit_-_complex_complex", complex, complex,
                        complex);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *lhs = fn.argument (builder, 0);
    llvm::Value *rhs = fn.argument (builder, 1);
    llvm::Value *real = builder.CreateFSub (complex_real (lhs),
                                            complex_real (rhs));
    llvm::Value *imag = builder.CreateFSub (complex_imag (lhs),
                                            complex_imag (rhs));
    fn.do_return (builder, complex_new (real, imag));
  }
  binary_ops[octave_value::op_sub].add_overload (fn);

  fn = create_external (JIT_FN (octave_jit_complex_mul),
                        complex, complex, complex);
  binary_ops[octave_value::op_mul].add_overload (fn);
  binary_ops[octave_value::op_el_mul].add_overload (fn);

  jit_function complex_div = create_external (JIT_FN (octave_jit_complex_div),
                                              complex, complex, complex);
  complex_div.mark_can_error ();
  binary_ops[octave_value::op_div].add_overload (fn);
  binary_ops[octave_value::op_ldiv].add_overload (fn);

  fn = create_external (JIT_FN (octave_jit_pow_complex_complex), complex,
                        complex, complex);
  binary_ops[octave_value::op_pow].add_overload (fn);
  binary_ops[octave_value::op_el_pow].add_overload (fn);

  fn = create_internal ("octave_jit_*_scalar_complex", complex, scalar,
                        complex);
  jit_function mul_scalar_complex = fn;
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::BasicBlock *complex_mul = fn.new_block ("complex_mul");
    llvm::BasicBlock *scalar_mul = fn.new_block ("scalar_mul");

    llvm::Value *fzero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *lhs = fn.argument (builder, 0);
    llvm::Value *rhs = fn.argument (builder, 1);

    llvm::Value *cmp = builder.CreateFCmpUEQ (complex_imag (rhs), fzero);
    builder.CreateCondBr (cmp, scalar_mul, complex_mul);

    builder.SetInsertPoint (scalar_mul);
    llvm::Value *temp = complex_real (rhs);
    temp = builder.CreateFMul (lhs, temp);
    fn.do_return (builder, complex_new (temp, fzero), false);


    builder.SetInsertPoint (complex_mul);
    temp = complex_new (builder.CreateFMul (lhs, complex_real (rhs)),
                        builder.CreateFMul (lhs, complex_imag (rhs)));
    fn.do_return (builder, temp);
  }
  binary_ops[octave_value::op_mul].add_overload (fn);
  binary_ops[octave_value::op_el_mul].add_overload (fn);


  fn = mirror_binary (mul_scalar_complex);
  binary_ops[octave_value::op_mul].add_overload (fn);
  binary_ops[octave_value::op_el_mul].add_overload (fn);

  fn = create_internal ("octave_jit_+_scalar_complex", complex, scalar,
                        complex);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *lhs = fn.argument (builder, 0);
    llvm::Value *rhs = fn.argument (builder, 1);
    llvm::Value *real = builder.CreateFAdd (lhs, complex_real (rhs));
    fn.do_return (builder, complex_real (rhs, real));
  }
  binary_ops[octave_value::op_add].add_overload (fn);

  fn = mirror_binary (fn);
  binary_ops[octave_value::op_add].add_overload (fn);

  fn = create_internal ("octave_jit_-_complex_scalar", complex, complex,
                        scalar);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *lhs = fn.argument (builder, 0);
    llvm::Value *rhs = fn.argument (builder, 1);
    llvm::Value *real = builder.CreateFSub (complex_real (lhs), rhs);
    fn.do_return (builder, complex_real (lhs, real));
  }
  binary_ops[octave_value::op_sub].add_overload (fn);

  fn = create_internal ("octave_jit_-_scalar_complex", complex, scalar,
                        complex);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *lhs = fn.argument (builder, 0);
    llvm::Value *rhs = fn.argument (builder, 1);
    llvm::Value *real = builder.CreateFSub (lhs, complex_real (rhs));
    fn.do_return (builder, complex_real (rhs, real));
  }
  binary_ops[octave_value::op_sub].add_overload (fn);

  fn = create_external (JIT_FN (octave_jit_pow_scalar_complex), complex, scalar,
                        complex);
  binary_ops[octave_value::op_pow].add_overload (fn);
  binary_ops[octave_value::op_el_pow].add_overload (fn);

  fn = create_external (JIT_FN (octave_jit_pow_complex_scalar), complex,
                        complex, scalar);
  binary_ops[octave_value::op_pow].add_overload (fn);
  binary_ops[octave_value::op_el_pow].add_overload (fn);

  // now for binary index operators
  add_binary_op (index, octave_value::op_add, llvm::Instruction::Add);

  // and binary bool operators
  add_binary_op (boolean, octave_value::op_el_or, llvm::Instruction::Or);
  add_binary_op (boolean, octave_value::op_el_and, llvm::Instruction::And);

  // now for printing functions
  print_fn.stash_name ("print");
  add_print (any, reinterpret_cast<void *> (&octave_jit_print_any));
  add_print (scalar, reinterpret_cast<void *> (&octave_jit_print_scalar));

  // initialize for loop
  for_init_fn.stash_name ("for_init");

  fn = create_internal ("octave_jit_for_range_init", index, range);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *zero = llvm::ConstantInt::get (index_t, 0);
    fn.do_return (builder, zero);
  }
  for_init_fn.add_overload (fn);

  // bounds check for for loop
  for_check_fn.stash_name ("for_check");

  fn = create_internal ("octave_jit_for_range_check", boolean, range, index);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *nelem
      = builder.CreateExtractValue (fn.argument (builder, 0), 3);
    llvm::Value *idx = fn.argument (builder, 1);
    llvm::Value *ret = builder.CreateICmpULT (idx, nelem);
    fn.do_return (builder, ret);
  }
  for_check_fn.add_overload (fn);

  // index variabe for for loop
  for_index_fn.stash_name ("for_index");

  fn = create_internal ("octave_jit_for_range_idx", scalar, range, index);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *idx = fn.argument (builder, 1);
    llvm::Value *didx = builder.CreateSIToFP (idx, scalar_t);
    llvm::Value *rng = fn.argument (builder, 0);
    llvm::Value *base = builder.CreateExtractValue (rng, 0);
    llvm::Value *inc = builder.CreateExtractValue (rng, 2);

    llvm::Value *ret = builder.CreateFMul (didx, inc);
    ret = builder.CreateFAdd (base, ret);
    fn.do_return (builder, ret);
  }
  for_index_fn.add_overload (fn);

  // logically true
  logically_true_fn.stash_name ("logically_true");

  jit_function gripe_nantl
    = create_external (JIT_FN (octave_jit_gripe_nan_to_logical_conversion), 0);
  gripe_nantl.mark_can_error ();

  fn = create_internal ("octave_jit_logically_true_scalar", boolean, scalar);
  fn.mark_can_error ();

  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::BasicBlock *error_block = fn.new_block ("error");
    llvm::BasicBlock *normal_block = fn.new_block ("normal");

    llvm::Value *check = builder.CreateFCmpUNE (fn.argument (builder, 0),
                                                fn.argument (builder, 0));
    builder.CreateCondBr (check, error_block, normal_block);

    builder.SetInsertPoint (error_block);
    gripe_nantl.call (builder);
    builder.CreateBr (normal_block);
    builder.SetInsertPoint (normal_block);

    llvm::Value *zero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *ret = builder.CreateFCmpONE (fn.argument (builder, 0), zero);
    fn.do_return (builder, ret);
  }
  logically_true_fn.add_overload (fn);

  // logically_true boolean
  fn = create_identity (boolean);
  logically_true_fn.add_overload (fn);

  // make_range
  // FIXME: May be benificial to implement all in LLVM
  make_range_fn.stash_name ("make_range");
  jit_function compute_nelem
    = create_external (JIT_FN (octave_jit_compute_nelem),
                       index, scalar, scalar, scalar);


  fn = create_internal ("octave_jit_make_range", range, scalar, scalar, scalar);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *base = fn.argument (builder, 0);
    llvm::Value *limit = fn.argument (builder, 1);
    llvm::Value *inc = fn.argument (builder, 2);
    llvm::Value *nelem = compute_nelem.call (builder, base, limit, inc);

    llvm::Value *dzero = llvm::ConstantFP::get (scalar_t, 0);
    llvm::Value *izero = llvm::ConstantInt::get (index_t, 0);
    llvm::Value *rng = llvm::ConstantStruct::get (range_t, dzero, dzero, dzero,
                                                  izero, NULL);
    rng = builder.CreateInsertValue (rng, base, 0);
    rng = builder.CreateInsertValue (rng, limit, 1);
    rng = builder.CreateInsertValue (rng, inc, 2);
    rng = builder.CreateInsertValue (rng, nelem, 3);
    fn.do_return (builder, rng);
  }
  make_range_fn.add_overload (fn);

  // paren_subsref
  jit_type *jit_int = intN (sizeof (int) * 8);
  llvm::Type *int_t = jit_int->to_llvm ();
  jit_function ginvalid_index
    = create_external (JIT_FN (octave_jit_ginvalid_index), 0);
  jit_function gindex_range = create_external (JIT_FN (octave_jit_gindex_range),
                                               0, jit_int, jit_int, index,
                                               index);

  fn = create_internal ("()subsref", scalar, matrix, scalar);
  fn.mark_can_error ();

  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *one_idx = llvm::ConstantInt::get (index_t, 1);
    llvm::Value *one_int = llvm::ConstantInt::get (int_t, 1);

    llvm::Value *undef = llvm::UndefValue::get (scalar_t);
    llvm::Value *mat = fn.argument (builder, 0);
    llvm::Value *idx = fn.argument (builder, 1);

    // convert index to scalar to integer, and check index >= 1
    llvm::Value *int_idx = builder.CreateFPToSI (idx, index_t);
    llvm::Value *check_idx = builder.CreateSIToFP (int_idx, scalar_t);
    llvm::Value *cond0 = builder.CreateFCmpUNE (idx, check_idx);
    llvm::Value *cond1 = builder.CreateICmpSLT (int_idx, one_idx);
    llvm::Value *cond = builder.CreateOr (cond0, cond1);

    llvm::BasicBlock *done = fn.new_block ("done");
    llvm::BasicBlock *conv_error = fn.new_block ("conv_error", done);
    llvm::BasicBlock *normal = fn.new_block ("normal", done);
    builder.CreateCondBr (cond, conv_error, normal);

    builder.SetInsertPoint (conv_error);
    ginvalid_index.call (builder);
    builder.CreateBr (done);

    builder.SetInsertPoint (normal);
    llvm::Value *len
      = builder.CreateExtractValue (mat, llvm::ArrayRef<unsigned> (2));
    cond = builder.CreateICmpSGT (int_idx, len);


    llvm::BasicBlock *bounds_error = fn.new_block ("bounds_error", done);
    llvm::BasicBlock *success = fn.new_block ("success", done);
    builder.CreateCondBr (cond, bounds_error, success);

    builder.SetInsertPoint (bounds_error);
    gindex_range.call (builder, one_int, one_int, int_idx, len);
    builder.CreateBr (done);

    builder.SetInsertPoint (success);
    llvm::Value *data = builder.CreateExtractValue (mat,
                                                    llvm::ArrayRef<unsigned> (1));
    llvm::Value *gep = builder.CreateInBoundsGEP (data, int_idx);
    llvm::Value *ret = builder.CreateLoad (gep);
    builder.CreateBr (done);

    builder.SetInsertPoint (done);

    llvm::PHINode *merge = llvm::PHINode::Create (scalar_t, 3);
    builder.Insert (merge);
    merge->addIncoming (undef, conv_error);
    merge->addIncoming (undef, bounds_error);
    merge->addIncoming (ret, success);
    fn.do_return (builder, merge);
  }
  paren_subsref_fn.add_overload (fn);

  // paren subsasgn
  paren_subsasgn_fn.stash_name ("()subsasgn");

  jit_function resize_paren_subsasgn
    = create_external (JIT_FN (octave_jit_paren_subsasgn_impl), matrix, matrix,
                       index, scalar);

  fn = create_internal ("octave_jit_paren_subsasgn", matrix, matrix, scalar,
                        scalar);
  fn.mark_can_error ();
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *one_idx = llvm::ConstantInt::get (index_t, 1);
    llvm::Value *one_int = llvm::ConstantInt::get (int_t, 1);

    llvm::Value *mat = fn.argument (builder, 0);
    llvm::Value *idx = fn.argument (builder, 1);
    llvm::Value *value = fn.argument (builder, 2);

    llvm::Value *int_idx = builder.CreateFPToSI (idx, index_t);
    llvm::Value *check_idx = builder.CreateSIToFP (int_idx, scalar_t);
    llvm::Value *cond0 = builder.CreateFCmpUNE (idx, check_idx);
    llvm::Value *cond1 = builder.CreateICmpSLT (int_idx, one_idx);
    llvm::Value *cond = builder.CreateOr (cond0, cond1);

    llvm::BasicBlock *done = fn.new_block ("done");

    llvm::BasicBlock *conv_error = fn.new_block ("conv_error", done);
    llvm::BasicBlock *normal = fn.new_block ("normal", done);
    builder.CreateCondBr (cond, conv_error, normal);
    builder.SetInsertPoint (conv_error);
    ginvalid_index.call (builder);
    builder.CreateBr (done);

    builder.SetInsertPoint (normal);
    llvm::Value *len = builder.CreateExtractValue (mat, 2);
    cond0 = builder.CreateICmpSGT (int_idx, len);

    llvm::Value *rcount = builder.CreateExtractValue (mat, 0);
    rcount = builder.CreateLoad (rcount);
    cond1 = builder.CreateICmpSGT (rcount, one_int);
    cond = builder.CreateOr (cond0, cond1);

    llvm::BasicBlock *bounds_error = fn.new_block ("bounds_error", done);
    llvm::BasicBlock *success = fn.new_block ("success", done);
    builder.CreateCondBr (cond, bounds_error, success);

    // resize on out of bounds access
    builder.SetInsertPoint (bounds_error);
    llvm::Value *resize_result = resize_paren_subsasgn.call (builder, mat,
                                                             int_idx, value);
    builder.CreateBr (done);

    builder.SetInsertPoint (success);
    llvm::Value *data
      = builder.CreateExtractValue (mat, llvm::ArrayRef<unsigned> (1));
    llvm::Value *gep = builder.CreateInBoundsGEP (data, int_idx);
    builder.CreateStore (value, gep);
    builder.CreateBr (done);

    builder.SetInsertPoint (done);

    llvm::PHINode *merge = llvm::PHINode::Create (matrix_t, 3);
    builder.Insert (merge);
    merge->addIncoming (mat, conv_error);
    merge->addIncoming (resize_result, bounds_error);
    merge->addIncoming (mat, success);
    fn.do_return (builder, merge);
  }
  paren_subsasgn_fn.add_overload (fn);

  fn = create_external (JIT_FN (octave_jit_paren_subsasgn_matrix_range), matrix,
                        matrix, range, scalar);
  fn.mark_can_error ();
  paren_subsasgn_fn.add_overload (fn);

  end1_fn.stash_name ("end1");
  fn = create_internal ("octave_jit_end1_matrix", scalar, matrix, index, index);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *mat = fn.argument (builder, 0);
    llvm::Value *ret = builder.CreateExtractValue (mat, 2);
    fn.do_return (builder, builder.CreateSIToFP (ret, scalar_t));
  }
  end1_fn.add_overload (fn);

  end_fn.stash_name ("end");
  fn = create_external (JIT_FN (octave_jit_end_matrix),scalar, matrix, index,
                        index);
  end_fn.add_overload (fn);

  // -------------------- create_undef --------------------
  create_undef_fn.stash_name ("create_undef");
  fn = create_external (JIT_FN (octave_jit_create_undef), any);
  create_undef_fn.add_overload (fn);

  casts[any->type_id ()].stash_name ("(any)");
  casts[scalar->type_id ()].stash_name ("(scalar)");
  casts[complex->type_id ()].stash_name ("(complex)");
  casts[matrix->type_id ()].stash_name ("(matrix)");
  casts[range->type_id ()].stash_name ("(range)");

  // cast any <- matrix
  fn = create_external (JIT_FN (octave_jit_cast_any_matrix), any, matrix);
  casts[any->type_id ()].add_overload (fn);

  // cast matrix <- any
  fn = create_external (JIT_FN (octave_jit_cast_matrix_any), matrix, any);
  casts[matrix->type_id ()].add_overload (fn);

  // cast any <- range
  fn = create_external (JIT_FN (octave_jit_cast_any_range), any, range);
  casts[any->type_id ()].add_overload (fn);

  // cast range <- any
  fn = create_external (JIT_FN (octave_jit_cast_range_any), range, any);
  casts[range->type_id ()].add_overload (fn);

  // cast any <- scalar
  fn = create_external (JIT_FN (octave_jit_cast_any_scalar), any, scalar);
  casts[any->type_id ()].add_overload (fn);

  // cast scalar <- any
  fn = create_external (JIT_FN (octave_jit_cast_scalar_any), scalar, any);
  casts[scalar->type_id ()].add_overload (fn);

  // cast any <- complex
  fn = create_external (JIT_FN (octave_jit_cast_any_complex), any, complex);
  casts[any->type_id ()].add_overload (fn);

  // cast complex <- any
  fn = create_external (JIT_FN (octave_jit_cast_complex_any), complex, any);
  casts[complex->type_id ()].add_overload (fn);

  // cast complex <- scalar
  fn = create_internal ("octave_jit_cast_complex_scalar", complex, scalar);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  {
    llvm::Value *zero = llvm::ConstantFP::get (scalar_t, 0);
    fn.do_return (builder, complex_new (fn.argument (builder, 0), zero));
  }
  casts[complex->type_id ()].add_overload (fn);

  // cast scalar <- complex
  fn = create_internal ("octave_jit_cast_scalar_complex", scalar, complex);
  body = fn.new_block ();
  builder.SetInsertPoint (body);
  fn.do_return (builder, complex_real (fn.argument (builder, 0)));
  casts[scalar->type_id ()].add_overload (fn);

  // cast any <- any
  fn = create_identity (any);
  casts[any->type_id ()].add_overload (fn);

  // cast scalar <- scalar
  fn = create_identity (scalar);
  casts[scalar->type_id ()].add_overload (fn);

  // cast complex <- complex
  fn = create_identity (complex);
  casts[complex->type_id ()].add_overload (fn);

  // -------------------- builtin functions --------------------
  add_builtin ("#unknown_function");
  unknown_function = builtins["#unknown_function"];

  add_builtin ("sin");
  register_intrinsic ("sin", llvm::Intrinsic::sin, scalar, scalar);
  register_generic ("sin", matrix, matrix);

  add_builtin ("cos");
  register_intrinsic ("cos", llvm::Intrinsic::cos, scalar, scalar);
  register_generic ("cos", matrix, matrix);

  add_builtin ("exp");
  register_intrinsic ("exp", llvm::Intrinsic::exp, scalar, scalar);
  register_generic ("exp", matrix, matrix);

  add_builtin ("balance");
  register_generic ("balance", matrix, matrix);

  add_builtin ("cond");
  register_generic ("cond", scalar, matrix);

  add_builtin ("det");
  register_generic ("det", scalar, matrix);

  add_builtin ("norm");
  register_generic ("norm", scalar, matrix);

  add_builtin ("rand");
  register_generic ("rand", matrix, scalar);
  register_generic ("rand", matrix, std::vector<jit_type *> (2, scalar));

  add_builtin ("magic");
  register_generic ("magic", matrix, scalar);
  register_generic ("magic", matrix, std::vector<jit_type *> (2, scalar));

  add_builtin ("eye");
  register_generic ("eye", matrix, scalar);
  register_generic ("eye", matrix, std::vector<jit_type *> (2, scalar));

  add_builtin ("mod");
  register_generic ("mod", scalar, std::vector<jit_type *> (2, scalar));

  casts.resize (next_id + 1);
  jit_function any_id = create_identity (any);
  jit_function grab_any = create_external (JIT_FN (octave_jit_grab_any),
                                           any, any);
  jit_function release_any = get_release (any);
  std::vector<jit_type *> args;
  args.resize (1);

  for (std::map<std::string, jit_type *>::iterator iter = builtins.begin ();
       iter != builtins.end (); ++iter)
    {
      jit_type *btype = iter->second;
      args[0] = btype;

      grab_fn.add_overload (jit_function (grab_any, btype, args));
      release_fn.add_overload (jit_function (release_any, 0, args));
      casts[any->type_id ()].add_overload (jit_function (any_id, any, args));

      args[0] = any;
      casts[btype->type_id ()].add_overload (jit_function (any_id, btype,
                                                           args));
    }
}

const jit_function&
jit_typeinfo::do_end (jit_value *value, jit_value *idx, jit_value *count)
{
  jit_const_index *ccount = dynamic_cast<jit_const_index *> (count);
  if (ccount && ccount->value () == 1)
    return end1_fn.overload (value->type (), idx->type (), count->type ());

  return end_fn.overload (value->type (), idx->type (), count->type ());
}

jit_type*
jit_typeinfo::new_type (const std::string& name, jit_type *parent,
                        llvm::Type *llvm_type, bool skip_paren)
{
  jit_type *ret = new jit_type (name, parent, llvm_type, skip_paren, next_id++);
  id_to_type.push_back (ret);
  return ret;
}

void
jit_typeinfo::add_print (jit_type *ty, void *fptr)
{
  std::stringstream name;
  name << "octave_jit_print_" << ty->name ();
  jit_function fn = create_external (engine, fptr, name.str (),
                                     0, intN (8), ty);
  print_fn.add_overload (fn);
}

// FIXME: cp between add_binary_op, add_binary_icmp, and add_binary_fcmp
void
jit_typeinfo::add_binary_op (jit_type *ty, int op, int llvm_op)
{
  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit_" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();

  jit_function fn = create_internal (fname.str (), ty, ty, ty);
  llvm::BasicBlock *block = fn.new_block ();
  builder.SetInsertPoint (block);
  llvm::Instruction::BinaryOps temp
    = static_cast<llvm::Instruction::BinaryOps>(llvm_op);

  llvm::Value *ret = builder.CreateBinOp (temp, fn.argument (builder, 0),
                                          fn.argument (builder, 1));
  fn.do_return (builder, ret);
  binary_ops[op].add_overload (fn);
}

void
jit_typeinfo::add_binary_icmp (jit_type *ty, int op, int llvm_op)
{
  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();

  jit_function fn = create_internal (fname.str (), boolean, ty, ty);
  llvm::BasicBlock *block = fn.new_block ();
  builder.SetInsertPoint (block);
  llvm::CmpInst::Predicate temp
    = static_cast<llvm::CmpInst::Predicate>(llvm_op);
  llvm::Value *ret = builder.CreateICmp (temp, fn.argument (builder, 0),
                                         fn.argument (builder, 1));
  fn.do_return (builder, ret);
  binary_ops[op].add_overload (fn);
}

void
jit_typeinfo::add_binary_fcmp (jit_type *ty, int op, int llvm_op)
{
  std::stringstream fname;
  octave_value::binary_op ov_op = static_cast<octave_value::binary_op>(op);
  fname << "octave_jit" << octave_value::binary_op_as_string (ov_op)
        << "_" << ty->name ();

  jit_function fn = create_internal (fname.str (), boolean, ty, ty);
  llvm::BasicBlock *block = fn.new_block ();
  builder.SetInsertPoint (block);
  llvm::CmpInst::Predicate temp
    = static_cast<llvm::CmpInst::Predicate>(llvm_op);
  llvm::Value *ret = builder.CreateFCmp (temp, fn.argument (builder, 0),
                                         fn.argument (builder, 1));
  fn.do_return (builder, ret);
  binary_ops[op].add_overload (fn);
}

jit_function
jit_typeinfo::create_function (jit_convention::type cc, const llvm::Twine& name,
                               jit_type *ret,
                               const std::vector<jit_type *>& args)
{
  jit_function result (module, cc, name, ret, args);
  return result;
}

jit_function
jit_typeinfo::create_identity (jit_type *type)
{
  size_t id = type->type_id ();
  if (id >= identities.size ())
    identities.resize (id + 1);

  if (! identities[id].valid ())
    {
      std::stringstream name;
      name << "id_" << type->name ();

      jit_function fn = create_internal (name.str (), type, type);
      llvm::BasicBlock *body = fn.new_block ();
      builder.SetInsertPoint (body);
      fn.do_return (builder, fn.argument (builder, 0));
      return identities[id] = fn;
    }

  return identities[id];
}

llvm::Value *
jit_typeinfo::do_insert_error_check (llvm::IRBuilderD& abuilder)
{
  return abuilder.CreateLoad (lerror_state);
}

llvm::Value *
jit_typeinfo::do_insert_interrupt_check (llvm::IRBuilderD& abuilder)
{
  llvm::LoadInst *val = abuilder.CreateLoad (loctave_interrupt_state);
  val->setVolatile (true);
  return abuilder.CreateICmpSGT (val, abuilder.getInt32 (0));
}

void
jit_typeinfo::add_builtin (const std::string& name)
{
  jit_type *btype = new_type (name, any, any->to_llvm (), true);
  builtins[name] = btype;

  octave_builtin *ov_builtin = find_builtin (name);
  if (ov_builtin)
    ov_builtin->stash_jit (*btype);
}

void
jit_typeinfo::register_intrinsic (const std::string& name, size_t iid,
                                  jit_type *result,
                                  const std::vector<jit_type *>& args)
{
  jit_type *builtin_type = builtins[name];
  size_t nargs = args.size ();
  llvm::SmallVector<llvm::Type *, 5> llvm_args (nargs);
  for (size_t i = 0; i < nargs; ++i)
    llvm_args[i] = args[i]->to_llvm ();

  llvm::Intrinsic::ID id = static_cast<llvm::Intrinsic::ID> (iid);
  llvm::Function *ifun = llvm::Intrinsic::getDeclaration (module, id,
                                                          llvm_args);
  std::stringstream fn_name;
  fn_name << "octave_jit_" << name;

  std::vector<jit_type *> args1 (nargs + 1);
  args1[0] = builtin_type;
  std::copy (args.begin (), args.end (), args1.begin () + 1);

  // The first argument will be the Octave function, but we already know that
  // the function call is the equivalent of the intrinsic, so we ignore it and
  // call the intrinsic with the remaining arguments.
  jit_function fn = create_internal (fn_name.str (), result, args1);
  llvm::BasicBlock *body = fn.new_block ();
  builder.SetInsertPoint (body);

  llvm::SmallVector<llvm::Value *, 5> fargs (nargs);
  for (size_t i = 0; i < nargs; ++i)
    fargs[i] = fn.argument (builder, i + 1);

  llvm::Value *ret = builder.CreateCall (ifun, fargs);
  fn.do_return (builder, ret);
  paren_subsref_fn.add_overload (fn);
}

octave_builtin *
jit_typeinfo::find_builtin (const std::string& name)
{
  // FIXME: Finalize what we want to store in octave_builtin, then add functions
  // to access these values in octave_value
  octave_value ov_builtin = symbol_table::find (name);
  return dynamic_cast<octave_builtin *> (ov_builtin.internal_rep ());
}

void
jit_typeinfo::register_generic (const std::string& name, jit_type *result,
                                const std::vector<jit_type *>& args)
{
  octave_builtin *builtin = find_builtin (name);
  if (! builtin)
    return;

  std::vector<jit_type *> fn_args (args.size () + 1);
  fn_args[0] = builtins[name];
  std::copy (args.begin (), args.end (), fn_args.begin () + 1);
  jit_function fn = create_internal (name, result, fn_args);
  fn.mark_can_error ();
  llvm::BasicBlock *block = fn.new_block ();
  builder.SetInsertPoint (block);
  llvm::Type *any_t = any->to_llvm ();
  llvm::ArrayType *array_t = llvm::ArrayType::get (any_t, args.size ());
  llvm::Value *array = llvm::UndefValue::get (array_t);
  for (size_t i = 0; i < args.size (); ++i)
    {
      llvm::Value *arg = fn.argument (builder, i + 1);
      jit_function agrab = get_grab (args[i]);
      if (agrab.valid ())
        arg = agrab.call (builder, arg);
      jit_function acast = cast (any, args[i]);
      array = builder.CreateInsertValue (array, acast.call (builder, arg), i);
    }

  llvm::Value *array_mem = builder.CreateAlloca (array_t);
  builder.CreateStore (array, array_mem);
  array = builder.CreateBitCast (array_mem, any_t->getPointerTo ());

  jit_type *jintTy = intN (sizeof (octave_builtin::fcn) * 8);
  llvm::Type *intTy = jintTy->to_llvm ();
  size_t fcn_int = reinterpret_cast<size_t> (builtin->function ());
  llvm::Value *fcn = llvm::ConstantInt::get (intTy, fcn_int);
  llvm::Value *nargin = llvm::ConstantInt::get (intTy, args.size ());
  size_t result_int = reinterpret_cast<size_t> (result);
  llvm::Value *res_llvm = llvm::ConstantInt::get (intTy, result_int);
  llvm::Value *ret = any_call.call (builder, fcn, nargin, array, res_llvm);

  jit_function cast_result = cast (result, any);
  fn.do_return (builder, cast_result.call (builder, ret));
  paren_subsref_fn.add_overload (fn);
}

jit_function
jit_typeinfo::mirror_binary (const jit_function& fn)
{
  jit_function ret = create_internal (fn.name () + "_reverse",
                                      fn.result (), fn.argument_type (1),
                                      fn.argument_type (0));
  if (fn.can_error ())
    ret.mark_can_error ();

  llvm::BasicBlock *body = ret.new_block ();
  builder.SetInsertPoint (body);
  llvm::Value *result = fn.call (builder, ret.argument (builder, 1),
                                 ret.argument (builder, 0));
  if (ret.result ())
    ret.do_return (builder, result);
  else
    ret.do_return (builder);

  return ret;
}

llvm::Value *
jit_typeinfo::pack_complex (llvm::IRBuilderD& bld, llvm::Value *cplx)
{
  llvm::Type *complex_ret = instance->complex_ret;
  llvm::Value *real = bld.CreateExtractValue (cplx, 0);
  llvm::Value *imag = bld.CreateExtractValue (cplx, 1);
  llvm::Value *ret = llvm::UndefValue::get (complex_ret);

  unsigned int re_idx[] = {0, 0};
  unsigned int im_idx[] = {0, 1};
  ret = bld.CreateInsertValue (ret, real, re_idx);
  return bld.CreateInsertValue (ret, imag, im_idx);
}

llvm::Value *
jit_typeinfo::unpack_complex (llvm::IRBuilderD& bld, llvm::Value *result)
{
  unsigned int re_idx[] = {0, 0};
  unsigned int im_idx[] = {0, 1};

  llvm::Type *complex_t = get_complex ()->to_llvm ();
  llvm::Value *real = bld.CreateExtractValue (result, re_idx);
  llvm::Value *imag = bld.CreateExtractValue (result, im_idx);
  llvm::Value *ret = llvm::UndefValue::get (complex_t);

  ret = bld.CreateInsertValue (ret, real, 0);
  return bld.CreateInsertValue (ret, imag, 1);
}

llvm::Value *
jit_typeinfo::complex_real (llvm::Value *cx)
{
  return builder.CreateExtractValue (cx, 0);
}

llvm::Value *
jit_typeinfo::complex_real (llvm::Value *cx, llvm::Value *real)
{
  return builder.CreateInsertValue (cx, real, 0);
}

llvm::Value *
jit_typeinfo::complex_imag (llvm::Value *cx)
{
  return builder.CreateExtractValue (cx, 1);
}

llvm::Value *
jit_typeinfo::complex_imag (llvm::Value *cx, llvm::Value *imag)
{
  return builder.CreateInsertValue (cx, imag, 1);
}

llvm::Value *
jit_typeinfo::complex_new (llvm::Value *real, llvm::Value *imag)
{
  llvm::Value *ret = llvm::UndefValue::get (complex->to_llvm ());
  ret = complex_real (ret, real);
  return complex_imag (ret, imag);
}

void
jit_typeinfo::create_int (size_t nbits)
{
  std::stringstream tname;
  tname << "int" << nbits;
  ints[nbits] = new_type (tname.str (), any, llvm::Type::getIntNTy (context,
                                                                    nbits));
}

jit_type *
jit_typeinfo::intN (size_t nbits) const
{
  std::map<size_t, jit_type *>::const_iterator iter = ints.find (nbits);
  if (iter != ints.end ())
    return iter->second;

  throw jit_fail_exception ("No such integer type");
}

jit_type *
jit_typeinfo::do_type_of (const octave_value &ov) const
{
  if (ov.is_function ())
    {
      // FIXME: This is ugly, we need to finalize how we want to to this, then
      // have octave_value fully support the needed functionality
      octave_builtin *builtin
        = dynamic_cast<octave_builtin *> (ov.internal_rep ());
      return builtin && builtin->to_jit () ? builtin->to_jit ()
                                           : unknown_function;
    }

  if (ov.is_range ())
    return get_range ();

  if (ov.is_double_type () && ! ov.is_complex_type ())
    {
      if (ov.is_real_scalar ())
        return get_scalar ();

      if (ov.is_matrix_type ())
        return get_matrix ();
    }

  if (ov.is_complex_scalar ())
    {
      Complex cv = ov.complex_value ();

      // We don't really represent complex values, instead we represent
      // complex_or_scalar. If the imag value is zero, we assume a scalar.
      if (cv.imag () != 0)
        return get_complex ();
    }

  return get_any ();
}

#endif
