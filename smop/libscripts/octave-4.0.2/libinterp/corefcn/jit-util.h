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

// Some utility classes and functions used throughout jit

#if !defined (octave_jit_util_h)
#define octave_jit_util_h 1

#ifdef HAVE_LLVM

#include <stdexcept>

#if defined(HAVE_LLVM_IR_DATALAYOUT_H) || defined(HAVE_LLVM_DATALAYOUT_H)
#define HAVE_LLVM_DATALAYOUT
#endif

// we don't want to include llvm headers here, as they require
// __STDC_LIMIT_MACROS and __STDC_CONSTANT_MACROS be defined in the entire
// compilation unit
namespace llvm
{
  class Value;
  class Module;
#ifdef LEGACY_PASSMANAGER
  namespace legacy {
    class FunctionPassManager;
    class PassManager;
  }
#else
  class FunctionPassManager;
  class PassManager;
#endif
  class ExecutionEngine;
  class Function;
  class BasicBlock;
  class LLVMContext;
  class Type;
  class StructType;
  class Twine;
  class GlobalVariable;
  class TerminatorInst;
  class PHINode;

  class ConstantFolder;

  template <bool preserveNames>
  class IRBuilderDefaultInserter;

  template <bool preserveNames, typename T, typename Inserter>
  class IRBuilder;

typedef IRBuilder<true, ConstantFolder, IRBuilderDefaultInserter<true> >
IRBuilderD;
}

class octave_base_value;
class octave_builtin;
class octave_value;
class tree;
class tree_expression;

// thrown when we should give up on JIT and interpret
class jit_fail_exception : public std::runtime_error
{
public:
  jit_fail_exception (void) : std::runtime_error ("unknown"), mknown (false) { }
  jit_fail_exception (const std::string& reason) : std::runtime_error (reason),
                                                   mknown (true)
  { }

  bool known (void) const { return mknown; }
private:
  bool mknown;
};

// llvm doesn't provide this, and it's really useful for debugging
std::ostream& operator<< (std::ostream& os, const llvm::Value& v);

template <typename HOLDER_T, typename SUB_T>
class jit_internal_node;

// jit_internal_list and jit_internal_node implement generic embedded doubly
// linked lists. List items extend from jit_internal_list, and can be placed
// in nodes of type jit_internal_node. We use CRTP twice.
template <typename LIST_T, typename NODE_T>
class
jit_internal_list
{
  friend class jit_internal_node<LIST_T, NODE_T>;
public:
  jit_internal_list (void) : use_head (0), use_tail (0), muse_count (0) { }

  virtual ~jit_internal_list (void)
  {
    while (use_head)
      use_head->stash_value (0);
  }

  NODE_T *first_use (void) const { return use_head; }

  size_t use_count (void) const { return muse_count; }
private:
  NODE_T *use_head;
  NODE_T *use_tail;
  size_t muse_count;
};

// a node for internal linked lists
template <typename LIST_T, typename NODE_T>
class
jit_internal_node
{
public:
  typedef jit_internal_list<LIST_T, NODE_T> jit_ilist;

  jit_internal_node (void) : mvalue (0), mnext (0), mprev (0) { }

  ~jit_internal_node (void) { remove (); }

  LIST_T *value (void) const { return mvalue; }

  void stash_value (LIST_T *avalue)
  {
    remove ();

    mvalue = avalue;

    if (mvalue)
      {
        jit_ilist *ilist = mvalue;
        NODE_T *sthis = static_cast<NODE_T *> (this);
        if (ilist->use_head)
          {
            ilist->use_tail->mnext = sthis;
            mprev = ilist->use_tail;
          }
        else
          ilist->use_head = sthis;

        ilist->use_tail = sthis;
        ++ilist->muse_count;
      }
  }

  NODE_T *next (void) const { return mnext; }

  NODE_T *prev (void) const { return mprev; }
private:
  void remove ()
  {
    if (mvalue)
      {
        jit_ilist *ilist = mvalue;
        if (mprev)
          mprev->mnext = mnext;
        else
          // we are the use_head
          ilist->use_head = mnext;

        if (mnext)
          mnext->mprev = mprev;
        else
          // we are the use tail
          ilist->use_tail = mprev;

        mnext = mprev = 0;
        --ilist->muse_count;
        mvalue = 0;
      }
  }

  LIST_T *mvalue;
  NODE_T *mnext;
  NODE_T *mprev;
};

// Use like: isa<jit_phi> (value)
// basically just a short cut type typing dyanmic_cast.
template <typename T, typename U>
bool isa (U *value)
{
  return dynamic_cast<T *> (value);
}

#define JIT_ASSIGN_ARG(i) the_args[i] = arg ## i;
#define JIT_EXPAND(ret, fname, type, isconst, N)                        \
  ret fname (JIT_PARAM_ARGS OCT_MAKE_DECL_LIST (type, arg, N)) isconst  \
  {                                                                     \
    std::vector<type> the_args (N);                                     \
    OCT_ITERATE_MACRO (JIT_ASSIGN_ARG, N);                              \
    return fname (JIT_PARAMS the_args);                                 \
  }

#endif
#endif
