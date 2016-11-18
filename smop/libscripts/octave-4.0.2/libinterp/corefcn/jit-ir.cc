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

#include "jit-ir.h"

#ifdef HAVE_LLVM_IR_FUNCTION_H
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#else
#include <llvm/BasicBlock.h>
#include <llvm/Instructions.h>
#endif

#include "error.h"

// -------------------- jit_factory --------------------
jit_factory::~jit_factory (void)
{
  for (value_list::iterator iter = all_values.begin ();
       iter != all_values.end (); ++iter)
    delete *iter;
}

void
jit_factory::track_value (jit_value *value)
{
  if (value->type ())
    mconstants.push_back (value);
  all_values.push_back (value);
}

// -------------------- jit_block_list --------------------
void
jit_block_list::insert_after (iterator iter, jit_block *ablock)
{
  ++iter;
  insert_before (iter, ablock);
}

void
jit_block_list::insert_after (jit_block *loc, jit_block *ablock)
{
  insert_after (loc->location (), ablock);
}

void
jit_block_list::insert_before (iterator iter, jit_block *ablock)
{
  iter = mlist.insert (iter, ablock);
  ablock->stash_location (iter);
}

void
jit_block_list::insert_before (jit_block *loc, jit_block *ablock)
{
  insert_before (loc->location (), ablock);
}

void
jit_block_list::label (void)
{
  if (mlist.size ())
    {
      jit_block *block = mlist.back ();
      block->label ();
    }
}

std::ostream&
jit_block_list::print (std::ostream& os, const std::string& header) const
{
  os << "-------------------- " << header << " --------------------\n";
  return os << *this;
}

std::ostream&
jit_block_list::print_dom (std::ostream& os) const
{
  os << "-------------------- dom info --------------------\n";
  for (const_iterator iter = begin (); iter != end (); ++iter)
    {
      assert (*iter);
      (*iter)->print_dom (os);
    }
  os << std::endl;

  return os;
}

void
jit_block_list::push_back (jit_block *b)
{
  mlist.push_back (b);
  iterator iter = mlist.end ();
  b->stash_location (--iter);
}

std::ostream&
operator<<(std::ostream& os, const jit_block_list& blocks)
{
  for (jit_block_list::const_iterator iter = blocks.begin ();
       iter != blocks.end (); ++iter)
    {
      assert (*iter);
      (*iter)->print (os, 0);
    }
  return os << std::endl;
}

// -------------------- jit_use --------------------
jit_block *
jit_use::user_parent (void) const
{
  return muser->parent ();
}

// -------------------- jit_value --------------------
jit_value::~jit_value (void)
{}

jit_block *
jit_value::first_use_block (void)
{
  jit_use *use = first_use ();
  while (use)
    {
      if (! isa<jit_error_check> (use->user ()))
        return use->user_parent ();

      use = use->next ();
    }

  return 0;
}

void
jit_value::replace_with (jit_value *value)
{
  while (first_use ())
    {
      jit_instruction *user = first_use ()->user ();
      size_t idx = first_use ()->index ();
      user->stash_argument (idx, value);
    }
}

#define JIT_METH(clname)                                \
  void                                                  \
  jit_ ## clname::accept (jit_ir_walker& walker)        \
  {                                                     \
    walker.visit (*this);                               \
  }

JIT_VISIT_IR_NOTEMPLATE
#undef JIT_METH

std::ostream&
operator<< (std::ostream& os, const jit_value& value)
{
  return value.short_print (os);
}

std::ostream&
jit_print (std::ostream& os, jit_value *avalue)
{
  if (avalue)
    return avalue->print (os);
  return os << "NULL";
}

// -------------------- jit_instruction --------------------
void
jit_instruction::remove (void)
{
  if (mparent)
    mparent->remove (mlocation);
  resize_arguments (0);
}

llvm::BasicBlock *
jit_instruction::parent_llvm (void) const
{
  return mparent->to_llvm ();
}

std::ostream&
jit_instruction::short_print (std::ostream& os) const
{
  if (type ())
    jit_print (os, type ()) << ": ";
  return os << "#" << mid;
}

void
jit_instruction::do_construct_ssa (size_t start, size_t end)
{
  for (size_t i = start; i < end; ++i)
    {
      jit_value *arg = argument (i);
      jit_variable *var = dynamic_cast<jit_variable *> (arg);
      if (var && var->has_top ())
        stash_argument (i, var->top ());
    }
}

// -------------------- jit_block --------------------
void
jit_block::replace_with (jit_value *value)
{
  assert (isa<jit_block> (value));
  jit_block *block = static_cast<jit_block *> (value);

  jit_value::replace_with (block);

  while (ILIST_T::first_use ())
    {
      jit_phi_incomming *incomming = ILIST_T::first_use ();
      incomming->stash_value (block);
    }
}

void
jit_block::replace_in_phi (jit_block *ablock, jit_block *with)
{
  jit_phi_incomming *node = ILIST_T::first_use ();
  while (node)
    {
      jit_phi_incomming *prev = node;
      node = node->next ();

      if (prev->user_parent () == ablock)
        prev->stash_value (with);
    }
}

jit_block *
jit_block::maybe_merge ()
{
  if (successor_count () == 1 && successor (0) != this
      && (successor (0)->use_count () == 1 || instructions.size () == 1))
    {
      jit_block *to_merge = successor (0);
      merge (*to_merge);
      return to_merge;
    }

  return 0;
}

void
jit_block::merge (jit_block& block)
{
  // the merge block will contain a new terminator
  jit_terminator *old_term = terminator ();
  if (old_term)
    old_term->remove ();

  bool was_empty = end () == begin ();
  iterator merge_begin = end ();
  if (! was_empty)
    --merge_begin;

  instructions.splice (end (), block.instructions);
  if (was_empty)
    merge_begin = begin ();
  else
    ++merge_begin;

  // now merge_begin points to the start of the new instructions, we must
  // update their parent information
  for (iterator iter = merge_begin; iter != end (); ++iter)
    {
      jit_instruction *instr = *iter;
      instr->stash_parent (this, iter);
    }

  block.replace_with (this);
}

jit_instruction *
jit_block::prepend (jit_instruction *instr)
{
  instructions.push_front (instr);
  instr->stash_parent (this, instructions.begin ());
  return instr;
}

jit_instruction *
jit_block::prepend_after_phi (jit_instruction *instr)
{
  // FIXME: Make this O(1)
  for (iterator iter = begin (); iter != end (); ++iter)
    {
      jit_instruction *temp = *iter;
      if (! isa<jit_phi> (temp))
        {
          insert_before (iter, instr);
          return instr;
        }
    }

  return append (instr);
}

void
jit_block::internal_append (jit_instruction *instr)
{
  instructions.push_back (instr);
  instr->stash_parent (this, --instructions.end ());
}

jit_instruction *
jit_block::insert_before (iterator loc, jit_instruction *instr)
{
  iterator iloc = instructions.insert (loc, instr);
  instr->stash_parent (this, iloc);
  return instr;
}

jit_instruction *
jit_block::insert_after (iterator loc, jit_instruction *instr)
{
  ++loc;
  iterator iloc = instructions.insert (loc, instr);
  instr->stash_parent (this, iloc);
  return instr;
}

jit_terminator *
jit_block::terminator (void) const
{
  assert (this);
  if (instructions.empty ())
    return 0;

  jit_instruction *last = instructions.back ();
  return dynamic_cast<jit_terminator *> (last);
}

bool
jit_block::branch_alive (jit_block *asucc) const
{
  return terminator ()->alive (asucc);
}

jit_block *
jit_block::successor (size_t i) const
{
  jit_terminator *term = terminator ();
  return term->successor (i);
}

size_t
jit_block::successor_count (void) const
{
  jit_terminator *term = terminator ();
  return term ? term->successor_count () : 0;
}

llvm::BasicBlock *
jit_block::to_llvm (void) const
{
  return llvm::cast<llvm::BasicBlock> (llvm_value);
}

std::ostream&
jit_block::print_dom (std::ostream& os) const
{
  short_print (os);
  os << ":\n";
  os << "  mid: " << mid << std::endl;
  os << "  predecessors: ";
  for (jit_use *use = first_use (); use; use = use->next ())
    os << *use->user_parent () << " ";
  os << std::endl;

  os << "  successors: ";
  for (size_t i = 0; i < successor_count (); ++i)
    os << *successor (i) << " ";
  os << std::endl;

  os << "  idom: ";
  if (idom)
    os << *idom;
  else
    os << "NULL";
  os << std::endl;
  os << "  df: ";
  for (df_iterator iter = df_begin (); iter != df_end (); ++iter)
    os << **iter << " ";
  os << std::endl;

  os << "  dom_succ: ";
  for (size_t i = 0; i < dom_succ.size (); ++i)
    os << *dom_succ[i] << " ";

  return os << std::endl;
}

void
jit_block::compute_df (size_t avisit_count)
{
  if (visited (avisit_count))
    return;

  if (use_count () >= 2)
    {
      for (jit_use *use = first_use (); use; use = use->next ())
        {
          jit_block *runner = use->user_parent ();
          while (runner != idom)
            {
              runner->mdf.insert (this);
              runner = runner->idom;
            }
        }
    }

  for (size_t i = 0; i < successor_count (); ++i)
    successor (i)->compute_df (avisit_count);
}

bool
jit_block::update_idom (size_t avisit_count)
{
  if (visited (avisit_count) || ! use_count ())
    return false;

  bool changed = false;
  for (jit_use *use = first_use (); use; use = use->next ())
    {
      jit_block *pred = use->user_parent ();
      changed = pred->update_idom (avisit_count) || changed;
    }

  jit_use *use = first_use ();
  jit_block *new_idom = use->user_parent ();
  use = use->next ();

  for (; use; use = use->next ())
    {
      jit_block *pred = use->user_parent ();
      jit_block *pidom = pred->idom;
      if (pidom)
        new_idom = idom_intersect (pidom, new_idom);
    }

  if (idom != new_idom)
    {
      idom = new_idom;
      return true;
    }

  return changed;
}

void
jit_block::label (size_t avisit_count, size_t& number)
{
  if (visited (avisit_count))
    return;

  for (jit_use *use = first_use (); use; use = use->next ())
    {
      jit_block *pred = use->user_parent ();
      pred->label (avisit_count, number);
    }

  mid = number++;
}

void
jit_block::pop_all (void)
{
  for (iterator iter = begin (); iter != end (); ++iter)
    {
      jit_instruction *instr = *iter;
      instr->pop_variable ();
    }
}

std::ostream&
jit_block::print (std::ostream& os, size_t indent) const
{
  print_indent (os, indent);
  short_print (os) << ":        %pred = ";
  for (jit_use *use = first_use (); use; use = use->next ())
    {
      jit_block *pred = use->user_parent ();
      os << *pred;
      if (use->next ())
        os << ", ";
    }
  os << std::endl;

  for (const_iterator iter = begin (); iter != end (); ++iter)
    {
      jit_instruction *instr = *iter;
      instr->print (os, indent + 1) << std::endl;
    }
  return os;
}

jit_block *
jit_block::maybe_split (jit_factory& factory, jit_block_list& blocks,
                        jit_block *asuccessor)
{
  if (successor_count () > 1)
    {
      jit_terminator *term = terminator ();
      size_t idx = term->successor_index (asuccessor);
      jit_block *split = factory.create<jit_block> ("phi_split", mvisit_count);

      // place after this to ensure define before use in the blocks list
      blocks.insert_after (this, split);

      term->stash_argument (idx, split);
      jit_branch *br = split->append (factory.create<jit_branch> (asuccessor));
      replace_in_phi (asuccessor, split);

      if (alive ())
        {
          split->mark_alive ();
          br->infer ();
        }

      return split;
    }

  return this;
}

void
jit_block::create_dom_tree (size_t avisit_count)
{
  if (visited (avisit_count))
    return;

  if (idom != this)
    idom->dom_succ.push_back (this);

  for (size_t i = 0; i < successor_count (); ++i)
    successor (i)->create_dom_tree (avisit_count);
}

jit_block *
jit_block::idom_intersect (jit_block *i, jit_block *j)
{
  while (i && j && i != j)
    {
      while (i && i->id () > j->id ())
        i = i->idom;

      while (i && j && j->id () > i->id ())
        j = j->idom;
    }

  return i ? i : j;
}

// -------------------- jit_phi_incomming --------------------

jit_block *
jit_phi_incomming::user_parent (void) const
{ return muser->parent (); }

// -------------------- jit_phi --------------------
bool
jit_phi::prune (void)
{
  jit_block *p = parent ();
  size_t new_idx = 0;
  jit_value *unique = argument (1);

  for (size_t i = 0; i < argument_count (); ++i)
    {
      jit_block *inc = incomming (i);
      if (inc->branch_alive (p))
        {
          if (unique != argument (i))
            unique = 0;

          if (new_idx != i)
            {
              stash_argument (new_idx, argument (i));
              mincomming[new_idx].stash_value (inc);
            }

          ++new_idx;
        }
    }

  if (new_idx != argument_count ())
    {
      resize_arguments (new_idx);
      mincomming.resize (new_idx);
    }

  assert (argument_count () > 0);
  if (unique)
    {
      replace_with (unique);
      return true;
    }

  return false;
}

bool
jit_phi::infer (void)
{
  jit_block *p = parent ();
  if (! p->alive ())
    return false;

  jit_type *infered = 0;
  for (size_t i = 0; i < argument_count (); ++i)
    {
      jit_block *inc = incomming (i);
      if (inc->branch_alive (p))
        infered = jit_typeinfo::join (infered, argument_type (i));
    }

  if (infered != type ())
    {
      stash_type (infered);
      return true;
    }

  return false;
}

llvm::PHINode *
jit_phi::to_llvm (void) const
{
  return llvm::cast<llvm::PHINode> (jit_value::to_llvm ());
}

// -------------------- jit_terminator --------------------
size_t
jit_terminator::successor_index (const jit_block *asuccessor) const
{
  size_t scount = successor_count ();
  for (size_t i = 0; i < scount; ++i)
    if (successor (i) == asuccessor)
      return i;

  panic_impossible ();
}

bool
jit_terminator::infer (void)
{
  if (! parent ()->alive ())
    return false;

  bool changed = false;
  for (size_t i = 0; i < malive.size (); ++i)
    if (! malive[i] && check_alive (i))
      {
        changed = true;
        malive[i] = true;
        successor (i)->mark_alive ();
      }

  return changed;
}

llvm::TerminatorInst *
jit_terminator::to_llvm (void) const
{
  return llvm::cast<llvm::TerminatorInst> (jit_value::to_llvm ());
}

// -------------------- jit_call --------------------
bool
jit_call::needs_release (void) const
{
  if (type () && jit_typeinfo::get_release (type ()).valid ())
    {
      for (jit_use *use = first_use (); use; use = use->next ())
        {
          jit_assign *assign = dynamic_cast<jit_assign *> (use->user ());
          if (assign && assign->artificial ())
            return false;
        }

      return true;
    }
  return false;
}

bool
jit_call::infer (void)
{
  // FIXME: explain algorithm
  for (size_t i = 0; i < argument_count (); ++i)
    {
      already_infered[i] = argument_type (i);
      if (! already_infered[i])
        return false;
    }

  jit_type *infered = moperation.result (already_infered);
  if (! infered && use_count ())
    {
      std::stringstream ss;
      ss << "Missing overload in type inference for ";
      print (ss, 0);
      throw jit_fail_exception (ss.str ());
    }

  if (infered != type ())
    {
      stash_type (infered);
      return true;
    }

  return false;
}

// -------------------- jit_error_check --------------------
std::string
jit_error_check::variable_to_string (variable v)
{
  switch (v)
    {
    case var_error_state:
      return "error_state";
    case var_interrupt:
      return "interrupt";
    default:
      panic_impossible ();
    }
}

std::ostream&
jit_error_check::print (std::ostream& os, size_t indent) const
{
  print_indent (os, indent) << "error_check " << variable_to_string (mvariable)
                            << ", ";

  if (has_check_for ())
    os << "<for> " << *check_for () << ", ";
  print_successor (os << "<normal> ", 1) << ", ";
  return print_successor (os << "<error> ", 0);
}

// -------------------- jit_magic_end --------------------
jit_magic_end::context::context (jit_factory& factory, jit_value *avalue,
                                 size_t aindex, size_t acount)
  : value (avalue), index (factory.create<jit_const_index> (aindex)),
    count (factory.create<jit_const_index> (acount))
{}

jit_magic_end::jit_magic_end (const std::vector<context>& full_context)
  : contexts (full_context)
{
  resize_arguments (contexts.size ());

  size_t i;
  std::vector<context>::const_iterator iter;
  for (iter = contexts.begin (), i = 0; iter != contexts.end (); ++iter, ++i)
    stash_argument (i, iter->value);
}

jit_magic_end::context
jit_magic_end::resolve_context (void) const
{
  size_t idx;
  for (idx = 0; idx < contexts.size (); ++idx)
    {
      jit_type *ctx_type = contexts[idx].value->type ();
      if (! ctx_type || ctx_type->skip_paren ())
        break;
    }

  if (idx >= contexts.size ())
    idx = 0;

  context ret = contexts[idx];
  ret.value = argument (idx);
  return ret;
}

bool
jit_magic_end::infer (void)
{
  jit_type *new_type = overload ().result ();
  if (new_type != type ())
    {
      stash_type (new_type);
      return true;
    }

  return false;
}

std::ostream&
jit_magic_end::print (std::ostream& os, size_t indent) const
{
  context ctx = resolve_context ();
  short_print (print_indent (os, indent)) << " (" << *ctx.value << ", ";
  return os << *ctx.index << ", " << *ctx.count << ")";
}

const jit_function&
jit_magic_end::overload () const
{
  const context& ctx = resolve_context ();
  return jit_typeinfo::end (ctx.value, ctx.index, ctx.count);
}

#endif
