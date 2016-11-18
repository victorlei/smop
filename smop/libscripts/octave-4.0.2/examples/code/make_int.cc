#include <octave/config.h>

#include <cstdlib>

#include <string>

#include <ostream>

#include <octave/lo-mappers.h>
#include <octave/lo-utils.h>
#include <octave/mx-base.h>
#include <octave/str-vec.h>

#include <octave/defun-dld.h>
#include <octave/error.h>
#include <octave/gripes.h>
#include <octave/oct-obj.h>
#include <octave/ops.h>
#include <octave/ov-base.h>
#include <octave/ov-typeinfo.h>
#include <octave/ov.h>
#include <octave/ov-scalar.h>
#include <octave/pager.h>
#include <octave/pr-output.h>
#include <octave/symtab.h>
#include <octave/variables.h>

class octave_value_list;

class tree_walker;

// Integer values.

class
octave_integer : public octave_base_value
{
public:

  octave_integer (void)
    : octave_base_value (), scalar (0) { }

  octave_integer (int i)
    : octave_base_value (), scalar (i) { }

  octave_integer (const octave_integer& s)
    : octave_base_value (), scalar (s.scalar) { }

  ~octave_integer (void) { }

  octave_base_value *clone (void) { return new octave_integer (*this); }

#if 0
  void *operator new (size_t size);
  void operator delete (void *p, size_t size);
#endif

  idx_vector index_vector (void) const { return idx_vector ((double) scalar); }

  int rows (void) const { return 1; }
  int columns (void) const { return 1; }

  bool is_constant (void) const { return true; }

  bool is_defined (void) const { return true; }
  bool is_real_scalar (void) const { return true; }

  octave_value all (void) const { return (double) (scalar != 0); }
  octave_value any (void) const { return (double) (scalar != 0); }

  bool is_real_type (void) const { return true; }
  bool is_scalar_type (void) const { return true; }
  bool is_numeric_type (void) const { return true; }

  bool valid_as_scalar_index (void) const
  { return scalar == 1; }

  bool valid_as_zero_index (void) const
  { return scalar == 0; }

  bool is_true (void) const { return (scalar != 0); }

  double double_value (bool = false) const { return (double) scalar; }

  int integer_value (bool = false) const { return scalar; }

  Matrix matrix_value (bool = false) const { return Matrix (1, 1, scalar); }

  Complex complex_value (bool = false) const { return scalar; }

  ComplexMatrix complex_matrix_value (bool = false) const
  { return  ComplexMatrix (1, 1, Complex (scalar)); }

  octave_value gnot (void) const { return octave_value ((double) ! scalar); }

  octave_value uminus (void) const { return new octave_integer (- scalar); }

  octave_value transpose (void) const { return new octave_integer (scalar); }

  octave_value hermitian (void) const { return new octave_integer (scalar); }

  void increment (void) { ++scalar; }

  void decrement (void) { --scalar; }

  void print (std::ostream& os, bool pr_as_read_syntax = false);

private:

  int scalar;


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

void
octave_integer::print (std::ostream& os, bool pr_as_read_syntax)
{
  os << scalar;
  newline (os);
}

#ifdef DEFUNOP_OP
#undef DEFUNOP_OP
#endif

#define DEFUNOP_OP(name, t, op) \
  UNOPDECL (name, a) \
  { \
    CAST_UNOP_ARG (const octave_ ## t&); \
    return octave_value (new octave_integer (op v.t ## _value ())); \
  }

DEFUNOP_OP (gnot, integer, !)
DEFUNOP_OP (uminus, integer, -)
DEFUNOP_OP (transpose, integer, /* no-op */)
DEFUNOP_OP (hermitian, integer, /* no-op */)

DEFNCUNOP_METHOD (incr, integer, increment)
DEFNCUNOP_METHOD (decr, integer, decrement)

#ifdef DEFBINOP_OP
#undef DEFBINOP_OP
#endif

#define DEFBINOP_OP(name, t1, t2, op) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const octave_ ## t1&, const octave_ ## t2&); \
    return octave_value \
      (new octave_integer (v1.t1 ## _value () op v2.t2 ## _value ())); \
  }

// integer by integer ops.

DEFBINOP_OP (add, integer, integer, +)
DEFBINOP_OP (sub, integer, integer, -)
DEFBINOP_OP (mul, integer, integer, *)

DEFBINOP (div, integer, integer)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v2.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v1.integer_value () / d);
}


DEFBINOP (i_s_div, integer, scalar)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return new octave_scalar (v1.double_value () / d);
}

DEFBINOP (ldiv, integer, integer)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v1.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v2.integer_value () / d);
}

DEFBINOP_OP (lt, integer, integer, <)
DEFBINOP_OP (le, integer, integer, <=)
DEFBINOP_OP (eq, integer, integer, ==)
DEFBINOP_OP (ge, integer, integer, >=)
DEFBINOP_OP (gt, integer, integer, >)
DEFBINOP_OP (ne, integer, integer, !=)

DEFBINOP_OP (el_mul, integer, integer, !=)

DEFBINOP (el_div, integer, integer)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v2.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v1.integer_value () / d);
}

DEFBINOP (el_ldiv, integer, integer)
{
  CAST_BINOP_ARGS (const octave_integer&, const octave_integer&);

  int d = v1.integer_value ();

  if (d == 0)
    gripe_divide_by_zero ();

  return new octave_integer (v2.integer_value () / d);
}

DEFBINOP_OP (el_and, integer, integer, &&)
DEFBINOP_OP (el_or, integer, integer, ||)

DEFUN_DLD (make_int, args, ,
           "int_val = make_int (val)\n\
\n\
Creates an integer variable from VAL.")
{
  static bool type_loaded = false;

  if (! type_loaded)
    {
      octave_integer::register_type ();
      mlock ();

      octave_stdout << "installing integer type at type-id = "
                    << octave_integer::static_type_id () << "\n";

      INSTALL_UNOP (op_not, octave_integer, gnot);
      INSTALL_UNOP (op_uminus, octave_integer, uminus);
      INSTALL_UNOP (op_transpose, octave_integer, transpose);
      INSTALL_UNOP (op_hermitian, octave_integer, hermitian);

      INSTALL_NCUNOP (op_incr, octave_integer, incr);
      INSTALL_NCUNOP (op_decr, octave_integer, decr);

      INSTALL_BINOP (op_add, octave_integer, octave_integer, add);
      INSTALL_BINOP (op_sub, octave_integer, octave_integer, sub);
      INSTALL_BINOP (op_mul, octave_integer, octave_integer, mul);
      INSTALL_BINOP (op_div, octave_integer, octave_integer, div);
      INSTALL_BINOP (op_ldiv, octave_integer, octave_integer, ldiv);
      INSTALL_BINOP (op_lt, octave_integer, octave_integer, lt);
      INSTALL_BINOP (op_le, octave_integer, octave_integer, le);
      INSTALL_BINOP (op_eq, octave_integer, octave_integer, eq);
      INSTALL_BINOP (op_ge, octave_integer, octave_integer, ge);
      INSTALL_BINOP (op_gt, octave_integer, octave_integer, gt);
      INSTALL_BINOP (op_ne, octave_integer, octave_integer, ne);
      INSTALL_BINOP (op_el_mul, octave_integer, octave_integer, el_mul);
      INSTALL_BINOP (op_el_div, octave_integer, octave_integer, el_div);
      INSTALL_BINOP (op_el_ldiv, octave_integer, octave_integer, el_ldiv);
      INSTALL_BINOP (op_el_and, octave_integer, octave_integer, el_and);
      INSTALL_BINOP (op_el_or, octave_integer, octave_integer, el_or);

      INSTALL_BINOP (op_div, octave_integer, octave_scalar, i_s_div);
    }

  octave_value retval;

  if (args.length () == 1)
    {
      double d = args(0).double_value ();

      if (! error_state)
        retval = octave_value (new octave_integer (NINT (d)));
    }
  else
    usage ("make_int");

  return retval;
}

DEFUN_DLD (doit, args, ,
           "doit (I)")
{
  octave_value_list retval;

  if (args(0).type_id () == octave_integer::static_type_id ())
    {
      // At this point, we know we have a handle for an octave_integer
      // object, so we can peek at the representation and extract the
      // data.

      const octave_base_value& rep = args(0).get_rep ();

      int my_value = ((const octave_integer&) rep) . integer_value ();

      message ("doit", "your lucky number is: %d", my_value);
    }
  else
    gripe_wrong_type_arg ("doit", args(0));

  return retval;
}


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_integer, "integer", "integer");
