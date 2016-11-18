// oct_demo.cc -- example of a dynamically linked function for Octave.

// To use this file, your version of Octave must support dynamic
// linking.  To find out if it does, type the command
//
//   octave_config_info ("ENABLE_DYNAMIC_LINKING")
//
// at the Octave prompt.  Support for dynamic linking is included if
// this expression returns the string "yes".
//
// To compile this file, type the command
//
//   mkoctfile oct_demo.cc
//
// from within Octave or from the shell prompt.  This will create a file
// called oct_demo.oct that can be loaded by Octave.  To test the
// oct_demo.oct file, start Octave and type the command
//
//   oct_demo ("easy as", 1, 2, 3)
//
// at the Octave prompt.  Octave should respond by printing
//
//   Hello, world!
//   easy as
//   1
//   2
//   3
//   ans = 3

// Additional samples of real dynamically loaded functions are available in
// the files of the libinterp/dldfcn directory of the Octave distribution.
// See also the chapter External Code Interface in the documentation.

#include <iostream>

#include <octave/oct.h>

// Every user function should include <octave/oct.h> which imports the
// basic set of Octave header files required.  In particular this will define
// the DEFUN_DLD macro (defun-dld.h) which is used for every user function
// that is visible to Octave.

// The four arguments to the DEFUN_DLD macro are:
// 1) The function name as seen in Octave.
// 2) The variable to hold any inputs (of type octave_value_list)
// 3) The number of output arguments
// 4) A string to use as help text if 'help <function_name>' is entered.
//
// Note below that the third parameter (nargout) of DEFUN_DLD is not used,
// so it is omitted from the list of arguments in order to avoid a warning
// from gcc about an unused function parameter.

DEFUN_DLD (oct_demo, args, ,
           "[...] = oct_demo (...)\n\
\n\
Print a greeting followed by the values of all the arguments passed.\n\
Return all arguments in reverse order.")
{
  // The list of values to return.  See the declaration in oct-obj.h

  octave_value_list retval;

  // This stream is normally connected to the pager.

  octave_stdout << "Hello, world!\n";

  // The inputs to this function are available in args.

  int nargin = args.length ();

  // The octave_value_list class is a zero-based array of octave_value objects.
  // The declaration for the octave_value class is in the file ov.h.
  // The print() method will send its output to octave_stdout,
  // so it will also end up going through the pager.

  for (int i = 0; i < nargin; i++)
    {
      octave_value tmp = args(i);
      tmp.print (octave_stdout);
      retval(nargin-i-1) = tmp;
    }

  return retval;
}
