## Copyright (C) 2006-2015 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @cindex warning ids
##
## @table @code
## @item Octave:abbreviated-property-match
## By default, the @code{Octave:abbreviated-property-match} warning is enabled.
##
## @item Octave:array-to-scalar
## If the @code{Octave:array-to-scalar} warning is enabled, Octave will
## warn when an implicit conversion from an array to a scalar value is
## attempted.
## By default, the @code{Octave:array-to-scalar} warning is disabled.
##
## @item Octave:array-to-vector
## If the @code{Octave:array-to-vector} warning is enabled, Octave will
## warn when an implicit conversion from an array to a vector value is
## attempted.
## By default, the @code{Octave:array-to-vector} warning is disabled.
##
## @item Octave:assign-as-truth-value
## If the @code{Octave:assign-as-truth-value} warning is
## enabled, a warning is issued for statements like
##
## @example
## @group
## if (s = t)
##   @dots{}
## @end group
## @end example
##
## @noindent
## since such statements are not common, and it is likely that the intent
## was to write
##
## @example
## @group
## if (s == t)
##   @dots{}
## @end group
## @end example
##
## @noindent
## instead.
##
## There are times when it is useful to write code that contains
## assignments within the condition of a @code{while} or @code{if}
## statement.  For example, statements like
##
## @example
## @group
## while (c = getc ())
##   @dots{}
## @end group
## @end example
##
## @noindent
## are common in C programming.
##
## It is possible to avoid all warnings about such statements by
## disabling the @code{Octave:assign-as-truth-value} warning,
## but that may also let real errors like
##
## @example
## @group
## if (x = 1)  # intended to test (x == 1)!
##   @dots{}
## @end group
## @end example
##
## @noindent
## slip by.
##
## In such cases, it is possible suppress errors for specific statements by
## writing them with an extra set of parentheses.  For example, writing the
## previous example as
##
## @example
## @group
## while ((c = getc ()))
##   @dots{}
## @end group
## @end example
##
## @noindent
## will prevent the warning from being printed for this statement, while
## allowing Octave to warn about other assignments used in conditional
## contexts.
##
## By default, the @code{Octave:assign-as-truth-value} warning is enabled.
##
## @item Octave:associativity-change
## If the @code{Octave:associativity-change} warning is
## enabled, Octave will warn about possible changes in the meaning of
## some code due to changes in associativity for some operators.
## Associativity changes have typically been made for @sc{matlab}
## compatibility.
## By default, the @code{Octave:associativity-change} warning is enabled.
##
## @item Octave:autoload-relative-file-name
## If the @code{Octave:autoload-relative-file-name} is enabled,
## Octave will warn when parsing autoload() function calls with relative
## paths to function files.  This usually happens when using autoload()
## calls in PKG_ADD files, when the PKG_ADD file is not in the same
## directory as the .oct file referred to by the autoload() command.
## By default, the @code{Octave:autoload-relative-file-name} warning is enabled.
##
## @item Octave:built-in-variable-assignment
## By default, the @code{Octave:built-in-variable-assignment} warning is
## enabled.
##
## @item Octave:deprecated-keyword
## If the @code{Octave:deprecated-keyword} warning is enabled, a
## warning is issued when Octave encounters a keyword that is obsolete and
## scheduled for removal from Octave.
## By default, the @code{Octave:deprecated-keyword} warning is enabled.
##
## @item Octave:divide-by-zero
## If the @code{Octave:divide-by-zero} warning is enabled, a
## warning is issued when Octave encounters a division by zero.
## By default, the @code{Octave:divide-by-zero} warning is enabled.
##
## @item Octave:fopen-file-in-path
## By default, the @code{Octave:fopen-file-in-path} warning is enabled.
##
## @item Octave:function-name-clash
## If the @code{Octave:function-name-clash} warning is enabled, a
## warning is issued when Octave finds that the name of a function
## defined in a function file differs from the name of the file.  (If
## the names disagree, the name declared inside the file is ignored.)
## By default, the @code{Octave:function-name-clash} warning is enabled.
##
## @item Octave:future-time-stamp
## If the @code{Octave:future-time-stamp} warning is enabled, Octave
## will print a warning if it finds a function file with a time stamp
## that is in the future.
## By default, the @code{Octave:future-time-stamp} warning is enabled.
##
## @item Octave:glyph-render
## By default, the @code{Octave:glyph-render} warning is enabled.
##
## @item Octave:imag-to-real
## If the @code{Octave:imag-to-real} warning is enabled, a warning is
## printed for implicit conversions of complex numbers to real numbers.
## By default, the @code{Octave:imag-to-real} warning is disabled.
##
## @item Octave:language-extension
## Print warnings when using features that are unique to the Octave
## language and that may still be missing in @sc{matlab}.
## By default, the @code{Octave:language-extension} warning is disabled.
## The @option{--traditional} or @option{--braindead} startup options for
## Octave may also be of use, @pxref{Command Line Options}.
##
## @item Octave:load-file-in-path
## By default, the @code{Octave:load-file-in-path} warning is enabled.
##
## @item Octave:logical-conversion
## By default, the @code{Octave:logical-conversion} warning is enabled.
##
## @item Octave:md5sum-file-in-path
## By default, the @code{Octave:md5sum-file-in-path} warning is enabled.
##
## @item Octave:missing-glyph
## By default, the @code{Octave:missing-glyph} warning is enabled.
##
## @item Octave:missing-semicolon
## If the @code{Octave:missing-semicolon} warning is enabled, Octave
## will warn when statements in function definitions don't end in
## semicolons.
## By default the @code{Octave:missing-semicolon} warning is disabled.
##
## @item Octave:mixed-string-concat
## If the @code{Octave:mixed-string-concat} warning is enabled, print a
## warning when concatenating a mixture of double and single quoted strings.
## By default, the @code{Octave:mixed-string-concat} warning is disabled.
##
## @item Octave:neg-dim-as-zero
## If the @code{Octave:neg-dim-as-zero} warning is enabled, print a warning
## for expressions like
##
## @example
## eye (-1)
## @end example
##
## @noindent
## By default, the @code{Octave:neg-dim-as-zero} warning is disabled.
##
## @item Octave:nested-functions-coerced
## By default, the @code{Octave:nested-functions-coerced} warning is enabled.
##
## @item Octave:noninteger-range-as-index
## By default, the @code{Octave:noninteger-range-as-index} warning is enabled.
##
## @item Octave:num-to-str
## If the @code{Octave:num-to-str} warning is enable, a warning is
## printed for implicit conversions of numbers to their ASCII character
## equivalents when strings are constructed using a mixture of strings and
## numbers in matrix notation.  For example,
##
## @example
## @group
## [ "f", 111, 111 ]
## @result{} "foo"
## @end group
## @end example
##
## @noindent
## elicits a warning if the @code{Octave:num-to-str} warning is
## enabled.  By default, the @code{Octave:num-to-str} warning is enabled.
##
## @item Octave:possible-matlab-short-circuit-operator
## If the @code{Octave:possible-matlab-short-circuit-operator} warning
## is enabled, Octave will warn about using the not short circuiting
## operators @code{&} and @code{|} inside @code{if} or @code{while}
## conditions.  They normally never short circuit, but @sc{matlab} always
## short circuits if any logical operators are used in a condition.  You
## can turn on the option
##
## @example
## @group
## do_braindead_shortcircuit_evaluation (1)
## @end group
## @end example
##
## @noindent
## if you would like to enable this short-circuit evaluation in
## Octave.  Note that the @code{&&} and @code{||} operators always short
## circuit in both Octave and @sc{matlab}, so it's only necessary to
## enable @sc{matlab}-style short-circuiting if it's too arduous to modify
## existing code that relies on this behavior.
## By default, the @code{Octave:possible-matlab-short-circuit-operator} warning
## is enabled.
##
## @item Octave:precedence-change
## If the @code{Octave:precedence-change} warning is enabled, Octave
## will warn about possible changes in the meaning of some code due to
## changes in precedence for some operators.  Precedence changes have
## typically been made for @sc{matlab} compatibility.
## By default, the @code{Octave:precedence-change} warning is enabled.
##
## @item Octave:recursive-path-search
## By default, the @code{Octave:recursive-path-search} warning is enabled.
##
## @item Octave:remove-init-dir
## The @code{path} function changes the search path that Octave uses
## to find functions.  It is possible to set the path to a value which
## excludes Octave's own built-in functions.  If the
## @code{Octave:remove-init-dir} warning is enabled then Octave will warn
## when the @code{path} function has been used in a way that may render
## Octave unworkable.
## By default, the @code{Octave:remove-init-dir} warning is enabled.
##
## @item Octave:reload-forces-clear
## If several functions have been loaded from the same file, Octave must
## clear all the functions before any one of them can be reloaded.  If
## the @code{Octave:reload-forces-clear} warning is enabled, Octave will
## warn you when this happens, and print a list of the additional
## functions that it is forced to clear.
## By default, the @code{Octave:reload-forces-clear} warning is enabled.
##
## @item Octave:resize-on-range-error
## If the @code{Octave:resize-on-range-error} warning is enabled, print a
## warning when a matrix is resized by an indexed assignment with
## indices outside the current bounds.
## By default, the ## @code{Octave:resize-on-range-error} warning is disabled.
##
## @item Octave:separator-insert
## Print warning if commas or semicolons might be inserted
## automatically in literal matrices.
## By default, the @code{Octave:separator-insert} warning is disabled.
##
## @item Octave:shadowed-function
## By default, the @code{Octave:shadowed-function} warning is enabled.
##
## @item Octave:single-quote-string
## Print warning if a single quote character is used to introduce a
## string constant.
## By default, the @code{Octave:single-quote-string} warning is disabled.
##
## @item  Octave:nearly-singular-matrix
## @itemx Octave:singular-matrix
## By default, the @code{Octave:nearly-singular-matrix} and
## @code{Octave:singular-matrix} warnings are enabled.
##
## @item Octave:sqrtm:SingularMatrix
## By default, the @code{Octave:sqrtm:SingularMatrix} warning is enabled.
##
## @item Octave:str-to-num
## If the @code{Octave:str-to-num} warning is enabled, a warning is printed
## for implicit conversions of strings to their numeric ASCII equivalents.
## For example,
##
## @example
## @group
## "abc" + 0
## @result{} 97 98 99
## @end group
## @end example
##
## @noindent
## elicits a warning if the @code{Octave:str-to-num} warning is enabled.
## By default, the @code{Octave:str-to-num} warning is disabled.
##
## @item Octave:undefined-return-values
## If the @code{Octave:undefined-return-values} warning is disabled,
## print a warning if a function does not define all the values in
## the return list which are expected.
## By default, the @code{Octave:undefined-return-values} warning is enabled.
##
## @item Octave:variable-switch-label
## If the @code{Octave:variable-switch-label} warning is enabled, Octave
## will print a warning if a switch label is not a constant or constant
## expression.
## By default, the @code{Octave:variable-switch-label} warning is disabled.
## @end table
##

function warning_ids ()
  help ("warning_ids");
endfunction


## Mark file as being tested.  No real test needed for a documentation .m file
%!assert (1)

