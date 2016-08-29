import sys
import argparse,textwrap
from textwrap import dedent
from version import __version__

parser = argparse.ArgumentParser(
    "smop",
    usage="""

    smop [options][file.m ...file.m][-l file.py...]
             or
    smop [options] library.tar [-l file.py...]
""",
    description= """
SMOP is Small Matlab and Octave to Python compiler.

Matlab files are translated to Python.  The name of the
resulting file is derived from the name of the first
compiled file unless explicitly set with -o .  Additional
libraries can be specified with -l (lowercase L), for
example -loctave.py.""",
epilog="""Hint: put into your Makefile the following rule

%.py: %.m
	$(SMOP) $^ $(FLAGS)
	($(PYTHON) $@ && cat $@ >> libscripts.py)""",
    formatter_class=argparse.RawTextHelpFormatter,
    )


parser.add_argument("-o", "--output", metavar="file.py", type=str, help=
"""Write the results to file.py.  Use -o- to send the results
to the standard output.  If not specified explicitly, output
file name is derived from the leftmost file name.  For example,

        smop filex.m filey.m filez.m

        generates file filex.py""") 

parser.add_argument("-l", "--link", metavar="file.py",help=
"""Import file.py . File core.py is always imported. For
example,

        smop test_primes.m -l octave.py

Option -l can be specified several times.
""")

parser.add_argument("-s", "--strict", action="store_true",
help="""stop after first syntax error (by default compiles
other .m files)""")

parser.add_argument("-V", '--version', action='version',
                    version=__version__)

parser.add_argument("-v", "--verbose",action="store_true")

parser.add_argument("-x", "--exclude",
                    metavar="filex.m,filey.m,filez.m",
                    type=str,
help="""comma-separated list of files to ignore""")

parser.add_argument("-D", "--debug", type=str,
help="""Colon-separated codes.
1 After parsing
""")

parser.add_argument("-L", "--debug-lexer", action="store_true",
help="enable built-in debugging tools-")

parser.add_argument("-P", "--debug-parser", action="store_true",
help="enable built-in debugging tools")

parser.add_argument("filelist", nargs="*", metavar="file.m", type=str)

#parser.add_argument("--graphviz", action="store_true")

parser.add_argument("-C","--no-comments", action="store_true",
help="""discard multiline comments""")

parser.add_argument("-N","--no-numbers", action="store_true",
help="""discard line-numbering information""")

parser.add_argument("-B","--no-backend", action="store_true",
help="omit code generation")

parser.add_argument("-R","--no-resolve", action="store_true",
help="omit name resolution")

parser.add_argument("-S","--strings", default="C",
help="""C for Octave style, F for Matlab style""")

parser.add_argument("-T","--testing-mode",action="store_true",
help= """support special "testing" percent-bang comments used to
write Octave test suite.  When disabled, behaves like
regular comments.""")

parser.add_argument("-I", "--ignore", type=int)


args = parser.parse_args(namespace=sys.modules[__name__])

xfiles = args.exclude.split(",") if args.exclude else []
debug = args.debug.split(":") if args.debug else []
filename = ""

def foo():
    """
    >>> args = parser.parse_args("a b c".split())
    >>> print args.filelist
    ['a', 'b', 'c'

    """
if __name__ == "__main__":
    import doctest
    doctest.testmod()
