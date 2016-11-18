import sys
import argparse,textwrap
from textwrap import dedent
from version import __version__

parser = argparse.ArgumentParser(
    "smop",
    usage="""

    smop [options] [file.m ...file.m]

""",
    description= """
SMOP is Small Matlab and Octave to Python compiler.

SMOP takes MATLAB files and translates them to Python.  The
name of the resulting file is derived from the name of the
source m-file unless explicitly set with -o .""",

epilog="""
Example:
    $ wget ftp://ftp.gnu.org/ftp/octave/octave-4.0.2.tar.gz
    $ smop -a octave-4.0.2.tar.gz -g '*/scripts/*.m'
    $ ls -1 *.py | wc
    $ python -m py_compile *.py
    $ ls -1 *.pyc | wc
""",
    formatter_class=argparse.RawTextHelpFormatter,
    )


parser.add_argument("-a", "--archive",
                    metavar="archive.tar",
help="""Read .m files from the archive.
Accepted formats: tar either uncompressed,
or compressed using gzip or bz2.""")


parser.add_argument("-g", "--glob-pattern",
                    metavar="PATTERN",
                    type=str,
help="""Apply unix glob pattern to the input
file list or to the archived files. For
example -g 'octave-4.0.2/*.m'

Quoted from fnmatch docs:

Note that the filename separator ('/' on
Unix) is not special to this
module. [...]  Similarly, filenames
starting with a period are not special
for this module, and are matched by the
* and ?  patterns.  """)

parser.add_argument("-o", "--output",
                    metavar="file.py",
                    type=str,
help="""Write the results to file.py.  Use
-o- to send the results to the standard
output.  If not specified explicitly,
output file names are  derived from
input file names by replacing ".m" with
".py".  For example,

    $ smop filex.m filey.m filez.m

generates files filex.py filey.py and filez.py""") 

# parser.add_argument("-l", "--link",
#                     metavar="file.py",
# help="""Import file.py . File core.py is
# always imported. For example,
                    
#    smop test_primes.m -l octave.py
                    
# Option -l can be specified several times.""")

# parser.add_argument("-s", "--strict",
#                     action="store_true",
# help="""stop after first syntax error (by
# default compiles other .m files)""")

parser.add_argument("-V", '--version',
                    action='version',
                    version=__version__)

parser.add_argument("-v", "--verbose",
                    action="store_true")

parser.add_argument("-x", "--exclude",
                    metavar="filex.m,filey.m,filez.m",
                    type=str,
help="""comma-separated list of files to ignore""")

parser.add_argument("-D", "--debug", type=str,
help="""Colon-separated codes.
a Main
b Lex
c Parse
1 After parsing
""")

parser.add_argument("-L", "--debug-lexer",
                    action="store_true",
help="enable built-in debugging tools-")

parser.add_argument("-P", "--debug-parser",
                    action="store_true",
help="enable built-in debugging tools")

parser.add_argument("filelist", nargs="*",
                    metavar="file.m", type=str)

#parser.add_argument("--graphviz", action="store_true")

parser.add_argument("-C","--no-comments",
                    action="store_true",
help="""discard multiline comments""")

parser.add_argument("-N", "--no-numbers",
                    action="store_true",
help="""discard line-numbering information""")

parser.add_argument("-B","--no-backend",
                    action="store_true",
help="omit code generation")

parser.add_argument("-R","--no-resolve",
                    action="store_true",
help="omit name resolution")

#parser.add_argument("-S","--strings", default="C",
#help="""C for Octave style, F for Matlab style""")

parser.add_argument("-T","--testing-mode",
                    action="store_true",
help= """support special "testing"
percent-bang comments used to write
Octave test suite.  When disabled,
behaves like regular comments.""")

# parser.add_argument("-E", "--ignore-errors",
#                     type=int,
#                     metavar="N",
#                     dest="ignore_errors",
#                     action="store",
# help="""Ignore first N exceptions.
# Other useful values are
# zero -- meaning "don't ignore errors"
# minus one --  meaning "ignore all errors" """)

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
