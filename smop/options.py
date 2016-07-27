import sys
import argparse,textwrap
from textwrap import dedent
from version import __version__

parser = argparse.ArgumentParser(
    "smop",
    usage="""python main.py [options][file.m ...file.m]""",
    description= """SMOP is Small Matlab and Octave to Python compiler""",
    epilog=
    """
    $ python [-mpdb] lexer.py fastsolver.m [options]
    $ python [-mpdb] main.py fastsolver.m [options]
    emacs (pdb): pdb main.py fastsolver.m [options]
    """,
    formatter_class=argparse.RawTextHelpFormatter,)


#parser.add_argument("--callgraph", metavar="F", type=str) 

parser.add_argument("--enumerate", action="store_true",
help="""enumerate statements (see  --debug-index)""")

parser.add_argument("--debug-index", type=int,
help="""enter pdb when stmt index equals  --debug-index""")

parser.add_argument("--debug-lexer", action="store_true",
help="enable built-in debugging tools")

parser.add_argument("--debug-parser", action="store_true",
help="enable built-in debugging tools")

parser.add_argument("filelist", nargs="*", metavar="file.m", type=str)

#parser.add_argument("--graphviz", action="store_true")

parser.add_argument("--keep-comments", action="store_true",
help="""preserve multiline comments""")

parser.add_argument("--line-numbers", action="store_true",
help="""include line-numbering information """)

parser.add_argument("-o", "--output", metavar="file.py", type=str,
help= """default output file name is 'a.py'""") 

parser.add_argument("--no-backend", action="store_true",
help="omit code generation")

parser.add_argument("--no-resolve", action="store_true",
help="omit name resolution")

parser.add_argument("-s", "--strict", action="store_true",
help="stop after first syntax error")

parser.add_argument("--testing-mode",action="store_true",
help= """support special "testing" percent-bang
comments used to write Octave test suite.  When
disabled, behave like regular comments.""")

parser.add_argument("-V", '--version', action='version',
                    version=__version__)

parser.add_argument("-v", "--verbose",action="store_true")

parser.add_argument("-x", "--exclude", metavar="f,g,h", type=str,
help="""comma-separated list of files to ignore""")

args = parser.parse_args(namespace=sys.modules[__name__])

xfiles = args.exclude.split(",") if args.exclude else []
filename = ""

def foo():
    """
    >>> args = parser.parse_args("a b c".split())
    >>> print args.filelist
    ['a', 'b', 'c'

    >>> args = parser.parse_args(["--keep-comments"])
    >>> print args.keep_comments
    True
    
    """
if __name__ == "__main__":
    import doctest
    doctest.testmod()
