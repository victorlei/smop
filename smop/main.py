# SMOP -- Simple Matlab/Octave to Python compiler
# Copyright 2011-2016 Victor Leikehman

from __future__ import print_function

import py_compile
import tempfile
import fnmatch
import tarfile
import sys
import os
import traceback
from os.path import basename, splitext

from . import options
from . import parse
from . import resolve
from . import backend
from . import version

def print_header(fp):
    if options.no_header:
        return
    #print("# Running Python %s" % sys.version, file=fp)
    print("# Generated with SMOP ", version.__version__, file=fp)
    print("from libsmop import *", file=fp)
    print("#", options.filename, file=fp)

def main():
    if "M" in options.debug:
        import pdb
        pdb.set_trace()
    if not options.filelist:
        options.parser.print_help()
        return
    if options.output == "-":
        fp = sys.stdout
    elif options.output:
        fp = open(options.output, "w")
    else:
        fp = None
    if fp:
        print_header(fp)

    nerrors = 0
    for i, options.filename in enumerate(options.filelist):
        try:
            if options.verbose:
                print(i, options.filename)
            if not options.filename.endswith(".m"):
                print("\tIgnored: '%s' (unexpected file type)" %
                      options.filename)
                continue
            if basename(options.filename) in options.xfiles:
                if options.verbose:
                    print("\tExcluded: '%s'" % options.filename)
                continue
            buf = open(options.filename).read()
            buf = buf.replace("\r\n", "\n")
            # FIXME buf = buf.decode("ascii", errors="ignore")
            stmt_list = parse.parse(buf if buf[-1] == '\n' else buf + '\n')

            if not stmt_list:
                continue
            if not options.no_resolve:
                G = resolve.resolve(stmt_list)
            if not options.no_backend:
                s = backend.backend(stmt_list)
            if not options.output:
                f = splitext(basename(options.filename))[0] + ".py"
                with open(f, "w") as fp:
                    print_header(fp)
                    fp.write(s)
            else:
                fp.write(s)
        except KeyboardInterrupt:
            break
        except:
            nerrors += 1
            traceback.print_exc(file=sys.stdout)
            if options.strict:
                break
        finally:
            pass
    if nerrors:
        print("Errors:", nerrors)
