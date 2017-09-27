# SMOP -- Simple Matlab/Octave to Python compiler
# Copyright 2011-2016 Victor Leikehman

import py_compile
import tempfile
import fnmatch
import tarfile
import sys
import os
import traceback
import getpass
from os.path import basename, splitext
from datetime import datetime

import lexer
import options
import parse
import resolve
import backend
import version


def print_header(fp):
    if options.no_header:
        return
    print >> fp, "# -*- coding: utf-8 -*-"
    if options.filename == "":
        print >> fp, "\"\"\"@package " + options.output
    else:
        print >> fp, "\"\"\"@package " + options.filename
    print >> fp, "@date Created on " + str(datetime.now())
    print >> fp, "@author " + getpass.getuser()
    print >> fp, '"""'


def main():
    tar = None
    if "M" in options.debug:
        import pdb
        pdb.set_trace()
    if not options.filelist:
        if options.archive:
            tar = tarfile.open(options.archive)
            options.filelist = tar.getnames()
        elif options.code:
            tmp = tempfile.NamedTemporaryFile(suffix=".m")
            tmp.file.write(options.code)
            tmp.file.flush()
            options.filelist = [tmp.name]
            if options.output:
                print "Conflicting options -c and -o"
                return
            options.output = "-"
        else:
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
    if options.glob_pattern:
        options.filelist = fnmatch.filter(options.filelist,
                                          options.glob_pattern)
    nerrors = 0
    for i, options.filename in enumerate(options.filelist):
        try:
            if options.verbose:
                print i, options.filename
            if not options.filename.endswith(".m"):
                if options.verbose:
                    print("\tIgnored: '%s' (unexpected file type)" %
                          options.filename)
                continue
            if basename(options.filename) in options.xfiles:
                if options.verbose:
                    print "\tExcluded: '%s'" % options.filename
                continue
            if tar:
                buf = tar.extractfile(options.filename).read()
            else:
                buf = open(options.filename).read()
            buf = buf.replace("\r\n", "\n")
            buf = buf.decode("ascii", errors="ignore")
            stmt_list = parse.parse(buf if buf[-1] == '\n' else buf + '\n')
            #assert None not in stmt_list
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
                try:
                    py_compile.compile(f,doraise=True)
                    if options.execfile:
                        execfile(f)
                except:
                    if options.delete-on-error:
                        os.unlink(f)
                        if options.verbose:
                            print "Removed",f
                    raise
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
            print "Errors:", nerrors

if __name__ == "__main__":
    main()
