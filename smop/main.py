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

sys.argv.append('temp/reaction_rate.m')
sys.argv.append('-o')
sys.argv.append('temp/reaction_rate.py')
#sys.argv.append('-H')
#sys.argv.append('-P')

import options
import parse
import resolve
import backend
import version
import node

def print_header(fp,s):
    if options.no_header:
        return
    if 'm.' in s:
        print("import math as m", file=fp)
    if 'np.' in s:
        print("import numpy as np", file=fp)
    if 're.' in s:
        print("import re", file=fp)
    if 'smop_util.' in s:
        print("import smop_util", file=fp)
    if 'plt.' in s:
        print("import matplotlib.pyplot as plt", file=fp)
    
def print_list(l):
    print(l)
    print(type(l))
    try:
        if type(l) != str:
            for i in l:
                print_list(i)
    except:
        pass
    finally:
        print("End of "+str(type(l)))
        pass

def resolve_array_refs(l,graph_list):
    try:
        if type(l) == node.funcall:
            for elem in graph_list:
                if str(l.func_expr) == elem[0] and "F" != elem[3]:
                    l.__class__ = node.arrayref
                    break
        if type(l) != str and type(l) != node.ident:
            for i in l:
                resolve_array_refs(i,graph_list)
    except Exception as e:
        pass

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
            #FIXME buf = buf.decode("ascii", errors="ignore")
            stmt_list = parse.parse(buf if buf[-1] == '\n' else buf + '\n')

            if not stmt_list:
                continue
            if not options.no_resolve:
                G = resolve.resolve(stmt_list)
                graph_list = []
                for n in G.nodes():
                    temp = str.split(n,'_')
                    while len(temp) > 3:
                        temp[0] += '_' + temp.pop(1)
                    graph_list.append(temp+[G.node[n]["ident"].props])
                resolve_array_refs(stmt_list,graph_list)
            #print_list(stmt_list)
            if not options.no_backend:
                s = backend.backend(stmt_list).strip()
            if not options.output:
                f = splitext(basename(options.filename))[0] + ".py"
                with open(f, "w") as fp:
                    print_header(fp,s)
                    fp.write(s)
            else:
                print_header(fp,s)
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

main()
