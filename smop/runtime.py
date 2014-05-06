# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license

import numpy
import __builtin__

def floor(t):
    return numpy.floor(t)

def length(t):
    try:
        return __builtin__.max(t.shape)
    except:
        return 1

def round(t):
    return numpy.round(t)
