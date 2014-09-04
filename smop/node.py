# SMOP compiler -- Simple Matlab/Octave to Python compiler
# Copyright 2011-2013 Victor Leikehman

from collections import namedtuple
from recipes import recordtype
import copy,sys,inspect
import options

# def preorder(u):
#     if isinstance(u,traversable):
#         yield u
#         for n in u:
#             for t in preorder(n):
#                 yield t

def decode(self):
    r = ""
    s = self.name
    while s:
        if len(s) >= 2 and s[0] == "_":
            r += s[1].upper()
            s = s[2:]
        else:
            r += s[0].lower()
            s = s[1:]
    return r

def encode(s):
    return "".join(c+"_" if c.isupper() or c=="_" else c.upper() for c in s)

def postorder(u):
    if isinstance(u,node):
        for v in u:
            for t in postorder(v):
                yield t
        yield u # returns only traversible objects

def extend(cls):
    return lambda f: (setattr(cls,f.__name__,f) or f)

def exceptions(f):
    def wrapper(self,*args,**kwargs):
        try:
            return f(self,*args,**kwargs)
        except:
            print "%s.%s()" % (self.__class__.__name__, f.__name__)
            raise
    wrapper.__name__ = f.__name__
    return wrapper

class node(object):
    def become(self,other):
        class Wrapper(self.__class__):
            def __copy__(self):
                other = object.__getattribute__(self,"other")
                return copy.copy(other)
            def __getattribute__(self,name):
                other = object.__getattribute__(self,"other")
                return getattr(other,name)
            def __setattr__(self,name,value):
                other = object.__getattribute__(self,"other")
                return setattr(other,name,value)
            def __iter__(self):
                other = object.__getattribute__(self,"other")
                return iter(other)
            #def __hash__(self):
            #    other = object.__getattribute__(self,"other")
            #    return id(other)
            def __repr__(self):
                other = object.__getattribute__(self,"other")
                return repr(other)
            def __len__(self):
                other = object.__getattribute__(self,"other")
                return len(other)
        assert self != other
        self.other = other
        self.__class__ = Wrapper

    def _type(self):
        raise AttributeError("_type")


######### LISTS

class concat_list(node,list):
    pass

class global_list(node,list):
    """space-separated list of variables used in GLOBAL statement"""
    pass

class expr_list(node,list):
    def __str__(self):
        return ",".join([str(t) for t in self])
    def __repr__(self):
        return "expr_list(%s)" % list.__repr__(self)

class stmt_list(node,list):
    def __str__(self):
        return "\n".join([str(t) for t in self])
    def __repr__(self):
        return "stmt_list(%s)" % list.__repr__(self)

#####################
#
#  ATOMS

class atom(node): pass

class string(atom,recordtype("string", "value lineno lexpos", default=None)):
    def __str__(self):
        return "'%s'" % self.value

class logical(atom,recordtype("logical", "value lineno lexpos", default=None)):
    pass

class number(atom,recordtype("number","value lineno lexpos",default=None)):
    def __str__(self):
        return str(self.value)

class ident(atom,recordtype("ident","name lineno column lexpos defs props",
        default=None)):
    def __str__(self):
        return self.name

class param(ident):
    pass

###########################
#
#  STATEMENTS
#

class stmt(node): pass

# class call_stmt(stmt,recordtype("call_stmt","func_expr args ret")):
#     """Sometimes called multiple assignment, call statements represent
#     something like [x,y]=foo(a,b,c); Note the square brackets around
#     the lhs.
#     SEE ALSO: funcall,let
#     """
#     def __str__(self):
#         return "%s=%s(%s)" % (str(self.ret),
#                               str(self.func_expr),
#                               str(self.args))

class let(stmt,recordtype("let",
                          "ret args lineno lexpos nargout",
                          default=None)):
    """Assignment statement, except [x,y]=foo(x,y,z),
    which is handled by call_stmt."""
    def __str__(self):
        return "%s=%s" % (str(self.ret), str(self.args))
    
class func_decl(stmt,recordtype("func_decl","ident ret args decl_list use_nargin",default=None)):
    pass

class lambda_expr(func_decl):
    pass

class function(stmt,recordtype("function","head body")):
    pass

class for_stmt(stmt,recordtype("for_stmt","ident expr stmt_list")):
    pass

class DO_STMT(stmt,recordtype("DO_STMT","ident start stop stmt_list")):
    pass

# We generate where_stmt to implement A(B==C) = D
class where_stmt(stmt,recordtype("where_stmt","cond_expr stmt_list")):
    pass

class if_stmt(stmt,recordtype("if_stmt","cond_expr then_stmt else_stmt")):
    pass

class global_stmt(stmt,recordtype("global_stmt","global_list")):
    def __str__(self):
        return "global %s" % str(self.global_list)

class return_stmt(stmt,namedtuple("return_stmt","ret")):
    def __str__(self):
        return "return"

class end_stmt(stmt,namedtuple("end_stmt","dummy")):
    def __str__(self):
        return "end"

class continue_stmt(stmt,namedtuple("continue_stmt","dummy")):
    def __str__(self):
        return "continue"

class break_stmt(stmt,namedtuple("break_stmt","dummy")):
    def __str__(self):
        return "break"

class expr_stmt(stmt,node,recordtype("expr_stmt","expr")):
    def __str__(self):
        return str(self.expr)

class while_stmt(stmt,node,recordtype("while_stmt","cond_expr stmt_list")):
    pass

class try_catch(stmt,recordtype("try_catch","try_stmt catch_stmt finally_stmt")):
    pass

class allocate_stmt(stmt,recordtype("allocate_stmt",
                                    "ident args")):
    pass

#######################################333
#
# FUNCALL

class funcall(node,recordtype("funcall","func_expr args nargout",default=None)):
    """Funcall instances represent 
    (a) Array references, both lhs and rhs
    (b) Function call expressions
    """
    def __str__(self):
        return "%s(%s)" % (str(self.func_expr),
                           str(self.args))

class builtins(funcall):
    """
    Builtin functions are represented as subclasses
    of class builtins.  Application of a function to
    specific arguments is represented as its instance.
    For example, builtin function foo is represented
    as class foo(builtins), and foo(x) is represented
    as foo(x).
    """

    def __init__(self,*args,**kwargs):
        """
        If a built-in function _foo takes three arguments
        a, b, and c, we can just say _foo(a,b,c) and let
        the constructor take care of proper structuring of
        the node (like expr_list around the arguments, etc.
        """
        funcall.__init__(self,
             func_expr=None,
             args=expr_list(args),
             **kwargs)
        #import pdb; pdb.set_trace()
        #self.func_expr.defs = set(self.func_expr)

    def __repr__(self):
        return "np.%s%s" % (self.__class__,repr(self.args))

    def __str__(self):
        return "np.%s(%s)" % (self.__class__.__name__,
                              str(self.args))

class arrayref(funcall):
    def __repr__(self):
        return "%s%s[%s]" % (self.__class__, 
                             self.func_expr,
                             self.args)


########################## EXPR

class expr(node,recordtype("expr","op args")):
    def __str__(self):
        if self.op == ".":
            return "%s%s" % (str(self.args[0]),self.args[1])
        if self.op == "parens":
            return "(%s)" % str(self.args[0])
        if not self.args:
            return str(self.op)
        if len(self.args) == 1:
            return "%s%s" % (self.op,self.args[0])
        if len(self.args) == 2:
            return "%s%s%s" % (self.args[0]._backend(),
                               self.op,
                               self.args[1]._backend())
        ret = "%s=" % str(self.ret) if self.ret else ""
        return ret+"%s(%s)" % (self.op,
                               ",".join([str(t) for t in self.args]))

# names in caps correspond to fortran funcs
builtins_list = [
    "ABS",
    "ALL",
    "ANY",
    "CEILING",
    "FIND",
    "ISNAN",
    "MAXVAL",
    "MINVAL",
    "MODULO",
    "RAND",
    "RESHAPE",
    "SHAPE",
    "SIGN",
    "SIZE",
    "SUM",

    #"abs",
    "add", # synthetic opcode
    #"all",
    #"any",
    "cellfun",
    #"ceil",
    "clazz",
    #"cumprod",
    #"cumsum",
    #"diff",
    "dot",      # Exists in numpy. Implements matlab .*
    #"exist",
    "false",
    #"fclose",
    #"find",
    #"findone",  # same as find, but returns ONE result
    #"floor",
    #"fopen",
    "getfield",
    "inf", "inf0",
    #"isempty",
    #"isequal",
    "isinf",
    "isnan",
    #"length",
    #"load",
    #"lower",
    #"max",
    #"min",
    #"mod",
    #"nnz",
    #"numel",
    #"ones",
    #"rand",
    #"range_",   # synthetic opcode
    "ravel",   # synthetic opcode
    #"rem",
    #"save",
    "setfield",
    #"sign",
    #"size",
    #"sort",
    #"strcmp",
    #"strcmpi",
    "sub", # synthetic opcode for subtract
    #"sum",
    "transpose",
    #"true",
    #"zeros",
]

for name in builtins_list:
    globals()[name] = type(name, (builtins,), {})

#class cellarrayref(node,recordtype("cellarrayref","ident args")):
class cellarrayref(funcall):
    pass

class cellarray(expr):
    pass

class matrix(builtins):
    """
    Anything enclosed in square brackets counts as matrix
    >>> print matrix([1,2,3])
    [1,2,3]
    >>> print matrix()
    []
    """
#    def __init__(self,args=expr_list()):
#        expr.__init__(self,op="[]",args=args)
#    def __str__(self):
#        return "[%s]" % ",".join([str(t) for t in self.args])

@extend(node)
def is_const(self):
    return False
@extend(number)
@extend(string)
def is_const(self):
    return True
@extend(expr_list)
def is_const(self):
    return all(t.is_const() for t in self)
@extend(matrix)
def is_const(self):
    return not self.args or self.args[0].is_const()
# vim: ts=8:sw=4:et
