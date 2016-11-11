# smop -- Simple Matlab to Python compiler
# Copyright 2011-2016 Victor Leikehman

"""
Calling conventions:

call site:  nargout=N is passed if and only if N > 1
func decl:  nargout=1 must be declared if function may return
            more than one return value.  Otherwise optional.
return value:  return (x,y,z)[:nargout] or return x
"""

import logging
logger = logging.getLogger(__name__)
import node,options
from node import extend,exceptions

indent = " "*4

optable = {
    "!" : "not",
    "~" : "not",
    "~=": "!=",
    "|" : "or",
    "&" : "and",
    "||": "or",
    "&&": "and",
    "^" : "**",
    "**": "**",
    ".^": "**",
    "./": "/",
    ".*": "*",
    ".*=" : "*",
    "./=" : "/",
    }

def backend(t,*args,**kwargs):
    return t._backend(level=1,*args,**kwargs)


# Sometimes user's variable names in the matlab code collide with Python
# reserved words and constants.  We handle this in the backend rather than in
# the lexer, to keep the target language separate from the lexer code.

# Some names, such as matlabarray, may collide with the user defined names.
# Both cases are solved by appending a trailing underscore to the user's names.

reserved = set(
    """
    and    assert  break class continue
    def    del     elif  else  except
    exec   finally for   from  global
    if     import  in    is    lambda
    not    or      pass  print raise
    return try     while with

    Data  Float Int   Numeric Oxphys
    array close float int     input
    open  range type  write

    len
    """.split())

    #acos  asin atan  cos e
    #exp   fabs floor log log10
    #pi    sin  sqrt  tan
    

@extend(node.add)
def _backend(self,level=0):
    if (self.args[0].__class__ is node.number and
        self.args[1].__class__ is node.number):
        return node.number(self.args[0].value +
                           self.args[1].value)._backend()
    else:
        return "(%s+%s)" % (self.args[0]._backend(),
                            self.args[1]._backend())

@extend(node.arrayref)
def _backend(self,level=0):
    fmt = "%s[%s]"
    return fmt % (self.func_expr._backend(),
                       self.args._backend())

@extend(node.break_stmt)
def _backend(self,level=0):
    return "break"

@extend(node.builtins)
def _backend(self,level=0):
    #if not self.ret:
        return "%s(%s)" % (self.__class__.__name__,
                           self.args._backend())

@extend(node.cellarray)
def _backend(self,level=0):
    return "cellarray([%s])" % self.args._backend()

@extend(node.cellarrayref)
def _backend(self,level=0):
    return "%s[%s]" % (self.func_expr._backend(),
                       self.args._backend())

@extend(node.comment_stmt)
def _backend(self,level=0):
    s = self.value.strip() 
    if not s:
        return ""
    if s[0] in "%#":
        return s.replace("%","#")
    return self.value

@extend(node.concat_list)
def _backend(self,level=0):
    #import pdb; pdb.set_trace()
    return ",".join(["[%s]"%t._backend() for t in self])

@extend(node.continue_stmt)
def _backend(self,level=0):
    return "continue"

@extend(node.expr)
def _backend(self,level=0):
    if self.op in ("!","~"): 
       return "logical_not(%s)" % self.args[0]._backend()

    if self.op == "&":
       return "logical_and(%s)" % self.args._backend()

    if self.op == "&&":
        return "%s and %s" % (self.args[0]._backend(),
                              self.args[1]._backend())
    if self.op == "|":
        return "logical_or(%s)" % self.args._backend()

    if self.op == "||":
        return "%s or %s" % (self.args[0]._backend(),
                             self.args[1]._backend())

    if self.op == '@': # FIXMEj
        return self.args[0]._backend()

    if self.op == "\\":
        return "numpy.linalg.solve(%s,%s)" % (self.args[0]._backend(),
                                              self.args[1]._backend())
    if self.op == "::":
        if not self.args:
            return ":"
        elif len(self.args) == 2:
            return "%s:%s" % (self.args[0]._backend(),
                              self.args[1]._backend())
        elif len(self.args) == 3:
            return "%s:%s:%s" % (self.args[0]._backend(),
                                 self.args[2]._backend(),
                                 self.args[1]._backend())
    if self.op == ":":
        return "arange(%s)" % self.args._backend()
    
    if self.op == "end":
#        if self.args:
#            return "%s.shape[%s]" % (self.args[0]._backend(),
#                                     self.args[1]._backend())
#        else:
            return "end()"

    if self.op == ".":
        #import pdb; pdb.set_trace()
        try:
            is_parens = self.args[1].op == "parens"
        except:
            is_parens = False
        if not is_parens:
            return "%s%s" % (self.args[0]._backend(),
                             self.args[1]._backend())
        else:
            return "getattr(%s,%s)" % (self.args[0]._backend(),
                                       self.args[1]._backend())

#     if self.op == "matrix":
#         return "[%s]" % ",".join([t._backend() for t in self.args])
    if self.op == "parens":
        return "(%s)" % self.args[0]._backend()
#    if self.op == "[]":
#        return "[%s]" % self.args._backend()
    if not self.args:
        return self.op
    if len(self.args) == 1:
        return "%s %s" % (optable.get(self.op,self.op),
                         self.args[0]._backend())
    if len(self.args) == 2:
        return "%s %s %s" % (self.args[0]._backend(),
                           optable.get(self.op,self.op),
                           self.args[1]._backend())
    #import pdb;pdb.set_trace()
    ret = "%s=" % self.ret._backend() if self.ret else ""
    return ret+"%s(%s)" % (self.op,
                           ",".join([t._backend() for t in self.args]))


@extend(node.expr_list)
def _backend(self,level=0):
    return ",".join([t._backend() for t in self])

@extend(node.expr_stmt)
def _backend(self,level=0):
    return self.expr._backend()

@extend(node.for_stmt)
def _backend(self,level=0):
    fmt = "for %s in %s.reshape(-1):%s"
    return fmt % (self.ident._backend(),
                  self.expr._backend(),
                  self.stmt_list._backend(level+1))


@extend(node.func_stmt)
def _backend(self,level=0):
    self.args.append(node.ident("*args"))
    self.args.append(node.ident("**kwargs"))
    s = """
@function
def %s(%s):
    varargin = %s.varargin
    nargin = %s.nargin
""" % (self.ident._backend(),
       self.args._backend(),
       self.ident._backend(),
       self.ident._backend())
    return s

@extend(node.funcall)
def _backend(self,level=0):
    #import pdb; pdb.set_trace()
    if not self.nargout or self.nargout == 1:
        return "%s(%s)" % (self.func_expr._backend(),
                           self.args._backend())
    elif not self.args:
        return "%s(nargout=%s)" % (self.func_expr._backend(),
                                   self.nargout)
    else:
        return "%s(%s,nargout=%s)" % (self.func_expr._backend(),
                                      self.args._backend(),
                                      self.nargout)


@extend(node.global_list)
def _backend(self,level=0):
    return ",".join([t._backend() for t in self])

@extend(node.ident)
def _backend(self,level=0):
    if self.name in reserved:
        self.name += "_"
    if self.init:
        return "%s=%s" % (self.name,
                          self.init._backend())
    return self.name

@extend(node.if_stmt)
def _backend(self,level=0):
    s = "if %s:%s" % (self.cond_expr._backend(),
                      self.then_stmt._backend(level+1))
    if self.else_stmt:
        # Eech. This should have been handled in the parser.
        if self.else_stmt.__class__ == node.if_stmt:
            self.else_stmt = node.stmt_list([self.else_stmt])
        s += "\n"+indent*level
        s += "else:%s" % self.else_stmt._backend(level+1)
    return s

@extend(node.lambda_expr)
def _backend(self,level=0):
    return 'lambda %s: %s' % (self.args._backend(),
                              self.ret._backend())

@extend(node.let)
def _backend(self,level=0):
    if not options.no_numbers:
        t = "\n# %s:%s" % (options.filename,
                             self.lineno)
                            # level*indent)
    else:
        t = ''

    s = ''
    #if self.args.__class__ is node.funcall:
    #    self.args.nargout = self.nargout
    if self.ret.__class__ is node.expr and self.ret.op == "." :
        try:
            if self.ret.args[1].op == 'parens':
                s += "setattr(%s,%s,%s)" % (self.ret.args[0]._backend(),
                                           self.ret.args[1].args[0]._backend(),
                                           self.args._backend())
        except:
            s += "%s%s = copy(%s)" % (self.ret.args[0]._backend(),
                                       self.ret.args[1]._backend(),
                                       self.args._backend())
    elif (self.ret.__class__ is node.ident and
        self.args.__class__ is node.ident):
        s += "%s=copy(%s)" % (self.ret._backend(),
                              self.args._backend())
    else:
        s += "%s=%s" % (self.ret._backend(), 
                       self.args._backend())
    return s+t

@extend(node.logical)
def _backend(self,level=0):
    if self.value == 0:
        return "false"
    else:
        return "true"

@extend(node.matrix)
def _backend(self,level=0):
    # TODO empty array has shape of 0 0 in matlab
    # size([])
    # 0 0
    if not self.args:
        return "[]"
    elif any(a.__class__ is node.string for a in self.args):
        return " + ".join(a._backend() for a in self.args)
    else:
        #import pdb; pdb.set_trace()
        return "cat(%s)" % self.args[0]._backend()

@extend(node.null_stmt)
def _backend(self,level=0):
    return ""

@extend(node.number)
def _backend(self,level=0):
    #if type(self.value) == int:
    #    return "%s.0" % self.value
    return str(self.value)

@extend(node.pass_stmt)
def _backend(self,level=0):
    return "pass"

@extend(node.persistent_stmt) #FIXME
@extend(node.global_stmt)
def _backend(self,level=0):
    return "global %s" % self.global_list._backend()

@extend(node.return_stmt)
def _backend(self,level=0):
    if not self.ret:
        return "return" 
    else:
        return "return %s" % self.ret._backend()


@extend(node.stmt_list)
def _backend(self,level=0):
    for t in self:
        if not isinstance(t,(node.null_stmt,
                             node.comment_stmt)):
            break
    else:
        self.append(node.pass_stmt())
    sep = "\n"+indent*level
    return sep+sep.join([t._backend(level) for t in self])

@extend(node.string)
def _backend(self,level=0):
    return "'%s'" % str(self.value).encode("string_escape")

@extend(node.sub)
def _backend(self,level=0):
    return "(%s-%s)" %  (self.args[0]._backend(),
                         self.args[1]._backend())

@extend(node.transpose)
def _backend(self,level=0):
    return "%s.T" % self.args[0]._backend()

@extend(node.try_catch)
def _backend(self,level=0):
    fmt = "try:%s\n%sfinally:%s"
    return fmt % (self.try_stmt._backend(level+1),
                  indent*level,
                  self.finally_stmt._backend(level+1))


@extend(node.while_stmt)
def _backend(self,level=0):
    fmt = "while %s:\n%s\n"
    return fmt % (self.cond_expr._backend(),
                  self.stmt_list._backend(level+1))

