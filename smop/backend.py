# smop -- Simple Matlab to Python compiler
# Copyright 2011-2013 Victor Leikehman

import logging
logger = logging.getLogger(__name__)
import node,options
from node import extend,exceptions

indent = " "*4

optable = {
    "~" : "not ",
    "~=": "!=",
    "||": "or",
    "&&": "and",
    "^" : "**",
    ".^": "**",
    "./": "/",
    ".*": "*",
    }

def backend(t,*args):
    return t._backend(*args)

@extend(node.matrix)
@exceptions
def _backend(self,level=0):
    # TODO empty array has shape of 0 0 in matlab
    # size([])
    # 0 0
    if not self.args:
        return "np.array([])"
    elif any(a.__class__ is node.string for a in self.args):
        return " + ".join(a._backend() for a in self.args)
    else:
        return "np.array([%s]).reshape(1, -1)" % self.args._backend()

@extend(node.cellarrayref)
@exceptions
def _backend(self,level=0):
    return "%s[%s]" % (self.func_expr._backend(),
                       self.args._backend())
@extend(node.cellarray)
@exceptions
def _backend(self,level=0):
    return "[%s]" % self.args._backend()

#@extend(node.concat_list)
#def _backend(self,level=0):
#    return ";".join([t._backend() for t in self])

@extend(node.ravel)
@exceptions
def _backend(self,level=0):
    return "%s.ravel()" % self.args[0]._backend()

@extend(node.transpose)
@exceptions
def _backend(self,level=0):
    return "%s.T" % self.args[0]._backend()

@extend(node.expr_stmt)
@exceptions
def _backend(self,level=0):
    return self.expr._backend()

@extend(node.return_stmt)
@exceptions
def _backend(self,level=0):
    if not self.ret:
        return "return" 
    else:
        return "return %s" % self.ret._backend()

@extend(node.continue_stmt)
@exceptions
def _backend(self,level=0):
    return "continue"

@extend(node.global_stmt)
@exceptions
def _backend(self,level=0):
    return "global %s" % self.global_list._backend()

@extend(node.global_list)
@exceptions
def _backend(self,level=0):
    return ",".join([t._backend() for t in self])

@extend(node.break_stmt)
@exceptions
def _backend(self,level=0):
    return "break"

@extend(node.string)
@exceptions
def _backend(self,level=0):
    return repr(self.value)

@extend(node.number)
@exceptions
def _backend(self,level=0):
    #if type(self.value) == int:
    #    return "%s.0" % self.value
    return str(self.value)

@extend(node.logical)
@exceptions
def _backend(self,level=0):
    if self.value == 0:
        return "false"
    else:
        return "true"
    
# @extend(node.range)
# def _backend(self,level=0):
#     i = node.ident.new("I")
#     return "[ (%s, %s=%s,%s) ]" % (i,i,self.args[0],self.args[1])

@extend(node.add)
@exceptions
def _backend(self,level=0):
    if (self.args[0].__class__ is node.number and
        self.args[1].__class__ is node.number):
        return node.number(self.args[0].value +
                           self.args[1].value)._backend()
    else:
        return "(%s +%s)" % (self.args[0]._backend(),
                            self.args[1]._backend())

@extend(node.sub)
@exceptions
def _backend(self,level=0):
    return "(%s -%s)" %  (self.args[0]._backend(),
                         self.args[1]._backend())

@extend(node.expr)
@exceptions
def _backend(self,level=0):
    if self.op == '*':
        return "np.dot(%s, %s)" % (self.args[0]._backend(),
                                   self.args[1]._backend())
    if self.op == '@': # FIXME
        return self.args[0]._backend()

    if self.op == "\\":
        return "np.linalg.solve(%s, %s)" % (self.args[0]._backend(),
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
        #return "arange(%s)" % self.args._backend()
        if not self.args:
            return ":"
        else:
            return "%s:%s" % (self.args[0],self.args[1])
    
    if self.op == "end":
        if self.args:
            return "%s.shape[%s]" % (self.args[0]._backend(),
                                     self.args[1]._backend())
        else:
            return "end"
    if self.op == ".":
        return "%s%s" % (self.args[0]._backend(),
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
    ret = "%s=" % self.ret._backend() if self.ret else ""
    return ret+"%s(%s)" % (self.op,
                           ",".join([t._backend() for t in self.args]))

@extend(node.arrayref)
@exceptions
def _backend(self,level=0):
#    if (len(self.args) == 1 and not
#        (self.args[0].__class__== node.expr and self.args[0].op=="::")):
#        fmt = "%s(%s)"
#    else:
    if options.subscripts == "round":
        fmt = "%s(%s)"
    elif options.subscripts == "square":
        fmt = "%s[%s]"
    else:
        assert False, options.subscripts
    return fmt % (self.func_expr._backend(),
                       self.args._backend())

@extend(node.funcall)
@exceptions
def _backend(self,level=0):
    # if self.func_expr:
    #     func_name = self.func_expr.name
    # else:
    #     func_name = self.__class__.__name__
    #if self.ret is None:
        return "%s(%s)" % (self.func_expr._backend(),
                           self.args._backend())
    #else:
    #    return ("%s = %s(%s)" % (self.ret._backend(),
    #                             self.func_expr._backend(),
    #                             self.args._backend()))

@extend(node.let)
@exceptions
def _backend(self,level=0):
    if options.line_numbering:
        s = "# %d\n" % self.lineno + level*indent
    else:
        s = ''
    if self.nargout > 1:
        return "%s = %s # nargout=%d" % (self.ret._backend(), 
                                       self.args._backend(),
                                       self.nargout)
    else:
        return "%s = %s" % (self.ret._backend(),
                          self.args._backend())


@extend(node.expr_list)
@exceptions
def _backend(self,level=0):
    return ", ".join([t._backend() for t in self])

@extend(node.concat_list)
@exceptions
def _backend(self,level=0):
    return ", ".join(["[%s]"%t._backend() for t in self])

# @extend(node.call_stmt)
# def _backend(self,level=0):
#     return "CALL %s(%s,%s)" % (self.func_expr._backend(),
#                                self.args._backend(),
# self.ret._backend())

fortran_type = {
    '@': '***',
    'd': 'DOUBLE PRECISION',
    'i': 'INTEGER',
    'l': 'LOGICAL',
    'c': 'CHARACTER',
}

# def decl__backend(i):
#     assert isinstance(i,ident)
#     try:
#         if i._rank() == 0:
#             return "%s :: %s\n" % (fortran_type[i._type()],
#                                    i)
#         return ("%s,DIMENSION(%s),ALLOCATABLE :: %s\n" % 
#                 (fortran_type[i._type()],
#                  ",".join([":" for j in range(i._rank())]), i))
#     except:
#         return "??? :: %s\n" % i
@extend(node.function)
@exceptions
def _backend(self,level=0):
    s = self.head._backend(level)
    t = self.body._backend(level+1)
    return "%s%s" %  (s,t)
        

# Sometimes variable names collide with _python reserved
# words and constants.  We handle this in the _backend rather than in
# the lexer, to keep the target language separate from
# the lexer code.
reserved = set(
    """
    abs all and any apply as assert basestring bin bool break buffer bytearray
    callable chr class classmethod cmp coerce compile complex continue copyright
    credits def del delattr dict dir divmod elif Ellipsis else enumerate eval
    except exec execfile exit False file filter finally float for format from
    frozenset getattr global globals hasattr hash help hex id if import __import__
    in input int intern is isinstance issubclass iter lambda len license list
    locals long map max memoryview min next None not not NotImplemented object oct
    open or ord pass pow print property quit raise range raw_input reduce reload
    repr return reversed round set setattr slice sorted staticmethod str sum super
    True try tuple type unichr unicode vars while with xrange yield zip
    """.split())

@extend(node.ident)
@exceptions
def _backend(self,level=0):
    return self.name if self.name not in reserved else self.name+"_"

@extend(node.stmt_list)
@exceptions
def _backend(self,level=0):
    sep = "\n"+indent*level
    if len(self):
        return sep+sep.join([t._backend(level) for t in self])
    else:
        return sep+"pass"

@extend(node.func_decl)
@exceptions
def _backend(self,level=0):
    if not self.use_nargin:
        s = "def %s(%s):" % (self.ident._backend(),
                             self.args._backend())
    else:
        s = "def %s(*varargin):" % self.ident._backend()
    if self.docstring:
        s += '\n    """' + self.docstring[0].lstrip()
        for d in self.docstring[1:]:
            if d.strip():
                s += '\n    ' + d
            else:
                s += '\n'
        s += '\n    """'
    if self.use_nargin:
        s += "\n    nargin = len(varargin)"
        for i in range(len(self.args)):
            s += "\n    if nargin > %d:" % i
            s += "\n        %s = varargin[%d]" % (self.args[i]._backend(), i)
    return s

"""
@extend(node.allocate_stmt)
def _backend(self,level=0):
    s = "ALLOCATE (%s(%s))" % (self.ident._backend(),
                               self.args._backend())
    return s
"""

@extend(node.lambda_expr)
@exceptions
def _backend(self,level=0):
    return 'lambda %s: %s' % (self.args._backend(),
                              self.ret._backend())
@extend(node.for_stmt)
@exceptions
def _backend(self,level=0):
    fmt = "for %s in %s:%s"
    return fmt % (self.ident._backend(),
                  self.expr._backend(),
                  self.stmt_list._backend(level+1))

@extend(node.if_stmt)
@exceptions
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

@extend(node.while_stmt)
@exceptions
def _backend(self,level=0):
    fmt = "while %s:\n%s\n"
    return fmt % (self.cond_expr._backend(),
                  self.stmt_list._backend(level+1))

@extend(node.try_catch)
@exceptions
def _backend(self,level=0):
    fmt = "try: %s\n%sexcept: %s"
    return fmt % (self.try_stmt._backend(),
                  "",
                  self.catch_stmt._backend())

@extend(node.builtins)
@exceptions
def _backend(self,level=0):
    #if not self.ret:
        return "%s(%s)" % (self.__class__.__name__,
                           self.args._backend())
    # else:
    #     return ("%s=%s(%s)" % (self.ret._backend(),
    #                            self.__class__.__name__,
    #                            self.args._backend()))

@extend(node.strcmp)
@exceptions
def _backend(self,level=0):
    return "%s == %s" % (self.args[0]._backend(),
                       self.args[1]._backend())
@extend(node.strcmpi)
@exceptions
def _backend(self,level=0):
    return "%s.lower() == %s.lower()" % (self.args[0]._backend(),
                                       self.args[1]._backend())

@extend(node.struct)
@exceptions
def _backend(self,level=0):
    return "type('struct', (), {})()"

@extend(node.isfield)
@exceptions
def _backend(self,level=0):
    return "hasattr(%s, %s)" % (self.args[0]._backend(),
                                self.args[1]._backend())

@extend(node.isequal)
@exceptions
def _backend(self,level=0):
    return "np.array_equal(%s)" % self.args._backend()

@extend(node.isempty)
@exceptions
def _backend(self,level=0):
    return "(0 in %s.shape)" % self.args[0]._backend()

@extend(node.numel)
@exceptions
def _backend(self,level=0):
    return "%s.size" % self.args[0]._backend()

@extend(node.size)
@exceptions
def _backend(self,level=0):
    if len(self.args) == 1:
        # if self.ret:
        #     return "%s = %s.shape" % (self.ret,
        #                               self.args[0]._backend())
        # else:
        return "%s.shape" % self.args[0]._backend()
            
    if self.args[1].__class__ is node.number:
        return "%s.shape[%s]" % (self.args[0]._backend(),
                                 self.args[1].value-1)
    return "%s.shape[%s-1]" % (self.args[0]._backend(),
                               self.args[1]._backend())

@extend(node.cumsum)
@exceptions
def _backend(self,level=0):
    if len(self.args) == 1:
        return "np.cumsum(%s,0)" % self.args[0]._backend()
    if self.args[1].__class__ is node.number:
        return "np.cumsum(%s,%s)" % (self.args[0]._backend(),
                                     self.args[1].value-1)
    return "np.cumsum(%s,%s-1)" % (self.args[0]._backend(),
                                   self.args[1]._backend())
    
@extend(node.dot)
@exceptions
def _backend(self,level=0):
    return "%s.dot(%s)" % (self.args[0]._backend(),
                           self.args[1]._backend())
@extend(node.length)
@exceptions
def _backend(self,level=0):
    return "max(%s.shape)" % self.args[0]._backend()

@extend(node.fopen)
@exceptions
def _backend(self,level=0):
    return "open(%s)" % self.args._backend()

@extend(node.fclose)
@exceptions
def _backend(self,level=0):
    return "%s.close()" % self.args._backend()

@extend(node.min)
@extend(node.max)
@extend(node.sum)
@exceptions
def _backend(self,level=0):
    cls_name = self.__class__.__name__
    return ("np." + cls_name  + "(%s)") % self.args._backend()

@extend(node.zeros)
@extend(node.ones)
@extend(node.inf)
@exceptions
def _backend(self,level=0):
    cls_name = self.__class__.__name__
    # The last arg might be a dtype string
    if type(self.args[-1]) is node.string:
        dtype_name = self.args.pop()._backend()
    else:
        dtype_name = "'float64'"  # default case
    # If only one shape argument is given, make a square matrix.
    if len(self.args) == 1:
        self.args.append(self.args[0])
    # There's not a direct equivalent for inf, but we can fake it.
    if cls_name == 'inf':
        return "(np.zeros(shape=(%s), dtype=%s) + np.inf)" % (self.args._backend(), dtype_name)
    return "np.%s(shape=(%s), dtype=%s)" % (cls_name, self.args._backend(), dtype_name)

@extend(node.exist)
@exceptions
def _backend(self,level=0):
    if self.args[1].__class__ is node.string and self.args[1].value == "file":
        return "os.path.exists(%s)" % self.args[0]._backend()
    if self.args[1].__class__ is node.string and self.args[1].value == "var":
        return "%s in globals()" % self.args[0]._backend()
    raise NotImplementedError("Not implemented: exist")

@extend(node.find)
@exceptions
def _backend(self,level=0):
    return "np.flatnonzero(%s)" % self.args[0]._backend()
    # if self.ret and len(self.ret) == 2:
    #     return "%s,%s = np.nonzero(%s)" % (self.ret[0]._backend(),
    #                                        self.ret[1]._backend(),
    #                                        self.args[0]._backend())
    # else:
    #     return "np.flatnonzero(%s)" % self.args[0]._backend()

@extend(node.load)
@exceptions
def _backend(self,level=0):
    return "loadmat(%s,matlab_compatible=True)" % self.args._backend()

@extend(node.save)
@exceptions
def _backend(self,level=0):
    return "savemat(%s)" % self.args._backend()

@extend(node.comment)
@exceptions
def _backend(self,level=0):
    return "%s" % self

@extend(node.Inf)
@exceptions
def _backend(self,level=0):
    return "np.inf"
