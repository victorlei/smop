"""
 SMOP compiler -- Simple Matlab/Octave to Python compiler
 Copyright 2011-2013 Victor Leikehman
"""

import inspect
import sys
import node
from node import extend

import options,parse

ZERO = node.number(0)

def rewrite(t):
    global modified; modified = []
    while do_rewrite(t) or modified:
        modified = []

def do_rewrite(t):
    for u in node.postorder(t):
        try:
            u._rewrite()
        except:
            assert 0

def lineno():
    """Returns the current line number in our program."""
    return inspect.currentframe().f_back.f_lineno

@extend(node.node)
def _rewrite(self):
    pass

@extend(node.sum)
@extend(node.min)
@extend(node.max)
def _rewrite(self):
    """max(foo[:]) --> max(foo)
    """
    cls = self.__class__
    if (self.args[0].__class__ is node.arrayref
        and self.args[0].args[0].__class__ is node.expr
        and self.args[0].args[0].op == "::"
        and len(self.args[0].args[0].args) == 0):

        self.become(cls(self.args[0].func_expr))

# """
# @extend(node.false)
# @extend(node.inf)
# @extend(node.ones)
# @extend(node.true)
# @extend(node.zeros)
# def _rewrite(self):
#     if self.__class__ == node.false:
#         v = node.logical(0)
#     elif self.__class__ == node.true:
#         v = node.logical(1)
#     elif self.__class__ == node.zeros:
#         v = node.number(0)
#     elif self.__class__ == node.ones:
#         v = node.number(1)
#     elif self.__class__ == node.inf:
#         v = node.number(0) # FIXME FIXME FIXme
#     s = self._shape()
#     if not s:
#         self.become(v)
#         modified.append(lineno())
#         return
    
#     if options.row_vector_ndim == 1 and s and s[0].__class__ == node.number:
#         del s[0]

#     self.become(node.RESHAPE(node.matrix(v),
#                              node.matrix(*s),
#                              node.matrix(v)))
#     modified.append(lineno())



# # elemental monadic
# @extend(node.isnan)
# @extend(node.sign)
# def _rewrite(self):
#     cls = getattr(node,self.__class__.__name__.upper(),None)
#     assert issubclass(cls,node.builtins)
#     self.become(cls(self.args[0]))
#     modified.append(lineno())


# @extend(node.rand)
# def _rewrite(self):
#     # r = rand(n)
#     # r = rand(m,n)
#     # r = rand([m,n])
#     # r = rand(m,n,p,...)
#     # r = rand([m,n,p,...])
#     # r = rand
#     # r = rand(size(A))
#     # r = rand(..., 'double')
#     # r = rand(..., 'single')

#     cls = getattr(node,self.__class__.__name__.upper(),None)
#     assert issubclass(cls,node.builtins)
#     self.become(cls(*self.args))
#     modified.append(lineno())

# @extend(node.length)
# def _rewrite(self):
#     if not self.args[0]._shape():
#         self.become(ONE)
#         modified.append(lineno())
#     else:
#         self.become(node.MAXVAL(node.SHAPE(self.args[0])))
#         modified.append(lineno())

# @extend(node.ceil)
# def _rewrite(self):
#     self.become(node.CEILING(self.args[0]))
#     modified.append(lineno())

# @extend(node.mod)
# def _rewrite(self):
#     self.become(node.MODULO(self.args[0],self.args[1]))
#     modified.append(lineno())

# @extend(node.func_decl)
# def _rewrite(self):
#     for s in self.args:
#         if s.__class__ == node.ident:
#             s.__class__ = node.param
#             modified.append(lineno())

# # @extend(node.stmt_list)
# # @extend(node.global_list)
# # @extend(node.concat_list)
# # @extend(node.expr_list)
# # def _rewrite(self):
# #     for expr in self:
# #         expr._rewrite()

# class LET(node.let): # final fortran version of assignment
#     def _rewrite(self):
#         pass

# @extend(node.let)
# def _rewrite(self):
#     global debug_variables

#     if self.ret.__class__ == node.ident:
#         if self.ret.name in debug_variables:
#             pdb.set_trace()
#         try:
#             self.ret._t = self.args._type()
#             self.ret._r = self.args._rank()
#             self.ret._s = node.expr_list() if self.ret._r == 0 else self.args._shape()
#             assert isinstance(self.ret._s,node.expr_list)
#             #assert all([not d._shape() for d in self.ret._s])
#             for d in self.ret._s:
#                 assert not d._shape()

#             ndims = len(self.ret._s)
#             if ndims:
#                 #dim_list = node.expr_list([node.arrayref(self.ret, node.number(i+1)) for i in range(len(self.ret._s))]),
#                 new_self = node.stmt_list([node.allocate_stmt(self.ret, self.ret._s),
#                                            LET(self.ret,self.args)])
#                 logging.debug("%s ==> %s" % (self,new_self))
#                 self.become(new_self)
#                 modified.append(lineno())
#         except:
#             logging.exception("")
#             pass


# # @extend(node.func_decl)
# # def _rewrite(self):
# #     #We dont know a priori type and rank
# #     #of function arguments.  We do know that
# #     #to get their size we use SIZE(args[i])
# #     
# #     for a in self.args:
# #         if a.__class__ == node.ident:
# #             a.__class__ = node.param

# @extend(node.call_stmt)
# def _rewrite(self):
#     if (self.func_expr.__class__ == node.ident and
#         self.func_expr.name == "size" and
#         self.func_expr.defs == set()):
#         self.become(node.stmt_list([node.let(self.ret[0],node.SIZE(self.args,ONE)),
#                                     node.let(self.ret[1],node.SIZE(self.args,TWO))]))
#         modified.append(lineno())
#     elif (self.func_expr.__class__ == node.ident and
#         self.func_expr.name == "find" and
#         self.func_expr.defs == set()):
#         self.func_expr.name = "FIND"
#         self.ret[0]._s = node.expr_list()
#         self.ret[1]._s = node.expr_list()
#         modified.append(lineno())
        
# @extend(node.arrayref)
# def _rewrite(self):
#     pass
        

# @extend(node.funcall)
# def _rewrite(self):
#     if self.func_expr.__class__ == node.ident:
#         # Convert funcall nodes to array references.
#         if self.func_expr.defs:
#             self.__class__ = node.arrayref
#             modified.append(lineno())
#         elif self.func_expr.defs == set():
#             # Convert recognized builtin functions to builtin nodes.
#             cls = getattr(node,self.func_expr.name,None)
#             if cls and issubclass(cls,node.builtins) and self.__class__ != cls:
#                 logging.debug("%s ==> %s",self.__class__, cls)
#                 self.__class__ = cls
#                 modified.append(lineno())
#             else:
#                 pass
#                 #raise Exception("'%s' used but not defined" %
#                 #                self.func_expr.name)




# @extend(node.matrix)
# def _rewrite(self):
#     pass



