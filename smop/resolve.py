# smop -- Matlab to Python compiler
# Copyright 2011-2013 Victor Leikehman
"""
if i.defs: 
    i is defined, possibly more than once.  
    Typical for vairable references.

if i.defs is None:
    i is a definition (lhs)
    
if i.defs == set():
    i is used but not defined.  
    Typical for function calls.

symtab is a temporary variable, which maps
variable names (strings) to sets of ident
instances, which possibly define the variable.
It is used in if_stmt, for_stmt, and while_stmt.
"""

import copy,sys,pprint

import node
from node import extend,exceptions
import backend,options

@exceptions
def rename(t):
    for u in node.postorder(t):
        if u.__class__ in (node.ident,node.param):
            if u.name[-1] == "_":
                continue
            if u.defs is None:
                u.name += "_%d_" % u.lineno
            else:
                u.name += "_"+"_".join(sorted(str(v.lineno) for v in u.defs))+"_"

@exceptions
def resolve(t, symtab={}):
    do_resolve(t,symtab)
    if options.do_rename:
        rename(t)

def do_resolve(t,symtab):
    """
    Array references
    ----------------

    a(x)         --> a[x-1]         if rank(a) == 1
                 --> a.flat[x-1]    otherwise

    a(:)         --> a              if rank(a) == 1
                 --> a.flat[-1,1]   otherwise

    a(x,y,z)     --> a[x-1,y-1,z-1]

    a(x:y)       --> a[x-1:y]
    a(x:y:z)     --> a[x-1,z,y]

    a(...end...) --> a[... a.shape[i]...]
    a(x==y)      --> ???

    Function calls
    --------------

    start:stop          --> np.arange(start,stop+1)
    start:step:stop     --> np.arange(start,stop+1,step)

    """
    t._resolve(symtab)
    #pprint.pprint(symtab)
    for u in node.postorder(t):
        if (u.__class__ is node.funcall and 
            u.func_expr.__class__ is node.expr and u.func_expr.op == "."):
            u.__class__ = node.arrayref
        elif (u.__class__ is node.funcall and 
            u.func_expr.__class__ is node.ident):
            if u.func_expr.defs:
                # Both node.arrayref and node.builtins are subclasses
                # of node.funcall, so we are allowed to assign to its
                # __class__ field.  Convert funcall nodes to array
                # references.
                u.__class__ = node.arrayref
            elif u.func_expr.defs == set():
                # Function used, but there is no definition. It's
                # either a builtin function, or a call to user-def
                # function, which is defined later.
                cls = getattr(node,u.func_expr.name,None)
                # """
                # if not cls:
                #     # This is the first time we met u.func_expr.name
                #     cls = type(u.func_expr.name,
                #                (node.funcall,),
                #                { 'code' : None })
                #     setattr(node,u.func_expr.name,cls)
                # assert cls
                # if issubclass(cls,node.builtins) and u.__class__ != cls:
                #     u.func_expr = None # same in builtins ctor

                if cls:
                    u.__class__ = cls
            else:
                # Only if we have A(B) where A.defs is None
                assert 0



        if u.__class__ in (node.arrayref,node.cellarrayref):
            # if (len(u.args) == 1
            #     and isinstance(u.args[0],node.expr)
            #     and u.args[0].op == ":"):
            #     # FOO(:) becomes ravel(FOO)
            #     u.become(node.ravel(u.func_expr))
            # else:
            for i in range(len(u.args)):
                cls = u.args[i].__class__
                if cls is node.number:
                    u.args[i].value -= 1
                elif cls is node.expr and u.args[i].op in ("==","!=","~=","<","=<",">",">="):
                    pass
                elif cls is node.expr and u.args[i].op == ":":
                    # Colon expression as a subscript becomes a
                    # slice.  Everywhere else it becomes a call to
                    # the "range" function (done in a separate pass,
                    # see below).
                    u.args[i].op = "::" # slice marked with op=::
                    if u.args[i].args:
                        if type(u.args[i].args[0]) is node.number:
                            u.args[i].args[0].value -= 1
                        else:
                            u.args[i].args[0] = node.sub(u.args[i].args[0],
                                                         node.number(1))
                    for s in node.postorder(u.args[i]):
                        if s.__class__==node.expr and s.op=="end" and not s.args:
                            s.args = node.expr_list([u.func_expr,node.number(i)])
                elif cls is node.expr and u.args[i].op == "end":
                    u.args[i] = node.number(-1)
                else:
                    u.args[i] = node.sub(u.args[i],node.number(1))

    for u in node.postorder(t):
        if u.__class__ == node.ident and u.defs == set():
            cls = getattr(node,u.name,None)
            if cls and issubclass(cls,node.builtins):
                u.become(cls())

        elif u.__class__ == node.expr and u.op == ":" and u.args:
            if len(u.args) == 2:
                u.become(node.range(u.args[0],
                                    node.add(u.args[1],node.number(1))))
            else:
                u.become(node.range(u.args[0],
                                    node.add(u.args[1],node.number(1)),
                                    u.args[2]))

#def _fix(tree):
#    for s in node.postorder(tree):
#        try:
#            if s.func_expr.name == "ai":
#                import pdb; pdb.set_trace()
#        except:
#            pass
#        # What looks like a referenced, but undefined variable is
#        # actually a function call without the arguments, except in
#        # the beginning of funcall, call_stmt, and func_decl.
#        for i,t in enumerate(s):
#            if i==0 and isinstance(s,node.expr) and s.op==".":
#                continue
#            if i==0 and isinstance(s,(node.funcall,
#                                      node.func_decl)):
#                continue
#            try:
#                # name appears to be a variable reference, but not
#                # defined, such as x=rand
#                if t.name[0] != "." and t.defs == set():
#                    s[i] = node.funcall(t,node.expr_list())
#            except:
#                pass
#
#        if s.__class__ == node.funcall:
#            if isinstance(s.func_expr, node.ident):
#                if s.func_expr.defs:
#                    s.__class__ = node.arrayref
#            else:
#                # Here go field references, such as foo.bar(bzz),
#                # const string indexing, such as 'helloworld'(3),
#                # and (in octave only, forbidden in matlab
#                # expressions such as
#                #   size(foo)(1)  and  [10 20 30](1)
#                s.__class__ = node.arrayref
#

def copy_symtab(symtab):
    new_symtab = copy.copy(symtab)
    for k,v in new_symtab.items():
        new_symtab[k] = copy.copy(v)
    return new_symtab

@extend(node.function)
@exceptions
def _resolve(self,symtab):
    self.head._resolve(symtab)
    self.body._resolve(symtab)
    self.head.ret._resolve(symtab)
    docstring = []
    while type(self.body[0]) is node.comment:
        docstring.append(str(self.body.pop(0))[1:])
    self.head.docstring = docstring

@extend(node.global_list)
@extend(node.concat_list)
@extend(node.expr_list)
@exceptions
def _resolve(self,symtab):
    for expr in self:
        expr._resolve(symtab)

@extend(node.global_list)
@extend(node.concat_list)
@extend(node.expr_list)
@exceptions
def _lhs_resolve(self,symtab):
    for expr in self:
        expr._lhs_resolve(symtab)
        
@extend(node.stmt_list)
@exceptions
def _resolve(self,symtab):
    for stmt in self:
        stmt._resolve(symtab)
        
@extend(node.number)
@extend(node.string)
@extend(node.comment)
@exceptions
def _resolve(self,symtab):
        pass

# @extend(node.call_stmt)
# def _resolve(self,symtab):
#     # TODO: does the order of A and B matter? Only if the
#     # evaluation of function args may change the value of the
#     # func_expr. 
#     self.func_expr._resolve(symtab) # A
#     self.args._resolve(symtab)      # B
#     self.ret._lhs_resolve(symtab)

@extend(node.let)
@exceptions
def _resolve(self,symtab):
    self.args._resolve(symtab)
    self.ret._lhs_resolve(symtab)

@extend(node.func_decl)
@exceptions
def _resolve(self,symtab):
    if self.ident:
        self.ident._resolve(symtab)
    self.args._lhs_resolve(symtab)
    self.ret._resolve(symtab)

@extend(node.for_stmt)
@exceptions
def _resolve(self,symtab):
    symtab_copy = copy_symtab(symtab)
    self.ident._lhs_resolve(symtab)
    self.expr._resolve(symtab)
    self.stmt_list._resolve(symtab)
    self.stmt_list._resolve(symtab) # 2nd time, intentionally
    # Handle the case where FOR loop is not executed
    for k,v in symtab_copy.items():
        symtab.setdefault(k,set()).update(v)

@extend(node.if_stmt)
@exceptions
def _resolve(self,symtab):
    symtab_copy = copy_symtab(symtab)
    self.cond_expr._resolve(symtab)
    self.then_stmt._resolve(symtab)
    if self.else_stmt:
        self.else_stmt._resolve(symtab_copy)
    for k,v in symtab_copy.items():
        symtab.setdefault(k,set()).update(v)
        
@extend(node.continue_stmt)  # FIXME
@extend(node.break_stmt)     # FIXME
@exceptions
def _resolve(self,symtab):
    pass

@extend(node.global_stmt)
@exceptions
def _resolve(self,symtab):
    self.global_list._lhs_resolve(symtab)
        
@extend(node.return_stmt)
@exceptions
def _resolve(self,symtab):
    self.ret._resolve(symtab)
        #symtab.clear()

@extend(node.expr_stmt)
@exceptions
def _resolve(self,symtab):
    self.expr._resolve(symtab)
        
@extend(node.where_stmt) # FIXME where_stmt ???
@extend(node.while_stmt)
@exceptions
def _resolve(self,symtab):
    symtab_copy = copy_symtab(symtab)
    self.cond_expr._resolve(symtab)
    self.stmt_list._resolve(symtab)
    self.cond_expr._resolve(symtab)
    self.stmt_list._resolve(symtab)
    # Handle the case where WHILE loop is not executed
    for k,v in symtab_copy.items():
        symtab.setdefault(k,set()).update(v)

@extend(node.try_catch)
@exceptions
def _resolve(self,symtab):
    self.try_stmt._resolve(symtab)
    self.catch_stmt._resolve(symtab) # ???

@extend(node.ident)
@exceptions
def _lhs_resolve(self,symtab):
    # try:
    #     symtab[self.name].add(self)
    # except:
    symtab[self.name] = set([self])
    # defs is None means definition
        
@extend(node.ident)
@exceptions
def _resolve(self,symtab):
    if self.defs is None:
        self.defs = set()
    try:
        self.defs |= symtab[self.name]
    except KeyError:
        # defs == set() means name used, but not defined
        pass
        
@extend(node.arrayref)
@extend(node.cellarrayref)
@extend(node.funcall)
@exceptions
def _resolve(self,symtab):
    # Matlab does not allow foo(bar)(bzz), so func_expr is usually
    # an ident, though it may be a field or a dot expression.
    if self.func_expr:
        self.func_expr._resolve(symtab)
    self.args._resolve(symtab)
    #if self.ret:
    #    self.ret._lhs_resolve(symtab)

@extend(node.setfield) # a subclass of funcall
@exceptions
def _resolve(self,symtab):
    self.func_expr._resolve(symtab)
    self.args._resolve(symtab)
    self.args[0]._lhs_resolve(symtab) 

@extend(node.arrayref)
@extend(node.cellarrayref)
@extend(node.funcall)    
@exceptions
def _lhs_resolve(self,symtab):
    # Definitely lhs array indexing.  It's both a ref and a def.
    # Must properly handle cases such as foo(foo(17))=42
    # Does the order of A and B matter?
    self.func_expr._resolve(symtab) # A
    self.args._resolve(symtab)      # B
    self.func_expr._lhs_resolve(symtab)
        
@extend(node.expr)
@exceptions
def _resolve(self,symtab):
    for expr in self.args:
        expr._resolve(symtab)

@extend(node.expr)
@exceptions
def _lhs_resolve(self,symtab):
    if self.op == ".": # see setfield
        self.args._resolve(symtab)
        self.args[0]._lhs_resolve(symtab)
    elif self.op == "[]":
        for arg in self.args:
            arg._lhs_resolve(symtab)

        

