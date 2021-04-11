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

import copy
import networkx as nx

import node
from node import extend

def as_networkx(t):
    G = nx.DiGraph()
    for u in node.postorder(t):
        if u.__class__ in (node.ident, node.param):
            uu = "%s_%s_%s" % (u.name, u.lineno, u.column)
            # label = "%s\\n%s" % (uu, u.props if u.props else "")
            G.add_node(uu, ident=u)
            if u.defs:
                for v in u.defs:
                    if v.__class__ is node.ident:
                        vv = "%s_%s_%s" % (v.name, v.lineno, v.column)
                        G.add_node(vv, ident=v)
                        if u.lexpos < v.lexpos:
                            G.add_edge(uu, vv, color="red")
                        else:
                            G.add_edge(uu, vv, color="black")
    return G



def resolve(t, symtab=None, fp=None, func_name=None):
    if symtab is None:
        symtab = {}
    do_resolve(t,symtab)
    G = as_networkx(t)
    for n in G.nodes():
        #print(n)
        #print(n.__class__.__name__)
        u = G.node[n]["ident"]
        if u.props:
            pass
        elif G.out_edges(n) and G.in_edges(n):
            u.props = "U" # variable update
            #print u.name, u.lineno, u.column
        elif G.in_edges(n):
            u.props = "D" # variable definition
        elif G.out_edges(n):
            u.props = "R" # variable reference
        else:
            u.props = "F" # function call
        G.node[n]["label"] = "%s\\n%s" % (n, u.props)
        #print(u.props)
    return G

def do_resolve(t,symtab):
    t._resolve(symtab)

def copy_symtab(symtab):
    new_symtab = copy.copy(symtab)
    for k,v in new_symtab.items():
        new_symtab[k] = copy.copy(v)
    return new_symtab


@extend(node.arrayref)
@extend(node.cellarrayref)
@extend(node.funcall)
def _lhs_resolve(self,symtab):
    # Definitely lhs array indexing.  It's both a ref and a def.
    # Must properly handle cases such as foo(foo(17))=42
    # Does the order of A and B matter?
    self.func_expr._resolve(symtab) # A
    self.args._resolve(symtab)      # B
    self.func_expr._lhs_resolve(symtab)



@extend(node.expr)
def _lhs_resolve(self,symtab):
    if self.op == ".": # see setfield
        self.args._resolve(symtab)
        self.args[0]._lhs_resolve(symtab)
    elif self.op == "[]":
        for arg in self.args:
            arg._lhs_resolve(symtab)

@extend(node.expr_stmt)
def _resolve(self,symtab):
    self.expr._resolve(symtab)

@extend(node.for_stmt)
def _resolve(self,symtab):
    symtab_copy = copy_symtab(symtab)
    self.ident._lhs_resolve(symtab)
    self.expr._resolve(symtab)
    self.stmt_list._resolve(symtab)
    self.stmt_list._resolve(symtab) # 2nd time, intentionally
    # Handle the case where FOR loop is not executed
    for k,v in symtab_copy.items():
        symtab.setdefault(k,[]).append(v)

@extend(node.func_stmt)
def _resolve(self,symtab):
    if self.ident:
        self.ident._resolve(symtab)
    self.args._lhs_resolve(symtab)
    self.ret._resolve(symtab)

@extend(node.global_list)
@extend(node.concat_list)
@extend(node.expr_list)
def _lhs_resolve(self,symtab):
    for expr in self:
        expr._lhs_resolve(symtab)

@extend(node.global_list)
@extend(node.concat_list)
@extend(node.expr_list)
def _resolve(self,symtab):
    for expr in self:
        expr._resolve(symtab)

@extend(node.global_stmt)
def _resolve(self,symtab):
    self.global_list._lhs_resolve(symtab)

@extend(node.ident)
def _lhs_resolve(self,symtab):
    symtab[self.name] = [self]

@extend(node.if_stmt)
def _resolve(self,symtab):
    symtab_copy = copy_symtab(symtab)
    self.cond_expr._resolve(symtab)
    self.then_stmt._resolve(symtab)
    if self.else_stmt:
        self.else_stmt._resolve(symtab_copy)
    for k,v in symtab_copy.items():
        symtab.setdefault(k,[]).append(v)

@extend(node.let)
def _lhs_resolve(self,symtab):
    self.args._resolve(symtab)
    self.ret._lhs_resolve(symtab)

@extend(node.let)
def _resolve(self,symtab):
    self.args._resolve(symtab)
    self.ret._lhs_resolve(symtab)

@extend(node.null_stmt)
@extend(node.continue_stmt)
@extend(node.break_stmt)
def _resolve(self,symtab):
    pass

@extend(node.setfield) # a subclass of funcall
def _resolve(self,symtab):
    self.func_expr._resolve(symtab)
    self.args._resolve(symtab)
    self.args[0]._lhs_resolve(symtab)

@extend(node.try_catch)
def _resolve(self,symtab):
    self.try_stmt._resolve(symtab)
    self.catch_stmt._resolve(symtab) # ???

@extend(node.ident)
def _resolve(self,symtab):
    if self.defs is None:
        self.defs = []
    try:
        self.defs += symtab[self.name]
    except KeyError:
        # defs == set() means name used, but not defined
        pass

@extend(node.arrayref)
@extend(node.cellarrayref)
@extend(node.funcall)
def _resolve(self,symtab):
    # Matlab does not allow foo(bar)(bzz), so func_expr is usually
    # an ident, though it may be a field or a dot expression.
    if self.func_expr:
        self.func_expr._resolve(symtab)
    self.args._resolve(symtab)
    #if self.ret:
    #    self.ret._lhs_resolve(symtab)

@extend(node.expr)
def _resolve(self,symtab):
    for expr in self.args:
        expr._resolve(symtab)

@extend(node.number)
@extend(node.string)
@extend(node.comment_stmt)
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

@extend(node.return_stmt)
def _resolve(self,symtab):
    self.ret._resolve(symtab)
        #symtab.clear()

@extend(node.stmt_list)
def _resolve(self,symtab):
    for stmt in self:
        stmt._resolve(symtab)

@extend(node.where_stmt) # FIXME where_stmt ???
@extend(node.while_stmt)
def _resolve(self,symtab):
    symtab_copy = copy_symtab(symtab)
    self.cond_expr._resolve(symtab)
    self.stmt_list._resolve(symtab)
    self.cond_expr._resolve(symtab)
    self.stmt_list._resolve(symtab)
    # Handle the case where WHILE loop is not executed
    for k,v in symtab_copy.items():
        symtab.setdefault(k,[]).append(v)

@extend(node.function)
def _resolve(self,symtab):
    self.head._resolve(symtab)
    self.body._resolve(symtab)
    self.head.ret._resolve(symtab)

