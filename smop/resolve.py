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
from node import extend
import backend,options
import networkx as nx

def graphviz(t,fp,func_name):
    fp.write("digraph %s {\n" % func_name)
    fp.write('graph [rankdir="LR"];\n')
    for u in node.postorder(t):
        if u.__class__ in (node.ident,node.param):
            fp.write("%s [label=%s_%s_%s];\n" % (u.lexpos,u.name,u.lineno,u.column))
            if u.defs:
                for v in u.defs:
                    fp.write("%s -> %s" % (u.lexpos,v.lexpos))
                    if u.lexpos < v.lexpos:
                        fp.write('[color=red]')
                    #else:
                    #    fp.write('[label=%s.%s]' % (v.lineno,v.column))
                    fp.write(';\n')
    fp.write("}\n")

def as_networkx(t):
    G = nx.DiGraph()
    for u in node.postorder(t):
        if u.__class__ in (node.ident,node.param):
            uu = "%s_%s_%s" % (u.name,u.lineno,u.column)
            #label = "%s\\n%s" % (uu, u.props if u.props else "")
            G.add_node(uu, ident=u) # label=label)
            if u.defs:
                for v in u.defs:
                    vv = "%s_%s_%s" % (v.name,v.lineno,v.column)
                    G.add_node(vv, ident=v)
                    if u.lexpos < v.lexpos:
                        G.add_edge(uu,vv,color="red")
                    else:
                        G.add_edge(uu,vv,color="black")
    return G

def resolve(t, symtab=None, fp=None, func_name=None):
    if symtab is None:
        symtab = {}
    do_resolve(t,symtab)
    G = as_networkx(t)
    #import pdb;pdb.set_trace()
    for n in G.nodes():
        u = G.node[n]["ident"]
        if u.props:
            pass
        elif G.out_edges(n) and G.in_edges(n):
            u.props = "U" # upd
            #print u.name, u.lineno, u.column
        elif G.in_edges(n):
            u.props = "D" # def
        elif G.out_edges(n):
            u.props = "R" # ref
        else:
            u.props = "F" # ???
        G.node[n]["label"] = "%s\\n%s" % (n, u.props)

    for u in node.postorder(t):
        #if u.__class__ is node.func_decl:
        #    u.ident.name += "_"
        if u.__class__ is node.funcall:
            try:
                if u.func_expr.props in "UR": # upd,ref
                    u.__class__ = node.arrayref
                #else:
                #    u.func_expr.name += "_"
            except:
                pass

    for u in node.postorder(t):
        if u.__class__ in (node.arrayref,node.cellarrayref):
            for i,v in enumerate(u.args):
                if v.__class__ is node.expr and v.op == ":":
                    v.op = "::"
#                for w in node.postorder(v):
#                    if w.__class__ is node.expr and w.op == "end":
#                        w.args[0] = u.func_expr
#                        w.args[1] = node.number(i)

    for u in node.postorder(t):
        if u.__class__ is node.let:
            if (u.ret.__class__ is node.ident and
                u.args.__class__ is node.matrix):
                u.args = node.funcall(func_expr=node.ident("matlabarray"),
                                      args=node.expr_list([u.args]))

    H = nx.connected_components(G.to_undirected())
    for i,component in enumerate(H):
        for nodename in component:
            if G.node[nodename]["ident"].props == "R":
                has_update = 1
                break
        else:
            has_update = 0
        if has_update:
            for nodename in component:
                G.node[nodename]["ident"].props += "S"  # sparse
        #S = G.subgraph(nbunch)
        #print S.edges()
    return G

def do_resolve(t,symtab):
    t._resolve(symtab)

def copy_symtab(symtab):
    new_symtab = copy.copy(symtab)
    for k,v in new_symtab.items():
        new_symtab[k] = copy.copy(v)
    return new_symtab

@extend(node.function)
def _resolve(self,symtab):
    self.head._resolve(symtab)
    self.body._resolve(symtab)
    self.head.ret._resolve(symtab)
        
@extend(node.global_list)
@extend(node.concat_list)
@extend(node.expr_list)
def _resolve(self,symtab):
    for expr in self:
        expr._resolve(symtab)

@extend(node.global_list)
@extend(node.concat_list)
@extend(node.expr_list)
def _lhs_resolve(self,symtab):
    for expr in self:
        expr._lhs_resolve(symtab)
        
@extend(node.stmt_list)
def _resolve(self,symtab):
    for stmt in self:
        stmt._resolve(symtab)
        
@extend(node.number)
@extend(node.string)
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
def _lhs_resolve(self,symtab):
    self.args._resolve(symtab)
    self.ret._lhs_resolve(symtab)

@extend(node.let)
def _resolve(self,symtab):
    self.args._resolve(symtab)
    self.ret._lhs_resolve(symtab)

@extend(node.func_decl)
def _resolve(self,symtab):
    if self.ident:
        self.ident._resolve(symtab)
    self.args._lhs_resolve(symtab)
    self.ret._resolve(symtab)

@extend(node.for_stmt)
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
def _resolve(self,symtab):
    pass

@extend(node.global_stmt)
def _resolve(self,symtab):
    self.global_list._lhs_resolve(symtab)
        
@extend(node.return_stmt)
def _resolve(self,symtab):
    self.ret._resolve(symtab)
        #symtab.clear()

@extend(node.expr_stmt)
def _resolve(self,symtab):
    self.expr._resolve(symtab)
        
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
        symtab.setdefault(k,set()).update(v)

@extend(node.try_catch)
def _resolve(self,symtab):
    self.try_stmt._resolve(symtab)
    self.catch_stmt._resolve(symtab) # ???

@extend(node.ident)
def _lhs_resolve(self,symtab):
    symtab[self.name] = set([self])
 
@extend(node.ident)
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
def _resolve(self,symtab):
    # Matlab does not allow foo(bar)(bzz), so func_expr is usually
    # an ident, though it may be a field or a dot expression.
    if self.func_expr:
        self.func_expr._resolve(symtab)
    self.args._resolve(symtab)
    #if self.ret:
    #    self.ret._lhs_resolve(symtab)

@extend(node.setfield) # a subclass of funcall
def _resolve(self,symtab):
    self.func_expr._resolve(symtab)
    self.args._resolve(symtab)
    self.args[0]._lhs_resolve(symtab) 

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
def _resolve(self,symtab):
    for expr in self.args:
        expr._resolve(symtab)

@extend(node.expr)
def _lhs_resolve(self,symtab):
    if self.op == ".": # see setfield
        self.args._resolve(symtab)
        self.args[0]._lhs_resolve(symtab)
    elif self.op == "[]":
        for arg in self.args:
            arg._lhs_resolve(symtab)

        

