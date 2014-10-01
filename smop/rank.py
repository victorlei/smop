import operator

from constraint import Problem,RecursiveBacktrackingSolver
import networkx as nx
import node
from node import extend
import resolve

def rank(tree):
    @extend(node.number)
    def _rank(self):
        problem.addVariable(id(self),[0])

    @extend(node.let)
    def _rank(self):
        if isinstance(self.ret,node.ident):
            # plain assignment -- not a field, lhs indexing
            vars = [id(self.ret), id(self.args)]
            try:
                problem.addVariables(vars,range(4))
                problem.addConstraint(operator.__eq__,vars)
            except ValueError:
                pass
        else:
            # lhs indexing or field
            pass

    @extend(node.for_stmt)
    def _rank(self):
        vars =  [id(self.ident), id(self.expr)]
        problem.addVariables(vars,range(4))
        problem.addConstraint((lambda u,v: u+1==v),vars)

    @extend(node.if_stmt)
    def _rank(self):
        # could use operator.__not__ instead of lambda expression
        problem.addVariable(id(self.cond_expr),range(4))
        problem.addConstraint(lambda t: t==0, [id(self.cond_expr)])

    @extend(node.ident)
    def _rank(self):
        try:
            x = id(self)
            problem.addVariable(x,range(4))
            for other in self.defs:
                y = id(other)
                try:
                    problem.addVariable(y,range(4))
                except ValueError:
                    pass
                problem.addConstraint(operator.__eq__, [x,y])
        except:
            print "Ignored ",self
    """

    @extend(funcall)
    def rank(self,problem):
        if not isinstance(self.func_expr,ident):
            # In MATLAB, chaining subscripts, such as size(a)(1)
            # is not allowed, so only fields and dot expressions
            # go here.  In Octave, chaining subscripts is allowed,
            # and such expressions go here.
            return
        try:
            if defs.degree(self.func_expr):
                # If a variable is defined, it is not a function,
                # except function handle usages, such as
                #    foo=@size; foo(17)
                # which is not handled properly yet.
                x = id(self.func_expr)
                n = len(self.args)
                problem.addVariable(x,range(4))
                problem.addConstraint((lambda u: u>=n),[x])
                return
        except TypeError: # func_expr is unhashable
            # For example [10 20 30](2)
            return
        except KeyError:
            # See tests/clear_margins.m
            return
        assert getattr(self.func_expr,"name",None)
        # So func_expr is an undefined variable, and we understand
        # it's a function call -- either builtin or user-defined.
        name = self.func_expr.name
#    if name not in builtins:
#        # User-defined function
#        return
#    builtins[name](self,problem)
#
#@extend(expr)
#def rank(self,problem):
#    try:
#        builtins[self.op](self,problem)
#    except:
#        pass
    """

    problem = Problem(RecursiveBacktrackingSolver())
    for v in node.postorder(tree):
        for u in v:
            try:
                u._rank()
            except AttributeError:
                pass
    s = problem.getSolution()
    if not s:
        print "No solutions"
    else:
        d = set()
        #for k in sorted(G.nodes(), key=lambda t: (t.name,t.lexpos)):
        for k in node.postorder(tree):
            if isinstance(k,node.ident):
                print k.name,k.lineno, s.get(id(k),-1)
                #if not k.name in d and s[id(k)]:
                #    print "%s(%d)" % (k.name,s[id(k)])
                #    d.add(k.name)
