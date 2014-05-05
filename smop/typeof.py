"""
Given node.function instance, apply it
to argument list, such as ["i","d"]
and return the result.
"""

import node,options
from node import extend,exceptions

callstack = set()

@extend(node.function)
@exceptions
def apply(self,args,symtab):
    name = self.head.ident.name 
    if name in callstack:
        return
    print "%%%", self.head.ident.name
    callstack.add(name)

    params = [(u.name,u.lineno) for u in self.head.args]
    if len(args) < len(self.head.args): # head.args are formal params
        args += [''] * (len(self.head.args)-len(args))
    symtab.update(zip(params,args))
    self.body._typeof(symtab)
    #return [symtab[u.name,u.lineno] for u in self.ret] 
#    for u in node.postorder(self):
#        if u.__class__ is node.ident:
#            try:
#                print u.name,u.lineno,symtab[u.name,u.lineno]
#            except:
#                print u.name,u.lineno,'?'
    return '' #[u._typeof(symtab) for u in self.head.ret]

@extend(node.node)
@exceptions
def _typeof(self,symtab):
    for u in self:
        u._typeof(symtab)

@extend(node.for_stmt)
@exceptions
def _typeof(self,symtab):
    symtab[self.ident.name,self.ident.lineno] = 'i'
    self.stmt_list._typeof(symtab)

@extend(node.func_decl)
@exceptions
def _typeof(self,symtab):
    pass # ignore ident,args,ret

@extend(node.let)
@exceptions
def _typeof(self,symtab):
    if self.ret.__class__ is node.ident:
        symtab[self.ret.name,self.ret.lineno] = self.args._typeof(symtab)

@extend(node.param)
@extend(node.cellfun)  # FIXME
@exceptions
def _typeof(self,symtab):
    return ''

@extend(node.ident)
@exceptions
def _typeof(self,symtab):
    """
    symtab maps pairs (name,lineno) to string
    encoding of type.
    """
    if self.defs is None:
        try:
            return symtab[self.name,self.lineno]
        except:
            print '+++ Missing type for',self.name
            return '' 
    ts = set([u._typeof(symtab) for u in self.defs if u.defs is None])
    if '' in ts:
        ts.remove('')
    if 'i' in ts and 'd' in ts:
        ts.remove('i')
    if len(ts) == 0:
        #print '+++','No definition of variable',self.name,self.lineno
        return ''
    if len(ts) == 1:
        self._t = ts.pop()
        return self._t
    if len(ts) > 1:
        print '+++','Conflicting defs for', self.name,self.lineno,ts
        return ''
        
    return ''

@extend(node.matrix)
@exceptions
def _typeof(self,symtab):
    ts = set([u._typeof(symtab) for u in self.args if u._typeof(symtab)])
    if len(ts) >= 1:
        return ts.pop()
    return 'd'

#    ts = set([u._typeof(symtab) for u in self])
#    if len(ts) == 1:
#        return ts.pop()
#    return ''

@extend(node.arrayref) 
@extend(node.cellarrayref) 
@exceptions
def _typeof(self,symtab):
    return self.func_expr._typeof(symtab)

@extend(node.expr)
@exceptions
def _typeof(self,symtab):
    nargs = len(self.args)
    if nargs == 0:
        assert 0
    if nargs == 1:
        if self.op in ("!","~"):
            return 'l'
        if self.op in ("-","parens"):
            return self.args[0]._typeof(symtab)
        assert 0, self.op
    if len(self.args) == 2:
        if self.op == ":":
            return 'i'
        if self.op in ("<",">","<=",">=","==","~=","!=","&&","||"):
            return 'l'
        if self.op in ("*","+","-","/","^",".*","./"):
            if (self.args[0]._typeof(symtab) == 'i' and 
                self.args[1]._typeof(symtab) == 'i'):
                return 'i'
            else:
                return 'd'

@extend(node.add)
@extend(node.sub)
@exceptions
def _typeof(self,symtab):
    if (self.args[0]._typeof(symtab) == 'i' and 
        self.args[1]._typeof(symtab) == 'i'):
        return 'i'
    else:
        return 'd'

@extend(node.ones)
@extend(node.zeros)
@exceptions
def _typeof(self,symtab):
    if self.args[-1]._typeof(symtab) == 'c':
        raise Exception("Only 'double' is implemented")
    return 'i'

@extend(node.RESHAPE)
@extend(node.MAXVAL)
@extend(node.MINVAL)
@extend(node.CEILING)
@extend(node.abs)
@extend(node.ceil)
@extend(node.sign)
@extend(node.min)
@extend(node.max)
@extend(node.transpose)
@exceptions
def _typeof(self,symtab):
    return self.args[0]._typeof(symtab)

# sum is different from similar funcs.
# re-check this
@extend(node.sum)
@extend(node.cumsum)
@extend(node.cumprod)
@exceptions
def _typeof(self,symtab):
    if self.args[0]._typeof(symtab) == 'l':
        return 'i'
    return self.args[0]._typeof(symtab)

@extend(node.number)
@exceptions
def _typeof(self,symtab):
    return 'i' if isinstance(self.value,int) else 'd'

@extend(node.diff)
@extend(node.RAND)
@extend(node.rand)
@extend(node.inf)
@extend(node.dot)
@extend(node.nnz)
@extend(node.mod)
@exceptions
def _typeof(self,symtab):
    return 'd'

@extend(node.rem)
@extend(node.SIZE)
@extend(node.size)
@extend(node.find)
#@extend(node.findone)
@exceptions
def _typeof(self,symtab):
    return 'ii'

@extend(node.sort)
@exceptions
def _typeof(self,symtab):
    return self.args[0]._typeof(symtab)
    #if len(self.ret) == 2:
    #    return self.args[0]._typeof(symtab) + 'i'

@extend(node.numel)
@extend(node.floor)
@extend(node.length)
@extend(node.range_)
@exceptions
def _typeof(self,symtab):
    return 'i'

@extend(node.false)
@extend(node.true)
@extend(node.isequal)
@extend(node.isnan)
@extend(node.isinf)
@extend(node.isempty)
@extend(node.all)
@extend(node.any)
@exceptions
def _typeof(self,symtab):
    return 'l'

@extend(node.funcall) # func_expr,args
@exceptions
def _typeof(self,symtab):
    func_obj = symtab[self.func_expr.name]
    #if not self.ret:
    return func_obj.apply(self.args,symtab)
    #return ['' for i in self.ret]

@extend(node.ravel)
@exceptions
def _typeof(self,symtab):
    return self.args[0]._typeof(symtab)

@extend(node.continue_stmt)
@extend(node.break_stmt)
@exceptions
def _typeof(self,symtab):
    pass
