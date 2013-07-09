import node
from node import extend,exceptions

def typerank(t):
    t._type()

i=0

@extend(node.node)
@exceptions
def _type(self):
    global i
    for u in self:
        i += 1
        u._type()

@extend(node.func_decl)
@exceptions
def _type(self):
    pass # ignore ident,args,ret

@extend(node.let)
@exceptions
def _type(self):
    if self.ret.__class__ is node.ident:
        self.ret._t = self.args._type()
        print self.ret.name, self.ret.lineno, "'%s'" % self.ret._t

@extend(node.param)
@exceptions
def _type(self):
    return ''

@extend(node.param)
@exceptions
def _rank(self):
    return ''
          
@extend(node.ident)
@exceptions
def _type(self):
    if self.defs is None:
        try:
            return self._t
        except:
            print '+++ missing type for',self.name,self.lineno
            return '+' 
    ts = set([u._type() for u in self.defs if u.defs is None])
    if len(ts) == 0:
        print '+++','No definition to variable',self.name,self.lineno
        return '+'
    if len(ts) == 1:
        return ts.pop()
    if len(ts) > 1:
        print '+++','Conflicting defs for', self.name,self.lineno
    return ''

@extend(node.matrix)
@exceptions
def _type(self):
    ts = set([u._type() for u in self.args])
    if len(ts) == 1:
        return ts.pop()
    return ''

#    ts = set([u._type() for u in self])
#    if len(ts) == 1:
#        return ts.pop()
#    return ''

@extend(node.arrayref) 
@exceptions
def _type(self):
    return self.func_expr._type()

@extend(node.arrayref) 
@exceptions
def _rank(self):
    return max(0,self.func_expr._rank()-len(self.args))

@extend(node.expr)
@exceptions
def _rank(self):
    nargs = len(self.args)
    if nargs == 0:
        assert 0
    elif nargs == 1:
        return self.args[0]._rank()
    elif nargs == 2:
        return max(self.args[0]._rank(),
                   self.args[1]._rank())
    assert 0

@extend(node.expr)
@exceptions
def _type(self):
    try:
        nargs = len(self.args)
        if nargs == 0:
            assert 0
        if nargs == 1:
            if self.op in ("!","~"):
                return 'l'
            assert 0, self.op
        if len(self.args) == 2:
            if self.op == ":":
                return 'i'
            if self.op in ("<",">","<=",">=","==","!=","&&","||"):
                return 'l'
            if self.op in ("*","+","-","/","^",".*","./"):
                if (self.args[0]._type() == 'i' and 
                    self.args[1]._type() == 'i'):
                    return 'i'
                else:
                    return 'd'
    except:
        assert 0

@extend(node.add)
@extend(node.sub)
@exceptions
def _type(self):
    if (self.args[0]._type() == 'i' and 
        self.args[1]._type() == 'i'):
        return 'i'
    else:
        return 'd'

@extend(node.ones)
@extend(node.zeros)
@exceptions
def _type(self):
    if self.args[-1]._type() == 'c':
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
@extend(node.sum)
@extend(node.transpose)
@exceptions
def _type(self):
    try:
        return self.args[0]._type()
    except:
        raise

@extend(node.number)
@exceptions
def _type(self):
    return 'i' if isinstance(self.value,int) else 'd'

@extend(node.RAND)
@extend(node.rand)
@extend(node.inf)
@exceptions
def _type(self):
    return 'd'

@extend(node.rem)
@extend(node.SIZE)
@extend(node.size)
@extend(node.find)
#@extend(node.findone)
@exceptions
def _type(self):
    return 'ii'

@extend(node.sort)
@exceptions
def _type(self):
    if len(self.ret) == 2:
        return self.args[0]._type() + 'i'
    if i==1:
        return 'i'
    return ''

@extend(node.floor)
@extend(node.length)
@extend(node.range)
@exceptions
def _type(self):
    return 'i'

@extend(node.false)
@extend(node.true)
@extend(node.isequal)
@extend(node.isnan)
@extend(node.all)
@extend(node.any)
@exceptions
def _type(self):
    return 'l'

@extend(node.funcall)
@exceptions
def _type(self):
    return ''

@extend(node.ravel)
@exceptions
def _type(self):
    return self.args[0]._type()

