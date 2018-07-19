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

import node
import options
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

#list of functions handled with func_convert
func_conversions = [
    "clear",
    "close_",
    "csvread",
    "dot",
    "erf",
    "exist",
    "exp",
    "figure",
    "iscellstr",
    "length",
    "log",
    "mean",
    "multiply",
    "num2str",
    "numel",
    "plot",
    "size",
    "sqrt",
    "std",
    "strcmp",
    "sum",
    "textscan"
    ]

#add entries to list of function names to be converted (func_conversions)
def func_convert(funcall,level):
    funname = funcall.func_expr._backend()
    args = commasplit(funcall.args._backend())
    if funname == "clear":
        return ""
    elif funname == "close_":
        return "plt.close("+args[0]+")"
    elif funname == "csvread":
        if len(args) == 1:
            return "np.nan_to_num(np.genfromtxt("+args[0]+", delimiter = ','), copy = False)"
        elif len(args) < 4:
            if len(args) == 2:
                args.append('')
            return "np.nan_to_num(np.genfromtxt("+args[0]+", delimiter = ',')["+args[1]+":,"+args[2]+":], copy = False)"
        else:
            indices = args[3][1:len(args[3])-1].split(',')
            return "np.nan_to_num(np.genfromtxt("+args[0]+", delimiter = ',')["+args[1]+":"+indices[2]+","+args[2]+":"+indices[3]+"], copy = False)"
    elif funname == "dot":
        return "np.dot("+args[0]+","+args[1]+")"
    elif funname == "erf":
        return "m.erf("+args[0]+")"
    elif funname == "exist":
        if args[1] == "'file'":
            return "os.path.isfile("+args[0]+")"
        raise Exception("no defined behavior for exist with arguments "+str(args))
    elif funname == "exp":
        return "m.exp("+args[0]+")"
    elif funname == "figure":
        return "plt.figure("+args[0]+")"
    elif funname == "iscellstr":
        return "isinstance("+args[0]+", str)"
    elif funname == "length":
        return args[0]+".size"
    elif funname == "log":
        return "np.log("+args[0]+")"
    elif funname == "mean":
        return "np.mean("+args[0]+")"
    elif funname == "multiply":
        return "np.multiply("+args[0]+","+args[1]+")"
    elif funname == "num2str":
        return "str("+args[0]+")"
    elif funname == "numel":
        return args[0]+".size"
    elif funname == "plot":
        return "plt.plot("+funcall.args._backend()+")"
    elif funname == "size":
        return args[0]+".shape["+args[1]+"-1]"
    elif funname == "sqrt":
        return "m.sqrt("+args[0]+")"
    elif funname == "std":
        return "np.std("+args[0]+", ddof = 1)"
    elif funname == "strcmp":
        return "("+args[0]+" == "+args[1]+")"
    elif funname == "sum":
        return "np.sum("+args[0]+")"
    elif funname == "textscan":
        if args[1] == "'%s'" and args[2] == "'delimiter'":
            args[0] = args[0].replace('char(','str(')
            return "np.asarray(["+args[0]+".split("+args[3]+")],dtype='object').T"
        raise Exception("no defined behavior for textscan with arguments "+str(args))
    else:
        raise Exception("func_convert called with unexpected function name '"+funname+"'")
    
#returns a string split by commas not enclosed by brackets or parentheses    
def commasplit(s):
    split = ['']
    brackets = 0
    parens = 0
    index = 0
    for char in s:
        if char == '[':
            brackets += 1
        elif char == ']':
            brackets -= 1
        elif char == '(':
            parens += 1
        elif char == ')':
            parens -= 1
        
        if char == ',' and parens == 0 and brackets == 0:
            split.append('')
            index+=1
        else:
            split[index] += char
    return split

def backend(t,*args,**kwargs):
    return t._backend(level=0,*args,**kwargs)


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
    
def is_tab_empty(tab):
    for elem in tab:
        if elem != "":
            return False
    return True

def compute_indexing(s):
    """
    This function makes the correlation between matlab indexing and python indexing
    Which specifications are listed below :
        - Python is zero-indexed
        - MATLAB is 1-indexed
        - MATLAB slice notation includes the endpoint
        - Python slice notation excludes the endpoint
        - MATLAB start:step:stop
        - Python start:stop:step
    :param s: string containing the content between squared brackets
    :return: returns the string being parsed and analysed
    """
    decomposition = s.split(",")
    k = 0
    for element in decomposition:
        if element.isnumeric():
            decomposition[k] = str(int(element) - 1)
        elif element.find(':') != -1:
            tab = element.split(':')
            if is_tab_empty(tab):
                k += 1
                continue
            start = 0
            if len(tab) == 2:
                step = 1
                stop = 2
            if len(tab) == 3:
                step = 2
                stop = 3
            try:
                if tab[start].isnumeric():
                    tab[start] = str(int(tab[start]) - 1)
                else:
                    tab[start] = str(tab[start]) + "-1"
            except TypeError:
                if tab[start].isnumeric():
                    tab[start] = str(int(tab[start]) - 1)
                else:
                    tab[start] = str(tab[start]) + "-1"
            for i in range(start, stop, step):
                if tab[i].find("end") != -1:
                    tab[i] = tab[i].replace('end()', "-1")
                    tab[i] = tab[i].replace('end', "-1")
            if len(tab) == 3:
                tmp = tab[1]
                tab[1] = tab[2]
                tab[2] = tmp
            decomposition[k] = ":".join(tab)
        else:
            decomposition[k] += "-1"
        k += 1
    return ",".join(decomposition)

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
    return "%s[%s]" % (self.func_expr._backend(),
                       compute_indexing(self.args._backend()))

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
                       compute_indexing(self.args._backend()))

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
        #regular expression to replace comma not inside brackets or parentheses
        temp = str.join(':', commasplit(self.args._backend()))
        if temp == "":
            temp = ":"
        return temp
    
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
    if isinstance(self.expr, node.expr_list):
        if len(self.expr) == 1 and isinstance(self.expr[0], node.ident):
            return "from "+str(self.expr[0])+" import *"
    return self.expr._backend()

@extend(node.for_stmt)
def _backend(self,level=0):
    temp = self.expr._backend().split(':')
    if len(temp) == 3:
        temp[1], temp[2] = temp[2], temp[1]
    temp[1] = str(temp[1])+'+1'
    return "for "+self.ident._backend()+" in range("+','.join(temp)+"):"+self.stmt_list._backend(level+1)

@extend(node.func_stmt)
def _backend(self,level=0):
    s = """
def %s(%s):
""" % (self.ident._backend(),
       self.args._backend())
    return s

@extend(node.funcall)
def _backend(self,level=0):
    #import pdb; pdb.set_trace()
    if self.func_expr._backend() in func_conversions:
        return func_convert(self,level)
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
    elif self.ret.__class__ in [node.arrayref, node.cellarrayref]:
        temp = self.ret._backend()
        s += temp[:temp.index('[')]+" = smop_util.safe_set("+temp[:temp.index('[')]+",("+temp[temp.index('[')+1:temp.rindex(']')]+",),"+self.args._backend()+")"
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
        return "np.asarray([], dtype='object')"
    elif any(a.__class__ is node.string for a in self.args[0]):
        return " + ".join(a._backend() for a in self.args[0])
    else:
        #import pdb; pdb.set_trace()
        return "np.asarray([%s], dtype='object')" % self.args[0]._backend()

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
    out = ''
    for t in self:
        out += sep
        if isinstance(t, node.func_stmt):
            level += 1
            sep = "\n"+indent*level
        elif isinstance(t, node.return_stmt):
            level -= 1
            sep = "\n"+indent*level
        out += t._backend(level)
    return out

@extend(node.string)
def _backend(self,level=0):
    try:
        return "'%s'" % str(self.value).encode("string_escape")
    except:
        return "'%s'" % str(self.value)

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