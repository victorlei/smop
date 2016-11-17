# SMOP compiler -- Simple Matlab/Octave to Python compiler
# Copyright 2011-2016 Victor Leikehman

import ply.yacc as yacc
import lexer
from lexer import tokens, raise_exception
import node
from node import exceptions
import options

# ident properties (set in parse.py)
# ----------------------------------
# G global
# P persistent
# A function argument
# F function return value
# I for-loop iteration index
#
# ident properties (set in resolve.py)
# ------------------------------------
# R ref    =...a  or  =...a(b)
# D def    a=...  or   [a,b,c]=...
# U update a(b)=...  or  [a(b) c(d)]=...


class error(Exception):
    pass


class syntax_error(error):
    pass


precedence = (
    ("right", "COMMA"),
    ("right", "DOTDIVEQ", "DOTMULEQ", "EQ", "EXPEQ", "MULEQ", "MINUSEQ",
     "DIVEQ", "PLUSEQ", "OREQ", "ANDEQ"),
    ("nonassoc", "HANDLE"),
    ("left", "COLON"),
    ("left", "ANDAND", "OROR"),
    ("left", "EQEQ", "NE", "GE", "LE", "GT", "LT"),
    ("left", "OR", "AND"),
    ("left", "PLUS", "MINUS"),
    ("left", "MUL", "DIV", "DOTMUL", "DOTDIV", "BACKSLASH"),
    ("right", "UMINUS", "NEG"),
    ("right", "TRANSPOSE"),
    ("right", "EXP", "DOTEXP", "POW"),
    ("nonassoc", "LPAREN", "RPAREN", "RBRACE", "LBRACE"),
    ("left", "FIELD", "DOT", "PLUSPLUS", "MINUSMINUS"), )


def p_top(p):
    """
    top :
        | top stmt
      """
    if len(p) == 1:
        p[0] = node.stmt_list()
    else:
        p[0] = p[1]
        p[0].append(p[2])


def p_end(p):
    """
    top : top END_STMT
    """
    p[0] = p[1]


def p_end_function(p):
    """
    top : top END_FUNCTION
    """
    p[0] = p[1]
    p[0].append(node.return_stmt(ret=ret_expr))
    p[0].append(node.comment_stmt("\nif __name__ == '__main__':\n    pass"))


@exceptions
def p_arg1(p):
    """
    arg1 : STRING
         | NUMBER
         | IDENT
         | GLOBAL
    """
    # a hack to support "clear global"
    p[0] = node.string(value=str(p[1]), lineno=p.lineno(1), lexpos=p.lexpos(1))


@exceptions
def p_arg_list(p):
    """
    arg_list : ident_init_opt
             | arg_list COMMA ident_init_opt
    """
    if len(p) == 2:
        p[0] = node.expr_list([p[1]])
    elif len(p) == 4:
        p[0] = p[1]
        p[0].append(p[3])
    else:
        assert 0
    assert isinstance(p[0], node.expr_list)


@exceptions
def p_args(p):
    """
    args : arg1
         | args arg1
    """
    if len(p) == 2:
        p[0] = node.expr_list([p[1]])
    else:
        p[0] = p[1]
        p[0].append(p[2])


@exceptions
def p_break_stmt(p):
    "break_stmt : BREAK SEMI"
    p[0] = node.break_stmt(None)


@exceptions
def p_case_list(p):
    """
    case_list :
              | CASE expr sep stmt_list_opt case_list
              | CASE expr error stmt_list_opt case_list
              | OTHERWISE stmt_list
    """
    if len(p) == 1:
        p[0] = node.stmt_list()
    elif len(p) == 3:
        assert isinstance(p[2], node.stmt_list)
        p[0] = p[2]
    elif len(p) == 6:
        p[0] = node.if_stmt(
            cond_expr=node.expr(
                op="==", args=node.expr_list([p[2]])),
            then_stmt=p[4],
            else_stmt=p[5])
        p[0].cond_expr.args.append(
            None)  # None will be replaced using backpatch()
    else:
        assert 0


@exceptions
def p_cellarray(p):
    """
    cellarray : LBRACE RBRACE
              | LBRACE expr_list RBRACE
              | LBRACE concat_list RBRACE
              | LBRACE concat_list SEMI RBRACE
    """
    if len(p) == 3:
        p[0] = node.cellarray(op="{}", args=node.expr_list())
    else:
        p[0] = node.cellarray(op="{}", args=p[2])


@exceptions
def p_cellarrayref(p):
    """expr : expr LBRACE expr_list RBRACE
            | expr LBRACE RBRACE
    """
    args = node.expr_list() if len(p) == 4 else p[3]
    assert isinstance(args, node.expr_list)
    p[0] = node.cellarrayref(func_expr=p[1], args=args)


@exceptions
def p_command(p):
    """
    command : ident args SEMI
    """
    #    if p[1].name == "load":
    #        # "load filename x" ==> "x=load(filename)"
    #        # "load filename x y z" ==> "(x,y,z)=load(filename)"
    #        ret=node.expr_list([node.ident(t.value) for t in p[2][1:]])
    #        p[0] = node.funcall(func_expr=p[1],
    #                            args=node.expr_list(p[2]),
    #                            ret=ret)
    #    else:
    p[0] = node.funcall(p[1], p[2])


####################


@exceptions
def p_comment_stmt(p):
    """
    comment_stmt : COMMENT
    """
    p[0] = node.comment_stmt(p[1])


@exceptions
def p_concat_list(p):
    """
    concat_list : expr_list SEMI expr_list
                | concat_list SEMI expr_list
    """
    if p[1].__class__ == node.expr_list:
        p[0] = node.concat_list([p[1], p[3]])
    else:
        p[0] = p[1]
        p[0].append(p[3])


@exceptions
def p_continue_stmt(p):
    "continue_stmt : CONTINUE SEMI"
    p[0] = node.continue_stmt(None)


@exceptions
def p_elseif_stmt(p):
    """
    elseif_stmt :
                | ELSE stmt_list_opt
                | ELSEIF expr sep stmt_list_opt elseif_stmt
    """
    if len(p) == 1:
        p[0] = node.stmt_list()
    elif len(p) == 3:
        p[0] = p[2]
    elif len(p) == 6:
        p[0] = node.if_stmt(cond_expr=p[2], then_stmt=p[4], else_stmt=p[5])
    else:
        assert 0


@exceptions
def p_error_stmt(p):
    """
    error_stmt : ERROR_STMT SEMI
    """
    p[0] = node.null_stmt()


@exceptions
def p_expr(p):
    """expr : ident
            | end
            | number
            | string
            | colon
            | NEG
            | matrix
            | cellarray
            | expr2
            | expr1
            | lambda_expr
            | expr PLUSPLUS
            | expr MINUSMINUS
    """
    #        | PLUSPLUS ident
    #        | MINUSMINUS ident
    if p[1] == "~":
        p[0] = node.ident(name="__")
    else:
        p[0] = p[1]


@exceptions
def p_expr1(p):
    """expr1 : MINUS expr %prec UMINUS
             | PLUS expr %prec UMINUS
             | NEG expr
             | HANDLE ident
             | PLUSPLUS ident
             | MINUSMINUS ident
    """
    p[0] = node.expr(op=p[1], args=node.expr_list([p[2]]))


@exceptions
def p_expr2(p):
    """expr2 : expr AND expr
             | expr ANDAND expr
             | expr BACKSLASH expr
             | expr COLON expr
             | expr DIV expr
             | expr DOT expr
             | expr DOTDIV expr
             | expr DOTDIVEQ expr
             | expr DOTEXP expr
             | expr DOTMUL expr
             | expr DOTMULEQ expr
             | expr EQEQ expr
             | expr POW expr
             | expr EXP expr
             | expr EXPEQ expr
             | expr GE expr
             | expr GT expr
             | expr LE expr
             | expr LT expr
             | expr MINUS expr
             | expr MUL expr
             | expr NE expr
             | expr OR expr
             | expr OROR expr
             | expr PLUS expr
             | expr EQ expr
             | expr MULEQ expr
             | expr DIVEQ expr
             | expr MINUSEQ expr
             | expr PLUSEQ expr
             | expr OREQ expr
             | expr ANDEQ expr
    """
    if p[2] == "=":
        if p[1].__class__ is node.let:
            raise_exception(SyntaxError,
                            "Not implemented assignment as expression",
                            new_lexer)
        # The algorithm, which decides if an
        # expression F(X)
        # is arrayref or funcall, is implemented in
        # resolve.py, except the following lines up
        # to XXX. These lines handle the case where
        # an undefined array is updated:
        #    >>> clear a
        #    >>> a[1:10]=123
        # Though legal in matlab, these lines
        # confuse the algorithm, which thinks that
        # the undefined variable is a function name.
        # To prevent the confusion, we mark these
        # nodes arrayref as early as during the parse
        # phase.
        if p[1].__class__ is node.funcall:
            # A(B) = C
            p[1].__class__ = node.arrayref
        elif p[1].__class__ is node.matrix:
            # [A1(B1) A2(B2) ...] = C
            for e in p[1].args:
                if e.__class__ is node.funcall:
                    e.__class__ = node.arrayref
        # XXX

        if isinstance(p[1], node.getfield):
            # import pdb;pdb.set_trace()
            # A.B=C  setfield(A,B,C)
            p[0] = node.setfield(p[1].args[0], p[1].args[1], p[3])
        else:
            # assert len(p[1].args) > 0
            ret = p[1].args if isinstance(p[1], node.matrix) else p[1]
            p[0] = node.let(ret=ret,
                            args=p[3],
                            lineno=p.lineno(2),
                            lexpos=p.lexpos(2))

            if isinstance(p[1], node.matrix):
                # TBD: mark idents as "P" - persistent
                if p[3].__class__ not in (node.ident, node.funcall
                                          ):  #, p[3].__class__
                    raise_exception(SyntaxError,
                                    "multi-assignment",
                                    new_lexer)
                if p[3].__class__ is node.ident:
                    # [A1(B1) A2(B2) ...] = F     implied F()
                    # import pdb; pdb.set_trace()
                    p[3] = node.funcall(func_expr=p[3], args=node.expr_list())
                # [A1(B1) A2(B2) ...] = F(X)
                p[3].nargout = len(p[1].args[0])
    elif p[2] == "*":
        p[0] = node.funcall(
            func_expr=node.ident("dot"), args=node.expr_list([p[1], p[3]]))
    elif p[2] == ".*":
        p[0] = node.funcall(
            func_expr=node.ident("multiply"),
            args=node.expr_list([p[1], p[3]]))

#    elif p[2] == "." and isinstance(p[3],node.expr) and p[3].op=="parens":
#        p[0] = node.getfield(p[1],p[3].args[0])
#        raise SyntaxError(p[3],p.lineno(3),p.lexpos(3))
    elif p[2] == ":" and isinstance(p[1], node.expr) and p[1].op == ":":
        # Colon expression means different things depending on the
        # context.  As an array subscript, it is a slice; otherwise,
        # it is a call to the "range" function, and the parser can't
        # tell which is which.  So understanding of colon expressions
        # is put off until after "resolve".
        p[0] = p[1]
        p[0].args.insert(1, p[3])
    else:
        p[0] = node.expr(op=p[2], args=node.expr_list([p[1], p[3]]))


@exceptions
def p_expr_colon(p):
    "colon : COLON"
    p[0] = node.expr(op=":", args=node.expr_list())


@exceptions
def p_expr_end(p):
    "end : END_EXPR"
    p[0] = node.expr(
        op="end", args=node.expr_list([node.number(0), node.number(0)]))


@exceptions
def p_expr_ident(p):
    "ident : IDENT"
    global use_nargin, use_varargin
    if p[1] == "varargin":
        use_varargin = 1
    if p[1] == "nargin":
        use_nargin = 1
    # import pdb; pdb.set_trace()
    p[0] = node.ident(
        name=p[1],
        lineno=p.lineno(1),
        lexpos=p.lexpos(1),
        column=p.lexpos(1) - p.lexer.lexdata.rfind("\n", 0, p.lexpos(1)))


@exceptions
def p_ident_init_opt(p):
    """
    ident_init_opt : NEG
                   | ident
                   | ident EQ expr
    """
    if p[1] == '~':
        p[0] = node.ident("__")
    else:
        p[0] = p[1]
    if len(p) == 2:
        p[0].init = node.ident(name="None")
    else:
        p[0].init = p[3]


@exceptions
def p_expr_list(p):
    """
    expr_list : exprs
              | exprs COMMA
    """
    p[0] = p[1]


@exceptions
def p_expr_number(p):
    "number : NUMBER"
    p[0] = node.number(p[1], lineno=p.lineno(1), lexpos=p.lexpos(1))


@exceptions
def p_expr_stmt(p):
    """
    expr_stmt : expr_list SEMI
    """
    assert isinstance(p[1], node.expr_list)
    p[0] = node.expr_stmt(expr=p[1])


@exceptions
def p_expr_string(p):
    "string : STRING"
    p[0] = node.string(p[1], lineno=p.lineno(1), lexpos=p.lexpos(1))


@exceptions
def p_exprs(p):
    """
    exprs : expr
          | exprs COMMA expr
    """
    if len(p) == 2:
        p[0] = node.expr_list([p[1]])
    elif len(p) == 4:
        p[0] = p[1]
        p[0].append(p[3])
    else:
        assert (0)
    assert isinstance(p[0], node.expr_list)


@exceptions
def p_field_expr(p):
    """
    expr : expr FIELD
    """
    p[0] = node.expr(
        op=".",
        args=node.expr_list([
            p[1], node.ident(
                name=p[2], lineno=p.lineno(2), lexpos=p.lexpos(2))
        ]))


@exceptions
def p_foo_stmt(p):
    "foo_stmt : expr OROR expr SEMI"
    expr1 = p[1][1][0]
    expr2 = p[3][1][0]
    ident = expr1.ret
    args1 = expr1.args
    args2 = expr2.args
    p[0] = node.let(ret=ident,
                    args=node.expr("or", node.expr_list([args1, args2])))


@exceptions
def p_for_stmt(p):
    """
    for_stmt : FOR ident EQ expr SEMI stmt_list END_STMT
             | FOR LPAREN ident EQ expr RPAREN SEMI stmt_list END_STMT
             | FOR matrix EQ expr SEMI stmt_list END_STMT
    """
    if len(p) == 8:
        if not isinstance(p[2], node.ident):
            raise_exception(SyntaxError, "Not implemented: for loop", new_lexer)
        p[2].props = "I"  # I= for-loop iteration variable
        p[0] = node.for_stmt(ident=p[2], expr=p[4], stmt_list=p[6])

@exceptions
def p_func_stmt(p):
    """func_stmt : FUNCTION ident lambda_args SEMI
                 | FUNCTION ret EQ ident lambda_args SEMI
    """
    # stmt_list of func_stmt is set below
    # marked with XYZZY
    global ret_expr, use_nargin, use_varargin
    ret_expr = node.expr_list()
    use_varargin = use_nargin = 0

    if len(p) == 5:
        assert isinstance(p[3], node.expr_list)
        p[0] = node.func_stmt(
            ident=p[2],
            ret=node.expr_list(),
            args=p[3],
            stmt_list=node.stmt_list())
        ret_expr = node.expr_list()
    elif len(p) == 7:
        assert isinstance(p[2], node.expr_list)
        assert isinstance(p[5], node.expr_list)
        p[0] = node.func_stmt(
            ident=p[4], ret=p[2], args=p[5], stmt_list=node.stmt_list())
        ret_expr = p[2]
    else:
        assert 0


@exceptions
def p_funcall_expr(p):
    """expr : expr LPAREN expr_list RPAREN
            | expr LPAREN RPAREN
    """
    if (len(p) == 5 and len(p[3]) == 1 and p[3][0].__class__ is node.expr and
            p[3][0].op == ":" and not p[3][0].args):
        # foo(:) => ravel(foo)
        p[0] = node.funcall(
            func_expr=node.ident("ravel"), args=node.expr_list([p[1]]))
    else:
        args = node.expr_list() if len(p) == 4 else p[3]
        assert isinstance(args, node.expr_list)
        p[0] = node.funcall(func_expr=p[1], args=args)


@exceptions
def p_global_list(p):
    """global_list : ident
                   | global_list ident
    """
    if len(p) == 2:
        p[0] = node.global_list([p[1]])
    elif len(p) == 3:
        p[0] = p[1]
        p[0].append(p[2])


@exceptions
def p_global_stmt(p):
    """
    global_stmt : GLOBAL global_list SEMI
                | GLOBAL ident EQ expr SEMI
    """
    p[0] = node.global_stmt(p[2])
    for ident in p[0]:
        ident.props = "G"  # G=global


@exceptions
def p_if_stmt(p):
    """
    if_stmt : IF expr sep stmt_list_opt elseif_stmt END_STMT
            | IF LPAREN expr RPAREN stmt_list_opt elseif_stmt END_STMT
    """
    p[0] = node.if_stmt(cond_expr=p[2], then_stmt=p[4], else_stmt=p[5])


@exceptions
def p_lambda_args(p):
    """lambda_args : LPAREN RPAREN
                   | LPAREN arg_list RPAREN
    """
    p[0] = p[2] if len(p) == 4 else node.expr_list()


@exceptions
def p_lambda_expr(p):
    """lambda_expr : HANDLE lambda_args expr
    """
    p[0] = node.lambda_expr(args=p[2], ret=p[3])


@exceptions
def p_matrix(p):
    """matrix : LBRACKET RBRACKET
              | LBRACKET concat_list RBRACKET
              | LBRACKET concat_list SEMI RBRACKET
              | LBRACKET expr_list RBRACKET
              | LBRACKET expr_list SEMI RBRACKET
    """
    if len(p) == 3:
        p[0] = node.matrix()
    else:
        p[0] = node.matrix(p[2])


@exceptions
def p_null_stmt(p):
    """
    null_stmt : SEMI
              | COMMA
    """
    p[0] = node.null_stmt()


@exceptions
def p_parens_expr(p):
    """
    expr :  LPAREN expr RPAREN
    """
    p[0] = node.expr(op="parens", args=node.expr_list([p[2]]))


@exceptions
def p_persistent_stmt(p):
    """
    persistent_stmt :  PERSISTENT global_list SEMI
                    |  PERSISTENT ident EQ expr SEMI
    """
    p[0] = node.null_stmt()


#    if len(p) == 4:
#        p[0] = node.global_stmt(p[2])
#        for ident in p[0]:
#            ident.props="G"  # G=global
#    else:
#    assert p[2].__class__ in (node.let,node.ident), p[2].__class__
#    p[0] = p[2]
#    #print p[2]


@exceptions
def p_ret(p):
    """
    ret : ident
        | LBRACKET RBRACKET
        | LBRACKET expr_list RBRACKET
    """
    if len(p) == 2:
        p[0] = node.expr_list([p[1]])
    elif len(p) == 3:
        p[0] = node.expr_list([])
    elif len(p) == 4:
        assert isinstance(p[2], node.expr_list)
        p[0] = p[2]
    else:
        assert 0
    for ident in p[0]:
        ident.props = "F"


# end func_decl


@exceptions
def p_return_stmt(p):
    "return_stmt : RETURN SEMI"
    p[0] = node.return_stmt(ret=ret_expr)


@exceptions
def p_semi_opt(p):
    """
    semi_opt :
             | semi_opt SEMI
             | semi_opt COMMA
    """
    pass


@exceptions
def p_separator(p):
    """
    sep : COMMA
        | SEMI
    """
    p[0] = p[1]


@exceptions
def p_stmt(p):
    """
    stmt : continue_stmt
         | comment_stmt
         | func_stmt
         | break_stmt
         | expr_stmt
         | global_stmt
         | persistent_stmt
         | error_stmt
         | command
         | for_stmt
         | if_stmt
         | null_stmt
         | return_stmt
         | switch_stmt
         | try_catch
         | while_stmt
         | foo_stmt
         | unwind
    """
    # END_STMT is intentionally left out
    p[0] = p[1]
    # print p[0]


@exceptions
def p_stmt_list(p):
    """
    stmt_list : stmt
              | stmt_list stmt
    """
    if len(p) == 2:
        p[0] = node.stmt_list([p[1]] if p[1] else [])
    elif len(p) == 3:
        p[0] = p[1]
        if p[2]:
            p[0].append(p[2])
    else:
        assert 0


@exceptions
def p_stmt_list_opt(p):
    """
    stmt_list_opt :
                  | stmt_list
    """
    if len(p) == 1:
        p[0] = node.stmt_list()
    else:
        p[0] = p[1]


@exceptions
def p_switch_stmt(p):
    """
    switch_stmt : SWITCH expr semi_opt case_list END_STMT
    """

    def backpatch(expr, stmt):
        if isinstance(stmt, node.if_stmt):
            stmt.cond_expr.args[1] = expr
            backpatch(expr, stmt.else_stmt)

    backpatch(p[2], p[4])
    p[0] = p[4]


@exceptions
def p_transpose_expr(p):
    # p[2] contains the exact combination of plain and conjugate
    # transpose operators, such as "'.''.''''".
    "expr : expr TRANSPOSE"
    p[0] = node.transpose(p[1], node.string(p[2]))


@exceptions
def p_try_catch(p):
    """
    try_catch : TRY stmt_list CATCH stmt_list END_STMT
              | TRY stmt_list END_STMT
    """
    assert isinstance(p[2], node.stmt_list)
    # assert isinstance(p[4],node.stmt_list)
    p[0] = node.try_catch(
        try_stmt=p[2],
        catch_stmt=node.stmt_list(),  # FIXME
        finally_stmt=node.stmt_list())


@exceptions
def p_unwind(p):
    """
    unwind : UNWIND_PROTECT stmt_list UNWIND_PROTECT_CLEANUP stmt_list END_UNWIND_PROTECT
    """
    p[0] = node.try_catch(
        try_stmt=p[2], catch_stmt=node.expr_list(), finally_stmt=p[4])


@exceptions
def p_while_stmt(p):
    """
    while_stmt : WHILE expr SEMI stmt_list END_STMT
    """
    assert isinstance(p[4], node.stmt_list)
    p[0] = node.while_stmt(cond_expr=p[2], stmt_list=p[4])


@exceptions
def p_error(p):
    if p is None:
        raise_exception(SyntaxError, "Unexpected EOF", new_lexer)
    if p.type == "COMMENT":
        # print "Discarded comment", p.value
        parser.errok()
        return
    raise_exception(SyntaxError,
                    ('Unexpected "%s"' % p.value),
                    new_lexer)
parser = yacc.yacc(start="top")


@exceptions
def parse(buf):
    global new_lexer  # used in main.main()
    new_lexer = lexer.new()
    p = parser.parse(
        buf, tracking=1, debug=options.debug_parser, lexer=new_lexer)

    if "1" in options.debug:
        for i, pi in enumerate(p):
            print i, pi.__class__.__name__, pi._backend()

    if "p" in options.debug:
        import pdb
        pdb.set_trace()

    for i in range(len(p)):
        if isinstance(p[i], node.func_stmt):
            break
    else:
        return None  # p[i] is a func decl

#    for j in range(i+1,len(p)):
#        if i < j and isinstance(p[j], node.func_stmt):
#            p.insert(j,node.return_stmt(ret=p[i].ret))
#            j += 1
#            i = j
#    p.append(node.return_stmt(ret=p[i].ret))
#
#    if "2" in options.debug:
#        for i,pi in enumerate(p):
#            print i,pi.__class__.__name__,str(pi)[:50]

    return p
