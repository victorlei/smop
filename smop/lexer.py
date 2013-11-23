# SMOP compiler -- Simple Matlab/Octave to Python compiler
# Copyright 2011-2013 Victor Leikehman

import sys
import re
from zlib import adler32
import lex
from lex import TOKEN


tokens = [
    "AND", "ANDAND", "BACKSLASH", "COLON", "COMMA", 
    "DIV", "DOT", "DOTDIV", "DOTEXP", "DOTMUL", "END_EXPR",
    "END_STMT", "EQ", "EXP", "FIELD", "GE", "GT", "HANDLE", "IDENT",
    "LBRACE", "LBRACKET", "LE", "LPAREN", "LT", "MINUS", "MUL", "NE",
    "NEG", "NUMBER", "OR" , "OROR", "PLUS", "RBRACE", "RBRACKET",
    "RPAREN", "SEMI", "STRING", "TRANSPOSE", "COMMENT"
]

reserved = {
    "break"    : "BREAK",
    "case"     : "CASE",
    "catch"    : "CATCH",
    "continue" : "CONTINUE",
    "else"     : "ELSE",
    "elseif"   : "ELSEIF",
    "for"      : "FOR",
    "function" : "FUNCTION",
    "global"   : "GLOBAL",
    "if"       : "IF",
    "otherwise": "OTHERWISE",
    "switch"   : "SWITCH",
    "try"      : "TRY",
    "while"    : "WHILE",
    "return"   : "RETURN",
    }
tokens += list(reserved.values())
literals = "="

def new():
    t_AND    = r"\&"
    t_ANDAND = r"\&\&"
    t_BACKSLASH = r"\\"
    t_COLON  = r":"
    t_DIV    = r"\/"
    t_DOT    = r"\."
    t_DOTDIV = r"\./"
    t_DOTEXP = r"\.\^"
    t_DOTMUL = r"\.\*"
    t_EQ     = r"=="
    t_EXP    = r"\^"
    t_GE     = r">="
    t_GT     = r"\>"
    t_HANDLE = r"\@"
    t_LE     = r"<="
    t_LT     = r"\<"
    t_MINUS  = r"\-"
    t_MUL    = r"\*"
    t_NE     = r"~="
    t_NEG    = r"\~"
    t_OR     = r"\|"
    t_OROR   = r"\|\|"
    t_PLUS   = r"\+"
    
    states = (("matrix","inclusive"),
              ("afterkeyword","exclusive"))

    #ws1 = r"(\s|\.\.\..*\n|%.*)+"
    ws0 = r"(\s|%.*\n|\.\.\..*\n)*"
    
    def t_afterkeyword_STRING(t):
        r"'([^']|(''))*'"
        t.value = t.value[1:-1].replace("''","'")
        t.lexer.begin("INITIAL")
        return t

    def t_afterkeyword_error(t):
        raise SyntaxError

    # A quote, immediately following any of: (1) an alphanumeric
    # charater, (2) right bracket, parenthesis or brace,
    # or (3) another TRANSPOSE, is a TRANSPOSE.  Otherwise, it starts a
    # string.  The order of the rules for TRANSPOSE (first) and STRING
    # (second) is important.  Luckily, if the quote is separated from
    # the term by line continuation (...), matlab starts a string, so
    # the above rule still holds.

    def t_TRANSPOSE(t):
        r"(?<=\w|\]|\)|\})((\.')|')+"
        # <---context ---><-quotes->
        # We let the parser figure out what that mix of quotes and
        # dot-quotes, which is kept in t.value, really means.
        return t

    def t_STRING(t):
        r"'([^']|(''))*'"
        t.value = t.value[1:-1].replace("''","'")
        return t

    def t_IDENT(t):
        r"(\.([ \t]|\.\.\..*\n)*)?[a-zA-Z_][a-zA-Z_0-9]*"
        #    <- space  idiom ->
        if t.value[0] == ".":
            # Reserved words are not reserved when used as fields.
            # So return=1 is illegal, but foo.return=1 is fine.
            t.type = "FIELD"
            return t
        if t.value == "end":
            if t.lexer.parens > 0 or t.lexer.brackets > 0 or t.lexer.braces > 0:
                t.type = "END_EXPR"
            else:
                t.type = "END_STMT"
        else:
            t.type = reserved.get(t.value,"IDENT")
            if t.type != "IDENT" and t.lexer.lexdata[t.lexer.lexpos]=="'":
                t.lexer.begin("afterkeyword")
        return t


    def t_LPAREN(t):
        r"\("
        t.lexer.parens += 1
        return t

    def t_RPAREN(t):
        r"\)"
        t.lexer.parens -= 1
        return t

    @TOKEN(ws0+r"\]")
    def t_RBRACKET(t):
        # compare w t_LBRACKET
        #r"(\s|\.\.\..*\n)*\]" # eating all whitespace, including vertical and continuation
        t.lexer.lineno += t.value.count("\n")
        t.lexer.brackets -= 1
        if t.lexer.brackets+t.lexer.braces == 0:
            t.lexer.begin("INITIAL")
        return t

    @TOKEN(r"\["+ws0)
    def t_LBRACKET(t):
        # compare w t_SEMI
        #r"\[(\s|\.\.\..*\n|%.*)*" # eating all whitespace, including vertical and continuation
        t.lexer.lineno += t.value.count("\n")
        t.lexer.brackets += 1
        if t.lexer.brackets+t.lexer.braces == 1:
            t.lexer.begin("matrix")
        return t

    # maybe we need a dedicated CELLARRAY state ???
    @TOKEN(ws0+r"\}")
    def t_RBRACE(t):
        #r"[ \t]*\}"
        t.lexer.braces -= 1
        if t.lexer.braces+t.lexer.brackets == 0:
            t.lexer.begin("INITIAL")
        return t

    @TOKEN(r"\{"+ws0)
    def t_LBRACE(t):
        #r"\{[ \t]*"
        t.lexer.braces += 1
        if t.lexer.brackets+t.lexer.braces == 1:
            t.lexer.begin("matrix")
        return t

    @TOKEN(r","+ws0)
    def t_COMMA(t):
        #r",[ \t]*" # eating spaces is important inside brackets
        t.lexer.lineno += t.value.count("\n")
        if (t.lexer.brackets == 0 and
            t.lexer.parens == 0 and
            t.lexer.braces == 0):
            t.type = "SEMI"
            return t
        return t

    @TOKEN(r"\;"+ws0)
    def t_SEMI(t):
        #r"\;(\s|\.\.\..*\n|%.*)*" # eating all whitespace, including vertical and continuation
        t.lexer.lineno += t.value.count("\n")
#        if t.lexer.brackets or t.lexer.braces > 0:
#            t.type = "CONCAT"
        return t

    def t_NUMBER(t):
        r"(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?[ij]?"
        if t.value[-1] == 'i':
            t.value = t.value[:-1]+'j'
        t.value = eval(t.value)
        return t

    def t_NEWLINE(t):
        r'\n+'
        t.lexer.lineno += len(t.value)
        t.value = ";"
        t.type = "SEMI"
        return t

    def t_COMMENT(t):
        r"%.*"
        t.lexer.lineno += 1
        t.value = '#' +t.value[1:]
        return t


#    @TOKEN(ws+r"(?=[-+]\S)")    
#    def t_matrix_WHITESPACE(t):
#        #r"\s+(?=[-+]\S)"
#        # Whitespace, followed by + or - followed by anything but whitespace
#        t.lexer.lineno += t.value.count("\n")
#        t.type = "COMMA"
#        return t

    def t_matrix_FOO(t):
        r"(?<=[])}'.]|\w)(\s|\.\.\..*\n|%.*)+(?=[-+]?([[({']|\w|\.\d))"
        # <-----term----><---whitespace-----><----term------->

        # This catches a whitespace separating two terms, without an
        # operator between them.  Terms end with (1) an alphanumeric
        # charater \w, (2) quote, (3) right parenthesis, bracket, or
        # brace, and (4) a dot (after a number, such as 3. ).
        #
        # The pattern for whitespace account for ellipsis as a
        # whitespace, and for the trailing junk.
        #
        # Terms start with (1) an alphanumeric character, (2) quote,
        # (3) left paren, bracket, or brace and finally (4) a dot
        # before a digit, such as .9  .

        # TODO: what about curly brackets ???
        # TODO: what about dot followed by a letter, as in field
        #   [foo  .bar]
        
        t.lexer.lineno += t.value.count("\n")
        t.type = "COMMA"
        return t

    def t_ELLIPSIS(t):
        r"\.\.\..*\n"
        t.lexer.lineno += 1
        pass

    def t_SPACES(t):
        r"[ \t\r]+"
        pass

    def t_error(t):
        print "Illegal character '%s'" % t.value[0]
        t.lexer.skip(1)

    lexer = lex.lex(reflags=re.I)
    lexer.brackets = 0  # count open square brackets
    lexer.parens = 0    # count open parentheses
    lexer.braces = 0    # count open curly braces
    return lexer

if __name__ == "__main__":
    lexer = new()
    while 1:
        line = sys.stdin.readline()
        if not line:
            break
        lexer.input(line)
        for tok in lexer:
            print tok
