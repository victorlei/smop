# SMOP compiler -- Simple Matlab/Octave to Python compiler
# Copyright 2011-2014 Victor Leikehman

import sys
import re
from zlib import adler32
import lex
from lex import TOKEN
import readline

class IllegalCharacterError(Exception):
    pass

tokens = [
    "AND", "ANDAND", "ANDEQ", "BACKSLASH", "COLON", "COMMA", "DIV","DIVEQ",
    "DOT", "DOTDIV", "DOTDIVEQ", "DOTEXP", "DOTMUL","DOTMULEQ", "END_EXPR",
    "END_STMT", "EQ", "EQEQ", "EXP", "EXPEQ", "FIELD", "GE", "GT", "HANDLE",
    "IDENT", "LBRACE", "LBRACKET", "LE", "LPAREN", "LT",
    "MINUS","MINUSMINUS","MINUSEQ","MUL","MULEQ","NE", "NEG",
    "NUMBER", "OR","OREQ", "OROR", "PLUS", "PLUSEQ","PLUSPLUS",
    "RBRACE", "RBRACKET", "RPAREN", "SEMI", "STRING", "TRANSPOSE",
]

reserved = {
    "break"                  : "BREAK",
    "case"                   : "CASE",
    "catch"                  : "CATCH",
    "continue"               : "CONTINUE",
    "else"                   : "ELSE",
    "elseif"                 : "ELSEIF",
    "end_unwind_protect"     : "END_UNWIND_PROTECT",
    "for"                    : "FOR",
    "function"               : "FUNCTION",
    "global"                 : "GLOBAL",
    "if"                     : "IF",
    "otherwise"              : "OTHERWISE",
    "persistent"             : "PERSISTENT",
    "return"                 : "RETURN",
    "switch"                 : "SWITCH",
    "try"                    : "TRY",
    "unwind_protect"         : "UNWIND_PROTECT",
    "unwind_protect_cleanup" : "UNWIND_PROTECT_CLEANUP",
    "while"                  : "WHILE",
    }
tokens += list(reserved.values())
#literals = "="

def new():
    t_AND         = r"\&"
    t_ANDAND      = r"\&\&"
    t_ANDEQ       = r"\&="
    t_BACKSLASH   = r"\\"
    t_COLON       = r":"
    t_DIV         = r"\/"
    t_DIVEQ       = r"\/="
    t_DOT         = r"\."
    t_DOTDIV      = r"\./"
    t_DOTDIVEQ    = r"\./="
    t_DOTEXP      = r"\.\^"
    t_DOTMUL      = r"\.\*"
    t_DOTMULEQ    = r"\.\*="
    t_EQ          = r"="
    t_EQEQ        = r"=="
    t_EXP         = r"\^"
    t_EXPEQ       = r"\^="
    t_GE          = r">="
    t_GT          = r"\>"
    t_HANDLE      = r"\@"
    t_LE          = r"<="
    t_LT          = r"\<"
    t_MINUS       = r"\-"
    t_MINUSEQ     = r"\-="
    t_MINUSMINUS  = r"\--"
    t_MUL         = r"\*"
    t_MULEQ       = r"\*="
    t_NE          = r"(~=)|(!=)"
    t_NEG         = r"\~|\!"
    t_OR          = r"\|"
    t_OREQ        = r"\|="
    t_OROR        = r"\|\|"
    t_PLUS        = r"\+"
    t_PLUSEQ      = r"\+="
    t_PLUSPLUS    = r"\+\+"
    
    states = (("matrix","inclusive"),
              ("afterkeyword","exclusive"))

    ws  = r"(\s|(\#|%).*\n|\.\.\..*\n|\\\n)"
    ws1 = ws+"+"
    ws0 = ws+"*"
    ms  = r"'([^']|(''))*'" 
    os  = r'"([^"\a\b\f\r\t\0\v\n\\]|(\\[abfn0vtr\"\n\\])|(""))*"'
    mos = "(%s)|(%s)" % (os,ms)
    id  = r"[a-zA-Z_][a-zA-Z_0-9]*"
    
    def unescape(s):
        if s[0] == "'":
            return s[1:-1].replace("''","'")
        else:
            return s[1:-1].replace('""','"')
            #return s[1:-1].decode('string_escape').replace('""','"')

    @TOKEN(mos)
    def t_afterkeyword_STRING(t):
        t.value = unescape(t.value)
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

    @TOKEN(mos)
    def t_STRING(t):
        t.value = unescape(t.value)
        return t

    @TOKEN(r"(\.%s)?%s" % (ws0,id))
    def t_IDENT(t):
        t.lexer.lineno += t.value.count("\n")
        if t.value[0] == ".":
            # Reserved words are not reserved when used as fields.
            # So return=1 is illegal, but foo.return=1 is fine.
            t.type = "FIELD"
            return t
        if t.value in ("endwhile","endfunction","endif","endfor",
                       "endswitch","end_try_catch"): # octave
            t.type = "END_STMT"
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
    def t_RBRACKET(t): # compare w t_LBRACKET
        t.lexer.lineno += t.value.count("\n")
        t.lexer.brackets -= 1
        if t.lexer.brackets+t.lexer.braces == 0:
            t.lexer.begin("INITIAL")
        return t

    @TOKEN(r"\["+ws0)
    def t_LBRACKET(t): # compare w t_SEMI
        t.lexer.lineno += t.value.count("\n")
        t.lexer.brackets += 1
        if t.lexer.brackets+t.lexer.braces == 1:
            t.lexer.begin("matrix")
        return t

    # maybe we need a dedicated CELLARRAY state ???
    @TOKEN(ws0+r"\}")
    def t_RBRACE(t):
        t.lexer.lineno += t.value.count("\n")
        t.lexer.braces -= 1
        if t.lexer.braces+t.lexer.brackets == 0:
            t.lexer.begin("INITIAL")
        return t

    @TOKEN(r"\{"+ws0)
    def t_LBRACE(t):
        t.lexer.lineno += t.value.count("\n")
        t.lexer.braces += 1
        if t.lexer.brackets+t.lexer.braces == 1:
            t.lexer.begin("matrix")
        return t

    @TOKEN(r","+ws0)
    def t_COMMA(t):  # eating spaces is important inside brackets
        t.lexer.lineno += t.value.count("\n")
        if (t.lexer.brackets == 0 and
            t.lexer.parens == 0 and
            t.lexer.braces == 0):
            t.type = "SEMI"
            return t
        return t

    @TOKEN(r"\;"+ws0)
    def t_SEMI(t):
        t.lexer.lineno += t.value.count("\n")
#        if t.lexer.brackets or t.lexer.braces > 0:
#            t.type = "CONCAT"
        return t

    def t_NUMBER(t):
        r"(0x[0-9A-Fa-f]+)|((\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?[ij]?)"
        if t.value[-1] == 'i':
            t.value = t.value[:-1]+'j'
        t.value = eval(t.value)
        return t

    def t_NEWLINE(t):
        r'\n+'
        t.lexer.lineno += len(t.value)
        if not t.lexer.parens and not t.lexer.braces:
            t.value = ";"
            t.type = "SEMI"
            return t

    def t_comment(t):
        r"(%|\#).*"
        pass


#    @TOKEN(ws+r"(?=[-+]\S)")    
#    def t_matrix_WHITESPACE(t):
#        #r"\s+(?=[-+]\S)"
#        # Whitespace, followed by + or - followed by anything but whitespace
#        t.lexer.lineno += t.value.count("\n")
#        t.type = "COMMA"
#        return t

    @TOKEN(r"(?<=\w)" + ws1 + r"(?=\()")
    def t_matrix_BAR(t):
        # Consume whitespace which follows end of name
        # and is followed a left paren.  This properly handles
        # a space between a func name and the arguments
        pass

    tend = r"(?<=[])}'\".]|\w)"
    tbeg = r"(?=[-+]?([[({'\"]|\w|\.\d))"
    @TOKEN(tend+ws1+tbeg)
    def t_matrix_FOO(t):
        # In matrix state, consume whitespace separating two
        # terms and return a fake COMMA token.  This allows
        # parsing [1 2 3] as if it was [1,2,3].  Handle
        # with care: [x + y] vs [x +y] 
        #
        # A term T is
        # (a) a name or a number
        # (b) literal string using single or doble quote
        # (c) (T) or [T] or {T} or T' or +T or -T
        #
        # Terms end with
        # (1) an alphanumeric charater \w
        # (2) single quote (in octave also double-quote)
        # (3) right parenthesis, bracket, or brace
        # (4) a dot (after a number, such as 3. 
        #
        # The pattern for whitespace accounts for ellipsis as a
        # whitespace, and for the trailing junk.
        #
        # Terms start with 
        # (1) an alphanumeric character
        # (2) a single or double quote,
        # (3) left paren, bracket, or brace and finally
        # (4) a dot before a digit, such as .3  .

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
        r"(\\\n|[ \t\r])+"
        pass

    def t_error(t):
        column=t.lexer.lexpos - t.lexer.lexdata.rfind("\n",0,t.lexer.lexpos)
        raise IllegalCharacterError(t.lineno,column,t.value[0])

    lexer = lex.lex(reflags=re.I)
    lexer.brackets = 0  # count open square brackets
    lexer.parens = 0    # count open parentheses
    lexer.braces = 0    # count open curly braces
    return lexer

if __name__ == "__main__":
    lexer = new()
    try:
        while 1:
            line = raw_input("=>> ")
            if not line:
                continue
            while line[-1] == "\\":
                line = line[:-1] + raw_input("... ")
            print len(line), [c for c  in line]
            lexer.input(line)
            for tok in lexer:
                print len(str(tok.value)), tok
    except EOFError:
        pass
