import unittest
import lexer

class TestLexer(unittest.TestCase):
    def setUp(self):
        self.lexer = lexer.new()
        
    def test010(self):
        """Reserved words are not reserved as field names"""
        self.lexer.input("for foo.for")
        t = [tok.type for tok in self.lexer]
        u = ["FOR","IDENT","FIELD"]
        self.assertEqual(t,u)

    def test020(self):
        """Fields are recognized beyond continuation lines"""
        self.lexer.input("foo ... hello \n\t .\t  ... world\n \t ... \n bar")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","FIELD"]
        self.assertEqual(t,u)

    ### commands are now recognized in the parser
    # def test_t03(self):
    #     """Name following another name is a command.  Names might be
    #     separated by whitespace, but must be on the same line . Once
    #     we detect a command, every name (including keywords) is
    #     reported as a string.
    #     """
    #     self.lexer.input("foo bar for")
    #     t = [tok.type for tok in self.lexer]
    #     u = ["COMMAND","STRING","STRING"]
    #     self.assertEqual(t,u)        

    # def test_t03a(self):
    #     self.lexer.input("dir '/windows'")
    #     t = [tok.type for tok in self.lexer]
    #     u = ["COMMAND","STRING"]
    #     self.assertEqual(t,u)        

    def test030(self):
        self.lexer.input("for bar =")
        t = [tok.type for tok in self.lexer]
        u = ["FOR", "IDENT", "EQ"]
        self.assertEqual(t,u)
        
    def test040(self):
        "Ignore anything from ... to end of line"
        self.lexer.input("foo ... hello world \n bar")
        # line continuation
        t = [tok.type for tok in self.lexer]
        u = ["IDENT", "IDENT"]
        self.assertEqual(t,u)

    def test042(self):
        "backslash-newline continues a line"
        s = "foo \\\n bar ... hello\n bzz"
        #print 43,`s`
        #print '#', s.count('\n')
        self.lexer.input(s)
        # line continuation
        t = [tok.type for tok in self.lexer]
        u = ["IDENT", "IDENT", "IDENT"]
        self.assertEqual(t,u)

    def test044(self):
        "backslash-newline continues a line"
        self.lexer.input(r'puts ("hel\"zvvzcvnzvzlozz")')
        # line continuation
        t = [tok.type for tok in self.lexer]
        u = ["IDENT", "LPAREN", "STRING","RPAREN"]
        self.assertEqual(t,u)

    def test050(self):
        self.lexer.input("clear all, figure on")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","IDENT","SEMI","IDENT","IDENT"]
        self.assertEqual(t,u)

    def test060(self):
        self.lexer.input("dir '.', for i=foo")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","STRING","SEMI","FOR","IDENT","EQ","IDENT"]
        self.assertEqual(t,u)

    def test070(self):
        """Line continuation is allowed between command arguments,
        but not between the command and the first argument."""
        self.lexer.input("dir 'hello' ... adfadfsda \n fooobar")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","STRING","IDENT"]
        self.assertEqual(t,u)  
        
    def test080(self):
        """Terms may start and end with a dot, when a floating
        point number starts or ends with a decimal point"""
        self.lexer.input("[1. .2]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","NUMBER","COMMA","NUMBER","RBRACKET"]
        self.assertEqual(t,u)  

    def test090(self):
        """Ops that start with a dot (./ etc) should not
        be understood as terms"""
        self.lexer.input("[1. ./ .2]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","NUMBER","DOTDIV","NUMBER","RBRACKET"]
        self.assertEqual(t,u)  

    def test100(self):
        "Complex constants, such as 1i and 1j"
        self.lexer.input("1i+1j")
        t = [tok.type for tok in self.lexer]
        u = ["NUMBER","PLUS","NUMBER"]
        self.assertEqual(t,u)  
        
    def test110(self):
        "Quotes and backslashes in matlab strings"
        self.lexer.input(r"'hello''world'")
        tok = self.lexer.next()
        self.assertEqual(tok.value,r"hello'world")
    
    def test112(self):
        "Quotes and backslashes in octave strings"
        self.lexer.input(r'"hello\"world"')
        tok = self.lexer.next()
        self.assertEqual(tok.value,r'hello"world')

    def test114(self):
        "Quotes and backslashes in octave strings"
        self.lexer.input('''"hello\
world"''')
        tok = self.lexer.next()
        self.assertEqual(tok.value,r'helloworld')

    def test116(self):
        "Quotes and backslashes in octave strings"
        self.lexer.input(r'"hello\n"')
        tok = self.lexer.next()
        self.assertEqual(tok.value,'hello\n')

    def test118(self):
        "Quotes and backslashes in octave strings"
        self.lexer.input(r'"hello\\world"')
        tok = self.lexer.next()
        self.assertEqual(tok.value,r'hello\world')

    def test119(self):
        "Quotes and backslashes in octave strings"
        self.lexer.input(r'"hello""world"')
        tok = self.lexer.next()
        self.assertEqual(tok.value,r'hello"world')

    def test120(self):
        "d5ef: cell arrays nested in regular arrays"
        self.lexer.input(r"[foo{i} bar]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","IDENT","LBRACE","IDENT","RBRACE","COMMA","IDENT","RBRACKET"]
        self.assertEqual(t,u)  

    def test130(self):
        "d5ef: Another one"
        self.lexer.input("[y / (x - x) ]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","IDENT","DIV","LPAREN","IDENT","MINUS","IDENT","RPAREN","RBRACKET"]
        self.assertEqual(t,u)  

    def test140(self):
        "quote immediatly following a keyword is always a string"
        self.lexer.input("case'abc'")
        t = [tok.type for tok in self.lexer]
        u = ["CASE","STRING"]
        self.assertEqual(t,u)  

    def test150(self):
        s = "g = (x < 3/8) .* (7/8 * x)\\\n + (x >= 3/8 & x < 3/4) .* (29/24 * x - 1/8)\\\n + (x >= 3/4) .* (7/8 * x + 1/8)"
        self.lexer.input(s)
        toklist = [tok.type for tok in self.lexer]
        self.assertEqual(True, "BACKSLASH" not in toklist)
        self.assertEqual(True, "SEMI" not in toklist)

    def test160(self):
        "comment must leave a newline"
        s = """if (ld > 127)   # convert 16 to 8 bit
               if (ld < 16384)
            """
        self.lexer.input(s)
        t = [tok.type for tok in self.lexer]
        self.assertEqual("SEMI",t[6])

if __name__ == "__main__":
    unittest.main()
