import unittest
import lexer

class TestLexer(unittest.TestCase):
    def setUp(self):
        self.lexer = lexer.new()
        
    def test_t01(self):
        """Reserved words are not reserved as field names"""
        self.lexer.input("for foo.for")
        t = [tok.type for tok in self.lexer]
        u = ["FOR","IDENT","FIELD"]
        self.assertEqual(t,u)

    def test_t02(self):
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

    def test_t03b(self):
        self.lexer.input("for bar =")
        t = [tok.type for tok in self.lexer]
        u = ["FOR", "IDENT", "="]
        self.assertEqual(t,u)
        
    def test_t03c(self):
        "Ignore anything from ... to end of line"
        self.lexer.input("foo ... hello world \n bar")
        # line continuation
        t = [tok.type for tok in self.lexer]
        u = ["IDENT", "IDENT"]
        self.assertEqual(t,u)

    def test_t03d(self):
        self.lexer.input("clear all, figure on")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","IDENT","SEMI","IDENT","IDENT"]
        self.assertEqual(t,u)

    def test_t03e(self):
        self.lexer.input("dir '.', for i=foo")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","STRING","SEMI","FOR","IDENT","=","IDENT"]
        self.assertEqual(t,u)

    def test_t03f(self):
        """Line continuation is allowed between command arguments,
        but not between the command and the first argument."""
        self.lexer.input("dir 'hello' ... adfadfsda \n fooobar")
        t = [tok.type for tok in self.lexer]
        u = ["IDENT","STRING","IDENT"]
        self.assertEqual(t,u)  
        
    def test_6bc4(self):
        """Terms may start and end with a dot, when a floating
        point number starts or ends with a decimal point"""
        self.lexer.input("[1. .2]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","NUMBER","COMMA","NUMBER","RBRACKET"]
        self.assertEqual(t,u)  

    def test_6bc4_1(self):
        """Ops that start with a dot (./ etc) should not
        be understood as terms"""
        self.lexer.input("[1. ./ .2]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","NUMBER","DOTDIV","NUMBER","RBRACKET"]
        self.assertEqual(t,u)  

    def test_f017(self):
        "Complex constants, such as 1i and 1j"
        self.lexer.input("1i+1j")
        t = [tok.type for tok in self.lexer]
        u = ["NUMBER","PLUS","NUMBER"]
        self.assertEqual(t,u)  
        
    def test_a7a2(self):
        "Quotes and backslashes in strings"
        self.lexer.input(r"'hello''world\abc\n'")
        tok = self.lexer.next()
        self.assertEqual(tok.value,"hello'world\\abc\\n")
    
    def test_d5ef(self):
        "d5ef: cell arrays nested in regular arrays"
        self.lexer.input(r"[foo{i} bar]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","IDENT","LBRACE","IDENT","RBRACE","COMMA","IDENT","RBRACKET"]
        self.assertEqual(t,u)  

    def test_d5ef_1(self):
        "d5ef: Another one"
        self.lexer.input("[y / (x - x) ]")
        t = [tok.type for tok in self.lexer]
        u = ["LBRACKET","IDENT","DIV","LPAREN","IDENT","MINUS","IDENT","RPAREN","RBRACKET"]
        self.assertEqual(t,u)  

    def test_82dc(self):
        "quote immediatly following a keyword is always a string"
        self.lexer.input("case'abc'")
        t = [tok.type for tok in self.lexer]
        u = ["CASE","STRING"]
        self.assertEqual(t,u)  

if __name__ == "__main__":
    unittest.main()
