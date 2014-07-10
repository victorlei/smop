import unittest
import parse

class TestParse(unittest.TestCase):
    def test_p03(self):
        """Expected failure"""
        s = """[1 ; 1; 1 ; ];"""
        t = parse.parse(s)
        self.assert_(t)

    def test_p04(self):
        """Dot has higher precedence than other operations"""
        s = "a+b.c.d;"
        t = parse.parse(s)
        u = [parse.expr_stmt(id=1, expr=[('+', parse.ident(name='a',
        lineno=1, lexpos=0),
        parse.field(expr=parse.field(expr=parse.ident(name='b',
        lineno=1, lexpos=2), ident=parse.ident(name='.c', lineno=1,
        lexpos=3)), ident=parse.ident(name='.d', lineno=1, lexpos=5)))])]
        self.assertEqual(t,u)

#    def test_p05(self):
#        """Iterate over LHS nodes (TBD)"""
#        s = "[foo(A.x(B.y)).bar(C.z).bzz,hello.world] =1;"
#        t = parse.parse_buf(s)
#        u = ["foo",".bar",".bzz","hello",".world"]
#        self.assertEqual([v[1] for v in dataflow.lhs(t[1][1])],u)
#
    def test_p06(self):
        """Cell arrays"""
        s = """
        {1 ...
	'foo' ...
        'bar' ...
        'bzz'};
        """
        t = parse.parse(s)
        self.assert_(t)

if __name__ == "__main__":
    unittest.main()
