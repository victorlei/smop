import unittest
import parse
import smop.node as node

class TestParse(unittest.TestCase):
    def test_p03(self):
        """Expected failure"""
        s = """[1 ; 1; 1 ; ];"""
        t = parse.parse(s)
        self.assert_(t)

    @unittest.skip('broken')
    def broken_test_p04(self):
        """Dot has higher precedence than other operations"""
        s = "a+b.c.d;"
        t = parse.parse(s)
        u = [node.expr_stmt(id=1, expr=[('+', node.ident(name='a',
                lineno=1, lexpos=0),
                node.field(expr=node.field(expr=node.ident(name='b',
                lineno=1, lexpos=2), ident=node.ident(name='.c', lineno=1,
                lexpos=3)), ident=node.ident(name='.d', lineno=1, lexpos=5)))])]
        self.assertEqual(t,u)
        # [node.expr_stmt(expr=[expr(op='+', args=expr_list([ident(name='a', lineno=1, column=1, lexpos=0, defs=None, props=None), expr(op='.', args=expr_list([expr(op='.', args=expr_list([ident(name='b', lineno=1, column=3, lexpos=2, defs=None, props=None), ident(name='.c', lineno=1, column=None, lexpos=3, defs=None, props=None)])), ident(name='.d', lineno=1, column=None, lexpos=5, defs=None, props=None)]))]))]))])

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

    def test_empty(self):
        s = ""
        t = parse.parse(s)
        self.assertEqual(t.__class__, node.stmt_list)

    def test_empty_function(self):
        s = """
        function fn
        """
        for input in (s, s + "end\n"):
            t = parse.parse(input)
            self.assertEqual(t[0].__class__, node.function)
            self.assertEqual(t[0].head.__class__, node.func_decl)
            self.assertEqual(t[0].head.ident.name, 'fn')
            self.assertEqual(t[0].body[0].__class__, node.return_stmt)

    def test_simple_function(self):
        s = """
        function toplevel
            x = 1
        end
        """
        t = parse.parse(s)
        self.assert_(t)

    def test_nested_function(self):
        s = """
        function toplevel
            x = 1
            function nested(y)
            end
            function nested2()
            end
        end
        """
        t = parse.parse(s)
        self.assert_(t)
        self.assertEqual(4, len(t[0].body))  # n.b. parser inserts a "return" statement

    def test_nonnested_function(self):
        s = """
        function [] = toplevel
            x = 1
        function [ret, y] = nonnested1(x)
            y = ~x
        function [] = nonnested2()
            z = 3
        end
        """
        t = parse.parse(s)
        self.assert_(t)
        self.assertEqual(3, len(t))

    @unittest.skip('broken')
    def broken_test_command(self):
        s = "zlabel y_3(1)\n"
        t = parse.parse(s)
        self.assert_(t)

if __name__ == "__main__":
    unittest.main()
