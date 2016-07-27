import parse,sys,os
import networkx as nx
import node,resolve,options

def callgraph(G, stmt_list):
    """
    Build callgraph of func_list, ignoring
    built-in functions
    """
    func_list = []
    for stmt in stmt_list:
        try:
            G.add_node(stmt.head.ident.name)
            func_list.append(stmt)
        except:
            pass
    for func in func_list:
        assert isinstance(func,node.function)
        func_name = func.head.ident.name
        #resolve.resolve(func)
        for s in node.postorder(func):
            if (s.__class__ is node.funcall and
                s.func_expr.__class__ is  node.ident):
                #if s.func_expr.name in G.nodes():
                    G.add_edge(func_name,s.func_expr.name)
    #nx.write_dot(G,"G.dot")
    #for u in G.nodes():
    #    if G.out_degree(u) == 0:
    #        print u
if __name__ == '__main__':
    main()
