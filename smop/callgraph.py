import parse,sys
import networkx as nx
import node,resolve

def callgraph(func_list):
    """
    Build callgraph of func_list, ignoring
    built-in functions
    """
    G = nx.DiGraph()
    for func in func_list:
        G.add_node(func.head.ident.name)
    for func in func_list:
        assert isinstance(func,node.function)
        func_name = func.head.ident.name
        resolve.resolve(func)
        for s in node.postorder(func):
            if (s.__class__ is node.funcall and
                s.func_expr.__class__ is  node.ident and
                s.func_expr.name in G.nodes()):
                G.add_edge(func_name,s.func_expr.name)
    return G

G = nx.DiGraph()

def postorder_edge(u):
    if isinstance(u,node.node):
        for v in u:
            for t in postorder_edge(v):
                yield (v,t)
        yield (u,u) # returns only traversible objects

def foo(tree):
    G = nx.DiGraph()
    for u,v in postorder_edge(tree):
        G.add_edge(id(u),id(v))
    return G

def main():
    func_list = parse.parse(open(sys.argv[1]).read())
    G = foo(func_list)
    #G = callgraph(func_list)
    nx.write_dot(G,"G.dot")
    #H = nx.dfs_tree(G,'solver')
    #nx.write_dot(H,"H.dot")
    #print nx.topological_sort(H)

if __name__ == '__main__':
    main()
