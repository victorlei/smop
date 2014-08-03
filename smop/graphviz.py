import parse,sys
import node
from node import extend
import networkx as nx
 
                

def resolve(t,fp,func_name):
    fp.write("digraph %s {\n" % func_name)
    fp.write('graph [rankdir="LR"];\n')
    for u in node.postorder(t):
        if u.__class__ in (node.ident,node.param):
            fp.write("%s [label=%s_%s_%s];\n" % (u.lexpos,u.name,u.lineno,u.column))
            if u.defs:
                for v in u.defs:
                    fp.write("%s -> %s" % (u.lexpos,v.lexpos))
                    if u.lexpos < v.lexpos:
                        fp.write('[color=red]')
                    #else:
                    #    fp.write('[label=%s.%s]' % (v.lineno,v.column))
                    fp.write(';\n')
    fp.write("}\n")

@extend(node.node)
def _graphviz(self,fp):
    if getattr(self,"__slots__",False):
        fp.write('"%s" [' % id(self))
        fp.write('label = "<f0> %s|' % self.__class__.__name__)
        fp.write('|'.join(['<f%d> %s' % (i+1,s) for i,s in enumerate(self.__slots__)]))
        fp.write('"]\n')
    else:
        fp.write('"%s" [' % id(self))
        fp.write('label = "<f0> %s|' % self.__class__.__name__)
        fp.write('|'.join(['<f%d> %d' % (i+1,i+1) for i in range(len(self))]))
        fp.write('"]\n')

    for i,s in enumerate(self):
        if not isinstance(s,node.node):
            fp.write('"%s" [label="%s",shape=ellipse]\n' % (id(s),s))
        if not s in [None,0,"",False]:
            fp.write('"%s":f%d -> "%s";\n' % (id(self), i+1, id(s)))

@extend(node.number)
def _graphviz(self,fp):
    fp.write('"%s" [label="<f0> %s|<f1> %s"]\n' % 
             (id(self), self.__class__.__name__,self.value))

@extend(node.ident)
def _graphviz(self,fp):
    fp.write('"%s" [label="<f0> %s|<f1> %s"]\n' % 
             (id(self), self.__class__.__name__,self.name))

def graphviz(tree,fp):
    fp.write('''strict digraph g {
                graph [rankdir="LR"];
                node [shape=record];
             ''')
    for u in node.postorder(tree):
        u._graphviz(fp)
    fp.write("}\n")

if __name__ == '__main__':
    main()
