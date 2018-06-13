import sys
import pickle
import networkx as nx

from . import node
from . import resolve
from . node import extend
 
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
