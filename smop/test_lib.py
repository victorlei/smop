import unittest
import smop.parse as parse
import smop.backend as backend
import smop.resolve as resolve
import numpy


def _make_arrays(d):
    return {k: numpy.asarray(d) if isinstance(d, list) else d for k, d in d.iteritems()}


class TestLib(unittest.TestCase):
    def assertDictEqual(self, d1, d2, msg=None):
        self.assertIsInstance(d1, dict, 'First argument is not a dictionary')
        self.assertIsInstance(d2, dict, 'Second argument is not a dictionary')

        d1_keys = sorted(d1.keys())
        d2_keys = sorted(d2.keys())
        self.assertSequenceEqual(d1_keys, d2_keys)
        for key in d1_keys:
            if isinstance(d1[key], numpy.ndarray) and isinstance(d2[key], numpy.ndarray):
                numpy.testing.assert_allclose(d1[key], d2[key])
            else:
                self.assertEquals(d1[key], d2[key])

    def run_regression(self, input, filename):
        symtab = {}
        locals_ = {}
        globals_ = {}
        eval(compile('from smop.runtime import *\nfrom smop.core import *\n', '<imports>', 'exec'), globals_, globals_)
        t = parse.parse(input + '\n')
        # print "t=", repr(t)
        resolve.resolve(t, symtab)
        # print "symtab:",symtab
        s = backend.backend(t)
        eval(compile(s, filename, 'exec'), globals_, locals_)
        return locals_

    def test_arithmetic(self):
        res = self.run_regression("""
a = 1
b = a * 2""", 'arithmetic')
        self.assertEquals(res, {'a': 1, 'b': 2})

    def test_trig(self):
        res = self.run_regression("""
a = asin(sin([.1 .5]))
b = acos(cos([.2, 1]))
d = b(1,1)
c = deg2rad(rad2deg([1 2]))""", 'trig')
        self.assertEquals(res, _make_arrays({'a': [[.1, .5]], 'b': [[.2, 1]], 'd': .2, 'c': [[1, 2]]}))
