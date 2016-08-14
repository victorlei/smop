import os
from setuptools import setup

from smop.version import __version__

setup(
    author = 'Victor Leikehman',
    author_email = 'victorlei@gmail.com',
    description = 'Matlab to Python converter',
    license = 'MIT',
    keywords = 'convert translate matlab octave python',
    url = 'https://github.com/victorlei/smop',
    download_url = 'https://github.com/victorlei/smop/archive/master.zip',
    name = 'smop',
    version = __version__,
    entry_points = { 'console_scripts': [ 'smop = smop.main:main', ], },
    packages = ['smop'],
    #package_dir = {'':'src'},
    #test_suite = "smop.testsuite.test_lexer",
    #include_package_data = True,
    #package_data = { 'smop': ['*.m', 'Makefile'], },
    install_requires = ['ply', 'numpy', 'scipy', 'networkx', 'pygraphviz'],
)
