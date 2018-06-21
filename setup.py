import os
from setuptools import setup

from smop.version import __version__ as __VERSION__

#try:
#    __VERSION__ = os.popen("git describe --tags").read().strip()
#except OSError as e:
#    __VERSION__ = ""
#
#open("smop/version.py","w").write("__version__='%s'" % __VERSION__)

setup(
    author = 'Victor Leikehman',
    author_email = 'victorlei@gmail.com',
    description = 'Matlab to Python converter',
    license = 'MIT',
    keywords = 'convert translate matlab octave python',
    url = 'https://github.com/victorlei/smop',
    download_url = 'https://github.com/victorlei/smop/archive/master.zip',
    name = 'smop',
    version = __VERSION__,
    entry_points = { 'console_scripts': [ 'smop = smop.main:main', ], },
    packages = ['smop'],
    #package_dir = {'':'src'},
    #test_suite = "smop.testsuite.test_lexer",
    #include_package_data = True,
    #package_data = { 'smop': ['*.m', 'Makefile'], },
    install_requires = ['ply', 'numpy', 'scipy', 'networkx'],
)
