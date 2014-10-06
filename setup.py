import os
from setuptools import setup

try:
    versionstring = os.popen("git describe").read().strip()
    open("smop/version.py","w").write("__version__ = '%s'\n" % versionstring)
except:
    versionstring = "'0.26'"

setup(
    author = 'Victor Leikehman',
    author_email = 'victorlei@gmail.com',
    description = 'Matlab to Python converter',
    license = 'MIT',
    keywords = 'convert translate matlab octave python',
    url = 'https://github.com/victorlei/smop',
    download_url = 'https://github.com/victorlei/smop/archive/master.zip',
    name = 'smop',
    version = versionstring,
    entry_points = { 'console_scripts': [ 'smop = smop.main:main', ], },
    packages = ['smop'],
    #package_dir = {'':'src'},
    #test_suite = "smop.testsuite.test_lexer",
    #include_package_data = True,
    #package_data = { 'smop': ['*.m', 'Makefile'], },
    install_requires = ['numpy', 'scipy', 'networkx'],
)
