from setuptools import setup

#open('version.txt')

setup(
    author = 'Victor Leikehman',
    author_email = 'victorlei@gmail.com',
    description = 'Matlab to Python compiler',
    license = 'MIT',
    keywords = 'matlab octave python compiler',
    #url = 'http://chiselapp.com/user/victorlei/repository/smop-dev/index',
    #download_url = 'http://code.google.com/p/smop-dev/downloads/list',
    name = 'smop',
    version = '0.24',
    entry_points = { 'console_scripts': [ 'smop = smop.main:main', ], },
    packages = ['smop'],
    #package_dir = {'':'src'},
    #test_suite = "smop.testsuite.test_lexer",
    #include_package_data = True,
    install_requires = ['numpy', 'scipy'],
)
