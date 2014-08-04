from setuptools import setup

#open('version.txt')

setup(
    author = 'Victor Leikehman',
    author_email = 'victorlei@gmail.com',
    description = 'Matlab to Python compiler',
    license = 'MIT',
    keywords = 'convert translate matlab octave python',
    url = 'https://github.com/victorlei/smop',
    download_url = 'https://github.com/victorlei/smop/archive/0.25.zip',
    name = 'smop',
    version = '0.25',
    entry_points = { 'console_scripts': [ 'smop = smop.main:main', ], },
    packages = ['smop'],
    #package_dir = {'':'src'},
    #test_suite = "smop.testsuite.test_lexer",
    #include_package_data = True,
    install_requires = ['numpy', 'scipy', 'networkx'],
)
