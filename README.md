# Pythonic Small Matlab to Python Compiler

Pythonic SMOP is a tool to translate MATLAB code into Python code. Pythonic SMOP is a fork of SMOP that aims to make four improvements:

1. Produce code that uses Python syntax instead of MATLAB syntax. This removes the reliance on the original MATLAB files that is present in SMOP.
2. Remove the dependency on a large library to emulate MATLAB behavior. Currenly, only a 30 line Python file is needed initially. The reliance on this file can and should be removed, but must be done by hand.
3. Allow translation of MATLAB functions to Python functions.
4. Fix bugs and improve parsing.

## Installation

Pythonic SMOP can currently only be installed through downloads or clones of this repository.

## Usage

Pythonic SMOP is run by calling ```main.py```. Running this without flags will list the description for all possible flags. To run a basic translation, use ```python main.py INPUT_FILE.m -o OUTPUT_FILE.py```.

If the outputted file uses arrays, there should be ```import smop_util``` near the top of the file. This includes just one function that is used to emulate MATLAB's ability to expand arrays by assigning a value to an element beyond the bounds of the array. To keep this functionality, place smop_util.py in the folder same folder as the generated Python file before running it. In most cases, the Python code should be edited to manually set the bounds of the array. This must be done manually, since it is difficult if not impossible to find the proper max bounds of an array by reading the MATLAB code. Once this is done, all references to smop_util.py can be removed by replacing ```safe_set``` function calls with assignment expressions.