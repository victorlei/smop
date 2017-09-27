# @Author: Samuel Radat
# @Date:   2017-09-13T11:14:55+02:00
# @Filename: smop_backend_.py
# @Last modified by:   Samuel Radat
# @Last modified time: 2017-09-27T16:43:00+02:00



import os
from subprocess import call
from sys import stdout
from datetime import datetime
from PyQt5.QtWidgets import QMessageBox


class Matlab_converter:


    def __init__(self, script, username):
        self.script = script
        self.header = "# -*- coding: utf-8 -*-\n\"\"\"@package plot_bemf\n@date Created on " + str(datetime.now()) +"\n@author " + username + "\n\"\"\"\n\n\n"
        self.plot = False


    def add_to_header(self, s):
        self.header += s + '\n'


    def normalise_header(self):
        h = self.header.splitlines()
        for l in h:
            if h.count(l) > 1 and l != '':
                h.pop(h.index(l))
        self.header = "\n".join(h)
        pass #TODO: instead of several lines of imports, do it on one line (not sure if good idea though)


    def set_new_script(self, script, plot_type):
        self.script = script
        self.plot_type = plot_type


    def put_parameter_in_parenthesis(self, filename, s, bracket_content):
        i = s.find('def ' + filename + '(')
        i += 3 + 1 + len(filename) + 1
        parenthesis_content = []
        while s[i] != ')':
            parenthesis_content.append(s[i])
            i += 1
        parenthesis_content = "".join(parenthesis_content)
        s = s.replace(parenthesis_content, bracket_content)
        return s


    def get_parenthesis_content(self, s, filename):
        i = s.find('def ' + filename + '(')
        i += 3 + 1 + len(filename) + 1
        parenthesis_content = []
        while s[i] != ')':
            parenthesis_content.append(s[i])
            i += 1
        return "".join(parenthesis_content)


    def put_as_parameter(self, line, s, filename):
        filename = filename.split('.')[0]
        var_objs = {'Electrical': 'elec'}
        try:
            line_decomposition = [line.split('=')[0], [line.split('.')[0].split('=')[1], line.split('.')[1], line.split('.')[2]]]
        except IndexError:
            return [line, ""]
        parameter_to_put = line_decomposition[0].strip()
        bracket_content = self.get_parenthesis_content(s, filename)
        s = self.put_parameter_in_parenthesis(filename, s, (self.get_parenthesis_content(s, filename) + ', ' + parameter_to_put))
        return ["", s]


    def get_spaces(self, line_tab):
        i = 0
        while line_tab[i] == '':
            i += 1
        space_string = ""
        for j in range(i):
            space_string += ' '
        return space_string


    def handle_plot_var(self, line, s):
        if not self.plot:
            self.add_to_header('from GUI import GUI_Option')
            i = s.find(line)
            line = self.get_spaces(line.split(' ')) + 'Input = GUI_Option()\n' + line.split('=')[0] + ' = Input.plot.' + line.split('.')[-1]
            self.plot = True
        else:
            line = line.split('=')[0] + ' = Input.plot.' + line.split('.')[-1]
        return [line, ""]


    def get_bracket_content(self, s, name_f):
        content = []
        i = s.find(name_f + '(') + len(name_f) + 1
        k = 1
        while s[i] != ')' or k > 1:
            if s[i] == '(':
                k += 1
            content.append(s[i])
            if s[i] == ')':
                k -= 1
            i += 1
        return "".join(content)


    def get_args_nb(self, s):
        return len(s.split(','))


    def function_is(self, s, fct_name):
        if s.find(fct_name + '(') != -1 and s.find(')') != -1:
            return True
        return False


    def check_fct(self, line):
        pyplot_fcts = [
            'plot', 'legend', 'xlabel', 'ylabel', 'title', 'grid', 'hold',
            'bar', 'subplot', 'axis', 'polar'
        ]
        numpy_fcts = [
            'diag', 'ravel', 'arange', 'dot', 'log10', 'sqrt', 'logical_not',
            'all', 'zeros', 'squeeze', 'shape', 'size', 'absolute', 'copy',
            'sign', 'transpose', 'ones'
        ]
        for fct_n in pyplot_fcts:
            if self.function_is(line, fct_n) and fct_n not in self.header.split():
                self.add_to_header('from matplotlib.pyplot import ' + fct_n)
        if self.function_is(line, 'Logger.warning'):
            self.add_to_header('from logging import Logger')
        for fct_n in numpy_fcts:
            if self.function_is(line, fct_n) and fct_n not in self.header.split():
                self.add_to_header('from numpy import ' + fct_n)


    def in_square_brackets(self, line, string):
        i = line.find(string)
        i_save = i
        begin = False
        end = False
        while i > 0:
            if line[i] == '[':
                begin = True
                break
            i -= 1
        if not begin:
            return False
        while i_save < len(line):
            if line[i_save] == ']':
                end = True
                break
            i_save += 1
        if begin and end:
            return True
        return False


    def evaluate_line(self, line, s, filename):

        # if line starts with these strings, delete the line
        if line.startswith('# C:') or line.startswith('# Autogenerated with SMOP') or line.startswith('from smop.core import *') or line.find('varargin') != -1 or line.find('nargin') != -1:
            return False

        # for zeros() function, adds surrounds the brackets_content with parenthesis to fit the right way to write this function
        if line.find('zeros(') != -1 and line.find(')') != -1:
            parenthesis_content = self.get_bracket_content(line, "zeros")
            if parenthesis_content == None:
                print('LINE=',line)
            if not parenthesis_content.startswith('(') or not parenthesis_content.endswith(')'):
                parenthesis_content = '(' + parenthesis_content + ')'
            line = line.split('(')[0] + '(' + parenthesis_content + ')\n'
            self.add_to_header('from numpy import zeros')

        # if it's the function figure(), removes the first parameter and adds some useful properties
        if self.function_is(line, 'figure'):
            line = (line.split('(')[0] + '(' + line.split(',')[1])[0:-2] + ", figsize=(12, 10), facecolor='w')\n"
            self.add_to_header('from matplotlib.pyplot import figure')
            return [line, ""]

        # replace dlmread() function by numpy.load()
        if self.function_is(line, 'dlmread') and self.get_args_nb(line) == 1:
            line = line.replace('dlmread(', 'load(')
            self.add_to_header("from numpy import load")

        # this part is specific to the internship project, it puts the output and input variables as parameter
        if (line.find('Input.') != -1 or line.find('Output.') != -1) and line.find("'") == -1:
            if line.find('Input.Plot.') != -1:
                return (self.handle_plot_var(line, s))
            return self.put_as_parameter(line, s, filename)

        # several correlations between matlab and python languages. Keywords are replaced
        replace_str = {'size(': 'shape(', 'grid(\'on\')': 'grid(True)',
                        'grid(\'off\')': 'grid(False)','disp(': 'print(',
                        'num2str': 'str', 'hold(\'on\')': 'hold(True)',
                        'hold(\'off\')': 'hold(False)', 'abs(': 'absolute(',
                        'floor(': 'np_floor(', 'isempty(': 'all(',
                        'length(': 'size(', 'matlabarray(': 'array('}

        if line.find('disp_com') != -1:
            line = line.replace('disp_com', 'Logger.warning')
            line = line.replace(',Input.Simu.is_warning,1)', ')')

        for replacement_s in replace_str.keys():
            line = line.replace(replacement_s, replace_str[replacement_s])

        self.check_fct(line)

        if line.find('end()') != -1 and self.in_square_brackets(line, 'end()'):
            line = line.replace('end()', '-1')

        return [line, ""]


    def delete_file_begin(self, s, filename):
        i = s.find('def ' + filename + '(')
        return s[i:]


    def rewrite_file(self, filename, filename_short, full_new_path, replace_by_self, Qobj):
        s = ""
        try:
            f = open(filename, 'r')
        except FileNotFoundError:
            buttonReply = QMessageBox.critical(Qobj, 'File not found', 'The input script file was not found.', QMessageBox.Ok)
            return False
        lines = f.readlines()
        f.close()
        for line in lines:
            # this will automatically replace any new function's parameters by "self"
            if replace_by_self and line.find('def ' + filename_short.split('.')[0]) != -1:
                print(line.split('(')[0] + '(self):\n')
                line = line.split('(')[0].split(' ')[0] + ' ' + line.split('(')[0].split(' ')[1] + '(self):\n'
            var = self.evaluate_line(line, s, filename_short)
            if not var:
                continue
            if var[0] == "":
                s = var[1]
                continue
            s += var[0]
        s = self.delete_file_begin(s, filename_short.split('.')[0])
        if replace_by_self:
            name_file = full_new_path + '_' + filename_short
        else:
            name_file = full_new_path + filename_short
        new_f = open(name_file, 'w')
        self.add_to_header('\n\n')
        self.normalise_header()
        new_f.write(self.header + s)
        new_f.close()
        return True
