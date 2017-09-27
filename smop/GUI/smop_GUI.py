# @Author: Samuel Radat <samuel_r>
# @Date:   2017-09-18T17:27:53+02:00
# @Filename: smop_GUI.py
# @Last modified by:   Samuel Radat
# @Last modified time: 2017-09-27T16:43:13+02:00


from sys import argv, exit
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QMessageBox, QMenu, QHBoxLayout, QVBoxLayout, QLabel
from PyQt5.QtWidgets import QCheckBox, QDialog
from PyQt5.QtWidgets import QDesktopWidget, QMainWindow, QAction, qApp, QLineEdit, QInputDialog, QGridLayout, QFileDialog
from PyQt5.QtCore import QCoreApplication
from functools import partial
from smop_backend_ import Matlab_converter
import os


class MainApp(QMainWindow):
    """Main application class"""
    def __init__(self, width, height):
        super().__init__(None)
        self.username = "samuel_r"
        self.initUi(width, height)


    def initUi(self, width, height):
        self.resize(width, height)
        self.center_ui()
        self.quit_ui()
        self.setWindowTitle('SMOP')
        self.centralWidget = GUI_smop(username=self.username)
        self.setCentralWidget(self.centralWidget)
        self.menu_Bar()
        self.show()


    def menu_Bar(self):
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('Menu')

        settings = QAction('Settings', self)
        fileMenu.addAction(settings)
        settings.triggered.connect(self.settingsMenu)

        quitmenu = QAction('Quit', self)
        fileMenu.addAction(quitmenu)
        quitmenu.triggered.connect(qApp.quit)

    def settingsMenu(self):
        settings_menu = GUI_settings(self.centralWidget)
        settings_menu.exec_()

    def center_ui(self):
        qr = self.frameGeometry()
        cp = QDesktopWidget().availableGeometry().center()
        qr.moveCenter(cp)
        self.move(qr.topLeft())


    def quit_ui(self):
        quit_btn = QAction('Exit', self)
        quit_btn.setToolTip('Exit program')
        quit_btn.triggered.connect(qApp.quit)


    def closeEvent(self, event):
        reply = QMessageBox.question(self, 'Warning', 'Are you sure to quit ?',
                            QMessageBox.Yes | QMessageBox.No, QMessageBox.No)
        if reply == QMessageBox.Yes:
            event.accept()
        else:
            event.ignore()


class GUI_smop(QWidget):


    def __init__(self, parent=None, username='samuel_r'):
        super().__init__(parent)
        self.username = username
        self.replace_by_self = False
        self.script = "plot_Ar_fft"
        self.plot_type = "Mechanics\\"
        self.full_new_path = self.get_path()
        self.python_executable = "C:\\Python27_64\\python.exe"
        self.older_folder = None
        self.init_UI()


    def get_path(self):
        if os.path.exists(os.path.join(os.path.curdir, "config.txt")):
            with open('config.txt', 'r') as f:
                s = f.read()
                f.close()
                return s
        return ''


    def init_UI(self):
        self.core()


    def set_new_script(self, script):
        self.script = script


    def set_python_executable(self, python_executable):
        self.python_executable = python_executable


    def set_output_path(self, path):
        self.full_new_path = path


    def core(self):

        self.grid = QGridLayout()
        self.grid.setSpacing(40)

        # SCRIPT FILE SET
        self.script_btn = QPushButton('Set script file', self)
        self.grid.addWidget(self.script_btn, 1, 0)
        self.script_btn.move(20, 20)
        self.script_btn.clicked.connect(partial(self.show_Dialog,selector=1))

        self.le = QLineEdit(self)
        self.grid.addWidget(self.le, 1, 1)
        self.le.setReadOnly(True)
        self.le.setPlaceholderText('plot_Ar_fft')
        # !SCRIPT FILE SET

        # python_executable SET
        self.plot_type_btn = QPushButton('Set python 2 executable path', self)
        self.grid.addWidget(self.plot_type_btn, 2, 0)
        self.plot_type_btn.clicked.connect(partial(self.show_Dialog,selector=2))

        self.plotle = QLineEdit(self)
        self.grid.addWidget(self.plotle, 2, 1)
        self.plotle.setReadOnly(True)
        self.plotle.setPlaceholderText(self.python_executable)
        # !python_executable SET

        # new_path SET
        self.new_path_btn = QPushButton('Set output path', self)
        self.grid.addWidget(self.new_path_btn, 3, 0)
        self.new_path_btn.clicked.connect(partial(self.show_Dialog,selector=3))

        self.new_pathle = QLineEdit(self)
        self.grid.addWidget(self.new_pathle, 3, 1)
        self.new_pathle.setReadOnly(True)
        self.new_pathle.setPlaceholderText(self.full_new_path)
        # !new_path SET

        # replace parameter by self check
        self.self_chk = QLabel('working on a plot_ script', self)
        self.grid.addWidget(self.self_chk, 4, 0)

        self.chk_widget = QCheckBox()
        self.grid.addWidget(self.chk_widget, 4, 1)
        # !replace parameter by self check

        # BUTTON TO CONVERT
        self.convert_btn = QPushButton('Convert to python', self)
        self.grid.addWidget(self.convert_btn, 5, 1, 2, 1)
        self.convert_btn.clicked.connect(self.convert_file)
        # !BUTTON TO CONVERT

        self.setLayout(self.grid)

        self.dictionary = {1: self.le, 2: self.plotle, 3: self.new_pathle}


    def show_Dialog(self, selector):
        fct_dict = {1: self.set_new_script, 2: self.set_python_executable, 3: self.set_output_path}
        other_dict = {1: 'Enter script file:', 2: 'Enter python executable:', 3: 'Enter output path'}

        if selector == 1 or selector == 2:
            dirname = QFileDialog.getOpenFileName(self, 'Select file')[0]
        else:
            if os.path.exists(os.path.join(os.path.curdir, "config.txt")):
                with open("config.txt", 'r') as f:
                    self.older_folder = f.read()
                    f.close()
            if self.older_folder != None:
                dirname = QFileDialog.getExistingDirectory(self, 'Select folder', self.older_folder)
                self.older_folder = dirname
                with open("config.txt", 'w') as f:
                    f.write(self.older_folder)
                    f.close()
            else:
                dirname = QFileDialog.getExistingDirectory(self, 'Select folder')
                self.older_folder = dirname
                with open("config.txt", 'w') as f:
                    f.write(self.older_folder)
                    f.close()
        self.dictionary[selector].setText(str(dirname))
        fct_dict[selector](str(dirname))
        if selector != 2:
            if not self.full_new_path.endswith('\\'):
                self.set_output_path(self.full_new_path + '\\')

    def self_replacement(self, sate):
        if state == Qt.Checked:
            self.replace_by_self = True
        else:
            self.replace_by_self = False

    def convert_file(self):
        temporary_file = self.script.split('/')[-1].split('.')[0] + '.py'
        print(self.python_executable + " " + os.path.abspath('../main.py') + " " + self.script + " -o " + temporary_file)
        os.system(self.python_executable + " \"" + os.path.abspath('../main.py') + "\" \"" + self.script + "\" -o \"" + temporary_file + '"')
        matlab_file = Matlab_converter(temporary_file, self.username)
        noerror = matlab_file.rewrite_file(temporary_file, temporary_file.split('/')[-1], self.full_new_path, self.replace_by_self, self)
        if noerror:
            os.remove(os.path.join(os.path.curdir, temporary_file))

class GUI_settings(QDialog):

    def __init__(self, Main_window):
        super().__init__()
        Main_window.username = 'samuel_r'
        self.smop_window = Main_window
        self.init_gui()

    def init_gui(self):
        self.resize(200, 150)


        # SCRIPT FILE SET
        self.script_btn = QPushButton('Set username', self)
        self.script_btn.move(20, 20)
        self.script_btn.clicked.connect(self.show_dial)

        self.le = QLineEdit(self)
        self.le.move(20, 50)
        self.le.setReadOnly(True)
        self.le.setPlaceholderText('login_r')
        # !SCRIPT FILE SET

        self.setWindowTitle('Header file settings')

    def show_dial(self):

        text, ok = QInputDialog.getText(self, 'username setting', 'enter username: ')

        if ok:
            self.le.setText(str(text))
            self.smop_window.username = str(text)


app = QApplication(argv)
smop = MainApp(1280, 720)
exit(app.exec_())
