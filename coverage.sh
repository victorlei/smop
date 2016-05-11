#!/bin/bash
set -e
python setup.py build_ext --inplace
nosetests.exe --with-coverage --cover-html --traverse-namespace --cover-branches --cover-erase --cover-tests
coverage html -d cover
