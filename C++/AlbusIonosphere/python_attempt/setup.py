# setup.py
# a setup file to help compile a C/C++ code module for Python
# 2006 ??? ??  James M Anderson  --JIVE  start
# 2007 Jan 10  JMA  --revise to also use Numarray stuff
# 2016 April AGW - changed for numpy and got rid of all numarray-related stuff
# 2019 July  AGW - script should now handle python 2 or python 3 

from setuptools import setup, Extension
from platform import python_version
import sys
import numpy
p_version = python_version()
# python2
if int(p_version[0])< 3:
  user_inp = raw_input('Sorry, Python2 is no longer supported')
  exit
else:
# python3
  user_inp = input(' ')
user_inp = user_inp.split()
ALBUS_PATH = user_inp[0]


module1 = Extension('AlbusIonosphere',
                    libraries = ['mim',
                                 'jmavex',
                                 'iri',
                                 'pim',
                                 'sofa',
                                 'vexplus',
                                 'vex',
                                 'fl',
                                 'gfortran',
#                                'lapacke',
                                 'm'],
                    library_dirs = [ALBUS_PATH+'/lib'],
                    runtime_library_dirs = [ALBUS_PATH+'/lib'],
                    include_dirs = [numpy.get_include()],
                    sources = ['AlbusIonosphere.cxx'])

setup (name = 'AlbusIonosphere',
       version = '1.0.0',
       long_description = '''
ALBUS Ionospheric Calibration C++ support package 
''',
       ext_modules = [module1])
