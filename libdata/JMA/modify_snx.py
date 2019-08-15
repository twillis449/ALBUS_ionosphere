#!/usr/bin/env python

import os
import sys
import numpy
import math 

from string import split, strip


def process_data(filename, filename1):
  text = open(filename, 'r').readlines()
  log = open(filename1,'a')
  L = len(text)
  for i in range(len(text)):
    str1 = text[i][0:55] + ' ' + text[i][55:67] + '  ' + text[i][68:75]+'\n'
    log.write(str1)

def main( argv ):
  process_data('temp_2018', 'temp_mod_2018')

#=============================
# argv[1]  incoming ALBUS results file 
if __name__ == "__main__":
  main(sys.argv)
