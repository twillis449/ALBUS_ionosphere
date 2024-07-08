import re, string
import numpy as np
import inspect
import warnings
import math
import sys
import os
from os import path
import time as systime

def convert_rnx3_to_rnx2_file(in_filename):
# first convert incoming cnx file to rnx
        cnx_loc = in_filename.find('.cnx')
        if cnx_loc < 0:
           data_file =  in_filename[:-1] + 'o'
        else:
           data_file = in_filename[:cnx_loc] + '.rnx'
        command = "crx2rnx %s - > %s"%(in_filename,data_file)
        print('system executing command', command)
        retcode = os.system(command)
        print('retcode', retcode)
        if(retcode):
           raise Albus_RINEX.No_RINEX_File_Error("Could not execute '%s'"%command)
        command = '/bin/rm -rf ' +  data_file +'_rnx3'
        print('system executing command', command)
        retcode = os.system(command)
        command = 'cp ' + data_file + ' ' +  data_file +'_rnx3'
        print('trying to save a copy of  RINEX 3 file') 
        print('system executing command', command)
        retcode = os.system(command)

        print('trying to convert RINEX 3 to RINEX 2') 
        print('using data file ', data_file)
        command = 'gfzrnx -finp ' + data_file + ' -fout ' + data_file + '_rnx2 -vo 2 -ot G:C1C,L1C,C2W,L2W,C2X,S1C,S2W,L2X,S2X,C1W'
        print('system executing command', command)
        retcode = os.system(command)
        if(retcode):
           raise Albus_RINEX.No_RINEX_File_Error("Could not execute '%s'"%command)
# check that the above operation produced a file
        if os.path.isfile(data_file + '_rnx2'):
          command = '/bin/rm -rf ' + data_file
          print('executing command', command)
          retcode = os.system(command)
          command = 'mv ' + data_file + '_rnx2 ' + data_file
          print('executing command', command)
          retcode = os.system(command)
          command = "rnx2crx %s - > %s"%(data_file,in_filename)
          print('executing command', command)
          retcode = os.system(command)
          if(retcode):
            print('failure to get suitable RINEX 2 file, returning')
            return 1
          return in_filename
        else:
#         print('cnvrnx3-rnx2 was unable to convert rinex3 file')
          print('gfzrnx was unable to convert rinex3 file')
          return 1

def main( argv ):
# argv[1] = name of RINEX3 compressed file
  result = convert_rnx3_to_rnx2_file(argv[1])
  print('got result', result)
  return
#=============================
if __name__ == "__main__":
  main(sys.argv)

