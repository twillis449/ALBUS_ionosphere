#!/usr/bin/env python
# Albus_RINEX_download.py
# Download files for RINEX stuff, using a timeout.  The Python timeout
# stuff for < 2.5 seems to not work or was not implemented, so
# I have made my own simple case
# 2007 Jan 19  James M Anderson  --JIVE  start
# 2020 Using pycurl with python3
# 2023 Updated to access current CDDIS server

global DEBUG_SET
DEBUG_SET = False

import sys
import pycurl
import requests
import os

def main():
    if DEBUG_SET:
      print('**** in Albus_RINEX_download')
    if(len(sys.argv) != 4):
        print("Error: correct usage is %s inURL, outfilename timeout"%sys.argv[0])
        sys.exit(-2)
    if DEBUG_SET:
      print('Albus_RINEX_download system parameters', sys.argv)

    if sys.argv[1].find('cddis')> -1:
       url =  sys.argv[1]
       filename = sys.argv[2]
       r = requests.get(url)
# Opens a local file of same name as remote file for writing to
       with open(filename, 'wb') as fd:
          for chunk in r.iter_content(chunk_size=1000):
             fd.write(chunk)
       fd.close()
       # is this a data file or stupid cddis html file returned when there's not actual data
       try:
         text= open(filename, 'r').readlines()
         if DEBUG_SET:
           print('text[0]', text[0])
           print('find location', text[0].find('html'))
         if text[0].find('html') >= 0: # its a garbage html file and not a data file
           os.remove(filename)
           if DEBUG_SET:
             print('Failed to get data from CDDIS for ', sys.argv[1], ' file probably not found!!')
           sys.exit(-3)
       except:
         file_stats = os.stat(filename)
         if DEBUG_SET:
           print('CDDIS returned a binary file with Byte size', file_stats.st_size )
         sys.exit(0)

    if sys.argv[1].find('sftp')> -1:
      use_pysftp = True
      try:
        import pysftp
      except:
        use_pysftp = False
      if not use_pysftp:
        print('Cannot use sftp protocol; exiting')
        sys.exit(-3)
      else:
         try:
           cnopts = pysftp.CnOpts()
           cnopts.hostkeys = None
           split_location = sys.argv[1].find('/')
           host_name =  sys.argv[1][:split_location]
           localFilePath = sys.argv[2]
           if DEBUG_SET:
             print('sftp host name', host_name)
             print('text[0]', text[0])
             print('localFilePath',localFilePath)
           with pysftp.Connection(host=host_name, username='anonymous', password='ALBUS_ionosphere',cnopts=cnopts) as sftp:
              remoteFilePath  = sys.argv[1][split_location+1:]
              if DEBUG_SET:
                print("Connection successfully established ... ")
                print('*** sftp trying to get file', remoteFilePath )
              sftp.get(remoteFilePath, localFilePath)
              if DEBUG_SET:
                print('*** obtained remote file')
              return
         except:
            print('**** sftp failed to get file: ', remoteFilePath )
            sys.exit(-3)
            

    HAS_PYCURL = True
    if HAS_PYCURL:  #  currently the system used for retrieving most ftp data
      if DEBUG_SET:
        print ('using PyCurl')
        print ('system parameters ', sys.argv)
      HAS_FTPLIB = False
      try:
        if DEBUG_SET:
          print("URL=",sys.argv[1]," File=",sys.argv[2])
        try:
          timeout = int(sys.argv[3])
          with open(sys.argv[2], 'wb') as f:
               c = pycurl.Curl()
               c.setopt(c.URL, sys.argv[1])
               c.setopt(pycurl.CONNECTTIMEOUT, 120)
               c.setopt(pycurl.TIMEOUT, timeout)
               c.setopt(c.WRITEDATA, f)
               if DEBUG_SET:
                 print('curl getting data at ',sys.argv[1])
               c.perform()
               if DEBUG_SET:
                 print('curl closing for ', sys.argv[1])
               c.close()
        except:
          print('PyCurl failed to get data for ', sys.argv[1], ' file probably not found!!')
          sys.exit(-3)
      except:
        pass

    sys.exit(0)

if __name__ == '__main__':
    main()
