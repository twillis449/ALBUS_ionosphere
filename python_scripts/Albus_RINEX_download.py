#!/usr/bin/env python
# Albus_RINEX_download.py
# Download files for RINEX stuff, using a timeout.  The Python timeout
# stuff for < 2.5 seems to not work or was not implemented, so
# I have made my own simple case
# 2007 Jan 19  James M Anderson  --JIVE  start


import sys
HAS_PYCURL = False
HAS_FTPLIB = False
try:
  import pycurl
  HAS_PYCURL = True
except:
  pass

try:
  import ftplib
  HAS_FTPLIB = True
except:
  pass

def main():
    print('in Albus_RINEX_download')
    if(len(sys.argv) != 4):
        print("Error: correct usage is %s inURL, outfilename timeout"%sys.argv[0])
        sys.exit(-2)
    if HAS_FTPLIB:
      in_str = sys.argv[1]
      # we ignore the initial ftp:// characters
      divider = in_str[6:].find('/') 
      ftp_server = in_str[6:6+divider]
      file_to_get  = in_str[6+divider:]
      print('file_server is ', ftp_server)
      print('file to get is ', file_to_get)
      try:
        timeout = int(sys.argv[3])
        ftp = ftplib.FTP(ftp_server, timeout=timeout)
        ftp.login()
        localfile = open(sys.argv[2], 'wb')
        ftp.retrbinary('RETR ' + file_to_get, localfile.write, 1024)
        ftp.quit()
        localfile.close()
      except ftplib.all_errors as msg: 
        print('An ftplib error occurred:', msg)
      HAS_PYCURL = False
      
   
    if HAS_PYCURL:  #  currently the system used for retrieving ftp data
      print ('using PyCurl')
      print ('system parameters ', sys.argv)
      try:
        print("URL=",sys.argv[1]," File=",sys.argv[2])
        try:
          timeout = int(sys.argv[3])
          with open(sys.argv[2], 'wb') as f:
               c = pycurl.Curl()
               c.setopt(c.URL, sys.argv[1])
               c.setopt(pycurl.CONNECTTIMEOUT, 120)
               c.setopt(pycurl.TIMEOUT, timeout)
               c.setopt(c.WRITEDATA, f)
               print('curl getting data at ',sys.argv[1])
               c.perform()
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
