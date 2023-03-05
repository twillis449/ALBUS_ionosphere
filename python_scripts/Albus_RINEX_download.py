#!/usr/bin/env python
# Albus_RINEX_download.py
# Download files for RINEX stuff, using a timeout.  The Python timeout
# stuff for < 2.5 seems to not work or was not implemented, so
# I have made my own simple case
# 2007 Jan 19  James M Anderson  --JIVE  start
# 2020 Using pycurl with python3
# 2023 Updated to access current CDDIS server


import sys
import pycurl
import requests

def main():
    print('**** in Albus_RINEX_download')
    if(len(sys.argv) != 4):
        print("Error: correct usage is %s inURL, outfilename timeout"%sys.argv[0])
        sys.exit(-2)
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
           split_location = sys.argv[1].find('/')
           host_name =  sys.argv[1][:split_location]
           print('sftp host name', host_name)
           localFilePath = sys.argv[2]
           print('localFilePath',localFilePath)
           with pysftp.Connection(host=host_name, username='anonymous', password='ALBUS_ionosphere') as sftp:
              print("Connection successfully established ... ")
              remoteFilePath  = sys.argv[1][split_location+1:]
              print('*** sftp trying to get file', remoteFilePath )
              sftp.get(remoteFilePath, localFilePath)
              print('*** obtained remote file')
              return
         except:
            print('**** sftp failed to get file: ', remoteFilePath )
            sys.exit(-3)
            

    HAS_PYCURL = True
    if HAS_PYCURL:  #  currently the system used for retrieving ftp data
      print ('using PyCurl')
      print ('system parameters ', sys.argv)
      HAS_FTPLIB = False
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
      
   
    sys.exit(0)

if __name__ == '__main__':
    main()
