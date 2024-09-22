# Albus_RINEX.py
# Python stuff for dealing with Ionosphere RINEX stuff
# 2006 Jul 17  James M Anderson  --JIVE  start
# 2023 Mar  4  A.G. Willis -- handle long product file names



global DEBUG_SET 
DEBUG_SET = False

################################################################################
# some import commands  The User should not need to change this.
################################################################################


################################################################################
# miscellaneous stuff
import copy, optparse, os, sys, signal, getpass
# import popen2
import re, string
import numpy as np
import inspect
import warnings
import math
import shlex
import time as systime
import subprocess
from subprocess import Popen, PIPE

import requests

################################################################################
# JMA's ionosphere stuff
import AlbusIonosphere
import Albus_Coordinates
import jma_tools
import GPS_stations
from Albus_rnx3_to_rnx2 import convert_rnx3_to_rnx2_file








################################################################################
# Global variables
SECONDS_PER_DAY = 24.0*60.0*60.0
DAYS_PER_SECOND = 1.0/SECONDS_PER_DAY
M_DEG2RAD = math.pi/180.0
M_RAD2DEG = 180.0/math.pi

RADIUS_EARTH = 6378137.0  # in m





# See JMA Notes from 2006 Jan 10 and some of BOb Campbell's notes
SPEED_OF_LIGHT = 299792458.0    # m s^{-1}
IONOSPHERE_Kp_2 = 80.6163848    # m^3 s^{-2}
IONOSPHERE_KB = 2.79924925E10   # s^{-1} T^{-1}






# default amount of time to allow for downloading RINEX, IONEX, etc. data, in s
DEFAULT_TIMEOUT = 150
# program name to get stuff
URL_GETTER = "Albus_RINEX_download.py"
# program name to get ftp stuff by password access
FTP_GETTER = "Albus_RINEX_userftp.py"
FTP_GETTER_ALLOW = True

_ISO_COUNTRY_CODES = ['AFG', 'ALA', 'ALB', 'DZA', 'ASM', 'AND', 'AGO', 'AIA', 'ATA', 'ATG', 'ARG', 'ARM', 'ABW', 'AUS', 'AUT', 'AZE', 'BHS', 'BHR', 'BGD', 'BRB', 'BLR', 'BEL', 'BLZ', 'BEN', 'BMU', 'BTN', 'BOL', 'BES', 'BIH', 'BWA', 'BVT', 'BRA', 'IOT', 'BRN', 'BGR', 'BFA', 'BDI', 'CPV', 'KHM', 'CMR', 'CAN', 'CYM', 'CAF', 'TCD', 'CHL', 'CHN', 'CXR', 'CCK', 'COL', 'COM', 'COG', 'COD', 'COK', 'CRI', 'CIV', 'HRV', 'CUB', 'CUW', 'CYP', 'CZE', 'DNK', 'DJI', 'DMA', 'DOM', 'ECU', 'EGY', 'SLV', 'GNQ', 'ERI', 'EST', 'SWZ', 'ETH', 'FLK', 'FRO', 'FJI', 'FIN', 'FRA', 'GUF', 'PYF', 'ATF', 'GAB', 'GMB', 'GEO', 'DEU', 'GHA', 'GIB', 'GRC', 'GRL', 'GRD', 'GLP', 'GUM', 'GTM', 'GGY', 'GIN', 'GNB', 'GUY', 'HTI', 'HMD', 'VAT', 'HND', 'HKG', 'HUN', 'ISL', 'IND', 'IDN', 'IRN', 'IRQ', 'IRL', 'IMN', 'ISR', 'ITA', 'JAM', 'JPN', 'JEY', 'JOR', 'KAZ', 'KEN', 'KIR', 'PRK', 'KOR', 'KWT', 'KGZ', 'LAO', 'LVA', 'LBN', 'LSO', 'LBR', 'LBY', 'LIE', 'LTU', 'LUX', 'MAC', 'MDG', 'MWI', 'MYS', 'MDV', 'MLI', 'MLT', 'MHL', 'MTQ', 'MRT', 'MUS', 'MYT', 'MEX', 'FSM', 'MDA', 'MCO', 'MNG', 'MNE', 'MSR', 'MAR', 'MOZ', 'MMR', 'NAM', 'NRU', 'NPL', 'NLD', 'NCL', 'NZL', 'NIC', 'NER', 'NGA', 'NIU', 'NFK', 'MKD', 'MNP', 'NOR', 'OMN', 'PAK', 'PLW', 'PSE', 'PAN', 'PNG', 'PRY', 'PER', 'PHL', 'PCN', 'POL', 'PRT', 'PRI', 'QAT', 'REU', 'ROU', 'RUS', 'RWA', 'BLM', 'SHN', 'KNA', 'LCA', 'MAF', 'SPM', 'VCT', 'WSM', 'SMR', 'STP', 'SAU', 'SEN', 'SRB', 'SYC', 'SLE', 'SGP', 'SXM', 'SVK', 'SVN', 'SLB', 'SOM', 'ZAF', 'SGS', 'SSD', 'ESP', 'LKA', 'SDN', 'SUR', 'SJM', 'SWE', 'CHE', 'SYR', 'TWN', 'TJK', 'TZA', 'THA', 'TLS', 'TGO', 'TKL', 'TON', 'TTO', 'TUN', 'TUR', 'TKM', 'TCA', 'TUV', 'UGA', 'UKR', 'ARE', 'GBR', 'USA', 'UMI', 'URY', 'UZB', 'VUT', 'VEN', 'VNM', 'VGB', 'VIR', 'WLF', 'ESH', 'YEM', 'ZMB', 'ZWE','XXX']

_RINEX_SITE_LIST_EUROPE =  ['acor', 'adar', 'agrn', 'ajac', 'alac', 'alba', 'alme', 'ank2', 'aqui', 'ara2', 'argi', 'aris', 'arj6', 'asir', 'aubg', 'aut1', 'autn', 'axpv', 'baca', 'badh', 'baia', 'baut', 'bbys', 'bcln', 'bell', 'birg', 'bisk', 'bme1', 'boge', 'bogi', 'bogo', 'bolg', 'bor1', 'borj', 'borr', 'bpdl', 'brmf', 'brmg', 'brst', 'brts', 'brux', 'bscn', 'bsvz', 'bucu', 'budd', 'budp', 'bute', 'bydg', 'bzr2', 'cace', 'cag1', 'cako', 'cant', 'carg', 'casc', 'case', 'cebr', 'ceu1', 'cfrm', 'chio', 'chiz', 'clib', 'cniv', 'coba', 'como', 'cost', 'cpar', 'crak', 'creu', 'ctab', 'dare', 'delf', 'dent', 'deva', 'dgor', 'diep', 'dill', 'dlf1', 'dnmu', 'dour', 'drag', 'dub2', 'duth', 'dvcn', 'dyng', 'ebre', 'eglt', 'eijs', 'elba', 'enao', 'enis', 'entz', 'enza', 'esco', 'eusk', 'ffmj', 'fins', 'flrs', 'foyl', 'fra2', 'frne', 'func', 'gaia', 'galh', 'ganp', 'gari', 'gdrs', 'gell', 'geno', 'glsv', 'goet', 'goml', 'gop6', 'gope', 'gor2', 'grac', 'gras', 'graz', 'gsr1', 'guip', 'gwwl', 'has6', 'hel2', 'helg', 'hers', 'hert', 'hett', 'hobu', 'hofj', 'hofn', 'huel', 'ibiz', 'ieng', 'igeo', 'igm2', 'igmi', 'ignf', 'ijmu', 'ildx', 'invr', 'irbe', 'isrn', 'ista', 'izan', 'izmi', 'joe2', 'joen', 'jon6', 'joz2', 'joze', 'kad6', 'karl', 'kato', 'kda2', 'kev2', 'khar', 'kilp', 'kir0', 'kir8', 'kiru', 'kiv2', 'klnk', 'klop', 'knja', 'kos1', 'kra1', 'kraw', 'krrs', 'krs1', 'ktvl', 'kunz', 'kure', 'kuu2', 'lago', 'lama', 'lamp', 'larm', 'ldb2', 'leij', 'lek6', 'leon', 'leri', 'licc', 'lida', 'lign', 'lil2', 'linz', 'lliv', 'lodz', 'lov6', 'lpal', 'lroc', 'm0se', 'mad2', 'madr', 'mala', 'mall', 'man2', 'mar6', 'mar7', 'marp', 'mars', 'mas1', 'mat1', 'mate', 'matg', 'mdvj', 'medi', 'meli', 'mers', 'met3', 'metg', 'mets', 'mik3', 'mikl', 'mkrs', 'mlhd', 'mlvl', 'mnkw', 'mnsk', 'mogi', 'mop2', 'mopi', 'mops', 'morp', 'msel', 'muk2', 'nabg', 'newl', 'nico', 'noa1', 'nor7', 'not1', 'novp', 'npaz', 'nya1', 'nya2', 'nyal', 'obe4', 'olk2', 'ons1', 'onsa', 'orid', 'oriv', 'oros', 'osk6', 'osls', 'ost6', 'oul2', 'ove6', 'pado', 'pasa', 'pat0', 'pdel', 'penc', 'pfa3', 'pins', 'pmth', 'polv', 'pore', 'pots', 'pous', 'poze', 'ppsh', 'prat', 'pryl', 'psto', 'pstv', 'ptbb', 'pulk', 'puyv', 'pyha', 'pza2', 'qaq1', 'rabt', 'raeg', 'rah1', 'ramo', 'rant', 'redu', 'redz', 'reyk', 'riga', 'rio1', 'rivo', 'rom2', 'rove', 'rvne', 'saba', 'sala', 'sart', 'sas2', 'savu', 'sbg2', 'scil', 'scoa', 'scor', 'sfer', 'shoe', 'sjdv', 'ske0', 'ske8', 'smla', 'sneo', 'snik', 'sod3', 'soda', 'sofi', 'sons', 'sprn', 'spt0', 'spt7', 'srjv', 'stas', 'stnb', 'sul5', 'suld', 'sulp', 'sun6', 'sur4', 'sve6', 'svll', 'svtl', 'swas', 'swki', 'tar0', 'teos', 'ter2', 'terc', 'ters', 'teru', 'tit2', 'tll1', 'tlmf', 'tlse', 'tlsg', 'toil', 'tor1', 'tori', 'torn', 'trds', 'treu', 'trf2', 'trmi', 'tro1', 'tubi', 'tubo', 'tuc2', 'tuo2', 'uben', 'ucag', 'ume6', 'unpg', 'untr', 'usal', 'usdl', 'uzhl', 'vaa2', 'vaas', 'vaco', 'vae6', 'vala', 'vale', 'vars', 'ven1', 'vfch', 'vigo', 'vil0', 'vil6', 'vill', 'vir2', 'virg', 'vis0', 'vis6', 'vitr', 'vlis', 'vln1', 'vlns', 'vnrs', 'ware', 'warn', 'wrlg', 'wroc', 'wsrt', 'wtza', 'wtzr', 'wtzs', 'wtzz', 'wuth', 'yebe', 'zada', 'zara', 'zeck', 'zim2', 'zimm', 'zouf', 'zprs', 'zywi', 'zzon']

################################################################################
# Need some exception classes to help with downloding files
class No_RINEX_File_Error(IOError):
    """Indicates that no appropriate RINEX file was found on the web"""

################################################################################
class Command_Timeout_Error(IOError):
    """Indicates that the attempted command timedout"""

################################################################################
class RINEX_Data_Barf(IOError):
    """Indicates that some internal header/data part of a RINEX file is bad"""

#def get_cddis_rinex_file(url, filename):
#    print('**** in get_cddis_rinex_file')
#    print('url = ', url)
#    print('output file', filename)
#    r = requests.get(url)
# Opens a local file of same name as remote file for writing to
#    with open(filename, 'wb') as fd:
#        for chunk in r.iter_content(chunk_size=1000):
#            fd.write(chunk)
#    fd.close()

#def get_cddis_file(site_str, our_file):
#   print('processing request to cddis')
#   print('site_str', site_str)
#   print('our_file', our_file)
#   r = requests.get(site_str)
## Opens a local file of same name as remote file for writing to
#   with open(our_file, 'wb') as fd:
#       for chunk in r.iter_content(chunk_size=10000000):
#           fd.write(chunk)
## Closes local file
#   fd.close()
#   print('cddis file written out')
#  return

##################################
def set_debug_option(debug_option):
  DEBUG_SET = debug_option
  print('Albus_RINEX setting debug_option to',debug_option)

################################################################################
def get_rinex3_filename(RINEX_filename, country):
   print('incoming parameters', RINEX_filename, country)
   RINEX3_filename = None
   process = Popen(['/usr/local/bin/RX3name', RINEX_filename],stdout=PIPE)
   (output, err) = process.communicate()
   if DEBUG_SET:
     print('output', output)
   if str(output).find('Station Name not found') > -1:
      if DEBUG_SET:
        print('initial station not found \n')
      if country in _ISO_COUNTRY_CODES:
        command = '/usr/local/bin/RX3name ' + RINEX_filename +  ' 00' + country
        if DEBUG_SET:
          print('command', command, '\n')
        process =Popen(shlex.split(command), stdout=PIPE)
        (output, err) = process.communicate()
        if DEBUG_SET:
          print('second output', output, '\n')
          print('err`', err, '\n')
        if str(output).find('Station Name not found') < 0:
           RINEX3_filename = output.decode('utf-8')[:-2]
      else:
        print('invalid country')
   else:
      RINEX3_filename = output.decode('utf-8')[:-2]
   if DEBUG_SET:
     print('RINEX3_filename', RINEX3_filename)
   return  RINEX3_filename

################################################################################
def run_command_timeout(command, args, timeout):
    """run some command with a timeout limit

INPUTS:
command   I  The name of the program to run
args      I  The full argument list tot he command.  Note that this should
             be an array, with element 0 the name of the program
timeout   I  The timeout, in s.  If the program has not finished within
             timeout seconds, kill it

OUTPUTS: retval
retval    O  The return code of the command.  Note that if the process times out,
             this function raises a Command_Timeout_Error


    """
    try:
      pid = os.fork()
      if(pid == 0):
          # We are the child, execute the command
          os.execvp(command, args)
          # If we are here, something bad happened.  It must be the
          # user's fault.  :)
          os._exit(2)
      elif(pid < 0):
          # fork failed
          raise OSError("Hey, I couldn't fork!")
      for i in range(int(timeout)):
          systime.sleep(1)
          status = os.waitpid(pid,os.WNOHANG)
          if(status == (0,0)):
              # still waiting
              continue
          if(os.WIFEXITED(status[1])):
              return os.WEXITSTATUS(status[1])
          if(os.WIFSIGNALED(status[1])):
              # someone killed the process.  This is probably bad, and means the
              # user got tired of waiting.  Try calling this a timeout
              raise Command_Timeout_Error("Command '%s' signalled with %d"%(command, os.WTERMSIG(status[1])))
      os.kill(pid,signal.SIGKILL)
      raise Command_Timeout_Error("Command '%s' timed out after %d seconds"%(command, timeout))
    except:
      raise RINEX_Data_Barf("General failure of Command '%s' "%(command))

################################################################################
def run_ftpcommand_timeout(command, args, timeout, username, password):
    """run some command with a timeout limit, expecting a username and password

INPUTS:
command   I  The name of the program to run
args      I  The full argument list tot he command.  Note that this should
             be an array, with element 0 the name of the program
timeout   I  The timeout, in s.  If the program has not finished within
             timeout seconds, kill it
username  I  Text username to pass on to the program
password  I  The text password to pass to the program

OUTPUTS: retval
retval    O  The return code of the command.  Note that if the process times out,
             this function raises a Command_Timeout_Error


    """
#   p = popen2.Popen3(args, False, 0)
#   p.fromchild.close()
#   p.tochild.write("%s\n"%username)
#   p.tochild.write("%s\n"%password)
#   for i in range(int(timeout)):
#       systime.sleep(1)
#       status = p.poll()
#       if(status == -1):
#           # still waiting
#           continue
#       return status
#   os.kill(p.pid,signal.SIGKILL)
#   raise Command_Timeout_Error("Command '%s' timed out after %d seconds"%(command, timeout))
    print ( 'If you have somehow ened up in the run_ftpcommand_timeout function')
    print ( 'you should code up replacement stuff for popen')


################################################################################
def test_downloaded_RINEX(filename, min_size = 4096):
    """test whether or not a file downloaded from the web is valid"""
    if(not os.path.isfile(filename)):
#       print ( '********** test_downloaded_RINEX did not get any file mamed ', filename)
        raise IOError("No File '%s'"%filename)
    file_size = os.path.getsize(filename)
    if(file_size < min_size):
        # file < min_size bytes for RINEX data?  Bad!
        print ( '********** test_downloaded_RINEX failed with file ', filename, 'because file only has size ', file_size, ' which is less than minimum allowed ', min_size)
        try:
          command = '/bin/rm -rf ' + filename
          os.system(command)
        except:
          os.remove(compressed_file)
        raise IOError("File '%s' too small"%filename)
    return


################################################################################
def get_url(infile, outfile, min_size = 4096):
    """get a file from a URL to a local file

INPUTS:
infile   I  The complete URL to get
outfile  O  The local filename to write this to


No returns.  If there is an error, raises a No_RINEX_File_Error or
a Command_Timeout_Error
"""
    if DEBUG_SET:
      sys.stdout.write("Trying %s\n"%infile)
      print(' in get_url to get ', infile, outfile)
    try:
        computer = infile.split('/')
        if DEBUG_SET:
          print('*** computer is ', computer)
        if(len(computer) < 2):
            raise IOError("Bad computer name '%s'"%infile)
        computer = computer[2]
# It is IMPOSSIBLE  for you to get a 'True' response 
# to the following test at present
        if(computer in get_url.computer_dict):
            get_cddis_rinex_file(infile,outfile)
        else:
            run_command_timeout(URL_GETTER,
                                [URL_GETTER, infile, outfile,
                                 "%d"%DEFAULT_TIMEOUT],
                                DEFAULT_TIMEOUT)
        if DEBUG_SET:
           print('get_url had success')
    except Command_Timeout_Error:
        # if there is a file there, it is corrupted
        if(os.path.isfile(outfile)):
          try:
            command = '/bin/rm -rf ' + outfile
            if DEBUG_SET:
              print('get_url commanf timeout error')
              print('get_url executing command',command)
            os.system(command)
          except:
            os.remove(outfile)
        raise
    test_downloaded_RINEX(outfile, min_size)
    if DEBUG_SET:
       print('get_url successfully completed')
    return
get_url.computer_dict = {}
#get_url.computer_dict["jop30"] = [None,None]
#get_url.computer_dict["cddis"] = [None,None]
#get_url.computer_dict["128.243.138.173"] = [None,None]  # BIGF restricted data








################################################################################
def get_VTEC_factor(El,
                    iono_height = 300E3,
                    obs_height = 0.0
                    ):
    """get the factor f, so VTEC = f * STEC

El           I  Elevation angle, in radians
iono_height  I  height of 2-D ionosphere layer, in m
obs_height   I  height of observer above mean Earth, in m
"""
    o_1 = ((RADIUS_EARTH + obs_height) * math.cos(El)
           / (RADIUS_EARTH + iono_height))
    o_0 = math.sqrt(1.0 - o_1*o_1)
    return o_0




################################################################################
def gunzip_some_file(compressed_file,
                     uncompressed_file,
                     delete_file = 1,
                     RX3_flag=False):
 
    if DEBUG_SET:
      print ( '*** uncompressing file 1 to file 2 ', compressed_file, uncompressed_file, RX3_flag)
# if file is already uncompressed, do nothing
    if compressed_file == uncompressed_file:
      return
    # make sure there is a file
    file_size = os.path.getsize(compressed_file)
    if(file_size <= 0):
        # file < min_size bytes for RINEX data?  Bad!
        if DEBUG_SET:
          print ( '********** zero sized compressed file  ', compressed_file)
        try:
          command = '/bin/rm -rf ' + compressed_file
          os.system(command)
        except:
          os.remove(compressed_file)
        raise RINEX_Data_Barf("File too small '%s' to uncompress"%compressed_file)

    if not os.path.isfile(compressed_file):
        if(compressed_file[-1] == 'Z'):
            # try another form
            warnings.warn("No .Z form of compressed file '%s', trying .gz"%compressed_file)
            print ( "No .Z form of compressed file  trying .gz")
            new_compressed = compressed_file[:-1] + "gz"
            return gunzip_some_file(new_compressed, uncompressed_file,
                                    delete_file,RX3_flag)
        raise RINEX_Data_Barf("No such file '%s' to uncompress"%compressed_file)
    if compressed_file[-3:] != 'zip' and compressed_file[-1] != 'Z':
        try:
          command = "gunzip -dc %s > %s"%(compressed_file,uncompressed_file)
        except:
        # file is most likely uncompressed ...
          command = "mv  %s %s"%(compressed_file, uncompressed_file)
    elif (compressed_file[-3:] == 'zip'): # we have a trignet file
      command = "unzip -np %s '*d' > %s"%(compressed_file,uncompressed_file)
    else:   
      command = "gunzip -dc %s > %s"%(compressed_file,uncompressed_file)
    if DEBUG_SET:
      print ( 'gunzip executing ', command)
    retcode = os.system(command)
    if DEBUG_SET:
      print ( 'gunzip returned ', retcode)
# handle non-zero return code error from extraction 
    if RX3_flag:
       uncompressed_file = convert_rnx3_to_rnx2_file(uncompressed_file)
       if uncompressed_file == 1:
         retcode = uncompressed_file
    if(retcode):
      raise No_RINEX_File_Error("Could not run '%s'"%command)
    if(delete_file):
      try:
        command = '/bin/rm -rf ' + compressed_file
        os.system(command)
      except:
        try:
          os.remove(compressed_file)
        except:
          print ( 'failure to remove compressed file - probably not a compressed file')
    return



################################################################################
def get_RINEX_obs_file_from_web(RINEX_data,
                                year, month, day, doy,
                                output_directory = ".",
                                FTP_site = 0,
                                overwrite = 0
                                ):
    """use urllib3 to get a RINEX station observation file from the Internet

RINEX_filename    I  The nameof the RINEX 2 file to get.  Should normally be
                     in the form ssssddd0.yyx, but you already know that.
country           I  ISO country code
year              I  The 4 digit year of the observation
month             I  the month of the observation
day               I  day of the month
doy               I  day of year
output_directory  I  Where to put things
FTP_site          I  Code for where to get the data from.  The user should
                     normally not need to change this.
overwrite         I  May files be overwritten?  0 No, else yes



OUTPUTS:  None
"""
    if DEBUG_SET:
      print ('in get_RINEX_obs_file_from_web with FTP_site =', FTP_site )
    assert(FTP_site >= 0)
    RINEX_filename = RINEX_data[0]
    country = RINEX_data[1]
    if DEBUG_SET:
      print('*** get_RINEX_obs_file_from_web: original RINEX2 file and country requested', RINEX_filename, country)
  
    RX3_flag = False
    our_file = output_directory + '/' + RINEX_filename
    # First, if we have run out of FTP sites, bail
    home_dir = os.path.expanduser('~')
    netrc_file = home_dir + '/.netrc'
    if os.path.isfile(netrc_file):
       use_cddis = True
    else:
       use_cddis = False
       print ( '************* unable to handle the CDDIS site')
       warnings.warn("To access the CDDIS site you need to open an account there")
       warnings.warn("Go to https://cddis.nasa.gov/About/CDDIS_File_Download_Documentation.html")
    if(FTP_site > 11): 
        # Out of FTP sites.
        if(RINEX_filename[-1] != 'd'):
            GPS_stations.add_to_missing(RINEX_filename)
            raise No_RINEX_File_Error("Error: failure at all GPS Internet sites")
        else:
            # The user was trying to get a compact RINEX observation file.
            # Try getting a full-sized RINEX observe file and fake it.
            new_file = our_file[:-1] + 'o'
            if(os.path.isfile(new_file)):
                # Hey, there already is one
                pass
            else:
                try:
                    rinex_parms = (RINEX_filename[:-1]+'o',country)
                    get_RINEX_obs_file_from_web(rinex_parms,
                                                year, month, day, doy,
                                                output_directory,
                                                0,
                                                overwrite)
                except No_RINEX_File_Error:
                    GPS_stations.add_to_missing(RINEX_filename)
                    raise
            # Now compact it
            command = "rnx2crx %s - > %s"%(new_file,our_file)
            retcode = os.system(command)
            if(retcode):
                raise No_RINEX_File_Error("Could not run '%s'"%command)
            return
    # Second, check if the file exists
    if(os.path.isfile(our_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_file)
            os.remove(our_file)
        else:
            return
    # Third, check for a compressed file
    our_Z_file = output_directory + '/' + RINEX_filename + ".Z"
    if(os.path.isfile(our_Z_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_Z_file)
            os.remove(our_Z_file)
        else:
            gunzip_some_file(our_Z_file,our_file)
            return
    # Fourth, check that the file is not in our missing list
    if GPS_stations.check_for_missing(RINEX_filename):
        raise No_RINEX_File_Error("Error: data %s is in the missing data list"%(RINEX_filename))
    # Fifth, check for known locations of some files
    if(FTP_site == 0):
     if country == 'AUS':
         FTP_site = 6
     if country == 'NLD':
         FTP_site = 7
     if country == 'ZAF':
         FTP_site = 9
     if country == 'NZL':
         FTP_site = 10
     if(RINEX_filename[0:4] in _RINEX_SITE_LIST_EUROPE):
         FTP_site = 5
    assert(year > 1979)
    assert(year < 2080)
    yy = year - 1900
    if(year >= 2000): yy = year - 2000
    site_str = ""
    if(FTP_site == 0 or FTP_site == 1 ):
        if DEBUG_SET:
          print('calling SOPAC')
        # SOPAC California
        if FTP_site == 0:
          if(RINEX_filename[-1] == 'd'):
            site_str = "ftp://garner.ucsd.edu/pub/rinex/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
          else:
             if(RINEX_filename[-1] == 'n'):
                site_str = "ftp://garner.ucsd.edu/pub/nav/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
        else:
          if year >= 2020:   # try RINEX3
            RX3_flag = True
            RINEX3_filename = get_rinex3_filename(RINEX_filename, country)
            if DEBUG_SET:
                print('SOPAC RINEX3_filename', RINEX3_filename)
            if RINEX3_filename is None:
              pass
            else:
              our_file = output_directory + '/' + RINEX_filename 
              our_Z_file = output_directory + '/' + RINEX3_filename + '.gz'
              site_str = "ftp://garner.ucsd.edu/pub/rinex/%4.4d/%3.3d/%s.gz"%(year, doy, RINEX3_filename)
          else:
            if(RINEX_filename[-1] == 'd'):
              site_str = "ftp://garner.ucsd.edu/pub/rinex/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
            else:
              if(RINEX_filename[-1] == 'n'):
                 site_str = "ftp://garner.ucsd.edu/pub/nav/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
    elif(FTP_site == 2):
       site_str = "https://geodesy.noaa.gov/corsdata/rinex/%4.4d/%3.3d/%s/%s.gz"%(year, doy, RINEX_filename[0:4], RINEX_filename)
       our_file = output_directory + '/' + RINEX_filename 
       our_Z_file = output_directory + '/' + RINEX_filename + '.gz'
    elif(FTP_site == 3):
        # EUREF in Italy note: also has RINEX_V3 directory stuff for years >= 2010
        site_str = "ftp://geodaf.mt.asi.it/GEOD/GPSD/RINEX/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
    elif(FTP_site == 4):
        # IGN France - hum -doesn't seem to have data for 2023
        if year < 2023:
          site_str = "ftp://igs.ensg.ign.fr/pub/igs/data/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
    elif(FTP_site == 5):
        if year < 2020:
        # Belgium centre for EUREF data - now migrating to RINEX 3
          site_str = "ftp://epncb.oma.be/pub/obs/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename.upper())
        else:   # try rinex 3
          RX3_flag = True
          if DEBUG_SET:
            print('converting to RINEX3 file name from RINEX2 name: ', RINEX_filename)
          output = subprocess.Popen(['RX3name', RINEX_filename],
                          stdout=subprocess.PIPE).communicate()[0]
          RINEX3_filename = output.decode('utf-8')[:-2]
          if DEBUG_SET:
            print('Belgium RINEX3_filename', RINEX3_filename)
          our_file = output_directory + '/' + RINEX_filename
          our_Z_file = output_directory + '/' + RINEX3_filename + '.gz'
          site_str = "ftp://epncb.oma.be/pub/obs/%4.4d/%3.3d/%s.gz"%(year, doy, RINEX3_filename)

    elif(FTP_site == 6):
# !@#$#%$%^^& f*ing Aussies - they stopped reporting rinex2 files 
# and switched to rinex3 in2020
        # Australia site
        if year >= 2020:   # need RINEX3
          RX3_flag = True
          if DEBUG_SET:
            print('converting to RINEX3 file name from RINEX2 name: ', RINEX_filename)
          RINEX3_filename = get_rinex3_filename(RINEX_filename, country)
          if RINEX3_filename is None:
            pass
          else:
            our_file = output_directory + '/' + RINEX_filename 
            our_Z_file = output_directory + '/' + RINEX3_filename + '.gz'
            site_str = "sftp.data.gnss.ga.gov.au/rinex/daily/%4.4d/%3.3d/%s.gz"%(year, doy, RINEX3_filename)
        else:
          our_file = output_directory + '/' + RINEX_filename 
          our_Z_file = output_directory + '/' + RINEX_filename + '.gz'
          if DEBUG_SET:
            print('trying to get ',  our_file )
          RX3_flag = False
          site_str = "sftp.data.gnss.ga.gov.au/rinex/daily/%4.4d/%3.3d/%s.gz"%(year, doy, RINEX_filename)
        if DEBUG_SET:
          print('trying to get ',  our_file )
          print ( '*********** accessing Australian FTP site!')
          print('site string', site_str)
    elif(FTP_site == 7):
            site_str = "ftp://gnss1.tudelft.nl/rinex/%4.4d/%3.3d/%s.Z"%(year, doy, RINEX_filename)
    elif(FTP_site == 8):
        pass
    elif(FTP_site == 9):
        # South Africa Trignet
        if DEBUG_SET:
          print ( '*********** accessing South African Trignet FTP site!')
        ftp_file_name = RINEX_filename[0:7].upper()+'Z.zip'
        site_str = "ftp://ftp.trignet.co.za/RefData.%2.2d/%3.3d/L1L2_30sec/%s"%(year-2000, doy, ftp_file_name)
        our_Z_file = output_directory + '/' + ftp_file_name
    elif(FTP_site == 10):
        # New Zealand GeoNet and LINZ servers
        if DEBUG_SET:
          print ( '*********** accessing New Zealand FTP site!')
          print('nz RINEX file name ', RINEX_filename)
        if RINEX_filename[11] == 'o':
           site_str = "https://data.geonet.org.nz/gnss/rinex/%4.4d/%3.3d/%s.gz"%(year, doy, RINEX_filename)

# Getting big Rinex files from CDDIS is not yet working
    elif(FTP_site == 11 and use_cddis): # cddis
       site_str = "https://cddis.nasa.gov/archive/gnss/data/daily/%4.4d/%3.3d/%2.2d%s/%s.Z"%(year, doy, yy, RINEX_filename[-1], RINEX_filename)
    else:
        print ( '************* unable to handle ftp site number ', FTP_site)
    try:
        print('+++++++++++++++ calling get_url with', site_str, our_Z_file)
        print('*********************************')
        get_url(site_str, our_Z_file)
    except IOError:
        get_RINEX_obs_file_from_web((RINEX_filename,country),
                                    year, month, day, doy,
                                    output_directory,
                                    FTP_site+1,
                                    overwrite)
        return

    # uncompress
    gunzip_some_file(our_Z_file,our_file,RX3_flag= RX3_flag)
    return
   

################################################################################
def make_RINEX_filename(station_name,
                        RINEX_type,
                        year,
                        doy
                        ):
    """generate a RINEX filename

station_name  I  name of a RINEX station.  Must be four characters.
RINEX_type    I  Type of RINEX file.
                 B JMA GPS bias corrected STEC file
                 D JMA GPS RinexDump file
                 R JMA GPSTK ResCor output file
                 d compact RINEX observation
                 g GLONASS ephemeris
                 m meteorological data
                 n navigation/GPS ephemeris
                 o standard RINEX observation
                 s summary file
year          I  4 digit year
doy           I  day of year


OUTPUT:  filename
filename      O  filename of RINEX file, no directory path
"""
#   print ( 'year doy ', year, doy)
    station_name_short = station_name[0:4].lower()
    country = station_name[4:7]
    assert(len(station_name_short) == 4)
    assert(year > 1979)
    assert(year < 2080)
    yy = year - 1900
    if(year >= 2000): yy = year - 2000
    assert(len(RINEX_type) == 1)
    if((RINEX_type=='B')
       or(RINEX_type=='D')
       or(RINEX_type=='R')
       or(RINEX_type=='d')
       or(RINEX_type=='g')
       or(RINEX_type=='m')
       or(RINEX_type=='n')
       or(RINEX_type=='o')
       or(RINEX_type=='s')
       ):
        pass
    else:
        raise KeyError("Unsupported RINEX type '%s'"%RINEX_type)
    filename = "%4.4s%3.3d0.%2.2d%1.1s"%(station_name_short,doy,yy,RINEX_type)
    return (filename, country)

################################################################################
def make_RINEX_ephemeris_filename(group_name, best_estimate, gps_week, dow, year, doy):
    """generate a RINEX ephemeris filename

group_name    I  name of an ephemeris producing group.  Must be three characters.
gps_week      I  the GPS week number
dow           I  the GPS day of week number

OUTPUT:  filename
filename      O  filename of RINEX ephemeris file, no directory path
"""
    assert(len(group_name) == 3)
    group_name = group_name.lower()
    extension = "sp3"
    if(group_name == 'igs'): pass
    elif(group_name == 'igr'): pass
    elif(group_name == 'jpl'): 
        extension = 'sp3'
    elif(group_name == 'cod'):
        extension = "eph"
    elif(group_name == 'esa'):
        extension = "eph"
    else:
        warnings.warn("Unrecognized ephemeris group '%s', if valid, inform programmer to update code.  Trying anyway."%group_name)
    if extension == 'sp3':
       filename = "%3.3s%4.4d%1.1d.%3.3s"%(group_name, gps_week, dow, extension)
    else:
      if gps_week < 2238: # week of 27 Nov 2022
        filename = "%3.3s%4.4d%1.1d.%3.3s"%(group_name, gps_week, dow, extension)
      else:
         if best_estimate:
             start_str = group_name.upper() + '0OPSRAP_'
         else:
             start_str = group_name.upper() + '0OPSFIN_'
         end_str = '0000_01D_05M_ORB.SP3'
         filename =  "%s%4.4d%3.3d%s"%(start_str,year, doy, end_str)
    return filename


################################################################################
def make_IONEX_filename(group_name,
                        best_estimate,
                        gps_week,
                        year,
                        doy
                        ):
    """generate an IONEX filename

group_name    I  name of an IONEX producing group.  Must be three characters.
best_estimate I  flag for getting a best estimate or a rapid file
                 0 rapid
                 else best
year          I  4 digit year
doy           I  day of year


OUTPUT:  filename
filename      O  filename of IONEX file, no directory path
"""
    assert(len(group_name) == 3)
    group_name = group_name.lower()
    if(group_name == 'igs'): pass
    elif(group_name == 'jpl'): pass
    elif(group_name == 'cod'): pass
    elif(group_name == 'esa'): pass
    else:
        warnings.warn("Unrecognized IONEX group '%s', if valid, inform programmer to update code.  Trying anyway."%group_name)
    assert(year > 1979)
    assert(year < 2080)
    yy = year - 1900
    if(year >= 2000): yy = year - 2000
    b = 'g'
    if(best_estimate and group_name == 'cod'):
      group_name = 'cor'
    
    if gps_week >= 2238:  # 27 Nov 2022
# sample file: COD0OPSFIN_20230100000_01D_01H_GIM.INX
        start_str = group_name.upper() + '0OPSFIN_'
        end_str = '0000_01D_01H_GIM.INX'
        filename = "%s%4.4d%3.3d%s"%(start_str,year, doy, end_str)
    else: 
        filename = "%3.3s%1.1s%3.3d0.%2.2di"%(group_name,b,doy,yy)
    if DEBUG_SET: 
      print('IONEX filename is ', filename)
    return filename


################################################################################
def get_GPS_ephemeris_file_from_web(ephemeris_filename,
                                    year,
                                    gps_week,
                                    output_directory = ".",
                                    FTP_site = 0,
                                    overwrite = 0
                                    ):
    """use urllib3 to get an ephemeris file from the Internet

ephemeris_filename I The nameof the ephemeris file to get.  Should normally be
                     in the form ggggddd0.yyi, but you already know that.
year              I  year of the observation - needed for call to CODE
gps_week          I  the GPS week number
output_directory  I  Where to put things
FTP_site          I  Code for where to get the data from.  The user should
                     normally not need to change this.
overwrite         I  May files be overwritten?  0 No, else yes



OUTPUTS:  return_code
return_code       O  Status of getting file from web
                      0 all ok
                     -1 could not find on any FTP site
"""
    if DEBUG_SET: 
      print ( 'in get_GPS_ephemeris_file_from_web with FTP_site =', FTP_site )
    assert(FTP_site >= 0)
    # First, if we have run out of FTP sites, bail
    if(FTP_site > 6):
        GPS_stations.add_to_missing(ephemeris_filename)
        return -1
    # Second, check if the file exists
    our_file = output_directory + '/' + ephemeris_filename
    if(os.path.isfile(our_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_file)
            os.remove(our_file)
        else:
            return 0
    # Third, check for a compressed file
    if gps_week < 2238:
       our_Z_file = output_directory + '/' + ephemeris_filename + ".Z"
    else:
       our_Z_file = output_directory + '/' + ephemeris_filename + ".gz"
    if(os.path.isfile(our_Z_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_Z_file)
            os.remove(our_Z_file)
        else:
            gunzip_some_file(our_Z_file,our_file)
            return 0
    # Fourth, check that the file is not in our missing list
    if GPS_stations.check_for_missing(ephemeris_filename):
        warnings.warn("Warning: data %s is in the missing data list"%(ephemeris_filename))
        return -1
    # Where are we getting this from?
    home_dir = os.path.expanduser('~')
    netrc_file = home_dir + '/.netrc'
    if os.path.isfile(netrc_file):
       use_cddis = True
    else:
       use_cddis = False
       warnings.warn("To access the CDDIS site you need to open an account there")
       warnings.warn("Go to https://cddis.nasa.gov/About/CDDIS_File_Download_Documentation.html")

    if FTP_site == 0:
       # cddis
       if use_cddis:
         if DEBUG_SET: 
           print('TRYING CDDIS for EPHEMERIS FILE', ephemeris_filename)
         if ephemeris_filename.find('jpl') >= 0:
             site_str = "https://cddis.nasa.gov/archive/gnss/products/%4.4d/%s.Z"%(gps_week, ephemeris_filename)
         else:
           if gps_week < 2238:
              site_str = "https://cddis.nasa.gov/archive/gnss/products/%4.4d/%s.Z"%(gps_week, ephemeris_filename)
           else:
              data_file = ephemeris_filename.upper()
              site_str = "https://cddis.nasa.gov/archive/gnss/products/%4.4d/%s.gz"%(gps_week, ephemeris_filename)
         
       else: # try SOPAC - currently does not seem to work
        data_file = ephemeris_filename.upper()
        if gps_week < 2238:
          site_str = " ftp://garner.ucsd.edu/pub/products/%4.4d/%s.Z"%(gps_week, ephemeris_filename)
        else:
          site_str = " ftp://garner.ucsd.edu/pub/products/%4.4d/%s.gz"%(gps_week, ephemeris_filename)
        print('we should be using SOPAC site string:', site_str)
    elif(FTP_site == 1): 
        # CODE, Switzerland
        data_file = ephemeris_filename.upper()
        if gps_week < 2238:
          site_str = "ftp://ftp.aiub.unibe.ch/CODE/%4.4d/%s.Z"%(year, data_file)
        else:
          site_str = "ftp://ftp.aiub.unibe.ch/CODE/%4.4d/%s.gz"%(year, data_file)
        if DEBUG_SET: 
           print('we should be using CODE site string:', site_str)
    else:
        return -1
        raise KeyError("Unknown ephemeris FTP site")
    try:
        get_url(site_str, our_Z_file)
    except IOError:
        return get_GPS_ephemeris_file_from_web(ephemeris_filename,
                                               year,
                                               gps_week,
                                               output_directory,
                                               FTP_site+1,
                                               overwrite)
    # uncompress
    gunzip_some_file(our_Z_file,our_file)
    return 0




# It looks like satellite orbit data can be provided quickly from:
# ftp igscb.jpl.nasa.gov
# cd pub/product/GPS_week ... where GPS_week is the numerical value of the week




################################################################################
def get_IONEX_file_from_web(IONEX_filename,
                            year, month, day, doy,
                            output_directory = ".",
                            FTP_site = 0,
                            overwrite = 0
                            ):
    """use urllib3 to get an IONEX file from the Internet

IONEX_filename    I  The nameof the IONEX file to get.  Should normally be
                     in the form ggggddd0.yyi, but you already know that.
year              I  The 4 digit year of the observation
month             I  the month of the observation
day               I  day of the month
doy               I  day of year
output_directory  I  Where to put things
FTP_site          I  Code for where to get the data from.  The user should
                     normally not need to change this.
overwrite         I  May files be overwritten?  0 No, else yes



OUTPUTS:  return_code
return_code       O  Status of getting file from web
                      0 all ok
                     -1 could not find on any FTP site
"""
    if DEBUG_SET: 
       print ( 'in get_IONEX_file_from_web FTP_site =', FTP_site)
    assert(FTP_site >= 0)
    # First, if we have run out of FTP sites, bail
    if(FTP_site > 3):
        GPS_stations.add_to_missing(IONEX_filename)
        return -1
    # Second, check if the file exists
    our_file = output_directory + '/' + IONEX_filename
    if(os.path.isfile(our_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_file)
            os.remove(our_file)
        else:
            return 0
    # Third, check for a compressed file
    our_Z_file = output_directory + '/' + IONEX_filename + ".Z"
    if(os.path.isfile(our_Z_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_Z_file)
            os.remove(our_Z_file)
        else:
            gunzip_some_file(our_Z_file,our_file)
            return 0
    # Fourth, check that the file is not in our missing list
    if GPS_stations.check_for_missing(IONEX_filename):
        warnings.warn("Warning: data %s is in the missing data list"%(IONEX_filename))
        return -1
    # Where are we getting this from?
    assert(year > 1979)
    assert(year < 2080)
    yy = year - 1900
    if(year >= 2000): yy = year - 2000
    if(FTP_site == 0): # cddis replaced by CODE
        # CODE, Switzerland
        data_file = IONEX_filename.upper()
        site_str = "ftp://ftp.aiub.unibe.ch/CODE/%4.4d/%s.gz"%(year, data_file)
        our_Z_file = output_directory + '/' + data_file + ".gz"
    elif(FTP_site == 1):
        # CODE, Switzerland
        data_file = IONEX_filename.upper()
        site_str = "ftp://ftp.aiub.unibe.ch/CODE/%4.4d/%s.Z"%(year, data_file)
        our_Z_file = output_directory + '/' + data_file + ".Z"
    elif(FTP_site == 2):
        # CODE, Switzerland rapid file
        data_file = IONEX_filename.upper()
        site_str = "ftp://ftp.aiub.unibe.ch/CODE/%s.Z"%(data_file)
        our_Z_file = output_directory + '/' + data_file + ".gz"
    elif(FTP_site == 3):
        # IGN France
        site_str = "ftp://igs.ensg.ign.fr/pub/igs/products/ionosphere/%4.4d/%3.3d/%s.Z"%(year, doy, IONEX_filename)
    else:
        raise KeyError("Unknown IONEX FTP site")
    try:
        get_url(site_str, our_Z_file)
    except IOError:
        return get_IONEX_file_from_web(IONEX_filename,
                                       year, month, day, doy,
                                       output_directory,
                                       FTP_site+1,
                                       overwrite)
    # uncompress
    gunzip_some_file(our_Z_file,our_file)
    return 0
 








################################################################################
def get_CODE_P1P2_file_from_web(year, month, data_type = 'P2',
                                output_directory = ".",
                                overwrite = 0
                                ):
    """use urllib3 to get a P1P2 differential code bias file from CODE

year              I  The 4 digit year of the observation
month             I  the month of the observation
data_type         I  Two letter code for which data type to get
                     should be 'P2' or 'C1'
output_directory  I  Where to put things
overwrite         I  May files be overwritten?  0 No, else yes


OUTPUTS:  P1P2_filename
P1P2_filename     O  The path+name of the P1P2 differential code bias file.
                     If no file found, or other error, this will be set to
                     None.
"""
    if DEBUG_SET: 
      print ('in get_CODE_P1P2_file_from_web')
      print('incoming parameters ', year, month, data_type)
    # First, check that there is a possibility of data
    if((year > 1997) or ((year == 1997) and (month >= 10))):
        pass
    else:
        return None
    assert(year < 2080)
    yy = year - 1900
    if(year >= 2000): yy = year - 2000
    filename = ''
    if(data_type == 'P2'):
        filename = "P1P2%2.2d%2.2d_ALL.DCB"%(yy,month)
    else:
        filename = "P1C1%2.2d%2.2d.DCB"%(yy,month)
    # Second, check if the file exists
    our_file = output_directory + '/' + filename
    if(os.path.isfile(our_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_file)
            os.remove(our_file)
        else:
            return our_file
    # Third, check for a compressed file
    our_Z_file = output_directory + '/' + filename + ".Z"
    if(os.path.isfile(our_Z_file)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%our_Z_file)
            os.remove(our_Z_file)
        else:
            gunzip_some_file(our_Z_file,our_file)
            return our_file
    # Where are we getting this from?  CODE
    site_str = "ftp://ftp.aiub.unibe.ch/CODE/%4.4d/%s.Z"%(year, filename)
    try:
        # note - even P2 files from early years, e,g, 2002, can be small
        if(data_type == 'P2'):
            get_url(site_str, our_Z_file,2048)
        else:
            # data file may be tiny
            get_url(site_str, our_Z_file,450)
    except IOError:
        # Failure.  Try an earlier month
        month -= 1
        if(month <= 0):
            month = 12
            year -= 1
        return get_CODE_P1P2_file_from_web(year, month, data_type,
                                           output_directory,
                                           overwrite)
    # uncompress
    gunzip_some_file(our_Z_file,our_file)
    return our_file






################################################################################
def find_DCB_info_from_IONEX(IONEX_filename):
    """Open and read an IONEX file to find the differential code bias corrections

IONEX files sometimes have as part of their header a
DIFFERENTIAL CODE BIASES
section, which may contain information about satellite and possibly station
receiver bias values for teh different receiver frequencies.  This bias
term linearly affects the calculated ionospheric TEC value, and must be
corrected.  As part of a global solution, groups making ionospheric
models normally come up with these delays as a by-product of their models.

Rather than makign our own complicated models to find the offsets, just try to
grab someone else's values.

At this time (2006 Jul 20), I do not know which is the best way to go about this.
There are several different groups doing the calculations to make IONEX files.
There is also an average file, put in as igs*.*, which appears around
the start of 2003.  Should one use the average values in the igs file, or
the values from a single group?  I don't know which is best.  At any rate,
before 2003 the station receiver data is not included, and only the satellite
biases are given.  IONEX files only start at 1998 day 152.  Before then, you
are SOL, and must do it yourself.

I suggest the following:  On and after 2003 001, get the cod g file(the final
version), or if that does not exist, get the cod r (rapid) version.  Get the
bias data from there,  The cod analysis includes GLONASS information, which
is stripped from everyone else

If you are in the timerange 1998 152 to 2002 365, you should get the cod IONEX
file (they go all the way back while jpl does not), and just grab the
satellite stuff.

On and after 1997 October, you can get the cod monthly average biases for
satellites and stations.  Note that the data are actually the average of the
*previous* month.

Before then, you are on your own.

INPUTS:
IONEX_filename  I  The full path+name of the IONEX file to use


OUTPUTS:
sats            O  dict of the satellite biases.  Keys are integer values.
stations        O  dict of the station biases.  GPS keys are 4 letter, lowercase
                   station codes.  May be totally empty.  GLONASS keys are
                   the 4 letter station code, lower case, with an _r appended
                   (e.g. wsrt_r).
"""
    sats = {}
    stations = {}
    found_start = 0
    try:
        fp = open(IONEX_filename, "r")
    except IOError:
        raise No_RINEX_File_Error("Error: IONEX file '%s' cannot be opened"%IONEX_filename)
    for row in fp:
        if(row[0:24] == "DIFFERENTIAL CODE BIASES"):
            if(row[60:77] == "START OF AUX DATA"):
                found_start = 1
            else:
                break
        elif(found_start):
            if(row[60:76] == "PRN / BIAS / RMS"):
                # satellite
                sat_code = row[3].lower()
                if((sat_code == 'g') or (sat_code == ' ')):
                    # GPS
                    sat_number = int(row[4:6])
                    bias = float(row[6:17]) * 1E-9  # result in s
                    sats[sat_number] = bias
                elif(sat_code == 'r'):
                    # GLONASS
                    sat_number = int(row[4:6]) + 100
                    bias = float(row[6:17]) * 1E-9  # result in s
                    sats[sat_number] = bias
                elif(sat_code == 'e'):
                    # Galileo
                    sat_number = int(row[4:6]) + 200
                    bias = float(row[6:17]) * 1E-9  # result in s
                    sats[sat_number] = bias
                else:
                    fp.close()
                    raise KeyError("Unsupported satellite type '%s'"%sat_code)
            elif(row[60:80] == "STATION / BIAS / RMS"):
                # station
                station_code = row[6:10].lower()
                sat_code = row[3].lower()
                if((sat_code == 'g') or (sat_code == ' ')):
                    # GPS
                    bias = float(row[21:37]) * 1E-9  # result in s
                    stations[station_code] = bias
                elif(sat_code == 'r'):
                    # GLONASS
                    station_code = station_code + '_r'
                    bias = float(row[21:37]) * 1E-9  # result in s
                    stations[station_code] = bias
                elif(sat_code == 'e'):
                    # Galileo
                    station_code = station_code + '_e'
                    bias = float(row[21:37]) * 1E-9  # result in s
                    stations[station_code] = bias
                else:
                    fp.close()
                    raise KeyError("Unsupported satellite type '%s'"%sat_code)
    else:
        # No DCB information found
        warnings.warn("No Differential Code Bias information found in '%s'"%IONEX_filename)
    fp.close()
    return sats, stations















################################################################################
def find_DCB_info_from_CODE_P1P2(P1P2_filename):
    """Get satellite and station differential code bias values from CODE P1P2


The CODE P1P2 file ontains information about satellite and possibly station
receiver bias values for the different receiver frequencies.  This bias
term linearly affects the calculated ionospheric TEC value, and must be
corrected.  As part of a global solution, groups making ionospheric
models normally come up with these delays as a by-product of their models.

CODE (and others) put the bias information for each day into their IONEX
files.  So, if you can get the information from an IONEX file, you should
really use that instead.  But, for early IONEX files, there is no station
information, and for earlier than that, there is no IONEX information.  So
if you cannot find data in an IONEX file, you can try here.

INPUTS:
P1P2_filename   I  The full path+name of the CODE P1P2 file to use


OUTPUTS:
sats            O  dict of the satellite biases.  Keys are integer values.
stations        O  dict of the station biases.  GPS keys are 4 letter, lowercase
                   station codes.  May be totally empty.  GLONASS keys are
                   the 4 letter station code, lower case, with an _r appended
                   (e.g. wsrt_r).
"""
    sats = {}
    stations = {}
    found_start = 0
    try:
        fp = open(P1P2_filename, "r")
    except IOError:
        raise No_RINEX_File_Error("Error: P1P2 file '%s' cannot be opened"%filename)
    for row in fp:
        if(len(row) < 35): continue
        if(row[0:47] == "***   ****************    *****.***   *****.***"):
                found_start = 1
        elif(found_start):
            sat_code = row[0].lower()
            bias = float(row[21:36]) * 1E-9  # result in s
            sat_str = row[1:3]
            if(sat_str != '  '):
                # satellite
                if((sat_code == 'g') or (sat_code == ' ')):
                    # GPS
                    sat_number = int(sat_str)
                    sats[sat_number] = bias
                elif(sat_code == 'r'):
                    # GLONASS
                    sat_number = int(sat_str) + 100
                    sats[sat_number] = bias
                elif(sat_code == 'e'):
                    # Galileo
                    sat_number = int(sat_str) + 200
                    sats[sat_number] = bias
                else:
                    fp.close()
                    raise KeyError("Unsupported satellite type '%s'"%sat_code)
            else:
                # station
                station_code = row[6:10].lower()
                if((sat_code == 'g') or (sat_code == ' ')):
                    # GPS
                    stations[station_code] = bias
                elif(sat_code == 'r'):
                    # GLONASS
                    station_code = station_code + '_r'
                    stations[station_code] = bias
                elif(sat_code == 'e'):
                    # Galileo
                    station_code = station_code + '_e'
                    stations[station_code] = bias
                else:
                    fp.close()
                    raise KeyError("Unsupported satellite type '%s'"%sat_code)
    fp.close()
    return sats, stations




















################################################################################
def get_RINEX_station_XYZ(rinex_file, station_code = None):
    """read in the XYZ coordinates of a station from a RINEX obs file"""
    try:
        fp = open(rinex_file, "r")
        try:
            for row in fp:
                if(row[60:79] == "APPROX POSITION XYZ"):
                    xyz = list(map(float,row.strip().split(None,3)[0:3]))
                    fp.close()
                    return xyz
        finally:
            fp.close()
            warnings.warn("Could not find station position in '%s'"%rinex_file)
    except:
        ## If we are here, then something went wrong with reading the
        ## position from the file.  No worry, try the less accurate
        ## station positions from the list
        pass
    if(station_code in GPS_stations.GPS_stations):
        return GPS_stations.GPS_stations[station_code]
    else:
        raise Albus_RINEX.RINEX_Data_Barf("Error: could not find station position in '%s' or the GPS station list"%rinex_file)
    







################################################################################
def RINEX_to_STEC_txt(rinex_compressed,
                      rinex_nav,
                      rinex_out,
                      ionosphere_height = 300E3,
                      clear_output_file = 0,
                      overwrite = 0):
    """convert a compressed RINEX file to text-based STEC (and VTEC)

rinex_compressed   I  filename of compressed (RINEX compression, not .Z) data
rinex_nav          I  filename of navigation data (may be sp3 format)
rinex_out          I  name of desired outpt file
ionosphere_height  I  Effective height of 2-D ionosphere, in m
clear_output_file  I  If rinex_out exists, should this function delete it?
                      0 no, else Yes.  By default, this function will append.
overwrite          I  may this function overwrite files? 0 No, else yes
"""
    RINEX_R_delete = 1
    # check output file
    if(clear_output_file):
        if(os.path.isfile(rinex_out)):
            warnings.warn("File '%s' already exists.  Deleting."%data_file)
            os.remove(rinex_out)
    # check o data file
    data_file = rinex_compressed[0:-1] + 'o'
    if((os.path.isfile(data_file)) and (not overwrite)):
        warnings.warn("File '%s' already exists.  Using existing file."%data_file)
    else:
        if(os.path.isfile(data_file)):
            warnings.warn("File '%s' already exists.  Deleting."%data_file)
            os.remove(data_file)
        # make a command string to uncompress the data
        command = "crx2rnx %s - > %s"%(rinex_compressed,data_file)
        print('system executing command', command)
        retcode = os.system(command)
        if(retcode):
            raise No_RINEX_File_Error("Could not run '%s'"%command)
    # check R data file
    data_file_R = rinex_compressed[0:-1] + 'R'
    if((os.path.isfile(data_file_R)) and (not overwrite)):
        RINEX_R_delete = 0
        warnings.warn("File '%s' already exists.  Using existing file."%data_file_R)
    else:
        if(os.path.isfile(data_file_R)):
            warnings.warn("File '%s' already exists.  Deleting."%data_file_R)
            os.remove(data_file_t)
        # Now need to get position information
        xyz = get_RINEX_station_XYZ(data_file)
        # Make the dumping command
        iono_km = ionosphere_height / 1000.0
        command = "ResCor -IF%s -OF%s --nav %s --RxXYZ %.5f,%.5f,%.5f -HDf --Callow --IonoHt %.8E -DOC1 -DOP1 -DOP2 -DOL1 -DOL2 -DOD1 -DOD2 -DOS1 -DOS2 -AOAZ -AOEL -AOSR"%(data_file,data_file_R, rinex_nav, xyz[0],xyz[1],xyz[2],iono_km)
        retcode = os.system(command)
        if(retcode):
            raise No_RINEX_File_Error("Could not run '%s'"%command)
    # Now append in a nice text table
    command = "RinexDump %s > %s"%(data_file_R,rinex_out)
    retcode = os.system(command)
    if(retcode):
        raise No_RINEX_File_Error("Could not run '%s'"%command)
    # try to delete intermediate files
    if(RINEX_R_delete): os.remove(data_file_R)
    return

















################################################################################
def read_RINEX_flat_STEC_file(data_file):
    """read a flat text file of RINEX data for time, satellite, Az, El, STEC

OUTPUTS: MJD, SAT, Az, El, STEC, XYZ
MJD      O  numpy array(i) of the MJDs of the observations
Az       0  numpy array(i,s) of the Azimuths for sat s, in radians
El       0  numpy array(i,s) of the Azimuths for sat s, in radians
            If -999.0, then no data present for any of Az, El, STEC
STEC     0  numpy array(i,s) of the STEC values for sat s, in TECU
            If -999.0, then no data present for any of Az, El, STEC
"""
    # Ok, first things first.  Count the number of valid rows
    num_rows = 0
    sat_min = 0
    sat_max = -2147483648
    MJD_last = -1E300
    try:
        fp = open(data_file, "r")
    except IOError:
        raise No_RINEX_File_Error("Error: flat STEC file '%s' cannot be opened"%data_file)
    for row in fp:
        if(row[0] == '#'): continue
        r = row.strip().split()
        sat = int(r[2][1:])
        if(r[2][0] == 'G'):
            pass # standard GPS
        elif(r[2][0] == 'R'):
            sat = sat + 100   # Put GLONASS satellites at position 100+
        else:
            fp.close()
            raise Albus_RINEX.RINEX_Data_Barf("Unknown satellite type")
        if(sat > sat_max): sat_max = sat
        if(sat < sat_min): sat_min = sat
        MJD_here = jma_tools.get_MJD_from_GPS_week(int(r[0]),
                                                   float(r[1]))
        if(MJD_here != MJD_last):
            MJD_last = MJD_here
            num_rows += 1
    assert(sat_min >= 0)
    assert(sat_max < 200)
    #print ( "satellites ", sat_min, sat_max)
    sat_max += 1
    # Now allocate some space for things
    MJD = np.zeros((num_rows), dtype='Float64')
    Az = np.zeros((num_rows,sat_max), dtype='Float64')
    El = np.zeros((num_rows,sat_max), dtype='Float64')
    STEC = np.zeros((num_rows,sat_max), dtype='Float64')
    # Set the Az,El,STEC arrays to indicate bad values
    Az = Az - 999.0
    El = El - 999.0
    STEC = STEC - 999.0
    # Now read them in
    MJD_last = -1E300
    fp.seek(0,0)
    num_rows = -1
    for row in fp:
        if(row[0] == '#'): continue
        r = row.strip().split()
        MJD_here = jma_tools.get_MJD_from_GPS_week(int(r[0]),
                                                   float(r[1]))
        if(MJD_here != MJD_last):
            MJD_last = MJD_here
            num_rows += 1
        MJD[num_rows] = MJD_here
        sat = int(r[2][1:])
        if(r[2][0] == 'G'):
            pass # standard GPS
        elif(r[2][0] == 'R'):
            sat = sat + 100   # Put GLONASS satellites at position 100+
        Az[num_rows,sat] = float(r[3]) * M_DEG2RAD
        El[num_rows,sat] = float(r[6]) * M_DEG2RAD
        STEC[num_rows,sat] = float(r[9])
    fp.close()
    return MJD, Az, El, STEC





################################################################################
def write_Albus_RINEX_flat_STEC(MJD, Az, El, STEC, filename,
                                ovewrite = 0):
    """write a flat text file of Albus_RINEX for time, satellite, Az, El, STEC

MJD      I  numpy array(i) of the MJDs of the observations
Az       I  numpy array(i,s) of the Azimuths for sat s, in radians
El       I  numpy array(i,s) of the Azimuths for sat s, in radians
            If -999.0, then no data present for any of Az, El, STEC
STEC     I  numpy array(i,s) of the STEC values for sat s, in TECU
            If -999.0, then no data present for any of Az, El, STEC
filename I  name of path+file to write to
overwrite I may this function overwrite files? 0 No, else yes
"""
    assert(STEC.shape == El.shape)
    assert(STEC.shape == Az.shape)
    assert(STEC.shape[0] == MJD.shape[0])
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            raise Albus_RINEX.RINEX_Data_Barf("Error: file '%s' already exists and not allowed to delete"%filename)
    fp = open(filename, "w")
    for n in range(STEC.shape[0]):
        for s in range(STEC.shape[1]):
            if((El[n,s] < 0.0) or (STEC[n,s] == -999.0)): continue
            line = "%20.12f%5d%10.4f%10.4f%15.8f\n"%(MJD[n],s,Az[n,s]*M_RAD2DEG,El[n,s]*M_RAD2DEG,STEC[n,s])
            fp.write(line)
    fp.close()
    return


################################################################################
def read_Albus_RINEX_flat_STEC(filenames):
    """reads text files of Albus_RINEX data for time, satellite, Az, El, STEC

INPUTS:
filenames I  A list of filenames to read data from.  Note that the data
             within the list must be in increasing time order

OUTPUTS: MJD, SAT, Az, El, STEC, XYZ
MJD      O  numpy array(i) of the MJDs of the observations
Az       0  numpy array(i,s) of the Azimuths for sat s, in radians
El       0  numpy array(i,s) of the Azimuths for sat s, in radians
            If -999.0, then no data present for any of Az, El, STEC
STEC     0  numpy array(i,s) of the STEC values for sat s, in TECU
            If -999.0, then no data present for any of Az, El, STEC
"""
    # Ok, first things first.  Count the number of valid rows
    num_rows = 0
    sat_min = 2147483647
    sat_max = -2147483648
    MJD_last = -1E300
    for f in filenames:
        try:
            fp = open(f, "r")
        except IOError:
            raise No_RINEX_File_Error("Error: IONEX file '%s' cannot be opened"%f)
        for row in fp:
            if(row[0] == '#'): continue
            MJD_here = float(row[0:20])
            sat = int(row[20:25])
            if(sat > sat_max): sat_max = sat
            if(sat < sat_min): sat_min = sat
            if(MJD_here < MJD_last):
                fp.close()
                raise Albus_RINEX.RINEX_Data_Barf("Error, time not increasing")
            if(MJD_here != MJD_last):
                MJD_last = MJD_here
                num_rows += 1
        fp.close()
    assert(sat_min >= 0)
    assert(sat_max < 200)
    #print ( "satellites ", sat_min, sat_max)
    sat_max += 1
    # Now allocate some space for things
    MJD = np.zeros((num_rows), dtype='Float64')
    Az = np.zeros((num_rows,sat_max), dtype='Float64')
    El = np.zeros((num_rows,sat_max), dtype='Float64')
    STEC = np.zeros((num_rows,sat_max), dtype='Float64')
    # Set the Az,El,STEC arrays to indicate bad values
    Az = Az - 999.0
    El = El - 999.0
    STEC = STEC - 999.0
    # Now read them in
    MJD_last = -1E300
    num_rows = -1
    for f in filenames:
        try:
            fp = open(f, "r")
        except IOError:
            raise No_RINEX_File_Error("Error: IONEX file '%s' cannot be opened"%f)
        for row in fp:
            if(row[0] == '#'): continue
            MJD_here = float(row[0:20])
            sat = int(row[20:25])
            if(MJD_here != MJD_last):
                MJD_last = MJD_here
                num_rows += 1
            MJD[num_rows] = MJD_here
            Az[num_rows,sat] = float(row[25:35]) * M_DEG2RAD
            El[num_rows,sat] = float(row[35:45]) * M_DEG2RAD
            STEC[num_rows,sat] = float(row[45:60])
            #print ( "Read in ",MJD[num_rows],sat,Az[num_rows,sat],El[num_rows,sat],STEC[num_rows,sat])
        fp.close()
    return MJD, Az, El, STEC








################################################################################
def standard_RINEX_bias_correction(STEC,
                                   station_code,
                                   sat_bias_IONEX,
                                   station_bias_IONEX,
                                   sat_bias_CODE_monthly,
                                   station_bias_CODE_monthly
                                   ):
    """standard bias corrections for RINEX data read in with GPSTK

This function will perform standard bias corrections, using bias time
dictionaries read in from other datasets.  It also performs some
scaling problems introduced by deficiencies in the GPSTK software.

First, this function corrects for the scaling error introduced by
GPSTK using slightly incorrect coefficients for the delay to TECU
conversion.  In this stage, it will also make corrections for the
frequency differences of the GLONASS satellites, if necessary.  (GPSTK
seems to treat GPS and GLONASS the same, but there is a small difference.)

Then, it will apply a differential frequency bias.  Both the satellites and
the ground station receivers have bias delays between the two different
frequencies, L1 and L2.  This function will correct for the satellite and
station biases, as appropriate, for GPS or GLONASS satellites.  (Note that
I use the convention that GLONASS satellites are numbered 100--199, even
though they are called R00--R99 in the RINEX stuff.  CODE does this as well,
so I'm not goiong too far out on a limb.  But wait for Galileo ....)

If the biases exist in the IONEX dictionaries, then this function uses them.
Otherwise, it tries to get them from the monthly average files from CODE.
If both of these fail, then it defaults to 0.0 for the bias.


INPUTS:
STEC                       I  A np array for slant TEC values, as
                              STEC[row,sat], with sat being the satellite
                              number (GLONASS satellites have 100 added to the
                              satellite number).  In TECU.  
station_code               I  4 letter code for a station position.
sat_bias_IONEX             I  dictionary for satellite biases from IONEX dataset,
                              in s.  The station dictionary keys are integer
                              values, with GLONASS satellites having 100 added to
                              them.
station_bias_IONEX         I  dictionary for station biases from IONEX dataset,
                              in s.  The station dictionary keys are 4 letter
                              codes with GLONASS stuff adding an appended
                              '_r'.
sat_bias_CODE_monthly      I  dictionary for satellite biases from CODE monthly
                              average files, in s
station_bias_CODE_monthly  I  dictionary for station biases from CODE monthly
                              average files, in s

"""
    station_code = station_code.lower()
    ## SOme numbers to help with biases and scaling issues
    ## Make a correction for GPSTK stuff.  They use 1/40.28
    ## where I calculate 2/IONOSPHERE_Kp_2, which is slightly
    ## different.
    TEC_scale_correct = 40.28 * 2.0 / IONOSPHERE_Kp_2
    nu_L1 = 1575.42E6          # GPS value, in Hz
    nu_L2 = nu_L1 * 60.0/77.0  # GPS value, in Hz
    GPS_freq_factor = nu_L1*nu_L1*nu_L2*nu_L2 / (nu_L1*nu_L1 - nu_L2*nu_L2)

    for sat in range(STEC.shape[1]):
        # satellite
        if(sat in sat_bias_IONEX):
            sat_bias = sat_bias_IONEX[sat]
        elif(sat in sat_bias_CODE_monthly):
            sat_bias = sat_bias_CODE_monthly[sat]
        else:
            sat_bias = 0.0
        # set up for satellite differences
        if(sat < 100):
            # GPS
            this_station_code = station_code
            STEC_factor = 2.0 / IONOSPHERE_Kp_2 * GPS_freq_factor * 1E-16
            freq_correction = 1.0
        elif(sat < 200):
            # GLONASS
            this_station_code = station_code + '_r'
            nu_G1 = 1600.0E6
            nu_G2 = nu_G1 * 7.0/9.0
            GLONASS_freq_factor = nu_G1*nu_G1*nu_G2*nu_G2
            GLONASS_freq_factor /= (nu_G1*nu_G1 - nu_G2*nu_G2)
            STEC_factor = 2.0 / IONOSPHERE_Kp_2 * GLONASS_freq_factor * 1E-16
            ## The GPSTK software does not account for the
            ## frequency difference properly
            freq_correction = GLONASS_freq_factor / GPS_freq_factor
        elif(sat < 300):
            # Galileo
            this_station_code = station_code + '_e'
            nu_E1 = 1575.42E6
            nu_E5 = 1176.45E6
            Galileo_freq_factor = nu_E1*nu_E1*nu_E5*nu_E5
            Galileo_freq_factor /= (nu_E1*nu_E1 - nu_E5*nu_E5)
            STEC_factor = 2.0 / IONOSPHERE_Kp_2 * Galileo_freq_factor * 1E-16
            ## The GPSTK software does not account for the
            ## frequency difference properly
            freq_correction = Galileo_freq_factor / GPS_freq_factor
        else:
            raise KeyError("Unknown satellite type")
        if(this_station_code in station_bias_IONEX):
            station_bias = station_bias_IONEX[this_station_code]
        elif(this_station_code in station_bias_CODE_monthly):
            station_bias = station_bias_CODE_monthly[this_station_code]
        else:
            station_bias = 0.0
        # form the total bias delay
        bias = sat_bias + station_bias
        TEC_bias = bias * SPEED_OF_LIGHT * STEC_factor
        #print ( "For satellite %d, bias is %.3f from %E %E"%(sat,TEC_bias,sat_bias,station_bias))
        ## Now run through each row
        for n in range(STEC.shape[0]):
            this_stec = STEC[n,sat]
            if(this_stec == -999.0):
                continue # bad data flag in JMA modified GPSTK
            #print ( "Original STEC for satellite %3d was %10.3f"%(sat,this_stec))
            ## Ok, correct for the coefficient problem
            this_stec = this_stec * TEC_scale_correct
            # correct for frequecy issues
            this_stec = this_stec * freq_correction
            #print ( "New STEC is %10.3f from bias %10.3E, %10.3f  %10.3f"%(this_stec + TEC_bias,bias,this_stec,TEC_bias))
            STEC[n,sat] = this_stec + TEC_bias
    return











################################################################################
def standard_RINEX_bias_correction_wrapper(STEC,
                                           station_code,
                                           year, month, day, doy,
                                           output_directory = ".",
                                           overwrite = 0
                                           ):
    """wrapper function to handle standard_RINEX_bias_correction calls

See standard_RINEX_bias_correction for details on the bias corrections.  This
function handles getting the bias correction datafiles and then sending the
information off to the mathematical part of the correction.

STEC                       I  A np array for slant TEC values, as
                              STEC[row,sat], with sat being the satellite
                              number (GLONASS satellites have 100 added to the
                              satellite number).  In TECU.  
station_code               I  4 letter code for a station position.
year                       I  The 4 digit year of the observation
month                      I  the month of the observation
day                        I  day of the month
doy                        I  day of year
output_directory           I  Where to put things
overwrite                  I  May files be overwritten?  0 No, else yes


"""
    print ( '*** in standard_RINEX_bias_correction_wrapper')
    # Ok, check against the last call.
    if((year == standard_RINEX_bias_correction_wrapper.last_year)
       and (month == standard_RINEX_bias_correction_wrapper.last_month)
       and (day == standard_RINEX_bias_correction_wrapper.last_day)
       and (doy == standard_RINEX_bias_correction_wrapper.last_doy)):
        pass # Nothing to do here
    else:
        standard_RINEX_bias_correction_wrapper.last_year = year
        standard_RINEX_bias_correction_wrapper.last_month = month
        standard_RINEX_bias_correction_wrapper.last_day = day
        standard_RINEX_bias_correction_wrapper.last_doy = doy
        # Now get the new bias files.  Start with IONEX
        IONEX_name = make_IONEX_filename('cod',0,year,doy)
        if DEBUG_SET: 
            print ( '.... IONEX_name', IONEX_name)
        return_code = get_IONEX_file_from_web(IONEX_name,
                                              year, month, day, doy,
                                              output_directory,
                                              overwrite = overwrite)
        if(return_code < 0):
            IONEX_name = make_IONEX_filename('cod',1,year,doy)
            if DEBUG_SET: 
                print ( '....1  IONEX_name', IONEX_name)
            return_code = get_IONEX_file_from_web(IONEX_name,
                                                  year, month, day, doy,
                                                  output_directory,
                                                  overwrite = overwrite)
        if(return_code >= 0):
            IONEX_filename = output_directory + '/' + IONEX_name
            if DEBUG_SET: 
              print ( '.... final IONEX_name', IONEX_name)
            s1,s2 = find_DCB_info_from_IONEX(IONEX_filename)
        else:
            s1 = {}
            s2 = {}
        standard_RINEX_bias_correction_wrapper.sat_bias_IONEX = s1
        standard_RINEX_bias_correction_wrapper.station_bias_IONEX = s2
        # Now do the CODE P1P2 file
        P1P2_filename = get_CODE_P1P2_file_from_web(year, month, 'P2',
                                                    output_directory,
                                                    overwrite)
        
        P1P2_filename = None
        if(P1P2_filename is not None):
            s1,s2 = find_DCB_info_from_CODE_P1P2(P1P2_filename)
        else:
            s1 = {}
            s2 = {}
        standard_RINEX_bias_correction_wrapper.sat_bias_CODE_monthly = s1
        standard_RINEX_bias_correction_wrapper.station_bias_CODE_monthly = s2
    # Now do the bias correction for the station
    standard_RINEX_bias_correction(STEC,
                                   station_code,
                                   standard_RINEX_bias_correction_wrapper.sat_bias_IONEX,
                                   standard_RINEX_bias_correction_wrapper.station_bias_IONEX,
                                   standard_RINEX_bias_correction_wrapper.sat_bias_CODE_monthly,
                                   standard_RINEX_bias_correction_wrapper.station_bias_CODE_monthly)
    return
standard_RINEX_bias_correction_wrapper.last_year = None
standard_RINEX_bias_correction_wrapper.last_month = None
standard_RINEX_bias_correction_wrapper.last_day = None
standard_RINEX_bias_correction_wrapper.last_doy = None
standard_RINEX_bias_correction_wrapper.sat_bias_IONEX = None
standard_RINEX_bias_correction_wrapper.station_bias_IONEX = None
standard_RINEX_bias_correction_wrapper.sat_bias_CODE_monthly = None
standard_RINEX_bias_correction_wrapper.station_bias_CODE_monthly = None









################################################################################
def rough_satellite_bias_correction(S, E):
    """perform a rough correction for satellite timing biases

Satellite transmitter timing biases can cause apparent negative STEC
values.  Remove them by forcing all STEC values to be at least 0.5 TECU
(a nominal minimum STEC value for Solar minimum).

Take in a list of STEC np arrays.  For each satellite, over all stations,
find the TECU offset which makes all STEC values for all stations for that
satellite > 0.5 TECU.  The satellite timing bias should cause a linear offset
in the TECU value measured.

Don't trust anything below 20 degrees elevation

Something to do for the future would be to make a rough correction for the
station receiver biases as well.

S            I  list of STEC np arrays
E            I  corresponding list of Elevation np arrays
"""
    elevation_limit = 20.0 * M_DEG2RAD
    # How many stations total?
    max_num_stations = 0
    for stec in S:
        num_sat = stec.shape[1]
        if(num_sat > max_num_stations):
            max_num_stations = num_sat
    for s in range(max_num_stations):
        min_STEC = +1E300
        min_STEC2 = +1E300
        for stec, el in zip(S,E):
            if(stec.shape[1] <= s): continue
            for n in range(stec.shape[0]):
                if(el[n,s] < elevation_limit): continue
                this_stec = stec[n,s]
                if(this_stec == -999.0): continue   # JMA modified
                                                    ## GPSTK uses -999.0 as flag
                if(this_stec < min_STEC):
                    if(this_stec < min_STEC2):
                        min_STEC = min_STEC2
                        min_STEC2 = this_stec
                    else:
                        min_STEC = this_stec
        if(DEBUG_SET):
            print ( "For satellite %2d got bias min %10.3f"%(s,min_STEC))
        if(min_STEC == +1E300): continue
        if(min_STEC > 0.5): continue   # no correction for satellite biases
                                       ## which are high
        offset = 0.5 - min_STEC
        for stec in S:
            if(stec.shape[1] <= s): continue
            for n in range(stec.shape[0]):
                this_stec = stec[n,s]
                if(this_stec == -999.0): continue   # JMA modified
                                                    ## GPSTK uses -999.0 as flag
                stec[n,s] += offset
    return


















################################################################################
def get_station_RINEX_stec(station_code,
                           year, month, day,
                           fraction_start, fraction_end,
                           data_directory = "./",
                           main_file_overwrite = 1,
                           overwrite = 0
                           ):
    """get RINEX information for GPS (and Galileo ???) info, time and STEC

Using a station-based RINEX file, convert the RINEX data to a flat
text-based file with time, satellite, Az, El and STEC values.  Do this
for the range of valid times.  Then, read the data in to arrays.

station_code      I  4-character code for a station.  See RINEX information.
year              I  4 digit year of start of observations
month             I  month of start
day               I  day of start
fraction_start    I  day fraction of start of observations
fraction_end      I  day fraction of end of observations
data_directory    I  Where is all of this stuff at?
main_file_overwrite I  May the main text file be overwritten? 0 No, else yes.
                     You probably want 1.
overwrite         I  May files be overwritten?  0 No, else yes.

OUTPUTS: MJD, Az, El, STEC, XYZ
MJD      O  np(i) of the MJDs of the observations
Az       0  np(i,s) of the Azimuths for sat s, in radians
El       0  np(i,s) of the Azimuths for sat s, in radians
            If -999.0, then no data present for any of Az, El, STEC
STEC     0  np(i,s) of the STEC values for sat s, in TECU
            If -999.0, then no data present for any of Az, El, STEC
xyz      O  array of station's XYZ position, in m
"""
    # Ok, when is the start?
    MJD_start = jma_tools.get_MJD(year, month, day)
    MJD_end = MJD_start + fraction_end
    MJD_start = MJD_start + fraction_start
    assert(MJD_start <= MJD_end)
    # Now, for all times, convert the data
    data_file_list = []
    for MJD in range(int(MJD_start),int(MJD_end)+1):
        if DEBUG_SET:
           print ( "Doing day", MJD)
        # create the bias corrected filename
        y,m,d,f = jma_tools.get_ymdf_from_JD(jma_tools.get_JD_from_MJD(MJD))
        doy = jma_tools.get_day_of_year(y,m,d)
        RINEX_B_name = make_RINEX_filename(station_code, 'B', y, doy)
        RINEX_B_path = data_directory + '/' + RINEX_B_name
        if DEBUG_SET:
          print ( "Making file ", RINEX_B_path)
        # Check if it exists
        if((os.path.isfile(RINEX_B_path)) and (not main_file_overwrite)):
            warnings.warn("File '%s' already exists.  Using existing file."%RINEX_B_path)
        else:
            if(os.path.isfile(RINEX_B_path)):
                warnings.warn("File '%s' already exists.  Deleting."%RINEX_B_path)
                os.remove(RINEX_B_path)
            # Need a Dump file
            RINEX_Dump = make_RINEX_filename(station_code,'D', y, doy)
            RINEX_D_path = data_directory + '/' + RINEX_Dump
            RINEX_D_delete = 1
            if((os.path.isfile(RINEX_D_path)) and (not overwrite)):
                RINEX_D_delete = 0
                warnings.warn("File '%s' already exists.  Using existing file."%RINEX_D_path)
            else:
                if(os.path.isfile(RINEX_D_path)):
                    warnings.warn("File '%s' already exists.  Deleting."%RINEX_D_path)
                    os.remove(RINEX_D_path)
                rinex_parms = make_RINEX_filename(station_code,'d', y, doy)
                nav_in = make_RINEX_filename('brdc','n', y, doy)
                rinex_gl = make_RINEX_filename('brdc','g', y, doy)
                try:
                    get_RINEX_obs_file_from_web(rinex_parms, y,m,d,doy,
                                                data_directory,
                                                overwrite = overwrite)
                    get_RINEX_obs_file_from_web(nav_in, y,m,d,doy,
                                                data_directory,
                                                overwrite = overwrite)
                    # the GLONASS data is not so important
                    try:
                        get_RINEX_obs_file_from_web(rinex_gl, y,m,d,doy,
                                                    data_directory,
                                                    overwrite = overwrite)
                    except No_RINEX_File_Error:
                        pass
                    data_in = data_directory + '/' + data_in
                    nav_in = data_directory + '/' + nav_in
                    assert(os.path.isfile(data_in))
                    assert(os.path.isfile(nav_in))
                    RINEX_to_STEC_txt(data_in,nav_in, RINEX_D_path,
                                      overwrite=overwrite)
                except No_RINEX_File_Error:
                    ## No data! Hopefully this is not too serious.
                    ## Fix up something and put blank data in RINEX_D_path
                    blank_fp = open(RINEX_D_path, "w")
                    blank_fp.close()
            # Now read in the data
            MJD, Az, El, STEC = read_RINEX_flat_STEC_file(RINEX_D_path)
            if(RINEX_D_delete): os.remove(RINEX_D_path)
            # Perform the bias corrections
            standard_RINEX_bias_correction_wrapper(STEC, station_code,
                                                   y, m, d, doy,
                                                   data_directory, overwrite)
            write_Albus_RINEX_flat_STEC(MJD, Az, El, STEC,RINEX_B_path,overwrite)
        data_file_list.append(RINEX_B_path)
        # Need the original data file for the XYZ position
        RINEX_o = make_RINEX_filename(station_code,'o', y, doy)
        # Get the station XYZ
        xyz = get_RINEX_station_XYZ(data_directory + '/' + RINEX_o, station_code)
    # Now read in the whole sequence of data
    MJD, Az, El, STEC = read_Albus_RINEX_flat_STEC(data_file_list)
    return MJD, Az, El, STEC, xyz








################################################################################
def find_interpolation_point_bisect(data,
                                    value
                                    ):
    """Find an index to interpolate between two points in a sorted array

Given some generic (probably np) array (1-D), which is in increasing
order, find an index which gives a good point to interpolate between.
Returns index, where   data[index] <= value < data[index+1]
under most circumstances, unless value is outside of the bounds of data.
(In which case, index would be 0 for low value, or len(data)-2 for high.)

This function uses bisection to find the best index, which is fast for
random value calls, but may be slow if you are calling in time sequence

data        I  1-D array of values, sorted in increasing time order
value       I  value to bracket
"""
    size = len(data)
    if(size <= 2):
        return 0
    top = size-2
    bottom = 0
    index = int(size/2)
    while(1):
        if(data[index] <= value):
            # index should be no lower
            bottom = index
        else:
            # index should be no higher
            top = index
        if((top == bottom) or ((top - bottom) == 1)):
            return bottom
        index = int((top+bottom+1)/2)
    # stop Xemacs bugginess
    return







################################################################################
def find_interpolation_point(data,
                             value,
                             last_index
                             ):
    """Find an index to interpolate between two points in a sorted array

Given some generic (probably np) array (1-D), which is in increasing
order, find an index which gives a good point to interpolate between.
Returns index, where   data[index] <= value < data[index+1]
under most circumstances, unless value is outside of the bounds of data.
(In which case, index would be 0 for low value, or len(data)-2 for high.)

The main point of this function is that it assumes that you are going through
an array in some sort of time order, so that the next index that you need
for interpolation is probably very close to the last.  If you are searching
for random values in an array, then this is *NOT* the function to use.

data        I  1-D array of values, sorted in increasing time order
value       I  value to bracket
last_index  I  The last index as to where the nearest time point was
               found.  Set to None to start over
"""
    if(last_index == None): return find_interpolation_point_bisect(data, value)
    assert(last_index >= 0)
    size = len(data)
    assert(last_index < size)
    # try doing down
    index = last_index
    while(index > 0):
        if(data[index] > value):
            index -= 1
        else:
            break
    # Check for increasing index
    while(index < size-2):
        if(data[index+1] <= value):
            index += 1
        else:
            break
    # Ok, this is it
    return index








################################################################################
def find_nearest_point(data,
                       value,
                       last_index
                       ):
    """Find the nearest index for value in the data array

Given some generic (probably np) array (1-D), which is in increasing
order, find an index which has the smallest difference between value and
data[index].

The main point of this function is that it assumes that you are going through
an array in some sort of time order, so that the next index that you need
is probably very close to the last.  If you are searching
for random values in an array, then this is *NOT* the function to use.

data        I  1-D array of values, sorted in increasing time order
value       I  value to bracket
last_index  I  The last index as to where the nearest time point was
               found.  Set to -1 to start over

"""
    if(last_index == None):
        last_index = find_interpolation_point_bisect(data, value)
    assert(last_index >= 0)
    size = len(data)
    assert(last_index < size)
    last_delta = math.fabs(data[last_index] - value)
    if(last_delta == 0.0): return last_index
    # try doing down
    index = last_index -1
    while(index >= 0):
        this_delta = math.fabs(data[index] - value)
        if(last_delta > this_delta):
            last_index = index
            last_delta = this_delta
            index -= 1
        else:
            break
    # Check for increasing index
    index = last_index +1
    while(index <= size-1):
        this_delta = math.fabs(data[index] - value)
        if(last_delta > this_delta):
            last_index = index
            last_delta = this_delta
            index += 1
        else:
            break
    # Ok, this is it
    return last_index




################################################################################
def find_MJD_range(MJD,
                   MJD_center,
                   full_width,
                   last_bottom,
                   last_top
                   ):
    """Find an index range (python notation) for a block of time in an array

Given an array (probably a np) of MJD times in MJD, and a window
of desired time specified by MJD_center and full_width, find all of the
indices of MJD which are within the window.  Note that the returned range
is pythonish, so that you can use range(index_bottom, index_top).

The main point of this function is that it assumes that you are going through
an array in some sort of time order, so that the next index that you need
is probably very close to the last.  If you are searching
for random values in an array, then this is *NOT* the function to use.


MJD            I  array of Modified Julian Dates
MJD_center     I  center of desired time window, as a Modified Julian Date
full_width     I  full width of the desired window, in seconds
last_bottom    I  the last index for the range bottom from a previous call.
                  Set to None to start over.
last_top       I  the last index for the range top from a previous call


OUTPUTS:  index_bottom, index_top
index_bottom   O  the bottom index to use for an range(index_bottom, index_top)
                  call.  If there are no MJDs in the window, then this
                  is None
index_top      O  the top index to use for an range(index_bottom, index_top)
                  call.  If there are no MJDs in the window, then this
                  is None

"""
    half_width = full_width * DAYS_PER_SECOND
    # Fudge the MJD bottom and top values by 1E-10 days to account for
    # possible roundoff problems.  MJD values are ~50 000, so 1E-10 days
    # is about the roundoff area of a double.  It corresponds to
    # 8.64E-6 s.
    MJD_bottom = MJD_center - half_width - 1E-10
    MJD_top    = MJD_center + half_width + 1E-10
    if(last_bottom == None):
        last_bottom = find_interpolation_point_bisect(MJD, MJD_bottom)
        last_top    = find_interpolation_point_bisect(MJD, MJD_top)
    assert(last_bottom >= 0)
    assert(last_top >= 0)
    size = len(data)
    assert(last_bottom < size)
    assert(last_top < size)
    # try doing down
    while(last_bottom > 0):
        index = last_bottom -1
        if(MJD[index] >= MJD_bottom):
            last_bottom = index
        else:
            break
    while(last_top > 0):
        index = last_top -1
        if(MJD[index] > MJD_top):
            last_top = index
        else:
            break
    # Check for increasing index
    while(last_bottom < size-1):
        index = last_bottom +1
        if(MJD[index] <= MJD_bottom):
            last_bottom = index
        else:
            break
    while(last_top < size):
        if(MJD[last_top] <= MJD_top):
            last_top += 1
        else:
            break
    # Ok, check for out of range
    if((last_top == 0) or (MJD[size-1] < MJD_bottom)):
        return None, None
    return last_bottom, last_top







################################################################################
def get_RINEX_points_at_index(index,
                              Az, El, STEC,
                              Az_target,
                              El_target
                              ):
    """Get all of the data values for all of the satellites at some time

index         I  The MJD time index to look up
Az            I  The Azimuth array (in radians) as Az[index,sat], where sat
                 is the satellite number.  A np  in radians
El            I  The Elevation array (in radians) as El[index,sat], where sat
                 is the satellite number.  A np.  in radians
STEC          I  The STEC array (in TECU) as STEC[index,sat], where sat
                 is the satellite number.  A np.  In TECU
Az_target     I  The target azimuth, in radians
El_target     I  The target elevation, in radians


OUTPUTS:  L
L             O  An array of array values, where
                 L[i] = [dist, Az, El, STEC],
                 where dist is the angulat distance to the target, in radians,
                 Az is the Azimuth of the satellite, in radians,
                 El is the Elevation of the satellite, in radians,
                 STEC is the STEC of the satellite, in TECU.
                 This array l has been sorted in order of increasing dist.
"""
    num_sat = Az.shape[1]
    L = []
    for s in range(num_sat):
        if(El[index,s] <= 0.0): continue
        if(STEC[index,s] == -999.0): continue  # JMA modified
                                               ##GPSTK uses -999.0 for a bad code
        az_s = Az[index,s]
        el_s = El[index,s]
        st_s = STEC[index,s]
        dist = Albus_Coordinates.angular_separation(az_s, el_s,
                                                    Az_target, El_target)
        A = [dist, az_s, el_s, st_s, s]
        L.append(A)
    B = [ (C[0], C) for C in L ]
    B.sort()
    L[:] = [ D[1] for D in B ]
    return L







################################################################################
def average_RINEX_SVTEC(L,
                        El_target,
                        max_dist = 10.0,
                        weight_scheme = 'N',
                        iono_height = 300E3,
                        obs_height = 0.0
                        ):
    """Make an average  of RINEX VTECs

L             I  List of RINEX VTECs for one time, as might come from
                 get_RINEX_points_at_index
El_target     I  The elevation angle of the target object
max_dist      I  The maximum angular separation to allow, in radians
weight_scheme I  Type of weighting to use.
                 'N'  weight eiach point the same
                 'I'  weight each point by 1/(0.05 + dist)
iono_height   I  height of 2-D ionosphere layer, in m
obs_height    I  height of observer above mean Earth, in m



OUTPUTS:  mean_VTEC, target_STEC
mean_VTEC     O  the mean VTEC, in TECU
target_STEC   O  The STEC in the direction of the target, using the mean_VTEC
                 in TECU
"""
    sum = 0.0
    sum_weight = 0.0
    for s in L:
        if(s[0] < max_dist):
            VTEC = get_VTEC_factor(s[2], iono_height, obs_height) * s[3]
            if(weight_scheme == 'N'):
                weight = 1.0
            elif(weight_scheme == 'I'):
                weight = 1.0 / (0.05 + s[0])
            else:
                raise KeyError("Unknown weight key '%s'"%weight_scheme)
            #print ( "Using VTEC, weight from Az,El, sat, STEC", VTEC, weight,s[1]*M_RAD2DEG,s[2]*M_RAD2DEG,s[4],s[3])
            sum += VTEC * weight
            sum_weight += weight
        else:
            break
    if(sum_weight > 0.0):
        mean_VTEC = sum / sum_weight
    else:
        warnings.warn("No points within max separation %E, using first point"%max_dist)
        mean_VTEC = get_VTEC_factor(L[0][2], iono_height, obs_height) * L[0][3]
    target_STEC = mean_VTEC / get_VTEC_factor(El_target, iono_height, obs_height)
    #print ( "I got mean_VTEC,target_STEC", mean_VTEC, target_STEC)
    return mean_VTEC, target_STEC














################################################################################
def get_average_RINEX_SVTEC_at_MJD(MJD_target,
                                   Az_target,    
                                   El_target,
                                   last_index,
                                   MJD, Az, El, STEC,
                                   max_dist = 10.0,
                                   weight_scheme = 'N',
                                   iono_height = 300E3,
                                   obs_height = 0.0
                                   ):
    """get the (interpolated) VTEC STEC for some MJD

Given some MJD, Az, El, STEC arrays from get_station_RINEX_stec, calculate
the VTEC and STEC value for some target position at the specified MJD.
This function assumes that you will be making multiple calls to this
function for an ordered sequence of MJD's, so it asks for an index into
the MJD, Az, El, STEC to start searching for times.


MJD_target     I  The MJD of the target time
Az_target      I  The target azimuth, in radians
El_target      I  The target elevation, in radians
last_index     I  The last index as to where the nearest time point was
                  found.  Set to None to start over
MJD            I  np(i) of the MJDs of the RINEX data
Az             I  np(i,s) of the Azimuths for sat s, in radians
El             I  np(i,s) of the Azimuths for sat s, in radians
                  If -999.0, then no data present for any of Az, El, STEC
STEC           I  np(i,s) of the STEC values for sat s, in TECU
                  If -999.0, then no data present for any of Az, El, STEC
max_dist       I  The maximum angular separation to allow, in radians
weight_scheme  I  Type of weighting to use.
                  'N'  weight eiach point the same
                  'I'  weight each point by 1/(0.05 + dist)
iono_height    I  height of 2-D ionosphere layer, in m
obs_height     I  height of observer above mean Earth, in m

    
OUTPUTS:  mean_VTEC, target_STEC, index
mean_VTEC     O  the mean VTEC, in TECU
target_STEC   O  The STEC in the direction of the target, using the mean_VTEC
                 in TECU
index         O  The index for the target MJD, to be used to feed the next
                 call of this function.
"""
    if(El_target <= 0.0):
        # below horizon, return sane values
        return 0.0,0.0,last_index
    index = find_interpolation_point(MJD, MJD_target, last_index)
    L_bot = get_RINEX_points_at_index(index, Az, El, STEC, Az_target, El_target)
    mean_VTEC_bot, targ_STEC_bot = average_RINEX_SVTEC(L_bot,
                                                       El_target,
                                                       max_dist,
                                                       weight_scheme,
                                                       iono_height,
                                                       obs_height
                                                       )
    # if one RINEX point is within 0.5 s of target time, do not interpolate
    if(math.fabs(MJD[index] - MJD_target) < 0.5*DAYS_PER_SECOND):
        return mean_VTEC_bot, targ_STEC_bot, index
    L_top = get_RINEX_points_at_index(index+1, Az, El, STEC,
                                      Az_target, El_target)
    mean_VTEC_top, targ_STEC_top = average_RINEX_SVTEC(L_top,
                                                       El_target,
                                                       max_dist,
                                                       weight_scheme,
                                                       iono_height,
                                                       obs_height
                                                       )
    # if one RINEX point is within 0.5 s of target time, do not interpolate
    if(math.fabs(MJD[index+1] - MJD_target) < 0.5*DAYS_PER_SECOND):
        return mean_VTEC_top, targ_STEC_top, index
    # Interpolate VTEC
    slope = (mean_VTEC_top - mean_VTEC_bot) / (MJD[index+1] - MJD[index])
    mean_VTEC = mean_VTEC_bot + slope * (MJD_target - MJD[index])
    # convert to STEC
    target_STEC = mean_VTEC / get_VTEC_factor(El_target, iono_height, obs_height)
    #print ( "I got mean_VTEC,target_STEC, index", mean_VTEC, target_STEC, index)
    return mean_VTEC, target_STEC, index


















        
                                       
################################################################################
def get_station_RADEC_STECs(station_code,
                            year, month, day,
                            fraction_start,
                            fraction_end,
                            RA, Dec,
                            time_interval = 30.0,
                            data_directory = ".",
                            max_dist = 10.0,
                            weight_scheme = 'N',
                            iono_height = 300E3,
                            obs_height = 0.0,
                            main_file_overwrite = 1,
                            overwrite = 0
                            ):
    """get a list of STEC values for some RA and Dec for a GPS station

Given a GPS station code, this function calculates the STEC values for
a RA, Dec sky position for a sequence of times, from some starting time to
some ending time.  It will get RINEX files for the station for the requested
time interval period, process those to STEC values for the satellites, and
then convert that to STEC values for the target object.


station_code      I  4-character code for a station.  See RINEX information.
year              I  4 digit year of start of observations
month             I  month of start
day               I  day of start
fraction_start    I  day fraction of start of observations.
                     Nice to start on 30 second boundaries, but not necessary.
fraction_end      I  day fraction of end of observations
RA                I  Right Ascension of source, in radians
Dec               I  Declination of source, in radians
time_interval     I  The spacing between STEC calculations for the target source,
                     in seconds.  The RINEX data normally comes at 30 s
                     intervals, so there may not be much sense in having
                     time_interval less than this.
data_directory    I  Where should all of the data be written to?
max_dist          I  The maximum angular separation to allow, in radians
weight_scheme     I  Type of weighting to use.
                     'N'  weight eiach point the same
                     'I'  weight each point by 1/(0.05 + dist)
iono_height       I  height of 2-D ionosphere layer, in m
obs_height        I  height of observer above mean Earth, in m
main_file_overwrite I  May the main text file be overwritten? 0 No, else yes.
                     You probably want 1.
overwrite         I  May files be overwritten?  0 No, else yes.


OUTPUTS:  A
A                 O  Output python list of STEC values.  Each element of the
                     list contains a list of the form
                     [MJD_target, Az_target, El_target, STEC_target]
"""
    assert(fraction_end >= fraction_start)
    assert(time_interval > 0.0)
    # Get a list of times to calculate.  The AlbusIonosphere stuff
    # wants seconds
    MJD_base = jma_tools.get_MJD(year, month, day)
    seconds_start = fraction_start * SECONDS_PER_DAY
    seconds_end   = fraction_end   * SECONDS_PER_DAY
    # In the equation below, add a fudge time of 0.01 s to prevent rounding
    # errors.
    count = int((seconds_end - seconds_start + 0.01) / time_interval)
    seconds = []
    for i in range(count):
        s = seconds_start + i*time_interval
        seconds.append(s)
    seconds.append(seconds_end)
    # Now get the station RINEX info
    MJD, Az, El, STEC, XYZ = get_station_RINEX_stec(station_code,
                                                    year, month, day,
                                                    fraction_start, fraction_end,
                                                    data_directory,
                                                    main_file_overwrite,
                                                    overwrite
                                                    )
    # Perform a rough bias correction
    rough_satellite_bias_correction([STEC],[El])
    # Now to calculate the apparent Az,El using the AlbusIonosphere software
    retval = AlbusIonosphere.set_reference_time(year,month,day,0,0,0.0)
    if(retval < 0):
        raise Albus_RINEX.RINEX_Data_Barf("Error: AlbusIonosphere.set_reference_time gave %d"%retval)
    retval = AlbusIonosphere.set_station_position(XYZ[0],XYZ[1],XYZ[2])
    if(retval < 0):
        raise Albus_RINEX.RINEX_Data_Barf("Error: AlbusIonosphere.set_station_position gave %d"%retval)
    retval = AlbusIonosphere.set_source_position(RA,Dec)
    if(retval < 0):
        raise Albus_RINEX.RINEX_Data_Barf("Error: AlbusIonosphere.set_source_position gave %d"%retval)
    # Now, for each time in the list, calculate the target Az,El,
    # get the RINEX VTEC prediction, and store it
    A = []
    last_index = None
    for s in seconds:
        MJD_target = MJD_base + s * DAYS_PER_SECOND
        retval, El_target, Az_target = AlbusIonosphere.get_source_AzEl(s)
        if(retval < 0):
            raise Albus_RINEX.RINEX_Data_Barf("Error: AlbusIonosphere.get_source_AzEl gave %d"%retval)
        #print ( "Found source at Az,El", Az_target*M_RAD2DEG,El_target*M_RAD2DEG)
        if(El_target > 0.0):
            mean_VTEC, target_STEC, last_index = \
                get_average_RINEX_SVTEC_at_MJD(MJD_target,
                                               Az_target,    
                                               El_target,
                                               last_index,
                                               MJD, Az, El, STEC,
                                               max_dist,
                                               weight_scheme,
                                               iono_height,
                                               obs_height,
                                               )
            A.append([MJD_target, Az_target, El_target, target_STEC])
        else:
            A.append([MJD_target, Az_target, El_target,0.0])
    return A










################################################################################
def get_WSRT_RADEC_STECs(year, month, day, hour, minute, second,
                         year2,month2,day2,hour2,minute2,second2,
                         RA_str, Dec_str,
                         time_interval = 30.0,
                         data_directory = ".",
                         output_filename = "WSRT_out.txt",
                         max_dist_d = 120.0,
                         weight_scheme = 'I',
                         iono_height = 300E3,
                         obs_height = 71.21,
                         main_file_overwrite = 1,
                         overwrite = 0
                         ):
    """get a list of STEC values for some RA and Dec for a GPS station

Given a GPS station code, this function calculates the STEC values for
a RA, Dec sky position for a sequence of times, from some starting time to
some ending time.  It will get RINEX files for the station for the requested
time interval period, process those to STEC values for the satellites, and
then convert that to STEC values for the target object.

The output file text has the format
MJD Az  El  STEC
where Az and El are in degrees, and STEC is in TECU.


year              I  4 digit year of start of observations
month             I  month of start
day               I  day of start
hour              I  hour of start
minute            I  minute of start
second            I  second of start
year2             I  4 digit year of end of observations
month2            I  month of end
day2              I  day of end
hour2             I  hour of end
minute2           I  minute of end
second2           I  second of end
RA_str            I  Right Ascension of source, as a sexagesimal string
                     such as '02h34m15.667s' or '14 23 10.89'
Dec_str           I  Declination of source, as a sexagesimal string
                     such as '-02d34\'15.667\"' or '+14 23 10.89'
time_interval     I  The spacing between STEC calculations for the target source,
                     in seconds.  The RINEX data normally comes at 30 s
                     intervals, so there may not be much sense in having
                     time_interval less than this.
data_directory    I  Where should all of the data be written to?
output_filename   I  The full path+name of a file to write output data
                     to in text format.  If it exists, this function will append.
max_dist_d        I  The maximum angular separation to allow, in degrees
weight_scheme     I  Type of weighting to use.
                     'N'  weight eiach point the same
                     'I'  weight each point by 1/(0.05 + dist)
iono_height       I  height of 2-D ionosphere layer, in m
obs_height        I  height of observer above mean Earth, in m
main_file_overwrite I  May the main text file be overwritten? 0 No, else yes.
                     You probably want 1.
overwrite         I  May files be overwritten?  0 No, else yes.


OUTPUTS:  None
"""
    max_dist = max_dist_d * M_DEG2RAD
    MJD_base = jma_tools.get_MJD(year, month, day)
    MJD_start = jma_tools.get_MJD_hms(year, month, day, hour, minute, second)
    MJD_end   = jma_tools.get_MJD_hms(year2, month2, day2, hour2, minute2,second2)
    assert(MJD_end >= MJD_start)
    fraction_start = MJD_start - MJD_base
    fraction_end = MJD_end - MJD_base
    RA, Dec = Albus_Coordinates.radec_str_to_rad2(RA_str, Dec_str)
    A = get_station_RADEC_STECs("wsrt",
                                year, month, day,
                                fraction_start,
                                fraction_end,
                                RA, Dec,
                                time_interval,
                                data_directory,
                                max_dist,
                                weight_scheme,
                                iono_height,
                                obs_height,
                                main_file_overwrite,
                                overwrite
                                )
    # Now prit it to the file
    fp = open(output_filename, "a")
    for s in A:
        line = "%12.6f   %10.5f  %10.5f  %10.3f\n"%(s[0],s[1]*M_RAD2DEG,
                                                    s[2]*M_RAD2DEG,s[3])
        fp.write(line)
    fp.close()
    return







################################################################################
def download_BIGF_data_products(host_area,
                                list_filename,
                                output_directory = "."):
    """download a set of files from the BIGF server


The BIGF ftp server connection drops out frequently.  This function will
load files one at a time with pauses in between files to not task the network
too much.


INPUTS:
host_area        I  A string containing ftp://hostname.stuff.net/directory
list_filename    I  A filename containing the names of files to download from
                    the host directory area
output_directory I  The output directory to dump stuff

OUTPUTS: None
"""
    fp = open(list_filename, "r")
    try:
        for f in fp:
            systime.sleep(10)
            host_file = host_area + '/' + f.strip()
            output_file = output_directory + '/' + f.strip()
            get_url(host_file, output_file)
    finally:
        fp.close()
    return



