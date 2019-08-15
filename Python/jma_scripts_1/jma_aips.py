# jma_aips.py
# My standard AIPS utilities
# 2006 May 19  James M Anderson  --JIVE  start
# 2007 Jul 25  JMA  --many enhancements before now.  Today, allow
#                     self-calibration to just use point-source calib




################################################################################
# some import commands  The User should not need to change this.
################################################################################
from AIPS import AIPS, AIPSDisk
from AIPSTask import AIPSTask, AIPSList
from AIPSData import AIPSUVData, AIPSImage
from AIPSTV import AIPSTV
import Wizardry.AIPSData
import ptversion

import LocalProxy
from xmlrpclib import ServerProxy

import copy, optparse, os, sys, socket, tempfile
import re, string
import numarray.ma
import numarray.ieeespecial
import numarray.nd_image
import inspect
import warnings
import math








################################################################################
# Global variables
SECONDS_PER_DAY = 24.0*60.0*60.0
DAYS_PER_SECOND = 1.0/SECONDS_PER_DAY
M_DEG2RAD = math.pi/180.0
M_RAD2DEG = 180.0/math.pi


aips_version_try = '31DEC07'
aips_version_std = '31DEC06'
aips_version_old = '31DEC04'



################################################################################
# I want to have the possibility of using the TV
# Set this to None, but allow the user to set it to something else
tv_jma_aips = None

################################################################################
# set the tv holder
def set_jma_tv(new_tv):
    global tv_jma_aips
    if(tv_jma_aips is None):
        tv_jma_aips = new_tv
        try:
            tv_jma_aips._open()
        except socket.error, e:
            tv_jma_aips.start()



# Should the program pause after plots to make sure the user has seen them?
pause_after_plots = 0
# Should the program pause before overwriting datafiles?
pause_before_overwrite = 0
imagr_edge_avoidance = 10 # how many pixels from the edge to ignore
def set_pause_stuff(p,o):
    """Sets pause information
p  I  pause after plots
o  I  pause before overwriting files?
"""
    global pause_after_plots,pause_before_overwrite
    pause_after_plots = p
    pause_before_overwrite = o
    return

def get_pause_stuff():
    return pause_after_plots,pause_before_overwrite

Do_Image_PLots_In_Color = 1
def set_image_color_status(stat):
    Do_Image_PLots_In_Color = stat














################################################################################
# Define some useful functions
################################################################################




################################################################################
# 6&*!%#@^ Python doesn't have the equivalent of __LINE__
def current_line():
    return inspect.getouterframes(inspect.currentframe())[1][2]



################################################################################
# Wait for the user to say that it is ok to move on
def pause_prog(text):
    print '#' * 78
    junk = raw_input(text)
    return junk



################################################################################
def make_sure_directory_exists(directory):
    """check for the existence of a directory, and make it if not there
    """
    if(os.exists(directory)):
        return
    os.makedirs(directory)
    return




################################################################################
def get_ParselTonuge_version_tuple():
    """get the ParselTongue version number in a useful form

You can use this to compare with version numbers by using
version.__lt__(other_version)
version.__ge__(other_version)
version.__eq__(other_version)
and so on

OUTPUT: version
version   O  A tuple containing the ParselTongue version information
"""
    try:
        version = tuple(map(int,ptversion.version.lower().split('.')))
        return version
    except ValueError, e:
        sys.stderr.write("Error: ParselTongue version '%s' is not supported\nPlease upgrade to a distribution version of ParselTongue\n"%(ptversion.version))
        raise RuntimeError("Invalid ParselTongue version '%s'"%(ptversion.version))









################################################################################
# Find the antenna number
def get_antenna_number(ant_name,uvdata):
    ant = 0
    for antenna in uvdata.antennas:
        ant = ant + 1
        if antenna == ant_name:
            return ant
    raise RuntimeError, "antenna not found!"
    return None

################################################################################
def get_antenna_number_subarray(aips_data, ant_name, subarray=1):
    """Get the number of an antenna for a specific subarray


INPUTS:
aips_data   I  The AIPSUVData object to investigate
ant_name    I  The name of the antenna as a string
subarray    I  The subarray number

OUTPUTS: ant_num
ant_num     O  The number of the antenna

Note: raises a RuntimeError if the antenna is not found in the subarray
    """
    assert(check_table_version_exists(aips_data, 'AIPS AN', subarray))
    ant_name = ant_name.strip().upper()
    an_table = aips_data.table('AIPS AN', subarray)
    for row in an_table:
        this_ant = row.anname.strip().upper()
        if(this_ant == ant_name):
            return row.nosta
    raise RuntimeError("antenna '%s' not found in subarray %d"%(ant_name,subarray))


    

################################################################################
# Find the antenna dictionary
def get_antenna_dictionary(uvdata):
    ant = 0
    a_dict = {}
    for antenna in uvdata.antennas:
        ant = ant + 1
        a_dict[antenna] = ant
    return a_dict




################################################################################
# Find the antenna dictionary
def to_aips_array(stuff):
    """converts something to an AIPS array for task adverbs"""
    if(stuff is None): return [None]
    if(type(stuff) == type([5,2])):
        if(stuff[0] is None):
            return stuff
        else:
            newstuff = [None]
            newstuff[1:] = stuff
            return newstuff
    return [None,stuff]










################################################################################
# dump a dictionary to a file in text mode
def print_dict_to_file(d, middle_name, overwrite=1, copy_to_stdout=0,
                       experiment="dict",dirname='.'):
    """prints a dict to a file based on a middle_name"""
    # generate the full file name
    filename = dirname + '/' + experiment + '.' + middle_name + '.dict'
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
    try:
        fp = open(filename, "w")
        dk = d.keys()
        dk.sort()
        for k in dk:
            print >> fp, "%30s"%k, d[k]
            if(copy_to_stdout): print "%30s"%k, d[k]
    finally:
        fp.close()








################################################################################
# write out the last plot to disk
def lwpla_last_plot(data_file, middle_name, overwrite=1,
                    experiment="lwpla",dirname='.'):
    """plots out the last plot to disk"""
    # generate the full file name
    filename = dirname + '/' + experiment + '.' + middle_name + '.ps'
    if(os.path.isfile(filename)):
        if(overwrite):
            warnings.warn("File '%s' already exists.  Deleting."%filename)
            os.remove(filename)
        else:
            warnings.warn("File '%s' already exists.  AIPS will probably append to it."%filename)
    # now run lwpla
    lwpla = AIPSTask('lwpla')
    lwpla.indata = data_file
    lwpla.outfile = filename
    lwpla.dparm[6] = 4
    lwpla.plver = 0
    lwpla.inver = data_file.table_highver('AIPS PL')
    if(Do_Image_PLots_In_Color):
        lwpla.ofmfile = 'RAINBOW'
        lwpla.dparm[9] = 1
        lwpla.dodark = 1
        lwpla.docolor=1
        lwpla.plcolors = [None,                  # 0
                                                 # Bright Lines
                          [None, 1, 1, 0],       # 1 Border lines, tick marks, internal character labels
                          [None, 1, 1, 1],       # 2 Contours, model lines
                          [None, 1, 0.5, 1],     # 3 Polarization vectors
                          [None, 0, 1, 1],       # 4 Stars (incl labels), symbols
                                                 # Dark Lines
                          [None, 0,0,0],         # 5 internal character labels
                          [None, 0,0,0],         # 6 Contours, model lines
                          [None, 0,0,0],         # 7 Polarization vectors
                          [None, 0,0,0],         # 8 Stars (incl labels), symbols
                                                 # Other Stuff
                          [None, 0,0,0],         # 9 Character labels outside the plot area
                          [None, 0.8, 0.8, 1]]   # 10 Overall background
    lwpla()








################################################################################
def get_frequency_information(aips_data):
    """get frequency information from header"""
    ctype = aips_data.header['ctype']
    for i in xrange(len(ctype)):
        if(ctype[i] == "FREQ"):
            return aips_data.header['crval'][i], aips_data.header['cdelt'][i]
    else:
        raise KeyError("No FREQ entry in ctype for %s"%aips_data.__str__())
    return None














################################################################################
# a tool to help with snplt
def snplot(snuvdata, inext, invers, optype, opcode='', stokes_use='', sources_use='', timerange=[0,0,0,0], nplots=10,
           version = None):
    if(version is not None):
        snplt = AIPSTask('snplt', version = version)
    else:
        snplt = AIPSTask('snplt')
    snplt.indata = snuvdata
    snplt.inext = inext
    snplt.invers = invers
    if(type(sources_use) == type('string')):
        snplt.sources[1] = sources_use
    else:
        snplt.sources[1:] = sources_use
    if(timerange != None):
        if(timerange[0] == None): snplt.timerang = timerange
        else: snplt.timerang[1:] = timerange
    snplt.optype = optype
    snplt.opcode = opcode
    snplt.nplots = nplots
    #snplt.factor = 0.5
    snplt.cutoff = 1e-6
    snplt.do3col = 1
    snplt.dotv = 1
    if(snplt.dotv > 0):
        if(tv_jma_aips): tv_jma_aips.clear()
    snplt()
    if(pause_after_plots): pause_prog('Done with plotting.  Continue?')










################################################################################
# a tool to help with plotting (u,v) points
def run_uvplt(uvdata,
              sources_use=' ',
              stokes_use='',
              timerange=[0,0,0,0],
              antennas=None,
              baseline=None,
              uvrange=None,
              bchan=0,
              echan=0,
              bif=0,
              eif=0,
              docalib=0,
              gainuse=0,
              dopol=0,
              flagver=0,
              doband=0,
              bpver=0,
              xinc=1,
              bparm=[None,3,1],
              doweight=0,
              refant=0,
              rotate=0,
              do3col=1,
              dotv=1,
              version = None
              ):
    """UVPLT facility"""
    if(version is not None):
        uvplt = AIPSTask('uvplt', version = version)
    else:
        uvplt = AIPSTask('uvplt')
    uvplt.indata = uvdata
    if(type(sources_use) == type('string')):
        uvplt.sources[1] = sources_use
    else:
        uvplt.sources[1:] = sources_use
    if(timerange != None):
        if(timerange[0] == None): uvplt.timerang = timerange
        else: uvplt.timerang[1:] = timerange
    if(bparm != None):
        if(bparm[0] == None): uvplt.bparm = bparm
        else: uvplt.bparm[1:] = bparm
    uvplt.bchan=bchan
    uvplt.echan=echan
    uvplt.bif=bif
    uvplt.eif=eif
    uvplt.stokes=stokes_use
    if(antennas != None):
        if(type(antennas) == type(1)):
            uvplt.antennas[1] = antennas
        else:
            if(antennas[0] == None):
                uvplt.antennas = antennas
            else: uvplt.antennas[1:] = antennas
    if(baseline != None):
        if(type(baseline) == type(1)):
            uvplt.baseline[1] = baseline
        else:
            if(baseline[0] == None):
                uvplt.baseline = baseline
            else: uvplt.baseline[1:] = baseline
    if(uvrange != None):
        if(uvrange[0] == None): uvplt.uvrange = uvrange
        else: uvplt.uvrange[1:] = uvrange
    uvplt.docalib = docalib
    uvplt.gainuse = gainuse
    uvplt.dopol = dopol
    uvplt.do3col = do3col
    uvplt.flagver = flagver 
    uvplt.doband = doband
    uvplt.bpver = bpver
    uvplt.xinc = xinc
    uvplt.doweight = doweight
    uvplt.refant = refant
    uvplt.rotate = rotate
    uvplt.dotv = dotv
    if(uvplt.dotv > 0):
        if(tv_jma_aips): tv_jma_aips.clear()
    uvplt()
    if(pause_after_plots): pause_prog('Done with plotting.  Continue?')








################################################################################
def run_clipm(uvdata,
              sources='',
              calcode = '',
              timerange=None,
              antennas=None,
              baseline=None,
              uvrange=None,
              bchan=0,
              echan=0,
              bif=0,
              eif=0,
              docalib=0,
              gainuse=0,
              dopol=0,
              flagver=0,
              doband=0,
              bpver=0,
              aparm=None,
              version = None
              ):
    """CLIPM facility"""
    if(version is not None):
        clipm = AIPSTask('clipm', version = version)
    else:
        clipm = AIPSTask('clipm')
    clipm.indata = uvdata
    clipm.sources = to_aips_array(sources)
    clipm.calcode = calcode
    clipm.timerang = to_aips_array(timerange)
    clipm.aparm = to_aips_array(aparm)
    clipm.bchan=bchan
    clipm.echan=echan
    clipm.bif=bif
    clipm.eif=eif
    clipm.antennas = to_aips_array(antennas)
    clipm.baseline = to_aips_array(baseline)
    clipm.uvrange = to_aips_array(uvrange)
    clipm.docalib = docalib
    clipm.gainuse = gainuse
    clipm.dopol = dopol
    clipm.flagver = flagver 
    clipm.doband = doband
    clipm.bpver = bpver
    clipm()












    







################################################################################
# something to help run possm
def possm_1(uvdata, sources_use, stokes_use, gainuse, solint,
            bchan = None, echan = None, timerange = None,
            version = None):
    if(version is not None):
        possm = AIPSTask('possm', version = version)
    else:
        possm = AIPSTask('possm')
    possm.indata = uvdata
    possm.solint = solint / 60.0    # convert from s to min
    if len(uvdata.stokes) == 1:
        possm.stokes = uvdata.stokes[0]
    else:
        possm.stokes = stokes_use
    possm.freqid = 1
    if(gainuse >= 0):
        possm.docalib = 2
        possm.gainuse = gainuse
    if(type(sources_use) == type('string')):
        possm.sources[1] = sources_use
    else:
        possm.sources[1:] = sources_use
    if(bchan): possm.bchan = bchan
    if(echan): possm.echan = echan
    #possm.baseline[1:3] = [refant,0]
    if(timerange): possm.timerang = to_aips_array(timerange)
    possm.aparm[2] = 1
    possm.aparm[5] = -180
    possm.aparm[6] = 180
    #possm.aparm[7] = 1
    possm.aparm[8] = 1
    possm.aparm[9] = 1
    possm.codetype = 'A&P'
    possm.nplots = 9
    possm.dotv = 1
    if(possm.dotv > 0):
        if(tv_jma_aips): tv_jma_aips.clear()
    possm()
    if(pause_after_plots): pause_prog('Done with plotting.  Continue?')

################################################################################
def possm_2(uvdata, sources_use, stokes_use, gainuse, solint,
            refant_use, flagver=0, bpver = 0, bchan=None, echan=None,
            timerange = None,
            version = None):
    if(version is not None):
        possm = AIPSTask('possm', version = version)
    else:
        possm = AIPSTask('possm')
    possm.indata = uvdata
    possm.solint = solint / 60.0      # convert from s to min
    possm.flagver = flagver
    if len(uvdata.stokes) == 1:
        possm.stokes = uvdata.stokes[0]
    else:
        possm.stokes = stokes_use
    possm.freqid = 1
    if(gainuse >= 0):
        possm.docalib = 2
        possm.gainuse = gainuse
    if(type(sources_use) == type('string')):
        possm.sources[1] = sources_use
    else:
        possm.sources[1:] = sources_use
    if(bchan): possm.bchan = bchan
    if(echan): possm.echan = echan
    if(bpver > 0):
        possm.doband = 1
        possm.bpver = bpver
    possm.baseline[1] = refant_use
    if(timerange): possm.timerang = to_aips_array(timerange)
    possm.aparm[2] = 1
    possm.aparm[5] = -180
    possm.aparm[6] = 180
    #possm.aparm[7] = 1
    #possm.aparm[8] = 1
    possm.aparm[9] = 1
    possm.codetype = 'A&P'
    possm.nplots = 9
    possm.dotv = 1
    if(possm.dotv > 0):
        if(tv_jma_aips): tv_jma_aips.clear()
    possm()
    if(pause_after_plots): pause_prog('Done with plotting.  Continue?')

################################################################################
# run the EDITR task to flag stuff
def editr1(uvdata_use, sources_use='',gainuse=0,flagver=0,timerange=[0,0,0,0],
           version = None):
    # print out the telescopes
    antenna_dict = get_antenna_dictionary(uvdata_use)
    for antenna in uvdata_use.antennas:
        print antenna_dict[antenna], antenna
    # create and editr object
    if(version is not None):
        editr = AIPSTask('editr', version = version)
    else:
        editr = AIPSTask('editr')
    editr.indata = uvdata_use
    if(type(sources_use) == type('string')):
        editr.sources[1] = sources_use
        editr.reason = 'Manual EDITR ' + sources_use
    else:
        editr.sources[1:] = sources_use
        editr.reason = 'Manual EDITR many' + sources_use
    if(timerange != None):
        if(timerange[0] == None): editr.timerang = timerange
        else: editr.timerang[1:] = timerange
    editr.docalib = 2
    editr.gainuse = gainuse
    editr.flagver = flagver
    editr.stokes = ''
    editr.dohist = 1
    editr.crowded = 1
    editr.antuse = [None, 1]
    editr.antuse[2:] = range(1,len(uvdata_use.antennas)+1)
    #editr.inputs()
    editr()










################################################################################
# print out simple things of the UV data
def run_listr(uvdata,
              sources = None,
              calcode = None,
              inext = None,
              inver = None,
              timerange = None,
              stokes = '',
              freqid = None,
              bif = None,
              eif = None,
              bchan=None,
              echan=None,
              antennas = None,
              baseline = None,
              docalib = None,
              gainuse = None,
              uvrange = None,
              flagver = None,
              optype = 'SCAN',
              dparm = None,
              outprint = None,
              dirname = '.',
              version = None
              ):
    """show basic dataset information

uvdata   I  the UV dataset
rest ... I  See LISTR
"""
    if(version is not None):
        listr = AIPSTask('listr', version = version)
    else:
        listr = AIPSTask('listr')
    listr.indata = uvdata
    listr.sources = to_aips_array(sources)
    if(calcode): listr.calcode = calcode
    if(inext): listr.inext = inext
    if(inver): listr.inver = inver
    listr.timerang = to_aips_array(timerange)
    if(stokes): listr.stokes = stokes
    if(freqid): listr.freqid = freqid
    if(bif): listr.bif = bif
    if(eif): listr.eif = eif
    if(bchan): listr.bchan = bchan,
    if(echan): listr.echan = echan,
    listr.antennas = to_aips_array(antennas)
    listr.baseline = to_aips_array(baseline)
    if(docalib): listr.docalib = docalib
    if(gainuse): listr.gainuse = gainuse
    listr.uvrange = to_aips_array(uvrange)
    if(flagver): listr.flagver = flagver
    listr.optype = optype
    listr.dparm = to_aips_array(dparm)
    listr.docrt = -1
    if(outprint == None):
        listr.outprint = dirname + '/' + uvdata.name + ".listr"
    else:
        listr.outprint = outprint
    if os.path.isfile(listr.outprint):
        warnings.warn("File '%s' already exists.  Deleting."%listr.outprint)
        os.remove(listr.outprint)
    #listr.inp()
    listr()








################################################################################
# my kntr runner
def run_kntr(imgdata, rms, blc=[0,0], trc=[0,0],dotv=1,pixrange=None,
             ltype=4, version = None):
    if(version is not None):
        kntr = AIPSTask('kntr', version = version)
    else:
        kntr = AIPSTask('kntr')
    kntr.indata = imgdata
    kntr.docont = 1
    kntr.dovect = 0
    kntr.dogrey = 1
    kntr.blc[1:] = blc
    kntr.trc[1:] = trc
    kntr.ltype = ltype  # 4 for relative to ref pixel, 3 is standard
    kntr.clev = rms
    for i in range(1,17):
        kntr.levs[i+4] = 2**i
    for i in range(1,5):
        kntr.levs[5-i] = -(2**i)
    kntr.cbplot = 1
    if(pixrange):
        kntr.pixrange = to_aips_array(pixrange)
    if(Do_Image_PLots_In_Color):
        kntr.functype = 'LN'
        kntr.ofmfile = 'RAINBOW'
    kntr.darkline = 0
    kntr.dowedge = 3
    if(pixrange): kntr.dowedge = 1
    kntr.darkline = 0
    kntr.dotv = dotv
    if(kntr.dotv > 0):
        if(tv_jma_aips): tv_jma_aips.clear()
    kntr()



################################################################################
# my kntr runner which takes in a C position
def run_kntr2(imgdata, rms, C_center = None, width=None, dotv=1, pixrange=None,
              ltype=4):
    if(C_center == None):
        C_center = numarray.array([imgdata.header.naxis[1],imgdata.header.naxis[0]],type=numarray.Float32)
        C_center = C_Center * 0.5 - 1.0
    if(width == None):
        width = max(imgdata.header.naxis[1],imgdata.header.naxis[0])
    # get the C values
    blc = C_center - width/2
    trc = C_center + width/2
    if(blc[0] < 0): blc[0] = 0
    if(blc[1] < 0): blc[1] = 0
    if(trc[0] >= imgdata.header.naxis[0]): trc[0] = imgdata.header.naxis[0] -1
    if(trc[1] >= imgdata.header.naxis[1]): trc[1] = imgdata.header.naxis[1] -1
    # convert to FORTRAN
    blc = blc[-1:None:-1] + 1
    trc = trc[-1:None:-1] + 1
    # convert to simple lists
    blc = [blc[0],blc[1]]
    trc = [trc[0],trc[1]]
    return run_kntr(imgdata, rms, blc, trc, dotv, pixrange, ltype)




################################################################################
# get image statistics, assuming that the source is at the center
def get_imstats(imgdata, blc=[0,0], trc=[0,0],
                version = None):
    if(version is not None):
        imean = AIPSTask('imean', version = version)
    else:
        imean = AIPSTask('imean')
    imean.indata = imgdata
    imean.blc[1:] = blc
    imean.trc[1:] = trc
    imean()
    return (imean.pixavg, imean.pixstd)





################################################################################
# bounds check aips_image
def bounds_check_aips(aips_image, blc, trc):
    # This function assumes that blc is actually the bottom left corner
    # blc and trc are assumed to be FORTRAN lists
    bad_message = 'fully out of bounds'
    if(blc[1] < 1):
        if(trc[1] < 1):
            raise Exception, bad_message
        elif(trc[1] > aips_image.header['naxis'][0]):
            trc[1] = aips_image.header['naxis'][0]
        blc[1] = 1
    elif(blc[1] > aips_image.header['naxis'][0]):
        raise Exception, bad_message
    if(blc[2] < 1):
        if(trc[2] < 1):
            raise Exception, bad_message
        elif(trc[2] > aips_image.header['naxis'][1]):
            trc[2] = header['naxis'][1]
        blc[2] = 1
    elif(blc[2] > aips_image.header['naxis'][1]):
        raise Exception, bad_message


################################################################################
# run jmfit for a single component
def jmfit_1(imgdata, outfile, center=None, half_width = 20,
            version = None):
    """run JMFIT for a single Gaussian component

    center is a FORTRAN array/list
    """
    # JMFIT can only handle so many pixels
    if(half_width > 49):
        half_width = 49
    if(version is not None):
        jmfit = AIPSTask('jmfit', version = version)
    else:
        jmfit = AIPSTask('jmfit')
    jmfit.indata = imgdata
    if(center == None):
        center = [imgdata.header['naxis'][0]/2,imgdata.header['naxis'][1]/2]
    else:
        jmfit.gpos[1] = [None, center[0], center[1]]
        gwidth = [None, 1.5*imgdata.header.bmaj/imgdata.header.cdelt[0], 1.5*imgdata.header.bmin/imgdata.header.cdelt[0], imgdata.header.bpa]
        # Grumble, grumble, grumble, AIPS JMFIT won't accept things bigger
        # than 180 pixels in size
        if(math.fabs(gwidth[1]) > 180.0):
            gwidth[1] = 180.0
        if(math.fabs(gwidth[2]) > 180.0):
            gwidth[2] = 180.0
        jmfit.gwidth[1] = gwidth
    blc=[None,center[0]-half_width,center[1]-half_width]
    trc=[None,center[0]+half_width,center[1]+half_width]
    bounds_check_aips(imgdata,blc,trc)
    jmfit.blc = blc
    jmfit.trc = trc
    jmfit.ngauss = 1
    jmfit.niter = 2000
    jmfit.domax[1] = 1
    jmfit.dopos[1] = [None, 1, 1]
    jmfit.dowidth[1] = [None, 1, 1, 1]
    jmfit.docrt = -1
    jmfit.outprint = outfile
    jmfit.prtlev=1
    #jmfit.inp()
    jmfit()
    return (jmfit.fmax[1], jmfit.fpos[1][1:], jmfit.fwidth[1][1:])



################################################################################
# use IMAGR to make an image
def run_imagr(uvdata, cellsize, imsize, sources_use='', out_data=None,
              gainuse=0, niter=1000,timerange=[0,0,0,0],flagver=0,bpver=0,
              stokes='I',flux=0,dotv=1,clbox=None,robust_use=+8,
              rashift=None,decshift=None,do3dimag=1,cmethod='',uvrange=None,
              restoring_beam = None, minpatch=None, details=None,
              version = None):
    # First, check what is going on with out_data
    cl_letters = 'CL'
    if(niter ==0): cl_letters = 'IM'
    if(out_data == None):
        out_data = AIPSImage(uvdata.name, stokes[0]+cl_letters+'001',
                             uvdata.disk, 1)
    else:
        # Don't trust that the user got the class right
        out_data = AIPSImage(out_data.name, stokes[0]+cl_letters+'001',
                             out_data.disk, out_data.seq)
    # Next, check for details
    if(details is not None):
        if((restoring_beam is None) and ('restoring_beam' in details)):
            restoring_beam = details['restoring_beam']
        if((clbox is None) and ('clbox' in details)):
            clbox = details['clbox']
        if((rashift is None) and ('rashift' in details)):
            rashift = details['rashift']
        if((decshift is None) and ('decshift' in details)):
            decshift = details['decshift']
    # Next, make the image.  Then, deal with any residual crud
    # set a flag so that I can tell easily whether or not IMAGR barfs
    retval=0
    try:
        print 'trying to IMAGR', sources_use, 'with robust', robust_use
        print 'using dataset', uvdata
        if(version is not None):
            imagr = AIPSTask('imagr', version = version)
        else:
            imagr = AIPSTask('imagr')
        imagr.indata = uvdata
        imagr.outname = out_data.name
        imagr.outseq = out_data.seq
        imagr.outdisk = out_data.disk
        imagr.sources = to_aips_array(sources_use)
        imagr.timerang = to_aips_array(timerange)
        imagr.freqid = 1
        if(gainuse >= 1):
            imagr.docalib = 2
            imagr.gainuse = gainuse
        imagr.flagver=flagver
        if(bpver > 0):
            imagr.doband = 3
            imagr.bpver = bpver
        imagr.nchav = uvdata.header['naxis'][2]
        #imagr.chinc = 1
        imagr.stokes = stokes
        imagr.uvwtfn = 'U'
        imagr.robust = robust_use
        if((type(cellsize) == type(5)) or (type(cellsize) == type(5.0))):
            imagr.cellsize[1:] = [cellsize, cellsize]
        else:
            imagr.cellsize = to_aips_array(cellsize)
        if((type(imsize) == type(5)) or (type(imsize) == type(5.0))):
            imagr.imsize[1:] = [imsize, imsize]
        else:
            imagr.imsize = to_aips_array(imsize)
        imagr.do3dimag = do3dimag
        imagr.cmethod = cmethod
        imagr.nfield=1
        if(rashift != None):
            assert(decshift != None)
            rashift = to_aips_array(rashift)
            decshift = to_aips_array(decshift)
            assert(len(rashift) == len(decshift))
            imagr.nfield=len(rashift)-1
            imagr.rashift = rashift
            imagr.decshift = decshift
        imagr.uvrange = to_aips_array(uvrange)
        imagr.xtype = 5
        imagr.ytype = 5
        imagr.niter = niter
        imagr.gain = 0.03
        imagr.flux=flux
        if(clbox != None):
            if(clbox[0] != None):
                if(type(clbox[0]) != type([5,2])):
                    imagr.clbox[1] = to_aips_array(clbox)
                    imagr.nboxes = 1
                else:
                    for i in xrange(len(clbox)):
                        imagr.clbox[i+1] = to_aips_array(clbox[i])
                    imagr.nboxes = len(clbox)
            else:
                if(type(clbox[1]) != type([5,2])):
                    imagr.clbox[1] = to_aips_array(clbox)
                    imagr.nboxes = 1
                else:
                    # assume the caller got this correct
                    for i in xrange(len(clbox)):
                        imagr.clbox[i] = clbox[i]
                    imagr.nboxes = len(clbox)-1
        if(restoring_beam is not None):
            imagr.bmaj = restoring_beam[0]
            imagr.bmin = restoring_beam[1]
            imagr.bpa  = restoring_beam[2]
        imagr.overlap=1
        if(minpatch):
            imagr.minpatch = minpatch
        else:
            imagr.minpatch = min(max(imsize / 3,101),501)
        #imagr.imagrprm[8] = -0.1e-4
        imagr.dotv = dotv
        if(imagr.dotv > 0):
            if(tv_jma_aips): tv_jma_aips.clear()
        #imagr.inp()
        retval = imagr()
    finally:
        # clean up after ourselves
        for i in xrange(int(imagr.nfield+0.5)):
            klass = stokes[0]+"BM%3.3d"%(i+1)
            beam = AIPSImage(out_data.name, klass, out_data.disk, out_data.seq)
            if beam.exists(): beam.zap()
        # Did IMAGR barf?  If no, then retval will be None
        if(retval != None):
            # yes, a barf
            if(robust_use > -4):
                # try again, with a smaller robust level
                robust_use = robust_use - 2
                if(robust_use > 4):
                    robust_use = 4
                warnings.warn('IMAGR barfed on us, so clean up and try again with ROBUST =%.1f'% robust_use)
                if(out_data.exists()): out_data.zap()
                return run_imagr(uvdata, cellsize, imsize, sources_use, out_data,gainuse, niter,timerange,flagver,bpver,stokes,flux,dotv,clbox,robust_use,rashift,decshift,do3dimag,cmethod,uvrange,restoring_beam,minpatch,version)
            else:
                # robust level too low
                warnings.warn('IMAGR barfed on us, but I cannot make robust smaller than %.1f'% robust_use)
    # send stuff back to the user if we made an image
    print out_data.__str__()
    if out_data.exists(): return out_data
    return None




################################################################################
# print out a CC file
def print_cc(imgdata, middle_name,experiment=None,dirname='.',
             version = None):
    # print out the CC table
    if(experiment == None): experiment = imgdata.name
    outfile = dirname +'/'+ experiment +'.'+ middle_name + '.ccdat'
    if os.path.isfile(outfile):
        warnings.warn("File '%s' already exists.  Deleting."%outfile)
        os.remove(outfile)
    if(version is not None):
        prtcc = AIPSTask('prtcc', version = version)
    else:
        prtcc = AIPSTask('prtcc')
    prtcc.indata = imgdata
    prtcc.docrt = -1
    prtcc.outprint = outfile
    prtcc()





################################################################################
# grab a standard calibration source image (typically for the VLA)
def run_calrd(source = "3C286",
              band = 'X',
              disk = 0,
              version = None
              ):
    """reads in one of the standard AIPS calibration images

source     I  The name of the calibrator, e.g. 3C48, 3C286, etc.
band       I  The NAME of the obsreving band, such as L, C, X, U, K, Q
disk       I  The number of the disk to put it on
"""
    if(version is not None):
        calrd = AIPSTask('calrd', version = version)
    else:
        calrd = AIPSTask('calrd')
    calrd.object = source
    calrd.band = band
    calrd.outdisk = disk
    calrd()
    for i in xrange(99,0,-1):
        imgdata = AIPSImage(source + '_' + band, "MODEL", disk, i)
        if(imgdata.exists()):
            return imgdata
    raise AssertionError, "Cannot find calibrator " + source + " " + band + " " + disk.__str__()








################################################################################
# use CALIB to self-cal
def run_calib(uvdata, refant, solint, solsub, sources_use='',
              gainuse=0,timerange=[0,0,0,0],flagver=0,bpver=0,
              image_data=None, ncomp=None, flux=None, smodel=None,
              aparm=None, soltype='', solmode='P!A', snver=1, save_data=0,
              out_data=None, uvrange = None, cparm = None,
              minamper = None, minphser = None,
              version = None, antuse = None, wtuv = None):
    """Runs CALIB to perform Self-Calibration"""
    if(out_data == None):
        out_data = AIPSUVData(uvdata.name, 'CALIB', uvdata.disk, 1)
    if out_data.exists():
        out_data.zap()
    try:
        print 'trying to CALIB', sources_use, 'in dataset', uvdata
        if(version is not None):
            calib = AIPSTask('calib', version = version)
        else:
            calib = AIPSTask('calib')
        calib.indata = uvdata
        calib.outdata = out_data
        calib.calsour = to_aips_array(sources_use)
        calib.timerang = to_aips_array(timerange)
        calib.freqid = 1
        calib.antuse = to_aips_array(antuse)
        calib.uvrange = to_aips_array(uvrange)
        if(wtuv is not None):
            calib.wtuv = wtuv
        if(gainuse >= 1):
            calib.docalib = 2
            calib.gainuse = gainuse
        calib.flagver=flagver
        if(bpver > 0):
            calib.doband = 3
            calib.bpver = bpver
        calib.refant = refant
        calib.solint= solint / 60.0        # convert from s to min
        calib.solsub = solsub
        calib.solmin = 1
        calib.soltype = soltype
        calib.solmode = solmode
        if(aparm): calib.aparm = to_aips_array(aparm)
        if(minphser is not None):
            calib.minphser = minphser
        if(minamper is not None):
            calib.minamper = minamper
        calib.cparm = to_aips_array(cparm)
        if(cparm is None):
            if(minphser is not None):
                calib.cparm[4] = minphser
            if(minamper is not None):
                calib.cparm[3] = minamper
        if (type(image_data) != type(None)):
            # We have an image, hopefully with clean components, to use
            calib.in2data = image_data
            if (ncomp != None):
                ncomp = to_aips_array(ncomp)
                calib.ncomp = ncomp
                if(len(ncomp) > 2):
                    calib.nmaps = len(ncomp)-1
            if(flux != None):
                calib.flux = flux
        else:
            # no image, so smodel needs to be checked
            if(smodel != None):
                if((type(smodel) == type(1)) or (type(smodel) == type(1.0))):
                    calib.smodel[1] = smodel
                else:
                    calib.smodel[1:] = smodel
        calib.snver = snver
        #calib.inp()
        #pause_prog("calib");
        calib()

    except:
        # clean up after ourselves
        if out_data.exists():
            out_data.zap()
        raise
    if(save_data):
        if out_data.exists():
            return out_data
    return None












################################################################################
# use FRING to self-cal
def run_fring(uvdata,
              refant,
              search_list,
              solint,
              solsub,
              sources_use='',
              gainuse=0,
              timerange=[0,0,0,0],
              flagver=0,
              bpver=0,
              image_data=None,
              ncomp=None,
              flux=None,
              smodel=None,
              aparm=None,
              dparm=None,
              snver=1,
              version = None):
    """Runs FRING to perform Self-Calibration on a multi-source dataset"""
    print 'trying to FRING', sources_use, 'in dataset', uvdata
    if(version is not None):
        fring = AIPSTask('fring', version = version)
    else:
        fring = AIPSTask('fring')
    fring.indata = uvdata
    if(type(sources_use) == type('string')):
        fring.calsour[1] = sources_use
    else: fring.calsour[1:] = sources_use
    if(timerange != None):
        if(timerange[0] == None): fring.timerang = timerange
        else: fring.timerang[1:] = timerange
    fring.freqid = 1
    if(gainuse >= 1):
        fring.docalib = 2
        fring.gainuse = gainuse
    fring.flagver=flagver
    if(bpver > 0):
        fring.doband = 3
        fring.bpver = bpver
    fring.refant = refant
    if(search_list != None):
        if(search_list[0] == None): fring.search=search_list
        else: fring.search[1:]=search_list
    fring.solint= solint / 60.0        # convert from s to min
    fring.solsub = solsub
    if(aparm != None):
        if(aparm[0] == None): fring.aparm = aparm
        else: fring.aparm[1:] = aparm
    if(dparm != None):
        if(dparm[0] == None): fring.dparm = dparm
        else: fring.dparm[1:] = dparm
    if (type(image_data) != type(None)):
        # We have an image, hopefully with clean components, to use
        fring.in2data = image_data
        if (ncomp != None):
            if(type(ncomp) == type(1)):
                fring.ncomp[1] = ncomp
            else:
                fring.ncomp[1:] = ncomp
                fring.nmaps = len(ncomp)
        if(flux != None):
            fring.flux = flux
    else:
        # no image, so smodel needs to be checked
        if(smodel != None):
            if((type(smodel) == type(1)) or (type(smodel) == type(1.0))):
                fring.smodel[1] = smodel
            else:
                fring.smodel[1:] = smodel
    fring.snver = snver
    #fring.inp()
    fring()









################################################################################
def run_sad(aips_image,
            cparm,
            gain,
            dparm,
            blc = [0,0],
            trc = [0,0],
            MF_version = 0,
            doresid = 0,
            Num_Comp = 5000,
            bw_smearing = 0,
            sort_order ='S',
            version = None
            ):
    """run the AIPS search and destroy program to find sources
INPUTS:
aips_image       I  AIPSImage dataset
cparm            I  a list of source levels to search down through.  See
                    AIPS help for details.  This should be a list of brightnesses
                    decreasing in order, ending a few times the RMS level,
                    followed by a 0.  No default, you need to give something.
gain             I  cutoff level gain.  See AIPS
dparm            I  rejection values.
                    1  min peak
                    2  min flux
                    3  min RMS level
                    4  max width in pixels
                    
blc              I  list giving the bottom left corner, FORTRAN notation
trc              I  list giving the top right corner, FORTRAN notation
MF_version       I  MF table version number to use
doresid          I  Create a residual image?
Num_Comp         I  The maximum number of components to potentially search
                    for.  Maximum is 40 000 in 31DEC07 AIPS
bw_smearing      I  rough bandwidth smearing coefficient.  This should be the
                    channel bandwidth divided by the center frequency.
sort_order       I  which way to sort.  'R' is Right Ascension, 'D for Dec,
                    or 'X' or 'Y' for pixels, or 'S' for flux
version          I  AIPS version to use

    """
    if(version is not None):
        sad = AIPSTask('sad', version = version)
    else:
        sad = AIPSTask('sad')
    sad.indata = aips_image
    sad.invers = MF_version
    sad.doresid = doresid
    sad.blc = to_aips_array(blc)
    sad.trc = to_aips_array(trc)
    sad.ngauss = Num_Comp
    sad.cparm = to_aips_array(cparm)
    sad.gain = gain
    sad.dparm = to_aips_array(dparm)
    sad.bwsmear = bw_smearing
    sad.sort = sort_order
    sad.outver = -1
    sad.dowidth = [None,[None,0.5,0.5,0.5],[None,0.5,0.5,0.5],[None,0.5,0.5,0.5],[None,0.5,0.5,0.5]]
    sad.docrt = -2
    fd, temp_path = tempfile.mkstemp(text=True)
    try:
        os.close(fd)
        sad.outprint = temp_path
        sad()
        # Now copy the output to the terminal and the log
        fp = open(temp_path, 'r')
        try:
            data = fp.read()
            sys.stdout.write(data)
            try:
                AIPS.log.write(data)
            except:
                pass
        finally:
            fp.close()
    finally:
        os.unlink(temp_path)
    return








################################################################################
# run snsmo in the basic modes
def run_snsmo(aips_data,
              refant=None,
              sources='',
              timerange=[0,0,0,0],
              samptype='BOX',
              smotype='VLBI',
              bparm=[None,1,1,60,1,1],
              cparm=[None,0,0,0,0,0],
              doblank=0,
              dobtween=-1,
              sn_invers=0,
              sn_outvers=0,
              version = None):
    """run SNSMO in the basic modes to smooth SN data

    aips_data    I  A standard UV dataset
    refant       I  The reference antenna
    sources      I  Sources to use, blank default
    timerange    I  The timerange to use
    samptype     I  Which type of smothing to use
    smotype      I  What type of smothing to do.  See AIPS for current
                    usage.  Right now, the current version supports
                    'AMPL' = amplitude smoothing only,
                    'PHAS' = phase smoothing only,
                    'BOTH' = amplitude and phase,
                    'DELA' = delay smoothing only,
                    'VLBI' = Coherent phase, rate and delay smoothing.
                             Phases smoothed in each IF separately.
                    'VLRI' = Coherent phase, rate and delay smoothing.
                             Phases smoothed in each IF separately, but
                             rates are averaged over IF as well as
                             polarization.
                    'VLMB' = Like VLBI and VLRI but phases are averaged
                             over IF before smoothing. The average phase is
                             the phase of vector average of the complex
                             amplitudes. The average amplitude is the
                             scalar average of the amplitudes.
                    'FULL' = same as VLBI
    bparm        I  Parameters controlling smothing.  Note that the values are
                    expected in seconds!!!
    cparm        I  Parameters controlling smothing.  Note that the values are
                    expected in seconds!!!
    doblank      I  controls bad value blanking
    dobtween     I  more AIPS stuff
    sn_invers    I  Incoming SN table version
    sn_outvers   I  Outgoing SN table version
    """
    # Smooth the SN table
    if(version is not None):
        snsmo = AIPSTask('snsmo', version = version)
    else:
        snsmo = AIPSTask('snsmo')
    snsmo.indata = aips_data
    if(type(sources) == type('string')):
        snsmo.sources[1] = sources
    else: snsmo.sources[1:] = sources
    if(timerange != None):
        if(timerange[0] == None): snsmo.timerang = timerange
        else: snsmo.timerang[1:] = timerange
    snsmo.refant=refant
    snsmo.samptype = samptype
    snsmo.smotype = smotype
    if(bparm != None):
        if(bparm[0] == None): snsmo.bparm = bparm
        else: snsmo.bparm[1:] = bparm
        for i in xrange(1,len(snsmo.bparm)):
            snsmo.bparm[i] = snsmo.bparm[i] / 3600.0  # convert to hours
    if(cparm != None):
        if(cparm[0] == None): snsmo.cparm = cparm
        else: snsmo.cparm[1:] = cparm
        for i in xrange(1,len(snsmo.cparm)):
            snsmo.cparm[i] = snsmo.cparm[i] / 3600.0  # convert to hours
    snsmo.doblank = doblank
    snsmo.dobtween = dobtween
    snsmo.invers= sn_invers
    snsmo.outvers = sn_outvers
    snsmo()









################################################################################
# run CLCAL in the basic modes
def run_clcal(uvdata,
              gainver_in=0,
              gainver_out=0,
              refant=None,
              sources='',
              calsources='',
              timerange=[0,0,0,0],
              opcode='CALI',
              interpol='',
              samptype='',
              bparm=None,
              doblank=0,
              dobtween=0,
              smotype='',
              snver_first=0,
              snver_last=0,
              antennas = None,
              version = None):
    """Run CLCAL for basic things to do"""
    if(version is not None):
        clcal = AIPSTask('clcal', version = version)
    else:
        clcal = AIPSTask('clcal')
    clcal.indata = uvdata
    clcal.sources = to_aips_array(sources)
    clcal.calsour = to_aips_array(calsources)
    clcal.timerang = to_aips_array(timerange)
    clcal.antennas = to_aips_array(antennas)
    clcal.refant=refant
    clcal.opcode = opcode
    clcal.interpol = interpol
    clcal.samptype = samptype
    if(bparm != None):
        if(bparm[0] == None): clcal.bparm = bparm
        else: clcal.bparm[1:] = bparm
    clcal.doblank = doblank
    clcal.dobtween = dobtween
    clcal.snver = snver_first
    clcal.invers= snver_last
    clcal.gainver = gainver_in
    clcal.gainuse = gainver_out
    clcal()










################################################################################
def run_clcor(uvdata,
              sources = None,
              stokes = None,
              timerange = None,
              bif = None,
              eif = None,
              antennas = None,
              subarray = 0,
              gainver = None,
              gainuse = None,
              opcode = None,
              clcorprm = None,
              infile = None,
              version = None):
    """Run CLCOR for basic things to do"""
    if(version is not None):
        clcor = AIPSTask('clcor', version = version)
    else:
        clcor = AIPSTask('clcor')
    clcor.indata = uvdata
    clcor.sources = to_aips_array(sources)
    if(stokes): clcor.stokes = stokes
    clcor.timerang = to_aips_array(timerange)
    if(bif): clcor.bif = bif
    if(eif): clcor.eif = eif
    clcor.antennas = to_aips_array(antennas)
    clcor.subarray = subarray
    if(gainver): clcor.gainver = gainver
    if(gainuse): clcor.gainuse = gainuse
    if(opcode): clcor.opcode = opcode
    clcor.clcorprm = to_aips_array(clcorprm)
    if(infile): clcor.infile = infile
    clcor()





    
              
              









################################################################################
# use split to slit out the data
def split_data(multi_uvdata, thisclass, thissequence, sources_use='', gainuse = 0, bpver=0, flagver=0,
               version=None):
    """Split out sources (or all sources) from a multisource file

    multi_uvdata      I  the multi-source UV data file

    thisclass         I  name of the class to write out split files to

    sources_use       I  a string, or a list of strings of the source names to
                         split

    gainuse           I  CL table to use (0 for highest)

    bpver             I  bandpass correction?

    flagver           I  which flag table to use
    

    returns a list of all of the split out files
    """
    # if we are doing all sources, then check that
    if(sources_use == ''):
        sources_use = multi_uvdata.sources
    if(type(sources_use) == type('string')):
        sources_use = [sources_use]
    # declare a list to hold all of the split files
    split_file_list = []
    # Now, check is we need to zap any of these files
    for source in sources_use:
        #print "looking for file '" +source.upper() +"'"+thisclass+"'", multi_uvdata.disk, thissequence
        s_data = AIPSUVData(source.upper(), thisclass, multi_uvdata.disk, thissequence)
        if s_data.exists():
            warnings.warn('Hey, I found an old split file \"%s\"'%
                          s_data.__str__())
            warnings.warn( 'Zapping ....')
            s_data.zap()
        else:
            #print 'No such file found'
            pass
        split_file_list.append(s_data)
    # now do the splitting    
    if(version is not None):
        split = AIPSTask('split', version = version)
    else:
        split = AIPSTask('split')
    split.indata = multi_uvdata
    split.sources[1:] = sources_use
    split.docalib = 2
    split.gainuse = gainuse
    split.flagver=flagver
    if(bpver > 0):
        split.doband = 3
        split.bpver = bpver
    split.outclass = thisclass
    split.outseq = thissequence
    split.outdisk = multi_uvdata.disk
    split.aparm[1] = 2
    split.aparm[4] = 1
    split()

    # check that all of the files were split properly
    for s_data in split_file_list:
        if not s_data.exists():
            msg = "Error: cannot find uvdata for split file \"%s\"" \
                  % (s_data)
            raise RuntimeError, msg
    return split_file_list



################################################################################
def run_split(uvdata_in,
              uvdata_out,
              sources = None,
              timerang = None,
              bif = None,
              eif = None,
              bchan = None,
              echan = None,
              docalib = None,
              gainuse = None,
              dopol = None,
              blver = None,
              flagver = None,
              doband = None,
              bpver = None,
              smooth = None,
              aparm = None,
              ichansel = None,
              nchav = None,
              chinc = None,
              overwrite = 0,
              version = None
              ):
    """Split out sources (or all sources) from a UV file

Inputs are basically direct from the AIPS task.

Overwrite = 0 says files may not be overwritten.  Else, zap old ones.

Returns list of files generated.

    """
    # if we are doing all sources, then check that
    if((sources == None) or (sources == '')):
        sources = uvdata_in.sources
    if(type(sources) == type('string')):
        sources = [sources]
    # declare a list to hold all of the split files
    split_file_list = []
    # Now, check is we need to zap any of these files
    for s in sources:
        if(s == None): continue
        s_data = AIPSUVData(s[0:12].upper(), uvdata_out.klass,
                            uvdata_out.disk, uvdata_out.seq)
        if s_data.exists():
            warnings.warn('Hey, I found an old split file \"%s\"'%
                          s_data.__str__())
            warnings.warn( 'Zapping ....')
            if(overwrite):
                s_data.zap()
            else:
                raise RuntimeError, "Cannot zap old file"
        else:
            #print 'No such file found'
            pass
        split_file_list.append(s_data)
    # now do the splitting    
    if(version is not None):
        split = AIPSTask('split', version = version)
    else:
        split = AIPSTask('split')
    split.indata = uvdata_in
    split.outclass = uvdata_out.klass
    split.outdisk = uvdata_out.disk
    split.outseq = uvdata_out.seq

    split.sources = to_aips_array(sources)
    if(timerang): split.timerang= to_aips_array(timerang)
    if(bif): split.bif = bif           
    if(eif): split.eif = eif           
    if(bchan): split.bchan = bchan
    if(echan): split.echan = echan         
    if(docalib):
        split.docalib = docalib       
        if(gainuse): split.gainuse = gainuse       
    if(dopol): split.dopol = dopol         
    if(blver): split.blver = blver         
    if(flagver): split.flagver = flagver       
    if(doband):
        split.doband  = doband        
        if(bpver): split.bpver = bpver         
    if(smooth): split.smooth = to_aips_array(smooth)   
    if(aparm): split.aparm = to_aips_array(aparm)
    else:
        split.aparm[6] = 1
    if(ichansel): split.ichansel= to_aips_array(ichansel)
    if(nchav): split.nchav = nchav       
    if(chinc): split.chinc = chinc         
    split()
    # check that all of the files were split properly
    for s_data in split_file_list:
        if not s_data.exists():
            msg = "Error: cannot find uvdata for split file \"%s\"" \
                  % (s_data)
            raise RuntimeError, msg
    return split_file_list









################################################################################
def run_splat(uvdata_in,
              uvdata_out,
              sources = None,
              timerang = None,
              bif = None,
              eif = None,
              bchan = None,
              echan = None,
              docalib = None,
              gainuse = None,
              dopol = None,
              blver = None,
              flagver = None,
              doband = None,
              bpver = None,
              smooth = None,
              aparm = None,
              ichansel = None,
              channel = None,
              chinc = None,
              solint = None,
              version = None
              ):
    """Splat out sources (or all sources) from a UV file

Inputs are basically direct from the AIPS task, except that
solint should be expressed in seconds, and will be converted to
AIPS units in this function.

    """
    if(version is not None):
        splat = AIPSTask('splat', version = version)
    else:
        splat = AIPSTask('splat')
    splat.indata = uvdata_in
    splat.outdata = uvdata_out

    if(sources): splat.sources = to_aips_array(sources)
    if(timerang): splat.timerang= to_aips_array(timerang)
    if(bif): splat.bif = bif           
    if(eif): splat.eif = eif           
    if(bchan): splat.bchan = bchan         
    if(echan): splat.echan = echan         
    if(docalib):
        splat.docalib = docalib       
        if(gainuse): splat.gainuse = gainuse       
    if(dopol): splat.dopol = dopol         
    if(blver): splat.blver = blver         
    if(flagver): splat.flagver = flagver       
    if(doband):
        splat.doband  = doband        
        if(bpver): splat.bpver = bpver         
    if(smooth): splat.smooth = to_aips_array(smooth)   
    if(aparm): splat.aparm = to_aips_array(aparm)
    if(ichansel): splat.ichansel= to_aips_array(ichansel)
    if(channel): splat.channel = channel       
    if(chinc): splat.chinc = chinc         
    if(solint): splat.solint = solint / 60.0
    splat()



































################################################################################
# bounds check
def bounds_check(data_array, blc, trc):
    # This function assumes that blc is actually the bottom left corner
    bad_message = 'fully out of bounds'
    if(blc[0] < 0):
        if(trc[0] < 0):
            raise Exception, bad_message
        elif(trc[0] >= data_array.size(0)):
            trc[0] = data_array.size(0)-1
        blc[0] = 0
    elif(blc[0] >= data_array.size(0)):
        raise Exception, bad_message
    if(blc[1] < 0):
        if(trc[1] < 0):
            raise Exception, bad_message
        elif(trc[1] >= data_array.size(1)):
            trc[1] = data_array.size(1)-1
        blc[1] = 0
    elif(blc[1] >= data_array.size(1)):
        raise Exception, bad_message





################################################################################
# Some stuff for masking areas

def mask_image_rectangle(data_array, blc, trc, inside=1, mask=1,\
                         C_indices=1):
    """This function works with a 2D numarray.ma data_array to moodify the mask.
    It works on a rectangular area from blc=[x,y] to trc=[x,y].

    By default, it uses C style (0-based) indices.  If you want
    1-based indices, set C_indices to 0.  Note that this also affects the
    ordering of the indices.  AIPS normally wants you to specify blc as
    blc=[x,y], but ParselTongue and other Python-based astronomical software
    has adopted C-based array notation, which would more naturally be
    blc=[y,x] for the way the data is stored.

    By default, this function will generate the mask to
    operate on the inside (edge inclusive) of the rectangle described by
    blc and trc.  If you want to mask the outside (edge EXclusive), then
    set inside=0.

    By default, this function will set the mask to be masked whereever the mask
    already existed, plus where the new rectangle mask indicates.  If, on the
    other hand, you wish to UNMASK regions which may have been previously masked,
    then set mask=0.  For instance, suppose that you have data_array which
    has size (512,512) and has an existing mask which is masking off the entire
    array.  You want to unmask the central region so that you can work with
    the pixels at the center.  Then call
    mask_image_rectangle(data_array, [256-10,256-10], [256+10,256+10], inside=1,\
                         mask=0)
    """
    blc = copy.copy(blc)
    trc = copy.copy(trc)
    # If FORTRAN indices, then fix
    if not C_indices:
        blc = [blc[1]-1,blc[0]-1]
        trc = [trc[1]-1,trc[0]-1]
    # If TRC is actually BLC, swap
    if(blc[0] > trc[0]):
        blc[0], trc[0] = trc[0], blc[0]
    if(blc[1] > trc[1]):
        blc[1], trc[1] = trc[1], blc[1]
    # bounds checking
    try:
        bounds_check(data_array, blc, trc)
    except:
        # if there was an exception raised, then the box is totally
        # off the image, and there is nothing to do
        return data_array
    # now make the mask
    m = numarray.ma.make_mask_none(data_array.shape)
    for i in range(blc[0],trc[0]+1):
        jarray = numarray.arrayrange(0,data_array.size(1))
        jarray = numarray.where(numarray.logical_and((jarray >= blc[1]),\
                                                     (jarray <= trc[1])), 1, 0)
        m[i,:] = jarray
    if (not inside)^(not mask):
        m = numarray.logical_not(m)
    if not mask:
        m = m * numarray.ma.getmaskarray(data_array)
    else:
        m = numarray.ma.mask_or(m, numarray.ma.getmask(data_array))
    return numarray.ma.array(data_array, mask=m, fill_value=data_array.fill_value())


################################################################################
def mask_image_circle(data_array, radius, center, inside=1, mask=1,\
                      C_indices=1):
    """This function works with a 2D numarray.ma data_array to moodify the mask.
    It works on a circular area centered at center with radius radius.

    By default, it uses C style (0-based) indices.  If you want
    1-based indices, set C_indices to 0.  Note that this also affects the
    ordering of the indices.  AIPS normally wants you to specify corner as
    center=[x,y], but ParselTongue and other Python-based astronomical software
    has adopted C-based array notation, which would more naturally be
    center=[y,x] for the way the data is stored.

    By default, this function will generate the mask to
    operate on the inside (edge inclusive) of the circle described by
    radius and center.  If you want to mask the outside (edge EXclusive), then
    set inside=0.

    By default, this function will set the mask to be masked whereever the mask
    already existed, plus where the new rectangle mask indicates.  If, on the
    other hand, you wish to UNMASK regions which may have been previously masked,
    then set mask=0.  For instance, suppose that you have data_array which
    has size (512,512) and has an existing mask which is masking off the entire
    array.  You want to unmask the central region so that you can work with
    the pixels at the center.  Then call
    mask_image_circle(data_array, 10, [256,256], inside=1,\
                      mask=0)
    """
    center = copy.copy(center)
    # If FORTRAN indices, then fix
    if not C_indices:
        center = [center[1]-1,center[0]-1]
    # now make the mask
    m = numarray.zeros(data_array.shape, type=numarray.Float64)
    x = numarray.arrayrange(data_array.size(1), type=numarray.Float64) - center[1]
    y = numarray.arrayrange(data_array.size(0), type=numarray.Float64) - center[0]
    x = x*x
    y = y*y
    radius2 = radius*radius
    for i in xrange(0,data_array.size(1)):
        m[:,i] += y
    for i in xrange(0,data_array.size(0)):
        m[i,:] += x
    if (not inside)^(not mask):
        m = numarray.where(m <= radius2, 0, 1)
    else:
        m = numarray.where(m <= radius2, 1, 0)
    if not mask:
        m = m * numarray.ma.getmaskarray(data_array)
    else:
        m = numarray.ma.mask_or(m, numarray.ma.getmask(data_array))
    return numarray.ma.array(data_array, mask=m, fill_value=data_array.fill_value())


    

################################################################################
def masked_get_min_max(data_array):
    """This function takes in a masked array.  It then searches for the
    minimum and maximum pixel values, and records their positions.  When
    finished, it returns the min and max, and their positions.
    """
    data = numarray.ma.filled(data_array, 0)
    mask = (numarray.ma.getmask(data_array) == 0)
    (min, max, min_pos, max_pos) = numarray.nd_image.extrema(data,mask)
    min_pos = numarray.array(min_pos)
    max_pos = numarray.array(max_pos)
    return (min, max, min_pos, max_pos)



################################################################################
def get_small_source_stats(data_array, source_radius, noise_inner_radius, \
                           noise_outer_radius,
                           edge_width=imagr_edge_avoidance, warning_level=1,
                           pointing_edge_width = None):
    """Get statistis from an image for a \"small\" source.

    This function will inspect an image to locate a single, strong,
    mostly circular-ish source.  The source will be found from the
    strongest pixel in the image, excluding edge pixels.  This function
    will the conpute the integrated source brightness within source_radius
    pixels of the max pixel, and the RMS noise level for pixels at least
    noise_radius pixels away from max, again, excluding edge pixels.

    data_array     I  the incoming image.  Note that data_array is expected
                      to be a MaskedArray from NumArray, and may
                      already have some pixels masked.

    source_radius  I  the radius from the center of the source to sum pixels

    noise_inner_radius I  inner radius of an annulus to calculate the noise level

    noise_outer_radius I  outer radius of an annulus to calculate the noise level

    edge_width     I  width in pixels to avoid at the edge of the image

    warning_level  I  how much should the user be warned.
                      0  None
                      1  Some
    pointing_edge_width I  The width in pixels to avoid at the edge of the
                     image because of source pointing uncertainty.  This
                     routine will only search for a peak position within this
                     region.  This assumes, for instance, that you could have
                     an uncertainty of x arcseconds (pixels) from the center
                     of the image because of uncertainty in the optical
                     position, or low resolution position, and so on.

    It returns information as (max, max_pos, sum, rms), where
    max is the maximum pixel value
    max_pos is the position list (array) [y,x] of the maximum pixel position
    sum is the integrated brightness of the source (may be affected by
        previous masking of pixels)
    rms is the RMS value off-source for pixels in radius range
        noise_inner_radius to noise_outer_radius
    """
    # First, make a version with the edge pixels masked off
    if((pointing_edge_width is None)
       or (pointing_edge_width < edge_width)):
        pointing_edge_width = edge_width
    blc=[pointing_edge_width-1,pointing_edge_width-1]
    trc=[data_array.size(0)-pointing_edge_width,data_array.size(1)-pointing_edge_width]
    base_array = mask_image_rectangle(data_array, blc, trc, inside=0, mask=1)
    # Now find the min, max stuff
    (min, max, min_pos, max_pos) = masked_get_min_max(base_array)
    # Check for how far this is from center
    if(warning_level > 0):
        dimensions = numarray.array(base_array.shape, type=numarray.Float64)
        blc = dimensions * 0.25
        trc = dimensions * 0.75
        if(numarray.any(max_pos[-2:] < blc[-2:]) or numarray.any(max_pos[-2:] > trc[-2:])):
            warnings.warn("'%s': Max position out of center of image"\
                          %get_small_source_stats.func_name)
    # Now flag everything except the source region
    blc=[edge_width-1,edge_width-1]
    trc=[data_array.size(0)-edge_width,data_array.size(1)-edge_width]
    base_array = mask_image_rectangle(data_array, blc, trc, inside=0, mask=1)
    source_array = mask_image_circle(base_array, source_radius, max_pos,\
                                     inside=0, mask=1)
    # Get the integrated value.
    integ_value = numarray.ma.sum(numarray.ma.ravel(source_array))
    del source_array
    # Now flag the source and get the image rms
    rms_array = mask_image_circle(base_array, noise_inner_radius, max_pos,\
                                  inside=1, mask=1)
    rms_array = mask_image_circle(rms_array, noise_outer_radius, max_pos,\
                                  inside=0, mask=1)
    rms = numarray.nd_image.standard_deviation(numarray.ma.filled(rms_array, 0),
                                               (numarray.ma.getmask(rms_array) == 0))
    #
    return (max, max_pos, integ_value, rms)

    

    
################################################################################
def get_small_source_stats_main(aips_image, \
                                source_radius, noise_inner_radius, \
                                noise_outer_radius,
                                edge_width=imagr_edge_avoidance, \
                                print_level=1,
                                pointing_edge_width = None):
    """User version for statistis from an image for a \"small\" source.

    This function takes an AIPSImage, and returns physical (Jy/beam and Jy)
    values.  Note that the returned position is a C position!!!

    This function will inspect an image to locate a single, strong,
    mostly circular-ish source.  The source will be found from the
    strongest pixel in the image, excluding edge pixels.  This function
    will the conpute the integrated source brightness within source_radius
    pixels of the max pixel, and the RMS noise level for pixels at least
    noise_radius pixels away from max, again, excluding edge pixels.

    aips_image     I  the incoming image as an AIPSImage

    source_radius  I  the radius from the center of the source to sum pixels

    noise_inner_radius I  inner radius of an annulus to calculate the noise level

    noise_outer_radius I  outer radius of an annulus to calculate the noise level

    edge_width     I  width in pixels to avoid at the edge of the image

    print_level    I  how much should the user be informed.
                      0  None
                      1  Some
    pointing_edge_width I  The width in pixels to avoid at the edge of the
                     image because of source pointing uncertainty.  This
                     routine will only search for a peak position within this
                     region.  This assumes, for instance, that you could have
                     an uncertainty of x arcseconds (pixels) from the center
                     of the image because of uncertainty in the optical
                     position, or low resolution position, and so on.

    It returns information as (max, max_pos, sum, rms), where
    max is the maximum pixel value, in specific intensity (Jy/beam)
    max_pos is the position list (array) [y,x] of the maximum pixel position
    sum is the integrated brightness of the source (may be affected by
        previous masking of pixels).  Note that this function returns the
        sum in flux density (Jy) units, 
    rms is the RMS value off-source for pixels in radius range
        noise_inner_radius to noise_outer_radius (Jy/beam)
    """
    # Get a Wizardry image to work on the pixels
    w = Wizardry.AIPSData.AIPSImage(aips_image.name, aips_image.klass, \
                                    aips_image.disk, aips_image.seq)
    # make a masked array
    if(__debug__):
        if(len(w.pixels.getshape()) != 7):
            warnings.warn("Hey, someone changed the AIPS Images")
            raise RuntimeError("AIPS Image does not have 7 dimensions")
    x = numarray.ma.array(w.pixels[0,0,0,0,0,:,:].copy(), \
                          mask=numarray.ma.make_mask_none((aips_image.header.naxis[1],aips_image.header.naxis[0])))
    (image_max, image_max_pos, image_sum, image_rms) = \
                get_small_source_stats(x, source_radius,
                                       noise_inner_radius, noise_outer_radius,
                                       edge_width, print_level,
                                       pointing_edge_width)
    # Now correct the sum for the beam size
    factor = aips_image.header.bmaj * aips_image.header.bmin / \
             math.fabs(aips_image.header.cdelt[1] * aips_image.header.cdelt[0])
    image_sum = image_sum / factor
    if(print_level > 0):
        print 'Got information from image of:'
        print 'Max:', image_max
        print 'Pos:', image_max_pos
        print 'Sum:', image_sum
        print 'RMS:', image_rms
    return (image_max, image_max_pos, image_sum, image_rms) 
    









################################################################################
def get_jmfit_statistics(aips_image,   
                         max_position, 
                         outfile,
                         remove_outfile=1,
                         half_width = 20
                         ):
    """Run JMFIT and get the statistics in a dictionary

    aips_image,   # an AIPSImage object
    max_position, # C notation, as an array (numarray)
    outfile       I filename to have JMFIT write out to
    remove_outfile I Should the outfile be removed?
    half_width    I half-width of JMFIT box
    """
    # if the file exists, delete it
    if os.path.isfile(outfile):
        os.remove(outfile)
    # form a FORTRAN position for AIPS
    image_max_pos_FORTRAN = max_position[-1:None:-1] + 1
    # do the actual JMFIT call
    jmfit_1(aips_image, outfile, center=image_max_pos_FORTRAN,
            half_width=half_width)
    # JMFIT finds lots of stuff, but doesn't return it.  It is only available
    # in the output file.  So get it as a dictionary
    jm_dict = {}
    jm_dict['Beam_Maj'] = aips_image.header['bmaj'] * 3600 # to arcseconds
    jm_dict['Beam_Min'] = aips_image.header['bmin'] * 3600 # to arcseconds
    jm_dict['Beam_PA'] = aips_image.header['bpa']
    # Calculate the beam area, for a Gaussian beam shape (1.133 correction)
    # in units of square arcseconds
    jm_dict['Beam_Area'] = jm_dict['Beam_Maj']*jm_dict['Beam_Min']*1.133
    jm_dict['Frequency'] = get_frequency_information(aips_image)[0]
    try:
        # open the file and read everything in
        fp = open(outfile, "r")
        jm_data = fp.read()
        fp.close()
        # Ok, should the file be removed?
        if(remove_outfile):
            os.remove(outfile)
        # Ok, where is the start of the main results section?
        main_index = jm_data.index("Solution from JMFIT")
        # Ok, get the peak stuff
        this_index = jm_data.index("Peak intensity", main_index)
        words = jm_data[this_index + len('Peak intensity    ='):].split(None,6)
        jm_dict['Peak_Int'] = float(words[0])
        jm_dict['Peak_Int_Err'] = float(words[2])
        # Ok, get the Integrated stuff
        this_index = jm_data.index("Integral intensity", main_index)
        words = jm_data[this_index + len('Integral intensity='):].split(None,5)
        jm_dict['Int_Int'] = float(words[0])
        jm_dict['Int_Int_Err'] = float(words[2])
        # Ok, get the RA position
        this_index = jm_data.index("RA", main_index)
        words = jm_data[this_index:].split(None,6)
        jm_dict['RA'] = float(words[3]) + (float(words[2]) \
                                           + (float(words[1])) * 60.0)*60.0
        jm_dict['RA_Err'] = float(words[5])
        jm_dict['RA_Str'] = words[1] + ':' + words[2] + ':' + words[3]
        # Ok, get the Dec position.  Be carefule of DECember
        this_index = jm_data.index(" DEC", main_index)
        words = jm_data[this_index:].split(None,6)
        jm_dict['DEC'] = float(words[3]) + (float(words[2]) \
                                           + (math.fabs(float(words[1]))) * 60.0)*60.0
        if(words[1][0] == '-'):
            jm_dict['DEC'] = -jm_dict['DEC']
        jm_dict['DEC_Err'] = float(words[5])
        jm_dict['DEC_Str'] = words[1] + ':' + words[2] + ':' + words[3]
        # Ok, get the X position
        this_index = jm_data.index("X-position", main_index)
        words = jm_data[this_index:].split(None,5)
        jm_dict['X_pos'] = float(words[2])
        jm_dict['X_pos_Err'] = float(words[4])
        # Ok, get the Y position
        this_index = jm_data.index("Y-position", main_index)
        words = jm_data[this_index:].split(None,5)
        jm_dict['Y_pos'] = float(words[2])
        jm_dict['Y_pos_Err'] = float(words[4])
        # Ok, get the deconvolved Major Axis
        main_index = jm_data.index("Deconvolution of component in asec",main_index)
        this_index = jm_data.index("Major ax", main_index)
        words = jm_data[this_index:].split(None,5)
        jm_dict['Comp_Maj_Ax'] = [float(words[2]), float(words[3]), float(words[4])]
        # Ok, get the deconvolved Minor Axis
        this_index = jm_data.index("Minor ax", main_index)
        words = jm_data[this_index:].split(None,5)
        jm_dict['Comp_Min_Ax'] = [float(words[2]), float(words[3]), float(words[4])]
        # Ok, get the deconvolved Position Angle
        this_index = jm_data.index("Pos ang", main_index)
        words = jm_data[this_index:].split(None,5)
        jm_dict['Comp_Pos_Ang'] = [float(words[2]), float(words[3]), float(words[4])]
    except:
        sys.stdout.write("Error while trying to read JMFIT results\n")
    return jm_dict






################################################################################
def get_sad_information(aips_image,
                        aips_data,
                        RMS,
                        min_sigma,
                        blc = None,
                        trc = None,
                        source_size = 20,
                        print_level = 1
                        ):
    """get source information from an image using SAD
INPUTS:
aips_image       I  AIPSImage dataset.  The image to use
aips_data        I  The UV dataset used to make the image (used to get
                    frequency information)
RMS              I  the noise level in the image
min_sigma        I  What is the minimum sigma level to use when searching
                    for sources.  (Typically 5-sigma)
blc              I  list giving the bottom left corner, FORTRAN notation
                    If None, then use the whole image, except for the edges
trc              I  list giving the top right corner, FORTRAN notation
                    If None, then use the whole image, except for the edges
source_size      I  The expected radii of sources, in pixels
print_level      I  How much information to print
                    0 Nothing
                    1 Some information on position and flux

OUTPUTS: statistics
statistics       O  a list of statistics dictionaries containing source
                    information as from get_jmfit_statistics
"""
    statistics = []
    # Ok, run SAD
    im_freq, im_Delta = get_frequency_information(aips_image)
    da_freq, da_Delta = get_frequency_information(aips_data)
    bandwidth_smearing = da_Delta / im_freq
    min_flux = RMS*min_sigma
    cparm = [None,1000*min_flux,300*min_flux,100*min_flux,30*min_flux,10*min_flux,3*min_flux,min_flux,0]
    dparm = [None,min_flux,min_flux,min_flux,source_size*2,0]
    if(blc is None):
        blc = [imagr_edge_avoidance,imagr_edge_avoidance]
    if(trc is None):
        trc = [aips_image.header.naxis[0]-imagr_edge_avoidance,aips_image.header.naxis[1]-imagr_edge_avoidance]
    run_sad(aips_image, cparm, 1.0, dparm, blc, trc,
            bw_smearing=bandwidth_smearing)
    # Now check out what happened
    # Get a Wizardry image to work on the pixels
    w = Wizardry.AIPSData.AIPSImage(aips_image)
    # make a masked array
    if(__debug__):
        if(len(w.pixels.getshape()) != 7):
            warnings.warn("Hey, someone changed the AIPS Images")
            raise RuntimeError("AIPS Image does not have 7 dimensions")
    width = 100
    # Run through the model table
    MF_table = aips_image.table('AIPS MF', aips_image.table_highver('AIPS MF'))
    for row in MF_table:
        x_center = row.center_x -1.0  # C
        y_center = row.center_y -1.0  # C
        center = numarray.array([int(y_center+0.5),int(x_center+0.5)])
        x_left = int(x_center - width + 0.5)
        x_right = int(x_center + width + 0.5)
        if(x_left < 0):
            x_left = 0
        if(x_right >= aips_image.header.naxis[0]):
            x_right = aips_image.header.naxis[0]-1
        y_bot = int(y_center - width + 0.5)
        y_top = int(y_center + width + 0.5)
        if(y_bot < 0):
            y_bot = 0
        if(y_top >= aips_image.header.naxis[1]):
            y_top = aips_image.header.naxis[1]-1
        x = numarray.ma.array(w.pixels[0,0,0,0,0,y_bot:y_top,x_left:x_right].copy(), \
                              mask=numarray.ma.make_mask_none((y_top-y_bot,
                                                               x_right-x_left)))
        (image_max, image_max_pos, image_sum, image_rms) = \
                get_small_source_stats(x, source_size,
                                       source_size + 20,
                                       source_size + 180,
                                       imagr_edge_avoidance)
        s = get_jmfit_statistics(aips_image, center, "junk.jmfit",
                                 half_width=max(4,min(int(source_size*0.5),12)))
        # Add on the plain image statistics from above
        s['Pixel_Max'] = image_max
        s['Pixel_Sum'] = image_sum
        s['Pixel_RMS'] = image_rms
        s['Pixel_Pos'] = image_max_pos
        s['SAD_Pixel_Center_X'] = x_center
        s['SAD_Pixel_Center_Y'] = y_center
        statistics.append(s)
    if(print_level >= 1):
        print "Found %d sources at"%(len(statistics))
        for s in statistics:
            print "%15s %15s %10.1E %10.1E"%(s['RA_Str'],s['DEC_Str'],
                                             s['Peak_Int'], s['Int_Int'])
    return statistics


















################################################################################
# get some (u,v) statistics
def run_uvprm(uvdata,
              sources=None,
              calcode=' ',
              freqid=0,
              timerange=None,
              antennas=None,
              baseline=None,
              uvrange=None,
              stokes=' ',
              docalib=0,
              gainuse=0,
              dopol=0,
              flagver=0,
              doband=0,
              version = None
              ):
    """get (u,v) statistics"""
    if(version is not None):
        uvprm = AIPSTask('uvprm', version = version)
    else:
        uvprm = AIPSTask('uvprm')
    uvprm.indata = uvdata
    uvprm.flagver = flagver
    if(sources != None):
        if(type(sources) == type('string')):
            uvprm.sources[1] = sources
        else: uvprm.sources[1:] = sources
    uvprm.calcode=calcode
    uvprm.timerang = to_aips_array(timerange)
    uvprm.uvrange = to_aips_array(uvrange)
    uvprm.freqid = freqid
    uvprm.docalib=docalib
    uvprm.gainuse=gainuse
    if(stokes is not None):
        uvprm.stokes=stokes
    uvprm.dopol=dopol
    uvprm.doband=doband
    if(antennas != None):
        if(type(antennas) == type(1)):
            uvprm.antennas[1] = antennas
        else:
            if(antennas[0] == None):
                uvprm.antennas = antennas
            else: uvprm.antennas[1:] = antennas
    if(baseline != None):
        if(type(baseline) == type(1)):
            uvprm.baseline[1] = baseline
        else:
            if(baseline[0] == None):
                uvprm.baseline = baseline
            else: uvprm.baseline[1:] = baseline
    uvprm()
    





################################################################################
def get_cellsize_info(uv_data,
                      stokes = None,
                      timerange = None,
                      uvrange = None):
    """get the proper cellsize for the observations somehow"""
    #obsfreq = uv_data.header['crval'][2]
    #obslambda = 2.99792458E8/obsfreq
    #baseline = 7000E3               # meters => 7000 km
    #cellsize = obslambda/baseline   # radians
    uvmax = 0.0
    try:
        uvmax = uv_data.keywords['UVPRAMAX']
    except:
        run_uvprm(uv_data, stokes=stokes,
                  timerange=timerange, uvrange=uvrange)
        uvmax = uv_data.keywords['UVPRAMAX']
    cellsize = 1.0/uvmax            # radians
    cellsize = cellsize*206264.806  # arcsec
    cellsize = cellsize/4.          # I like 4 cells per resolution element
    return cellsize


################################################################################
def get_observation_duration_info(uv_data,
                                  stokes = None,
                                  timerange = None,
                                  uvrange = None):
    """get the amount of time actually on source, in seconds"""
    obs_time = 0.0
    try:
        obs_time = uv_data.keywords['UVPOTIME']
    except:
        run_uvprm(uv_data, stokes=stokes,
                  timerange=timerange, uvrange=uvrange)
        obs_time = uv_data.keywords['UVPOTIME']
    obs_time *= SECONDS_PER_DAY
    return obs_time
    














################################################################################
def check_table_version_valid(aips_data, table, version):
    """Check that version is a valid table, as 'AIPS CL', 'AIPS BP', etc."""
    # Check for ANY version of the table.  0 implies 0 or more
    if(version == 0): return 1
    # make sure there is at least ONE table
    #if(aips_data.table_highver(table) == 0):
    #    raise AttributeError, "No table"+table
    for t in aips_data.tables:
        if((t[0] == version) and (t[1] == table)): break
    else: return 0
    return 1

################################################################################
def check_table_version_exists(aips_data, table, version):
    """Check whether a table version exists, as 'AIPS CL', 'AIPS BP', etc.

    if version == 0, then check for ANY of that table, 
    """
    # If version is zero, then accept any of that table
    if(version == 0):
        for t in aips_data.tables:
            if(t[1] == table): break
        else: return 0
        return 1
    # Otherwise, check for the version number too
    for t in aips_data.tables:
        if((t[0] == version) and (t[1] == table)): break
    else: return 0
    return 1


################################################################################
def verify_table_version_free(aips_data, table, version, overwrite):
    """check that a table version is vacant, and possibly delete if ok"""
    # check for an existing table version
    if(check_table_version_exists(aips_data, table, version)):
        # Are we allowed to delete it?
        if(overwrite):
            aips_data.zap_table(table, version)
        else:
            raise RuntimeError, "Table %s %d exists in dataset '%s', but cannot delete"%(table, version, aips_data.__str__())







################################################################################
# Find the highest numbered antenna
def find_antenna_highnumber(aips_data):
    """Find the highest numbered antenna in the dataset
    """
    highnum = 0
    for an in xrange(1,aips_data.table_highver('AIPS AN')+1):
        # check that this exists
        if not(check_table_version_exists(aips_data, 'AIPS AN', an)): continue
        # Ok, grab the AN table
        an_table = aips_data.table('AIPS AN', an)
        for row in an_table:
            if(highnum < row.nosta): highnum = row.nosta
    return highnum










################################################################################
# Count visibilities
def count_visibilities(aips_data_orig):
    """Count the number of visibilities per antenna in a dataset

    aips_data_orig  I  a non-Wizardry object to use
    """
    aips_data = Wizardry.AIPSData.AIPSUVData(aips_data_orig.name,
                                             aips_data_orig.klass, 
                                             aips_data_orig.disk,
                                             aips_data_orig.seq)
    # What is the highest valued antenna number?
    high_ant = find_antenna_highnumber(aips_data_orig)
    #print high_ant
    antennas = numarray.zeros((high_ant+1))
    for row in aips_data:
        baseline = row.baseline
        a1 = baseline[0]
        a2 = baseline[1]
        if(a1 != a2):
            antennas[a1] = antennas[a1] +1
            antennas[a2] = antennas[a2] +1
    return antennas



################################################################################
# Add the standard antenna number of visibilities to a dictionary
def add_visibility_count_to_dict(uvdata, d, vis):
    """ Add the standard antenna number of visibilities to a dictionary


uvdata I  the original uvdata set.
d    B  the dictionary to add stuff to
vis  I  the number of visibilities, as a list or array

This function uses antenna_array and antenna_dict to get antenna info
"""
    antenna_array = [None]
    antenna_array[1:] = uvdata.antennas
    max = len(vis)
    if(max > len(antenna_array)): max = len(antenna_array)
    for i in xrange(max):
        if(antenna_array[i] == None): continue
        d['Ant_Vis_Count_'+antenna_array[i]] = vis[i]
    return











################################################################################
def get_MJD(year, month, day):
    """get the Modified Julian Date (JD - 2400000.5)

Note that this really only works for Gregorian dates.

This assumes that you give it a correct year, month, day.  It is
your own fault if you are out of range.

Taken from _Astronomical Algorithms_, Meeus, 1991
    """
    mm = month
    yy = year
    if(mm<= 2):
        mm += 12
        yy -= 1
    a = int(yy/100)
    b = 2 - a + int(a/4)
    MJD = int(365.25*(yy+4716)) + int(30.6001*(mm+1)) + day + b - 1524.5
    MJD -= 2400000.5
    return MJD

################################################################################
def get_MJD_from_JD(JD):
    MJD = JD - 2400000.5
    return MJD

################################################################################
def get_JD_from_MJD(MJD):
    JD = MJD + 2400000.5
    return JD



################################################################################
def get_MJD_hms(year, month, day, hour = 0, minute = 0, second = 0):
    """get the Modified Julian Date (JD - 2400000.5)
    """
    day_frac = ((hour) * 60.0 + minute) * 60.0 + second
    day_frac *= DAYS_PER_SECOND
    MJD = day_frac + get_MJD(year, month, day)
    return MJD
    
################################################################################
def get_JD_hms(year, month, day, hour = 0, minute = 0, second = 0):
    """get the Julian Date
    """
    MJD = get_MJD_hms(year, month, day, hour, minute, second)
    JD = MJD + 2400000.5
    return JD

################################################################################
def get_MJD_frac(year, month, day, day_fraction = 0):
    """get the Modified Julian Date (JD - 2400000.5)
    """
    MJD = day_fraction + get_MJD_hms(year, month, day)
    return MJD
    
################################################################################
def get_JD_frac(year, month, day, day_fraction = 0):
    """get the Julian Date
    """
    MJD = get_MJD_frac(year, month, day, day_fraction)
    JD = MJD + 2400000.5
    return JD


################################################################################
def get_ymdf_from_JD(JD):
    """get the year, month, day, day_fraction from an MJD

Taken from _Astronomical Algorithms_, Meeus, 1991
    """
    JD2 = JD + 0.5
    Z = int(JD2)
    F = JD2 - Z
    A = Z
    if(Z>= 2299161):
        alpha = int((Z-1867216.25)/36524.25)
        A = Z + 1 + alpha - int(alpha/4)
    B = A + 1524
    C = int((B-122.1)/365.25)
    D = int(365.25*C)
    E = int((B-D)/30.6001)
    day_total = B - D - int(30.6001*E) + F
    month = E - 1
    if(E >= 14): month = E - 13
    year = C - 4716
    if(month <= 2): year += 1
    day = int(day_total)
    day_fraction = day_total - day
    return year, month, day, day_fraction


################################################################################
def get_hms_from_frac(day_fraction):
    """get hours, minues, seconds from a fractional day.

Does not worry about leap seconds.
"""
    h = day_fraction * 24.0
    hour = int(h+2E-13)
    m = (h - hour) * 60.0
    minute = int(m+1E-11)
    second = (m - minute) * 60.0
    return hour, minute, second


################################################################################
def get_ymdh_from_JD(JD):
    """get hours, minues, seconds from a fractional day.
"""
    year, month, day, day_fraction = get_ymdf_from_JD(JD)
    hour, minute, second = get_hms_from_frac(day_fraction)
    return year, month, day, hour, minute, second




################################################################################
# Get the day of year from the Year, month, day
def get_day_of_year(year, month, day):
    """Get the day-of-year integer from the year/month/day

    year   I  YYYY
    month  I  MM starting from January == 1
    day    I  DD starting from 1 == 1
    """
    day_of_year_list = [0,31,59,90,120,151,181,212,243,273,304,334]
    doy = day_of_year_list[month-1] + day
    if(month>2):
        if((year&0x3)==0):
            if((year % 100 != 0) or (year % 400 == 0)):
                doy = doy+1
    return doy

################################################################################
# Get the day of year from the Year, month, day for the start of observations
def get_observation_year_month_day(aips_data):
    date_string = aips_data.header.date_obs
    date_list = date_string.split('-')
    year = int(date_list[0])
    month = int(date_list[1])
    day = int(date_list[2])
    return (year, month, day)

################################################################################
# Get the day of year from the Year, month, day for the start of observations
def get_observation_day_of_year(aips_data):
    (year, month, day) = get_observation_year_month_day(aips_data)
    return get_day_of_year(year, month, day)


################################################################################
# Get the number of days of observations
def get_number_days_observations(aips_data):
    """Find out how long the observations are.  Assumes the data has been sorted

    """
    nx_table = aips_data.table('AIPS NX', 0)
    num_rows = len(nx_table)
    # The NX table is FORTRANish
    # Check the first observation
    first_row = nx_table[0]
    start_time = first_row.time - 0.5 * first_row.time_interval
    if(start_time < 0.0):
        raise RuntimeError, "Starting observation before starting date: Run FXTIM!"
    # Now check the last scan
    last_row = nx_table[num_rows-1]
    end_time = last_row.time + 0.5 * last_row.time_interval
    return int(math.ceil(end_time))









################################################################################
# Generic flagging capability
def run_uvflg(uvdata,
              flagver=0,
              infile=None,
              sources=None,
              freqid=0,
              timerange=[0,0,0,0],
              bchan=0,
              echan=0,
              bif=0,
              eif=0,
              antennas=None,
              baseline=None,
              stokes=' ',
              aparm=None,
              opcode='FLAG',
              reason='USER FLAG',
              dohist=0,
              version = None
              ):
    """Generic flagging tool"""
    if(version is not None):
        uvflg = AIPSTask('uvflg', version = version)
    else:
        uvflg = AIPSTask('uvflg')
    uvflg.indata = uvdata
    uvflg.flagver = flagver
    if(infile): uvflg.infile = infile
    if(sources != None):
        if(type(sources) == type('string')):
            uvflg.sources[1] = sources
        else: uvflg.sources[1:] = sources
    uvflg.timerang = to_aips_array(timerange)
    uvflg.freqid = freqid
    if(aparm != None):
        if(aparm[0] == None): uvflg.aparm = aparm
        else: uvflg.aparm[1:] = aparm
    uvflg.bchan=bchan
    uvflg.echan=echan
    uvflg.bif=bif
    uvflg.eif=eif
    uvflg.stokes=stokes
    uvflg.opcode=opcode
    uvflg.reason=reason
    uvflg.dohist=dohist
    uvflg.antennas = to_aips_array(antennas)
    uvflg.baseline = to_aips_array(baseline)
    uvflg()




















################################################################################
def make_selfcal_calib_data_in(main_data_file, source_name,
                               calib_class, sequence_in):
    """Create an AIPSData variable with the appropriate name, class, seq"""
    if((sequence_in == None) or (sequence_in <= 0)):
        # use the main data file
        calib_in = main_data_file
    else:
        if((source_name == None) or (source_name == '')):
            source_name = main_data_file.name
        if((calib_class == None) or (calib_class == '')):
            calib_class = 'CALIB'
        calib_in = AIPSUVData(source_name, calib_class,
                              main_data_file.disk, sequence_in)
    assert(calib_in.exists())
    # Clean up.  Remove all SN files.
    if(calib_in.table_highver('AIPS SN') > 0):
        calib_in.zap_table('AIPS SN', -1)
    #assert(check_table_version_valid(calib_in, 'AIPS CL', gainuse_start))
    #assert(check_table_version_valid(calib_in, 'AIPS BP', bpver))
    return calib_in


################################################################################
def make_selfcal_calib_data_in_nocheck(main_data_file, source_name,
                                       calib_class, sequence_in):
    """Create an AIPSData variable with the appropriate name, class, seq"""
    if((sequence_in == None) or (sequence_in <= 0)):
        # use the main data file
        calib_in = main_data_file
    else:
        if((source_name == None) or (source_name == '')):
            source_name = main_data_file.name
        if((calib_class == None) or (calib_class == '')):
            calib_class = 'CALIB'
        calib_in = AIPSUVData(source_name, calib_class,
                              main_data_file.disk, sequence_in)
    return calib_in


################################################################################
def make_selfcal_calib_data_out(calib_data_in, source_name,
                                calib_class, sequence_out):
    """Create an AIPSData variable with the appropriate name, class, seq"""
    if((source_name == None) or (source_name == '')):
        source_name = calib_data_in.name
    if((calib_class == None) or (calib_class == '')):
        calib_class = 'CALIB'
    if((sequence_out == None) or (sequence_out <= 0)):
        sequence_out = 1
    calib_out = AIPSUVData(source_name, calib_class,
                           calib_data_in.disk, sequence_out)
    if(calib_out.exists()):
        if __debug__:
            #warnings.warn("'%s': CALIB file \"%s\" already exists. Hit Enter to delete."\
            #              %(make_selfcal_calib_data_out.func_name,calib_out))
            #pause_prog('Continue?')
            pass
        calib_out.zap()
    return calib_out


################################################################################
def make_selfcal_imagr_outfile(calib_data_in, source_name, sequence_out,
                               stokes = 'I'):
    """Create an AIPSImage variable with the appropriate name, class, seq"""
    if((source_name == None) or (source_name == '')):
        source_name = calib_data_in.name
    if((sequence_out == None) or (sequence_out <= 0)):
        sequence_out = 1
    # Make the Beam image.
    image_class = stokes[0] + 'BM001'
    image_data = AIPSImage(source_name, image_class,
                           calib_data_in.disk, sequence_out)
    # If this exists, delete it, as we may be using a different imsize
    if(image_data.exists()):
        if __debug__:
            warnings.warn("'%s': Beam file \"%s\" already exists. Hit Enter to delete."\
                          %(make_selfcal_imagr_outfile.func_name,image_data))
            if(pause_before_overwrite): pause_prog('Continue?')
        image_data.zap()
    # Now make the image image
    image_class = stokes[0] + 'CL001'
    image_data = AIPSImage(source_name, image_class,
                           calib_data_in.disk, sequence_out)
    # If this exists, delete it, as we may be using a different imsize
    if(image_data.exists()):
        if __debug__:
            warnings.warn("'%s': Image file \"%s\" already exists. Hit Enter to delete."\
                          %(make_selfcal_imagr_outfile.func_name,image_data))
            if(pause_before_overwrite): pause_prog('Continue?')
        image_data.zap()
    return image_data


################################################################################
def check_selfcal_flagver(aips_data, flagver):
    if(check_table_version_valid(aips_data, 'AIPS FG', flagver)):
        return flagver
    else: return 0


################################################################################
def is_solmode_doing_amplitude_single(solmode):
    """is solmode doing aplitude calibration for single source file?"""
    flag = 0
    if((solmode == None) or (solmode == '')): pass
    elif((solmode[0] == 'A') or (solmode[0] == 'G')): flag=1
    return flag




################################################################################
def make_default_selfcal_calib_parameters_dict(imsize,
                                               solint,
                                               C_center,
                                               source_size,
                                               combine_data,
                                               weak_source,
                                               niter = 1000,
                                               imagr_details = None,
                                               imagr_fields = 1):
    """Setup default CALIB and IMAGR parameters for selfcal runs

    solint        I  the solution interval, in seconds
    C_center      I  the center location for a clean box for IMAGR, as a C array
    source_size   I  the radial size of the source, in pixels
    combine_data  I  Should the polarizations and IFs be combined in the fit?
    weak_source   I  Should the RMS noise cutoff be lowered?
    niter         I  number of clean iterations
    imagr_details I  A dict containting special information for IMAGR to
                     specify many imaging details
    imagr_fields  I  The total number of fields that IMAGR will image in one go
    """
    cal_par = {}
    cal_par['run_imagr'] = 1              # run IMAGR to make an image to selfcal
    cal_par['imagr_flux_fraction'] = 0.001# Don't trust things below 0.1%
    cal_par['imagr_noise_cutoff'] = 1.0   # Don't trust things below 2-sigma
    cal_par['niter'] = niter              # normally do up to 1000 clean iters
    cal_par['dotv'] = 0                   # For now, turn off the tv
    clbox = [None,-1,source_size,C_center[1]+1,C_center[0]+1]
    if(imagr_fields == 1):
        cal_par['clbox'] = clbox
    else:
        cb = [None]
        for i in xrange(imagr_fields):
            cb.append(clbox)
        cal_par['clbox'] = cb
    cal_par['sequence_in'] = 0            # use the original data
    cal_par['sequence_out'] = 1           # and make sequence 1
    cal_par['solint'] = solint            # user specified 
    cal_par['solsub'] = 2                 # step by half a solint
    if(imagr_fields == 1):
        cal_par['ncomp'] = 100            # take first 100 components + or -
    else:
        cal_par['ncomp'] = [100]*imagr_fields # take first 100 components + or -
    cal_par['flux'] = 0.0                 # take all flux valued components
    if(math.fabs(solint) < 1001):
        cal_par['soltype'] = ''           # use the default least squares
    else:
        cal_par['soltype'] = 'L1'         # Use something slightly more robust
    cal_par['solmode'] = 'P!A'            # phase only self-cal
    cal_par['aparm'] = [None, 3,0,0,0,0,2,3,0,1]
                                          # default aparm
                                          # This sets the min number antennas 3
                                          # some print info level 2
                                          # minimum RMS 3
                                          # keep data from failed solutions
    cal_par['cparm'] = [None,0,0,0,0,0,0,0,0,0]
    if(combine_data):
        cal_par['aparm'][3] = 1
        cal_par['aparm'][5] = 1
    if(weak_source): cal_par['aparm'][7] = 2.5
    cal_par['show_SN_table'] = 0          # do not run SNPLT for now
    cal_par['imagr_details'] = imagr_details
    cal_par['num_imagr_fields'] = imagr_fields
    return cal_par




################################################################################
def make_default_selfcal_calib_parameters_list(num_selfcals,
                                               imsize,
                                               solint,
                                               solint_amp,
                                               C_center,
                                               source_size,
                                               combine_data,
                                               weak_source,
                                               do_ampl_cal,
                                               extended_source = 0,
                                               niter = 1000,
                                               imagr_details = None,
                                               imagr_fields = 1
                                               ):
    """Make an array of dictionaries to control CALIB and IMAGR for selfcal

    num_selfcals  I  how many self-cals do you want?

    solint        I  the solution interval, in seconds for phase-only calibration
    solint_amp    I  the solution intercal in seconds for amplitude selfcal
    C_center      I  the center location for a clean box for IMAGR, as a C array
    source_size   I  the radial size of the source, in pixels
    combine_data  I  Should the polarizations and IFs be combined in the fit?
    weak_source   I  Should the RMS noise cutoff be lowered?
    do_ampl_cal   I  should an amplitude calibration be done?
    extended_source I  Is the source mostly point-like (0), or is it very
                     extended (>0)  This actually puts in the number of
                     components you wish to use in ncomp
    niter         I  number of clean iterations
    imagr_details I  A dict containting special information for IMAGR to
                     specify many imaging details
    imagr_fields  I  The total number of fields that IMAGR will image in one go
    """
    cal_par = range(0,num_selfcals)  # make a list
    # Now, for each element in the list, assign the default dict to it
    for i in xrange(0,num_selfcals):
        cal_par[i] = \
            make_default_selfcal_calib_parameters_dict(imsize,solint,C_center,
                                                       source_size, combine_data,
                                                       weak_source, niter,
                                                       imagr_details,
                                                       imagr_fields)
        if(extended_source): cal_par[i]['ncomp'] = extended_source
    # Now go back and modify niter, ncomp, etc. for the first few iterations
    if(num_selfcals > 2):
        if(not extended_source):
            cal_par[0]['niter'] = 50
            cal_par[0]['ncomp'] = 5
            if(imagr_fields > 1):
                cal_par[0]['ncomp'] = [5]*imagr_fields
            if(source_size < imsize/2):
                cal_par[0]['clbox'] = [None,-1,6,C_center[1]+1,C_center[0]+1]
        cal_par[0]['solint'] = cal_par[0]['solint']*2
        #cal_par[0]['aparm'][7] = 3
    if(num_selfcals > 3):
        if(not extended_source):
            cal_par[1]['niter'] = 75
            cal_par[1]['ncomp'] = 10
            if(imagr_fields > 1):
                cal_par[1]['ncomp'] = [10]*imagr_fields
            if(source_size < imsize/2):
                cal_par[1]['clbox'] = [None,-1,10,C_center[1]+1,C_center[0]+1]
        cal_par[1]['solint'] = cal_par[0]['solint']*1.5
    if(num_selfcals > 4):
        if(not extended_source):
            cal_par[2]['niter'] = 100
            cal_par[2]['ncomp'] = 15
            if(imagr_fields > 1):
                cal_par[2]['ncomp'] = [15]*imagr_fields
            if(source_size < imsize/2):
                cal_par[2]['clbox'] = [None,-1,15,C_center[1]+1,C_center[0]+1]
    if(num_selfcals > 5):
        if(not extended_source):
            cal_par[3]['niter'] = 150
            cal_par[3]['ncomp'] = 20
            if(imagr_fields > 1):
                cal_par[3]['ncomp'] = [20]*imagr_fields
    # show the first and last SN tables
    cal_par[0]['show_SN_table'] = 1
    cal_par[num_selfcals-1]['show_SN_table'] = 1
    # check amplitude calibration stuff
    if(do_ampl_cal):
        cal_par[num_selfcals-1]['solint'] = solint_amp
        cal_par[num_selfcals-1]['solmode'] = 'A&P'
        cal_par[num_selfcals-1]['cparm'] = [None,0,2,0] # constrain mean gain
        if(num_selfcals > 1):
            cal_par[num_selfcals-1]['sequence_in'] = 1 # the phase calibrated set
            cal_par[num_selfcals-1]['sequence_out'] = 2# and make sequence 2
            cal_par[num_selfcals-1]['run_imagr'] = 0
            cal_par[num_selfcals-2]['show_SN_table'] = 1
    if((do_ampl_cal) and (num_selfcals > 8)):
        cal_par[num_selfcals-3]['solint'] = solint_amp
        cal_par[num_selfcals-3]['solmode'] = 'A&P'
        cal_par[num_selfcals-3]['sequence_in'] = 1 # use the phase calibrated set
        cal_par[num_selfcals-3]['sequence_out'] = 2# and make sequence 2
        cal_par[num_selfcals-3]['run_imagr'] = 0
        cal_par[num_selfcals-3]['cparm'] = [None,0,2,0] # constrain mean gain
    # that's all from here
    return cal_par

    






################################################################################
# Run a series of IMAGR clean and CALIB self-cal's on a Single Source Dataset
def self_cal_source(aips_data,
                    source_name='',
                    flagver = 0,
                    bpver = -1,
                    refant = 1,
                    timerange=[0,0,0,0],
                    imsize = 512,
                    cellsize=1,
                    robust=8,
                    previous_image_max = 0.0,
                    previous_image_rms = 0.0,
                    calib_parameters = None,
                    calib_class_name = 'CALIB',
                    use_initial_point_source_calibration=1,
                    image_sequence_number = 1,
                    save_final_image = 0,
                    save_final_calib_output=0,
                    processing_name = '',
                    stokes = 'I',
                    uvrange = None
                    ):
    """Runs a series of IMAGR/CALIB steps to self-cal a source

source_name       # I  Used mostly for possible generation of file names
                  #    may leave blank


calib_parameters  # I  A list of dictionaries giving the CALIB parameters.
use_initial_point_source_calibration    If nonzero, then apply an initial
                    point source calibration using the calibration parameters
                    found in the 0 element of calib_parameters.  If
                    use_initial_point_source_calibration == 2 and
                    len(calib_parameters) == 1, then just do the single
                    point-source calibration

There is no gainuse variable, as this is assumed to be a single source file
which has been split off and the calibration (CL) table applied.  Similarly, this
routine does not pay attention to a possible BP table for bandpass corrections,
which it assumes have also been applied.
    """
    assert(type(calib_parameters) == type([0])), "Need a list here"
    # If there are existing SN tables, warn the user that they are about
    # to be deleted
    if((__debug__) and (aips_data.table_highver('AIPS SN') > 0)):
        warnings.warn("'%s': SN tables exist. Hit Enter to delete."\
                      %self_cal_source.func_name)
        if(pause_before_overwrite): pause_prog('Continue?')
    # AIPS stupidly forces SNVER to be 0 for single-source files.  So
    # I have to check which version is to be used myself
    snver_use = 0
    # check that the specified flag table is present
    assert(check_table_version_valid(aips_data, 'AIPS FG', flagver))
    # Create the IMAGR file variable
    imagr_image = make_selfcal_imagr_outfile(aips_data, source_name,
                                             image_sequence_number, stokes)
    # IMAGR will need to run on a dataset
    calib_out = aips_data
    # check for valid image max and RMS
    if(previous_image_max <= 0.0): previous_image_max = 1
    if(previous_image_rms <= 0.0): previous_image_rms = 0
    # Are there multiple images?
    max_number_images = 0
    
    

    # Run through the list of SELFCAL steps
    for step in xrange(-1,len(calib_parameters)):
        if __debug__: print 'Entering step', step, 'of the SELFCAL process'
        # Get the right list of calibration parameters.  If this is step -1
        # and we are not doing a point source CALIB run, then skip
        if(step < 0):
            if not(use_initial_point_source_calibration): continue
            if((use_initial_point_source_calibration == 2)
               and (len(calib_parameters) == 1)): continue
            step = 0
        cal_par = calib_parameters[step]
        if __debug__: print cal_par
        # Now, check whether or not we are making a new image
        if( not(use_initial_point_source_calibration) and
            (cal_par['run_imagr']) ):
            if(max_number_images < cal_par['num_imagr_fields']):
                max_number_images = cal_par['num_imagr_fields']
            flagver_use = check_selfcal_flagver(calib_out, flagver)
            flux_level = max(math.fabs(previous_image_max*cal_par['imagr_flux_fraction']),
                             previous_image_rms*cal_par['imagr_noise_cutoff'])
            run_imagr(calib_out,
                      cellsize,
                      imsize,
                      source_name,
                      imagr_image,
                      calib_out.table_highver('AIPS SN'),
                      cal_par['niter'],
                      timerange,
                      flagver_use,
                      bpver,
                      stokes,             # Stokes
                      flux_level,
                      cal_par['dotv'],
                      cal_par['clbox'],
                      robust,
                      uvrange=uvrange,
                      details=cal_par['imagr_details']
                      )
            # Get the image statistics for next time
            (previous_image_max, junk1, junk2, previous_image_rms) \
                = get_small_source_stats_main(imagr_image, imsize/4, imsize/4,
                                              imsize/2)
##             if __debug__:
##                 # print out the CC table
##                 print_cc(imagr_image, processing_name)
        # End of running imagr part
        # set up the data files
        calib_in = make_selfcal_calib_data_in(aips_data, source_name,
                                              calib_class_name,
                                              cal_par['sequence_in'])
        if( not(calib_out == calib_in) and not(calib_out == aips_data) ):
            if(calib_out.exists()): calib_out.zap()
        calib_out = make_selfcal_calib_data_out(calib_in, source_name,
                                                calib_class_name,
                                                cal_par['sequence_out'])
        #print aips_data
        #print calib_in
        #print calib_out

        flagver_use = check_selfcal_flagver(calib_in, flagver)

        if not(use_initial_point_source_calibration):
            run_calib(calib_in,
                      refant,
                      cal_par['solint'],
                      cal_par['solsub'],
                      source_name,
                      0,                  # gainuse
                      timerange,
                      flagver_use,
                      bpver,
                      imagr_image,
                      cal_par['ncomp'],
                      cal_par['flux'],
                      None,               # smodel
                      cal_par['aparm'],
                      cal_par['soltype'],
                      cal_par['solmode'],
                      snver_use,
                      1,                  # save_data
                      calib_out,
                      uvrange=uvrange,
                      cparm = cal_par['cparm']
                      )
        else:
            run_calib(calib_in,
                      refant,
                      cal_par['solint'],
                      cal_par['solsub'],
                      source_name,
                      0,                  # gainuse
                      timerange,
                      flagver_use,
                      bpver,
                      None,               # image_data
                      None,               # ncomp
                      None,               # flux
                      previous_image_max, # smodel
                      cal_par['aparm'],
                      cal_par['soltype'],
                      cal_par['solmode'],
                      snver_use,
                      1,                  # save_data
                      calib_out,
                      uvrange=uvrange,
                      cparm = cal_par['cparm']
                      )
            use_initial_point_source_calibration = 0
            calib_out.zap()
            calib_out = calib_in
        if(cal_par['show_SN_table']):
            snplot(calib_in, 'SN', calib_in.table_highver('AIPS SN'), 'PHAS',opcode='ALSI',
                   nplots=9#len(calib_in.antennas)
                   )
            #pause_prog('Check SN table')
            if(is_solmode_doing_amplitude_single(cal_par['solmode'])):
                snplot(calib_in, 'SN',calib_in.table_highver('AIPS SN'),'AMP',opcode='ALSI',
                       nplots=9#len(calib_in.antennas)
                       )
                #pause_prog('Check SN table')
    # end of for step in xrange over calib steps

    # clean up all of the CALIB files
    for step in xrange(0,len(calib_parameters)):
        calib_in =\
            make_selfcal_calib_data_in_nocheck(aips_data, source_name,
                         calib_class_name, calib_parameters[step]['sequence_in'])
        if( not(calib_out == calib_in) and not(calib_in == aips_data) ):
            if(calib_in.exists()): calib_in.zap()





    # Clean up after ourselves.  If we are not keeping the last CALIB output
    # file, then delete it, unless it is the original data file for some reason.
    if not(save_final_calib_output):
        if(calib_out == aips_data): pass
        else:
            if(calib_out.exists()): calib_out.zap()
    if not(save_final_image):
        if(imagr_image.exists()): imagr_image.zap()
        if(max_number_images > 1):
            for i in xrange(2,max_number_images+1):
                klass = imagr_image.klass[0:3]+"%3.3d"%(i)
                this_image = AIPSImage(imagr_image.name, klass,
                             imagr_image.disk, imagr_image.seq)
                if(this_image.exists()): this_image.zap()
    # return some stuff to the caller, in case they want them
    return calib_out,imagr_image,previous_image_max,previous_image_rms
    













################################################################################
# load in the data from the VLA
def run_fillm(filename,
              dataname,
              band,
              data_seq,
              data_disk,
              program_code = '',
              shadow_distance = 28,
              cl_interval = 30,
              nfiles = 0,
              version = None,
              ncount = 0,
              doweight = 1
              ):
    """Load VLA data

filename   I  The name of file to load from disk
dataname   I  The name of the AIPS dataset to create
band       I  Observing band to load.  Should be one of 'L', 'C', 'X', etc.
              or an array of such bands
data_seq   I  The AIPS sequence number to use
data_disk  I  Which disk?
program_code I  Program code to suck out
shadow_distance I  Minimum baseline length, to avoid shadowing, in m
cl_interval I  The time between CL table entries, in s
nfiles     I  Where to start?  You probably want 0
version    I  Which AIPS version to use
ncount     I  How many files to load.  0 means all
doweight   I  Apply data weights?


OUTPUTS: uvdata
uvdata     I  If band is a simple character, then this is a simple
              AIPSUVData object
              Otherwise, if band is a list, then this is a list of the
              AIPSUVData objects created
"""
    if(version is not None):
        fillm = AIPSTask('fillm', version = version)
    else:
        fillm = AIPSTask('fillm')
    fillm.infile = filename
    fillm.nfiles = nfiles
    fillm.vlaobs = program_code
    fillm.outname = dataname
    fillm.outseq = data_seq
    fillm.outdisk = data_disk
    fillm.douvcomp = -1
    if(ncount != 1):
        fillm.doconcat = 1
    fillm.ncount = ncount
    fillm.doweight = doweight
    fillm.cparm[4] = shadow_distance
    fillm.cparm[8] = cl_interval / 60.0
    fillm()
    if(type(band) == type('L')):
        uvdata = AIPSUVData(dataname, band + ' BAND', data_disk, data_seq)
        if(uvdata.exists()): return uvdata
        raise AssertionError, "After FILLM, did not find " + uvdata.__str__()
    else:
        uvdata = []
        for b in band:
            uv = AIPSUVData(dataname, b + ' BAND', data_disk, data_seq)
            if(uv.exists()):
                uvdata.append(uv)
            else:
                raise AssertionError, "After FILLM, did not find " + uv.__str__()
        return uvdata
    raise RuntimeError("Programmer Error")





################################################################################
# VLA needs to be qucked
def run_quack(uvdata,
              flagver_use,
              opcode = '',
              reason = "Standard Quack",
              aparm = [None,0,10,0],
              version = None
              ):
    """run quack

uvdata      I  dataset to be quacked
flagver_use I  Which flag table
opcode      I  OPCODE  '' is 'BEG'
reason      I  reason string
aparm       I  codes for how much flagging to do, in s
"""
    if(version is not None):
        quack = AIPSTask('quack', version = version)
    else:
        quack = AIPSTask('quack')
    quack.indata = uvdata
    quack.flagver = flagver_use
    quack.opcode = opcode
    quack.reason = reason
    index = 1
    for x in aparm:
        if(x == None): continue
        quack.aparm[index] = x/60.0
        index = index+1
    quack()



    
    
    
    
################################################################################
# set the flux density scale
def run_setjy(uvdata,
              sources,
              zerosp = [None,0],
              optype = 'CALC',
              calc_date_option = 0,
              calc_flux_option = 1.0,
              version = None
              ):
    """run setjy

uvdata      I  dataset to be setjyed
sources     I  Name of soruce(s) to calibrate
zerosp      I  zero spacing flux info
opcode      I  OPCODE
calc_date_option I  which calibration to use for 'CALC'
calc_flux_option I  how much to scale the flux
"""
    if(version is not None):
        setjy = AIPSTask('setjy', version = version)
    else:
        setjy = AIPSTask('setjy')
    setjy.indata = uvdata
    setjy.sources = to_aips_array(sources)
    setjy.zerosp = to_aips_array(zerosp)
    setjy.optype = optype
    setjy.aparm[2] = calc_date_option
    setjy.aparm[3] = calc_flux_option
    setjy()


    
    
    
    
################################################################################
# calibrate the amplitudes of a phase calibrator
def run_getjy(uvdata,
              sources,
              calsources,
              snver = None,
              timerange = None,
              antennas = None,
              version = None
              ):
    """run getjy

uvdata      I  dataset to be setjyed
sources     I  Name of soruce(s) to calibrate, i.e. phase calibrators
calsources  I  Name of the amplitude calibrators to use
...
"""
    if(version is not None):
        getjy = AIPSTask('getjy', version = version)
    else:
        getjy = AIPSTask('getjy')
    getjy.indata = uvdata
    getjy.sources = to_aips_array(sources)
    getjy.calsour = to_aips_array(calsources)
    if(snver): getjy.snver = snver
    getjy.antennas = to_aips_array(antennas)
    getjy.timerang = to_aips_array(timerange)
    getjy()


    



################################################################################
# change (u,v,w)'s to J2000
def run_uvfix_BtoJ(uvdata_in,
                   uvdata_out,
                   VLA_tau_int = 0,
                   observing_epoch = None,
                   version = None
                   ):
    """Convert (VLA) data from B1950 coordinates to J2000

uvdata_in       I  incoming dataset
uvdata_out      I  outgoing dataset
VLA_tau_int     I  integration time, in seconds, for VLA data, to
                   correct the MODCOMP timing error.  If not VLA data, set
                   to 0
observing_epoch I  Observing epoch, such as 1989.5.  If None, get from the
                   dataset
"""
    # Now run UVFIX to try to fix the (u,v,w)'s which DTSIM screws up
    if(version is not None):
        uvfix = AIPSTask('uvfix', version = version)        
    else:
        uvfix = AIPSTask('uvfix')
    uvfix.indata = uvdata_in
    uvfix.outdata = uvdata_out
    uvfix.uvfixprm[13] = VLA_tau_int * 0.5
    if(observing_epoch is None):
        year, month, day = get_observation_year_month_day(uvdata_in)
        observing_epoch =  year + get_observation_day_of_year(uvdata_in)/365.0
    uvfix.uvfixprm[17] = observing_epoch
    uvfix()





################################################################################
# change (u,v,w)'s to J2000
def run_uvfix(uvdata_in,
              uvdata_out,
              ra_shift = None,
              dec_shift = None,
              UT1_UTC = None,
              IAT_UTC = None,
              tau_int_shift = None,
              observing_epoch = None,
              uvfixprm = None,
              version = None
              ):
    """Convert (VLA) data from B1950 coordinates to J2000

uvdata_in       I  incoming dataset
uvdata_out      I  outgoing dataset
ra_shift        I  right ascension shift (in AIPS terms) in as
dec_shift       I  declination shift (in AIPS terms) in as
UT1_UTC         I  UT1 - UTC time difference, in s
IAT_UTC         I  IAT - UTC time difference, in s
tau_int_shift   I  clock error, in seconds
observing_epoch I  Observing epoch, such as 1989.5
uvfixprm        I  Array of UVFIXPRM values.  If present, this overrides
                   UT1_UTC, IAT_UTC, tau_int_shift, and observing_epoch
"""
    # Now run UVFIX to try to fix the (u,v,w)'s which DTSIM screws up
    if(version is not None):
        uvfix = AIPSTask('uvfix', version = version)        
    else:
        uvfix = AIPSTask('uvfix')
    uvfix.indata = uvdata_in
    uvfix.outdata = uvdata_out
    if(ra_shift): uvfix.shift[1] = ra_shift
    if(dec_shift): uvfix.shift[2] = dec_shift
    if(uvfixprm):
        uvfix.uvfixprm = to_aips_array(antennas)
    else:
        if(UT1_UTC): uvfix.uvfixprm[11] = UT1_UTC
        if(IAT_UTC): uvfix.uvfixprm[12] = IAT_UTC
        if(tau_int_shift): uvfix.uvfixprm[13] = tau_int_shift
        if(observing_epoch is None):
            year, month, day = get_observation_year_month_day(uvdata_in)
            observing_epoch =  year + get_observation_day_of_year(uvdata_in)/365.0
        if(observing_epoch): uvfix.uvfixprm[17] = observing_epoch
    uvfix()














################################################################################
# load a UV fits file
def run_fitld(uvdata_out,
              filename,
              cl_table_interval = 10.,
              weight_threshold = 0.7,
              digicor = -1,
              ncount = 1,
              version = None
              ):
    """Load a UV file into AIPS with FITLD, from *DISK*

uvdata_out    I the output file
filename      I the FITS file on disk
cl_table_interval I  The desired CL table interval, in s
weight_threshold  I  Weight threshold
digicor       I  Correlator corrections?
ncount        I  Number of files to load, when they are semi-indipendent
"""
    if(version is not None):
        fitld = AIPSTask('fitld', version = version)
    else:
        fitld = AIPSTask('fitld')
    fitld.infile = filename
    fitld.outdata = uvdata_out
    fitld.ncount = ncount
    fitld.douvcomp = -1
    fitld.doconcat = 1
    fitld.clint = cl_table_interval / 60.0   # convert from s to min
    fitld.wtthresh = weight_threshold
    fitld.digicor=digicor
    fitld()




################################################################################
# sort stuff in the datafile
def run_msort(uvdata_in,
              uvdata_out,
              version = None):
    if(version is not None):
        msort = AIPSTask('msort', version=version)
    else:
        msort = AIPSTask('msort')
    msort.indata = uvdata_in
    msort.outdata = uvdata_out
    msort()



################################################################################
# run indxr to generate scan info
def run_indxr(uvdata,
              cl_table_interval = 10.,
              max_scan_length = 1200.,
              version = None
              ):
    """run INDXR

uvdata                I  dataset
cl_table_interval     I  Desired CL table interval, in s
max_scan_length       I  Maximum allowed scan length, in s
"""
    if(version is not None):
        indxr = AIPSTask('indxr', version=version)
    else:
        indxr = AIPSTask('indxr')
    indxr.indata = uvdata
    indxr.cparm[2] = max_scan_length / 60.0   # minutes
    indxr.cparm[3] = cl_table_interval / 60.0   # convert from s to min
    indxr()





################################################################################
# dump a datafile out to disk
def run_fittp(uvdata,
              output_filename,
              version = None
              ):
    """Dump out a fits file"""
    if(version is not None):
        fittp = AIPSTask('fittp', version = None)
    else:
        fittp = AIPSTask('fittp')
    fittp.indata = uvdata
    fittp.outfile = output_filename
    fittp()




################################################################################
# run dtsum
def run_dtsum(uvdata,
              print_level = None,
              outprint = None,
              version = None
              ):
    """run dtsum

uvdata        I  The dataset
print_level   I  How much to print
outprint      I  File to print to.  If None, make one up
"""
    if(version is not None):
        dtsum = AIPSTask('dtsum', version = version)
    else:
        dtsum = AIPSTask('dtsum')
    dtsum.indata = uvdata
    if(print_level): dtsum.aparm[1] = print_level
    dtsum.docrt = -1
    if(outprint == None):
        dtsum.outprint = dirname + '/' + uvdata.name + ".listr"
    else:
        dtsum.outprint = outprint
    if os.path.isfile(dtsum.outprint):
        warnings.warn("File '%s' already exists.  Deleting."%dtsum.outprint)
        os.remove(dtsum.outprint)
    dtsum()







################################################################################
def run_tacop(aips_data_in,
              table_ext,
              table_inver = None,
              ncount = None,
              aips_data_out = None,
              table_outver = None,
              version = None
              ):
    """Run TACOP with default options

aips_data_in    I  The incoming dataset
table_ext       I  The table extension type to use (e.g. 'CL')
table_inver     I  The version number to copy
ncount          I  How many to copy (0 means all in AIPS???)
aips_data_out   I  The outgoing dataset.  If None, uses aips_data_in
table_outver    I  The outgoing version number to write to
"""
    if(version is not None):
        tacop = AIPSTask('tacop', version=version)
    else:
        tacop = AIPSTask('tacop')
    tacop.indata = aips_data_in
    if(aips_data_out != None):
        tacop.outdata = aips_data_out
    else:
        tacop.outdata = aips_data_in
    tacop.inext = table_ext
    if(table_inver): tacop.inver = table_inver
    if(ncount): tacop.ncount = ncount
    if(table_outver): tacop.outver = table_outver
    tacop()




################################################################################
def run_tasav(aips_data_in,
              aips_data_out,
              version = None
              ):
    """Run TASAV with default options

aips_data_in    I  The incoming dataset
aips_data_out   I  The outgoing dataset.
"""
    if(version is not None):
        tasav = AIPSTask('tasav', version=version)
    else:
        tasav = AIPSTask('tasav')
    tasav.indata = aips_data_in
    tasav.outdata = aips_data_out
    tasav()




################################################################################
def run_tecor(aips_data,
              ionex_directory,
              ionex_type,
              CL_in = None,
              CL_out = None,
              aparm=[None,1,0],
              version = None
              ):
    """Runs TECOR using standard IONEX files

aips_data,       I  The aips dataset to use
ionex_directory  I  The directory path to the IONEX files.  You might
                    use '.'
ionex_type       I  The type of IONEX file you want to use.  More properly,
                    this is the name of the group which produces the
                    IONEX file of your choosing.  Probable options are
                    'cod', 'esa', 'jpl', etc.
CL_in            I  The number of the CL table to start with
CL_out           I  The number of the CL table to write to
aparm            I  Control parameters
"""
    # Get the name of the first TECOR file
    year = aips_data.header.date_obs[2:4]
    doy = get_observation_day_of_year(aips_data)
    filename = ionex_directory + '/' + ionex_type + 'g%3.3d0.%si'%(doy,year)
    # Now run TECOR
    if(version is not None):
        tecor = AIPSTask('tecor', version = version)
    else:
        tecor = AIPSTask('tecor')
    tecor.indata = aips_data
    tecor.infile = filename
    tecor.nfiles = get_number_days_observations(aips_data)
    if(CL_in): tecor.gainver = CL_in
    if(CL_out): tecor.gainuse = CL_out
    tecor.aparm = to_aips_array(aparm)
    tecor()




