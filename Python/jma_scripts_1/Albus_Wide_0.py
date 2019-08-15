# Albus_Wide_0.py
# Python stuff for dealing with wide field imaging
# 2006 Jul 12  James M Anderson  --JIVE  start



################################################################################
# some import commands  The User should not need to change this.
################################################################################

################################################################################
# Mark's ParselTongue stuff
from AIPS import AIPS, AIPSDisk
from AIPSTask import AIPSTask, AIPSList
from AIPSData import AIPSUVData, AIPSImage
from AIPSTV import AIPSTV
import Wizardry.AIPSData

################################################################################
# miscellaneous stuff
import copy, optparse, os, sys
import re, string
#import numarray
#import numarray.ma
#import numarray.ieeespecial
#import numarray.nd_image
#import inspect
import warnings
import math
import time as systime

################################################################################
# JMA's ionosphere stuff
import Albus_Coordinates
import jma_aips











################################################################################
# Global variables






################################################################################
# FUNCTIONS






################################################################################
def read_wide_position_list_0(filename):
    """Reads in a list of source names and positions

filename     I  The name of the file to open

OUTPUT returned as:  uvdata, ra, dec

uvdata       O  list of AIPSUVData objects corresponding to the
                input names
ra           0  list of right ascensions, in radians
dec          O  list of declinations, in radians


The file has the following form:
# Lines starting with a # are comment lines and are ignored
# Name   Class  Disk  Sequence    RA_2000        Dec_2000
MY_Src   clss   1     2           12h04m45.6s     -01:34:21.5

The Name and Class items must contain no internal spaces, and should
conform to AIPS standards for acceptible characters.  The Disk and
Sequence items must be integers.  RA and Dec can be specified in multiple
formats, but should be in Hours minutes, seconds, and Degrees, minutes, seconds.

    """
    fp = open(filename, "r")
    uvdata = []
    ra = []
    dec = []
    for line in fp:
        if(line[0] == '#'): continue
        s = line.split(None, 4)
        # AIPS has 12 character name limit
        s[0] = s[0][0:12]
        this_uv = AIPSUVData(s[0].upper(), s[1].upper(), int(s[2]), int(s[3]))
        this_ra, this_dec = Albus_Coordinates.radec_str_to_rad(s[4])
        uvdata.append(this_uv)
        ra.append(this_ra)
        dec.append(this_dec)
    return uvdata, ra, dec






################################################################################
def split_out_wide_uvdata_0(uvdata_in,
                            current_source,
                            position_filename,
                            scratch_disk = 1,
                            output_object = None,
                            current_ra = None,
                            current_dec = None,
                            split_splat = 'splat',
                            dopol = None,
                            blver = None,
                            flagver = None,
                            doband = None,
                            bpver = None,
                            n_channel_average = None,
                            solint = None,
                            overwrite = 0
                            ):
    """splits out 1 or more positions from a file for wide-field imaging

uvdata_in         I  The AIPSUVData object with the original data
current_source    I  The name of the current source to use.
position_filename I  Full path+filename of the text file to read the positions
                     from.  See read_wide_position_list_0 for more info.
scratch_disk      I  Number of the scratch disk to use.  This needs to be
                     big enough to hold a copy of uvdata_in
output_object     I  The object to output.  If None, do all of the objects
                     found in position_filename.  If an integer, then work on
                     the output_object'th object.  If a string, then
                     work on the first object whose name matches.
current_ra        I  The current phase center position of uvdata_in
                     in right ascension.  In radians.  If None, use
                     SU table 1, source current_source.  If None,
                     current_dec must also be None.
current_dec       I  The current phase center position of uvdata_in
                     in declination.  In radians.  If None, use
                     SU table 1, source current_source.  If None,
                     current_ra must also be None.
split_splat       I  Which AIPS splitting routine to use?  Should be
                     'splat' for splat
                     'split' for split
                     else error
overwrite         I  May output files be overwritten?  0 no, else yes.


OUTPUT as: uvdata

uvdata            O  list of AIPSUVData objects.
"""
    # First, get the possible objects
    new_uvdata, ra, dec = read_wide_position_list_0(position_filename)
    # Figure out how many we are doing
    start_pos = 0
    end_pos = len(new_uvdata)
    if(output_object is not None):
        if(type(output_object) == type(5)):
            assert((output_object>=0) and (output_object<len(new_uvdata)))
            start_pos = output_object
            end_pos = output_object + 1
        elif(type(output_object) == type("string")):
            for i in xrange(len(new_uvdata)):
                if(new_uvdata.name == output_object.upper()):
                    start_pos = i
                    end_pos = i + 1
                    break
            else:
                raise KeyError, "Name '%s' not found"%output_object
        else:
            raise TypeError, "output_object has unsupported type "+type(output_object).__str__()
    # Get the source position
    source_id = None
    sources = Albus_Iono.get_source_positions(uvdata_in)
    for s in xrange(len(sources)):
        if(sources[s] is None): continue
        if(sources[s][1] == current_source):
            source_id = s
            # Get the current position information
            if(current_ra is None):
                assert(current_dec is None)
                current_ra = sources[s][2]
                current_dec = sources[s][3]
    else:
        raise KeyError, "Source name '%s' not found"%current_source
    cos_dec = math.cos(current_dec)
    if(cos_dec <= 0.0): cos_dec = 1.0
    uvdata = []
    for i in xrange(start_pos,end_pos):
        # calculate the shift
        # From AIPS, RA' = RA + SHIFT_RA/COS(DEC)
        #            DEC' = DEC + SHIFT_DEC
        delta_ra_as = (ra[i] - current_ra) * cos_dec * M_RAD2AS
        delta_dec_as = (dec[i] - current_dec) * M_RAD2AS
        # need a scratch file
        for i in xrange(1,256):
            temp_uv = AIPSUVData("SP_WIDE_0", "ALBTMP", scratch_disk, i)
            if(not temp_uv.exists()):
                break
        else:
            raise IndexError, "Cannot find temp slot "
        run_uvfix(uvdata_in,
                  temp_uv,
                  ra_shift = delta_ra_as,
                  dec_shift = delta_dec_as
                  )
        # Ok, fudge the source name
        temp_wiz = Wizardry.AIPSData.AIPSUVData(temp_uv.name,
                                                temp_uv.klass, 
                                                temp_uv.disk,
                                                temp_uv.seq)
        SU_table = temp_wiz.table('AIPS SU', 1)
        for row in SU_table:
            if(row.id__no == source_id):
                row.source = new_uvdata.name
                row.update()
                break
        # Now split out the data
        if(split_splat == 'splat'):
            aparm=[None,1]
            run_splat(temp_uv,new_uvdata, new_uvdata.name,
                      docalib = docalib, gainuse = CL_out,
                      dopol = dopol, blver = blver,
                      flagver = flagver, doband = doband,
                      bpver = bpver, aparm=aparm,
                      channel = n_channel_average,
                      solint = solint)
        elif(split_splat == 'split'):
            if(solint != None):
                warnings.warn("solint value set, but split does not perform time averaging")
            aparm=[None,1]
            out_list = run_split(temp_uv,new_uvdata, new_uvdata.name,
                                 docalib = docalib, gainuse = CL_out,
                                 dopol = dopol, blver = blver,
                                 flagver = flagver, doband = doband,
                                 bpver = bpver, aparm=aparm,
                                 nchav = n_channel_average,
                                 overwrite=overwrite)
            if(out_list[0] == new_uvdata):
                pass
            else:
                raise RuntimeError, "Split output file not what expected, wanted %s, got %s"%(new_uvdata.__str__(),out_list[0].__str__())
        else:
            raise KeyError, "Unknown split_splat type %s"%split_splat
        
        

