# jma_EVN.py
# functions for specific ENV processing tasks
# 2007 Jun 25  James M Anderson  --JIVE  start





# Functions that the user is probably interested in calling are:
#     correct_180_phase_jumps













################################################################################
# some import commands  The User should not need to change this.
################################################################################
from AIPS import AIPS, AIPSDisk
from AIPSTask import AIPSTask, AIPSList
from AIPSData import AIPSUVData, AIPSImage
from AIPSTV import AIPSTV
import Wizardry.AIPSData

import copy, optparse, os, sys
import re, string
import numarray.ma
import numarray.ieeespecial
import numarray.nd_image
import inspect
import warnings
import math
import time as systime
import bisect

# import some of my own ionosphere stuff
import jma_aips




################################################################################
def convert_scan_list_to_times(aips_data, scan_list):
    """convert a list of scan ID numbers into times by the start of the scan

INPUTS:
aips_data    I  The AIPSUVData object to investigate
scan_list    I  A list of scan ID numbers to fid the start times for

OUTPUTS: time_list
time_list    I  A list of times as doubles in days since the reference date of
                the start of each scan.  This array is sorted in increasing time
                order.
    """
    assert(jma_aips.check_table_version_exists(aips_data, 'AIPS NX', 0))
    nx_table = aips_data.table('AIPS NX', 0)
    time_list = []
    for scan in scan_list:
        row = nx_table[scan-1]
        start_time = row.time - 0.5 * row.time_interval
        time_list.append(start_time)
    time_list.sort()
    return time_list


################################################################################
def convert_timerange_list_to_times(aips_data, timerange_list):
    """convert a list of AIPS timerange times to floating point days

INPUTS:
aips_data    I  The AIPSUVData object to investigate
timerange_list I  A list of AIPS-ish times, to use as boundary conditions.
                Each element of this list should have the form
                [day, hour, minute, second]

OUTPUTS: time_list
time_list    I  A list of times as doubles in days since the reference date of
                the start of each scan.  This array is sorted in increasing time
                order.
    """
    time_list = []
    for t in timerange_list:
        start_time = (((t[3] / 60.0 + t[2]) / 60.0 + t[1]) / 24.0 + t[0])
        time_list.append(start_time)
    time_list.sort()
    return time_list



################################################################################
def find_time_slot(t, time_list):
    """find the right index for the time_list where t < time_list[index]

INPUTS:
t            I  A time to investigate
time_list    I  A list of times as doubles in days since the reference date of
                the start of each scan.  This array must be sorted in increasing
                time order.  It must also have time_list[-1] greater than
                any possible t
    """
    # Check the last index, which is probably correct
    if(t < time_list[find_time_slot.last_index]):
        if(find_time_slot.last_index == 0):
            return find_time_slot.last_index
        elif(t >= time_list[find_time_slot.last_index-1]):
            return find_time_slot.last_index
    find_time_slot.last_index = bisect.bisect(time_list,t)
    return find_time_slot.last_index
find_time_slot.last_index = 0



################################################################################
def fix_scan_180_degree_phase_jumps(aips_data,
                                    CL_in,
                                    CL_out,
                                    subarray,
                                    antenna_number,
                                    time_list,
                                    overwrite=0
                                    ):
    """Fix a CL table for 180 degree phase jumps introduced at random scan starts

Some telescopes (Onsala) sometimes introduce 180 degree phase jumps in the
instrument (LO) phase at the beginning of scans.  This process appears to be
approximately random as to which scans this appears in.  This function takes in
a list of times of the beginnings of scans where there was a 180 degree phase
jump, and it corrects a CL table for those phase jumps.

This is intended to help with phase referencing, but it should also help
to a small extent with bright objects too.  However, for phase referencing,
if the target object is weak, there is no way to tell whether a phase jump
has happened at the start of the target scan.  :(

INPUTS:
aips_data    I  The AIPSUVData object to investigate
CL_in        I  Number of CL table to use for input.  If 0, use the highest.
CL_out       I  Number of CL table to write out to (must not exist if overwrite
                is false!)  If 0, use the highest +1
subarray     I  subarray number to correct
antenna_number I  antenna number in the subarray to correct
time_list    I  A list of times as doubles in days since the reference date of
                the start of each scan.  This array must be sorted in increasing
                time order.  This list gives the times at which the new phase
                starts.
overwrite    I  May the CL_out table be overwritten?

OUTPUTS: None
    """
    if(CL_out == 0):
        CL_out = aips_data.table_highver('AIPS CL') + 1
    if(CL_in == 0):
        CL_in = aips_data.table_highver('AIPS CL')
    assert(jma_aips.check_table_version_exists(aips_data, 'AIPS CL', CL_in))
    delete_CL_in_flag = 0
    if(CL_in == CL_out):
        delete_CL_in_flag = 1
        CL_in = aips_data.table_highver('AIPS CL') +1
        jma_aips.run_tacop(aips_data, 'CL', CL_out, 1, aips_data, CL_in)
        aips_data.zap_table('AIPS CL', CL_out)
    # Next, make sure the new CL table is free
    jma_aips.verify_table_version_free(aips_data, 'AIPS CL', CL_out, overwrite)
    # Ok, create the new CL table using a Wizardry dataset
    W_dataset = Wizardry.AIPSData.AIPSUVData(aips_data)
    # Get the old table and the new one
    CL_table_in = W_dataset.table('CL', CL_in)
    CL_table_out = W_dataset.attach_table(
        'CL', CL_out, no_term=CL_table_in.keywords['NO_TERM'])
    # How big is the CL table?
    Num_Total_CL_Rows = len(CL_table_in)
    #no_if=CL_table_in.keywords['NO_IF']  This is automatically copied
    #no_pol=CL_table_in.keywords['NO_POL']  This is automatically copied
    CL_table_out.keywords['REVISION'] = CL_table_in.keywords['REVISION']
    CL_table_out.keywords['NO_ANT'] = CL_table_in.keywords['NO_ANT']
    CL_table_out.keywords['MGMOD'] = CL_table_in.keywords['MGMOD']
    # how many polarizations are there? If 2, then set a flag
    two_pol = 0
    if(CL_table_in.keywords['NO_POL'] > 1): two_pol = 1
    NUM_IF = CL_table_in.keywords['NO_IF']
    # ready the time checks
    time_list = copy.copy(time_list)
    # &*@#! Python has no way of accessing standard IEEE 754 +Inf
    time_list.append(1E3000)    
    num_time_slots = len(time_list)
    phase_array = [0]*num_time_slots
    for i in xrange(num_time_slots):
        if(i&0x1 == 1):
            phase_array[i] = math.pi
    # Now loop over all of the rows in the old table to update and
    # place into the new one.  Keep track of how far along I am, so that
    # I can print out some progress messages
    start_time = systime.clock()
    count = -1
    for row in CL_table_in:
        count = count + 1
        if( (count&0x3FF) == 0 ):
            print "%5.1f%% completed in %10.1f s"%\
                  ((float(count)*100.0/Num_Total_CL_Rows),
                   systime.clock()-start_time)
        if((row.subarray == subarray) and (row.antenna_no == antenna_number)):
            # Ok, check the time.  Use 0.499 instead of 0.5 to avoid roundoff
            # errors, as AIPS only has 32-bit floats
            index = find_time_slot(row.time+0.499*row.time_interval, time_list)
            # Ok, do we rotate the phase?
            if(phase_array[index]):
                # assume phase 180 degrees
                for i in xrange(NUM_IF):
                    row.real1[i] = -row.real1[i]
                    row.imag1[i] = -row.imag1[i]
                if(two_pol):
                    for i in xrange(NUM_IF):
                        row.real2[i] = -row.real2[i]
                        row.imag2[i] = -row.imag2[i]
        CL_table_out.append(row)
    # Close the file to make sure the rows get written.
    CL_table_out.close()
    # Do we need to zap a temp CL file?
    if(delete_CL_in_flag):
        aips_data.zap_table('AIPS CL', CL_in)
    return


            

    
    
################################################################################
def get_subarray_scan_times(aips_data, subarray):
    """get a list of [scan_id, start_time, end_time] for all subarray scans

INPUTS:
aips_data    I  The AIPSUVData object to investigate
subarray     I  subarray number to check

OUTPUTS: scan_times
scan_times   O  A list of [scan_id, start_time, end_time] lists for all
                scans for the subarray.  The times are in AIPS days since the
                reference date

    """    
    assert(jma_aips.check_table_version_exists(aips_data, 'AIPS NX', 0))
    nx_table = aips_data.table('AIPS NX', 0)
    scan_times = []
    count = -1
    for row in nx_table:
        count = count+1
        if(row.subarray == subarray):
            start_time = row.time - 0.5 * row.time_interval
            end_time = row.time + 0.5 * row.time_interval
            scan_times.append([copy.copy(count), start_time, end_time])
    return scan_times





################################################################################
def find_SN_phase_data(SN_table, NUM_IF, NUM_POL,
                       subarray, antenna_number,
                       start_time, end_time, late):
    phase1 = [None]*NUM_IF
    phase2 = [None]*NUM_IF
    found_flag = 0
    for row in SN_table:
        if((row.subarray != subarray) or (row.antenna_no != antenna_number)):
            continue
        t = row.time
        if((t >= start_time) and (t <= end_time)):
            # found a solution in this scan.  Check for valid data
            for i in xrange(NUM_IF):
                if(row.weight_1[i] > 0.0):
                    if(phase1[i] is not None):
                        if(late):
                            if(phase1[i][0] < t):
                                phase1[i] =[t,row.real1[i], row.imag1[i]]
                        else:
                            if(phase1[i][0] > t):
                                phase1[i] =[t,row.real1[i], row.imag1[i]]
                    else:
                        found_flag = 1
                        phase1[i] =[t,row.real1[i], row.imag1[i]]
                if((NUM_POL > 1) and (row.weight_2[i] > 0.0)):
                    if(phase2[i] is not None):
                        if(late):
                            if(phase2[i][0] < t):
                                phase2[i] =[t,row.real2[i], row.imag2[i]]
                        else:
                            if(phase2[i][0] > t):
                                phase2[i] =[t,row.real2[i], row.imag2[i]]
                    else:
                        found_flag = 2
                        phase2[i] =[t,row.real2[i], row.imag2[i]]
    phase = [phase1, phase2]
    return found_flag, phase



################################################################################
def compare_SN_phase_data(NUM_IF, NUM_POL, e_phase, l_phase):
    """Compare SN results to check for average phase differences

OUTPUTS: mean_phase
mean_phase      O  The mean phase difference, in radians.  If None, then
                   the SN results do not have values for the same IF/Polarization
                   combinations
    """
    summ = 0.0+0.0j
    sum_sqr = 0.0+0.0j
    count = 0
    for p in xrange(NUM_POL):
        for i in xrange(NUM_IF):
            if((e_phase[p][i] is None) or (l_phase[p][i] is None)):
                continue
            ze = complex(e_phase[p][i][1],e_phase[p][i][2])
            zl = complex(l_phase[p][i][1],l_phase[p][i][2])
            try:
                # difference in phase is division in complex numbers
                z = ze/zl
                # scale to unit vector
                z /= abs(z)
                summ += z
                sum_sqr += z*z
                count += 1
            except ZeroDivisionError:
                pass
    if(count == 0):
        return None
    average = summ / count
    mean_phase = math.atan2(average.imag,average.real)
    print "Mean phase difference in degrees is %.2f"%( mean_phase*180.0/math.pi)
    if(count > 1):
        var = (sum_sqr - summ*summ/count)/ (count-1)
        std_dev = math.sqrt(abs(var))
        print "Standard deviation in degrees is %.2f"%(std_dev*180.0/math.pi)
    return mean_phase
        






################################################################################
def locate_180_phase_jumps_in_SN(aips_data,
                                 SN,
                                 subarray,
                                 antenna_number
                                 ):
    """Search for 180 degree phase jumps in data from an SN table

INPUTS:
aips_data    I  The AIPSUVData object to investigate
SN           I  The SN table version to investigate
subarray     I  subarray number to correct
antenna_number I  antenna number in the subarray to correct

OUTPUTS:
time_list    O  A list of times as doubles in days since the reference date of
                the start of each scan.  This array must be sorted in increasing
                time order.  This list gives the times at which the new phase
                starts.
    """
    assert(jma_aips.check_table_version_exists(aips_data, 'AIPS SN', SN))
    w = Wizardry.AIPSData.AIPSUVData(aips_data)
    SN_table = w.table('AIPS SN',SN)
    NUM_IF = SN_table.keywords['NO_IF']
    NUM_POL = SN_table.keywords['NO_POL']
    scan_times = get_subarray_scan_times(aips_data, subarray)
    if(len(scan_times) < 2):
        # 1 or fewer scans means there cannot be a phase jump
        return None
    time_list = []
    early_scan = 0
    found_flag = 0
    while(early_scan < len(scan_times) -1):
        start_time = scan_times[early_scan][1]
        end_time = scan_times[early_scan][2]
        found_flag, early_phase = \
                    find_SN_phase_data(SN_table, NUM_IF, NUM_POL,
                                       subarray, antenna_number,
                                       start_time, end_time, 1)
        if(found_flag):
            break
        early_scan = early_scan + 1
    if(found_flag == 0):
        raise RuntimeError("No data in SN table to match")
    late_scan = early_scan
    while(late_scan < len(scan_times) -1):
        # check the next scan
        late_scan = late_scan+1
        start_time = scan_times[late_scan][1]
        end_time = scan_times[late_scan][2]
        found_flag, late_phase = \
                    find_SN_phase_data(SN_table, NUM_IF, NUM_POL,
                                       subarray, antenna_number,
                                       start_time, end_time, 0)
        if(found_flag):
            # Hey, found the next set of data
            print "Scan %3d"%late_scan
            mean_phase = compare_SN_phase_data(NUM_IF, NUM_POL,
                                               early_phase, late_phase)
            if(mean_phase is not None):
                if(math.fabs(mean_phase) > math.pi*0.5):
                    print "found phase difference at scan ", late_scan
                    time_list.append(start_time)
                # now reget the early phase SN values for this scan
                found_flag, early_phase = \
                            find_SN_phase_data(SN_table, NUM_IF, NUM_POL,
                                               subarray, antenna_number,
                                               start_time, end_time, 1)
            else:
                # cannot compare this scan with the previous scan.
                # Oh well.  Skip it and hope for the best
                pass
    if(len(time_list) == 0):
        return None
    return time_list
            
            
        
                            
        
################################################################################
def correct_180_phase_jumps(aips_data,
                            antenna,
                            refant,
                            CL_use,
                            CL_in,
                            CL_out,
                            SN_out,
                            solint=120,
                            source_list = None,
                            subarray=1,
                            overwrite=0
                            ):
    """Fix a dataset for 180 degree phase jumps introduced at random scan starts

Some telescopes (Onsala) sometimes introduce 180 degree phase jumps in the
instrument (LO) phase at the beginning of scans.  This process appears to be
approximately random as to which scans this appears in.  This function will
try to figure out wheresuch phase jumps are located and correct for them.

This is intended to help with phase referencing, but it should also help
to a small extent with bright objects too.  However, for phase referencing,
if the target object is weak, there is no way to tell whether a phase jump
has happened at the start of the target scan.  :(

The idea is that you should have the data reasonably calibrated and flagged.
For VLBI data, hopefully you have corrected for the major delay and rate terms
using bright FRINGE finder scans (but at this point you should NOT have
applied any phase corrections from your FRING runs).  Ionospheric calibration
should also have been applied.  Hopefully, your sources are bright and reasonably
compact, or at least have structure which does not change rapidly with time.
This routine will run a simple point-source model for all supplied sources
with CALIB to determine instrumental phases.  This is done using a solint of
solint.  Because this is expected only for L Band Onsala data at the moment, a
default of 120 seconds solint has been chosen.  The resulting SN table is
examined to search for phase jumps in the supplied antenna.

INPUTS:
aips_data    I  The AIPSUVData object to investigate
antenna      I  antenna in the subarray to correct.  This may either be the
                antenna number or the name of the antenna
refant       I  A reference antenna to use for CALIB.  This must not be the
                same as antenna!  You may give the name or the number
CL_use       I  CL table to use for gain calibration in order to search for
                phase jumps.  May be different from CL_in.
                If 0, use the highest.
CL_in        I  Number of CL table to use to start to make the next CL table
                with the calibrated jumps.  If 0, use the highest.
CL_out       I  Number of CL table to write out to (must not exist if overwrite
                is false!)  If 0, use the highest +1
SN_out       I  Number of SN table to create to search for phase jumps
solint       I  Solution interval to use for CALIB run, in seconds
source_list  I  Source list to use for CALIB run.  Defaults to None, which
                means to use all sources.  If you select a subset of sources,
                then only those sources will be checked and corrected.  This
                could be potentially useful if a target source you wish to
                fix has lots of structure
subarray     I  subarray number to correct
overwrite    I  May the CL_out table be overwritten?

OUTPUTS: None
    """
    # Check antennas
    antenna_number = antenna
    if(type(antenna) != type(5)):
        antenna_number = jma_aips.get_antenna_number_subarray(aips_data, antenna,
                                                              subarray)
    if(type(refant) != type(5)):
        refant = jma_aips.get_antenna_number_subarray(aips_data, refant,subarray)
    if(refant == antenna_number):
        warnings.warn("Hey, don't use same antenna for antenna and refant here.\nTold to use %d"%refant)
        if(refant <= 1):
            refant = 2
        else:
            refant -= 1
    # Check CL tables
    if(CL_out == 0):
        CL_out = aips_data.table_highver('AIPS CL') + 1
    if(CL_in == 0):
        CL_in = aips_data.table_highver('AIPS CL')
    if(CL_use == 0):
        CL_use = aips_data.table_highver('AIPS CL')
    assert(jma_aips.check_table_version_exists(aips_data, 'AIPS CL', CL_in))
    assert(jma_aips.check_table_version_exists(aips_data, 'AIPS CL', CL_use))
    # Make sure the SN table spot is free
    if(SN_out == 0):
        SN_out = aips_data.table_highver('AIPS SN') + 1
    jma_aips.verify_table_version_free(aips_data, 'AIPS SN', SN_out, overwrite)
    # Now, run CALIB
    jma_aips.run_calib(aips_data, refant, solint, 0,
                       sources_use = source_list,
                       gainuse=CL_use,
                       aparm=[None,3,0,0,0,0,0,2.5],
                       soltype='L1',
                       solmode='P!A',
                       snver=SN_out)
    
    # Now get the list of times
    time_list = locate_180_phase_jumps_in_SN(aips_data, SN_out,
                                             subarray, antenna_number)
    # Now fix the problem
    if(time_list):
        fix_scan_180_degree_phase_jumps(aips_data,
                                        CL_in,
                                        CL_out,
                                        subarray,
                                        antenna_number,
                                        time_list,
                                        overwrite=0
                                        )
    elif(CL_in != CL_out):
        # There is nothing to fix.  But to keep things sane, just copy
        # over the CL table
        jma_aips.run_tacop(aips_data, 'CL', CL_in, 1, aips_data, CL_out)
    return

