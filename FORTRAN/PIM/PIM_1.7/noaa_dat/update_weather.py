#!/usr/bin/env python3
import argparse
import os
from datetime import timezone
import datetime
import shutil
import urllib.request
from glob import glob
import re
from calendar import monthrange
import numpy as np

def parse_kpf(lines):
    """
    -------------------------------------------------------------------------------
    COLUMNS   FMT   DESCRIPTION
    -------------------------------------------------------------------------------
    1- 2     I2    YEAR
    3- 4     I2    MONTH
    5- 6     I2    DAY

    7-10     I4    BARTELS SOLAR ROTATION NUMBER--a sequence of 27-day intervals
                    counted continuously from February 8, 1832.
    11-12     I2    NUMBER OF DAY within the Bartels 27-day cycle.

    13-14     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 0000 - 0300 UT.
    15-16     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 0300 - 0600 UT.
    17-18     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 0600 - 0900 UT.
    19-20     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 0900 - 1200 UT.
    21-22     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 1200 - 1500 UT.
    23-24     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 1500 - 1800 UT.
    25-26     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 1800 - 2100 UT.
    27-28     I2    Kp or PLANETARY 3-HOUR RANGE INDEX for 2100 - 2400 UT.
    29-31     I3    SUM of the eight Kp indices for the day expressed to the near-
                    est third of a unit.

    32-34     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 0000 - 0300 UT.
    35-37     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 0300 - 0600 UT.
    38-40     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 0600 - 0900 UT.
    41-43     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 0900 - 1200 UT.
    44-46     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 1200 - 1500 UT.
    47-49     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 1500 - 1800 UT.
    50-52     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 1800 - 2100 UT.
    53-55     I3    ap or PLANETARY EQUIVALENT AMPLITUDE for 2100 - 2400 UT.
    56-58     I3    Ap or PLANETARY EQUIVALENT DAILY AMPLITUDE--the arithmetic mean
                    of the day's eight ap values.

    59-61     F3.1  Cp or PLANETARY DAILY CHARACTER FIGURE--a qualitative estimate
                    of overall level of magnetic activity for the day determined
                    from the sum of the eight ap amplitudes.  Cp ranges, in steps
                    of one-tenth, from 0 (quiet) to 2.5 (highly disturbed).

    62-62     I1    C9--a conversion of the 0-to-2.5 range of the Cp index to one
                    digit between 0 and 9.

    63-65     I3    INTERNATIONAL SUNSPOT NUMBER.  Records contain the Zurich num-
                    ber through December 31, 1980, and the International Brus-
                    sels number thereafter.

    66-70     F5.1  OTTAWA 10.7-CM SOLAR RADIO FLUX ADJUSTED TO 1 AU--measured at
                    1700 UT daily and expressed in units of 10 to the -22 Watts/
                    meter sq/hertz.  Observations began on February 14, 1947. 
                    From that date through December 31, 1973, the fluxes given
                    here don't reflect the revisions Ottawa made in 1966. NOTE: 
                    If a solar radio burst is in progress during the observation
                    the pre-noon or afternoon value is used (as indicated by a
                    flux qualifier value of 1 in column 71.

    71-71     I1    FLUX QUALIFIER.  "0" indicates flux required no adjustment; 
                    "1" indicates flux required adjustment for burst in progress
                    at time of measurement; "2" indicates a flux approximated by
                    either interpolation or extrapolation; and "3" indicates no
                    observation.
    -------------------------------------------------------------------------------
    """
    dicokpf = []
    for il, l in enumerate(map(lambda l: l.replace('\n',''), lines)):
        needfixing=False
        errorstr = f"Current KPF file fails to verify at line {il} due to"
        def fslice(start, end, step=1):
            ''' fortran indexing plus inclusive end for easier reading '''
            return slice(start-1, end, step)
        try:
            if len(l) > 71: raise ValueError("Line exceeds 71 character columns")
            year = int(l[fslice(1,2)].strip())
            month = int(l[fslice(3,4)].strip())
            day = int(l[fslice(5,6)].strip())
            bartelsrot = l[fslice(7,10)]
            bartelsday = l[fslice(11,12)]
            utindx1 = int(l[fslice(13,14)].strip())
            utindx2 = int(l[fslice(15,16)].strip())
            utindx3 = int(l[fslice(17,18)].strip())
            utindx4 = int(l[fslice(19,20)].strip())
            utindx5 = int(l[fslice(21,22)].strip())
            utindx6 = int(l[fslice(23,24)].strip())
            utindx7 = int(l[fslice(25,26)].strip())
            utindx8 = int(l[fslice(27,28)].strip())
            kpsum =   int(l[fslice(29,31)].strip())
            apindx1 = int(l[fslice(32,34)].strip())
            apindx2 = int(l[fslice(35,37)].strip())
            apindx3 = int(l[fslice(38,40)].strip())
            apindx4 = int(l[fslice(41,43)].strip())
            apindx5 = int(l[fslice(44,46)].strip())
            apindx6 = int(l[fslice(47,49)].strip())
            apindx7 = int(l[fslice(50,52)].strip())
            apindx8 = int(l[fslice(53,55)].strip())
            apmean = int(l[fslice(56,58)].strip())
            cpfig = float(l[fslice(59,61)].strip())
            c9a = int(l[fslice(62,62)])
            # some records missing sunsport numbers
            if len(l) < 65:
                isn = None
                f107 = 0
                fqual = 3
                needfixing=True
            else:
                try:
                    isn = int(l[fslice(63,65)].strip())
                except ValueError:
                    needfixing=True
                    isn = None # some are missing
            # some records missing f107 and qualifier numbers
            if len(l) < 71:
                f107 = 0
                fqual = 3
                needfixing=True
            else:
                try:
                    f107 = float(l[fslice(66,70)].strip())
                    try:
                        fqual = int(l[fslice(71,71)].strip())
                    except ValueError:
                        needfixing=True
                        fqual = 3 # no observation
                except ValueError:
                    needfixing=True
                    f107 = 0
                    fqual = 3
        except ValueError as e:
            raise ValueError(f"{errorstr} invalid value encountered {str(e)}")
        if not(year >= 0 and year <= 99):
            raise ValueError(f"{errorstr} Invalid year code")
        if year > 80:
            cc = 1900
        else:
            cc = 2000
        if not(month >= 1 and month <= 12):
            raise ValueError(f"{errorstr} Invalid month code")
        if not(day >= 1 and day <= monthrange(year+cc, month)[1]):
            raise ValueError(f"{errorstr} Invalid day code")
        f107unix = datetime.datetime(year+cc,month,day,
                                     17,0,0,tzinfo=timezone.utc).timestamp()
        if not(fqual) in [0,1,2,3]:
            raise ValueError("Invalid flux qualifier")
        if not (cpfig >= 0 and cpfig <= 2.5):
            raise ValueError("Invalid cpfig")
        dicokpf.append({
            'year': year,
            'month': month,
            'day': day,
            'bartelsrot': bartelsrot,
            'bartelsday': bartelsday,
            'utindx1': utindx1,
            'utindx2': utindx2,
            'utindx3': utindx3,
            'utindx4': utindx4,
            'utindx5': utindx5,
            'utindx6': utindx6,
            'utindx7': utindx7,
            'utindx8': utindx8,
            'kpsum': kpsum,
            'apindx1': apindx1,
            'apindx2': apindx2,
            'apindx3': apindx3,
            'apindx4': apindx4,
            'apindx5': apindx5,
            'apindx6': apindx6,
            'apindx7': apindx7,
            'apindx8': apindx8,
            'apmean': apmean,
            'cpfig': cpfig,
            'c9a': c9a,
            'isn': isn,
            'f107': f107,
            'fqual': fqual,
            'original': l.replace("\n", ""),
            'needfixing': needfixing,
            'f107unix': f107unix
        })
    return dicokpf

def reconstitute_kpf(dicokpf, only_neadfixing=True):
    lines = []
    for kpf in dicokpf:
        if kpf.get("needfixing", False):
            fmt = [
                str(kpf['year']).rjust(2),
                str(kpf['month']).rjust(2),
                str(kpf['day']).rjust(2),
                str(kpf['bartelsrot']).rjust(3),
                str(kpf['bartelsday']).rjust(2),
                str(kpf['utindx1']).rjust(2),
                str(kpf['utindx2']).rjust(2),
                str(kpf['utindx3']).rjust(2),
                str(kpf['utindx4']).rjust(2),
                str(kpf['utindx5']).rjust(2),
                str(kpf['utindx6']).rjust(2),
                str(kpf['utindx7']).rjust(2),
                str(kpf['utindx8']).rjust(2),
                str(kpf['kpsum']).rjust(3),
                str(kpf['apindx1']).rjust(3),
                str(kpf['apindx2']).rjust(3),
                str(kpf['apindx3']).rjust(3),
                str(kpf['apindx4']).rjust(3),
                str(kpf['apindx5']).rjust(3),
                str(kpf['apindx6']).rjust(3),
                str(kpf['apindx7']).rjust(3),
                str(kpf['apindx8']).rjust(3),
                str(kpf['apmean']).rjust(3),
                f"{kpf['cpfig']:.1f}".rjust(3)[:4],
                str(kpf['c9a']),
                str(kpf['isn']).rjust(3) if kpf['isn'] is not None else "---",
                f"{kpf['f107'] if kpf['f107'] is not None else 0.0:.1f}".rjust(5)[:5],
                str(kpf['fqual'])
            ]
            lines.append("".join(fmt))
        else:
            lines.append(kpf["original"]) # to make diff more manageable since some lines fill blanks with zeros others doesnt etc.
        assert len(lines[-1]) == 71
    return lines

def merge_dicos(old, new):
    """ Merge dates plus return list of inserted values"""
    def __fmtkpfday(y, m, d): 
        return f'{y}/{m}/{d}'
    # warning should probably not use this script past 2080 for because the PIM format will start overlapping in century!
    havedates = list(map(lambda n: __fmtkpfday(n['year'],n['month'],n['day']), old))
    notexist = list(filter(lambda n: __fmtkpfday(n['year'],n['month'],n['day']) not in havedates,
                           new))
    merged = old + notexist
    merged_sorted = sorted(merged, key=lambda n: n['f107unix'])
    return merged_sorted, notexist

def process_otowa(otowalines):
    otowalines = filter(lambda l: len(l) > 0, otowalines)
    otowalines = map(lambda l: l.strip(), otowalines)
    foundhdr = False
    __compulsory_hdr = ['fluxdate', 'fluxtime', 'fluxadjflux']
    vels = {}
    headervals = []
    for il, l in enumerate(otowalines):
        errstr = f"Error parsing Otowa datafile near line {il}."
        l = l.split("#")[0]
        if re.match(r'(?:[\-]+[\W]+)+', l): continue
        if not foundhdr:
            headervals = re.findall(r"[a-zA-Z0-9]+", l)
            if all(map(lambda c: c in headervals, __compulsory_hdr)):
                foundhdr = True
        else:
            lnvels = re.findall(r"[0-9.]+", l)
            if len(lnvels) != len(headervals):
                raise RuntimeError(f"{errstr} Column number mismatch")
            for c, v in zip(headervals, lnvels):
                if c == 'fluxdate':
                    if len(v) != 8:
                        ValueError(f"{errstr} Year not YYYYMMDD format")
                    try:
                        year = int(v[0:4])
                        month = int(v[4:6])
                        day = int(v[6:])
                    except ValueError as e:
                        raise ValueError(f"{errstr} Invalid date encountered")
                    if not(month >= 1 and month <= 12):
                        raise ValueError(f"{errstr} Invalid month")
                    if not(day >= 1 and day <= monthrange(year, month)[1]):
                        raise ValueError(f"{errstr} Invalid day")
                    v = (year, month, day)
                if c == 'fluxtime':
                    if len(v) != 8:
                        ValueError(f"{errstr} Time not in hhmmss format")
                    try:
                        hr = int(v[0:2])
                        mm = int(v[2:4])
                        ss = int(v[4:])
                    except ValueError as e:
                        raise ValueError(f"{errstr} Invalid time encountered")
                    if not(hr >= 0 and hr <= 23):
                        raise ValueError(f"{errstr} Invalid hour")
                    if not(mm >= 0 and mm <= 59):
                        raise ValueError(f"{errstr} Invalid minute")
                    if not(ss >= 0 and ss <= 59):
                        raise ValueError(f"{errstr} Invalid second")
                    v = (hr, mm, ss)
                if c == 'fluxadjflux':
                    try:
                        v = float(v)
                    except ValueError as e:
                        raise ValueError(f"{errstr} Invalid flux value encountered")
                vels.setdefault(c, []).append(v)
    if not foundhdr:
        raise RuntimeError(f"No header information found in Otowa data")
    for d, t in zip(vels['fluxdate'], vels['fluxtime']):
        dt = datetime.datetime(d[0], d[1], d[2],
                               t[0], t[1], t[2], tzinfo=timezone.utc)
        vels.setdefault('utctimestamp', []).append(dt.timestamp())
    return vels


parser = argparse.ArgumentParser(description="Update NOAA weather",
                                formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("startyear", type=int, help="Provide starting year for lookup")
parser.add_argument("endyear", type=int, help="Provide ending year for lookup")
parser.add_argument("--potsdam", type=str, help="URL for Potsdam yearly", default="ftp://ftp.gfz-potsdam.de/pub/home/obs/kp-ap/wdc/yearly/")
parser.add_argument("--otowa", type=str, help="URL for Otowa F10.7 numbers", default="ftp://ftp.seismo.nrcan.gc.ca/spaceweather/solar_flux/daily_flux_values/fluxtable.txt")
parser.add_argument("--kpffile", type=str, help="kpf107 file to append with new data (will not overwrite file)", default="kpf107.dat")
parser.add_argument("--cachedir", type=str, help="Cache directory for downloaded files", default=".noaacache") # make sure this is in gitignore and dockerignore...
parser.add_argument("--clearcache", help="Remove cached files before running", action='store_true', default=False) # make sure this is in gitignore and dockerignore...
args = parser.parse_args()

if args.startyear > 2080:
    raise ValueError("PIM format does not support year 2081 and beyond")
if args.startyear < 2005:
    raise ValueError("This script only accepts starting years from 2005 onwards (Otowa don't have historic records beyond this)")
if args.startyear > args.endyear:
    raise ValueError("Start year exceeds end year")
newkpffile = False
if not(os.path.exists(args.kpffile) and os.path.isfile(args.kpffile)):
    newkpffile = True
    raise ValueError(f"WARNING: KPF file '{args.kpffile}' does not exist! Attempting to constitute one now")
if not(os.path.exists(args.cachedir) and os.path.isdir(args.cachedir)):
    os.mkdir(args.cachedir)
elif args.clearcache:
    shutil.rmtree(args.cachedir)
    os.mkdir(args.cachedir)

dt = datetime.datetime.now(timezone.utc)
utc_time = dt.replace(tzinfo=timezone.utc)
utc_timestamp = utc_time.timestamp()
outputkpffile = args.kpffile + "." + str(utc_timestamp) if not newkpffile else args.kpffile

current_records = []
if not newkpffile:
    with open(args.kpffile) as oldkpf:
        current_kpffile = oldkpf.readlines()
    current_records = parse_kpf(current_kpffile)
print(f"Processed {len(current_records)} lines from current KPF file")

merged_kpffile = []
for year in range(args.startyear, args.endyear+1):
    print(f"Processing year {year}...")
    # Contact Potsdam for data in kpf107.fmt specification
    # if needed
    fn = f'kp{year:d}.wdc'
    outdatafile = os.path.join(args.cachedir, fn)
    if not (os.path.exists(outdatafile) and os.path.isfile(outdatafile)):
        requrl = args.potsdam + f'/{fn}'
        print(f"\tCalling Potsdam at '{requrl}'...")   
        urllib.request.urlretrieve(requrl, outdatafile)
    else:
        print(f"\tUsing cached file at '{outdatafile}'")
    with open(outdatafile) as updatedata:
        new_records = parse_kpf(updatedata.readlines())
    merged_kpffile, new_insertions = merge_dicos(current_records, new_records)
    print(f"\tProcessed {len(new_records)} lines from KPF file '{outdatafile}'. "
          f"{len(new_insertions)} new entries added")

# since 2018 potsdam has not propagated f10.7 cm flux values. We need to call our Canadian friends to get them
needsf107 = list(filter(lambda kpf: kpf['fqual'] == 3, # i.e. missing
                        merged_kpffile))
if len(needsf107) > 0:
    print(f"We detect {len(needsf107)} KPF records need 10.7cm solar flux values. Fetching them from Otowa...")
    f107cachefiles = glob(os.path.join(args.cachedir, 'f107.*.dat'))
    closest_match = ""
    closest_match_time = -1
    for f in f107cachefiles:
        cfn = os.path.split(f)[1]
        cfn_date = float(cfn.replace('f107.', '').replace('.dat', ''))
        if closest_match_time == -1 or abs(cfn_date - utc_timestamp) < closest_match_time:
            closest_match_time = cfn_date
            closest_match = f
    if closest_match:
        oldest_f107 = abs(closest_match_time - utc_timestamp)
        print(f"Nearest available Otowa data in cache is {abs(closest_match_time - utc_timestamp):.0f} seconds old.")
         # these files are huge and the sun is quite stable over long periods so don't do this too often!
        if oldest_f107 > 3600 * 24 * 7:
            print(f"Otowa data may be stale... will redownload")
            closest_match = ""
        else:
            print("Using Otowa data from cached values")
    if not closest_match:
        newfile = os.path.join(args.cachedir, f'f107.{utc_timestamp}.dat')
        print(f"Calling Otowa at '{args.otowa}'...")
        urllib.request.urlretrieve(args.otowa, newfile)
        closest_match = newfile
    with open(closest_match) as otowa:
        otowalines = otowa.readlines()
        otowa_db = process_otowa(otowalines)
    #NN interp 10000Jy fluxscale onto the kpf database using nearest neighbour
    for kpf in needsf107:
        tdiff = abs(kpf['f107unix'] - np.array(otowa_db['utctimestamp']))
        nni = np.argmin(tdiff)
        kpf['fqual'] = 2 # interpolated / extrapolated
        kpf['f107'] = otowa_db['fluxadjflux'][nni]
        if tdiff[nni] > 3600*24*31*6:
            print(f"Warning: Data record at {kpf['year']}/{kpf['month']}/{kpf['day']} will have interpolated "
                  f"flux more than {tdiff[nni]}s old!. Cannot take. Invalidating!")
            kpf['f107'] = 30.
            kpf['fqual'] = 3
        elif tdiff[nni] > 3600*24:
            print(f"Warning: Data record at {kpf['year']}/{kpf['month']}/{kpf['day']} will have interpolated "
                  f"flux more than one day old")
        
print(f"New KPF file '{outputkpffile}' will have {len(merged_kpffile)} entries "
      f"compared to {len(current_records)} previous entries")
reconstituted = reconstitute_kpf(merged_kpffile)
with open(outputkpffile, 'w+') as outf:
    for li, l in enumerate(reconstituted):
       outf.write(f"\n{l}" if li > 0 else l)
