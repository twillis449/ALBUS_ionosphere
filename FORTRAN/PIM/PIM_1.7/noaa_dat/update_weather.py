#!/usr/bin/env python3
import argparse
import os
from datetime import timezone
import datetime
import shutil
import urllib.request

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
        if not(month >= 1 and month <= 12):
            raise ValueError(f"{errorstr} Invalid month code")
        if not(day >= 1 and day <= 31): #ambiguous century
            raise ValueError(f"{errorstr} Invalid day code")
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
            'needfixing': needfixing
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
        try:
            assert len(lines[-1]) == 71
        except:
            import ipdb; ipdb.set_trace()
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
    merged_sorted = sorted(merged, key=lambda n: __fmtkpfday(n['year'],n['month'],n['day']))
    return merged_sorted, notexist

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

print(f"New KPF file '{outputkpffile}' will have {len(merged_kpffile)} entries "
      f"compared to {len(current_records)} previous entries")
reconstituted = reconstitute_kpf(merged_kpffile)
with open(outputkpffile, 'w+') as outf:
    for li, l in enumerate(reconstituted):
       outf.write(f"\n{l}" if li > 0 else l)
