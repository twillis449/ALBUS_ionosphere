C  -------------------------------------------------------------
C
C  PIMVLBI4 uses PIM (currently v1.7) to calculate the modelled 
C  ionospheric/plasmaspheric TEC along the line-of-sight to a source.  
C  The principal modifications to PIM include:
C
C   a) GETDAT has been removed:  normal PIM reads its inputs from a
C      user-prepared input file, good for one UT.  Now, all the
C      parameters/variables PIM needs are extracted automatically from:
C        VLBI export files 
C        geophysical data files previously downloaded from NASA/NOAA
C        station/source databases prepared separately by the user
C      These "GETDAT"-functions have been moved to separate subroutines
C      to avoid making major modifications to the existing PIM COMMON-
C      block structure.
C
C  b) SET_UP has been modified to get paths for the 2 geophysical
C     parameter datafiles and the Source & Station catalogues.
C     These are now included in the PATH_NAM_VLBI.TXT that must
C     be in the working directory (& modified to match one's specific
C     directory structure as in the PIM instructions)
C
C  c) Calls to the actual PIM database-reading & TEC-calculating
C     subroutines put inside the appropriate Epoch-Station-Source loops
C
C  d) For VLBI purposes, only interested in the TEC along a specific
C     Az/El; therefore the OUTPUT routine writes only UT, station,
C     source, and the corresponding TEC at 0/+1s (to allow separate
C     calculation of the ionospheric delay-rate contribution).
C     Reconfiguration into baseline-based delays/rates is anticipated
C     to be done separately
C
C  e) Add routines from GSFC SHELLIG --> IGRF.F to calculate the
C     terrestrial magnetic field vector & hence allow cacluclation
C     of Faraday rotation along LoS by integrating n_e*B_\parallel
C
C  Additional control options added (over those in PIMVLBI2/3) for the
C  method of treating DR, Faraday rotation, available geophysical
C  parameters, available GPS/ISS real-time data.  
C  The main program has been more fully subroutinized, and added
C  speed has been sought via minimized file access in loops.
CC
C  We have still made only the bare minimum number of changes to
C  actual PIM code (*.f & *.inc), as described in the document 
C  PIMVLBI4SOP.PS in a tar file on our FTP site:
C      ftp ftp.nfra.nl
C      cd ~/pub/rmc/
C        pim+gps.tar.gz
C
C-------------------
C  BLOCK DATA structure remains the same as in PIM, but included here
C  as an include file to keep the length down
C
      INCLUDE 'pim.blckdt'


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*
C
      PROGRAM pimvlbi4

C	Program to drive PIM via subroutine calls (vice here-files) via
C	data-reads from export files & geophys-params files, outputting
C	the slant TEC to all sources from each station

C---------------
C  Include files used in PIM & GETDAT
C
      INCLUDE 'dpath.inc'
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'logicuni.inc'
      INCLUDE 'ssnstuff.inc'
C     INCLUDE 'lower.inc'
C  LOWER.INC commented out & SUBROUTINE SETSCT added to avoid confusion
C  between the variable NALT that exists in LOWER.INC as a 2-D array
C  and the NALT herein which specifies the number of altitude points

C-----------------
C  Declarations
C
      EXTERNAL ap_to_kp, ap_to_kptab
      REAL ap_to_kp, ap_to_kptab
      REAL*8 pi, degrad, nul,een,twee
      REAL umr
      INTEGER maxut,maxsta,maxsrc,maxdays, maxgps,maxiss
      PARAMETER (maxut=800, maxsta=20, maxsrc=10, maxdays=14)
      PARAMETER (maxgps=13, maxiss=12)
      PARAMETER (pi = 3.14159265358979323846d0)
      PARAMETER (degrad = pi / 1.8d2)
      PARAMETER (nul=0.d0, een=1.d0, twee=2.d0)
      INTEGER ndays,nlli,nllo,nll, nkploop, llo,lli,lkp, mll,mkp 
      INTEGER grdtyp, outtyp, year,day,month, yrdyv(maxut), imed,iyear
      INTEGER pyr,pdy,phr,pmin, phr0,pmin0,  ndayyr,ngood, mexptype
      INTEGER nday81,n0day81,nleapd, mday,mhr
      INTEGER nmax, i,j,k, ii,ll,iap, llfsct, nut,nsta,nsrc, jsta,ksrc 
      INTEGER idom, imfna, ngps,niss, ldbg, kalt
      INTEGER ngpsrcvr,nissrcvr, istat,filerr
      REAL ut
      REAL kpmin,kpmax, kpsp(2)
cREAL by,bz
      REAL uthr, tec1, tec(maxsta,maxsrc,2,2) 
      REAL era, erequ,erpol, aquad,bquad, yearigrf, time, gh1(144)
      REAL ed(maxaltpt),rng(maxaltpt),edmax,rngmax, edc(maxaltpt)
      REAL s(maxgps), rho(maxiss), delta(maxiss)
      REAL sigs(maxgps), sigrho(maxiss), sigdelta(maxiss)
      REAL loslat(maxaltpt), loslon(maxaltpt)
      REAL rmi1,rmi(maxsta,maxsrc,2,2), tper
creal tec2,tec3
      REAL*8 utv(maxut),epv(maxut), utdp, delt(2), x,x0
      REAL*8 rav(maxsrc),decv(maxsrc), latv(maxsta),lonv(maxsta) 
      REAL*8 sf107(maxdays+2), sap((maxdays+2)*8) 
      REAL*8 sfmin,sfmax, apmin,apmax, fut,f107dp,ssndp, apdp(2)
      REAL*8 bydp,bysig,bzdp,bzsig, dimo
      REAL*8 azgps(maxgps),elgps(maxgps)
      REAL*8 aziss(maxiss),eliss(maxiss)
      REAL*8 gast, azv(maxsta,maxsrc,2),elv(maxsta,maxsrc,2), az,el
      REAL*8 tmp,psec,psec0
      CHARACTER stav(maxsta)*1, srcv(maxsrc)*8
      CHARACTER a1*1, a8*8, fdbg*32, fil1*12
      CHARACTER ans*1,ans3*3, smode*1, units*8
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 pkpf,pimf,psrc,psta,pigrf,prtd
      CHARACTER*256 outfile, ftmp, flgps,fliss
      CHARACTER rtdtxt*32
      LOGICAL leapyrflg,dbgflg, dofr, gpssimflg, dodbg
      LOGICAL dodrpim,dodrazel, dogps,doiss 
      LOGICAL exstgpsrtd(maxsta),exstissrtd(maxsta)
      LOGICAL*1 gooddat(maxut,maxsta,maxsrc)

      COMMON /igrfstuff/ umr, era, aquad, bquad, nmax, time, gh1
      COMMON /igrfstuff1/ fil1
      umr = SNGL(degrad)
      a1 = '1'
      a8 = '12345678'
c     DATA delt /-1.d0, 1.d0/
c      DATA delt /nul, 1.d1/
      DATA delt /nul, 6.d1/
      dodbg = .FALSE.
      gpssimflg = .FALSE.
C  First, those constant PIM parameters (throughout all epoch, source
C     station loops)
C    Most of the variables in the COMMON block GRID as definded in
C    BLOCK DATA ITERATS are okay as they are (since not used anyway)
C
      outtyp = 0
      grdtyp = 2
      usessn = 1
      ursisw = .FALSE.
      eswitch = .FALSE.
      geog = .TRUE.
      nalt = 100
      plasph = .TRUE.
      llfsct = 0
      CALL SETSCT(llfsct) 

C  The last element of ZOUT is R_{GPS}-R_{earth} to the closest km.  
C    This aids the incorporation of ionosonde real-time data.  See notes
C    in RTDAPPL  (tecvlbi4.f)
C
      DATA zout /90., 95., 100., 105., 110., 115., 120., 125., 130.,
     1  135., 140., 145., 150., 155., 160., 165., 170., 175., 180., 
     2  190., 200., 210., 220., 230., 240., 250., 260., 270., 280.,
     3  290., 300., 310., 320., 330., 340., 350., 360., 370., 380.,
     4  390., 400., 420., 440., 460., 480., 500., 520., 540., 560.,
     5  580., 600., 620., 640., 660., 680., 700., 720., 740., 770.,
     6  800., 850., 900., 950., 1000., 1100., 1200., 1300., 1400., 
     7  1500., 1600., 1700., 1800., 1900., 2000., 2100., 2200., 2300.,
     8  2400., 2500., 3000., 3500., 4000., 4500., 5000., 5500., 6000.,
     9  6500., 7000., 7500., 8000., 8500., 9000., 10000., 11000.,
     a  12000., 13000., 14000., 15000., 17500., 20182. /

C---------------------------------------------------------------
C
C  PKPF/PIMF    CHAR    paths of geophysical datafiles
C  PIGRF        CHAR    path for IGRF databases
C  PSTA/PSRC    CHAR    paths for station & source catalogues
C  PRTD         CHAR    path for real-time data (GPS/ISS)
C  OUTFILE      CHAR    path for output file 
C  YRDY		INT    	yyddd
C  MONTH	INT	month (1-12) 
C  MEXPTYPE     INT     the type of input file (export/VLBI3cards/..)
C  UTV		REAL	vector of UTs [hh.hhhhhh]
C  GOODDAT	LOGICAL	whether specific UT-STA-SRC combination 
C			    exists in export file
C  RAV		REAL*8  vector of source RAs      [rad]
C  DECV		REAL*8	vector of source DECs     [rad]
C  LATV		REAL*8  vector of station LATs    [rad]
C  LONV         REAL*8  vector of station LONGs   [rad]
C  NLLI/NLLO    INT     controls looping for DR estimates:
C                          PIM databses read NLLO times
C                          PIM called MAX(nllo,nlli) times w/diff. LoS
C  JYR          INT     4-digit year
C  JDY		INT     3-digit day-of-year
C  JD           REAL*8  Julian Date  (244.... .5)
C  UT24HR	REAL    UT [fraction of day] used in LAST calc
C  JDK          REAL*8  Julian Date of epoch 
C  EQEQNX	REAL*8  Equation of equinoxes
C  SIDT		REAL*8  Local apparent sidereal time
C  HA           REAL*8  Hour angle
C  ELV		REAL    vector(3-matrix) of elevations  [deg]
C  AZV		REAL    vector(3-matrix) of azimuths    [deg]
C  SF107        REAL*8  27-d avg F10.7 for all NDAYS of data, +/- 1day
C  SAP          REAL*8  24-hr avg Ap for all NDAYS of data, +/- 1day,
C                         sampled every 3 UT hrs
C  F107V	REAL	vector of 27-d avg F10.7 at each UT
C  SSNV		REAL    vector of SSN (calc from F10.7)
C  APV		REAL    vector of 24-hr avg Ap
C  KPV		REAL	vector of 24-hr avg Kp (from APV)
C  UTMID	REAL	mid-point UT of obs., for reading IMF By,Bz
C  YEARIGRF     REAL    year (as REAL) for IGRF subroutines
C  YEAR         INT     year (as INT) for PIM subroutines
C  ED           REAL    values of electron density along LoS
C  RNG          REAL    values of range along LoS
C  TEC          REAL    value of slant TEC 
C  RMI          REAL    Integrated rotation measure 

C-------------------------------
C  Now take care of data-file / logical-unit assignments by using the
C  regular PIM  SET_UP subroutine as modified to read the geophysical
C  datafile names
C
      CALL set_up_vlbi (lucgdb, pkpf, pimf, pigrf, psrc, psta, prtd)

      
C-----------------------------------------------------------------
C  Determine whether to run PIM driven by an acutual data file
C  or as a uniformly-sampled simulation [YTD] -- or as-yet un-documented
C  (on-line) option to read file containing GPS SVN Az/El separately
C
 5    WRITE (*,'(a,$)') 'Read a data file [f] or make a uniformly-sample
     1d simulation [s]? '
      READ (*,'(a1)') smode
      IF ((smode .NE. 'f') .AND. (smode .NE. 's') .AND. (smode .NE. 'g')
     1    .AND. (smode .NE. 'd'))   GOTO 5

      IF (smode .EQ. 'd') THEN
         dodbg = .TRUE.
 20      WRITE (*,'(a,$)') '  Input  output debug file:  '
         READ (*,'(a)') fdbg
         OPEN (69, FILE=fdbg, STATUS='new', ERR=25)
         GOTO 28
 25      WRITE (*,'(a)') 'File already exists'
         GOTO 20 
 28      CONTINUE
         GOTO 5
      END IF

      mexptype = -1
      IF (smode .EQ. 'f') THEN
         CALL read_export (maxut,maxsta,maxsrc, nut,nsta,nsrc,
     1                       yrdyv,utv,epv, stav,srcv,gooddat, mexptype)
      ELSE IF (smode .EQ. 's') THEN
         CALL read_simul (maxut,maxsta,maxsrc, nut,nsta,nsrc,
     1                       yrdyv,utv, stav,srcv,gooddat)
      ELSE
C  Read external file containing Time, Az, El for a GPS SVN 
C   This is started in READ_GPSSIM.F; but the changes in the loop
C   structure and dimensioning of AZV,ELV will affect this option,
C   so put off until later.
c     gpssimflg = .TRUE.
         STOP 'GPS-file reading option not yet modified'
      END IF

      ndayyr = 365
      leapyrflg = .FALSE.
      iyear = yrdyv(1)/1000
      IF (MOD(iyear,4) .EQ. 0) THEN
         ndayyr = 366
         leapyrflg = .TRUE.
      END IF

C	Now calculate the RA & Dec for each of the sources in SRCV ....
C          (skip if a GPS situation)
C
      IF (.NOT. gpssimflg) THEN
         CALL stasrc_lookup (maxsta,maxsrc, nsta,nsrc, stav,srcv,
     1                         mexptype, psrc,psta, rav,decv, latv,lonv)
      END IF


C  Query for including Faraday rotation
C    if so, load spherical-harmonic databases, using median time of UTV
C
      dofr = .FALSE.
 40   WRITE (*,'(/,a,$)') 'Include Faraday rotation  [y/n]? '
      READ (*,'(a1)') ans
      IF ((ans .NE. 'y') .AND. (ans .NE. 'n')) GOTO 40
      IF (ans .EQ. 'y') dofr = .TRUE.

      IF (dofr) THEN
         era = 6371.2
         erequ = 6378.16
         erpol = 6356.775
         aquad = erequ * erequ
         bquad = erpol * erpol
C             now get YEAR/DAY-->REAL as IGRF subroutines expect
         imed = (nut+1)/2
         year = yrdyv(imed)/1000
         day = yrdyv(imed) - year*1000
         IF (year .LT. 81) THEN
            year = year + 2000
         ELSE
            year = year + 1900
         END IF
         yearigrf = FLOAT(year) + FLOAT(day)/FLOAT(ndayyr)
         CALL feldcof (yearigrf, pigrf, dimo)
      END IF

C  Query for how to handle DR estimates
C
      dodrpim = .FALSE.
      dodrazel = .FALSE.
      nllo = 1
      nlli = 1
 50   WRITE (*,'(a,$)') 'Include Delay-Rate modelling [y/n]? '
      READ (*,'(a1)') ans
      IF ((ans .NE. 'y') .AND. (ans .NE. 'n')) GOTO 50
      IF (ans .EQ. 'y') THEN
 55      WRITE (*,'(a,$)') ' DR from diff. LoS or also diff. PIM DB-look
     1up times [los/pim]? '
         READ (*,'(a3)') ans3
         IF ((ans3 .NE. 'los') .AND. (ans3 .NE. 'pim')) GOTO 55
         IF (ans3 .EQ. 'pim') THEN
            nllo = 2
            dodrpim = .TRUE.
         ELSE
            nlli = 2
            dodrazel = .TRUE.
         END IF
      END IF
      nll = MAX0(nllo,nlli)

C  Query for how to handle Real-Time Data
C
 60   dogps = .FALSE.
      doiss = .FALSE.
      WRITE (*,'(/,a,$)') 'Any GPS, Ionosonde, Both, or No Real-Time Dat
     1a [g/i/b/n]? '
      READ (*,'(a1)') ans
      IF ((ans .NE. 'g') .AND. (ans .NE. 'i') .AND. (ans .NE. 'b') .AND.
     1    (ans .NE. 'n')) GOTO 60
      IF (ans .EQ. 'g') THEN
         dogps = .TRUE.
      ELSE IF (ans .EQ. 'i') THEN
         doiss = .TRUE.
      ELSE IF (ans .EQ. 'b') THEN
         dogps = .TRUE.
         doiss = .TRUE.
      END IF
      IF (dogps .OR. doiss) THEN
         WRITE (*,'(a,$)') '   Input RTD-file identifying text (incl. pa
     1th under PRTD):  '
         READ (*,'(a32)') rtdtxt
      END IF

C  Test for existence of various RTD files
C
      IF (dogps) THEN
         ngpsrcvr = 0
         DO 6010 j = 1, nsta
            CALL strcct (prtd, rtdtxt, 32, ftmp, istat)
            CALL strcct (ftmp, stav(j)//'.gps', 32, flgps, istat)
            OPEN (66, FILE=flgps, STATUS='old', IOSTAT=filerr)
            IF (filerr .EQ. 0) THEN
               ngpsrcvr = ngpsrcvr + 1
               exstgpsrtd(j) = .TRUE.
            ELSE
               IF (filerr .EQ. 1018) THEN
                  exstgpsrtd(j) = .FALSE.
               ELSE 
                  WRITE (*,*) 'Unanticpated file-error for GPS:',filerr
                  STOP
               END IF
            END IF
            CLOSE (66)
 6010    CONTINUE
         WRITE (*,6019) ngpsrcvr
 6019    FORMAT ('   There are ',i3,' GPS RTD files: ',$)
         DO 6020 j = 1, nsta
            IF (exstgpsrtd(j)) WRITE (*,6029) stav(j)
 6020    CONTINUE
 6029    FORMAT (a3,$)
         WRITE (*,'(/a,$)') '     Is this right [y/n/stop]? ' 
         READ (*,'(a1)') ans
         IF ((ans .EQ. 'S') .OR. (ans .EQ. 's')) STOP
         IF ((ans .NE. 'Y') .AND. (ans .NE. 'y')) GOTO 60
      END IF
                  

      IF (doiss) THEN
         nissrcvr = 0
         DO 6110 j = 1, nsta
            CALL strcct (prtd, rtdtxt, 32, ftmp, istat)
            CALL strcct (ftmp, stav(j)//'.iss', 32, fliss, istat)
            OPEN (67, FILE=fliss, STATUS='old', IOSTAT=filerr)
            IF (filerr .EQ. 0) THEN
               nissrcvr = nissrcvr + 1
               exstissrtd(j) = .TRUE.
            ELSE
               IF (filerr .EQ. 1018) THEN
                  exstissrtd(j) = .FALSE.
               ELSE 
                  WRITE (*,*) 'Unanticpated file-error for ISS:',filerr
                  STOP
               END IF
            END IF
            CLOSE (67)
 6110    CONTINUE
         WRITE (*,6119) ngpsrcvr
 6119    FORMAT ('   There are ',i3,' ISS RTD files: ',$)
         DO 6120 j = 1, nsta
            IF (exstissrtd(j)) WRITE (*,6029) stav(j)
 6120    CONTINUE
         WRITE (*,'(/a,$)') '     Is this right [y/n/stop]? ' 
         READ (*,'(a1)') ans
         IF ((ans .EQ. 'S') .OR. (ans .EQ. 's')) STOP
         IF ((ans .NE. 'Y') .AND. (ans .NE. 'y')) GOTO 60
      END IF


C  Query for output file
C
 70   WRITE (*,'(/,a,$)') 'Input name of output file:  '
      READ (*,'(a)') outfile
      OPEN (UNIT=lutext, FILE=outfile, STATUS='new', ERR=75)
      GOTO 90 
 75   WRITE (*,'(a)') 'File already exists'
      GOTO 70


C  Print expected duration of run (using the 12.42s per UT/Sta/Src 
C  found in tests on the Sun Ultra 1 on 1 Apr 98)
C
 90   ngood = 0
      DO 91 i=1,nut
         DO 92 j=1,nsta
            DO 93 k=1,nsrc
               IF (gooddat(i,j,k)) ngood = ngood + 1
 93         CONTINUE
 92      CONTINUE
 91   CONTINUE
      units = ' hr  ***'
      tper = 11.642 * FLOAT(ngood) / 3.6e3
C      tper = 6.02 * FLOAT(ngood) / 3.6e3
      IF (tper .LT. 1.0) THEN
         tper = tper * 6.e1
         units = ' min ***'
      END IF
      WRITE (*,99) tper, units
 99   FORMAT (/,'*** Expected run-time =',f6.2,a8)


C----------------------
C  All station/source coords loaded.
C  Open the Kp/Ap;F10.7 file here outside the NUT loop, and load
C    necessary elements from file to memory
C
      CALL read_kpf107 (pkpf,maxdays, yrdyv(1),yrdyv(nut), 
     1                    ndays,sf107,sap)

C  Loop through returned SF107, SAP to find max/min to print out as
C   diagnostics/warnings
C
      sfmin = 3.d2
      sfmax = -een
      DO 130 i = 1, (ndays+2)
         sfmin = DMIN1(sfmin, sf107(i))
         sfmax = DMAX1(sfmax, sf107(i))
 130  CONTINUE
      apmin = 5.d2
      apmax = -een
      DO 135 i = 1, ((ndays+2)*8)
         apmin = DMIN1(apmin, sap(i))
         apmax = DMAX1(apmax, sap(i))
 135  CONTINUE
      kpmin = ap_to_kp(SNGL(apmin))
      kpmax = ap_to_kp(SNGL(apmax))

      WRITE (*,138) sfmin,sfmax
 138  FORMAT (/,'Range of Solar F10.7 around obs = ',f7.2, ' to',f7.2)
      WRITE (*,139) kpmin,kpmax
 139  FORMAT ('Range of 3-hr Kp in 24-hrs around obs =',f4.1, ' to',
     1        f4.1)

C  Read IMF parameters from (GSFC OMNIWEB-generated) file
C
      CALL read_imf (pimf, yrdyv(1),yrdyv(nut), utv(1),utv(nut),
     1                bydp,bysig,bzdp,bzsig, imfna)

      IF (bydp .GE. nul) THEN
         by = 1.0
      ELSE 
         by = -1.0
      END IF
      IF ((bzdp + bzsig/2.d0) .LE. nul) THEN
         bz = -1.0
      ELSE
         bz = 1.0
         DO 160 i = 1, ((ndays+2)*8)
            sap(i) = 4.d0
 160     CONTINUE
      END IF
      nkploop = 1
      IF (imfna .EQ. 2) nkploop = 2
      IF (imfna .EQ. 1) bz = 1.0


C  Here we start the large outer loop over all the times YRDYV+UTV
C   represented in the data  (because of the possible depth of the
C   nested loops, shift to only 2-space nested indentation)
C
      DO 200 i = 1, nut

        year = yrdyv(i)/1000
        day = yrdyv(i) - year*1000
        IF (year .LT. 81) THEN
           year = year + 2000
        ELSE
           year = year + 1900
        END IF
        nday81 = (year-1981)*365 + day
        nleapd = (year-1981)/4
        nday81 = nday81 + nleapd
        IF (i .EQ. 1) n0day81 = nday81

C   Calculate the output time fields
C
        pyr = yrdyv(i)/1000
        pdy = yrdyv(i) - pyr*1000  
        phr = IDINT(utv(i))
        tmp = (utv(i) - DBLE(phr))*6.d1
        pmin = IDINT(tmp)
        psec = (tmp - DBLE(pmin))*6.d1
        IF (psec .GT. (6.d1-1.d-9)) THEN
           pmin = pmin + 1
           psec = 0.d0
        END IF


C  First, derive the parabolically (2nd-order Lagrangian) interpolated
C   geophysical parameters (assume K_p measured at midpoint of their
C   3-hr intervals; time of F10.7 measurement adjusted for move from
C   Ottawa to Penticton via FUT)
C
        fut = 2.d1
        IF ((yrdyv(i) .GE. 81000) .AND. (yrdyv(i) .LE. 91181)) fut=1.7d1
        x = (utv(i)-fut) / 2.4d1
        mday = nday81 - n0day81 + 2
        f107dp = x*(x-een)*sf107(mday-1)/twee - (x*x - een)*sf107(mday) 
     1        + x*(x+een)*sf107(mday+1)/twee
        ssndp = DMAX1(nul, DSQRT(93918.4d0 + 1117.3d0*f107dp)-406.37d0)

        mhr = IDINT(utv(i))/3 + 1
        x0 = 3.d0*DBLE(mhr) - 1.5d0
        x = (utv(i)-x0) / 3.d0
        iap = (mday-1)*8 + mhr
        apdp(1) = x*(x-een)*sap(iap-1)/twee - (x*x - een)*sap(iap) +
     1         x*(x+een)*sap(iap+1)/twee
        kpsp(1) = ap_to_kp(SNGL(apdp(1)))


C  Now, adjust the geophysical parameters according to specified method
C   of treating the IMF data  (IMFNA=0 --> no change)
C
        IF (imfna .EQ. 1) THEN
           apdp(1) = 4.d0
           kpsp(1) = 1.0
        ELSE IF (imfna .EQ. 2) THEN
           apdp(2) = 4.d0
           kpsp(2) = 1.0
        END IF

C   Start the loop over NLLO:  whether PIM databases are called at
C     slightly different times in the calculation of ionospheric DR
C     (The adjustment of UTV(i) --> UTDP assumes DELT(1)=0.0)
C
        DO 220 llo = 1, nllo
          utdp = utv(i) + delt(llo)/3.6d3 

C   Check that the adjusted UTDP is in the range 00-24
C
          IF (delt(llo) .NE. nul) CALL utrngchk (utdp,year,day,ndayyr)

C  Start the loop over different K_p (if IMFNA=2 chosen to run PIM
C    twice, once for Bz +/-, to estimate an IMF-based uncertainty)
C
          DO 240 lkp = 1, nkploop
            IF (lkp .EQ. 2) bz = +1.0

C  Now ready to call PIM databases:  get all the COMMON-block PIM
C   variables/parameters into their proper names/TYPES
C
            CALL TIMMDM (year, day, month, idom)
            uthr = SNGL(utdp)
            ut = uthr * 3.6e3
            rf10p7 = SNGL(f107dp)
            f10p7 = rf10p7
            ssn = SNGL(ssndp)
            ap = SNGL(apdp(lkp))
            rkp = kpsp(lkp)
            ekp = MIN(rkp,6.5)
            ekp = MAX(ekp,0.0)
            DO 245 ii=0,2
               kp(ii) = ekp
 245        CONTINUE

C  Call the PIM databases for this time & these geophysical parameters
C
            CALL INITPR
            CALL READ_DBASES (day,month, uthr, pusu,pmid,plow,plme,
     1                          paws, jopen)

C  Now start loop over stations/antennas
C
            DO 300 jsta = 1, nsta

C  Calculate Real-Time-Data correction factors for this station,
C   as applicable (as per interactive query)
c    GPS returns AZGPS,ELGPS,S dimensioned to MAXGPS 
c    ISS returns AZISS,ELISS,DELTA,RHO dimensioned to MAXISS
C
              ngps = 1
              azgps(1) = pi/8.d0
              elgps(1) = azgps(1)
              s(1) = SNGL(een) 
              sigs(1) = SNGL(nul)
              IF (dogps .AND. exstgpsrtd(jsta)) THEN
                 CALL strcct (prtd, rtdtxt, 32, ftmp, istat)
                 CALL strcct (ftmp, stav(jsta)//'.gps', 32,flgps,istat)
                 CALL gpsrta (maxsta, year,day,utdp, stav,jsta, flgps,
     1                          ngps, s,sigs, azgps,elgps)
              END IF
              niss = 1
              aziss(1) = pi/8.d0
              eliss(1) = aziss(1)
              delta(1) = SNGL(een)
              sigdelta(1) = SNGL(nul)
              rho(1) = SNGL(nul)
              sigrho(1) = SNGL(nul)
              IF (doiss .AND. exstissrtd(jsta)) THEN
                 WRITE (*,*) 'ISS RTD-reading subroutine not yet coded'
c                CALL strcct (prtd, rtdtxt, 32, ftmp, istat)
c                CALL strcct (ftmp, stav(jsta)//'.iss', 32,fliss,istat)
c                CALL issrta (..., niss,aziss,eliss,delta,sigdelta,rho,sigrho)
              END IF 

C  Now loop over sources; execute loop only where GOODDAT=.TRUE.
C
              DO 320 ksrc = 1, nsrc
               IF (gooddat(i,jsta,ksrc)) THEN

                WRITE (*,329) i,nut, jsta,nsta, ksrc,nsrc
 329            FORMAT (' UT: ',i3.3,'/',i3.3,'   Sta: ',i2.2,'/',i2.2,
     1                  '   Src: ',i1,'/',i1)

C    And the inner DR-calculating option for just using just the 
C     time-variation of the LoS (from earth rotation/sidereal motion)
C
                DO 340 lli = 1, nlli
                  ll = MAX0(llo, lli)
                  utdp = utv(i) + delt(ll)/3.6d3

C    Check that the adjusted UTDP is in the range 00-24
C
                  IF (delt(lli) .NE. nul) CALL utrngchk (utdp,year,day,
     1                                                    ndayyr)

C    Calculate Sidereal Time (returned in radians) and Az/El of this
C     KSRCth source from this JSTAth station
C
                  IF (.NOT. gpssimflg) THEN

                   CALL sidtim (year,day,utdp, gast)
                   CALL azel (gast, latv(jsta),lonv(jsta), rav(ksrc),
     1                         decv(ksrc), az,el)
                  END IF

C      If El<0, set GOODDAT=.F. for this UT/STA/SRC combination, and 
C       proceed immediately with the next SRC for this UT/STA (don't
C       call any PIM routines)
C
                  IF (el .LT. 0.d0) THEN
                     phr0 = IDINT(utdp)
                     tmp = (utdp - DBLE(phr0))*6.d1
                     pmin0 = IDINT(tmp)
                     psec0 = (tmp - DBLE(pmin0))*6.d1
                     IF (psec0 .GT. (6.d1-1.d-9)) pmin0 = pmin0+1
                     WRITE (*,349) srcv(ksrc),stav(jsta),phr0,pmin0,
     1                              el/degrad
 349   FORMAT (' Below horizon:  ',a9,' from ',a1,' at ',2i2.2,':',f9.3)
                     gooddat(i,jsta,ksrc) = .FALSE.

C           If in SIMULATION-mode, save "placeholder" TEC records 
C             for scans with negative elevations (so that simulation 
C             output remains uniformly sampled in UT-STA-SRC space)
C
                     IF (smode .EQ. 's') THEN
                       DO 345 mll = 1, nll
                         azv(jsta,ksrc,mll) = 0.d0
                         elv(jsta,ksrc,mll) = -6.666d0
                         DO 346 mkp = 1, nkploop
                           tec(jsta,ksrc,mkp,mll) = -999.  
                           rmi(jsta,ksrc,mkp,mll) = -999.
 346                     CONTINUE
 345                   CONTINUE 
                      END IF

C........... next SRC (skipping PIM calls if EL<0)
C
                     GOTO 320
                  END IF

C      Ready to start calling PIM routines via TECVLBI(4)
C      Get all COMMON-block PIM variables/parameters into their proper
C       names/TYPES; also build single-UT arrays of variables to
C       later print out
C 
                  azv(jsta,ksrc,ll) = az / degrad
                  elv(jsta,ksrc,ll) = el / degrad
                  saz = SNGL(az/degrad)
                  sel = SNGL(el/degrad)
                  obslat = SNGL(latv(jsta)/degrad)
                  obslon = SNGL(lonv(jsta)/degrad)

C       Get the electron densities along the LoS
C
                  CALL slantedp (year,day,ut, dbgflg,ldbg, stav(jsta),
     1                            srcv(ksrc), ed,rng,edmax,rngmax,
     2                            loslat,loslon)

C       Apply any real-time data constraints to get corrected EDC
C
                  IF (dogps .OR. doiss) THEN
                    CALL rtdappl (maxgps,maxiss, nalt, az,el,rng,ed,
     1                             rngmax, dogps,doiss, ngps,niss,
     2                             s,sigs,elgps,azgps, rho,sigrho,
     3                             delta,sigdelta,aziss,eliss, edc)
                  ELSE
                    DO 360 kalt = 1, nalt
                      edc(kalt) = ed(kalt)
 360                CONTINUE
                  END IF

C       Integrate to get TEC, Faraday Rotation, etc.
C
                  CALL tecfr (dofr, nalt, az,el, latv(jsta),lonv(jsta),
     1                         zout,rng, loslat,loslon, edc, tec1,rmi1)

C        Assign intra-UT TEC/RMI temporoary-storage arrays
C
                  tec(jsta,ksrc,lkp,ll) = tec1
                  rmi(jsta,ksrc,lkp,ll) = rmi1

 340            CONTINUE

               ELSE

C    If GOODDAT = .F., write out some "placeholder" data to the
C     TEC/RMI temporary-storate arrays (to make the consolidation
C     to only (JSTA,KSRC) more straightforward)
C
                  DO 325 mll = 1, nll
                    azv(jsta,ksrc,mll) = 0.d0
                    elv(jsta,ksrc,mll) = -6.666d0
                    DO 326 mkp = 1, nkploop
                      tec(jsta,ksrc,mkp,mll) = -999.  
                      rmi(jsta,ksrc,mkp,mll) = -999.
 326                CONTINUE
 325              CONTINUE 
               
               END IF

 320          CONTINUE
 300        CONTINUE
 240      CONTINUE
 220    CONTINUE

C   All work done for this UT (220=llo, 240=lkp, 300=ksrc, 320=jsta,
C     340 = lli;  ENDIF = GOODDAT).  Prepare TEC/RMI temporary-storage
C     arrays for UT-based printout  (collapse to V(jsta,ksrc)
C

        CALL pimout4 (gooddat,i,smode, imfna,nsta,nsrc,nll, tec,rmi, 
     1                 dodrpim,dodrazel, delt,lutext, stav,srcv,
     2                 pyr,pdy,phr,pmin,psec, azv,elv)

 200  CONTINUE

 499  FORMAT (i2,1x,i3,'-',2i2.2,f5.1,2x,a1,1x,a8,3(f10.4,f8.4),2f9.3)
 529  FORMAT (i2,1x,i3,'-',2i2.2,f5.1,2x,a1,1x,a8,2f11.5,f11.5,2f9.3)

      END
