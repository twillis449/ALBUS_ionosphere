C	pim_asub.f
C	PIM Astronomical Subroutine
C	A subroutine for making simple calls to PIM routines,
C	for a single source/station at a single time.
C	2005 Aug 30  James M Anderson  --JIVE  start
C                                        This code is based on PIMVLBI4
C                                        developed by R Campbell, and PIM
C                                        from CPI.  See notes below
C       2007 Apr 03  JMA  --add functionality to call for electron density
C                           at individual points
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
C  BLOCK DATA structure moved to pim_blok.f


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*
C



      subroutine PIM_ASUB(
     &     BASEPATH,
     &     STALAT, STALON, STAELEV,
     &     OB_AZ, OB_EL,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     STEC, SRM, SOLCODE, RETCODE)

      IMPLICIT NONE
C_TITLE PIM_ASUB --astronomical subroutine for calling PIM

C_ARGS  TYPE           VARIABLE I/O DESCRIPTION
      CHARACTER*256 BASEPATH   ! I  The base pathname to the databases

      REAL*8 STALAT            ! I  station latitude, in rad, geocentric
      REAL*8 STALON            ! I  station longitude, in rad, geocentric
      REAL*8 STAELEV           ! I  station elevation above Earth radius, in km
      REAL*8 OB_AZ             ! I  object azimuth, in rad, geocentric
      REAL*8 OB_EL             ! I  object elevation, in rad, geocentric

C	ALL TIMES ARE UT!!!
      INTEGER TYEAR            ! I  The year of the observation (must be
                               !    in the range 1981 to ~ 2015)
      INTEGER TYDAY            ! I  The day number of the year, days since Jan 0
      INTEGER TMON             ! I  The month number, Jan == 1
      INTEGER TDAY             ! I  The day of the month
      REAL*8 TUT               ! I  The time of day, in fractional hours

      REAL*8 STEC              ! O  The slant total electron content, in 
                               !    TECU (10^{16} m^{-2}) along the line of sight
      REAL*8 SRM               ! O  The slant rotation measure, in RMU
                               !    (10^{12} T m^{-2})
      INTEGER SOLCODE          ! B  Solar Magnetic Field Code
                               !    on input:
                               !    <=0 try reading frm file, if data missing,
                               !        assume by=1,bz=1
                               !      1 try reading frm file, if data missing,
                               !        assume by=1,bz=-1
                               !      2 assume by=1,bz=1
                               !    >=3 assume by=1,bz=1
                               !    On return:
                               !      0 data read ok
                               !      1 no data available, or values guessed
      INTEGER RETCODE          ! O  The return code
                               !    Note that for really fatal errors, the
                               !    software here and called from here may simply
                               !    STOP
                               !    0 all ok.
                               !    1 negative elevation

C_USER  any user input?

C_VARS  TYPE           VARIABLE I/O DESCRIPTION
C     put include and common stuff here

C_DESC  full description of program

C_FILE  files used and logical units used

C_LIMS  design limitations

C_BUGS  known bugs

C_CALL  list of calls

C_KEYS  

C_HIST  DATE NAME PLACE INFO
C	2005 Aug 30  James M Anderson  --JIVE  start

C_END








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
      REAL*8 pi, degrad, nul,een,twee,deg2rad,rad2deg
      INTEGER maxut,maxsta,maxsrc,maxdays
      PARAMETER (maxut=1, maxsta=1, maxsrc=1, maxdays=1)
      PARAMETER (pi = 3.14159265358979323846d0)
      PARAMETER (degrad = pi / 1.8d2)
      PARAMETER (deg2rad = pi / 180d0)
      PARAMETER (rad2deg = 180d0 / pi)
      PARAMETER (nul=0.d0, een=1.d0, twee=2.d0)
      INTEGER ndays,nlli,nllo
      INTEGER grdtyp, outtyp, year,day,month, yrdyv(maxut), imed,iyear
      INTEGER pyr,pdy,phr,pmin,  ndayyr
      INTEGER nday81,n0day81,nleapd, mday,mhr
      INTEGER i, ii,iap, llfsct, nut,nsta,nsrc
      INTEGER idom, imfna, kalt
      REAL ut
      REAL kpmin,kpmax, kpsp(2)
cREAL by,bz
      REAL uthr, tec1
      REAL yearigrf
      REAL ed(maxaltpt),rng(maxaltpt),edmax,rngmax, edc(maxaltpt)
      REAL loslat(maxaltpt), loslon(maxaltpt)
      REAL rmi1
creal tec2,tec3
      REAL*8 utv(maxut), utdp, x,x0
      REAL*8 sf107(maxdays+2), sap((maxdays+2)*8) 
      REAL*8 sfmin,sfmax, apmin,apmax, fut,f107dp,ssndp, apdp(2)
      REAL*8 bydp,bysig,bzdp,bzsig
C      REAL*8 dimo
      REAL*8 tmp,psec
      CHARACTER stav(maxsta)*1, srcv(maxsrc)*8
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 pkpf,pimf
C      CHARACTER*256 psrc,psta,pigrf
C      CHARACTER*256 prtd
      LOGICAL leapyrflg,dbgflg, dofr







CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Start Code Section Here
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     Set the default return values
      RETCODE = 0
      STEC = 0.0
      SRM = 0.0
C     If El<0, set GOODDAT=.F. for this UT/STA/SRC combination, and 
C     proceed immediately with the next SRC for this UT/STA (don't
C     call any PIM routines)
      IF (OB_EL .LT. 0.d0) THEN
         RETCODE = 1
         RETURN
      END IF

      dofr = .TRUE.
      dbgflg = .FALSE.

c$$$      PRINT *, STALAT, STALON, STAELEV
c$$$      PRINT *, OB_AZ, OB_EL
c$$$      PRINT *, TYEAR, TYDAY, TMON, TDAY, TUT
c$$$      PRINT *, BASEPATH
c$$$      PRINT *, 'A',BASEPATH(256:256),'A'
c$$$      STOP




C     First, those constant PIM parameters (throughout all epoch, source
C     station loops)
C     Most of the variables in the COMMON block GRID as definded in
C     BLOCK DATA ITERATS are okay as they are (since not used anyway)
C     
      saz = SNGL(OB_AZ*rad2deg)
      sel = SNGL(OB_EL*rad2deg)
      obslat = SNGL(STALAT*rad2deg)
      obslon = SNGL(STALON*rad2deg)
      obselev= SNGL(STAELEV)

      outtyp = 0
      grdtyp = 2
      usessn = 1
      ursisw = .FALSE.
      eswitch = .FALSE.
      geog = .TRUE.
      nalt = MAXALTPT
C     Initialize the ZOUT area
      CALL INIT_ZOUT()
      plasph = .TRUE.
      llfsct = 0
C     2005 Sep 01  James M Anderson  --JIVE  why on Earth is this being called?
C      CALL SETSCT(llfsct) 

C---------------------------------------------------------------
C     
C     PKPF/PIMF    CHAR    paths of geophysical datafiles
C     PIGRF        CHAR    path for IGRF databases
C     PSTA/PSRC    CHAR    paths for station & source catalogues
C     PRTD         CHAR    path for real-time data (GPS/ISS)
C     OUTFILE      CHAR    path for output file 
C     YRDY		INT    	yyddd
C     MONTH	INT	month (1-12) 
C     MEXPTYPE     INT     the type of input file (export/VLBI3cards/..)
C     UTV		REAL	vector of UTs [hh.hhhhhh]
C     GOODDAT	LOGICAL	whether specific UT-STA-SRC combination 
C     exists in export file
C     RAV		REAL*8  vector of source RAs      [rad]
C     DECV		REAL*8	vector of source DECs     [rad]
C     LATV		REAL*8  vector of station LATs    [rad]
C     LONV         REAL*8  vector of station LONGs   [rad]
C     NLLI/NLLO    INT     controls looping for DR estimates:
C     PIM databses read NLLO times
C     PIM called MAX(nllo,nlli) times w/diff. LoS
C     JYR          INT     4-digit year
C     JDY		INT     3-digit day-of-year
C     JD           REAL*8  Julian Date  (244.... .5)
C     UT24HR	REAL    UT [fraction of day] used in LAST calc
C     JDK          REAL*8  Julian Date of epoch 
C     EQEQNX	REAL*8  Equation of equinoxes
C     SIDT		REAL*8  Local apparent sidereal time
C     HA           REAL*8  Hour angle
C     ELV		REAL    vector(3-matrix) of elevations  [deg]
C     AZV		REAL    vector(3-matrix) of azimuths    [deg]
C     SF107        REAL*8  27-d avg F10.7 for all NDAYS of data, +/- 1day
C     SAP          REAL*8  24-hr avg Ap for all NDAYS of data, +/- 1day,
C     sampled every 3 UT hrs
C     F107V	REAL	vector of 27-d avg F10.7 at each UT
C     SSNV		REAL    vector of SSN (calc from F10.7)
C     APV		REAL    vector of 24-hr avg Ap
C     KPV		REAL	vector of 24-hr avg Kp (from APV)
C     UTMID	REAL	mid-point UT of obs., for reading IMF By,Bz
C     YEARIGRF     REAL    year (as REAL) for IGRF subroutines
C     YEAR         INT     year (as INT) for PIM subroutines
C     ED           REAL    values of electron density along LoS
C     RNG          REAL    values of range along LoS
C     TEC          REAL    value of slant TEC 
C     RMI          REAL    Integrated rotation measure 

C-------------------------------
C     Now take care of data-file / logical-unit assignments by using the
C     regular PIM  SET_UP subroutine as modified to read the geophysical
C     datafile names
C     
      CALL SET_UP_VLBI2 (BASEPATH,lucgdb,pkpf,pimf)

      
      nut = 1
      nsta = 1
      nsrc = 1
      yrdyv(1) = TYEAR*1000+TYDAY
      utv(1) = TUT

      nllo = 1
      nlli = 1


      ndays = 1

      ndayyr = 365
      leapyrflg = .FALSE.
      iyear = yrdyv(1)/1000
      IF (MOD(iyear,4) .EQ. 0) THEN
         ndayyr = 366
         leapyrflg = .TRUE.
      END IF



C     Query for including Faraday rotation
C     if so, load spherical-harmonic databases, using median time of UTV
C     
C     now get YEAR/DAY-->REAL as IGRF subroutines expect
      imed = (nut+1)/2
      year = yrdyv(imed)/1000
      day = yrdyv(imed) - year*1000
C     2005 Aug 31  James M Anderson  --make all years full years
c$$$      IF (year .LT. 81) THEN
c$$$         year = year + 2000
c$$$      ELSE
c$$$         year = year + 1900
c$$$      END IF
      yearigrf = FLOAT(year) + FLOAT(day)/FLOAT(ndayyr)
c$$$      CALL feldcof (yearigrf, pigrf, dimo)

C----------------------
C     All station/source coords loaded.
C     Open the Kp/Ap;F10.7 file here outside the NUT loop, and load
C     necessary elements from file to memory
C     
      CALL read_kpf107 (pkpf,maxdays, yrdyv(1),yrdyv(nut), 
     1     ndays,sf107,sap)

C     Loop through returned SF107, SAP to find max/min to print out as
C     diagnostics/warnings
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

c$$$      WRITE (*,138) sfmin,sfmax
c$$$ 138  FORMAT (/,'Range of Solar F10.7 around obs = ',f7.2, ' to',f7.2)
c$$$      WRITE (*,139) kpmin,kpmax
c$$$ 139  FORMAT ('Range of 3-hr Kp in 24-hrs around obs =',f4.1, ' to',
c$$$     1     f4.1)

C     Read IMF parameters from (GSFC OMNIWEB-generated) file
C     
      IF(SOLCODE .le. 1) THEN
         IMFNA = SOLCODE
         CALL read_imf (pimf, yrdyv(1),yrdyv(nut), utv(1),utv(nut),
     1        bydp,bysig,bzdp,bzsig, imfna)
         SOLCODE = IMFNA
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
 160        CONTINUE
         END IF
      ELSEIF(SOLCODE .eq. 2) THEN
         SOLCODE = 1
         by = 1.0
         bz = 1.0
         DO 161 i = 1, ((ndays+2)*8)
            sap(i) = 4.d0
 161     CONTINUE
      ELSE
         SOLCODE = 1
         by = 1.0
         bz = -1.0
      ENDIF



C     Start the main part of the processing
      year = TYEAR
      day = TYDAY
      nday81 = (year-1981)*365 + day
      nleapd = (year-1981)/4
      nday81 = nday81 + nleapd
      n0day81 = nday81

C     Calculate the output time fields
C     
      pyr = TYEAR
      pdy = TYDAY
      phr = IDINT(utv(1))
      tmp = (utv(1) - DBLE(phr))*6.d1
      pmin = IDINT(tmp)
      psec = (tmp - DBLE(pmin))*6.d1
      IF (psec .GT. (6.d1-1.d-9)) THEN
         pmin = pmin + 1
         psec = 0.d0
      END IF


C     First, derive the parabolically (2nd-order Lagrangian) interpolated
C     geophysical parameters (assume K_p measured at midpoint of their
C     3-hr intervals; time of F10.7 measurement adjusted for move from
C     Ottawa to Penticton via FUT)
C     
      fut = 2.d1
      IF ((yrdyv(1) .GE. 1981000) .AND. (yrdyv(1) .LE. 1991181))
     &     fut=1.7d1
      x = (utv(1)-fut) / 2.4d1
      mday = nday81 - n0day81 + 2
      f107dp = x*(x-een)*sf107(mday-1)/twee - (x*x - een)*sf107(mday) 
     1     + x*(x+een)*sf107(mday+1)/twee
      ssndp = DMAX1(nul, DSQRT(93918.4d0 + 1117.3d0*f107dp)-406.37d0)

      mhr = IDINT(utv(1))/3 + 1
      x0 = 3.d0*DBLE(mhr) - 1.5d0
      x = (utv(1)-x0) / 3.d0
      iap = (mday-1)*8 + mhr
      apdp(1) = x*(x-een)*sap(iap-1)/twee - (x*x - een)*sap(iap) +
     1     x*(x+een)*sap(iap+1)/twee
      kpsp(1) = ap_to_kp(SNGL(apdp(1)))


C     Now, adjust the geophysical parameters according to specified method
C     of treating the IMF data  (IMFNA=0 --> no change)
C     
      IF (SOLCODE .EQ. 1) THEN
         apdp(1) = 4.d0
         kpsp(1) = 1.0
      END IF

C     Now ready to call PIM databases:  get all the COMMON-block PIM
C     variables/parameters into their proper names/TYPES
C     
C     CALL TIMMDM (year, day, month, idom)
      year = TYEAR
      day = TYDAY
      month = TMON
      idom = TDAY
      utdp = utv(1)
      uthr = SNGL(utdp)
      ut = uthr * 3.6e3
      rf10p7 = SNGL(f107dp)
      f10p7 = rf10p7
      ssn = SNGL(ssndp)
      ap = SNGL(apdp(1))
      rkp = kpsp(1)
      ekp = MIN(rkp,6.5)
      ekp = MAX(ekp,0.0)
      DO 245 ii=0,2
         kp(ii) = ekp
 245  CONTINUE

C     Call the PIM databases for this time & these geophysical parameters
C     
      CALL INITPR
      CALL READ_DBASES (day,month, uthr, pusu,pmid,plow,plme,
     1     paws, jopen)


C     Get the electron densities along the LoS
C     
      CALL slantedp (year,day,ut, dbgflg,LUSTDERR, stav(1),
     1     srcv(1), ed,rng,edmax,rngmax,
     2     loslat,loslon)

C     Apply any real-time data constraints to get corrected EDC
C     
      DO 360 kalt = 1, nalt
         edc(kalt) = ed(kalt)
 360  CONTINUE

C     Integrate to get TEC, Faraday Rotation, etc.
C     
      CALL tecfr (dofr, yearigrf, nalt, OB_AZ, OB_EL, STALAT,STALON,
     1     zout,rng, loslat,loslon, edc, tec1,rmi1)

C     Assign intra-UT TEC/RMI temporoary-storage arrays
C     
      STEC = tec1
      SRM = rmi1

      RETURN
      END





      SUBROUTINE INIT_ZOUT()

      IMPLICIT NONE
C_TITLE INIT_ZOUT --initialize the ZOUT array

C_ARGS  TYPE           VARIABLE I/O DESCRIPTION

C_USER  any user input?

C_VARS  TYPE           VARIABLE I/O DESCRIPTION
C     put include and common stuff here
      INCLUDE 'grid.inc'
      INCLUDE 'logicuni.inc'

C_DESC  full description of program
C     Initialize the ZOUT common array, to give a grid of values
C     for the calculation of the ionosphere at certain altitudes.

C_FILE  files used and logical units used

C_LIMS  design limitations

C_BUGS  known bugs

C_CALL  list of calls

C_KEYS  

C_HIST  DATE NAME PLACE INFO
C     2005 Sep 08  James M Anderson  --JIVE  start

C_END

      INTEGER MFACTOR
      INTEGER I,J
      REAL FACTOR
C      REAL DELTA
      LOGICAL inited
      SAVE inited
      DATA inited/.FALSE./

      IF(inited.eqv..TRUE.) THEN
         RETURN
      ENDIF
      inited = .TRUE.
      
C     Ok, how big is ZOUT supposed to be, and how big is the original?
C     I want this to be some reasonable INTEGER multiple
      FACTOR=REAL(MAXALTPT)/NUMZOUTORIG
      MFACTOR=FACTOR
      IF(MFACTOR.NE.FACTOR) THEN
         WRITE(LUSTDERR,*) 'Error: need integer multiple of zoutorig'
         WRITE(LUSTDERR,*) 'Got', MAXALTPT, NUMZOUTORIG
         STOP 'INIT_ZOUT'
      ENDIF
      FACTOR=1.0/FACTOR

      IF(MFACTOR.EQ.1) THEN
         DO i=1,NUMZOUTORIG
            ZOUT(i) = ZOUTORIG(i)
         ENDDO
      ELSE
         ZOUT(1) = ZOUTORIG(1)
         DO i=1,NUMZOUTORIG-1
            DO j=1,MFACTOR
               ZOUT(MFACTOR*i+j) = (ZOUTORIG(i)*(MFACTOR-j)
     &              + ZOUTORIG(i+1)*j)*FACTOR
c$$$               PRINT *, i, j, MFACTOR*i+j,
c$$$     &              ZOUT(MFACTOR*i+j), ZOUTORIG(i), 
c$$$     &              ZOUTORIG(i+1)
            ENDDO
         ENDDO
         DO j=2,MFACTOR
            ZOUT(j) = (ZOUT(1)*(MFACTOR-j+FACTOR)
     &           +ZOUT(MFACTOR+1)*(j-FACTOR))*FACTOR
         ENDDO
      ENDIF
c$$$      DO i=1,MAXALTPT
c$$$         PRINT *, ZOUT(i)
c$$$      ENDDO
c$$$      STOP


c$$$      IF(MAXALTPT.GE.180) THEN
c$$$         FACTOR = 86.
c$$$         DO i=1,MAXALTPT
c$$$            ZOUT(i) = FACTOR
c$$$            IF(FACTOR.lt.100.) THEN
c$$$               DELTA = 2.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.130.) THEN
c$$$               DELTA = 3.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.500.) THEN
c$$$               DELTA = 5.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.700.) THEN
c$$$               DELTA = 8.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.850.) THEN
c$$$               DELTA = 12.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.1000.) THEN
c$$$               DELTA = 25.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.1300.) THEN
c$$$               DELTA = 50.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.1800.) THEN
c$$$               DELTA = 100.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.2500.) THEN
c$$$               DELTA = 150.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.5000.) THEN
c$$$               DELTA = 250.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.10000.) THEN
c$$$               DELTA = 400.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.20000.) THEN
c$$$               DELTA = 600.*REAL(MAXALTPT)*0.005
c$$$            ELSEIF(FACTOR.lt.50000.) THEN
c$$$               DELTA = 1000.*REAL(MAXALTPT)*0.005
c$$$            ENDIF
c$$$            FACTOR = FACTOR + DELTA
c$$$C            print *, i, ZOUT(i)
c$$$         ENDDO
c$$$C     Make the last one really far out
c$$$         ZOUT(MAXALTPT) = ZOUT(MAXALTPT) + 25000.0
c$$$      ENDIF


      RETURN
      END


      subroutine PIM_ASUB_ED(
     &     BASEPATH,
     &     OBSLATD, OBSLOND, OBSELEVD,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     E_D, SOLCODE, RETCODE)

      IMPLICIT NONE
C_TITLE PIM_ASUB_ED --astronomical subroutine for calling PIM electron density

C_ARGS  TYPE           VARIABLE I/O DESCRIPTION
      CHARACTER*256 BASEPATH   ! I  The base pathname to the databases

      REAL*8 OBSLATD           ! I  observation latitude, in rad, geocentric
      REAL*8 OBSLOND           ! I  observation longitude, in rad, geocentric
      REAL*8 OBSELEVD          ! I  observation elevation above Earth radius, in km
C	ALL TIMES ARE UT!!!
      INTEGER TYEAR            ! I  The year of the observation (must be
                               !    in the range 1981 to ~ 2015)
      INTEGER TYDAY            ! I  The day number of the year, days since Jan 0
      INTEGER TMON             ! I  The month number, Jan == 1
      INTEGER TDAY             ! I  The day of the month
      REAL*8 TUT               ! I  The time of day, in fractional hours

      REAL*8 E_D               ! O  The electron density, in 
                               !    m^{-3} at the observation point
      INTEGER SOLCODE          ! B  Solar Magnetic Field Code
                               !    on input:
                               !    <=0 try reading frm file, if data missing,
                               !        assume by=1,bz=1
                               !      1 try reading frm file, if data missing,
                               !        assume by=1,bz=-1
                               !      2 assume by=1,bz=1
                               !    >=3 assume by=1,bz=1
                               !    On return:
                               !      0 data read ok
                               !      1 no data available, or values guessed
      INTEGER RETCODE          ! O  The return code
                               !    Note that for really fatal errors, the
                               !    software here and called from here may simply
                               !    STOP
                               !    0 all ok.
                               !    1 negative elevation

C_USER  any user input?

C_VARS  TYPE           VARIABLE I/O DESCRIPTION
C     put include and common stuff here

C_DESC  full description of program

C_FILE  files used and logical units used

C_LIMS  design limitations

C_BUGS  known bugs

C_CALL  list of calls

C_KEYS  

C_HIST  DATE NAME PLACE INFO
C	2007 Apr 03  James M Anderson  --JIVE  start

C_END

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
      INCLUDE 'array.inc'
      include 'const.inc'

C-----------------
C  Declarations
C
      EXTERNAL ap_to_kp, ap_to_kptab
      REAL ap_to_kp, ap_to_kptab
      REAL*8 nul,een,twee
      PARAMETER (nul=0.d0, een=1.d0, twee=2.d0)
      INTEGER ndays,nlli,nllo
      INTEGER grdtyp, outtyp, year,day,month, yrdyv,iyear
      INTEGER pyr,pdy,phr,pmin,  ndayyr
      INTEGER nday81,n0day81,nleapd, mday,mhr
      INTEGER i, ii,iap, llfsct, nut,nsta,nsrc
      INTEGER idom, imfna
      REAL ut
      REAL kpsp(2)
      REAL uthr
      REAL yearigrf
      REAL*8 utv, utdp, x,x0
      REAL*8 sf107(1+2), sap((1+2)*8) 
      REAL*8 fut,f107dp,ssndp, apdp(2)
      REAL*8 bydp,bysig,bzdp,bzsig
C      REAL*8 dimo
      REAL*8 tmp,psec
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 pkpf,pimf
C      CHARACTER*256 psrc,psta,pigrf
C      CHARACTER*256 prtd
      LOGICAL leapyrflg,dbgflg, dofr, main_init
      INTEGER tyear_last, tyday_last, tmon_last, tday_last
      REAL*8 tut_last
      REAL mlat,mlon,mlt
      INTEGER MAX_ALT_NUM7
      PARAMETER(MAX_ALT_NUM7=MAX_ALT_NUM+7)
      INTEGER MAX_ALT_NUM8
      PARAMETER(MAX_ALT_NUM8=MAX_ALT_NUM+8)
      REAL ddata(MAX_ALT_NUM8),cdata(MAX_ALT_NUM7)
      CHARACTER*16 dsgntr
      DATA tyear_last/-1/,tyday_last/-1/,tmon_last/-1/,tday_last/-1/
      DATA tut_last/-1.0D0/,main_init/.false./
      SAVE


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Start Code Section Here
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C     Set the default return values
      RETCODE = 0
      E_D = 0.0
C     First, those constant PIM parameters (throughout all epoch, source
C     station loops)
C     Most of the variables in the COMMON block GRID as definded in
C     BLOCK DATA ITERATS are okay as they are (since not used anyway)
C     
      obslat = SNGL(OBSLATD*RTOD)
      obslon = SNGL(OBSLOND*RTOD)
      obselev= SNGL(OBSELEVD)
C	Check if we need to initilize
      IF((TUT.ne.tut_last).or.(TYEAR.ne.tyear_last).or.
     &     (TYDAY.ne.tyday_last).or.(TMON.ne.tmon_last).or.
     &     (TDAY.ne.tday_last)) THEN

         IF(main_init.eqv..false.) THEN
            main_init = .true.
            dsgntr = 'EDP'
            dofr = .TRUE.
            dbgflg = .FALSE.
            outtyp = 0
            grdtyp = 2
            usessn = 1
            ursisw = .FALSE.
            eswitch = .FALSE.
            geog = .TRUE.
            nalt = 1
            plasph = .TRUE.
            llfsct = 0
C---------------------------------------------------------------
C     
C     PKPF/PIMF    CHAR    paths of geophysical datafiles
C     PIGRF        CHAR    path for IGRF databases
C     PSTA/PSRC    CHAR    paths for station & source catalogues
C     PRTD         CHAR    path for real-time data (GPS/ISS)
C     OUTFILE      CHAR    path for output file 
C     YRDY		INT    	yyddd
C     MONTH	INT	month (1-12) 
C     MEXPTYPE     INT     the type of input file (export/VLBI3cards/..)
C     UTV		REAL	vector of UTs [hh.hhhhhh]
C     GOODDAT	LOGICAL	whether specific UT-STA-SRC combination 
C     exists in export file
C     RAV		REAL*8  vector of source RAs      [rad]
C     DECV		REAL*8	vector of source DECs     [rad]
C     LATV		REAL*8  vector of station LATs    [rad]
C     LONV         REAL*8  vector of station LONGs   [rad]
C     NLLI/NLLO    INT     controls looping for DR estimates:
C     PIM databses read NLLO times
C     PIM called MAX(nllo,nlli) times w/diff. LoS
C     JYR          INT     4-digit year
C     JDY		INT     3-digit day-of-year
C     JD           REAL*8  Julian Date  (244.... .5)
C     UT24HR	REAL    UT [fraction of day] used in LAST calc
C     JDK          REAL*8  Julian Date of epoch 
C     EQEQNX	REAL*8  Equation of equinoxes
C     SIDT		REAL*8  Local apparent sidereal time
C     HA           REAL*8  Hour angle
C     ELV		REAL    vector(3-matrix) of elevations  [deg]
C     AZV		REAL    vector(3-matrix) of azimuths    [deg]
C     SF107        REAL*8  27-d avg F10.7 for all NDAYS of data, +/- 1day
C     SAP          REAL*8  24-hr avg Ap for all NDAYS of data, +/- 1day,
C     sampled every 3 UT hrs
C     F107V	REAL	vector of 27-d avg F10.7 at each UT
C     SSNV		REAL    vector of SSN (calc from F10.7)
C     APV		REAL    vector of 24-hr avg Ap
C     KPV		REAL	vector of 24-hr avg Kp (from APV)
C     UTMID	REAL	mid-point UT of obs., for reading IMF By,Bz
C     YEARIGRF     REAL    year (as REAL) for IGRF subroutines
C     YEAR         INT     year (as INT) for PIM subroutines
C     ED           REAL    values of electron density along LoS
C     RNG          REAL    values of range along LoS
C     TEC          REAL    value of slant TEC 
C     RMI          REAL    Integrated rotation measure 

C-------------------------------
C     Now take care of data-file / logical-unit assignments by using the
C     regular PIM  SET_UP subroutine as modified to read the geophysical
C     datafile names
C     
            CALL SET_UP_VLBI2 (BASEPATH,lucgdb,pkpf,pimf)
            nut = 1
            nsta = 1
            nsrc = 1
            nllo = 1
            nlli = 1
            ndays = 1
         ENDIF
         IF((TYEAR.eq.tyear_last).and.(TYDAY.eq.tyday_last).and.
     &        (TMON.eq.tmon_last).and.(TDAY.eq.tday_last)) THEN
C     The only thing that has changed is the hour.  Leave everything else
C     alone
         ELSE
            yrdyv = TYEAR*1000+TYDAY
            ndayyr = 365
            leapyrflg = .FALSE.
            iyear = TYEAR
            IF (MOD(iyear,4) .EQ. 0) THEN
               ndayyr = 366
               leapyrflg = .TRUE.
            END IF
C     now get YEAR/DAY-->REAL as IGRF subroutines expect
            year = TYEAR
            day = TYDAY
            nday81 = (year-1981)*365 + day
            nleapd = (year-1981)/4
            nday81 = nday81 + nleapd
            n0day81 = nday81
C     2005 Aug 31  James M Anderson  --make all years full years
c$$$      IF (year .LT. 81) THEN
c$$$         year = year + 2000
c$$$      ELSE
c$$$         year = year + 1900
c$$$      END IF
C            yearigrf = FLOAT(year) + FLOAT(day)/FLOAT(ndayyr)
c$$$      CALL feldcof (yearigrf, pigrf, dimo)

C----------------------
C     All station/source coords loaded.
C     Open the Kp/Ap;F10.7 file here outside the NUT loop, and load
C     necessary elements from file to memory
C     
            CALL read_kpf107 (pkpf,1, yrdyv,yrdyv, 
     &           ndays,sf107,sap)
         ENDIF
         tyear_last = TYEAR
         tyday_last = TYDAY
         tmon_last = TMON
         tday_last = TDAY
         tut_last = TUT

C     Read IMF parameters from (GSFC OMNIWEB-generated) file
C     
         utv = TUT
         IF(SOLCODE .le. 1) THEN
            IMFNA = SOLCODE
            CALL read_imf (pimf, yrdyv,yrdyv, utv,utv,
     &           bydp,bysig,bzdp,bzsig, imfna)
            SOLCODE = IMFNA
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
 160           CONTINUE
            END IF
         ELSEIF(SOLCODE .eq. 2) THEN
            SOLCODE = 1
            by = 1.0
            bz = 1.0
            DO 161 i = 1, ((ndays+2)*8)
               sap(i) = 4.d0
 161        CONTINUE
         ELSE
            SOLCODE = 1
            by = 1.0
            bz = -1.0
         ENDIF


C     Calculate the output time fields
C     
         pyr = TYEAR
         pdy = TYDAY
         phr = IDINT(utv)
         tmp = (utv - DBLE(phr))*6.d1
         pmin = IDINT(tmp)
         psec = (tmp - DBLE(pmin))*6.d1
         IF (psec .GT. (6.d1-1.d-9)) THEN
            pmin = pmin + 1
            psec = 0.d0
         END IF
C     First, derive the parabolically (2nd-order Lagrangian) interpolated
C     geophysical parameters (assume K_p measured at midpoint of their
C     3-hr intervals; time of F10.7 measurement adjusted for move from
C     Ottawa to Penticton via FUT)
C     
         fut = 2.d1
         IF ((yrdyv .GE. 1981000) .AND. (yrdyv .LE. 1991181))
     &        fut=1.7d1
         x = (utv-fut) / 2.4d1
         mday = nday81 - n0day81 + 2
         f107dp = x*(x-een)*sf107(mday-1)/twee - (x*x - een)*sf107(mday) 
     1        + x*(x+een)*sf107(mday+1)/twee
         ssndp = DMAX1(nul, DSQRT(93918.4d0 + 1117.3d0*f107dp)-406.37d0)

         mhr = IDINT(utv)/3 + 1
         x0 = 3.d0*DBLE(mhr) - 1.5d0
         x = (utv-x0) / 3.d0
         iap = (mday-1)*8 + mhr
         apdp(1) = x*(x-een)*sap(iap-1)/twee - (x*x - een)*sap(iap) +
     1        x*(x+een)*sap(iap+1)/twee
         kpsp(1) = ap_to_kp(SNGL(apdp(1)))
C     Now, adjust the geophysical parameters according to specified method
C     of treating the IMF data  (IMFNA=0 --> no change)
C     
         IF (SOLCODE .EQ. 1) THEN
            apdp(1) = 4.d0
            kpsp(1) = 1.0
         END IF
C     Now ready to call PIM databases:  get all the COMMON-block PIM
C     variables/parameters into their proper names/TYPES
C     
C     CALL TIMMDM (year, day, month, idom)
         year = TYEAR
         day = TYDAY
         month = TMON
         idom = TDAY
         utdp = utv
         uthr = SNGL(utdp)
         ut = uthr * 3.6e3
         rf10p7 = SNGL(f107dp)
         f10p7 = rf10p7
         ssn = SNGL(ssndp)
         ap = SNGL(apdp(1))
         rkp = kpsp(1)
         ekp = MIN(rkp,6.5)
         ekp = MAX(ekp,0.0)
         DO 245 ii=0,2
            kp(ii) = ekp
 245     CONTINUE
C     Call the PIM databases for this time & these geophysical parameters
C     
         CALL INITPR
         CALL READ_DBASES (day,month, uthr, pusu,pmid,plow,plme,
     1        paws, jopen)
      ENDIF


C     Get the electron densities at the observation point
C     First, get the geomagnetic lat and lon
      CALL gcxcgm (day,ut, obslat,obslon, mlat,mlon,mlt)
C   *) compute the electron density there [cm^{-3}]
C
      ddata(1) = 1.0
      ddata(2) = obselev
      CALL regmod (dsgntr,year,day,ut, obslat,obslon, mlat,mlon,mlt,
     1     1,ddata,cdata)
      E_D = cdata(1) * 1E6

      RETURN
      END




