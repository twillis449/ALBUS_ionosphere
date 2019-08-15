PRO USU,PATH,LUN,DAY,UT,MLT,MLAT,F10P7,KP,BY,SPEC,USEDEC,USEAP,PCIDL,NALT, $
        ALT,DEN
;
;  PURPOSE
;     A driver for constructing NO+, O2+, and O+ density altitude profiles from
;     the parameterized high-latitude USU database using modular USU database
;     access and manipulation routines.
;
;  METHOD
;     From the given input parameters, the USU case identifier and ion species
;     descriptor are determined.  EOFs for the case and ion are read from the
;     appropriate USU EOF file.  Orthogonal polynomial coefficients for the
;     case, ion, and universal time are read from the appropriate USU
;     orthogonal polynomial coefficients file.  The orthogonal polynomial
;     coefficients describe the behavior of Fourier coefficients in magnetic
;     latitude for a given universal time.  Fourier coefficients are determined
;     from the orthogonal polynomial coefficients.  The Fourier coefficients
;     describe the behavior of EOF coefficients in magnetic local time for a
;     given magnetic latitude and universal time.  EOF coefficients are
;     determined from the Fourier coefficients.  The EOF coefficients, in
;     combination with the EOFs, describe the ion density altitude profile
;     for a given magnetic local time, magnetic latitude, and universal time.
;
;     Before USU can be called with parameters, it must be called once with
;     no parameters to initialize common block CBUSU.
;
;  INPUT PARAMETERS
;     BY       The direction of By, '+' or '-'
;     DAY      The day of the year
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     KP       The magnetic Kp index
;     LUN      The logical unit number used to access USU database files
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     PATH     The location of USU database files
;     PCIDL    A logical flag, 1 if the IBM-PC/Windows IDL convention for
;              associated file variables is to be used, 0 if the VAX/VMS IDL
;              convention is to be used
;     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
;     USEAP    A logical flag, 1 if Ap is to be used to determine the magnetic
;              activity level descriptor, 0 if Kp is to be used
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the month descriptor, 0 if the day of the year is to
;              be used
;     UT       The universal time, in decimal hours
;
;  OUTPUT PARAMETERS
;     ALT      The altitude grid, in km
;     DEN      The ion density altitude profile, in cm-3
;     NALT     The number of altitude points
;
;  LOCAL VARIABLES
;     None
;
;  PROCEDURES REQUIRED
;     USUDEN   Generates an ion density altitude profile from EOFs and EOF
;              coefficients
;     USUEC    Generates EOF coefficients from Fourier coefficients
;     USUEOF   Reads an EOF file
;     USUFC    Generates Fourier coefficients from orthogonal polynomial
;              coefficients
;     USUID    Determines the case identifier
;     USUINI   Initializes common block CBUSU
;     USUION   Determines the ion species descriptor
;     USUOPC   Reads an orthogonal polynomial coefficients file
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Initialize common block CBUSU on an initialization call to USU (with no
;  arguments)
;
IF N_PARAMS() EQ 0 THEN BEGIN
   USUINI
;
;  Generate an ion density profile on a normal call to USU (with arguments)
;
ENDIF ELSE BEGIN
;
;  Determine the case identifier
;
   USUID,DAY,MLAT,F10P7,KP,BY,USEDEC,USEAP
;
;  Determine the ion species descriptor
;
   USUION,SPEC
;
;  Read the USU EOF file
;
   USUEOF,PATH,LUN,PCIDL,NALT,ALT
;
;  Read the USU orthogonal polynomial coefficients file
;
   USUOPC,PATH,LUN,UT,PCIDL
;
;  Generate Fourier coefficients from the orthogonal polynomial coefficients
;
   USUFC,UT,MLAT
;
;  Generate EOF coefficients from the Fourier coefficients
;
   USUEC,UT,MLT,MLAT
;
;  Generate an ion density altitude profile from the EOFs and EOF coefficients
;
   USUDEN,UT,MLT,MLAT,NALT,DEN
;
;  Store historical quantities for the next call to USU
;
   USUHIS,DAY,UT,MLT,MLAT,F10P7,KP,BY,SPEC,USEDEC,USEAP
;
ENDELSE
;
RETURN
END
PRO USUINI
;
;  PURPOSE
;     To initialize the contents of common block CBUSU.
;
;  METHOD
;     Common block CBUSU is intended for internal use by the USU modules.
;
;  INPUT PARAMETERS
;     None
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     None
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MALT     The maximum number of altitude points
;     MEOF     The maximum number of EOFs
;     MFC      The maximum number of Fourier coefficients
;     MOPM1    The maximum number of orthogonal polynomials - 1
;
MALT=37
MEOF=37
MFC=9
MOPM1=8
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  Initialize common block CBUSU
;
ID='       '
ION='  '
YEAR=0
DAY=0
F10P7=0.
AP=0.
KP=0.
NMLT=0
SMLT=0.
DMLT=0.
NMLAT=0
SMLAT=0.
DMLAT=0.
NUT=0
SUT=0.
DUT=0.
D=FLTARR(MEOF)
DSHFT=FLTARR(MEOF)
EOFS=FLTARR(MALT,MEOF)
NFC=0
NOPM1=0
NEOF=0
DELTA=0.
OPC=FLTARR(MFC,MOPM1+1,MEOF)
FC=FLTARR(MFC,MEOF)
EC=FLTARR(MEOF)
PRVDAY=9999
PRVUT=9999.
PRVMLT=9999.
PRVMLA=9999.
PRVF10=9999.
PRVKP=9999.
PRVBY='9'
PRVSPE='999'
PRVUSD=9999
PRVUSA=9999
PRVID='9999999'
PRVION='99'
;
RETURN
END
PRO USUID,XDAY,MLAT,XF10P7,XKP,BY,USEDEC,USEAP
;
;  PURPOSE
;     To determine an USU case identifier.
;
;  METHOD
;     The USU case identifier is a code for describing the ambient conditions
;     (day of the year, magnetic hemisphere, magnetic activity level, solar
;     activity level, and direction of By) of a USU case.  The case identifier
;     has the form 'aHbcdee', where 'a' describes the magnetic hemisphere, 'H'
;     defines the USU cases as High-latitude, 'b' describes the season, 'c'
;     describes the magnetic activity level, 'd' describes the solar actviity
;     level, and 'ee' describes the Heppner-Maynard convection pattern used.
;     The magnetic latitude determines 'a':  'N' for a Northern hemisphere case
;     and 'S' for a Southern hemisphere case.  The day of the year determines
;     'b':  'W' for Winter, 'S' for Summer, and 'E' for Equinox.  The magnetic
;     Kp index determines 'c':  'L' for Low, 'M' for Moderate, and 'H' for
;     High.  The solar F10.7 index determines 'd':  'L' for Low, 'M' for
;     Moderate, and 'H' for High.  The direction of By determines 'ee':  'BC'
;     for By positive and 'DE' for By negative.   Logical flag USEDEC
;     determines how the month descriptor is chosen.  Logical flag USEAP
;     determines how the magnetic activity level descriptor is chosen.
;
;  INPUT PARAMETERS
;     BY       The direction of By, '+' or '-'
;     MLAT     The magnetic latitude, in degrees north
;     USEAP    A logical flag, 1 if Ap is to be used to determine the magnetic
;              activity level descriptor, 0 if Kp is to be used
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the season descriptor, 0 if the day of the year is to
;              be used
;     XDAY     The day of the year
;     XF10P7   The 10.7 cm solar flux, in solar flux units
;     XKP      The magnetic Kp index
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     AP       The magnetic Ap index
;     DEC      The solar declination, in degrees north
;     HEMIS    The hemisphere descriptor, 'N' for a northern hemisphere case,
;              'S' for a southern hemisphere case
;     HMPATT   The Heppner-Maynard convection pattern descriptor, 'BC' for By
;              positive, 'DE' for By negative
;     MAGACT   The magnetic activity level descriptor, 'L' for low, 'M' for
;              moderate, and 'H' for high
;     SEASON   The season descriptor, 'W' for winter, 'S' for summer, and 'E'
;              for equinox
;     SOLACT   The solar activity level descriptor, 'L' for low, 'M' for
;              moderate, and 'H' for high
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     KPTOAP   Converts Kp to Ap
;     SOLDEC   Determines the solar declination from the day of the year
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the day of the year, magnetic latitude, 10.7 cm solar flux, magnetic Kp
;  index, direction of By, logical flag USEDEC, or logical flag USEAP are
;  different than those of the previous call, then the case identifier must be
;  redetermined
;
IF (XDAY NE PRVDAY) OR (MLAT NE PRVMLA) OR (XF10P7 NE PRVF10) OR $
   (XKP NE PRVKP) OR (BY NE PRVBY) OR ((USEDEC EQ 1) XOR (PRVUSD EQ 1)) OR $
   ((USEAP EQ 1) XOR (PRVUSA EQ 1)) THEN BEGIN
;
;  Determine the hemisphere descriptor from the magnetic latitude
;
   IF MLAT GE 0. THEN HEMIS='N' ELSE HEMIS='S'
;
;  Determine the season descriptor from the solar declination
;  Note: If you want to use the solar declination to determine the season
;        descriptor, then the logical flag USEDEC should be set to 1; if you
;        want to use the day of the year directly, then the logical flag USEDEC
;        should be set to 0.  Solar declination is a better indicator of
;        season; however, you may find it more convenient to work with the day
;        of the year directly.
;
   IF USEDEC EQ 1 THEN BEGIN
      DEC=SOLDEC(FLOAT(XDAY))
      IF DEC LE -23.5/2. THEN BEGIN
         IF MLAT GE 0. THEN SEASON='W' ELSE SEASON='S'
      ENDIF ELSE IF ABS(DEC) LT 23.5/2. THEN BEGIN
         SEASON='E'
      ENDIF ELSE BEGIN
         IF MLAT GE 0. THEN SEASON='S' ELSE SEASON='W'
      ENDELSE
;
;  Determine the season descriptor directly from the day of the year
;  Note: If you want to use the solar declination to determine the season
;        descriptor, then the logical flag USEDEC should be set to 1; if you
;        want to use the day of the year directly, then the logical flag USEDEC
;        should be set to 0.  Solar declination is a better indicator of
;        season; however, you may find it more convenient to work with the day
;        of the year directly.
;
   ENDIF ELSE BEGIN
     IF XDAY LE 82 THEN BEGIN
        IF 365+XDAY-357 LE 82-XDAY THEN BEGIN
           IF MLAT GE 0. THEN SEASON='W' ELSE SEASON='S'
        ENDIF ELSE BEGIN
           SEASON='E'
        ENDELSE
     ENDIF ELSE IF (XDAY GT 82) AND (XDAY LE 173) THEN BEGIN
        IF XDAY-82 LE 173-XDAY THEN BEGIN
           SEASON='E'
        ENDIF ELSE BEGIN
           IF MLAT GE 0. THEN SEASON='S' ELSE SEASON='W'
        ENDELSE
     ENDIF ELSE IF (XDAY GT 173) AND (XDAY LE (173+357)/2) THEN BEGIN
        IF XDAY-173 LE (173+357)/2-XDAY THEN BEGIN
           IF MLAT GE 0. THEN SEASON='S' ELSE SEASON='W'
        ENDIF ELSE BEGIN
           SEASON='E'
        ENDELSE
     ENDIF ELSE IF (XDAY GT (173+357)/2) AND (XDAY LE 357) THEN BEGIN
        IF XDAY-(173+357)/2 LE 357-XDAY THEN BEGIN
           SEASON='E'
        ENDIF ELSE BEGIN
           IF MLAT GE 0. THEN SEASON='W' ELSE SEASON='S'
        ENDELSE
     ENDIF ELSE BEGIN
        IF MLAT GE 0. THEN SEASON='W' ELSE SEASON='S'
     ENDELSE
   ENDELSE
;
;  Determine the magnetic activity level descriptor from Ap
;  Note: If you want to use Ap to determine the magnetic activity level
;        descriptor, then the logical flag USEAP should be set to 1; if you
;        want to use Kp directly, then the logical flag USEAP should be set to
;        0.  Ap is a better indicator of magnetic activity level because it is
;        quasi-linear while Kp is quasi-logarithmic; however, you may find it
;        more convenient to work with Kp directly.
;
   IF USEAP EQ 1 THEN BEGIN
      AP=KPTOAP(XKP)
      IF AP LE KPTOAP(1.) THEN BEGIN
         MAGACT='L'
      ENDIF ELSE IF (AP GT KPTOAP(1.)) AND (AP LE KPTOAP(3.5)) THEN BEGIN
         IF AP-KPTOAP(1.) LE KPTOAP(3.5)-AP THEN MAGACT='L' ELSE MAGACT='M'
      ENDIF ELSE IF (AP GT KPTOAP(3.5)) AND (AP LE KPTOAP(6.)) THEN BEGIN
         IF AP-KPTOAP(3.5) LE KPTOAP(6.)-AP THEN MAGACT='M' ELSE MAGACT='H'
      ENDIF ELSE BEGIN
         MAGACT='H'
      ENDELSE
;
;  Determine the magnetic activity level descriptor directly from Kp
;  Note: If you want to use Ap to determine the magnetic activity level
;        descriptor, then the logical flag USEAP should be set to 1; if you
;        want to use Kp directly, then the logical flag USEAP should be set to
;        0.  Ap is a better indicator of magnetic activity level because it is
;        quasi-linear while Kp is quasi-logarithmic; however, you may find it
;        more convenient to work with Kp directly.
;
   ENDIF ELSE BEGIN
      IF XKP LE 1. THEN BEGIN
         MAGACT='L'
      ENDIF ELSE IF (XKP GT 1.) AND (XKP LE 3.5) THEN BEGIN
         IF XKP-1. LE 3.5-XKP THEN MAGACT='L' ELSE MAGACT='M'
      ENDIF ELSE IF (XKP GT 3.5) AND (XKP LE 6.) THEN BEGIN
         IF XKP-3.5 LE 6.-XKP THEN MAGACT='M' ELSE MAGACT='H'
      ENDIF ELSE BEGIN
         MAGACT='H'
      ENDELSE
   ENDELSE
;
;  Determine the solar activity level descriptor from F10.7
;
   IF XF10P7 LE 70. THEN BEGIN
      SOLACT='L'
   ENDIF ELSE IF (XF10P7 GT 70.) AND (XF10P7 LE 130.) THEN BEGIN
      IF XF10P7-70. LE 130.-XF10P7 THEN SOLACT='L' ELSE SOLACT='M'
   ENDIF ELSE IF (XF10P7 GT 130.) AND (XF10P7 LE 210.) THEN BEGIN
      IF XF10P7-130. LE 210.-XF10P7 THEN SOLACT='M' ELSE SOLACT='H'
   ENDIF ELSE BEGIN
      SOLACT='H'
   ENDELSE
;
;  Determine the Heppner-Maynard convection pattern descriptor
;
IF BY EQ '+' THEN BEGIN
   HMPATT='BC'
ENDIF ELSE IF BY EQ '-' THEN BEGIN
   HMPATT='DE'
ENDIF ELSE BEGIN
   STOP,'USU_ID:Invalid By direction.'
ENDELSE
;
;  Determine the case identifier
;
ID=HEMIS+'H'+SEASON+MAGACT+SOLACT+HMPATT
;
ENDIF
;
RETURN
END
PRO USUION,SPEC
;
;  PURPOSE
;     To determine the ion species descriptor.
;
;  METHOD
;     The ion species descriptor is a code for describing the ion.  It is
;     is determined by the ion species.
;
;  INPUT PARAMETERS
;     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     None
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the ion species is different than that of the previous call, then the ion
;  species descriptor must be redetermined
;
IF SPEC NE PRVSPE THEN BEGIN
;
;  Determine the ion species descriptor
;
   IF STRUPCASE(STRCOMPRESS(SPEC,/REMOVE_ALL)) EQ 'NO+' THEN BEGIN
      ION='NO'
   ENDIF ELSE IF STRUPCASE(STRCOMPRESS(SPEC,/REMOVE_ALL)) EQ 'O2+' THEN BEGIN
      ION='O2'
   ENDIF ELSE IF STRUPCASE(STRCOMPRESS(SPEC,/REMOVE_ALL)) EQ 'O+' THEN BEGIN
      ION='O'
   ENDIF ELSE BEGIN
      STOP,'USUION:Invalid ion species.'
   ENDELSE
;
ENDIF
;
RETURN
END
PRO USUEOF,PATH,LUN,PCIDL,NALT,ALT
;
;  PURPOSE
;     To read a USU EOF file.
;
;  METHOD
;     The name of the USU EOF file is determined from the case identifier and
;     ion species descriptor.  Header information, followed by the altitude
;     grid, eigenvalues, and EOFs are read from the USU EOF file.
;
;  INPUT PARAMETERS
;     LUN      The logical unit number used to access the USU EOF file
;     PATH     The location of the USU EOF file
;     PCIDL    A logical flag, 1 if the IBM-PC/Windows IDL convention for
;              associated file variables is to be used, 0 if the VAX/VMS IDL
;              convention is to be used
;
;  OUTPUT PARAMETERS
;     ALT      The altitude grid, in km
;     NALT     The number of altitude points
;
;  LOCAL VARIABLES
;     A        An associated file variable
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     #LUN     The USU EOF file
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MALT     The maximum number of altitude points
;
MALT=37
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the case identifier or ion species descriptor are different than those of
;  the previous call, then the USU EOF file must be read
;
IF (ID NE PRVID) OR (ION NE PRVION) THEN BEGIN
;
;  Open the USU EOF file
;
   OPENR,LUN,STRLOWCASE(STRCOMPRESS(PATH+ID+'.'+ION+'E',/REMOVE_ALL))
;
;  Read header information from the USU EOF file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(MALT,/NOZERO))
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(3,/NOZERO))
   ENDELSE
   YEAR=FIX(A(0,0))
   DAY=FIX(A(1,0))
   F10P7=A(0,1)
   AP=A(1,1)
   KP=A(2,1)
;
;  Read the altitude grid from the USU EOF file
;
   NALT=FIX(A(0,2))
   A=ASSOC(LUN,FLTARR(NALT,/NOZERO))
   ALT=A(3)
;
;  Read the eigenvalues from the USU EOF file
;
   D=A(4)
   DSHFT=A(5)
;
;  Read the EOFs from the USU EOF file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(NALT,NALT,/NOZERO),6*NALT*4)
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(NALT,NALT,/NOZERO),6)
   ENDELSE
   EOFS=A(0)
;
;  Close the USU EOF file
;
   CLOSE,LUN
;
ENDIF
;
RETURN
END
PRO USUOPC,PATH,LUN,UT,PCIDL
;
;  PURPOSE
;     To read a USU orthogonal polynomial coefficients file.
;
;  METHOD
;     The name of the USU orthogonal polynomial coefficients file is determined
;     from the case identifier and ion species descriptor.  Header information
;     is read from the USU orthogonal polynomial coefficients file.  The
;     orthogonal polynomial coefficients for the given universal time are read
;     from the USU orthogonal polynomial coefficients file.  If the given
;     universal time does not lie exactly on the universal time grid, then the
;     orthogonal polynomial coefficients are linearly interpolated from the
;     orthogonal polynomial coefficients of the two nearest universal times on
;     the universal time grid.
;
;  INPUT PARAMETERS
;     LUN      The logical unit number used to access the USU orthogonal
;              polynomial coefficients file
;     PATH     The location of the USU orthogonal polynomial coefficients file
;     PCIDL    A logical flag, 1 if the IBM-PC/Windows IDL convention for
;              associated file variables is to be used, 0 if the VAX/VMS IDL
;              convention is to be used
;     UT       The universal time, in decimal hours
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     A        An associated file variable
;     IUT      The universal time index
;     IUT1     A universal time index used for interpolation
;     IUT2     A universal time index used for interpolation
;     T        An interpolation factor
;     UTG      The grid universal time, in decimal hours
;     UTR      The universal time, in decimal hours, restricted to the
;              range [>=0,<24]
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     #LUN     The USU orthogonal polynomial coefficients file
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MFC      The maximum number of Fourier coefficients
;
MFC=9
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the case identifier, ion species descriptor, or universal time are
;  different than those of the previous call, then the USU orthogonal
;  polynomial coefficients file must be read
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) THEN BEGIN
;
;  Open the USU orthogonal polynomial coefficients file
;
   OPENR,LUN,STRLOWCASE(STRCOMPRESS(PATH+ID+'.'+ION+'P',/REMOVE_ALL))
;
;  Read header information from the USU orthogonal polynomial coefficients file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(MFC,/NOZERO))
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(4,/NOZERO))
   ENDELSE
   YEAR=FIX(A(0,0))
   DAY=FIX(A(1,0))
   DELTA=A(2,0)
   F10P7=A(0,1)
   AP=A(1,1)
   NFC=FIX(A(2,1))
   NOPM1=FIX(A(3,1))
   KP=A(0,2)
   NEOF=FIX(A(1,2))
   NUT=FIX(A(0,3))
   SUT=A(1,3)
   DUT=A(2,3)
   NMLAT=FIX(A(0,4))
   SMLAT=ABS(A(1,4))
   DMLAT=ABS(A(2,4))
   NMLT=FIX(A(0,5))
   SMLT=A(1,5)
   DMLT=A(2,5)
;
;  Associate a variable with the USU orthogonal polynomial coefficients file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(NFC,NOPM1+1,NEOF,/NOZERO),6*NFC*4)
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(NFC,NOPM1+1,NEOF,/NOZERO),6)
   ENDELSE
;
;  Restrict the universal time to the range [>=0,<24]
;
   UTR=(24.+(UT MOD 24.)) MOD 24.
;
;  Calculate the universal time index
;
   IUT=FIX((UTR-SUT)/DUT)
;
;  Calculate the grid universal time
;
   UTG=SUT+DUT*FLOAT(IUT)
;
;  If the universal time is exactly on the universal time grid, then the
;  orthogonal polynomial coefficients can be read directly from the USU
;  orthogonal polynomial coefficients file without interpolation
;
   IF UTR EQ UTG THEN BEGIN
      OPC=A(IUT)
;
;  Otherwise, the orthogonal polynomial coefficients are interpolated from the
;  orthogonal polynomial coefficients of the two nearest universal times
;
   ENDIF ELSE BEGIN
;
;  Determine the indices of the grid universal times to be used in the
;  interpolation and the interpolation factor
;
      IF UTR LT SUT THEN BEGIN
         IUT1=NUT-1
         IUT2=0
         T=(UTR-SUT)/DUT+1.
      ENDIF ELSE BEGIN
         IF IUT EQ NUT THEN BEGIN
            IUT1=NUT-1
            IUT2=0
         ENDIF ELSE BEGIN
            IUT1=IUT
            IUT2=IUT+1
         ENDELSE
         T=(UTR-UTG)/DUT
      ENDELSE
;
;  Interpolate the orthogonal polynomial coefficients from the orthogonal
;  polynomial coefficients at the two nearest universal times on the
;  universal time grid
;
      OPC=(1.-T)*A(IUT1)+T*A(IUT2)
;
   ENDELSE
;
;  Close the USU orthogonal polynomial coefficients file
;
   CLOSE,LUN
;
ENDIF
;
RETURN
END
PRO USUFC,UT,MLAT
;
;  PURPOSE
;     To generate Fourier coefficients from orthogonal polynomial coefficients
;     for USU.
;
;  METHOD
;     A set of orthogonal polynomials is determined.  The orthogonal
;     polynomials are combined with the orthogonal polynomial coefficients to
;     produce Fourier coefficients for the given magnetic latitude.  If the
;     magnetic latitude does not lie exactly on the magnetic latitude grid,
;     then the orthogonal polynomial is linearly interpolated from orthogonal
;     polynomials at the two nearest magnetic latitudes on the magnetic
;     latitude grid.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     UT       The universal time, in decimal hours
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     I        A loop counter
;     IMLAT    The magnetic latitude index
;     IMLAT1   A magnetic latitude index used for interpolation
;     IMLAT2   A magnetic latitude index used for interpolation
;     MLATG    The grid magnetic latitude, in degrees north
;     OP       Orthogonal polynomials
;     OPINT    An interpolated orthogonal polynomial
;     T        An interpolation factor
;     X        The grid for the orthogonal polynomials
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     USUOP    Generates the orthogonal polynomials
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
;        The Golam Press, Boulder, CO, 1973, pp 90-92.
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the case identifier, ion species descriptor, universal time, or magnetic
;  latitude are different than those of the previous call, then the Fourier
;  coefficients must be regenerated
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) OR (MLAT NE PRVMLA) $
   THEN BEGIN
;
;  Calculate the grid for the orthogonal polynomials
;
   X=DELTA*FINDGEN(NMLAT)-1.
;
;  Generate the orthogonal polynomials
;
   OP=USUOP(NOPM1,NMLAT,X)
;
;  Calculate the magnetic latitude index
;
   IMLAT=MIN([NMLAT-1,MAX([0,FIX((ABS(MLAT)-SMLAT)/DMLAT)])])
;
;  Calculate the grid magnetic latitude
;
   MLATG=SMLAT+DMLAT*FLOAT(IMLAT)
;
;  Define the Fourier coefficients array
;
   FC=FLTARR(NFC,NEOF)
;
;  If the magnetic latitude is exactly on the magnetic latitude grid, then no
;  interpolation of the orthogonal polynomials in magnetic latitude is
;  necessary
;
   IF ABS(MLAT) EQ MLATG THEN BEGIN
;
;  For each Fourier coefficient, sum the contributions of the orthogonal
;  polynomials
;
      FOR I=0,NEOF-1 DO FC(*,I)=OPC(*,*,I)#TRANSPOSE(OP(IMLAT,*))
;
;  If the magnetic latitude is not exactly on the magnetic latitude grid, then
;  interpolation of the orthogonal polynomials in magnetic latitude is
;  necessary
;
   ENDIF ELSE BEGIN
;
;  Determine the indices of the two nearest magnetic latitudes on the magnetic
;  latitude grid and the magnetic latitude interpolation factor
;
      IF IMLAT EQ NMLAT-1 THEN BEGIN
         IMLAT1=NMLAT-2
         IMLAT2=NMLAT-1
         T=(ABS(MLAT)-MLATG)/DMLAT+1.
      ENDIF ELSE BEGIN
         IMLAT1=IMLAT
         IMLAT2=IMLAT+1
         T=(ABS(MLAT)-MLATG)/DMLAT
      ENDELSE
;
;  Linearly interpolate the orthogonal polynomial from the orthogonal
;  polynomials at the two nearest magnetic latitudes on the magnetic latitude
;  grid
;
      OPINT=TRANSPOSE((1.-T)*OP(IMLAT1,*)+T*OP(IMLAT2,*))
;
;  Interpolate the Fourier coefficients from the Fourier coefficients at the
;  two nearest magnetic latitudes on the magnetic latitude grid
;
      FOR I=0,NEOF-1 DO FC(*,I)=OPC(*,*,I)#OPINT
;
   ENDELSE
;
ENDIF
;
RETURN
END
FUNCTION USUOP,NOPM1,NX,X
;
;  PURPOSE
;     To generate orthogonal polynomials.
;
;  METHOD
;     See the reference for a description of orthogonal polynomials.
;
;  INPUT PARAMETERS
;     NOPM1    The number of orthogonal polynomials - 1
;     NX       The number of points on the grid for the orthogonal polynomials
;     X        The grid for the orthogonal polynomials
;
;  OUTPUT PARAMETERS
;     U        Orthogonal polynomials
;
;  LOCAL VARIABLES
;     B        A term in the recursion relation
;     H2       A term in the recursion relation
;     IOP      The loop counter for orthogonal polynomials
;     IX       The loop counter for the grid for the orthogonal polynomials
;     RATIO    A term in the recursion relation
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
;        The Golam Press, Boulder, CO, 1973, pp 90-92.
;
;  SPECIAL CONSTANTS
;     None
;
;  Define the orthogonal polynomials array
;
U=FLTARR(NX,NOPM1+1)
;
;  Define auxiliary arrays
;
H2=FLTARR(NOPM1+1)
B=FLTARR(NOPM1+1)
;
;  Calculate U(X,0)
;
H2(0)=FLOAT(NX)
B(0)=0.
FOR IX=0,NX-1 DO BEGIN
   U(IX,0)=1.
   B(0)=B(0)+X(IX)
ENDFOR
B(0)=B(0)/H2(0)
;
;  Calculate U(X,1)
;
H2(1)=0.
B(1)=0.
FOR IX=0,NX-1 DO BEGIN
   U(IX,1)=X(IX)-B(0)
   H2(1)=H2(1)+U(IX,1)^2
   B(1)=B(1)+X(IX)*(U(IX,1)^2)
ENDFOR
B(1)=B(1)/H2(1)
;
;  Use the recursion relationship to calculate U(X,N) where N=2,NOPM1
;
FOR IOP=2,NOPM1 DO BEGIN
   H2(IOP)=0.
   B(IOP)=0.
   RATIO=H2(IOP-1)/H2(IOP-2)
   FOR IX=0,NX-1 DO BEGIN
      U(IX,IOP)=(X(IX)-B(IOP-1))*U(IX,IOP-1)-RATIO*U(IX,IOP-2)
      H2(IOP)=H2(IOP)+U(IX,IOP)^2
      B(IOP)=B(IOP)+X(IX)*(U(IX,IOP)^2)
   ENDFOR
   B(IOP)=B(IOP)/H2(IOP)
ENDFOR
;
RETURN,U
END
PRO USUEC,UT,MLT,MLAT
;
;  PURPOSE
;     To generate EOF coefficients from Fourier coefficients for USU.
;
;  METHOD
;     The Fourier coefficients are combined with sines and cosines of magnetic
;     local time to produce EOF coefficients at the given magnetic local time.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     UT       The universal time, in decimal hours
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     ATERM    The angular terms of the Fourier expansion
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Waltham, MA 02154  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the case identifier, ion species descriptor, universal time, magnetic
;  local time, or magnetic latitude are different than those of the previous
;  call, then the EOF coefficients must be regenerated
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) OR (MLT NE PRVMLT) OR $
   (MLAT NE PRVMLA) THEN BEGIN
;
;  Define the angular terms for the Fourier expansion
;
   ATERM=[.5,COS((FINDGEN((NFC-1)/2)+1.)*!PI*MLT/12.), $
             SIN((FINDGEN((NFC-1)/2)+1.)*!PI*MLT/12.)]
;
;  For each EOF coefficient, sum the contributions of the Fourier terms
;
   EC=TRANSPOSE(FC)#ATERM
;
ENDIF
;
RETURN
END
PRO USUDEN,UT,MLT,MLAT,NALT,DEN
;
;  PURPOSE
;     To construct a USU ion density altitude profile from EOFs and EOF
;     coefficients.
;
;  METHOD
;     The EOFs are combined with the EOF coefficients to produce the natural
;     logarithm of an ion density altitude profile.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     NALT     The number of altitude points
;     UT       The universal time, in decimal hours
;
;  OUTPUT PARAMETERS
;     DEN      The ion density altitude profile, in cm-3
;
;  LOCAL VARIABLES
;     None
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  If the case identifier, ion species descriptor, universal time, magnetic
;  local time, or magnetic latitude are different than those of the previous
;  call, then the ion density altitude profile must be reconstructed
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) OR (MLT NE PRVMLT) OR $
   (MLAT NE PRVMLA) THEN BEGIN
;
;  Sum the contributions of the EOFs at each altitude
;
   DEN=EOFS(*,0:NEOF-1)#EC
;
;  Convert the result to an actual ion density by exponentiation
;
   DEN=EXP(DEN)
;
ENDIF
;
RETURN
END
PRO USUHIS,XDAY,UT,MLT,MLAT,XF10P7,XKP,BY,SPEC,USEDEC,USEAP
;
;  PURPOSE
;     To store historical quantities for the next call to USU.
;
;  METHOD
;     The day of the year, universal time, magnetic local time, magnetic
;     latitude, 10.7 cm solar flux, magnetic Kp index, direction of By, ion
;     species, logical flag USEDEC, logical flag USEAP, case identifier, and
;     ion species descriptor are stored for the next call to USU.
;
;  INPUT PARAMETERS
;     BY       The direction of By, '+' or '-'
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
;     USEAP    A logical flag, 1 if Ap is to be used to determine the magnetic
;              activity level descriptor, 0 if Kp is to be used
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the month descriptor, 0 if the day of the year is to
;              be used
;     UT       The universal time, in decimal hours
;     XDAY     The day of the year
;     XF10P7   The 10.7 cm solar flux, in solar flux units
;     XKP      The magnetic Kp index
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     None
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.0   21-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        21-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBUSU
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     FC       Fourier coefficients
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and 'O'
;              for O+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NFC      The number of Fourier coefficients
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVBY    The direction of By from the previous call
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
;              'O2' for O2+, and 'O' for O+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBUSU,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,SMLAT,DMLAT, $
       NUT,SUT,DUT,D,DSHFT,EOFS,NFC,NOPM1,NEOF,DELTA,OPC,FC,EC,PRVDAY, $
       PRVUT,PRVMLT,PRVMLA,PRVF10,PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID, $
       PRVION
;
;  Store the day of the year
;
PRVDAY=XDAY
;
;  Store the universal time
;
PRVUT=UT
;
;  Store the magnetic local time
;
PRVMLT=MLT
;
;  Store the magnetic latitude
;
PRVMLA=MLAT
;
;  Store the 10.7 cm solar flux
;
PRVF10=XF10P7
;
;  Store the magnetic Kp index
;
PRVKP=XKP
;
;  Store the direction of By
;
PRVBY=BY
;
;  Store the ion species
;
PRVSPE=SPEC
;
;  Store the logical flag USEDEC
;
PRVUSD=USEDEC
;
;  Store the logical flag USEAP
;
PRVUSA=USEAP
;
;  Store the case identifier
;
PRVID=ID
;
;  Store the ion species descriptor
;
PRVION=ION
;
RETURN
END
