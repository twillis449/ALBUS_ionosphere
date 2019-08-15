PRO LME,PATH,LUN,DAY,UT,MLAT,MLON,F10P7,KP,SPEC,USEDEC,USEAP,PCIDL,NALT,ALT,DEN
;
;  PURPOSE
;     A driver for constructing NO+ and O2+ density altitude profiles from the
;     parameterized low- and mid- latitude LME database using modular LME
;     database access and manipulation routines.
;
;  METHOD
;     From the given input parameters, the LME case identifier and ion species
;     descriptor are determined.  EOFs for the case and ion are read from the
;     appropriate LME EOF file.  Orthogonal polynomial coefficients for the
;     case, ion, and universal time are read from the appropriate LME
;     orthogonal polynomial coefficients file.  The orthogonal polynomial
;     coefficients describe the behavior of EOF coefficients in magnetic
;     latitude for a given magnetic longitude and universal time.  EOF
;     coefficients are determined from the orthogonal polynomial coefficients.
;     The EOF coefficients, in combination with the EOFs, describe the ion
;     density altitude profile for a given magnetic latitude, magnetic
;     longitude, and universal time.
;
;     Before LME can be called with parameters, it must be called once with
;     no parameters to initialize common block CBLME.
;
;  INPUT PARAMETERS
;     DAY      The day of the year
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     KP       The magnetic Kp index
;     LUN      The logical unit number used to access LME database files
;     MLAT     The magnetic latitude, in degrees north
;     MLON     The magnetic longitude, in degrees east
;     PATH     The location of LME database files
;     PCIDL    A logical flag, 1 if the IBM-PC/Windows IDL convention for
;              associated file variables is to be used, 0 if the VAX/VMS IDL
;              convention is to be used
;     SPEC     The ion species, 'NO+' or 'O2+'
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
;     LMEDEN   Generates an ion density altitude profile from EOFs and EOF
;              coefficients
;     LMEEC    Generates EOF coefficients from orthogonal polynomial
;              coefficients
;     LMEEOF   Reads an EOF file
;     LMEID    Determines the case identifier
;     LMEINI   Initializes common block CBLME
;     LMEION   Determines the ion species descriptor
;     LMEOPC   Reads an orthogonal polynomial coefficients file
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
;     1.1   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Added user-specified location and logical
;                                   unit number for LME database files.
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;                                   The calls to LMEFC and LMEEC have been
;                                   combined into a single call to LMEEC to
;                                   reflect the elimination of Fourier fitting
;                                   in magnetic longitude in the low- and mid-
;                                   latitude E-region parameterized model
;                                   (LME).
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Initialize common block CBLME on an initialization call to LME (with no
;  arguments)
;
IF N_PARAMS() EQ 0 THEN BEGIN
   LMEINI
;
;  Generate an ion density profile on a normal call to LME (with arguments)
;
ENDIF ELSE BEGIN
;
;  Determine the case identifier
;
   LMEID,DAY,F10P7,KP,USEDEC,USEAP
;
;  Determine the ion species descriptor
;
   LMEION,SPEC
;
;  Read the LME EOF file
;
   LMEEOF,PATH,LUN,PCIDL,NALT,ALT
;
;  Read the LME orthogonal polynomial coefficients file
;
   LMEOPC,PATH,LUN,UT,PCIDL
;
;  Generate EOF coefficients from the orthogonal polynomial coefficients
;
   LMEEC,UT,MLAT,MLON
;
;  Generate an ion density altitude profile from the EOFs and EOF coefficients
;
   LMEDEN,UT,MLAT,MLON,NALT,DEN
;
;  Store historical quantities for the next call to LME
;
   LMEHIS,DAY,UT,MLAT,MLON,F10P7,KP,SPEC,USEDEC,USEAP
;
ENDELSE
;
RETURN
END
PRO LMEINI
;
;  PURPOSE
;     To initialize the contents of common block CBLME.
;
;  METHOD
;     Common block CBLME is intended for internal use by the LME modules.
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
;     1.0   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         4-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MALT     The maximum number of altitude points
;     MEOF     The maximum number of EOFs
;     MMLON    The maximum number of magnetic longitudes
;     MOPM1    The maximum number of orthogonal polynomials - 1
;
MALT=28
MEOF=28
MMLON=24
MOPM1=19
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  Initialize common block CBLME
;
ID='      '
ION='  '
YEAR=0
DAY=0
F10P7=0.
AP=0.
KP=0.
NMLON=0
SMLON=0.
DMLON=0.
NMLAT=0
SMLAT=0.
DMLAT=0.
NUT=0
SUT=0.
DUT=0.
D=FLTARR(MEOF)
DSHFT=FLTARR(MEOF)
EOFS=FLTARR(MALT,MEOF)
NOPM1=0
NEOF=0
DELTA=0.
OPC=FLTARR(MOPM1+1,MEOF,MMLON)
EC=FLTARR(MEOF)
PRVDAY=9999
PRVUT=9999.
PRVMLA=9999.
PRVMLO=9999.
PRVF10=9999.
PRVKP=9999.
PRVSPE='999'
PRVUSD=9999
PRVUSA=9999
PRVID='999999'
PRVION='99'
;
RETURN
END
PRO LMEID,XDAY,XF10P7,XKP,USEDEC,USEAP
;
;  PURPOSE
;     To determine an LME case identifier.
;
;  METHOD
;     The LME case identifier is a code for describing the ambient conditions
;     (day of the year, magnetic activity level, and solar activity level) of
;     an LME case.  The case identifier has the form 'LMnnab', where 'LM'
;     defines the LME case as low- and mid-latitude, 'nn' describes the month,
;     'a' describes the magnetic activity level, and 'b' describes the solar
;     activity level.  The day of the year determines 'nn':  '03' for March
;     (equinox), '06' for June (solstice), and '12' for December (solstice).
;     The magnetic Kp index determines 'a':  'L' for Low, 'M' for Moderate,
;     and 'H' for High.  The solar F10.7 index determines 'b':  'L' for Low,
;     'M' for Moderate, and 'H' for High.  Logical flag USEDEC determines how
;     the month descriptor is chosen.  Logical flag USEAP determines how the
;     magnetic activity level descriptor is chosen.
;
;  INPUT PARAMETERS
;     USEAP    A logical flag, 1 if Ap is to be used to determine the magnetic
;              activity level descriptor, 0 if Kp is to be used
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the month descriptor, 0 if the day of the year is to
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
;     MAGACT   The magnetic activity level descriptor, 'L' for low, 'M' for
;              moderate, and 'H' for high
;     MONTH    The month descriptor, '03' for March, '06' for June, and '12'
;              for December
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
;     1.2   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown        13-Nov-1991   1.0 ==> 1.1
;                                   Complete rewrite to simplify coding and to
;                                   correct a bug in the determination of the
;                                   month descriptor.
;                                   The selection of month and magnetic
;                                   activity level have been enhanced as well
;                                   (see the comments below).
;     L. Brown         4-Oct-1994   1.1 ==> 1.2
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;                                   The form of the case identifier has been
;                                   changed to a use 2-digit month for all
;                                   three seasons ('03', '06', and '12').
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  If the day of the year, 10.7 cm solar flux, magnetic Kp index, logical flag
;  USEDEC, or logical flag USEAP are different than those of the previous call,
;  then the case identifier must be redetermined
;
IF (XDAY NE PRVDAY) OR (XF10P7 NE PRVF10) OR (XKP NE PRVKP) OR $
   ((USEDEC EQ 1) XOR (PRVUSD EQ 1)) OR ((USEAP EQ 1) XOR (PRVUSA EQ 1)) $
   THEN BEGIN
;
;  Determine the month descriptor from the solar declination
;  Note: If you want to use the solar declination to determine the month
;        descriptor, then the logical flag USEDEC should be set to 1; if you
;        want to use the day of the year directly, then the logical flag USEDEC
;        should be set to 0.  Solar declination is a better indicator of
;        season; however, you may find it more convenient to work with the day
;        of the year directly.
;
   IF USEDEC EQ 1 THEN BEGIN
      DEC=SOLDEC(FLOAT(XDAY))
      IF DEC LE -23.5/2. THEN BEGIN
         MONTH='12'
      ENDIF ELSE IF ABS(DEC) LT 23.5/2. THEN BEGIN
         MONTH='03'
      ENDIF ELSE BEGIN
         MONTH='06'
      ENDELSE
;
;  Determine the month descriptor directly from the day of the year
;  Note: If you want to use the solar declination to determine the month
;        descriptor, then the logical flag USEDEC should be set to 1; if you
;        want to use the day of the year directly, then the logical flag USEDEC
;        should be set to 0.  Solar declination is a better indicator of
;        season; however, you may find it more convenient to work with the day
;        of the year directly.
;
   ENDIF ELSE BEGIN
      IF XDAY LE 82 THEN BEGIN
         IF 365+XDAY-357 LE 82-XDAY THEN BEGIN
            MONTH='12'
         ENDIF ELSE BEGIN
            MONTH='03'
         ENDELSE
      ENDIF ELSE IF (XDAY GT 82) AND (XDAY LE 173) THEN BEGIN
         IF XDAY-82 LE 173-XDAY THEN BEGIN
            MONTH='03'
         ENDIF ELSE BEGIN
            MONTH='06'
         ENDELSE
      ENDIF ELSE IF (XDAY GT 173) AND (XDAY LE (173+357)/2) THEN BEGIN
         IF XDAY-173 LE (173+357)/2-XDAY THEN BEGIN
            MONTH='06'
         ENDIF ELSE BEGIN
            MONTH='03'
         ENDELSE
      ENDIF ELSE IF (XDAY GT (173+357)/2) AND (XDAY LE 357) THEN BEGIN
         IF XDAY-(173+357)/2 LE 357-XDAY THEN BEGIN
            MONTH='03'
         ENDIF ELSE BEGIN
            MONTH='12'
         ENDELSE
      ENDIF ELSE BEGIN
         MONTH='12'
      ENDELSE
   ENDELSE
;
;  Determine the magnetic activity level descriptor from Ap
;  Note: If you want to use Ap to determine the magnetic activity level
;        descriptor, then the logical flag USEAP should be set to 1; if you
;        want to use Kp directly, then the logial flag USEAP should be set to
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
;        want to use Kp directly, then the logial flag USEAP should be set to
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
;  Determine the case identifier
;
   ID='LM'+MONTH+MAGACT+SOLACT
;
ENDIF
;
RETURN
END
PRO LMEION,SPEC
;
;  PURPOSE
;     To determine the ion species descriptor.
;
;  METHOD
;     The ion species descriptor is a code for describing the ion.  It is
;     is determined by the ion species.
;
;  INPUT PARAMETERS
;     SPEC     The ion species, 'NO+' or 'O2+'
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
;     1.1   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
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
   ENDIF ELSE BEGIN
      STOP,'LMEION:Invalid ion species.'
   ENDELSE
;
ENDIF
;
RETURN
END
PRO LMEEOF,PATH,LUN,PCIDL,NALT,ALT
;
;  PURPOSE
;     To read a LME EOF file.
;
;  METHOD
;     The name of the LME EOF file is determined from the case identifier and
;     ion species descriptor.  Header information, followed by the altitude
;     grid, eigenvalues, and EOFs are read from the LME EOF file.
;
;  INPUT PARAMETERS
;     LUN      The logical unit number used to access the LME EOF file
;     PATH     The location of the LME EOF file
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
;     #LUN     The LME EOF file
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.2   20-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Added user-specified location and logical
;                                   unit number for LME EOF file.
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;     L. Brown        20-Oct-1994   1.1 ==> 1.2
;                                   The full file specification of the LME EOF
;                                   file is converted to lower-case letters in
;                                   the OPENR statement.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MALT     The maximum number of altitude points
;
MALT=28
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  If the case identifier or ion species descriptor are different than those of
;  the previous call, then the LME EOF file must be read
;
IF (ID NE PRVID) OR (ION NE PRVION) THEN BEGIN
;
;  Open the LME EOF file
;
   OPENR,LUN,STRLOWCASE(STRCOMPRESS(PATH+ID+'.'+ION+'E',/REMOVE_ALL))
;
;  Read header information from the LME EOF file
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
   NMLON=FIX(A(0,2))
   SMLON=A(1,2)
   DMLON=A(2,2)
   NMLAT=FIX(A(0,3))
   SMLAT=A(1,3)
   DMLAT=A(2,3)
   NUT=FIX(A(0,4))
   SUT=A(1,4)
   DUT=A(2,4)
;
;  Read the altitude grid from the LME EOF file
;
   NALT=FIX(A(0,5))
   A=ASSOC(LUN,FLTARR(NALT,/NOZERO))
   ALT=A(6)
;
;  Read the eigenvalues from the LME EOF file
;
   D=A(7)
   DSHFT=A(8)
;
;  Read the EOFs from the LME EOF file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(NALT,NALT,/NOZERO),9*NALT*4)
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(NALT,NALT,/NOZERO),9)
   ENDELSE
   EOFS=A(0)
;
;  Close the LME EOF file
;
   CLOSE,LUN
;
ENDIF
;
RETURN
END
PRO LMEOPC,PATH,LUN,UT,PCIDL
;
;  PURPOSE
;     To read a LME orthogonal polynomial coefficients file.
;
;  METHOD
;     The name of the LME orthogonal polynomial coefficients file is determined
;     from the case identifier and ion species descriptor.  Header information
;     is read from the LME orthogonal polynomial coefficients file.  The
;     orthogonal polynomial coefficients for the given universal time are read
;     from the LME orthogonal polynomial coefficients file.  If the given
;     universal time does not lie exactly on the universal time grid, then the
;     orthogonal polynomial coefficients are linearly interpolated from the
;     orthogonal polynomial coefficients of the two nearest universal times on
;     the universal time grid.
;
;  INPUT PARAMETERS
;     LUN      The logical unit number used to access the LME orthogonal
;              polynomial coefficients file
;     PATH     The location of the LME orthogonal polynomial coefficients file
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
;     #LUN     The LME orthogonal polynomial coefficients file
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.2   20-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Added user-specified location and logical
;                                   unit number for LME orthogonal polynomial
;                                   coefficients file.
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;                                   The LME orthogonal polynomial coefficients
;                                   file has been restructured to reflect the
;                                   elimination of Fourier fitting in magnetic
;                                   longitude in the low- and mid- latitude
;                                   E-region parameterized model (LME).
;                                   The record-length of the orthogonal
;                                   polynomial coefficients file now depends on
;                                   the ion species.
;     L. Brown        20-Oct-1994   1.1 ==> 1.2
;                                   The full file specification of the LME
;                                   orthogonal polynomial coefficients file is
;                                   converted to lower-case letters in the
;                                   OPENR statement.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  If the case identifier, ion species descriptor, or universal time are
;  different than those of the previous call, then the LME orthogonal
;  polynomial coefficients file must be read
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) THEN BEGIN
;
;  Open the LME orthogonal polynomial coefficients file
;
   OPENR,LUN,STRLOWCASE(STRCOMPRESS(PATH+ID+'.'+ION+'P',/REMOVE_ALL))
;
;  Read header information from the LME orthogonal polynomial coefficients file
;
   IF PCIDL EQ 1 THEN BEGIN
      IF ION EQ 'NO' THEN NOPM1=12 ELSE NOPM1=19
      A=ASSOC(LUN,FLTARR(NOPM1+1,/NOZERO))
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(3,/NOZERO))
   ENDELSE
   YEAR=FIX(A(0,0))
   DAY=FIX(A(1,0))
   F10P7=A(0,1)
   AP=A(1,1)
   KP=A(2,1)
   NMLON=FIX(A(0,2))
   SMLON=A(1,2)
   DMLON=A(2,2)
   NMLAT=FIX(A(0,3))
   SMLAT=A(1,3)
   DMLAT=A(2,3)
   NUT=FIX(A(0,4))
   SUT=A(1,4)
   DUT=A(2,4)
   NOPM1=FIX(A(0,5))
   NEOF=FIX(A(1,5))
   DELTA=A(0,6)
;
;  Associate a variable with the LME orthogonal polynomial coefficients file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(NOPM1+1,NEOF,NMLON,/NOZERO),7*(NOPM1+1)*4)
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(NOPM1+1,NEOF,NMLON,/NOZERO),7)
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
;  orthogonal polynomial coefficients can be read directly from the LME
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
;  Close the LME orthogonal polynomial coefficients file
;
   CLOSE,LUN
;
ENDIF
;
RETURN
END
PRO LMEEC,UT,MLAT,MLON
;
;  PURPOSE
;     To generate EOF coefficients from orthogonal polynomial coefficients
;     for LME.
;
;  METHOD
;     A set of orthogonal polynomials is determined.  The orthogonal
;     polynomials are combined with the orthogonal polynomial coefficients to
;     produce EOF coefficients for the given magnetic latitude.  If the
;     magnetic latitude does not lie exactly on the magnetic latitude grid,
;     then the orthogonal polynomial is linearly interpolated from orthogonal
;     polynomials at the two nearest magnetic latitudes on the magnetic
;     latitude grid.  If the magnetic longitude does not lie exactly on the
;     magnetic longitude grid, then the orthogonal polynomial coefficients are
;     linearly interpolated from orthogonal polynomial coefficients at the two
;     nearest magnetic longitudes on the magnetic longitude grid.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLON     The magnetic longitude, in degrees east
;     UT       The universal time, in decimal hours
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     IMLAT    The magnetic latitude index
;     IMLAT1   A magnetic latitude index used for interpolation
;     IMLAT2   A magnetic latitude index used for interpolation
;     IMLON    The magnetic longitude index
;     IMLON1   A magnetic longitude index used for interpolation
;     IMLON2   A magnetic longitude index used for interpolation
;     MLATG    The grid magnetic latitude, in degrees north
;     MLONG    The grid magnetic longitude, in degrees east
;     MLONR    The magnetic longitude, in degrees east, restricted to the
;              range [>=0,<360]
;     OP       Orthogonal polynomials
;     OPINT    An interpolated orthogonal polynomial
;     TMLAT    The interpolation factor for magnetic latitude
;     TMLON    The interpolation factor for magnetic longitude
;     X        The grid for the orthogonal polynomials
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     LMEOP    Generates the orthogonal polynomials
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
;     1.1   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;                                   Renamed from LMEFC to LMEEC and modified to
;                                   reflect the elimination of Fourier fitting
;                                   in magnetic longitude in the low- and mid-
;                                   latitude E-region parameterized model
;                                   (LME).
;
;  REFERENCES
;     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
;        The Golam Press, Boulder, CO, 1973, pp 90-92.
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  If the case identifier, ion species descriptor, universal time, magnetic
;  latitude, or magnetic longitude are different than those of the previous
;  call, then the EOF coefficients must be regenerated
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) OR (MLAT NE PRVMLA) OR $
   (MLON NE PRVMLO) THEN BEGIN
;
;  Calculate the grid for the orthogonal polynomials
;
   X=DELTA*FINDGEN(NMLAT)-1.
;
;  Generate the orthogonal polynomials
;
   OP=LMEOP(NOPM1,NMLAT,X)
;
;  Calculate the magnetic latitude index
;
   IMLAT=MIN([NMLAT-1,MAX([0,FIX((MLAT-SMLAT)/DMLAT)])])
;
;  Calculate the grid magnetic latitude
;
   MLATG=SMLAT+DMLAT*FLOAT(IMLAT)
;
;  Restrict the magnetic longitude to the range [>=0,<360]
;
   MLONR=(360.+(MLON MOD 360.)) MOD 360.
;
;  Calculate the grid magnetic longitude index
;
   IMLON=FIX((MLONR-SMLON)/DMLON)
;
;  Calculate the grid magnetic longitude
;
   MLONG=SMLON+DMLON*FLOAT(IMLON)
;
;  If the magnetic latitude is exactly on the magnetic latitude grid, then no
;  interpolation of the orthogonal polynomials in magnetic latitude is
;  necessary
;
   IF MLAT EQ MLATG THEN BEGIN
;
;  If the magnetic longitude is exactly on the magnetic longitude grid, then no
;  interpolation of the orthogonal polynomial coefficients in magnetic
;  longitude is necessary
;
      IF MLONR EQ MLONG THEN BEGIN
;
;  Calculate the EOF coefficients
;
         EC=TRANSPOSE(OPC(*,*,IMLON))#TRANSPOSE(OP(IMLAT,*))
;
;  If the magnetic longitude is not exactly on the magnetic longitude grid,
;  then interpolation of the orthogonal polynomial coefficients in magnetic
;  longitude is necessary
;
      ENDIF ELSE BEGIN
;
;  Determine the indices of the two nearest magnetic longitudes on the magnetic
;  longitude grid and the magnetic longitude interpolation factor
;
         IF MLONR LT SMLON THEN BEGIN
            IMLON1=NMLON-1
            IMLON2=0
            TMLON=(MLONR-SMLON)/DMLON+1.
         ENDIF ELSE BEGIN
            IF IMLON EQ NMLON THEN BEGIN
               IMLON1=NMLON-1
               IMLON2=0
            ENDIF ELSE BEGIN
               IMLON1=IMLON
               IMLON2=IMLON+1
            ENDELSE
            TMLON=(MLONR-MLONG)/DMLON
         ENDELSE
;
;  Calculate the EOF coefficients, linearly interpolating the orthogonal
;  polynomial coefficients from the orthogonal polynomial coefficients at the
;  two nearest magnetic longitudes on the magnetic longitude grid
;
         EC=TRANSPOSE((1.-TMLON)*OPC(*,*,IMLON1)+TMLON*OPC(*,*,IMLON2)) $
           #TRANSPOSE(OP(IMLAT,*))
;
      ENDELSE
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
         TMLAT=(MLAT-MLATG)/DMLAT+1.
      ENDIF ELSE BEGIN
         IMLAT1=IMLAT
         IMLAT2=IMLAT+1
         TMLAT=(MLAT-MLATG)/DMLAT
      ENDELSE
;
;  Linearly interpolate the orthogonal polynomial from the orthogonal
;  polynomials at the two nearest magnetic latitudes on the magnetic latitude
;  grid
;
      OPINT=TRANSPOSE((1.-TMLAT)*OP(IMLAT1,*)+TMLAT*OP(IMLAT2,*))
;
;  If the magnetic longitude is exactly on the magnetic longitude grid, then no
;  interpolation of the orthogonal polynomial coefficients in magnetic
;  longitude is necessary
;
      IF MLONR EQ MLONG THEN BEGIN
;
;  Calculate the EOF coefficients
;
         EC=TRANSPOSE(OPC(*,*,IMLON))#OPINT
;
;  If the magnetic longitude is not exactly on the magnetic longitude grid,
;  then interpolation of the orthogonal polynomial coefficients in magnetic
;  longitude is necessary
;
      ENDIF ELSE BEGIN
;
;  Determine the indices of the two nearest magnetic longitudes on the magnetic
;  longitude grid and the magnetic longitude interpolation factor
;
         IF MLONR LT SMLON THEN BEGIN
            IMLON1=NMLON-1
            IMLON2=0
            TMLON=(MLONR-SMLON)/DMLON+1.
         ENDIF ELSE BEGIN
            IF IMLON EQ NMLON THEN BEGIN
               IMLON1=NMLON-1
               IMLON2=0
            ENDIF ELSE BEGIN
               IMLON1=IMLON
               IMLON2=IMLON+1
            ENDELSE
            TMLON=(MLONR-MLONG)/DMLON
         ENDELSE
;
;  Calculate the EOF coefficients, linearly interpolating the orthogonal
;  polynomial coefficients from the orthogonal polynomial coefficients at the
;  two nearest magnetic longitudes on the magnetic longitude grid
;
         EC=TRANSPOSE((1.-TMLON)*OPC(*,*,IMLON1)+TMLON*OPC(*,*,IMLON2))#OPINT
;
      ENDELSE
;
   ENDELSE
;
ENDIF
;
RETURN
END
FUNCTION LMEOP,NOPM1,NX,X
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
;     1.1   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Loop counter variables renamed to be more
;                                   indicative of their function.
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
PRO LMEDEN,UT,MLAT,MLON,NALT,DEN
;
;  PURPOSE
;     To construct a LME ion density altitude profile from EOFs and EOF
;     coefficients.
;
;  METHOD
;     The EOFs are combined with the EOF coefficients to produce the natural
;     logarithm of an ion density altitude profile.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLON     The magnetic longitude, in degrees east
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
;     1.1   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         2-Aug-1991   1.0 ==> Created
;     L. Brown         4-Oct-1994   1.0 ==> 1.1
;                                   Added common block CBLME for internal use
;                                   by the LME modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LME, as is appropriate.
;                                   Common block CBLME is initialized in
;                                   procedure LMEINI.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  If the case identifier, ion species descriptor, universal time, magnetic
;  latitude, or magnetic longitude are different than those of the previous
;  call, then the ion density altitude profile must be reconstructed
;
IF (ID NE PRVID) OR (ION NE PRVION) OR (UT NE PRVUT) OR (MLAT NE PRVMLA) OR $
   (MLON NE PRVMLO) THEN BEGIN
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
PRO LMEHIS,XDAY,UT,MLAT,MLON,XF10P7,XKP,SPEC,USEDEC,USEAP
;
;  PURPOSE
;     To store historical quantities for the next call to LME.
;
;  METHOD
;     The day of the year, universal time, magnetic latitude, magnetic
;     longitude, 10.7 cm solar flux, magnetic Kp index, ion species, logical
;     flag USEDEC, logical flag USEAP, case identifier, and ion species
;     descriptor are stored for the next call to LME.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLON     The magnetic longitude, in degrees east
;     SPEC     The ion species, 'NO+' or 'O2+'
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
;     1.0   4-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         4-Oct-1994   1.0 ==> Created
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLME
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLON    The increment of the magnetic longitude grid, in degrees east
;     DSHFT    Shifted eigenvalues
;     DAY      The day of the year
;     DUT      The increment of the universal time grid, in decimal hours
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     ION      The ion species descriptor, 'NO' for NO+ and 'O2' for O2+
;     KP       The magnetic Kp index
;     NEOF     The number of EOFs used to construct an ion density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLON    The number of magnetic longitude grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     NUT      The number of universal time grid points
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVION   The ion species descriptor from the previous call,  'NO' for NO+
;              and 'O2' for O2+
;     PRVKP    The magnetic Kp index from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLO   The magnetic longitude from the previous call, in degrees east
;     PRVSPE   The ion species from the previous call
;     PRVUSA   The logical flag USEAP from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     PRVUT    The universal time from the previous call, in decimal hours
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLON    The starting value of the magnetic longitude grid, in degrees
;              east
;     SUT      The starting value of the universal time grid, in decimal hours
;     YEAR     The year
;
COMMON CBLME,ID,ION,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,SMLAT,DMLAT, $
             NUT,SUT,DUT,D,DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVUT, $
             PRVMLA,PRVMLO,PRVF10,PRVKP,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
;
;  Store the day of the year
;
PRVDAY=XDAY
;
;  Store the universal time
;
PRVUT=UT
;
;  Store the magnetic latitude
;
PRVMLA=MLAT
;
;  Store the magnetic longitude
;
PRVMLO=MLON
;
;  Store the 10.7 cm solar flux
;
PRVF10=XF10P7
;
;  Store the magnetic Kp index
;
PRVKP=XKP
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
