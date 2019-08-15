PRO LLF,PATH,LUN,DAY,MLT,MLAT,SECTOR,F10P7,USEDEC,PCIDL,NALT,ALT,DEN
;
;  PURPOSE
;     A driver for constructing O+ density altitude profiles from the
;     parameterized low-latitude LLF database using modular LLF database access
;     and manipulation routines.
;
;  METHOD
;     From the given input parameters, the LLF case identifier is determined.
;     EOFs for the case are read from the appropriate LLF EOF file.  Orthogonal
;     polynomial coefficients for the case and magnetic local time are read
;     from the appropriate LLF orthogonal polynomial coefficients file.  The
;     orthogonal polynomial coefficients describe the behavior of EOF
;     coefficients in magnetic latitude for a given magnetic local time.  EOF
;     coefficients are determined from the orthogonal polynomial coefficients.
;     The EOF coefficients, in combination with the EOFs, describe the O+
;     density altitude profile for a given magnetic latitude and magnetic local
;     time.
;
;     Before LLF can be called with parameters, it must be called once with
;     no parameters to initialize common block CBLLF.
;
;  INPUT PARAMETERS
;     DAY      The day of the year
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     LUN      The logical unit number used to access LLF database files
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     PATH     The location of LLF database files
;     PCIDL    A logical flag, 1 if the IBM-PC/Windows IDL convention for
;              associated file variables is to be used, 0 if the VAX/VMS IDL
;              convention is to be used
;     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
;              Indian, 'PAC' for Pacific, and 'USA' for USA
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the month descriptor, 0 if the day of the year is to
;              be used
;
;  OUTPUT PARAMETERS
;     ALT      The altitude grid, in km
;     DEN      The O+ density altitude profile, in cm-3
;     NALT     The number of altitude points
;
;  LOCAL VARIABLES
;     None
;
;  PROCEDURES REQUIRED
;     LLFDEN   Generates an O+ density altitude profile from EOFs and EOF
;              coefficients
;     LLFEC    Generates EOF coefficients from orthogonal polynomial
;              coefficients
;     LLFEOF   Reads an EOF file
;     LLFID    Determines the case identifier
;     LLFINI   Initializes common block CBLLF
;     LLFOPC   Reads an orthogonal polynomial coefficients file
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
;     1.2   30-September-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         5-Jun-1991   1.0 ==> Created
;     L. Brown         7-Apr-1993   1.0 ==> 1.1
;                                   Added user-specified location and logical
;                                   unit number for LLF database files.
;                                   Added common block CBLLF for internal use
;                                   by the LLF modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LLF, as is appropriate.
;                                   Common block CBLLF is initialized in
;                                   procedure LLFINI.
;     L. Brown        30-Sep-1994   1.1 ==> 1.2
;                                   The calls to LLFFC and LLFEC have been
;                                   combined into a single call to LLFEC to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Initialize common block CBLLF on an initialization call to LLF (with no
;  arguments)
;
IF N_PARAMS() EQ 0 THEN BEGIN
   LLFINI
;
;  Generate an O+ density profile on a normal call to LLF (with arguments)
;
ENDIF ELSE BEGIN
;
;  Determine the case identifier
;
   LLFID,DAY,SECTOR,F10P7,USEDEC
;
;  Read the LLF EOF file
;
   LLFEOF,PATH,LUN,PCIDL,NALT,ALT
;
;  Read the LLF orthogonal polynomial coefficients file
;
   LLFOPC,PATH,LUN,PCIDL
;
;  Generate EOF coefficients from the orthogonal polynomial coefficients
;
   LLFEC,MLT,MLAT
;
;  Generate an O+ density altitude profile from the EOFs and EOF coefficients
;
   LLFDEN,MLT,MLAT,NALT,DEN
;
;  Store historical quantities for the next call to LLF
;
   LLFHIS,DAY,MLT,MLAT,SECTOR,F10P7,USEDEC
;
ENDELSE
;
RETURN
END
PRO LLFINI
;
;  PURPOSE
;     To initialize the contents of common block CBLLF.
;
;  METHOD
;     Common block CBLLF is intended for internal use by the LLF modules.
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
;     1.2   1-May-1997
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         7-Apr-1993   1.0 ==> Created
;     L. Brown        30-Sep-1994   1.0 ==> 1.1
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;     L. Brown         1-May-1997   1.1 ==> 1.2
;                                   Increased constant MOPM1 from 11 to 14.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MALT     The maximum number of altitude points
;     MEOF     The maximum number of EOFs
;     MMLT     The maximum number of magnetic local times
;     MOPM1    The maximum number of orthogonal polynomials - 1
;
MALT=55
MEOF=55
MOPM1=14
MMLT=48
;
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  Initialize common block CBLLF
;
ID='        '
YEAR=0
DAY=0
F10P7=0.
AP=0.
KP=0.
MLON=0.
NMLAT=0
SMLAT=0.
DMLAT=0.
NMLT=0
SMLT=0.
DMLT=0.
D=FLTARR(MEOF)
DSHFT=FLTARR(MEOF)
EOFS=FLTARR(MALT,MEOF)
NOPM1=0
NEOF=0
DELTA=0.
OPC=FLTARR(MOPM1+1,MEOF,MMLT)
EC=FLTARR(MEOF)
PRVDAY=9999
PRVMLT=9999.
PRVMLA=9999.
PRVSCT='999'
PRVF10=9999.
PRVUSD=9999
PRVID='99999999'
;
RETURN
END
PRO LLFID,XDAY,SECTOR,XF10P7,USEDEC
;
;  PURPOSE
;     To determine a LLF case identifier.
;
;  METHOD
;     The LLF case identifier is a code for describing the ambient conditions
;     (magnetic longitude sector, day of the year, and solar activity level) of
;     a LLF case.  The case identifier has the form 'sssnnccc', where 'sss'
;     describes the magnetic longitude sector, 'nn' describes the month, and
;     'ccc' describes the solar activity level.  The magnetic longitude sectors
;     are 'BRZ' for Brazilian, 'IND' for Indian, 'PAC' for Pacific, and 'USA'
;     for USA.  The day of the year determines 'nn':  '03' for March (equinox),
;     '06' for June (solstice), and '12' for December (solstice). The solar
;     F10.7 index determines 'ccc':  'MIN' for Minimum, 'MOD' for Moderate, and
;     'MAX' for Maximum.  Logical flag USEDEC determines how the month
;     descriptor is chosen.
;
;  INPUT PARAMETERS
;     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
;              Indian, 'PAC' for Pacific, and 'USA' for USA
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the month descriptor, 0 if the day of the year is to
;              be used
;     XDAY     The day of the year
;     XF10P7   The 10.7 cm solar flux, in solar flux units
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     DEC      The solar declination, in degrees north
;     MONTH    The month descriptor, '03' for March, '06' for June, and '12'
;              for December
;     SCTR     The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
;              Indian, 'PAC' for Pacific, and 'USA' for USA
;     SOLACT   The solar activity level descriptor, 'MIN' for Minimum, 'MOD'
;              for Moderate, and 'MAX' for Maximum
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
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
;     1.3   30-September-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         5-Jun-1991   1.0 ==> Created
;     L. Brown        13-Nov-1991   1.0 ==> 1.1
;                                   Complete rewrite to simplify coding and to
;                                   correct a bug in the determination of the
;                                   month descriptor.
;                                   The selection of the month descriptor has
;                                   been enhanced as well (see the comments
;                                   below).
;     L. Brown         7-Apr-1993   1.1==> 1.2
;                                   Added common block CBLLF for internal use
;                                   by the LLF modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LLF, as is appropriate.
;                                   Common block CBLLF is initialized in
;                                   procedure LLFINI.
;     L. Brown        30-Sep-1994   1.2 ==> 1.3
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
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
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  If the day of the year, magnetic longitude sector, 10.7 cm solar flux, or
;  logical flag USEDEC are different than those of the previous call, then
;  the case identifier must be redetermined
;
IF (XDAY NE PRVDAY) OR (SECTOR NE PRVSCT) OR (XF10P7 NE PRVF10) OR $
   ((USEDEC EQ 1) XOR (PRVUSD EQ 1)) THEN BEGIN
;
;  Check the magnetic longitude sector descriptor
;
   SCTR=STRUPCASE(STRCOMPRESS(SECTOR,/REMOVE_ALL))
   IF (SCTR NE 'BRZ') AND (SCTR NE 'IND') AND (SCTR NE 'PAC') AND $
      (SCTR NE 'USA') THEN STOP,'LLFID:Invalid magnetic longitude sector.'
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
;  Determine the solar activity level descriptor from F10.7
;
   IF XF10P7 LE 70. THEN BEGIN
      SOLACT='MIN'
   ENDIF ELSE IF (XF10P7 GT 70.) AND (XF10P7 LE 130.) THEN BEGIN
      IF XF10P7-70. LE 130.-XF10P7 THEN SOLACT='MIN' ELSE SOLACT='MOD'
   ENDIF ELSE IF (XF10P7 GT 130.) AND (XF10P7 LE 210.) THEN BEGIN
      IF XF10P7-130. LE 210.-XF10P7 THEN SOLACT='MOD' ELSE SOLACT='MAX'
   ENDIF ELSE BEGIN
      SOLACT='MAX'
   ENDELSE
;
;  Determine the case identifier
;
   ID=SCTR+MONTH+SOLACT
;
ENDIF
;
RETURN
END
PRO LLFEOF,PATH,LUN,PCIDL,NALT,ALT
;
;  PURPOSE
;     To read a LLF EOF file.
;
;  METHOD
;     The name of the LLF EOF file is determined from the case identifier.
;     Header information, followed by the altitude grid, eigenvalues, and EOFs
;     are read from the LLF EOF file.
;
;  INPUT PARAMETERS
;     LUN      The logical unit number used to access the LLF EOF file
;     PATH     The location of the LLF EOF file
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
;     #LUN     The LLF EOF file
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.3   20-October-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         5-Jun-1991   1.0 ==> Created
;     L. Brown         7-Apr-1993   1.0 ==> 1.1
;                                   Added user-specified location and logical
;                                   unit number for LLF database files.
;                                   Added common block CBLLF for internal use
;                                   by the LLF modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LLF, as is appropriate.
;                                   Common block CBLLF is initialized in
;                                   procedure LLFINI.
;     L. Brown        30-Sep-1994   1.1 ==> 1.2
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;     L. Brown        20-Oct-1994   1.2 ==> 1.3
;                                   The full file specification of the LLF EOF
;                                   file is converted to lower-case letters in
;                                   the OPENR statement.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MALT     The maximum number of altitude points
;
MALT=55
;
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  If the case identifier is different than that of the previous call, then the
;  LLF EOF file must be read
;
IF ID NE PRVID THEN BEGIN
;
;  Open the LLF EOF file
;
   OPENR,LUN,STRLOWCASE(STRCOMPRESS(PATH+ID+'.OE',/REMOVE_ALL))
;
;  Read header information from the LLF EOF file
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
   MLON=A(0,2)
   NMLAT=FIX(A(0,3))
   SMLAT=A(1,3)
   DMLAT=A(2,3)
   NMLT=FIX(A(0,4))
   SMLT=A(1,4)
   DMLT=A(2,4)
;
;  Read the altitude grid from the LLF EOF file
;
   NALT=FIX(A(0,5))
   A=ASSOC(LUN,FLTARR(NALT,/NOZERO))
   ALT=A(6)
;
;  Read the eigenvalues from the LLF EOF file
;
   D=A(7)
   DSHFT=A(8)
;
;  Read the EOFs from the LLF EOF file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(NALT,NALT,/NOZERO),9*NALT*4)
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(NALT,NALT,/NOZERO),9)
   ENDELSE
   EOFS=A(0)
;
;  Close the LLF EOF file
;
   CLOSE,LUN
;
ENDIF
;
RETURN
END
PRO LLFOPC,PATH,LUN,PCIDL
;
;  PURPOSE
;     To read a LLF orthogonal polynomial coefficients file.
;
;  METHOD
;     The name of the LLF orthogonal polynomial coefficients file is determined
;     from the case identifier.  Header information, followed by orthogonal
;     polynomial coefficients, are read from the LLF orthogonal polynomial
;     coefficients file.
;
;  INPUT PARAMETERS
;     LUN      The logical unit number used to access the LLF orthogonal
;              polynomial coefficients file
;     PATH     The location of the LLF orthogonal polynomial coefficients file
;     PCIDL    A logical flag, 1 if the IBM-PC/Windows IDL convention for
;              associated file variables is to be used, 0 if the VAX/VMS IDL
;              convention is to be used
;
;  OUTPUT PARAMETERS
;     None
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
;     #LUN     The LLF orthogonal polynomial coefficients file
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Newton, MA 02164  USA
;     (617)-487-2250
;
;  VERSION
;     1.4   1-May-1997
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         5-Jun-1991   1.0 ==> Created
;     L. Brown         7-Apr-1993   1.0 ==> 1.1
;                                   Added user-specified location and logical
;                                   unit number for LLF database files.
;                                   Added common block CBLLF for internal use
;                                   by the LLF modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LLF, as is appropriate.
;                                   Common block CBLLF is initialized in
;                                   procedure LLFINI.
;     L. Brown        30-Sep-1994   1.1 ==> 1.2
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;                                   The LLF orthogonal polynomial coefficients
;                                   file has been restructured to reflect the
;                                   elimination of Fourier fitting in UT in the
;                                   low-latitude F-region parameterized model
;                                   (LLF).
;     L. Brown        20-Oct-1994   1.2 ==> 1.3
;                                   The full file specification of the LLF
;                                   orthogonal polynomial coefficients file is
;                                   converted to lower-case letters in the
;                                   OPENR statement.
;     L. Brown         1-May-1997   1.3 ==> 1.4
;                                   Increased constant MOPM1 from 11 to 14.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     MOPM1    The maximum number of orthogonal polynomials - 1
;
MOPM1=14
;
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  If the case identifier is different than that of the previous call, then the
;  LLF orthogonal polynomial coefficients file must be read
;
IF ID NE PRVID THEN BEGIN
;
;  Open the LLF orthogonal polynomial coefficients file
;
   OPENR,LUN,STRLOWCASE(STRCOMPRESS(PATH+ID+'.OP',/REMOVE_ALL))
;
;  Read header information from the LLF orthogonal polynomial coefficients file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(MOPM1+1,/NOZERO))
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(3,/NOZERO))
   ENDELSE
   YEAR=FIX(A(0,0))
   DAY=FIX(A(1,0))
   F10P7=A(0,1)
   AP=A(1,1)
   KP=A(2,1)
   MLON=A(0,2)
   NMLAT=FIX(A(0,3))
   SMLAT=A(1,3)
   DMLAT=A(2,3)
   NMLT=FIX(A(0,4))
   SMLT=A(1,4)
   DMLT=A(2,4)
   NOPM1=FIX(A(0,5))
   NEOF=FIX(A(1,5))
   DELTA=A(0,6)
;
;  Associate a variable with the LLF orthogonal polynomial coefficients file
;
   IF PCIDL EQ 1 THEN BEGIN
      A=ASSOC(LUN,FLTARR(NOPM1+1,NEOF,NMLT,/NOZERO),7*(NOPM1+1)*4)
   ENDIF ELSE BEGIN
      A=ASSOC(LUN,FLTARR(NOPM1+1,NEOF,NMLT,/NOZERO),7)
   ENDELSE
;
;  Read the orthogonal polynomial coefficients from the LLF orthogonal
;  polynomial coefficients file
;
   OPC=A(0)
;
;  Close the LLF orthogonal polynomial coefficients file
;
   CLOSE,LUN
;
ENDIF
;
RETURN
END
PRO LLFEC,MLT,MLAT
;
;  PURPOSE
;     To generate EOF coefficients from orthogonal polynomial coefficients
;     for LLF.
;
;  METHOD
;     A set of orthogonal polynomials is determined.  The orthogonal
;     polynomials are combined with the orthogonal polynomial coefficients to
;     produce EOF coefficients for the given magnetic latitude.  If the
;     magnetic latitude does not lie exactly on the magnetic latitude grid,
;     then the orthogonal polynomial is linearly interpolated from orthogonal
;     polynomials at the two nearest magnetic latitudes on the magnetic
;     latitude grid.  If the magnetic local time does not lie exactly on the
;     magnetic local time grid, then the orthogonal polynomial coefficients are
;     linearly interpolated from orthogonal polynomial coefficients at the two
;     nearest magnetic local times on the magnetic local time grid.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;
;  OUTPUT PARAMETERS
;     None
;
;  LOCAL VARIABLES
;     IEOF     The loop counter for EOFs
;     IMLAT    The magnetic latitude index
;     IMLAT1   A magnetic latitude index used for interpolation
;     IMLAT2   A magnetic latitude index used for interpolation
;     IMLT     The magnetic local time index
;     IMLT1    A magnetic local time index used for interpolation
;     IMLT2    A magnetic local time index used for interpolation
;     MLATG    The grid magnetic latitude, in degrees north
;     MLTG     The grid magnetic local time, in decimal hours
;     MLTR     The magnetic local time, in decimal hours, restricted to the
;              range [>=0,<24]
;     ONEMTM   1.-TMLAT
;     ONEMTT   1.-TMLT
;     OP       Orthogonal polynomials
;     OPINT    An interpolated orthogonal polynomial
;     TMLAT    The interpolation factor for magnetic latitude
;     TMLT     The interpolation factor for magnetic local time
;     X        The grid for the orthogonal polynomials
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     LLFOP    Generates the orthogonal polynomials
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
;     1.2   30-September-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         5-Jun-1991   1.0 ==> Created
;     L. Brown         7-Apr-1993   1.0 ==> 1.1
;                                   Added common block CBLLF for internal use
;                                   by the LLF modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LLF, as is appropriate.
;                                   Common block CBLLF is initialized in
;                                   procedure LLFINI.
;     L. Brown        30-Sep-1994   1.1 ==> 1.2
;                                   Renamed from LLFFC to LLFEC and modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;
;  REFERENCES
;     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
;        The Golam Press, Boulder, CO, 1973, pp 90-92.
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  If the case identifier, magnetic local time, or magnetic latitude are
;  different than those of the previous call, then the EOF coefficients must be
;  regenerated
;
IF (ID NE PRVID) OR (MLT NE PRVMLT) OR (MLAT NE PRVMLA) THEN BEGIN
;
;  Calculate the grid for the orthogonal polynomials
;
   X=DELTA*FINDGEN(NMLAT)-1.
;
;  Generate the orthogonal polynomials
;
   OP=LLFOP(NOPM1,NMLAT,X)
;
;  Restrict the magnetic local time to the range [>=0,<24]
;
   MLTR=(24.+(MLT MOD 24.)) MOD 24.
;
;  Calculate the grid magnetic local time index
;
   IMLT=FIX((MLTR-SMLT)/DMLT)
;
;  Calculate the grid magnetic local time
;
   MLTG=SMLT+DMLT*FLOAT(IMLT)
;
;  Calculate the magnetic latitude index
;
   IMLAT=MIN([NMLAT-1,MAX([0,FIX((MLAT-SMLAT)/DMLAT)])])
;
;  Calculate the grid magnetic latitude
;
   MLATG=SMLAT+DMLAT*FLOAT(IMLAT)
;
;  If the magnetic latitude is exactly on the magnetic latitude grid, then no
;  interpolation of the orthogonal polynomials in magnetic latitude is
;  necessary
;
   IF MLAT EQ MLATG THEN BEGIN
;
;  If the magnetic local time is exactly on the magnetic local time grid, then
;  no interpolation of the orthogonal polynomial coefficients in magnetic local
;  time is necessary
;
      IF MLTR EQ MLTG THEN BEGIN
;
;  Calculate the EOF coefficients
;
         EC=TRANSPOSE(OPC(*,*,IMLT))#TRANSPOSE(OP(IMLAT,*))
;
;  If the magnetic local time is not exactly on the magnetic local time grid,
;  then interpolation of the orthogonal polynomial coefficients in magnetic
;  local is necessary
;
      ENDIF ELSE BEGIN
;
;  Determine the indices of the two nearest magnetic local times on the
;  magnetic local time grid and the magnetic local time interpolation factor
;
         IF MLTR LT SMLT THEN BEGIN
            IMLT1=NMLT-1
            IMLT2=0
            TMLT=(MLTR-SMLT)/DMLT+1.
         ENDIF ELSE BEGIN
            IF IMLT EQ NMLT THEN BEGIN
               IMLT1=NMLT-1
               IMLT2=0
            ENDIF ELSE BEGIN
               IMLT1=IMLT
               IMLT2=IMLT+1
            ENDELSE
            TMLT=(MLTR-MLTG)/DMLT
         ENDELSE
         ONEMTT=1.-TMLT
;
;  Calculate the EOF coefficients, linearly interpolating the orthogonal
;  polynomial coefficients from the orthogonal polynomial coefficients at the
;  two nearest magnetic local times on the magnetic local time grid
;
         EC=TRANSPOSE(ONEMTT*OPC(*,*,IMLT1)+TMLT*OPC(*,*,IMLT2)) $
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
      ONEMTM=1.-TMLAT
;
;  Linearly interpolate the orthogonal polynomial from the orthogonal
;  polynomials at the two nearest magnetic latitudes on the magnetic latitude
;  grid
;
      OPINT=TRANSPOSE(ONEMTM*OP(IMLAT1,*)+TMLAT*OP(IMLAT2,*))
;
;  If the magnetic local time is exactly on the magnetic local time grid, then
;  no interpolation of the orthogonal polynomial coefficients in magnetic local
;  time is necessary
;
      IF MLTR EQ MLTG THEN BEGIN
;
;  Calculate the EOF coefficients
;
         EC=TRANSPOSE(OPC(*,*,IMLT))#OPINT
;
;  If the magnetic local time is not exactly on the magnetic local time grid,
;  then interpolation of the orthogonal polynomial coefficients in magnetic
;  local time is necessary
;
      ENDIF ELSE BEGIN
;
;  Determine the indices of the two nearest magnetic local times on the
;  magnetic local time grid and the magnetic local time interpolation factor
;
         IF MLTR LT SMLT THEN BEGIN
            IMLT1=NMLT-1
            IMLT2=0
            TMLT=(MLTR-SMLT)/DMLT+1.
         ENDIF ELSE BEGIN
            IF IMLT EQ NMLT THEN BEGIN
               IMLT1=NMLT-1
               IMLT2=0
            ENDIF ELSE BEGIN
               IMLT1=IMLT
               IMLT2=IMLT+1
            ENDELSE
            TMLT=(MLTR-MLTG)/DMLT
         ENDELSE
         ONEMTT=1.-TMLT
;
;  Calculate the EOF coefficients, linearly interpolating the orthogonal
;  polynomial coefficients from the orthogonal polynomial coefficients at the
;  two nearest magnetic local times on the magnetic local time grid
;
         EC=TRANSPOSE(ONEMTT*OPC(*,*,IMLT1)+TMLT*OPC(*,*,IMLT2))#OPINT
;
      ENDELSE
;
   ENDELSE
;
ENDIF
;
RETURN
END
FUNCTION LLFOP,NOPM1,NX,X
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
;     1.1   7-April-1993
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        22-Feb-1991   1.0 ==> Created
;     L. Brown         7-Apr-1993   1.0 ==> 1.1
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
PRO LLFDEN,MLT,MLAT,NALT,DEN
;
;  PURPOSE
;     To construct a LLF O+ density altitude profile from EOFs and EOF
;     coefficients.
;
;  METHOD
;     The EOFs are combined with the EOF coefficients to produce the natural
;     logarithm of an O+ density altitude profile.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     NALT     The number of altitude points
;
;  OUTPUT PARAMETERS
;     DEN      The O+ density altitude profile, in cm-3
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
;     1.2   30-September-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         5-Jun-1991   1.0 ==> Created
;     L. Brown         7-Apr-1993   1.0 ==> 1.1
;                                   Added common block CBLLF for internal use
;                                   by the LLF modules for sharing information
;                                   instead of passing it through argument
;                                   lists, greatly simplifying and clarifying
;                                   their usage.  Common storage also allows
;                                   computational savings by using results from
;                                   a previous call to LLF, as is appropriate.
;                                   Common block CBLLF is initialized in
;                                   procedure LLFINI.
;     L. Brown        30-Sep-1994   1.1 ==> 1.2
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  If the case identifier, magnetic local time, or magnetic latitude are
;  different than those of the previous call, then the O+ density altitude
;  profile must be reconstructed
;
IF (ID NE PRVID) OR (MLT NE PRVMLT) OR (MLAT NE PRVMLA) THEN BEGIN
;
;  Sum the contributions of the EOFs at each altitude
;
   DEN=EOFS(*,0:NEOF-1)#EC
;
;  Convert the result to an actual O+ density by exponentiation
;
   DEN=EXP(DEN)
;
ENDIF
;
RETURN
END
PRO LLFHIS,XDAY,MLT,MLAT,SECTOR,XF10P7,USEDEC
;
;  PURPOSE
;     To store historical quantities for the next call to LLF.
;
;  METHOD
;     The day of the year, magnetic local time, magnetic latitude, magnetic
;     longitude sector, 10.7 cm solar flux, logical flag USEDEC, and case
;     identifier are stored for the next call to LLF.
;
;  INPUT PARAMETERS
;     MLAT     The magnetic latitude, in degrees north
;     MLT      The magnetic local time, in decimal hours
;     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
;              Indian, 'PAC' for Pacific, and 'USA' for USA
;     USEDEC   A logical flag, 1 if the solar declination is to be used to
;              determine the month descriptor, 0 if the day of the year is to
;              be used
;     XDAY     The day of the year
;     XF10P7   The 10.7 cm solar flux, in solar flux units
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
;     1.1   30-September-1994
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown         7-Apr-1993   1.0 ==> Created
;     L. Brown        30-Sep-1994   1.0 ==> 1.1
;                                   Common block CBLLF has been modified to
;                                   reflect the elimination of Fourier fitting
;                                   in UT in the low-latitude F-region
;                                   parameterized model (LLF).
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Common block CBLLF
;     AP       The magnetic Ap index
;     D        Eigenvalues
;     DAY      The day of the year
;     DELTA    The step size for the orthogonal polynomials
;     DMLAT    The increment of the magnetic latitude grid, in degrees north
;     DMLT     The increment of the magnetic local time grid, in decimal hours
;     DSHFT    Shifted eigenvalues
;     EC       EOF coefficients
;     EOFS     Empirical orthogonal functions (EOFs)
;     F10P7    The 10.7 cm solar flux, in solar flux units
;     ID       The case identifier
;     KP       The magnetic Kp index
;     MLON     The magnetic longitude, in degrees east
;     NEOF     The number of EOFs used to construct an O+ density altitude
;              profile
;     NMLAT    The number of magnetic latitude grid points
;     NMLT     The number of magnetic local time grid points
;     NOPM1    The number of orthogonal polynomials - 1
;     OPC      Orthogonal polynomial coefficients
;     PRVDAY   The day of the year from the previous call
;     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
;              units
;     PRVID    The case identifier from the previous call
;     PRVMLA   The magnetic latitude from the previous call, in degrees north
;     PRVMLT   The magnetic local time from the previous call, in decimal hours
;     PRVSCT   The magnetic longitude sector from the previous call
;     PRVUSD   The logical flag USEDEC from the previous call
;     SMLAT    The starting value of the magnetic latitude grid, in degrees
;              north
;     SMLT     The starting value of the magnetic local time grid, in decimal
;              hours
;     YEAR     The year
;
COMMON CBLLF,ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D, $
             DSHFT,EOFS,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,PRVMLT,PRVMLA,PRVSCT, $
             PRVF10,PRVUSD,PRVID
;
;  Store the day of the year
;
PRVDAY=XDAY
;
;  Store the magnetic local time
;
PRVMLT=MLT
;
;  Store the magnetic latitude
;
PRVMLA=MLAT
;
;  Store the magnetic longitude sector
;
PRVSCT=SECTOR
;
;  Store the 10.7 cm solar flux
;
PRVF10=XF10P7
;
;  Store the logical flag USEDEC
;
PRVUSD=USEDEC
;
;  Store the case identifier
;
PRVID=ID
;
RETURN
END
