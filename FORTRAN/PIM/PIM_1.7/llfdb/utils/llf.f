      SUBROUTINE LLF(PATH,LUN,DAY,MLT,MLAT,SECTOR,F10P7,USEDEC,BYTES,
     &               NALT,ALT,DEN)
C
C  PURPOSE
C     A driver for constructing O+ density altitude profiles from the
C     parameterized low-latitude LLF database using modular LLF database
C     access and manipulation routines.
C
C  METHOD
C     From the given input parameters, the LLF case identifier is determined.
C     EOFs for the case are read from the appropriate LLF EOF file.  Orthogonal
C     polynomial coefficients for the case and magnetic local time are read
C     from the appropriate LLF orthogonal polynomial coefficients file.  The
C     orthogonal polynomial coefficients describe the behavior of EOF
C     coefficients in magnetic latitude for a given magnetic local time.  EOF
C     coefficients are determined from the orthogonal polynomial coefficients.
C     The EOF coefficients, in combination with the EOFs, describe the O+
C     density altitude profile for a given magnetic latitude and magnetic local
C     time.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     LUN      The logical unit number used to access LLF database
C              files
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     PATH     The location of LLF database files
C     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
C              Indian, 'PAC' for Pacific, and 'USA' for USA
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the month descriptor, .FALSE. if the day of the
C              year is to be used
C
C  OUTPUT PARAMETERS
C     ALT      The altitude grid, in km
C     DEN      The O+ density altitude profile, in cm-3
C     NALT     The number of altitude points
C
C  LOCAL VARIABLES
C     None
C
C  SUBROUTINES REQUIRED
C     LLFDEN   Generates an O+ density altitude profile from EOFs and EOF
C              coefficients
C     LLFEC    Generates EOF coefficients from orthogonal polynomial
C              coefficients
C     LLFEOF   Reads an EOF file
C     LLFHIS   Stores historical quantities for the next call to LLF
C     LLFID    Determines the case identifier
C     LLFOPC   Reads an orthogonal polynomial coefficients file
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown        26-Feb-1993   1.0 ==> 1.1
C                                   Added user-specified location and logical
C                                   unit number for LLF database files.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBLLF for internal use
C                                   by the LLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to LLF, as is appropriate.
C                                   Common block CBLLF is initialized in block
C                                   data BDLLF.
C     L. Brown        30-Sep-1994   1.2 ==> 1.3
C                                   The calls to LLFFC and LLFEC have been
C                                   combined into a single call to LLFEC to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C     L. Brown        27-Jun-1995   1.3 ==> 1.4
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.4 ==> 1.5
C                                   Corrected order of arguments in call to
C                                   routine LLFHIS.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C
      INTEGER MALT
      PARAMETER(MALT=55)
C
C  Input variables
C
      INTEGER LUN,DAY
      REAL MLT,MLAT,F10P7
      CHARACTER*(*) PATH
      CHARACTER*3 SECTOR
      LOGICAL USEDEC,BYTES
C
C  Output variables
C
      INTEGER NALT
      REAL ALT(MALT),DEN(MALT)
C
C  Determine the case identifier
C
      CALL LLFID(DAY,SECTOR,F10P7,USEDEC)
C
C  Read the LLF EOF file
C
      CALL LLFEOF(PATH,LUN,BYTES,NALT,ALT)
C
C  Read the LLF orthogonal polynomial coefficients file
C
      CALL LLFOPC(PATH,LUN,BYTES)
C
C  Generate EOF coefficients from the orthogonal polynomial coefficients
C
      CALL LLFEC(MLT,MLAT)
C
C  Generate an O+ density altitude profile from the EOFs and EOF coefficients
C
      CALL LLFDEN(MLT,MLAT,NALT,DEN)
C
C  Store historical quantities for the next call to LLF
C
      CALL LLFHIS(DAY,MLT,MLAT,SECTOR,F10P7,USEDEC)
C
      RETURN
      END
      SUBROUTINE LLFID(XDAY,SECTOR,XF10P7,USEDEC)
C
C  PURPOSE
C     To determine a LLF case identifier.
C
C  METHOD
C     The LLF case identifier is a code for describing the ambient conditions
C     (magnetic longitude sector, day of the year, and solar activity level) of
C     a LLF case.  The case identifier has the form 'sssnnccc', where 'sss'
C     describes the magnetic longitude sector, 'nn' describes the month, and
C     'ccc' describes the solar activity level.  The magnetic longitude sectors
C     are 'BRZ' for Brazilian, 'IND' for Indian, 'PAC' for Pacific, and 'USA'
C     for USA.  The day of the year determines 'nn':  '03' for March (equinox),
C     '06' for June (solstice), and '12' for December (solstice). The solar
C     F10.7 index determines 'ccc':  'MIN' for Minimum, 'MOD' for Moderate, and
C     'MAX' for Maximum.  Logical flag USEDEC determines how the month
C     descriptor is chosen.
C
C  INPUT PARAMETERS
C     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
C              Indian, 'PAC' for Pacific, and 'USA' for USA
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the month descriptor, .FALSE. if the day of the
C              year is to be used
C     XDAY     The day of the year
C     XF10P7   The 10.7 cm solar flux, in solar flux units
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     DEC      The solar declination, in degrees north
C     ISTAT    The status returned from the STRUCA routine
C     MONTH    The month descriptor, '03' for March, '06' for June, and '12'
C              for December
C     SCTR     The magnetic longitude sector descriptor, 'BRZ' for Brazilian,
C              'IND' for Indian, 'PAC' for Pacific, and 'USA' for USA
C     SOLACT   The solar activity level descriptor, 'MIN' for Minimum, 'MOD'
C              for Moderate, and 'MAX' for Maximum
C
C  SUBROUTINES REQUIRED
C     STRUCA   Converts lower-case characters to upper-case in a string
C
C  FUNCTIONS REQUIRED
C     SOLDEC   Determines the solar declination from the day of the year
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown        13-Nov-1991   1.0 ==> 1.1
C                                   Complete rewrite to simplify coding and to
C                                   correct a bug in the determination of the
C                                   month descriptor.
C                                   The selection of the month descriptor has
C                                   been enhanced as well (see the comments
C                                   below).
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBLLF for internal use
C                                   by the LLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to LLF, as is appropriate.
C                                   Common block CBLLF is initialized in block
C                                   data BDLLF.
C     L. Brown        30-Sep-1994   1.2 ==> 1.3
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   The form of the case identifier has been
C                                   changed to a use 2-digit month for all
C                                   three seasons ('03', '06', and '12').
C                                   Call to UPCASE replaced by call to STRUCA.
C     L. Brown        27-Jun-1995   1.3 ==> 1.4
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.4 ==> 1.5
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Input variables
C
      INTEGER XDAY
      REAL XF10P7
      CHARACTER*3 SECTOR
      LOGICAL USEDEC
C
C  Local variables
C
      INTEGER ISTAT
      REAL SOLDEC,DEC
      CHARACTER*2 MONTH
      CHARACTER*3 SCTR,SOLACT
C
C  If the day of the year, magnetic longitude sector, 10.7 cm solar flux, or
C  logical flag USEDEC are different than those of the previous call, then
C  the case identifier must be redetermined
C
      IF((XDAY .NE. PRVDAY) .OR. (SECTOR .NE. PRVSCT) .OR.
     &   (XF10P7 .NE. PRVF10) .OR.
     &   ((USEDEC .OR. PRVUSD) .AND. (.NOT. (USEDEC .AND. PRVUSD))))
     &   THEN
C
C  Note: If the exclusive-OR (.XOR.) logical operator becomes part of the
C        FORTRAN standard, then the last two continuation lines of the above IF
C        statement can be replaced with the following continuation line:
C
C    &   (USEDEC .XOR. PRVUSD)) THEN
C
C  Check the magnetic longitude sector descriptor
C
         SCTR=SECTOR
         CALL STRUCA(SCTR,ISTAT)
         IF((SCTR .NE. 'BRZ') .AND. (SCTR .NE. 'IND') .AND.
     &      (SCTR .NE. 'PAC') .AND. (SCTR .NE. 'USA'))
     &      STOP 'LLFID:Invalid magnetic longitude sector.'
C
C  Determine the month descriptor from the solar declination
C  Note: If you want to use the solar declination to determine the month
C        descriptor, then the logical flag USEDEC should be set .TRUE.; if you
C        want to use the day of the year directly, then the logical flag USEDEC
C        should be set .FALSE..  Solar declination is a better indicator of
C        season; however, you may find it more convenient to work with the day
C        of the year directly.
C
         IF(USEDEC) THEN
            DEC=SOLDEC(FLOAT(XDAY))
            IF(DEC .LE. -23.5/2.) THEN
               MONTH='12'
            ELSE IF(ABS(DEC) .LT. 23.5/2.) THEN
               MONTH='03'
            ELSE
               MONTH='06'
            ENDIF
C
C  Determine the month descriptor directly from the day of the year
C  Note: If you want to use the solar declination to determine the month
C        descriptor, then the logical flag USEDEC should be set .TRUE.; if you
C        want to use the day of the year directly, then the logical flag USEDEC
C        should be set .FALSE..  Solar declination is a better indicator of
C        season; however, you may find it more convenient to work with the day
C        of the year directly.
C
         ELSE
            IF(XDAY .LE. 82) THEN
               IF(365+XDAY-357 .LE. 82-XDAY) THEN
                  MONTH='12'
               ELSE
                  MONTH='03'
               ENDIF
            ELSE IF((XDAY .GT. 82) .AND. (XDAY .LE. 173)) THEN
               IF(XDAY-82 .LE. 173-XDAY) THEN
                  MONTH='03'
               ELSE
                  MONTH='06'
               ENDIF
            ELSE IF((XDAY .GT. 173) .AND. (XDAY .LE. (173+357)/2)) THEN
               IF(XDAY-173 .LE. (173+357)/2-XDAY) THEN
                  MONTH='06'
               ELSE
                  MONTH='03'
               ENDIF
            ELSE IF((XDAY .GT. (173+357)/2) .AND. (XDAY .LE. 357)) THEN
               IF(XDAY-(173+357)/2 .LE. 357-XDAY) THEN
                  MONTH='03'
               ELSE
                  MONTH='12'
               ENDIF
            ELSE
               MONTH='12'
            ENDIF
         ENDIF
C
C  Determine the solar activity level descriptor from F10.7
C
         IF(XF10P7 .LE. 70.) THEN
            SOLACT='MIN'
         ELSE IF((XF10P7 .GT. 70.) .AND. (XF10P7 .LE. 130.)) THEN
            IF(XF10P7-70. .LE. 130.-XF10P7) THEN
               SOLACT='MIN'
            ELSE
               SOLACT='MOD'
            ENDIF
         ELSE IF((XF10P7 .GT. 130.) .AND. (XF10P7 .LE. 210.)) THEN
            IF(XF10P7-130. .LE. 210.-XF10P7) THEN
               SOLACT='MOD'
            ELSE
               SOLACT='MAX'
            ENDIF
         ELSE
            SOLACT='MAX'
         ENDIF
C
C  Determine the case identifier
C
         ID=SCTR//MONTH//SOLACT
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE LLFEOF(PATH,LUN,BYTES,NALT,ALT)
C
C  PURPOSE
C     Reads a LLF EOF file.
C
C  METHOD
C     The name of the LLF EOF file is determined from the case identifier.
C     Header information, followed by the altitude grid, eigenvalues, and EOFs
C     are read from the LLF EOF file.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     LUN      The logical unit number used to access the LLF EOF file
C     PATH     The location of the LLF EOF file
C
C  OUTPUT PARAMETERS
C     ALT      The altitude grid, in km
C     NALT     The number of altitude points
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FSPEC    The full specification of the LLF EOF file
C     IALT     The loop counter for altitudes
C     IEOF     The loop counter for EOFs
C     ISTAT    The status returned from the STRCCT and STRLCA routines
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters in a character string to lower-case
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     #LUN     The LLF EOF file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown        26-Feb-1993   1.0 ==> 1.1
C                                   Added location for LLF EOF file.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBLLF for internal use
C                                   by the LLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to LLF, as is appropriate.
C                                   Common block CBLLF is initialized in block
C                                   data BDLLF.
C     L. Brown        30-Sep-1994   1.2 ==> 1.3
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   Removed unnecessary call to CONCAT routine.
C                                   Call to CONCAT has been replaced by call to
C                                   STRCCT.
C     L. Brown        20-Oct-1994   1.3 ==> 1.4
C                                   A call to STRLCA insures that the full file
C                                   specification of the LLF EOF file is in
C                                   lower-case letters.
C     L. Brown        27-Jun-1995   1.4 ==> 1.5
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.5 ==> 1.6
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Input variables
C
      INTEGER LUN
      CHARACTER*(*) PATH
      LOGICAL BYTES
C
C  Output variables
C
      INTEGER NALT
      REAL ALT(MALT)
C
C  Local variables
C
      INTEGER ISTAT,IALT,IEOF
      REAL DUM1,DUM2
      CHARACTER*160 FSPEC
C
C  If the case identifier is different than that of the previous call, then the
C  LLF EOF file must be read
C
      IF(ID .NE. PRVID) THEN
C
C  Determine the full specification of the LLF EOF file
C
         CALL STRCCT(PATH,ID//'.OE',32,FSPEC,ISTAT)
         CALL STRLCA(FSPEC,ISTAT)
C
C  Open the LLF EOF file.  Note that in the VAX Fortran implementation, the
C  record-length of unformatted direct-access files (specified by the RECL
C  keyword) is given in longwords (4 byte units), corresponding to the space
C  required for an integer*4 or a real*4.  In other implementations, the
C  record-length might be given in bytes.
C
         IF(BYTES) THEN
            OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     &           ACCESS='DIRECT',RECL=MALT*4)
         ELSE
            OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     &           ACCESS='DIRECT',RECL=MALT)
         ENDIF
C
C  Read header information from the LLF EOF file
C
         READ(LUN,REC=1) DUM1,DUM2
         YEAR=INT(DUM1)
         DAY=INT(DUM2)
         READ(LUN,REC=2) F10P7,AP,KP
         READ(LUN,REC=3) MLON
         READ(LUN,REC=4) DUM1,SMLAT,DMLAT
         NMLAT=INT(DUM1)
         READ(LUN,REC=5) DUM1,SMLT,DMLT
         NMLT=INT(DUM1)
C
C  Read the altitude grid from the LLF EOF file
C
         READ(LUN,REC=6) DUM1
         NALT=INT(DUM1)
         READ(LUN,REC=7) (ALT(IALT),IALT=1,NALT)
C
C  Read the eigenvalues from the LLF EOF file
C
         READ(LUN,REC=8) (D(IEOF),IEOF=1,NALT)
         READ(LUN,REC=9) (DSHFT(IEOF),IEOF=1,NALT)
C
C  Read the EOFs from the LLF EOF file
C
         DO 10 IEOF=1,NALT
            READ(LUN,REC=9+IEOF) (EOF(IALT,IEOF),IALT=1,NALT)
   10    CONTINUE
C
C  Close the LLF EOF file
C
         CLOSE(UNIT=LUN)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE LLFOPC(PATH,LUN,BYTES)
C
C  PURPOSE
C     Reads a LLF orthogonal polynomial coefficients file.
C
C  METHOD
C     The name of the LLF orthogonal polynomial coefficients file is determined
C     from the case identifier.  Header information is read from the LLF
C     orthogonal polynomial coefficients file.  The orthogonal polynomial
C     coefficients are read from the LLF orthogonal polynomial coefficients
C     file.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     LUN      The logical unit number used to access the LLF orthogonal
C              polynomial coefficients file
C     PATH     The location of the LLF orthogonal polynomial coefficients file
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FSPEC    The full specification of the LLF orthogonal polynomial
C              coefficients file
C     IEOF     The EOF coefficient index
C     IMLT     The magnetic local time index
C     IOPM11   The orthogonal polynomial coefficient index
C     IREC     A record number in the LLF orthogonal polynomial coefficients
C              file
C     ISTAT    The status returned from the STRCCT and STRLCA routines
C     NOPM11   NOPM1+1
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters in a character string to lower-case
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     #LUN     The LLF orthogonal polynomial coefficients file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown        26-Feb-1993   1.0 ==> 1.1
C                                   Added location for LLF orthogonal
C                                   polynomial coefficients file.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBLLF for internal use
C                                   by the LLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to LLF, as is appropriate.
C                                   Common block CBLLF is initialized in block
C                                   data BDLLF.
C     L. Brown        30-Sep-1994   1.2 ==> 1.3
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   The LLF orthogonal polynomial coefficients
C                                   file has been restructured to reflect the
C                                   elimination of Fourier fitting in UT in the
C                                   low-latitude F-region parameterized model
C                                   (LLF).
C                                   Removed unnecessary call to CONCAT routine.
C                                   Call to CONCAT has been replaced by call to
C                                   STRCCT.
C     L. Brown        20-Oct-1994   1.3 ==> 1.4
C                                   A call to STRLCA insures that the full file
C                                   specification of the LLF orthogonal
C                                   polynomial coefficients file is in
C                                   lower-case letters.
C     L. Brown        27-Jun-1995   1.4 ==> 1.5
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.5 ==> 1.6
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Input variables
C
      INTEGER LUN
      CHARACTER*(*) PATH
      LOGICAL BYTES
C
C  Local variables
C
      INTEGER ISTAT,IMLT,IEOF,IREC,NOPM11,IOPM11
      REAL DUM1,DUM2
      CHARACTER*160 FSPEC
C
C  If the case identifier is different than that of the previous call, then the
C  LLF orthogonal polynomial coefficients file must be read
C
      IF(ID .NE. PRVID) THEN
C
C  Determine the full specification of the LLF orthogonal polynomial
C  coefficients file
C
         CALL STRCCT(PATH,ID//'.OP',32,FSPEC,ISTAT)
         CALL STRLCA(FSPEC,ISTAT)
C
C  Open the LLF orthogonal polynomial coefficients file.  Note that in the VAX
C  Fortran implementation, the record-length of unformatted direct-access files
C  (specified by the RECL keyword) is given in longwords (4 byte units),
C  corresponding to the space required for an integer*4 or a real*4.  In other
C  implementations, the record-length might be given in bytes.
C
         IF(BYTES) THEN
            OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     &           ACCESS='DIRECT',RECL=(MOPM1+1)*4)
         ELSE
            OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     &           ACCESS='DIRECT',RECL=MOPM1+1)
         ENDIF
C
C  Read header information from the LLF orthogonal polynomial coefficients file
C
         READ(LUN,REC=1) DUM1,DUM2
         YEAR=INT(DUM1)
         DAY=INT(DUM2)
         READ(LUN,REC=2) F10P7,AP,KP
         READ(LUN,REC=3) MLON
         READ(LUN,REC=4) DUM1,SMLAT,DMLAT
         NMLAT=INT(DUM1)
         READ(LUN,REC=5) DUM1,SMLT,DMLT
         NMLT=INT(DUM1)
         READ(LUN,REC=6) DUM1,DUM2
         NOPM1=INT(DUM1)
         NEOF=INT(DUM2)
         READ(LUN,REC=7) DELTA
C
C  Determine the starting record number in the LLF orthogonal polynomial
C  coefficients file
C
         IREC=8
C
C  Read the orthogonal polynomial coefficients from the LLF orthogonal
C  polynomial coefficients file
C
         NOPM11=NOPM1+1
         DO 20 IMLT=1,NMLT
            DO 10 IEOF=1,NEOF
               READ(LUN,REC=IREC) (OPC(IOPM11,IEOF,IMLT),
     &                             IOPM11=1,NOPM11)
               IREC=IREC+1
   10       CONTINUE
   20    CONTINUE
C
C  Close the LLF orthogonal polynomial coefficients file
C
         CLOSE(UNIT=LUN)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE LLFEC(MLT,MLAT)
C
C  PURPOSE
C     To generate EOF coefficients from orthogonal polynomial coefficients
C     for LLF.
C
C  METHOD
C     A set of orthogonal polynomials is determined.  The orthogonal
C     polynomials are combined with the orthogonal polynomial coefficients to
C     produce EOF coefficients for the given magnetic latitude.  If the
C     magnetic latitude does not lie exactly on the magnetic latitude grid,
C     then the orthogonal polynomial is linearly interpolated from orthogonal
C     polynomials at the two nearest magnetic latitudes on the magnetic
C     latitude grid.  If the magnetic local time does not lie exactly on the
C     magnetic local time grid, then the orthogonal polynomial coefficients are
C     linearly interpolated from orthogonal polynomial coefficients at the two
C     nearest magnetic local times on the magnetic local time grid.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     IEOF     The loop counter for EOFs
C     IMLAT    The magnetic latitude index
C     IMLAT1   A magnetic latitude index used for interpolation
C     IMLAT2   A magnetic latitude index used for interpolation
C     IMLT     The magnetic local time index
C     IMLT1    A magnetic local time index used for interpolation
C     IMLT2    A magnetic local time index used for interpolation
C     IOPM11   The loop counter for orthogonal polynomials
C     MLATG    The grid magnetic latitude, in degrees north
C     MLTG     The grid magnetic local time, in decimal hours
C     MLTR     The magnetic local time, in decimal hours, restricted to the
C              range [>=0,<24]
C     NOPM11   NOPM1+1
C     ONEMTM   1.-TMLAT
C     ONEMTT   1.-TMLT
C     OP       Orthogonal polynomials
C     OPINT    An interpolated orthogonal polynomial
C     TMLAT    The interpolation factor for magnetic latitude
C     TMLT     The interpolation factor for magnetic local time
C     X        The grid for the orthogonal polynomials
C
C  SUBROUTINES REQUIRED
C     LLFOP    Generates the orthogonal polynomials
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Added common block CBLLF for internal use
C                                   by the LLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to LLF, as is appropriate.
C                                   Common block CBLLF is initialized in block
C                                   data BDLLF.
C     L. Brown        30-Sep-1994   1.1 ==> 1.2
C                                   Renamed from LLFFC to LLFEC and modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C     L. Brown        27-Jun-1995   1.2 ==> 1.3
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C                                   Changed the value of PARAMETER MX from 33
C                                   to 35 to accommodate the expanded magnetic
C                                   latitude range of the new LLF
C                                   parameterization.
C     L. Brown         1-May-1997   1.3 ==> 1.4
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C                                   Increased PARAMETER MX from 35 to 45.
C
C  REFERENCES
C     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
C        The Golam Press, Boulder, CO, 1973, pp 90-92.
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     MX       The maximum size of the orthogonal polynomial grid
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
      INTEGER MX
      PARAMETER(MX=45)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Input variables
C
      REAL MLT,MLAT
C
C  Local variables
C
      INTEGER NOPM11,IMLAT,IMLT,IMLT1,IMLT2,IMLAT1,IMLAT2,IEOF,IOPM11
      REAL MLTR,MLTG,MLATG,TMLT,ONEMTT,TMLAT,ONEMTM
      REAL X(MX),OP(MX,MOPM1+1),OPINT(MOPM1+1)
C
C  If the case identifier, magnetic local time, or magnetic latitude are
C  different than those of the previous call, then the EOF coefficients must be
C  regenerated
C
      IF((ID .NE. PRVID) .OR. (MLT .NE. PRVMLT) .OR. (MLAT .NE. PRVMLA))
     &   THEN
C
C  Calculate NOPM1+1
C
         NOPM11=NOPM1+1
C
C  Calculate the grid for the orthogonal polynomials
C
         DO 10 IMLAT=1,NMLAT
            X(IMLAT)=DELTA*FLOAT(IMLAT-1)-1.
   10    CONTINUE
C
C  Generate the orthogonal polynomials
C
         CALL LLFOP(NOPM1,NMLAT,X,OP)
C
C  Restrict the magnetic local time to the range [>=0,<24]
C
         MLTR=MOD(24.+MOD(MLT,24.),24.)
C
C  Calculate the grid magnetic local time index
C
         IMLT=1+INT((MLTR-SMLT)/DMLT)
C
C  Calculate the grid magnetic local time
C
         MLTG=SMLT+DMLT*FLOAT(IMLT-1)
C
C  Calculate the grid magnetic latitude index
C
         IMLAT=MIN(NMLAT,MAX(1,1+INT((MLAT-SMLAT)/DMLAT)))
C
C  Calculate the grid magnetic latitude
C
         MLATG=SMLAT+DMLAT*FLOAT(IMLAT-1)
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then no
C  interpolation of the orthogonal polynomials in magnetic latitude is
C  necessary
C
         IF(MLAT .EQ. MLATG) THEN
C
C  If the magnetic local time is exactly on the magnetic local time grid, then
C  no interpolation of the orthogonal polynomial coefficients in magnetic local
C  time is necessary
C
            IF(MLTR .EQ. MLTG) THEN
C
C  Loop over the number of EOFs used to construct an O+ density altitude
C  profile
C
               DO 200 IEOF=1,NEOF
C
C  Initialize the EOF coefficient
C
                  EC(IEOF)=0.
C
C  Loop over the orthogonal polynomial coefficients
C
                  DO 100 IOPM11=1,NOPM11
C
C  Add the contribution to the EOF coefficient by the orthogonal polynomial
C
                     EC(IEOF)=EC(IEOF)
     &                       +OPC(IOPM11,IEOF,IMLT)*OP(IMLAT,IOPM11)
C
C  End the loop over orthogonal polynomial coefficients
C
  100             CONTINUE
C
C  End the loop over the number of EOFs used to construct an O+ density
C  altitude profile
C
  200          CONTINUE
C
C  If the magnetic local time is not exactly on the magnetic local time grid,
C  then interpolation of the orthogonal polynomial coefficients in magnetic
C  local time is necessary
C
            ELSE
C
C  Determine the indices of the two nearest magnetic local times on the
C  magnetic local time grid and the magnetic local time interpolation factor
C
               IF(MLTR .LT. SMLT) THEN
                  IMLT1=NMLT
                  IMLT2=1
                  TMLT=(MLTR-SMLT)/DMLT+1.
               ELSE
                  IF(IMLT .EQ. NMLT) THEN
                     IMLT1=NMLT
                     IMLT2=1
                  ELSE
                     IMLT1=IMLT
                     IMLT2=IMLT+1
                  ENDIF
                  TMLT=(MLTR-MLTG)/DMLT
               ENDIF
               ONEMTT=1.-TMLT
C
C  Loop over the number of EOFs used to construct an O+ density altitude
C  profile
C
               DO 400 IEOF=1,NEOF
C
C  Initialize the EOF coefficient
C
                  EC(IEOF)=0.
C
C  Loop over the orthogonal polynomial coefficients
C
                  DO 300 IOPM11=1,NOPM11
C
C  Add the contribution to the EOF coefficient by the orthogonal polynomial,
C  linearly interpolating the orthogonal polynomial coefficients from the
C  orthogonal polynomial coefficients at the two nearest magnetic local times
C  on the magnetic local time grid
C
                     EC(IEOF)=EC(IEOF)
     &                       +(ONEMTT*OPC(IOPM11,IEOF,IMLT1)
     &                        +TMLT*OPC(IOPM11,IEOF,IMLT2))
     &                       *OP(IMLAT,IOPM11)
C
C  End the loop over orthogonal polynomial coefficients
C
  300             CONTINUE
C
C  End the loop over the number of EOFs used to construct an O+ density
C  altitude profile
C
  400          CONTINUE
C
            ENDIF
C
C  If the magnetic latitude is not exactly on the magnetic latitude grid, then
C  interpolation of the orthogonal polynomials in magnetic latitude is
C  necessary
C
         ELSE
C
C  Determine the indices of the two nearest magnetic latitudes on the magnetic
C  latitude grid and the magnetic latitude interpolation factor
C
            IF(IMLAT .EQ. NMLAT) THEN
               IMLAT1=NMLAT-1
               IMLAT2=NMLAT
               TMLAT=(MLAT-MLATG)/DMLAT+1.
            ELSE
               IMLAT1=IMLAT
               IMLAT2=IMLAT+1
               TMLAT=(MLAT-MLATG)/DMLAT
            ENDIF
            ONEMTM=1.-TMLAT
C
C  Linearly interpolate the orthogonal polynomial from the orthogonal
C  polynomials at the two nearest magnetic latitudes on the magnetic latitude
C  grid
C
            DO 500 IOPM11=1,NOPM11
               OPINT(IOPM11)=ONEMTM*OP(IMLAT1,IOPM11)
     &                      +TMLAT*OP(IMLAT2,IOPM11)
  500       CONTINUE
C
C  If the magnetic local time is exactly on the magnetic local time grid, then
C  no interpolation of the orthogonal polynomial coefficients in magnetic local
C  time is necessary
C
            IF(MLTR .EQ. MLTG) THEN
C
C  Loop over the number of EOFs used to construct an O+ density altitude
C  profile
C
               DO 700 IEOF=1,NEOF
C
C  Initialize the EOF coefficient
C
                  EC(IEOF)=0.
C
C  Loop over the orthogonal polynomial coefficients
C
                  DO 600 IOPM11=1,NOPM11
C
C  Add the contribution to the EOF coefficient by the interpolated orthogonal
C  polynomial
C
                     EC(IEOF)=EC(IEOF)
     &                       +OPC(IOPM11,IEOF,IMLT)*OPINT(IOPM11)
C
C  End the loop over orthogonal polynomial coefficients
C
  600             CONTINUE
C
C  End the loop over the number of EOFs used to construct an O+ density
C  altitude profile
C
  700          CONTINUE
C
C  If the magnetic local time is not exactly on the magnetic local time grid,
C  then interpolation of the orthogonal polynomial coefficients in magnetic
C  local time is necessary
C
            ELSE
C
C  Determine the indices of the two nearest magnetic local times on the
C  magnetic local time grid and the magnetic local time interpolation factor
C
               IF(MLTR .LT. SMLT) THEN
                  IMLT1=NMLT
                  IMLT2=1
                  TMLT=(MLTR-SMLT)/DMLT+1.
               ELSE
                  IF(IMLT .EQ. NMLT) THEN
                     IMLT1=NMLT
                     IMLT2=1
                  ELSE
                     IMLT1=IMLT
                     IMLT2=IMLT+1
                  ENDIF
                  TMLT=(MLTR-MLTG)/DMLT
               ENDIF
               ONEMTT=1.-TMLT
C
C  Loop over the number of EOFs used to construct an O+ density altitude
C  profile
C
               DO 900 IEOF=1,NEOF
C
C  Initialize the EOF coefficient
C
                  EC(IEOF)=0.
C
C  Loop over the orthogonal polynomial coefficients
C
                  DO 800 IOPM11=1,NOPM11
C
C  Add the contribution to the EOF coefficient by the interpolated orthogonal
C  polynomial, linearly interpolating the orthogonal polynomial coefficients
C  from the orthogonal polynomial coefficients at the two nearest magnetic
C  local times on the magnetic local time grid
C
                     EC(IEOF)=EC(IEOF)
     &                       +(ONEMTT*OPC(IOPM11,IEOF,IMLT1)
     &                        +TMLT*OPC(IOPM11,IEOF,IMLT2))
     &                       *OPINT(IOPM11)
C
C  End the loop over orthogonal polynomial coefficients
C
  800             CONTINUE
C
C  End the loop over the number of EOFs used to construct an O+ density
C  altitude profile
C
  900          CONTINUE
C
            ENDIF
C
         ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE LLFOP(NOPM1,NX,X,U)
C
C  PURPOSE
C     To generate orthogonal polynomials for LLF.
C
C  METHOD
C     See the reference for a description of orthogonal polynomials.
C
C  INPUT PARAMETERS
C     NOPM1    The number of orthogonal polynomials - 1
C     NX       The number of points on the grid for the orthogonal polynomials
C     X        The grid for the orthogonal polynomials
C
C  OUTPUT PARAMETERS
C     U        Orthogonal polynomials
C
C  LOCAL VARIABLES
C     B        A term in the recursion relation
C     H2       A term in the recursion relation
C     IOP      The loop counter for orthogonal polynomials
C     IX       The loop counter for the grid for the orthogonal polynomials
C     RATIO    A term in the recursion relation
C
C  SUBROUTINES REQUIRED
C     None
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Loop counter variables renamed to be more
C                                   indicative of their function.
C     L. Brown        27-Jun-1995   1.1 ==> 1.2
C                                   Changed the value of PARAMETER MX from 33
C                                   to 35 to accommodate the expanded magnetic
C                                   latitude range of the new LLF
C                                   parameterization.
C     L. Brown         1-May-1997   1.2 ==> 1.3
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C                                   Increased PARAMETER MX from 35 to 45.
C
C  REFERENCES
C     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
C        The Golam Press, Boulder, CO, 1973, pp 90-92.
C
C  SPECIAL CONSTANTS
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     MX       The maximum size of the orthogonal polynomial grid
C
      INTEGER MOPM1,MX
      PARAMETER(MOPM1=14,MX=45)
C
C  Input variables
C
      INTEGER NOPM1,NX
      REAL U(MX,MOPM1+1)
C
C  Output variables
C
      REAL X(MX)
C
C  Local variables
C
      INTEGER IX,IOP
      REAL RATIO
      REAL H2(MOPM1+1),B(MOPM1+1)
C
C  Calculate U(X,1)
C
      H2(1)=FLOAT(NX)
      B(1)=0.
      DO 10 IX=1,NX
         U(IX,1)=1.
         B(1)=B(1)+X(IX)
   10 CONTINUE
      B(1)=B(1)/H2(1)
C
C  Calculate U(X,2)
C
      H2(2)=0.
      B(2)=0.
      DO 100 IX=1,NX
         U(IX,2)=X(IX)-B(1)
         H2(2)=H2(2)+U(IX,2)**2
         B(2)=B(2)+X(IX)*(U(IX,2)**2)
  100 CONTINUE
      B(2)=B(2)/H2(2)
C
C  Use the recursion relationship to calculate U(X,N) where N=3,NOPM1+1
C
      DO 210 IOP=3,NOPM1+1
         H2(IOP)=0.
         B(IOP)=0.
         RATIO=H2(IOP-1)/H2(IOP-2)
         DO 200 IX=1,NX
            U(IX,IOP)=(X(IX)-B(IOP-1))*U(IX,IOP-1)-RATIO*U(IX,IOP-2)
            H2(IOP)=H2(IOP)+U(IX,IOP)**2
            B(IOP)=B(IOP)+X(IX)*(U(IX,IOP)**2)
  200    CONTINUE
         B(IOP)=B(IOP)/H2(IOP)
  210 CONTINUE
C
      RETURN
      END
      SUBROUTINE LLFDEN(MLT,MLAT,NALT,DEN)
C
C  PURPOSE
C     To construct a LLF O+ density altitude profile from EOFs and EOF
C     coefficients.
C
C  METHOD
C     The EOFs are combined with the EOF coefficients to produce the natural
C     logarithm of a LLF O+ density altitude profile.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     NALT     The number of altitude points
C
C  OUTPUT PARAMETERS
C     DEN      The O+ density altitude profile, in cm-3
C
C  LOCAL VARIABLES
C     IALT     The loop counter for altitudes
C     IEOF     The loop counter for EOFs
C
C  SUBROUTINES REQUIRED
C     None
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         4-Jun-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Added common block CBLLF for internal use
C                                   by the LLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to LLF, as is appropriate.
C                                   Common block CBLLF is initialized in block
C                                   data BDLLF.
C     L. Brown        30-Sep-1994   1.1 ==> 1.2
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C     L. Brown        27-Jun-1995   1.2 ==> 1.3
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.3 ==> 1.4
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Input variables
C
      INTEGER NALT
      REAL MLT,MLAT
C
C  Output variables
C
      REAL DEN(MALT)
C
C  Local variables
C
      INTEGER IALT,IEOF
C
C  If the case identifier, magnetic local time, or magnetic latitude are
C  different than those of the previous call, then the O+ density altitude
C  profile must be reconstructed
C
      IF((ID .NE. PRVID) .OR. (MLT .NE. PRVMLT) .OR. (MLAT .NE. PRVMLA))
     &   THEN
C
C  Loop over altitude
C
         DO 100 IALT=1,NALT
C
C  Initialize the natural logarithm of the O+ density at a given altitude
C
            DEN(IALT)=0.
C
C  Sum the EOF terms for the natural logarithm of the O+ density
C
            DO 10 IEOF=1,NEOF
               DEN(IALT)=DEN(IALT)+EC(IEOF)*EOF(IALT,IEOF)
   10       CONTINUE
C
C  Convert the result to an actual O+ density by exponentiation
C
            DEN(IALT)=EXP(DEN(IALT))
C
C  End of the loop over altitude
C
  100    CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE LLFHIS(XDAY,MLT,MLAT,SECTOR,XF10P7,USEDEC)
C
C  PURPOSE
C     To store historical quantities for the next call to LLF.
C
C  METHOD
C     The day of the year, magnetic local time, magnetic latitude, magnetic
C     longitude sector, 10.7 cm solar flux, logical flag USEDEC, and case
C     identifier are stored for the next call to LLF.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
C              Indian, 'PAC' for Pacific, and 'USA' for USA
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the month descriptor, .FALSE. if the day of the
C              year is to be used
C     XDAY     The day of the year
C     XF10P7   The 10.7 cm solar flux, in solar flux units
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     None
C
C  SUBROUTINES REQUIRED
C     None
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         3-Mar-1993   1.0 ==> Created
C     L. Brown        30-Sep-1994   1.0 ==> 1.1
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C     L. Brown        27-Jun-1995   1.1 ==> 1.2
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.2 ==> 1.3
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     MMLT     The maximum number of magnetic local times
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Input variables
C
      INTEGER XDAY
      REAL MLT,MLAT,XF10P7
      CHARACTER*3 SECTOR
      LOGICAL USEDEC
C
C  Store the day of the year
C
      PRVDAY=XDAY
C
C  Store the magnetic local time
C
      PRVMLT=MLT
C
C  Store the magnetic latitude
C
      PRVMLA=MLAT
C
C  Store the magnetic longitude sector
C
      PRVSCT=SECTOR
C
C  Store the 10.7 cm solar flux
C
      PRVF10=XF10P7
C
C  Store the logical flag USEDEC
C
      PRVUSD=USEDEC
C
C  Store the case identifier
C
      PRVID=ID
C
      RETURN
      END
      BLOCK DATA BDLLF
C
C  PURPOSE
C     To initialize the contents of common block CBLLF.
C
C  METHOD
C     Common block CBLLF is intended for internal use by the LLF modules.
C
C  INPUT PARAMETERS
C     None
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     None
C
C  SUBROUTINES REQUIRED
C     None
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     None
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   1-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         3-Mar-1993   1.0 ==> Created
C     L. Brown        30-Sep-1994   1.0 ==> 1.1
C                                   Common block CBLLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   Character variable PRVID initialized to
C                                   string constant of proper length.
C     L. Brown        27-Jun-1995   1.1 ==> 1.2
C                                   Changed all references to universal time to
C                                   magnetic local time since the LLF model has
C                                   been reparameterized in magnetic local time
C                                   instead of universal time.
C     L. Brown         1-May-1997   1.2 ==> 1.3
C                                   Increased PARAMETER MOPM1 from 11 to 14.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MAE      MALT*MEOF
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOEM     (MOPM1+1)*MEOF*MMLT
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MOPM1,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MMLT=48)
      INTEGER MAE,MOEM
      PARAMETER(MAE=MALT*MEOF,MOEM=(MOPM1+1)*MEOF*MMLT)
C
C  Common block CBLLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSCT   The magnetic longitude sector from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLAT,NMLT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DELTA,PRVMLT,PRVMLA,
     &     PRVF10
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLT),
     &     EC(MEOF)
      CHARACTER*3 PRVSCT
      CHARACTER*8 ID,PRVID
      LOGICAL PRVUSD
      COMMON/CBLLF/ID,YEAR,DAY,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,NMLT,
     &             SMLT,DMLT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,OPC,EC,PRVDAY,
     &             PRVMLT,PRVMLA,PRVSCT,PRVF10,PRVUSD,PRVID
C
C  Initialize common block CBLLF
C
      DATA ID/'        '/
      DATA YEAR/0/
      DATA DAY/0/
      DATA F10P7/0./
      DATA AP/0./
      DATA KP/0./
      DATA MLON/0./
      DATA NMLAT/0/
      DATA SMLAT/0./
      DATA DMLAT/0./
      DATA NMLT/0/
      DATA SMLT/0./
      DATA DMLT/0./
      DATA D/MEOF*0./
      DATA DSHFT/MEOF*0./
      DATA EOF/MAE*0./
      DATA NOPM1/0/
      DATA NEOF/0/
      DATA DELTA/0./
      DATA OPC/MOEM*0./
      DATA EC/MEOF*0./
      DATA PRVDAY/9999/
      DATA PRVMLT/9999./
      DATA PRVMLA/9999./
      DATA PRVSCT/'999'/
      DATA PRVF10/9999./
      DATA PRVUSD/.TRUE./
      DATA PRVID/'99999999'/
C
      END
