      SUBROUTINE MLF(PATH,LUN,DAY,UT,MLAT,MLON,F10P7,KP,USEDEC,USEAP,
     &               BYTES,NALT,ALT,DEN)
C
C  PURPOSE
C     A driver for constructing O+ density altitude profiles from the
C     parameterized mid-latitude MLF database using modular MLF database access
C     and manipulation routines.
C
C  METHOD
C     From the given input parameters, the MLF case identifier is determined.
C     EOFs for the case are read from the appropriate MLF EOF file.
C     Orthogonal polynomial coefficients for the case and universal time are
C     read from the appropriate MLF orthogonal polynomial coefficients file.
C     The orthogonal polynomial coefficients describe the behavior of EOF
C     coefficients in magnetic latitude for a given magnetic longitude and
C     universal time.  EOF coefficients are determined from the orthogonal
C     polynomial coefficients.  The EOF coefficients, in combination with the
C     EOFs, describe the O+ density altitude profile for a given magnetic
C     latitude, magnetic longitude, and universal time.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     KP       The magnetic Kp index
C     LUN      The logical unit number used to access MLF database files
C     MLAT     The magnetic latitude, in degrees north
C     MLON     The magnetic longitude, in degrees east
C     PATH     The location of MLF database files
C     USEAP    A logical flag, .TRUE. if Ap is to be used to determine the
C              magnetic activity level descriptor, .FALSE. if Kp is to be
C              used
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the season descriptor, .FALSE. if the day of the
C              year is to be used
C     UT       The universal time, in decimal hours
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
C     MLFDEN   Generates an O+ density altitude profile from EOFs and EOF
C              coefficients
C     MLFEC    Generates EOF coefficients from orthogonal polynomial
C              coefficients
C     MLFEOF   Reads a MLF EOF file
C     MLFHIS   Stores historical quantities for the next call to MLF
C     MLFID    Determines the case identifier
C     MLFOPC   Reads a MLF orthogonal polynomial coefficients file
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
C     1.4   30-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown        28-May-1991   1.0 ==> 1.1
C                                   References to ion species removed since
C                                   only O+ is represented.  Internal
C                                   documentation improved.
C     L. Brown        26-Feb-1993   1.1 ==> 1.2
C                                   Added user-specified location and logical
C                                   unit number for MLF database files.
C     L. Brown         3-Mar-1993   1.2 ==> 1.3
C                                   Added common block CBMLF for internal use
C                                   by the MLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to MLF, as is appropriate.
C                                   Common block CBMLF is initialized in block
C                                   data BDMLF.
C     L. Brown        30-Sep-1994   1.3 ==> 1.4
C                                   The calls to MLFFC and MLFEC have been
C                                   combined into a single call to MLFEC to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C
      INTEGER MALT
      PARAMETER(MALT=49)
C
C  Input variables
C
      INTEGER LUN,DAY
      REAL UT,MLON,MLAT,F10P7,KP
      CHARACTER*(*) PATH
      LOGICAL USEDEC,USEAP,BYTES
C
C  Output variables
C
      INTEGER NALT
      REAL ALT(MALT),DEN(MALT)
C
C  Determine the case identifier
C
      CALL MLFID(DAY,MLAT,F10P7,KP,USEDEC,USEAP)
C
C  Read the MLF EOF file
C
      CALL MLFEOF(PATH,LUN,BYTES,NALT,ALT)
C
C  Read the MLF orthogonal polynomial coefficients file
C
      CALL MLFOPC(PATH,LUN,BYTES,UT)
C
C  Generate EOF coefficients from the orthogonal polynomial coefficients
C
      CALL MLFEC(UT,MLAT,MLON)
C
C  Generate an O+ density altitude profile from the EOFs and EOF coefficients
C
      CALL MLFDEN(UT,MLAT,MLON,NALT,DEN)
C
C  Store historical quantities for the next call to MLF
C
      CALL MLFHIS(DAY,UT,MLAT,MLON,F10P7,KP,USEDEC,USEAP)
C
      RETURN
      END
      SUBROUTINE MLFID(XDAY,MLAT,XF10P7,XKP,USEDEC,USEAP)
C
C  PURPOSE
C     To determine a MLF case identifier.
C
C  METHOD
C     The MLF case identifier is a code for describing the ambient conditions
C     (day of the year, magnetic hemisphere, magnetic activity level, and solar
C     activity level) of a MLF case.  The case identifier has the form 'aHbcd',
C     where 'a' describes the magnetic hemisphere, 'M' defines the MLF cases as
C     Mid-latitude, 'b' describes the season, 'c' describes the magnetic
C     activity level, and 'd' describes the solar actviity level.  The magnetic
C     latitude determines 'a':  'N' for a Northern hemisphere case and 'S' for
C     a Southern hemisphere case.  The day of the year determines 'b':  'W' for
C     Winter, 'S' for Summer, and 'E' for Equinox.  The magnetic Kp index
C     determines 'c':  'L' for Low, 'M' for Moderate, and 'H' for High.  The
C     solar F10.7 index determines 'd':  'L' for Low, 'M' for Moderate, and 'H'
C     for High.  Logical flag USEDEC determines how the season descriptor is
C     chosen.  Logical flag USEAP determines how the magnetic activity level
C     descriptor is chosen.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     USEAP    A logical flag, .TRUE. if Ap is to be used to determine the
C              magnetic activity level descriptor, .FALSE. if Kp is to be
C              used
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the season descriptor, .FALSE. if the day of the
C              year is to be used
C     XDAY     The day of the year
C     XF10P7   The 10.7 cm solar flux, in solar flux units
C     XKP      The magnetic Kp index
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     DEC      The solar declination, in degrees north
C     HEMIS    The hemisphere descriptor, 'N' for a northern hemisphere case,
C              'S' for a southern hemisphere case
C     MAGACT   The magnetic activity level descriptor, 'L' for low, 'M' for
C              moderate, and 'H' for high
C     SEASON   The season descriptor, 'W' for winter, 'S' for summer, and 'E'
C              for equinox
C     SOLACT   The solar activity level descriptor, 'L' for low, 'M' for
C              moderate, and 'H' for high
C     XAP      The magnetic Ap index
C
C  SUBROUTINES REQUIRED
C     None
C
C  FUNCTIONS REQUIRED
C     KPTOAP   Converts Kp to Ap
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
C     1.4   30-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown        28-May-1991   1.0 ==> 1.1
C                                   Internal documentation improved.
C     L. Brown        13-Nov-1991   1.1 ==> 1.2
C                                   Complete rewrite to simplify coding and to
C                                   correct a bug in the determination of the
C                                   season.
C                                   The selection of season and magnetic
C                                   activity level have been enhanced as well
C                                   (see the comments below).
C     L. Brown         3-Mar-1993   1.2 ==> 1.3
C                                   Added common block CBMLF for internal use
C                                   by the MLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to MLF, as is appropriate.
C                                   Common block CBMLF is initialized in block
C                                   data BDMLF.
C     L. Brown        30-Sep-1994   1.3 ==> 1.4
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
C
C  Input variables
C
      INTEGER XDAY
      REAL MLAT,XF10P7,XKP
      LOGICAL USEDEC,USEAP
C
C  Local variables
C
      REAL SOLDEC,DEC,KPTOAP,XAP
      CHARACTER*1 HEMIS,SEASON,MAGACT,SOLACT
C
C  If the day of the year, magnetic latitude, 10.7 cm solar flux, magnetic Kp
C  index, logical flag USEDEC, or logical flag USEAP are different than those
C  of the previous call, then the case identifier must be redetermined
C
      IF((XDAY .NE. PRVDAY) .OR. (MLAT .NE. PRVMLA) .OR.
     &   (XF10P7 .NE. PRVF10) .OR. (XKP .NE. PRVKP) .OR.
     &   ((USEDEC .OR. PRVUSD) .AND. (.NOT. (USEDEC .AND. PRVUSD))) .OR.
     &   ((USEAP .OR. PRVUSA) .AND. (.NOT. (USEAP .AND. PRVUSA)))) THEN
C
C  Note: If the exclusive-OR (.XOR.) logical operator becomes part of the
C        FORTRAN standard, then the last two continuation lines of the above
C        IF statement can be replaced with the following continuation line:
C
C    &   (USEDEC .XOR. PRVUSD) .OR. (USEAP .XOR. PRVUSA)) THEN
C
C
C  Determine the hemisphere descriptor from the magnetic latitude
C
         IF(MLAT .GE. 0.) THEN
            HEMIS='N'
         ELSE
            HEMIS='S'
         ENDIF
C
C  Determine the season descriptor from the solar declination
C  Note: If you want to use the solar declination to determine the season
C        descriptor, then the logical flag USEDEC should be set .TRUE.; if you
C        want to use the day of the year directly, then the logical flag USEDEC
C        should be set .FALSE..  Solar declination is a better indicator of
C        season; however, you may find it more convenient to work with the day
C        of the year directly.
C
         IF(USEDEC) THEN
            DEC=SOLDEC(FLOAT(XDAY))
            IF(DEC .LE. -23.5/2.) THEN
               IF(MLAT .GE. 0.) THEN
                  SEASON='W'
               ELSE
                  SEASON='S'
               ENDIF
            ELSE IF(ABS(DEC) .LT. 23.5/2.) THEN
               SEASON='E'
            ELSE
               IF(MLAT .GE. 0.) THEN
                  SEASON='S'
               ELSE
                  SEASON='W'
               ENDIF
            ENDIF
C
C  Determine the season descriptor directly from the day of the year
C  Note: If you want to use the solar declination to determine the season
C        descriptor, then the logical flag USEDEC should be set .TRUE.; if you
C        want to use the day of the year directly, then the logical flag USEDEC
C        should be set .FALSE..  Solar declination is a better indicator of
C        season; however, you may find it more convenient to work with the day
C        of the year directly.
C
         ELSE
            IF(XDAY .LE. 82) THEN
               IF(365+XDAY-357 .LE. 82-XDAY) THEN
                  IF(MLAT .GE. 0.) THEN
                     SEASON='W'
                  ELSE
                     SEASON='S'
                  ENDIF
               ELSE
                  SEASON='E'
               ENDIF
            ELSE IF((XDAY .GT. 82) .AND. (XDAY .LE. 173)) THEN
               IF(XDAY-82 .LE. 173-XDAY) THEN
                  SEASON='E'
               ELSE
                  IF(MLAT .GE. 0.) THEN
                     SEASON='S'
                  ELSE
                     SEASON='W'
                  ENDIF
               ENDIF
            ELSE IF((XDAY .GT. 173) .AND. (XDAY .LE. (173+357)/2)) THEN
               IF(XDAY-173 .LE. (173+357)/2-XDAY) THEN
                  IF(MLAT .GE. 0.) THEN
                     SEASON='S'
                  ELSE
                     SEASON='W'
                  ENDIF
               ELSE
                  SEASON='E'
               ENDIF
            ELSE IF((XDAY .GT. (173+357)/2) .AND. (XDAY .LE. 357)) THEN
               IF(XDAY-(173+357)/2 .LE. 357-XDAY) THEN
                  SEASON='E'
               ELSE
                  IF(MLAT .GE. 0.) THEN
                     SEASON='W'
                  ELSE
                     SEASON='S'
                  ENDIF
               ENDIF
            ELSE
               IF(MLAT .GE. 0.) THEN
                  SEASON='W'
               ELSE
                  SEASON='S'
               ENDIF
            ENDIF
         ENDIF
C
C  Determine the magnetic activity level descriptor from Ap
C  Note: If you want to use Ap to determine the magnetic activity level
C        descriptor, then the logical flag USEAP should be set .TRUE.; if you
C        want to use Kp directly, then the logical flag USEAP should be set
C        .FALSE..  Ap is a better indicator of magnetic activity level because
C        it is quasi-linear while Kp is quasi-logarithmic; however, you may
C        find it more convenient to work with Kp directly.
C
         IF(USEAP) THEN
            XAP=KPTOAP(XKP)
            IF(XAP .LE. KPTOAP(1.)) THEN
               MAGACT='L'
            ELSE IF((XAP .GT. KPTOAP(1.)) .AND.
     &              (XAP .LE. KPTOAP(3.5))) THEN
               IF(XAP-KPTOAP(1.) .LE. KPTOAP(3.5)-XAP) THEN
                  MAGACT='L'
               ELSE
                  MAGACT='M'
               ENDIF
            ELSE IF((XAP .GT. KPTOAP(3.5)) .AND.
     &              (XAP .LE. KPTOAP(6.))) THEN
               IF(XAP-KPTOAP(3.5) .LE. KPTOAP(6.)-XAP) THEN
                  MAGACT='M'
               ELSE
                  MAGACT='H'
               ENDIF
            ELSE
               MAGACT='H'
            ENDIF
C
C  Determine the magnetic activity level descriptor directly from Kp
C  Note: If you want to use Ap to determine the magnetic activity level
C        descriptor, then the logical flag USEAP should be set .TRUE.; if you
C        want to use Kp directly, then the logical flag USEAP should be set
C        .FALSE..  Ap is a better indicator of magnetic activity level because
C        it is quasi-linear while Kp is quasi-logarithmic; however, you may
C        find it more convenient to work with Kp directly.
C
         ELSE
            IF(XKP .LE. 1.) THEN
               MAGACT='L'
            ELSE IF((XKP .GT. 1.) .AND. (XKP .LE. 3.5)) THEN
               IF(XKP-1. .LE. 3.5-XKP) THEN
                  MAGACT='L'
               ELSE
                  MAGACT='M'
               ENDIF
            ELSE IF((XKP .GT. 3.5) .AND. (XKP .LE. 6.)) THEN
               IF(XKP-3.5 .LE. 6.-XKP) THEN
                  MAGACT='M'
               ELSE
                  MAGACT='H'
               ENDIF
            ELSE
               MAGACT='H'
            ENDIF
         ENDIF
C
C  Determine the solar activity level descriptor from F10.7
C
         IF(XF10P7 .LE. 70.) THEN
            SOLACT='L'
         ELSE IF((XF10P7 .GT. 70.) .AND. (XF10P7 .LE. 130.)) THEN
            IF(XF10P7-70. .LE. 130.-XF10P7) THEN
               SOLACT='L'
            ELSE
               SOLACT='M'
            ENDIF
         ELSE IF((XF10P7 .GT. 130.) .AND. (XF10P7 .LE. 210.)) THEN
            IF(XF10P7-130. .LE. 210.-XF10P7) THEN
               SOLACT='M'
            ELSE
               SOLACT='H'
            ENDIF
         ELSE
            SOLACT='H'
         ENDIF
C
C  Determine the case identifier
C
         ID=HEMIS//'M'//SEASON//MAGACT//SOLACT
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE MLFEOF(PATH,LUN,BYTES,NALT,ALT)
C
C  PURPOSE
C     Reads a MLF EOF file.
C
C  METHOD
C     The name of the MLF EOF file is determined from the case identifier.
C     Header information, followed by the altitude grid, eigenvalues, and EOFs
C     are read from the MLF EOF file.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     LUN      The logical unit number used to access the MLF EOF file
C     PATH     The location of the MLF EOF file
C
C  OUTPUT PARAMETERS
C     ALT      The altitude grid, in km
C     NALT     The number of altitude points
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FSPEC    The full specification of the MLF EOF file
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
C     #LUN     The MLF EOF file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown        28-May-1991   1.0 ==> 1.1
C                                   References to ion species removed since
C                                   only O+ is represented.  Internal
C                                   documentation improved.
C     L. Brown        26-Feb-1993   1.1 ==> 1.2
C                                   Added location for MLF EOF file.
C     L. Brown         3-Mar-1993   1.2 ==> 1.3
C                                   Added common block CBMLF for internal use
C                                   by the MLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to MLF, as is appropriate.
C                                   Common block CBMLF is initialized in block
C                                   data BDMLF.
C     L. Brown        30-Sep-1994   1.3 ==> 1.4
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C                                   Removed unnecessary call to CONCAT routine.
C                                   Call to CONCAT has been replaced by call to
C                                   STRCCT.
C     L. Brown        20-Oct-1994   1.4 ==> 1.5
C                                   A call to STRLCA insures that the full file
C                                   specification of the MLF EOF file is in
C                                   lower-case letters.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
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
C  MLF EOF file must be read
C
      IF(ID .NE. PRVID) THEN
C
C  Determine the full specification of the MLF EOF file
C
         CALL STRCCT(PATH,ID//'.OE',32,FSPEC,ISTAT)
         CALL STRLCA(FSPEC,ISTAT)
C
C  Open the MLF EOF file.  Note that in the VAX Fortran implementation, the
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
C  Read header information from the MLF EOF file
C
         READ(LUN,REC=1) DUM1,DUM2
         YEAR=INT(DUM1)
         DAY=INT(DUM2)
         READ(LUN,REC=2) F10P7,AP,KP
         READ(LUN,REC=3) DUM1,SMLON,DMLON
         NMLON=INT(DUM1)
         READ(LUN,REC=4) DUM1,SMLAT,DMLAT
         NMLAT=INT(DUM1)
         READ(LUN,REC=5) DUM1,SUT,DUT
         NUT=INT(DUM1)
C
C  Read the altitude grid from the MLF EOF file
C
         READ(LUN,REC=6) DUM1
         NALT=INT(DUM1)
         READ(LUN,REC=7) (ALT(IALT),IALT=1,NALT)
C
C  Read the eigenvalues from the MLF EOF file
C
         READ(LUN,REC=8) (D(IEOF),IEOF=1,NALT)
         READ(LUN,REC=9) (DSHFT(IEOF),IEOF=1,NALT)
C
C  Read the EOFs from the MLF EOF file
C
         DO 10 IEOF=1,NALT
            READ(LUN,REC=9+IEOF) (EOF(IALT,IEOF),IALT=1,NALT)
   10    CONTINUE
C
C  Close the MLF EOF file
C
         CLOSE(UNIT=LUN)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE MLFOPC(PATH,LUN,BYTES,UT)
C
C  PURPOSE
C     Reads a MLF orthogonal polynomial coefficients file.
C
C  METHOD
C     The name of the MLF orthogonal polynomial coefficients file is determined
C     from the case identifier.  Header information is read from the MLF
C     orthogonal polynomial coefficients file.  The orthogonal polynomial
C     coefficients for the given universal time are read from the MLF
C     orthogonal polynomial coefficients file.  If the given universal time
C     does not lie exactly on the universal time grid, then the orthogonal
C     polynomial coefficients are linearly interpolated from the orthogonal
C     polynomial coefficients of the two nearest universal times on the
C     universal time grid.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     LUN      The logical unit number used to access the MLF orthogonal
C              polynomial coefficients file
C     PATH     The location of the MLF orthogonal polynomial coefficients file
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FSPEC    The full specification of the MLF orthogonal polynomial
C              coefficients file
C     IEOF     The loop counter for EOFs
C     IMLON    The magnetic longitude index
C     IOPM11   The loop counter for orthogonal polynomials
C     IREC     A record number in the MLF orthogonal polynomial coefficients
C              file
C     IREC1    A record number in the MLF orthogonal polynomial coefficients
C              file
C     IREC2    A record number in the MLF orthogonal polynomial coefficients
C              file
C     ISTAT    The status returned from the STRCCT and STRLCA routines
C     IUT      The universal time index
C     NOPM11   NOPM1+1
C     OPC1     Orthogonal polynomial coefficients used for interpolation
C     OPC2     Orthogonal polynomial coefficients used for interpolation
C     ONEMT    1.-T
C     T        An interpolation factor
C     UTG      The grid universal time, in decimal hours
C     UTR      The universal time, in decimal hours, restricted to the
C              range [>=0,<24]
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters in a character string to lower-case
C
C  FUNCTIONS REQUIRED
C     None
C
C  FILES ACCESSED
C     #LUN     The MLF orthogonal polynomial coefficients file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown        28-May-1991   1.0 ==> 1.1
C                                   References to ion species removed since
C                                   only O+ is represented.  Internal
C                                   documentation improved.
C     L. Brown        26-Feb-1993   1.1 ==> 1.2
C                                   Added location for MLF orthogonal
C                                   polynomial coefficients file.
C     L. Brown         3-Mar-1993   1.2 ==> 1.3
C                                   Added common block CBMLF for internal use
C                                   by the MLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to MLF, as is appropriate.
C                                   Common block CBMLF is initialized in block
C                                   data BDMLF.
C     L. Brown        30-Sep-1994   1.3 ==> 1.4
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C                                   The MLF orthogonal polynomial coefficients
C                                   file has been restructured to reflect the
C                                   elimination of Fourier fitting in magnetic
C                                   longitude in the mid-latitude F-region
C                                   parameterized model (MLF).
C                                   Removed unnecessary call to CONCAT routine.
C                                   Call to CONCAT has been replaced by call to
C                                   STRCCT.
C     L. Brown        20-Oct-1994   1.4 ==> 1.5
C                                   A call to STRLCA insures that the full file
C                                   specification of the MLF orthogonal
C                                   polynomial coefficients file is in
C                                   lower-case letters.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
C
C  Input variables
C
      INTEGER LUN
      REAL UT
      CHARACTER*(*) PATH
      LOGICAL BYTES
C
C  Local variables
C
      INTEGER NOPM11,ISTAT,IUT,IREC,IMLON,IEOF,IOPM11,IREC1,IREC2
      REAL DUM1,DUM2,UTR,UTG,T,ONEMT
      REAL OPC1(MOPM1+1),OPC2(MOPM1+1)
      CHARACTER*160 FSPEC
C
C  If the case identifier or universal time are different than those of the
C  previous call, then the MLF orthogonal polynomial coefficients file must be
C  read
C
      IF((ID .NE. PRVID) .OR. (UT .NE. PRVUT)) THEN
C
C  Determine the full specification of the MLF orthogonal polynomial
C  coefficients file
C
         CALL STRCCT(PATH,ID//'.OP',32,FSPEC,ISTAT)
         CALL STRLCA(FSPEC,ISTAT)
C
C  Open the MLF orthogonal polynomial coefficients file.  Note that in the VAX
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
C  Read header information from the MLF orthogonal polynomial coefficients file
C
         READ(LUN,REC=1) DUM1,DUM2
         YEAR=INT(DUM1)
         DAY=INT(DUM2)
         READ(LUN,REC=2) F10P7,AP,KP
         READ(LUN,REC=3) DUM1,SMLON,DMLON
         NMLON=INT(DUM1)
         READ(LUN,REC=4) DUM1,SMLAT,DMLAT
         NMLAT=INT(DUM1)
         READ(LUN,REC=5) DUM1,SUT,DUT
         NUT=INT(DUM1)
         READ(LUN,REC=6) DUM1,DUM2
         NOPM1=INT(DUM1)
         NEOF=INT(DUM2)
         READ(LUN,REC=7) DELTA
C
C  Calculate NOPM1+1
C
         NOPM11=NOPM1+1
C
C  Restrict the universal time to the range [>=0,<24]
C
         UTR=MOD(24.+MOD(UT,24.),24.)
C
C  Calculate the universal time index
C
         IUT=1+INT((UTR-SUT)/DUT)
C
C  Calculate the grid universal time
C
         UTG=SUT+DUT*FLOAT(IUT-1)
C
C  If the universal time is exactly on the universal time grid, then the
C  orthogonal polynomial coefficients can be read directly from the MLF
C  orthogonal polynomial coefficients file without interpolation
C
         IF(UTR .EQ. UTG) THEN
C
C  Determine the starting record number in the MLF orthogonal polynomial
C  coefficients file
C
            IREC=NMLON*NEOF*(IUT-1)+8
C
C  Read the orthogonal polynomial coefficients for the given universal time
C
            DO 20 IMLON=1,NMLON
               DO 10 IEOF=1,NEOF
                  READ(LUN,REC=IREC) (OPC(IOPM11,IEOF,IMLON),
     &                                IOPM11=1,NOPM11)
                  IREC=IREC+1
   10          CONTINUE
   20       CONTINUE
C
C  Otherwise, the orthogonal polynomial coefficients are interpolated from the
C  orthogonal polynomial coefficients of the two nearest universal times
C
         ELSE
C
C  Determine the starting record numbers in the MLF orthogonal polynomial
C  coefficients file and the interpolation factor
C
            IF(UTR .LT. SUT) THEN
               IREC1=NMLON*NEOF*(NUT-1)+8
               IREC2=8
               T=(UTR-SUT)/DUT+1.
            ELSE
               IF(IUT .EQ. NUT) THEN
                  IREC1=NMLON*NEOF*(NUT-1)+8
                  IREC2=8
               ELSE
                  IREC1=NMLON*NEOF*(IUT-1)+8
                  IREC2=NMLON*NEOF*IUT+8
               ENDIF
               T=(UTR-UTG)/DUT
            ENDIF
            ONEMT=1.-T
C
C  Read the orthogonal polynomial coefficients for the two nearest universal
C  times and linearly interpolate the orthogonal polynomial coefficients at
C  the given universal time
C
            DO 120 IMLON=1,NMLON
               DO 110 IEOF=1,NEOF
                  READ(LUN,REC=IREC1) (OPC1(IOPM11),IOPM11=1,NOPM11)
                  IREC1=IREC1+1
                  READ(LUN,REC=IREC2) (OPC2(IOPM11),IOPM11=1,NOPM11)
                  IREC2=IREC2+1
                  DO 100 IOPM11=1,NOPM11
                     OPC(IOPM11,IEOF,IMLON)=ONEMT*OPC1(IOPM11)
     &                                     +T*OPC2(IOPM11)
  100             CONTINUE
  110          CONTINUE
  120       CONTINUE
C
         ENDIF
C
C  Close the MLF orthogonal polynomial coefficients file
C
         CLOSE(UNIT=LUN)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE MLFEC(UT,MLAT,MLON)
C
C  PURPOSE
C     To generate EOF coefficients from orthogonal polynomial coefficients
C     for MLF.
C
C  METHOD
C     A set of orthogonal polynomials is determined.  The orthogonal
C     polynomials are combined with the orthogonal polynomial coefficients to
C     produce EOF coefficients for the given magnetic latitude.  If the
C     magnetic latitude does not lie exactly on the magnetic latitude grid,
C     then the orthogonal polynomial is linearly interpolated from orthogonal
C     polynomials at the two nearest magnetic latitudes on the magnetic
C     latitude grid.  If the magnetic longitude does not lie exactly on the
C     magnetic longitude grid, then the orthogonal polynomial coefficients are
C     linearly interpolated from orthogonal polynomial coefficients at the two
C     nearest magnetic longitudes on the magnetic longitude grid.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLON     The magnetic longitude, in degrees east
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     IEOF     The loop counter for EOFs
C     IMLAT    The magnetic latitude index
C     IMLAT1   A magnetic latitude index used for interpolation
C     IMLAT2   A magnetic latitude index used for interpolation
C     IMLON    The magnetic longitude index
C     IMLON1   A magnetic longitude index used for interpolation
C     IMLON2   A magnetic longitude index used for interpolation
C     IOPM11   The loop counter for orthogonal polynomials
C     MLATG    The grid magnetic latitude, in degrees north
C     MLONG    The grid magnetic longitude, in degrees east
C     MLONR    The magnetic longitude, in degrees east, restricted to the
C              range [>=0,<360]
C     NOPM11   NOPM1+1
C     ONEMTA   1.-TMLAT
C     ONEMTO   1.-TMLON
C     OP       Orthogonal polynomials
C     OPINT    An interpolated orthogonal polynomial
C     TMLAT    The interpolation factor for magnetic latitude
C     TMLON    The interpolation factor for magnetic longitude
C     X        The grid for the orthogonal polynomials
C
C  SUBROUTINES REQUIRED
C     MLFOP    Generates the orthogonal polynomials
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
C     1.4   23-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown        28-May-1991   1.0 ==> 1.1
C                                   Calculation of magnetic latitude index
C                                   IMLAT corrected to insure that IMLAT does
C                                   not exceed NMLAT.  Internal documentation
C                                   improved.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBMLF for internal use
C                                   by the MLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to MLF, as is appropriate.
C                                   Common block CBMLF is initialized in block
C                                   data BDMLF.
C     L. Brown        30-Sep-1994   1.2 ==> 1.3
C                                   Renamed from MLFFC to MLFEC and modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C     L. Brown        23-May-1997   1.3 ==> 1.4
C                                   Decreased PARAMETER MX from 12 to 11.
C
C  REFERENCES
C     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
C        The Golam Press, Boulder, CO, 1973, pp 90-92.
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     MX       The maximum size of the orthogonal polynomial grid
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
      INTEGER MX
      PARAMETER(MX=11)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
C
C  Input variables
C
      REAL UT,MLAT,MLON
C
C  Local variables
C
      INTEGER NOPM11,IMLAT,IMLON,IMLON1,IMLON2,IMLAT1,IMLAT2,IEOF,IOPM11
      REAL MLATG,MLONR,MLONG,TMLON,ONEMTO,TMLAT,ONEMTA
      REAL X(MX),OP(MX,MOPM1+1),OPINT(MOPM1+1)
C
C  If the case identifier, universal time, magnetic latitude, or magnetic
C  longitude are different than those of the previous call, then the EOF
C  coefficients must be regenerated
C
      IF((ID .NE. PRVID) .OR. (UT .NE. PRVUT) .OR.
     &   (MLAT .NE. PRVMLA) .OR. (MLON .NE. PRVMLO)) THEN
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
         CALL MLFOP(NOPM1,NMLAT,X,OP)
C
C  Calculate the magnetic latitude index
C
         IMLAT=MIN(NMLAT,
     &             MAX(1,1+INT((ABS(MLAT)-ABS(SMLAT))/ABS(DMLAT))))
C
C  Calculate the grid magnetic latitude
C
         MLATG=SMLAT+DMLAT*FLOAT(IMLAT-1)
C
C  Restrict the magnetic longitude to the range [>=0,<360]
C
         MLONR=MOD(360.+MOD(MLON,360.),360.)
C
C  Calculate the grid magnetic longitude index
C
         IMLON=1+INT((MLONR-SMLON)/DMLON)
C
C  Calculate the grid magnetic longitude
C
         MLONG=SMLON+DMLON*FLOAT(IMLON-1)
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then no
C  interpolation of the orthogonal polynomials in magnetic latitude is
C  necessary
C
         IF(MLAT .EQ. MLATG) THEN
C
C  If the magnetic longitude is exactly on the magnetic longitude grid, then no
C  interpolation of the orthogonal polynomial coefficients in magnetic
C  longitude is necessary
C
            IF(MLONR .EQ. MLONG) THEN
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
     &                       +OPC(IOPM11,IEOF,IMLON)*OP(IMLAT,IOPM11)
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
C  If the magnetic longitude is not exactly on the magnetic longitude grid,
C  then interpolation of the orthogonal polynomial coefficients in magnetic
C  longitude is necessary
C
            ELSE
C
C  Determine the indices of the two nearest magnetic longitudes on the
C  magnetic longitude grid and the magnetic longitude interpolation factor
C
               IF(MLONR .LT. SMLON) THEN
                  IMLON1=NMLON
                  IMLON2=1
                  TMLON=(MLONR-SMLON)/DMLON+1.
               ELSE
                  IF(IMLON .EQ. NMLON) THEN
                     IMLON1=NMLON
                     IMLON2=1
                  ELSE
                     IMLON1=IMLON
                     IMLON2=IMLON+1
                  ENDIF
                  TMLON=(MLONR-MLONG)/DMLON
               ENDIF
               ONEMTO=1.-TMLON
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
C  orthogonal polynomial coefficients at the two nearest magnetic longitudes
C  on the magnetic longitude grid
C
                     EC(IEOF)=EC(IEOF)
     &                       +(ONEMTO*OPC(IOPM11,IEOF,IMLON1)
     &                        +TMLON*OPC(IOPM11,IEOF,IMLON2))
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
            ONEMTA=1.-TMLAT
C
C  Linearly interpolate the orthogonal polynomial from the orthogonal
C  polynomials at the two nearest magnetic latitudes on the magnetic latitude
C  grid
C
            DO 500 IOPM11=1,NOPM11
               OPINT(IOPM11)=ONEMTA*OP(IMLAT1,IOPM11)
     &                      +TMLAT*OP(IMLAT2,IOPM11)
  500       CONTINUE
C
C  If the magnetic longitude is exactly on the magnetic longitude grid, then no
C  interpolation of the orthogonal polynomial coefficients in magnetic
C  longitude is necessary
C
            IF(MLONR .EQ. MLONG) THEN
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
     &                       +OPC(IOPM11,IEOF,IMLON)*OPINT(IOPM11)
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
C  If the magnetic longitude is not exactly on the magnetic longitude grid,
C  then interpolation of the orthogonal polynomial coefficients in magnetic
C  longitude is necessary
C
            ELSE
C
C  Determine the indices of the two nearest magnetic longitudes on the
C  magnetic longitude grid and the magnetic longitude interpolation factor
C
               IF(MLONR .LT. SMLON) THEN
                  IMLON1=NMLON
                  IMLON2=1
                  TMLON=(MLONR-SMLON)/DMLON+1.
               ELSE
                  IF(IMLON .EQ. NMLON) THEN
                     IMLON1=NMLON
                     IMLON2=1
                  ELSE
                     IMLON1=IMLON
                     IMLON2=IMLON+1
                  ENDIF
                  TMLON=(MLONR-MLONG)/DMLON
               ENDIF
               ONEMTO=1.-TMLON
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
C  longitudes on the magnetic longitude grid
C
                     EC(IEOF)=EC(IEOF)
     &                       +(ONEMTO*OPC(IOPM11,IEOF,IMLON1)
     &                        +TMLON*OPC(IOPM11,IEOF,IMLON2))
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
      SUBROUTINE MLFOP(NOPM1,NX,X,U)
C
C  PURPOSE
C     To generate orthogonal polynomials.
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
C     1.2   23-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Loop counter variables renamed to be more
C                                   indicative of their function.
C     L. Brown        23-May-1997   1.1 ==> 1.2
C                                   Decreased PARAMETER MX from 12 to 11.
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
      PARAMETER(MOPM1=8,MX=11)
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
      SUBROUTINE MLFDEN(UT,MLAT,MLON,NALT,DEN)
C
C  PURPOSE
C     To construct an O+ density altitude profile from EOFs and EOF
C     coefficients for MLF.
C
C  METHOD
C     The EOFs are combined with the EOF coefficients to produce the natural
C     logarithm of an O+ density altitude profile.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLON     The magnetic longitude, in degrees east
C     NALT     The number of altitude points
C     UT       The universal time, in decimal hours
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
C     1.3   30-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0 ==> Created
C     L. Brown        28-May-1991   1.0 ==> 1.1
C                                   Internal documentation improved.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBMLF for internal use
C                                   by the MLF modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to MLF, as is appropriate.
C                                   Common block CBMLF is initialized in block
C                                   data BDMLF.
C     L. Brown        30-Sep-1994   1.2 ==> 1.3
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
C
C  Input variables
C
      INTEGER NALT
      REAL UT,MLAT,MLON
C
C  Output variables
C
      REAL DEN(MALT)
C
C  Local variables
C
      INTEGER IALT,IEOF
C
C  If the case identifier, universal time, magnetic latitude, or magnetic
C  longitude are different than those of the previous call, then the ion
C  density altitude profile must be reconstructed
C
      IF((ID .NE. PRVID) .OR. (UT .NE. PRVUT) .OR.
     &   (MLAT .NE. PRVMLA) .OR. (MLON .NE. PRVMLO)) THEN
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
      SUBROUTINE MLFHIS(XDAY,UT,MLAT,MLON,XF10P7,XKP,USEDEC,USEAP)
C
C  PURPOSE
C     To store historical quantities for the next call to MLF.
C
C  METHOD
C     The day of the year, universal time, magnetic latitude, magnetic
C     longitude, 10.7 cm solar flux, magnetic Kp index, logical flag USEDEC,
C     logical flag USEAP, and case identifier are stored for the next call
C     to MLF.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLON     The magnetic longitude, in degrees east
C     USEAP    A logical flag, .TRUE. if Ap is to be used to determine the
C              magnetic activity level descriptor, .FALSE. if Kp is to be
C              used
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the month descriptor, .FALSE. if the day of the
C              year is to be used
C     UT       The universal time, in decimal hours
C     XDAY     The day of the year
C     XF10P7   The 10.7 cm solar flux, in solar flux units
C     XKP      The magnetic Kp index
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
C     1.1   30-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         3-Mar-1993   1.0 ==> Created
C     L. Brown        30-Sep-1994   1.0 ==> 1.1
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
C
C  Input variables
C
      INTEGER XDAY
      REAL UT,MLAT,MLON,XF10P7,XKP
      LOGICAL USEDEC,USEAP
C
C  Store the day of the year
C
      PRVDAY=XDAY
C
C  Store the universal time
C
      PRVUT=UT
C
C  Store the magnetic latitude
C
      PRVMLA=MLAT
C
C  Store the magnetic longitude
C
      PRVMLO=MLON
C
C  Store the 10.7 cm solar flux
C
      PRVF10=XF10P7
C
C  Store the magnetic Kp index
C
      PRVKP=XKP
C
C  Store the logical flag USEDEC
C
      PRVUSD=USEDEC
C
C  Store the logical flag USEAP
C
      PRVUSA=USEAP
C
C  Store the case identifier
C
      PRVID=ID
C
      RETURN
      END
      BLOCK DATA BDMLF
C
C  PURPOSE
C     To initialize the contents of common block CBMLF.
C
C  METHOD
C     Common block CBMLF is intended for internal use by the MLF modules.
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
C     1.1   30-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         3-Mar-1993   1.0 ==> Created
C     L. Brown        30-Sep-1994   1.0 ==> 1.1
C                                   Common block CBMLF has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MAE      MALT*MEOF
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOEM     (MOPM1+1)*MEOF*MMLON
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MMLON,MOPM1
      PARAMETER(MALT=49,MEOF=49,MMLON=24,MOPM1=8)
      INTEGER MAE,MOEM
      PARAMETER(MAE=MALT*MEOF,MOEM=(MOPM1+1)*MEOF*MMLON)
C
C  Common block CBMLF
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     DSHFT    Shifted eigenvalues
C     DAY      The day of the year
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     ID       The case identifier
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an O+ density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLO   The magnetic longitude from the previous call, in degrees east
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLON,NMLAT,NUT,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLA,PRVMLO,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MEOF,MMLON),
     &     EC(MEOF)
      CHARACTER*5 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBMLF/ID,YEAR,DAY,F10P7,AP,KP,NMLON,SMLON,DMLON,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NOPM1,NEOF,DELTA,
     &             OPC,EC,PRVDAY,PRVUT,PRVMLA,PRVMLO,PRVF10,PRVKP,
     &             PRVUSD,PRVUSA,PRVID
C
C  Initialize common block CBMLF
C
      DATA ID/'     '/
      DATA YEAR/0/
      DATA DAY/0/
      DATA F10P7/0./
      DATA AP/0./
      DATA KP/0./
      DATA NMLON/0/
      DATA SMLON/0./
      DATA DMLON/0./
      DATA NMLAT/0/
      DATA SMLAT/0./
      DATA DMLAT/0./
      DATA NUT/0/
      DATA SUT/0./
      DATA DUT/0./
      DATA D/MEOF*0./
      DATA DSHFT/MEOF*0./
      DATA EOF/MAE*0./
      DATA NOPM1/0/
      DATA NEOF/0/
      DATA DELTA/0./
      DATA OPC/MOEM*0./
      DATA EC/MEOF*0./
      DATA PRVDAY/9999/
      DATA PRVUT/9999./
      DATA PRVMLA/9999./
      DATA PRVMLO/9999./
      DATA PRVF10/9999./
      DATA PRVKP/9999./
      DATA PRVUSD/.TRUE./
      DATA PRVUSA/.TRUE./
      DATA PRVID/'99999'/
C
      END
