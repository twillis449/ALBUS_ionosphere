      SUBROUTINE USU(PATH,LUN,DAY,UT,MLT,MLAT,F10P7,KP,BY,SPEC,USEDEC,
     &               USEAP,BYTES,NALT,ALT,DEN)
C
C  PURPOSE
C     A driver for constructing ion density altitude profiles from the
C     parameterized high-latitude USU database using modular USU database
C     access and manipulation routines.
C
C  METHOD
C     From the given input parameters, the USU case identifier and ion species
C     descriptor are determined.  EOFs for the case and ion are read from the
C     appropriate USU EOF file.  Orthogonal polynomial coefficients for the
C     case, ion, and universal time are read from the appropriate USU
C     orthogonal polynomial coefficients file.  The orthogonal polynomial
C     coefficients describe the behavior of Fourier coefficients in magnetic
C     latitude for a given universal time.  Fourier coefficients are determined
C     from the orthogonal polynomial coefficients.  The Fourier coefficients
C     describe the behavior of EOF coefficients in magnetic local time for a
C     given magnetic latitude and universal time.  EOF coefficients are
C     determined from the Fourier coefficients.  The EOF coefficients, in
C     combination with the EOFs, describe the ion density altitude profile
C     for a given magnetic local time, magnetic latitude, and universal time.
C
C  INPUT PARAMETERS
C     BY       The direction of By, '+' or '-'
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     KP       The magnetic Kp index
C     LUN      The logical unit number used to access USU database files
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     PATH     The location of USU database files
C     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
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
C     DEN      The ion density altitude profile, in cm-3
C     NALT     The number of altitude points
C
C  LOCAL VARIABLES
C     None
C
C  SUBROUTINES REQUIRED
C     USUDEN   Generates an ion density altitude profile from EOFs and EOF
C              coefficients
C     USUEC    Generates EOF coefficients from Fourier coefficients
C     USUEOF   Reads an USU EOF file
C     USUFC    Generates Fourier coefficients from orthogonal polynomial
C              coefficients
C     USUHIS   Stores historical quantities for the next call to USU
C     USUID    Determines the case identifier
C     USUION   Determines the ion species descriptor
C     USUOPC   Reads an USU orthogonal polynomial coefficients file
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
C     1.3   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown        26-Feb-1993   1.0 ==> 1.1
C                                   Added user-specified location and logical
C                                   unit number for USU database files.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C     L. Brown        20-Oct-1994   1.2 ==> 1.3
C                                   PARAMETERs MEOF, MFC, and MOPM1 have been
C                                   removed because they are unused.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C
      INTEGER MALT
      PARAMETER(MALT=37)
C
C  Input variables
C
      INTEGER LUN,DAY
      REAL UT,MLT,MLAT,F10P7,KP
      CHARACTER*(*) PATH
      CHARACTER*1 BY
      CHARACTER*3 SPEC
      LOGICAL USEDEC,USEAP,BYTES
C
C  Output variables
C
      INTEGER NALT
      REAL ALT(MALT),DEN(MALT)
C
C  Determine the USU case identifier
C
      CALL USUID(DAY,MLAT,F10P7,KP,BY,USEDEC,USEAP)
C
C  Determine the ion species descriptor
C
      CALL USUION(SPEC)
C
C  Read the USU EOF file
C
      CALL USUEOF(PATH,LUN,BYTES,NALT,ALT)
C
C  Read the USU orthogonal polynomial coefficients file
C
      CALL USUOPC(PATH,LUN,BYTES,UT)
C
C  Generate Fourier coefficients from the orthogonal polynomial coefficients
C
      CALL USUFC(UT,MLAT)
C
C  Generate EOF coefficients from the Fourier coefficients
C
      CALL USUEC(UT,MLT,MLAT)
C
C  Generate an ion density altitude profile from the EOFs and EOF coefficients
C
      CALL USUDEN(UT,MLT,MLAT,NALT,DEN)
C
C  Store historical quantities for the next call to USU
C
      CALL USUHIS(DAY,UT,MLT,MLAT,F10P7,KP,BY,SPEC,USEDEC,USEAP)
C
      RETURN
      END
      SUBROUTINE USUID(XDAY,MLAT,XF10P7,XKP,BY,USEDEC,USEAP)
C
C  PURPOSE
C     To determine a USU case identifier.
C
C  METHOD
C     The USU case identifier is a code for describing the ambient conditions
C     (day of the year, magnetic hemisphere, magnetic activity level, solar
C     activity level, and direction of By) of a USU case.  The case identifier
C     has the form 'aHbcdee', where 'a' describes the magnetic hemisphere, 'H'
C     defines the USU cases as High-latitude, 'b' describes the season, 'c'
C     describes the magnetic activity level, 'd' describes the solar actviity
C     level, and 'ee' describes the Heppner-Maynard convection pattern used.
C     The magnetic latitude determines 'a':  'N' for a Northern hemisphere case
C     and 'S' for a Southern hemisphere case.  The day of the year determines
C     'b':  'W' for Winter, 'S' for Summer, and 'E' for Equinox.  The magnetic
C     Kp index determines 'c':  'L' for Low, 'M' for Moderate, and 'H' for
C     High.  The solar F10.7 index determines 'd':  'L' for Low, 'M' for
C     Moderate, and 'H' for High.  The direction of By determines 'ee':  'BC'
C     for By positive and 'DE' for By negative.
C
C  INPUT PARAMETERS
C     BY       The direction of By, '+' or '-'
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
C     HMPATT   The Heppner-Maynard convection pattern descriptor, 'BC' for By
C              positive, 'DE' for By negative
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
C     1.2   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown        12-Nov-1991   1.0 ==> 1.1
C                                   Complete rewrite to simplify coding and to
C                                   correct a bug in the determination of the
C                                   season.
C                                   The selection of season and magnetic
C                                   activity level have been enhanced as well
C                                   (see the comments below).
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Input variables
C
      INTEGER XDAY
      REAL MLAT,XF10P7,XKP
      CHARACTER*1 BY
      LOGICAL USEDEC,USEAP
C
C  Local variables
C
      REAL SOLDEC,DEC,KPTOAP,XAP
      CHARACTER*1 HEMIS,SEASON,MAGACT,SOLACT
      CHARACTER*2 HMPATT
C
C  If the day of the year, magnetic latitude, 10.7 cm solar flux, magnetic Kp
C  index, direction of By, logical flag USEDEC, or logical flag USEAP are
C  different than those of the previous call, then the case identifier must be
C  redetermined
C
      IF((XDAY .NE. PRVDAY) .OR. (MLAT .NE. PRVMLA) .OR.
     &   (XF10P7 .NE. PRVF10) .OR. (XKP .NE. PRVKP) .OR.
     &   (BY .NE. PRVBY) .OR.
     &   ((USEDEC .OR. PRVUSD) .AND. (.NOT. (USEDEC .AND. PRVUSD))) .OR.
     &   ((USEAP .OR. PRVUSA) .AND. (.NOT. (USEAP .AND. PRVUSA)))) THEN
C
C  Note: If the exclusive-OR (.XOR.) logical operator becomes part of the
C        FORTRAN standard, then the last two continuation lines of the above
C        IF statement can be replaced with the following continuation line:
C
C    &   (USEDEC .XOR. PRVUSD) .OR. (USEAP .XOR. PRVUSA)) THEN
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
C        is it is quasi-linear while Kp is quasi-logarithmic; however, you may
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
C        is it is quasi-linear while Kp is quasi-logarithmic; however, you may
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
C  Determine the Heppner-Maynard convection pattern descriptor
C
         IF(BY .EQ. '+') THEN
            HMPATT='BC'
         ELSE IF(BY .EQ. '-') THEN
            HMPATT='DE'
         ELSE
            STOP 'USUID:Invalid By direction.'
         ENDIF
C
C  Determine the case identifier
C
         ID=HEMIS//'H'//SEASON//MAGACT//SOLACT//HMPATT
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USUION(SPEC)
C
C  PURPOSE
C     To determine the ion species descriptor.
C
C  METHOD
C     The ion species descriptor is a code for describing the ion.  It is
C     is determined by the ion species:  'NO' for NO+, 'O2' for O2+, and
C     'O ' for O+.
C
C  INPUT PARAMETERS
C     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     ISTAT    The status returned from STRTRM and STRUCA routines
C     STEMP    A temporary string variable
C
C  SUBROUTINES REQUIRED
C     STRTRM   Removes (trims) leading blanks from a character string
C     STRUCA   Converts lower-case letters in a character string to upper-case
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
C     1.2   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C     L. Brown        20-Oct-1994   1.1 ==> 1.2
C                                   Call to TRIM has been replaced by call to
C                                   STRTRM.
C                                   Call to UPCASE has been replaced by call to
C                                   STRUCA.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Input variables
C
      CHARACTER*3 SPEC
C
C  Local variables
C
      INTEGER ISTAT
      CHARACTER*3 STEMP
C
C  If the ion species is different than that of the previous call, then the ion
C  species descriptor must be redetermined
C
      IF(SPEC .NE. PRVSPE) THEN
C
C  Determine the ion species descriptor
C
         STEMP=SPEC
         CALL STRTRM(STEMP,32,ISTAT)
         CALL STRUCA(STEMP,ISTAT)
         IF(STEMP .EQ. 'NO+') THEN
            ION='NO'
         ELSE IF(STEMP .EQ. 'O2+') THEN
            ION='O2'
         ELSE IF(STEMP .EQ. 'O+') THEN
            ION='O '
         ELSE
            STOP 'USUION:Invalid ion species.'
         ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USUEOF(PATH,LUN,BYTES,NALT,ALT)
C
C  PURPOSE
C     Reads a USU EOF file.
C
C  METHOD
C     The name of the USU EOF file is determined from the case identifier and
C     the ion species descriptor.  Header information, followed by the altitude
C     grid, eigenvalues, and EOFs are read from the USU EOF file.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     LUN      The logical unit number used to access the USU EOF file
C     PATH     The location of the USU EOF file
C
C  OUTPUT PARAMETERS
C     ALT      The altitude grid, in km
C     NALT     The number of altitude points
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FILE     The name of the USU EOF file
C     FSPEC    The full specification of the USU EOF file
C     IALT     A loop counter for altitudes
C     IEOF     A loop counter for EOFs
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
C     #LUN     The USU EOF file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown        26-Feb-1993   1.1 ==> 1.1
C                                   Added location for USU EOF file.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C     L. Brown        20-Oct-1994   1.2 ==> 1.3
C                                   A call to STRLCA insures that the full file
C                                   specification of the USU EOF file is in
C                                   lower-case letters.
C                                   Calls to CONCAT have been replaced by calls
C                                   to STRCCT.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
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
      CHARACTER*12 FILE
      CHARACTER*160 FSPEC
C
C  If the case identifier or ion species descriptor are different than those of
C  the previous call, then the USU EOF file must be read
C
      IF((ID .NE. PRVID) .OR. (ION .NE. PRVION)) THEN
C
C  Determine the name of the USU EOF file
C
         CALL STRCCT(ID//'.'//ION,'E',32,FILE,ISTAT)
C
C  Determine the full specification of the USU EOF file
C
         CALL STRCCT(PATH,FILE,32,FSPEC,ISTAT)
         CALL STRLCA(FSPEC,ISTAT)
C
C  Open the USU EOF file.  Note that in the VAX Fortran implementation, the
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
C  Read header information from the USU EOF file
C
         READ(LUN,REC=1) DUM1,DUM2
         YEAR=INT(DUM1)
         DAY=INT(DUM2)
         READ(LUN,REC=2) F10P7,AP,KP
C
C  Read the altitude grid from the USU EOF file
C
         READ(LUN,REC=3) DUM1
         NALT=INT(DUM1)
         READ(LUN,REC=4) (ALT(IALT),IALT=1,NALT)
C
C  Read the eigenvalues from the USU EOF file
C
         READ(LUN,REC=5) (D(IEOF),IEOF=1,NALT)
         READ(LUN,REC=6) (DSHFT(IEOF),IEOF=1,NALT)
C
C  Read the EOFs from the USU EOF file
C
         DO 10 IEOF=1,NALT
            READ(LUN,REC=6+IEOF) (EOF(IALT,IEOF),IALT=1,NALT)
   10    CONTINUE
C
C  Close the USU EOF file
C
         CLOSE(UNIT=LUN)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USUOPC(PATH,LUN,BYTES,UT)
C
C  PURPOSE
C     Reads a USU orthogonal polynomial coefficients file.
C
C  METHOD
C     The name of the USU orthogonal polynomial coefficients file is determined
C     from the USU case identifier and the ion species descriptor.  Header
C     information is read from the orthogonal polynomial coefficients file.
C     The orthogonal polynomial coefficients for the given universal time are
C     read from the orthogonal polynomial coefficients file.  If the given
C     universal time does not lie exactly on the universal time grid, then
C     the orthogonal polynomial coefficients are interpolated from the
C     orthogonal polynomial coefficients of the two nearest universal times
C     on the universal time grid.
C
C  INPUT PARAMETERS
C     BYTES    A logical flag, .TRUE. if record-lengths of direct-access files
C              are specified in bytes in OPEN statements, .FALSE. if they are
C              specified in words (4-byte units)
C     LUN      The logical unit number used to access the USU orthogonal
C              polynomial coefficients file
C     PATH     The location of the USU orthogonal polynomial coefficients file
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FILE     The name of the USU orthogonal polynomial coefficients file
C     FSPEC    The full specification of the USU orthogonal polynomial
C              coefficients file
C     IEOF     The loop counter for EOFs
C     IFC      The loop counter for Fourier coefficients
C     IOPC     The loop counter for orthogonal polynomial coefficients
C     IREC     A record number in the orthogonal polynomial coefficients file
C     IREC1    A record number in the orthogonal polynomial coefficients file
C     IREC2    A record number in the orthogonal polynomial coefficients file
C     ISTAT    The status returned from the STRCCT and STRLCA routines
C     IUT      The universal time index
C     OPC1     Orthogonal polynomial coefficients used for interpolation
C     OPC2     Orthogonal polynomial coefficients used for interpolation
C     T        An interpolation factor
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
C     #LUN     The USU orthogonal polynomial coefficients file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown        26-Feb-1993   1.0 ==> 1.1
C                                   Added location for USU orthogonal
C                                   polynomial coefficients file.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C     L. Brown        20-Oct-1994   1.2 ==> 1.3
C                                   A call to STRLCA insures that the full file
C                                   specification of the USU orthogonal
C                                   polynomial coefficients file is in
C                                   lower-case letters.
C                                   Calls to CONCAT have been replaced by calls
C                                   to STRCCT.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
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
      INTEGER ISTAT,IUT,IREC,IREC1,IREC2,IEOF,IOPC,IFC
      REAL UTR,T,DUM1,DUM2
      REAL OPC1(MFC),OPC2(MFC)
      CHARACTER*13 FILE
      CHARACTER*160 FSPEC
C
C  If the case identifier, ion species descriptor, or universal time are
C  different than those of the previous call, then the LME OPC file must be
C  read
C
      IF((ID .NE. PRVID) .OR. (ION .NE. PRVION) .OR. (UT .NE. PRVUT))
     &   THEN
C
C  Determine the name of the USU orthogonal polynomial coefficients file
C
         CALL STRCCT(ID//'.'//ION,'P',32,FILE,ISTAT)
C
C  Determine the full specification of the USU orthogonal polynomial
C  coefficients file
C
         CALL STRCCT(PATH,FILE,32,FSPEC,ISTAT)
         CALL STRLCA(FSPEC,ISTAT)
C
C  Open the USU orthogonal polynomial coefficients file.  Note that in the VAX
C  Fortran implementation, the record-length of unformatted direct-access
C  files (specified by the RECL keyword) is given in longwords (4 byte units),
C  corresponding to the space required for an integer*4 or a real*4.  In other
C  implementations, the record-length might be given in bytes.
C
         IF(BYTES) THEN
            OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     &           ACCESS='DIRECT',RECL=MFC*4)
         ELSE
            OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     &           ACCESS='DIRECT',RECL=MFC)
         ENDIF
C
C  Read header information from the USU orthogonal polynomial coefficients file
C
         READ(LUN,REC=1) DUM1,DUM2,DELTA
         YEAR=INT(DUM1)
         DAY=INT(DUM2)
         READ(LUN,REC=2) F10P7,AP,DUM1,DUM2
         NFC=INT(DUM1)
         NOPM1=INT(DUM2)
         READ(LUN,REC=3) KP,DUM2
         NEOF=INT(DUM2)
         READ(LUN,REC=4) DUM1,SUT,DUT
         NUT=INT(DUM1)
         READ(LUN,REC=5) DUM1,SMLAT,DMLAT
         NMLAT=INT(DUM1)
         SMLAT=ABS(SMLAT)
         DMLAT=ABS(DMLAT)
         READ(LUN,REC=6) DUM1,SMLT,DMLT
         NMLT=INT(DUM1)
C
C  Restrict the universal time to the range [>=0,<24]
C
         UTR=MOD(24.+MOD(UT,24.),24.)
C
C  Calculate the universal time index
C
         IUT=1+INT((UTR-SUT)/DUT)
C
C  If the universal time is exactly on the universal time grid, then the
C  orthogonal polynomial coefficients can be read directly from the USU
C  orthogonal polynomial coefficients file without interpolation
C
         IF(UTR .EQ. SUT+DUT*FLOAT(IUT-1)) THEN
C
C  Determine the starting record number in the USU orthogonal polynomial
C  coefficients file
C
            IREC=NEOF*(NOPM1+1)*(IUT-1)+7
C
C  Read the orthogonal polynomial coefficients for the given universal time
C  from the USU orthogonal polynomial coefficients file
C
            DO 20 IEOF=1,NEOF
               DO 10 IOPC=1,NOPM1+1
                  READ(LUN,REC=IREC) (OPC(IOPC,IFC,IEOF),IFC=1,NFC)
                  IREC=IREC+1
   10          CONTINUE
   20       CONTINUE
C
C  Otherwise, the orthogonal polynomial coefficients are linearly interpolated
C  from the orthogonal polynomial coefficients of the two nearest universal
C  times
C
         ELSE
C
C  Determine the starting record numbers in the USU orthogonal polynomial
C  coefficients file
C
            IF((UTR .LT. SUT) .OR. (IUT .EQ. NUT)) THEN
               IREC1=NEOF*(NOPM1+1)*(NUT-1)+7
               IREC2=7
            ELSE
               IREC1=NEOF*(NOPM1+1)*(IUT-1)+7
               IREC2=NEOF*(NOPM1+1)*IUT+7
            ENDIF
C
C  Determine the interpolation factor
C
            IF(UTR .LT. SUT) THEN
               T=(UTR-(SUT-DUT))/DUT
            ELSE IF(IUT .EQ. NUT) THEN
               T=(UTR-(SUT+DUT*FLOAT(NUT-1)))/DUT
            ELSE
               T=(UTR-(SUT+DUT*FLOAT(IUT-1)))/DUT
            ENDIF
C
C  Read the orthogonal polynomial coefficients for the two nearest universal
C  times from the USU orthogonal polynomial coefficients file and linearly
C  interpolate the orthogonal polynomial coefficients at the given universal time
C
            DO 120 IEOF=1,NEOF
               DO 110 IOPC=1,NOPM1+1
                  READ(LUN,REC=IREC1) (OPC1(IFC),IFC=1,NFC)
                  IREC1=IREC1+1
                  READ(LUN,REC=IREC2) (OPC2(IFC),IFC=1,NFC)
                  IREC2=IREC2+1
                  DO 100 IFC=1,NFC
                     OPC(IOPC,IFC,IEOF)=(1.-T)*OPC1(IFC)+T*OPC2(IFC)
  100             CONTINUE
  110          CONTINUE
  120       CONTINUE
C
         ENDIF
C
C  Close the USU orthogonal polynomial coefficients file
C
         CLOSE(UNIT=LUN)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USUFC(UT,MLAT)
C
C  PURPOSE
C     To generate Fourier coefficients from orthogonal polynomial coefficients.
C
C  METHOD
C     A set of orthogonal polynomials is determined.  The orthogonal
C     polynomials are combined with the orthogonal polynomial coefficients to
C     produce Fourier coefficients for the given magnetic latitude.  If the
C     magnetic latitude does not lie exactly on the magnetic latitude grid,
C     then the Fourier coefficients are interpolated from Fourier coefficients
C     at the two nearest magnetic latitudes.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     IEOF     A loop counter for EOFs
C     IFC      A loop counter for Fourier coefficients
C     IMLAT    The magnetic latitude index
C     IMLAT1   A magnetic latitude index used for interpolation
C     IMLAT2   A magnetic latitude index used for interpolation
C     IOP      A loop counter for orthogonal polynomials
C     OP       Orthogonal polynomials
C     X        The grid for the orthogonal polynomials
C     T        An interpolation factor
C
C  SUBROUTINES REQUIRED
C     USUOP    Generates the orthogonal polynomials
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
C     1.2   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown        29-May-1991   1.0 ==> 1.1
C                                   Calculation of magnetic latitude index
C                                   IMLAT corrected to insure that IMLAT does
C                                   not exceed NMLAT.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C
C  REFERENCES
C     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
C        The Golam Press, Boulder, CO, 1973, pp 90-92.
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     MX       The maximum size of the orthogonal polynomial grid
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
      INTEGER MX
      PARAMETER(MX=20)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Input variables
C
      REAL UT,MLAT
C
C  Local variables
C
      INTEGER IMLAT,IMLAT1,IMLAT2,IEOF,IFC,IOP
      REAL T
      REAL OP(MX,MOPM1+1),X(MX)
C
C  If the case identifier, ion species descriptor, universal time, or magnetic
C  latitude are different than those of the previous call, then the Fourier
C  coefficients must be regenerated
C
      IF((ID .NE. PRVID) .OR. (ION .NE. PRVION) .OR.
     &   (UT .NE. PRVUT) .OR. (MLAT .NE. PRVMLA)) THEN
C
C  Calculate the grid for the orthogonal polynomials
C
         DO 10 IOP=1,NMLAT
            X(IOP)=DELTA*FLOAT(IOP-1)-1.
   10    CONTINUE
C
C  Generate the orthogonal polynomials
C
         CALL USUOP(NOPM1,NMLAT,X,OP)
C
C  Calculate the magnetic latitude index
C
         IMLAT=MIN(NMLAT,MAX(1,1+INT((ABS(MLAT)-SMLAT)/DMLAT)))
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then the
C  Fourier coefficients can be calculated directly from the orthogonal
C  polynomials and orthogonal polynomial coefficients without interpolation
C
         IF(ABS(MLAT) .EQ. SMLAT+DMLAT*FLOAT(IMLAT-1)) THEN
C
C  Loop over the number of EOFs used to construct an ion density altitude
C  profile
C
            DO 300 IEOF=1,NEOF
C
C  Loop over the number of Fourier coefficients
C
               DO 200 IFC=1,NFC
C
C  Initialize the Fourier coefficient
C
                  FC(IFC,IEOF)=0.
C
C  Loop over the orthogonal polynomial coefficients
C
                  DO 100 IOP=1,NOPM1+1
C
C  Add the contribution to the Fourier coefficient by the orthogonal polynomial
C
                     FC(IFC,IEOF)=FC(IFC,IEOF)
     &                           +OPC(IOP,IFC,IEOF)*OP(IMLAT,IOP)
C
C  End the loop over orthogonal polynomial coefficients
C
  100             CONTINUE
C
C  End the loop over Fourier coefficients
C
  200          CONTINUE
C
C  End the loop over the number of EOFs used to construct an ion density
C  altitude profile
C
  300       CONTINUE
C
C  Otherwise, the Fourier coefficients at the given magnetic latitude are
C  linearly interpolated from the Fourier coefficients of the two nearest
C  magnetic latitudes
C
         ELSE
C
C  Determine the indices of the two nearest magnetic latitudes
C
            IF(IMLAT .GE. NMLAT) THEN
               IMLAT1=NMLAT-1
               IMLAT2=NMLAT
            ELSE
               IMLAT1=IMLAT
               IMLAT2=IMLAT+1
            ENDIF
C
C  Determine the interpolation factor
C
            T=(ABS(MLAT)-(SMLAT+DMLAT*FLOAT(IMLAT1-1)))/DMLAT
C
C  Loop over the number of EOFs used to construct an ion density altitude
C  profile
C
            DO 600 IEOF=1,NEOF
C
C  Loop over the number of Fourier coefficients
C
               DO 500 IFC=1,NFC
C
C  Initialize the Fourier coefficient
C
                  FC(IFC,IEOF)=0.
C
C  Loop over the orthogonal polynomial coefficients
C
                  DO 400 IOP=1,NOPM1+1
C
C  Add the contribution to the Fourier coefficient by the orthogonal
C  polynomials at the two nearest magnetic latitudes
C
                     FC(IFC,IEOF)=FC(IFC,IEOF)
     &                           +OPC(IOP,IFC,IEOF)
     &                            *((1.-T)*OP(IMLAT1,IOP)
     &                             +T*OP(IMLAT2,IOP))
C
C  End the loop over orthogonal polynomial coefficients
C
  400             CONTINUE
C
C  End the loop over Fourier coefficients
C
  500          CONTINUE
C
C  End the loop over the number of EOFs used to construct an ion density
C  altitude profile
C
  600       CONTINUE
C
         ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USUOP(NOPM1,NX,X,U)
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
C     1.1   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Loop counter variables renamed to be more
C                                   indicative of their function.
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
      PARAMETER(MOPM1=8,MX=20)
C
C  Input variables
C
      INTEGER NOPM1,NX
      REAL X(MX)
C
C  Output variables
C
      REAL U(MX,MOPM1+1)
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
      SUBROUTINE USUEC(UT,MLT,MLAT)
C
C  PURPOSE
C     To generate EOF coefficients from Fourier coefficients.
C
C  METHOD
C     The Fourier coefficients are combined with sines and cosines of magnetic
C     local time to produce EOF coefficients at the given magnetic local time.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     None
C
C  LOCAL VARIABLES
C     COSS     Cosine terms for the Fourier expansion of the EOF coefficients
C     IEOF     A loop counter for EOFs
C     IFC      A loop counter for Fourier coefficients
C     PI       Pi
C     SINS     Sine terms for the Fourier expansion of the EOF coefficients
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
C     1.1   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Pi is now calculated using the formula
C                                   4*ArcTan(1) instead of being stored as
C                                   a constant.
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Input variables
C
      REAL UT,MLT,MLAT
C
C  Local variables
C
      INTEGER IFC,IEOF
      REAL PI
      REAL SINS((MFC-1)/2),COSS((MFC-1)/2)
C
C  If the case identifier, ion species descriptor, universal time, magnetic
C  local time, or magnetic latitude are different than those of the previous
C  call, then the EOF coefficients must be regenerated
C
      IF((ID .NE. PRVID) .OR. (ION .NE. PRVION) .OR.
     &   (UT .NE. PRVUT) .OR. (MLT .NE. PRVMLT) .OR.
     &   (MLAT .NE. PRVMLA)) THEN
C
C  Calculate pi
C
         PI=4.*ATAN(1.)
C
C  Calculate the sines and cosines for the Fourier expansion of the EOF
C  coefficients
C
         DO 10 IFC=1,(NFC-1)/2
            SINS(IFC)=SIN(FLOAT(IFC)*2.*PI*MLT/24.)
            COSS(IFC)=COS(FLOAT(IFC)*2.*PI*MLT/24.)
   10    CONTINUE
C
C  Loop over the number of EOFs used to construct an ion density altitude
C  profile
C
         DO 200 IEOF=1,NEOF
C
C  Initialize the EOF coefficient with the DC term of the Fourier series
C
            EC(IEOF)=FC(1,IEOF)/2.
C
C  Add the Fourier sine and cosine terms to the EOF coefficient
C
            DO 100 IFC=1,(NFC-1)/2
               EC(IEOF)=EC(IEOF)
     &                 +FC(IFC+1+(NFC-1)/2,IEOF)*SINS(IFC)
     &                 +FC(IFC+1,IEOF)*COSS(IFC)
  100       CONTINUE
C
C  End the loop over the number of EOFs used to construct an ion density
C  altitude profile
C
  200    CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE USUDEN(UT,MLT,MLAT,NALT,DEN)
C
C  PURPOSE
C     To construct an ion density altitude profile from EOFs and EOF
C     coefficients.
C
C  METHOD
C     The EOFs are combined with the EOF coefficients to produce the natural
C     logarithm of an ion density altitude profile.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     NALT     The number of altitude points
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     DEN      The ion density altitude profile, in cm-3
C
C  LOCAL VARIABLES
C     IALT     A loop counter for altitudes
C     IEOF     A loop counter for EOFs
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
C     1.1   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0 ==> Created
C     L. Brown         3-Mar-1993   1.0 ==> 1.1
C                                   Added common block CBUSU for internal use
C                                   by the USU modules for sharing information
C                                   instead of passing it through argument
C                                   lists, greatly simplifying and clarifying
C                                   their usage.  Common storage also allows
C                                   computational savings by using results from
C                                   a previous call to USU, as is appropriate.
C                                   Common block CBUSU is initialized in block
C                                   data BDUSU.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Input variables
C
      INTEGER NALT
      REAL UT,MLT,MLAT
C
C  Output variables
C
      REAL DEN(MALT)
C
C  Local variables
C
      INTEGER IALT,IEOF
C
C  If the case identifier, ion species descriptor, universal time, magnetic
C  local time, or magnetic latitude are different than those of the previous
C  call, then the ion density altitude profile must be reconstructed
C
      IF((ID .NE. PRVID) .OR. (ION .NE. PRVION) .OR.
     &   (UT .NE. PRVUT) .OR. (MLT .NE. PRVMLT) .OR.
     &   (MLAT .NE. PRVMLA)) THEN
C
C  Loop over altitude
C
         DO 100 IALT=1,NALT
C
C  Initialize the natural logarithm of the ion density at a given altitude
C
            DEN(IALT)=0.
C
C  Sum the EOF terms for the natural logarithm of the ion density
C
            DO 10 IEOF=1,NEOF
               DEN(IALT)=DEN(IALT)+EC(IEOF)*EOF(IALT,IEOF)
   10       CONTINUE
C
C  Convert the result to an actual ion density by exponentiation
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
      SUBROUTINE USUHIS(XDAY,UT,MLT,MLAT,XF10P7,XKP,BY,SPEC,USEDEC,
     &                  USEAP)
C
C  PURPOSE
C     To store historical quantities for the next call to USU.
C
C  METHOD
C     The day of the year, universal time, magnetic local time,
C     magnetic latitude, 10.7 cm solar flux, magnetic Kp index,
C     direction of By, ion species, logical flag USEDEC, logical flag
C     USEAP, case identifier, and ion species descriptor are stored for
C     the next call to USU.
C
C  INPUT PARAMETERS
C     MLAT     The magnetic latitude, in degrees north
C     MLT      The magnetic local time, in decimal hours
C     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
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
C     1.0   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         3-Mar-1993   1.0 ==> Created
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Input variables
C
      INTEGER XDAY
      REAL UT,MLT,MLAT,XF10P7,XKP
      CHARACTER*1 BY
      CHARACTER*3 SPEC
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
C  Store the magnetic local time
C
      PRVMLT=MLT
C
C  Store the magnetic latitude
C
      PRVMLA=MLAT
C
C  Store the 10.7 cm solar flux
C
      PRVF10=XF10P7
C
C  Store the magnetic Kp index
C
      PRVKP=XKP
C
C  Store the direction of By
C
      PRVBY=BY
C
C  Store the ion species
C
      PRVSPE=SPEC
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
C  Store the ion species descriptor
C
      PRVION=ION
C
      RETURN
      END
      BLOCK DATA BDUSU
C
C  PURPOSE
C     To initialize the contents of common block CBUSU.
C
C  METHOD
C     Common block CBUSU is intended for internal use by the USU modules.
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
C     1.0   3-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown         3-Mar-1993   1.0 ==> Created
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     MAE      MALT*MEOF
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MFC      The maximum number of Fourier coefficients
C     MFE      MFC*MEOF
C     MOFE     (MOPM1+1)*MFC*MEOF
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MFC,MOPM1
      PARAMETER(MALT=37,MEOF=37,MFC=9,MOPM1=8)
      INTEGER MAE,MOFE,MFE
      PARAMETER(MAE=MALT*MEOF,MOFE=(MOPM1+1)*MFC*MEOF,MFE=MFC*MEOF)
C
C  Common block CBUSU
C     AP       The magnetic Ap index
C     D        Eigenvalues
C     DAY      The day of the year
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux, in solar flux units
C     FC       Fourier coefficients
C     ID       The case identifier
C     ION      The ion species descriptor, 'NO' for NO+, 'O2' for O2+, and
C              'O ' for O+
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NFC      The number of Fourier coefficients
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     PRVBY    The direction of By from the previous call
C     PRVDAY   The day of the year from the previous call
C     PRVF10   The 10.7 cm solar flux from the previous call, in solar flux
C              units
C     PRVID    The case identifier from the previous call
C     PRVION   The ion species descriptor from the previous call, 'NO' for NO+,
C              'O2' for O2+, and 'O ' for O+
C     PRVKP    The magnetic Kp index from the previous call
C     PRVMLA   The magnetic latitude from the previous call, in degrees north
C     PRVMLT   The magnetic local time from the previous call, in decimal hours
C     PRVSPE   The ion species from the previous call
C     PRVUSA   The logical flag USEAP from the previous call
C     PRVUSD   The logical flag USEDEC from the previous call
C     PRVUT    The universal time from the previous call, in decimal hours
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     SUT      The starting value of the universal time grid, in decimal hours
C     YEAR     The year
C
      INTEGER YEAR,DAY,NMLT,NMLAT,NUT,NFC,NOPM1,NEOF,PRVDAY
      REAL F10P7,AP,KP,SMLT,DMLT,SMLAT,DMLAT,SUT,DUT,DELTA,PRVUT,
     &     PRVMLT,PRVMLA,PRVF10,PRVKP
      REAL D(MEOF),DSHFT(MEOF),EOF(MALT,MEOF),OPC(MOPM1+1,MFC,MEOF),
     &     FC(MFC,MEOF),EC(MEOF)
      CHARACTER*1 PRVBY
      CHARACTER*2 ION,PRVION
      CHARACTER*3 PRVSPE
      CHARACTER*7 ID,PRVID
      LOGICAL PRVUSD,PRVUSA
      COMMON/CBUSU/ID,ION,YEAR,DAY,F10P7,AP,KP,NMLT,SMLT,DMLT,NMLAT,
     &             SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,EOF,NFC,NOPM1,NEOF,
     &             DELTA,OPC,FC,EC,PRVDAY,PRVUT,PRVMLT,PRVMLA,PRVF10,
     &             PRVKP,PRVBY,PRVSPE,PRVUSD,PRVUSA,PRVID,PRVION
C
C  Initialize common block CBUSU
C
      DATA ID/'       '/
      DATA ION/'  '/
      DATA YEAR/0/
      DATA DAY/0/
      DATA F10P7/0./
      DATA AP/0./
      DATA KP/0./
      DATA NMLT/0/
      DATA SMLT/0./
      DATA DMLT/0./
      DATA NMLAT/0/
      DATA SMLAT/0./
      DATA DMLAT/0./
      DATA NUT/0/
      DATA SUT/0./
      DATA DUT/0./
      DATA D/MEOF*0./
      DATA DSHFT/MEOF*0./
      DATA EOF/MAE*0./
      DATA NFC/0/
      DATA NOPM1/0/
      DATA NEOF/0/
      DATA DELTA/0./
      DATA OPC/MOFE*0./
      DATA FC/MFE*0./
      DATA EC/MEOF*0./
      DATA PRVDAY/9999/
      DATA PRVUT/9999./
      DATA PRVMLT/9999./
      DATA PRVMLA/9999./
      DATA PRVF10/9999./
      DATA PRVKP/9999./
      DATA PRVBY/'9'/
      DATA PRVSPE/'999'/
      DATA PRVUSD/.TRUE./
      DATA PRVUSA/.TRUE./
      DATA PRVID/'9999999'/
      DATA PRVION/'99'/
C
      END
