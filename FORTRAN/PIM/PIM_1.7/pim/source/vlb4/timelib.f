C******************************************************************************
C  TIMELIB is a FORTRAN library of time manipulations.  It contains the
C  following routines:
C
C    *FUNCTION   TIMCDM (YEAR,MONTH)
C                       Determines the cumulative number of days at the end of
C                       a month
C     FUNCTION   TIMLPY (YEAR)
C                       Determines if a year is a leap year
C    *SUBROUTINE TIMMDM (YEAR,DOY,MONTH,DAY)
C                       Determines the month and day of the month
C
C  Routines marked by an asterisk (*) use other routines in the library.
C******************************************************************************
      FUNCTION TIMCDM(YEAR,MONTH)
C
C  PURPOSE
C     To determine the cumulative number of days at the end of a month.
C
C  METHOD
C     The cumulative number of days at the end of a given month is determined
C     from the year and the month, with leap years taken into consideration.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     MONTH  Integer    Scalar        n/a             1 <= MONTH <= 12
C        The month of the year
C     YEAR   Integer    Scalar        n/a             > 0
C        The year
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     TIMCDM Integer    Scalar        n/a             31 <= TIMCDM <= 366
C        The cumulative number of days at the end of the month
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     TIMLPY Logical    Determines if a year is a leap year
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       29-Mar-1994  1.0 ==> Created
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     NCDAUG Integer    Days            The cumulative number of days at the
C                                       end of August for a non-leap year
C     NCDAPR Integer    Days            The cumulative number of days at the
C                                       end of April for a non-leap year
C     NCDDEC Integer    Days            The cumulative number of days at the
C                                       end of December for a non-leap year
C     NCDFEB Integer    Days            The cumulative number of days at the
C                                       end of February for a non-leap year
C     NCDJAN Integer    Days            The cumulative number of days at the
C                                       end of January
C     NCDJUN Integer    Days            The cumulative number of days at the
C                                       end of June for a non-leap year
C     NCDJUL Integer    Days            The cumulative number of days at the
C                                       end of July for a non-leap year
C     NCDMAR Integer    Days            The cumulative number of days at the
C                                       end of March for a non-leap year
C     NCDMAY Integer    Days            The cumulative number of days at the
C                                       end of May for a non-leap year
C     NCDNOV Integer    Days            The cumulative number of days at the
C                                       end of November for a non-leap year
C     NCDOCT Integer    Days            The cumulative number of days at the
C                                       end of October for a non-leap year
C     NCDSEP Integer    Days            The cumulative number of days at the
C                                       end of September for a non-leap year
C
      INCLUDE 'logicuni.inc'
      INTEGER NCDJAN,NCDFEB,NCDMAR,NCDAPR,NCDMAY,NCDJUN,NCDJUL,NCDAUG,
     &        NCDSEP,NCDOCT,NCDNOV,NCDDEC
      PARAMETER(NCDJAN=31,NCDFEB=59,NCDMAR=90,NCDAPR=120,NCDMAY=151,
     &          NCDJUN=181,NCDJUL=212,NCDAUG=243,NCDSEP=273,NCDOCT=304,
     &          NCDNOV=334,NCDDEC=365)
C
C  Input variables
C
      INTEGER YEAR,MONTH
C
C  Output variables
C
      INTEGER TIMCDM
C
C  Local variables
C
      LOGICAL TIMLPY
C
C  Check the validity of input parameters
C
      IF(YEAR .LE. 0) THEN
         WRITE(LUSTDERR,'(1X,A,I4,A)') 'TIMCDM:Year (',YEAR,
     &                          ') must be > 0 ...'
         STOP 'Program terminated with error in TIMCDM.'
      ENDIF
      IF((MONTH .LT. 1) .OR. (MONTH .GT. 12)) THEN
         WRITE(LUSTDERR,'(1X,A,I2,A)') 'TIMCDM:Month (',MONTH,
     &                          ') must be >= 1, <= 12 ...'
         STOP 'Program terminated with error in TIMCDM.'
      ENDIF
C
C  Determine the cumulative number of days at the end of the month
C
      IF(MONTH .EQ. 1) THEN
         TIMCDM=NCDJAN
      ELSE
         IF(MONTH .EQ. 2) THEN
            TIMCDM=NCDFEB
         ELSE IF(MONTH .EQ. 3) THEN
            TIMCDM=NCDMAR
         ELSE IF(MONTH .EQ. 4) THEN
            TIMCDM=NCDAPR
         ELSE IF(MONTH .EQ. 5) THEN
            TIMCDM=NCDMAY
         ELSE IF(MONTH .EQ. 6) THEN
            TIMCDM=NCDJUN
         ELSE IF(MONTH .EQ. 7) THEN
            TIMCDM=NCDJUL
         ELSE IF(MONTH .EQ. 8) THEN
            TIMCDM=NCDAUG
         ELSE IF(MONTH .EQ. 9) THEN
            TIMCDM=NCDSEP
         ELSE IF(MONTH .EQ. 10) THEN
            TIMCDM=NCDOCT
         ELSE IF(MONTH .EQ. 11) THEN
            TIMCDM=NCDNOV
         ELSE
            TIMCDM=NCDDEC
         ENDIF
         IF(TIMLPY(YEAR)) TIMCDM=TIMCDM+1
      ENDIF
C
      RETURN
      END
      FUNCTION TIMLPY(YEAR)
C
C  PURPOSE
C     To determine if a year is a leap year.
C
C  METHOD
C     If a year is evenly divisible by 4, then it is a leap year, except if
C     the year is evenly divisible by 100 and not evenly divisible by 400.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     YEAR   Integer    Scalar        n/a             > 0
C        The year
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     TIMLPY Logical    Scalar        n/a             .TRUE. or .FALSE.
C        A flag, .TRUE. if the input year is a leap year, .FALSE. if the input
C        year is not a leap year
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.2   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       25-Mar-1993  1.0 ==> Created
C     L. Brown       14-Apr-1993  1.0 ==> 1.1
C                                 Added error checking for input year.
C                                 Internal documentation improved.
C     L. Brown       29-Mar-1994  1.1 ==> 1.2
C                                 Brought internal documenation up to specs.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER YEAR
C
C  Output variables
C
      LOGICAL TIMLPY
C
C  Check the validity of the year
C
      INCLUDE 'logicuni.inc'
      IF(YEAR .LE. 0) THEN
         WRITE(LUSTDERR,'(1X,A,I4,A)') 'TIMLPY:Year (',YEAR,
     &                          ') must be > 0 ...'
         STOP 'Program terminated with error in TIMLPY.'
      ENDIF
C
C  Determine if the year is a leap year
C
      IF(MOD(YEAR,4) .EQ. 0) THEN
         IF(MOD(YEAR,100) .EQ. 0) THEN
            IF(MOD(YEAR,400) .EQ. 0) THEN
               TIMLPY=.TRUE.
            ELSE
               TIMLPY=.FALSE.
            ENDIF
         ELSE
            TIMLPY=.TRUE.
         ENDIF
      ELSE
         TIMLPY=.FALSE.
      ENDIF
C
      RETURN
      END
      SUBROUTINE TIMMDM(YEAR,DOY,MONTH,DAY)
C
C  PURPOSE
C     To determine the month and day of the month.
C
C  METHOD
C     The month and day of the month are determined from the year and day of
C     the year, with leap years taken into consideration.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DOY    Integer    Scalar        n/a             1 <= DOY <= 366
C        The day of the year
C     YEAR   Integer    Scalar        n/a             > 0
C        The year
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DAY    Integer    Scalar        n/a             Varies
C        The day of the month
C     MONTH  Integer    Scalar        n/a             1 <= MONTH <= 12
C        The month of the year
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     None
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     TIMCDM Integer    Determines the cumulative number of days at the end of
C                       a month
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.2   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       25-Mar-1993  1.0 ==> Created
C     L. Brown       14-Apr-1993  1.0 ==> 1.1
C                                 Added error checking for input parameters.
C                                 Uses cumulative days in each month instead of
C                                 number of days in each month.
C                                 Internal documentation improved.
C     L. Brown       29-Mar-1994  1.1 ==> 1.2
C                                 The cumulative number of days at the end of a
C                                 month is now determined by function TIMCDM.
C                                 Brought internal documenation up to specs.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER YEAR,DOY
C
C  Output variables
C
      INTEGER MONTH,DAY
C
C  Local variables
C
      INTEGER TIMCDM
      INCLUDE 'logicuni.inc'
C
C  Check the validity of input parameters
C
      IF(YEAR .LE. 0) THEN
         WRITE(LUSTDERR,'(1X,A,I4,A)') 'TIMMDM:Year (',YEAR,
     &                          ') must be > 0 ...'
         STOP 'Program terminated with error in TIMMDM.'
      ENDIF
      IF((DOY .LT. 1) .OR. (DOY .GT. TIMCDM(YEAR,12))) THEN
         WRITE(LUSTDERR,'(1X,A,I3,A,I3,A)') 'TIMMDM:Day of year (',DOY,
     &                               ') must be >= 1, <= ',
     &                               TIMCDM(YEAR,12),' ...'
         STOP 'Program terminated with error in TIMMDM.'
      ENDIF
C
C  Determine the month
C
      MONTH=1
   10 IF(DOY .GT. TIMCDM(YEAR,MONTH)) THEN
         MONTH=MONTH+1
         GOTO 10
      ENDIF
C
C  Determine the day of the month
C
      DAY=DOY
      IF(MONTH .GT. 1) DAY=DAY-TIMCDM(YEAR,MONTH-1)
C
      RETURN
      END
