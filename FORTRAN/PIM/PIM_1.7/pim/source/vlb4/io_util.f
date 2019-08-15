      SUBROUTINE UDETID(DAY,UT,MLAT,F10P7,KP,BY,ID)
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
C  NAME  TYPE   ARRAY  Description
C  BY    REAL          The direction of By, '+' or '-'
C  DAY   INTEGER       The day of the year
C  F10P7 REAL          The 10.7 cm solar flux
C  KP    REAL          The magnetic Kp index
C  MLAT  REAL          The magnetic latitude, in degrees north
C  UT    REAL          Universal Time, in decimal hours
C
C  OUTPUT PARAMETERS
C  NAME  TYPE   ARRAY  Description
C  ID    CHARACTER     The case identifier
C
C  LOCAL VARIABLES
C  NAME   TYPE  ARRAY  Description
C  AP     REAL         The magnetic Ap index
C  DEC    REAL         The solar declination, in degrees north
C  HEMIS  CHARACTER    The hemisphere descriptor, 'N' for a northern
C                      hemisphere case,'S' for a southern hemisphere case
C  HMPATT CHARACTER    The Heppner-Maynard convection pattern descriptor,
C                      'BC' for By positive, 'DE' for By negative
C  MAGACT CHARACTER    The magnetic activity level descriptor, 'L' for low,
C                      'M' for moderate, and 'H' for high
C  SEASON CHARACTER    The season descriptor, 'W' for winter, 'S' for summer,
C                      and 'E' for equinox
C  SOLACT CHARACTER    The solar activity level descriptor, 'L' for low, 'M'
C                      for moderate, and 'H' for high
C  USEAP  LOGICAL      A logical flag, .TRUE. if Ap is to be used to determine
C                      the magnetic activity level descriptor, .FALSE. if Kp
C                      is to be used
C  USEDEC LOGICAL      A logical flag, .TRUE. if the solar declination is to
C                      be used to determine the season descriptor, .FALSE. if
C                      the day of the year is to be used
C
C  SUBROUTINES REQUIRED
C     KPTOAP   Converts Kp to Ap
C     SOLDEC   Determines the solar declination
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   30-September-1996
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        23-Jan-1991   1.0
C                                   Created
C     L. Brown        12-Nov-1991   1.0 --> 1.1
C                                   Complete rewrite to simplify coding and to
C                                   correct a bug in the determination of the
C                                   season.
C                                   The selection of season and magnetic
C                                   activity level have been enhanced as well
C                                   (see the comments below).
C     L. Brown        30-Sep-1996   1.0.6 ==> 1.5
C                                   Added input argument UT.
C                                   Argument DAY is no longer converted to a
C                                   real number in call to SOLDEC.
C                                   Added argument UT to call to SOLDEC.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER DAY
      REAL UT,MLAT,F10P7,KP
      REAL SOLDEC,DEC,KPTOAP,AP,BY
      CHARACTER*1 HEMIS,SEASON,MAGACT,SOLACT
      CHARACTER*2 HMPATT
      CHARACTER*7 ID
      LOGICAL USEDEC,USEAP
C
C  Determine the hemisphere descriptor from the magnetic latitude
C
      IF(MLAT .GE. 0.) THEN
         HEMIS='N'
      ELSE
         HEMIS='S'
      ENDIF
C
C  If you want to use the solar declination to determine the season descriptor,
C  the logical flag USEDEC should be set .TRUE.; if you want to use the day
C  of the year directly, USEDEC should be set .FALSE.
C  The solar declination is a better indicator of season; however, you may
C  find it more convenient to work with the day of the year directly.
C
      USEDEC=.TRUE.
C
C  Determine the season descriptor from the solar declination
C
      IF(USEDEC) THEN
         DEC=SOLDEC(DAY,UT)
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
C  Determine the season descriptor from the day of the year directly
C
      ELSE
         IF(DAY .LE. 82) THEN
            IF(365+DAY-357 .LE. 82-DAY) THEN
               IF(MLAT .GE. 0.) THEN
                  SEASON='W'
               ELSE
                  SEASON='S'
               ENDIF
            ELSE
               SEASON='E'
            ENDIF
         ELSE IF((DAY .GT. 82) .AND. (DAY .LE. 173)) THEN
            IF(DAY-82 .LE. 173-DAY) THEN
               SEASON='E'
            ELSE
               IF(MLAT .GE. 0.) THEN
                  SEASON='S'
               ELSE
                  SEASON='W'
               ENDIF
            ENDIF
         ELSE IF((DAY .GT. 173) .AND. (DAY .LE. (173+357)/2)) THEN
            IF(DAY-173 .LE. (173+357)/2-DAY) THEN
               IF(MLAT .GE. 0.) THEN
                  SEASON='S'
               ELSE
                  SEASON='W'
               ENDIF
            ELSE
               SEASON='E'
            ENDIF
         ELSE IF((DAY .GT. (173+357)/2) .AND. (DAY .LE. 357)) THEN
            IF(DAY-(173+357)/2 .LE. 357-DAY) THEN
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
C  If you want to use Ap to determine the magnetic activity level descriptor,
C  the logical flag USEAP should be set .TRUE.; if you want to use Kp directly,
C  USEAP should be set .FALSE.
C  Ap is a better indicator of magnetic activity level because is it is quasi-
C  linear while Kp is quasi-logarithmic; however, you may find it more
C  convenient to work with Kp directly.
C
      USEAP=.TRUE.
C
C  Determine the magnetic activity level descriptor from Ap
C
      IF(USEAP) THEN
         AP=KPTOAP(KP)
         IF(AP .LE. KPTOAP(1.)) THEN
            MAGACT='L'
         ELSE IF((AP .GT. KPTOAP(1.)) .AND. (AP .LE. KPTOAP(3.5))) THEN
            IF(AP-KPTOAP(1.) .LE. KPTOAP(3.5)-AP) THEN
               MAGACT='L'
            ELSE
               MAGACT='M'
            ENDIF
         ELSE IF((AP .GT. KPTOAP(3.5)) .AND. (AP .LE. KPTOAP(6.))) THEN
            IF(AP-KPTOAP(3.5) .LE. KPTOAP(6.)-AP) THEN
               MAGACT='M'
            ELSE
               MAGACT='H'
            ENDIF
         ELSE
            MAGACT='H'
         ENDIF
C
C  Determine the magnetic activity level descriptor from Kp
C
      ELSE
         IF(KP .LE. 1.) THEN
            MAGACT='L'
         ELSE IF((KP .GT. 1.) .AND. (KP .LE. 3.5)) THEN
            IF(KP-1. .LE. 3.5-KP) THEN
               MAGACT='L'
            ELSE
               MAGACT='M'
            ENDIF
         ELSE IF((KP .GT. 3.5) .AND. (KP .LE. 6.)) THEN
            IF(KP-3.5 .LE. 6.-KP) THEN
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
      IF(F10P7 .LE. 70.) THEN
         SOLACT='L'
      ELSE IF((F10P7 .GT. 70.) .AND. (F10P7 .LE. 130.)) THEN
         IF(F10P7-70. .LE. 130.-F10P7) THEN
            SOLACT='L'
         ELSE
            SOLACT='M'
         ENDIF
      ELSE IF((F10P7 .GT. 130.) .AND. (F10P7 .LE. 210.)) THEN
         IF(F10P7-130. .LE. 210.-F10P7) THEN
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
      IF(BY .GT. 0.0) THEN
         HMPATT='BC'
      ELSE IF(BY .LT. 0.0) THEN
         HMPATT='DE'
      ELSE
         STOP 'DETID:Invalid By direction.'
      ENDIF
C
C  Determine the case identifier
C
      ID=HEMIS//'H'//SEASON//MAGACT//SOLACT//HMPATT
C
      RETURN
      END
C
      SUBROUTINE OPEN_FILE(PATH,FLNM,L1,N1,IOPEN)
C
C  PURPOSE
C     To open the files given by the name FLNM
C
C  METHOD
C     No discussion
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    FLNM    CHAR              The name of the file to be opened
C    IOPEN   INTEGER           1 for byte record lengths, 2 for longword
C                              record lengths
C    L1      INTEGER           The unit to assign the file to
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C     N1     INTEGER           Will be passed to NUM_HT for the proper
C                              File call, otherwise, unused.
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    FSPEC   CHAR*100          The file specification
C    ISTAT   INTEGER           Status returned from the STRCCT routine
C
C  SUBROUTINES CALLED
C     STRCCT   Concatenates two character strings
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    FLNM
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby     1-Feb-1990   1.0.6
C     L. Brown        25-Sep-1995   1.0.6 ==> 1.3
C                                   The concatenation of the path and file name
C                                   has been replaced by a call to routine
C                                   STRCCT in order to resolve an
C                                   incompatibility with the Watcom and
C                                   Microsoft FORTRAN compilers.  This problem
C                                   was reported by Jerry Ferguson and George
C                                   Bendo.
C                                   Reporting of OPEN errors has been improved.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of declaration
      INCLUDE 'logicuni.inc'
      INTEGER L1,N1,ISTAT,IOPEN
      CHARACTER*(*) PATH
      CHARACTER*(*) FLNM
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
C     End of declaration statements, beginning of executable code
C     Try to open the file
      CALL STRCCT(PATH,FLNM,32,FSPEC,ISTAT)
      IF (IOPEN .EQ. 1) THEN
       OPEN(UNIT=L1,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',ERR=50,
     &      ACCESS='DIRECT',RECL=N1*4)
      ELSE IF (IOPEN .EQ. 2) THEN
       OPEN(UNIT=L1,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',ERR=50,
     &      ACCESS='DIRECT',RECL=N1)
      ELSE
       WRITE(LUSTDERR,*) IOPEN
       STOP 'WRONG IOPEN IN OPEN_FILE'
      ENDIF
      GOTO 100
 50   CONTINUE
      WRITE(LUSTDERR,*) ' OPEN_FILE:Error opening file'
      WRITE(LUSTDERR,*) ' ',FSPEC
      WRITE(LUSTDERR,*) ' as unit ',L1
      STOP 'Error in OPEN_FILE.'
 100  CONTINUE
      RETURN
      END
C
        SUBROUTINE GET_INDX(MLAT,INDX)
C
C  PURPOSE
C     To give the correct index to use for the USU database arrays
C
C  METHOD
C     If the magnetic latitude is less than -51. degrees, use index 1
C  corresponding to the southern high latitude database. If the magnetic
C  latitude is greater than 51. degrees, use index 2 corresponding to the
C  nothern high latitude database. This subroutine will be generalized to
C  give the correct index for all areas once HLISM is merged with PRISM.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    MLAT    REAL              Magnetic latitude
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    INDX    INTEGER           Index aof USU arrays to be used.
C
C  LOCAL VARIABLES
C     NONE
C
C  SUBROUTINES CALLED
C     NONE
C
C  FUNCTIONS CALLED
C     NONE
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.6   04-Jan-1991
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C
        REAL MLAT
        INTEGER INDX
        IF (MLAT .LE. -51.) THEN
         INDX = 1
        ELSE IF (MLAT .GE. 51.) THEN
         INDX = 2
        ELSE
         INDX = 2
         IF (MLAT .LT. 0.0) INDX = 1
C         PRINT *,' Error in get index',MLAT,INDX
        ENDIF
        RETURN
        END
C
