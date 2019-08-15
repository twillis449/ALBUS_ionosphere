       SUBROUTINE READ_DBASES(IDOY,MONTH,UT,USUPATH,MIDPATH,LOWPATH,
     1  LMEPATH,AWSPATH,IOPEN)
C
C  PURPOSE
C     To read the appropriate USU database.
C
C  METHOD
C     Use and UT (input) to determine which set of coefficients to use.
C
C  INPUT PARAMETERS
C    NAME      TYPE       ARRAY  description
C    AWSPATH   CHARACTER         Path to the directory with the URSI co-
C                                efficients
C    IDOY      INTEGER           Day of the year
C    LMEPATH   CHARACTER         Path to directory where low and mid latitude
C                                E layer database is stored
C    LOWPATH   CHARACTER         Path to directory where low latitude F layer
C                                database is stored
C    MIDPATH   CHARACTER         Path to directory where midlatitude F layer
C                                database is stored
C    MONTH     INTEGER           Month of the year
C    OFLNM     CHARACTER         Name of database from a previous read to
C                                determine if the read is necessary.
C    USUPATH   CHARACTER         Path to directory where USU database is stored
C    UT        REAL              Universal time
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY    description
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C
C  SUBROUTINES CALLED
C    NAME             description
C    GET_CHAR         Converts indirect parameters to components of the
C                     USU database file name.
C    GET_FILE_NAME    Gets the root file name
C    RDR              Opens the files, reads the altitude values and the
C                     empirical orthogonal functions, and reads the co-
C                     efficients of the derived orthogonal functions for
C                     the reconstruction.
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     William Whartenby
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
C     W. Whartenby     1-May-1990   1.0.6
C     L. Brown        25-Sep-1995   1.0.6 ==> 1.3
C                                   Removed input parameter BATCH since it is
C                                   no longer used.
C     L. Brown        30-Sep-1996   1.3 ==> 1.5
C                                   Added argument UT to call to RDLOW.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
       INCLUDE 'indirect.inc'
       INCLUDE 'usuarr.inc'
       INTEGER IDOY,MONTH,IOPEN
       REAL UT
       CHARACTER*(*) MIDPATH,USUPATH,LOWPATH,LMEPATH,AWSPATH
C
C  Read the parameterized low- and mid- latitude E-region model database
C
C       WRITE(*,'(1X,A)') 'Reading parameterized low- and mid- '//
C     &      'latitude E-region model database...'
      CALL READ_E(LMEPATH,IDOY,UT,IOPEN)
C
C  Read the parameterized low-latitude F-region model database
C
C      WRITE(*,'(1X,A)') 'Reading parameterized low-latitude F-region '//
C     &     'model database...'
      CALL RDLOW(LOWPATH,IDOY,UT,IOPEN)
C
C  Read the parameterized mid-latitude F-region model database
C
C      WRITE(*,'(1X,A)') 'Reading parameterized mid-latitude F-region '//
C     &     'model database...'
      CALL READMID(MIDPATH,IDOY,UT,IOPEN)
C
C  Read the parameterized USU model database
C
C      WRITE(*,'(1X,A)') 'Reading parameterized USU model database...'
      CALL READUSU(IDOY,BY,UT,USUPATH,IOPEN)
C
C  Read the URSI-88 coefficients database
C
C      WRITE(*,'(1X,A)') 'Reading URSI-88 coefficients database...'
      CALL READAWS(AWSPATH,MONTH,IOPEN)
c     WRITE(*,'(1X,A)') 'Running...'
C
      RETURN
      END
C
      SUBROUTINE RDLOW(PATH,DAY,UT,IOPEN)
C
C  PURPOSE
C     To read the low latitude LOWLAT-F database using modular
C     LOWLAT-F database access routines
C
C  METHOD
C     From the given input parameters, the LOWLAT-F case identifier is
C     determined.  EOFs for the case are read from the appropriate EOF file.
C     Orthogonal polynomial coefficients for the case are read from the
C     appropriate orthogonal polynomial coefficients file.  The orthogonal
C     polynomial coefficients describe the behavior of EOF coefficients
C     in magnetic latitude.
C
C  INPUT PARAMETERS
C     DAY      The day of the year
C     UT       Universal Time, in decimal hours
C
C  OUTPUT PARAMETERS
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     EOF      Empirical orthogonal functions (EOFs)
C     MLON     The magnetic longitude of each sector, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density profile
C     NMLAT    The number of magnetic latitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C
C  LOCAL VARIABLES
C     D        Eigenvalues
C     DSHFT    Shifted eigenvalues
C     DUM      A dummy argument
C     ID       The case identifier
C     IDUM     A dummy argument
C     JF10P7L  Starting index for linear F10.7 interpolation
C     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
C              Indian, 'PAC' for Pacific, and 'USA' for USA
C              north
C
C  SUBROUTINES REQUIRED
C     LDETID   Determines the case identifier
C     LRDEOFF  Reads an EOF file
C     LRDOPCF  Reads an orthogonal polynomial coefficients file
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
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby    11-Jun-1991   1.0.6
C                                    This subroutine was originally a driver
C                                   that read and evaluated each case for
C                                   testing puroses. Now the evaluation rou-
C                                   tines have been placed in a different
C                                   calling sequence, and all READ variables
C                                   have had their dimensions extended so that
C                                   this READ routine need only be called once.
C     L. Brown         7-Oct-1994   1.0.6 ==> 1.1
C                                   Common block LOWER (in INCLUDE file
C                                   'lower.inc') has been modified to reflect
C                                   the elimination of Fourier fitting in UT
C                                   in the low-latitude F-region parameterized
C                                   model (LLF).
C                                   The calls to routines LRDEOFF and LRDOPCF
C                                   have been modified to reflect the
C                                   elimination of Fourier fitting in UT in the
C                                   low-latitude F-region parameterized model
C                                   (LLF).
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   Added INCLUDE statement for INCLUDE file
C                                   'indirect.inc'.
C                                   Added calculation of F10.7 starting index
C                                   and linear interpolation factors.  This
C                                   allows the use of a 2 x 4 (F10.7 x
C                                   Longitude Sector) profile matrix instead of
C                                   a 3 x 4 profile matrix.
C                                   Removed local variables AP and KP to avoid
C                                   conflict with common block INDIRECT.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Replaced arguments NUT(IL,JL), SUT(IL,JL),
C                                   and DUT(IL,JL) with NMLT(IL,JL),
C                                   SMLT(IL,JL), and DMLT(IL,JL) in the calls
C                                   to routines LRDEOFF and LRDOPCF.
C     L. Brown        30-Sep-1996   1.3 ==> 1.5
C                                   Added input argument UT.
C                                   Added argument UT to call to LDETID.
C     L. Brown        13-Jan-1998   1.5 ==> 1.7
C                                   Extrapolation in F10.7 above 210 is now
C                                   allowed.
C
C  REFERENCES
C     NONE
C
      INCLUDE 'lower.inc'
      INCLUDE 'indirect.inc'
      INTEGER DAY,IL,JL,IOPEN,J
      INTEGER JF10P7L
      REAL UT
      REAL D(MEOF),DSHFT(MEOF),DUM
      CHARACTER*3 SECTNAM(MSECT)
      CHARACTER*8 ID
      CHARACTER*(*) PATH
      DATA SECTNAM/'BRZ','IND','PAC','USA'/
C
C  Calculate the F10.7 starting index and linear interpolation factors
C
      IF(F10P7 .LT. F10P7L(2)) THEN
         JF10P7L=1
      ELSE
         JF10P7L=2
      ENDIF
      TF10P7L=MAX(0.,(F10P7-F10P7L(JF10P7L))
     &              /(F10P7L(JF10P7L+1)-F10P7L(JF10P7L)))
      OMTF10P7L=1.-TF10P7L
C
C     Determine the case identifier and read for all cases
C
      JL=0
      DO J = JF10P7L,JF10P7L+1
       JL=JL+1
       DO IL = 1,MSECT
        CALL LDETID(DAY,UT,SECTNAM(IL),F10P7L(J),ID)
C
C       Read the LOWLAT-F O+ EOF file
C
        CALL LRDEOFF(98,PATH,ID,DUM,DUM,DUM,MLN(IL,JL),NMLAT(IL,JL),
     &               SMLAT(IL,JL),DMLAT(IL,JL),NMLT(IL,JL),SMLT(IL,JL),
     &               DMLT(IL,JL),D,DSHFT,NALT(IL,JL),ALT(1,IL,JL),
     &               EOF(1,1,IL,JL),IOPEN)
C
C       Read the LOWLAT-F O+ orthogonal polynomial coefficients file
C
        CALL LRDOPCF(99,PATH,ID,DUM,DUM,DUM,MLN(IL,JL),NMLAT(IL,JL),
     &               SMLAT(IL,JL),DMLAT(IL,JL),NMLT(IL,JL),SMLT(IL,JL),
     &               DMLT(IL,JL),NOPM1(IL,JL),NEOF(IL,JL),DELTA(IL,JL),
     &               OPC(1,1,1,IL,JL),IOPEN)
C
       ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE LDETID(DAY,UT,SECTOR,F10P7,ID)
C
C  PURPOSE
C     To determine a LOWLAT-F case identifier.
C
C  METHOD
C     The LOWLAT-F case identifier is a code for describing the ambient
C     conditions (magnetic longitude sector, day of the year, and solar
C     activity level) of a LOWLAT-F case.  The case identifier has the form
C     'sssnnccc', where 'sss' describes the magnetic longitude sector, 'nn'
C     describes the month, and 'ccc' describes the solar activity level.  The
C     magnetic longitude sectors are 'BRZ' for Brazilian, 'IND' for Indian,
C     'PAC' for Pacific, and 'USA' for USA.  The day of the year determines
C     'nn':  '03' for March (equinox), '06' for June (solstice), and '12' for
C     December (solstice). The solar F10.7 index determines 'ccc':  'MIN' for
C     Minimum, 'MOD' for Moderate, and 'MAX' for Maximum.
C
C  INPUT PARAMETERS
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux
C     SECTOR   The magnetic longitude sector, 'BRZ' for Brazilian, 'IND' for
C              Indian, 'PAC' for Pacific, and 'USA' for USA
C     UT       Universal Time, in decimal hours
C
C  OUTPUT PARAMETERS
C     ID       The case identifier
C
C  LOCAL VARIABLES
C     DEC      The solar declination, in degrees north
C     MONTH    The month descriptor, '03' for March, '06' for June, and '12'
C              for December
C     SOLACT   The solar activity level descriptor, 'MIN' for Minimum, 'MOD'
C              for Moderate, and 'MAX' for Maximum
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the month descriptor, .FALSE. if the day of the
C              year is to be used
C
C  SUBROUTINES REQUIRED
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
C     L. Brown         4-Jun-1991   1.0
C                                   Created
C     L. Brown        13-Nov-1991   1.0 --> 1.1
C                                   Complete rewrite to simplify coding and to
C                                   correct a bug in the determination of the
C                                   month descriptor.
C                                   The selection of the month descriptor has
C                                   been enhanced as well (see the comments
C                                   below).
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Calls to CONCAT and UPCASE eliminated.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   The month descriptor is now always two
C                                   characters long ('03', '06', or '12').
C     L. Brown        30-Sep-1996   1.1 ==> 1.5
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
      REAL UT,F10P7
      REAL SOLDEC,DEC
      CHARACTER*2 MONTH
      CHARACTER*3 SECTOR
      CHARACTER*3 SOLACT
      CHARACTER*8 ID
      LOGICAL USEDEC
C
C  Check the magnetic longitude sector descriptor
C
      IF((SECTOR .NE. 'BRZ') .AND. (SECTOR .NE. 'IND') .AND.
     $   (SECTOR .NE. 'PAC') .AND. (SECTOR .NE. 'USA'))
     $   STOP 'LDETID:Invalid magnetic longitude sector.'
C
C  If you want to use the solar declination to determine the month descriptor,
C  the logical flag USEDEC should be set .TRUE.; if you want to use the day
C  of the year directly, USEDEC should be set .FALSE.
C
      USEDEC=.TRUE.
C
C  Determine the month descriptor from the solar declination
C
      IF(USEDEC) THEN
         DEC=SOLDEC(DAY,UT)
         IF(DEC .LE. -23.5/2.) THEN
            MONTH='12'
         ELSE IF(ABS(DEC) .LT. 23.5/2.) THEN
            MONTH='03'
         ELSE
            MONTH='06'
         ENDIF
C
C  Determine the month descriptor from the day of the year directly
C
      ELSE
         IF(DAY .LE. 82) THEN
            IF(365+DAY-357 .LE. 82-DAY) THEN
               MONTH='12'
            ELSE
               MONTH='03'
            ENDIF
         ELSE IF((DAY .GT. 82) .AND. (DAY .LE. 173)) THEN
            IF(DAY-82 .LE. 173-DAY) THEN
               MONTH='03'
            ELSE
               MONTH='06'
            ENDIF
         ELSE IF((DAY .GT. 173) .AND. (DAY .LE. (173+357)/2)) THEN
            IF(DAY-173 .LE. (173+357)/2-DAY) THEN
               MONTH='06'
            ELSE
               MONTH='03'
            ENDIF
         ELSE IF((DAY .GT. (173+357)/2) .AND. (DAY .LE. 357)) THEN
            IF(DAY-(173+357)/2 .LE. 357-DAY) THEN
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
      IF(F10P7 .LE. 70.) THEN
         SOLACT='MIN'
      ELSE IF((F10P7 .GT. 70.) .AND. (F10P7 .LE. 130.)) THEN
         IF(F10P7-70. .LE. 130.-F10P7) THEN
            SOLACT='MIN'
         ELSE
            SOLACT='MOD'
         ENDIF
      ELSE IF((F10P7 .GT. 130.) .AND. (F10P7 .LE. 210.)) THEN
         IF(F10P7-130. .LE. 210.-F10P7) THEN
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
      IF(MONTH(1:1) .EQ. ' ') THEN
         ID=SECTOR//MONTH(2:2)//SOLACT
      ELSE
         ID=SECTOR//MONTH//SOLACT
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE LRDEOFF(LUN,PATH,ID,F10P7,AP,KP,MLON,
     $    NMLAT,SMLAT,DMLAT,NMLT,SMLT,DMLT,D,DSHFT,NALT,ALT,EOF,IOPEN)
C
C  PURPOSE
C     Reads a LOWLAT-F O+ EOF file.
C
C  METHOD
C     The name of the LOWLAT-F O+ EOF file is determined from the case
C     identifier.  Header information, followed by the altitude grid,
C     eigenvalues, and EOFs are read from the LOWLAT-F O+ EOF file.
C
C  INPUT PARAMETERS
C     ID       The case identifier
C     LUN      The logical unit number used to access the LOWLAT-F O+ EOF file
C
C  OUTPUT PARAMETERS
C     AP       The magnetic Ap index
C     ALT      The altitude grid, in km
C     D        Eigenvalues corresponding to the EOFs
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     DSHFT    Shifted eigenvalues corresponding to the EOFs
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude of the sector, in degrees east
C     NALT     The number of altitude points
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FILE     The name of the LOWLAT-F O+ EOF file
C     FSPEC    The file specification of the LOWLAT-F O+ EOF file
C     I        A loop counter
C     J        A loop counter
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters to lower-case in a character string
C
C  FILES ACCESSED
C     #LUN     The LOWLAT-F O+ EOF file
C
C  AUTHOR
C     Lincoln Brown
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
C     L. Brown         4-Jun-1991   1.0.6 ==> Created
C     L. Brown         7-Oct-1994   1.0.6 ==> 1.1
C                                   Eliminated a call to STRCCT since the ID is
C                                   now always 8 characters long.
C     L. Brown        25-Sep-1995   1.1 ==> 1.3
C                                   The concatenation of the path and file name
C                                   has been replaced by a call to routine
C                                   STRCCT in order to resolve an
C                                   incompatibility with the Watcom and
C                                   Microsoft FORTRAN compilers.  This problem
C                                   was reported by Jerry Ferguson and George
C                                   Bendo.
C                                   Replaced input parameters NUT, SUT, and DUT
C                                   with NMLT, SMLT, and DMLT.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C
      INTEGER MALT,MEOF
      PARAMETER(MALT=55,MEOF=55)
      INCLUDE 'logicuni.inc'
C
      INTEGER LUN,NMLAT,NMLT,NALT
      INTEGER I,J,IOPEN
      REAL D(MEOF),DSHFT(MEOF),ALT(MALT),EOF(MALT,MEOF)
      REAL F10P7,AP,KP,MLON,SMLAT,DMLAT,SMLT,DMLT,DUM1
      CHARACTER*8 ID
      CHARACTER*11 FILE
      CHARACTER*(*) PATH
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
C
C  Determine the specification of the LOWLAT-F O+ EOF file
C
      FILE=ID//'.OE'
      CALL STRLCA(FILE,I)
      CALL STRCCT(PATH,FILE,32,FSPEC,I)
C
C  Open the LOWLAT-F O+ EOF file.  Note that in the VAX Fortran implementation,
C  the record-length of unformatted direct-access files (specified by the RECL
C  keyword) is given in longwords (4 byte units), corresponding to the space
C  required for an integer*4 or a real*4.  In other implementations, the
C  record-length might be given in bytes. In a modern FORTRAN implementation
C  this OPEN statement may include a parameter for Read Only for multi-user
C  or data security purposes.
C
      IF (IOPEN .EQ. 1) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=MALT*4)
      ELSE IF (IOPEN .EQ. 2) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=MALT)
      ELSE
       WRITE(LUSTDERR,*) IOPEN
       STOP 'WRONG IOPEN IN LRDEOFF'
      ENDIF
C
C  Read header information from the LOWLAT-F O+ EOF file
C
C      Don't use the year and day of the data
C      READ(LUN,REC=1) DUM1,DUM2
C      YEAR=INT(DUM1)
C      DAY=INT(DUM2)
C
      READ(LUN,REC=2) F10P7,AP,KP
      READ(LUN,REC=3) MLON
      READ(LUN,REC=4) DUM1,SMLAT,DMLAT
      NMLAT=INT(DUM1)
      READ(LUN,REC=5) DUM1,SMLT,DMLT
      NMLT=INT(DUM1)
C
C  Read the altitude grid from the LOWLAT-F O+ EOF file
C
      READ(LUN,REC=6) DUM1
      NALT=INT(DUM1)
      READ(LUN,REC=7) (ALT(I),I=1,NALT)
C
C  Read the eigenvalues from the LOWLAT-F O+ EOF file
C
      READ(LUN,REC=8) (D(I),I=1,NALT)
      READ(LUN,REC=9) (DSHFT(I),I=1,NALT)
C
C  Read the EOFs from the LOWLAT-F O+ EOF file
C
      DO 10 J=1,NALT
         READ(LUN,REC=9+J) (EOF(I,J),I=1,NALT)
   10 CONTINUE
C
C  Close the LOWLAT-F O+ EOF file
C
      CLOSE(UNIT=LUN)
C
      RETURN
      END
C
      SUBROUTINE LRDOPCF(LUN,PATH,ID,F10P7,AP,KP,MLON,NMLAT,SMLAT,DMLAT,
     &                   NMLT,SMLT,DMLT,NOPM1,NEOF,DELTA,OPC,IOPEN)
C
C  PURPOSE
C     Reads a LOWLAT-F O+ orthogonal polynomial coefficients file.
C
C  METHOD
C     The name of the LOWLAT-F O+ orthogonal polynomial coefficients file is
C     determined from the case identifier.  Header information, followed by
C     orthogonal polynomial coefficients, are read from the LOWLAT-F O+
C     orthogonal polynomial coefficients file.
C
C  INPUT PARAMETERS
C     ID       The case identifier
C     LUN      The logical unit number used to access the LOWLAT-F O+
C              orthogonal polynomial coefficients file
C
C  OUTPUT PARAMETERS
C     AP       The magnetic Ap index
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     MLON     The magnetic longitude of the sector, in degrees east
C     NEOF     The number of EOFs used to construct an O+ density profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OPC      Orthogonal polynomial coefficients
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     FILE     The name of the LOWLAT-F O+ orthogonal polynomial coefficients
C              file
C     FSPEC    The file specification of the LOWLAT-F O+ orthogonal polynomial
C              coefficients file
C     I        A loop counter
C     IMAX     NOPM1+1
C     IREC     A record number
C     J        A loop counter
C     K        A loop counter
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters to lower-case in a character string
C
C  FILES ACCESSED
C     #LUN     The LOWLAT-F O+ orthogonal polynomial coefficients file
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Call to CONCAT replaced by call to STRCCT.
C                                   Call to LOCASE replaced by call to STRLCA.
C                                   Calculation of IREC optimized.
C                                   Redundant calculation of quantity NOPM1+1
C                                   has been eliminated by storing in a
C                                   variable.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   The argument list has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   The structure of the orthogonal polynomial
C                                   coefficients file has changed to reflect
C                                   the elimination of Fourier fitting in UT in
C                                   the low-latitude F-region parameterized
C                                   model (LLF).
C                                   Eliminated a call to STRCCT since the ID is
C                                   now always 8 characters long.
C     L. Brown        25-Sep-1995   1.1 ==> 1.3
C                                   The concatenation of the path and file name
C                                   has been replaced by a call to routine
C                                   STRCCT in order to resolve an
C                                   incompatibility with the Watcom and
C                                   Microsoft FORTRAN compilers.  This problem
C                                   was reported by Jerry Ferguson and George
C                                   Bendo.
C                                   Replaced input parameters NUT, SUT, and DUT
C                                   with NMLT, SMLT, and DMLT.
C     L. Brown        13-Jan-1998   1.3 ==> 1.7
C                                   Changed PARAMETER MOPM1 from 11 to 14 for
C                                   new LLF parameterization.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INCLUDE 'logicuni.inc'
      INTEGER MEOF,MOPM1,MMLT
      PARAMETER(MEOF=55,MOPM1=14,MMLT=48)
C
      INTEGER LUN,NOPM1,NEOF,NMLAT,NMLT
      INTEGER I,J,K,IOPEN
      INTEGER IREC,IMAX
      REAL OPC(MOPM1+1,MEOF,MMLT)
      REAL F10P7,AP,KP,DELTA,MLON,SMLAT,DMLAT,SMLT,DMLT
      REAL DUM1,DUM2
      CHARACTER*8 ID
      CHARACTER*11 FILE
      CHARACTER*(*) PATH
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
C
C  Determine the specification of the LOWLAT-F O+ orthogonal polynomial
C  coefficients file
C
      FILE=ID//'.OP'
      CALL STRLCA(FILE,I)
      CALL STRCCT(PATH,FILE,32,FSPEC,I)
C
C  Open the LOWLAT-F O+ orthogonal polynomial coefficients file.  Note that in
C  the VAX Fortran implementation, the record-length of unformatted direct-
C  access files (specified by the RECL keyword) is given in longwords (4 byte
C  units), corresponding to the space required for an integer*4 or a real*4.
C  In other implementations, the record-length might be given in bytes.In a
C  modern FORTRAN implementation this OPEN statement may include a parameter
C  for Read Only for multi-user or data security purposes.
C
      IF (IOPEN .EQ. 1) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=(MOPM1+1)*4)
      ELSE IF (IOPEN .EQ. 2) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=MOPM1+1)
      ELSE
       WRITE(LUSTDERR,*) IOPEN
       STOP 'WRONG IOPEN IN LRDOPFC'
      ENDIF
C
C  Read header information from the LOWLAT-F O+ orthogonal polynomial
C  coefficients file
C
C     Dont use the year or day read in for this particular data set.
C      READ(LUN,REC=1) DUM1,DUM2
C      YEAR=INT(DUM1)
C      DAY=INT(DUM2)
C
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
C  Read the orthogonal polynomial coefficients from the LOWLAT-F O+ orthogonal
C  polynomial coefficients file
C
      IREC=7
      IMAX=NOPM1+1
      DO 20 K=1,NMLT
         DO 10 J=1,NEOF
            IREC=IREC+1
            READ(LUN,REC=IREC) (OPC(I,J,K),I=1,IMAX)
   10    CONTINUE
   20 CONTINUE
C
C  Close the LOWLAT-F O+ orthogonal polynomial coefficients file
C
      CLOSE(UNIT=LUN)
C
      RETURN
      END
C
      SUBROUTINE READ_E(PATH,DAY,UT,IOPEN)
C
C     PURPOSE
C       To read the low and midlatitude E layer databases
C
C     METHOD
C       No discussion
C
C     INPUT VARIABLES
C     NAME     TYPE      ARRAY    description
C     DAY      INTEGER            Day of the year
C     PATH     CHARACTER*(*)      Disk path to databases
C     UT       REAL               Universal time
C
C     OUTPUT VARIABLES
C     None
C
C     LOCAL VARIABLES
C     NAME     TYPE      ARRAY    description
C     AP       REAL               Magnetic activity AP index
C     D        REAL     (MEOF)    Array of vectors used in fitting
C     DSHFT    REAL     (MEOF)    Array of shift vectors
C     DMUT     REAL               Delta universal time for the different
C                                 databases
C     DUM1     REAL               Dummy variable returned from RDOPFC
C     DUM2     REAL               Dummy variable returned from RDOPFC
C     I        INTEGER            Do loop index
C     ID       CHARACTER*6        Root name of database for this run
C     IDUM     INTEGER            Dummy variable returned from RDOPFC
C     ION      CHARACTER*2        The ion species descriptor, 'NO' for NO+
C                                 and 'O2' for O2+
C     JF10P7LE INTEGER            Starting index for linear F10.7 interpolation
C     JKPLE    INTEGER            Starting index for linear Kp interpolation
C     NUT      INTEGER            Universal time index for reads
C     RECLEN   INTEGER            The record length of the orthogonal
C                                 polynomial coefficients file, in bytes
C     SMUT     REAL               Smallest universal time for the different
C                                 databases
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.9
C                                   Calculation of NREC optimized.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   Common block LOW_E1 (in INCLUDE file
C                                   'low_e.inc') has been modified to reflect
C                                   the elimination of Fourier fitting in
C                                   magnetic longitude in the low- and mid-
C                                   latitude E-region parameterized model
C                                   (LME).
C                                   The calls to routines RDEOFF and RDOPCF
C                                   have been modified to reflect the
C                                   elimination of Fourier fitting in magnetic
C                                   longitude in the low- and mid- latitude
C                                   E-region parameterized model (LME).
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   Added INCLUDE statement for INCLUDE file
C                                   'indirect.inc'.
C                                   Added calculation of F10.7 and Kp starting
C                                   indices and linear interpolation factors.
C                                   This allows the use of a 2 x 2 (F10.7 x Kp)
C                                   profile matrix instead of a 3 x 3 profile
C                                   matrix.
C     L. Brown        30-Sep-1996   1.2 ==> 1.5
C                                   Added argument UT to call to EDETID.
C     L. Brown        13-Jan-1998   1.5 ==> 1.7
C                                   Extrapolation in F10.7 above 210 is now
C                                   allowed.
C
      INCLUDE 'low_e.inc'
      INCLUDE 'indirect.inc'
C
C     I/O variable declarations
C
      INTEGER DAY
      REAL UT
      CHARACTER*(*) PATH
C
C     Local variable declarations
C
      INTEGER NUT,I,J,K,NREC,IOPEN,RECLEN
      INTEGER JF10P7LE,JKPLE
      REAL SUT,DUT
      REAL D(MEOF),DSHFT(MEOF)
      CHARACTER*2 ION
      CHARACTER*6 ID
C
C  Calculate the F10.7 starting index and linear interpolation factors
C
      IF(F10P7 .LT. F10P7LE(2)) THEN
         JF10P7LE=1
      ELSE
         JF10P7LE=2
      ENDIF
      TF10P7LE=MAX(0.,(F10P7-F10P7LE(JF10P7LE))
     &               /(F10P7LE(JF10P7LE+1)-F10P7LE(JF10P7LE)))
      OMTF10P7LE=1.-TF10P7LE
C
C  Calculate the Kp starting index and linear interpolation factors
C
      IF(EKP .LT. KPLE(2)) THEN
         JKPLE=1
      ELSE
         JKPLE=2
      ENDIF
      TKPLE=MAX(0.,MIN(1.,(EKP-KPLE(JKPLE))
     &                   /(KPLE(JKPLE+1)-KPLE(JKPLE))))
      OMTKPLE=1.-TKPLE
C
C     Determine the case identifier and read for all cases
C
      NREC=0
      DO K = JKPLE,JKPLE+1
       DO J = JF10P7LE,JF10P7LE+1
        NREC=NREC+1
        CALL EDETID(DAY,UT,F10P7LE(J),KPLE(K),ID)
        DO I = 1,2
C
C        Determine the ion species descriptor
C
         CALL EDETION(I,ION)
C
C        Read the ECSD EOF file
C
         CALL RDEOFF(98,PATH,ID,ION,NMLON(I,NREC),SMLON(I,NREC),
     &               DMLON(I,NREC),NMLAT(I,NREC),SMLAT(I,NREC),
     &               DMLAT(I,NREC),NUT,SUT,DUT,D,DSHFT,N1ALT(I,NREC),
     &               MALT,MEOF,ALT1(1,I,NREC),EOF(1,1,I,NREC),IOPEN)
C
C        Read the ECSD orthogonal polynomial coefficients file
C
         IF(ION .EQ. 'NO') THEN
            RECLEN=13
         ELSE
            RECLEN=MOPM1+1
         ENDIF
         CALL RDOPCF(99,PATH,ID,ION,UT,NMLON(I,NREC),SMLON(I,NREC),
     &               DMLON(I,NREC),NMLAT(I,NREC),SMLAT(I,NREC),
     &               DMLAT(I,NREC),NUT,SUT,DUT,NOPM1(I,NREC),
     &               NEOF(I,NREC),MEOF,MOPM1,MMLON,DELTA(I,NREC),
     &               OPC(1,1,1,I,NREC),IOPEN,RECLEN)
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE EDETID(DAY,UT,F10P7,KP,ID)
C
C  PURPOSE
C     To determine an ECSD case identifier.
C
C  METHOD
C     The ECSD case identifier is a code for describing the ambient conditions
C     (day of the year, magnetic activity level, and solar activity level) of
C     an ECSD case.  The case identifier has the form 'LMnnab', where 'LM'
C     defines the ECSD case as low- and mid-latitude, 'nn' describes the month,
C     'a' describes the magnetic activity level, and 'b' describes the solar
C     activity level.  The day of the year determines 'nn':  '03' for March
C     (equinox), '06' for June (solstice), and '12' for December (solstice).
C     The magnetic Kp index determines 'a':  'L' for Low, 'M' for Moderate,
C     and 'H' for High.  The solar F10.7 index determines 'b':  'L' for Low,
C     'M' for Moderate, and 'H' for High.
C
C  INPUT PARAMETERS
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     UT       Universal Time, in decimal hours
C
C  OUTPUT PARAMETERS
C     ID       The case identifier
C
C  LOCAL VARIABLES
C     AP       The magnetic Ap index
C     DEC      The solar declination, in degrees north
C     MAGACT   The magnetic activity level descriptor, 'L' for low, 'M' for
C              moderate, and 'H' for high
C     MONTH    The month descriptor, '03' for March, '06' for June, and '12'
C              for December
C     SOLACT   The solar activity level descriptor, 'L' for low, 'M' for
C              moderate, and 'H' for high
C     USEAP    A logical flag, .TRUE. if Ap is to be used to determine the
C              magnetic activity level descriptor, .FALSE. if Kp is to be
C              used
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the month descriptor, .FALSE. if the day of the
C              year is to be used
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
C     L. Brown         2-Aug-1991   1.0
C                                   Created
C     L. Brown        13-Nov-1991   1.0 --> 1.1
C                                   Complete rewrite to simplify coding and to
C                                   correct a bug in the determination of the
C                                   month descriptor.
C                                   The selection of month and magnetic
C                                   activity level have been enhanced as well
C                                   (see the comments below).
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Call to CONCAT eliminated.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   The month descriptor is now always two
C                                   characters long ('03', '06', or '12').
C     L. Brown        30-Sep-1996   1.1 ==> 1.5
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
      REAL UT,F10P7,KP
      REAL SOLDEC,DEC,KPTOAP,AP
      CHARACTER*1 MAGACT,SOLACT
      CHARACTER*2 MONTH
      CHARACTER*6 ID
      LOGICAL USEDEC,USEAP
C
C  If you want to use the solar declination to determine the month descriptor,
C  the logical flag USEDEC should be set .TRUE.; if you want to use the day
C  of the year directly, USEDEC should be set .FALSE.
C
      USEDEC=.TRUE.
C
C  Determine the month descriptor from the solar declination
C
      IF(USEDEC) THEN
         DEC=SOLDEC(DAY,UT)
         IF(DEC .LE. -23.5/2.) THEN
            MONTH='12'
         ELSE IF(ABS(DEC) .LT. 23.5/2.) THEN
            MONTH='03'
         ELSE
            MONTH='06'
         ENDIF
C
C  Determine the month descriptor from the day of the year directly
C
      ELSE
         IF(DAY .LE. 82) THEN
            IF(365+DAY-357 .LE. 82-DAY) THEN
               MONTH='12'
            ELSE
               MONTH='03'
            ENDIF
         ELSE IF((DAY .GT. 82) .AND. (DAY .LE. 173)) THEN
            IF(DAY-82 .LE. 173-DAY) THEN
               MONTH='03'
            ELSE
               MONTH='06'
            ENDIF
         ELSE IF((DAY .GT. 173) .AND. (DAY .LE. (173+357)/2)) THEN
            IF(DAY-173 .LE. (173+357)/2-DAY) THEN
               MONTH='06'
            ELSE
               MONTH='03'
            ENDIF
         ELSE IF((DAY .GT. (173+357)/2) .AND. (DAY .LE. 357)) THEN
            IF(DAY-(173+357)/2 .LE. 357-DAY) THEN
               MONTH='03'
            ELSE
               MONTH='12'
            ENDIF
         ELSE
            MONTH='12'
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
C  Determine the case identifier
C
      IF(MONTH(1:1) .EQ. ' ') THEN
         ID='LM'//MONTH(2:2)//MAGACT//SOLACT
      ELSE
         ID='LM'//MONTH//MAGACT//SOLACT
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE EDETION(I,ION)
C
C  PURPOSE
C     To determine the ion species descriptor.
C
C  METHOD
C     The ion species descriptor is a code for describing the ion.  It is
C     is determined by the ion species.
C
C  INPUT PARAMETERS
C     NAME     TYPE      ARRAY    description
C     I        INTEGER            Input for ion species...1 for NO+,
C                                 2 for O2+
C  OUTPUT PARAMETERS
C     NAME     TYPE      ARRAY    description
C     ION      CHARACTER*2        The ion species descriptor, 'NO' for NO+
C                                 and 'O2' for O2+
C
C  LOCAL VARIABLES
C     NAME     TYPE      ARRAY    description
C
C  SUBROUTINES REQUIRED
C     NAME     description
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
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Calls to UPCASE and TRIM eliminated.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     I/O variable declarations
C
      INTEGER I
      CHARACTER*2 ION
C
      IF(I .EQ. 1) THEN
         ION='NO'
      ELSE IF(I .EQ. 2) THEN
         ION='O2'
      ELSE
         STOP 'EDETION:Invalid ion species index.'
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE READMID(PATH,DAY,UT,IOPEN)
C
C  METHOD
C     From the given input parameters, the MIDLAT case identifier and ion
C     species descriptor are determined.  EOFs for the case and ion are read
C     from the appropriate EOF file.  Orthogonal polynomial coefficients for
C     the case, ion, and universal time are read from the appropriate
C     orthogonal polynomial coefficients file.
C
C  INPUT PARAMETERS
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     MLAT     The magnetic latitude, in degrees north
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C
C  LOCAL VARIABLES
C     D        Eigenvalues
C     DELTA    The step size for the orthogonal polynomials
C     DSHFT    Shifted eigenvalues
C     DUT      The increment of the universal time grid, in decimal hours
C     EOF      Empirical orthogonal functions (EOFs)
C     ID       The case identifier
C     ION      The ion species descriptor, 'NOP' for NO+, 'O2P' for O2+, and
C              'OP' for O+
C     JF10P7M  Starting index for linear F10.7 interpolation
C     JKPM     Starting index for linear Kp interpolation
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     RECLEN   The record length of the orthogonal polynomial coefficients
C              file, in bytes
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C
C  SUBROUTINES REQUIRED
C     DETID    Determines the case identifier
C     DETION   Determines the ion species descriptor
C     RDEOFF   Reads an EOF file
C     RDOPCF   Reads an orthogonal polynomial coefficients file
C
C  FUNCTIONS CALLED
C     NONE
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby    12-Apr-1991   Broke up the original MLION ( of which
C                                   this was a part) into the reader and exe-
C                                   cution parts to aviod the reading being
C                                   done each call.
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Calculation of NINDX optimized.
C                                   SPEC assigned "O+ " instead of " O+".
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   Common block MIDLT1 (in INCLUDE file
C                                   'midlat.inc') has been modified to reflect
C                                   the elimination of Fourier fitting in
C                                   magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C                                   The calls to routines RDEOFF and RDOPCF
C                                   have been modified to reflect the
C                                   elimination of Fourier fitting in magnetic
C                                   in the mid-latitude F-region parameterized
C                                   model (MLF).
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   Added INCLUDE statement for INCLUDE file
C                                   'indirect.inc'.
C                                   Added calculation of F10.7 and Kp starting
C                                   indices and linear interpolation factors.
C                                   This allows the use of a 2 x 2 (F10.7 x Kp)
C                                   profile matrix instead of a 3 x 3 profile
C                                   matrix.
C     L. Brown        30-Sep-1996   1.2 ==> 1.5
C                                   Added argument UT to call to DETID.
C     L. Brown        13-Jan-1998   1.5 ==> 1.7
C                                   Extrapolation in F10.7 above 210 is now
C                                   allowed.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C
      INCLUDE 'midlat.inc'
      INCLUDE 'indirect.inc'
      INTEGER DAY,NUT(2),I,J,K,NINDX,IOPEN,RECLEN
      INTEGER JF10P7M,JKPM
      REAL D(MEOF,2),DSHFT(MEOF,2),UT,SUT(2),DUT(2)
      REAL XMLAT
      CHARACTER*3 SPEC
      CHARACTER*2 ION
      CHARACTER*(*) PATH
      CHARACTER*6 ID
C
C  Calculate the F10.7 starting index and linear interpolation factors
C
      IF(F10P7 .LT. F10P7M(2)) THEN
         JF10P7M=1
      ELSE
         JF10P7M=2
      ENDIF
      TF10P7M=MAX(0.,(F10P7-F10P7M(JF10P7M))
     &              /(F10P7M(JF10P7M+1)-F10P7M(JF10P7M)))
      OMTF10P7M=1.-TF10P7M
C
C  Calculate the Kp starting index and linear interpolation factors
C
      IF(EKP .LT. KPM(2)) THEN
         JKPM=1
      ELSE
         JKPM=2
      ENDIF
      TKPM=MAX(0.,MIN(1.,(EKP-KPM(JKPM))/(KPM(JKPM+1)-KPM(JKPM))))
      OMTKPM=1.-TKPM
C
C     Determine the case identifier and read for all cases
C
      NINDX=0
      DO K = JKPM,JKPM+1
       DO J = JF10P7M,JF10P7M+1
        NINDX=NINDX+1
        DO I = 1,2
         XMLAT = 50.
         IF ( I .EQ. 2) XMLAT = -50.
         CALL DETID(DAY,UT,XMLAT,F10P7M(J),KPM(K),ID)
C
C        Determine the ion species descriptor
C
         SPEC = 'O+ '
         CALL DETION(SPEC,ION)
C
C        Read the EOF file
C
         CALL RDEOFF(98,PATH,ID,ION,NMLON(I,NINDX),SMLON(I,NINDX),
     &               DMLON(I,NINDX),NMLAT(I,NINDX),SMLAT(I,NINDX),
     &               DMLAT(I,NINDX),NUT(I),SUT(I),DUT(I),D(1,I),
     &               DSHFT(1,I),NALT(I,NINDX),MALT,MEOF,ALT(1,I,NINDX),
     &               EOF(1,1,I,NINDX),IOPEN)
C
C        Read the orthogonal polynomial coefficients file
C
         RECLEN=MOPM1+1
         CALL RDOPCF(99,PATH,ID,ION,UT,NMLON(I,NINDX),SMLON(I,NINDX),
     &               DMLON(I,NINDX),NMLAT(I,NINDX),SMLAT(I,NINDX),
     &               DMLAT(I,NINDX),NUT(I),SUT(I),DUT(I),NOPM1(I,NINDX),
     &               NEOF(I,NINDX),MEOF,MOPM1,MMLON,DELTA(I,NINDX),
     &               OPC(1,1,1,I,NINDX),IOPEN,RECLEN)
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END
C
      SUBROUTINE DETID(DAY,UT,MLAT,F10P7,KP,ID)
C
C  PURPOSE
C     To determine a MIDLAT-F case identifier.
C
C  METHOD
C     The MIDLAT-F case identifier is a code for describing the ambient
C     conditions (day of the year, magnetic hemisphere, magnetic activity
C     level, and solar activity level) of a MIDLAT-F case.  The case identifier
C     has the form 'aHbcd', where 'a' describes the magnetic hemisphere, 'M'
C     defines the MIDLAT-F cases as Midlatitude, 'b' describes the season, 'c'
C     describes the magnetic activity level, and 'd' describes the solar
C     actviity level.  The magnetic latitude determines 'a':  'N' for a
C     Northern hemisphere case and 'S' for a Southern hemisphere case.  The day
C     of the year determines 'b':  'W' for Winter, 'S' for Summer, and 'E' for
C     Equinox.  The magnetic Kp index determines 'c':  'L' for Low, 'M' for
C     Moderate, and 'H' for High.  The solar F10.7 index determines 'd':  'L'
C     for Low, 'M' for Moderate, and 'H' for High.
C
C  INPUT PARAMETERS
C     DAY      The day of the year
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     MLAT     The magnetic latitude, in degrees north
C     UT       Universal Time, in decimal hours
C
C  OUTPUT PARAMETERS
C     ID       The case identifier
C
C  LOCAL VARIABLES
C     AP       The magnetic Ap index
C     DEC      The solar declination, in degrees north
C     HEMIS    The hemisphere descriptor, 'N' for a northern hemisphere case,
C              'S' for a southern hemisphere case
C     MAGACT   The magnetic activity level descriptor, 'L' for low, 'M' for
C              moderate, and 'H' for high
C     SEASON   The season descriptor, 'W' for winter, 'S' for summer, and 'E'
C              for equinox
C     SOLACT   The solar activity level descriptor, 'L' for low, 'M' for
C              moderate, and 'H' for high
C     USEAP    A logical flag, .TRUE. if Ap is to be used to determine the
C              magnetic activity level descriptor, .FALSE. if Kp is to be
C              used
C     USEDEC   A logical flag, .TRUE. if the solar declination is to be used
C              to determine the season descriptor, .FALSE. if the day of the
C              year is to be used
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
C     L. Brown        28-May-1991   1.0 --> 1.1
C                                   Internal documentation improved.
C     L. Brown        13-Nov-1991   1.1 --> 1.2
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
      REAL SOLDEC,DEC,KPTOAP,AP
      CHARACTER*1 HEMIS,SEASON,MAGACT,SOLACT
      CHARACTER*6 ID
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
C  Determine the case identifier
C
      ID=HEMIS//'M'//SEASON//MAGACT//SOLACT
C
      RETURN
      END
C
      SUBROUTINE DETION(SPEC,ION)
C
C  PURPOSE
C     To determine the ion species descriptor.
C
C  METHOD
C     The ion species descriptor is a code for describing the ion.  It is
C     is determined by the ion species:  'NOP' for NO+, 'O2P' for O2+, and
C     'OP' for O+.
C
C  INPUT PARAMETERS
C     SPEC     The ion species, 'NO+', 'O2+', or 'O+'
C
C  OUTPUT PARAMETERS
C     ION      The ion species descriptor, 'NOP' for NO+, 'O2P' for O2+, and
C              'OP' for O+
C
C  LOCAL VARIABLES
C
C  SUBROUTINES REQUIRED
C     NONE
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
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Calls to UPCASE and TRIM eliminated.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      CHARACTER*3 SPEC
      CHARACTER*2 ION
C
      IF(SPEC .EQ. 'NO+') THEN
         ION='NO'
      ELSE IF(SPEC .EQ. 'O2+') THEN
         ION='O2'
      ELSE IF(SPEC .EQ. 'O+ ') THEN
         ION='O '
      ELSE
         STOP 'DETION:Invalid ion species.'
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE RDEOFF(LUN,PATH,ID,ION,NMLON,
     $            SMLON,DMLON,NMLAT,SMLAT,DMLAT,NUT,SUT,DUT,D,DSHFT,
     $                  NALT,MALT,MEOF,ALT,EOF,IOPEN)
C
C  PURPOSE
C     Reads an EOF file.
C
C  METHOD
C     The name of the EOF file is determined from the case identifier and the
C     ion species descriptor.  Header information, followed by the altitude
C     grid, eigenvalues, and EOFs are read from the EOF file.
C
C  INPUT PARAMETERS
C     ID       The case identifier
C     ION      The ion species descriptor, 'NOP' for NO+, 'O2P' for O2+, and
C              'OP' for O+
C     LUN      The logical unit number used to access the EOF file
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C
C  OUTPUT PARAMETERS
C     AP       The magnetic Ap index
C     ALT      The altitude grid, in km
C     D        Eigenvalues corresponding to the EOFs
C     DMLAT    The increment of the magnetic latitude grid, in degrees
C     DMLON    The increment of the magnetic longitude grid, in degrees
C     DSHFT    Shifted eigenvalues corresponding to the EOFs
C     DUT      The increment of the universal time grid, in decimal hours
C     EOF      Empirical orthogonal functions (EOFs)
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     NALT     The number of altitude points
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NUT      The number of universal time grid points
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     EXT      The file name extension for the ion
C     FILE     The name of the EOF file
C     FSPEC    The file specification of the EOF file
C     I        A loop counter
C     J        A loop counter
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters to lower-case in a character string
C
C  FILES ACCESSED
C     #LUN     The EOF file
C
C  AUTHOR
C     Lincoln Brown
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
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Calls to CONCAT replaced by calls to
C                                   STRCCT.
C                                   Calls to LOCASE replaced by calls to
C                                   STRLCA.
C     L. Brown        25-Sep-1995   1.0.9 ==> 1.3
C                                   String concatenations moved out of the call
C                                   to routine STRCCT for portability.
C                                   The case of the file path is preserved.
C                                   The concatenation of the path and file name
C                                   has been replaced by a call to routine
C                                   STRCCT in order to resolve an
C                                   incompatibility with the Watcom and
C                                   Microsoft FORTRAN compilers.  This problem
C                                   was reported by Jerry Ferguson and George
C                                   Bendo.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INCLUDE 'logicuni.inc'
      INTEGER LUN,NMLON,NMLAT,NUT,NALT,MALT,MEOF
      INTEGER I,J,IOPEN
      REAL D(MALT),DSHFT(MALT),ALT(MALT),EOF(MALT,MEOF)
C
C     Remove comments from this line and read of record 2 if
C     the routine will output F10p7,Kp, and Ap.
C      REAL F10P7,AP,KP
      REAL SMLON,DMLON,SMLAT,DMLAT,SUT,DUT,DUM1
      CHARACTER*2 ION
      CHARACTER*3 EXT
      CHARACTER*6 ID
      CHARACTER*(*) PATH
      CHARACTER*9 RTFILE
      CHARACTER*10 FILE
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
C
C  Determine the specification of the EOF file
C
      EXT='.'//ION
      CALL STRCCT(ID,EXT,32,RTFILE,I)
      CALL STRCCT(RTFILE,'E',32,FILE,I)
      CALL STRLCA(FILE,I)
      CALL STRCCT(PATH,FILE,32,FSPEC,I)
C
C  Open the EOF file.  Note that in the VAX Fortran implementation, the
C  record-length of unformatted direct-access files (specified by the RECL
C  keyword) is given in longwords (4 byte units), corresponding to the space
C  required for an integer*4 or a real*4.  In other implementations, the
C  record-length might be given in bytes. In a modern FORTRAN implementation
C  this OPEN statement may include a parameter for Read Only for multi-user
C  or data security purposes.
C
      IF (IOPEN .EQ. 1) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=MALT*4)
      ELSE IF (IOPEN .EQ. 2) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=MALT)
      ELSE
       WRITE(LUSTDERR,*) IOPEN
       STOP 'WRONG IOPEN IN RDEOFF'
      ENDIF
C
C  Read header information from the EOF file
C
C      Use this database without introducing the actual year and day
C      that the data was taken
C      READ(LUN,REC=1) DUM1,DUM2
C      YEAR=INT(DUM1)
C      DAY=INT(DUM2)
C      READ(LUN,REC=2) F10P7,AP,KP
C
      READ(LUN,REC=3) DUM1,SMLON,DMLON
      NMLON=INT(DUM1)
      READ(LUN,REC=4) DUM1,SMLAT,DMLAT
      NMLAT=INT(DUM1)
      READ(LUN,REC=5) DUM1,SUT,DUT
      NUT=INT(DUM1)
C
C  Read the altitude grid from the EOF file
C
      READ(LUN,REC=6) DUM1
      NALT=INT(DUM1)
      READ(LUN,REC=7) (ALT(I),I=1,NALT)
C
C  Read the eigenvalues from the EOF file
C
      READ(LUN,REC=8) (D(I),I=1,NALT)
      READ(LUN,REC=9) (DSHFT(I),I=1,NALT)
C
C  Read the EOFs from the EOF file
C
      DO 10 J=1,NALT
         READ(LUN,REC=9+J) (EOF(I,J),I=1,NALT)
   10 CONTINUE
C
C  Close the EOF file
C
      CLOSE(UNIT=LUN)
C
      RETURN
      END
C
      SUBROUTINE RDOPCF(LUN,PATH,ID,ION,UT,NMLON,SMLON,DMLON,NMLAT,
     &                  SMLAT,DMLAT,NUT,SUT,DUT,NOPM1,NEOF,MEOF,MOPM1,
     &                  MMLON,DELTA,OPC,IOPEN,RECLEN)
C
C  PURPOSE
C     Reads an orthogonal polynomial coefficients file.
C
C  METHOD
C     The name of the orthogonal polynomial coefficients file is determined
C     from the case identifier and the ion species descriptor.  Header
C     information is read from the orthogonal polynomial coefficients file.
C     The orthogonal polynomial coefficients for the given universal time are
C     read from the orthogonal polynomial coefficients file.  If the given
C     universal time does not lie exactly on the universal time grid, then
C     the orthogonal polynomial coefficients are interpolated from the
C     orthogonal polynomial coefficients of the two nearest universal times
C     on the universal time grid.
C
C  INPUT PARAMETERS
C     ID       The case identifier
C     ION      The ion species descriptor, 'NOP' for NO+, 'O2P' for O2+, and
C              'OP' for O+
C     LUN      The logical unit number used to access the orthogonal polynomial
C              coefficients file
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     RECLEN   The record length of the orthogonal polynomial coefficients
C              file, in bytes
C     UT       The universal time, in decimal hours
C
C  OUTPUT PARAMETERS
C     AP       The magnetic Ap index
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees
C     DMLON    The increment of the magnetic longitude grid, in degrees
C     DUT      The increment of the universal time grid, in decimal hours
C     F10P7    The 10.7 cm solar flux
C     KP       The magnetic Kp index
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     NUT      The number of universal time grid points
C     OPC      Orthogonal polynomial coefficients
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     SUT      The starting value of the universal time grid, in decimal hours
C
C  LOCAL VARIABLES
C     DUM1     A dummy variable used for reading
C     DUM2     A dummy variable used for reading
C     EXT      The file name extension for the ion
C     FILE     The name of the orthogonal polynomial coefficients file
C     FSPEC    The file specification of the orthogonal polynomial coefficients
C              file
C     I        A loop counter
C     IREC     A record number in the orthogonal polynomial coefficients file
C     IREC1    A record number in the orthogonal polynomial coefficients file
C     IREC2    A record number in the orthogonal polynomial coefficients file
C     IUT      The universal time index
C     J        A loop counter
C     K        A loop counter
C     NETNM    NEOF*NMLON
C     NOPM11   NOPM1+1
C     ONEMT    1.-T
C     OPC1     Orthogonal polynomial coefficients used for interpolation
C     OPC2     Orthogonal polynomial coefficients used for interpolation
C     T        An interpolation factor
C     UTR      The universal time, in decimal hours, restricted to the
C              range [>=0,<24]
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters to lower-case in a character string
C
C  FILES ACCESSED
C     #LUN     The orthogonal polynomial coefficients file
C
C  AUTHOR
C     Lincoln Brown
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
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Calls to CONCAT replaced by calls to
C                                   STRCCT.
C                                   Calls to LOCASE replaced by calls to
C                                   STRLCA.
C                                   Redundant calculation of quantities
C                                   NOPM1+1 and 1.-T has been eliminated by
C                                   storing in variables.
C                                   Calculation of IREC1 and IREC2 has been
C                                   optimized.
C                                   Unnecessary ELSE clause has been
C                                   eliminated.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   The argument list has been modified to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF) and the
C                                   low- and mid- latitude E-region
C                                   parameterized model (LME).
C                                   The structure of the orthogonal polynomial
C                                   coefficients file has changed to reflect
C                                   the elimination of Fourier fitting in
C                                   magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF) and the
C                                   low- and mid- latitude E-region
C                                   parameterized model (LME).
C     L. Brown        25-Sep-1995   1.1 ==> 1.3
C                                   String concatenations moved out of the call
C                                   to routine STRCCT for portability.
C                                   The case of the file path is preserved.
C                                   The concatenation of the path and file name
C                                   has been replaced by a call to routine
C                                   STRCCT in order to resolve an
C                                   incompatibility with the Watcom and
C                                   Microsoft FORTRAN compilers.  This problem
C                                   was reported by Jerry Ferguson and George
C                                   Bendo.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C
      INCLUDE 'logicuni.inc'
      INTEGER LUN,NOPM1,NEOF,NUT,NMLON,NMLAT
      INTEGER IUT,IREC,IREC1,IREC2,I,J,K,MEOF,MOPM1,MMLON,IOPEN,RECLEN
      INTEGER NOPM11,NETNM
      REAL ONEMT
      REAL OPC(MOPM1+1,MEOF,MMLON)
      REAL OPC1(20),OPC2(20)
C
C      Remove comment line here and at read record 2 if you want
C      the F10P7 Kp and Ap values to be returned
C      REAL F10P7,AP,KP
      REAL UT,DELTA,SUT,DUT,SMLON,DMLON,SMLAT,DMLAT
      REAL UTR,T,DUM1,DUM2
      CHARACTER*(*) PATH
      CHARACTER*2 ION
      CHARACTER*3 EXT
      CHARACTER*6 ID
      CHARACTER*9 RTFILE
      CHARACTER*10 FILE
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
C
C  Determine the specification of the orthogonal polynomial coefficients file
C
      EXT='.'//ION
      CALL STRCCT(ID,EXT,32,RTFILE,I)
      CALL STRCCT(RTFILE,'P',32,FILE,I)
      CALL STRLCA(FILE,I)
      CALL STRCCT(PATH,FILE,32,FSPEC,I)
C
C  Open the orthogonal polynomial coefficients file.  Note that in the VAX
C  Fortran implementation, the record-length of unformatted direct-access
C  files (specified by the RECL keyword) is given in longwords (4 byte units),
C  corresponding to the space required for an integer*4 or a real*4.  In other
C  implementations, the record-length might be given in bytes. In a modern FORTRAN
C  implementation this OPEN statement may include a parameter for Read Only for
C  multi-user or data security purposes.
C
C
      IF (IOPEN .EQ. 1) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=RECLEN*4)
      ELSE IF (IOPEN .EQ. 2) THEN
       OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='UNFORMATTED',
     $     ACCESS='DIRECT',RECL=RECLEN)
      ELSE
       WRITE(LUSTDERR,*) IOPEN
       STOP 'WRONG IOPEN IN RDOPCF'
      ENDIF
C
C  Read header information from the orthogonal polynomial coefficients file
C      READ(LUN,REC=1) DUM1,DUM2
C      YEAR=INT(DUM1)
C      DAY=INT(DUM2)
C      READ(LUN,REC=2) F10P7,AP,KP
C
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
C  Restrict the universal time to the range [>=0,<24]
C
      UTR=MOD(24.+MOD(UT,24.),24.)
C
C  Calculate the universal time index
C
      IUT=1+INT((UTR-SUT)/DUT)
C
C  If the universal time is exactly on the universal time grid, then the
C  orthogonal polynomial coefficients can be read directly from the orthogonal
C  polynomial coefficients file without interpolation
C
      IF(UTR .EQ. SUT+DUT*FLOAT(IUT-1)) THEN
C
C  Determine the starting record number in the orthogonal polynomial
C  coefficients file
C
         NOPM11=NOPM1+1
         IREC=NEOF*NMLON*(IUT-1)+8
C
C  Read the orthogonal polynomial coefficients for the given universal time
C
         DO 20 K=1,NMLON
            DO 10 J=1,NEOF
               READ(LUN,REC=IREC) (OPC(I,J,K),I=1,NOPM11)
               IREC=IREC+1
   10       CONTINUE
   20    CONTINUE
C
C  Otherwise, the orthogonal polynomial coefficients are linearly interpolated
C  from the orthogonal polynomial coefficients of the two nearest universal
C  times
C
      ELSE
C
C  Determine the starting record numbers in the orthogonal polynomial
C  coefficients file
C
         NOPM11=NOPM1+1
         IF((UTR .LT. SUT) .OR. (IUT .EQ. NUT)) THEN
            IREC1=NEOF*NMLON*(NUT-1)+8
            IREC2=8
         ELSE
            NETNM=NEOF*NMLON
            IREC1=NETNM*(IUT-1)+8
            IREC2=IREC1+NETNM
         ENDIF
C
C  Determine the interpolation factor
C
         IF(UTR .LT. SUT) THEN
            T=(UTR-(SUT-DUT))/DUT
         ELSE
            T=(UTR-(SUT+DUT*FLOAT(IUT-1)))/DUT
         ENDIF
         ONEMT=1.-T
C
C  Read the orthogonal polynomial coefficients for the two nearest universal
C  times and linearly interpolate the orthogonal polynomial coefficients at
C  the given universal time
C
         DO 120 K=1,NMLON
            DO 110 J=1,NEOF
               READ(LUN,REC=IREC1) (OPC1(I),I=1,NOPM11)
               IREC1=IREC1+1
               READ(LUN,REC=IREC2) (OPC2(I),I=1,NOPM11)
               IREC2=IREC2+1
               DO 100 I=1,NOPM11
                  OPC(I,J,K)=ONEMT*OPC1(I)+T*OPC2(I)
  100          CONTINUE
  110       CONTINUE
  120    CONTINUE
C
      ENDIF
C
C  Close the orthogonal polynomial coefficients file
C
      CLOSE(UNIT=LUN)
C
      RETURN
      END
C
        SUBROUTINE READUSU(IDOY,XBY,UT,USUPATH,IOPEN)
C
C       PURPOSE
C         Read the USU database
C
C       METHOD
C         Get file names and read appropriate files
C
C       INPUT PARAMETERS
C       NAME     TYPE     ARRAY     Description
C       XBY      REAL               Aximuthal magnetic field
C       IDOY     INTEGER            Day of the year
C
C       OUTPUT PARAMETERS
C       None
C
C       LOCAL VARIABLES
C       NAME     TYPE     ARRAY     Description
C       JF10P7H  INTEGER            Starting index for linear F10.7 interpolation
C       JKPH     INTEGER            Starting index for linear Kp interpolation
C
C       SUBROUTINES CALLED
C       NAME         Description
C       RDR          Reads the USU database from file given by FLL1
C       UDETID       Gets name FLL1 from f10P7 and KP
C
C       FUNCTIONS CALLED
C       None
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.9
C                                   Calculation of NCASE has been optimized.
C     L. Brown        27-Apr-1995   1.1 ==> 1.2
C                                   Added INCLUDE statement for INCLUDE file
C                                   'indirect.inc'.
C                                   Added calculation of F10.7 and Kp starting
C                                   indices and linear interpolation factors.
C                                   This allows the use of a 2 x 2 (F10.7 x Kp)
C                                   profile matrix instead of a 3 x 3 profile
C                                   matrix.
C                                   Renamed input variable BY to XBY to avoid
C                                   conflict with common block INDIRECT.
C     L. Brown        30-Sep-1996   1.2 ==> 1.5
C                                   Added argument UT to calls to UDETID.
C     L. Brown        13-Jan-1998   1.5 ==> 1.7
C                                   Extrapolation in F10.7 above 210 is now
C                                   allowed.
C
C       REFERENCES
C       None
C
C       Special constants
C       None
C
        INCLUDE 'usuarr.inc'
        INCLUDE 'indirect.inc'
        INTEGER IDOY,J,K,NCASE,IOPEN
        INTEGER JF10P7H,JKPH
        CHARACTER*7 FLL1
        CHARACTER*(*) USUPATH
        REAL XBY,UT
C
C  Calculate the F10.7 starting index and linear interpolation factors
C
      IF(F10P7 .LT. F10P7H(2)) THEN
         JF10P7H=1
      ELSE
         JF10P7H=2
      ENDIF
      TF10P7H=MAX(0.,(F10P7-F10P7H(JF10P7H))
     &              /(F10P7H(JF10P7H+1)-F10P7H(JF10P7H)))
      OMTF10P7H=1.-TF10P7H
C
C  Calculate the Kp starting index and linear interpolation factors
C
      IF(EKP .LT. KPH(2)) THEN
         JKPH=1
      ELSE
         JKPH=2
      ENDIF
      TKPH=MAX(0.,MIN(1.,(EKP-KPH(JKPH))/(KPH(JKPH+1)-KPH(JKPH))))
      OMTKPH=1.-TKPH
C
C     Determine the case identifier and read for all cases
C
        NCASE=0
        DO K = JKPH,JKPH+1
         DO J = JF10P7H,JF10P7H+1
          NCASE=NCASE+1
          CALL UDETID(IDOY,UT,-50.,F10P7H(J),KPH(K),XBY,FLL1)
          CALL RDR(USUPATH,FLL1,A(1,1,1,1,NCASE),Z(1,1,1,NCASE),
     1    CAPA(0,1,1,1,1,NCASE),UT,IOPEN)
          CALL UDETID(IDOY,UT,50.,F10P7H(J),KPH(K),XBY,FLL1)
          CALL RDR(USUPATH,FLL1,A(1,1,1,2,NCASE),Z(1,1,2,NCASE),
     1    CAPA(0,1,1,1,2,NCASE),UT,IOPEN)
         ENDDO
        ENDDO
        RETURN
        END
C
        SUBROUTINE RDR(PATH,FLL1,A,Z,CAPA,UT,IOPEN)
C
C  PURPOSE
C     To read a particular USU database
C
C  METHOD
C     Cocatenate the path and hemisphere and latitude area with the
C   root file name and then cocatenate this with the extension for each of
C   the three ions to get the full name of the database file. Then call
C   READ1 and READER to do the reading.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    FLL1    CHARACTER         The root file name
C    PATH    CHARACTER         Path to directory where USU database is stored
C    UT      REAL              Universal time
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    A       REAL (40,6,3)     Empirical Orthogonal function values.
C    CAPA    REAL (0:8,17,6,3) Coefficients to reconstruct the EOF coefficients
C    Z       REAL (40,3)       Altitude profiles for the EOF reconstruction.
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    FLNM    CHAR              FLL1 +'.OP', The O+ coefficient file
C    FLNM2   CHAR              FLL1 +'.NOP', The NO+ coefficient file
C    FLNM3   CHAR              FLL1 +'.O2P', The O2+ coefficient file
C    FLNM4   CHAR              FLL1 +'.OE', The O+ Orthog funct. file
C    FLNM5   CHAR              FLL1 +'.NOE', The NO+ Orthog funct. file
C    FLNM6   CHAR              FLL1 +'.O2E', The O2+ Orthog funct. file
C    ROOT    CHAR*7            The root of the coefficient file names
C
C  SUBROUTINES CALLED
C    NAME             description
C    GET_TIMES        Gets the UT, mag lat an from GET_CHAR output
C    OPEN_FILE        Opens the files
C    READ1            Reads the altitude values and the empirical orthogonal
C                     functions.
C    READER           Reads the coefficients of the derived orthogonal
C                     functions for the reconstruction.
C
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C     See LOCAL VARIABLES for the description of each file accessed
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Call to LOCASE replaced by call to STRLCA.
C                                   Reduced calls to STRLCA.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C
        INCLUDE 'logicuni.inc'
        REAL A(40,6,3),Z(40,3),CAPA(0:8,17,6,3)
        REAL UT,XNMLT,XNMLAT
        INTEGER NUM_HT,NOF,L1UT,LNMLT,LNMLAT,M,IOPEN,I1
        CHARACTER*15 FLNM,FLNM2,FLNM3,FLNM4, FLNM5, FLNM6
        CHARACTER*7 FLL1
        CHARACTER*7 ROOT
        CHARACTER*(*) PATH
        DATA NUM_HT/37/
C
C       Construct the file names from the root name
C
        NOF=9
        ROOT=FLL1
        CALL STRLCA(ROOT,I1)
        FLNM = ROOT//'.op'
        FLNM2 = ROOT//'.nop'
        FLNM3 = ROOT//'.o2p'
        FLNM4 = ROOT//'.oe'
        FLNM5 = ROOT//'.noe'
        FLNM6 = ROOT//'.o2e'
C       Get the Universal time, the magnetic latitude and the local time
C       for this profile
        CALL GET_TIMES(UT,XNMLT,XNMLAT,L1UT,LNMLT,LNMLAT)
C
C       Open all the files
C
        CALL OPEN_FILE(PATH,FLNM,LUCOP,NOF,IOPEN)
        CALL OPEN_FILE(PATH,FLNM2,LUCNOP,NOF,IOPEN)
        CALL OPEN_FILE(PATH,FLNM3,LUCO2P,NOF,IOPEN)
C
C       Read in the appropriate orthogonal coefficients
C
        M=1
        CALL READER (UT,CAPA,M,L1UT,NUM_HT,NOF)
        CLOSE(UNIT=LUCOP)
        CLOSE(UNIT=LUCNOP)
        CLOSE(UNIT=LUCO2P)
C
C       Open all the files
C
        CALL OPEN_FILE(PATH,FLNM4,LUFOP,NUM_HT,IOPEN)
        CALL OPEN_FILE(PATH,FLNM5,LUFNOP,NUM_HT,IOPEN)
        CALL OPEN_FILE(PATH,FLNM6,LUFO2P,NUM_HT,IOPEN)
C
C       Read the altitude values and the empirical orthogonal functions
C
        CALL READ1(Z,A,NUM_HT,NOF)
        CLOSE(UNIT=LUFOP)
        CLOSE(UNIT=LUFNOP)
        CLOSE(UNIT=LUFO2P)
        RETURN
        END
C
      SUBROUTINE READ1(Z,A,N,NOF)
C
C  PURPOSE
C    To read in the altitude values and the empirical orthogonal functions
C    for the reconstruction.
C
C  METHOD
C    No discussion
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    N       INTEGER           The record length ( also = NUM_HT)
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    A       REAL     (40,6,3) The empirical orthogonal polynomials
C    Z       REAL     (40,3)   The altitudes associated with the array indices
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    I       INTEGER           Do loop index
C    J       INTEGER           Do loop index
C    JP6     INTEGER           J+6
C
C  SUBROUTINES CALLED
C    NONE
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C     FLNM
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Redundant calculation of quantity 6+J has
C                                   been eliminated by storing in variable.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of declaration and dimension
      INCLUDE 'logicuni.inc'
      INTEGER I,J,N,NOF
      INTEGER JP6
      REAL Z(40,3),A(40,6,3)
C
C     End of declaration and dimension statements, beginning of executable
C     code
C     Read the altitude values
      READ(LUFOP,REC=4) (Z(I,1),I=1,N)
      READ(LUFNOP,REC=4) (Z(I,2),I=1,N)
      READ(LUFO2P,REC=4) (Z(I,3),I=1,N)
C
C  Read the eigenvectors from the EOFs file
C
      DO J=1,NOF
       JP6=J+6
       READ(LUFOP,REC=JP6) (A(I,J,1),I=1,N)
       READ(LUFNOP,REC=JP6) (A(I,J,2),I=1,N)
       READ(LUFO2P,REC=JP6) (A(I,J,3),I=1,N)
      END DO
C
      RETURN
      END
C
      SUBROUTINE READER(UT,CAPA,M,LUT,N1,NOF)
C  PURPOSE
C   To read in the coefficients of the deived orthogonal polynomials
C   for the reconstruction of the altitude profiles.
C  METHOD
C    Use the input array indices (LUT) to determine the
C    correct record to read.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    LUT     INTEGER           Index for the universal time
C    M       INTEGER           Marker to determine if the header information
C                              has been read already
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    AP      REAL
C    CAPA    REAL (0:8,17,6,3) The coefficients for the derived orthogonal
C                              polynomials to determine the Fourier coefficients
C    DELTA   REAL              Step size for the derived orthogonal polynomials
C    DMLAT   REAL              Step size for the magnetic latitude
C    DMLT    REAL              Step size for the local time
C    DUT     REAL              Step size for the Universal time
C    F10P7   REAL              The 10.7 parameter
C    IDAY    INTEGER           The day of the year the data was taken
C    IYEAR   INTEGER           The year the data was taken
C    NEOF    INTEGER           The number of empirical orthogonal functions used
C    NMAX    INTEGER           The number of derived orthogonal functions used
C    NMLAT   INTEGER           The number of magnetic latitudes
C    NMLT    INTEGER           The number of local times
C    NUM_EFC INTEGER           The number of Fourier coefficients used in E
C                              layer calculations
C    NUM_FFC INTEGER           The number of Fourier coefficients used in E
C                              layer calculations
C    NUM_HT  INTEGER           The number of altitude points
C    NUT     INTEGER           The number of universal times
C    SMLAT   REAL              Smallest mag lat
C    SMLT    REAL              Smallest local time
C    SUT     REAL              Smallest universal time
C    XKP     REAL              The KP value
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    I       INTEGER           Array index
C    J       INTEGER           Do loop index
C    JM1     INTEGER           J-1
C    K       INTEGER           Do loop index
C    L       INTEGER           record index
C    NETNM1  INTEGER           NEOF*(NMAX+1)
C    NMAXP1  INTEGER           NMAX+1
C    UTLUT   REAL              FLOAT(2*LUT-1)
C    XIDAY   REAL              Real value read in corresponding to IDAY
C    XIYEAR  REAL              Real value read in corresponding to IYEAR
C    XNEOF   REAL              Real value read in corresponding to NEOF
C    XNMAX   REAL              Real value read in corresponding to NMAX
C    XNMLAT  REAL              Real value read in corresponding to NMLAT
C    XNMLT   REAL              Real value read in corresponding to NMLT
C    XNUM_FC REAL              Real value read in corresponding to NUM_FC
C    XNUT    REAL              Real value read in corresponding to NUT
C
C  SUBROUTINES REQUIRED
C    NONE
C  FILES ACCESSED
C    FLNM
C  AUTHOR
C     William Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Redundant calculation of quantities NMAX+1,
C                                   NEOF*(NMAX+1), FLOAT(2*LUT-1), and J-1 has
C                                   been eliminated by storing in variables.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C Start of dimension and declaration statements
C
      INCLUDE 'io.inc'
      INCLUDE 'logicuni.inc'
      INTEGER N1,NOF,I,J,K,L,LUT,L1,L2,M
      INTEGER NMAXP1,NETNM1,JM1
      REAL CAPA(0:8,17,6,3),C1(17),C2(17)
      REAL XIDAY,XIYEAR,XNUM_FFC,XNUM_EFC,XNMAX,XNEOF,UT,X1,X2
      REAL XNUT,XNMLAT,XNMLT
      REAL UTLUT
C
C     End of dimension and declaration statements
C     Beginning of executable code.
      NUM_HT = N1
      IF ( M .EQ. 1) THEN
C  Read environment information from the unformatted fit parameters database
C
       READ(LUCNOP,REC=1) XIYEAR,XIDAY,DELTA
       XIDAY = XIDAY
       READ(LUCOP,REC=2) F10P7,AP,XNUM_FFC,XNMAX
       READ(LUCNOP,REC=2) F10P7,AP,XNUM_EFC,XNMAX
       READ(LUCNOP,REC=3) XKP,XNEOF
C
C  Read grid information from the unformatted fit parameters database
C
       READ(LUCNOP,REC=4) XNUT,SUT,DUT
       READ(LUCNOP,REC=5) XNMLAT,SMLAT,DMLAT
       READ(LUCNOP,REC=6) XNMLT,SMLT,DMLT
       IYEAR = IFIX(XIYEAR)
       NUT = IFIX(XNUT)
       NUM_EFC = IFIX(XNUM_EFC)
       NUM_FFC = IFIX(XNUM_FFC)
       NMLAT = IFIX(XNMLAT)
       NMLT = IFIX(XNMLT)
       NEOF = IFIX(XNEOF)
       NOF = NEOF
       NMAX = IFIX(XNMAX)
      END IF
C     Determine the correct record to read
      NMAXP1=NMAX+1
      NETNM1=NEOF*NMAXP1
      UTLUT=FLOAT(2*LUT-1)
      IF(UT .EQ. UTLUT) THEN
       L=NETNM1*(LUT-1)+7
       DO K =1,NEOF
        DO J=1,NMAXP1
         JM1=J-1
         READ(LUCOP,REC=L) (CAPA(JM1,I,K,1),I=1,NUM_FFC)
         READ(LUCNOP,REC=L) (CAPA(JM1,I,K,2),I=1,NUM_EFC)
         READ(LUCO2P,REC=L) (CAPA(JM1,I,K,3),I=1,NUM_EFC)
         L = L+1
        END DO
       END DO
      ELSE IF(UT .LT. UTLUT) THEN
C      The index is high
       IF ( LUT .GT. 1) THEN
        L1=NETNM1*(LUT-2)+7
       ELSE
C       as there are 12 UT data points
        L1=NETNM1*11+7
       ENDIF
       X2 = (UTLUT-UT)/2.
       X1 = 1.-X2
       L2=NETNM1*(LUT-1)+7
       DO K =1,NEOF
        DO J=1,NMAXP1
         JM1=J-1
         READ(LUCOP,REC=L1) (C1(I),I = 1,NUM_FFC)
         READ(LUCOP,REC=L2) (C2(I),I = 1,NUM_FFC)
         DO I = 1, NUM_FFC
          CAPA(JM1,I,K,1)=X1*C1(I)+X2*C2(I)
         ENDDO
         READ(LUCNOP,REC=L1) (C1(I),I = 1,NUM_EFC)
         READ(LUCNOP,REC=L2) (C2(I),I = 1,NUM_EFC)
         DO I = 1, NUM_EFC
          CAPA(JM1,I,K,2)=X1*C1(I)+X2*C2(I)
         ENDDO
         READ(LUCO2P,REC=L1) (C1(I),I = 1,NUM_EFC)
         READ(LUCO2P,REC=L2) (C2(I),I = 1,NUM_EFC)
         DO I = 1, NUM_EFC
          CAPA(JM1,I,K,3)=X1*C1(I)+X2*C2(I)
         ENDDO
         L1 = L1+1
         L2 = L2+1
        END DO
       END DO
      ELSE
C      The index is low
       X2=(UT-UTLUT)/2.
       X1=1.-X2
       L1=NETNM1*(LUT-1)+7
       IF ( LUT .LT. 12) THEN
        L2=L1+NETNM1
       ELSE
        L2 = 7
       ENDIF
       DO K =1,NEOF
        DO J=1,NMAXP1
         JM1=J-1
         READ(LUCOP,REC=L1) (C1(I),I = 1,NUM_FFC)
         READ(LUCOP,REC=L2) (C2(I),I = 1,NUM_FFC)
         DO I = 1, NUM_FFC
          CAPA(JM1,I,K,1)=X1*C1(I)+X2*C2(I)
         ENDDO
         READ(LUCNOP,REC=L1) (C1(I),I = 1,NUM_EFC)
         READ(LUCNOP,REC=L2) (C2(I),I = 1,NUM_EFC)
         DO I = 1, NUM_EFC
          CAPA(JM1,I,K,2)=X1*C1(I)+X2*C2(I)
         ENDDO
         READ(LUCO2P,REC=L1) (C1(I),I = 1,NUM_EFC)
         READ(LUCO2P,REC=L2) (C2(I),I = 1,NUM_EFC)
         DO I = 1, NUM_EFC
          CAPA(JM1,I,K,3)=X1*C1(I)+X2*C2(I)
         ENDDO
         L1 = L1+1
         L2 = L2+1
        END DO
       END DO
      ENDIF
      RETURN
      END
      SUBROUTINE READAWS(PATH,IMO,IOPEN)
C
C     PURPOSE
C       To read the URSI coefficient database
C
C     METHOD
C       Open the file and read
C
C     INPUT PARAMETERS
C     NAME  TYPE  ARRAY  Dexcription
C     IMO   INTEGER      Month of the year
C     IOPEN INTEGER      1 for byte record lengths, 2 for longword
C                        record lengths
C     PATH  CHARACTER    Path to the URSI data sets
C
C     OUTPUT PARAMETERS
C     All in common block GWC1
C
C     LOCAL VARIABLES
C        FSPEC    The file specification of the URSI coefficient database
C
C     SUBROUTINES CALLED
C        STRCCT   Concatenates two character strings
C
C     FUNCTIONS CALLED
C     None
C
C     AUTHOR
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
C     L. Brown        25-Sep-1995   1.3
C                                   The concatenation of the path and file name
C                                   has been replaced by a call to routine
C                                   STRCCT in order to resolve an
C                                   incompatibility with the Watcom and
C                                   Microsoft FORTRAN compilers.  This problem
C                                   was reported by Jerry Ferguson and George
C                                   Bendo.
C     SPECIAL CONSTANTS
C     None
C
C     REFERENCES
C     None
C
      INCLUDE 'logicuni.inc'
      INCLUDE 'gwc1.inc'
      INTEGER I,IMO,J,JM,JN,L,LS(4),LK(4),M,IREC,IOPEN
      REAL F2COEF(4)
      CHARACTER*(*) PATH
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
C
      CALL STRCCT(PATH,'ursi88da.dat',32,FSPEC,I)
      IF (IOPEN .EQ. 1) THEN
       OPEN(UNIT=14,FILE=FSPEC,STATUS='OLD',ACCESS='DIRECT',RECL=48)
      ELSE IF (IOPEN .EQ. 2) THEN
       OPEN(UNIT=14,FILE=FSPEC,STATUS='OLD',ACCESS='DIRECT',RECL=12)
      ELSE
       WRITE(LUSTDERR,*) IOPEN
       STOP 'WRONG IOPEN IN READAWS'
      ENDIF
C.......................................................................
C  POSITION URSI DATASET TO DESIRED MONTH                              .
C.......................................................................
      IREC = (IMO-1)*496 + 1
C......................................................................
C  READ IN URSI FOF2 COEFFICIENTS FOR SUNSPOT NUMBER = 0              .
C......................................................................
      READ(14,REC=IREC,ERR=990) (KF(I),I=1,10)
      IF (KF(1) .NE. 11) GO TO 990
      DO 300 M=1,988,4
         IREC = IREC + 1
         READ(14,REC=IREC,ERR=990) (LS(I),LK(I),F2COEF(I),I=1,4)
         DO 200 I=1,4
            JN=LS(I)+1
            JM=LK(I)+1
            UF0(JN,JM)=F2COEF(I)
  200    CONTINUE
  300 CONTINUE
C......................................................................
C  READ IN URSI FOF2 COEFFICIENTS FOR SUNSPOT NUMBER = 100            .
C......................................................................
      IREC = IREC + 1
c     PRINT *,'SSN = 100: IREC = ',IREC
      READ(14,REC=IREC,ERR=990) (KF(I),I=1,10)
      IF(KF(1) .NE. 11) GOTO 990
      DO 500 M=1,988,4
         IREC = IREC + 1
         READ(14,REC=IREC,ERR=990) (LS(I),LK(I),F2COEF(I),I=1,4)
         DO 400 I=1,4
            JN=LS(I)+1
            JM=LK(I)+1
            UF100(JN,JM)=F2COEF(I)
  400    CONTINUE
  500 CONTINUE
C.......................................................................
C  SKIP OVER REMAINING FOF2 COEFFICIENTS                               .
C.......................................................................
      IREC = IREC + (12-IMO)*496
C......................................................................
C     POSITION TO CORRECT MONTH OF M(3000) COEFFICIENTS               .
C......................................................................
      IREC = IREC + (IMO-1)*99 + 1
C.......................................................................
C  READ IN URSI M(3000) COEFFICIENTS                                   .
C.......................................................................
      READ(14,REC=IREC,ERR=990) (KX(I),I=1,10)
      IF(KX(1) .NE. 6) GOTO 990
      DO 600 L=1,2
        DO 650 J=1,49
          IREC = IREC + 1
          READ(14,REC=IREC,ERR=990) (FM3(I,J,L),I=1,9)
  650   CONTINUE
  600 CONTINUE
      CLOSE(UNIT=14)
      RETURN
  990 CONTINUE
      WRITE(LUSTDERR,*) 'URSIDA:  IREC = ',IREC
      WRITE(LUSTDERR,*) 'KF: ',KF
      STOP 'URSI:Bad read from URSI coefficient database.'
      END
C
