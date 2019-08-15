      FUNCTION SOLDEC(DAY)
C
C  PURPOSE
C     To calculate the solar declination.
C
C  METHOD
C     The expression for the solar declination was derived using spherical
C     trigonometry.
C
C  INPUT PARAMETERS
C     DAY      The day of the year
C
C  OUTPUT PARAMETERS
C     SOLDEC   The solar declination, in degrees north
C
C  LOCAL VARIABLES
C     PI       Pi
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
C     1.3   12-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        10-Jan-1989   1.0 ==> Created
C     L. Brown        12-Nov-1991   1.0 ==> 1.1
C                                   Pi determined by ACOS(-1.) instead of
C                                   being hardwired.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Pi determined by 4*ATAN(1) instead of
C                                   ACOS(-1).
C                                   Improved internal documentation.
C     L. Brown        12-Mar-1993   1.2 ==> 1.3
C                                   Improved internal documentation.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     DAYVE    The day number of the vernal equinox
C     SIDYR    The number of days in the sidereal year
C     SOLINC   The solar inclination, in degrees
C
      REAL DAYVE,SIDYR,SOLINC
      PARAMETER(DAYVE=80.,SIDYR=365.4,SOLINC=23.5)
C
C  Input variables
C
      REAL DAY
C
C  Output variables
C
      REAL SOLDEC
C
C  Local variables
C
      REAL PI
C
C  Calculate pi
C
      PI=4.*ATAN(1.)
C
C  Calculate the solar declination
C
      SOLDEC=ASIN(SIN(2.*PI*(DAY-DAYVE)/SIDYR)*SIN(SOLINC*PI/180.))
     &      *180./PI
C
      RETURN
      END
      FUNCTION KPTOAP(KP)
C
C  PURPOSE
C     Calculates the quasi-linear magnetic activity index Ap from the
C     quasi-logarithmic index Kp using Table 6 from Mayaud (1980).
C
C  METHOD
C     Table lookup in which the subscript is just Kp*3, followed by linear
C     interpolation (to allow for averaged Kp's).
C
C  INPUT PARAMETERS
C     KP       The magnetic activity index Kp
C
C  OUTPUT PARAMETERS
C     KPTOAP   The magnetic activity index Ap
C
C  LOCAL VARIABLES
C    IKP3      KP3 truncated to an integer
C    KAP       Ap corresponding to Kp*3
C    KP3       KP*3
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
C     Robert E. Daniell, Jr.
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   12-March-1993
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     R. Daniell      22-Jun-1990   1.0 ==> Created
C     L. Brown        12-Nov-1991   1.0 ==> 1.1
C                                   Cleaned up code.
C     L. Brown         3-Mar-1993   1.1 ==> 1.2
C                                   Improved internal documentation.
C     L. Brown        12-Mar-1993   1.2 ==> 1.3
C                                   Definition of lookup table changed from
C                                   data statement to assignment statements
C                                   because local storage is not necessarily
C                                   retained between calls.
C                                   Improved internal documentation.
C
C  REFERENCES
C      Mayaud, P. N., Derivation, 'Meaning, and Use of Geomagnetic Indices',
C      Geophysical Monograph 22, American Geophysical Union, Washington, D.C.,
C      1980.
C
C  SPECIAL CONSTANTS
C     None
C
C  Input variables
C
      REAL KP
C
C  Output variables
C
      REAL KPTOAP
C
C  Local variables
C
      INTEGER IKP3
      REAL KP3
      REAL KAP(0:27)
C
C  Define the lookup table
C
      KAP( 0)=  0.
      KAP( 1)=  2.
      KAP( 2)=  3.
      KAP( 3)=  4.
      KAP( 4)=  5.
      KAP( 5)=  6.
      KAP( 6)=  7.
      KAP( 7)=  9.
      KAP( 8)= 12.
      KAP( 9)= 15.
      KAP(10)= 18.
      KAP(11)= 22.
      KAP(12)= 27.
      KAP(13)= 32.
      KAP(14)= 39.
      KAP(15)= 48.
      KAP(16)= 56.
      KAP(17)= 67.
      KAP(18)= 80.
      KAP(19)= 94.
      KAP(20)=111.
      KAP(21)=132.
      KAP(22)=154.
      KAP(23)=179.
      KAP(24)=207.
      KAP(25)=236.
      KAP(26)=300.
      KAP(27)=400.
C
C  Limit Kp to the range [0,9]
C
      IF(KP .LT. 0.) THEN
         KP3=0.
      ELSE IF(KP .GT. 9.) THEN
         KP3=27.
      ELSE
         KP3=KP*3.
      ENDIF
C
C  Determine the index of the lookup table
C
      IKP3=MIN(INT(KP3),26)
C
C  Linearly interpolate the Ap index from the lookup table
C
      KPTOAP=KAP(IKP3)+(KAP(IKP3+1)-KAP(IKP3))*(KP3-FLOAT(IKP3))
C
      RETURN
      END
