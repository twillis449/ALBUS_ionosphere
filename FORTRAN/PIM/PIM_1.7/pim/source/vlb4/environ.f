      FUNCTION SOLANG(DAY,UT,GLAT,GLON)
C
C  PURPOSE
C     To calculate the great-circle angular separation between an observer
C     and the subsolar point given the day of the year, Universal Time, and
C     geographic location of the observer.
C
C  METHOD
C     Using the law of cosines for oblique spherical triangles, an expression
C     can be derived for the great-circle angular separation between the
C     observer and the subsolar point (the angle between the observer and the
C     subsolar point subtended by the center of the Earth):
C
C        cos(A) = sin(GLAT)*sin(GLATSS)+cos(GLAT)*cos(GLATSS)*cos(GLONSS-GLON)
C
C     where GLAT and GLON are the geographic coordinates of the observer,
C     GLATSS and GLONSS are the geographic coordinates of the subsolar point,
C     and A is the great-circle angular separation between the observer and the
C     subsolar point.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        The day of the year
C     GLAT   Real       Scalar        Degrees north   -90. <= GLAT <= 90.
C        The geographic latitude of the observer
C     GLON   Real       Scalar        Degrees east    0. <= GLON < 360.
C        The geographic longitude of the observer
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        The Universal Time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     SOLANG Real       Scalar        Degrees         0. <= SOLANG <= 180.
C        The great-circle angular separation between the observer and the
C        subsolar point
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     GLATSS Real       Scalar        Degrees north   -90. <= GLATSS <= 90.
C        The geographic latitude of the subsolar point
C     GLONSS Real       Scalar        Degrees east    0. <= GLONSS < 360.
C        The geographic longitude of the subsolar point
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     SOLSUB Calculates the subsolar point
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
C     1.1   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       14-Jun-1993  1.0 ==> Created
C     L. Brown       29-Mar-1994  1.0 ==> 1.1
C                                 The ampersand (&) is now used as the
C                                 continuation character.
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
      INTEGER DAY
      REAL UT,GLAT,GLON
C
C  Output variables
C
      REAL SOLANG
C
C  Local variables
C
      REAL GLATSS,GLONSS
C
C  Calculate the conversion factor from degrees to radians
C
      INCLUDE 'const.inc'
C      DTOR=ATAN(1.)/45.
C
C  Calculate the subsolar point
C
      CALL SOLSUB(DAY,UT,GLATSS,GLONSS)
C
C  Calculate the great-circle angular distance between the observer and the
C  subsolar point
C
      SOLANG=ACOS(SIN(GLAT*DTOR)*SIN(GLATSS*DTOR)
     &           +COS(GLAT*DTOR)*COS(GLATSS*DTOR)
     &            *COS((GLONSS-GLON)*DTOR))
     &      *RTOD
C
      RETURN
      END
      FUNCTION SOLDEC(DAY,UT)
C
C  PURPOSE
C     To approximate the solar declination.
C
C  METHOD
C     This approximation for the solar declination was derived using spherical
C     trigonometry.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        The day of the year
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        The Universal Time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     SOLDEC Real       Scalar        Degrees         -SOLINC<= SOLDEC <=SOLINC
C        The solar declination
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     PI     Real       Scalar        n/a             pi
C        Pi
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
C     1.8   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       10-Jan-1989  1.0 ==> Created
C     L. Brown       12-Nov-1991  1.0 ==> 1.1
C                                 Pi determined by ACOS(-1.) instead of
C                                 being hardwired.
C     L. Brown        3-Mar-1993  1.1 ==> 1.2
C                                 Pi determined by 4*ATAN(1) instead of
C                                 ACOS(-1).
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.2 ==> 1.3
C                                 Improved internal documentation.
C     L. Brown        8-Apr-1993  1.3 ==> 1.4
C                                 The conversion factor DTOR from degrees to
C                                 radians is calculated and used in the
C                                 expression for the solar declination rather
C                                 than using the ratio pi/180 in the formula.
C     L. Brown       14-Apr-1993  1.4 ==> 1.5
C                                 Input parameter DAY and constant DAYVE are
C                                 integers.  Universal Time is considered.
C                                 Improved internal documentation.
C     L. Brown        7-May-1993  1.5 ==> 1.6
C                                 Changed 'UT/3600.-.5' to '(UT-12.)/24.'
C                                 since UT is given in decimal hours, not
C                                 seconds.
C                                 Improved internal documentation.
C     L. Brown        8-Jun-1993  1.6 ==> 1.7
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.7 ==> 1.8
C                                 The ampersand (&) is now used as the
C                                 continuation character.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     DAYVE  Integer    Days            The day number of the vernal equinox
C     SIDYR  Real       Decimal days    The number of days in the sidereal year
C     SOLINC Real       Degrees         The solar inclination
C
      INTEGER DAYVE
      REAL SIDYR,SOLINC
      PARAMETER(DAYVE=80,SIDYR=365.4,SOLINC=23.5)
C
C  Input variables
C
      INTEGER DAY
      REAL UT
C
C  Output variables
C
      REAL SOLDEC
C
C  Local variables
C
      INCLUDE 'const.inc'
C      REAL PI,DTOR
C
C  Calculate pi
C
C      PI=4.*ATAN(1.)
C
C  Calculate the conversion factor from degrees to radians
C
C      DTOR=PI/180.
C
C  Calculate the solar declination
C
      SOLDEC=ASIN(SIN(2.*PI*(FLOAT(DAY-DAYVE)+(UT-12.)/24.)/SIDYR)
     &           *SIN(SOLINC*DTOR))*RTOD
C
      RETURN
      END
      FUNCTION SOLLDY(DAY,UT,GLAT,ALT)
C
C  PURPOSE
C     Determines the length of the day at an observer given the day of the
C     year, Universal Time, geographic latitude of the observer, and altitude
C     of the observer.
C
C  METHOD
C     The solar declination can be determined from the day of the year and the
C     Universal Time.  The solar declination is identical to the geographic
C     latitude of the subsolar point.
C
C     Using the trigonometry of right plane triangles, an expression can be
C     derived for the great-circle angle between the observer and the Sun
C     (subsolar point) such that the line-of-sight between the observer and the
C     Sun is tangent to the surface of the Earth:
C
C        A = acos(Re/Ro) + acos(Re/Rs)
C
C     where Re is the radius of the Earth, Ro is the radius of the observer, Rs
C     is the radius of the Sun, and A is the great-circle angle between the
C     observer and the Sun (subsolar point) such that the line-of-sight between
C     the observer and the Sun is tangent to the surface of the Earth.
C
C     With the solar declination and the angle A determined, an expression for
C     the difference in geographic longitude between the Sun (subsolar point)
C     and the observer can be derived using the law of cosines of sides for
C     oblique spherical triangles:
C
C                     cos(A)-sin(DEC)*sin(GLAT)
C        cos(DGLON) = -------------------------
C                        cos(DEC)*cos(GLAT)
C
C     where DEC is the solar declination (i.e., the geographic latitude of the
C     subsolar point), GLAT is the geographic latitude of the observer, and
C     DGLON is the difference in geographic longitude between the Sun (subsolar
C     point) and the observer.
C
C     The right-hand side of the expression for cos(DGLON) above can have
C     absolute values greater > 1, signifying special cases:  for values <= -1,
C     the observer is always in daylight, so the length of the day is 24 hours;
C     for values >= 1, the observer is never in daylight, so the length of the
C     day is 0 hours.
C
C     For absolute values of the right-hand side <= 1, DGLON can be calculated
C     by taking the arccosine of the right-hand side.  With DGLON determined,
C     the length of the day is calculated as twice DGLON converted into hours.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALT    Real       Scalar        km              >= -Re
C        Altitude of the observer
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        Day of the year
C     GLAT   Real       Scalar        Degrees north   -90. <= GLAT <= 90.
C        Geographic latitude of the observer
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        Universal Time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     SOLLDY Real       Scalar        Hours           0. <= SOLLDY <= 24.
C        The length of the day
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     A      Real       Scalar        Radians         0. <= A <= pi
C        The great-circle angle between the observer and the Sun (subsolar
C        point) such that the line-of-sight between the observer and the Sun is
C        tangent to the surface of the Earth
C     CDCG   Real       Scalar        n/a             0. <= CDCG <= 1.
C        The product of the cosine of the solar declination and the cosine of
C        the geographic latitude of the observer
C     DEC    Real       Scalar        Radians         -90. <= DEC <= 90.
C        The solar declination
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     GLATR  Real       Scalar        Radians         -pi/2. <= GLATR <= pi/2.
C        Geographic latitude of the observer
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     SOLDEC Real       Determines the solar declination
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
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
C     1.0   25-September-1996
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       25-Sep-1996  1.0 ==> Created
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     AU     Real       km              An Astronomical Unit
C     RE     Real       km              The equatorial radius of the Earth
C
      REAL AU,RE
      PARAMETER(AU=1.4959787E8,RE=6378.164)
C
C  Input variables
C
      INTEGER DAY
      REAL UT,GLAT,ALT
C
C  Output variables
C
      REAL SOLLDY
C
C  Local variables
C
      REAL DEC,A,GLATR,CDCG
C
C  Function declarations
C
      REAL SOLDEC
C
C  Calculate the conversion factor from degrees to radians
C
      INCLUDE 'const.inc'
C      DTOR=ATAN(1.)/45.
C
C  Calculate the solar declination
C
      DEC=SOLDEC(DAY,UT)*DTOR
C
C  Calculate the great-circle angle between the observer and the Sun (subsolar
C  point) such that the line-of-sight between the observer and the Sun is
C  tangent to the surface of the Earth
C
      A=ACOS(RE/(RE+ALT))+ACOS(RE/AU)
C
C  Convert the geographic latitude of the observer to radians
C
      GLATR=GLAT*DTOR
C
C  Calculate the product of the cosine of the solar declination and the cosine
C  of the geographic latitude of the observer
C
      CDCG=COS(DEC)*COS(GLATR)
C
C  The observer is at or very near a geographic pole
C
      IF(CDCG .EQ. 0.) THEN
C
C  The observer is always in sunlight
C
         IF(ABS(GLATR-DEC) .LE. A) THEN
            SOLLDY=24.
C
C  The observer is never in sunlight
C
         ELSE
            SOLLDY=0.
         ENDIF
C
C  If the observer is not at or very near a geographic pole, then calculate the
C  length of the day at the observer as twice the difference in geographic
C  longitude between the Sun (subsolar point) and the observer
C
      ELSE
         SOLLDY=ACOS(MAX(-1.,MIN(1.,(COS(A)-SIN(DEC)*SIN(GLATR))/CDCG)))
     &         *2./DTOR/15.
C
      ENDIF
C
      RETURN
      END
      FUNCTION SOLSLT(UT,GLON)
C
C  PURPOSE
C     To calculate the solar local time at an observer.
C
C  METHOD
C     The solar local time at an observer is related to Universal Time (solar
C     local time at Greenwich) by the following formula:
C
C        SLT = UT + GLON * (24 hours / 360 degrees = 1 hour / 15 degrees)
C
C     where SLT is the solar local time at the observer, UT is Universal Time,
C     and GLON is the geographic longitude of the observer.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     GLON   Real       Scalar        Degrees east    0. <= GLON < 360.
C        The geographic longitude of the observer
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        Universal Time (solar local time at Greenwich)
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     SOLSLT Real       Scalar        Hours           0. <= SOLSLT < 24.
C        The solar local time at the observer
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
C     1.0   14-June-1993
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       14-Jun-1993  1.0 ==> Created
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
      REAL UT,GLON
C
C  Output variables
C
      REAL SOLSLT
C
C  Calculate the solar local time at the observer
C
      SOLSLT=MOD(24.+MOD(UT+GLON/15.,24.),24.)
C
      RETURN
      END
      SUBROUTINE SOLSUB(DAY,UT,GLATSS,GLONSS)
C
C  PURPOSE
C     To determine the geographic coordinates of the subsolar point.
C
C  METHOD
C     The solar declination defines the latitude of the subsolar point.  The
C     meridian of solar local noon defines the longitude of the subsolar point.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        The day of the year
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        The Universal Time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     GLATSS Real       Scalar        Degrees north   -90 <= GLATSS <= 90.
C        The geographic latitude of the subsolar point, in degrees north
C     GLONSS Real       Scalar        Degrees east    0. <= GLONSS < 360.
C        The geographic longitude of the subsolar point, in degrees east
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
C     SOLDEC Real       Determines the solar declination
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
C     1.4   8-June-1993
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       10-Jan-1989  1.0 ==> Created
C     L. Brown        8-Apr-1993  1.0 ==> 1.1
C                                 Changed expression for subsolar longitude
C                                 to insure that it falls with the range
C                                 [0,360].
C                                 Improved internal documentation.
C     L. Brown       14-Apr-1993  1.1 ==> 1.2
C                                 Input parameter DAY is an integer.
C                                 Improved internal documentation.
C     L. Brown        7-May-1993  1.2 ==> 1.3
C                                 Improved internal documentation.
C     L. Brown        8-Jun-1993  1.3 ==> 1.4
C                                 Improved internal documentation.
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
      INTEGER DAY
      REAL UT
C
C  Output variables
C
      REAL GLATSS,GLONSS
C
C  Local variables
C
      REAL SOLDEC
C
C  Calculate the geographic latitude of the subsolar point
C
      GLATSS=SOLDEC(DAY,UT)
C
C  Calculate the geographic longitude of the subsolar point
C
      GLONSS=MOD(360.+MOD((12.-UT)*15.,360.),360.)
C
      RETURN
      END
      FUNCTION SOLSZA(DAY,UT,GLAT,GLON,ALT)
C
C  PURPOSE
C     To calculate the solar zenith angle at an observer.
C
C  METHOD
C     Using the law of sines for plane triangles, an exact expression for the
C     solar zenith angle at an observer can be derived:
C
C                           cos(A)-r                  Re+Z
C        cos(X) = ----------------------------- , r = ---- >= 0
C                 sqrt(sin(A)**2+(cos(A)-r)**2)       Re+H
C
C     where X is the solar zenith angle at the observer, A is the great-circle
C     angular separation between the observer and the subsolar point, Re is the
C     radius of the Earth, Z is the altitude of the observer, and H is the
C     altitude of the Sun.
C
C     The special case sin(A)**2+(cos(A)-r)**2)=0 is possible when A = 0
C     degrees and r = 1, implying that X = 0 degrees.
C
C     For points near the Earth (Z << H), the term r = (Re+Z)/(Re+H) in the
C     expression for the solar zenith angle X is very close to zero and can be
C     ignored, simplifying the formula to
C
C        cos(X) = cos(A)
C
C     Therefore, to a good approximation, the solar zenith angle X for points
C     near the Earth is equal to the angle A.  For cases where Z is appreciable
C     compared to H, the term r = (Re+Z)/(Re+H) cannot be ignored.  In this
C     routine, the exact expression for the solar zenith angle is used
C     regardless of the proximity of the observer to the Earth.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALT    Real       Scalar        km              >= -Re
C        The altitude of the observer
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        The day of the year
C     GLAT   Real       Scalar        Degrees north   -90. <= GLAT <= 90.
C        The geographic latitude of the observer
C     GLON   Real       Scalar        Degrees east    0. <= GLON < 360.
C        The geographic longitude of the observer
C     UT     Real       Scalar        Hours           0. <= UT < 24.
C        The Universal Time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     SOLSZA Real       Scalar        Degrees         0. <= SOLSZA <= 180.
C        The solar zenith angle at the observer
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     A      Real       Scalar        n/a             0. <= A <= 180.
C        The great-circle angular separation between the observer and the
C        subsolar point
C     ARG1   Real       Scalar        n/a             <= 1.
C        The numerator of the argument to the ACOS intrinsic function
C     ARG2   Real       Scalar        n/a             >= 0.
C        The square of the denominator of the argument to the ACOS intrinsic
C        function
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     R      Real       Scalar        n/a             R >= 0.
C        The term (Re+Z)/(Re+H)
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     SOLANG Real       Calculates the great-circle angular separation between
C                       observer and the subsolar point
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
C     1.11   26-September-1996
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        9-Jan-1989  1.0 ==> Created
C     L. Brown        8-Apr-1993  1.0 ==> 1.1
C                                 The exact formula for the solar zenith angle
C                                 is used, requiring the altitude of the
C                                 observer.
C                                 The conversion factor DTOR is calculated
C                                 instead of being defined as a constant.
C                                 Improved internal documentation.
C     L. Brown       12-Apr-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       13-Apr-1993  1.2 ==> 1.3
C                                 Improved internal documentation.
C     L. Brown       14-Apr-1993  1.3 ==> 1.4
C                                 Input parameter DAY is an integer.
C                                 Improved internal documentation.
C     L. Brown       23-Apr-1993  1.4 ==> 1.5
C                                 Use expression for cos(X) instead of
C                                 for tan(X) since X varies from 0 to
C                                 180 degrees and the principal domain
C                                 of the ACOS(x) intrinsic function is
C                                 [0,180] degrees.
C     L. Brown        3-May-1993  1.5 ==> 1.6
C                                 Check square of denominator for values <= 0
C                                 instead of denominator itself in case round-
C                                 off produces a very small negative value,
C                                 which is invalid for the intrinsic sqrt()
C                                 function when applied to real numbers.
C     L. Brown        4-May-1993  1.6 ==> 1.7
C                                 Improved internal documentation.
C     L. Brown        7-May-1993  1.7 ==> 1.8
C                                 Improved internal documentation.
C     L. Brown        8-Jun-1993  1.8 ==> 1.9
C                                 Renamed from SOLZA to SOLSZA to be consistent
C                                 with naming convention for library.
C                                 Improved internal documentation.
C     L. Brown       14-Jun-1993  1.9 ==> 1.10
C                                 The angle A is now calculated by function
C                                 SOLANG.
C                                 Improved internal documentation.
C     L. Brown       26-Sep-1996  1.10 ==> 1.11
C                                 Limited argument of ACOS function to range
C                                 [-1.,1.] to avoid invalid argument due to
C                                 numerical roundoff.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     AU     Real       km              An Astronomical Unit
C     RE     Real       km              The equatorial radius of the Earth
C
      REAL AU,RE
      PARAMETER(AU=1.4959787E8,RE=6378.164)
C
C  Input variables
C
      INTEGER DAY
      REAL UT,GLAT,GLON,ALT
C
C  Output variables
C
      REAL SOLSZA
C
C  Local variables
C
      REAL A,R,ARG1,ARG2
      REAL SOLANG
C
C  Calculate the conversion factor from degrees to radians
C
      INCLUDE 'const.inc'
C      DTOR=ATAN(1.)/45.
C
C  Calculate the angle A (see the method discussion above)
C
      A=SOLANG(DAY,UT,GLAT,GLON)
C
C  Calculate the term (Re+Z)/(Re+H)
C
      R=(RE+ALT)/AU
C
C  Calculate the solar zenith angle at the observer
C
      ARG1=COS(A*DTOR)-R
      ARG2=SIN(A*DTOR)**2+(COS(A*DTOR)-R)**2
      IF(ARG2 .LE. 0.) THEN
         SOLSZA=0.
      ELSE
         SOLSZA=ACOS(MAX(-1.,MIN(1.,ARG1/SQRT(ARG2))))*RTOD
      ENDIF
C
      RETURN
      END
      FUNCTION SOLUT(SLT,GLON)
C
C  PURPOSE
C     To calculate the Universal Time.
C
C  METHOD
C     Universal Time (solar local time at Greenwich) is related to the solar
C     local time at an observer by the following formula:
C
C        UT = SLT - GLON * (24 hours / 360 degrees = 1 hour / 15 degrees)
C
C     where UT is Universal Time, SLT is the solar local time at the observer,
C     and GLON is the geographic longitude of the observer.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     GLON   Real       Scalar        Degrees east    0. <= GLON < 360.
C        The geographic longitude of the observer
C     SLT    Real       Scalar        Hours           0. <= SLT < 24.
C        The solar local time at the observer
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     SOLUT  Real       Scalar        Hours           0. <= SOLUT < 24.
C        Universal Time (solar local time at Greenwich)
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
C     1.0   14-June-1993
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       14-Jun-1993  1.0 ==> Created
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
      REAL SLT,GLON
C
C  Output variables
C
      REAL SOLUT
C
C  Calculate the Universal Time
C
      SOLUT=MOD(24.+MOD(SLT-GLON/15.,24.),24.)
C
      RETURN
      END
