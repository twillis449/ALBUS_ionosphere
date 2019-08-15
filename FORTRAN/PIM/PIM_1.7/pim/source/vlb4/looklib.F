C******************************************************************************
C  LOOKLIB is a FORTRAN library of line-of-sight (LOS) manipulations that
C  assume that the observer is at ground level (zero altitude) and restrict
C  elevation angles to positive values.  It contains the following routines:
C
C     FUNCTION   LKALT  (EL,RANGE)
C                       Calculates the altitude at a point along a LOS from an
C                       observer at ground level (zero altitude) given the
C                       elevation angle of the LOS and the range at the point
C                       along the LOS from the observer
C     FUNCTION   LKANG  (ELEV,EL,ALT)
C                       Calculates the great-circle angular separation at a
C                       point along a LOS from an observer at ground level
C                       (zero altitude) given the elevation angle of the LOS
C                       and the altitude of the point
C    *SUBROUTINE LKCRDS (LAT,LON,AZ,EL,ALT,LATP,LONP)
C                       Calculates the location in latitude and longitude at a
C                       point along a LOS from an observer at ground level
C                       (zero altitude) given the location at the observer in
C                       latitude and longitude, the azimuth and elevation
C                       angles of the LOS from the observer, and the altitude
C                       at the point
C    *FUNCTION   LKRANG (EL,ALT)
C                       Calculates the range at a point along a LOS from an
C                       observer given the elevation angle of the LOS from the
C                       observer and the altitude at the point along the LOS
C                       from the observer
C
C  Routines marked by an asterisk (*) use other routines in the library.
C******************************************************************************
      FUNCTION LKALT(EL,RANGE)
C
C  PURPOSE
C     To calculate the altitude at a point along a LOS from an observer at
C     ground level (zero altitude) given the elevation angle of the LOS and the
C     range at the point along the LOS from the observer.
C
C  METHOD
C     The law of cosines for oblique plane triangles is used.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EL     Real       Scalar        Degrees         0. <= EL <= 90.
C        The elevation angle of the LOS from the observer
C     RANGE  Real       Scalar        Km              >= 0.
C        The range at the point along the LOS from the observer
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     LKALT  Real       Scalar        Km              >= 0.
C        The altitude at the point along the LOS from the observer
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     RESQ   Real       Scalar        Km**2           Re**2
C        The square of the mean radius of the Earth
C     RSQ    Real       Scalar        Km**2           >= Re**2
C        An intermediate variable used in the calculation of the altitude at
C        the point along the LOS from the observer
C     SINEL  Real       Scalar        n/a             0. <= SINEL <= 1.
C        The sine of the elevation angle of the LOS from the observer
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
C     1.5   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        8-May-1989  1.0 ==> Created
C     L. Brown       22-Oct-1992  1.0 ==> 1.1
C                                 The conversion factor from degrees to radians
C                                 is now calculated.
C                                 Improved internal documentation.
C     L. Brown        9-Aug-1993  1.1 ==> 1.2
C                                 Converted from vector to scalar operation.
C                                 Converted from subroutine to function.
C                                 Added handling of special cases.
C                                 Improved internal documentation.
C     L. Brown       15-Aug-1993  1.2 ==> 1.3
C                                 Added coding to attempt to handle invalid
C                                 operations due to machine roundoff.
C                                 Improved parameter validation.
C                                 Improved internal documentation.
C                                 Optimized coding.
C     L. Brown       18-Aug-1993  1.3 ==> 1.4
C                                 Renamed variable ARG to RSQ.
C                                 Improved handling of special cases.
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.4 ==> 1.5
C                                 The ampersand (&) is now used as the
C                                 continuation character.
C
C  REFERENCES
C     1. Selby, Samuel M., editor, "CRC Standard Mathematical Tables",
C        23rd Edition, CRC Press, Inc., Cleveland, Ohio, 1975, p. 236.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     RE     Real       Km              The mean radius of the Earth
C
      INCLUDE 'const.inc'
      REAL RE
      PARAMETER(RE=6375.)
C
C  Input variables
C
      REAL EL,RANGE
C
C  Output variables
C
      REAL LKALT
C
C  Local variables
C
      REAL SINEL,RESQ,RSQ
C
C  Check input parameters
C
#ifdef DEBUG
      IF((EL .LT. 0.) .OR. (EL .GT. 90.)) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKALT:Elevation angle (=',EL,
     &      ') must be >= 0 and <= 90 degrees.'
         STOP 'Program terminated with error in LKALT.'
      ENDIF
      IF(RANGE .LT. 0.) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKALT:Range (=',RANGE,') must be >= 0 km.'
         STOP 'Program terminated with error in LKALT.'
      ENDIF
#endif
C
C  Calculate the conversion factor from degrees to radians
C
C      DTOR=ATAN(1.)/45.
C
C  Calculate the altitude at the point along the LOS from the observer
C
      IF(1.+RANGE .EQ. 1.) THEN
         LKALT=0.
      ELSE
         SINEL=SIN(EL*DTOR)
         IF(SINEL .EQ. 1.) THEN
            LKALT=RANGE
         ELSE
            RESQ=RE**2
            RSQ=RANGE**2+RESQ+2.*RANGE*RE*SINEL
            IF(RSQ .LE. RESQ) THEN
               LKALT=0.
            ELSE
               LKALT=SQRT(RSQ)-RE
               LKALT=MAX(LKALT,0.)
            ENDIF
         ENDIF
      ENDIF
C
      RETURN
      END
      FUNCTION LKANG(ELEV,EL,ALT)
C
C  PURPOSE
C     To calculate the great-circle angular separation at a point along a LOS
C     from an observer at ground level (zero altitude) given the elevation
C     angle of the LOS and the altitude of the point.
C
C  METHOD
C     The law of sines for oblique plane triangles is used.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ELEV   Real       Scalar        Km              
C        The elevation of the observation position, in km above Earth radius
C     ALT    Real       Scalar        Km              >= 0.
C        The altitude at the point along the LOS from the observer
C     EL     Real       Scalar        radians         0. <= EL <= \pi/2
C        The elevation angle of the LOS from the observer
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     LKANG  Real       Scalar        radians         0. <= LKANG <= \pi/2
C        The great-circle angular separation between the observer and the point
C        along the LOS from the observer
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     COSTRM Real       Scalar        n/a             0. <= COSTRM <= 1.
C        An intermediate variable used in the calculation of the great-circle
C        angular separation between the observer and the point along the LOS
C        from the observer
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     R      Real       Scalar        Km              >= Re
C        The radius at the point along the LOS from the observer
C     RECOSE Real       Scalar        Km              0. <= RECOSE <= Re
C        The product of the mean radius of the Earth and the cosine of the
C        elevation angle of the LOS from the observer
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
C     1.3   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        9-Aug-1993  1.0 ==> Created
C     L. Brown       15-Aug-1993  1.0 ==> 1.1
C                                 Added coding to attempt to handle invalid
C                                 operations due to machine roundoff.
C                                 Improved parameter validation.
C                                 Improved internal documentation.
C     L. Brown       18-Aug-1993  1.1 ==> 1.2
C                                 Variable ARG1 renamed to RECOSE.
C                                 Variable ARG2 renamed to R.
C                                 Variable ARG3 renamed to COSTRM.
C                                 Improved handling of special cases.
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The ampersand (&) is now used as the
C                                 continuation character.
C     2005 Sep 05  James M Anderson  --JIVE  convert to use radians,
C                                      use a consistent Earth radius, and
C                                      allow for a non-zero starting elevation
C
C  REFERENCES
C     1. Selby, Samuel M., editor, "CRC Standard Mathematical Tables",
C        23rd Edition, CRC Press, Inc., Cleveland, Ohio, 1975, p. 236.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     RE     Real       Km              The mean radius of the Earth
C
C      REAL RE
C      PARAMETER(RE=6375.)
      IMPLICIT NONE
      include 'const.inc'
      include 'logicuni.inc'
C
C  Input variables
C
      REAL ELEV,EL,ALT
C
C  Output variables
C
      REAL LKANG
C
C  Local variables
C
      REAL RECOSE,COSTRM
C
C  Check input parameters
C
C     2005 Sep 05  JMA  --As for EL>90\degr, when would
C                         that really happen?
#ifdef DEBUG
      IF((EL .LT. 0.) .OR. (EL .GT. PI*0.5)) THEN
         WRITE(LUSTDERR,'(A,0PF12.6,A)')
     &      'LKANG:Elevation angle (=',EL,
     &      ') must be >= 0 and <= 90 degrees.'
         STOP 'Program terminated with error in LKANG.'
      ENDIF
c$$$      IF(ALT .LT. 0.) THEN
c$$$         WRITE(LUSTDERR,'(A,0PF12.6,A)')
c$$$     &      'LKANG:Altitude (=',ALT,') must be >= 0 km.'
c$$$         STOP 'Program terminated with error in LKANG.'
c$$$      ENDIF
#endif
C
C  Calculate the conversion factor from degrees to radians
C
C      DTOR=ATAN(1.)/45.
C
C  Calculate the great-circle angular separation between the observer and the
C  point along the LOS from the observer
C
#ifdef DEBUG
      IF((ELEV.EQ.ALT).AND.(EL.LT.PI*0.4999999999D0)) THEN
         WRITE(LUSTDERR,*) 'Bad ALT and EL combination', ALT, EL
         STOP 'Program terminated with error in LKANG.'
      ENDIF
#endif         
      RECOSE=(RADIUS_EARTH+ELEV)*COS(EL)
      COSTRM=RECOSE/(RADIUS_EARTH+ALT)
#ifdef DEBUG
      IF(COSTRM.GT.1.0) THEN
         WRITE(LUSTDERR,*) 'Bad COSTRM combination', ALT, EL
         STOP 'Program terminated with error in LKANG.'
      ENDIF
#endif         
      LKANG=ACOS(COSTRM)-EL
C     don't bother to check
c$$$
c$$$      IF(1.+RECOSE .EQ. 1.) THEN
c$$$         LKANG=0.
c$$$      ELSE
c$$$         R=RE+ALT
c$$$         IF(R .EQ. RE) THEN
c$$$            LKANG=0.
c$$$         ELSE
c$$$            COSTRM=RECOSE/R
c$$$            IF(1+COSTRM .EQ. 1.) THEN
c$$$               LKANG=0.
c$$$            ELSE
c$$$               IF(COSTRM .GE. 1.) THEN
c$$$                  LKANG=0.
c$$$               ELSE
c$$$                  LKANG=ACOS(COSTRM)/DTOR-EL
c$$$                  LKANG=MAX(MIN(LKANG,90.),0.)
c$$$               ENDIF
c$$$            ENDIF
c$$$         ENDIF
c$$$      ENDIF
C
      RETURN
      END
      SUBROUTINE LKCRDS(LAT,LON,ELEV,AZ,EL,ALT,LATP,LONP)
C
C  PURPOSE
C     To calculate the location in latitude and longitude at a point along a
C     LOS from an observer at ground level (zero altitude) given the location
C     at the observer in latitude and longitude, the azimuth and elevation
C     angles of the LOS from the observer, and the altitude at the point.
C
C  METHOD
C     The great-circle angular separation between the observer and the point
C     along the LOS from the observer is calculated using plane geometry.  The
C     latitude and longitude at the point along the LOS from the observer are
C     then calculated using spherical trigonometry.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALT    Real       Scalar        Km              >= 0.
C        The altitude at the point along the LOS from the observer
C     AZ     Real       Scalar        Degrees         0. <= AZ < 360.
C        The azimuth angle of the LOS from the observer
C     EL     Real       Scalar        Degrees         0. <= EL <= 90.
C        The elevation angle of the LOS from the observer
C     ELEV   Real       Scalar        Km      
C        The elevation of the observer above the radius of the Earth
C     LAT    Real       Scalar        Degrees north   -90. < LAT < 90.
C        The latitude at the observer, not allowed at a pole because of the
C        ambiguity of the azimuth angle there
C     LON    Real       Scalar        Degrees east    0. <= LON < 360.
C        The longitude at the observer
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     LATP   Real       Scalar        Degrees north   -90. <= LATP <= 90.
C        The latitude at the point along the LOS from the observer
C     LONP   Real       Scalar        Degrees east    0. <= LONP < 360.
C        The longitude at the point along the LOS from the observer
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ANGSEP Real       Scalar        Radians         0. <= ANGSEP <= pi/2.
C        The great-circle angular separation between the observer and the point
C        along the LOS from the observer
C     ARG1   Real       Scalar        n/a             -1. < ARG1 < 1.
C        The first argument of the ATAN2 intrinsic function used in the
C        calculation of the longitude at the point along the LOS from the
C        observer
C     ARG2   Real       Scalar        n/a             -1. < ARG2 < 2.
C        The second argument of the ATAN2 intrinsic function used in the
C        calculation of the longitude at the point along the LOS from the
C        observer
C     AZR    Real       Scalar        Radians         0. <= AZR < 2.*pi
C        The azimuth angle of the LOS from the observer
C     COSAS  Real       Scalar        n/a             0. <= COSAS <= 1.
C        The cosine of the great-circle angular separation between the observer
C        and the point along the LOS from the observer
C     COSTHP Real       Scalar        n/a             -1. <= COSTHP <= 1.
C        The cosine of the colatitude at the point along the LOS from the
C        observer
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C     LATR   Real       Scalar        Radians         -pi/2. < LATR < pi/2.
C        The latitude at the observer, not allowed at a pole because of the
C        ambiguity of the azimuth angle there
C     SACLAT Real       Scalar        n/a             0. <= SACLAT <= 1.
C        The product of the sine of the great-circle angular separation between
C        the observer and the point along the LOS from the observer and the
C        cosine of the latitude at the observer
C     SINLAT Real       Scalar        n/a             -1. < SINLAT < 1.
C        The sine of the latitude at the observer
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     LKANG  Real       Calculates the great-circle angular separation between
C                       an observer at ground level (zero altitude) and a point
C                       along a LOS from the observer
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
C     L. Brown       13-Apr-1989  1.0 ==> Created
C     L. Brown       22-Oct-1992  1.0 ==> 1.1
C                                 The conversion factor from degrees to radians
C                                 is now calculated.
C                                 Improved internal documentation.
C     L. Brown        9-Aug-1993  1.1 ==> 1.2
C                                 Converted from vector to scalar operation.
C                                 Uses routine LKANG.
C                                 Optimized coding.
C                                 Improved internal documentation.
C     L. Brown       10-Aug-1993  1.2 ==> 1.3
C                                 Improved internal documentation.
C     L. Brown       13-Aug-1993  1.3 ==> 1.4
C                                 Further optimization of coding.
C     L. Brown       15-Aug-1993  1.4 ==> 1.5
C                                 Added coding to attempt to handle invalid
C                                 operations due to machine roundoff.
C                                 Improved parameter validation.
C                                 Improved internal documentation.
C     L. Brown       15-Aug-1993  1.5 ==> 1.6
C                                 Improved coding to attempt to handle invalid
C                                 operations due to machine roundoff.
C                                 Improved internal documentation.
C     L. Brown       18-Aug-1993  1.6 ==> 1.7
C                                 Further optimization of coding.
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.7 ==> 1.8
C                                 The ampersand (&) is now used as the
C                                 continuation character.
C     2005 Sep 08  James M Anderson  --JIVE  change LKANG to use radians,
C                                 and to use an observer elevation
C
C  REFERENCES
C     1. Selby, Samuel M., editor, "CRC Standard Mathematical Tables",
C        23rd Edition, CRC Press, Inc., Cleveland, Ohio, 1975, pp. 236-240.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      IMPLICIT NONE
      REAL LAT,LON,ELEV,AZ,EL,ALT
C
C  Output variables
C
      REAL LATP,LONP
C
C     included variables
C     
      include 'const.inc'
C
C  Local variables
C
      REAL LATR,SINLAT,AZR,ANGSEP,LKANG,COSAS,SACLAT,COSTHP,ARG1,
     &     ARG2
C
C  Check input parameters
C
#ifdef DEBUG
      IF(ABS(LAT) .GE. 90.) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKCRDS:Latitude (=',LAT,') must be > -90 and < 90 degrees.'
         STOP 'Program terminated with error in LKCRDS.'
      ENDIF
      IF((EL .LT. 0.) .OR. (EL .GT. 90.)) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKCRDS:Elevation angle (=',EL,
     &      ') must be >= 0 and <= 90 degrees.'
         STOP 'Program terminated with error in LKCRDS.'
      ENDIF
      IF(ALT .LT. 0.) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKCRDS:Altitude (=',ALT,') must be >= 0 km.'
         STOP 'Program terminated with error in LKCRDS.'
      ENDIF
#endif
C
C  Calculate the conversion factor from degrees to radians
C
C      DTOR=ATAN(1.)/45.
C
C  Convert the latitude at the observer to radians
C
      LATR=LAT*DTOR
C
C  Calculate the sine of the latitude at the observer
C
      SINLAT=SIN(LATR)
C
C  Convert the azimuth angle to radians
C
      AZR=AZ*DTOR
C
C  Calculate the great-circle angular separation between the observer and the
C  point along the LOS
C
      ANGSEP=LKANG(ELEV,REAL(EL*DTOR),ALT)
C
C  Calculate the cosine of the great-circle angular separation between the
C  the observer and the point along the LOS
C
      COSAS=COS(ANGSEP)
C
C  Calculate the product of the sine of the great-circle angular separation
C  between the observer and the point along the LOS and the cosine of the
C  latitude at the observer
C
      SACLAT=SIN(ANGSEP)*COS(LATR)
C
C  Calculate the cosine of the colatitude at the point along the LOS from the
C  observer
C
      COSTHP=COSAS*SINLAT+SACLAT*COS(AZR)
      COSTHP=MAX(MIN(COSTHP,1.),-1.)
C
C  Calculate the latitude at the point along the LOS from the observer
C
      LATP=90.-ACOS(COSTHP)/DTOR
      LATP=MAX(MIN(LATP,90.),-90.)
C
C  Calculate the longitude at the point along the LOS from the observer
C
      ARG1=SIN(AZR)*SACLAT
      ARG2=COSAS-SINLAT*COSTHP
      IF((1.+ARG1 .EQ. 1.) .AND. (1.+ARG2 .EQ. 1.)) THEN
         LONP=LON
      ELSE
         LONP=LON+ATAN2(ARG1,ARG2)/DTOR
         LONP=MOD(360.+MOD(LONP,360.),360.)
      ENDIF
C
      RETURN
      END
      FUNCTION LKRANG(ELEV,EL,ALT)
C
C  PURPOSE
C     To calculate the range at a point along a LOS from an observer given the
C     elevation angle of the LOS from the observer and the altitude at the
C     point along the LOS from the observer.
C
C  METHOD
C     The law of sines for oblique plane triangles is used.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALT    Real       Scalar        Km              >= 0.
C        The altitude at the point along the LOS from the observer
C     EL     Real       Scalar        radians         0. <= EL <= \pi/2
C        The elevation angle of the LOS from the observer
C     ELEV   Real       Scalar        Km      
C        The elevation of the observer above the radius of the Earth
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     LKRANG Real       Scalar        Km              >= 0.
C        The range at the point along the LOS from the observer
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     COSEL  Real       Scalar        n/a             0. <= COSEL <= 1.
C        The cosine of the elevation angle of the LOS from the observer
C     DTOR   Real       Scalar        Radians/degree  pi/180.
C        The conversion factor from degrees to radians
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     LKANG  Real       Calculates the great-circle angular separation between
C                       an observer at ground level (zero altitude) and a point
C                       along a LOS from the observer
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
C     1.5   29-March-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       30-Aug-1989  1.0 ==> Created
C     L. Brown       22-Oct-1992  1.0 ==> 1.1
C                                 The conversion factor from degrees to radians
C                                 is now calculated.
C                                 Improved internal documentation.
C     L. Brown        9-Aug-1993  1.1 ==> 1.2
C                                 Converted from vector to scalar operation.
C                                 Converted from subroutine to function.
C                                 Uses routine LKANG.
C                                 Added handling of special cases.
C                                 Optimized coding.
C                                 Improved internal documentation.
C     L. Brown       15-Aug-1993  1.2 ==> 1.3
C                                 Improved handling of special cases.
C                                 Improved parameter validation.
C                                 Improved internal documentation.
C     L. Brown       18-Aug-1993  1.3 ==> 1.4
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.4 ==> 1.5
C                                 The ampersand (&) is now used as the
C                                 continuation character.
C     2005 Sep 05  James M Anderson  --JIVE  convert to new LKANG
C
C  REFERENCES
C     1. Selby, Samuel M., editor, "CRC Standard Mathematical Tables",
C        23rd Edition, CRC Press, Inc., Cleveland, Ohio, 1975, p. 236.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     RE     Real       Km              The mean radius of the Earth
C
C      REAL RE
C      PARAMETER(RE=6375.)
C
C  Input variables
C
      REAL EL,ALT
C
C  Output variables
C
      REAL LKRANG
C
C     included variables
C     
      include 'const.inc'
C
C  Local variables
C
      REAL COSEL,LKANG
C
C  Check input parameters
C
#ifdef DEBUG
      IF((EL .LT. 0.) .OR. (EL .GT. 90.)) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKRANG:Elevation angle (=',EL,
     &      ') must be >= 0 and <= 90 degrees.'
         STOP 'Program terminated with error in LKRANG.'
      ENDIF
      IF(ALT .LT. 0.) THEN
         WRITE(*,'(A,0PF12.6,A)')
     &      'LKRANG:Altitude (=',ALT,') must be >= 0 km.'
         STOP 'Program terminated with error in LKRANG.'
      ENDIF
#endif
C
C  Calculate the conversion factor from degrees to radians
C
C      DTOR=ATAN(1.)/45.
C
C  Calculate the cosine of the elevation angle of the LOS from the observer
C
      COSEL=COS(EL)
C
C  Calculate the range at the point along the LOS from the observer
C
      IF(1.+COSEL .EQ. 1.) THEN
         LKRANG=ALT-ELEV
      ELSE
         IF(RADIUS_EARTH+ALT .EQ. RADIUS_EARTH+ELEV) THEN
            LKRANG=0.
         ELSE
            LKRANG=(RADIUS_EARTH+ALT)*SIN(LKANG(ELEV,EL,ALT))/COSEL
         ENDIF
      ENDIF
C
      RETURN
      END
