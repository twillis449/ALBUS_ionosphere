      SUBROUTINE RDCGDB(PATH,LUN)
C  PURPOSE
C  Subroutine to read the CG DataBase
C
C  METHOD
C     No discussion
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    LUN     INTEGER           Logical Unit Number of the CGM database
C    PATH    CHARACTER         Path to the CGM database
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    CG      REAL    (2,36,89) The array holding the coefficients from
C                              geographic to corrected geomagnetic coordinates.
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    FSPEC   CHAR*100          The file specification of the CGM database
C    I       INTEGER           Do Loop Index
C    J       INTEGER           Do Loop Index
C    K       INTEGER           Do Loop Index
C
C  SUBROUTINES CALLED
C     STRCCT   Concatenates two character strings
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C     NAME                   description
C     cglalo.dat             Holds the CG array
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
C     W. Whartenby    21-Dec-1990   1.0.6
C     L. Brown        25-Sep-1995   1.0.6 ==> 1.3
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
      INCLUDE 'cgdb.inc'
      CHARACTER*(*) PATH
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 FSPEC
      INTEGER I,J,K,LUN
C
      CALL STRCCT(PATH,'cglalo.dat',32,FSPEC,I)
      OPEN(UNIT=LUN,FILE=FSPEC,STATUS='OLD',FORM='FORMATTED')
      READ(LUN,100) (((CG(I,J,K),I=1,2),J=1,36),K=1,89)
      CLOSE(UNIT=LUN)
C
      RETURN
  100 FORMAT(0P8F6.1)
      END
C
      SUBROUTINE CGINV1(CGLAT,CGLON,GGLAT,GGLON)
C
C  PURPOSE
C    This subroutine converts coorected geomagnetic
C  coordinates back to geographic coordinates, be-
C  cause some of the HLISM model applications soft-
C  ware requires the coordinates in that frame. The
C  subroutine was developed by Logicon,Inc. in July
C  1973 and later modified by Ms. Hausman of NOAA/NGDC,
C  Boulder,CO and Rod Cregier, USAF Academy, CO. CGINV
C  is the driver of the conversion, and calls CGLAL1.
C
C  METHOD
C    1. Selects polar or mid-latitude algorithms
C    2. If polar gridpoint, converts the gridpoint
C       using polar stereographic coordinates to
C       geographic
C    3. If mid-latitude gridpoint, converts the grid-
C       point using rectangular coordinates to
C       geographic.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    CGLAT   REAL              Corrected Geomagnetic Latitude
C    CGLON   REAL              Corrected Geomagnetic Longitude
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    GGLAT   REAL              Geographic Latitude
C    GGLON   REAL              Geographic Longitude
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    N       INTEGER           North/south flag (n=1,s=2)
C    BA      REAL              Difference between gridpoint long-
C                              itude and the geomag longitude of
C                              pole in radians
C    BL      REAL              Geomag longitude of pole in radians
C    CGLAT   REAL              Input geomag latitude
C    CGLON   REAL              Input geomag longitude
C    ELAT    REAL              Incremental difference in latitude
C    ELON    REAL              Incremental difference in longitude
C    FRACT   REAL
C    GGLAT   REAL              Returned geographic latitude
C    GGLON   REAL              Returned geographic longitude
C    I       INTEGER           Do loop index
C    PFLAG   LOGICAL           .TRUE. when input is near magnetic pole
C    PLAT    REAL       (2)    Geographic latitude of poles
C    PLONG   REAL       (2)    Geographic longitude of poles
C    PMAT    REAL       (2)    Geomagnetic latitude of poles
C    PMONG   REAL       (2)    Geomagnetic longitude of poles
C    QLAT    REAL              Temporary latitude increment value
C    QLONG   REAL              Temporary longitude increment value
C    RAD     REAL              Radian to degree conversion factor
C    RLAT    REAL       (2)    Ratio of differences between geogra-
C                              PHIC and geomagnetic latitude at pole
C    RLONG   REAL       (2)    Ratio of defference between geograp-
C                              hic and geomagnetic longitude at pole
C    SB      REAL              Ratio of (90-lat) to (90-pole)
C    SC      REAL              (90-pole) in geomagnetic coordinates
C    SGN     REAL              Sign of latitude (n=1,s=-1)
C    SVCGLT  REAL              Temporary variable to save original
C                              value of cglat when near mag pole
C    X       REAL              First guess geographic latitude
C    Y       REAL              First guess geographic longitude
C
C   SUBROUTINES CALLED
C    NAME     description
C    CGLAL1   Converts geographic to geomagnetic
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER
C
C  VERSION
C     1.0.6   22-Dec-1990
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     AFGWC/SDDE       May 86        Upgraded documentation
C     NATIONAL GEO-    Aug 87
C     PHYSICAL DATA
C     CENTER (87-I)
C     NGDC (88-II)     Mar 88        Replace CORGM1 with CGLAL1
C     CPI              Mar 89        added special case in which input is
C                                    is either magnetic pole; also added
C                                    coding to take care of cases within a
C                                    degree of magnetic poles
C     CPI              Jan 91        Changed the header and declarations to
C                                    current format
C
C  REFERENCES
C  1.  ICED-III SYSTEM DOCUMENTATION
C  2.  SPERRY FORTRAN-77 (ASCII) PRM
C  3.  AFGWC FORM 10 DX-50194
C  4.  AFGWC FORM 10 DX-60096
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER N,I
      LOGICAL PFLAG
      REAL BA,BL,CGLAT,CGLON,ELAT,ELONG,GGLAT,GGLON,PLAT(2),PLONG(2)
      REAL PMAT(2),PMONG(2),QLAT,QLONG,RLAT(2),RLONG(2),SB,SC,SGN
      REAL SVCGLT,X,Y,FRACT
C
      INCLUDE 'const.inc'
      DATA PLAT/80.,75./,PLONG/280.,127./
      DATA PMAT/81.6,74.5/,PMONG/169.8,17.3/
C
C
C     If input is at either pole, bypass search
C
      IF (CGLAT .EQ. 90.0) THEN
        GGLAT = PLAT(1)
        GGLON = PLONG(1)
        RETURN
      ELSE IF (CGLAT .EQ. -90.0) THEN
        GGLAT = -PLAT(2)
        GGLON = PLONG(2)
        RETURN
      ENDIF
C
C     Begin execution by choosing polar or mid-latitude
C
      IF(ABS(CGLAT) .GE. 40.) THEN
C
C      Polar case -- search using polar coordinates.
C       First check to see if input coordinate is within one degree
C       pole.  If so, calculate coordinates of point at same CGLON.
C       But at CGLAT = 89.0, then linearly interpolate
C
       IF (CGLAT .GT. 89.0) THEN
        SVCGLT = CGLAT
        CGLAT = 89.0
        PFLAG = .TRUE.
       ELSE IF (CGLAT .LT. -88.0) THEN
        SVCGLT = CGLAT
        CGLAT = -88.0
        PFLAG = .TRUE.
       ELSE
        PFLAG = .FALSE.
       ENDIF
C
       DO I = 1,2
        RLONG(I)=PLONG(I)+180.-PMONG(I)
        RLAT(I)=(90.-PMAT(I))/(90.-PLAT(I))
       ENDDO
       N=1
       IF(CGLAT.LT.0.) N=2
       SGN=3.-2.*N
       SC=90.-PLAT(N)
       SB=(90.-ABS(CGLAT))/RLAT(N)
       BA=(CGLON+RLONG(N))*DTOR
       BL=PLONG(N)*DTOR
       DO I = 1,50
        X=SC*COS(BL)+SB*COS(BA)
        Y=SC*SIN(BL)+SB*SIN(BA)
        GGLAT=(90.-SQRT(X*X+Y*Y))*SGN
        GGLON=ATAN2(Y,X)*RTOD
        IF(GGLON.LT.0.) GGLON=GGLON+360.
        CALL CGLAL1(GGLAT,GGLON,QLAT,QLONG)
        ELAT=(CGLAT-QLAT)*SGN
        ELONG=CGLON-QLONG
        IF(ABS( ELAT).GT.0.05.AND.ABS( ELAT).LT.359.95) GO TO 120
        IF(ABS(ELONG).GT.0.05.AND.ABS(ELONG).LT.359.95) GO TO 120
        GO TO 135
120     SB=SB-ELAT
        BA=BA+ELONG*DTOR
       ENDDO
135    CONTINUE
       IF (PFLAG) THEN
        IF (N .EQ. 1) THEN
          FRACT = SVCGLT - CGLAT
        ELSE
          FRACT = 0.5*(CGLAT - SVCGLT)
        ENDIF
        GGLAT = GGLAT + FRACT*(SGN*PLAT(N) - GGLAT)
        GGLON = GGLON + FRACT*(PLONG(N) - GGLON)
        CGLAT = SVCGLT
       ENDIF
      ELSE
C
C      Mid-latitude case -- search using rectangular coordinates.
C
       GGLAT=CGLAT
       GGLON=AMOD(CGLON+290.,360.)
       DO I = 1,20
        CALL CGLAL1(GGLAT,GGLON,QLAT,QLONG)
        ELAT=CGLAT-QLAT
        ELONG=CGLON-QLONG
        IF(ABS( ELAT).GT.0.05.AND.ABS( ELAT).LT.359.95) GO TO 150
        IF(ABS(ELONG).GT.0.05.AND.ABS(ELONG).LT.359.95) GO TO 150
        RETURN
150     GGLAT=GGLAT+ELAT
        GGLON=AMOD(GGLON+ELONG+360.,360.)
       ENDDO
      ENDIF
      RETURN
      END
C
      SUBROUTINE CGLAL1( GGLAT,GGLON, CGLAT,CGLON )
C
C  PURPOSE
C    This subroutine is the Hakura-Gustofsson coord-
C  inate transformation of geographics lat and long
C  to corrected geomagnetic lat and long.  the
C  routine has been changed many times.
C
C
C  METHOD
C  1.  Insure lat & lon are within proper limits
C  2.  Find the four corners of a rectangle con-
C      taining the requested gridpoint.
C  3.  Check the north, south, east and west bound-
C      aries of the box to insure they are within
C      the transformation array.
C  4.  Convert the gridpoint to geomag lat and lon.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    GGLAT   REAL              Geographic Latitude
C    GGLON   REAL              Geographic Longitude
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    CGLAT   REAL              Corrected Geomagnetic Latitude
C    CGLON   REAL              Corrected Geomagnetic Longitude
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    I       INTEGER           Quality control variable
C    J       INTEGER           Index into longitude / 10
C    K       INTEGER           Index into data array
C    ALPHA   REAL              Coefficient of interpolation
C    BETA    REAL              Coefficient of interpolation
C    CG      REAL    (2,36,89) Geomagnetic coordinates of gridpoints
C    CGLAT   REAL              Corrected geomag latitude
C    CGLON   REAL              Corrected geomag longitude
C    CGMNLA  REAL              Mag latitude for north pole
C    CGMNLO  REAL              Mag longitude for north pole
C    CGMSLA  REAL              Mag latitude for south pole
C    CGMSLO  REAL              Mag longitude for south pole
C    EGGLA   REAL              Southern geographic latitude
C    GGLAT   REAL              Geographic latitude
C    GGLAT1  REAL              Temp. geographic latitude
C    GGLON   REAL              Geographic longitude
C    GGLON1  REAL              Temp. geographic latitude
C    TEMP    REAL              Local calculation variable
C    WGGLO   REAL              Western boundary of grid
C    X1LAT   REAL              Northeast latitude of box
C    X1LON   REAL              Northeast longitude of box
C    X2LAT   REAL              Northwest latitude of box
C    X2LON   REAL              Northwest longitude of box
C    X3LAT   REAL              Southwest latitude of box
C    X3LON   REAL              Southwest longitude of box
C    X4LAT   REAL              Southeast latitude of box
C    X4LON   REAL              Southeast longitude of box
C
C   SUBROUTINES CALLED
C    NONE
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER
C
C  VERSION
C     1.0.6   22-Dec-1990
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     R.K. ROSICH     23 Feb 79     Corrected to allow calculation of
C                                   CGM up to both poles
C     F.J.RICH        AUG 83        Modified to use arguments based on
C                                   IGRF80 and discontinuities in table
C                                   removed by curve fitting.
C                     JUL 85        Corrected and updated by Herbert
C                                   Kroehl and Bonnie Hausman of
C                                   USDOC/NOAA/NGDC/DSD
C     AFFGWC/SDDE     MAY 86        Upgraded documentation
C     NGDC (87-I)     AUG 87
C     NGDC (88-II)    MAR 88        Documentation requirements
C     CPI             MAR 89        Renamed COMMON/COORDS/ to COMMON/CGDB/
C                                   and corrected coordinates to be con-
C                                   sistent with CGINV1
C     CPI             MAR 90        Changed the COMMON/CGDB/ to an in-
C                                   clude statement
C     CPI             Jan 91        Changed the header,declarations and
C                                   comments to current format
C
C  REFERENCES
C  1.  ICED-III SYSTEM DOCUMENTATION
C  2.  SPERRY FORTRAN-77 (ASCII) PRM
C  3.  AFGWC FORM 10 DX-50194
C  4.  AFGWC FORM 10 DX-60096
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER I,J,K
      REAL ALPHA,BETA,CGLAT,CGLON,CGMNLA,CGMNLO,CGMSLA,CGMSLO,EGGLA
      REAL GGLAT,GGLAT1,GGLON,GGLON1,TEMP,WGGLO,X1LAT,X1LON,X2LAT
      REAL X2LON,X3LAT,X3LON,X4LAT,X4LON,Y
C
      INCLUDE 'cgdb.inc'
C
C  Commented out data statement is from MAR 88 version received
C    from NGDC.  The values in current use are consistent with
C    CGINV1    ---RED 3/2/89
C     DATA CGMNLA,CGMNLO/82.1,170.8/,  CGMSLA,CGMSLO/-74.0,19.5/
C
      DATA CGMNLA,CGMNLO/81.6,169.8/,  CGMSLA,CGMSLO/-74.5,17.3/
C
C     Save original inputs
C
      GGLAT1 = GGLAT
      GGLON1 = GGLON

C
C     Reduce GGLAT To the range -90 to +90 degrees
C
      DO WHILE (GGLAT1 .GT. 90.)
       GGLAT1=180.-GGLAT1
       GGLON1=GGLON1+180.
      ENDDO
      DO WHILE (GGLAT1 .LT. -90.)
       GGLAT1=-180.-GGLAT1
       GGLON1=GGLON1+180.
      ENDDO
C
C     Reduce GGLON to .lt. 360 degrees (positive or negative)
C
 40   GGLON1 = AMOD(GGLON1,360.)
C
C     Check to see if GGLON > 0, if not correct to be > zero
C
      IF(GGLON1 .LT. 0.0)GGLON1 = GGLON1 + 360.0
      I = IFIX((90.0 - GGLAT1)/2.0)
      J = IFIX(GGLON1/10.) + 1
C
C     Check to see if I is within bounds
C
      IF((I .LE. 0) .OR. (I.GE.89)) THEN
C
C      Set to interpolate to the poles (N or S geographic)
C
       IF (J .GT. 36) J=J-36
       IF (J .LT.  1) J=J+36
       K=J+1
       IF (K .GT. 36) K=K-36
       IF (I .LE.  0) THEN
        I=0
        X1LAT=CGMNLA
        X1LON=CGMNLO
        X2LAT=CG(1,J,I+1)
        X2LON=CG(2,J,I+1)
        X3LAT=CG(1,K,I+1)
        X3LON=CG(2,K,I+1)
        X4LAT=CGMNLA
        X4LON=CGMNLO
       ELSE
        I=89
        X1LAT=CG(1,J,I)
        X1LON=CG(2,J,I)
        X2LAT=CGMSLA
        X2LON=CGMSLO
        X3LAT=CGMSLA
        X3LON=CGMSLO
        X4LAT=CG(1,K,I)
        X4LON=CG(2,K,I)
       ENDIF
      ELSE
C
C      Statement 60.. check to see if J is within bounds.  If out of
C      bounds, restore to in bounds.
C
       IF(J.GT.36)J = J - 36
       IF(J.LT.1)J = J + 36
C
C      Find CGLAT and CGLON of the four corners of rectangle within
C      which GGLAT and GGLON fall.  The corners of the rectangle are X1,
C      X2,X3 and X4.  X1 refers to northwest corner of the rectangle.
C      The labeling goes in a counterclockwise manner.
C
       X1LAT = CG(1,J,I)
       X1LON = CG(2,J,I)
       X2LAT = CG(1,J,I+1)
       X2LON = CG(2,J,I+1)
C
C      Check to see if the easterly edge of the rectangle is within the
C      limits of the argument bounds of array CG
C
       K = J + 1
       IF(K .GT. 36)K = K - 36
       X3LAT = CG(1,K,I+1)
       X3LON = CG(2,K,I+1)
       X4LAT = CG(1,K,I)
       X4LON = CG(2,K,I)
      ENDIF
C
C      Eliminate the discontinuities in the X1, X2, X3, and X4 lon which
C      would occur if the prime meridian of the corrected geomagnetic
C      coordinate system passed through rectangle X1,X2,X3,X4.
C
      Y = AMAX1(X1LON,X2LON,X3LON,X4LON)
      IF(ABS(Y-X1LON).GT.180.0)X1LON = X1LON + 360.0
      IF(ABS(Y-X2LON).GT.180.0)X2LON = X2LON + 360.0
      IF(ABS(Y-X3LON).GT.180.0)X3LON = X3LON + 360.0
      IF(ABS(Y-X4LON).GT.180.0)X4LON = X4LON + 360.0
C
C     Calculate the southern and the westward boundaries (geographic
C     latitude and longitude) of the rectangle.
C
      EGGLA = FLOAT(-2*I + 88)
      WGGLO = FLOAT(10*J - 10)
C
C     Calculate the coefficients of the interpolation equation.  These
C     coefficients represent the fractional part of the box the point
C     GGLAT-GGLON is from the southern and the westward boundaries of
C     the rectangle.
C
      ALPHA = (GGLAT1 - EGGLA)/2.0
      BETA = (GGLON1 - WGGLO)/10.
C
C     Now calculate the corrected geomagnetic coordinates that corresp-
C     pond to geographic coordinates of the input point (GGLAT,GGLON)
C
      TEMP = (ALPHA*BETA)*(X4LAT+X2LAT-X1LAT-X3LAT)
      CGLAT = X2LAT + ALPHA*(X1LAT-X2LAT) + BETA*(X3LAT-X2LAT) + TEMP
      TEMP = (ALPHA*BETA)*(X4LON+X2LON-X1LON-X3LON)
      CGLON = X2LON + ALPHA*(X1LON-X2LON) + BETA*(X3LON-X2LON) + TEMP
C
C     Check to determine if the corrected geomagnetic longitude is less
C     than 360 degrees.  If it is not, reduce the calculated value by
C     360 degrees.
C
      IF(CGLON.GT.360.0)CGLON = CGLON - 360.0
      RETURN
      END
C
      FUNCTION FAP(KP)
C
C  PURPOSE
C  Calculates the quasi-linear magnetic activity index Ap from the
C  quasi-logarithmic index Kp using Table 6 from Mayaud (1980).
C
C  METHOD
C   Table lookup in which the subscript is just 3*Kp, followed by
C   linear interpolation (to allow for averaged Kp's)
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    KP      REAL            The magnetic activity index Kp.
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    FAP     REAL            The magnetic activity index Ap.
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    IKP     INTEGER           Integer of TKP
C    TKP     REAL              3*KP
C
C  SUBROUTINES REQUIRED
C     None.
C
C  FILES ACCESSED
C     None.
C
C  AUTHOR
C     Robert E. Daniell
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.6    22-June-1990
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C      Mayaud, P. N., Derivation, Meaning, and Use of Geomagnetic
C        Indices, Geophysical Monograph 22, American Geophysical
C        Union, Washington, DC, 1980
C
C  SPECIAL CONSTANTS
C     NONE
C
      REAL KP,TKP,FAP,AP1,AP2
      INTEGER KAP(0:27),IKP
C     +  +         +         +         +         +         +         + +
C  3*Kp:         0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
C               14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27/
      DATA KAP/  0,  2,  3,  4,  5,  6,  7,  9, 12, 15, 18, 22, 27, 32,
     1          39, 48, 56, 67, 80, 94,111,132,154,179,207,236,300,400/
C
      IF (KP .LT. 0.0) KP = 0.0
      IF (KP .GT. 9.0) KP = 9.0
C
      TKP = 3.0*KP
      IKP = IFIX(TKP)
      IF (IKP .EQ. 27) IKP = IKP - 1
      AP1 = FLOAT(KAP(IKP))
      AP2 = FLOAT(KAP(IKP+1))
      FAP = AP1 + (AP2-AP1)*(TKP - FLOAT(IKP))
C
      RETURN
      END
C
      SUBROUTINE GCXCGM(DAY,UT,LAT,LON,MLAT,MLON,MLT)
C
C  PURPOSE
C     To calculate the corrected geomagnetic latitude and local time from
C     geocentric coordinates.
C
C  METHOD
C     No discussion
C
C  INPUT PARAMETERS
C     DAY     INTEGER           The day of the year
C     UT      REAL              The universal time, in seconds
C     LAT     REAL              The geocentric latitude, in degrees north
C     LON     REAL              The geocentric longitude, in degrees east
C
C  OUTPUT PARAMETERS
C     MLAT    REAL              The corrected geomagnetic latitude, in degrees north
C     MLON    REAL              The corrected geomagnetic longitude, in degrees east
C     MLT     REAL              The corrected geomagnetic local time, in hours
C
C  LOCAL VARIABLES
C     LATAS   REAL              The geocentric latitude of the antisolar point, in degrees north
C     LONAS   REAL              The geocentric longitude of the antisolar point, in degrees east
C     MLATAS  REAL              The corrected geomagnetic latitude of the antisolar point, in
C                               degrees north
C     MLONAS  REAL              The corrected geomagnetic longitude of the antisolar point, in
C                               degrees east
C
C  SUBROUTINES REQUIRED
C     CGLAL1   Converts geocentric latitude and longitude to corrected geo-
C              magnetic latitude and longitude
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
C     1.0.6      22-June-1990
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
      INTEGER DAY
      REAL UT,LAT,LON,MLAT,MLT
      REAL MLON,LATAS,LONAS,MLATAS,MLONAS
C
C  Calculate the geomagnetic coordinates of the geocentric location
C
      CALL CGLAL1(LAT,LON,MLAT,MLON)
C
C  Calculate the geocentric coordinates of the antisolar point
C
      LATAS=23.45*COS(6.283185*FLOAT(DAY+10)/365.0)
      LONAS=360.-UT/240.
C
C  Calculate the geomagnetic coordinates of the antisolar point
C
      CALL CGLAL1(LATAS,LONAS,MLATAS,MLONAS)
C
C  Calculate the magnetic local time
C
      MLT=MOD(24.+(MLON-MLONAS)/15.,24.)
C
      RETURN
      END
C
      SUBROUTINE BNDRY(CGMLAT,CGMLT,RB)
C
C
C  PURPOSE
C     To determine the high latitude region of a given location.
C
C  METHOD
C     Three regions are defined for the high latitude ionosphere:  polar cap,
C     auroral oval, and trough.  The equatorward boundary of each region is
C     represented by a circle with centers given in the common block PRECIP.
C     See subroutine INIT for a description of how these values are determined.
C
C  INPUT PARAMETERS
C    LAT0    REAL       (3)    The Geomagnetic latitude corresponding
C                              to the circle center of the oval region
C                       (i)
C                        i  =  1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    MLT     REAL              The corrected geomagnetic local time, in hours
C    MLT0    REAL       (3)    The Geomagnetic local time corresponding
C                              to the circle centero f the oval region
C                       (i)
C                        i  =  1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    RAD0    REAL       (3)    The radius of the circle of the oval region
C                       (i)
C                        i  =  1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    LATR    REAL              The Geomagnetic latitude corresponding
C                              to the circle center of the trough region
C    MLTR    REAL              The Geomagnetic local time corresponding
C                              to the circle centero f the trough region
C    RADTR   REAL              The radius of the circle of the trough region
C
C
C  OUTPUT PARAMETERS
C     RB      REAL       ARRAY  Gives the equatorward boundaries of the three
C                                 regions
C  LOCAL VARIABLES
C     I       INTEGER           1 for northern hemsiphere, 2 otherwise
C
C  SUBROUTINES REQUIRED
C     NONE
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     Robert E. Daniell
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.6    22-June-1990
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     NONE
C
C
C
      INCLUDE 'indirect.inc'
      INCLUDE 'precip.inc'
      INCLUDE 'aindex.inc'
      INCLUDE 'region_b.inc'
      REAL LAT1(3),MLT1(3),RAD1(3)
      REAL RB(0:2),CGMLT,CGMLAT
      INTEGER I
C
C     Set up the latitude-local time- radius array to correspond
C     to the trough-oval-cap boundary
      I = 1
      IF (CGMLAT .LT. 0.0) I = 2
      LAT1(1) = MLATR0(I)
      MLT1(1) = MLTR0(I)
      RAD1(1) = ATR(0,1) +ATR(1,1)*KP(0)
      LAT1(2) = LAT0(1,1,1,I)
      MLT1(2) = MLT0(1,1,1,I)
      RAD1(2) = A(0,1,1)+A(1,1,1)*KP(1)
      LAT1(3) = LAT0(3,1,1,I)
      MLT1(3) = MLT0(3,1,1,I)
      RAD1(3) = A(0,2,1)+A(1,2,1)*KP(2)
      CALL PREGION(CGMLT,LAT1,MLT1,RAD1,RB)
C     Make sure minimum widths are met
      IF ((RB(1) -RB(0)) .LT. CB(1)) THEN
       RB(0) = RB(1) - CB(1)
      ENDIF
      IF ((RB(2) -RB(1)) .LT. CB(2)) THEN
       RB(2) = RB(1) + CB(2)
      ENDIF
      RETURN
      END
C
      FUNCTION KPTOAP(KP)
C
C  PURPOSE
C     Calculates the quasi-linear magnetic activity index Ap from the
C     quasi-logarithmic index Kp using Table 6 from Mayaud (1980).
C
C  METHOD
C     Table lookup in which the subscript is just Kp*3, followed by
C     linear interpolation (to allow for averaged Kp's).
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
C     NONE
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     Robert E. Daniell
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.6    12-November-1991
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     R. Daniell      22-Jun-1990   1.0
C                                   Created
C     L. Brown        12-Nov-1991   1.0 --> 1.1
C                                   Cleaned up code.
C
C  REFERENCES
C      Mayaud, P. N., Derivation, 'Meaning, and Use of Geomagnetic Indices',
C      Geophysical Monograph 22, American Geophysical Union, Washington, D.C.,
C      1980.
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER IKP3
      REAL KP,KPTOAP
      REAL KP3
      REAL KAP(0:27)
C
      DATA KAP/0.,2.,3.,4.,5.,6.,7.,9.,12.,15.,18.,22.,27.,32.,39.,48.,
     $         56.,67.,80.,94.,111.,132.,154.,179.,207.,236.,300.,400./
C
      IF(KP .LT. 0.) THEN
         KP3=0.
      ELSE IF(KP .GT. 9.) THEN
         KP3=27.
      ELSE
         KP3=KP*3.
      ENDIF
      IKP3=MIN(INT(KP3),26)
      KPTOAP=KAP(IKP3)+(KAP(IKP3+1)-KAP(IKP3))*(KP3-FLOAT(IKP3))
C
      RETURN
      END
C
