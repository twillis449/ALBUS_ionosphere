      SUBROUTINE INITPR
C
C  PURPOSE
C     To initialize parameters necessary for SUBROUTINE PRECIP
C
C  METHOD
C     From the data points compute the centers and radii of the circles
C  describing the precipitation boundaries and maximum, and compute the
C  Fourier coefficients describing the local time variation of the para-
C  meters.
C
C  INPUT PARAMETERS
C    NAME    TYPE   ARRAY     description
C  LOCAL VARIABLES
C    NAME   TYPE   Array   Description
C    J      INTEGER        Do loop index
C    K      INTEGER        Do loop index
C    L      INTEGER        Do loop index
C    M1     INTEGER        Do loop index
C    M2     INTEGER        Do loop index
C    M3     INTEGER        Do loop index
C    XKP    REAL   (2,2,2) Holds Kp values
C
C  SUBROUTINES CALLED
C    NAME       description
C    GET_CIRC   Computes the magnetic latitude and local time of
C               the center of a circle which goes through the two
C               data points input and has a radius equal to the
C               average of the colatitudes of the two points.
C
C    GETFC      Computes the Fourier coefficients of the magnetic
C               local time variation of the maximum value of each
C               parameter.
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.1   18-Nov-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby     5-Dec-1990   1.0 ==> Created
C     L. Brown        18-Nov-1994   1.0 ==> 1.1
C                                   The argument list of routine GET_CIRCL has
C                                   changed.
C                                   Removed local variable ZKP.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      REAL CRTFLX
      PARAMETER (CRTFLX = 0.1)
      REAL XKP(2,2,2,2)
      REAL KP01,KP02
      INTEGER J,M,IC1,IC2,IL,IC,M1,M2,M3,M4
C
C
      INCLUDE 'aindex.inc'
      INCLUDE 'precip.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'logicuni.inc'
C
      KP01 = KP(0)
      KP02 = KP(0)
      DO M = 1,2
         DO L = 1,2
            DO K = 1,2
               DO J = 1,2
                  XKP(J,K,L,M) = KP(J)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
C     First, determine the trough data
      CALL GET_CIRCL(ABS(VALTR(1,1,1)),ABS(VALTR(2,1,1)),MLATR(1,1,1),
     1  MLTR(1,1,1),MLATR(2,1,1),MLTR(2,1,1),
     2  ABS(VALTR(1,1,2)),ABS(VALTR(2,1,2)),MLATR(1,1,2),
     3  MLTR(1,1,2),MLATR(2,1,2),MLTR(2,1,2),
     4  MLATR0(1),MLTR0(1),RADTR0(1),1,ATR,KP01,IC1)
      CALL GET_CIRCL(ABS(VALTR(1,2,1)),ABS(VALTR(2,2,1)),MLATR(1,2,1),
     1  MLTR(1,2,1),MLATR(2,2,1),MLTR(2,2,1),
     2  ABS(VALTR(1,2,2)),ABS(VALTR(2,2,2)),MLATR(1,2,2),
     3  MLTR(1,2,2),MLATR(2,2,2),MLTR(2,2,2),
     4  MLATR0(2),MLTR0(2),RADTR0(2),1,ATR,KP02,IC2)
      IF ((MIN(IC1,IC2) .GE. 2) .OR. (MAX(IC1,IC2) .LT. 2)) THEN
       KP(0) = (KP01 + KP02)*0.5
      ELSE IF (( IC1 .GE. 2) .AND. (IC2 .LT. 2)) THEN
       KP(0) = KP01
      ELSE IF (( IC2 .GE. 2) .AND. (IC1 .LT. 2)) THEN
       KP(0) = KP02
      ELSE
      WRITE(LUSTDERR,*)' Error from trough boundary computation',IC1,IC2
      ENDIF
      DO K = 1,2
C      K = 1; electrons, K = 2, protons
       DO L = 1,2
C       L = 1, fluxes, L = 2 mean energies
        DO IL = 1,2
C        IL = 1, Northern Hemisphere, IL = 2; Southern Hemisphere
C
C        Determine the radius and center of the circles which describe
C        the oval-trough boundary, the oval maximum boundary and the oval
C        cap boundary.
C
         CALL GET_CIRCL(ERG(1,K,L,1,IL,1),ERG(1,K,L,2,IL,1),
     1   LAT(1,K,L,1,IL,1),MLT(1,K,L,1,IL,1),LAT(1,K,L,2,IL,1),
     2   MLT(1,K,L,2,IL,1),ERG(1,K,L,1,IL,2),ERG(1,K,L,2,IL,2),
     3   LAT(1,K,L,1,IL,2),MLT(1,K,L,1,IL,2),LAT(1,K,L,2,IL,2),
     4   MLT(1,K,L,2,IL,2),LAT0(1,K,L,IL),MLT0(1,K,L,IL),
     5   RAD0(1,K,L,IL),3,A(0,1,K),XKP(1,K,L,IL),IC)
C
         IF ((K .EQ. 1) .AND. (L .EQ. 1) .AND. (IL .EQ. 2)) THEN
            DO M1 = 1,2
               KP(M1) = 0.5*(XKP(M1,K,L,1) + XKP(M1,K,L,2))
            ENDDO
            DO M4 = 1,2
               DO M3 = 1,2
                  DO M2 = 1,2
                     DO M1 = 1,2
                        XKP(M1,M2,M3,M4) = KP(M1)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
C        Determine the maximum and minimum values for the parameter as a
C        function of KP
        ENDDO
       ENDDO
      ENDDO
      RETURN
      END
C
       SUBROUTINE GET_CIRCL(ERG1,ERG2,LAT1,MLT1,LAT2,MLT2,
     1 ERG3,ERG4,LAT3,MLT3,LAT4,MLT4,
     2 LAT0,MLT0,RAD0,JDIM,ATRM,KP,IX)
C
C  PURPOSE
C    To find the center and radius of the circles which describe the
C    precipitation boundaries and maximum.
C
C  METHOD
C    There are 5 distinct cases to consider:
C  1) There are four data points.
C       For each pair of data points, compute the magnetic latitude
C     and local time of the center of the circle which passes through
C     the two input points, and has a radius equal to the average of
C     the colatitudes of the two data points.  This is done in sub-
C     routine CENTER. Then take an average center point and radius.
C
C  2) There are three data points.
C       Compute the circle which passes through the three data points
C      exactly.
C
C  3) There are two data points.
C       Compute  the magnetic latitude and local time of the
C     center of a circle which passes through the two input data
C     points, and has a radius equal to the average of the two data
C     points. This is done in subroutine CENTER
C
C   4) There is only one data point. The center of the circle is set
C  at the magnetic pole and the radius is the colatitude of the point.
C
C   5) There are no data points... The center of the circle is set at
C  the magnetic pole, and the radius is computed from the Kp value:
C
C  poleward radius = 13.4 + 1.7*Kp
C  equatorward radius = 20.9 + 1.7*Kp
C  radius of maximum precipitation = 0.5*(poleward + equatorward)
C
C  INPUT PARAMETERS
C    NAME    TYPE  ARRAY  description
C    ERG1    REAL   (3)   Value of parameter at first crossing
C                    i  =  1) Equatorward value
C                          2) Value of maximum
C                          3) Poleward value
C    ERG2    REAL   (3)   Value of parameter at second crossing
C                    i  =  1) Equatorward value
C                          2) Value of maximum
C                          3) Poleward value
C    JDIM    INTEGER      Dimensions of the arrays
C    KP      REAL   (2)   The Kp values used if no data
C    LAT1    REAL   (3)   Magnetic latitude of first crossing
C                    i  =  1) Equatorward value
C                          2) Value of maximum
C                          3) Poleward value
C    LAT2    REAL   (3)   Magnetic latitude of second crossing
C                    i  =  1) Equatorward value
C                          2) Value of maximum
C                          3) Poleward value
C    MLT1    REAL   (3)   Magnetic local time of first crossing
C                    i  =  1) Equatorward value
C                          2) Value of maximum
C                          3) Poleward value
C    MLT2    REAL   (3)   Magnetic local time of second crossing
C                    i  =  1) Equatorward value
C                          2) Value of maximum
C                          3) Poleward value
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    LAT0    REAL       (3)    The Geomagnetic latitude corresponding
C                              to the circle center
C                       (i)
C                        i  =  1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    MLT0    REAL       (3)    The Geomagnetic local time corresponding
C                              to the circle center
C                       (i)
C                        i  =  1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    RAD0    REAL       (3)    The radius of the circle
C                       (i)
C                        i  =  1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    J       INTEGER           Do loop index
C    XLAT01  REAL              One value to average for LAT(J) if two
C                              CENTER calls made (4 data points)
C    XLAT02  REAL              One value to average for LAT(J) if two
C                              CENTER calls made (4 data points)
C    XMLT01  REAL              One value to average for MLT(J) if two
C                              CENTER calls made (4 data points)
C    XMLT02  REAL              One value to average for MLT(J) if two
C                              CENTER calls made (4 data points)
C    XRAD01  REAL              One value to average for RAD(J) if two
C                              CENTER calls made (4 data points)
C    XRAD02  REAL              One value to average for RAD(J) if two
C                              CENTER calls made (4 data points)
C
C  SUBROUTINES CALLED
C    NAME      description
C    CENTER    Gives the center and radius of a circle if there are two
C              data points
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.1   18-Nov-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby                  1.0 ==> Created
C     L. Brown        22-Sep-1994   1.0 ==> 1.0.9
C                                   Statements "EXTERNAL FPOLY" and
C                                   "REAL FPOLY" commented out because external
C                                   function FPOLY never used.
C     L. Brown        18-Nov-1994   1.0.9 ==> 1.1
C                                   Removed EKP from argument list.
C                                   Removed local variables THMX1 and DETKP.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C       EXTERNAL FPOLY
C       REAL FPOLY
       INCLUDE 'logicuni.inc'
C
C      I/O variable declaration
C
       INTEGER JDIM,IX
       REAL ERG1(JDIM),ERG2(JDIM),LAT1(JDIM),MLT1(JDIM),LAT2(JDIM),
     1 MLT2(JDIM),ERG3(JDIM),ERG4(JDIM),LAT3(JDIM),MLT3(JDIM),
     2 LAT4(JDIM),MLT4(JDIM),LAT0(JDIM),MLT0(JDIM),RAD0(JDIM),
!     3 ATRM(0:1,(JDIM+1)/2),KP((JDIM+1)/2),XLAT(4),XMLT(4),XERG(4)
     3 ATRM(0:1,JDIM),KP(JDIM),XLAT(4),XMLT(4),XERG(4)
C
C      Local variable declaration
C
       INTEGER J
       REAL XLAT01,XLAT02,XMLT01,XMLT02,RAD01,RAD02
C
       DO J = 1,JDIM
C
C       Count the number of valid data points and load the temporary
C       arrays
C
        IX = 0
        IF (ERG1(J) .GT. 0.0) THEN
         IX = IX+1
         CALL SET(XERG(IX),XLAT(IX),XMLT(IX),ERG1(J),LAT1(J),MLT1(J))
        ENDIF
        IF (ERG2(J) .GT. 0.0) THEN
         IX = IX+1
         CALL SET(XERG(IX),XLAT(IX),XMLT(IX),ERG2(J),LAT2(J),MLT2(J))
        ENDIF
        IF (ERG3(J) .GT. 0.0) THEN
         IX = IX+1
         CALL SET(XERG(IX),XLAT(IX),XMLT(IX),ERG3(J),LAT3(J),MLT3(J))
        ENDIF
        IF (ERG4(J) .GT. 0.0) THEN
         IX = IX+1
         CALL SET(XERG(IX),XLAT(IX),XMLT(IX),ERG4(J),LAT4(J),MLT4(J))
        ENDIF
C
C       Determine the boundary circle
C
        IF ( IX .EQ. 4) THEN
C
C        We have four data points... Call CENTER 2 times and take the
C        average
         CALL CENTER(XLAT(1),XMLT(1),XLAT(2),XMLT(2),XLAT01,XMLT01,
     1   RAD01)
         CALL CENTER(XLAT(3),XMLT(3),XLAT(4),XMLT(4),XLAT02,XMLT02,
     3   RAD02)
         LAT0(J) = 0.5*(XLAT01+XLAT02)
         MLT0(J) = 0.5*(XMLT01+XMLT02)
         RAD0(J) = 0.5*(RAD01+RAD02)
C
        ELSE IF (IX .EQ. 3) THEN
C
C        We have three data points... we may fit a circle exactly
C
         CALL FULL_CIRCLE(XLAT,XMLT,LAT0(J),MLT0(J),RAD0(J))
C
        ELSE IF ( IX .EQ. 2) THEN
C
C        We have two data points... find the circle which goes through
C        both the points and has a radius the average of the colatitudes.
C
         CALL CENTER(XLAT(1),XMLT(1),XLAT(2),XMLT(2),LAT0(J),MLT0(J),
     1   RAD0(J))
C
        ELSE
C        We have either one or no data points...set the center of the
C        circle at the magnetic pole
         LAT0(J)=90.0
         MLT0(J)=0.0
C
         IF (IX .EQ. 1) THEN
          IF (XLAT(1) .LE. 0.0) THEN
           WRITE(LUSTDERR,*) LAT1(J),ERG1(J)
           STOP 'ERROR 1 IN GET_CIRCL'
          ELSE
           RAD0(J) = 90.-XLAT(1)
          ENDIF
         ELSE
C
C         We have no data points...the radius is set by the real Kps
          IF ( J .EQ. 1) RAD0(1) = ATRM(0,1) + KP(1)*ATRM(1,1)
          IF ( J .EQ. 3) THEN
           RAD0(3) = ATRM(0,2) + KP(2)*ATRM(1,2)
           IF ((ERG1(2) .LE. 0.0) .AND. (ERG2(2) .LE. 0.0)) THEN
            RAD0(2) = RAD0(3)+((RAD0(1)-RAD0(3))/2.)
           ENDIF
          ENDIF
         ENDIF
        ENDIF
       ENDDO
       KP(1) = (RAD0(1) - ATRM(0,1))/ATRM(1,1)
       IF (JDIM .GT. 1) KP(2) = (RAD0(3) - ATRM(0,2))/ATRM(1,2)
       RETURN
       END
      SUBROUTINE CENTER(MLAT1,MLT1,MLAT2,MLT2,MLAT0,MLT0,R)
C
C  PURPOSE
C     Determining of the location of the center of
C     a circle and radius from two points on the circle given as
C     magnetic latitude and local time pairs.
C
C  METHOD
C     First put the data points on a cartesian coordinate
C     system with the magnetic pole being the origin:
C     Let
C     Phi = (Magnetic local time)   in degrees
C        1                       1
C     Phi = (Magnetic local time)   in degrees
C        2                       2
C so
C     x  = 90. - MLAT *Cos(Phi )
C      1             1        1
C
C     y  = 90. - MLAT *Sin(Phi )
C      1             1        1
C
C     x  = 90. - MLAT *Cos(Phi )
C      2             2        2
C
C     y  = 90. - MLAT *Sin(Phi )
C      2             2        2
C
C
C  Let the radius be the average of the two colatitudes
C  R = 90. - (MLAT  + MLAT )/2.
C                 1       2
C
C Then
C
C           2          2   2
C    (x -x ) + (y - y ) = R
C      1  0      1   0
C and
C
C           2          2   2
C    (x -x ) + (y - y ) = R
C      2  0      2   0
C
CLetting
C    (x -x ) = U
C      1  0
C
C    (y - y ) = V
C      1   0
C
C    (x -x ) = Dx
C      2  1
C
C    (y - y ) = Dy
C      2   1
C
C Then
C         2          2   2    2   2
C  (U +Dx)  + (V +Dy) = R  = U + V
C or
C             2              2           2   2   2
C  2.*U*Dx+ Dx  +2.*V*Dy + Dy = 0.0 and V = R - U
C
C or
C             2    2              2   2
C  2.*U*Dx+ Dx + Dy = 2.*Dy*SQRT(R - U )
C
C                   2    2
C or, letting a = Dx + Dy
C
C   2                     2       2  2
C  U *4.*a + 4.*U*Dx*a + a - 4.*Dy *R = 0.0
C
C                             2  2          2  2  2
C Hence, U = {-Dx*a +- SQRT[Dx *a + a*(4.*Dy *R -a )]}/(2.*a)
C
C  x = x -U
C   0   1
C                                                      2
C and substituting back into the first equation gives V . However,
C   2          2           2
C  V = (y - y )  = (y - y )
C        1  0       0   1
C Hence, there are two values for y  for each of the x s. The values
C                                  0                  0
C are re inserted into the second equation to determine which two are the
C valid y s. Therefore, there are two solutions obtained. The solution se-
C        0
C lected is the one which puts the circle center closer to the magnetic
C pole. The valid solution is then put onto the magnetic latitude and local
C time grid, and returned to the calling program.
C
C  INPUT PARAMETERS
C    NAME    TYPE  ARRAY  description
C    MLAT1   REAL         Magnetic latitude of first crossing
C    MLAT2   REAL         Magnetic latitude of second crossing
C    MLT1    REAL         Magnetic local time of first crossing
C    MLT2    REAL         Magnetic local time of second crossing
C
C  OUTPUT PARAMETERS
C    NAME    TYPE ARRAY  description
C    LAT0    REAL        The Geomagnetic latitude corresponding
C    MLT0    REAL        The Geomagnetic local time corresponding
C    R       REAL        The radius of the circle
C
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    A1      REAL              DX**2 + DY**2
C    DX      REAL              X2 - X1
C    DY      REAL              Y2 - Y1
C    R1      REAL              Radius of point MLAT1, MLT1
C    R2      REAL              Radius of point MLAT2, MLT2
C    U1      REAL              The first solution for (X1-X0)
C    U2      REAL              The second solution for (X1-X0)
C    V1      REAL              R**2 - (X2-XA)**2 -(Y2-YA1)**2 used to check
C                              the validity of the solution (XA,YA1)
C    V2      REAL              R**2 - (X2-XA)**2 -(Y2-YA2)**2 used to check
C                              the validity of the solution (XA,YA2)
C    V3      REAL              R**2 - (X2-XB)**2 -(Y2-YB1)**2 used to check
C                              the validity of the solution (XB,YB1)
C    V4      REAL              R**2 - (X2-XB)**2 -(Y2-YB2)**2 used to check
C                              the validity of the solution (XB,YB1)
C    XA      REAL              The first solution for X0
C    XB      REAL              The second solution for X0
C    YA1     REAL              The solution to Y1 - Y0 = SQRT(R**2 -U1**2)
C    YA2     REAL              The solution to Y0 - Y1 = SQRT(R**2 -U1**2)
C    YB1     REAL              The solution to Y1 - Y0 = SQRT(R**2 -U2**2)
C    YB2     REAL              The solution to Y0 - Y1 = SQRT(R**2 -U2**2)
C    X0      REAL              X cartesian coordinate of the center point
C                              selected
C    Y0      REAL              Y cartesian coordinate of the center point
C                              selected
C    X1      REAL              X cartesian coordinate of the point MLAT1,MLT1
C    Y1      REAL              Y cartesian coordinate of the point MLAT1,MLT1
C    X2      REAL              X cartesian coordinate of the point MLAT2,MLT2
C    Y2      REAL              Y cartesian coordinate of the point MLAT2,MLT2
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
C     1.2  31-Dec-1990
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     EPS = .003  If the Denominator is less than 0, but greater than
C     -EPS, then the denominator is set to 0. This allows circles that
C     'nearly' fit to be used for boundaries.
C
      REAL EPS
      PARAMETER (EPS =3.0E-3)
      INCLUDE 'const.inc'
      REAL*8 HTOR
      PARAMETER(HTOR=PI/12.0D0)
      INCLUDE 'logicuni.inc'
C
      REAL MLAT0,MLAT1,MLAT2,MLT0,MLT1,MLT2
C
      REAL R,R1,X1,Y1,X2,Y2,THETA,DX,DY,A1,U1,U2,XA,XB,YA,YB
      REAL YB1,YB2,YA1,YA2,V1,V2,V3,V4,X0,Y0
C
C     Define the radius of the circle to be the average of the colatitudes.
C
      R = (180.-MLAT1-MLAT2)/2
C
C     Put the first point on a cartesian coordinate system
C
      R1 = 90.-MLAT1
      THETA = (MLT1-6.)*HTOR
      X1 = R1*COS(THETA)
      Y1 = R1*SIN(THETA)
C
C     Put the second point on a cartesian coordinate system
C
      R1 = 90.-MLAT2
      THETA = (MLT2-6.)*HTOR
      X2 = R1*COS(THETA)
      Y2 = R1*SIN(THETA)
C
C     Compute the variables needed for the quadratic described above
C
      DX = X2-X1
      DY = Y2-Y1
      A1 =  DX**2 +DY**2
c$$$      U1 = SQRT(4.*(R**2)*(DY**2)+A1*(DX**2)-(A1**2))/(2.*SQRT(A1))
c$$$      U1 = U1-(DX/2.)
c$$$      U2 = -SQRT(4.*(R**2)*(DY**2)+A1*(DX**2)-(A1**2))/(2.*SQRT(A1))
c$$$      U2 = U2-(DX/2.)
      U1 = SQRT(4.*(R**2)*(DY**2)+A1*(DX**2)-(A1**2))/(2.*SQRT(A1))
      U2 = -U1
      U1 = U1-(DX/2.)
      U2 = U2-(DX/2.)
C
C     Get the X values for the center
C
      XA = X1-U1
      XB = X1-U2
C
C     Get the y values that satisfy equation 1
C
      YB1 = Y1-SQRT(R**2 -(X1-XB)**2)
      YB2 = Y1+SQRT(R**2 -(X1-XB)**2)
      YA1 = Y1-SQRT(R**2 -(X1-XA)**2)
      YA2 = Y1+SQRT(R**2 -(X1-XA)**2)
C
C     Compute the difference between the input radius and the radius
C     of the computed circle
C
      V1 = R-SQRT((X2-XA)**2+(Y2-YA1)**2)
      V2 = R-SQRT((X2-XB)**2+(Y2-YB1)**2)
      V3 = R-SQRT((X2-XA)**2+(Y2-YA2)**2)
      V4 = R-SQRT((X2-XB)**2+(Y2-YB2)**2)
C
C     As we took the square of an equation for the derivation, two of
C     the solutions may be spurious... check for this
C
      IF (MAX(ABS(V1),ABS(V2),ABS(V3),ABS(V4)) .GT. EPS) THEN
       IF (ABS(V1) .LE. EPS) THEN
        YA = YA1
       ELSE IF (ABS(V3) .LE. EPS) THEN
        YA = YA2
       ELSE
#ifdef DEBUG
        WRITE(LUSTDERR,*) ' ERROR IN YA'
#endif
       ENDIF
       IF (ABS(V2) .LE. EPS) THEN
        YB = YB1
       ELSE IF (ABS(V4) .LE. EPS) THEN
        YB = YB2
       ELSE
#ifdef DEBUG
        WRITE(LUSTDERR,*) ' ERROR IN YB'
#endif
       ENDIF
      ELSE IF (MAX(ABS(YA1-YB1),ABS(YA2-YB2)) .LT. EPS ) THEN
C
C      The equation had nearly 0.0 for a discriminant... choose one
C      Y value from each pair
       YA = YA1
       YB = YB2
      ELSE
#ifdef DEBUG
       WRITE(LUSTDERR,*) ' Error '
#endif
      ENDIF
C
C     Choose the value closest to the pole ( nearest the origin in
C     this cartesian coordinate system)
C
      IF (XA**2+YA**2 .LT. XB**2+YB**2) THEN
       X0 = XA
       Y0 = YA
      ELSE
       X0 = XB
       Y0 = YB
      ENDIF
      V1 = SQRT((X1-XA)**2+(Y1-YA)**2)
      V2 = SQRT((X2-XA)**2+(Y2-YA)**2)
      V3 = SQRT((X1-XB)**2+(Y1-YB)**2)
      V4 = SQRT((X2-XB)**2+(Y2-YB)**2)
C
C     And put the selected data point back onto the magnetic latitude,local
C     time grid.
C
      MLAT0 = SQRT(X0**2 +Y0**2)
      MLAT0 = 90.-MLAT0
      IF (ABS(Y0) .LT. 1.0E5*ABS(X0)) THEN
       MLT0 = ATAN(Y0/X0)
       MLT0 = MLT0 /HTOR +6.
      ELSE
       MLT0 = 12.
      ENDIF
      IF (X0 .LT. 0.0) MLT0 = MLT0 + 12.
      IF (MLT0 .GT. 24) MLT0 = MLT0-24.
      IF (MLT0 .LT. 0) MLT0 = MLT0+24.
      RETURN
      END
      SUBROUTINE SET(XERG,XLAT,XMLT,ERG1,XLAT1,XMLT1)
C
C  PURPOSE
C     Set XERG,XLAT and XMLT equal to ERG1,XLAT1 and XMLT1, respectively
C
C  METHOD
C     No discussion
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ERG1    REAL
C    XLAT1   REAL
C    XMLT1   REAL
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ERG     REAL
C    XLAT    REAL
C    XMLT    REAL
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
C     1.1   22-Jul-1991
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
      REAL XERG,XLAT,XMLT,ERG1,XLAT1,XMLT1
C
      XERG = ERG1
      XLAT = XLAT1
      XMLT = XMLT1
      RETURN
      END
C
      SUBROUTINE FULL_CIRCLE(XLAT,XMLT,LAT0,MLT0,RAD0)
C
C  PURPOSE
C     To compute the center and radius of a circle which contains the
C  three points (XLAT,XMLT) as boundary points
C
C  METHOD
C     Put the XLAT,XMLT points on a caresian coordinate grid, with 0,0
C  being the origin. Compute the center and radius of the circle in the
C  following way:
C     If X0 and Y0 are the X and Y coordinates of the center, then
C  (X(i)-X0)**2 + (Y(i)-Y0)**2 = R**2
C  Where R is the radius of the circle.
C  Letting R2(i) = (x(i)**2) + Y(i)**2) then
C
C  2.*( ( X(2) - X(1))*X0 + (Y(2) - Y(1))*Y0) = R2(2) - R2(1)
C  2.*( ( X(3) - X(2))*X0 + (Y(3) - Y(2))*Y0) = R2(3) - R2(2)
C
C  Solving the equations for X0 and Y0 give the determinant and
C  equations in Code.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    XLAT    REAL        (3)   The latitudes of the known boundaries
C    XMLT    REAL        (3)   The local times of the known boundaries
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    LAT0    REAL               The latitude of the center of the circle
C    MLT0    REAL               The local time of the center of the circle
C    RAD0    REAL               The radius of the circle
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    DET     REAL              The determinant of the equation
C    X0      REAL              The X coordinate of the center in the cartesian
C                              coordinate sytem.
C    Y0      REAL              The Y coordinate of the center in the cartesian
C                              coordinate sytem.
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
C     1.5  01-Jun-1991
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     (PI = 3.1415926...)
C
      INCLUDE 'const.inc'
      REAL*8 HTOR
      PARAMETER(HTOR=PI/12.)
C
C     I/O variable declarations
C
      REAL XLAT(3),XMLT(3),LAT0,MLT0,RAD0,X(3),Y(3),ARG(3),R2(3)
C
C     Local variable declarations
C
      INTEGER I
      REAL DET,X0,Y0
C
C     Put the known boundary points from a Latitude-local time grid
C     to a cartesian coordinate system
C
      DO I = 1,3
       ARG(I) = HTOR*(XMLT(I)-6.)
       X(I) = (90.-XLAT(I))*COS(ARG(I))
       Y(I) = (90.-XLAT(I))*SIN(ARG(I))
       R2(I) = X(I)**2 + Y(I)**2
      ENDDO
C
C     Compute the Determinant
C
      DET = 4.*((X(2)-X(1))*(Y(3)-Y(2)) - (Y(2)-Y(1))*(X(3)-X(2)))
C
C     solve the equations for the center in the cartesian coordinate system
C
      X0 = 2*((Y(3)-Y(2))*(R2(2)-R2(1)) + (Y(1)-Y(2))*(R2(3)-R2(2)))
      X0 = X0/DET
      Y0 = 2*((X(2)-X(3))*(R2(2)-R2(1)) + (X(2)-X(1))*(R2(3)-R2(2)))
      Y0 = Y0/DET
C
C     Compute the latitude of the center
C
      LAT0 = 90.-(X0**2+Y0**2)**0.5
      RAD0 = SQRT((X(1)-X0)**2 + (Y(1)-Y0)**2)
C
C     Compute the local time of the center point, checking the special cases
C
      IF (1.E-12*ABS(Y0) .LE. ABS(X0)) THEN
       MLT0 = ATAN(Y0/X0)/HTOR + 6.
C
C      This is necessary since ATAN for the VAX has range (-PI/2 to PI/2)
C
       IF (X0 .LT. 0.0) MLT0 = MLT0 + 12.
       IF ( MLT0 .GT. 24.) MLT0 = MLT0-24.
       IF ( MLT0 .LT. 0.0) MLT0 = MLT0+24
      ELSE IF ( Y0 .GT. 0.0) THEN
       MLT0 = 12.
      ELSE
       MLT0 = 0.0
      ENDIF
      RETURN
      END
C
