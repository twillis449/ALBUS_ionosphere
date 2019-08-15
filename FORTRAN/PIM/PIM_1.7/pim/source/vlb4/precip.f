      SUBROUTINE PREGION(CGMLT,XLAT,XMLT0,RADIUS,REGB)
C
C  PURPOSE
C     To determine the latitude of the equatorward boundary, maximum
C  and poleward boundary at a given local time given the latitude and
C  local time corresponding to the center of the circular boundary and
C  the radius of this circle.
C
C  METHOD
C    Use the Pythagorean theorem...
C     2  2                       2
C    R +R  - 2.*R R *Cos(Phi) = R
C     0  1       0 1
C
C   Where R = The colatitude of the circle center
C          0
C         R = The circle radius in degrees
C
C       Phi = The current local time - local time of the circle center,
C             in degrees
C
C        R = The colatitude of the boundary at the current local time
C         1
C
C        2                          2   2
C       R  - R *(2.*R *Cos(phi)) + R - R = 0
C        1    1      0              0
C Or
C                               2    2        2   2
C      R = R *Cos(Phi) +- SQRT{R *Cos (Phi) +R - R }
C       1   0                   0                 0
C
C     Choose the postive solution ...in the case of two positive
C   solutions, The smaller one is chosen.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    CGMLT   REAL              The current local time
C    RAD0    REAL       (3)    The radius of the circular boundary
C                              1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    XLAT    REAL       (3)    The latitude of the center of the circular
C                              boundary
C                              1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C    XMLT0   REAL       (3)    The local time of the center of the circular
C                              boundary
C                              1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    REGB    REAL       (3)    The latitude of the circular boundary at local
C                              time CGMLT
C                              1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    J       INTEGER           Do loop index
C    SOL1    REAL              One solution of the quadratic equation
C    SOL2    REAL              One solution of the quadratic equation
C    TH1     REAL              The difference between the current local
C                              time and the local time of the center of
C                              the circular  boundary, in degrees, = Phi
C                              in description above.
C    XLT0    REAL              The colatitude of the center of the circ-
C                              ular boundary, in degrees, = R  in description
C                              above.                        0
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
C     1.0.6   01-Dec-1990
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     EPS = -1.E-3... The maximum a discriminant may go negative
C     and be considered valid (Set equal to 0.0)
C
      REAL EPS
      PARAMETER (EPS = -1.E-3)
      REAL XLAT(3),XMLT0(3),RADIUS(3),REGB(3),DISC,XLT0,TH1,XCT1
      REAL SOL1,SOL2,CGMLT
      INTEGER J
      INCLUDE 'const.inc'
      REAL*8 HTOR
      PARAMETER(HTOR=PI/12.0D0)
      INCLUDE 'logicuni.inc'
C
C
      DO J = 1,3
C      Compute the discriminant from the equation described above...
C
       XLT0 = 90.-XLAT(J)
       TH1 = (CGMLT-XMLT0(J))*HTOR
       XCT1 = XLT0*COS(TH1)
       DISC = XCT1**2 - (XLT0**2-RADIUS(J)**2)
C
       IF ( DISC .LT. EPS) THEN
C
C       There is an error
        WRITE(LUSTDERR,*)' Index, lat,local time radius,discriminant = '
        WRITE(LUSTDERR,*) CGMLT,J,XLAT(J),XMLT0(J),RADIUS(J),DISC
        STOP 'NEGATIVE DISCRIMINANT IN PREGION'
C
       ELSE
C
C       Compute the two solutions
C
        IF ( DISC .LT. 0.0) DISC = 0.0
        SOL1 = XCT1 +SQRT(DISC)
        SOL2 = XCT1 -SQRT(DISC)
        IF (MIN(SOL1,SOL2) .GT. 0) THEN
C        Take the minimum
         REGB(J)=MIN(SOL1,SOL2)
C
C        Otherwise , take the postitve one
        ELSE IF (SOL1 .GT. 0) THEN
         REGB(J)=SOL1
        ELSE IF (SOL1 .GT. 0) THEN
         REGB(J)=SOL2
        ELSE
         WRITE(LUSTDERR,*)' Index, lat,local time radius = '
         WRITE(LUSTDERR,*)CGMLT,J,XLAT(J),XMLT0(J),RADIUS(J)
         STOP 'NEGATIVE SOLUTION IN PREGION'
        ENDIF
C       Return the latitude
C
        REGB(J) = 90.-REGB(J)
       ENDIF
      ENDDO
      RETURN
      END
