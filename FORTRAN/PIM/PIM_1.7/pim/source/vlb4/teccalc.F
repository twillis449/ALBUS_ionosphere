      SUBROUTINE TECCLC(NALT,ALT,E,TEC)
C
C  PURPOSE
C     To calculate the total electron content.
C
C  METHOD
C     The total electron content is determined by integrating the electron
C     density over its entire altitude range, using Romberg integration.
C
C  INPUT PARAMETERS
C     ALT      The altitude grid, in km
C     E        The electron density, in cm-3
C     NALT     The number of points in the altitude grid
C
C  OUTPUT PARAMETERS
C     TEC      The total electron content using Romberg integration, in cm-2
C
C  LOCAL VARIABLES
C     EZ       The electron density on the Z grid, in cm-3
C     EZLN     The natural logarithm of the electon density on the Z grid
C     I        A loop counter
C     IABV     The index on the Z grid of the boundary between the peak and
C              upper integration regions
C     IBLW     The index on the Z grid of the boundary between the peak and
C              lower regions of integration
C     IPK      The index on the Z grid of the peak electron density
C     NZ       The number of altitude points in the Z grid
C     TECABV   The integrated electron density above the peak region, in km cm-3
C     TECBLW   The integrated electron density below the peak region, in km cm-3
C     TECPK    The integrated electron density in the peak region, in km cm-3
C     Z        The stored altitude grid, with altitude increasing with
C              increasing index, in km
C
C  SUBROUTINES REQUIRED
C     EFZ      Calculates the electron density at a given altitude
C     QROMB    Romberg integration on a closed interval
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     385 Elliot Street
C     Newton, MA  02164
C     [617]964-7553
C
C  VERSION
C     1.0.6    12-April-1989
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     KMTOCM   The conversion factor from kilometers to centimeters
C
      INCLUDE 'array.inc'
      REAL KMTOCM
      PARAMETER(KMTOCM=1.E5)
C
      REAL Z(MAX_ALT_NUM),EZ(MAX_ALT_NUM),EZLN(MAX_ALT_NUM)
      INTEGER NZ
      COMMON/TECCALC/NZ,Z,EZ,EZLN
C
      INTEGER NALT
      REAL ALT(NALT),E(NALT)
      REAL TEC
      REAL TECABV,TECBLW,TECPK
      INTEGER I,IPK,IABV,IBLW
C
      EXTERNAL EFZ
C
C  Store the altitude grid, the electron density, and the natural logarithm
C  of the electron density such that the altitude grid is increasing with
C  increasing index
C
#ifdef DEBUG
      IF(NALT.GT.MAX_ALT_NUM) THEN
         WRITE(LUSTDERR,*) 'Error: too many points in NALT', NALT
         STOP 'TECCALC'
      ENDIF
#endif
      NZ=NALT
      IF(ALT(1) .LT. ALT(NALT)) THEN
         DO I=1,NALT
            Z(I)=ALT(I)
            EZ(I)=E(I)
            EZLN(I)=LOG(E(I))
         ENDDO
      ELSE
         DO I=1,NALT
            Z(NALT-I+I)=ALT(I)
            EZ(NALT-I+1)=E(I)
            EZLN(NALT-I+1)=LOG(E(I))
         ENDDO
      ENDIF
C
C  Calculate the total electron content using Romberg integration
C
C  Find the index of the peak electron density
C
      IPK=1
      DO I=1,NZ
         IF(EZ(I) .GT. EZ(IPK)) IPK=I
      ENDDO
C
C  If the electron density peaks at the bottom or top of the grid, then the
C  total electron content is calculated by performing a single integration
C  of the electron density over the entire altitude range
C
      IF((IPK .EQ. 1) .OR. (IPK .EQ. NZ)) THEN
         CALL QROMB(EFZ,Z(1),Z(NZ),TEC)
C
C  If the electron density peaks at an internal point of the altitude grid,
C  then the electron density is integrated using up to three separate intervals:
C  a region around the peak, and regions above and below the peak region
C
      ELSE
C
C  Find the highest altitude point where the electron density is a specified
C  fraction of the peak electron density; this defines the boundary between the
C  peak and upper integration regions
C
         IABV=IPK+1
         DO I=IPK+1,NZ
            IF(EZ(I) .GE. .1*EZ(IPK)) IABV=I
         ENDDO
C
C  If there are an insufficient number of points in the upper region to
C  integrate the electron density, then the upper boundary of the peak region
C  is extended to include the upper region; otherwise, the electron density
C  is integrated in the upper region
C
         IF(IABV .GT. NZ-5) THEN
            IABV=NZ
            TECABV=0
         ELSE
            CALL QROMB(EFZ,Z(IABV),Z(NZ),TECABV)
         ENDIF
C
C  Find the lowest altitude point where the electron density is a specified
C  fraction of the peak electron density; this defines the boundary between the
C  peak and lower integration regions
C
         IBLW=IPK-1
         DO I=IPK-1,1,-1
            IF(EZ(I) .GE. .1*EZ(IPK)) IBLW=I
         ENDDO
C
C  If there are an insufficient number of points in the lower region to
C  integrate the electron density, then the lower boundary of the peak region
C  is extended to include the lower region; otherwise, the electron density
C  is integrated in the lower region
C
         IF(IBLW .LT. 5) THEN
            IBLW=1
            TECBLW=0
         ELSE
            CALL QROMB(EFZ,Z(1),Z(IBLW),TECBLW)
         ENDIF
C
C  The electron density is integrated in the peak region
C
         CALL QROMB(EFZ,Z(IBLW),Z(IABV),TECPK)
C
C  The total electron content is the sum of the integrated electron densities
C  of the upper, peak, and lower regions
C
         TEC=TECBLW+TECPK+TECABV
c     write (*,77) iblw,iabv,tecblw/1.e8,tecpk/1.e8,tecabv/1.e8,tec/1.e8
c77	format (2i5,4f11.6)
C
      ENDIF
C
C  Convert the total electron content from km cm-3 to cm-2
C
      TEC=TEC*KMTOCM
C
      RETURN
      END
      FUNCTION EFZ(ALT)
C
C  PURPOSE
C     To calculate the electron density as a function of altitude.
C
C  METHOD
C     No discussion
C
C  INPUT PARAMETERS
C     ALT      The altitude
C
C  OUTPUT PARAMETERS
C     EFZ      The electron density at the altitude
C
C  LOCAL VARIABLES
C     H        The scale height of the electron density at the altitude
C
C  SUBROUTINES REQUIRED
C     LINTRP   Linear interpolation and extrapolation
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
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        12-Apr-1994   1.0.6
C     L. Brown        25-Sep-1995   1.0.6 ==> 1.3
C                                   Replaced quadratic interpolation with
C                                   linear interpolation to avoid overestimate
C                                   of TEC for pathological electron density
C                                   profiles.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INCLUDE 'array.inc'
      REAL Z(MAX_ALT_NUM),EZ(MAX_ALT_NUM),EZLN(MAX_ALT_NUM)
      INTEGER NZ
      COMMON/TECCALC/NZ,Z,EZ,EZLN
C
      REAL*8 EFZ
      REAL*8 ALT
      REAL H
C     FUNCTIONS
      REAL*8 LINTRP8

C
C  The altitude grid is decreasing with increasing index
C
      IF(Z(1) .GT. Z(NZ)) THEN
C
C  If the altitude is above the grid, assume a constant scale height of
C  the electron density, determined by the top two grid points
C
         IF(ALT .GT. Z(1)) THEN
            H=(Z(1)-Z(2))/(EZLN(2)-EZLN(1))
            EFZ=EZLN(1)-(ALT-Z(1))/H
C
C  If the altitude is below the grid, assume a constant scale height of
C  the electron density, determined by the bottom two grid points
C
         ELSE IF(ALT .LT. Z(NZ)) THEN
            H=(Z(NZ-1)-Z(NZ))/(EZLN(NZ)-EZLN(NZ-1))
            EFZ=EZLN(NZ)-(ALT-Z(NZ))/H
C
C  If the altitude is enclosed by the grid boundaries, interpolate on the
C  logarithm of the density at the altitude of the point
C
         ELSE
            EFZ=LINTRP8(NZ,Z,EZLN,ALT)
         ENDIF
C
C The altitude grid is increasing with increasing index
C
      ELSE
C
C  If the altitude is above the grid, assume a constant scale height of
C  the electron density, determined by the top two grid points
C
         IF(ALT .GT. Z(NZ)) THEN
            H=(Z(NZ)-Z(NZ-1))/(EZLN(NZ-1)-EZLN(NZ))
            EFZ=EZLN(NZ)-(ALT-Z(NZ))/H
C
C  If the altitude is below the grid, assume a constant scale height of
C  the electron density, determined by the bottom two grid points
C
         ELSE IF(ALT .LT. Z(1)) THEN
            H=(Z(2)-Z(1))/(EZLN(1)-EZLN(2))
            EFZ=EZLN(1)-(ALT-Z(1))/H
C
C  If the altitude is enclosed by the grid boundaries, interpolate on the
C  logarithm of the density at the altitude of the point
C
         ELSE
            EFZ=LINTRP8(NZ,Z,EZLN,ALT)
         ENDIF
C
      ENDIF
C
C  Convert the logarithm of the density to a density
C
      EFZ=EXP(EFZ)
C
      RETURN
      END
      SUBROUTINE LINTRP(NIN,XIN,YIN,NOUT,XOUT,YOUT)
C
C  PURPOSE
C     To linearly interpolate a tabulated function onto a grid.
C
C  METHOD
C     An input tabulated function is given on an input grid.  The input grid is
C     assumed to be monotonic.  An output tabulated function is desired on an
C     output grid.  Each output grid point is located on the input grid.  If
C     the output grid point is internal to the input grid, then the two
C     bracketing input grid points are used to linearly interpolate the input
C     tabulated function onto the output grid point.  If the output grid point
C     is external to input grid, then the two nearest input grid points are
C     used to linearly extrapolate the input tabulated function onto the output
C     grid point.
C
C  INPUT PARAMETERS
C     NIN      The size of the input grid, assumed to be > 1
C     NOUT     The size of the output grid
C     XIN      The input grid, assumed to be monotonic
C     XOUT     The output grid
C     YIN      The input tabulated function
C
C  OUTPUT PARAMETERS
C     YOUT     The output tabulated function
C
C  LOCAL VARIABLES
C     I        A loop counter
C     I1       The index of the first input grid point to be used for the
C              interpolation
C     I2       The index of the second input grid point to be used for the
C              interpolation
C     IX       The location of the output grid point on the input grid
C
C  SUBROUTINES REQUIRED
C     LOCATE   Locates a point on a grid
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
C     1.5   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        25-Sep-1995   1.5 ==> Created
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER NIN,NOUT
      INTEGER I,IX,I1,I2
      REAL XIN(NIN),YIN(NIN),XOUT(NOUT),YOUT(NOUT)
C
C  Loop over the output grid
C
      DO 10 I=1,NOUT
C
C  Locate the output grid point on the input grid.  The result is an index IX
C  on the input grid.  If the input grid is monotonically increasing, then
C  XIN(IX) < XOUT(I) <= XIN(IX+1); if the input grid is monotonically
C  decreasing, then XIN(IX+1) < XOUT(I) <= XIN(IX).
C
         CALL LOCATE(XIN,NIN,XOUT(I),IX)
C
C  Find the two input grid points that are closest to the output grid point.
C  Note the special handling for output grid points beyond or near the ends
C  of the input grid.
C
         IF(IX .EQ. 0) THEN
            I1=1
            I2=2
         ELSE IF(IX .EQ. NIN) THEN
            I1=NIN-1
            I2=NIN
         ELSE
            I1=IX
            I2=IX+1
         ENDIF
C
C  Linearly interpolate the input function onto the output grid point
C
         YOUT(I)=YIN(I1)
     &          +(YIN(I2)-YIN(I1))*(XOUT(I)-XIN(I1))/(XIN(I2)-XIN(I1))
C
C  End the loop over the output grid
C
   10 CONTINUE
C
      RETURN
      END
      FUNCTION LINTRP8(N,X,Y,XOUT)
C
C  PURPOSE
C     To linearly interpolate a tabulated function onto a grid.
C
C  METHOD
C     An input tabulated function is given on an input grid.  The input grid is
C     assumed to be monotonic.  An output tabulated function is desired on an
C     output grid.  Each output grid point is located on the input grid.  If
C     the output grid point is internal to the input grid, then the two
C     bracketing input grid points are used to linearly interpolate the input
C     tabulated function onto the output grid point.  If the output grid point
C     is external to input grid, then the two nearest input grid points are
C     used to linearly extrapolate the input tabulated function onto the output
C     grid point.
C
C  INPUT PARAMETERS
C     N       The size of the input grid, assumed to be > 1
C     X        The input grid, assumed to be monotonic
C     XOUT     The output position
C     Y        The input tabulated function
C
C  OUTPUT PARAMETERS
C     LINTRP8  The output tabulated function
C
C  LOCAL VARIABLES
C     I        A loop counter
C     I1       The index of the first input grid point to be used for the
C              interpolation
C     I2       The index of the second input grid point to be used for the
C              interpolation
C     IX       The location of the output grid point on the input grid
C
C  SUBROUTINES REQUIRED
C     LOCATE   Locates a point on a grid
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
C     1.5   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        25-Sep-1995   1.5 ==> Created
C     2005 Sep 09  James M Anderson  --JIVE  start from LINTRP
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER N
      REAL X(N),Y(N)
      REAL*8 LINTRP8
      REAL*8 XOUT

      INTEGER JL,JU,JM,J
C
      JL=0
      JU=N+1
      IF(X(N) .GT. X(1))  THEN
 10      IF(JU-JL .GT. 1) THEN
            JM=(JU+JL)/2
            IF(XOUT .GT. X(JM)) THEN
               JL=JM
            ELSE
               JU=JM
            ENDIF
            GOTO 10
         ENDIF
      ELSE
 20      IF(JU-JL .GT. 1) THEN
            JM=(JU+JL)/2
            IF(XOUT .GT. X(JM)) THEN
               JU=JM
            ELSE
               JL=JM
            ENDIF
            GOTO 20
         ENDIF
      ENDIF
      J=JL
C
      IF(J .LE. 0) THEN
         JL=1
         JU=2
      ELSE IF(J .GE. N) THEN
         JL=N-1
         JU=N
      ELSE
         JL=J
         JU=J+1
      ENDIF
C
C  Linearly interpolate the input function onto the output grid point
C
      LINTRP8=Y(JL)
     &     +(Y(JU)-Y(JL))*(XOUT-X(JL))/(X(JU)-X(JL))
C
      RETURN
      END
      SUBROUTINE LOCATE(XX,N,X,J)
C
C  PURPOSE
C     To find the position of a value X in an ordered array XX.
C
C  METHOD
C     Given an array XX of length N, and given a value X, index J is returned
C     such that X is between X(J) and X(J+1).  XX must be monotonic, either
C     increasing or decreasing.  J=0 or J=N is returned to indicate that X is
C     out of range.
C     Bisection is used to determine J.  If XX is monotonically increasing,
C     then XX(J) < X <= XX(J+1); if XX is monotonically decreasing, then
C     XX(J+1) < X <= XX(J).
C
C  INPUT PARAMETERS
C     N        The length of array XX
C     X        The value to be located in array XX
C     XX       An ordered array, either monotonically increasing or decreasing
C
C  OUTPUT PARAMETERS
C     J        The location of value X in array XX
C
C  LOCAL VARIABLES
C     JL       The lower index stored by the bisection
C     JM       The middle index stored by the bisection
C     JU       The upper index stored by the bisection
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
C     385 Elliot Street
C     Newton, MA  02164
C     [617]964-7553
C
C  VERSION
C     1.5   30-September-1996
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        28-Jun-1995   Created
C     L. Brown        30-Sep-1996   1.5
C                                   Modified logic to eliminate .EQV. logical
C                                   operator, which caused a run-time access
C                                   violation error under Microsoft Fortran
C                                   PowerStation.
C
C  REFERENCES
C     [1] Press, W. H., B. P. Flannery, S. A. Teukolsky, and W. T. Vettering,
C         'Numerical Recipes', Cambridge University Press, Cambridge, 1986.
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER N
      INTEGER JL,JU,JM,J
      REAL XX(N)
      REAL X
C
      JL=0
      JU=N+1
   10 IF(JU-JL .GT. 1) THEN
         JM=(JU+JL)/2
         IF(XX(N) .GT. XX(1))  THEN
            IF(X .GT. XX(JM)) THEN
               JL=JM
            ELSE
               JU=JM
            ENDIF
         ELSE
            IF(X .GT. XX(JM)) THEN
               JU=JM
            ELSE
               JL=JM
            ENDIF
         ENDIF
         GOTO 10
      ENDIF
      J=JL
C
      RETURN
      END
