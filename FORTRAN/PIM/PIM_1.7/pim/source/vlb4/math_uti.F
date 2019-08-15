      SUBROUTINE SCALEP(N,ALT,F,FMAX,ALTMAX,FSCALE)
C
C  PURPOSE
C    To produce a 'scaled' version of F.
C
C  METHOD
C     Given the quantity F tabulated on the altitude grid ALT, 'scale'
C     F so that it has a new maximum ( FMAXD) at an altitude ALTMXD
C     on a new altitude grid, then use polynomial interpolation to
C     calculate the scaled values of F into FSCALE on the original
C     altitude grid
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ALT     REAL        (N)   The original altitude grid for the array F
C    ALTMAX  REAL              A defined altitude for the maximum value
C                              of F
C    F       REAL        (N)   The original parameter array
C    FMAX    REAL              A defined maximum value of F
C    N       INTEGER           The dimension of ALT,F,FSCALE,TMPALT,FT
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    FSCALE  REAL        (N)   The array F, scaled and put onto the original
C                              output grid.
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    ALTMXD  REAL              The computed altitude for the maximum value
C                              of F
C    DIF1    REAL              ALTMAX-ALT(IMX) The difference between the
C                              altitude of the polynomial maximum and the
C                              array maximum ( used for scaling)
C    DIF2    REAL              FMAX/FMAXD The difference between the
C                              the polynomial maximum and the array
C                              maximum ( used for scaling)
C    FMAXD   REAL              The computed maximum value of F
C    FT      REAL      (100)   The 'scaled' version of F on the altitude
C                              grid TMPALT.
C    I       INTEGER           Do loop index.
C    TMPALT  REAL      (100)   The scaled altitude array.
C
C  SUBROUTINES CALLED
C    NAME       description
C    FNDMAX     Finds the maximum of an array F by quadratic interpolation.
C    NEWGRID    Computes the parameter array for a different altitude grid.
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     William Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0.6    01-June-1990
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
      INCLUDE 'array.inc'
      INTEGER I,N
      REAL ALT(N),TMPALT(MAX_ALT_NUM),F(N),FT(MAX_ALT_NUM),
     &     FSCALE(N),FMAX,ALTMAX,
     1FMAXD,ALTMXD,DIF1,DIF2
C     Find the value of the maximum of the array F by polynomial
C     interpolation.
      CALL FNDMAX(N,ALT,F,FMAXD,ALTMXD)
C     The difference between the interpolated maximum and the maximum
C     array value will be used in the rescaling.
      DIF1 = ALTMAX-ALTMXD
      DIF2 = FMAX-FMAXD
C     Rescale the altitude grid and the parameter value array.
      DO I = 1,N
       TMPALT(I) = ALT(I) + DIF1
       FT(I) = F(I)+DIF2
      END DO
C     And put the scaled values (FT) back onto the original altitude
C     grid.
      CALL NEWGRID(N,TMPALT,FT,N,ALT,FSCALE)
      RETURN
      END
C
        SUBROUTINE NEWGRID(N1,ALT1,F1,N2,ALT2,F2)
C
C  PURPOSE
C
C       Given an altitude grid, ALT1, of dimension N1, interpolate
C       the quantities F1 onto a new altitude grid, ALT2, of dimension
C       N2 to produce the quantities F2.
C
C  METHOD
C      Use polynomial interpolation for the points around each of the
C      new altitude grid points to produce the interpolated parameter
C      values.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ALT1    REAL       (N1)   Original altitude grid
C    F1      REAL       (N1)   Original value array
C    N1      INTEGER           Number of original altitude values
C    N2      INTEGER           Number of output altitude values
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ALT2    REAL       (N2)   Output altitude grid
C    F2      REAL       (N2)   Output value array
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    NM1     INTEGER           N-1
C    NM2     INTEGER           N-2
C
C  SUBROUTINES CALLED
C    NONE
C
C  FUNCTIONS CALLED
C    NAME    TYPE       description
C    LAG3PT  REAL       3-point Lagrangian interpolation/extrapolation
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
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Redundant calculation of quantities N1-1
C                                   and N1-2 has been eliminated by storing in
C                                   variables.
C                                   Subroutine calls to POLINT replaced by
C                                   function calls to LAG3PT.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
        INTEGER N1, N2,I,J
        INTEGER N1M1,N1M2
        REAL ALT1(N1),F1(N1),ALT2(N2),F2(N2)
        REAL LAG3PT
        N1M1=N1-1
        N1M2=N1-2
        J = 1
        DO I = 1, N2
         IF ( ALT2(I) .LT. ALT1(1)) THEN
C         Determine if you should use linear or quadratic
C         interpolation.
          IF (F1(3) .GE. F1(2)) THEN
           IF (F1(1) .GE. F1(2)) THEN
C           Set F2 to the smalller value as interpolation will give a
C           large number.
            F2(I) = F1(2)
           ELSE
C           Interpolate
            F2(I)=((F1(2)-F1(1))/(ALT1(2)-ALT1(1)))*(ALT2(I)-ALT1(1))
            F2(I) = F2(I) + F1(1)
           ENDIF
          ELSE
           IF (F1(1) .LT. F1(2)) THEN
C           Interpolate back from the first three points.
            F2(I)=LAG3PT(ALT1(1),F1(1),ALT2(I))
           ELSE
            F2(I) = F1(2)
           ENDIF
          ENDIF
         ELSE IF (ALT2(I) .GT. ALT1(N1M2)) THEN
C         Interpolate forward from the last three points.
          IF ( ALT2(I) .GT. ALT1(N1)) THEN
           IF (F1(N1M2) .GE. F1(N1M1)) THEN
            IF (F1(N1) .GE. F1(N1M1)) THEN
C            Set F2 to the smalller value as interpolation will give a
C            large number.
             F2(I) = F1(N1M1)
            ELSE
C            Interpolate
             F2(I)=((F1(N1)-F1(N1M1))/(ALT1(N1)-ALT1(N1M1)))*
     1       (ALT2(I)-ALT1(N1))
             F2(I) = F2(I) + F1(N1)
            ENDIF
           ELSE
            IF (F1(N1) .GT. F1(N1M1)) THEN
             F2(I) = F1(N1M1)
            ELSE
             F2(I)=LAG3PT(ALT1(N1M2),F1(N1M2),ALT2(I))
            ENDIF
           ENDIF
          ELSE
           F2(I)=LAG3PT(ALT1(N1M2),F1(N1M2),ALT2(I))
          ENDIF
         ELSE
          DO WHILE ( ALT2(I) .GE. ALT1(J))
           J = J+1
          END DO
          IF ( J .NE. 1) J = J-1
          IF ( ALT2(I) .EQ. ALT1(J)) THEN
C          This altitude grid point is represented in the original
C          altitude grid.
           F2(I) = F1(J)
          ELSE
C          Interpolate for the value of F at this altitude grid point.
           F2(I)=LAG3PT(ALT1(J),F1(J),ALT2(I))
          END IF
         END IF
        END DO
        RETURN
        END
C
      SUBROUTINE FNDMAX(N,X,Y,YMAX,XMAX)
C
C  PURPOSE
C     To find the maximum of a tabulated function.
C
C  METHOD
C     If the maximum of the tabulated function is at one of its endpoints, then
C     that value is returned as the function maximum.  If the maximum of the
C     tabulated function is at an internal point, then a parabola is fit to
C     three points around the maximum of the tabulated function to get an
C     interpolated value for the function maximum.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     N      Integer    Scalar        n/a             >= 1
C        The number of tabulated function points
C     X      Real       Vector        n/a             n/a
C                       (N)
C        The X-values for the tabulated function, which must be monotonically
C        increasing or decreasing with index
C     Y      Real       Vector        n/a             n/a
C                       (N)
C        The tabulated function
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     XMAX   Real       Scalar        n/a             min(X(1),X(N)) <= XMAX
C                                                     <= max(X(1),X(N))
C        The X-value of the function maximum
C     YMAX   Real       Scalar        n/a             n/a
C        The function maximum
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C0     Real       Scalar        n/a             n/a
C        The constant term in the parabolic equation
C     C1     Real       Scalar        n/a             n/a
C        The coefficient of linear term in the parabolic equation
C     C2     Real       Scalar        n/a             n/a
C        The coefficient of the quadratic term in the parabolic equation
C     I      Integer    Scalar        n/a             2 <= I <= NM1
C        A loop counter
C     IMAX   Integer    Scalar        n/a             1 <= IMAX <= N
C        The index of the maximum of the tabulated function
C     IMAXM1 Integer    Scalar        n/a             0 <= IMAXM1 <= N-1
C        IMAX-1
C     NM1    Integer    Scalar        n/a             N-1
C        N-1
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     PAR3PT Fits a parabola to three data points
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
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.2   27-April-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       24-May-1994  1.0 ==> Created
C     L. Brown       26-May-1994  1.0 ==> 1.1
C                                 Removed status flag variable from argument
C                                 list of routine PAR3PT.
C                                 Added description of PAR3PT to header.
C     L. Brown       27-Apr-1995  1.1 ==> 1.2
C                                 Added handling of special case C2=0.
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
      INTEGER N
      REAL X(N),Y(N)
C
C  Output variables
C
      REAL XMAX,YMAX
C
C  Local variables
C
      INTEGER IMAX,NM1,I,IMAXM1
      REAL C0,C1,C2
C
C  Find the maximum of the tabulated function
C
      IMAX=1
      IF(N .GT. 1) THEN
         NM1=N-1
         IF(N .GT. 2) THEN
            DO 10 I=2,NM1
               IF(Y(I) .GE. Y(IMAX)) IMAX=I
   10       CONTINUE
         ENDIF
         IF(IMAX .EQ. NM1) THEN
            IF(Y(N) .GT. Y(IMAX)) IMAX=N
         ENDIF
      ENDIF
C
C  If the tabulated function has a maximum at its first point, then that value
C  is returned as the function maximum without interpolation
C
      IF(IMAX .EQ. 1) THEN
         XMAX=X(1)
         YMAX=Y(1)
C
C  If the tabulated function has a maximum at its last point, then that value
C  is returned as the function maximum without interpolation
C
      ELSE IF(IMAX .EQ. N) THEN
         XMAX=X(N)
         YMAX=Y(N)
C
C  If the tabulated function has a maximum at an internal point, then fit a
C  parabola to three points around the maximum of the tabulated function in
C  order to interpolate to get the function maximum
C
      ELSE
         IMAXM1=IMAX-1
         CALL PAR3PT(X(IMAXM1),Y(IMAXM1),C0,C1,C2)
         IF(C2 .EQ. 0) THEN
            XMAX=X(IMAX)
            YMAX=C0+C1*XMAX
         ELSE
            XMAX=-.5*C1/C2
            YMAX=C0+C1*XMAX+C2*XMAX*XMAX
         ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE PAR3PT(X,Y,C0,C1,C2)
C
C  PURPOSE
C     To fit a parabola to three data points.
C
C  METHOD
C     A parabola has the functional form Y(X) = C0 + C1*X + C2*X**2.  The
C     coefficients C0, C1, and C2 are determined by solving the three
C     simultaneous linear equations obtained from the three data points.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     X      Real       Vector        n/a             n/a
C                       (3)
C        The X-values for the data points, which must be monotonic,
C        i.e. either X(1) < X(2) < X(3) or X(1) > X(2) > X(3)
C     Y      Real       Vector        n/a             n/a
C                       (3)
C        The Y-values for the data points
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C0     Real       Scalar        n/a             n/a
C        The constant term in the parabolic equation
C     C1     Real       Scalar        n/a             n/a
C        The coefficient of linear term in the parabolic equation
C     C2     Real       Scalar        n/a             n/a
C        The coefficient of the quadratic term in the parabolic equation
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     X12X23 Real       Scalar        n/a             <> 0.
C        (X(1)-X(2))*(X(2)-X(3))
C     X1MX2  Real       Scalar        n/a             <> 0.
C        X(1)-X(2)
C     X2MX3  Real       Scalar        n/a             <> 0.
C        X(2)-X(3)
C     X2SQ   Real       Scalar        n/a             >= 0.
C        X(2)**2
C     X3MX1  Real       Scalar        n/a             <> 0.
C        X(3)-X(1)
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
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.2   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       12-May-1992  1.0 ==> Created
C     L. Brown       24-May-1994  1.0 ==> 1.1
C                                 Brought internal documentation up to specs.
C                                 Eliminated unnecessary recalculation of
C                                 quantities X(1)-X(2), X(2)-X(3), X(3)-X(1),
C                                 and X(2)**2 by storing in variables.
C                                 Changed squares (x**2) to simple
C                                 multiplication (x*x) for optimization.
C     L. Brown       26-May-1994  1.1 ==> 1.2
C                                 The X-values must now be monotonic.
C                                 Eliminated returned status; routine stops
C                                 if X-values are not monotonic.
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
      REAL X(3),Y(3)
C
C  Output variables
C
      REAL C0,C1,C2
C
C  Local variables
C
      REAL X1MX2,X2MX3,X12X23,X3MX1,X2SQ
C
C  Calculate differences between the X-values
C
      X1MX2=X(1)-X(2)
      X2MX3=X(2)-X(3)
      X12X23=1.0d0/(X1MX2*X2MX3)
      X2SQ=X(2)*X(2)
C
C  Check the X-values
C
C     2005 Sep 01  James M Anderson  --JIVE  remove debugging code
C                                      to help speed up this function
#ifdef DEBUG
      IF(X12X23 .LE. 0.) STOP 'PAR3PT: X-values are not monotonic.'
#endif
C
C  Calculate C2 for evenly-spaced X-values
C
      IF(X1MX2 .EQ. X2MX3) THEN
         C2=.5*(Y(1)-2.*Y(2)+Y(3))*X12X23
C
C  Calculate C2 for arbitrarily-spaced X-values
C
      ELSE
         X3MX1=X(3)-X(1)
         C2=-(Y(1)*X2MX3+Y(2)*X3MX1+Y(3)*X1MX2)*X12X23/X3MX1
      ENDIF
C
C  Calculate C1
C
      C1=(Y(1)-Y(2)-C2*(X(1)*X(1)-X2SQ))/X1MX2
C
C  Calculate C0
C
      C0=Y(2)-C1*X(2)-C2*X2SQ
C
      RETURN
      END
C
      FUNCTION LAG3PT(XIN,YIN,XOUT)
C
C  PURPOSE
C     To interpolate onto a three-point data grid using Lagrange's classical
C     formula.
C
C  METHOD
C     An output data point at XOUT is interpolated onto a three-point input
C     data grid YIN(XIN) using Lagrange's classical three-point formula.  The
C     input X-values must be monotonic.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     XIN    Real       Vector        n/a             n/a
C                       (3)
C        The X-values for the input data points, which must be monotonic,
C        i.e. either XIN(1) < XIN(2) < XIN(3) or XIN(1) > XIN(2) > XIN(3)
C     YIN    Real       Vector        n/a             n/a
C                       (3)
C        The Y-values for the input data points
C     XOUT   Real       Scalar        n/a             n/a
C        The X-value for the output data point
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     LAG3PT Real       Scalar        n/a             n/a
C        The interpolated Y-value for the output data point
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     X12X23 Real       Scalar        n/a             <> 0.
C        (XIN(1)-XIN(2))*(XIN(2)-XIN(3))
C     X1MX2  Real       Scalar        n/a             <> 0.
C        XIN(1)-XIN(2)
C     X2MX3  Real       Scalar        n/a             <> 0.
C        XIN(2)-XIN(3)
C     X3MX1  Real       Scalar        n/a             <> 0.
C        XIN(3)-XIN(1)
C     XMX1   Real       Scalar        n/a             n/a
C        XOUT-XIN(1)
C     XMX2   Real       Scalar        n/a             n/a
C        XOUT-XIN(2)
C     XMX3   Real       Scalar        n/a             n/a
C        XOUT-XIN(3)
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
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.0   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-May-1994  1.0 ==> Created
C
C  REFERENCES
C     1. Press, W. H., S. A. Teukolsky, W. T. Vetterling, and B. P. Flannery,
C        'Numerical Recipes in FORTRAN', 2nd ed., Cambridge University Press,
C        1992, p. 102.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      REAL XOUT
      REAL XIN(3),YIN(3)
C
C  Output variables
C
      REAL LAG3PT
C
C  Local variables
C
      REAL X1MX2,X2MX3,X12X23,XMX1,XMX2,XMX3,X3MX1
C
C  Calculate differences between the input X-values
C
      X1MX2=XIN(1)-XIN(2)
      X2MX3=XIN(2)-XIN(3)
C
C  Calculate differences between the output X-value and input X-values
C
      XMX1=XOUT-XIN(1)
      XMX2=XOUT-XIN(2)
      XMX3=XOUT-XIN(3)


      X12X23=X1MX2*X2MX3
C
C  Check the input X-values
C
C      IF(X12X23 .LE. 0.) STOP 'LAG3PT: X-values are not monotonic.'
C     2005 Sep 01  James M Anderson  --remove debugging code, as this function
C     is the largest use of time according to gprof, and the STOP is
C     never called.
#ifdef DEBUG
      IF(X12X23 .GT. 0.) THEN
      ELSE
         STOP 'LAG3PT: X-values are not monotonic.'
      ENDIF
#endif
C
C  Interpolate using Lagrange's classical three-point formula for evenly
C  spaced input X-values
C
      IF(X1MX2 .EQ. X2MX3) THEN
         LAG3PT=.5*(XMX2*XMX3*YIN(1)-2.*XMX1*XMX3*YIN(2)
     &             +XMX1*XMX2*YIN(3))/X12X23
C
C  Interpolate using Lagrange's classical three-point formula for arbitrarily
C  spaced input X-values
C
      ELSE
         X3MX1=XIN(3)-XIN(1)
         LAG3PT=-(X2MX3*XMX2*XMX3*YIN(1)+X3MX1*XMX1*XMX3*YIN(2)
     &           +X1MX2*XMX1*XMX2*YIN(3))/(X12X23*X3MX1)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
C
C  PURPOSE
C    To perform polynomial interpolation and extrapolation.
C
C  METHOD
C    This scheme is based on Neville's algorithm.
C
C  INPUT PARAMETERS
C    N       INTEGER         The number of points on the grid
C    X       REAL            The point to be interpolated or extrapolated
C    XA      REAL            The grid of points
C    YA      REAL            The data on the grid
C
C  OUTPUT PARAMETERS
C    DY      REAL            An error estimate of the interpolated or
C                             extrapolated value
C    Y       REAL            The interpolated value
C
C  LOCAL VARIABLES
C    C       REAL            Array of corrections to the interpolation or
C                             extrapolation at each step, used to form the
C                             next stage
C    D       REAL            Array of corrections to the interpolation or
C                             extrapolation
C    DEN     REAL            Used to update the corrections
C    DIF     REAL            Used to find the location of X on the grid
C    DIFT    REAL            Used to find the location of X on the grid
C    HO      REAL            Used to update the corrections
C    HP      REAL            Used to update the corrections
C    NS      INTEGER         An index holder
C    W       REAL            Used to update the corrections
C
C  SUBROUTINES CALLED
C    None
C
C  FUNCTIONS CALLED
C    None
C
C  FILES ACCESSED
C    None
C
C  AUTHOR
C    Lincoln Brown
C    Computational Physics, Inc.
C    240 Bear Hill Road  Suite 202A
C    Waltham, MA 02154  USA
C    (617)-487-2250
C
C  VERSION
C    1.0.6   22-June-1990
C
C  MODIFICATIONS
C    ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     [1] Press, W. H., B. P. Flannery, S. A. Teukolsky, and W. T. Vettering,
C         'Numerical Recipes', Cambridge University Press, Cambridge, 1986.
C
C  SPECIAL CONSTANTS
C    NMAX    INTEGER         The maximum allowed value of N
C
      INTEGER NMAX
      PARAMETER(NMAX=10)
C
      INTEGER N
      REAL*8 XA(N),YA(N)
      REAL*8 C(NMAX),D(NMAX)
      REAL*8 X,Y,DY
      REAL*8 DIF,DIFT,HO,HP,W,DEN
      INTEGER NS,I,J
C
C  Find the location of X on the grid and initialize the corrections
C
      NS=1
      DIF=ABS(X-XA(1))
      DO I=1,N
         DIFT=ABS(X-XA(I))
         IF(DIFT .LT. DIF) THEN
            NS=I
            DIF=DIFT
         ENDIF
         C(I)=YA(I)
         D(I)=YA(I)
      ENDDO
C
C  Interpolate or extrapolate the point
C
      Y=YA(NS)
      NS=NS-1
      DO J=1,N-1
       DO I=1,N-J
        HO=XA(I)-X
        HP=XA(I+J)-X
        W=C(I+1)-D(I)
        DEN=HO-HP
        IF(DEN .EQ. 0.) STOP 'POLINT:Identical points on grid.'
        DEN=W/DEN
        D(I)=HP*DEN
        C(I)=HO*DEN
       ENDDO
       IF(2*NS .LT. N-J) THEN
        DY=C(NS+1)
       ELSE
        DY=D(NS)
        NS=NS-1
       ENDIF
       Y=Y+DY
      ENDDO
C
      RETURN
      END
C
      SUBROUTINE LOBKSB(A,INDX,B)
C
C  Specialized version of Numerical Recipes' SUBROUTINE LUBKSB
C  Assumes A is already decomposed into LU form
C  B contains right hand side on input, solution on output
C  INDX contains permutation indices produced during the LU decomposition
C       of A
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        7-Oct-1994  1.1
C                                 Eliminated redundant calculation of
C                                 quantities I-1 and I+1 by storing in
C                                 variables.
C
      INTEGER INDX(4),I,LL,J,IM1,IP1
      REAL A(4,4),B(4),SUM
C
      DO I=1,4
        LL = INDX(I)
        SUM = B(LL)
        B(LL) = B(I)
        IM1=I-1
        DO J=1,IM1
          SUM = SUM - A(I,J)*B(J)
        ENDDO
        B(I) = SUM
      ENDDO
C
      DO I=4,1,-1
        SUM = B(I)
        IF (I .LT. 4) THEN
          IP1=I+1
          DO J=IP1,4
            SUM = SUM - A(I,J)*B(J)
          ENDDO
        ENDIF
        B(I) = SUM/A(I,I)
      ENDDO
C
      RETURN
      END
      REAL FUNCTION FINTRP(F,X)
C
C  Performs Fourier interpolation on F(x) to obtain F at x=X.
C
C     F(x) = c0 + c1*cos(x) + c2*sin(x) + c3*cos(2x) + c4*sin(2x)
C
C  F is assumed given at the four magnetic longitudes corresponding to the
C     four LOWLAT longitude sectors:
C     x1 =  30.295 (Brazil)
C     x2 = 149.301 (India)
C     x3 = 250.167 (Pacific)
C     x4 = 329.301 (United States)
C
C  The Fourier coefficient c0 is arbitrarily chosen to be
C
C      c0 = (F(x1) + F(x2) + F(x3) + F(x4))/4
C
C  The other Fourier coefficients are  obtained from the set of four
C     simultaneous equations
C
C  c1*cos(x1) + c2*sin(x1) + c3*cos(2*x1) + c4*sin(2*x1) = F(x1) - c0
C  c1*cos(x2) + c2*sin(x2) + c3*cos(2*x2) + c4*sin(2*x4) = F(x2) - c0
C  c1*cos(x3) + c2*sin(x3) + c3*cos(2*x3) + c4*sin(2*x4) = F(x3) - c0
C  c1*cos(x4) + c2*sin(x4) + c3*cos(2*x4) + c4*sin(2*x4) = F(x4) - c0
C
C  The matrix A consisting of the coefficients of c1...c4 in the above
C  equations has already been decomposed into LU form.  This decomposition
C  is stored in the array ALU in the DATA statement below.  The permutation
C  indices generated during the decomposition are stored in INDX.  So all
C  that is necessary is to do the forward and back substitutions to obtain the
C  fourier coefficients.
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        7-Oct-1994  1.1
C                                 Variables ALU and INDX put in common block
C                                 CBFINT and initialized in BLOCK DATA BDFINT,
C                                 to avoid the requirement of static storage.
C                                 Eliminated redundant calculation of
C                                 quantities X*RPD and 2.0*COSX by storing in
C                                 variables.
C     L. Brown       25-Sep-1995  1.1 ==> 1.3
C                                 Changed magnetic longitude values in header
C                                 comments for new LLF parameterization.
C
      REAL F(4),X
      INCLUDE 'const.inc'
C
      INTEGER INDX(4),I
      REAL ALU(4,4),C(4),C0,COSX,SINX,COS2X,SIN2X,XRPD,TWOCOX
C
      COMMON/CBFINT/ALU,INDX
C
      C0 = 0.0
      DO I=1,4
        C0 = C0 + F(I)                !  sum the right hand sides
      ENDDO
      C0 = 0.25*C0                    !  c0 = mean of right hand sides
C
      DO I=1,4
        C(I) = F(I) - C0              !  fill right hand side
      ENDDO
C
      CALL LOBKSB(ALU,INDX,C)         !  forward and back substitution to get
C                                     !  Fourier coefficients
C     PRINT *,'C=',C
      XRPD=X*DTOR
      COSX = COS(XRPD)
      SINX = SIN(XRPD)
      TWOCOX=2.0*COSX
      COS2X = TWOCOX*COSX - 1.0
      SIN2X = TWOCOX*SINX
C
      FINTRP = C0 + C(1)*COSX + C(2)*SINX + C(3)*COS2X + C(4)*SIN2X
C
      RETURN
      END
      BLOCK DATA BDFINT
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        7-Oct-1994  1.1 ==> Created
C     L. Brown       25-Sep-1995  1.1 ==> 1.3
C                                 Changed values of ALU matrix and INDX array
C                                 for new LLF parameterization.
C
      INTEGER INDX(4)
      REAL ALU(4,4)
      COMMON/CBFINT/ALU,INDX
C
      DATA ALU/8.634396E-01, 9.958556E-01,-9.958556E-01,-3.929397E-01,
     &         5.044523E-01,-1.012890E+00,-1.000000E+00, 7.330179E-01,
     &         4.910558E-01,-1.029842E-02, 9.574445E-01,-5.945769E-01,
     &         8.711281E-01,-1.745484E+00,-1.755933E+00, 1.216046E+00/
      DATA INDX/1,4,4,4/
C
      END
      REAL FUNCTION GINTRP(Y,X,XAT)
C
C     PURPOSE
C      To interpolate the function Y on X to GINTRP at XAT where
C      1) GINTRP<MAX(Y)
C      2) GINTRP>MIN(Y)
C      3) If all Y(i) = Y0 then GINTRP = y(0) regardless of the XAT
C      4) Where XAT = X(I), then GINTRP = Y(I)
C      5) The interpolated function is periodic and continuous in the function
C          and derivatives
C
C     METHOD
C      Define a function to have the following properties:
C      LET
C      d(i) = (1.-COS(XAT-X(I)))/2.>=0.
C
C      THEN LET
C      GINTRP = (SUM OVER i (y(i)/d(i)))*(PROD OVER i d(i))/T
C      T is the normalizing factor determine T by condition 3)
C
C      WHERE all Y(i) = Y0 then
C      GINTRP = Y0*((SUM OVER i (1./d(i)))*(PROD OVER i d(i))/T
C      GINTRP/Y0 = ((SUM OVER i (1./d(i)))*(PROD OVER i d(i))/T
C
C      By condition 3, GINTRP/Y0 = 1.
C      1. = ((SUM OVER i (1./d(i)))*(PROD OVER i d(i))/T
C      implies
C      T = ((SUM OVER i (1./d(i)))*(PROD OVER i d(i))
C
C      Simplifying this so that 0/0 will not appear
C
C      T = SUM OVER I((PRODUCT OVER J D(J))/D(I))
C      T = (D(2)*D(3)*D(4) + D(1)*D(3)*D(4) + D(1)*D(2)*D(4) + D(1)*D(2)*D(3))
C      and
C      GINTRP = SUM OVER I((PRODUCT OVER J D(J))*Y(I)/D(I))
C      GINTRP = (Y(1)*D(2)*D(3)*D(4) + Y(2)*D(1)*D(3)*D(4) + Y(3)*D(1)*D(2)*D(4)
C      + Y(4)*D(1)*D(2)*D(3))/T
C
C     INPUT PARAMETERS
C     NAME   TYPE    ARRAY    Description
C     X      REAL     (4)     Array of longitude values (X axis)
C     XAT    REAL             X value to interpolate on
C     Y      REAL     (4)     Array of data values (Y axis)
C
C     OUTPUT PARAMETERS
C     NAME   TYPE    ARRAY    Description
C     GINTRP REAL             interpolated value
C
C     LOCAL VARIABLES
C     NAME   TYPE    ARRAY    Description
C     D      REAL     (4)     See METHOD for description of d(i)
C     I      INTEGER          Do loop counter
C     T      REAL             Normalizing coefficient
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown        7-Oct-1994  1.1
C                                 Eliminated redundant calculation of
C                                 quantities PI/180., D(1)*D(2), D(3)*D(4),
C                                 D(2)*D(3)*D(4), D(1)*D(3)*D(4),
C                                 D(1)*D(2)*D(4), and D(1)*D(2)*D(3) by storing
C                                 in variables.
C
      INCLUDE 'const.inc'
      INCLUDE 'math4_co.inc'
C
C     2005 Sep 01  James M Anderson  --JIVE  Hey, math4_co.inc already has RPD
C      REAL X(4),Y(4),D(4),XAT,T,PIO180,D12,D34,D234,D134,D124,D123
      REAL X(4),Y(4),D(4),XAT,T,D12,D34,D234,D134,D124,D123
      INTEGER I
C
C     First, compute the the D(I)
C
C      PIO180=PI/180.
      DO I = 1,4
C       D(I) = (1.-COS((XAT-X(I))*PIO180))/2.
       D(I) = (1.-COS((XAT-X(I))*RPD))*0.5
      ENDDO
C
C     Next compute the normalizing factor T
C
      D12=D(1)*D(2)
      D34=D(3)*D(4)
      D234=D(2)*D34
      D134=D(1)*D34
      D124=D12*D(4)
      D123=D12*D(3)
      T=1.0/(D234+D134+D124+D123)
C
C     Finally compute the GINTRP
C
      GINTRP=(Y(1)*D234+Y(2)*D134+Y(3)*D124+Y(4)*D123)*T
C
      RETURN
      END
C
      SUBROUTINE QROMB(FUNC,A,B,SS)
C
C  PURPOSE
C    To perform Romberg integration on the closed interval [a,b].  The
C     integrand is supplied in the external function FUNC.
C
C  METHOD
C    This integration scheme is based on Richardson's deferred approach
C     to the limit.
C
C  INPUT PARAMETERS
C    A       REAL            The lower integration limit
C    B       REAL            The upper integration limit
C    FUNC    REAL            A function that calculates the integrand
C
C  OUTPUT PARAMETERS
C    SS      REAL            The value of the integral
C
C  LOCAL VARIABLES
C    DSS     REAL            An error estimate of the extrapolated integral
C    S       REAL            A sequence of integral estimates, calculated by
C                             the trapezoid rule, corresponding to decreasing
C                             step size
C    H       REAL            A sequence of decreasing extrapolation factors
C    DEL     REAL            The spacing between added points in the
C                             trapezoid integration
C    NPOINTS INTEGER         The number of added points in the trapezoid
C                             integration
C    SUM     REAL            An intermediate sum for the integration
C    X       REAL            A location on the integration path
C
C  SUBROUTINES REQUIRED
C    POLINT   Performs polynomial interpolation and extrapolation
C
C  FUNCTIONS CALLED
C    FUNC     Calculates the integrand
C
C  FILES ACCESSED
C    None
C
C  AUTHORS
C    Lincoln Brown, 12-January-1989
C    Robert W. Simon, 9-April-1990
C    Computational Physics, Inc.
C    240 Bear Hill Road  Suite 202A
C    Waltham, MA 02154  USA
C    (617)-487-2250
C
C  VERSION
C    1.0.6   22-June-1990
C
C  MODIFICATIONS
C    ----Person----  ----Date----  ----------------Description----------------
C
C  REFERENCES
C     [1] Press, W. H., B. P. Flannery, S. A. Teukolsky, and W. T. Vettering,
C         'Numerical Recipes', Cambridge University Press, Cambridge, 1986.
C
C  SPECIAL CONSTANTS
C    EPS     REAL            A convergence criterion
C    IMAX    INTEGER         The maximum allowed number of rows in the
C                             Romberg table
C    NROWS   INTEGER         The number of the rows in the extrapolation,
C                             i.e., the latest NROWS values of the integral
C                             are used
C
      INCLUDE 'logicuni.inc'
      REAL EPS
      PARAMETER(EPS=1.E-6)
      INTEGER IMAX,NROWS
      PARAMETER(IMAX=20,NROWS=5)
C
      REAL*8 S(IMAX+1),H(IMAX+1)
      REAL*8 FUNC
      REAL A,B
      REAL*8 SS8,DSS
      REAL SS
      INTEGER I,J,NPOINTS
      REAL*8 DEL,X,SUM
C
      EXTERNAL FUNC
C
C  Set the initial value of the relative step size to 1.  The sequence of
C   H's will then be 1, 1/4, 1/16, ..., to which will correspond a sequence
C   of estimates of the integral.  In the extrapolation step we take H -> 0,
C   and the sequence of integrals extrapolates to the "zero step size" value.
C
      H(1)=1.
C
C  Loop over stepsize to refine the integral until convergence is reached
C
      DO I=1,IMAX
C
C  Calculate the integral by using the trapezoid rule.  On each pass, use
C   one new point between each two previous ones, so that there are twice
C   as many intervals.  The current NPOINTS is the number of points added
C   the next time.  To save time, recycle the previous result.  For a
C   sufficient number of intervals, the truncation error goes as step size
C   squared, enabling extrapolation by a uniform factor of four.
C
         IF(I .EQ. 1) THEN
            S(I)=.5*(B-A)*(FUNC(DBLE(A))+FUNC(DBLE(B)))
            NPOINTS=1
         ELSE
            DEL=(B-A)/FLOAT(NPOINTS)
            X=A+.5*DEL
            SUM=0.
            DO J=1,NPOINTS
               SUM=SUM+FUNC(X)
               X=X+DEL
            ENDDO
            S(I)=.5*(S(I-1)+DEL*SUM)
            NPOINTS=2*NPOINTS
         ENDIF
C
C  When a sufficient tableau of integral estimates has been formed, extrapolate
C   the integral to a step size of zero.  The extrapolation uses the latest
C   NROWS estimates of the integral produced by the trapezoid rule.
C
         IF(I .GE. NROWS) THEN
C
C  Extrapolate the integral estimate.  The calling arguments H and S start at
C   the beginning of their last NROWS elements.
C
            CALL POLINT(H(I-NROWS+1),S(I-NROWS+1),NROWS,0.0D0,SS8,DSS)
C
C  Check for convergence of the integral estimate:  If relative error
C   (error estimate divided by the value of the integral) is less than EPS,
C   the integration is finished.
C
C            write (*,*) i,ss8,dss,dss/ss8
            IF(ABS(DSS) .LT. EPS*ABS(SS8)) THEN
               SS = SS8
               RETURN
            ENDIF
C
         ENDIF
C
C  Otherwise, calculate the next relative step size ("extrapolation factor")
C   and get another trapezoid-rule estimate.
C
         H(I+1)=.25*H(I)
C
      ENDDO
C
C  The integration did not converge in the allowed number of refinements (if
C   IMAX is 20, the last estimate would contain about a million steps...)
C
      STOP 'QROMB:Too many steps for me.'
C
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==

      SUBROUTINE piksrt(n,arr,a)
      INTEGER n
      CHARACTER arr(n)*(*)
      INTEGER i,j
      CHARACTER a*(*)
      DO 12 j=2,n
         a=arr(j)
         DO 11 i=(j-1),1,-1
            IF (arr(i) .LE. a) GOTO 10
            arr(i+1) = arr(i)
 11      CONTINUE
         i=0
 10      arr(i+1)=a
 12   CONTINUE
      RETURN
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==

      SUBROUTINE piksrt3(n,dparr,arr,brr)
      INTEGER n
      REAL*8 dparr(n), brr(n)
      INTEGER i,j, arr(n), a
      REAL*8 dp,b
      DO 12 j=2,n
         dp=dparr(j)
         a=arr(j)
         b=brr(j)
         DO 11 i=(j-1),1,-1
            IF (dparr(i) .LE. dp) GOTO 10
            dparr(i+1) = dparr(i)
            arr(i+1) = arr(i)
            brr(i+1) = brr(i)
 11      CONTINUE
         i=0
 10      dparr(i+1) = dp
         arr(i+1) = a
         brr(i+1) = b
 12   CONTINUE
      RETURN
      END
