      SUBROUTINE GENEC(MEOF,MOPM1,MX,MMLOUT,DELTA,NOPM1,NEOF,NMLAT,
     &                 IMLAT,IMLAT1,IMLAT2,T,TEST,OPC,MLOUT,NMLOUT,
     &                 SMLOUT,DMLOUT,EC)
C
C  PURPOSE
C     To generate EOF coefficients from orthogonal polynomial coefficients.
C
C  METHOD
C     A set of orthogonal polynomials is determined.  The orthogonal
C     polynomials are combined with the orthogonal polynomial coefficients to
C     produce EOF coefficients for the given magnetic latitude and magnetic
C     longitude/universal time.  If the magnetic latitude does not lie exactly
C     on the magnetic latitude grid, then the orthogonal polynomials are
C     interpolated from orthogonal polynomials at the two nearest magnetic
C     latitudes on the magnetic latitude grid.  If the magnetic longitude/
C     universal time does not lie exactly on the magnetic longitude/universal
C     time grid, then the orthogonal polynomial coefficients are interpolated
C     from orthogonal polynomial coefficients at the two nearest magnetic
C     longitudes/universal times on the magnetic longitude/universal time
C     grid.
C
C  INPUT PARAMETERS
C  NAME  TYPE ARRAY             Description
C  DELTA REAL                   The step size for the orthogonal polynomials
C  DMLAT REAL                   The increment of the magnetic latitude grid,
C                               in degrees
C  DMLOUT REAL                  The increment of the magnetic longitude/
C                               universal time grid, in degrees east/decimal
C                               hours
C  IMLAT  INTEGER               The magnetic latitude index
C  IMLAT1 INTEGER               A magnetic latitude index used for interpolation
C  IMLAT2 INTEGER               A magnetic latitude index used for interpolation
C  MLAT  REAL                   The magnetic latitude, in degrees north
C  MLOUT REAL                   The magnetic longitude/universal time, in
C                               degrees east/decimal hours
C  NEOF  INTEGER                The number of EOFs used to construct an ion
C                               density altitude profile
C  NMLAT INTEGER                The number of magnetic latitude grid points
C  NMLOUT INTEGER               The number of magnetic longitude/universal time
C                               grid points
C  NOPM1 INTEGER                The number of orthogonal polynomials - 1
C  OPC   REAL(MOPM1+1,MEOF,     Orthogonal polynomial coefficients
C             MMLOUT)
C  SMLAT REAL                   The starting value of the magnetic latitude
C                               grid, in degrees
C  SMLOUT REAL                  The starting value of the magnetic longitude/
C                               universal time grid, in degrees east/decimal
C                               hours
C  T      REAL                  The magnetic latitude interpolation factor
C  TEST   LOGICAL               The magnetic latitude interpolation flag,
C                               .TRUE. if no interpolation is needed, .FALSE.
C                               if interpolation is needed
C
C  OUTPUT PARAMETERS
C  NAME  TYPE ARRAY             Description
C  EC    REAL (MEOF)            EOF coefficients
C
C  LOCAL VARIABLES
C  NAME   TYPE ARRAY            Description
C  I      INTEGER               A loop counter
C  IEOF   INTEGER               A loop counter for EOF coefficients
C  IMLOU1 INTEGER               A magnetic longitude/universal time index used
C                               for interpolation
C  IMLOU2 INTEGER               A magnetic longitude/universal time index used
C                               for interpolation
C  IMLOUT INTEGER               A magnetic longitude/universal time index used
C                               for interpolation
C  IOPM11 INTEGER               A loop counter for orthogonal polynomial
C                               coefficients
C  MLOUTG INTEGER               The grid magnetic longitude/universal time, in
C                               degrees east/decimal hours
C  NOPM11 INTEGER               NOPM1+1
C  ONEMT  REAL                  1.-T
C  ONEMTM REAL                  1.-TMLOUT
C  OP     REAL (MAXMX,MAXOP)    Orthogonal polynomials
C  OPINT  REAL (MAXOP)          Interpolated orthogonal polyomials
C  X      REAL (MAXMX)          The grid for the orthogonal polynomials
C  TMLOUT REAL                  The magnetic longitude/universal time
C                               interpolation factor
C
C  SUBROUTINES REQUIRED
C     NAME     Description
C     ORPOLY   Generates the orthogonal polynomials
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
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   Calculation of orthogonal polynomial grid
C                                   has been optimized.
C                                   Redundant calculation of quantity NOPM1+1
C                                   has been eliminated by storing in a
C                                   variable.
C                                   Linear interpolation of orthogonal
C                                   polynomial to magnetic latitude of
C                                   interest has been moved outside loops
C                                   to eliminate redundant calculation.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   Renamed from GENFC to GENEC and modified to
C                                   reflect the elimination of Fourier fitting
C                                   in universal time in the low-latitude
C                                   F-region parameterized model (LLF), the
C                                   elimination of Fourier fitting in magnetic
C                                   longitude in the mid-latitude F-region
C                                   model (MLF), and the elimination of Fourier
C                                   fitting in magnetic longitude in the low-
C                                   and mid- latitude E-region parameterized
C                                   model (LME).
C                                   MAXOP increased from 15 to 25.
C     L. Brown        13-Jan-1998   1.1 ==> 1.7
C                                   Changed PARAMETER MAXMX from 40 to 50 to
C                                   accommodate new LLF parameterization.
C
C  REFERENCES
C     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
C        The Golam Press, Boulder, CO, 1973, pp 90-92.
C
C  SPECIAL CONSTANTS
C
      INCLUDE 'logicuni.inc'
      INTEGER MEOF,MOPM1,MX,MMLOUT,MAXMX,MAXOP
      PARAMETER(MAXMX=50,MAXOP=25)
      INTEGER NEOF,NOPM1,NMLAT,NMLOUT
      INTEGER IMLAT,IMLAT1,IMLAT2,I,IEOF,IOPM11,IMLOUT,IMLOU1,IMLOU2
      INTEGER NOPM11
      REAL OPC(MOPM1+1,MEOF,MMLOUT),EC(MEOF)
      REAL OP(MAXMX,MAXOP),X(MAXMX)
      REAL OPINT(MAXOP)
      REAL DELTA,T,MLOUT,SMLOUT,DMLOUT
      REAL MLOUTG,ONEMT,TMLOUT,ONEMTM
      LOGICAL TEST
C
C     Check the inputs
C
#ifdef DEBUG
      IF(MX .GT. MAXMX) THEN
       WRITE(LUSTDERR,*)'Maximum set in GENEC ',MAXMX,' less than ',MX
       STOP 'ORTHOGONAL POLYNOMIAL GRID ERROR IN GENEC'
      ENDIF
      IF(MOPM1 .GE. MAXOP) THEN
       WRITE(LUSTDERR,*)
     &        'Maximum set in GENEC ',MAXOP,' less than ',MOPM1+1
       STOP 'ORTHOGONAL POLYNOMIAL ERROR IN GENEC'
      ENDIF
#endif
C
C  Calculate the grid for the orthogonal polynomials
C
      X(1)=-1.
      DO 10 I=2,NMLAT
         X(I)=X(I-1)+DELTA
   10 CONTINUE
C
C  Generate the orthogonal polynomials
C
      CALL ORPOLY(MAXOP,MAXMX,NOPM1,NMLAT,X,OP)
C
C  Calculate NOPM1+1
C
      NOPM11=NOPM1+1
C
C  Calculate the grid magnetic longitude/universal time index
C
      IMLOUT=1+INT((MLOUT-SMLOUT)/DMLOUT)
C
C  Calculate the grid magnetic longitude/universal time
C
      MLOUTG=SMLOUT+DMLOUT*FLOAT(IMLOUT-1)
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then no
C  interpolation of the orthogonal polynomials in magnetic latitude is
C  necessary
C
      IF(TEST) THEN
C
C  If the magnetic longitude/universal time is exactly on the magnetic
C  longitude/universal time grid, then no interpolation of the orthogonal
C  polynomial coefficients in magnetic longitude/universal time is necessary
C
         IF(MLOUT .EQ. MLOUTG) THEN

C     2005 Aug 31  James M Anderson  --JIVE  reorder loops to speed up
C  Initialize the EOF coefficient
            DO IEOF=1,NEOF
               EC(IEOF)=0.
            ENDDO
C  Loop over the orthogonal polynomial coefficients
            DO IOPM11=1,NOPM11
               DO IEOF=1,NEOF
                  EC(IEOF)=EC(IEOF)
     &                    +OPC(IOPM11,IEOF,IMLOUT)*OP(IMLAT,IOPM11)
               ENDDO
            ENDDO

c$$$C
c$$$C  Loop over the number of EOFs used to construct an ion density altitude
c$$$C  profile
c$$$C
c$$$            DO 200 IEOF=1,NEOF
c$$$C
c$$$C  Initialize the EOF coefficient
c$$$C
c$$$               EC(IEOF)=0.
c$$$C
c$$$C  Loop over the orthogonal polynomial coefficients
c$$$C
c$$$               DO 100 IOPM11=1,NOPM11
c$$$C
c$$$C  Add the contribution to the EOF coefficient by the orthogonal polynomial
c$$$C
c$$$                  EC(IEOF)=EC(IEOF)
c$$$     &                    +OPC(IOPM11,IEOF,IMLOUT)*OP(IMLAT,IOPM11)
c$$$C
c$$$C  End the loop over orthogonal polynomial coefficients
c$$$C
c$$$  100          CONTINUE
c$$$C
c$$$C  End the loop over the number of EOFs used to construct an ion density
c$$$C  altitude profile
c$$$C
c$$$  200       CONTINUE
C
C  If the magnetic longitude/universal time is not exactly on the magnetic
C  longitude/universal time grid, then interpolation of the orthogonal
C  polynomial coefficients in magnetic longitude/universal time is necessary
C
         ELSE
C
C  Determine the indices of the two nearest magnetic longitudes/universal times
C  on the magnetic longitude/universal time grid and the magnetic longitude/
C  universal time interpolation factor
C
            IF(MLOUT .LT. SMLOUT) THEN
               IMLOU1=NMLOUT
               IMLOU2=1
               TMLOUT=(MLOUT-SMLOUT)/DMLOUT+1.
            ELSE
               IF(IMLOUT .EQ. NMLOUT) THEN
                  IMLOU1=NMLOUT
                  IMLOU2=1
               ELSE
                  IMLOU1=IMLOUT
                  IMLOU2=IMLOUT+1
               ENDIF
               TMLOUT=(MLOUT-MLOUTG)/DMLOUT
            ENDIF
            ONEMTM=1.-TMLOUT
C
C  Loop over the number of EOFs used to construct an ion density altitude
C  profile
C
C     2005 Aug 31  James M Anderson  --JIVE  reorder loops for speedup
            DO IEOF=1,NEOF
               EC(IEOF)=0.
            ENDDO
            DO IOPM11=1,NOPM11
               DO IEOF=1,NEOF
                  EC(IEOF)=EC(IEOF)+(ONEMTM*OPC(IOPM11,IEOF,IMLOU1)
     &                              +TMLOUT*OPC(IOPM11,IEOF,IMLOU2))
     &                             *OP(IMLAT,IOPM11)
               ENDDO
            ENDDO

c$$$            DO 400 IEOF=1,NEOF
c$$$C
c$$$C  Initialize the EOF coefficient
c$$$C
c$$$               EC(IEOF)=0.
c$$$C
c$$$C  Loop over the orthogonal polynomial coefficients
c$$$C
c$$$               DO 300 IOPM11=1,NOPM11
c$$$C
c$$$C  Add the contribution to the EOF coefficient by the orthogonal polynomial,
c$$$C  linearly interpolating the orthogonal polynomial coefficients from the
c$$$C  orthogonal polynomial coefficients at the two nearest magnetic longitudes/
c$$$C  universal times on the magnetic longitude/universal time grid
c$$$C
c$$$                  EC(IEOF)=EC(IEOF)+(ONEMTM*OPC(IOPM11,IEOF,IMLOU1)
c$$$     &                              +TMLOUT*OPC(IOPM11,IEOF,IMLOU2))
c$$$     &                             *OP(IMLAT,IOPM11)
c$$$C
c$$$C  End the loop over orthogonal polynomial coefficients
c$$$C
c$$$  300          CONTINUE
c$$$C
c$$$C  End the loop over the number of EOFs used to construct an ion density
c$$$C  altitude profile
c$$$C
c$$$  400       CONTINUE
c$$$C
         ENDIF
C
C  If the magnetic latitude is not exactly on the magnetic latitude grid, then
C  interpolation of the orthogonal polynomials in magnetic latitude is
C  necessary
C
      ELSE
C
C  Linearly interpolate the orthogonal polynomial from the orthogonal
C  polynomials at the two nearest magnetic latitudes on the magnetic latitude
C  grid
C
         ONEMT=1.-T
         DO 500 IOPM11=1,NOPM11
            OPINT(IOPM11)=ONEMT*OP(IMLAT1,IOPM11)
     &                   +T*OP(IMLAT2,IOPM11)
  500    CONTINUE
C
C  If the magnetic longitude/universal time is exactly on the magnetic
C  longitude/universal time grid, then no interpolation of the orthogonal
C  polynomial coefficients in magnetic longitude/universal time is necessary
C
         IF(MLOUT .EQ. MLOUTG) THEN
C
C  Loop over the number of EOFs used to construct an ion density altitude
C  profile
C
C     2005 Aug 31  James M Anderson  --JIVE  reorder loops for speedup
            DO IEOF=1,NEOF
               EC(IEOF)=0.
            ENDDO
            DO IOPM11=1,NOPM11
               DO IEOF=1,NEOF
                  EC(IEOF)=EC(IEOF)
     &                    +OPC(IOPM11,IEOF,IMLOUT)*OPINT(IOPM11)
               ENDDO
            ENDDO



c$$$            DO 700 IEOF=1,NEOF
c$$$C
c$$$C  Initialize the EOF coefficient
c$$$C
c$$$               EC(IEOF)=0.
c$$$C
c$$$C  Loop over the orthogonal polynomial coefficients
c$$$C
c$$$               DO 600 IOPM11=1,NOPM11
c$$$C
c$$$C  Add the contribution to the EOF coefficient by the interpolated orthogonal
c$$$C  polynomial
c$$$C
c$$$                  EC(IEOF)=EC(IEOF)
c$$$     &                    +OPC(IOPM11,IEOF,IMLOUT)*OPINT(IOPM11)
c$$$C
c$$$C  End the loop over orthogonal polynomial coefficients
c$$$C
c$$$  600          CONTINUE
c$$$C
c$$$C  End the loop over the number of EOFs used to construct an ion density
c$$$C  altitude profile
c$$$C
c$$$  700       CONTINUE
C
C  If the magnetic longitude/universal time is not exactly on the magnetic
C  longitude/universal time grid, then interpolation of the orthogonal
C  polynomial coefficients in magnetic longitude/universal time is necessary
C
         ELSE
C
C  Determine the indices of the two nearest magnetic longitudes/universal times
C  on the magnetic longitude/universal time grid and the magnetic longitude/
C  universal time interpolation factor
C
            IF(MLOUT .LT. SMLOUT) THEN
               IMLOU1=NMLOUT
               IMLOU2=1
               TMLOUT=(MLOUT-SMLOUT)/DMLOUT+1.
            ELSE
               IF(IMLOUT .EQ. NMLOUT) THEN
                  IMLOU1=NMLOUT
                  IMLOU2=1
               ELSE
                  IMLOU1=IMLOUT
                  IMLOU2=IMLOUT+1
               ENDIF
               TMLOUT=(MLOUT-MLOUTG)/DMLOUT
            ENDIF
            ONEMTM=1.-TMLOUT
C
C  Loop over the number of EOFs used to construct an ion density altitude
C  profile
C
C     2005 Aug 31  James M Anderson  --JIVE  reorder loops for speedup
            DO IEOF=1,NEOF
               EC(IEOF)=0.
            ENDDO
            DO IOPM11=1,NOPM11
               DO IEOF=1,NEOF
                  EC(IEOF)=EC(IEOF)+(ONEMTM*OPC(IOPM11,IEOF,IMLOU1)
     &                              +TMLOUT*OPC(IOPM11,IEOF,IMLOU2))
     &                             *OPINT(IOPM11)
               ENDDO
            ENDDO


c$$$            DO 900 IEOF=1,NEOF
c$$$C
c$$$C  Initialize the EOF coefficient
c$$$C
c$$$               EC(IEOF)=0.
c$$$C
c$$$C  Loop over the orthogonal polynomial coefficients
c$$$C
c$$$               DO 800 IOPM11=1,NOPM11
c$$$C
c$$$C  Add the contribution to the EOF coefficient by the interpolated orthogonal
c$$$C  polynomial, linearly interpolating the orthogonal polynomial coefficients
c$$$C  from the orthogonal polynomial coefficients at the two nearest magnetic
c$$$C  longitudes/universal times on the magnetic longitude/universal time grid
c$$$C
c$$$                  EC(IEOF)=EC(IEOF)+(ONEMTM*OPC(IOPM11,IEOF,IMLOU1)
c$$$     &                              +TMLOUT*OPC(IOPM11,IEOF,IMLOU2))
c$$$     &                             *OPINT(IOPM11)
c$$$C
c$$$C  End the loop over orthogonal polynomial coefficients
c$$$C
c$$$  800          CONTINUE
c$$$C
c$$$C  End the loop over the number of EOFs used to construct an ion density
c$$$C  altitude profile
c$$$C
c$$$  900       CONTINUE
c$$$C
         ENDIF
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE ORPOLY(MAXOP,MAXMX,NOPM1,NX,X,U)
C
C  PURPOSE
C     To generate orthogonal polynomials.
C
C  METHOD
C     See the reference for a description of orthogonal polynomials.
C
C  INPUT PARAMETERS
C  NAME  TYPE ARRAY         Description
C  MAXOP INTEGER            Maximum number of orthogonal polynomials
C  MAXMX INTEGER            Maximum number of grid points
C  NOPM1 INTEGER            The number of orthogonal polynomials - 1
C  NX    INTEGER            The number of points on the grid for the
C                           orthogonal polynomials
C  X     INTEGER            The grid for the orthogonal polynomials
C
C  OUTPUT PARAMETERS
C  NAME  TYPE ARRAY         Description
C  U     REAL (MAXMX,MAXOP) Orthogonal polynomials
C
C  LOCAL VARIABLES
C  NAME  TYPE ARRAY         Description
C     B  REAL (MXOP)        A term in the recursion relation
C     H2 REAL (MXOP)        A term in the recursion relation
C     I                     A loop counter
C     J                     A loop counter
C     JMAX                  NOPM1+1
C     JM1                   J-1
C     JM2                   J-2
C     RATIO                 A term in the recursion relation
C     USQ                   U**2
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
C     1.1   7-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   Squares (x**2) have been changed to
C                                   products for faster calculation.
C                                   Redundant calculation of quantities
C                                   U(I,2)**2, NOPM1+1, J-1, J-2, and U(I,J)**2
C                                   has been eliminated by storing in
C                                   variables.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   MXOP increased from 15 to 25.
C
C  REFERENCES
C     1. Beckmann, P., 'Orthogonal Polynomials For Engineers and Physicists',
C        The Golam Press, Boulder, CO, 1973, pp 90-92.
C
C  SPECIAL CONSTANTS
C     MXOP     The maximum size of the orthogonal polynomial grid
C
C
      INTEGER MXOP,MAXMX,MAXOP
      PARAMETER(MXOP=25)
      INTEGER NOPM1,NX
      INTEGER I,J
      INTEGER JMAX,JM1,JM2
      REAL U(MAXMX,MAXOP),X(MAXMX)
      REAL H2(MXOP),B(MXOP)
      REAL RATIO
      REAL*8 USQ
C     2005 Aug 31  James M Anderson  --JIVE  see what I can do for speedup
      REAL*8 B_TEMP, H_TEMP
C
C  Calculate U(X,1)
C
      H2(1)=FLOAT(NX)
      B_TEMP = 0.0
      !B(1)=0.
      DO I=1,NX
         U(I,1)=1.
         B_TEMP = B_TEMP+X(I)
         !B(1)=B(1)+X(I)
      ENDDO
      B(1)=B_TEMP/H2(1)
      !B(1)=B(1)/H2(1)
C
C  Calculate U(X,2)
C
      H_TEMP = 0.0
      B_TEMP = 0.0
      !H2(2)=0.
      !B(2)=0.
c$$$      DO I=1,NX
c$$$         U(I,2)=X(I)-B(1)
c$$$         USQ=U(I,2)*U(I,2)
c$$$         H_TEMP = H_TEMP+USQ
c$$$         B_TEMP=B_TEMP+X(I)*USQ
c$$$         !H2(2)=H2(2)+USQ
c$$$         !B(2)=B(2)+X(I)*USQ
c$$$      ENDDO
      DO I=1,NX
         U(I,2)=X(I)-B(1)
      ENDDO
      DO I=1,NX
         USQ=U(I,2)*U(I,2)
         H_TEMP = H_TEMP+USQ
         B_TEMP=B_TEMP+X(I)*USQ
         !H2(2)=H2(2)+USQ
         !B(2)=B(2)+X(I)*USQ
      ENDDO
      H2(2)=H_TEMP
      B(2)=B_TEMP/H_TEMP
      !B(2)=B(2)/H2(2)
C
C  Use the recursion relationship to calculate U(X,N) where N=3,NOPM1+1
C
      JMAX=NOPM1+1
      DO J=3,JMAX
         H_TEMP = 0.0
         B_TEMP = 0.0
         !H2(J)=0.
         !B(J)=0.
         JM1=J-1
         JM2=J-2
         RATIO=H2(JM1)/H2(JM2)
c$$$         DO I=1,NX
c$$$            U(I,J)=(X(I)-B(JM1))*U(I,JM1)-RATIO*U(I,JM2)
c$$$            USQ=U(I,J)*U(I,J)
c$$$            H_TEMP=H_TEMP+USQ
c$$$            B_TEMP=B_TEMP+X(I)*USQ
c$$$            !H2(J)=H2(J)+USQ
c$$$            !B(J)=B(J)+X(I)*USQ
c$$$         ENDDO
         DO I=1,NX
            U(I,J)=(X(I)-B(JM1))*U(I,JM1)-RATIO*U(I,JM2)
         ENDDO
         DO I=1,NX
            USQ=U(I,J)*U(I,J)
            H_TEMP=H_TEMP+USQ
            B_TEMP=B_TEMP+X(I)*USQ
            !H2(J)=H2(J)+USQ
            !B(J)=B(J)+X(I)*USQ
         ENDDO
         H2(J)=H_TEMP
         B(J)=B_TEMP/H_TEMP
         !B(J)=B(J)/H2(J)
      ENDDO
C
      RETURN
      END
C
      SUBROUTINE GENDEN(MEOF,MALT,NEOF,EOF,EC,NALT,DEN)
C
C  PURPOSE
C     To construct an ion density altitude profile from EOFs and EOF
C     coefficients.
C
C  METHOD
C     The EOFs are combined with the EOF coefficients to produce the natural
C     logarithm of an ion density altitude profile.
C
C  INPUT PARAMETERS
C     EC       EOF coefficients
C     EOF      Empirical orthogonal functions (EOFs)
C     NALT     The number of altitude points
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C
C  OUTPUT PARAMETERS
C     DEN      The natural logarithm of the ion density, in cm-3
C
C  LOCAL VARIABLES
C     I        A loop counter
C     J        A loop counter
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
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        21-Feb-1991   1.0.3
C     L. Brown        25-Sep-1995   1.0.3 ==> 1.3
C                                   The natural logarithm of the ion density is
C                                   now returned instead of the actual value.
C     2005 Aug 31  James M Anderson  --JIVE  reorder loops to speed up
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C
      INTEGER MALT,MEOF
C
      INTEGER NEOF,NALT
      INTEGER I,J
      REAL EOF(MALT,MEOF),EC(MEOF),DEN(MALT)

C     Initialize the natural logarithm of the density at a given altitude
      DO J=1,NALT
         DEN(J)=0.
      ENDDO
C  Sum the EOF terms for the natural logarithm of the density
      DO I=1,NEOF
         DO J=1,NALT
            DEN(J)=DEN(J)+EC(I)*EOF(J,I)
         ENDDO
      ENDDO




c$$$C
c$$$C  Loop over altitude
c$$$C
c$$$      DO J=1,NALT
c$$$C
c$$$C  Initialize the natural logarithm of the density at a given altitude
c$$$C
c$$$         DEN(J)=0.
c$$$C
c$$$C  Sum the EOF terms for the natural logarithm of the density
c$$$C
c$$$         DO I=1,NEOF
c$$$            DEN(J)=DEN(J)+EC(I)*EOF(J,I)
c$$$         ENDDO
c$$$C
c$$$C  Convert the result to an actual density by exponentiation
c$$$C
c$$$C         DEN(J)=EXP(DEN(J))
c$$$C
c$$$C  End of the loop over altitude
c$$$C
c$$$      ENDDO
c$$$C
      RETURN
      END
C
      SUBROUTINE GEN_E(MLAT,MLON,NALT,ALT,DEN)
C
C     PURPOSE
C       To generate the E layer profiles from the original coefficients
C
C     METHOD
C       Generate the EOF coefficients from the othogonal function
C     coefficients.
C
C     INPUT PARAMETERS
C     NAME       TYPE      ARRAY        Description
C     MLAT       REAL                   Magnetic latitude of point in
C                                       question
C     MLON       REAL                   Magnetic longitude of point in
C                                       question
C     OUTPUT PARAMETERS
C     NAME       TYPE      ARRAY        Description
C     ALT        REAL      (MALT,2)     Altitude array for densities
C     DEN        REAL      (MALT,2)     Density of each molecular ion
C     NALT       INTEGER    (2)         Number of altitude- density points
C                                       for each ion
C
C     LOCAL VARIABLES
C     NAME       TYPE      ARRAY        Description
C     DENMIN     REAL                   The minimum allowed value of the natural
C                                       logarithm of the ion density
C     EC         REAL      (MEOF)       Empirical orthogonal function co-
C                                       efficients
C     I          INTEGER                Do loop index
C     IMLAT      INTEGER                Magnetic latitude index number
C     IMLAT1     INTEGER                Lower magnetic latitude index number
C                                       if two databases must be read
C     IMLAT2     INTEGER                Higher magnetic latitude index number
C                                       if two databases must be read
C     J          INTEGER                Do loop index
C     KP2        INTEGER                K+2
C     LP2        INTEGER                L+2
C     NRCNRW     INTEGER                NREC+NROW
C     NROWP1     INTEGER                NROW+1
C     T          REAL                   Interpolation index
C     TEST       LOGICAL                Read test
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.9
C                                   In the IF statement that checks for a
C                                   magnetic latitude exactly on the magnetic
C                                   latitude grid, a bug has been fixed by
C                                   changing "ABS(MLAT)" to "MLAT".
C                                   Calculation of magnetic latitude indices
C                                   and interpolation factor has been moved
C                                   outside loops to eliminate redundant
C                                   calculation, assuming that the magnetic
C                                   latitude grid is the same throughout the
C                                   LME database.
C                                   Calculation of NREC has been optimized.
C                                   Redundant calculation of quantities
C                                   NREC+NROW, K+3, L+3, and NROW+1 has been
C                                   eliminated by storing in variables.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   Common block LOW_E1 (in INCLUDE file
C                                   'low_e.inc') has been modified to reflect
C                                   the elimination of Fourier fitting in
C                                   magnetic longitude in the low- and mid-
C                                   latitude E-region parameterized model
C                                   (LME).
C                                   The calls to GENFC and GENEC have been
C                                   combined into a single call to GENEC to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the low- and mid-
C                                   latitude E-region parameterized model
C                                   (LME).
C     L. Brown         27-Apr-1995  1.1 ==> 1.2
C                                   The output profiles are now interpolated
C                                   from a 2 x 2 (F10.7 x Kp) profile matrix
C                                   instead of a 3 x 3 profile matrix.
C                                   Removed input parameters F10P7 and KP since
C                                   they are no longer used.
C     L. Brown         25-Sep-1995  1.2 ==> 1.3
C                                   Routine GENDEN now returns the natural
C                                   logarithm of the ion density instead of the
C                                   actual value.
C                                   Peak density is now interpolated in F10.7
C                                   and Kp instead of critical frequency.
C
      INCLUDE 'low_e.inc'
C
C     I/O variable declaration
C
      INTEGER NALT(2),N2ALT(MPOSS,2)
C
      REAL MLAT,MLON,ALT(MALT,2),DEN(MALT,2),AFPEL
      PARAMETER (AFPEL=-4.7127554)
      REAL TMPALT(MALT,MPOSS*2),TMPDEN(MALT,MPOSS*2),FF2(4),FF1(4)
C
C     Local variable declaration
C
      INTEGER I,J,K,L,IMLAT,IMLAT1,IMLAT2,NREC,NROW
      INTEGER NRCNRW,KP2,LP2,NROWP1
      REAL EC(MEOF),T,DF1(4),DF2(4),SV1(4,2),SV2(4,2)
      REAL NMAX1,NMAX2,HMAX1,HMAX2,NMAXIN(MPOSS),HMAXIN(MPOSS),DUM1,DUM2
      REAL DENMIN
      LOGICAL TEST
C
C     Executable code
C
      NROW = NKPLE*NF10P7LE
      DO I = 1,2
       NALT(I) = N1ALT(I,1)
       DO J = 1,NROW
        N2ALT(J,I) = N1ALT(I,J)
       ENDDO
       DO J = 1,NALT(I)
        ALT(J,I) = ALT1(J,I,1)
       ENDDO
      ENDDO
C
C  Determine the location of the magnetic latitude on the magnetic latitude
C  grid
C
      IMLAT=MIN(NMLAT(1,1),MAX(1,1+INT((MLAT-SMLAT(1,1))/DMLAT(1,1))))
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then the
C  orthogonal polynomials do not need to be interpolated
C
      IF(SMLAT(1,1)+DMLAT(1,1)*FLOAT(IMLAT-1) .EQ. MLAT) THEN
         TEST=.TRUE.
C
C  Otherwise, the orthogonal polynomials will be interpolated to the magnetic
C  latitude
C
      ELSE
         TEST=.FALSE.
         IF(IMLAT .GE. NMLAT(1,1)) THEN
            IMLAT1=NMLAT(1,1)-1
            IMLAT2=NMLAT(1,1)
         ELSE
            IMLAT1=IMLAT
            IMLAT2=IMLAT+1
         ENDIF
         T=(MLAT-(SMLAT(1,1)+DMLAT(1,1)*FLOAT(IMLAT1-1)))/DMLAT(1,1)
      ENDIF
C
      NREC=0
C      DENMIN=LOG(1.E-16)
      DENMIN=-36.8414
      DO L = 1,NKPLE
       DO K =1 ,NF10P7LE
        NREC=NREC+1
        NRCNRW=NREC+NROW
        DO I = 1,2
C
C  Generate EOF coefficients from the orthogonal polynomial coefficients
C
         CALL GENEC(MEOF,MOPM1,MX,MMLON,DELTA(I,NREC),NOPM1(I,NREC),
     &              NEOF(I,NREC),NMLAT(I,NREC),IMLAT,IMLAT1,IMLAT2,T,
     &              TEST,OPC(1,1,1,I,NREC),
     &              MOD(360.+MOD(MLON,360.),360.),NMLON(I,NREC),
     &              SMLON(I,NREC),DMLON(I,NREC),EC)
C
C        Generate an O+ density profile from the EOFs and EOF coefficients
C
         CALL GENDEN(MEOF,MALT,NEOF(I,NREC),EOF(1,1,I,NREC),EC,
     1   N1ALT(I,NREC),DEN(1,I))
         IF ( I .EQ. 1) THEN
          DO J = 1,N1ALT(I,NREC)
           TMPALT(J,NREC) = ALT1(J,I,NREC)
           TMPDEN(J,NREC) = MAX(DEN(J,I),DENMIN)
          ENDDO
         ELSE
          DO J = 1,N1ALT(I,NREC)
           TMPALT(J,NRCNRW) = ALT1(J,I,NREC)
           TMPDEN(J,NRCNRW) = MAX(DEN(J,I),DENMIN)
          ENDDO
         ENDIF
        ENDDO
C
C       Next, get the critical frequency and heights. Notice that
C       we will find bothe HmE and FoE, but also the global maximum
C       for the molecular ions. This should allow this calculation
C       to be valid even if the user is restricting the range of altitudes
C       for valid E layer maximum.
C
C       Get the critical frequency and height for the NO+ ion for altitude
C       restricted to below 150. Km
C
        KP2=K+2
        CALL GET_FOS(N1ALT(1,NREC),TMPALT(1,NREC),TMPDEN(1,NREC),
     1  N1ALT(1,NREC),TMPALT(1,NREC),TMPDEN(1,NREC),DF1(KP2),
     2  SV1(KP2,L),DF1(K),SV1(K,L),'E')
C
C       Get the critical frequency and height for the NO+ ion for no altitude
C       restriction
C
        CALL GET_FOS(N1ALT(1,NREC),TMPALT(1,NREC),TMPDEN(1,NREC),
     1  N1ALT(1,NREC),TMPALT(1,NREC),TMPDEN(1,NREC),DUM2,
     2  SV1(KP2,L),DUM1,SV1(K,L),'F')
C
C       Compute the difference between the E layer maximum and the
C       global molecular ion maximum ( 0 if there is no restriction
C       on HmE
C
C
        SV1(K,L) = 2*(LOG(SV1(K,L)) -LOG(DF1(K)))
        DF1(K)=1.24E4*DF1(K)*DF1(K)
        SV1(KP2,L) = SV1(KP2,L)-DF1(KP2)
C
C       Get the critical frequency and height for the O2+ ion for altitude
C       restricted to below 150. Km
C
        CALL GET_FOS(N1ALT(1,NREC),TMPALT(1,NRCNRW),
     1  TMPDEN(1,NRCNRW),N1ALT(1,NREC),TMPALT(1,NRCNRW),
     2  TMPDEN(1,NRCNRW),DF2(KP2),SV2(KP2,L),DF2(K),SV2(K,L),'E')
C
C       Get the critical frequency and height for the O2+ ion for no altitude
C       restriction.
C
        CALL GET_FOS(N1ALT(1,NREC),TMPALT(1,NRCNRW),
     1  TMPDEN(1,NRCNRW),N1ALT(1,NREC),TMPALT(1,NRCNRW),
     2  TMPDEN(1,NRCNRW),DF2(KP2),SV2(KP2,L),DF2(K),SV2(K,L),'F')
C
        SV2(K,L) = 2*(LOG(SV2(K,L)) - LOG(DF2(K)))
        DF2(K)=1.24E4*DF2(K)*DF2(K)
        SV2(KP2,L) = SV2(KP2,L)-DF2(KP2)
       ENDDO
C
C      Fit over the F10P7 data
C
       LP2=L+2
       CALL LINR(FF1(L),DF1,TF10P7LE,OMTF10P7LE)
       CALL LINR(FF1(LP2),DF1(3),TF10P7LE,OMTF10P7LE)
       CALL LINR(FF2(L),DF2,TF10P7LE,OMTF10P7LE)
       CALL LINR(FF2(LP2),DF2(3),TF10P7LE,OMTF10P7LE)
      ENDDO
C
C     Fit over the KP data
C
      CALL LINR(NMAX1,FF1,TKPLE,OMTKPLE)
      NMAX1=LOG(NMAX1)
      CALL LINR(HMAX1,FF1(3),TKPLE,OMTKPLE)
      CALL LINR(NMAX2,FF2,TKPLE,OMTKPLE)
      NMAX2=LOG(NMAX2)
      CALL LINR(HMAX2,FF2(3),TKPLE,OMTKPLE)
C
C     Get the actual numbers to send the profiles to
C
      NREC=0
      DO L = 1,NKPLE
       DO K = 1,NF10P7LE
        NREC=NREC+1
        NMAXIN(NREC) = NMAX1 + SV1(K,L)
        HMAXIN(NREC) = HMAX1 + SV1(K+2,L)
       ENDDO
      ENDDO
C
C     Merge the various profiles for the final profile
C
      CALL MERGE4(MALT,NF10P7LE,NKPLE,N2ALT,TF10P7LE,OMTF10P7LE,TKPLE,
     &            OMTKPLE,TMPALT,TMPDEN,NMAXIN,HMAXIN,DEN)
C
      NREC=0
      DO L = 1,NKPLE
       DO K = 1,NF10P7LE
        NREC=NREC+1
        NMAXIN(NREC) = NMAX2 + SV2(K,L)
        HMAXIN(NREC) = HMAX2 + SV2(K+2,L)
       ENDDO
      ENDDO
C
      NROWP1=NROW+1
      CALL MERGE4(MALT,NF10P7LE,NKPLE,N2ALT(1,2),TF10P7LE,OMTF10P7LE,
     &            TKPLE,OMTKPLE,TMPALT(1,NROWP1),TMPDEN(1,NROWP1),
     &            NMAXIN,HMAXIN,DEN(1,2))
      DO J = 1,2
       DO I = 1,NALT(J)
        DEN(I,J) = EXP(DEN(I,J))
        IF (DEN(I,J) .LE. 0.0) DEN(I,J) = 1.E-18
       ENDDO
      ENDDO
      RETURN
      END
C
