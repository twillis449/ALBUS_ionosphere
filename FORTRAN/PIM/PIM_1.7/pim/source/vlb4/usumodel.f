      SUBROUTINE USUMODEL(MLAT,MLT,FOE,HME,FOF2,HMF2,EDEN,FDEN)
C
C  PURPOSE
C     To generate E-layer and F-layer densities and critical parameters from
C     the parameterized USU model.
C
C  METHOD
C     The parameterized USU model is used to generate E-layer and F-layer
C     densities.  The peaks of the densities are determined to calculate
C     critical parameters.  The critical parameters are interpolated in F10.7
C     and Kp, and then used to merge the density profiles in F10.7 and Kp.  The
C     peaks of the merged density profiles are determined to calculate final
C     critical parameters.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     MLAT   Real       Scalar        deg N           -90. <= MLAT <= 90.
C        Magnetic latitude
C     MLT    Real       Scalar        hr              0. <= MLT < 24.
C        Magnetic local time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EDEN   Real       Vector        log(cm-3)       n/a
C                       (40)
C        The logarithmic E-layer density
C     FDEN   Real       Vector        log(cm-3)       n/a
C                       (40)
C        The logarithmic F-layer density
C     FOE    Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency, not including the contribution from
C        the F-layer density
C     FOF2   Real       Scalar        MHz             >= 0.
C        The F-layer critical frequency, not including the contribution from
C        the E-layer density
C     HME    Real       Scalar        km              >= 0.
C        The E-layer critical height
C     HMF2   Real       Scalar        km              >= 0.
C        The F-layer critical height
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DATAC  Real       Matrix        n/a             n/a
C                       (4,2)
C        Profile peak parameters
C     DMAX   Real       Scalar        log(cm-3)       n/a
C        The interpolated logarithmic peak density
C     DUM1   Real       Matrix        n/a             n/a
C                       (4,2)
C        Profile peak parameters interpolated in F10.7
C     HMAXIN Real       Vector        km              >= 0.
C                       (MPOSS)
C        Peak density height for the profile merging
C     IALT   Integer    Scalar        n/a             1 <= IALT <= 37
C        The altitude index
C     IF10P7 Integer    Scalar        n/a             1 <= IF10P7 <= NF10P7H
C        The F10.7 index
C     IHSP   Integer    Scalar        n/a             1 or 2
C        The hemisphere index
C     IKP    Integer    Scalar        n/a             1 <= IKP <= NKPH
C        The Kp index
C     IPOSS  Integer    Scalar        n/a             1 <= IPOSS <= NPOSS
C        The profile index
C     NDALT  Integer    Vector        n/a             >= 1
C                       (MPOSS)
C        The number of points in the altitude grids for the profile merging
C     NMAXIN Real       Vector        log(cm-3)       n/a
C                       (MPOSS)
C        Logarithmic peak density for the profile merging
C     NPOSS  Integer    Scalar        n/a             1 <= NPOSS <= MPOSS
C        The number of profiles to be merged
C     RDMIN  Integer    Scalar        log(cm-3)       n/a
C        The minimum allowed F-layer density
C     REC_DATA
C            Real       Matrix        log(cm-3)       n/a
C                       (40,3)
C        Logarithmic ion density profiles reconstructed from the parameterized
C        USU model
C     TMPALT Real       Matrix        km              >= 0.
C                       (40,MPOSS,2)
C        Altitude grids for the profile merging
C     TMPDEN Real       Matrix        log(cm-3)       n/a
C                       (40,MPOSS,2)
C        Logarithmic density profiles to be merged
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     FNDMAX Determines the peak density and height of a density profile
C     GET_INDX
C            Determines the hemisphere index
C     LINR   Linear interpolation
C     MERGE4 Merges four density profiles
C     NEWGRID
C            Interpolates a density profile onto a new altitude grid
C     RECON  Reconstructs ion density profiles from the parameterized USU model
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
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   30-September-1996
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     W. Whartenby                Created
C     L. Brown       22-Sep-1994  1.0.3 ==> 1.0.9
C                                 Calculation of NREC has been optimized.
C                                 A call to NEWGRID has been eliminated,
C                                 assuming that the altitude grid is the same
C                                 for the molecular ions throughout the USU
C                                 database.
C                                 Redundant calculation of quantities
C                                 2.*LOG(FOE/8.98E-3), NF10P7H*NKPH, and
C                                 2.*LOG(FOF2/8.98E-3) has been eliminated by
C                                 storing in variables.
C     L. Brown        27-Apr-1995 1.0.9 ==> 1.2
C                                 The output profiles are now interpolated from
C                                 a 2 x 2 (F10.7 x Kp) profile matrix instead
C                                 of a 3 x 3 profile matrix.
C     L. Brown       25-Sep-1995  1.2 ==> 1.3
C                                 Removed unnecessary initializations of
C                                 REC_DATA matrix.
C                                 Added layer flag to calls to routine RECON.
C                                 Routine RECON now returns the natural
C                                 logarithm of the ion densities instead of the
C                                 actual values.
C                                 Removed magnetic latitude scalings since they
C                                 are not used.
C                                 Removed unnecessary calls to routine SCALEP.
C                                 Peak density now is interpolated in F10.7 and
C                                 Kp instead of critical frequency.
C     L. Brown       30-Sep-1996  1.3 ==> 1.5
C                                 Rewrite for optimization and readability.
C                                 Removed input argument LAYR since it is no
C                                 longer used.
C                                 Changed intrinsic function ALOG to its
C                                 generic equivalent LOG.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     F2TOD  Real       cm-3 MHz-2      The conversion factor from frequency**2
C                                       (MHz**2) to density (cm-3)
C
      REAL F2TOD
      PARAMETER(F2TOD=1.24E4)
C
      INCLUDE 'indirect.inc'
      INCLUDE 'usuarr.inc'
C
C  Input variables
C
      REAL MLAT,MLT
C
C  Output variables
C
      REAL FOF2,HMF2,FOE,HME
      REAL EDEN(40),FDEN(40)
C
C  Local variables
C
      INTEGER IHSP,IPOSS,IKP,IF10P7,IALT,NPOSS
      INTEGER NDALT(MPOSS)
      REAL RDMIN,DMAX
      REAL REC_DATA(40,3),TMPDEN(40,MPOSS,2),TMPALT(40,MPOSS,2),
     &     DATAC(4,2),DUM1(4,2),HMAXIN(MPOSS),NMAXIN(MPOSS)
C
C  Determine the hemisphere index
C
      CALL GET_INDX(MLAT,IHSP)
C
C  Defined the minimum allowed F-layer density
C
C      RDMIN=LOG(1.E-6)
      RDMIN=-36.8414
C
C  Define the number of profiles to be merged
C
      NPOSS=NF10P7H*NKPH
C
C  Initialize the profile index
C
      IPOSS=0
C
C  Loop over Kp
C
      DO 200 IKP=1,NKPH
C
C  Loop over F10.7
C
         DO 100 IF10P7=1,NF10P7H
C
C  Increment the profile index
C
            IPOSS=IPOSS+1
C
C  Store the number of altitudes for the profile
C
            NDALT(IPOSS)=37
C
C  Reconstruct ion density profiles from the parameterized USU model
C
            CALL RECON(ABS(MLAT),MLT,REC_DATA,A(1,1,1,IHSP,IPOSS),
     &                 CAPA(0,1,1,1,IHSP,IPOSS))
C
C  Loop over altitude
C
            DO 10 IALT=1,37
C
C  Store the altitude grid for the E-layer density
C
               TMPALT(IALT,IPOSS,1)=Z(IALT,3,IHSP,IPOSS)
C
C  Store the E-layer density
C
               TMPDEN(IALT,IPOSS,1)=LOG(EXP(REC_DATA(IALT,2))
     &                                 +EXP(REC_DATA(IALT,3)))
C
C  Store the altitude grid for the F-layer density
C
               TMPALT(IALT,IPOSS,2)=Z(IALT,1,IHSP,IPOSS)
C
C  Store the F-layer density
C
               TMPDEN(IALT,IPOSS,2)=MAX(REC_DATA(IALT,1),RDMIN)
C
C  End the loop over altitude
C
   10       CONTINUE
C
C  Find the peak of the E-layer density
C
            CALL FNDMAX(37,TMPALT(1,IPOSS,1),TMPDEN(1,IPOSS,1),
     &                  DATAC(IF10P7,1),DATAC(IF10P7+2,1))
            DATAC(IF10P7,1)=EXP(DATAC(IF10P7,1))
C
C  Find the peak of the F-layer density
C
            CALL FNDMAX(37,TMPALT(1,IPOSS,2),TMPDEN(1,IPOSS,2),
     &                  DATAC(IF10P7,2),DATAC(IF10P7+2,2))
            DATAC(IF10P7,2)=EXP(DATAC(IF10P7,2))
C
C  End the loop over F10.7
C
  100    CONTINUE
C
C  Interpolate the E-layer peak parameters in F10.7
C
         CALL LINR(DUM1(IKP,1),DATAC(1,1),TF10P7H,OMTF10P7H)
         CALL LINR(DUM1(IKP+2,1),DATAC(3,1),TF10P7H,OMTF10P7H)
C
C  Interpolate the F-layer peak parameters in F10.7
C
         CALL LINR(DUM1(IKP,2),DATAC(1,2),TF10P7H,OMTF10P7H)
         CALL LINR(DUM1(IKP+2,2),DATAC(3,2),TF10P7H,OMTF10P7H)
C
C  End the loop over Kp
C
  200 CONTINUE
C
C  Interpolate the E-layer peak parameters in Kp
C
      CALL LINR(DMAX,DUM1(1,1),TKPH,OMTKPH)
      CALL LINR(HME,DUM1(3,1),TKPH,OMTKPH)
      DMAX=LOG(DMAX)
C
C  Store the interpolated E-layer peak parameters for the E-layer profile
C  merging
C
      DO 300 IPOSS=1,NPOSS
         NMAXIN(IPOSS)=DMAX
         HMAXIN(IPOSS)=HME
  300 CONTINUE
C
C  Merge the E-layer profiles
C
      CALL MERGE4(40,NF10P7H,NKPH,NDALT,TF10P7H,OMTF10P7H,TKPH,OMTKPH,
     &            TMPALT(1,1,1),TMPDEN(1,1,1),NMAXIN,HMAXIN,EDEN)
C
C  Calculate the E-layer critical frequency and height
C  Note:The E-layer critical frequency does not include the contribution from
C       the F-layer density.
C
      CALL FNDMAX(37,Z(1,3,IHSP,1),EDEN,DMAX,HME)
      CALL NEWGRID(37,Z(1,3,IHSP,1),EDEN,1,HME,DMAX)
      FOE=SQRT(EXP(DMAX)/F2TOD)
C
C  Interpolate the F-layer peak parameters in Kp
C
      CALL LINR(DMAX,DUM1(1,2),TKPH,OMTKPH)
      CALL LINR(HMF2,DUM1(3,2),TKPH,OMTKPH)
      DMAX=LOG(DMAX)
C
C  Store the interpolated F-layer peak parameters for the F-layer profile
C  merging
C
      DO 400 IPOSS=1,NPOSS
         NMAXIN(IPOSS)=DMAX
         HMAXIN(IPOSS)=HMF2
  400 CONTINUE
C
C  Merge the F-layer profiles
C
      CALL MERGE4(40,NF10P7H,NKPH,NDALT,TF10P7H,OMTF10P7H,TKPH,OMTKPH,
     &            TMPALT(1,1,2),TMPDEN(1,1,2),NMAXIN,HMAXIN,FDEN)
C
C  Calculate the F-layer critical frequency and height
C  Note:The F-layer critical frequency does not include the contribution from
C       the E-layer density.
C
      CALL FNDMAX(37,Z(1,1,IHSP,1),FDEN,DMAX,HMF2)
      CALL NEWGRID(37,Z(1,1,IHSP,1),FDEN,1,HMF2,DMAX)
      FOF2=SQRT(EXP(DMAX)/F2TOD)
C
      RETURN
      END
C
      SUBROUTINE RECON(XNMLAT,XNMLT,REC_DATA,A,CAPA)
C
C  PURPOSE
C   To reconstruct the values of the altitude profiles from the orthogonal
C   polynomial representation of the Fourier coefficients.
C
C  METHOD
C   Reconstruct the Fourier coefficients from the orthogonal polynomial
C   representation and the reconstruct the empirical orthogonal coefficients
C   from the Fourier series. Next, multiply the derived coefficients by the
C   empirical orthogonal functions for the natural log of the data.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY     description
C    A       REAL   (40,6,3)      The empirical orthogonal functions
C    CAPA    REAL   (0:8,17,6,3)  The coefficients for the orthogonal functions
C    XNMLAT  REAL                 Real value of magnetic latitude
C    XNMLT   REAL                 Real value of local time
C
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY     description
C  REC_DATA  REAL       (40,3) The natural logarithm of the recreated data
C                       ( *,1) Is the O+ data
C                       ( *,2) Is the NO+ data
C                       ( *,3) Is the O2+ data
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY     description
C  SUBROUTINES CALLED
C    NAME             description
C    FOUR_COEFF       Generates the Fourier coefficients from the stored
C                     polynomial coefficients
C    FULL_DATA        Generates the reconstructed data from the Fourier
C                     coefficients
C    GET_TIMES        Gets the UT, mag lat and Local time for the profile
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
C     1.5   30-September-1996
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby    22-Jun-1990   1.0.3
C     L. Brown        25-Sep-1995   1.0.3 ==> 1.3
C                                   Added input parameter LAYR.
C                                   Added argument LAYR to call to routine
C                                   FOUR_COEFF.
C                                   Added argument LAYR to call to routine
C                                   FULL_DATA.
C                                   Routine FULL_DATA now returns the natural
C                                   logarithm of the ion densities instead of
C                                   the actual values.
C     L. Brown        30-Sep-1996   1.3 ==> 1.5
C                                   Removed argument LAYR from calls to
C                                   FOUR_COEFF and FULL_DATA.
C                                   Removed input argument LAYR since it is
C                                   no longer used.
C
C  REFERENCES
C     NONE (See references in FOUR_COEFF and FULL_DATA)
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of declaration, dimension and common
C
      INCLUDE 'io.inc'
      REAL XNMLAT,XNMLT
      REAL FOUR_C(17,6,3),A(40,6,3),CAPA(0:8,17,6,3),REC_DATA(40,3)
C
C     End of declaration, dimension and common statements, beginning
C     of executable code
      CALL FOUR_COEFF(CAPA,FOUR_C,XNMLAT)
C     and then recreate the altitude profiles
      CALL FULL_DATA(FOUR_C,REC_DATA,A,XNMLT)
C
      RETURN
      END
C
      SUBROUTINE GET_TIMES(XLUT,XNMLT,XNMLAT,LUT,LNMLT,LNMLAT)
C
C  PURPOSE
C    To get the correct array indices for the local time, universal time
C    and the magnetic latitude.
C
C  METHOD
C    Since the universal time indexes represent odd hours (12 values for
C    the index representing 1hrUT, 3hrUT, 5hrUT , the conversion is
C    nearest integer of (UT+1)/2. Similar conversions exist for magnetic
C    latitude (1-20 index representing 51-89 degrees in 2 degree increments)
C    and local time = index -0.5. The actual values of UT, mag lat
C    and local time will be converted to the proper array indices to determine
C    the correct record to read, and the the correct latititude and local time
C    to rebuild the profile.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    XLUT    REAL              Real value of universal time
C    XNMLAT  REAL              Real value of magnetic latitude
C    XNMLT   REAL              Real value of local time
C
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    LNMLAT  INTEGER           Index for the magnetic latitude
C    LNMLT   INTEGER           Index for the Local time
C    LUT     INTEGER           Index for the Universal time
C  LOCAL VARIABLES
C    NONE
C
C  SUBROUTINES CALLED
C    NONE
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
C     1.0.9   22-September-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   Calculation of LUT and LNMLAT has been
C                                   optimized.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of declarations
C
      REAL XLUT,XNMLAT,XNMLT
      INTEGER LUT,LNMLAT,LNMLT
C
C
C     Get the correct array index...
C
      LUT = IFIX((XLUT+1.)/2.+0.5)
C
C     Get the magnetic latitude
C
      LNMLAT = MAX(IFIX((XNMLAT-49.)/2.+0.5),1)
C
C     And the Local time
C
      LNMLT = IFIX(XNMLT+1.0)
C
      RETURN
      END
C
      SUBROUTINE FOUR_COEFF(CAPA,FVAL1,XNMLAT)
C
C  PURPOSE
C    To rebuild the Fourier coefficients from the orthogonal polynomial
C    coefficients.
C
C  METHOD
C    See reference 1.
C      a sub m(lamda) = p sub n,m * u sub n (lamda)
C      b sub m(lamda) = q sub n,m * u sub n (lamda)
C Where
C        a, b are the Fourier coefficients. There are NUM_FC a,b
C        So CAPA(0:8;1,...) rebuilds the DC term ( a sub 0),
C        CAPA(0:8,2...) rebuilds a sub 1...CAPA(0:8,(NUM_FC+1)/2,...)
C        rebuilds the a sub (num_fc+1)/2 term, which is the last a term
C        The remaining CAPA(0:8,j,...) terms rebuild the
C        b sub (j - (num_fc+1)/2) terms.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    CAPA    REAL (0:8,17,6,3) The coefficients of the derived orthogonal
C                              functions
C    DELTA   REAL              The step size for the derived orthogonal
C                              functions
C    XNMLAT  INTEGER           The mag latitude
C    NEOF    INTEGER           The number of empirical orthogonal functions
C    NUM_EFC INTEGER           The number of Fourier coefficients used in E
C                              layer fitting
C    NUM_FFC INTEGER           The number of Fourier coefficients used in F
C                              layer fitting
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    FVAL1   REAL   (17,6,3)   The reconstructed Fourier coefficients
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    B       REAL       (0:8)  B sub n as described in the reference
C    H2      REAL       (0:8)  H as described in the reference
C    I       INTEGER           Do loop index
C    J       INTEGER           Do loop index
C    L1      INTEGER           The mag latitude index
C    MU      REAL       (20)   X sub i as described in the reference
C    NN      INTEGER           The number of the derived orthogonal polynomial
C    U       REAL     (20,0:8) The derived Orthogonal polynomials
C    UNIT    REAL       (0:8)  Interpolated orthogonal polynomial
C
C  SUBROUTINES CALLED
C    NAME      description
C    USUGEN    Generates the derived orthogonal polynomials as described
C              in the reference
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C
C  AUTHOR
C     Robert E. Daniell
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
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   Calculation of the orthogonal polynomial
C                                   grid has been optimized.
C                                   Calculation of L1 has been optimized.
C                                   Linear interpolation of orthogonal
C                                   polynomial to magnetic latitude of
C                                   interest has been moved outside loops to
C                                   eliminate redundant calculation.
C     L. Brown        25-Sep-1995   1.0.9 ==> 1.3
C                                   Added input parameter LAYR.
C                                   If input parameter LAYR='E', then Fourier
C                                   coefficients for only the NO+ and O2+ ions
C                                   are rebuilt.  If input parameter LAYR='F',
C                                   then Fourier coefficients for only the O+
C                                   ion are rebuilt.
C                                   Commented out PRINT statement that displays
C                                   warning when latitude is below range.
C     L. Brown        30-Sep-1996   1.3 ==> 1.5
C                                   Fourier coefficients are now calculated for
C                                   all three ions.
C                                   Removed input argument LAYR since it is
C                                   no longer used.
C
C  REFERENCES
C     1) Beckmann, P.,  Orthogonal Polynomials For Engineers and Physicists,
C                      ------------------------------------------------------
C        The Golam Press, Boulder, CO, 1973. pp 90-92
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of declaration, dimension and common
C
      INCLUDE 'io.inc'
      REAL CAPA(0:8,17,6,3),FVAL1(17,6,3)
      REAL H2(0:8),B(0:8),MU(20),U(20,0:8)
      REAL UINT(0:8)
      INTEGER I,J,L1,L2,M,NN,NUM_FC
      REAL X,X1,X2,XNMLAT
C
C      End of declaration, dimension and common statements, beginning
C      of executable code
C
C      DELTA = 2.0/FLOAT(NMLAT)
C      Set up the grid for the polynomials
       X=-DELTA-1.
       DO J=1,NMLAT
         X = X+DELTA
         MU(J) = X
C        FVALUE(J) = 10*(0.5 + X*X*(1.0 + X))*SQRT(2.0+X)
       ENDDO
C
C      Get the magnetic latitude
C
       L1 =IFIX((XNMLAT-49.)/2.+0.5)
C
C      If the call is below the lowest magnitude of latitude for USU, then
C      set the index = 1 and print a warning message
       IF (L1 .LT. 1) THEN
        L1 = 1
C        PRINT *,' Warning in USUMODEL, Latitude for call is ',XNMLAT
       ENDIF
C      If the latitude is exactly 90 degrees, this will give L1 = 21.
C      Check for this
       IF (L1 .GT. 20) L1 = 20
C
       IF ((XNMLAT .LE. 51.) .OR. (XNMLAT .GE. 89.) .OR.
     1  (2.*L1+49. .EQ. XNMLAT)) THEN
        X1 = 1.
        X2 = 0.
        L2 = L1
       ELSE
        IF (2.*L1+49. .GT. XNMLAT) THEN
C        The index is high
         L2 = L1
         L1 = L1-1
        ELSE
C        The index is low
         L2 = L1+1
        ENDIF
        X2 = (XNMLAT-(2.*L1+49.))/2
        X1 = (2.*L2+49-XNMLAT)/2.
       ENDIF
C      Get the values of the polynomials
       CALL USUGEN(NMAX,H2,B,NMLAT,MU,U)
       DO NN=0,NMAX
          UINT(NN)=X1*U(L1,NN)+X2*U(L2,NN)
       END DO
       DO J=1,3
       NUM_FC = NUM_EFC
       IF (J .EQ. 1) NUM_FC = NUM_FFC
        DO I = 1, NEOF
         DO M = 1, NUM_FC
C          Initialize the Fourier coefficient
           FVAL1(M,I,J) = 0.0
C           PRINT 100
           DO NN=0,NMAX
C            rebuild according to ref 1.
             FVAL1(M,I,J)=FVAL1(M,I,J)+CAPA(NN,M,I,J)*UINT(NN)
           END DO
         END DO
        END DO
       END DO
      RETURN
      END
C
      SUBROUTINE USUGEN(NMAX,H2,B,IMAX,X,U)
C
C  PURPOSE
C  Generates norms HN2 and constants BN needed for recursion relation for
C  USU orthorgonal polynomials
C  METHOD
C    See the reference.
C    U sub -1 (x) = 0.0
C    U sub 0 (x) = 1.0 constant                             (1)
C    U sub n+1 (x) = (x-B sub n)*U sub n (x) -
C    (h sub n/h sub n-1)*U sub n-1 (x)                      (2)
C
C    Where
C         h sub n = sum i (U sub n (x sub i)**2)            (3)
C    and
C         B sub n = ( h sub n)**(-2)*                       (4)
C         sum i (x sub i * U sub n (X sub i))
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    IMAX    INTEGER           The number of latitude points
C    NMAX    INTEGER           The number of orthogonal polynomials
C    X       REAL      (IMAX)  The grid for the orthogonal polynomials
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    B       REAL    (0:NMAX)  The B(n) term described in the reference
C    H2      REAL    (0:NMAX)  The H sub n term**2 described in the reference
C    U       REAL(IMAX,0:NMAX) The orthogonal polynomials described in
C                              the reference
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    B0      REAL              Temporary holder for B(0)
C    B1      REAL              Temporary holder for B(1)
C    Bn      REAL              Temporary holder for B(n)
C    h12     REAL              Temporary holder for h(1)**2
C    hn2     REAL              Temporary holder for h(n)**2
C    NM1     INTEGER           N-1
C    NM2     INTEGER           N-2
C    RATIO   REAL
C    U2      REAL
C  SUBROUTINES CALLED
C    NONE
C
C  FUNCTIONS CALLED
C    NONE
C
C  FILES ACCESSED
C    NONE
C  AUTHOR
C     Robert E. Daniell
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
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   Squares (x**2) have been changed to
C                                   products (x*x) for faster calculation.
C
C  REFERENCES
C     1) Beckmann, P.,  Orthogonal Polynomials For Engineers and Physicists,
C                      ------------------------------------------------------
C        The Golam Press, Boulder, CO, 1973. pp 90-92
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of dimension statements
C
      INTEGER IMAX,NMAX,I,NM1,NM2,N
      REAL H2(0:NMAX),B(0:NMAX),X(IMAX),U(IMAX,0:NMAX)
      REAL HN2,B0,H12,B1,BN,UN2,RATIO,U2
C
C     End of dimension staements, beginning of executable code
C
      H2(0) = IMAX
      B0 = 0.0
      DO I=1,IMAX
C       U sub 0 (x) = 1.0 ( EQ 1 Above)
        U(I,0) = 1.0
        B0 = B0 + X(I)
      END DO
      B(0) = B0/H2(0)
C
      H12 = 0.0
      B1 = 0.0
      DO I=1,IMAX
C       The Eq 2 reduces to this next statement as for
C      n = 1 then  n-2 = -1 U sub -1 =0.0 and U sub 0 = 1.0.
        U(I,1) = X(I) - B(0)
        UN2 = U(I,1)*U(I,1)
C       Compute h sub 1 **2 ( eq 3)
        H12 = H12 + UN2
C       And B sub 1         ( eq 4)
        B1 = B1 + X(I)*UN2
      END DO
C     Load the appropriate arrays.
      H2(1) = H12
      B(1) = B1/H12
C     Now begin to use the full recursion relationship
      DO N=2,NMAX
        NM1 = N - 1
        NM2 = N - 2
        HN2 = 0.0
        BN = 0.0
        RATIO = H2(NM1)/H2(NM2)
        DO I=1,IMAX
C         The full recursion relationship for U sub n ( eq 2)
          U(I,N) = (X(I) - B(NM1))*U(I,NM1) - RATIO*U(I,NM2)
C         The relationship for H sub n **2 ( eq 3)
          U2 = U(I,N)*U(I,N)
          HN2 = HN2 + U2
C         And  B sub n   (eq 4)
          BN = BN + X(I)*U2
        END DO
C       Load the appropriate arrays.
        H2(N) = HN2
        B(N) = BN/HN2
      END DO
C
      RETURN
      END
C
      SUBROUTINE FULL_DATA(FOUR_C,REC_DATA,A,XNMLT)
C
C  PURPOSE
C   To regenerate the altitude profiles from the Fourier coefficients
C
C  METHOD
C     f(x) = a0/2 + a(m)*cos(m*x) + b(m)*sin(m*x)
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    A       REAL     (40,6,3) The empirical orthogonal functions
C    FOUR_C  REAL    (17,6,3)  The array of Fourier coefficients
C    LNMLT   INTEGER           The local time index
C    LNMLAT  INTEGER           The magnetic latitude index
C    NEOF    INTEGER           Number of empirical orthogonal functions
C    NMLT    INTEGER           Number of local times
C    NUM_FC  INTEGER           Number of Fourier coefficients
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C  REC_DATA  REAL     (40,3)   The natural logarithm of the reconstructed
C                              altitude profile
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY     description
C    ARG     REAL                 PI*XNMLT/12.
C    I       INTEGER              Do loop index
C    IM1                          I-1
C    IM2                          I-2
C    IMAX    INTEGER              Maximum number of Fourier terms
C    J       INTEGER              Do loop index
C    J1      INTEGER              Do loop index
C    K       INTEGER              Do loop index
C    M       INTEGER              Do loop index
C    MMAX                         Number of Fourier terms
C    MP1                          M+1
C    TEMP    REAL                 A temporary storage for parts of REC_DATA
C    TWOCOA  REAL                 2.*COS(ARG)
C    ZCS     REAL    (NUM_FC-1)/2 The cosine values needed for the
C                                 Fourier series expansion
C    ZSN     REAL    (NUM_FC-1)/2 The sine values needed for the
C                                 Fourier series expansion
C
C  SUBROUTINES CALLED
C    NONE
C
C  FUNCTIONS CALLED
C    NONE
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
C     1.5   30-September-1996
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   Calculation of sines and cosines for the
C                                   Fourier series has been optimized by 1)
C                                   moving it outside the loop and 2)
C                                   minimizing the use of the intrinsic SIN()
C                                   and COS() functions and using instead
C                                   recursion relations for sin(nx) and
C                                   cos(nx).
C                                   Evaluation of Fourier series has been
C                                   optimized.
C                                   Calls to intrinsic EXP() function reduced
C                                   in loops by reconstructing the log of the
C                                   density instead of the density itself.
C     L. Brown        25-Sep-1995   1.0.9 ==> 1.3
C                                   Added input parameter LAYR.
C                                   If input parameter LAYR='E', then altitude
C                                   profiles for only the NO+ and O2+ ions are
C                                   are regenerated.  If input parameter
C                                   LAYR='F', then an altitude profile for only
C                                   the O+ ion is regenerated.
C                                   The natural logarithm of the altitude
C                                   profiles is now returned instead of the
C                                   actual values.
C     L. Brown        30-Sep-1996   1.3 ==> 1.5
C                                   Altitude profiles are now calculated for
C                                   all three ions.
C                                   Removed input argument LAYR since it is
C                                   no longer used.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C     End of header, beginning of declaration, dimension and common
C
      INCLUDE 'io.inc'
      INTEGER I,J,J1,K,M,NUM_FC
      INTEGER IMAX,IM1,IM2,MMAX,MP1
      REAL FOUR_C(17,6,3),REC_DATA(40,3),A(40,6,3),ZCS(11),ZSN(11)
      REAL XNMLT,TEMP
      REAL ARG,TWOCOA
C
C     End of dimension, declaration and common statements, beginning
C     of executable code
      ARG=3.1415926*XNMLT/12.
C     Generate the cosines and sines before the large loop to avoid
C     Many identical calls to cos and sine functions. The local time has
C     already been selected, so the angle is fixed.
      ZSN(1)=SIN(ARG)
      ZCS(1)=COS(ARG)
      IMAX=(MAX(NUM_EFC,NUM_FFC)-1)/2
      IF(IMAX .GT. 1) THEN
         TWOCOA=2.*ZCS(1)
         ZSN(2)=ZSN(1)*TWOCOA
         ZCS(2)=ZCS(1)*TWOCOA-1.
         IF(IMAX .GT. 2) THEN
            DO I=3,IMAX
               IM1=I-1
               IM2=I-2
               ZSN(I)=ZSN(IM1)*TWOCOA-ZSN(IM2)
               ZCS(I)=ZCS(IM1)*TWOCOA-ZCS(IM2)
            ENDDO
         ENDIF
      ENDIF
C     For all three ions
      DO K = 1,3
       IF(K .EQ. 1) THEN
          NUM_FC=NUM_FFC
       ELSE
          NUM_FC=NUM_EFC
       ENDIF
       MMAX=(NUM_FC-1)/2
C      Initialize the data
       DO I = 1,NUM_HT
        REC_DATA(I,K)=0.
       END DO
C      Over all EOF
       DO J = 1,NEOF
C       Rebuild the coefficient to multiply the EOF from the Fourier series
        TEMP= FOUR_C(1,J,K)/2.
        DO M=1,MMAX
           MP1=M+1
           TEMP=TEMP+FOUR_C(MP1+MMAX,J,K)*ZSN(M)+FOUR_C(MP1,J,K)*ZCS(M)
        END DO
        DO J1 = 1,NUM_HT
         REC_DATA(J1,K)=REC_DATA(J1,K)+TEMP*A(J1,J,K)
        END DO
       END DO
C       DO J1=1,NUM_HT
C          REC_DATA(J1,K)=EXP(REC_DATA(J1,K))
C       END DO
      END DO
      RETURN
      END
C
      SUBROUTINE LINR(Y,YA,T,ONEMT)
C
C     PURPOSE
C        Linear interpolation using predetermined interpolation factors.
C
C     METHOD
C        Linear interpolation using the formula Y=Y1*(1-T)+Y2*T.
C
C     INPUT PARAMETERS
C     NAME   TYPE    ARRAY     Description
C     ONEMT  REAL              1.-T
C     T      REAL              Linear interpolation factor
C     YA     REAL     (2)      Y axis values
C
C     OUTPUT PARAMETERS
C     NAME   TYPE   ARRAY     Description
C     Y    REAL               Computed Y axis value
C
C     LOCAL VARIABLES
C     NONE
C
C     AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.2   27-April-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.9
C                                   General optimization by change of logic.
C     L. Brown        27-Apr-1995   1.0.9 ==> 1.2
C                                   Complete rewrite to redefine function.
C
C     REFERENCES
C     None
C
C     SPECIAL CONSTANTS
C     None
C
      REAL Y,YA(2),T,ONEMT
C
      Y=YA(1)*ONEMT+YA(2)*T
C
      RETURN
      END
