      SUBROUTINE MID_PARAM(MLAT,MLON,FOF2,HMF2,FOE,HME,NALT,ALTE,DENE,
     &                     ALTF,DENF)
C
C  PURPOSE
C     To generate E-layer and F-layer densities and critical parameters from
C     the parameterized mid-latitude models.
C
C  METHOD
C     The parameterized mid-latitude models are used to generate E-layer and
C     F-layer densities.  The peaks of the densities are determined to
C     calculate the critical parameters.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     MLAT   Real       Scalar        deg N           -90. <= MLAT <= 90.
C        Magnetic latitude
C     MLON   Real       Scalar        deg E           0. <= MLON < 360.
C        Magnetic longitude
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALTE   Real       Vector        km              >= 0.
C                       (NALT(1))
C        The E-layer density altitude grid
C     ALTF   Real       Vector        km              >= 0.
C                       (NALT(2))
C        The F-layer density altitude grid
C     DENE   Real       Vector        log(cm-3)       n/a
C        The logarithmic E-layer density
C     DENF   Real       Vector        log(cm-3)       n/a
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
C     NALT   Integer    Vector        n/a             >= 1
C                       (2)
C        The number of altitudes
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALT    Real       Matrix        km              >= 0.
C                       (MALTE,2)
C        The altitude grids for the molecular ion densities
C     DEN    Real       Matrix        cm-3            >= 0.
C                       (MALTE,2)
C        The molecular ion densities
C     DMAX   Real       Scalar        log(cm-3)       n/a
C        The logarithmic peak density of a profile
C     IALT   Integer    Scalar        n/a             >= 1
C        The altitude index
C     NALTS  Integer    Vector        n/a             >= 1
C                       (2)
C        The number of altitudes for the molecular ion densities
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     FNDMAX Determines the peak density and height of a density profile
C     GEN_E  Calculates molecular ion densities from the parameterized
C            low/mid-latitude E-region model
C     MID_F  Calculates O+ densities from the parameterized mid-latitude
C            F-region model
C     NEWGRID
C            Interpolates a density profile onto a new altitude grid
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
C     L. Brown       22-Sep-1994  1.0.3 ==> 1.0.9
C                                 In the calculation of the E-layer density
C                                 profile, an altitude loop that converts
C                                 density to log density and a call to NEWGRID
C                                 have been eliminated, assuming that the
C                                 altitude grid is the same throughout the LME
C                                 database.
C     L. Brown       27-Apr-1995  1.0.9 ==> 1.2
C                                 Removed arguments F10P7 and KP from the call
C                                 to routine GEN_E since they are no longer
C                                 needed by that routine.
C                                 Removed arguments F10P7 and KP from the call
C                                 to routine MID_F since they are no longer
C                                 needed by that routine.
C                                 Removed input parameters F10P7 and KP since
C                                 they are no longer used.
C     L. Brown       30-Sep-1996  1.2 ==> 1.5
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
C     MALT   Integer    n/a             The maximum allowed number of altitudes
C                                       in the output arrays
C     MALTE  Integer    n/a             The maximum allowed number of altitudes
C                                       in local arrays
C
      INTEGER MALT,MALTE
      REAL F2TOD
      PARAMETER(MALT=49,MALTE=28,F2TOD=1.24E4)
C
C  Input variables
C
      REAL MLAT,MLON
C
C  Output variables
C
      INTEGER NALT(2)
      REAL FOF2,HMF2,FOE,HME
      REAL ALTE(MALT),DENE(MALT),ALTF(MALT),DENF(MALT)
C
C  Local variables
C
      INTEGER IALT
      INTEGER NALTS(2)
      REAL DMAX
      REAL DEN(MALTE,2),ALT(MALTE,2)
C
C  Calculate the E-layer density
C
      CALL GEN_E(MLAT,MLON,NALTS,ALT,DEN)
      NALT(1)=NALTS(1)
      DO 10 IALT=1,NALT(1)
         ALTE(IALT)=ALT(IALT,1)
         DENE(IALT)=LOG(DEN(IALT,1)+DEN(IALT,2))
   10 CONTINUE
C
C  Calculate the E-layer critical frequency and height
C  Note:The E-layer critical frequency does not include the contribution from
C       the F-layer density.
C
      CALL FNDMAX(NALT(1),ALTE,DENE,DMAX,HME)
      CALL NEWGRID(NALT(1),ALTE,DENE,1,HME,DMAX)
      FOE=SQRT(EXP(DMAX)/F2TOD)
C
C  Calculate the F-layer density
C
      CALL MID_F(MLAT,MLON,NALT(2),ALTF,DENF)
C
C  Calculate the F-layer critical frequency and height
C  Note:The F-layer critical frequency does not include the contribution from
C       the E-layer density.
C
      CALL FNDMAX(NALT(2),ALTF,DENF,DMAX,HMF2)
      CALL NEWGRID(NALT(2),ALTF,DENF,1,HMF2,DMAX)
      FOF2=SQRT(EXP(DMAX)/F2TOD)
C
      RETURN
      END
C
      SUBROUTINE MID_F(MLAT,MLON,NALT1,ALT1,DEN)
C
C  PURPOSE
C     To reconstruct the midlatitude ion profiles from EOFs and their
C   coefficients.
C  METHOD
C     The orthogonal polynomial coefficients describe the behavior of EOF
C   coefficients in magnetic latitude for a given universal time and magnetic
C   longitude.  These are passed through common block MIDLAT from the read
C   subroutine. EOF coefficients are determined from the orthogonal polynomial
C   coefficients.  The EOF coefficients, in combination with the EOFs, describe
C   the ion density altitude profile for a given magnetic latitude, magnetic
C   longitude, and universal time.
C
C  INPUT PARAMETERS
C     NAME       TYPE       ARRAY       Description
C     MLAT       REAL                   The magnetic latitude, in degrees north
C     MLON       REAL                   The magnetic longitude, in degrees east
C
C  OUTPUT PARAMETERS
C     NAME       TYPE       ARRAY       Description
C     ALT1       REAL       (MALT)      The altitude grid, in km
C     DEN        REAL       (MALT)      The ion density, in cm-3
C     NALT1                             The number of altitude points
C
C     LOCAL VARIABLES
C     NAME       TYPE       ARRAY       Description
C     ADMLAT     REAL                   ABS(DMLAT(I,1))
C     AMLAT      REAL                   ABS(MLAT)
C     ASMLAT     REAL                   ABS(SMLAT(I,1))
C     DF1        REAL        (4)        For FoF2 and HmF2 calculations
C     DF2        REAL        (4)        For FoF2 and HmF2 calculations
C     EC         REAL      (MEOF)       Empirical orthogonal function co-
C                                       efficients
C     I          INTEGER                Do loop index
C     IMLAT      INTEGER                Magnetic latitude index number
C     IMLAT1     INTEGER                Lower magnetic latitude index number
C                                       if two databases must be read
C     IMLAT2     INTEGER                Higher magnetic latitude index number
C                                       if two databases must be read
C     K          INTEGER                Do loop index
C     T          REAL                   Interpolation index
C     TEST       LOGICAL                Read test
C     TMPALT     REAL      (MALT,MPOSS) Holds the different altitudes for
C                                       densities to be used for a final
C                                       mixing into a final EDP
C     TMPALT     REAL      (MALT,MPOSS) Holds the different densities to be
C                                       used for a final mixing into a final
C                                       EDP
C
C  SUBROUTINES REQUIRED
C     GENDEN   Generates an ion density altitude profile from EOFs and EOF
C              coefficients
C     GENEC    Generates EOF coefficients from orthogonal polynomial
C              coefficients
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
C                                   Calculation of magnetic latitude indices
C                                   and interpolation factor has been moved
C                                   outside loops to eliminate redundant
C                                   calculation, assuming that the magnetic
C                                   latitude grid is the same throughout the
C                                   MLF database.
C                                   Calculation of NREC has been optimized.
C     L. Brown         7-Oct-1994   1.0.9 ==> 1.1
C                                   Common block MIDLT1 (in INCLUDE file
C                                   'midlat.inc') has been modified to reflect
C                                   the elimination of Fourier fitting in
C                                   magnetic longitude in the mid-latitude
C                                   F-region parameterized model (MLF).
C                                   The calls to GENFC and GENEC have been
C                                   combined into a single call to GENEC to
C                                   reflect the elimination of Fourier fitting
C                                   in magnetic longitude in the mid-latitude
C                                   F-region parameterized model (LLF).
C     L. Brown         27-Apr-1995  1.1 ==> 1.2
C                                   The output profiles are now interpolated
C                                   from a 2 x 2 (F10.7 x Kp) profile matrix
C                                   instead of a 3 x 3 profile matrix.
C                                   Removed input parameters F10P7 and KP since
C                                   they are no longer used.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Routine GENDEN now returns the natural
C                                   logarithm of the ion density instead of the
C                                   actual value.
C                                   Removed PARAMETER EPSLOG since it is no
C                                   longer used.
C                                   The actual value of the peak density is now
C                                   interpolated in F10.7 and Kp instead of its
C                                   logarithm.
C     L. Brown        13-Jan-1998   1.3 ==> 1.7
C                                   Corrected test for magnetic latitude above
C                                   the magnetic latitude grid by changing
C                                   "(AMLAT .GT.
C                                     ASMLAT+FLOAT(NMLAT(I,1))*ADMLAT)"
C                                   to
C                                   "(AMLAT .GT.
C                                     ASMLAT+FLOAT(NMLAT(I,1)-1)*ADMLAT)".
C
C  REFERENCES
C     NONE
C
      REAL LOGMIN
      PARAMETER (LOGMIN=-21.)
      INCLUDE 'midlat.inc'
C
C     I/O variable declarations
C
      INTEGER NALT1
      REAL MLON,MLAT,NMAX,HMAX
      REAL DEN(MALT),ALT1(MALT),NMAXIN(MPOSS),HMAXIN(MPOSS)
      REAL TMPDEN(MALT,MPOSS),TMPALT(MALT,MPOSS),DF1(4),DF2(4)
C
C     Local variable declarations
C
      INTEGER IMLAT,IMLAT1,IMLAT2,I,J,K,L,NREC
      REAL EC(MEOF),T
      REAL AMLAT,ASMLAT,ADMLAT
      LOGICAL TEST
C
C     Beginning of executable code
C
      I = 1
      IF (MLAT .LT. 0.0) I = 2
       NALT1 = NALT(I,1)
       DO K = 1,NALT1
        ALT1(K) = ALT(K,I,1)
       ENDDO
C
C  Use the midlatitude formulation for the index
C
      AMLAT=ABS(MLAT)
      ASMLAT=ABS(SMLAT(I,1))
      ADMLAT=ABS(DMLAT(I,1))
      IMLAT=MIN(NMLAT(I,1),MAX(1,1+INT((AMLAT-ASMLAT)/ADMLAT)))
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then the
C  orthogonal polynomials do not need to be interpolated
C
      IF((AMLAT .EQ. ASMLAT+FLOAT(IMLAT-1)*ADMLAT) .OR.
     &   (AMLAT .LT. ASMLAT) .OR.
     &   (AMLAT .GT. ASMLAT+FLOAT(NMLAT(I,1)-1)*ADMLAT)) THEN
         TEST=.TRUE.
         IMLAT1=IMLAT
         IMLAT2=IMLAT
         T=0.
C
C  Otherwise, the orthogonal polynomials will be interpolated to the magnetic
C  latitude
C
      ELSE
         TEST=.FALSE.
         IF(IMLAT .GE. NMLAT(I,1)) THEN
            IMLAT1=NMLAT(I,1)-1
            IMLAT2=NMLAT(I,1)
         ELSE
            IMLAT1=IMLAT
            IMLAT2=IMLAT+1
         ENDIF
         T=(AMLAT-(ASMLAT+FLOAT(IMLAT1-1)*ADMLAT))/ADMLAT
      ENDIF
C
      NREC=0
      DO J = 1,NKPM
       DO K = 1,NF10P7M
        NREC=NREC+1
C
        DO L = 1,NALT(I,NREC)
         TMPALT(L,NREC) = ALT(L,I,NREC)
        ENDDO
C
C  Generate EOF coefficients from the orthogonal polynomial coefficients
C
        CALL GENEC(MEOF,MOPM1,MX,MMLON,DELTA(I,NREC),NOPM1(I,NREC),
     &             NEOF(I,NREC),NMLAT(I,NREC),IMLAT,IMLAT1,IMLAT2,T,
     &             TEST,OPC(1,1,1,I,NREC),MOD(360.+MOD(MLON,360.),360.),
     &             NMLON(I,NREC),SMLON(I,NREC),DMLON(I,NREC),EC)
C
C       Generate an ion density altitude profile from the EOFs and EOF
C       coefficients
C
        CALL GENDEN(MEOF,MALT,NEOF(I,NREC),EOF(1,1,I,NREC),EC,
     1  NALT(I,NREC),TMPDEN(1,NREC))
C
        DO L=1,NALT(I,NREC)
         TMPDEN(L,NREC) = MAX(TMPDEN(L,NREC),LOGMIN)
        ENDDO
C
C       Find the maximum for this profile and keep it
C
        CALL FNDMAX(NALT(I,NREC),TMPALT(1,NREC),TMPDEN(1,NREC),
     1  DF1(K),DF1(K+2))
        DF1(K)=EXP(DF1(K))
       ENDDO
C
C      Compute the fits over all F10P7 densities
C
       CALL LINR(DF2(J),DF1,TF10P7M,OMTF10P7M)
       CALL LINR(DF2(J+2),DF1(3),TF10P7M,OMTF10P7M)
C
      ENDDO
C
C     Compute the fits over all F10P7 densities over all KP values
C
      CALL LINR(NMAX,DF2,TKPM,OMTKPM)
      CALL LINR(HMAX,DF2(3),TKPM,OMTKPM)
      NMAX=LOG(NMAX)
      NREC=0
      DO I = 1,NKPM
       DO J = 1,NF10P7M
        NREC=NREC+1
        NMAXIN(NREC) = NMAX
        HMAXIN(NREC) = HMAX
       ENDDO
      ENDDO
C
C     Merge the various profiles for the final profile
C
      CALL MERGE4(MALT,NF10P7M,NKPM,NALT,TF10P7M,OMTF10P7M,TKPM,OMTKPM,
     &            TMPALT,TMPDEN,NMAXIN,HMAXIN,DEN)
C
C
      RETURN
      END
C
C
