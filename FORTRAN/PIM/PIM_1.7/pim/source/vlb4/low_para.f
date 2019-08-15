      SUBROUTINE LOW_PARAM(MLAT,MLON,MLT,FOF2,HMF2,FOE,HME,NALT,ALTE,
     &                     EDEN,ALTF,FDEN)
C
C  PURPOSE
C     To generate E-layer and F-layer densities and critical parameters from
C     the parameterized low-latitude models.
C
C  METHOD
C     The parameterized low-latitude models are used to generate E-layer and
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
C     MLT    Real       Scalar        hr              0. <= MLT < 24.
C        Magnetic local time
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
C     EDEN   Real       Vector        log(cm-3)       n/a
C                       (NALT(1))
C        The logarithmic E-layer density
C     FDEN   Real       Vector        log(cm-3)       n/a
C                       (NALT(2))
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
C                       (MALT,2)
C        The altitude grids for the molecular ion densities
C     DEN    Real       Matrix        cm-3            >= 0.
C                       (MALT,2)
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
C     LOW_F  Calculates O+ densities from the parameterized low-latitude
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
C     W. Whartenby                Created
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
C                                 Removed argument F10P7 from the call to
C                                 routine LOW_F since it is no longer needed by
C                                 that routine.
C                                 Removed input parameters F10P7 and KP since
C                                 they are no longer used.
C     L. Brown       25-Sep-1995  1.2 ==> 1.3
C                                 Removed arguments DAY, LON, and UT from the
C                                 call to routine LOW_F since they are no
C                                 longer needed by that routine.
C                                 Removed input parameters DAY, UT, and LON
C                                 since they are no longer used.
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
C     MALT   Integer    n/a             The maximum allowed number of altitudes
C                                       in the output arrays
C     MAXALT Integer    n/a             The maximum allowed number of altitudes
C                                       in local arrays
C
      INTEGER MALT,MAXALT
      REAL F2TOD
      PARAMETER(MALT=28,MAXALT=60,F2TOD=1.24E4)
C
C  Input variables
C
      REAL MLAT,MLON,MLT
C
C  Output variables
C
      INTEGER NALT(2)
      REAL FOF2,HMF2,FOE,HME
      REAL ALTE(MAXALT),EDEN(MAXALT),ALTF(MAXALT),FDEN(MAXALT)
C
C  Local variables
C
      INTEGER IALT
      INTEGER NALTS(2)
      REAL DMAX
      REAL DEN(MALT,2),ALT(MALT,2)
C
C  Calculate the E-layer density
C
      CALL GEN_E(MLAT,MLON,NALTS,ALT,DEN)
      NALT(1)=NALTS(1)
      DO 10 IALT=1,NALT(1)
         ALTE(IALT)=ALT(IALT,1)
         EDEN(IALT)=LOG(DEN(IALT,1)+DEN(IALT,2))
   10 CONTINUE
C
C  Calculate the E-layer critical frequency and height
C  Note:The E-layer critical frequency does not include the contribution from
C       the F-layer density.
C
      CALL FNDMAX(NALT(1),ALTE,EDEN,DMAX,HME)
      CALL NEWGRID(NALT(1),ALTE,EDEN,1,HME,DMAX)
      FOE=SQRT(EXP(DMAX)/F2TOD)
C
C  Calculate the F-layer density
C
      CALL LOW_F(MLAT,MLON,MLT,NALT(2),ALTF,FDEN)
C
C  Calculate the F-layer critical frequency and height
C  Note:The F-layer critical frequency does not include the contribution from
C       the E-layer density.
C
      CALL FNDMAX(NALT(2),ALTF,FDEN,DMAX,HMF2)
      CALL NEWGRID(NALT(2),ALTF,FDEN,1,HMF2,DMAX)
      FOF2=SQRT(EXP(DMAX)/F2TOD)
C
      RETURN
      END
C
      SUBROUTINE LOW_F(MLAT,MLON,MLT,N1ALT,ALT1,
     1 DEN1)
C
C  PURPOSE
C    To compute the unadjusted ionic logarithmic density from the low
C  latitude paramaterization and get the logarithm of the maximum density
C  and the height of the maximum density
C
C  METHOD
C     For each of the four magnetic longitudes and the magnetic local time, the
C  orthogonal polynomial coefficients describe the behavior of EOF coefficients
C  in magnetic latitude.  EOF coefficients are determined from the orthogonal
C  polynomial coefficients.  The EOF coefficients, in combination with the
C  EOFs, describe the O+ density profile for a given magnetic latitude and
C  magnetic local time.
C     Compute the density grid for the four magnetic longitudes at the
C  magnetic local time. Take the log of the density. Compute the maximum
C  density and the height of the maximum.Then use function FINTRP to
C  interpolate the four FoF2s and HmF2s. Finally, use function GINTRP to fit
C  the profile altitude point by point
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    MLAT    REAL              Magnetic latitude of point to compute
C    MLON    REAL              Magnetic longitude of point to compute
C    MLT     REAL              Magnetic local time of point to compute
C    N1ALT   INTEGER           The number of altitude points for ALT1
C    ALT1    REAL       (MALT) The altitude grid to use
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    DEN1    REAL       (MALT) The output density grid on ALT1
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY       description
C     ALT    REAL (MALT,MSECT)      The altitude grid read from the
C                                   LOWLAT database, in km
C     DMLAT  REAL                   The increment of the magnetic latitude
C                                   grid, in degrees north
C     EOF    REAL (MALT,MEOF,MSECT) Empirical orthogonal functions (EOFs)
C     I      INTEGER                Do loop index
C     I1     INTEGER                The lower sector loop boundary
C     I2     INTEGER                The upper sector loop boundary
C     J      INTEGER                Do loop index
C     NEOF   INTEGER                The number of EOFs used to construct an O+ density profile
C     NMLAT  INTEGER                The number of magnetic latitude grid points
C     NOPM1  INTEGER                The number of orthogonal polynomials - 1
C     SMLAT  REAL                   The starting value of the magnetic
C                                   latitude grid, in degrees north
C     EC     REAL     (MEOF)        EOF coefficients
C     OPC    REAL (MOPM1+1,MEOF,    Orthogonal polynomial coefficients
C                  MMLT,MSECT,MPOSS)
C     SECTOR CHARACTER              The magnetic longitude sector:
C                                   'BRZ' for Brazilian, 'IND' for Indian,
C                                   'PAC' for Pacific, and 'USA' for USA
C
C  SUBROUTINES CALLED
C     NAME     description
C     FNDMAX   Findx the maximum density and altitude of maximum density of
C              input grid ALT, DEN
C     SCALEP   Scales a density grid to a new maximum density and altitude
C              of maximum density
C     GENDEN   Generates an O+ density profile from EOFs and EOF coefficients
C     GENEC    Generates EOF coefficients from orthogonal polynomial
C              coefficients
C
C  FUNCTIONS CALLED
C     NAME     Description
C     FINTRP   Fourier interpolation for critical frequencies and heights
C     GINTRP   See GINTRP internal description
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
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby     5-Nov-1993   Changed the routine so that only the USA
C                                   sector is used for computation.
C     L. Brown        22-Sep-1994   1.0.3 ==> 1.0.9
C                                   In the IF statement that checks for a
C                                   magnetic latitude exactly on the magnetic
C                                   latitude grid, a bug has been fixed by
C                                   changing "ABS(MLAT)" to "MLAT".
C                                   Call to GET_TMS has been moved outside loop
C                                   to eliminate redundant calculation,
C                                   assuming that the sector magnetic
C                                   longitudes are the same for all solar
C                                   activity levels in the LLF database.
C                                   Calculation of magnetic latitude indices
C                                   and interpolation factor has been moved
C                                   outside loops to eliminate redundant
C                                   calculation, assuming that the magnetic
C                                   latitude grid is the same throughout the
C                                   LLF database.
C                      3-Jan-1995   1.0.9 ==> 1.1
C                                   Common block LOWER (in INCLUDE file
C                                   'lower.inc') has been modified to reflect
C                                   the elimination of Fourier fitting in UT
C                                   in the low-latitude F-region parameterized
C                                   model (LLF).
C                                   The calls to GENFC and GENEC have been
C                                   combined into a single call to GENEC to
C                                   reflect the elimination of Fourier fitting
C                                   in UT in the low-latitude F-region
C                                   parameterized model (LLF).
C                                   As part of the input stream, the user
C                                   specifies which longitude sector to use.
C                                   The choice is stored in integer flag LLFSCT
C                                   in common block LOWER.  Its meaning is as
C                                   follows:
C                                      0=Use all four sectors
C                                      1=Use Brazilian sector only
C                                      2=Use Indian sector only
C                                      3=Use Pacific sector only
C                                      4=Use USA sector only
C                                   If the magnetic latitude is outside the LLF
C                                   range ([-32,32] degrees), then the
C                                   coefficients at the appropriate edge of the
C                                   LLF latitude grid are used without
C                                   interpolation.
C     L. Brown         27-Apr-1995  1.1 ==> 1.2
C                                   The output profiles are now interpolated
C                                   from a 2 x 4 (F10.7 x Longitude Sector)
C                                   profile matrix instead of a 3 x 4 profile
C                                   matrix.
C                                   Removed input parameter F10P7 since it is
C                                   no longer used.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   Added INCLUDE statement for INCLUDE file
C                                   'indirect.inc'.
C                                   Modified the IF statement that tests for
C                                   the high solar activity Indian sector case
C                                   to work with the 2 x 4 (F10.7 x Longitude
C                                   Sector) profile matrix.
C                                   Routine GENDEN now returns the natural
C                                   logarithm of the O+ density instead of the
C                                   actual value.
C                                   The actual values of the peak density and
C                                   density profile are now interpolated in
C                                   F10.7 instead of their logarithms.
C                                   The LLF model has been reparameterized in
C                                   magnetic local time instead of universal
C                                   time.
C                                   Removed input parameters DAY, LON, and UT
C                                   since they are no longer used.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INCLUDE 'array.inc'
      INCLUDE 'lower.inc'
      INCLUDE 'indirect.inc'
C
      INTEGER N1ALT,IMLAT,IMLAT1,IMLAT2,I,J,K,L,I1,I2
C
      REAL MLAT,MLON,MLT,T
      REAL DEN(MALT,MSECT),FF2(MSECT),H2(MSECT)
      REAL ALT1(MALT),DEN1(MALT),DF1(4),DF2(2)
      REAL EC(MEOF),DUM(MAX_ALT_NUM)
      REAL TMPALT(MALT,2),TMPDEN(MALT,2)
C
      LOGICAL TEST
C
C  Determine the location of the magnetic latitude on the magnetic latitude
C  grid
C
      IMLAT=MIN(NMLAT(1,1),MAX(1,1+INT((MLAT-SMLAT(1,1))/DMLAT(1,1))))
C
C  If the magnetic latitude is exactly on the magnetic latitude grid, then the
C  EOF coefficients can be calculated directly from the orthogonal
C  polynomials and orthogonal polynomial coefficients without interpolation
C
      IF(MLAT .EQ. SMLAT(1,1)+DMLAT(1,1)*FLOAT(IMLAT-1)) THEN
         TEST=.TRUE.
C
C  If the magnetic latitude is outside the magnetic latitude range on which
C  the parameterized LLF model is defined, then the EOF coefficients are
C  calculated directly from the orthogonal polynomials and orthogonal
C  polynomial coefficients at the appropriate edge of the parameterized LLF
C  model magnetic latitude range without interpolation
C
      ELSE IF(ABS(MLAT) .GE. ABS(SMLAT(1,1))) THEN
         TEST=.TRUE.
C
C  Otherwise, the EOF coefficients must be interpolated from the EOF
C  coefficients at the magnetic latitude grid points
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
C  Set the loop boundaries for the sector loop according to the value of the
C  LLFSCT integer flag in common block LOWER
C
      IF(LLFSCT .EQ. 0) THEN
         I1=1
         I2=MSECT
      ELSE
         I1=LLFSCT
         I2=LLFSCT
      ENDIF
C
C     Do all the F10p7 cases we have now
C
      DO J = 1, NF10P7L
C
C      Do over all the various sector data
C
       DO I = I1,I2
C
C  Generate EOF coefficients from the orthogonal polynomial coefficients
C
        CALL GENEC(MEOF,MOPM1,MX,MMLT,DELTA(I,J),NOPM1(I,J),NEOF(I,J),
     &             NMLAT(I,J),IMLAT,IMLAT1,IMLAT2,T,TEST,OPC(1,1,1,I,J),
     &             MOD(24.+MOD(MLT,24.),24.),NMLT(I,J),SMLT(I,J),
     &             DMLT(I,J),EC)
C
C       Generate an O+ density profile from the EOFs and EOF coefficients
C
        CALL GENDEN(MEOF,MALT,NEOF(I,J),EOF(1,1,I,J),EC,
     1   NALT(I,J),DEN(1,I))
C
        DO K =1,NALT(I,J)
         TMPALT(K,J) = ALT(K,I,J)
        ENDDO
C
C       Find the maximum of the logarithmic density profile
C
        CALL FNDMAX(NALT(I,J),ALT(1,I,J),DEN(1,I),FF2(I),H2(I))
       ENDDO
C
C      If all four longitude sectors are to be used (LLFSCT=0), then fit the
C      four low latitude profiles over magnetic longitude
C
       IF(LLFSCT .EQ. 0) THEN
          CALL LOW_MERGE(MALT,MSECT,NALT(1,J),FF2,H2,MLON,MLN(1,J),
     1    ALT(1,1,J),DEN,N1ALT,TMPALT(1,J),TMPDEN(1,J))
C
C      If a single longitude sector is to be used (LLFSCT=1-4), then store
C      profile
C
       ELSE
          N1ALT = NALT(LLFSCT,J)
          DO K = 1,N1ALT
           TMPALT(K,J) = ALT(K,LLFSCT,J)
           TMPDEN(K,J) = DEN(K,LLFSCT)
          ENDDO
       ENDIF
C
       CALL FNDMAX(N1ALT,TMPALT(1,J),TMPDEN(1,J),DF1(J),
     1 DF1(J+2))
       DF1(J)=EXP(DF1(J))
      ENDDO
C
C     Compute the fits over all F10P7 densities
C
      CALL LINR(DF2(1),DF1,TF10P7L,OMTF10P7L)
      DF2(1)=LOG(DF2(1))
      CALL LINR(DF2(2),DF1(3),TF10P7L,OMTF10P7L)
C
      DO L = 1,NF10P7L
       DO K = 1,MALT
        DUM(K) = TMPDEN(K,L)
       ENDDO
       CALL SCALEP(N1ALT,TMPALT(1,L),DUM,DF2(1),DF2(2),TMPDEN(1,L))
      ENDDO
      DO L = 1,MALT
       DO K = 1,NF10P7L
        DUM(K) = EXP(TMPDEN(L,K))
       ENDDO
       CALL LINR(DEN1(L),DUM,TF10P7L,OMTF10P7L)
       DEN1(L)=LOG(MAX(1.E-35,DEN1(L)))
      ENDDO
      N1ALT = NALT(1,1)
      DO K = 1,MALT
       TMPDEN(K,1) = DEN1(K)
       ALT1(K) = ALT(K,1,1)
      ENDDO
      CALL SCALEP(NALT(1,1),ALT1,TMPDEN,DF2(1),DF2(2),DEN1)
      RETURN
      END
C
      SUBROUTINE LOW_MERGE(MALT,MSECT,NALT,FF2,H2,MLON,MLN,ALT,
     1  DEN,N1ALT,ALT1,DEN1)
C
C     PURPOSE
C       To merge the four profiles for different magnetic longitudes
C     into a profile for a particular magnetic longitude
C
C     METHOD
C       Compute NMAX and HMAX by using FINTRP over longitude. Next,
C     fit 3 profiles by taking the different F10P7 profiles for eack KP
C     and then fitting each point over F10P7. Next fit the three
C     distilled profiles over KP. Finally, take the resulting profile
C     and fit it to the computed NMAX and HMAX.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ALT     REAL (MALT,MSECT) The altitude grid read from the
C                              LOWLAT database, in km
C    MLON    REAL              Magnetic longitude of point to compute
C    MLT     REAL              Magnetic local time of point to compute
C    MALT    INTEGER           The maximum number of altitude points for ALT
C    MSECT   INTEGER           The maximum number of sectors
C    NALT    INTEGER   (MSECT) The number of altitude points for ALT
C    ALT1    REAL       (MALT) The altitude grid to use
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       25-Sep-1995  1.3
C                                 Commented out diagnostic PRINT statements.
C
C     Use Fourier interpolation to determine the FoF2 for this run
C
      EXTERNAL FINTRP,GINTRP
      REAL FINTRP,GINTRP
      INCLUDE 'array.inc'
C
C
      INTEGER I,J,MSECT,MALT,NALT(MSECT),N1ALT
C
      REAL NMAX,HMF2,FF2(MSECT),H2(MSECT),MLON
      REAL ALT(MALT,MSECT),DEN(MALT,MSECT),MLN(MSECT)
      REAL TEMPDEN(MAX_ALT_NUM),ALT1(MALT),DEN1(MALT)
      REAL XTEMP,NMAX1,HMF21
C
      NMAX = FINTRP(FF2,MLON)
      HMF2 = FINTRP(H2,MLON)
C
C      Scale each of the densities to the computed FoF2 and HmF2
C
      DO I = 1,MSECT
       CALL SCALEP(NALT(I),ALT(1,I),DEN(1,I),NMAX,HMF2,TEMPDEN)
       DO J = 1,NALT(I)
        DEN(J,I) = TEMPDEN(J)
       ENDDO
C
      ENDDO
C
C     set as the altitude grid of the first computed array.
C
      N1ALT = NALT(1)
      DO J = 1, N1ALT
       ALT1(J) = ALT(J,1)
      ENDDO
C
C     Put the log density arrays onto the output grid...
C
      DO I = 1,MSECT
       CALL NEWGRID(NALT(I),ALT(1,I),DEN(1,I),N1ALT,ALT1,TEMPDEN)
       DO J = 1,N1ALT
        DEN(J,I) = TEMPDEN(J)
       ENDDO
      ENDDO
C
C     Put the four different densities onto the current output grid
C     by interpolation of the actual densities. Note that the exponent
C     of the densities must be taken and the the output logged. If this
C     method gives a result that is more than 50% lower than the lowest
C     current value, then fit on the logs.
C
      DO J = 1,N1ALT
       DO I = 1,MSECT
        FF2(I) = EXP(DEN(J,I))
       ENDDO
C      Compute the fitting here
C
       XTEMP = GINTRP(FF2,MLN,MLON)
       TEMPDEN(J) = ALOG(XTEMP)
      ENDDO
C
C     Call SCALEP to insure the fitted profile has the same FoF2
C     and HmF2 as the four input profiles... This is not necessarily
C     true if the profiles are pathologically different.
C
      CALL SCALEP(N1ALT,ALT1,TEMPDEN,NMAX,HMF2,DEN1)
C
C     Check the output profile for consistency
C
      CALL FNDMAX(N1ALT,ALT1,DEN1,NMAX1,HMF21)
      IF (ABS(NMAX1-NMAX) .GT. 0.05*MIN(NMAX1,NMAX)) THEN
C       PRINT *,' Error for the low latitude grid NMAX ',NMAX1,NMAX
      ENDIF
      IF (ABS(HMF21-HMF2) .GT. 0.05*MIN(HMF21,HMF2)) THEN
C       PRINT *,' Error for the low latitude grid HMF2 ',HMF21,HMF2
      ENDIF
C
      RETURN
      END
