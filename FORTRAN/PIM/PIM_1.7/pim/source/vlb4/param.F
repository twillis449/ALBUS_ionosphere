      SUBROUTINE PARAM(YEAR,DAY,UT,LAT,LON,MLAT,MLON,MLT,RB,FOF2,HMF2,
     &                 FOE,HME,NALTO,ALTO,EOUT,FOUT)
C
C  PURPOSE
C     To calculate E-layer and F-layer densities and critical parameters from
C     regional parameterized models.
C
C  METHOD
C     Regional parameterized models appropriate to the given magnetic latitude
C     generate E-layer and F-layer densities and critical parameters.  In
C     transition regions between the regional models, density profiles are
C     merged.  Densities are interpolated onto the output altitude grid.
C     The table below describes the latitude regions and the actions taken in
C     each region:
C        Pure high-latitude region: |MLAT| >= RB
C           The parameterized high-latitude model is used to generate density
C           profiles and critical parameters.
C           The densities are interpolated onto the output altitude grid.
C           If foF2 normalization to URSI is requested, then the F-layer
C           density is scaled to the URSI foF2.
C           Note that the parameterized model foE is used regardless of the
C           foE normalization requested.
C        Mid/high-latitude transition region: RB-8. < |MLAT| < RB
C           The parameterized mid-latitude and high-latitude models are used to
C           generate density profiles and critical parameters.
C           The densities are interpolated onto the output altitude grid.
C           The mid-latitude and high-latitude densities are merged in a linear
C           fashion.
C           If foE normalization to the CCIR foE model is requested, then the
C           E-layer density is scaled to the CCIR foE.
C           If foF2 normalization to URSI is requested, then the F-layer
C           density is scaled to the URSI foF2.
C        Pure mid-latitude region: 44. <= |MLAT| <= RB-8.
C           The parameterized mid-latitude model is used to generate density
C           profiles and critical parameters.
C           The densities are interpolated onto the output altitude grid.
C           If foE normalization to the CCIR foE model is requested, then the
C           E-layer density is scaled to the CCIR foE.
C           If foF2 normalization to URSI is requested, then the F-layer
C           density is scaled to the URSI foF2.
C        Low/mid-latitude transition region: 34. < |MLAT| < 44.
C           The parameterized low-latitude and mid-latitude models are used to
C           generate density profiles and critical parameters.
C           The densities are interpolated onto the output altitude grid.
C           The low-latitude and mid-latitude densities are merged in a linear
C           fashion.
C           If foE normalization to the CCIR foE model is requested, then the
C           E-layer density is scaled to the CCIR foE.
C           If foF2 normalization to URSI is requested, then the F-layer
C           density is scaled to the URSI foF2.
C        Pure low-latitude region: |MLAT| <= 34.
C           The parameterized low-latitude model is used to generate density
C           profiles and critical parameters.
C           The densities are interpolated onto the output altitude grid.
C           If foE normalization to the CCIR foE model is requested, then the
C           E-layer density is scaled to the CCIR foE.
C           If foF2 normalization to URSI is requested, then the F-layer
C           density is scaled to the URSI foF2.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ALTO   Real       Vector        km              >= 0.
C                       (NALTO)
C        The output altitude grid
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        The day of the year
C     LAT    Real       Scalar        deg N           -90. <= LAT <= 90.
C        Geographic latitude
C     LON    Real       Scalar        deg E           0. <= LON < 360.
C        Geographic longitude
C     MLAT   Real       Scalar        deg N           -90. <= MLAT <= 90.
C        Magnetic latitude
C     MLON   Real       Scalar        deg E           0. <= MLON < 360.
C        Magnetic longitude
C     MLT    Real       Scalar        hr              0. <= MLT < 24.
C        Magnetic local time
C     NALTO  Integer    Scalar        n/a             >= 1
C        The number of output altitudes
C     RB     Real       Scalar        deg E           0. <= RB <= 90.
C        The absolute magnetic latitude of the equatorward boundary of the
C        trough
C     UT     Real       Scalar        hr              0. <= UT < 24.
C        Universal Time
C     YEAR   Integer    Scalar        Years           n/a
C        The year
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EOUT   Real       Vector        log(cm-3)       n/a
C                       (NALTO)
C        The logarithmic E-layer density
C     FOE    Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency (foE)
C     FOF2   Real       Scalar        MHz             >= 0.
C        The F2-layer critical frequency (foF2)
C     FOUT   Real       Vector        log(cm-3)       n/a
C                       (NALTO)
C        The logarithmic F-layer density
C     HME    Real       Scalar        km              >= 0.
C        The E-layer critical height (hmE)
C     HMF2   Real       Scalar        km              >= 0.
C        The F2-layer critical height (hmF2)
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ABSMLA Real       Scalar        deg             0. <= MLAT <= 90.
C        The absolute value of the magnetic latitude
C     ALTH   Real       Matrix        km              >= 0.
C                       (MALT,2)
C        Altitude grids from the parameterized high-latitude model
C     ALTL   Real       Matrix        km              >= 0.
C                       (MALT,2)
C        Altitude grids from the parameterized low-latitude model
C     ALTM   Real       Matrix        km              >= 0.
C                       (MALT,2)
C        Altitude grids from the parameterized mid-latitude model
C     DENH   Real       Matrix        log(cm-3)       n/a
C                       (MALT,2)
C        Logarithmic densities from the parameterized high-latitude model
C     DENL   Real       Matrix        log(cm-3)       n/a
C                       (MALT,2)
C        Logarithmic densities from the parameterized low-latitude model
C     DENM   Real       Matrix        log(cm-3)       n/a
C                       (MALT,2)
C        Logarithmic densities from the parameterized mid-latitude model
C     DMAX   Real       Scalar        log(cm-3)       n/a
C        The logarithmic peak density of a profile
C     FOEH   Real       Scalar        MHz             >= 0.
C        foE from the parameterized high-latitude model
C     FOEL   Real       Scalar        MHz             >= 0.
C        foE from the parameterized low-latitude model
C     FOEM   Real       Scalar        MHz             >= 0.
C        foE from the parameterized mid-latitude model
C     FOF2H  Real       Scalar        MHz             >= 0.
C        foF2 from the parameterized high-latitude model
C     FOF2L  Real       Scalar        MHz             >= 0.
C        foF2 from the parameterized low-latitude model
C     FOF2M  Real       Scalar        MHz             >= 0.
C        foF2 from the parameterized mid-latitude model
C     HMAX   Real       Scalar        km              >= 0.
C        The altitude of the peak density of a profile
C     HMEH   Real       Scalar        km              >= 0.
C        hmE from the parameterized high-latitude model
C     HMEL   Real       Scalar        km              >= 0.
C        hmE from the parameterized low-latitude model
C     HMEM   Real       Scalar        km              >= 0.
C        hmE from the parameterized mid-latitude model
C     HMF2H  Real       Scalar        km              >= 0.
C        hmF2 from the parameterized high-latitude model
C     HMF2L  Real       Scalar        km              >= 0.
C        hmF2 from the parameterized low-latitude model
C     HMF2M  Real       Scalar        km              >= 0.
C        hmF2 from the parameterized mid-latitude model
C     IALT   Integer    Scalar        n/a             >= 1
C        The altitude index
C     IDOM   Integer    Scalar        Days            1 <= IDOM <= 31
C        The day of the month
C     IMONTH Integer    Scalar        Months          1 <= IMONTH <= 12
C        The month of the year
C     INDX   Integer    Scalar        n/a             1 or 2
C        The hemisphere index
C     IUT    Integer    Scalar        hhmm            0000 <= IUT <= 2359
C        Universal Time
C     LDMIN  Real       Scalar        log(cm-3)       n/a
C        The lower limit on logarithmic density
C     NHALT  Integer    Vector        n/a             >= 1
C                       (2)
C        The number of altitude grid points from the parameterized
C        high-latitude model
C     NLALT  Integer    Vector        n/a             >= 1
C                       (2)
C        The number of altitude grid points from the parameterized low-latitude
C        model
C     NMALT  Integer    Vector        n/a             >= 1
C                       (2)
C        The number of altitude grid points from the parameterized mid-latitude
C        model
C     SCLFAC Real       Scalar        n/a             >= 0.
C        A density scale factor
C     USEHI  Logical    Scalar        n/a             .TRUE. or .FALSE.
C        A logical flag, .TRUE. if high-latitude densities are to be used,
C        .FALSE. otherwise
C     USELOW Logical    Scalar        n/a             .TRUE. or .FALSE.
C        A logical flag, .TRUE. if low-latitude densities are to be used,
C        .FALSE. otherwise
C     USEMID Logical    Scalar        n/a             .TRUE. or .FALSE.
C        A logical flag, .TRUE. if mid-latitude densities are to be used,
C        .FALSE. otherwise
C     XX1    Real       Scalar        n/a             0. <= XX1 <= 1.
C        An interpolation factor
C     XX2    Real       Scalar        n/a             0. <= XX2 <= 1.
C        An interpolation factor
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     FRAC   Calculates interpolation weights for transition regions
C     F2URSI Calculates URSI-88 foF2 and hmF2
C     GETSSN Converts between sunspot number and F10.7
C     GET_INDX
C            Determines the hemisphere index
C     LOW_PARAM
C            Calculates low-latitude E-layer and F-layer densities
C     MERGE  Merges two density profiles
C     MID_PARAM
C            Calculates mid-latitude E-layer and F-layer densities
C     NEWGRID
C            Interpolates a density profile onto a new altitude grid
C     TIMMDM Calculates the month and day of the month from the year and day of
C            the year
C     USUMODEL
C            Calculates high-latitude E-layer and F-layer densities
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     CCRFOE Real       Calculates foE from the CCIR recommendation
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
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     W. Whartenby   22-Mar-1991  Created
C     W. Whartenby   21-May-1993  1.0.6
C                                 Changed the MERGE calls in the exclusively
C                                 low latitude regions to only use the low
C                                 latitude data. The MERGE routines had been
C                                 passed the mid latitude data with 0
C                                 weighting, meaning no difference in output,
C                                 but in some special cases the low latitude
C                                 can be called before the mid latitude is
C                                 defined, leaving the passed arrays ill
C                                 defined, and causing a crash.
C     L. Brown       18-Nov-1994  1.0.6 ==> 1.1
C                                 Call to routine MONTH replaced by call to
C                                 routine TIMMDM.
C                                 Removed references to function FLPYR.
C                                 Removed local variable IERR.
C                                 Removed references to local variable TEST to
C                                 avoid call to routine MID_PARAM for latitudes
C                                 outside its normal operating range.
C     L. Brown       27-Apr-1995  1.1 ==> 1.2
C                                 Removed arguments F10P7 and KP from the calls
C                                 to routine LOW_PARAM since they are no longer
C                                 needed by that routine.
C                                 Removed arguments F10P7 and KP from the calls
C                                 to routine MID_PARAM since they are no longer
C                                 needed by that routine.
C     L. Brown       25-Sep-1995  1.2 ==> 1.3
C                                 Removed layer flag from calls to routine
C                                 MERGE since it is no longer used by that
C                                 routine.
C                                 Removed arguments DAY, UT, and LON from the
C                                 calls to routine LOW_PARAM since they are no
C                                 longer needed by that routine.
C     L. Brown       30-Sep-1996  1.3 ==> 1.5
C                                 Rewrite for optimization and readability.
C                                 Fixed a bug in the conversion of UT from
C                                 hours to HHMM for the call to F2URSI.
C                                 In the mid/high-latitude transition region,
C                                 weighted averages of mid-latitude and
C                                 high-latitude peak densities and heights are
C                                 now used in the merging of profiles instead
C                                 of the mid-latitude peak density and height.
C                                 In the low/mid-latitude transition region,
C                                 the peak density used to merge profiles is
C                                 calculated from a weighted average of
C                                 low-latitude and mid-latitude peak density
C                                 instead of a weighted average of low-latitude
C                                 and mid-latitude critical frequency, to be
C                                 consistent with the profile merging process.
C                                 Simplified logic since LAYR flag is no longer
C                                 used.
C                                 Removed input argument LAYR since it is no
C                                 longer used.
C                                 Removed last argument from calls to USUMODEL
C                                 since it is no longer used.
C                                 A single call to USUMODEL is now used instead
C                                 of two calls.
C                                 Removed last argument from calls to MID_PARAM
C                                 since it is no longer used.
C                                 A single call to MID_PARAM is now used
C                                 instead of two calls.
C                                 Removed last argument from calls to LOW_PARAM
C                                 since it is no longer used.
C                                 A single call to LOW_PARAM is now used
C                                 instead of two calls.
C                                 Replaced call to GET_FOE with call to CCRFOE.
C                                 The returned E-layer density is now a
C                                 logarithm for consistency with the returned
C                                 F-layer density.
C                                 Changed intrinsic function ALOG to its
C                                 generic equivalent LOG.
C     L. Brown       14-Feb-1997  1.5 ==> 1.6
C                                 Changed "semi-empirical foE" to "CCIR foE" in
C                                 comments.
C     L. Brown       13-Jan-1998  1.6 ==> 1.7
C                                 Changed lower absolute latitude boundary of
C                                 pure mid-latitude region from 34. degrees to
C                                 44. degrees.
C                                 Changed upper absolute latitude boundary of
C                                 low/mid-latitude transition region from 34.
C                                 degrees to 44. degrees.
C                                 Changed lower absolute latitude boundary of
C                                 low/mid-latitude transition region from 30.
C                                 degrees to 34. degrees.
C                                 Changed upper absolute latitude boundary of
C                                 pure low-latitude region from 30. degrees to
C                                 34. degrees.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     F2TOD  Real       cm-3 MHz-2      The conversion factor from frequency**2
C                                       (MHz**2) to density (cm-3)
C     MALT   Integer    n/a             The maximum allowed number of altitudes
C
      INTEGER MALT
      REAL F2TOD
      PARAMETER(MALT=60,F2TOD=1.24E4)
C
      INCLUDE 'indirect.inc'
      INCLUDE 'usuarr.inc'
      INCLUDE 'ssnstuff.inc'
      EXTERNAL MERGE
C
C  Input variables
C
      INTEGER YEAR,DAY,NALTO
      REAL UT,LAT,LON,MLAT,MLON,MLT,RB
      REAL ALTO(NALTO)
C
C  Output variables
C
      REAL FOF2,HMF2,FOE,HME
      REAL EOUT(NALTO),FOUT(NALTO)
C
C  Local variables
C
      INTEGER INDX,IALT,IMONTH,IDOM,IUT
      INTEGER NHALT(2),NMALT(2),NLALT(2)
      REAL ABSMLA,FOEH,HMEH,FOF2H,HMF2H,FOF2M,HMF2M,FOEM,HMEM,FOF2L,
     &     HMF2L,FOEL,HMEL,LDMIN,SCLFAC,XX1,XX2,DMAX,HMAX
      REAL DENH(MALT,2),ALTH(MALT,2),ALTM(MALT,2),DENM(MALT,2),
     &     ALTL(MALT,2),DENL(MALT,2)
      LOGICAL USEHI,USEMID,USELOW
C
C  Function declarations
C
      REAL CCRFOE
C
C  Calculate the absolute value of the magnetic latitude
C
      ABSMLA=ABS(MLAT)
C
C  If the magnetic latitude is in the high-latitude range, then calculate
C  high-latitude E-layer and F-layer densities
C
      IF(ABSMLA .GT. RB-8.) THEN
         USEHI=.TRUE.
         CALL GET_INDX(MLAT,INDX)
         CALL USUMODEL(MLAT,MLT,FOEH,HMEH,FOF2H,HMF2H,DENH,DENH(1,2))
         NHALT(1)=37
         NHALT(2)=37
         DO 10 IALT=1,37
            ALTH(IALT,1)=Z(IALT,3,INDX,1)
            ALTH(IALT,2)=Z(IALT,1,INDX,1)
   10    CONTINUE
      ELSE
         USEHI=.FALSE.
      ENDIF
C
C  If the magnetic latitude is in the mid-latitude range, then calculate
C  mid-latitude E-layer and F-layer densities
C
      IF((ABSMLA .LT. RB) .AND. (ABSMLA .GT. 34.)) THEN
         USEMID=.TRUE.
         CALL MID_PARAM(MLAT,MLON,FOF2M,HMF2M,FOEM,HMEM,NMALT,ALTM,DENM,
     &                  ALTM(1,2),DENM(1,2))
      ELSE
         USEMID=.FALSE.
      ENDIF
C
C  If the magnetic latitude is in the low-latitude range, then calculate
C  low-latitude E-layer and F-layer densities
C  Note:A low density at 90 km altitude is added to the F-layer density.  The
C       parameterized low-latitude F-layer model gives F-layer densities down
C       to only 160 km altitude.  Because the F-layer can be broad in this
C       latitude region, an extrapolation to 90 km altitude will be in error
C       if it does not account for the typical rapid falloff in the F-layer
C       density at E-layer altitudes.  Adding a low F-layer density at 90 km
C       altitude prevents the F-layer density from erroneously influencing
C       The E-layer density.
C
      IF(ABSMLA .LT. 44.) THEN
         USELOW=.TRUE.
         CALL LOW_PARAM(MLAT,MLON,MLT,FOF2L,HMF2L,FOEL,HMEL,NLALT,ALTL,
     &                  DENL,ALTL(1,2),DENL(1,2))
         DO 100 IALT=NLALT(2),1,-1
            ALTL(IALT+1,2)=ALTL(IALT,2)
            DENL(IALT+1,2)=DENL(IALT,2)
  100    CONTINUE
         NLALT(2)=NLALT(2)+1
         ALTL(1,2)=90.
         DENL(1,2)=-7.5
      ELSE
         USELOW=.FALSE.
      ENDIF
C
C  If foE normalization to the CCIR foE model is requested, then calculate foE
C  from the CCIR foE model
C
      IF(ESWITCH) FOE=CCRFOE(DAY,UT/3600.,LAT,LON,F10P7)
C
C  If foF2 normalization to URSI is requested, then calculate foF2 from URSI
C
      IF(URSISW) THEN
         CALL TIMMDM(YEAR,DAY,IMONTH,IDOM)
         IUT=100*INT(UT/3600.)+INT(MOD(UT,3600.)/60.)
         CALL GETSSN(USESSN,F10P7,SSN)
         CALL F2URSI(YEAR,IMONTH,IDOM,IUT,SSN,LAT,LON,MLAT,MLON,MLT,
     &               FOF2,HMF2)
      ENDIF
C
C  Calculate the density floor
C
C      LDMIN=LOG(1.E-35)
      LDMIN=-80.5905
C
C  Merge, normalize, and interpolate profiles as necessary
C
C  A pure high-latitude case
C  Note:The foE from the parameterized high-latitude E-layer model is used
C       regardless of the requested foE normalization, presumably because the
C       solar-driven CCIR foE model is not valid in the auroral region.
C
      IF(USEHI .AND. (.NOT. USEMID)) THEN
C
C  Interpolate the E-layer density onto the output altitude grid
C
         CALL NEWGRID(NHALT(1),ALTH(1,1),DENH(1,1),NALTO,ALTO,EOUT)
C
C  Interpolate the F-layer density onto the output altitude grid
C
         CALL NEWGRID(NHALT(2),ALTH(1,2),DENH(1,2),NALTO,ALTO,FOUT)
C
C  If foF2 normalization to URSI is requested, then scale the F-layer density
C  to the URSI foF2
C
         IF(URSISW) THEN
            SCLFAC=2.*LOG(FOF2/FOF2H)
            DO 200 IALT=1,NALTO
               FOUT(IALT)=FOUT(IALT)+SCLFAC
  200       CONTINUE
         ENDIF
C
C  Set a floor on the densities
C
         DO 300 IALT=1,NALTO
            EOUT(IALT)=MAX(LDMIN,EOUT(IALT))
            FOUT(IALT)=MAX(LDMIN,FOUT(IALT))
  300    CONTINUE
C
C  A mid/high-latitude transition case
C
      ELSE IF(USEHI .AND. USEMID) THEN
C
C  Calculate the weights for the parameterized mid-latitude and high-latitude
C  models
C
         CALL FRAC(ABSMLA,RB,XX1,XX2,8.)
C
C  If foE normalization to the CCIR foE model is requested, then calculate the
C  E-layer peak density from the CCIR model foE
C
         IF(ESWITCH) THEN
            DMAX=LOG(F2TOD*FOE**2)
C
C  If no foE normalization to the CCIR foE model is requested, then calculate
C  the E-layer peak density from a weighted average of the E-layer peak
C  densities from the parameterized mid-latitude and high-latitude E-layer
C  models
C
         ELSE
            DMAX=LOG(F2TOD*(XX1*FOEM**2+XX2*FOEH**2))
         ENDIF
C
C  Calculate the E-layer peak height as a weighted average of the E-layer
C  peak heights from the parameterized mid-latitude and high-latitude E-layer
C  models
C
         HMAX=XX1*HMEM+XX2*HMEH
C
C  Merge the mid-latitude and high-latitude E-layer densities
C
         CALL MERGE(NMALT(1),ALTM(1,1),DENM(1,1),NHALT(1),ALTH(1,1),
     &              DENH(1,1),XX1,XX2,DMAX,HMAX,NALTO,ALTO,EOUT)
C
C  If foF2 normalization to URSI is requested, then calculate the F-layer peak
C  density from the URSI foF2
C
         IF(URSISW) THEN
            DMAX=LOG(F2TOD*FOF2**2)
C
C  If no foF2 normalization to URSI is requested, then calculate the F-layer
C  peak density from a weighted average of the F-layer peak densities from the
C  parameterized mid-latitude and high-latitude F-layer models
C
         ELSE
            DMAX=LOG(F2TOD*(XX1*FOF2M**2+XX2*FOF2H**2))
         ENDIF
C
C  Calculate the F-layer peak height as a weighted average of the F-layer
C  peak heights from the parameterized mid-latitude and high-latitude F-layer
C  models
C
         HMAX=XX1*HMF2M+XX2*HMF2H
C
C  Merge the mid-latitude and high-latitude F-layer densities
C
         CALL MERGE(NMALT(2),ALTM(1,2),DENM(1,2),NHALT(2),ALTH(1,2),
     &              DENH(1,2),XX1,XX2,DMAX,HMAX,NALTO,ALTO,FOUT)
C
C  A pure mid-latitude case
C
      ELSE IF(USEMID .AND. (.NOT. USELOW)) THEN
C
C  Interpolate the E-layer density onto the output altitude grid
C
         CALL NEWGRID(NMALT(1),ALTM(1,1),DENM(1,1),NALTO,ALTO,EOUT)
C
C  If foE normalization to the CCIR foE model is requested, then scale the
C  E-layer density to the CCIR model foE
C
         IF(ESWITCH) THEN
            SCLFAC=2.*LOG(FOE/FOEM)
            DO 400 IALT=1,NALTO
               EOUT(IALT)=EOUT(IALT)+SCLFAC
  400       CONTINUE
         ENDIF
C
C  Interpolate the F-layer density onto the output altitude grid
C
         CALL NEWGRID(NMALT(2),ALTM(1,2),DENM(1,2),NALTO,ALTO,FOUT)
C
C  If foF2 normalization to URSI is requested, then scale the F-layer density
C  to the URSI foF2
C
         IF(URSISW) THEN
            SCLFAC=2.*LOG(FOF2/FOF2M)
            DO 500 IALT=1,NALTO
               FOUT(IALT)=FOUT(IALT)+SCLFAC
  500       CONTINUE
         ENDIF
C
C  Set a floor on the densities
C
         DO 600 IALT=1,NALTO
            EOUT(IALT)=MAX(LDMIN,EOUT(IALT))
            FOUT(IALT)=MAX(LDMIN,FOUT(IALT))
  600    CONTINUE
C
C  A low/mid-latitude transition case
C  Note:No merging of the low-latitude and mid-latitude E-layer densities is
C       needed since both come from the same parameterized model and should be
C       identical.
C
      ELSE IF(USEMID .AND. USELOW) THEN
C
C  Interpolate the E-layer density onto the output altitude grid
C
         CALL NEWGRID(NLALT(1),ALTL(1,1),DENL(1,1),NALTO,ALTO,EOUT)
C
C  If foE normalization to the CCIR foE model is requested, then scale the
C  E-layer density to the CCIR model foE
C
         IF(ESWITCH) THEN
            SCLFAC=2.*LOG(FOE/FOEL)
            DO 700 IALT=1,NALTO
               EOUT(IALT)=MAX(LDMIN,EOUT(IALT)+SCLFAC)
  700       CONTINUE
         ENDIF
C
C  Calculate the weights for the parameterized low-latitude and mid-latitude
C  models
C
         CALL FRAC(ABSMLA,44.,XX1,XX2,10.)
C
C  If foF2 normalization to URSI is requested, then calculate the F-layer peak
C  density from the URSI foF2
C
         IF(URSISW) THEN
            DMAX=LOG(F2TOD*FOF2**2)
C
C  If no foF2 normalization to URSI is requested, then calculate the F-layer
C  peak density from a weighted average of the F-layer peak densities from the
C  parameterized low-latitude and mid-latitude F-layer models
C
         ELSE
            DMAX=LOG(F2TOD*(XX1*FOF2L**2+XX2*FOF2M**2))
         ENDIF
C
C  Calculate the F-layer peak height as a weighted average of the F-layer
C  peak heights from the parameterized low-latitude and mid-latitude F-layer
C  models
C
         HMAX=XX1*HMF2L+XX2*HMF2M
C
C  Merge the mid-latitude and high-latitude F-layer densities
C
         CALL MERGE(NLALT(2),ALTL(1,2),DENL(1,2),NMALT(2),ALTM(1,2),
     &              DENM(1,2),XX1,XX2,DMAX,HMAX,NALTO,ALTO,FOUT)
C
C  A pure low-latitude case
C
      ELSE
C
C  Interpolate the E-layer density onto the output altitude grid
C
         CALL NEWGRID(NLALT(1),ALTL(1,1),DENL(1,1),NALTO,ALTO,EOUT)
C
C  If foE normalization to the CCIR foE model is requested, then scale the
C  E-layer density to the CCIR model foE
C
         IF(ESWITCH) THEN
            SCLFAC=2.*LOG(FOE/FOEL)
            DO 800 IALT=1,NALTO
               EOUT(IALT)=EOUT(IALT)+SCLFAC
  800       CONTINUE
         ENDIF
C
C  Interpolate the F-layer density onto the output altitude grid
C
         CALL NEWGRID(NLALT(2),ALTL(1,2),DENL(1,2),NALTO,ALTO,FOUT)
C
C  If foF2 normalization to URSI is requested, then scale the F-layer density
C  to the URSI foF2
C
         IF(URSISW) THEN
            SCLFAC=2.*LOG(FOF2/FOF2L)
            DO 900 IALT=1,NALTO
               FOUT(IALT)=FOUT(IALT)+SCLFAC
  900       CONTINUE
         ENDIF
C
C  Set a floor on the densities
C
         DO 1000 IALT=1,NALTO
            EOUT(IALT)=MAX(LDMIN,EOUT(IALT))
            FOUT(IALT)=MAX(LDMIN,FOUT(IALT))
 1000    CONTINUE
C
      ENDIF
C
C  Calculate critical frequencies and heights
C
      CALL GET_FOS(NALTO,ALTO,EOUT,NALTO,ALTO,FOUT,HME,HMF2,FOE,FOF2,
     &             'B')
C
      RETURN
      END
C
         SUBROUTINE GETSSN(USESSN,F10P7,SSN)
         INTEGER USESSN
         REAL F10P7,SSN
         IF (USESSN .GT. 0) THEN
          IF (USESSN .EQ. 1) THEN
           IF (F10P7 .GT. 63.74) THEN
            SSN = SQRT(93918.4+1117.3*F10P7)-406.37
           ELSE
            SSN = 0.0
           ENDIF
          ELSE
           IF (SSN .GT. 0.0) THEN
            F10P7 = ((SSN+406.37)**2-93918.4)/1117.3
           ELSE
            F10P7 = 63.74
           ENDIF
          ENDIF
         ENDIF
         RETURN
         END
       SUBROUTINE FRAC(MLAT,RB,XX1,XX2,DIST)
C
C  PURPOSE
C     To determine the fraction of the USU and Midlatitude model
C  output to use.
C
C  METHOD
C     Linear interpolation using the distance from the equatorward
C  trough boundary over an 8 degree transition.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    DIST    REAL              Width of area to interpolate over.
C    MLAT    REAL              Magnetic latitude of point in question.
C    RB      REAL              Magnetic latitude of The higher boundary
C                              of the area to interpolate over.
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    XX1     REAL              Fraction of lower latitude output to use
C    XX2     REAL              Fraction of higher latitude output to use
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
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby    12-Jun-1991   1.0.6
C     L. Brown        25-Sep-1995   1.0.6 ==> 1.3
C                                   Replaced WRITE statement to unit 6 with
C                                   PRINT statement.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C
       INCLUDE 'logicuni.inc'
       REAL MLAT,RB,XX1,XX2,DIST
       XX1 = (RB-ABS(MLAT))/DIST
       XX2 = 1.- XX1
       IF ((ABS(MLAT) .GT. RB) .OR. (ABS(MLAT) .LT. RB-DIST)) THEN
#ifdef DEBUG
        WRITE(LUSTDERR,1000) MLAT, RB,RB-DIST,XX1,XX2
#endif
       ENDIF
       RETURN
 1000  FORMAT (' Latitude = ',0PF6.2,' Boundaries are ',0PF6.2,' and ',
     1 0PF6.2,/,' Returned values are ',0PF6.4,' and ',0PF6.4,/,
     1 ' Error in FRAC')
       END
C
          SUBROUTINE MERGE(NLALT,LALT,LDEN,NHALT,HALT,HDEN,XX1,XX2,
     1    NMAX,HMAX,NOALT,OUTALT,OUTDEN)
C
C  PURPOSE
C    To get and admixture of density profiles LDEN and HDEN in proportion
C  XX1 and XX2, respectively, and put them on altitude grid OUTALT
C
C  METHOD
C     Fit the logarithmic density LDEN on altitude grid LALT, to have
C  Maximum density NMAX at altitude HMAX. Then put this density onto
C  the output grid OUTALT. Take the exponent of each element in the
C  density vector and multiply it by XX1. Save this vector. Repeat the
C  procedure for HDEN, except use XX2 for the final multiplication.
C  Finally, add the two results for the final output density on OUTALT.
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    HALT    REAL      (NHALT) Altitude vector for the 'higher' density
C    HDEN    REAL      (NHALT) Higher density
C    LALT    REAL      (NLALT) Altitude vector for the 'lower' density
C    LDEN    REAL      (NLALT) Lower density
C    NHALT   INTEGER           Array dimensions
C    NLALT   INTEGER           Array dimensions
C    NOUTALT INTEGER           Array dimensions
C    OUTALT  REAL    (NOUTALT) Altitude grid to put output density on
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    OUTDEN  REAL    (NOUTALT) Output density
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    I       INTEGER           Do loop index
C    T1DEN   REAL       (100)  Temporory holding array for LDEN or HDEN
C                              when modified
C    T2DEN   REAL       (100)  Temporory holding array for LDEN or HDEN
C                              when modified
C
C  SUBROUTINES CALLED
C    NAME     description
C    NEWGRID  Puts a density vector on a specific altitude vector onto a
C             new altitude vector
C    SCALEP   Scales a density vecot on an altitude vector to have a given
C             maximum density and height of maximum density.
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
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby    21-Jul-1991   1.0.6
C     L. Brown        25-Sep-1995   1.0.6 ==> 1.3
C                                   Removed coding for E-layer since it is not
C                                   used.
C                                   Removed input argument LAYR since it is no
C                                   longer used.
C                                   The actual value of the density is now
C                                   merged instead of its logarithm.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
C
          INCLUDE 'array.inc'
          INTEGER NLALT,NHALT,NOALT
          REAL NMAX,HMAX
          REAL LALT(NLALT),LDEN(NLALT),HALT(NHALT),HDEN(NHALT)
          REAL OUTALT(NOALT),OUTDEN(NOALT)
C
          INTEGER I
          REAL T1DEN(MAX_ALT_NUM),T2DEN(MAX_ALT_NUM)
          REAL XX1,XX2
C
C         Scale this density to the input FMAX and HMF2
          CALL SCALEP(NLALT,LALT,LDEN,NMAX,HMAX,T1DEN)
C
C         Put the lower density on the output altitude grid
          CALL NEWGRID(NLALT,LALT,T1DEN,NOALT,OUTALT,T2DEN)
C
C         Load the output grid with the corresponding fraction of
C         this adjusted grid.
          DO I = 1,NOALT
           OUTDEN(I) = EXP(T2DEN(I))*XX1
          ENDDO
C
C         Scale this density to the input FMAX and HMF2
          CALL SCALEP(NHALT,HALT,HDEN,NMAX,HMAX,T1DEN)
C
C         Put the higher density on the output altitude grid
          CALL NEWGRID(NHALT,HALT,T1DEN,NOALT,OUTALT,T2DEN)
C
C         Load the output grid with the corresponding fraction of
C         this adjusted grid.
          DO I = 1,NOALT
           OUTDEN(I) = LOG(MAX(1.E-35,OUTDEN(I) + EXP(T2DEN(I))*XX2))
          ENDDO
          RETURN
          END
C
C
