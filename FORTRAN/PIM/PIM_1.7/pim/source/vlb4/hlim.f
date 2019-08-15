      SUBROUTINE REGMOD(DSGNTR,YEAR,DAY,UT,LAT,LON,MLAT,MLON,MLT,
     1  NDATA,DATA,CDATA)
C
C  PURPOSE
C     To calculate a quantity corresponding to direct data, using regional
C     high latitude models.
C
C  METHOD
C     Call the appropriate regional models to obtain the type of output
C     (Ionosonde, EDP, etc.) requested by the calling program.
C
C  INPUT PARAMETERS
C     DATA     REAL       ARRAY  A vector containing a direct data record
C     DAY      INTEGER           The day of the year
C     DSGNTR   CHARACTER         A code designating a type of direct data
C     F10P7    REAL              The solar activity index
C     INDRCT   REAL       ARRAY  A vector containing the indirect parameters
C     LAT      REAL              The geocentric latitude, in degrees north
C     LON      REAL              The geocentric longitude, in degrees east
C     MLAT     REAL              The magnetic latitude, in degrees north
C     MLON     REAL              The magnetic longitude, in degrees east
C     MLT      REAL              The magnetic local time, in hours
C     NDATA    INTEGER           The length of the DATA and CDATA vectors
C     YEAR     INTEGER           The year
C
C  OUTPUT PARAMETERS
C     CDATA    REAL       ARRAY  A vector containing the calculated quantity corresponding to
C                                a direct data record
C
C  LOCAL VARIABLES
C     EKP      REAL              KP of E layer
C     FOED     REAL              fOE from an IONOSONDE data record, in MHz
C     FOF1D    REAL              fOF1 from an IONOSONDE data record, in MHz
C     FOF2D    REAL              fOF2 from an IONOSONDE data record, in MHz
C     HMED     REAL              HmE from an IONOSONDE data record, in km
C     HMF1D    REAL              HmF1 from an IONOSONDE data record, in km
C     HMF2D    REAL              HmF2 from an IONOSONDE data record, in km
C     I        INTEGER           A loop counter
C     IP7      INTEGER           I+7
C     KP       REAL              KP for the 3 ions
C     NZD      INTEGER           The number of altitude points from an EDP data
C                                record
C     NED      REAL       ARRAY  The electron density from an EDP data record,
C                                in cm-3
C     POUT     REAL       ARRAY  The plasmaspheric contribution to total
C                                electron density, in cm-3
C     ZD       REAL       ARRAY  The altitude grid from an EDP data record, in km
C
C  SUBROUTINES REQUIRED
C     PLAEXT                     Extends E-region and F-region densities to
C                                plasmaspheric altitudes and calculates the
C                                plasmaspheric contribution to the total
C                                electron density
C     TECCLC                     Calculates TEC
C
C  FILES ACCESSED
C     NONE
C
C  AUTHOR
C     Robert E. Daniell and Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        22-Sep-1994   1.0.6 ==> 1.0.9
C                                   Include statements for math4_co.inc and
C                                   phys4_co.inc deleted because their
C                                   parameters are unused.
C     L. Brown        18-Nov-1994   1.0.9 ==> 1.1
C                                   TEC has been added to designator IONOSONDE
C                                   output.
C                                   Designator ALL output has been redefined to
C                                   be a combination of designator IONOSONDE
C                                   and designator EDP output.
C                                   Designator ALL output IF block has been
C                                   separated from designator IONOSONDE IF
C                                   block.
C                                   Eliminated extra call to PARAM in
C                                   designator IONOSONDE IF block.
C                                   Removed references to function FLPYR.
C     L. Brown        25-Sep-1995   1.1 ==> 1.3
C                                   Removed logic for designators MIDIONOSONDE,
C                                   PRECIPITATION, TROUGH, PLASMA, SSIES, and
C                                   BOTTOMSIDE since they are not used.
C                                   Logical flag ONLYOP is now a PARAMETER
C                                   instead of a variable in common block
C                                   INDIRECT.  ONLYOP should normally be FALSE.
C     L. Brown        30-Sep-1996   1.3 ==> 1.5
C                                   The E-layer density returned from PARAM is
C                                   now a logarithm for consistency with the
C                                   F-layer density returned from PARAM.
C                                   Removed last argument in calls to PARAM
C                                   since it is no longer used by that routine.
C                                   Removed call to REGION since it is no
C                                   longer used.
C                                   Removed local variable RCODE since it is no
C                                   longer used.
C     L. Brown        14-Feb-1997   1.5 ==> 1.6
C                                   Added points to altitude grid stored in
C                                   local array TECALT to include plasmaspheric
C                                   altitudes.
C                                   Added PARAMETER IA1400.
C                                   Added local array POUT to store the
C                                   plasmaspheric contribution to the total
C                                   electron density.
C                                   Modified calls to routine PARAM so that
C                                   E-region and F-region densities are
C                                   calculated only up to 1400 km altitude.
C                                   Added calls to routine PLAEXT after calls
C                                   to routine PARAM to extend E-region and
C                                   F-region densities to plasmaspheric
C                                   altitudes and to calculate the
C                                   plasmaspheric contribution to the total
C                                   electron density.
C                                   Removed PARAMETERs AFPE and EMAXALT since
C                                   they are not used.
C                                   Removed comment for PARAMETER NZDE since it
C                                   is not defined.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     IA1400   The index of the highest altitude <= 1400 km on the TEC altitude
C              grid
C     NTEC     Number of altitude points for TEC calls.
C     ONLYOP   A logical flag, .TRUE. if PIM is to output O+ density instead
C              of electron density, .FALSE. if PIM is to output electron
C              density
C
      INCLUDE 'indirect.inc'
      INCLUDE 'array.inc'
      INCLUDE 'logicuni.inc'
C
      INTEGER NTEC,IA1400
      LOGICAL ONLYOP
      PARAMETER (NTEC=100,IA1400=68,ONLYOP=.FALSE.)
C
      INTEGER NDATA,YEAR,DAY,I,NZD,IL1,IL,IP7
      REAL DATA(NDATA+1),CDATA(NDATA),RB(0:2)
      REAL ZD(MAX_ALT_NUM),NED(MAX_ALT_NUM),EOUT(MAX_ALT_NUM)
      REAL FOUT(MAX_ALT_NUM),TECALT(NTEC),POUT(MAX_ALT_NUM)
      REAL LAT,LON,MLAT,MLON,MLT,UT
      REAL FOED,HMED,FOF2,HMF2,FOE,HME
      CHARACTER*(*) DSGNTR
      INTEGER IA14002,NALT2,ZDPOS2
      REAL TECALT2(6)
      DATA TECALT/90.,95.,100.,105,110,115,120,125,130.,135.,140.,145.,
     1 150.,160.,170.,180.,190.,200.,210.,220.,230.,240.,250.,260.,270.,
     2 280.,290.,300.,310.,320.,330.,340.,350.,360.,370.,380.,390.,400.,
     3 410.,420.,430.,440.,450.,460.,470.,480.,490.,500.,525.,550.,575.,
     4 600.,625.,650.,675.,700.,800.,825.,850.,875.,900.,1000.,1050.,
     5 1100.,1150.,1200.,1300.,1400.,1500.,1600.,1700.,1800.,1900.,
     & 2000.,2100.,2200.,2300.,2400.,2500.,3000.,3500.,4000.,4500.,
     & 5000.,5500.,6000.,6500.,7000.,7500.,8000.,8500.,9000.,9500.,
     & 10000.,12500.,15000.,17500.,20000.,22500.,25000./
C
C
C     Get the region and do other initializing.
      CALL BNDRY(MLAT,MLT,RB)
C     
      DO IL1 = 1,NDATA
         CDATA(IL1) = 0.0
      ENDDO
      IF(DSGNTR .EQ. 'IONOSONDE') THEN
C     The direct data is of type IONOSONDE
C     Transfer the contents of the IONOSONDE data record to specific variables
C     
         FOED=DATA(5)
         HMED=DATA(6)
         FOF2 = 0.0
         HMF2 = 0.0
         FOE = 0.0
         HME = 0.0
         IF ((FOED .NE. 0.0) .AND. ( HMED .EQ. 0.0)) HMED = 115.
         CALL PARAM(YEAR,DAY,UT,LAT,LON,MLAT,MLON,MLT,RB(0),
     1        FOF2,HMF2,FOE,HME,IA1400,TECALT,EOUT,FOUT)
         CALL PLAEXT(MLAT,MLT,IA1400,NTEC,TECALT,FOE,HME,FOF2,HMF2,EOUT,
     &        FOUT,POUT)
         CDATA(1) = FOF2
         CDATA(2) = HMF2
         CDATA(5) = FOE
         CDATA(6) = HME
         DO IL = 1, NTEC
            NED(IL) = MAX(EXP(EOUT(IL))+EXP(FOUT(IL))+POUT(IL),1.E-9)
         ENDDO
         CALL TECCLC(NTEC,TECALT,NED,CDATA(7))
         CDATA(7)=CDATA(7)*1.E-12
      ELSE IF(DSGNTR .EQ. 'EDP') THEN
C     The direct data is of type EDP
C     
         IF(NDATA.EQ.1) THEN
            ZD(1) = DATA(2)
            IF(ZD(1).LT.1350.) THEN
               TECALT2(1) = ZD(1)*0.99
               TECALT2(2) = ZD(1)
               TECALT2(3) = ZD(1)*1.01
               TECALT2(4) = 1375.
               TECALT2(5) = 1400.
               TECALT2(6) = 1425.
               NALT2 = 6
               IA14002 = 5
               ZDPOS2=2
            ELSEIF(ZD(1).GE.1450.) THEN
               TECALT2(1) = 1375.
               TECALT2(2) = 1400.
               TECALT2(3) = 1425.
               TECALT2(4) = ZD(1)*0.99
               TECALT2(5) = ZD(1)
               TECALT2(6) = ZD(1)*1.01
               NALT2 = 6
               IA14002 = 2
               ZDPOS2=5
            ELSEIF(ZD(1).LT.1375.) THEN
               TECALT2(1) = ZD(1)
               TECALT2(2) = 1375.
               TECALT2(3) = 1400.
               TECALT2(4) = 1425.
               NALT2 = 4
               IA14002 = 3
               ZDPOS2=1
            ELSEIF(ZD(1).LT.1400.) THEN
               TECALT2(1) = 1375.
               TECALT2(2) = ZD(1)
               TECALT2(3) = 1400.
               TECALT2(4) = 1425.
               NALT2 = 4
               IA14002 = 3
               ZDPOS2=2
            ELSEIF(ZD(1).eq.1400.) THEN
               TECALT2(1) = 1375.
               TECALT2(2) = 1400.
               TECALT2(3) = 1425.
               NALT2 = 3
               IA14002 = 2
               ZDPOS2=2
            ELSEIF(ZD(1).LT.1425.) THEN
               TECALT2(1) = 1375.
               TECALT2(2) = 1400.
               TECALT2(3) = ZD(1)
               TECALT2(4) = 1425.
               NALT2 = 4
               IA14002 = 2
               ZDPOS2=3
            ELSE
               TECALT2(1) = 1375.
               TECALT2(2) = 1400.
               TECALT2(3) = 1425.
               TECALT2(4) = ZD(1)
               NALT2 = 4
               IA14002 = 2
               ZDPOS2=4
            ENDIF
            CALL PARAM(YEAR,DAY,UT,LAT,LON,MLAT,MLON,MLT,RB(0),
     &           FOF2,HMF2,FOE,HME,IA14002,TECALT2,EOUT,FOUT)
            CALL PLAEXT2(MLAT,MLT,IA14002,NALT2,ZDPOS2,
     &           TECALT2,FOE,HME,FOF2,
     &           HMF2,EOUT,FOUT,POUT)
            IF ( .NOT. ONLYOP) THEN
C     
C     output the full electron density
C     
               FOUT(ZDPOS2) = EXP(FOUT(ZDPOS2))
               NED(ZDPOS2)=MAX(EXP(EOUT(ZDPOS2))+FOUT(ZDPOS2)
     &              +POUT(ZDPOS2),1.E-9)
            ELSE
C     
C     output only the O+ profile
C     
               NED(ZDPOS2) = EXP(FOUT(ZDPOS2))
            ENDIF
C     
C     Transfer the data to the user specified altitude grid and get the
C     TEC for this run. First, put the log density on the new altitude grid.
C     
            CDATA(1) = NED(ZDPOS2)
C     print *, 'got new ', CDATA(1), ZD(1)
         ELSE
            CALL PARAM(YEAR,DAY,UT,LAT,LON,MLAT,MLON,MLT,RB(0),
     1           FOF2,HMF2,FOE,HME,IA1400,TECALT,EOUT,FOUT)
            CALL PLAEXT(MLAT,MLT,IA1400,NTEC,TECALT,FOE,HME,FOF2,HMF2,
     &           EOUT,
     &           FOUT,POUT)
C     
            IF ( .NOT. ONLYOP) THEN
C     
C     output the full electron density
C     
               DO IL = 1, NTEC
                  FOUT(IL) = EXP(FOUT(IL))
                  NED(IL) = LOG(MAX(EXP(EOUT(IL))+FOUT(IL)+POUT(IL),
     &                 1.E-9))
               ENDDO
            ELSE
C     
C     output only the O+ profile
C     
               DO IL = 1,NTEC
                  NED(IL) = FOUT(IL)
               ENDDO
            ENDIF
C     
C     Transfer the contents of the EDP data record to specific variables
            NZD=INT(DATA(1))
            DO I=1,NZD
               ZD(I)=DATA(1+I)
            ENDDO
C     
C     Transfer the data to the user specified altitude grid and get the
C     TEC for this run. First, put the log density on the new altitude grid.
C     
            CALL NEWGRID(NTEC,TECALT,NED,NZD,ZD,CDATA(1))
            DO I=1,NZD
               CDATA(I) = EXP(CDATA(I))
            ENDDO
            
C     print *, 'got new ', CDATA(1)
         ENDIF
      ELSE IF(DSGNTR .EQ. 'ALL') THEN
C  The direct data is of type ALL
C
       FOED=DATA(5)
       HMED=DATA(6)
       FOF2 = 0.0
       HMF2 = 0.0
       FOE = 0.0
       HME = 0.0
       IF ((FOED .NE. 0.0) .AND. ( HMED .EQ. 0.0)) HMED = 115.
       CALL PARAM(YEAR,DAY,UT,LAT,LON,MLAT,MLON,MLT,RB(0),
     1  FOF2,HMF2,FOE,HME,IA1400,TECALT,EOUT,FOUT)
       CALL PLAEXT(MLAT,MLT,IA1400,NTEC,TECALT,FOE,HME,FOF2,HMF2,EOUT,
     &             FOUT,POUT)
       CDATA(1) = FOF2
       CDATA(2) = HMF2
       CDATA(5) = FOE
       CDATA(6) = HME
       DO IL = 1, NTEC
        NED(IL) = MAX(EXP(EOUT(IL))+EXP(FOUT(IL))+POUT(IL),1.E-9)
       ENDDO
       CALL TECCLC(NTEC,TECALT,NED,CDATA(7))
       CDATA(7)=CDATA(7)*1.E-12
C
       IF ( .NOT. ONLYOP) THEN
C
C       output the full electron density
C
        DO IL = 1, NTEC
         NED(IL) = LOG(NED(IL))
        ENDDO
       ELSE
C
C        output only the O+ profile
C
        DO IL = 1,NTEC
         NED(IL) = FOUT(IL)
        ENDDO
       ENDIF
C
C       Transfer the contents of the EDP data record to specific variables
        NZD=INT(DATA(8))
        DO I=1,NZD
         ZD(I)=DATA(8+I)
        ENDDO
C
C      Transfer the data to the user specified altitude grid and get the
C      TEC for this run. First, put the log density on the new altitude grid.
C
       CALL NEWGRID(NTEC,TECALT,NED,NZD,ZD,CDATA(8))
       DO I=1,NZD
        IP7=I+7
        CDATA(IP7) = EXP(CDATA(IP7))
       ENDDO
      ELSE
C  The direct data type is unknown
         WRITE(LUSTDERR,910)DSGNTR
         STOP
      ENDIF
C
      RETURN
  910 FORMAT(1X,'REGMOD:',A,' is not a defined data designator.')
      END
C
      SUBROUTINE PLAEXT(MLAT,MLT,IA1400,NZ,Z,FOE,HME,FOF2,HMF2,EOUT,
     &                  FOUT,POUT)
C
C  PURPOSE
C     To extend E-region and F2-region densities to plasmaspheric altitudes and
C     calculate the plasmaspheric contribution to the total electron density.
C
C  METHOD
C     The E-region and F2-region densities are extrapolated to plasmaspheric
C     altitudes (defined to be > 1400 km) using a gravitationally-corrected
C     scale height.  If a plasmasphere is to be included, then the Gallagher
C     model is used to calculate the plasmaspheric contribution to the total
C     electron density, and the E-region and F2-region critical frequencies
C     (foE and foF2) are modified to include a plasmaspheric contribution.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic E-layer density
C     FOE    Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency (foE)
C     FOF2   Real       Scalar        MHz             >= 0.
C        The F2-layer critical frequency (foF2)
C     FOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic F-layer density
C     HME    Real       Scalar        km              >= 0.
C        The E-layer critical height (hmE)
C     HMF2   Real       Scalar        km              >= 0.
C        The F2-layer critical height (hmF2)
C     IA1400 Integer    Scalar        n/a             3 <= IA1400 <= NZ
C        The index of the highest altitude <= 1400 km on the altitude grid
C     MLAT   Real       Scalar        deg N           -90. <= MLAT <= 90.
C        Magnetic latitude
C     MLT    Real       Scalar        hr              0. <= MLT < 24.
C        Magnetic local time
C     NZ     Integer    Scalar        n/a             >= 1
C        The number of altitudes
C     Z      Real       Vector        km              >= 0.
C                       (NZ)
C        The altitude grid, assumed to be increasing with index
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic E-layer density with a plasmasphere extrapolation
C     FOE    Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency (foE) with a plasmaspheric contribution
C     FOF2   Real       Scalar        MHz             >= 0.
C        The F2-layer critical frequency (foF2) with a plasmaspheric
C        contribution
C     FOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic F-layer density with a plasmasphere extrapolation
C     POUT   Real       Vector        cm-3            >= 0.
C                       (NZ)
C        The plasmaspheric contribution to the total electron density
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DELDLN Real       Scalar        n/a             n/a
C        The difference between logarithmic densities
C     DLNMIN Real       Scalar        n/a             log(1.E-35)
C        The minimum allowed extrapolated logarithmic density
C     H      Real       Scalar        km              n/a
C        The scale height used for extrapolating E-layer and F2-layer densities
C        to plasmaspheric altitudes
C     H0     Real       Scalar        km              25. <= H0 <= 800.
C        The gravitationally-uncorrected scale height
C     IZ     Integer    Scalar        n/a             1 <= IZ <= NZ
C        The altitude index
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     GALHPL Real       Calculates the plasmaspheric (H+) density according to
C                       Gallagher's empirical plasmasphere model
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
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
C     L. Brown       14-Feb-1997  1.6 ==> Created
C     L. Brown       13-Jan-1998  1.6 ==> 1.7
C                                 Added handling for constant topside density
C                                 in the calculation of the gravitationally-
C                                 uncorrected scale height.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     RE     Real       km              The mean radius of the Earth
C
      REAL RE
      PARAMETER(RE=6375.)
C
C  Input variables
C
      INTEGER IA1400,NZ
      REAL MLAT,MLT,FOE,HME,FOF2,HMF2
      REAL Z(NZ),EOUT(NZ),FOUT(NZ)
C
C  Output variables
C
      REAL POUT(NZ)
C
C  Local variables
C
      INTEGER IZ
      REAL DLNMIN,DELDLN,H0,H
C
C  Function declarations
C
      REAL GALHPL
C
      INCLUDE 'grid.inc'
C
C  The altitude grid contains altitudes greater than 1400 km
C
      IF(IA1400 .LT. NZ) THEN
C
C  Calculate the minimum allowed extrapolated logarithmic density
C
C         DLNMIN=LOG(1.E-35)
         DLNMIN=-80.5905
C
C  Extrapolate the E-layer density to plasmaspheric altitudes assuming that the
C  E-layer density above 1400 km altitude has a scale height that takes into
C  account the fall-off in gravitational attraction with increasing altitude
C  Note:The gravitationally uncorrected scale height for the E-layer density is
C       restricted to the range [25.,800.] km.
C
         DELDLN=EOUT(IA1400)-EOUT(IA1400-1)
         IF(DELDLN .EQ. 0.) THEN
            H0=800.
         ELSE
            H0=MIN(800.,MAX(25.,-(Z(IA1400)-Z(IA1400-1))/DELDLN))
         ENDIF
         DO 10 IZ=IA1400+1,NZ
            H=H0*(RE+Z(IZ))/(RE+Z(IA1400))
            EOUT(IZ)=MAX(DLNMIN,EOUT(IA1400)-(Z(IZ)-Z(IA1400))/H)
   10    CONTINUE
C
C  Extrapolate the F2-layer density to plasmaspheric altitudes assuming that
C  the F2-layer density above 1400 km altitude has a scale height that takes
C  into account the fall-off in gravitational attraction with increasing
C  altitude
C  Note:The gravitationally uncorrected scale height for the F2-layer density
C       is restricted to the range [25.,800.] km.
C
         DELDLN=FOUT(IA1400)-FOUT(IA1400-1)
         IF(DELDLN .EQ. 0.) THEN
            H0=800.
         ELSE
            H0=MIN(800.,MAX(25.,-(Z(IA1400)-Z(IA1400-1))/DELDLN))
         ENDIF
         DO 100 IZ=IA1400+1,NZ
            H=H0*(RE+Z(IZ))/(RE+Z(IA1400))
            FOUT(IZ)=MAX(DLNMIN,FOUT(IA1400)-(Z(IZ)-Z(IA1400))/H)
  100    CONTINUE
C
      ENDIF
C
C  A plasmasphere is to be included
C
      IF(PLASPH) THEN
C
C  Calculate the plasmaspheric contribution to the total electron density
C
         DO 200 IZ=1,NZ
            POUT(IZ)=GALHPL(MLAT,MLT,Z(IZ))
  200    CONTINUE
C
C  Modify the E-layer critical frequency (foE) to include a plasmaspheric
C  contribution
C
         FOE=SQRT((1.24E4*FOE**2+GALHPL(MLAT,MLT,HME))/1.24E4)
C
C  Modify the F2-layer critical frequency (foF2) to include a plasmaspheric
C  contribution
C
         FOF2=SQRT((1.24E4*FOF2**2+GALHPL(MLAT,MLT,HMF2))/1.24E4)
C
C  A plasmasphere is not to be included
C
      ELSE
C
C  Set the plasmaspheric contribution to the total electron density to zero
C
         DO 300 IZ=1,NZ
            POUT(IZ)=0.
  300    CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE PLAEXT2(MLAT,MLT,IA1400,NZ,IZZ,
     &     Z,FOE,HME,FOF2,HMF2,EOUT,
     &                  FOUT,POUT)
C
C  PURPOSE
C     To extend E-region and F2-region densities to plasmaspheric altitudes and
C     calculate the plasmaspheric contribution to the total electron density.
C     *Intended for single altitude correction only*
C
C  METHOD
C     The E-region and F2-region densities are extrapolated to plasmaspheric
C     altitudes (defined to be > 1400 km) using a gravitationally-corrected
C     scale height.  If a plasmasphere is to be included, then the Gallagher
C     model is used to calculate the plasmaspheric contribution to the total
C     electron density, and the E-region and F2-region critical frequencies
C     (foE and foF2) are modified to include a plasmaspheric contribution.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic E-layer density
C     FOE    Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency (foE)
C     FOF2   Real       Scalar        MHz             >= 0.
C        The F2-layer critical frequency (foF2)
C     FOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic F-layer density
C     HME    Real       Scalar        km              >= 0.
C        The E-layer critical height (hmE)
C     HMF2   Real       Scalar        km              >= 0.
C        The F2-layer critical height (hmF2)
C     IA1400 Integer    Scalar        n/a             3 <= IA1400 <= NZ
C        The index of the highest altitude <= 1400 km on the altitude grid
C     MLAT   Real       Scalar        deg N           -90. <= MLAT <= 90.
C        Magnetic latitude
C     MLT    Real       Scalar        hr              0. <= MLT < 24.
C        Magnetic local time
C     NZ     Integer    Scalar        n/a             >= 1
C        The number of altitudes
C     IZZ    Integer    Scalar        n/a             >= 1
C        The single altitude to correct
C     Z      Real       Vector        km              >= 0.
C                       (NZ)
C        The altitude grid, assumed to be increasing with index
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     EOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic E-layer density with a plasmasphere extrapolation
C     FOE    Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency (foE) with a plasmaspheric contribution
C     FOF2   Real       Scalar        MHz             >= 0.
C        The F2-layer critical frequency (foF2) with a plasmaspheric
C        contribution
C     FOUT   Real       Vector        log(cm-3)       n/a
C                       (NZ)
C        The logarithmic F-layer density with a plasmasphere extrapolation
C     POUT   Real       Vector        cm-3            >= 0.
C                       (NZ)
C        The plasmaspheric contribution to the total electron density
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DELDLN Real       Scalar        n/a             n/a
C        The difference between logarithmic densities
C     DLNMIN Real       Scalar        n/a             log(1.E-35)
C        The minimum allowed extrapolated logarithmic density
C     H      Real       Scalar        km              n/a
C        The scale height used for extrapolating E-layer and F2-layer densities
C        to plasmaspheric altitudes
C     H0     Real       Scalar        km              25. <= H0 <= 800.
C        The gravitationally-uncorrected scale height
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     GALHPL Real       Calculates the plasmaspheric (H+) density according to
C                       Gallagher's empirical plasmasphere model
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
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
C     L. Brown       14-Feb-1997  1.6 ==> Created
C     L. Brown       13-Jan-1998  1.6 ==> 1.7
C                                 Added handling for constant topside density
C                                 in the calculation of the gravitationally-
C                                 uncorrected scale height.
C     James M Anderson 2007Apr13  Copy from PLAEXT to use for single altitude
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     RE     Real       km              The mean radius of the Earth
C
      REAL RE
      PARAMETER(RE=6375.)
C
C  Input variables
C
      INTEGER IA1400,NZ
      REAL MLAT,MLT,FOE,HME,FOF2,HMF2
      REAL Z(NZ),EOUT(NZ),FOUT(NZ)
C
C  Output variables
C
      REAL POUT(NZ)
C
C  Local variables
C
      INTEGER IZ,IZZ
      REAL DLNMIN,DELDLN,H0,H
C
C  Function declarations
C
      REAL GALHPL
C
      INCLUDE 'grid.inc'
C
C  The altitude grid contains altitudes greater than 1400 km
C
      IF(IA1400 .LT. NZ) THEN
C
C  Calculate the minimum allowed extrapolated logarithmic density
C
C         DLNMIN=LOG(1.E-35)
         DLNMIN=-80.5905
C
C  Extrapolate the E-layer density to plasmaspheric altitudes assuming that the
C  E-layer density above 1400 km altitude has a scale height that takes into
C  account the fall-off in gravitational attraction with increasing altitude
C  Note:The gravitationally uncorrected scale height for the E-layer density is
C       restricted to the range [25.,800.] km.
C
         DELDLN=EOUT(IA1400)-EOUT(IA1400-1)
         IF(DELDLN .EQ. 0.) THEN
            H0=800.
         ELSE
            H0=MIN(800.,MAX(25.,-(Z(IA1400)-Z(IA1400-1))/DELDLN))
         ENDIF
         DO 10 IZ=IA1400+1,NZ
            H=H0*(RE+Z(IZ))/(RE+Z(IA1400))
            EOUT(IZ)=MAX(DLNMIN,EOUT(IA1400)-(Z(IZ)-Z(IA1400))/H)
   10    CONTINUE
C
C  Extrapolate the F2-layer density to plasmaspheric altitudes assuming that
C  the F2-layer density above 1400 km altitude has a scale height that takes
C  into account the fall-off in gravitational attraction with increasing
C  altitude
C  Note:The gravitationally uncorrected scale height for the F2-layer density
C       is restricted to the range [25.,800.] km.
C
         DELDLN=FOUT(IA1400)-FOUT(IA1400-1)
         IF(DELDLN .EQ. 0.) THEN
            H0=800.
         ELSE
            H0=MIN(800.,MAX(25.,-(Z(IA1400)-Z(IA1400-1))/DELDLN))
         ENDIF
         DO 100 IZ=IA1400+1,NZ
            H=H0*(RE+Z(IZ))/(RE+Z(IA1400))
            FOUT(IZ)=MAX(DLNMIN,FOUT(IA1400)-(Z(IZ)-Z(IA1400))/H)
  100    CONTINUE
C
      ENDIF
C
C  A plasmasphere is to be included
C
      IF(PLASPH) THEN
C
C  Calculate the plasmaspheric contribution to the total electron density
C
         POUT(IZZ)=GALHPL(MLAT,MLT,Z(IZZ))
C
C  A plasmasphere is not to be included
C
      ELSE
C
C  Set the plasmaspheric contribution to the total electron density to zero
C
         POUT(IZZ)=0.
C
      ENDIF
C
      RETURN
      END
      FUNCTION GALHPL(LAMBDA,T,Z)
C
C  PURPOSE
C     To calculate the H+ concentration (number density) based on Gallagher.
C
C  METHOD
C     The Gallagher model of plasmaspheric H+ is based on work by Gallagher et
C     al. [1].  Given the dipole geomagnetic latitude, dipole geomagnetic
C     local time, and altitude, the plasmaspheric H+ concentration is returned.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     Z      Real       Scalar        km              >= 0.
C        The altitude
C     LAMBDA Real       Scalar        Degrees north   -90. <= LAMBDA <= 90.
C        The dipole geomagnetic latitude
C     T      Real       Scalar        Hours           0. <= T < 24.
C        The dipole geomagnetic local time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     GALHPL Real       Scalar        cm-3            >= 0.
C        The H+ concentration
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     A6     Real       Scalar        km-1            n/a
C        The value of the factor a6(T)
C     A8     Real       Scalar        km              n/a
C        The value of the factor a8(T)
C     A9     Real       Scalar        km              n/a
C        The value of the factor a9(T)
C     A9M1   Real       Scalar        km              n/a
C        A9-1.
C     COSLA2 Real       Scalar        n/a             0. <= COSLA2 <= 1.
C        The square of the cosine of the dipole geomagnetic latitude
C     COSLAM Real       Scalar        n/a             -1. <= COSLAM <= 1.
C        The cosine of the dipole geomagnetic latitude
C     DTOR   Real       Scalar        Degrees-1       pi/180.
C        The conversion factor from degrees to radians
C     F      Real       Scalar        n/a             n/a
C        The value of the function F(L-value,Lambda)
C     G      Real       Scalar        n/a             n/a
C        The value of the function G(L-value,T)
C     H      Real       Scalar        n/a             n/a
C        The value of the function H(L-value,T)
C     HTOR   Real       Scalar        Hours-1         pi/12.
C        The conversion factor from hours to radians
C     L      Real       Scalar        n/a             >= 0.
C        The L-value
C     PI     Real       Scalar        n/a             pi
C        Pi
C     X      Real       Scalar        Hours           -12. <= X <= 12.
C        T-12.
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
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.6   14-February-1997
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       14-Feb-1997  1.6 ==> Created
C
C  REFERENCES
C     1. Gallagher, D. L., P. D. Craven, and R. H. Comfort, An empirical model
C        of the earth's plasmasphere, Adv. Space Res., 8, 15-24, 1988.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     A1     Real       log10(cm-3)     The factor a1
C     A2     Real       n/a             The factor a2
C     A3     Real       n/a             The factor a3
C     A4     Real       n/a             The factor a4
C     A5     Real       km              The factor a5
C     A7     Real       n/a             The factor a7
C     RE     Real       km              The mean radius of the Earth
C
      REAL*8 RE,A1,A2,A3,A4,A5,A7
      PARAMETER(RE=6375.D0,A1=1.4D0,A2=1.53D0,A3=-.036D0,A4=30.76D0,
     &     A5=159.9D0,A7=6.27D0)
C
C  Input variables
C
      REAL LAMBDA,T,Z
C
C  Output variables
C
      REAL GALHPL
C
C  Local variables
C
C      REAL PI,DTOR,HTOR,COSLAM,COSLA2,L,F,X,A6,G,A8,A9,A9M1,H
      REAL*8 COSLAM,COSLA2,L,F,X,A6,G,A8,A9,A9M1,H
C
C  Calculate pi
C
C      PI=4.*ATAN(1.)
C
C  Calculate the conversion factor from degrees to radians
C
C      DTOR=PI/180.
C
C  Calculate the conversion factor from hours to radians
C
C      HTOR=PI/12.
C     2005 Sep 01  James M Anderson  --JIVE  whoa! why calculate these 
C                                      every function call?  Use PARAMETER
      REAL*8 HTOR
      INCLUDE 'const.inc'
      PARAMETER(HTOR=PI/12.0d0)

C
C  Calculate the L-value
C  Note:To stabilize the Gallagher model at high latitudes, the calculated
C       L-value is restricted to values <= 12.5.  This limit is derived from an
C       upper limit of 1.E36 for the expression (L/A8)**(2*(A9-1)) in the H
C       term.  The limit (L/A8)**(2*(A9-1)) <= 1.E36 can be rewritten as
C       L <= A8*10**(36/(2*(A9-1))).  For the smallest possible value of A8
C       (3.7) and the largest possible value of A9 (35), L is 12.52, thus the
C       upper limit of 12.5 on L.
C
      COSLAM=COS(LAMBDA*DTOR)
      COSLA2=MAX(COSLAM*COSLAM,1.E-6)
      L=MIN(12.5,(1.+Z/RE)/COSLA2)
C
C  Calculate F(L-value,Lambda)
C
      F=A2-EXP(A3*(1.-A4*EXP(-Z/A5)))
C
C  Calculate X(T)
C
      X=T-12.
C
C  Calculate a6(T)
C
      A6=-.87+.12*EXP(-X*X/9.)
C
C  Calculate G(L-value,T)
C
      G=A6*L+A7
C
C  Calculate a8(T)
C
      A8=4.4+.7*COS((T-21.)*HTOR)
C
C  Calculate a9(T)
C
      A9=19.7+15.3*COS(T*HTOR)
C
C  Calculate H(L-value,T)
C
      A9M1=A9-1.
      H=(1.+(L/A8)**(2.*A9M1))**(-A9/A9M1)
C
C  Calculate the H+ concentration
C
      GALHPL=10.**(A1*F*G*H)
C
      RETURN
      END
