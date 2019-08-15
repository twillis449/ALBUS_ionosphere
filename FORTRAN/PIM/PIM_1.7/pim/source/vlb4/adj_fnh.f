      SUBROUTINE GET_FOS(NE,ALTE,DENE,NF,ALTF,DENF,HME,HMF2,FOE,FOF2,
     1 LAYR)
C
C  PURPOSE
C     To return the critical frequency and height of maximum density
C  for the E and F2 layers
C
C  METHOD
C     From the molecular ion altitude vs density grid, compute the
C  altitude of the maximum molecular ion density. Use this as the
C  height of the E layer. Similarly, use the O+ density for the height
C  of the F layer. The combine the two densities for the combined density
C  at the two points. Form the total density compute the critical frequencies
C
C
C  INPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    ALTE    REAL       (NE)   Altitude grid for the E layer
C    ALTF    REAL       (NF)   Altitude grid for the F2 layer
C    DENE    REAL       (NE)   Density grid for the E layer
C    DENF    REAL       (NF)   Density grid for the F2 layer
C    LAYR    CHARACTER*1       'E','F' or 'B' for which layer to compute
C    NE      INTEGER           Dimension for E layer grid
C    NF      INTEGER           Dimension for F2 layer grid
C
C  OUTPUT PARAMETERS
C    NAME    TYPE       ARRAY  description
C    FOE     REAL              Critical frequency of the E layer
C    FOF2    REAL              Critical frequency of the F2 layer
C    HME     REAL              Height of the maximum density of the E layer
C    HMF2    REAL              Height of the maximum density of the F2 layer
C
C  LOCAL VARIABLES
C    NAME    TYPE       ARRAY  description
C    EE1     REAL              Logarithmic density of the E layer at the
C                              given altitude (HME or HMF2)
C    EF1     REAL              Logarithmic density of the F2 layer at the
C                               given altitude (HME or HMF2)
C    NEI     INTEGER           Array index of highest altitude to look for
C                              an E layer peak
C
C  SUBROUTINES CALLED
C    NAME      description
C    FNDMAX    Finds the maximum density and altitude of maximum density
C    NEWGRID   Puts the density profiles onto a new altitude grid.
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
C     1.5   30-September-1996
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     W. Whartenby     4-May-1992   1.0.6 ==> Created
C     L. Brown        30-Sep-1996   1.0.6 ==> 1.5
C                                   Removed argument ERR from the call to
C                                   INFLECTION since it is no longer used by
C                                   that routine.
C                                   Removed local variable ERR since it is no
C                                   longer used.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     AFPE Conversion... FO = AFPE*SQRT(NMAX)
C
C
C     Parameter declarations
C
      REAL AFPE
      PARAMETER (AFPE= 8.98E-3)
C
C     I/O variable declarations
C
      INCLUDE 'logicuni.inc'
      INTEGER NF,NE
      REAL FOE,HME,FOF2,HMF2
      REAL ALTE(NE),DENE(NE),ALTF(NF),DENF(NF)
      CHARACTER*1 LAYR
C
C     Local variable declarations
C
      INTEGER NEI
      REAL EMAX,FMAX,EE1,EF1
C
      IF ((LAYR .EQ. 'E') .OR. (LAYR .EQ. 'B') .OR. (LAYR .EQ. 'F'))
     2 THEN
C
C      Valid LAYR call
C
      ELSE
       WRITE(LUSTDERR,*) LAYR
       STOP 'Invalid layer in GET_FOS'
      ENDIF
      IF ((LAYR .EQ. 'E') .OR. (LAYR .EQ. 'B')) THEN
       NEI = 1
       DO WHILE ( ALTE(NEI) .LE. 150.)
        NEI = NEI+1
       ENDDO
       IF (NEI .GT. 2) NEI = NEI-1
       CALL FNDMAX(NEI,ALTE,DENE,EMAX,HME)
       CALL NEWGRID(NEI,ALTE,DENE,1,HME,EE1)
       IF (HME .GE. ALTE(NEI)-1.) THEN
C
C       Maximum was found at the top of allowed altitude grid... call
C       INFLECTION for another try
C
        CALL INFLECTION(NEI,ALTE,DENE,EE1,HME)
       ENDIF
       IF (LAYR .EQ. 'B') THEN
        CALL NEWGRID(NF,ALTF,DENF,1,HME,EF1)
        FOE = AFPE*SQRT(EXP(EE1)+EXP(EF1))
       ELSE
        FOE = AFPE*SQRT(EXP(EE1))
       ENDIF
      ENDIF
      IF ((LAYR .EQ. 'F') .OR. (LAYR .EQ. 'B')) THEN
       CALL FNDMAX(NF,ALTF,DENF,FMAX,HMF2)
       CALL NEWGRID(NF,ALTF,DENF,1,HMF2,EF1)
       IF (LAYR .EQ. 'B') THEN
        CALL NEWGRID(NE,ALTE,DENE,1,HMF2,EE1)
        FOF2 = AFPE*SQRT(EXP(EE1)+EXP(EF1))
       ELSE
        FOF2 = AFPE*SQRT(EXP(EF1))
       ENDIF
      ENDIF
      RETURN
      END
C
      SUBROUTINE INFLECTION(NALT,ALT,DEN,DENM,HM)
C
C     PURPOSE
C      To find the height and density of the E layer maximum by local maximum,
C     or, failing that, inflection. This subroutine is to be used if the peak
C     finding algorithm FNDMAX cannot find a peak.
C      Note...THIS SUBROUTINE IS MEANT TO BE USED WITH DEN REPRESENTING THE
C     NATURAL LOGARITHM OF THE DENSITY. Understand and adjust the condition
C     for determining the inflection point if you want to use this for DEN
C     representing the base 10 log of the density, the actual density, or
C     any other representation.
C
C     METHOD
C      Determine where the change in the natural log of the density changes
C     by less than SLPMAX times the change in altitude (in Km). Use the average
C     of the two altitudes as the HmE. Return the input HM unchanged if the
C     subroutine cannot find an inflection point.
C
C     INPUT PARAMETERS
C     NAME     TYPE      ARRAY      description
C     ALT      REAL      (NALT)     Altitude vector
C     DEN      REAL      (NALT)     Density vector
C     NALT     INTEGER              Vector dimensions
C
C     OUTPUT PARAMETERS
C     NAME     TYPE      ARRAY      description
C     DENM     REAL                 Density at HM
C     HM       REAL                 Estimated height of local maximum
C                                   or inflection point.
C
C     LOCAL VARIABLES
C     NAME     TYPE      ARRAY      description
C     IL       INTEGER              Do loop index
C     ILSV     INTEGER              Vector element of bottom of inflection
C                                   point
C     NTRY     INTEGER              Number of iterations
C     NMAX     INTEGER              Number of iterations to try
C     SLP      REAL                 Slope of density-altitude plot...note
C                                   this slope considers density on the Y
C                                   axis...different from convention.
C     TEST                          .TRUE. once slope of two neighboring
C                                   points is greater than SLPMAX...
C                                   avoids spurious low HmE when lowest
C                                   altitude density points are nearly
C                                   equal
C
C     SUBROUTINES CALLED
C     None
C
C     FUNCTIONS CALLED
C     None
C
C     AUTHOR
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
C     W. Whartenby                  Created
C     L. Brown        30-Sep-1996   1.5
C                                   Removed input argument ERR since it is not
C                                   used.
C                                   Changed declaration of local variable TEST
C                                   from LOGICAL*1 to LOGICAL.
C
C     REFERENCES
C     None
C
C     SPECIAL CONSTANTS
C     NAME     Description
C     SLPMAX   Slope must be less than  SLPMAX for an inflection point
C              For natural log, SLPMAX = 0.1 means that the higher density
C              divided by the altitude difference is less than 1.105 times
C              the lower density divided by the altitude difference.
C
C
C     Parameter declarations
C
      REAL SLPMAX
      PARAMETER (SLPMAX=0.1)
C
C     I/O variable declarations
C
      INTEGER NALT
      REAL ALT(NALT),DEN(NALT),DENM,HM
C
C     Local variable declarations
C
      INTEGER IL,ILSV,NTRY,NMAX
      REAL SLP
      LOGICAL TEST
      DATA NMAX/6/
C
C     Initialize variables
C
      ILSV = 0
C
C     Search for local maximum
C
      DO IL = 2,NALT-1
       IF (DEN(IL) .GT. 2) THEN
C
C       Density is high enough to avoid 'noise' problems. Check for
C       local maximum
C
        IF (DEN(IL) .GT. MAX(DEN(IL+1),DEN(IL-1))) THEN
C
C        Determine if this is the only local maximum so far
C
         IF (ILSV .NE. 0) THEN
C         Always choose the greatest local maximum.
          IF (DEN(IL) .GT. DEN(ILSV)) ILSV = IL
         ELSE
C         First local maximum
          ILSV = IL
         ENDIF
        ENDIF
       ENDIF
      ENDDO
      IF (ILSV .NE. 0) THEN
C
C      We found a local maximum
C
       DENM = DEN(ILSV)
       HM = ALT(ILSV)
      ELSE
C
C      Search for inflection point
C
       TEST = .FALSE.
       NTRY = 0
       DO WHILE ((NTRY .LE. NMAX) .AND. (.NOT. TEST))
C       Increment NTRY. If first pass found no place where function showed
C       ' signifigant increase ', then lower standards...find something
C       like an inflection point
        NTRY = NTRY+1
        DO IL = 1,NALT-1
C
         IF (DEN(IL) .GT. 2) THEN
          SLP = (DEN(IL+1)-DEN(IL))/(ALT(IL+1)-ALT(IL))
C         ' Turn on' search for inflection point once the function has shown
C         a signifigant increase
          IF (SLP .GT. SLPMAX/FLOAT(NTRY)) TEST = .TRUE.
          IF (TEST) THEN
C
C          Check for inflection point...choose lowest inflection point
C
           IF ((SLP .LT. SLPMAX/FLOAT(NTRY)).AND.(ILSV .EQ. 0)) ILSV=IL
          ENDIF
         ENDIF
        ENDDO
       ENDDO
       IF (ILSV .NE. 0) THEN
        IF (ILSV .LT. NALT-1) ILSV = ILSV+1
        DENM = DEN(ILSV)
        HM = ALT(ILSV)
       ELSE
C       If no inflection point found, do nothing
       ENDIF
      ENDIF
      RETURN
      END
C
      SUBROUTINE MERGE4(MALT,NF10P7,NKP,NALT,TF10P7,OMTF10P7,TKP,OMTKP,
     &                  ALT,DEN,NMAX1,HMAX1,OUTDEN)
C
C     PURPOSE
C       To merge the input profiles into one
C
C     METHOD
C       First, give all 4 profiles the same maximum density and altitude
C     Then, use NEWGRID to fit each point over the F10.7s. Finally, use
C     NEWGRID to fit over KP. Finally, take the output profile and scale
C     it to have the correct NMAX and HMAX
C
C     INPUT PARAMETERS
C     NAME     TYPE     ARRAY      Description
C     ALT      REAL    (NALT,4)    Alititude points
C     DEN      REAL    (NALT,4)    Input densities
C     HMAX1    REAL     (4)        Height of maximum density
C     NALT     REAL     (4)        Number of altitude points
C     NF10P7   INTEGER             Array dimension
C     NKP      INTEGER             Array dimension
C     NMAX1    REAL     (4)        Log of number density at maximum
C     OMTF10P7 REAL                1.-TF10P7
C     OMTKP    REAL                1.-TKP
C     TF10P7   REAL                Linear interpolation factor for F10.7
C     TKP      REAL                Linear interpolation factor for Kp
C
C     OUTPUT PARAMETERS
C     NAME     TYPE     ARRAY      Description
C     OUTDEN   REAL     (MALT)     Merged density
C
C     LOCAL VARIABLES
C     NAME     TYPE     ARRAY      Description
C     HMAX1    REAL                Height of maximum density
C     NMAX1    REAL                Log of number density at maximum
C
C     SUBROUTINES CALLED
C     NAME     Description
C     NEWGRID  Uses polynomial interpolation to put a function on a new grid
C
C     AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A.
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        27-Apr-1995   1.2
C                                   Renamed from MERGE9 to MERGE4.
C                                   A 2 x 2 (F10.7 x Kp) profile matrix is
C                                   now merged instead of a 3 x 3 profile
C                                   matrix.
C     L. Brown        25-Sep-1995   1.2 ==> 1.3
C                                   The actual values of the peak density and
C                                   density profile are now interpolated in
C                                   F10.7 and Kp instead of their logarithms.
C     2005 Sep 05  James M Anderson  --JIVE  bump up array sizes
C
C     REFERENCES
C
C     SPECIAL CONSTANTS
C
C
      include 'array.inc'
      INTEGER NF10P7,NKP,NALT(4),I,J,K,L,NREC,NLT,MALT,LT
      REAL TF10P7,OMTF10P7,TKP,OMTKP,ALT(MALT,4),DEN(MALT,4)
      REAL OUTDEN(MALT),DUM(MAX_ALT_NUM),NMAX1(4),HMAX1(4)
      REAL DMDEN(MAX_ALT_NUM,2)
      REAL NMAX, HMAX,HMX(2),NMX(2),HMX1(2),NMX1(2)
C
C
      DO I=1,NKP
       DO J = 1,NF10P7
        NREC = J + NF10P7*(I-1)
        NLT = NALT(NREC)
        NMX1(J) = EXP(NMAX1(NREC))
        HMX1(J) = HMAX1(NREC)
C
C       Put this density vector to NMAX and HMAX
C
        DO K = 1,NLT
         DUM(K) = DEN(K,NREC)
        ENDDO
        CALL SCALEP(NLT,ALT(1,NREC),DUM,NMAX1(NREC),HMAX1(NREC),
     1  DEN(1,NREC))
C
C
C      Fit the data over F10P7
C
       ENDDO
C
C      Fit NMAX and HMAX over F10.7
C
       CALL LINR(NMX(I),NMX1,TF10P7,OMTF10P7)
       CALL LINR(HMX(I),HMX1,TF10P7,OMTF10P7)
       DO K = 1,NF10P7
        LT = NF10P7*(I-1)+K
        DO L = 1,NLT
         DUM(L) = DEN(L,LT)
        ENDDO
        CALL SCALEP(NLT,ALT(1,LT),DUM,LOG(NMX(I)),HMX(I),DEN(1,LT))
       ENDDO
       DO L = 1,NLT
        DO K = 1,NF10P7
         DUM(K) = EXP(DEN(L,NF10P7*(I-1)+K))
        ENDDO
        CALL LINR(OUTDEN(L),DUM,TF10P7,OMTF10P7)
        OUTDEN(L)=LOG(MAX(1.E-35,OUTDEN(L)))
       ENDDO
C
C      Fit data over Kp
C
       DO K = 1,NLT
        DMDEN(K,I) = OUTDEN(K)
       ENDDO
      ENDDO
C
C     Fit NMAX and HMAX over Kp
C
      CALL LINR(NMAX,NMX,TKP,OMTKP)
      CALL LINR(HMAX,HMX,TKP,OMTKP)
      DO I = 1,NKP
       DO J = 1,NLT
        DUM(J) = DMDEN(J,I)
       ENDDO
       CALL SCALEP(NLT,ALT(1,1),DUM,LOG(NMAX),HMAX,DMDEN(1,I))
      ENDDO
      DO J = 1,NLT
       DO I = 1,NKP
        DUM(I) = EXP(DMDEN(J,I))
       ENDDO
       CALL LINR(DMDEN(J,1),DUM,TKP,OMTKP)
       DMDEN(J,1)=LOG(MAX(1.E-35,DMDEN(J,1)))
      ENDDO
C
C     Scale the final array to the critical frequency and height
C
      CALL SCALEP(NLT,ALT,DMDEN,LOG(NMAX),HMAX,OUTDEN)
      RETURN
      END
C
      FUNCTION CCRFOE(DAY,UT,GLAT,GLON,F10P7)
C
C  PURPOSE
C     To calculate foE by CCIR recommendation.
C
C  METHOD
C     The E-layer critical frequency foE is calculated from the CCIR
C     recommendation.  The CCIR recommendation is an empirical model of monthly
C     median foE.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     DAY    Integer    Scalar        Days            1 <= DAY <= 366
C        Day of the year
C     F10P7  Real       Scalar        10**22 *        >= 0.
C                                     W m-2 Hz-1
C        Ottawa 2800 MHz (10.7 cm) solar flux
C     GLAT   Real       Scalar        deg N           -90. <= GLAT <= 90.
C        Geographic latitude
C     GLON   Real       Scalar        deg E           0. <= GLON < 360.
C        Geographic longitude
C     UT     Real       Scalar        hr              0. <= UT < 24.
C        Universal Time
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CCRFOE Real       Scalar        MHz             >= 0.
C        The E-layer critical frequency
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ABSGLA Real       Scalar        deg             0. <= ABSGLA <= 90.
C        The absolute value of the geographic latitude
C     CGLAT  Real       Scalar        n/a             0. <= CGLAT <= 1.
C        The cosine of the geographic latitude
C     CXNOON Real       Scalar        n/a             -1. <= CXNOON <= 1.
C        The cosine of the solar zenith angle at solar local noon
C     DTOR   Real       Scalar        deg Radian-1    pi/180.
C        The conversion factor from degrees to radians
C     FF10P7 Real       Scalar        n/a             n/a
C        The F10.7 factor
C     FGLAT  Real       Scalar        n/a             n/a
C        The geographic latitude factor
C     FSZA   Real       Scalar        n/a             n/a
C        The solar zenith angle factor
C     FTOD   Real       Scalar        n/a             n/a
C        The time of day factor
C     P      Real       Scalar        n/a             n/a
C        A constant for the time of day factor
C     SLT    Real       Scalar        hr              0. <= SLT < 24.
C        The solar local time
C     SSN    Real       Scalar        n/a             >= 0.
C        The sunspot number
C     TDAWN  Real       Scalar        hr              0. <= TDAWN < 24.
C        The solar local time of dawn
C     TDUSK  Real       Scalar        hr              0. <= TDUSK < 24.
C        The solar local time of dusk
C     XPRIME Real       Scalar        deg             0. <= XPRIME <= 180.
C        A solar zenith angle used for the time of day factor
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     SOLLDY Real       Calculates the length of the day
C     SOLSLT Real       Calculates solar local time
C     SOLSZA Real       Calculates solar zenith angle
C     SOLUT  Real       Calculates Universal Time
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
C     L. Brown       30-Sep-1996  1.5 ==> Created
C     L. Brown       14-Feb-1997  1.5 ==> 1.6
C                                 Corrected comment in METHOD comment section
C                                 by changing "montly" to "monthly".
C
C  REFERENCES
C     1. "Supplement to Report 252-2: Second CCIR Computer-Based Interim Method
C        for Estimating Sky-Wave Field Strength and Transmission Loss at
C        Frequencies Between 2 and 30 MHz (Study Programme 30A/6), XIVth
C        Plenary Assembly, Kyoto, 1978", CCIR (International Radio Consultative
C        Committee), International Telecommunication Union, Geneva, 1980.
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER DAY
      REAL UT,GLAT,GLON,F10P7
C
C  Output variables
C
      REAL CCRFOE
C
C  Local variables
C
      REAL ABSGLA,CGLAT,FF10P7,CXNOON,FSZA,FGLAT,XPRIME,P,FTOD,SLT,
     &     TDAWN,TDUSK,SSN
C
C  Function declarations
C
      REAL SOLSZA,SOLUT,SOLSLT,SOLLDY
C
C  Calculate the conversion factor from degrees to radians
C
      INCLUDE 'const.inc'
C      DTOR=ATAN(1.)/45.
C
C  Calculate the absolute value of the geographic latitude
C
      ABSGLA=ABS(GLAT)
C
C  Calculate the cosine of the geographic latitude
C
      CGLAT=COS(GLAT*DTOR)
C
C  Calculate the F10.7 factor for the CCIR foE recommendation
C
      FF10P7=1.+.0094*(F10P7-66.)
C
C  Calculate the solar zenith angle factor for the CCIR foE recommendation
C
      CXNOON=COS(SOLSZA(DAY,SOLUT(12.,GLON),GLAT,GLON,0.)*DTOR)
      IF(CXNOON .LE. 0.) THEN
         FSZA=0.
      ELSE
         IF(ABSGLA .LT. 32.) THEN
            FSZA=CXNOON**(-1.93+1.92*CGLAT)
         ELSE
            FSZA=CXNOON**(.11-.49*CGLAT)
         ENDIF
      ENDIF
C
C  Calculate the geographic latitude factor for the CCIR foE recommendation
C
      IF(ABSGLA .LT. 32.) THEN
         FGLAT=23.+116.*CGLAT
      ELSE
         FGLAT=92.+35.*CGLAT
      ENDIF
C
C  Calculate the time of day factor for the CCIR foE recommendation
C
      IF(ABSGLA .LE. 23.) THEN
         XPRIME=SOLSZA(DAY,UT,GLAT,GLON,0.)
      ELSE
         XPRIME=SOLSZA(DAY,MOD(24.+MOD(UT-.05,24.),24.),GLAT,GLON,0.)
      ENDIF
      IF(ABSGLA .LE. 12.) THEN
         P=1.31
      ELSE
         P=1.2
      ENDIF
      IF(XPRIME .LE. 73.) THEN
         FTOD=COS(XPRIME*DTOR)**P
      ELSE IF(XPRIME .LT. 90.) THEN
         FTOD=COS((XPRIME-6.27E-13*(XPRIME-50.)**8)*DTOR)**P
      ELSE
         SLT=SOLSLT(UT,GLON)
         IF(SLT .LE. 12.) THEN
            TDAWN=12.-SOLLDY(DAY,UT,GLAT,0.)/2.
            FTOD=(.077**P)*EXP(-1.68*MAX(0.,TDAWN-SLT))
         ELSE
            TDUSK=12.+SOLLDY(DAY,UT,GLAT,0.)/2.
            FTOD=(.077**P)*EXP(-1.01*MAX(0.,SLT-TDUSK))
         ENDIF
      ENDIF
C
C  Calculate the CCIR foE recommendation
C
      SSN=MAX(0.,SQRT(93918.4+1117.3*F10P7)-406.37)
      CCRFOE=(MAX(.017*(1.+.0098*SSN)**2,FF10P7*FSZA*FGLAT*FTOD))**.25
C
      RETURN
      END
