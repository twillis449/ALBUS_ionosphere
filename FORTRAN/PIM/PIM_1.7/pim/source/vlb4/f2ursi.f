      SUBROUTINE F2URSI(IYEAR,IMONTH,IDAY,IUT,SSN,GLAT,GLON,
     $                  GMLAT,GMLON,GMLT,FOF2,HMAX)
C***********************************************************************
C* SUBROUTINE NAME:  F2URSI       CPC:                                 *
C*         VERSION:  1.0         DATE:  2 Oct 89                       *
C*                                                                     *
C*         PURPOSE:  This subroutine calculates foF2 and HMAX using    *
C*                   the CCIR method and 1988 URSI coefficients.       *
C*                                                                     *
C*         CALLING                                                     *
C*       ARGUMENTS:  IYEAR,IMONTH,IDAY,IUT,SSN,GLAT,GLON,        *
C*                   GMLAT,GMLON,GMLT,FOF2,HMAX                        *
C*                                                                     *
C*   ABBREVIATIONS:  NONE                                              *
C*                                                                     *
C*   COMMON BLOCKS:  F6PARM                                            *
C*                                                                     *
C*  FILES ACCESSED:  NONE                                              *
C*                                                                     *
C*          METHOD:  1. STORE THE LOCATION PARAMETERS AND INITIALIZE   *
C*                   2. CALCULATE THE F2 LAYER AT THE LOCATION         *
C*                                                                     *
C*        REMARKS:  DEFINITIONS -                                      *
C*                   FOF2 - THE FUNDAMENTAL FREQUENCY OF THE IONO-     *
C*                          SPHERIC F1 LAYER                           *
C*                   URSI  - CLIMATOLOGICAL REFERENCE IONOSPHERE FROM  *
C*                          URSI 1988                                  *
C*                   M3000 - FREQUENCY WHICH PROPAGATES BEST OVER A    *
C*                          3000 KM PATH IN THE SPECIFIED IONOSPHERE   *
C*                   EFFECTIVE SUNSPOT NUMBER - THE SUNSPOT NUMBER     *
C*                          THAT WOULD HAVE BEEN RESPONSIBLE FOR THE   *
C*                          CURRENT STATE OF THE IONOSPHERE            *
C*                                                                     *
C*     REFERENCES:   1. ICED-III SYSTEM DOCUMENTATION                  *
C*                                                                     *
C* GLOBAL VARIABLES :                                                  *
C*                                                                     *
C*  LOCAL VARIABLES :                                                  *
C*     IDAY   IS    THE DAY OF THE MONTH                               *
C*     IMONTH IS    THE MONTH OF THE YEAR                              *
C*     ITIME  IS    TIME HHMM OF DATA                                  *
C*     IUT    IS    THE UNIVERSAL TIME, IN HHMM                        *
C*     IYEAR  IS    THE YEAR                                           *
C*     JDAY   IV    NUMBER OF DAYS CUMULATIVE IN MONTHS                *
C*     FOF2   RS    FREQUENCY OF F2 LAYER IN IONOSPHERE                *
C*     GLAT   RS    THE GEOGRAPHIC LATITUDE OF THE LOCATION            *
C*     GLON   RS    THE GEOGRAPHIC LONGITUDE OF THE LOCATION           *
C*     GMLAT  RS    THE CORRECTED GEOMAGNETIC LATITUDE OF THE LOCATION *
C*     GMLON  RS    THE CORRECTED GEOMAGNETIC LONGITUDE OF THE LOCATION*
C*     GMLT   RS    THE CORRECTED GEOMAGNETIC LOCAL TIME OF THE LOCAT'N*
C*     HMAX   RS    HEIGHT IN KM OF F2 LAYER IN IONOSPHERE             *
C*     SSN    RS    THE EFFECTIVE SUNSPOT NUMBER                       *
C* (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR,  A=ARRAY)      *
C*                                                                     *
C*   SETC OPTIONS:  NONE USED                                          *
C*                                                                     *
C*     SUBPROGRAMS                                                     *
C*         CALLED:  URSIDA - TO READ IN THE URSI COEFFICIENTS          *
C*                           from a direct access file                 *
C*                  F2LAYR - TO CALCULATE THE FREQ/HGT OF F2 LAYER     *
C*                                                                     *
C*PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER          *
C*        UPDATES:  MAY 86 - AFGWC/SDDE - UPDATED DOCUMENTATION        *
C*                  AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)   *
C*                  MAR 88 - NATIONAL GEOPHYSICAL DATA CENTER (88-II)  *
C*                  MAY 89 - MADE INTO SUBROUTINE BY L. BROWN          *
C*                  AUG 89 - CORRECTIONS BY M. REILLY MADE BY L. BROWN *
C*                  OCT 89 - MODIFIED BY R. E. DANIELL TO ELLIMINATE   *
C*                           F1 AND E LAYER CALLS, TO ELLIMINATE       *
C*                           Q REFERENCES AND ELKINS-RUSH CORRECTIONS  *
C*                                                                     *
C***********************************************************************
      INCLUDE 'f6parm.inc'
      INTEGER IDAY,IMONTH,ITIME,IUT,IYEAR,JDAY(12)
      REAL FOF2
      REAL GLAT,GLON,GMLAT,GMLON,GMLT,HMAX,SSN
C
      DATA JDAY/1,32,60,91,121,152,182,213,244,274,305,335/
C......................................................................
C     Store input parameters                                          .
C......................................................................
      IYR=IYEAR
      IMO=IMONTH
      IDA=IDAY
      ITIME=IUT
      EFFSSN=SSN
      GGLAT=GLAT
      GGLON=GLON
      CGLAT=GMLAT
      CGLON=GMLON
      TCGM=GMLT
C.......................................................................
C     sun/earth position parameters                                    .
C.......................................................................
      TIME = ITIME/100 + MOD(ITIME,100)/60.
      DAYJUL = JDAY(IMO) + IDA - 1
C......................................................................
C     Read in the URSI coefficients using the direct access version   .
C        of URSI:  URSIDA                                             .
C......................................................................
      CALL URSIDA(EFFSSN)
C.....................................................................
C     Calculate all frequencies and heights for the location         .
C.....................................................................
C
      CALL F2LAYR(FOF2,HMAX)
C
      RETURN
      END
      SUBROUTINE ABSICO(LH,ASTAR,BSTAR,COTIME,SITIME,OMEGA)
C***********************************************************************
C* SUBROUTINE NAME :  ABSICO            CPC:  SS/MOD/RTN/ABSICO        *
C*          VERSION:  88-II            DATE:  16 MAR 88                *
C*                                                                     *
C*          PURPOSE:  THIS SUBROUTINE COMPUTES THE VALUE OF FOF2/M3000 *
C*                    FROM THE SUM OF THE FOURIER COEFFICIENT TERMS OF *
C*                    ASTAR, BSTAR, COTIME, AND SITIME.                *
C*                                                                     *
C*          CALLING                                                    *
C*        ARGUMENTS:  LH, ASTAR, BSTAR, COTIME, SITIME, OMEGA          *
C*                                                                     *
C*    ABBREVIATIONS:  NONE                                             *
C*                                                                     *
C*    COMMON BLOCKS:  NONE ACCESSED                                    *
C*                                                                     *
C*   FILES ACCESSED:  NONE                                             *
C*                                                                     *
C*           METHOD:  1.  SUMS THE SEVEN FOURIER COEFFICIENT TERMS IN  *
C*                        THE SERIES.                                  *
C*                                                                     *
C*          REMARKS:  NONE                                             *
C*                                                                     *
C*       REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                *
C*                    2.  SPERRY FORTRAN-77 (ASCII) PRM                *
C*                    3.  AFGWC FORM 10 DX-50194                       *
C*                    4.  AFGWC FORM 10 DX-60096                       *
C*                                                                     *
C*  LOCAL VARIABLES:  J        IS  LOOP INDEX                          *
C*                    LH       IS  LIMIT ON SUM (=6)                   *
C*                    OMEGA    RS  FOF2 OR M3000 VALUE                 *
C*                    ASTAR(8) RV  FIRST FOURIER SERIES COEFFICIENT    *
C*                    BSTAR(8) RV  SECOND FOURIER SERIES COEFFICIENT   *
C*                    COTIME(8)RV  COSINE EXPANSION OF TIME            *
C*                    SITIME(8)RV  SINE EXPANSION OF TIME              *
C*  (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)      *
C*                                                                     *
C*     SETC OPTIONS:  NONE USED                                        *
C*                                                                     *
C*       SUBPROGRAMS                                                   *
C*           CALLED:  NONE                                             *
C*                                                                     *
C*       SUBPROGRAM                                                    *
C*       CALLED BY :  FLDHR                                            *
C*                                                                     *
C*                                                                     *
C* PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER         *
C*                   MAY 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION      *
C*                   AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)  *
C*                   MAR 88 - NGDC (88-II) DOCUMENTATION REQUIREMENTS  *
C*                                                                     *
C***********************************************************************
      INTEGER J
      INTEGER LH
      REAL OMEGA
      REAL*8 OMEGA_TEMP
      REAL ASTAR(8)
      REAL BSTAR(8)
      REAL COTIME(8)
      REAL SITIME(8)
C
      OMEGA_TEMP=ASTAR(1)
      DO 5 J=1,LH
         OMEGA_TEMP=OMEGA_TEMP
     &        +(ASTAR(J+1)*COTIME(J))+(BSTAR(J+1)*SITIME(J))
    5 CONTINUE
      OMEGA=OMEGA_TEMP
C
      RETURN
      END
      SUBROUTINE AJBJ(LH,MX,U,G,ASTAR,BSTAR)
C***********************************************************************
C* SUBROUTINE NAME :  AJBJ              CPC:  SS/MOD/RTN/AJBJ          *
C*          VERSION:  88-II            DATE:  16 MAR 88                *
C*                                                                     *
C*          PURPOSE:  THIS SUBROUTINE COMPUTES THE FOURIER COEFFICIENTS*
C*                    FOR A FIXED GEOGRAPHIC POINT                     *
C*                                                                     *
C*          CALLING                                                    *
C*        ARGUMENTS:  LH, MX, U, G, ASTAR, BSTAR                       *
C*                                                                     *
C*    ABBREVIATIONS:  NONE                                             *
C*                                                                     *
C*    COMMON BLOCKS:  NONE ACCESSED                                    *
C*                                                                     *
C*   FILES ACCESSED:  NONE                                             *
C*                                                                     *
C*           METHOD:  1.  THIS SUBROUTINE IS PART OF THE BENT MODEL,   *
C*                        GIVEN TO AFGWC BY NASA DURING THE 1970'S.    *
C*                        IT WAS BORROWED BY NGDC FOR THE ICED MODEL   *
C*                        AND NOW IT'S COME BACK TO HAUNT US.  THE     *
C*                        ASTAR AND BSTAR ARRAYS ARE USED IN CONVERTING*
C*                        THE FOURIER COEFFICIENTS INTO A REAL VALUE   *
C*                        OF FOF2 OR M-FACTOR.  ONLY AN ANALYST WHO    *
C*                        HAS FAMILIARITY WITH FOURIER COEFFICIENTS    *
C*                        WILL HAVE THE ABILITY TO UNDERSTAND WHAT THE *
C*                        SUBROUTINE DOES.                             *
C*           METHOD:  1.  SUMS THE SEVEN FOURIER COEFFICIENT TERMS IN  *
C*                        THE SERIES.                                  *
C*                                                                     *
C*          REMARKS:  NONE                                             *
C*                                                                     *
C*       REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                *
C*                    2.  SPERRY FORTRAN-77 (ASCII) PRM                *
C*                    3.  AFGWC FORM 10 DX-50194                       *
C*                    4.  AFGWC FORM 10 DX-60096                       *
C*                                                                     *
C*  LOCAL VARIABLES:  J        IS  LOOP INDEX                          *
C*                    K        IS  LOOP INDEX                          *
C*                    LH       IS  NUMBER OF ITERATIONS FOR ASTAR/BSTAR*
C*                    MX       IS  NUMBER OF COEFFICIENTS IN G(76) AND *
C*                                 U(I,MX) ARRAYS (76)                 *
C*                    N        IS  NUMBER OF ITERATIONS PLUS ONE       *
C*                    ASTAR(8) RV  FIRST FOURIER SERIES COEFFICIENT    *
C*                    BSTAR(8) RS  SECOND FOURIER SERIES COEFFICIENT   *
C*                    G(76)    RV  GEOGRAPHIC COEFFICIENT ARRAY        *
C*                    U(13,76) RV  URSI COEFFICIENT SET FOR FOF2, MFAC *
C*  (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)      *
C*                                                                     *
C*     SETC OPTIONS:  NONE USED                                        *
C*                                                                     *
C*       SUBPROGRAMS                                                   *
C*           CALLED:  NONE                                             *
C*                                                                     *
C*       SUBPROGRAM                                                    *
C*       CALLED BY :  FLDHR                                            *
C*                                                                     *
C*                                                                     *
C* PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER         *
C*                   MAY 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION      *
C*                   AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)  *
C*                   MAR 88 - NATIONAL GEOPHYSICAL DATA CENTER (88-II) *
C*                                                                     *
C*********************************************************************
      INTEGER J
      INTEGER K
      INTEGER LH
      INTEGER MX
      INTEGER N
      REAL ASTAR(8)
      REAL BSTAR(8)
      REAL G(76)
      REAL U(13,76)
C
      N=LH+1
C......................................................................
C     FIRST CLEAR THE COEFFICIENT ASTAR                               .
C......................................................................
      J=1
      ASTAR(J) = 0.
C......................................................................
C       NOW FILL THE ARRAY BY SUMMATION OVER THE 76 TERMS             .
C......................................................................
      DO 40 K=1,MX
         ASTAR(J)=ASTAR(J)+U(2*J-1,K)*G(K)
 40   CONTINUE
C......................................................................
C     CLEAR THE BSTAR COEFFICIENT ARRAY                               .
C......................................................................
      DO 70 J=2,N
         ASTAR(J) = 0.
         BSTAR(J)=0.
C......................................................................
C       NOW FILL BSTAR ARRAY WITH THE SUMMATION OVER 76 TERMS         .
C......................................................................
         DO 60 K=1,MX
            ASTAR(J)=ASTAR(J)+U(2*J-1,K)*G(K)
            BSTAR(J)=BSTAR(J)+U(2*J-2,K)*G(K)
 60      CONTINUE
 70   CONTINUE
C
      RETURN
      END
      SUBROUTINE F2LAYR(FOF2,HMAX)
C***********************************************************************
C* SUBROUTINE NAME :  F2LAYR            CPC:  SS/MOD/RTN/F2LAYR        *
C*          VERSION:  88-II            DATE:  16 MAR 88                *
C*                                                                     *
C*          PURPOSE:  THIS SUBROUTINE IS THE UNMODIFIED VERSION        *
C*                    OF THE URSI MODEL.                               *
C*                                                                     *
C*          CALLING                                                    *
C*        ARGUMENTS:  FOF2,HMAX                                        *
C*                                                                     *
C*    ABBREVIATIONS:  NONE                                             *
C*                                                                     *
C*    COMMON BLOCKS:  F6PARM, GWC1                                     *
C*                                                                     *
C*   FILES ACCESSED:  NONE                                             *
C*                                                                     *
C*           METHOD:  1.  .  CALCULATE THE FOF2 AND HMAX AT THE LOCATION  *
C*                                                                     *
C*          REMARKS:  NONE                                             *
C*                                                                     *
C*       REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                *
C*                    2.  SPERRY FORTRAN-77 (ASCII) PRM                *
C*                    3.  AFGWC FORM 10 DX-50194                       *
C*                    4.  AFGWC FORM 10 DX-60096                       *
C*                                                                     *
C*  LOCAL VARIABLES:  CHI      RS  SOLAR ZENITH ANGLE                  *
C*                    DELTAN   RS  SMALL INCREMENT OF TIME             *
C*                    EB       RS  EQUATORIAL BOUNDARY VALUE           *
C*  XXX               EQBN     RS  EQUATORIAL BOUNDARY VALUE           *
C*  XXX               EQLAT    RS  EQUATORIAL BOUNDARY LATITUDE VALUE  *
C*  XXX               EQLON    RS  EQUATORIAL BOUNDARY LONGITUDE VALUE *
C*  XXX               FACME    RS  FOURIER ANALYSIS COEFFICIENT - EQ.  *
C*  XXX               FACMP    RS  FOURIER ANALYSIS COEFFICIENT - POLAR*
C*                    FOF      RS  TEMP VALUE OF FREQUENCY OF F2 LAYER *
C*                    FOF2     RS  FREQUENCY OF THE FOF2 LAYER         *
C*  XXX               FRAC     RS  FRACTIONAL PART - TEMP VARIABLE     *
C*                    HMAX     RS  HEIGHT OF THE F2 LAYER              *
C*  XXX               HMAXC    RS  HEIGHT OF THE F2 LAYER - CENTER     *
C*  XXX               HMAXE    RS  HEIGHT OF THE F2 LAYER - EQUATOR    *
C*  XXX               HMAXP    RS  HEIGHT OF THE F2 LAYER - POLAR      *
C*                    PB       RS  POLAR BOUNDARY OF AURORAL ZONE      *
C*  XXX               PHIMIN   RS  TEMP CORRECTION VALUE OF MINIMUM    *
C*  XXX               PHIPLS   RS  TEMP CORRECTION VALUE - POLAR       *
C*                    PI       RS  3.1415926 - CONSTANT                *
C*  XXX               POLAT    RS  POLEWARD LATITUDE                   *
C*  XXX               POLON    RS  POLEWARD LONGITUDE                  *
C*                    TCGM     RS  CORRECTED GEOMAGNETIC TIME          *
C* M3000  - RS - TEMPORARY M(3000) FACTOR                              *
C* PT1    - RS - TEMP. BOTTOM F2 HEIGHT IN LONGITUDE INTERPOLATION BOX *X
C* PT2    - RS - TEMP. TOP F2 HEIGHT IN LONGITUDE INTERPOLATION BOX    *
C* F      - RS - M3000 CALCULATION VARIABLE                            *
C* DELTAM - RS - M3000 CORRECTION TERM                                 *
C*  (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)      *
C*                                                                     *
C*     SETC OPTIONS:  NONE USED                                        *
C*                                                                     *
C*       SUBPROGRAMS                                                   *
C*           CALLED:  FLDHR  - RETRIEVES FOURIER ANALYSIS COEFFICIENTS *
C*                    CGINV1 - CONVERTS GEOMAG TO GEOGRAPHIC COORDS    *
C*                    SOLPOS - COMPUTES SOLAR ZENITH ANGLE             *
C*                                                                     *
C*       SUBPROGRAM                                                    *
C*       CALLED BY :  F2URSI                                           *
C*                                                                     *
C* PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER         *
C*                   MAY 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION      *
C*                   AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)  *
C*                   MAR 88 - NGDC (88-II) NEW SOLAR ZENITH ANGLE      *
C* PROGRAM MODIFIED: OCT 92 - CPI (Boston) William G. Whartenby        *
C*                            1) Changed common blocks F6PARM and GWC1 *
C*                            to INCLUDE statements                    *
C*                            Removed the following variables, as they *
C*                            are no longer used in the computations   *
C*                            TRFEB,TRFMAX,TRFPB... Trough boundary    *
C*                            CGLN21,CGLN03,CGLN06,CGLN21,GLAT,GLON    *
C*                            HMAXBL,HMAXBR,HMAXTL,HMAXTR,HFRM21       *
C*                            FRLAT,FRLON,PT1,PT2,CH1,DELTAN,EB,DUM1   *
C*                            FOF,PB,PHIA,T,T1,TIMLOC,TPC,X1,XA,T0     *
C*                   SEP 94 - CPI (Boston) Lincoln D. Brown            *
C*                            Include statement for math4_co.inc       *
C*                            deleted because its parameters are       *
C                             unused.                                  *
C*                                                                     *
C***********************************************************************
      INCLUDE 'f6parm.inc'
      INCLUDE 'gwc1.inc'
C
      REAL M3000
      REAL F,XE,DELTAM
      REAL FOF2,HMAX
C
C.......................................................................
C     BEGIN EXECUTION                                                  .
C
      CALL FLDHR(KF,UF,GGLAT,GGLON,FOF2,TIME)
C......................................................................
C     F2 HEIGHT CALCULATION                                           .
C......................................................................
      XE = 0.0
  170 CALL FLDHR(KX,UX,GGLAT,GGLON,M3000,TIME)
      F=M3000*SQRT((.0196*M3000*M3000+1)/(1.2967*M3000*M3000-1))
C----------  BY CCIR RECOMMENDATION
      IF(XE .LT. 1.7) XE=1.7
      DELTAM=(.253/(XE-1.215))-.012
      HMAX=(1490.*F)/(M3000+DELTAM)-176.
C
      RETURN
      END
      SUBROUTINE FLDHR(K,U,DLAT,DLON,VAL,TIME)
C***********************************************************************
C* SUBROUTINE NAME :  FLDHR             CPC:  SS/MOD/RTN/FLDHR         *
C*          VERSION:  88-II            DATE:  16 MAR 88                *
C*                                                                     *
C*          PURPOSE:  THIS SUBROUTINE SETS VARIABLES AND CALLS FIVE    *
C*                    SUBROUTINES.  ITS PURPOSE IS TO CALCULATE THE    *
C*                    FOURIER ANALYSIS COEFFICIENTS WHICH ARE USED TO  *
C*                    DEFINE THE FOF2 OR M3000 VALUE IN SUBROUTINE     *
C*                    F2LAYR.                                          *
C*                                                                     *
C*          CALLING                                                    *
C*        ARGUMENTS:  K, U, DLAT, DLON, VAL, TIME                      *
C*                    (ACTUAL ARGUMENTS VARY WITH CONTEXT OF CALL)     *
C*                                                                     *
C*    ABBREVIATIONS:  NONE                                             *
C*                                                                     *
C*    COMMON BLOCKS:  NONE ACCESSED                                    *
C*                                                                     *
C*   FILES ACCESSED:  NONE                                             *
C*                                                                     *
C*           METHOD:  1.  CALLS MAGFIN TO CALCULATE THE MAGNETIC FIELD *
C*                        COMPONENTS AT THE LOCATION DEFINED BY LAT    *
C*                        AND LON.                                     *
C*                    2.  CALLS SUBROUTINES GK AND AJBJ TO SELECT THE  *
C*                        FOURIER COEFFICIENT SET FOR THE LOCATION     *
C*                    3.  CALLS SUBROUTINES SICOJT AND ABSICO TO       *
C*                        CALCULATE THE VALUE OF FOF2 OR M3000 FROM THE*
C*                        COEFFICIENTS                                 *
C*                                                                     *
C*          REMARKS:  CALLED WITH DIFFERENT ARGUMENT NAMES DEPENDING   *
C*                    IF FOF2 OR M3000 COEFFICIENT COMPUTATION         *
C*                                                                     *
C*       REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                *
C*                    2.  SPERRY FORTRAN-77 (ASCII) PRM                *
C*                    3.  AFGWC FORM 10 DX-50194                       *
C*                    4.  AFGWC FORM 10 DX-60096                       *
C*                                                                     *
C*  LOCAL VARIABLES:  MX       IS  NUMBER OF CEOFFICIENTS TO SUM OVER  *
C*                    K(10)    IV  EITHER THE KF OR KX VECTOR FROM THE *
C*                                 URSI COEFFICIENT SET                *
C*                    DLAT     RS  LOCATION LATITUDE                   *
C*                    DLON     RS  LOCATION LONGITUDE                  *
C*                    DR       RS  DEGREE/RADIAN CONVERSION CONSTANT   *
C*                    RD       RS  RADIAN/DEGREE CONVERSION CONSTANT   *
C*                    T        RS  DEGREES LONGITUDE AT ANTISUBSOLAR PT*
C*                    TIME     RS  HOUR ANGLE AT THE ANTISUBSOLAR POINT*
C*                    TMP      RS  TEMP MAG FIELD CONPONENTS           *
C*                    VAL      RS  RETURN ARGUMENT = TO FOF2 OR M3000  *
C*                    ASTAR(8) RV  VECTOR OF FIRST FOURIER COEFFICIENTS*
C*                    BSTAR(8) RV  VECTOR OF SECOND FOURIER COEFFIC.   *
C*                    C(3)     RV  VECTOR OF TEMP ARRAY FOR GK         *
C*                    UNE(3)   RV  VECTOR OF GEOMAG COMPONENTS         *
C*                    COTIME(3)RV  VECTOR OF COSINES OF TIME           *
C*                    G(76)    RV  VECTOR OF COEFFICIENTS FROM URSI    *
C*                    POS(3)   RV  VECTOR OF LOCATION AND HGT          *
C*                                 OF AVERAGE F2 LAYER                 *
C*                    SITIME(8)RV  VECTOR OF SINES OF TIME             *
C*                    U(13,76) RA  ARRAY OF URSI COEFFICIENTS OF FOF2  *
C*                                 OR M3000                            *
C*  (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)      *
C*                                                                     *
C*     SETC OPTIONS:  NONE USED                                        *
C*                                                                     *
C*       SUBPROGRAMS                                                   *
C*           CALLED:  MAGFIN, GK, AJBJ, SICOJT, ABSICO                 *
C*                                                                     *
C*       SUBPROGRAM                                                    *
C*       CALLED BY :  F2LAYR                                           *
C*                                                                     *
C*                                                                     *
C* PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER         *
C*                   MAY 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION      *
C*                   AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)  *
C*                   MAR 88 - NGDC (88-II) DOCUMENTATION REQUIREMENTS  *
C*                                                                     *
C***********************************************************************
      INTEGER K(10)
      INTEGER MX
      REAL ASTAR(8)
      REAL BSTAR(8)
      REAL C(3)
      REAL COTIME(8)
      REAL DLAT
      REAL DLON
      REAL G(76)
      REAL POS(3)
      REAL SITIME(8)
      REAL T
      REAL TIME
      REAL TMP
      REAL VAL
      REAL UNE(3)
      REAL U(13,76)
C
      INCLUDE 'const.inc'
      DATA POS(3)/3.E5/
C......................................................................
C     BEGIN EXECUTION                                                 .
C......................................................................
      MX=K(9)+1
      POS(2)=DLON
      POS(1)=DLAT
C......................................................................
C     COMPUTE MAGNETIC FIELD COMPONENTS                               .
C......................................................................
      CALL MAGFIN(POS,UNE)
      TMP=(UNE(2)*UNE(2))+(UNE(3)*UNE(3))
      C(1)=RTOD*ATAN(ATAN(-UNE(1)/SQRT(TMP))/SQRT(COS(DTOR*POS(1))))
      C(2)=DLON
      C(3)=POS(1)
C.......................................................................
C     COMPUTE THE COORDINATE FUNCTION                                  .
C.......................................................................
      CALL GK(K,C,G)
      CALL AJBJ(K(10),MX,U,G,ASTAR,BSTAR)
      T=-180.+TIME*15.
C......................................................................
C     COMPUTE SINE AND COSINE OF TIME                                 .
C......................................................................
      CALL SICOJT(8,COTIME,SITIME,T)
      CALL ABSICO(K(10),ASTAR,BSTAR,COTIME,SITIME,VAL)
C
      RETURN
      END
      SUBROUTINE GK(K,C,G)
C***********************************************************************
C* SUBROUTINE NAME :  GK                CPC:  SS/MOD/RTN/GK            *
C*          VERSION:  88-II            DATE:  16 MAR 88                *
C*                                                                     *
C*          PURPOSE:  THIS SUBROUTINE COMPUTES THE COORDINATE FUNCTIONS*
C*                    G(I), WHERE I = 1...K+1, AND C(1) = MODIFIED LAT-*
C*                    ITUDE, C(2) = LONGITUDE AND C(3) = LATITUDE      *
C*                                                                     *
C*          CALLING                                                    *
C*        ARGUMENTS:  K, C, G                                          *
C*                                                                     *
C*    ABBREVIATIONS:  NONE                                             *
C*                                                                     *
C*    COMMON BLOCKS:  NONE ACCESSED                                    *
C*                                                                     *
C*   FILES ACCESSED:  NONE                                             *
C*                                                                     *
C*           METHOD:  1.  THIS SUBROUTINE WAS BORROWED FROM THE BENT   *
C*                    IONOSPHERIC MODEL DESIGNED BY NASA.  IT WAS      *
C*                    ORIGINALLY GIVEN TO AFGWC FOR RAYTRACING, BORR-  *
C*                    OWED BY NGDC FOR ICED, AND NOW HAS COME BACK TO  *
C*                    HAUNT US.  THE METHOD IS UNKNOWN, BUT THE DOCU-  *
C*                    MENTATION IS AS WAS FOUND IN THE NASA TR AD-772- *
C*                    733 FOR THE BENT MODEL.  ANY FURTHER KNOWLEDGE   *
C*                    IS DISAVOWED.                                    *
C*                                                                     *
C*          REMARKS:  NONE                                             *
C*                                                                     *
C*       REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                *
C*                    2.  SPERRY FORTRAN-77 (ASCII) PRM                *
C*                    3.  AFGWC FORM 10 DX-50194                       *
C*                    4.  AFGWC FORM 10 DX-60096                       *
C*                                                                     *
C*  LOCAL VARIABLES:  I        IS  INDEX                               *
C*                    J        IS  INDEX                               *
C*                    K(10)    IV  URSI COEFFICIENT VECTOR             *
C*                    KC       IS  UNKNOWN VARIABLE                    *
C*                    KDIF     IS  UNKNOWN VARIABLE                    *
C*                    KN       IS  UNKNOWN VARIABLE                    *
C*                    KO       IS  UNKNOWN VARIABLE                    *
C*                    N        IS  TOTAL NUMBER OR ORDERS              *
C*                    C(3)     RS  VECTOR OF LOCATION                  *
C*                    CX       RS  UNKNOWN VARIABLE                    *
C*                    CX1      RS  UNKNOWN VARIABLE                    *
C*                    DR       RS  DEGREE TO RADIAN CONVERSION CONSTANT*
C*                    FJ       RS  UNKNOWN VARIABLE                    *
C*                    G(76)    RS  GEOGRAPHIC FUNCTION VECTOR          *
C*                    SX       RS  SINE OF X                           *
C*                    T        RS  TEMP VARIABLE FOR LONGITUDE         *
C*                    X        RS  TEMP VARIABLE FOR NORTH-SOUTH       *
C*                    Y        RS  TEMP VARIABLE FOR EAST-WEST         *
C*                    Z        RS  TEMP VARIABLE FOR VERTICAL AXIS     *
C*  (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)      *
C*                                                                     *
C*     SETC OPTIONS:  NONE USED                                        *
C*                                                                     *
C*       SUBPROGRAMS                                                   *
C*           CALLED:  NONE                                             *
C*                                                                     *
C*       SUBPROGRAM                                                    *
C*       CALLED BY :  FLDHR                                            *
C*                                                                     *
C*                                                                     *
C* PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER         *
C*                   MAY 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION      *
C*                   AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)  *
C*                   MAR 88 - NATIONAL GEOPHYSICAL DATA CENTER (88-II) *
C*                                                                     *
C*********************************************************************
      INTEGER I
      INTEGER J
      INTEGER K(10)
      INTEGER KC
      INTEGER KDIF
      INTEGER KN
      INTEGER KO
      INTEGER N
      REAL C(3)
      REAL CX
      REAL CX1
      REAL FJ
      REAL G(76)
      REAL SX
      REAL T
      REAL X
      REAL Y
      REAL Z
C
      INCLUDE 'const.inc'
      DATA N/8/
C......................................................................
C     BEGIN EXECUTION                                                 .
C......................................................................
      X=DTOR*C(1)
      Y=DTOR*C(2)
      Z=DTOR*C(3)
      KO=K(1)
      SX=SIN(X)
C......................................................................
C     SET TERMS DUE TO MAIN LATITUDINAL VARIATION                     .
C......................................................................
      G(2)=SX
      G(1)=1.
      IF(KO .NE. 1) THEN
         DO 100 I=2,KO
            G(I+1)=SX*G(I)
  100    CONTINUE
      ENDIF
      KDIF=K(2)-KO
      IF(KDIF .EQ. 0) RETURN
      J=1
      CX1=COS(Z)
      CX=CX1
      T=Y
  180 KC=K(J)+4
C......................................................................
C     COMPUTE FIRST 2 TERMS OF J-TH ORDER LONGITUDINAL VARIATION      .
C......................................................................
      G(KC-2)=CX*COS(T)
      G(KC-1)=CX*SIN(T)
      KN=K(J+1)
C......................................................................
C     ARE ONLY TWO TERMS TO BE COMPUTED FOR THIS ORDER LONGITUDE?     .
C......................................................................
      IF(KDIF .EQ. 2) GOTO 280
      IF(KC .GT. 75) GOTO 280
C......................................................................
C     COMPUTE REMAINING TERMS OF THE J-TH ORDER LONGITUDE             .
C.....................................................................
      DO 220 I=KC,KN,2
         G(I)=SX*G(I-2)
         G(I+1)=SX*G(I-1)
  220 CONTINUE
C......................................................................
C     ARE TERMS FOR MAXIMUM ORDER LONGITUDE COMPUTED?                 .
C......................................................................
  280 IF(J .EQ. N) RETURN
C......................................................................
C     PREPARE FOR NEXT ORDER LONGITUDE COMPUTATIONS                   .
C......................................................................
      KDIF=K(J+2)-KN
      IF(KDIF .NE. 0) THEN
         CX=CX*CX1
         J=J+1
         FJ=J
         T=FJ*Y
         GOTO 180
      ENDIF
C
      RETURN
      END
      SUBROUTINE MAGFIN(POS,UNE)
C***********************************************************************
C* SUBROUTINE NAME:  MAGFIN            CPC:  SS/MOD/RTN/MAGFIN         *
C*         VERSION:  88-II            DATE:  16 MAR 88                 *
C*                                                                     *
C*         PURPOSE:  THIS SUBROUTINE COMPUTES THE NASA MAGNETIC FIELD  *
C*                   COMPONENTS USED BY SUBROUTINE FLDHR               *
C*                                                                     *
C*         CALLING                                                     *
C*       ARGUMENTS:  POS, UNE                                          *
C*                                                                     *
C*  ABBREVIATIONS:  NONE                                               *
C*                                                                     *
C*  COMMON BLOCKS:  NONE ACCESSED                                      *
C*                                                                     *
C* FILES ACCESSED:  NONE                                               *
C*                                                                     *
C*         METHOD:  1.  THIS SUBROUTINE USES THE ASSOCIATED LEGENDRE   *
C*                  POLYNOMIALS TO CALCULATE THE EARTH'S MAGNETIC FIELD*
C*                  COMPONENTS AT A SELECTED LOCATION.  IT IS DOCU-    *
C*                  MENTED IN DTIC TECHNICAL REPORT-NASA AD-772-733.   *
C*                  IT WAS DEVELOPED AS PART OF THE BENT IONOSPHERIC   *
C*                  MODEL USED FOR RAYTRACING IN THE IONOSPHERE BY NASA*
C*                  DURING THE 1970'S, CAME TO AFGWC AS PART OF THE    *
C*                  BENT MODEL, WAS BORROWED BY NGDC FOR ICED, AND IS  *
C*                  NOW BACK HERE.  THE LIMITED DOCUMENTATION AVAILABLE*
C*                  IN THE NASA TECH REPORT MEANS SOMETHING ONLY TO    *
C*                  THOSE WELL-VERSED IN LEGENDRE POLYNOMIALS.  THE    *
C*                  COMMENTS CONTAINED HEREIN ARE FROM THE NASA TR.    *
C*                                                                     *
C*        REMARKS:  NONE                                               *
C*                                                                     *
C*     REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                  *
C*                  2.  SPERRY FORTRAN-77(ASCII) PRM                   *
C*                  3.  AFGWC FORM 10 DX-50194                         *
C*                  4.  AFGWC FORM 10 DX-60096                         *
C*                                                                     *
C*LOCAL VARIABLES:  M      - IS - LOOP INDEX                           *
C*                  N      - IS - LOOP INDEX                           *
C*                  AR     - RS - RATIO OF EARTH RADIUS TO F2 RADIUS   *
C*                  BN     - RS - MAGNETIC FIELD NORTH  COMPONENT      *
C*                  BPHI   - RS - MAGNETIC FIELD EAST   COMPONENT      *
C*                  BV     - RS - MAGNETIC FIELD VERTICAL COMPONENT    *
C*                  C      - RS - SINE OF LOCATION LATITUDE            *
C*                  FM     - RS - INDEX M - 1                          *
C*                  FN     - RS - INDEX N                              *
C*                  HC     - RS - RADIUS OF EARTH IN METERS            *
C*                  P1     - RS - LATITUDE OF LOCATION                 *
C*                  P2     - RS - LONGITUDE OF LOCATION                *
C*                  PHI    - RS - LONGITUDE OF LOCATION IN RADIANS     *
C*                  RD     - RS - RADIAN TO DEGREE CONVERSION CONSTANT *
C*                  S      - RS - SQUARE ROOT OF (1-C**2)              *
C*                  SUMP   - RS - SUM OF TERMS IN PHI DIRECTION        *
C*                  SUMR   - RS - SUM OF TERMS IN RADIUS DIRECTION     *
C*                  SUMT   - RS - SUM OF TERMS IN THETA  DIRECTION     *
C*                  TS     - RS - UNKNOWN INTERMEDIATE TERM            *
C*                  AOR(7) - RV - UNKNOWN INTERMEDIATE VECTORS         *
C*                  CP(7)  - RV - SUM OF SINE AND COSINE OF LONGITUDE  *
C*                  POS(3) - RV - POSITION VECTOR (LAT,LON,HGT)        *
C*                  SP(7)  - RV - SUM OF SINE AND COSINE OF LONGITUDE  *
C*                  UNE(3) - RV - OUTPUT VECTOR OF MAGNETIC FIELD      *
C*                  CT(7,7)- RA - UNKNOWN INTERMEDIATE ARRAY           *
C*                  DP(7,7)- RA - UNKNOWN INTERMEDIATE ARRAY           *
C*                  G(7,7) - RA - UNKNOWN MAGNETIC FIELD ARRAY         *
C*                  H(7,7) - RA - UNKNOWN MAGNETIC FIELD ARRAY         *
C*                  P(7,7) - RA - UNKNOWN MAGNETIC FIELD ARRAY         *
C* (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)       *
C*                                                                     *
C*   SETC OPTIONS:  NONE USED                                          *
C*                                                                     *
C*     SUBPROGRAMS                                                     *
C*         CALLED:  NONE                                               *
C*                                                                     *
C*     SUBPROGRAM                                                      *
C*      CALLED BY:  FLDHR                                              *
C*                                                                     *
C*ROUTINE WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER          *
C*        UPDATES:  JUL 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION       *
C*                  AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER (87-I)   *
C*                  MAR 88 - NATIONAL GEOPHYSICAL DATA CENTER (88-II)  *
C*                                                                     *
C***********************************************************************
      INTEGER M
      INTEGER N
      REAL AOR(7)
      REAL AR
      REAL BN
      REAL BPHI
      REAL BV
      REAL C
      REAL CP(7)
      REAL CT(7,7)
      REAL DP(7,7)
      REAL FM
      REAL FN
      REAL G(7,7)
      REAL H(7,7)
      REAL HC
      REAL P(7,7)
      REAL P1
      REAL P2
      REAL PHI
      REAL POS(3)
      REAL S
      REAL SP(7)
      REAL SUMP
      REAL SUMR
      REAL SUMT
      REAL TS
      REAL UNE(3)
C
      DATA ((P(M,N),M=1,7),N=1,7)/49*1./,((DP(M,N),M=1,7),N=1,7)/49*0./
      DATA SP(1)/0./,CP(1)/1./,CT/2*0.,
     $  .33333333,.26666667,.25714286,.25396825,.25252525,3*0.,.2,
     $  .22857142,.23809523,.24242424,4*0.,.14285714,.19047619,
     $  .21212121,5*0.,.11111111,.16161616,6*0.,.09090909, 14*0./
      DATA G/0.,.304112,.024035,-.031518,-.041794,.016256,
     $   -.019523,0.,.021474,-.051253,.062130,-.045298,-.034407,
     $   -.004853,2*0.,-.013381,-.024898,-.021795,-.019447,
     $   .003212,3*0.,-.006496,.007008,-.000608,.021413,4*0.,
     $   -.002044,.002775,.001051,5*0.,.000697,.000227,6*0.,
     $   .001115/
      DATA H/8*0.,-.057989,.033124,.014870,-.011825,-.000796,
     $   -.005758,2*0.,-.001579,-.004075,.010006,-.002,-.008735,
     $   3*0.,.00021,.00043,.004597,-.003406,4*0.,.001385,
     $   .002421,-.000118,5*0.,-.001218,-.001116,6*0.,-.000325/
      INCLUDE 'const.inc'
      DATA HC/6371200./
C......................................................................
C     BEGIN EXECUTION                                                 .
C......................................................................
      P2=POS(2)
      P1=POS(1)
      IF((P1-89.9) .GT. 0.) THEN
         P1=89.9
         POS(1)=89.9
         P2=0.
      ELSE IF((P1-89.9) .LT. 0.) THEN
         IF((P1+89.9) .LT. 0.) THEN
            P1=-89.9
C.................................................................
C         ERROR SINCE DAY ONE ADDED BY JWT ON 12 SEP 80          :
C.................................................................
            POS(1)=-89.9
            P2=0.
         ENDIF
      ENDIF
      PHI=P2*DTOR
      AR=HC/(HC+POS(3))
      C=SIN(P1*DTOR)
      S=SQRT(CP(1)-C*C)
      SP(2)=SIN(PHI)
      CP(2)=COS(PHI)
      AOR(1)=AR*AR
      AOR(2)=AOR(1)*AR
C...............................................................
C     COMPUTE SINE/COSINE SERIES FOR MULTIPLE LONGITUDE ANGLES :
C...............................................................
      DO 50 M=3,7
         SP(M)=(SP(2)*CP(M-1))+(CP(2)*SP(M-1))
         CP(M)=(CP(2)*CP(M-1))-(SP(2)*SP(M-1))
         AOR(M)=AR*AOR(M-1)
   50 CONTINUE
C................................................................
C     CLEAR OUTER SUMS AND SET UP LOOP                          :
C................................................................
      BV=0.
      BN=BV
      BPHI=BV
      DO 110 N=2,7
         FN=N
C................................................................
C       CLEAR INNER SUMS AND SET UP LOOP                        :
C................................................................
         SUMR=0.
         SUMT=SUMR
         SUMP=SUMT
         DO 100 M=1,N
C................................................................
C         COMPUTE FUNCTIONS AND DERIVATIVES OF MULTIPLE         :
C         ASSOCIATED LEGENDRE POLYNOMIAL FUNCTIONS              :
C................................................................
            IF(((N-M) .LT. 0) .OR. ((N-M) .GT. 0)) GOTO 70
   60       P(N,N)=S*P(N-1,N-1)
            DP(N,N)=(S*DP(N-1,N-1))-(C*P(N-1,N-1))
            GOTO 90
   70       P(N,M)=C*P(N-1,M)
            DP(N,M)=(C*DP(N-1,M))+(S*P(N-1,M))
            IF(N .NE. 2) THEN
               P(N,M)=P(N,M)-(CT(N,M)*P(N-2,M))
               DP(N,M)=DP(N,M)-(CT(N,M)*DP(N-2,M))
            ENDIF
   90       FM=M-1
            TS=(G(N,M)*CP(M))+(H(N,M)*SP(M))
C................................................................
C         SUM INTO INNER SUMS FOR Z,X,Y                         :
C................................................................
            SUMR=SUMR+(P(N,M)*TS)
            SUMT=SUMT+(DP(N,M)*TS)
            SUMP=SUMP+FM*P(N,M)*((-G(N,M)*SP(M))+(H(N,M)*CP(M)))
  100    CONTINUE
C................................................................
C       SUM INTO OUTER SUMS FOR Z,X,Y                           :
C................................................................
         BV=BV+(AOR(N)*FN*SUMR)
         BN=BN-(AOR(N)*SUMT)
         BPHI=BPHI-(AOR(N)*SUMP)
  110 CONTINUE
C................................................................
C     SET UP GEOMAG FIELD COMPONENTS :                          :
C          B(Z) = VERTICAL MAG FIELD COMPONENT (UP)             :
C          B(X) = NORTH/SOUTH MAG FIELD COMPONENT (NORTH)       :
C          B(Y) = EAST/WEST MAG FIELD COMPONENT (EAST)          :
C................................................................
      UNE(1)=-BV
      UNE(2)=BN
      UNE(3)=-BPHI/S
C
      RETURN
      END
      SUBROUTINE SICOJT(L,COTIME,SITIME,A)
C***********************************************************************
C* SUBROUTINE NAME:  SICOJT            CPC:  SS/MOD/RTN/SICOJT         *
C*         VERSION:  88-II            DATE:  16 MAR 88                 *
C*                                                                     *
C*         PURPOSE:  THIS SUBROUTINE COMPUTES THE SINE AND COSINE OF   *
C*                   TIME (JT), WHERE 'A' IS THE TIME IN DEGREES.      *
C*                                                                     *
C*         CALLING                                                     *
C*       ARGUMENTS:  L, COTIME, SITIME, A                              *
C*                                                                     *
C*  ABBREVIATIONS:  NONE                                               *
C*                                                                     *
C*  COMMON BLOCKS:  NONE ACCESSED                                      *
C*                                                                     *
C* FILES ACCESSED:  NONE                                               *
C*                                                                     *
C*         METHOD:  THIS SUBROUTINE WAS DEVELOPED BY NASA FOR THE BENT *
C*                  IONOSPHERIC REFRACTION MODEL DURING THE 1970'S.  IT*
C*                  WAS GIVEN TO AFGWC FOR RAYTRACING PURPOSES.  IT    *
C*                  WAS LATER PASSED TO NGDC TO PUT IN THE ICED MODEL  *
C*                  FOR CCIR COEFFICIENT TRANSFORMATION, AND NOW HAS   *
C*                  COME BACK TO HAUNT US.  UNLESS YOU UNDERSTAND THE  *
C*                  FOURIER SERIES EXPANSION IN TIME, HOW IT OPERATES  *
C*                  WILL BE AS MUCH A MYSTERY TO YOU AS IT IS TO ME.   *
C*                                                                     *
C*        REMARKS:  NONE                                               *
C*                                                                     *
C*     REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                  *
C*                  2.  SPERRY FORTRAN-77(ASCII) PRM                   *
C*                  3.  AFGWC FORM 10 DX-50194                         *
C*                  4.  AFGWC FORM 10 DX-60096                         *
C*                                                                     *
C*      VARIABLES:  L         - IS - NUMBER OF COEFFICIENTS (SET = 8)  *
C*                  A         - RS - TIME IN DEGREES                   *
C*                  COTIME(8) - RV - COSINES OF JT (TIME)              *
C*                  SITIME(8) - RV - SINES OF JT (TIME)                *
C*                  T         - RS - TIME IN RADIANS                   *
C* (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)       *
C*                                                                     *
C*   SETC OPTIONS:  NONE USED                                          *
C*                                                                     *
C*     SUBPROGRAMS                                                     *
C*         CALLED:  NONE                                               *
C*                                                                     *
C*     SUBPROGRAM                                                      *
C*      CALLED BY:  FLDHR                                              *
C*                                                                     *
C*PROGRAM WRITTEN:  JAN 86 - NATIONAL GEOPHYSICAL DATA CENTER          *
C*        UPDATES:  JUL 86 - AFGWC/SDDE - UPGRADED DOCUMENTATION       *
C*                  AUG 87 - NATIONAL GEOPHYSICAL DATA CENTER          *
C*                  MAR 88 - NGDC (88-II) DOCUMENTATION REQUIREMENTS   *
C*                                                                     *
C***********************************************************************
      INTEGER I
      INTEGER L
      REAL A
      REAL COTIME(8)
      REAL SITIME(8)
      REAL T
      INCLUDE 'const.inc'
C.................................................................
C     CONVERT TIME IN DEGREES TO TIME IN RADIANS                 :
C.................................................................
      T=DTOR*A
C.................................................................
C     SET FIRST COEFFICIENT OF TIME TO COS/SIN OF TIME           :
C.................................................................
      COTIME(1)=COS(T)
      SITIME(1)=SIN(T)
C.................................................................
C     CALCULATE THE OTHER SEVEN COEFFICIENTS OF TIME             :
C.................................................................
      DO 10 I=2,L
         COTIME(I)=(COTIME(1)*COTIME(I-1))-(SITIME(1)*SITIME(I-1))
         SITIME(I)=(COTIME(1)*SITIME(I-1))+(SITIME(1)*COTIME(I-1))
   10 CONTINUE
C
      RETURN
      END
      SUBROUTINE URSIDA(EFFSSN)
C***********************************************************************
C* SUBROUTINE NAME :  URSI              CPC:  SS/MOD/RTN/URSI          *
C*          VERSION:  88-II RESEARCH   DATE:  12 APR 88                *
C*                                                                     *
C*          PURPOSE:  THIS SUBROUTINE USES THE URSI COEFFICIENTS TO    *
C*                    PREPARE A SUBSET OF COEFFICIENTS CONTAINING FOF2 *
C*                    AND M3000 VALUES FOR A PARTICULAR MONTH AND SUN- *
C*                    SPOT NUMBER.  USES DIRECT ACCESS.                *
C*                                                                     *
C*          CALLING                                                    *
C*        ARGUMENTS:  IMO, EFFSSN                                      *
C*                                                                     *
C*    ABBREVIATIONS:  NONE                                             *
C*                                                                     *
C*    COMMON BLOCKS:  GWC1                                             *
C*                                                                     *
C*   FILES ACCESSED:  FILE 14 - INPUT FILE OF URSI COEFFICIENTS         *
C*                    UNIT 14 MUST BE OPENED FOR DIRECT ACCESS
C*                                                                     *
C*           METHOD:  1.IF THE CURRENT MONTH IS DIFFERENT THAN THE     *
C*                      MONTH OF THE PREVIOUS CALL, THEN:              *
C*                      - POSITION COEFFICIENT FILE TO DESIRED MONTH   *
C*                      - READ IN THE FOF2 COEFFICIENTS FOR SSN = 0    *
C*                      - READ IN THE FOF2 COEFFICIENTS FOR SSN = 100  *
C*                      - POSITION TO CORRECT MONTH OF M(3000)         *
C*                      - READ IN THE M(3000) COEFFICIENTS             *
C*                      - FORCE THE INTERPOLATION/EXTRAPOLATIONS IN    *
C*                        STEP 2                                       *
C*                    2.IF THE CURRENT SUNSPOT NUMBER IS DIFFERENT THAN*
C*                      THE SUNSPOT NUMBER OF THE PREVIOUS CALL, THEN: *
C*                      - INTERPOLATE/EXTRAPOLATE THE FOF2 COEFFICIENTS*
C*                        TO THE CURRENT SUNSPOT NUMBER                *
C*                      - INTERPOLATE/EXTRAPOLATE THE M(3000) COEFF-   *
C*                        ICIENTS TO THE CURRENT SUNSPOT NUMBER        *
C*                                                                     *
C*          REMARKS:  URSI COEFFICIENTS ARE ITS-88/URSI VERSION        *
C*                                                                     *
C*       REFERENCES:  1.  ICED-III SYSTEM DOCUMENTATION                *
C*                    2.  SPERRY FORTRAN-77 (ASCII) PRM                *
C*                    3.  AFGWC FORM 10 DX-50194                       *
C*                    4.  AFGWC FORM 10 DX-60096                       *
C*                                                                     *
C*  LOCAL VARIABLES:  I        IS  LOOP AND ARRAY INDEX                *
C*                    IMO      IS  INTEGER MONTH (1-12)                *
C*                    IMOPRV   IS  PREVIOUS INTEGER MONTH              *
C*                    J        IS  LOOP AND ARRAY INDEX                *
C*                    L        IS  LOOP AND ARRAY INDEX                *
C*                    M        IS  LOOP AND ARRAY INDEX                *
C*                    JM       IS  ARRAY INDEX                         *
C*                    JN       IS  ARRAY INDEX                         *
C*                    EFFSSN   RS  SUNSPOT NUMBER (1-200)              *
C*                    F2COEF   RV  TEMPORARY FOF2 COEFFICIENTS         *
C*                    FM3      RA  URSI M(3000) COEFFICIENTS           *
C*                    SSNPRV   RS  PREVIOUS SUNSPOT NUMBER             *
C*                    UF0      RA  URSI FOF2 COEFFICIENTS FOR SSN = 0  *
C*                    UF100    RA  URSI FOF2 COEFFICIENTS FOR SSN = 100*
C*  (I=INTEGER, R=REAL, C=CHARACTER, S=SCALAR, V=VECTOR, A=ARRAY)      *
C*                                                                     *
C*     SETC OPTIONS:  NONE USED                                        *
C*                                                                     *
C*       SUBPROGRAMS                                                   *
C*           CALLED:  NONE                                             *
C*                                                                     *
C*       SUBPROGRAM                                                    *
C*       CALLED BY :  F6URSI                                           *
C*                                                                     *
C* PROGRAM WRITTEN:  APR 88 - NGDC (88-II) RESEARCH VERSION            *
C* MODIFIED       :  MAY 89 - BY L. BROWN TO ALLOW FOR MULTIPLE CALLS  *
C*                   OCT 92 - CPI (Boston) William G. Whartenby        *
C*                            Removed variables ACCPRM,ISKIP and SKIP  *
C*                            as they are no longer used in the routine*
C***********************************************************************
C
      INCLUDE 'gwc1.inc'
      INTEGER I,J
      REAL EFFSSN,SSNPRV
C
      DATA SSNPRV/-1./
C.......................................................................
C  IF THE CURRENT SUNSPOT NUMBER IS DIFFERENT THAN THE SUNSPOT NUMBER  .
C  OF THE PREVIOUS CALL, INTERPOLATE THE URSI COEFFICIENTS TO THE      .
C  CURRENT SUNSPOT NUMBER                                              .
C.......................................................................
      IF(EFFSSN .NE. SSNPRV) THEN
         DO 710 J=1,76
            DO 700 I=1,13
               UF(I,J)=UF0(I,J)+(EFFSSN/100.)*(UF100(I,J)-UF0(I,J))
  700       CONTINUE
  710    CONTINUE
         DO 810 J=1,49
            DO 800 I=1,9
               UX(I,J)=(FM3(I,J,1)*(100.-EFFSSN)+FM3(I,J,2)*EFFSSN)/100.
  800       CONTINUE
  810    CONTINUE
      ENDIF
C.......................................................................
C  STORE THE CURRENT MONTH AND CURRENT SUNSPOT NUMBER                  .
C.......................................................................
      SSNPRV=EFFSSN
C
      RETURN
      END
C
