C*  COMMON /F6PARM/ IYR,IMO,IDA,TIME,DAYJUL,EFFSSN,EFFQ,EFFAP,EFFKP,   *
C*                  GGLAT,GGLON,CGLAT,CGLON,TCGM                       *
C*     IYR    IS    YEAR OF THE DATA SET                               *
C*     IMO    IS    MONTH OF DATA SET                                  *
C*     IDA    IS    DAY OF DATA SET                                    *
C*     DAYJUL RS    JULIAN DAY OF DATA SET                             *
C*     TCGM   RS    CORRECTED GEOMAGNETIC TIME OF THE LOCATION         *
C*     TIME   RS    UNIVERSAL TIME IN HH.HH OF DATA SET                *
C*     EFFSSN RS    EFFECTIVE SUNSPOT NUMBER                           *
C*     GGLAT  RS    GEOGRAPHIC LATITUDE OF THE LOCATION                *
C*     GGLON  RS    GEOGRAPHIC LONGITUDE OF THE LOCATION               *
C*     CGLAT  RS    CORRECTED GEOMAGNETIC LATITUDE OF THE LOCATION     *
C*     CGLON  RS    CORRECTED GEOMAGNETIC LONGITUDE OF THE LOCATION    *
C*                                                                     *
      INTEGER IDA,IMO,IYR
      REAL CGLAT,CGLON,DAYJUL,EFFAP,EFFKP,EFFSSN,EFFQ
      REAL GGLAT,GGLON,TCGM,TIME
C
      COMMON/F6PARM/IYR,IMO,IDA,TIME,DAYJUL,EFFSSN,EFFQ,EFFAP,EFFKP,
     $              GGLAT,GGLON,CGLAT,CGLON,TCGM
C
C
