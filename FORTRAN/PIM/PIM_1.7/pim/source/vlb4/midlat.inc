C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        13-Jan-1998   1.7
C                                   Changed PARAMETER MX from 12 to 11 for new
C                                   MLF parameterization.
C
C     DMLAT    The increment of the magnetic latitude grid, in degrees
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     NEOF     The number of EOFs used to construct an ion density altitude
C              profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OMTF10P7M
C              1.-TF10P7M
C     OMTKPM   1.-TKPM
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     TF10P7M  Linear interpolation factor for F10.7
C     TKPM     Linear interpolation factor for Kp
C
      INTEGER MALT,MEOF,MOPM1,MMLON,MX,MPOSS
      PARAMETER(MALT=49,MEOF=49,MOPM1=8,MMLON=24,MX=11,MPOSS=4)
C
      INTEGER NALT(2,MPOSS),NMLAT(2,MPOSS),NMLON(2,MPOSS),NEOF(2,MPOSS)
      INTEGER NOPM1(2,MPOSS),NF10P7M,NKPM
      REAL ALT(MALT,2,MPOSS),EOF(MALT,MEOF,2,MPOSS),SMLAT(2,MPOSS)
      REAL DMLAT(2,MPOSS),SMLON(2,MPOSS),DMLON(2,MPOSS),
     &     OPC(MOPM1+1,MEOF,MMLON,2,MPOSS),DELTA(2,MPOSS),F10P7M(3),
     &     KPM(3)
      REAL TF10P7M,OMTF10P7M,TKPM,OMTKPM
C
      COMMON/MIDLT1/NALT,NMLAT,NMLON,NEOF,NOPM1,ALT,EOF,SMLAT,DMLAT,
     &              SMLON,DMLON,OPC,DELTA,NF10P7M,NKPM,F10P7M,KPM,
     &              TF10P7M,OMTF10P7M,TKPM,OMTKPM
C
C
