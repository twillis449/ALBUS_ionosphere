C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLON    The increment of the magnetic longitude grid, in degrees east
C     EOF      Empirical orthogonal functions (EOFs)
C     NEOF     The number of EOFs used to construct an ion density profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLON    The number of magnetic longitude grid points
C     OMTF10P7LE
C              1.-TF10P7LE
C     OMTKPLE  1.-TKPLE
C     OPC      Orthogonal polynomial coefficients
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C              north
C     SMLON    The starting value of the magnetic longitude grid, in degrees
C              east
C     TF10P7LE Linear interpolation factor for F10.7
C     TKPLE    Linear interpolation factor for Kp
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLON    The maximum number of magnetic longitudes
C     MOPM1    The maximum number of orthogonal polynomials - 1
C
      INTEGER MALT,MEOF,MOPM1,MMLON,MX,MPOSS
      PARAMETER(MALT=28,MEOF=28,MOPM1=19,MMLON=24,MX=39,MPOSS=4)
C
      INTEGER NEOF(2,MPOSS),NOPM1(2,MPOSS),NMLAT(2,MPOSS)
      INTEGER N1ALT(2,MPOSS),NF10P7LE,NKPLE,NMLON(2,MPOSS)
      REAL EOF(MALT,MEOF,2,MPOSS),OPC(MOPM1+1,MEOF,MMLON,2,MPOSS)
      REAL DELTA(2,MPOSS),F10P7LE(3),KPLE(3)
      REAL SMLAT(2,MPOSS),DMLAT(2,MPOSS),ALT1(MALT,2,MPOSS),
     &     SMLON(2,MPOSS),DMLON(2,MPOSS)
      REAL TF10P7LE,OMTF10P7LE,TKPLE,OMTKPLE
C
      COMMON/LOW_E1/NEOF,NOPM1,NMLAT,NMLON,EOF,OPC,DELTA,SMLAT,DMLAT,
     &              SMLON,DMLON,N1ALT,ALT1,F10P7LE,KPLE,NF10P7LE,NKPLE,
     &              TF10P7LE,OMTF10P7LE,TKPLE,OMTKPLE
C
