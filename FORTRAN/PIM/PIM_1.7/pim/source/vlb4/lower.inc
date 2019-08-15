C
C  VERSION
C     1.7   13-January-1998
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        13-Jan-1998   1.7
C                                   Changed PARAMETER MOPM1 from 11 to 14 for
C                                   new LLF parameterization.
C                                   Changed PARAMETER MX from 35 to 45 for new
C                                   LLF parameterization.
C
C     DELTA    The step size for the orthogonal polynomials
C     DMLAT    The increment of the magnetic latitude grid, in degrees north
C     DMLT     The increment of the magnetic local time grid, in decimal hours
C     EOF      Empirical orthogonal functions (EOFs)
C     LLFSCT   The sector to use in the LLF parameterized model, 0=all,
C              1=Brazilian only, 2=Indian only, 3=Pacific only, 4=USA only
C     MLON     The magnetic longitude of each sector, in degrees east
C     NALT     The actual number of altitude points
C     NEOF     The number of EOFs used to construct an O+ density profile
C     NMLAT    The number of magnetic latitude grid points
C     NMLT     The number of magnetic local time grid points
C     NOPM1    The number of orthogonal polynomials - 1
C     OMTF10P7L
C              1.-TF10P7L
C     OPC      Orthogonal polynomial coefficients
C     SMLAT    The starting value of the magnetic latitude grid, in degrees
C     SMLT     The starting value of the magnetic local time grid, in decimal
C              hours
C     TF10P7L  Linear interpolation factor for F10.7
C
C  SPECIAL CONSTANTS
C     MALT     The maximum number of altitude points
C     MEOF     The maximum number of EOFs
C     MMLT     The maximum number of magnetic local times
C     MOPM1    The maximum number of orthogonal polynomials - 1
C     MSECT    The number of sectors
C     MPOSS    The maximum number of different databases stored in program
      INTEGER MALT,MEOF,MOPM1,MSECT,MX,MPOSS,MMLT
      PARAMETER(MALT=55,MEOF=55,MOPM1=14,MSECT=4,MX=45,MPOSS=2,MMLT=48)
C
      INTEGER NF10P7L,LLFSCT
      INTEGER NALT(MSECT,MPOSS),NEOF(MSECT,MPOSS)
      INTEGER NOPM1(MSECT,MPOSS),NMLAT(MSECT,MPOSS),NMLT(MSECT,MPOSS)
      REAL OPC(MOPM1+1,MEOF,MMLT,MSECT,MPOSS),ALT(MALT,MSECT,MPOSS)
      REAL DMLAT(MSECT,MPOSS),DMLT(MSECT,MPOSS),DELTA(MSECT,MPOSS),
     &     MLN(MSECT,MPOSS)
      REAL SMLAT(MSECT,MPOSS),SMLT(MSECT,MPOSS),
     &     EOF(MALT,MEOF,MSECT,MPOSS)
      REAL F10P7L(3)
      REAL TF10P7L,OMTF10P7L
C
      COMMON/LOWER/DELTA,NOPM1,NEOF,NMLAT,DMLAT,SMLAT,NMLT,SMLT,DMLT,
     &             OPC,EOF,NALT,ALT,MLN,F10P7L,NF10P7L,LLFSCT,TF10P7L,
     &             OMTF10P7L
C
C
