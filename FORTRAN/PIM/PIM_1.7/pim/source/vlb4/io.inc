C    Variables in COMMON IO
C    NAME    TYPE       ARRAY     description
C    AP      REAL                 Input data from the data file
C    DELTA   REAL                 The step size for the orthogonal functions
C    DMLAT   REAL                 The magnetic lat step size in degrees
C    DMLT    REAL                 The local time step size in hours
C    DUT     REAL                 The universal time step size in hours
C    F10P7   REAL                 The 10.7 parameter
C    IDAY    INTEGER              The day the data was taken
C    IYEAR   INTEGER              The year the data was taken
C    NEOF    INTEGER              Number of empirical orthogonal functions
C    NMAX    INTEGER              Number (0:nmax) of derived orthogonal
C                                 functions used
C    NMLAT   INTEGER              Number of mag lat points available
C    NMLT    INTEGER              Number of local times available
C    NUM_EFC INTEGER              Total number of Fourier coefficients used in
C                                 E layer calculations
C    NUM_FFC INTEGER              Total number of Fourier coefficients used in
C                                 F layer calculations
C    NUM_HT  INTEGER              Number of height points
C    NUT     INTEGER              Total number of universal times available
C    SMLAT   REAL                 Smallest mag lat
C    SMLT    REAL                 Smallest local time
C    SUT     REAL                 Smallest universal time
C    XKP     REAL                 The value of KP
C    Note... FORTRAN 77 does not allow the mixing of character and non-
C    character variables in the same common block... If your system is
C    not bound by this particular limitation of FORTRAN, then IOPEN may
C    be put in common block IO and IO1 may be removed
C
      CHARACTER*15 FLNM
      INTEGER IYEAR,IDAY,NUM_EFC,NUM_FFC,NMAX,NUT,NMLAT
      INTEGER NMLT,NUM_HT,NEOF
      REAL F10P7,AP,XKP,SUT,DUT,SMLAT,DMLAT,SMLT,DMLT,DELTA
C
      COMMON/IO/IYEAR,IDAY,F10P7,AP,NUM_EFC,NUM_FFC,NMAX,XKP,NUT,SUT,
     1DUT,NMLAT,SMLAT,DMLAT,DELTA,NMLT,SMLT,DMLT,NUM_HT,NEOF
      COMMON/IO1/FLNM
C
