C    NAME    TYPE      ARRAY        Description
C    A       REAL   (MAXALT,MAXEOF,
C                   MAXSPEC,MAXHSP) The empirical orthogonal functions
C    CAPA    REAL  (0:MAXOP,MAXFC,
C                  MAXEOF,MAXSPEC,  The coefficients for the orthogonal
C                    MAXHSP)            functions
C    MAXALT  INTEGER                Maximum number of altitude points
C    MAXEOF  INTEGER                Maximum number of empirical orthogonal
C                                   functions
C    MAXEOF  INTEGER                Maximum number of Fourier coeffiecients
C    MAXHSP  INTEGER                Maximum number of hemispheres
C    MAXOP   INTEGER                Maximum number of orthogonal polynomials
C                                   functions (EOFs)
C    MAXSPEC INTEGER                Maximum number of species
C    OMTF10P7H
C            REAL                   1.-TF10P7H
C    OMTKPH  REAL                   1.-TKPH
C    TF10P7H REAL                   Linear interpolation factor for F10.7
C    TKPH    REAL                   Linear interpolation factor for Kp
C    Z       REAL   (MAXALT,MAXSPEC,
C                     MAXHSP)       The values of the altitudes at index
C
C
      INTEGER MAXALT,MAXSPEC,MAXHSP,MAXOP,MAXEOF,MAXFC,MPOSS
      PARAMETER (MAXALT=40,MAXSPEC=3,MAXHSP=2,MAXOP=8)
      PARAMETER (MAXEOF=6,MAXFC=17,MPOSS=4)
C
      INTEGER NF10P7H,NKPH
      REAL F10P7H(3),KPH(3)
      REAL Z(MAXALT,MAXSPEC,MAXHSP,MPOSS)
      REAL A(MAXALT,MAXEOF,MAXSPEC,MAXHSP,MPOSS)
      REAL CAPA(0:MAXOP,MAXFC,MAXEOF,MAXSPEC,MAXHSP,MPOSS)
      REAL TF10P7H,OMTF10P7H,TKPH,OMTKPH
C
      COMMON /USUARRS/A,CAPA,Z,NF10P7H,NKPH,F10P7H,KPH,TF10P7H,
     &                OMTF10P7H,TKPH,OMTKPH
C
C
