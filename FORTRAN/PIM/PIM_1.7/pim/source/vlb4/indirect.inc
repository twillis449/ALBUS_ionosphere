C
C    Variables in common block INDIRECT
C    NAME      TYPE ARRAY  description
C    AP        REAL        Computed Ap from user supplied E layer Kp.
C    BY        REAL        Y component of the IMF
C    BZ        REAL        Z component of the IMF
C    EKP       REAL        E Kp for choosing the USU database, or to
C                          iterate on in HLEMODEL
C    ESWITCH   LOGICAL     .TRUE. if the CCIR foE model is to be used for foE,
C                          .FALSE. for parameterized model foE
C    F10P7     REAL        F10P7 to iterate on in HLEMODEL
C    KP        REAL        F Kp
C    RF10P7    REAL        Kp read from the station files for calls to NEUATM
C    RKP       REAL        Kp read from the station files for computing AP
C    URSISW    LOGICAL     .TRUE. if use URSI coefficients for FoF2 normalization,
C                          .FALSE. if the parameterized model used
C
       REAL KP(0:2),AP,BY,BZ
       REAL F10P7,EKP
       REAL RKP,RF10P7
       LOGICAL URSISW,ESWITCH
C
       COMMON/INDIRECT/EKP,KP,F10P7,BY,BZ,AP,
     1 RF10P7,RKP,URSISW,ESWITCH
C
