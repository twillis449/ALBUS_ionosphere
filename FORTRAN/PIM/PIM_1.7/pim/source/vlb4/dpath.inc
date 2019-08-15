C    JOPEN  INTEGER           1 for byte record lengths
C                              2 for longword record lengths
C    PAWS    CHARACTER         Path to the URSI data set
C    PLME    CHARACTER         path name for the low and mid latitude
C                              E layer database
C    PLOW    CHARACTER         path name for the low latitude F layer
C                              database
C    PMID    CHARACTER         path name for the midlatitude F layer
C                              database
C    PUSU    CHARACTER         path name for the USU database
C
C    Note... FORTRAN 77 does not allow the mixing of character and non-
C    character variables in the same common block... If your system is
C    not bound by this particular limitation of FORTRAN, then JOPEN may
C    be put in common block DPATH and DP1 may be removed
C
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      INTEGER JOPEN
      CHARACTER*256 PUSU,PMID,PLOW,PLME,PAWS
      COMMON/DPATH/PUSU,PMID,PLOW,PLME,PAWS
      COMMON/DPTH1/JOPEN
C
