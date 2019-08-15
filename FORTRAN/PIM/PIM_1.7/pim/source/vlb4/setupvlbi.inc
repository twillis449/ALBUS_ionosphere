C     setupvlbi.inc
C     include file for relative path structure for databases for PIM
C     2005 Sep 01  James M Anderson  --JIVE   start
      INTEGER BLOCK_SIZE
      PARAMETER(BLOCK_SIZE=1)
      CHARACTER*32 RELPATHCGM, RELPATHUSU, RELPATHMLF, RELPATHLLF
      CHARACTER*32 RELPATHLME, RELPATHURS, RELPATHNOA, RELPATHIMF
      CHARACTER*32 RELPATHIGR
      PARAMETER(RELPATHCGM='/cgmdb/')
      PARAMETER(RELPATHUSU='/usudb/unform/')
      PARAMETER(RELPATHMLF='/mlfdb/unform/')
      PARAMETER(RELPATHLLF='/llfdb/unform/')
      PARAMETER(RELPATHLME='/lmedb/unform/')
      PARAMETER(RELPATHURS='/ursidb/unform/')
      PARAMETER(RELPATHNOA='/noaa_dat/')
      PARAMETER(RELPATHIMF='/noaa_dat/IMF24.dat')
      PARAMETER(RELPATHIGR='/igrf/')
