C    Variables in COMMON GRID
C    NAME     TYPE     Description
C    DAZ      REAL     Azimuth increment (degrees)
C    DEL      REAL     Elevation increment (degrees)
C    DLAT     REAL     latitude increments
C    DLON     REAL     longitude increments
C    GGLAT0   REAL     minimum latitude
C    GGLON0   REAL     minimum longitude
C    GGLATF   REAL     maximum latitude
C    GGLONF   REAL     maximum longitude
C    GEOG     LOGICAL  TRUE for regular grid in geographic coordinates,
C                      FALSE for regular grid in geomagnetic coordinates.
C    MAXALTPT INTEGER  Dimension of ZOUT
C    MPR      INTEGER  Maximum allowed number of latitude/longitude pairs
C    NALT     INTEGER  number of altitude points
C    NAZ      INTEGER  Number of azimuths
C    NEL      INTEGER  Number of elevations
C    NPR      INTEGER  Number of latitude/longitude pairs
C    NUMLAT   INTEGER  number of latitude values
C    NUMLON   INTEGER  number of longitude values
C    OBSLAT   REAL     Observer latitude (degrees north)
C    OBSLON   REAL     Observer longitude (degrees east)
C    OBSELEV  REAL     Observer elevation (km above Earth radius)
C    PLASPH   LOGICAL  The plasmasphere flag, .TRUE. if a plasmasphere is to be
C                      included, .FALSE. otherwise
C    PRLAT    REAL     Latitudes of latitude/longitude pairs (degrees north)
C    PRLON    REAL     Longitudes of latitude/longitude pairs (degrees east)
C    SAZ      REAL     Starting azimuth (degrees)
C    SEL      REAL     Starting elevation (degrees)
C
      INTEGER MPR,MAXALTPT,NUMZOUTORIG
      PARAMETER (MPR=1000,MAXALTPT=200,NUMZOUTORIG=100)
C
      INTEGER NUMLAT,NUMLON,NPR,NAZ,NEL,NALT
      REAL GGLAT0,DLAT,GGLON0,DLON,GGLATF,GGLONF,OBSLAT,OBSLON,OBSELEV,
     &     SAZ,DAZ,
     &     SEL,DEL
      REAL PRLAT(MPR),PRLON(MPR),ZOUT(MAXALTPT),ZOUTORIG(NUMZOUTORIG)
      LOGICAL GEOG,PLASPH
C
      COMMON/GRID/NUMLAT,GGLAT0,DLAT,NUMLON,GGLON0,DLON,NALT,ZOUT,
     1  GGLATF,GGLONF,GEOG,NPR,PRLAT,PRLON,OBSLAT,OBSLON,OBSELEV,
     &     NAZ,SAZ,DAZ,
     &  NEL,SEL,DEL,PLASPH,ZOUTORIG
C
