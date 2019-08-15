C    ERG     REAL   (3,2,2,2)  The value of the parameters on a DMSP
C                              Sattellite pass.
C    LAT     REAL   (3,2,2,2)  Latitudes at which the corresponding ERG values
C                              were obtained
C                   (i,j,k,l)
C                   i  =       1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C                   j  =       1) Electron values
C                              2) Proton values
C                   k  =       1) Fluxes
C                              2) Mean energies
C                   l  =       1) First intersection of point by DMSP satellite
C                              2) Second intersection of point by DMSP satellite
C    MLT     REAL   (3,2,2,2)  Local times at which the corresponding ERG values
C                              were obtained
C                   (i,j,k,l)
C                   i  =       1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C                   j  =       1) Electron values
C                              2) Proton values
C                   k  =       1) Fluxes
C                              2) Mean energies
C                   l  =       1) First intersection of point by DMSP satellite
C                              2) Second intersection of point by DMSP satellite
C
C    LAT0    REAL   (3,2,2)    The Geomagnetic latitude corresponding
C                              to the circle center
C                   (i,j,k)
C                   i  =       1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C                   j  =       1) Electron values
C                              2) Proton values
C                   k  =       1) Fluxes
C                              2) Mean energies
C    MLT0    REAL   (3,2,2)    The Geomagnetic local time corresponding
C                              to the circle center
C                   (i,j,k)
C                   i  =       1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C                   j  =       1) Electron values
C                              2) Proton values
C                   k  =       1) Fluxes
C                              2) Mean energies
C    RAD0    REAL   (3,2,2)    The radius of the circular boundary
C                   (i,j,k)
C                   i  =       1) Equatorward value
C                              2) Value of maximum
C                              3) Poleward value
C                   j  =       1) Electron values
C                              2) Proton values
C                   k  =       1) Fluxes
C                              2) Mean energies
C
C    MLATR   REAL              Magnetic latitude of trough-midlatitude
C                              boundary
C    MLTR    REAL              Magnetic latitude of trough-midlatitude
C                              boundary
C    VALTR   REAL              Magnetic latitude of trough-midlatitude
C                              boundary
C    MLATR0  REAL              Magnetic latitude of trough-midlatitude
C                              boundary
C    MLTR0   REAL              Magnetic latitude of trough-midlatitude
C                              boundary
C    RADTR0  REAL              Magnetic latitude of trough-midlatitude
C                              boundary
C
       REAL A0,A1,B1,LAT0,MLT0,RAD0,LAT,MLT,ERG,MLATR,MLTR,VALTR,
     1 MLATR0,MLTR0,RADTR0
C
      COMMON/PRECP1/LAT0(3,2,2,2),MLT0(3,2,2,2),RAD0(3,2,2,2),
     1 A0(2,2,2),A1(2,2,2),B1(2,2,2),LAT(3,2,2,2,2,2),MLT(3,2,2,2,2,2),
     2 ERG(3,2,2,2,2,2),MLATR(2,2,2),MLTR(2,2,2),VALTR(2,2,2),MLATR0(2),
     3 MLTR0(2),RADTR0(2)
C
C
