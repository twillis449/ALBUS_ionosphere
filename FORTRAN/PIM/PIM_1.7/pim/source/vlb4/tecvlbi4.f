      SUBROUTINE slantedp (year,day,ut, dbgflg,ldbg,sta,src,
     1                       ed,rng,edmax,rngmax, loslat,loslon)

C  Subroutine to calcluate the electron-density profile along the
C    (slant) line-of-sight from the ground station (OBSLAT,OBSLON) in
C    the direction (SAZ,SEL)  [all passed via COMMON block in GRID.INC]
C
C  Culled from PIM subroutine AZEL_OUT, and in turn from TECVLBI
C

C  Common block
      INCLUDE 'array.inc'
      include 'const.inc'
      INCLUDE 'grid.inc'
      INCLUDE 'indirect.inc'
      INCLUDE 'ssnstuff.inc'

      INTEGER MAX_ALT_NUM7
      PARAMETER(MAX_ALT_NUM7=MAX_ALT_NUM+7)
      INTEGER MAX_ALT_NUM8
      PARAMETER(MAX_ALT_NUM8=MAX_ALT_NUM+8)


      INTEGER i,k, year,day
      INTEGER ndat, ldbg, phr,pmin, lcnt
      REAL mlat,mlt,data(MAX_ALT_NUM8),cdata(MAX_ALT_NUM7),ut,mlon
      REAL gglat,gglon,az,el,ed(maxaltpt),rng(maxaltpt),edmax
      REAL rngmax,altmax,fpmax,lkalt,lkrang
      REAL uthh, dbglat(MAX_ALT_NUM),dbglon(MAX_ALT_NUM)
      REAL loslat(maxaltpt), loslon(maxaltpt)
      CHARACTER*16 dsgntr
      CHARACTER sta*1, src*8
      LOGICAL dbgflg

C----------------------------------
C
C  UT - UT in [s]
C
C  CDATA - A vector containing the calculated electron density
C              as returned from REGMOD
C  GGLAT - Geographic latitude, in degrees north |--> of each point 
C  GGLON - Geographic longitude, in degrees east |   along the LoS
C  MLAT  - Magnetic latitude, in degrees north
C  MLT   - Magnetic local time, in hours
C
C  DATA   -  A vector containing a direct data record |--> tells REGMOD
C  DSGNTR -  A code designating a type of direct data |  what to return
C     I       INTEGER           A loop counter
C  ED     -  Electron density at gridded Alt's along LoS  [cm^{-3}]
C  EDMAX  -  Maxiumem electron density encountered along LoS
C  RNGMAX -  Range to EDMAX  [km]
C  LOSLAT -  Geographic latitude [deg] of samples along LoS
C  LOSNON -  Geographic longitude [deg] of samples along LoS
C
C  SUBROUTINES REQUIRED
C     CGLAL1   Transforms geographic to corrected geomagnetic coords
C     REGION   Determines the high latitude region of a data record
C     REGMOD   Calculates a quantity using regional models
C
C--------------------------------------

      dsgntr = 'EDP'
C
C       Set up the altitude grid for the calls to REGMOD
      ndat = 1
      data(1) = 1.0
C
C  Determine the combined coordinate system type / output grid type /
C  plasmasphere flag and write it to the PIM output file
C  Note:The combined coordinate system /output grid /plasmasphere
C     flag is determined as follows:
C     0 for geographic or 1 for corrected geomagnetic coordinate system
C     + 10*2 for azimuth/elevation (ground-based) output grid type
C     + 100*0 for no plasmasphere or 100*1 for Gallagher plasmasphere
C
      i=0
      i=i+20
      IF (plasph) i = i+100
      az = saz
      el = sel

C  Loop over each altitude sample:
C   *) compute geographic position of the intersection of the LoS with
C        the vertical altitude ZOUT(k)
C   *) compute the magnetic coordinates of this point
C   *) compute the electron density there [cm^{-3}]
C
      DO 500 k = 1, nalt
         data(2) = zout(k)

         IF (el .GT. 89.9999) THEN
            gglat = obslat
            gglon = obslon
            rng(k) = zout(k)
         ELSE
            CALL lkcrds (obslat,obslon,obselev,
     &           az,el, zout(k), gglat,gglon)
            rng(k) = lkrang (obselev,REAL(el*DTOR),zout(k))
         ENDIF

         IF (gglon .LT. 0.0) gglon = gglon + 360.0
         IF (gglon .GE. 360.0) gglon = gglon - 360.0

         loslat(k) = gglat
         loslon(k) = gglon

C  The grid is in geographic coordinates ... get the corrected
C   geomagnetic coordinates.
C
         CALL gcxcgm (day,ut, gglat,gglon, mlat,mlon,mlt)
         IF (dbgflg) THEN
            dbglon(k) = mlon 
            dbglat(k) = mlat 
         END IF

C  Only call the model for the actual data
C
         CALL regmod (dsgntr,year,day,ut, gglat,gglon, mlat,mlon,mlt,
     1                 ndat,data,cdata)
         ed(k) = cdata(1)

 500  CONTINUE

C  Get Max electron density & range to max  (if ionosonde RTD)
C
      CALL fndmax (nalt,rng,ed, edmax,rngmax)
      fpmax = SQRT(edmax/1.24e4)
      IF (el .GT. 89.9999) THEN
         altmax = rngmax
      ELSE
         altmax = LKALT (el,rngmax)
      ENDIF

C	Debugging output if DBGFLG set
C
      IF (dbgflg) THEN
         uthh = ut/3.6e3
         phr = INT(uthh)
         pmin = NINT((uthh-phr)*6.e1)
         IF (pmin .EQ. 60) THEN
            pmin = 0
            phr = phr + 1
            IF (phr .GE. 24) phr = phr - 24
         END IF
         WRITE (ldbg,609) src, sta, phr,pmin, az,el
 609     FORMAT (1x,a8,2x,a1,2x,2i2.2,'   Az = ',f11.6,'  El = ',
     1             f9.6,/)
         WRITE (ldbg,606) rf10p7, rkp, ut
 606     FORMAT (9x,'rF10.7 = ',f10.5,'  rKp  = ',f7.4,'   UT = ',f11.4)
         WRITE (ldbg,605) f10p7, kp(0)
 605     FORMAT (9x,' F10.7 = ',f10.5,'   Kp0 = ',f7.4)
         WRITE (ldbg,604) kp(1)
 604     FORMAT (31x,'Kp1 = ',f7.4)
         WRITE (ldbg,603) kp(2)
 603     FORMAT (31x,'Kp2 = ',f7.4/)
         WRITE (ldbg,607) 'Alt', 'Rng', 'n_e', 'MagLat', 'MagLon'
 607     FORMAT (a10,a10,a12,a9,a10/)
         lcnt = 9
         DO 600 i = 1,nalt
            WRITE (ldbg, 608) zout(i),rng(i),ed(i), dbglat(i),dbglon(i)
	    lcnt = lcnt + 1
 600     CONTINUE
 608     FORMAT (2f10.1, f12.1, f9.3, f10.3)
         DO 610 i = 1, (124-lcnt)
            WRITE (ldbg,'(a1)') ' '
 610     CONTINUE
      END IF

      RETURN
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==


      SUBROUTINE rtdappl (maxgps,maxiss, nalt, az,el,rng,ed0,rmax0,
     1                     dogps,doiss, ngps,niss, s,sigs,azgps,elgps,
     2                     rho,sigrho,delta,sigdelta,aziss,eliss, ed4)

C  Subroutine to apply the (CPI) Kwajalein real-time data algorithm
C   to the base PIM electron densities along the LoS

      INCLUDE 'array.inc'
      INCLUDE 'logicuni.inc'
      INTEGER maxexpalt, maxgps,maxiss
      PARAMETER (maxexpalt=MAX_ALT_NUM)
      INTEGER nalt, i,j,k, ngps,niss
      REAL ed0(nalt), ed4(nalt)
      REAL ed1(maxexpalt),ed2(maxexpalt),ed3(maxexpalt),ed2x(maxexpalt)
      REAL rng(nalt), x(maxexpalt), rmax0,rmax3,xcoeff, rgps
      REAL s(maxgps),rho(maxiss),delta(maxiss), partway, a,st
      REAL sigs(maxgps),sigrho(maxiss),sigdelta(maxiss)
      REAL sm,rhom,deltam, sigsm,sigrhom,sigdeltam
      REAL*8 az,el, azgps(maxgps),elgps(maxgps)
      REAL*8 aziss(maxiss),eliss(maxiss)
      LOGICAL dogps,doiss
c     PARAMETER (agps = 26560.18, ae = 6378.137)
C---------------------------------
C
C  MAXGPS,MAXISS - Dimension-sizes of GPS-,ISS-related data
C  NGPS,NISS - Acutual number of contributiing GPS SVNs, ionosondes
C  NALT  - number of samples along LoS
C  RNG   - vector of ranges to samples along LoS
C  ED0   - "base" PIM electron densities
C  RMAX0 - range to "base" maximum electron density
C  S     - GPS correction factors
C  RHO   - Max-density ISS corrections
C  DELTA - Range-to-Max ISS corrections
C  SIGx  - The uncertainties in the various RTD correction factors
C  AZ,EL - LoS to source
C  AZGPS,ELGPS - LoS's to the NGPS GPS SVNs
C  AZISS,ELISS - LoS's to the NISS ionosonde data locations
C
C  ED4   - real-time data corrected electron densities
C
C  xM    - The RTD correction factors mapped to the source LoS
C  SIGxM - The uncertainties in the mapped correction factors
C               (not currently used)
C  ED1  - GPS-corrected elec.dens.
C  ED2  - Max-density ISS-corrected elec.dens.
C  RMAX3 - Range-to-Max ISS-corrected Range-to-Max
C  X - rescaled ranges following Range-to-Max correction
C  ED3  - ISS-corrected elec.dens.  (not properly normalized)
C  A,STOP - Intermediate variables to rescale ED3
C  
C-----------------------------------

      IF (nalt .GT. maxexpalt) THEN
         WRITE (LUSTDERR,*) 'Too many Altitude samples in RTDAPPL'
         STOP
      END IF
 
C  Apply GPS corrections if there were any  
C
      IF (dogps) THEN

C      Compute mapped GPS correction factor along LoS to source
C
         CALL mapcf (maxgps,ngps, az,el,azgps,elgps, s,sigs, sm,sigsm)
         DO 100 k = 1, nalt
            ed1(k) = sm * ed0(k)
 100     CONTINUE

      ELSE

C      No GPS corrections
         DO 890 k = 1, nalt
            ed1(k) = ed0(k)
 890     CONTINUE
      END IF


C  Apply ISS corrections if there are any
C
      IF (doiss) THEN

C      Compute mapped ISS correction factor along LoS to source
C
         CALL mapcf (maxiss,niss, az,el,aziss,eliss, rho,sigrho,
     1                  rhom,sigrhom)
         CALL mapcf (maxiss,niss, az,el,aziss,eliss, delta,sigdelta,
     1                  deltam,sigdeltam)

C    Apply the Max.density correction & compute the resampling along
C      the LoS for the Range-to-Max correction
C
         rmax3 = rmax0 + deltam
         xcoeff = (rmax0-rng(1)) / (rmax3-rng(1))
         DO 200 k = 1, nalt
            x(k) = rng(1) + xcoeff*(rng(k)-rng(1))
            ed2(k) = rhom * ed1(k)
 200     CONTINUE

C     Interpolate ED2(R) -->  ED2x(X)  via 2nd-order Lagrangian
C
         DO 220 i = 1, nalt

C      Find Jth RNG to use as center of interpolation
C
            IF (x(i) .GE. rng(nalt-1)) THEN

C         X is too close to end of RNG; use last 3 RNG(k)
C
               j = nalt-1

            ELSE IF (x(i) .LE. rng(2)) THEN

C         X is too close to beginning of RNG; use first 3 RNG(k)
C
               j = 2

            ELSE
               j = 0
 230           j = j+1
               IF ( rng(j) .LE. x(i) ) GOTO 230
               partway = ( x(i) - rng(j-1) ) / ( rng(j) - rng(j-1) )
               IF (partway .LT. 0.5) j = j-1
            END IF

            ed2x(i) = (x(i) - rng(j)) * (x(i) - rng(j+1)) * ed2(j-1) / 
     1                  ( (rng(j-1) - rng(j)) * (rng(j-1) - rng(j+1)) )
     2              + (x(i) - rng(j-1)) * (x(i) - rng(j+1)) * ed2(j) / 
     3                  ( (rng(j) - rng(j-1)) * (rng(j) - rng(j+1)) )
     4              + (x(i) - rng(j-1)) * (x(i) - rng(j)) * ed2(j+1) / 
     5                  ( (rng(j+1) - rng(j-1)) * (rng(j+1) - rng(j)) )
            IF (ed2x(i) .LT. 0.) ed2x(i) = 0.

 220     CONTINUE

C   Compute unnormalized Range-to-Max corrected electron densities
C
         DO 240 k = 1, nalt
            ed3(k) = ed2x(k) / xcoeff
 240     CONTINUE

C   Renormalize topside profile of Range-to-Max corrected electron densities
C    to integrated GPS RTD values
C   Here, we use the fact that we selected ZOUT(NALT) to be AGPS to 
C     the closest km (as determined from the mean R_{GPS} from the 
C      IGS-post-processed ephemeris from 95-298.... done at PLH for 
C      GPS/MET work).  
C
         rgps = rng(nalt)
         a = (ed1(nalt) - ed3(nalt)) / (ed3(nalt) * (rgps - rmax3))
         DO 300 k = 1, nalt
            IF (rng(k) .LE. rmax3) THEN
               st = 1.e0
            ELSE
               IF (rng(k) .GE. rgps) THEN
                  st = 1.e0 + a*(rgps - rmax3)
               ELSE
                  st = 1.e0 + a*(rng(k) - rmax3)
               END IF
            END IF
            ed4(k) = st * ed3(k)
 300     CONTINUE 

      ELSE

C     No ISS corrections
         DO 880 k = 1, nalt
            ed4(k) = ed1(k)
 880     CONTINUE
      END IF

      RETURN
      END


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==


      SUBROUTINE mapcf (maxn,n, az,el, azrtd,elrtd, cf,sigcf, 
     1                    cfm,sigcfm)

C  Subroutine to map individual real-time data correction factors
C    to the LoS of the source

      INTEGER maxn,n, i
      REAL cf(maxn),sigcf(maxn), cfm,sigcfm
      REAL sw
      REAL*8 az,el, azrtd(maxn),elrtd(maxn), thet,w(16)
      REAL*8 d(16),dprod,wprism(16)
C---------------------------------
C
C  MAXN  - Dimension-size of RTD
C  N     - Actual number of RTD
C  AZ,EL - LoS to source
C  AZRTD,ELRTD - LoS's to various RTD sources
C  CF    - Individual RTD correction factors
C  SIGCF - Uncertainties in individual correction factors
C
C  CFM   - Correction factors mapped to source LoS
C  SIGCFM- Uncertainty in correction factor mapped to source LoS
C
C--------------------------------

C  Compute angles between the source LoS & all RTD sources;
C   compute "regular" weights
C
      DO 100 i = 1, n
         thet = DSIN(el) * DSIN(elrtd(i))
         thet = thet + DCOS(el) * DCOS(elrtd(i)) * DCOS(az - azrtd(i))
         thet = DACOS(thet)
         w(i) = 1.d0 / (thet*thet)
         d(i) = (1.d0 - DCOS(thet)) / 2.d0 
 100  CONTINUE

C  Compute PRISM-like weights (in case they're ever wanted -- replace
C                               W(I) in block-200 with WPRISM(I)) 
C
      dprod = 1.d0
      DO 150 i = 1, n
         dprod = dprod * d(i) 
 150  CONTINUE
      DO 160 i = 1, n
         wprism(i) = dprod / d(i)
 160  CONTINUE

C  Apply weights to map the individual correction factors 
C    to the source LoS
C
      sw = 0.
      cfm = 0.
      sigcfm = 0.
      DO 200 i = 1, n
         sw = sw + SNGL(w(i))
         cfm = cfm + SNGL(w(i)) * cf(i) 
         sigcfm = sigcfm + SNGL(w(i)) * sigcf(i)*sigcf(i)
 200  CONTINUE
      cfm = cfm / sw
      sigcfm = SQRT(sigcfm) / sw

      RETURN
      END


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==


      SUBROUTINE tecfr (dofr, yearigrf, nalt, az,el,lat,lon, z,rng,
     1                    loslat,loslon, ed, tec,rmi)

C  Subroutine to integrate TEC(delay), Faraday rotation, etc.
C
      INCLUDE 'array.inc'
       INTEGER maxexpalt
       PARAMETER (maxexpalt = MAX_ALT_NUM)
       INTEGER nalt,i
       REAL z(nalt),rng(nalt),ed(nalt), loslat(nalt),loslon(nalt) 
       REAL saz,sel,slat,slon
       REAL*8 losx,losy,losz
C       REAL bnorthr,beastr,bdownr
       REAL*8 bnorth,beast,bdown
C       REAL babs
       REAL*8 bx,by,bz
       REAL*8 bpar(maxexpalt)
C       REAL*8 bperp(maxexpalt)
       REAL rotmeas(maxexpalt)
       REAL*8 coslat, coslon, sinlat, sinlon
       REAL tec, rmi,rotmin
       REAL*8 az,el, lat,lon
       REAL yearigrf
       LOGICAL dofr
C-------------------------
C
C  YEARIGRF - fractional year of observation date, as in 1996.435
C  NALT     - Actual number of LoS samples
C  AZ,EL    - Direction of LoS
C  LAT,LON  - Geographic latitude,longitude of ground station
C  LOSLAT,LOSLON - Geog latitude,longitude of each sample along LoS
C  Z        - Heights of LoS samples
C  RNG      - Distance along LoS of LoS samples
C  ED       - Electron density sampled along LoS
C
C  TEC      - TEC (1st order ionospheric (group) delay) in [TECU]
C  RMI      - 1st order Faraday-rotation integral in [T/m^2]
C
C  LOS[xyz] - Direction cosines of propagation in ECEF frame  (-LoS)
C  B[xyz]   - ECEF components of Earth's magnetic field  (IGRF)
C  BPAR     - Component of B parallel to propagation at each LoS sample
C  BPERP    - Component of B perpendicular to prop at each LoS sample
C  ROTMEAS  - Electron density * B_parallel at each LoS sample
C  ROTMIN   - Kludge to avoid any ROTMEAS<0 before passing to TECCLC
C
C------------------------
       include 'const.inc'
       INCLUDE 'logicuni.inc'

      IF (nalt .GT. maxexpalt) THEN
         WRITE (LUSTDERR,*) 'Too many altitude samples in TECFR'
         STOP
      END IF

C  If calculating Faraday rotation, compute components of B along
C   the LoS  (for higher order calculations, may want to do this
C   whether calculating Faraday rotation or not)
C
      saz = SNGL(az)
      sel = SNGL(el)
      slat = SNGL(lat)
      slon = SNGL(lon)

      IF (dofr) THEN

         losx = SIN(sel) * COS(slat) * COS(slon) -
     1          COS(sel) * SIN(saz) * SIN(slon) - 
     2          COS(sel) * COS(saz) * SIN(slat) * COS(slon)
         losy = SIN(sel) * COS(slat) * SIN(slon) + 
     1          COS(sel) * SIN(saz) * COS(slon) - 
     2          COS(sel) * COS(saz) * SIN(slat) * SIN(slon)
         losz = SIN(sel)*SIN(slat) + COS(sel)*COS(saz)*COS(slat)
         losx = -losx
         losy = -losy
         losz = -losz

         DO 100 i = 1, nalt
c$$$            CALL feldg (loslat(i),loslon(i), z(i),
c$$$     1                    bnorthr,beastr,bdownr,babs)
            coslat = cos(loslat(i)*DTOR)
            coslon = cos(loslon(i)*DTOR)
            sinlat = sin(loslat(i)*DTOR)
            sinlon = sin(loslon(i)*DTOR)
            CALL jma_igrf10syn (DBLE(yearigrf),
     &           (RADIUS_EARTH+z(i))*1000.0D0,    ! needs to come in in meters
     &           coslat,sinlat,
     &           coslon,sinlon,bnorth,beast,bdown)
C     Convert JMA_igrf10syn magnetic field From T to Gauss
            Bnorth = Bnorth * 1D+4
            Beast = Beast * 1D+4
            Bdown = Bdown * 1D+4
        
            bx = beast * sinlon +
     1           bnorth * sinlat * coslon +
     2           bdown * coslat * coslon
            bx = -bx
            by = beast * coslon +
     1           bnorth * sinlat * sinlon - 
     2           bdown * coslat * sinlon
            bz = bnorth * coslat - bdown * sinlat
  
            bpar(i) = losx*bx + losy*by + losz*bz
c$$$            print *, 'at', i, ' got Bpar', bpar(i), Bnorth, Beast,
c$$$     &           Bdown
c$$$            print *, bnorthr, beastr, bdownr
C            bperp(i) = SQRT(babs*babs - bpar(i)*bpar(i))
 100     CONTINUE

      END IF

C  Now we have ED(k), BPAR(k), BPERP(k):  only integrating left
C  For now, stick with only the linear TEC term for delay and
C    the 1st order "Faraday rotation" term -- higher orders can
C    be constructed from ED, BPAR, & BPERP.  If frequency also
C    passed, Delay & Physical Faraday rotation can be returned
C    to whatever order. 
C  For the future, we can look into better integration schemes
C    than that in regular PIM (QROMB, TECCLC), but for now, we'll
C    stick with that, including the kludgy ROTMIN addition to the
C    Faraday rotation calculation (because of the logrithmic
C    interpolation in TECCLC)
C    
C  TEC finally in TECU  [10^{16}/m^2]
C
      CALL tecclc (nalt, rng, ed, tec)
      tec = tec * 1.e-12

C  "Rotation measure" finally in RMU  [10^{12} T/m^2]
C     remember having to convert from 1/cm^2-->1/m^2 & from G-->T
C
      rmi = 0.0
      IF (dofr) THEN

C  ROTMIN (<0) is the minimum ROTMEAS, to be subtracted from all
C    ROTMEAS(k) before passing to TECCLC (to ensure all positive)
C    Subtract this extra area (\Delta RNG * ROTMIN) from the 
C    returned RMI
C
         rotmin = 0.0
         DO 200 i = 1, nalt
            rotmeas(i) = ed(i) * bpar(i)
            IF (rotmeas(i) .LT. rotmin) rotmin = rotmeas(i)

 200     CONTINUE

         IF (rotmin .LT. 0.) THEN
            rotmin = rotmin - 1.0
            DO 205 i = 1, nalt
               rotmeas(i) = rotmeas(i) - rotmin
 205        CONTINUE
         END IF

         CALL tecclc (nalt, rng, rotmeas, rmi)
         IF (rotmin .LT. 0.) THEN
            rmi = rmi + rotmin * ABS(rng(nalt)-rng(1)) * 1.e5
         END IF
         rmi = rmi * 1.e-12

      END IF

      RETURN
      END
