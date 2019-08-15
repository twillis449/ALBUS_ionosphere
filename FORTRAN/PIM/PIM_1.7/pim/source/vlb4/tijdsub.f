      SUBROUTINE utrngchk (uthr, iyr,idoy, ndayyr)

C  Subroutine to check that UTHR is in the range 0-24, and to ajust
C   the Day and Year as necessary if not
C
      INTEGER iyr,idoy,ndayyr
      REAL*8 uthr, nul,fullday
      PARAMETER (nul=0.d0, fullday=2.4d1)

      IF (uthr .LT. nul) THEN
         uthr = uthr + fullday
         idoy = idoy - 1
         IF (idoy .LT. 1) THEN
            iyr = iyr - 1
            idoy = 365
            IF (MOD(iyr,4) .EQ. 0) idoy = 366
         END IF
      END IF

      IF (uthr .GT. fullday) THEN
         uthr = uthr - fullday
         idoy = idoy + 1
         IF (idoy .GT. ndayyr) THEN
            iyr = iyr + 1
            idoy = 1
         END IF
      END IF
        
      RETURN
      END


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==


      SUBROUTINE sidtim (iyr,idoy,ut, gast)

C  Subroutine to calculate Greenwich apparent sidereal time
C
      INCLUDE 'logicuni.inc'
      INTEGER iyr,idoy, ieq, nday81,nleapd, neq
      REAL*8 ut,ut24hr, jd,jd81p0, t0,th0, gmst,gast, pi,degrad
C  neq good through 2006 
      PARAMETER (neq = 27)
      REAL eqeqnx(neq)
      PARAMETER (jd81p0 = 2444604.5d0)
      PARAMETER (pi = 3.14159265358979323846d0)
      PARAMETER (degrad = pi / 1.8d2)
C   eqeqnx updated through 2006  (the 0.0384 value; 26may2005)
C   2005 value interpolated until 2005AA found
      DATA eqeqnx/-0.8990, -1.0220, -1.0425, -0.9398, -0.7153, -0.4233,
     1            -0.1304, 0.2, 0.5642, 0.8137, 0.9791, 1.0411, 0.9943,
     2             0.8287, 0.5766, 0.2445, -0.1134, -0.4620, -0.7468,
     3             -0.95005,-1.04165,-1.01890,-0.8625,-0.6114,-0.2992,
     4             0.0348,0/
C------------
C
C  JD81P0 - JD for 0Jan81
C  EQEQNX - for now, set Equation of Equinoxes to be the mean of its 
C            range of values for each specific year from 81-00.  This 
C            gives an error of no more than .24s (usually <1/6s).  
C            Ignoring EQEQNX altogether gives an error of no more than
C            ~9/8s (i.e., GMST vs. GAST)
C       The DATA statement loads an array of EQEQNX, addressable by
C            IYR-1980, with 0 as a value for years >2000.  This 
C            DATA statment (& the REAL) can be updated as new AstrAlm
C            come out.  When updating EQEQNX, remember also to update
C            the value of NEQ so that the proper dimensioning/testing
C            will occur.
C       A proper nutation-cacluculating subroutine could be created
C         for more accurate values for specific YRDYV & UTV
C         see Astron.Alm. Explanatory Suppl. 111-117
C
C----------------

      nday81 = (iyr-1981)*365 + idoy
      nleapd = (iyr-1981)/4
      nday81 = nday81 + nleapd
      ut24hr = ut / 2.4d1
      jd = jd81p0 + DBLE(nday81) 

      t0 = (jd - 2451545.d0) / 36525.d0
      th0 = 24110.54841d0 + 8640184.812866d0*t0 + 0.093104d0*t0**2 -
     1        6.2d-6*t0**3
      gmst = 1.0027379093d0*ut24hr*8.64d4 + th0 
      ieq = iyr - 1980
      IF (ieq .GE. neq) THEN
         WRITE (LUSTDERR,*)
     &        'Update Equation of Equinox DATA in "sidtim.f"'
         ieq = neq
      END IF
      gast = gmst + DBLE(eqeqnx(ieq))
      gast = 1.5d1 * gast * degrad / 3.6d3

      RETURN
      END


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==


      SUBROUTINE azel (gast, lat,lon, ra,dec, az,el)

C  Subroutine to calculate the Azimuth/Elevation of a source as seen
C   from a station at a given sidereal time
C  No checking for EL<0  (do back in calling program)
C
      REAL*8 gast, lat,lon, ra,dec, az,el, pi,degrad, twopi
      REAL*8 ha, elarg, aznum,azdenom
      PARAMETER (pi = 3.14159265358979323846d0)
      PARAMETER (degrad = pi / 1.8d2)
      PARAMETER (twopi = 2.d0 * pi)
C-------------
C
C
C--------------

      ha = gast + lon - ra

      elarg = DSIN(lat) * DSIN(dec) + DCOS(lat) * DCOS(dec) * DCOS(ha)
      el = DASIN(elarg)

      IF ((1.d0-DABS(elarg)) .LT. 1.d-5) THEN
         az = 0.d0
         IF (dec .LT. lat) az = 1.8d2
         RETURN
      END IF
      aznum = -DCOS(dec) * DSIN(ha)
      azdenom = DCOS(lat) * DSIN(dec) - DSIN(lat) * DCOS(dec) * DCOS(ha)
      az = DATAN2(aznum,azdenom)

C   Put azimuths in appropriate angular range for plotting based on
C    relative LAT/DECL of source & station
C
      IF (dec .GT. lat) THEN
         IF (az .LT. -pi) az = az + twopi
         IF (az .GT. pi) az = az - twopi
      ELSE
         IF (az .LT. 0.d0) az = az + twopi
         IF (az .GT. twopi) az = az - twopi
      END IF

      RETURN
      END


C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==


      SUBROUTINE mmdd_doy (imon,idom,iyear, idoy)

C  Subroutine to calculate the DoY from MM/DD/YY;  needed to input
C    data from VLBI3 cards

      INTEGER imon, idom, iyear, idoy, idm(12)

      DATA idm/0,31,59,90,120,151,181,212,243,273,304,334/

      idoy = idm(imon) + idom
      IF ((MOD(iyear,4) .EQ. 0) .AND. (imon .GT. 2)) idoy=idoy+1
C  leap-year correction won't work >= 2100 (not a leap year);
C   (but years >2080 will already be interpreted as 19--)

      RETURN
      END
