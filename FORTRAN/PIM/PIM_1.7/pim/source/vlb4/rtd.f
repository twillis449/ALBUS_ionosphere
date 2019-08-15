      SUBROUTINE gpsrta (maxsta, year,day,utdp, stav,jsta, flgps,
     1                     ngps, s,sigs, azgps,elgps)

C  Subroutine to calculate correction factors for all GPS SVN visible
C    from a station at a UT  (GPS RINEX files processed by GPSVOORPIM
C    into time-ordered output).
C      *)  TEC(isvn), SIGTEC(isvn) interpolated to VLBI-obs. epoch
C      *)  AZGPS(isvn), ELGPS(isvn)    "         "     "       "
C      *)  correction factors & uncertainties S(isvn), SIGS(isvn)
C            = TEC(gps) / TEC(pim)

      INCLUDE 'grid.inc'

      INTEGER maxgps,maxsta, ngps, year,day, jsta, maxsvn
      PARAMETER (maxgps=13, maxsvn=31)
      INTEGER iyrg,idyg,ihrg,iming, nsvn,nsvn0, malen(maxsvn)
      INTEGER svn, isvn,jep,kk
      REAL tec(maxgps),sigtec(maxgps), s(maxgps),sigs(maxgps)
      REAL tecint(maxsvn,3), sigtecint(maxsvn,3)
CREAL obslat,obslon
      REAL ut, ed(maxaltpt),rng(maxaltpt), edmax,rngmax
      REAL loslat(maxaltpt), loslon(maxaltpt), tecpim
      REAL*8 azgps(maxgps),elgps(maxgps)
      REAL*8 azgint(maxsvn,3), elgint(maxsvn,3)
      REAL*8 utdp, secg, utg, utgint(3), utfact,utf(3)
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER stav(maxsta)*1, flgps*256, dummc*1
C--------------------------------
C
C MAXGPS - Dimension-size of GPS-related arrays  (as PARAMETER instead
C            of passed because of local arrays TEC & SIGTEC)
C MAXSVN - "Maximum" SVN number (for indexing arrays via SVN number);
C             different from number of SVNs since some SVN#s skipped
C MAXSTA - Dimension-size of station array
C YEAR - year  (4-digits; 1981-2080)
C UTDP - hh.hhhhhh
C UT - REAL*4 UT in [s], for calling SLANTEDP
C FLGPS - Filename of GPS RTD file
C 
C NGPS - Actual number of contributing GPS SVNs
C S - Array of GPS correction factors  (TEC(gps)/TEC(pim))
C SIGS - Array of GPS c.f. uncertainties
C AZGPS - Array of GPS SVN azimuths
C ELGPS - Array of GPS SVN elevations
C
C SVN - the SVN # of an indivivdual satellite read from file
C MALEN - the number of times each SVN has data in the 3 epochs used
C          to interpolate to UTDP 
C
C ED - electron density at gridded Alt's along LoS (from SLANTEDP)
C EDMAX - maximum electron density encountered along LoS   (")
C RNG - range to gridded Alt's                             (")
C RNGMAX - range to EDMAX                                  (")
C LOSLAT - geographic latitude [deg] of samples along LoS  (")
C LOSLON - geographic longitude "    of  "       "     "   (")
C---------------------------------

C  Open GPS RTD file for this station
C   Existence of file checked earlier in PIMVLBI4
C
      OPEN (66, FILE=flgps, STATUS='old')

C  Read header record for receiver location 
C
      READ (66,109) obslat,obslon
 109  FORMAT (6x, 2f12.6)

C  Read epoch records, looking for 1st UT > UTDP  (incl. YYDDD)
C
      nsvn0 = 1
 110  READ (66,119,END=900) iyrg,idyg,ihrg,iming,secg, nsvn
 119  FORMAT (i4, 1x,i3, 1x,2i2, f5.1, i6)
 118  FORMAT (a1)
      IF (iyrg .EQ. 0) THEN
         WRITE (*,*) '  Somehow reached the end of the GPS RTD file'
         STOP
      END IF
      IF (iyrg .LT. year) THEN
         DO 121 kk=1,nsvn
            READ (66,118,END=900) dummc
 121     CONTINUE
         nsvn0 = nsvn
         GOTO 110
      END IF
      IF (idyg .LT. day) THEN
         DO 122 kk=1,nsvn
            READ (66,118,END=900) dummc
 122     CONTINUE
         nsvn0 = nsvn
         GOTO 110
      END IF
      utg = DBLE(ihrg)*3.6d3 + DBLE(iming)*6.d1 + secg
      utg = utg / 3.6d3
      IF (utg .LT. utdp) THEN
         DO 123 kk=1,nsvn
            READ (66,118,END=900) dummc
 123     CONTINUE
         nsvn0 = nsvn
         GOTO 110
      END IF

C  When we get here, we've found the first epoch-record in the GPS RTD
C    file that comes after the current VLBI-obs epoch
C  Backspace to the previous epoch-record, load GPS data for interp.
C
      DO 150 kk=1, nsvn0+2
         BACKSPACE (66)
 150  CONTINUE

      DO 160 isvn = 1, maxsvn
         malen(isvn) = 0 
         DO 165 jep = 1, 3
            tecint(isvn,jep) = -1.
            sigtecint(isvn,jep) = 0.
            azgint(isvn,jep) = 0.
            elgint(isvn,jep) = 0.
 165     CONTINUE
 160  CONTINUE

C  Load the three epochs (one before UTDP, two after) with which
C   to interpolate.  We're now using the minimum 30-min running
C   std.dev as the error in TEC (i.e., the "10x" before the "f10.5"
C   in the FORMAT.198)
C  Fill arrays at those SVN where data exists (a little wasteful of
C   space, but no separate indexing required).
C
      DO 180 jep = 1, 3
         READ (66,119,END=900) iyrg,idyg,ihrg,iming,secg, nsvn
         IF (iyrg .EQ. 0) THEN
            WRITE (*,*) '  Somehow reached the end of the GPS RTD file'
            STOP
         END IF
         utgint(jep) = DBLE(ihrg)*3.6d3 + DBLE(iming)*6.d1 + secg
         utgint(jep) = utgint(jep) / 3.6d3
         DO 190 isvn = 1, nsvn
            READ (66,199,END=900) svn
            malen(svn) = malen(svn) + 1
            BACKSPACE (66)
            READ (66,198) tecint(svn,jep),sigtecint(svn,jep),
     1                     elgint(svn,jep),azgint(svn,jep)
 190    CONTINUE
 180  CONTINUE
      CLOSE (66)
 199  FORMAT (i2)
 198  FORMAT (2x, f11.5, 10x,f10.5, 2f9.3)

C  Interpolate:  parabolicly if there's data in all three epochs
C                linearly if there's data in the 1st two epochs
C     Skip if neither of the above is true
C
      ngps = 0
      DO 210 isvn = 1, maxsvn

	if (malen(isvn) .GT. 0) then
cwrite (*,*) isvn, malen(isvn)
	end if

C      Not enough data to interpolate
         IF (malen(isvn) .LE. 1) GOTO 210

C      Enough for linear-interpolation (if it's the right 2 epochs) 
         IF (malen(isvn) .EQ. 2) THEN

C         if there's data in the 3rd epoch, there's not in one of the
C         first 2 --> skip
            IF (tecint(isvn,3) .GT. 0.) THEN
               malen(isvn) = 1
cwrite (*,*) '   MALEN=2 in wrong places'
               GOTO 210
            END IF

C         okay to linearly interpolate
            ngps = ngps + 1
            utfact = (utdp - utgint(1)) / (utgint(2)-utgint(1))
            tec(ngps) = (tecint(isvn,2) - tecint(isvn,1))*utfact
     1                    + tecint(isvn,1)
            sigtec(ngps) = (sigtecint(isvn,2)-sigtecint(isvn,1))*utfact
     1                        + sigtecint(isvn,1)
            elgps(ngps) = (elgint(isvn,2) - elgint(isvn,1))*utfact
     1                      + elgint(isvn,1)
            azgps(ngps) = (azgint(isvn,2) - azgint(isvn,1))*utfact
     1                      + azgint(isvn,1)
cwrite (*,*) '   Linearly interp:'
cwrite (*,*) tec(ngps),sigtec(ngps),elgps(ngps),azgps(ngps)
            GOTO 210
         END IF

C      Enough for parabolic interpolation
         IF (malen(isvn) .GT. 3) THEN
            WRITE (*,*) 'Somehow more than 3 interpolating epochs'
            STOP
         END IF
         ngps = ngps + 1
	
         utf(1) = (utdp-utgint(2)) * (utdp-utgint(3)) /
     1               ((utgint(1)-utgint(2)) * (utgint(1)-utgint(3)))   
         utf(2) = (utdp-utgint(1)) * (utdp-utgint(3)) /
     1               ((utgint(2)-utgint(1)) * (utgint(2)-utgint(3)))   
         utf(3) = (utdp-utgint(1)) * (utdp-utgint(2)) /
     1               ((utgint(3)-utgint(1)) * (utgint(3)-utgint(2)))   

         tec(ngps) = utf(1) * tecint(isvn,1)
         sigtec(ngps) = utf(1) * sigtecint(isvn,1)
         elgps(ngps) = utf(1) * elgint(isvn,1)
         azgps(ngps) = utf(1) * azgint(isvn,1)
         DO 240 kk=2,3
            tec(ngps) = tec(ngps) + utf(kk) * tecint(isvn,kk)
            sigtec(ngps) = sigtec(ngps) + utf(kk) * sigtecint(isvn,kk)
            elgps(ngps) = elgps(ngps) + utf(kk) * elgint(isvn,kk)
            azgps(ngps) = azgps(ngps) + utf(kk) * azgint(isvn,kk)
 240     CONTINUE
cwrite (*,*) '   Parabolicly interp:'
cwrite (*,*) tec(ngps),sigtec(ngps),elgps(ngps),azgps(ngps)

 210  CONTINUE

C  Loop over each SVN:
C     Test for no interpolatable SVN
C     Prep by loading necessary GRID.INC variables 
C     Call SLANTEDP to get electron densities
C     Call TECCLC to integrate TEC  (TECFR unnec. since no Farad.Rot)
C
cwrite (*,*) ngps
      IF (ngps .EQ. 0) THEN
         ngps =1
         RETURN
      END IF

      ut = SNGL(utdp*3.6d3)
      DO 310 isvn = 1, ngps
         saz = SNGL(azgps(isvn))
         sel = SNGL(elgps(isvn))

         CALL slantedp (year,day,ut, .FALSE.,0, stav(jsta),'GPS',
     1                    ed,rng, edmax,rngmax, loslat,loslon)
         CALL tecclc (nalt, rng,ed, tecpim)
         tecpim = tecpim * 1.e-12 

         s(isvn) = tec(isvn) / tecpim
         sigs(isvn) = sigtec(isvn) / tecpim
cwrite (*,*) tecpim, s(isvn), sigs(isvn)
 310  CONTINUE
      
      RETURN

 900  WRITE (*,*) '  Somehow trying to read past end of GPS RTD file'
      STOP

      END
