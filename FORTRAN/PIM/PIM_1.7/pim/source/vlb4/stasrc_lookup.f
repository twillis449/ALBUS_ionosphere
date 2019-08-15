      SUBROUTINE stasrc_lookup (maxsta,maxsrc,nsta,nsrc,stav,srcv,
     1                            mexptype, psrc,psta, 
     2                            rav,decv, latv,lonv)

C  Subroutine to extract the RA&Dec for each of the sources and
C  Lat&Lon for each of the stations from separate user-built
C  catalogues

      INTEGER maxsta,maxsrc, nsta,nsrc, j,k, mexptype
      INTEGER irh,irm,idd,idm, iltd,iltm,ilgd,ilgm
      REAL*8 rav(maxsrc),decv(maxsrc), latv(maxsta),lonv(maxsta)
      REAL*8 rs,ds, lts,lgs, pi,degrad
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER stav(maxsta)*1, srcv(maxsrc)*8, psrc*256,psta*256
      CHARACTER src*8,dsgn*1, sta1*1,stanom*8,ltc*1,lgc*1, ans*1
      CHARACTER srcnom*8, src0*8
      PARAMETER (pi = 3.14159265358979323846d0)
      PARAMETER (degrad = pi/1.8d2)
C---------
C
C  MAXSTA,MAXSRC - Dimension-sizes of arrays
C  NSTA,NSRC - Actual number of stations,sources
C  STAV,SRCV - Arrays of station,source names
C  MEXPTYPE - The type of input file
C  PSRC,PSTA - Path+Filenames of source,station catalogues
C
C  RAV,DECV - Arrays of source coordinates
C  LATV,LONV - Arrays of station coordinates
C
C--------------
C
C  First, the RA & Dec for the sources
C
      OPEN (62,FILE=psrc,STATUS='old')
      DO 110 k=1,nsrc

C    If from VLBI3 cards, query identity of source "codes"
C
 111      IF (mexptype .EQ. 3) THEN
             WRITE (*,299) srcv(k)
             READ (*,'(a8)') srcnom
             src0 = srcv(k)
             srcv(k) = srcnom
          END IF
 299      FORMAT ('    VLBI3-card source ',a2,'  = ',$)

          REWIND (62)
 112      READ (62,119,END=115) src,irh,irm,rs,dsgn,idd,idm,ds
          IF (src .EQ. srcv(k)) THEN
          IF ( (irh .GE. 24) .OR. (irm .GE. 60) .OR. (rs .GT. 6.d1) .OR.
     1         (idd .GE. 91) .OR. (idm .GE. 60) .OR. (ds .GT. 6.d1) .OR.
     2         ((dsgn .NE. '-') .AND. (dsgn .NE. '+')) ) THEN
             WRITE (*,'(/,a)') 'Format problem in the SOURCE catalogue'
             STOP
          END IF
              rav(k) = DBLE(irh) + DBLE(irm)/6.d1 + rs/3.6d3
              rav(k) = rav(k) * degrad * 1.5d1
              decv(k) = DBLE(idd) + DBLE(idm)/6.d1 + ds/3.6d3
              decv(k) = decv(k) * degrad
              IF (dsgn .EQ. '-') decv(k) = -decv(k)
              GOTO 110
          END IF
          GOTO 112
 115      WRITE (*,118) srcv(k)
 118      FORMAT (/,'Source ',a8,' not found in src.cat')
          IF (mexptype .EQ. 3) THEN
             WRITE (*,'(a,$)') '   Try again [y/n]? '
             READ (*,'(a1)') ans
             IF ((ans .EQ. 'y') .OR. (ans .EQ. 'Y')) THEN
                srcv(k) = src0
                GOTO 111
             END IF
          END IF
          STOP
 110  CONTINUE
 119  FORMAT (a8, 2x,2(i2,1x),f7.4,3x,a1,2(i2,1x),f7.4)
      CLOSE (62)

      WRITE (*,'(a1)') ' '

C...And then the Lat & Long for the stations 
C
      OPEN (63,FILE=psta,STATUS='old')
      DO 120 j=1,nsta
          REWIND (63)
 122      READ (63,129,END=125) sta1,stanom,iltd,iltm,lts,ltc,
     1                            ilgd,ilgm,lgs,lgc
          IF (sta1 .EQ. stav(j)) THEN
          IF ( (iltd.GE.90) .OR. (iltm.GE.60) .OR. (lts.GT.6.d1) .OR.
     1         (ilgd.GE.360) .OR. (ilgm.GE.60) .OR. (lgs.GT.6.d1) .OR.
     2         ((ltc .NE. 'S') .AND. (ltc .NE. 'N')) .OR.
     3         ((lgc .NE. 'W') .AND. (lgc .NE. 'E')) ) THEN
             WRITE (*,'(/,a)') 'Format problem in the STATION catalogue'
             STOP
          END IF
              latv(j) = DBLE(iltd) + DBLE(iltm)/6.d1 + lts/3.6d3
              latv(j) = latv(j) * degrad
              IF (ltc .EQ. 'S') latv(j) = -latv(j)
              lonv(j) = DBLE(ilgd) + DBLE(ilgm)/6.d1 + lgs/3.6d3
              lonv(j) = lonv(j) * degrad
              IF (lgc .EQ. 'W') lonv(j) = -lonv(j)
              WRITE (*,127) stav(j),stanom
 127          FORMAT ('    Found station ',a1,' == ',a8)
              GOTO 120
          END IF
          GOTO 122
 125      WRITE (*,128) stav(j)
 128      FORMAT (/,'Station ',a1,' not found in  sta.cat')
          STOP
 120  CONTINUE
 129  FORMAT (a1,1x,a8, 1x,2(i2,1x),f6.3,a1,3x,i3,1x,i2,1x,f6.3,a1)
      CLOSE (63)

C	check that these are the desired stations -- the export only
C	provides 1-letter for the station; ambiguities may/will arise
C	   (GreenBank 140' vs. 85';  Richmond vs. Brewster; ...)
C
      WRITE (*,'(a,$)') 'Are these Letter-Station assignments right [y/n
     1]? '    
      READ (*,'(a1)') ans
      IF ((ans .NE. 'y') .AND. (ans .NE. 'Y')) THEN
          WRITE (*,'(/,a)') '*** Edit  sta.cat ***'
          STOP
      END IF

      RETURN
      END
