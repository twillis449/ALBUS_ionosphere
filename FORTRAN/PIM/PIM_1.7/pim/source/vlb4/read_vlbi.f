      SUBROUTINE read_export (maxut,maxsta,maxsrc, nut,nsta,nsrc,
     1                          yrdyv,utv,epv, stav,srcv,gooddat,
     2                          mexptype)

C  Subroutine to read MkIII export file  (HP FRNGX or HOPS frngx2)
C
      INTEGER maxut,maxsta,maxsrc
      INTEGER mexptype, iyear,idoy,ihr,imin, yrdyv(maxut),yrdy
      INTEGER iut,jsta,ksrc, nut,nsta,nsrc, ilgn,i,j,k, jsta1,jsta2 
      INTEGER imon,idom
      REAL*8 sec, utv(maxut),utd, epv(maxut)
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER fexp*256, fmtexp*46, src*8,sta1*1,sta2*1, dumm*1 
      CHARACTER stav(maxsta)*1, srcv(maxsrc)*8, a1*1,a8*8
      LOGICAL newflg, deja
      LOGICAL*1 gooddat(maxut,maxsta,maxsrc)
      a1 = '1'
      a8 = '12345678'
C------------
C
C  MAXUT,MAXSTA,MAXSRC - Dimension-sizes of arrays
C 
C  NUT,NSTA,NSRC - Actual sizes of data arrays
C  YRDYV - array of YYDDD   to pass back
C  UTV - array of HH.HHHH       "
C  STAV - array of stations     "
C  SRCV - array of sources      "
C  GOODDAT - whether the specified UT-STA-SRC combination exists in
C               the export file
C
C  MEXPTYPE - whether HP-FRNGX, HOPS-frngx2, or VLBI3 cards
C  IYEAR,IDOY,IHR,IMIN,SRC,STA1,STA2,SEC - read from export
C  UTD,YRDY,EPV - local variables helping to form YRDYV & UTV
C  NEWFLG,DEJA - logicals determining whether each UT/STA/SRC has
C                  already been loaded into its appropriate array
C  
C ------------
C
 4    WRITE (*,'(a,$)') 'Input  data file:  '
      READ (*,'(a)') fexp
      OPEN (61, FILE=fexp, STATUS='old', ERR=6)
      GOTO 8
 6    WRITE (*,'(/,a)') '  Requested export file not found'
      GOTO 4

 8    WRITE (*,19) 'Is this [1] an HP-1000 FRNGX mode 302/402 export,',
     1             '        [2] a hops frngx2 mode 902/402 export,',
     2             '     or [3] VLBI3 cards via VLBAFILE?'
 19   FORMAT (/,a,/,a,/,a)
      WRITE (*,'(a,$)') 'Input file type:  '
      READ (*,*) mexptype
      IF ((mexptype .LT. 1) .OR. (mexptype .GT. 3)) THEN
         WRITE (*,'(/,a)') ' Unsupported export-file format'
         GOTO 8
      END IF
      fmtexp = '(33x,i2,1x,i3, 1x,2i2, 1x,a8, 1x,2a1, 2x,f4.1)'
      IF (mexptype .EQ. 2)
     1     fmtexp = '(34x,i2,1x,i3, 1x,2i2, 1x,a8, 1x,2a1, 2x,f4.1)'
      IF (mexptype .EQ. 3)
     1     fmtexp = '(2a1, 1x,a2, 1x,3i2, 2i2, f2.0)'

C---------------------------------------
C  Load arrays containing the epochs (YYDDD, HH.HHHH), sources, and
C  stations represented in the export
C
C	load initial element of arrays
C
      IF (mexptype .LE. 2) THEN
         READ (61,fmtexp) iyear,idoy,ihr,imin,src,sta1,sta2,sec
         READ (61,'(a1)') dumm
      ELSE
         READ (61,fmtexp) sta1,sta2,src,imon,idom,iyear,ihr,imin,sec
         CALL mmdd_doy (imon,idom,iyear, idoy)
      END IF

      yrdyv(1) = iyear*1000 + idoy
      utv(1) = DBLE(ihr)*3.6d3 + DBLE(imin)*6.d1 + sec
      stav(1) = sta1
      stav(2) = sta2
      srcv(1) = src
      iut = 1
      jsta = 2
      ksrc = 1

C	read subsequent export records, appending "newly-encountered"
C	epochs/stations/sources into the arrays; determine total number
C	of epochs/stations/sources represented in export file
C
 30   IF (mexptype .LE. 2) THEN
         READ (61,fmtexp,END=80) iyear,idoy,ihr,imin,src,sta1,sta2,sec
         READ (61,'(a1)') dumm
      ELSE
         READ (61,fmtexp,END=80) sta1,sta2,src,imon,idom,iyear,
     1                              ihr,imin,sec
         CALL mmdd_doy (imon,idom,iyear, idoy)
      END IF
        
      yrdy = iyear*1000 + idoy
      utd = DBLE(ihr)*3.6d3 + DBLE(imin)*6.d1 + sec

      newflg = .TRUE.
      DO 40 i=1,iut
         deja = ((utd .EQ. utv(i)) .AND. (yrdy .EQ. yrdyv(i)))
         IF (deja) newflg = .FALSE.
 40   CONTINUE
      IF (newflg) THEN 
            iut = iut + 1
            utv(iut) = utd
            yrdyv(iut) = yrdy
      END IF

      newflg = .TRUE.
      DO 50 j=1,jsta
         deja = (sta1 .EQ. stav(j))
         IF (deja) newflg = .FALSE. 
 50   CONTINUE
      IF (newflg) THEN
            jsta = jsta + 1
            stav(jsta) = sta1
      END IF
      newflg = .TRUE.
      DO 55 j=1,jsta
         deja = (sta2 .EQ. stav(j))
         IF (deja) newflg = .FALSE.
 55   CONTINUE
      IF (newflg) THEN 
         jsta = jsta + 1
         stav(jsta) = sta2
      END IF

      newflg = .TRUE.
      DO 60 k=1,ksrc
         deja = (src .EQ. srcv(k))
         IF (deja) newflg = .FALSE.
 60   CONTINUE
      IF (newflg) THEN
         ksrc = ksrc + 1
         srcv(ksrc) = src
      END IF

      GOTO 30

 80   nsta = jsta
      nsrc = ksrc
      nut = iut

C	sort the final arrays in increasing numerical/alphabetic order
C	  (uses NumRec straightforward insertion sort)
C       UTV now in [hr]
C       EPV a temp variable to allow single sort for both UTV & YRDYV
C
      DO 90 i=1,nut
         utv(i) = utv(i) / 3.6d3
         epv(i) = DBLE(yrdyv(i))+ utv(i)/2.4d1
 90   CONTINUE
     
C  Sort data arrays -- exports crossing from 1999-2000 will currently
C   not sort properly -- a matter of where the 1900 or 2000 is added
C   to the 2-digit year
C 
      CALL piksrt (nsrc, srcv, a8)
      CALL piksrt (nsta, stav, a1)
      CALL piksrt3 (nut, epv, yrdyv, utv) 

C  Assign the elements of the logical array GOODDAT
C	initialize
C
      WRITE (*,'(a)') 'Assigning  GOODDAT  array'
      DO 901 iut=1,nut
         DO 902 jsta=1,nsta
            DO 903 ksrc=1,nsrc
               gooddat(iut,jsta,ksrc) = .FALSE.
 903        CONTINUE
 902     CONTINUE
 901  CONTINUE
      REWIND (61)

C	Read through export file again, finding indices associated with
C	each observed scan
C
      ilgn=0
 910  ilgn=ilgn+1
      IF (mexptype .LE. 2) THEN
         READ (61,fmtexp,END=950) iyear,idoy,ihr,imin,src,sta1,sta2,sec
         READ (61,'(a1)') dumm
      ELSE
         READ (61,fmtexp,END=950) sta1,sta2,src,imon,idom,iyear,
     1                              ihr,imin,sec
         CALL mmdd_doy (imon,idom,iyear, idoy)
      END IF
      yrdy = iyear*1000 + idoy
      utd = DBLE(ihr)*3.6d3 + DBLE(imin)*6.d1 + sec
      utd = utd/3.6d3

      iut = -1
      DO 921 i=1,nut
         IF ((utd .EQ. utv(i)) .AND. (yrdy .EQ. yrdyv(i))) THEN
            iut = i
            GOTO 930
         END IF
 921  CONTINUE
      IF (iut .LE. 0) THEN
         WRITE (*,929) ilgn,yrdy,ihr,imin
         STOP
      END IF
 929  FORMAT (5x,'Couldnt find UT:  line ',i4,i9,i3.2,i2.2)

 930  jsta1 = -1
      jsta2 = -1
      DO 931 j=1,nsta
         IF (sta1 .EQ. stav(j)) jsta1 = j
         IF (sta2 .EQ. stav(j)) jsta2 = j
 931  CONTINUE
      IF ((MIN(jsta1,jsta2) .LE. 0) .OR. (jsta1 .EQ. jsta2)) THEN
         WRITE (*,939) ilgn,yrdy,ihr,imin,sta1,sta2
         STOP
      END IF
 939  FORMAT (5x,'Couldnt find STA:  line ',i4,i9,i3.2,i2.2,2x,2a1) 

 940  ksrc = -1
      DO 941 k=1,nsrc
         IF (src .EQ. srcv(k)) ksrc = k
 941  CONTINUE
      IF (ksrc .LE. 0) THEN
         WRITE (*,949) ilgn,yrdy,ihr,imin,src
         STOP
      END IF
 949  FORMAT (5x,'Couldnt find SRC:  line ',i4,i9,i3.2,i2.2,2x,a8)

      gooddat(iut,jsta1,ksrc) = .TRUE. 
      gooddat(iut,jsta2,ksrc) = .TRUE.
      GOTO 910

 950  CLOSE (61)
      WRITE (*,'(a,/)') ' End of Export file reached gracefully'

      RETURN
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==

      SUBROUTINE read_simul (maxut,maxsta,maxsrc, nut,nsta,nsrc,
     1                         yrdyv,utv, stav,srcv, gooddat)

C  Subroutine to set up uniformly-sampled simulation;
C   Info read interactively from terminal
     
      INTEGER maxut,maxsta,maxsrc, i,j,k, nut,nsta,nsrc
      INTEGER iyear,idoy,ihr,imin, idelm,lenghr, ndayyr
      INTEGER yrdyv(maxut), len,len1,len2
      REAL*8 utv(maxut)
      CHARACTER stav(maxsta)*1,sta1*1, srcv(maxsrc)*8, a1*1,a8*8
      CHARACTER instastr*(41), insrcstr*(81)
      LOGICAL*1 gooddat(maxut,maxsta,maxsrc)
      a1 = '1'
      a8 = '12345678'
C----------------
C
C  MAXUT,MAXSTA,MAXSRC - Dimension-sizes of arrays
C 
C  NUT,NSTA,NSRC - Actual sizes of arrays
C  YRDYV - array of YYDDD   to pass back
C  UTV - array of HH.HHHH       "
C  STAV - array of stations     "
C  SRCV - array of sources      "
C  GOODDAT - whether the specified UT-SRC-STA combination exists; since
C              this forms a uniformly-sampled simulation, GOODDAT is
C              entirely .TRUE. -- the test for El>0 comes later in
C              another subroutine
C
C  IYEAR,IDOY,IHR,IMIN - the initial time of the simulation
C  IDELM - the simulation sampling time [min]
C  LENGHR - the simulation duration [hr]
C  NDAYYR - # of days in this year (in case we need to simulate across
C                multiple days
C  INSTASTR,INSRCSTR - input strings for simulated stations/sources:
C                        current dimensions -> 40sta, 9src
C  LEN,LEN1,LEN2 - input source-string manipulating counters
C
C---------------------
C
 100  WRITE (*,'(a,$)') 'Input inital YR [yy], DOY, HR, MIN:  '
      READ (*,*) iyear, idoy, ihr, imin  
      IF (iyear .LT. 0 .OR. iyear .GT. 99) GOTO 100
      IF (idoy .LT. 0 .OR. idoy .GT. 366) GOTO 100
      IF (ihr .LT. 0 .OR. ihr .GT. 23) GOTO 100
      IF (imin .LT. 0 .OR. imin .GT. 59) GOTO 100
 105  WRITE (*,'(a,$)') '  ..and sampling time [min]:  '
      READ (*,*) idelm
      IF (idelm .LT. 1) GOTO 105
 110  WRITE (*,'(a,$)') '  ...and simulation duration [hr]:  '
      READ (*,*) lenghr
      IF (lenghr .LT. 0) GOTO 110
      nut = (lenghr*60) / idelm + 1

C  Test for leap year  (2000 is a leap year; century bug in 2081, hence
C                           no concern that 2100 isn't a leap year)
      ndayyr = 365
      IF (MOD(iyear,4) .EQ. 0) ndayyr = 366

      utv(1) = DBLE(ihr)*6.d1 + DBLE(imin)
      yrdyv(1) = iyear*1000 + idoy

C  Adjust UTV & YRDYV for possibility of entering new (UT) day;
C   UTV --> [hr] & build EPV for sorting in 2nd loop
C
      DO 150 i = 2, nut
         utv(i) = utv(i-1) + DBLE(idelm)
         IF (utv(i) .GE. 1.44d3) THEN
            utv(i) = utv(i) - 1.44d3
            idoy = idoy + 1
            IF (idoy .GT. ndayyr) THEN
               idoy = 1
               iyear = iyear + 1
               IF (iyear .GT. 99) iyear = 0
            END IF
         END IF
         yrdyv(i) = iyear*1000 + idoy
 150  CONTINUE

      DO 160 i = 1, nut 
         utv(i) = utv(i) / 6.d1
 160  CONTINUE

 200  WRITE (*,'(a,$)') 'Input list of stations (no spaces, end with /):
     1  '
      READ (*,'(a)') instastr
      i=0
 210  i=i+1
      IF (instastr(i:i) .EQ. ' ') GOTO 200
      IF (i .GT. (maxsta+1)) THEN 
         WRITE (*,'(a)') '    Reading past string looking for the /'
         GOTO 200
      END IF
      sta1 = instastr(i:i)
      IF (sta1 .NE. '/') THEN
         stav(i) = sta1
         GOTO 210
      END IF
      nsta = i-1
      CALL piksrt (nsta, stav, a1)

 250  WRITE (*,'(a,$)') 'Input list of sources (space-delimited, end wit
     1h /):  '
      READ (*,'(a)') insrcstr
      i=0
      len = INDEX(insrcstr,'/') - 1
      IF (len .EQ. -1) THEN
         WRITE (*,'(a)') '   Reading past string looking for the /'
         GOTO 250
      END IF
      len1 = 1
 260  i=i+1
      len2 = INDEX(insrcstr(len1:len),' ') - 2 + len1
      IF (len2 .NE. len1-2) THEN
         srcv(i) = insrcstr(len1:len2)
         len1 = len2+2
         GOTO 260
      END IF
      srcv(i) = insrcstr(len1:len)
      nsrc = i
      CALL piksrt (nsrc, srcv, a8)

C	load GOODDAT as all .T.
C
      DO 281 i=1,nut
         DO 282 j=1,nsta
            DO 283 k=1,nsrc
               gooddat(i,j,k) = .TRUE.
 283        CONTINUE
 282     CONTINUE
 281  CONTINUE

      RETURN
      END
