      SUBROUTINE read_kpf107 (pkpf,maxdays, yrdy0_in,yrdy1_in,
     1                          ndays,sf107,sap)

C  Subroutine to read F10.7 and Ap from datafiles downloaded from NOAA
C    First attempt is to read from final database, maintained locally
C      from the final-processed data (about 1-2 month lag)
C    Second, from preliminary data for the immediate past
C    Finally, from forecase data for the near future (~30d)
C  This subroutine only reads the data in from the file (so the database
C    only has to opened once), and then does the 27-d averaging for 
C    F10.7 and the 24-hr averaging for Ap
C
C  The Ap data read from file are stored in 1-D arrays of length
C    8*(Ndays+3), rather than 2-D arrays (Ndays+3,8), to make forming 
C    the running 24-hr averages more straightforward
C
C  The returned arrays span:
C    SF107:  The 27-d averaged F10.7 starting at 20UT (17UT .LT. Jun91)
C             of the UT day before the 1st UT day having any data,
C             and continuing to 20UT of the UT day after the last day
C             having any data.
C    SAP:  The 24-hr averaged ap, sampled every 3 hours, starting with
C           the 00-03UT sample on the UT day before the 1st UT day
C           having any data and continuing to the 21-24UT sample on the
C           UT day after the last day having any data.
C
      include 'logicuni.inc'
      INTEGER maxdays,istat, yrdy0,yrdy1, l,m,isamp
      INTEGER iyr0,idoy0,month0,idom0, iyr1,idoy1,month1,idom1
      INTEGER irec,nleapd,irec1,ndays, f107flg,kyr,kmo,kdy
      INTEGER rawap((14+3)*8)
      INTEGER rec_count, yrdy0_in, yrdy1_in
      REAL*8 rawf(14+28), sf107(maxdays+2)
      REAL*8 sap((maxdays+2)*8),s0
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER pkpf*256, flin*256, dumm*1
      INTEGER FIRST
      DATA FIRST/1/
      SAVE FIRST

C---------------
C
C  MAXDAY - Dimension-size of number of days in data/simul duration 
C  YRDY0,YRDY1 - 1st & last days of data
C  NDAYS - Number of UT days with any data
C  IREC - #days since 0Jan81 == record number in KPF107.DAT
C  RAWF - array of daily F10.7 read from file - explicitly DIMENSIONED
C           because it's not a passed array: assmume MAXDAYS=14
C  SF107 - array of 27-d averaged F10.7
C  RAWAP - array of 3-hrly Ap read from file, stored as 1-D vector
C            explicitly DIMENSIONED as was RAWF
C  SAP - array of 24-hr averaged 3-hrly Ap
C
C---------------
C
C  Parse 1st & last YRDYs
C
      yrdy0 = yrdy0_in
      yrdy1 = yrdy1_in
 100  CONTINUE
      iyr0 = yrdy0/1000
      iyr1 = yrdy1/1000
      idoy0 = yrdy0 - iyr0*1000
      idoy1 = yrdy1 - iyr1*1000
C     2005 Aug 31  James M Anderson --Make all years be the full years
c$$$      IF (iyr0 .LT. 81) THEN
c$$$         iyr0 = iyr0 + 2000
c$$$      ELSE
c$$$         iyr0 = iyr0 + 1900
c$$$      END IF
c$$$      IF (iyr1 .LT. 81) THEN
c$$$         iyr1 = iyr1 + 2000
c$$$      ELSE
c$$$         iyr1 = iyr1 + 1900
c$$$      END IF
      CALL TIMMDM(iyr0, idoy0, month0, idom0)
      CALL TIMMDM(iyr1, idoy1, month1, idom1)

C  Calculate record number in KPF107.DAT for day of 1st data,
C    adjusted for leap years (using IDOY takes care of leap-days in
C    the year IYR0)
C
      irec = (iyr0-1981)*365 + idoy0
      nleapd = (iyr0-1981)/4
      irec = irec + nleapd
      irec1 = (iyr1-1981)*365 + idoy1
      nleapd = (iyr1-1981)/4
      irec1 = irec1 + nleapd
      ndays = irec1 - irec + 1

      IF (irec .LT. 28) THEN
         WRITE (LUSTDERR,*)
     &        'Too close to 0 Jan 1981: can''t compute F10.7'
         STOP
      END IF
      IF (ndays .GT. 14) THEN
         WRITE (LUSTDERR,*) 'Currently limited to 2 weeks of data'
         STOP
      END IF

      CALL STRCCT (pkpf,'kpf107.dat',32,flin,istat)
C     WRITE (LUSTDERR,'(a)') '*******' 
C     WRITE (LUSTDERR,'(a)') flin 
      OPEN (LUKPF, FILE=flin, STATUS='old')

C  Position file to the day before the first one contributing to 
C   the 27d averages to parabolically interpolate
C
      rec_count = 0
C     WRITE (LUSTDERR,430) irec
C430  FORMAT (5x,'weather expected irec is ',I9)
      DO 105 l = 1, (irec-28)
         READ (LUKPF,'(a1)',END=150) dumm
         rec_count = rec_count + 1
 105  CONTINUE

C  Accumulate F10.7s to average, check FLUX QUALIFIERs & dates
C
      DO 120 l = 1, (28+ndays)
         READ (LUKPF,119,END=150)kyr,kmo,kdy,rawf(l),f107flg
         rec_count = rec_count + 1
#ifdef DEBUG
         IF (f107flg .NE. 0) WRITE (LUSTDERR,118) kyr,kmo,kdy, f107flg
         IF (l .EQ. 28) THEN
           IF ((kmo .NE. month0) .OR. (kdy .NE. idom0)) THEN
               WRITE (LUSTDERR,117) kyr,kmo,kdy
           END IF
         END IF
#endif
 120  CONTINUE
      GOTO 131
 119  FORMAT (3i2, 25x,27x, 7x,f5.1,i1)
 118  FORMAT ('Caut: Non-zero F10.7 Flag at ',3(i2.2,1x),' : ',i1)
 117  FORMAT (5x,'Mismatch reading day for F10.7:  ',i2,'-',i2.2,
     1                '-',i2.2)

C  Desired data is later than end of "final-processed" database, 
C    need to check "preliminary" file and/or "forecast" file
C
 150  CONTINUE
      IF(FIRST.EQ.1) THEN
      WRITE (LUSTDERR,'(/,a)') 
     &     '*** Requested dates past end of KPF107.DAT ***'
      WRITE (LUSTDERR,'(a)') 
     &   '    Preliminary and/or Forecast NOAA files should be checked'
      FIRST=0
      END IF
      CLOSE (LUKPF)
      irec1 = rec_count - 28 - ndays
      iyr1 = irec1/365 + 1981
      idoy1 = irec1-(iyr1-1981)*365
      yrdy1 = iyr1*1000+idoy1
      irec = irec1 - ndays + 1
      iyr0 = irec/365 + 1981
      idoy0 = irec-(iyr0-1981)*365
      yrdy0 = iyr0*1000+idoy0
      GOTO 100
      RETURN
C      STOP 'read_kpf107 data failure'
C      RETURN

C  Calculate the 27d averages to parabolically interpolate
C
 131  DO 130 l = 1, ndays+2
         sf107(l) = 0.0
         DO 135 m = 1, 27
            sf107(l) = sf107(l) + rawf(l+m-1)
 135     CONTINUE
         sf107(l) = sf107(l) / 27.0
 130  CONTINUE

         
C  Second, Kp:  obtain 24-hr averaged (8 of the 3-hr samples) &
C    parabolically interopolate -- 24-hr average chosen to reflect
C    time taken for magnetic changes to make themselves felt in
C    TECs in Midlat & Lowlat (not yet a well-defined relaxation
C    time - DTDecker).  Response times in Highlat (auroral regions)
C    is much quicker, so use of this average will not track rapid
C    e- density changes there.  And of course, PIM itself doesn't
C    attempt to model storms etal...
C
      REWIND (LUKPF)

C	Position file to the day before IREC; read in 3 days worth of ap
C
      DO 205 l = 1, (irec-3)
 205     READ (LUKPF, '(a1)') dumm

      DO 210 l = 1, ndays+3
         READ (LUKPF, 219) kyr, kmo, kdy, (rawap((l-1)*8+m),m=1,8)
#ifdef DEBUG
         IF (l .EQ. 3) THEN
            IF ((kmo .NE. month0) .OR. (kdy .NE. idom0)) THEN
               WRITE (LUSTDERR,217) kyr,kmo,kdy
            END IF
         END IF
#endif
 210  CONTINUE
 219  FORMAT (3i2, 25x, 8i3)
 217  FORMAT (5x,'Mismatch reading day for Ap:  ',i2,'-',i2.2,
     1                '-',i2.2)

C  Calculate the 24hr averages to parabolically interpolate
C    do this by keeping track of 8-sample running average of MAGAP
C
      s0 = 0.d0
      DO 250 l=1,8
         s0 = s0 + DBLE(rawap(l))
 250  CONTINUE

      DO 270 isamp = 1, (ndays+2)*8
         s0 = s0 + DBLE(rawap(isamp+8)) - DBLE(rawap(isamp))
         sap(isamp) = s0 / 8.d0
 275  CONTINUE
 270  CONTINUE

      CLOSE (LUKPF)
      RETURN
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==

      SUBROUTINE read_imf (pimf, yrdy0,yrdy1, ut0,ut1,
     1                       by,bysig,bz,bzsig, imfna)

C  Get values for the IMF By & Bz at the mid-point of the data
C   from the daily sampled GSFC/NSSDC files.  Note this isn't ideal
C   since the components can change sign more frequently than daily,
C   but doing so in PIM would cause discontinuities as, for example,
C   Bz changes from -/+ hence K_p changes from K_p/1.a
C     Query PLH about using coupling prop to sin^4(theta/2) as in
C      Hargreaves p200 in order to get a continuous effect on K_p.
C
C  If time of data is after file runs out, or if there is no IMF data
C    at the time of data, query how to treat the IMF parameters
C
      include 'logicuni.inc'
      INTEGER yrdy0,yrdy1, imfna, irec0,irec1,irec, i,l
#ifdef DEBUG
      INTEGER mback,mforw
#endif
      INTEGER iyr0,idoy0, iyr1,idoy1, ndays,nleapd
      REAL*8 by,bysig,bz,bzsig, ut0,ut1,utmid
#ifdef DEBUG
      REAL*8 x,x1,x2
#endif
      INTEGER LAST_GOOD_DATE
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER pimf*256, datstr*12, dumm*1
      DATA LAST_GOOD_DATE/2100000/
      SAVE LAST_GOOD_DATE
      INTEGER FIRST
      DATA FIRST/1/
      SAVE FIRST

C------------
C
C  YRDY0,YRDY1 - 1st & last days of data
C  UT0,UT1 - 1st & last UTs
C  BY,BZ - Values of IMF components  (not yet just the sign)
C  BYSIG,BZSIG - Uncertainty of By,Bz
C  IMFNA - Controls how to treat IMF parameters
C           0 = no problem reading IMF from file
C           1 = IMF N/A:  assume K_p=1
C           2 = IMF N/A:  run PIM once each for K_p=1 & K_p=K_p
C  IREC - # days since 0Jan81 of midpoint of data
C  UTMID - UT of midpoint of data
C
C------------
C
C  Parse 1st & last YRDYs
C
C     write (*,*) 'yrdy0 yrdy1 ',yrdy0,yrdy1
      IF(yrdy1.ge.LAST_GOOD_DATE) GOTO 150
      iyr0 = yrdy0/1000
      iyr1 = yrdy1/1000
      idoy0 = yrdy0 - iyr0*1000
      idoy1 = yrdy1 - iyr1*1000

         
C  Calculate record number in IMF24.DAT for day of 1st data,
C    adjusted for leap years (using IDOY takes care of leap-days in
C    the year IYR0)
C
      irec0 = (iyr0-1981)*365 + idoy0
      nleapd = (iyr0-1981)/4
      irec0 = irec0 + nleapd
      irec1 = (iyr1-1981)*365 + idoy1
      nleapd = (iyr1-1981)/4
      irec1 = irec1 + nleapd
      ndays = irec1 - irec0 + 1
C     write (*,*) 'irec0 irec1 ',irec0,irec1

C  Compute mean time of data
C
      utmid = (ut0+ut1)/2.d0 + DBLE(ndays-1)*1.2d1
      irec = irec0 + IDINT(utmid)/24
C     write (*,*) 'irec ',irec
      utmid = DMOD(utmid,2.4d1)

C     WRITE (LUSTDERR,'(a)') 
C    &     '*** opening IMF file ***'
C     WRITE (LUSTDERR,'(a)') pimf 
      OPEN (LUIMF, FILE=pimf, STATUS='old')

C  Position file to the day of IREC; read in "today" data
C    As of 2/2/00, format of daily-averaged data from NSSDC/GSFC has 
C       changed - fortunately, this just affects FORMAT-119 because
C       IMF file entered by day-index rather than parsing DATSTR
C       (ie assumes there is a record for every day since 1981jan01)
C
      DO 105 l = 1, (irec-1)
 105     READ (LUIMF, '(a1)',END=150) dumm
C     write (*,*) 'IMF data read at  ',irec
      DO 110 i = 1,1
        READ (LUIMF, 119,END=150) 
     &        datstr,by,bz,bysig,bzsig
 110  CONTINUE
C 119  FORMAT (a11,15x,2f6.1,12x,2f6.1)
 119  FORMAT (a12,15x,2f6.1,12x,2f6.1)
C  Test to see if the IMF data exist for this days; if so, all OK
C
      IF (by .LT. 5.d2) THEN
         imfna = 0
#ifdef DEBUG
         WRITE (LUSTDERR,448) by,bysig
         WRITE (LUSTDERR,449) bz,bzsig
#endif
         GOTO 900
      ELSE
         GOTO 160
      END IF

C  Date of data past end of IMF file, or missing data indicated in file
C
 150  CONTINUE
C     IF here, then no data exists for this date
      IF(yrdy1.lt.LAST_GOOD_DATE) THEN
         LAST_GOOD_DATE = yrdy1
      ENDIF
 160  CONTINUE
      by = 1.d0
      bysig = 0.d0
      IF(IMFNA.le.0) THEN
         bz = 1.d0
      ELSE
         bz = -1.0d0
      ENDIF
      bzsig = 0.d0
      IMFNA = 1
      IF (FIRST.EQ.1) THEN
      WRITE (LUSTDERR,'(/,a)') 
     &     '*** Requested dates past end of IMF24.DAT or bad data***'
      WRITE (LUSTDERR,450) irec
      WRITE (LUSTDERR,'(a)') 
     &   '    Using default values for bz field'
      FIRST = 0
      END IF
      GOTO 900



 448  FORMAT (5x,'IMF By =',f7.3,' +/- ',f5.3)
 449  FORMAT (5x,'IMF Bz =',f7.3,' +/- ',f5.3)
 450  FORMAT (5x,'failure to read valid data from record ',I9)

 900  CLOSE (LUIMF)
      RETURN
      END
