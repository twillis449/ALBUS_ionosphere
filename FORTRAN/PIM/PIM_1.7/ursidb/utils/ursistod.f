      PROGRAM URSISD
C
C  Converts the formatted sequential URSI coefficients file to an unformatted
C  direct-access file
C
      INTEGER I
      INTEGER J
      INTEGER LS(4)
      INTEGER LK(4)
      INTEGER IM
      INTEGER IREC
      INTEGER L
      INTEGER M
      REAL FM3(9,49,2)
      REAL F2COEF(4)
      CHARACTER*1 BORL
      CHARACTER*30 SEQFIL,DAFIL
      INTEGER KF(10)
      INTEGER KX(10)
C
   10 FORMAT(A80)
   20 FORMAT(1X,10I5,29X)
   30 FORMAT(4(2I2,E14.7),8X)
   40 FORMAT(10I6)
   50 FORMAT(6(E12.6,1X))
C
C  Get the record-length specification for unformatted direct-access files
C
      PRINT *,'In OPEN statements, are record-lengths of unformatted ',
     &        'direct-access files'
      PRINT *,'specified in bytes or longwords (a longword is a ',
     &        '4-byte unit) ([B]/L)? '
      READ 5,BORL
    5 FORMAT(A)
C
C  Open a log file
C
      OPEN(UNIT=3,FILE='ursistod.log',STATUS='UNKNOWN')
C
C Open the sequential URSI coefficients database
C
      SEQFIL = 'ursi88.dat'
      OPEN(UNIT=1,FILE=SEQFIL,STATUS='OLD')
      PRINT *,'"',SEQFIL,'" opened for input.'
      WRITE(3,*) '"',SEQFIL,'" opened for input.'
C
C  Open the new URSI direct access file
C
      DAFIL = 'ursi88da.dat'
      IF((BORL .EQ. 'L') .OR. (BORL .EQ. 'l')) THEN
         OPEN(UNIT=2,FILE=DAFIL,STATUS='UNKNOWN',ACCESS='DIRECT',
     &        FORM='UNFORMATTED',RECL=12)
      ELSE
         OPEN(UNIT=2,FILE=DAFIL,STATUS='UNKNOWN',ACCESS='DIRECT',
     &        FORM='UNFORMATTED',RECL=12*4)
      ENDIF
      PRINT *,'"',DAFIL,'" opened for output.'
      WRITE(3,*) '"',DAFIL,'" opened for output.'
C
C  Read the sequential database month by month
C
      IREC = 0
      DO IM=1,12              ! 12 months
        PRINT *,'Processing month ',IM,', IREC=',IREC
        WRITE(3,*) 'Processing month ',IM,', IREC=',IREC
C
C  Read in URSI foF2 coefficients for SSN=0
C
        READ(1,20,ERR=990) (KF(I),I=1,10)
        IF (KF(1) .NE. 11) GO TO 990
        IREC = IREC + 1
        WRITE(2,REC=IREC) (KF(I),I=1,10)
        IF (IM .EQ. 10) THEN
          WRITE(3,*) 'IM,IREC,KF=',IM,IREC,KF
          PRINT *, 'IM,IREC,KF=',IM,IREC,KF
        ENDIF
        DO M=1,988,4
          READ(1,30,ERR=990) (LS(I),LK(I),F2COEF(I),I=1,4)
          IREC = IREC + 1
          WRITE(2,REC=IREC) (LS(I),LK(I),F2COEF(I),I=1,4)
        ENDDO
        PRINT *,'  sunspot 0 complete: IREC = ',IREC
        WRITE(3,*) '  sunspot 0 complete: IREC = ',IREC
C
C  Read in URSI foF2 coefficients for SSN=100
C
        READ(1,20,ERR=990) (KF(I),I=1,10)
        IF (KF(1) .NE. 11) GO TO 990
        IREC = IREC + 1
        WRITE(2,REC=IREC) (KF(I),I=1,10)
        DO M=1,988,4
          READ(1,30,ERR=990) (LS(I),LK(I),F2COEF(I),I=1,4)
          IREC = IREC + 1
          WRITE(2,REC=IREC) (LS(I),LK(I),F2COEF(I),I=1,4)
        ENDDO
        PRINT *,'  sunspot 100 complete: IREC = ',IREC
        WRITE(3,*) '  sunspot 100 complete: IREC = ',IREC
      ENDDO
      PRINT *,'foF2 coefficients complete.'
      WRITE(3,*) 'foF2 coefficients complete.'
C
C  Read in URSI M(3000) coefficients
C
      DO IM=1,12
        PRINT *,'M(3000): month ',IM,', IREC=',IREC
        WRITE(3,*) 'M(3000): month ',IM,', IREC=',IREC
        READ(1,40,ERR=990) (KX(I),I=1,10)
        IF (KX(1) .NE. 6) GO TO 990
        IREC = IREC + 1
        WRITE(2,REC=IREC) (KX(I),I=1,10)
        READ(1,50,ERR=990) (((FM3(I,J,L),I=1,9),J=1,49),L=1,2)
        DO L=1,2
          DO J=1,49
            IREC = IREC + 1
            WRITE(2,REC=IREC) (FM3(I,J,L),I=1,9)
          ENDDO
        ENDDO
        PRINT *,'  final IREC = ',IREC
        WRITE(3,*) '  final IREC = ',IREC
      ENDDO
C
      CLOSE(UNIT=1)
      PRINT *,' sequential file closed'
      WRITE(3,*) ' sequential file closed'
      CLOSE(UNIT=2)
      PRINT *,' direct-access file closed'
      WRITE(3,*) ' direct-access file closed'
C
      STOP 'URSI sequential converted to direct'
C
  990 CONTINUE
      PRINT *,' ERR: BAD READ FROM URSI-88 COEFFICIENT DATABASE'
      PRINT *,'IREC = ',IREC
      STOP
      END
