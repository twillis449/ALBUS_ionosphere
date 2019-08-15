      PROGRAM LMEUNF
C
C  PURPOSE
C     To convert LME (Low- and Mid- latitude E-region) PIM database files from
C     formatted to unformatted.
C
C  METHOD
C     The user is asked for the root name of a formatted LME PIM database
C     file, the root name of the unformatted LME PIM database files to be
C     created, and the ion.  The appropriate LME PIM database files are
C     converted from formatted (text) to unformatted (binary).
C
C  INPUT PARAMETERS
C     NONE
C
C  OUTPUT PARAMETERS
C     NONE
C
C  LOCAL VARIABLES
C     BORL     The answer to the record-length specification question
C     FILE     A file name
C     I        A loop counter
C     ION      The ion descriptor
C     J        A loop counter
C     NEOF     The number of EOF coefficients
C     NOPM1    The number of orthogonal polynomial coefficients
C     REC      A record of numbers from the database
C     SPEC     The ion species
C     ROOTF    The root name of a formatted database file
C     ROOTU    The root name of an unformatted database file
C     YORN     The answer to a yes/no question
C
C  SUBROUTINES REQUIRED
C     STRCCT   Concatenates two character strings
C     STRLCA   Converts upper-case letters in a character string to lower-case
C     STRUCA   Converts lower-case letters in a character string to upper-case
C
C  FILES ACCESSED
C     Unit 1   Formatted LME PIM database files
C     Unit 2   Unformatted LME PIM database files
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.2   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        26-Jun-1992   1.0 ==> Created
C     L. Brown         4-Oct-1994   1.0 ==> 1.1
C                                   The structure of the orthgonal polynomial
C                                   coefficients file has changed to reflect
C                                   the elimination of Fourier fitting in
C                                   magnetic longitude in the low- and mid-
C                                   latitude E-region parameterized model
C                                   (LME).
C                                   The number of EOF coefficients and the
C                                   number of orthogonal polynomial
C                                   coefficients now depend on the ion
C                                   species.
C                                   Calls to CONCAT replaced by calls to
C                                   STRCCT.
C                                   Calls to UPCASE replaced by calls to
C                                   STRUCA.
C     L. Brown        20-Oct-1994   1.1 ==> 1.2
C                                   An initial question determines whether the
C                                   record-lengths of unformatted direct-access
C                                   files are specified in bytes or longwords
C                                   (4-byte units) in OPEN statements.
C                                   Calls to STRLCA insure that file names
C                                   are in lower-case letters.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER I,J,NEOF,NOPM1
      REAL REC(28)
      CHARACTER*1 BORL,YORN
      CHARACTER*2 ION
      CHARACTER*3 SPEC
      CHARACTER*8 ROOTF,ROOTU
      CHARACTER*40 FILE
C
C  Get the record-length specification for unformatted direct-access files
C
      PRINT *,'In OPEN statements, are record-lengths of unformatted ',
     &        'direct-access files'
      PRINT *,'specified in bytes or longwords (a longword is a ',
     &        '4-byte unit) ([B]/L)? '
      READ 10,BORL
   10 FORMAT(A)
      CALL STRUCA(BORL,I)
C
C  Get the root name of the formatted files
C
  100 PRINT *,'What is the root name of the formatted files? '
      READ 10,ROOTF
C
C  Get the root name of the unformatted files to be generated
C
      PRINT *,'What is the root name of the unformatted files to be ',
     &        'generated? '
      READ 10,ROOTU
C
C  Get the ion
C
  200 PRINT *,'What is the ion, NO+ or O2+ [NO+]? '
      READ 10,SPEC
      CALL STRUCA(SPEC,I)
      IF(SPEC .EQ. 'O2+') THEN
         ION='O2'
         NEOF=14
         NOPM1=19
      ELSE
         ION='NO'
         NEOF=7
         NOPM1=12
      ENDIF
C
C  Open the formatted EOF file
C
      PRINT *,'Processing the EOF file ...'
      CALL STRCCT(ROOTF,'.'//ION//'E',32,FILE,I)
      CALL STRLCA(FILE,I)
      OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='FORMATTED')
C
C  Open the unformatted EOF file.  Note that in the VAX Fortran implementation,
C  the record-length of unformatted direct-access files (specified by the RECL
C  keyword) is given in longwords (4 byte units), corresponding to the space
C  required for an integer*4 or a real*4.  In other implementations, the
C  record-length might be given in bytes.
C
      CALL STRCCT(ROOTU,'.'//ION//'E',32,FILE,I)
      CALL STRLCA(FILE,I)
      IF(BORL .EQ. 'L') THEN
         OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=28)
      ELSE
         OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=28*4)
      ENDIF
C
C  Read each record in the formatted EOF file and write it to the unformatted
C  EOF file
C
      READ(1,300) (REC(I),I=1,2)
  300 FORMAT(5E14.7)
      WRITE(2,REC=1) (REC(I),I=1,2)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=2) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=3) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=4) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=5) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,1)
      WRITE(2,REC=6) (REC(I),I=1,1)
      READ(1,300) REC
      WRITE(2,REC=7) REC
      READ(1,300) REC
      WRITE(2,REC=8) REC
      READ(1,300) REC
      WRITE(2,REC=9) REC
      DO 310 I=1,28
         READ(1,300) REC
         WRITE(2,REC=9+I) REC
  310 CONTINUE
C
C  Close the formatted EOF file
C
      CLOSE(UNIT=1)
C
C  Close the unformatted EOF file
C
      CLOSE(UNIT=2)
C
C  Open the formatted orthogonal polynomial coefficients file
C
      PRINT *,'Processing the orthogonal polynomial coefficients ',
     &        'file ...'
      CALL STRCCT(ROOTF,'.'//ION//'P',32,FILE,I)
      CALL STRLCA(FILE,I)
      OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='FORMATTED')
C
C  Open the unformatted orthogonal polynomial coefficients file.  Note that in
C  the VAX Fortran implementation, the record-length of unformatted direct-
C  access files (specified by the RECL keyword) is given in longwords (4 byte
C  units), corresponding to the space required for an integer*4 or a real*4.
C  In other implementations, the record-length might be given in bytes.
C
      CALL STRCCT(ROOTU,'.'//ION//'P',32,FILE,I)
      CALL STRLCA(FILE,I)
      IF(BORL .EQ. 'L') THEN
         OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=NOPM1+1)
      ELSE
         OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=(NOPM1+1)*4)
      ENDIF
C
C  Read each record in the formatted orthogonal polynomial coefficients file
C  and write it to the unformatted orthogonal polynomial coefficients file
C
      READ(1,300) (REC(I),I=1,2)
      WRITE(2,REC=1) (REC(I),I=1,2)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=2) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=3) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=4) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,3)
      WRITE(2,REC=5) (REC(I),I=1,3)
      READ(1,300) (REC(I),I=1,2)
      WRITE(2,REC=6) (REC(I),I=1,2)
      READ(1,300) (REC(I),I=1,1)
      WRITE(2,REC=7) (REC(I),I=1,1)
      DO 400 J=1,NEOF*24*12
         READ(1,300) (REC(I),I=1,NOPM1+1)
         WRITE(2,REC=7+J) (REC(I),I=1,NOPM1+1)
  400 CONTINUE
C
C  Close the formatted orthogonal polynomial coefficients file
C
      CLOSE(UNIT=1)
C
C  Close the unformatted orthogonal polynomial coefficients file
C
      CLOSE(UNIT=2)
C
C  Ask the user whether they want another ion
C
      PRINT *,'Do you want another ion (Y/[N])? '
      READ 10,YORN
      CALL STRUCA(YORN,I)
      IF(YORN .EQ. 'Y') GOTO 200
C
C  Ask the user if they want to convert another file set
C
      PRINT *,'Do you want to convert another file set (Y/[N])? '
      READ 10,YORN
      CALL STRUCA(YORN,I)
      IF(YORN .EQ. 'Y') GOTO 100
C
      END
