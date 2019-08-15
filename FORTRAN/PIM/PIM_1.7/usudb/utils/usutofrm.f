      PROGRAM USUFRM
C
C  PURPOSE
C     To convert USU (high-latitude E- and F- region) PIM database files from
C     unformatted to formatted.
C
C  METHOD
C     The user is asked for the root name of an unformatted USU PIM database
C     file, the root name of the formatted USU PIM database files to be
C     created, and the ion.  The appropriate USU PIM database files are
C     converted from unformatted (binary) to formatted (text).
C
C  INPUT PARAMETERS
C     NONE
C
C  OUTPUT PARAMETERS
C     NONE
C
C  LOCAL VARIABLES
C     BORL     The answer to the record-length specification question
C     EXT      A file extension
C     FILE     A file name
C     I        A loop counter
C     ION      The ion descriptor
C     J        A loop counter
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
C     Unit 1   Unformatted USU PIM database files
C     Unit 2   Formatted USU PIM database files
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.1   20-October-1994
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        24-Mar-1991   1.0 ==> Created
C     L. Brown        20-Oct-1994   1.0 ==> 1.1
C                                   Major rewrite to be consistent with the
C                                   conversion utilities of the other regional
C                                   parameterized models.
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER I,J
      REAL REC(37)
      CHARACTER*1 BORL,YORN
      CHARACTER*2 ION
      CHARACTER*3 SPEC
      CHARACTER*4 EXT
      CHARACTER*8 ROOTU,ROOTF
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
C  Get the root name of the unformatted files
C
  100 PRINT *,'What is the root name of the unformatted files? '
      READ 10,ROOTU
C
C  Get the root name of the formatted files to be generated
C
      PRINT *,'What is the root name of the formatted files to be ',
     &        'generated? '
      READ 10,ROOTF
C
C  Get the ion
C
  200 PRINT *,'What is the ion, NO+, O2+, or O+ [NO+]? '
      READ 10,SPEC
      CALL STRUCA(SPEC,I)
      IF(SPEC .EQ. 'O2+') THEN
         ION='O2'
      ELSE IF(SPEC .EQ. 'O+') THEN
         ION='O '
      ELSE
         ION='NO'
      ENDIF
C
C  Open the unformatted EOF file.  Note that in the VAX Fortran implementation,
C  the record-length of unformatted direct-access files (specified by the RECL
C  keyword) is given in longwords (4 byte units), corresponding to the space
C  required for an integer*4 or a real*4.  In other implementations, the
C  record-length might be given in bytes.
C
      PRINT *,'Processing the EOF file ...'
      CALL STRCCT('.'//ION,'E',32,EXT,I)
      CALL STRCCT(ROOTU,EXT,32,FILE,I)
      CALL STRLCA(FILE,I)
      IF(BORL .EQ. 'L') THEN
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=37)
      ELSE
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=37*4)
      ENDIF
C
C  Open the formatted EOF file
C
      CALL STRCCT(ROOTF,EXT,32,FILE,I)
      CALL STRLCA(FILE,I)
      OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='FORMATTED')
C
C  Read each record in the unformatted EOF file and write it to the formatted
C  EOF file
C
      READ(1,REC=1) (REC(I),I=1,2)
      WRITE(2,300) (REC(I),I=1,2)
  300 FORMAT(1P5E14.7)
      READ(1,REC=2) (REC(I),I=1,3)
      WRITE(2,300) (REC(I),I=1,3)
      READ(1,REC=3) (REC(I),I=1,1)
      WRITE(2,300) (REC(I),I=1,1)
      READ(1,REC=4) REC
      WRITE(2,300) REC
      READ(1,REC=5) REC
      WRITE(2,300) REC
      READ(1,REC=6) REC
      WRITE(2,300) REC
      DO 310 I=1,37
         READ(1,REC=6+I) REC
         WRITE(2,300) REC
  310 CONTINUE
C
C  Close the unformatted EOF file
C
      CLOSE(UNIT=1)
C
C  Close the formatted EOF file
C
      CLOSE(UNIT=2)
C
C  Open the unformatted orthogonal polynomial coefficients file.  Note that in
C  the VAX Fortran implementation, the record-length of unformatted direct-
C  access files (specified by the RECL keyword) is given in longwords (4 byte
C  units), corresponding to the space required for an integer*4 or a real*4.
C  In other implementations, the record-length might be given in bytes.
C
      PRINT *,'Processing the orthogonal polynomial coefficients ',
     &        'file ...'
      CALL STRCCT('.'//ION,'P',32,EXT,I)
      CALL STRCCT(ROOTU,EXT,32,FILE,I)
      CALL STRLCA(FILE,I)
      IF(BORL .EQ. 'L') THEN
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=9)
      ELSE
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=9*4)
      ENDIF
C
C  Open the formatted orthogonal polynomial coefficients file
C
      CALL STRCCT(ROOTF,EXT,32,FILE,I)
      CALL STRLCA(FILE,I)
      OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='FORMATTED')
C
C  Read each record in the unformatted orthogonal polynomial coefficients file
C  and write it to the formatted orthogonal polynomial coefficients file
C
      READ(1,REC=1) (REC(I),I=1,3)
      WRITE(2,300) (REC(I),I=1,3)
      READ(1,REC=2) (REC(I),I=1,4)
      WRITE(2,300) (REC(I),I=1,4)
      READ(1,REC=3) (REC(I),I=1,2)
      WRITE(2,300) (REC(I),I=1,2)
      READ(1,REC=4) (REC(I),I=1,3)
      WRITE(2,300) (REC(I),I=1,3)
      READ(1,REC=5) (REC(I),I=1,3)
      WRITE(2,300) (REC(I),I=1,3)
      READ(1,REC=6) (REC(I),I=1,3)
      WRITE(2,300) (REC(I),I=1,3)
      DO 400 J=1,12*6*9
         READ(1,REC=6+J) (REC(I),I=1,9)
         WRITE(2,300) (REC(I),I=1,9)
  400 CONTINUE
C
C  Close the unformatted orthogonal polynomial coefficients file
C
      CLOSE(UNIT=1)
C
C  Close the formatted orthogonal polynomial coefficients file
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
C  Ask the user whether they want to convert another file set
C
      PRINT *,'Do you want to convert another file set (Y/[N])? '
      READ 10,YORN
      CALL STRUCA(YORN,I)
      IF(YORN .EQ. 'Y') GOTO 100
C
      END
