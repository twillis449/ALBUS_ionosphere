      PROGRAM LLFFRM
C
C  PURPOSE
C     To convert LLF (Low-Latitude F-region) PIM database files from
C     unformatted to formatted.
C
C  METHOD
C     The user is asked for the root name of an unformatted LLF PIM database
C     file and the root name of the formatted LLF PIM database files to be
C     created.  The appropriate LLF PIM database files are converted from
C     unformatted (binary) to formatted (text).
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
C     J        A loop counter
C     REC      A record of numbers from the database
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
C     Unit 1   Unformatted LLF PIM database files
C     Unit 2   Formatted LLF PIM database files
C
C  AUTHOR
C     Lincoln Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-964-7553
C
C  VERSION
C     1.3   23-May-1997
C
C  MODIFICATIONS
C     ----Person----  ----Date----  ----------------Description----------------
C     L. Brown        26-Jun-1992   1.0 ==> Created
C     L. Brown        29-Sep-1994   1.0 ==> 1.1
C                                   The structure of the orthgonal polynomial
C                                   coefficient file has changed to reflect the
C                                   elimination of Fourier fitting in UT in the
C                                   low-latitude F-region parameterized model
C                                   (LLF).
C                                   Calls to CONCAT replaced by calls to
C                                   STRCCT.
C                                   Call to UPCASE replaced by call to STRUCA.
C     L. Brown        20-Oct-1994   1.1 ==> 1.2
C                                   An initial question determines whether the
C                                   record-lengths of unformatted direct-access
C                                   files are specified in bytes or longwords
C                                   (4-byte units) in OPEN statements.
C                                   Calls to STRLCA insure that file names
C                                   are in lower-case letters.
C     L. Brown        23-May-1997   1.2 ==> 1.3
C                                   Increased record length of unformatted
C                                   orthogonal polynomial coefficient file from
C                                   11 to 15 longwords (4 byte units).
C
C  REFERENCES
C     NONE
C
C  SPECIAL CONSTANTS
C     NONE
C
      INTEGER I,J
      REAL REC(55)
      CHARACTER*1 BORL,YORN
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
C  Open the unformatted EOF file.  Note that in the VAX Fortran implementation,
C  the record-length of unformatted direct-access files (specified by the RECL
C  keyword) is given in longwords (4 byte units), corresponding to the space
C  required for an integer*4 or a real*4.  In other implementations, the
C  record-length might be given in bytes.
C
      PRINT *,'Processing the EOF file ...'
      CALL STRCCT(ROOTU,'.OE',32,FILE,I)
      CALL STRLCA(FILE,I)
      IF(BORL .EQ. 'L') THEN
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=55)
      ELSE
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=55*4)
      ENDIF
C
C  Open the formatted EOF file
C
      CALL STRCCT(ROOTF,'.OE',32,FILE,I)
      CALL STRLCA(FILE,I)
      OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='FORMATTED')
C
C  Read each record in the unformatted EOF file and write it to the formatted
C  EOF file
C
      READ(1,REC=1) (REC(I),I=1,2)
      WRITE(2,200) (REC(I),I=1,2)
  200 FORMAT(1P5E14.7)
      READ(1,REC=2) (REC(I),I=1,3)
      WRITE(2,200) (REC(I),I=1,3)
      READ(1,REC=3) (REC(I),I=1,1)
      WRITE(2,200) (REC(I),I=1,1)
      READ(1,REC=4) (REC(I),I=1,3)
      WRITE(2,200) (REC(I),I=1,3)
      READ(1,REC=5) (REC(I),I=1,3)
      WRITE(2,200) (REC(I),I=1,3)
      READ(1,REC=6) (REC(I),I=1,1)
      WRITE(2,200) (REC(I),I=1,1)
      READ(1,REC=7) (REC(I),I=1,55)
      WRITE(2,200) (REC(I),I=1,55)
      READ(1,REC=8) (REC(I),I=1,55)
      WRITE(2,200) (REC(I),I=1,55)
      READ(1,REC=9) (REC(I),I=1,55)
      WRITE(2,200) (REC(I),I=1,55)
      DO 210 J=1,55
         READ(1,REC=9+J) (REC(I),I=1,55)
         WRITE(2,200) (REC(I),I=1,55)
  210 CONTINUE
C
C  Close the unformatted EOF file
C
      CLOSE(UNIT=1)
C
C  Close the formatted EOF file
C
      CLOSE(UNIT=2)
C
C  Open the unformatted orthogonal polynomial coefficient file.  Note that in
C  the VAX Fortran implementation, the record-length of unformatted direct-
C  access files (specified by the RECL keyword) is given in longwords (4 byte
C  units), corresponding to the space required for an integer*4 or a real*4.
C  In other implementations, the record-length might be given in bytes.
C
      PRINT *,'Processing the orthogonal polynomial coefficient ',
     &        'file ...'
      CALL STRCCT(ROOTU,'.OP',32,FILE,I)
      CALL STRLCA(FILE,I)
      IF(BORL .EQ. 'L') THEN
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=15)
      ELSE
         OPEN(UNIT=1,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &        ACCESS='DIRECT',RECL=15*4)
      ENDIF
C
C  Open the formatted orthogonal polynomial coefficient file
C
      CALL STRCCT(ROOTF,'.OP',32,FILE,I)
      CALL STRLCA(FILE,I)
      OPEN(UNIT=2,FILE=FILE,STATUS='NEW',FORM='FORMATTED')
C
C  Read each record in the unformatted orthogonal polynomial coefficient file
C  and write it to the formatted orthogonal polynomial coefficient file
C
      READ(1,REC=1) (REC(I),I=1,2)
      WRITE(2,200) (REC(I),I=1,2)
      READ(1,REC=2) (REC(I),I=1,3)
      WRITE(2,200) (REC(I),I=1,3)
      READ(1,REC=3) (REC(I),I=1,1)
      WRITE(2,200) (REC(I),I=1,1)
      READ(1,REC=4) (REC(I),I=1,3)
      WRITE(2,200) (REC(I),I=1,3)
      READ(1,REC=5) (REC(I),I=1,3)
      WRITE(2,200) (REC(I),I=1,3)
      READ(1,REC=6) (REC(I),I=1,2)
      WRITE(2,200) (REC(I),I=1,2)
      READ(1,REC=7) (REC(I),I=1,1)
      WRITE(2,200) (REC(I),I=1,1)
      DO 300 J=1,48*9
         READ(1,REC=7+J) (REC(I),I=1,15)
         WRITE(2,200) (REC(I),I=1,15)
  300 CONTINUE
C
C  Close the unformatted orthogonal polynomial coefficient file
C
      CLOSE(UNIT=1)
C
C  Close the formatted orthogonal polynomial coefficient file
C
      CLOSE(UNIT=2)
C
C  Ask the user whether they want to convert another file set
C
      PRINT *,'Do you want to convert another file set (Y/[N])? '
      READ 10,YORN
      CALL STRUCA(YORN,I)
      IF(YORN .EQ. 'Y') GOTO 100
C
      END
