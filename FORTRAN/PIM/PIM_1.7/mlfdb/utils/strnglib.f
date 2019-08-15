C******************************************************************************
C  STRNGLIB is a FORTRAN library of string manipulations.  It contains the
C  following routines:
C
C     SUBROUTINE STRCCT [CONCAT] (C1,C2,IADC,C,ISTAT)
C                       Concatenates two character strings
C     SUBROUTINE STRDEL [DELETE] (C,CD,IADC,ISTAT)
C                       Deletes occurrences of a character string from another
C                       string
C     SUBROUTINE STRFLL [FILL  ] (C,IADC,ISTAT)
C                       Fills a character string with a specified character
C     SUBROUTINE STRINS [INSERT] (C,CI,IPOS,IADC,ISTAT)
C                       Inserts a character string into another string at a
C                       specified location
C     SUBROUTINE STRLCA [LOCASE] (C,ISTAT)
C                       Converts upper-case letters in a character string to
C                       lower-case letters
C     SUBROUTINE STRLEN [LENGTH] (C,IADC,L,ISTAT)
C                       Determines the length of a character string ignoring
C                       specified trailing characters
C     SUBROUTINE STRNPO [NTHPOS] (C,CF,NOCC,IADC,IPOS,ISTAT)
C                       Finds the Nth position of a character string within
C                       another string
C     SUBROUTINE STRRPL [REPLAC] (C,CF,CR,IADC,ISTAT)
C                       Replaces occurrences of a character string in a string
C                       with another string
C     SUBROUTINE STRSHC [SHIFTC] (C,IS,ISTAT)
C                       Cyclically shifts the contents of a character string
C     SUBROUTINE STRSHI [SHIFT ] (C,IS,IADC,ISTAT)
C                       Shifts the contents of a character string
C     SUBROUTINE STRTRM [TRIM  ] (C,IADC,ISTAT)
C                       Trims specified leading characters from a character
C                       string
C     SUBROUTINE STRUCA [UPCASE] (C,ISTAT)
C                       Converts lower-case letters in a character string to
C                       upper-case letters
C
C  They use the ANSI FORTRAN 77 standard intrinsic FORTRAN functions
C
C     len(C)     The length of the string C
C     char(n)    The character represented by ASCII decimal code n
C     ichar(c)   The ASCII decimal code representing the character c
C
C  but are independent of each other.  Names in square brackets are equivalent
C  names for routines, included for backward compatibility.  The two names may
C  be used interchangeably; the functionality is identical.
C******************************************************************************
      SUBROUTINE STRCCT(C1,C2,IADC,C,ISTAT)
C
C  PURPOSE
C     To concatenate two character strings.
C
C  METHOD
C     Two strings C1 and C2 are concatenated to produce the string C, ignoring
C     trailing characters in string C1 and leading characters in string C2
C     specified by ASCII decimal code IADC.  Any unused trailing elements in
C     string C are filled with characters specified by ASCII decimal code IADC.
C     The completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C1     Char*(*)   Scalar        n/a             n/a
C        The first string in the concatenation
C     C2     Char*(*)   Scalar        n/a             n/a
C        The second string in the concatenation
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty trailing
C        elements in string C1, empty leading elements in string C2, and unused
C        trailing elements in string C
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The concatenation of strings C1 and C2,  ignoring trailing characters
C        in string C1 and leading characters in string C2
C     ISTAT  Integer    Scalar        n/a             1 or -1
C        The completion status flag:
C            1, Successfully completed
C           -1, Illegal value of IADC; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     IC     Integer    Scalar        n/a             >= 0
C        The length of string C1 to assign to string C
C     IC2    Integer    Scalar        n/a             1 <= IC2 <= len(C2)+1
C        The position in string C2 of the first non-empty element
C     ICPJC1 Integer    Scalar        n/a             >= 1
C        IC+JC+1
C     JC     Integer    Scalar        n/a             >= 0
C        The length of string C2 to concatenate to string C1, ignoring leading
C        empty elements in string C2
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C     LC1    Integer    Scalar        n/a             0 <= LC1 <= len(C1)
C        The length of string C1, ignoring trailing empty elements
C     LC2    Integer    Scalar        n/a             len(C2)
C        The length of string C2
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IADC
      CHARACTER*(*) C1,C2
C
C  Output variables
C
      INTEGER ISTAT
      CHARACTER*(*) C
C
C  Local variables
C
      INTEGER LC1,LC2,IC2,LC,IC,JC,ICPJC1,I
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  Find the last non-empty element in string C1
C
         LC1=LEN(C1)
         CIADC=CHAR(IADC)
   10    IF(C1(LC1:LC1) .EQ. CIADC) THEN
            LC1=LC1-1
            IF(LC1 .GT. 0) GOTO 10
         ENDIF
C
C  Find the first non-empty element in string C2
C
         LC2=LEN(C2)
         IC2=1
  100    IF(C2(IC2:IC2) .EQ. CIADC) THEN
            IC2=IC2+1
            IF(IC2 .LE. LC2) GOTO 100
         ENDIF
C
C  Assign as much of string C1 to string C, ignoring trailing empty elements in
C  string C1, as possible without exceeding the length of string C
C
         LC=LEN(C)
         IC=MIN(LC1,LC)
         IF(IC .GT. 0) C(1:IC)=C1(1:IC)
C
C  Concatenate as much of string C2 to string C, ignoring leading empty
C  elements in string C2 and trailing empty elements in string C, as possible
C  without exceeding the length of string C
C
         IF(IC .LT. LC) THEN
            JC=MIN(LC2-IC2+1,LC-IC)
            IF(JC .GT. 0) C(IC+1:IC+JC)=C2(IC2:IC2+JC-1)
         ENDIF
C
C  Fill any unused trailing elements in string C with characters specified by
C  ASCII decimal code IADC
C
         ICPJC1=IC+JC+1
         IF(ICPJC1 .LE. LC) THEN
            DO 200 I=ICPJC1,LC
               C(I:I)=CIADC
  200       CONTINUE
         ENDIF
C
C  Set the completion status flag for a successful completion
C
         ISTAT=1
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRDEL(C,CD,IADC,ISTAT)
C
C  PURPOSE
C     To delete occurrences of a character string from another string.
C
C  METHOD
C     All occurrences of string CD are deleted from string C.  Trailing
C     characters in C and CD specified by ASCII decimal code IADC are ignored.
C     Any resulting freed trailing elements in string C are filled with
C     characters specified by ASCII decimal code IADC.  The completion status
C     is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be changed
C     CD     Char*(*)   Scalar        n/a             n/a
C        The string to be deleted from string C
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty trailing
C        elements in strings C and CD
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string with occurrences of CD removed
C     ISTAT  Integer    Scalar        n/a             >= -1
C       The completion status flag:
C          > 0, Successfully completed with ISTAT deletions
C            0, Successfully completed with no deletions; string C remains
C               unchanged
C           -1, Illegal value of IADC; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     IC     Integer    Scalar        n/a             >= 1
C        The starting position in string C to search for an occurrence of
C        string CD
C     ICLCD1 Integer    Scalar        n/a             >= 1
C        IC+LCD-1
C     ICPLCD Integer    Scalar        n/a             >= 2
C        IC+LCD
C     LC     Integer    Scalar        n/a             0 <= LC <= len(C)
C        The length of input string C, ignoring trailing empty elements
C     LCD    Integer    Scalar        n/a             0 <= LCD <= len(CD)
C        The length of string CD, ignoring trailing empty elements
C     LLC    Integer    Scalar        n/a             0 <= LLC <= len(C)
C        The length of string C, ignoring trailing empty elements
C     LLCLCD Integer    Scalar        n/a             >= 1
C        LLC-LCD
C     LLCP1  Integer    Scalar        n/a             1 <= LLCP1 <= len(C)+1
C        LLC+1
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IADC
      CHARACTER*(*) C,CD
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,LCD,LLC,IC,ICPLCD,ICLCD1,LLCLCD,LLCP1,I
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  Find the last non-empty element in string C
C
         LC=LEN(C)
         CIADC=CHAR(IADC)
   10    IF(C(LC:LC) .EQ. CIADC) THEN
            LC=LC-1
            IF(LC .GT. 0) GOTO 10
         ENDIF
C
C  Find the last non-empty element in string CD
C
         LCD=LEN(CD)
  100    IF(CD(LCD:LCD) .EQ. CIADC) THEN
            LCD=LCD-1
            IF(LCD .GT. 0) GOTO 100
         ENDIF
C
C  Initialize the completion status flag
C
         ISTAT=0
C
C  If the length of string C is greater than the length of string CD, ignoring
C  trailing empty elements in both, and if the lengths of both, ignoring
C  trailing empty elements, are greater than zero, then string C is searched
C  for occurrences of string CD
C
         IF((LC .GE. LCD) .AND. (LC .GT. 0) .AND. (LCD .GT. 0)) THEN
C
C  Find and delete every occurrence of string CD in string C
C
            LLC=LC
            IC=1
  200       ICPLCD=IC+LCD
            ICLCD1=ICPLCD-1
            IF(ICLCD1 .LE. LLC) THEN
               IF(C(IC:ICLCD1) .EQ. CD(1:LCD)) THEN
                  LLCLCD=LLC-LCD
                  C(IC:LLCLCD)=C(ICPLCD:LLC)
                  LLC=LLCLCD
                  ISTAT=ISTAT+1
               ENDIF
               IC=IC+1
               GOTO 200
            ENDIF
C
C  Fill any resulting freed trailing elements in string C with characters
C  specified by ASCII decimal code IADC
C
            LLCP1=LLC+1
            IF(LLCP1 .LE. LC) THEN
               DO 300 I=LLCP1,LC
                  C(I:I)=CIADC
  300          CONTINUE
            ENDIF
C
         ENDIF
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRFLL(C,IADC,ISTAT)
C
C  PURPOSE
C     To fill a character string with a specified character.
C
C  METHOD
C     String C is filled with characters specified by ASCII decimal code IADC.
C     The completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the fill character
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The filled string
C     ISTAT  Integer    Scalar        n/a             1 or -1
C        The completion status flag:
C            1, Successfully completed
C           -1, Illegal value of IADC; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IADC
C
C  Output variables
C
      INTEGER ISTAT
      CHARACTER*(*) C
C
C  Local variables
C
      INTEGER LC,I
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  Fill string C with characters specified by ASCII decimal code IADC
C
         LC=LEN(C)
         CIADC=CHAR(IADC)
         DO 10 I=1,LC
            C(I:I)=CIADC
   10    CONTINUE
C
C  Set the completion status flag for a successful completion
C
         ISTAT=1
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRINS(C,CI,IPOS,IADC,ISTAT)
C
C  PURPOSE
C     To insert a character string into another string at a specified location.
C
C  METHOD
C     String CI is inserted into string C at position IPOS in string C.
C     Trailing characters specified by ASCII decimal code IADC are ignored in
C     string CI.  The completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be changed
C     CI     Char*(*)   Scalar        n/a             n/a
C        The string to be inserted into string C
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty trailing
C        elements in string CI
C     IPOS   Integer    Scalar        n/a             1 <= IPOS <= len(C)
C        The position in string C where string CI is to be inserted
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string with string CI inserted at position IPOS
C     ISTAT  Integer    Scalar        n/a             1, -1, or -2
C        The completion status flag:
C            1, Successfully completed
C           -1, Illegal value of IADC; string C remains unchanged
C           -2, Illegal value of IPOS; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     IPPLCI Integer    Scalar        n/a             2 <= IPPLCI
C                                                     <= len(C)+len(CI)
C        IPOS+LCI
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C     LCI    Integer    Scalar        n/a             0 <= LCI <= len(CI)
C        The length of string CI, ignoring trailing empty elements
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IPOS,IADC
      CHARACTER*(*) C,CI
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,LCI,IPPLCI
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  IPOS has a valid value
C
         LC=LEN(C)
         IF((IPOS .GE. 1) .AND. (IPOS .LE. LC)) THEN
C
C  Find the last non-empty element in string CI
C
            LCI=LEN(CI)
            CIADC=CHAR(IADC)
   10       IF(CI(LCI:LCI) .EQ. CIADC) THEN
               LCI=LCI-1
               IF(LCI .GT. 0) GOTO 10
            ENDIF
C
C  If the length of string CI is greater than zero, ignoring trailing empty
C  elements, then insert string CI into string C at position IPOS
C
            IF(LCI .GT. 0) THEN
               IPPLCI=IPOS+LCI
               IF(IPPLCI .LE. LC) THEN
                  C(IPPLCI:LC)=C(IPOS:LC-LCI)
                  C(IPOS:IPPLCI-1)=CI(1:LCI)
               ELSE
                  C(IPOS:LC)=CI(1:LC-IPOS+1)
               ENDIF
            ENDIF
C
C  Set the completion status flag for a successful completion
C
            ISTAT=1
C
C  IPOS has a invalid value
C
         ELSE
            ISTAT=-2
         ENDIF
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRLCA(C,ISTAT)
C
C  PURPOSE
C     To convert upper-case letters in a character string to lower-case
C     letters.
C
C  METHOD
C     Upper-case letters (ASCII decimal codes 65 - 90) in string C are
C     converted to lower-case letters (ASCII decimal codes 97 - 122).  The
C     completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be converted
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string with all upper-case letters converted to lower-case letters
C     ISTAT  Integer    Scalar        n/a             0 <= ISTAT <= len(C)
C        The completion status flag:
C           > 0, Successfully completed with ISTAT upper-case letters converted
C                to lower-case letters
C             0, Successfully completed with no upper-case letters converted to
C                lower-case letters; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CC     Char*1     Scalar        n/a             n/a
C        A character in string C
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 Brought internal documentation up to specs.
C     L. Brown       18-Apr-1994  1.3 ==> 1.4
C                                 Corrected range in description of ISTAT.
C     L. Brown       26-May-1994  1.4 ==> 1.5
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      CHARACTER*(*) C
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,I
      CHARACTER*1 CC
C
C  Convert upper-case letters in string C to lower-case letters
C
      ISTAT=0
      LC=LEN(C)
      DO 10 I=1,LC
         CC=C(I:I)
         IF((CC .GE. 'A') .AND. (CC .LE. 'Z')) THEN
            C(I:I)=CHAR(ICHAR(CC)+32)
            ISTAT=ISTAT+1
         ENDIF
   10 CONTINUE
C
      RETURN
      END
      SUBROUTINE STRLEN(C,IADC,L,ISTAT)
C
C  PURPOSE
C     To determine the length of a character string ignoring specified
C     trailing characters.
C
C  METHOD
C     The length L of string C is determined, ignoring trailing characters in
C     string C specified by ASCII decimal code IADC.  The completion status is
C     returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string whose length is to be found
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty trailing
C        elements in string C
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     ISTAT  Integer    Scalar        n/a             1 or -1
C        The completion status flag:
C            1, Successfully completed
C           -1, Illegal value of IADC; L remains unchanged
C     L      Integer    Scalar        n/a             0 <= L <= len(C)
C        The length of string C, ignoring empty trailing elements
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IADC
      CHARACTER*(*) C
C
C  Output variables
C
      INTEGER L,ISTAT
C
C  Local variables
C
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  Find the last non-empty element in string C
C
         L=LEN(C)
         CIADC=CHAR(IADC)
   10    IF(C(L:L) .EQ. CIADC) THEN
            L=L-1
            IF(L .GT. 0) GOTO 10
         ENDIF
C
C  Set the completion status flag for a successful completion
C
         ISTAT=1
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRNPO(C,CF,NOCC,IADC,IPOS,ISTAT)
C
C  PURPOSE
C     To find the Nth position of a character string within another string.
C
C  METHOD
C     The position IPOS in string C of the NOCC occurrence of string CF is
C     found.  Trailing characters in strings C and CF specified by ASCII
C     decimal code IADC are ignored.  The completion status is returned in
C     ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be searched
C     CF     Char*(*)   Scalar        n/a             n/a
C        The string to be found in string C
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty trailing
C        elements in strings C and CF
C     NOCC   Integer    Scalar        n/a             > 0
C        The number of the occurrence of string CF in string C to search for
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     IPOS   Integer    Scalar        n/a             1 <= IPOS <= len(C)
C        The position in string C of the NOCC occurrence of string CF
C     ISTAT  Integer    Scalar        n/a             -2 <= ISTAT <= NOCC
C        The completion status flag:
C             NOCC             , Successfully completed
C             0 <= ISTAT < NOCC, Only ISTAT occurrences of string CF were
C                                found; IPOS remains unchanged
C             -1               , Illegal value of IADC; IPOS remains unchanged
C             -2               , Illegal value of NOCC; IPOS remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     IC     Integer    Scalar        n/a             >= 1
C        The starting position in string C to search for an occurrence of
C        string CF
C     ICLCF1 Integer    Scalar        n/a             >= 1
C        IC+LCF-1
C     ICPLCF Integer    Scalar        n/a             >= 2
C        IC+LCF
C     LC     Integer    Scalar        n/a             0 <= LC <= len(C)
C        The length of string C, ignoring trailing empty elements
C     LCF    Integer    Scalar        n/a             0 <= LCF <= len(CF)
C        The length of string CF, ignoring trailing empty elements
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER NOCC,IADC
      CHARACTER*(*) C,CF
C
C  Output variables
C
      INTEGER IPOS,ISTAT
C
C  Local variables
C
      INTEGER LC,LCF,IC,ICPLCF,ICLCF1
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  NOCC has a valid value
C
         IF(NOCC .GT. 0) THEN
C
C  Find the last non-empty element in string C
C
            LC=LEN(C)
            CIADC=CHAR(IADC)
   10       IF(C(LC:LC) .EQ. CIADC) THEN
               LC=LC-1
               IF(LC .GT. 0) GOTO 10
            ENDIF
C
C  Find the last non-empty element in string CF
C
            LCF=LEN(CF)
  100       IF(CF(LCF:LCF) .EQ. CIADC) THEN
               LCF=LCF-1
               IF(LCF .GT. 0) GOTO 100
            ENDIF
C
C  Initialize the completion status flag
C
            ISTAT=0
C
C  If the length of string C is greater than the length of string CF, ignoring
C  trailing empty elements in both, and if the lengths of both, ignoring
C  trailing empty elements, are greater than zero, then find occurrences of
C  string CF in string C
C
            IF((LC .GE. LCF) .AND. (LC .GT. 0) .AND. (LCF .GT. 0)) THEN
               IC=1
  200          ICPLCF=IC+LCF
               ICLCF1=ICPLCF-1
               IF(ICLCF1 .LE. LC) THEN
                  IF(C(IC:ICLCF1) .EQ. CF(1:LCF)) THEN
                     ISTAT=ISTAT+1
                     IF(ISTAT .EQ. NOCC) IPOS=IC
                     IC=ICPLCF
                  ELSE
                     IC=IC+1
                  ENDIF
                  IF(ISTAT .LT. NOCC) GOTO 200
               ENDIF
            ENDIF
C
C  NOCC has an invalid value
C
         ELSE
            ISTAT=-2
         ENDIF
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRRPL(C,CF,CR,IADC,ISTAT)
C
C  PURPOSE
C    To replace occurrences of a character string in a string with another
C    string.
C
C  METHOD
C     All occurrences of string CF in string C are replaced by string CR.
C     Trailing characters specified by ASCII decimal code IADC are ignored in
C     strings C, CF, and CR.  Any resulting freed trailing elements in string C
C     are filled with characters specified by ASCII decimal code IADC.  The
C     completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be changed
C     CF     Char*(*)   Scalar        n/a             n/a
C        The string to be found and replaced in string C
C     CR     Char*(*)   Scalar        n/a             n/a
C        The string to replace string CF in string C
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty trailing
C        elements in strings C, CF, and CR
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string with occurrences of string CF replaced by string CR
C     ISTAT  Integer    Scalar        n/a             >= -1
C        The completion status flag:
C           > 0, Successfully completed with ISTAT replacements
C             0, Successfully completed with no replacements; string C remains
C                unchanged
C            -1, Illegal value of IADC; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     IC     Integer    Scalar        n/a             1 <= IC <= len(C)
C        The starting position in string C to search for an occurrence of
C        string CF
C     ICLCF1 Integer    Scalar        n/a             ???
C        IC+LCF-1
C     ICPLCF Integer    Scalar        n/a             ???
C        IC+LCF
C     ICPLCR Integer    Scalar        n/a             ???
C        IC+LCR
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C     LCF    Integer    Scalar        n/a             0 <= LCF <= len(CF)
C        The length of string CF, ignoring trailing empty elements
C     LCMRPF Integer    Scalar        n/a             ???
C        LC-LCR+LCF
C     LCP1   Integer    Scalar        n/a             ???
C        LC+1
C     LCR    Integer    Scalar        n/a             0 <= LCR <= len(CR)
C        The length of string CR, ignoring trailing empty elements
C     LCRMF  Integer    Scalar        n/a             ???
C        LCR-LCF
C     LLC    Integer    Scalar        n/a             0 <= LLC <= len(C)
C        The length of string C as replacements are made, ignoring trailing
C        empty elements
C     LLCI   Integer    Scalar        n/a             0 <= LLCI <= len(C)
C        The initial length of string C, ignoring trailing empty elements
C     LLCLCF Integer    Scalar        n/a             ???
C        LLC-LCF
C     LLCP1  Integer    Scalar        n/a             1 <= LLC <= len(C)+1
C        LLC+1
C     LLCRMF Integer    Scalar        n/a             ???
C        LLC+LCR-LCF for LCR < LCF, min(LLC+LCR-LCF,LC) for LCR > LCF
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Fixed bug that occurred when LCR > LCF.
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IADC
      CHARACTER*(*) C,CF,CR
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,LLC,LLCI,LCF,LCR,IC,ICPLCF,ICLCF1,LLCLCF,LCRMF,ICPLCR,
     &        LLCRMF,LCMRPF,LCP1,LLCP1,I
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  Find the last non-empty element in string C
C
         LC=LEN(C)
         LLC=LC
         CIADC=CHAR(IADC)
   10    IF(C(LLC:LLC) .EQ. CIADC) THEN
            LLC=LLC-1
            IF(LLC .GT. 0) GOTO 10
         ENDIF
         LLCI=LLC
C
C  Find the last non-empty element in string CF
C
         LCF=LEN(CF)
  100    IF(CF(LCF:LCF) .EQ. CIADC) THEN
            LCF=LCF-1
            IF(LCF .GT. 0) GOTO 100
         ENDIF
C
C  Find the last non-empty element in string CR
C
         LCR=LEN(CR)
  200    IF(CR(LCR:LCR) .EQ. CIADC) THEN
            LCR=LCR-1
            IF(LCR .GT. 0) GOTO 200
         ENDIF
C
C  Initialize the completion status flag
C
         ISTAT=0
C
C  If the length of string C is at least as large as the length of string CF,
C  ignoring trailing empty elements in both, and if the lengths of both are
C  greater than zero, ignoring trailing empty elements in both, then proceed
C
         IF((LLC .GE. LCF) .AND. (LLC .GT. 0) .AND. (LCF .GT. 0)) THEN
            IC=1
            ICPLCF=IC+LCF
            ICLCF1=ICPLCF-1
C
C  Replace occurrences of string CF with string CR in string C when the length
C  of string CR is zero, ignoring trailing empty elements in both strings CF
C  and CR; this effectively deletes occurrences of string CF from string C
C
            IF(LCR .EQ. 0) THEN
C
C  If the length of string CF is less than or equal to the remaining length of
C  string C to be searched, ignoring trailing empty elements in both, then
C  proceed
C
  300          IF(ICLCF1 .LE. LLC) THEN
C
C  If an occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then proceed
C
                  IF(C(IC:ICLCF1) .EQ. CF(1:LCF)) THEN
C
C  Replace string CF with string CR in string C, ignoring trailing empty
C  elements in both strings CF and CR
C
                     LLCLCF=LLC-LCF
                     C(IC:LLCLCF)=C(ICPLCF:LLC)
                     LLC=LLCLCF
C
C  Update the completion status flag
C
                     ISTAT=ISTAT+1
C
C  If no occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then increment IC
C
                  ELSE
                     IC=IC+1
                     ICPLCF=IC+LCF
                     ICLCF1=ICPLCF-1
                  ENDIF
C
C  Search for the next occurrence of string CF in string C
C
                  GOTO 300
               ENDIF
C
C  Replace occurrences of string CF with string CR in string C when the length
C  of string CR is less than the length of string CF, ignoring trailing empty
C  elements in both strings CF and CR
C
            ELSE IF(LCR .LT. LCF) THEN
C
C  If the length of string CF is less than or equal to the remaining length of
C  string C to be searched, ignoring trailing empty elements in both, then
C  proceed
C
               LCRMF=LCR-LCF
               ICPLCR=IC+LCR
  400          IF(ICLCF1 .LE. LLC) THEN
C
C  If an occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then proceed
C
                  IF(C(IC:ICLCF1) .EQ. CF(1:LCF)) THEN
C
C  Replace string CF with string CR in string C, ignoring trailing empty
C  elements in both strings CF and CR
C
                     LLCRMF=LLC+LCRMF
                     C(ICPLCR:LLCRMF)=C(ICPLCF:LLC)
                     C(IC:ICPLCR-1)=CR(1:LCR)
                     LLC=LLCRMF
C
C  Update the starting position in string C
C
                     IC=ICPLCR
C
C  Update the completion status flag
C
                     ISTAT=ISTAT+1
C
C  If no occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then increment IC
C
                  ELSE
                     IC=IC+1
                  ENDIF
C
C  Search for the next occurrence of string CF in string C
C
                  ICPLCF=IC+LCF
                  ICLCF1=ICPLCF-1
                  ICPLCR=IC+LCR
                  GOTO 400
               ENDIF
C
C  Replace occurrences of string CF with string CR in string C when the length
C  of string CR equals the length of string CF, ignoring trailing empty
C  elements in both strings CF and CR
C
            ELSE IF(LCR .EQ. LCF) THEN
C
C  If the length of string CF is less than or equal to the remaining length of
C  string C to be searched, ignoring trailing empty elements in both, then
C  proceed
C
  500          IF(ICLCF1 .LE. LLC) THEN
C
C  If an occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then proceed
C
                  IF(C(IC:ICLCF1) .EQ. CF(1:LCF)) THEN
C
C  Replace string CF with string CR in string C, ignoring trailing empty
C  elements in both strings CF and CR
C
                     C(IC:ICLCF1)=CR(1:LCF)
C
C  Update the starting position in string C
C
                     IC=ICPLCF
C
C  Update the completion status flag
C
                     ISTAT=ISTAT+1
C
C  If no occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then increment IC
C
                  ELSE
                     IC=IC+1
                  ENDIF
C
C  Search for the next occurrence of string CF in string C
C
                  ICPLCF=IC+LCF
                  ICLCF1=ICPLCF-1
                  GOTO 500
               ENDIF
C
C  Replace occurrences of string CF with string CR in string C when the length
C  of string CR is greater than the length of string CF, ignoring trailing
C  empty elements in both strings CF and CR
C
            ELSE
C
C  If the length of string CF is less than or equal to the remaining length of
C  string C to be searched, ignoring trailing empty elements in both, then
C  proceed
C
               LCRMF=LCR-LCF
               LCMRPF=LC-LCRMF
               LCP1=LC+1
               ICPLCR=IC+LCR
  600          IF(ICLCF1 .LE. LLC) THEN
C
C  If an occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then proceed
C
                  IF(C(IC:ICLCF1) .EQ. CF(1:LCF)) THEN
C
C  Replace string CF with string CR in string C, ignoring trailing empty
C  elements in both strings CF and CR
C
                     LLCRMF=MIN(LLC+LCRMF,LC)
                     IF(ICPLCR .LE. LLC) C(ICPLCR:LLCRMF)
     &                                  =C(ICPLCF:MIN(LLC,LCMRPF))
                     C(IC:MIN(ICPLCR-1,LC))=CR(1:MIN(LCR,LCP1-IC))
                     LLC=LLCRMF
C
C  Update the starting position in string C
C
                     IC=ICPLCR
C
C  Update the completion status flag
C
                     ISTAT=ISTAT+1
C
C  If no occurrence of string CF is found in string C at starting position IC
C  in string C, ignoring trailing empty elements in both, then increment IC
C
                  ELSE
                     IC=IC+1
                  ENDIF
C
C  Search for the next occurrence of string CF in string C
C
                  ICPLCF=IC+LCF
                  ICLCF1=ICPLCF-1
                  ICPLCR=IC+LCR
                  GOTO 600
               ENDIF
C
            ENDIF
C
C  Fill any resulting freed trailing empty elements in string C with characters
C  specified by ASCII decimal code IADC
C
            LLCP1=LLC+1
            IF(LLCP1 .LE. LLCI) THEN
               DO 700 I=LLCP1,LLCI
                  C(I:I)=CIADC
  700          CONTINUE
            ENDIF
C
         ENDIF
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRSHC(C,IS,ISTAT)
C
C  PURPOSE
C     To cyclically shift the contents of a character string.
C
C  METHOD
C     The contents of string C are cyclically shifted by IS elements.  If
C     IS < 0, the shift is towards the beginning of string C; if IS > 0, the
C     shift is towards the end of string C.  The completion status is returned
C     in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be cyclically shifted
C     IS     Integer    Scalar        n/a             n/a
C        The number of elements to cyclically shift string C:
C           > 0, Shift towards the end of C
C             0, No shift
C           < 0, Shift towards the beginning of C
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The cyclically shifted string
C     ISTAT  Integer    Scalar        n/a             0 or 1
C        The completion status flag:
C           1, Successfully completed with an IS shift
C           0, Successfully completed with no shift; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CE     Char*1     Scalar        n/a             n/a
C        The end character in string C
C     I      Integer    Scalar        n/a             1 <= I < len(C)
C        A loop counter
C     ISEN   Integer    Scalar        n/a             0 <= ISEN < len(C)
C        The equivalent negative shift (towards the beginning of string C)
C     ISEP   Integer    Scalar        n/a             0 <= ISEP < len(C)
C        The equivalent positive shift (towards the end of string C)
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C     LCM1   Integer    Scalar        n/a             len(C)-1
C        LC-1
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 Rewrote algorithm for clarity and speed.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IS
      CHARACTER*(*) C
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,ISEP,ISEN,LCM1,I
      CHARACTER*1 CE
C
C  Since this is a cyclical shift, an equivalent positive shift (toward the
C  end of string C) and an equivalent negative shift (toward the beginning of
C  string C) can be calculated
C
      LC=LEN(C)
      ISEP=MOD(LC+MOD(IS,LC),LC)
      ISEN=LC-ISEP
C
C  A shift is necessary if the equivalent shift is nonzero
C
      IF(ISEP .NE. 0) THEN
         LCM1=LC-1
C
C  The equivalent positive cyclical shift of string C requires the least number
C  of iterations
C
         IF(ISEP .LE. ISEN) THEN
            DO 10 I=1,ISEP
               CE=C(LC:LC)
               C(2:LC)=C(1:LCM1)
               C(1:1)=CE
   10       CONTINUE
C
C  The equivalent negative cyclical shift of string C requires the least number
C  of iterations
C
         ELSE
            DO 100 I=1,ISEN
               CE=C(1:1)
               C(1:LCM1)=C(2:LC)
               C(LC:LC)=CE
  100       CONTINUE
         ENDIF
C
C  Set the completion status flag for a successful completion with a shift
C
         ISTAT=1
C
C  No shift is necessary because the equivalent shift is zero
C
      ELSE
         ISTAT=0
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRSHI(C,IS,IADC,ISTAT)
C
C  PURPOSE
C     To shift the contents of a character string.
C
C  METHOD
C     The contents of string C are shifted by IS elements.  If IS < 0, the
C     shift is towards the beginning of string C; if IS > 0, the shift is
C     towards the end of string C.  Characters shifted out of the bounds of
C     string C are lost.  Any resulting freed empty elements in string C are
C     filled with characters specified by ASCII decimal code IADC.  The
C     completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be shifted
C     IS     Integer    Scalar        n/a             n/a
C        The number of elements to shift string C:
C           > 0, Shift towards the end of string C
C             0, No shift
C           < 0, Shift towards the beginning of string C
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character used to fill freed empty
C        elements in string C
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The shifted string
C     ISTAT  Integer    Scalar        n/a             -1 <= ISTAT <= 1
C        The completion status flag:
C            1, Successfully completed with an IS shift
C            0, Successfully completed with no shift; string C remains
C               unchanged
C           -1, Illegal value of IADC; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     ISA    Integer    Scalar        n/a             0 <= ISA <= len(C)
C        The absolute value of IS, restricted to values less than or equal to
C        len(C)
C     ISTART Integer    Scalar        n/a             1 <= ISTART <= len(C)
C        The starting index of the loop to fill the resulting freed trailing
C        empty elements in string C with characters specified by ASCII decimal
C        code IADC
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.4   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 Rewrote algorithm for clarity.
C                                 Removed restriction on value of IS.
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       26-May-1994  1.3 ==> 1.4
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IS,IADC
      CHARACTER*(*) C
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,ISA,ISTART,I
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  If IS is nonzero, then a shift is necessary
C
         IF(IS .NE. 0) THEN
            LC=LEN(C)
            ISA=MIN(ABS(IS),LC)
            CIADC=CHAR(IADC)
C
C  IS is negative
C
            IF(IS .LT. 0) THEN
C
C  Shift string C in the direction of the beginning of string C
C
               IF(ISA .LT. LC) C(1:LC-ISA)=C(ISA+1:LC)
C
C  Fill the resulting freed trailing empty elements in string C with characters
C  specified by ASCII decimal code IADC
C
               ISTART=LC-ISA+1
               DO 10 I=ISTART,LC
                  C(I:I)=CIADC
   10          CONTINUE
C
C  IS is positive
C
            ELSE
C
C  Shift string C in the direction of the end of string C
C
               IF(ISA .LT. LC) C(ISA+1:LC)=C(1:LC-ISA)
C
C  Fill the resulting freed leading empty elements in string C with characters
C  specified by ASCII decimal code IADC
C
               DO 100 I=1,ISA
                  C(I:I)=CIADC
  100          CONTINUE
            ENDIF
C
C  Set the completion status flag for a successful completion with a shift
C
            ISTAT=1
C
C  No shift is necessary because IS is zero
C
         ELSE
            ISTAT=0
         ENDIF
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRTRM(C,IADC,ISTAT)
C
C  PURPOSE
C     To trim specified leading characters from a character string.
C
C  METHOD
C     Leading characters specified by ASCII decimal code IADC are removed from
C     string C.  Any resulting freed trailing elements in string C are filled
C     with characters specified by ASCII decimal code IADC.  The completion
C     status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be trimmed
C     IADC   Integer    Scalar        n/a             0 <= IADC <= 255
C        The ASCII decimal code of the character signifying empty leading
C        elements in string C
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The trimmed string
C     ISTAT  Integer    Scalar        n/a             -1 <= ISTAT <= len(C)
C        The completion status flag:
C           > 0, Successfully completed with ISTAT leading characters trimmed
C             0, Successfully completed with no leading characters trimmed;
C                string C remains unchanged
C            -1, Illegal value of IADC; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CIADC  Char*1     Scalar        n/a             char(IADC)
C        The character represented by ASCII decimal code IADC
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     IC     Integer    Scalar        n/a             1 <= IC <= len(C)+1
C        The location of the first non-empty element in string C
C     ISTART Integer    Scalar        n/a             1 <= ISTART <= len(C)
C        The starting index of the loop to fill any resulting freed trailing
C        elements in string C with characters specified by ASCII decimal code
C        IADC
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C     LCMIC  Integer    Scalar        n/a             0 <= LCMIC <= len(C)-2
C        LC-IC
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 Rewrote algorithm for clarity.
C                                 The maximum value of IADC is now 255.
C                                 Brought internal documentation up to specs.
C     L. Brown       18-Apr-1994  1.3 ==> 1.4
C                                 Corrected range in description of ISTAT.
C     L. Brown       26-May-1994  1.4 ==> 1.5
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      INTEGER IADC
      CHARACTER*(*) C
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,IC,LCMIC,ISTART,I
      CHARACTER*1 CIADC
C
C  IADC has a valid value
C
      IF((IADC .GE. 0) .AND. (IADC .LE. 255)) THEN
C
C  Find the first non-empty element in string C
C
         CIADC=CHAR(IADC)
         LC=LEN(C)
         IC=1
   10    IF(C(IC:IC) .EQ. CIADC) THEN
            IC=IC+1
            IF(IC .LE. LC) GOTO 10
         ENDIF
C
C  If there are any non-empty elements in string C, then check string C for
C  leading empty elements
C
         IF(IC .LE. LC) THEN
C
C  If there are leading empty elements in string C, then trim string C
C
            IF(IC .GT. 1) THEN
C
C  Shift string C toward the beginning of string C to remove the leading empty
C  elements
C
               LCMIC=LC-IC
               C(1:LCMIC+1)=C(IC:LC)
C
C  Fill any resulting freed trailing elements in string C with characters
C  specified by ASCII decimal code IADC
C
               ISTART=LCMIC+2
               DO 100 I=ISTART,LC
                  C(I:I)=CIADC
  100          CONTINUE
C
C  Set the completion status flag for a successful completion with IC-1 leading
C  empty elements trimmed from string C
C
               ISTAT=IC-1
C
C  String C has no leading empty elements
C
            ELSE
               ISTAT=0
            ENDIF
C
C  String C contains no non-empty elements
C
         ELSE
            ISTAT=LC
         ENDIF
C
C  IADC has an invalid value
C
      ELSE
         ISTAT=-1
      ENDIF
C
      RETURN
      END
      SUBROUTINE STRUCA(C,ISTAT)
C
C  PURPOSE
C     To convert lower-case letters in a character string to upper-case
C     letters.
C
C  METHOD
C     Lower-case letters (ASCII decimal codes 97 - 122) in string C are
C     converted to upper-case letters (ASCII decimal codes 65 - 90).  The
C     completion status is returned in ISTAT.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string to be converted
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     C      Char*(*)   Scalar        n/a             n/a
C        The string with all lower-case letters converted to upper-case letters
C     ISTAT  Integer    Scalar        n/a             0 <= ISTAT <= len(C)
C        The completion status flag:
C           > 0, Successfully completed with ISTAT lower-case letters converted
C                to upper-case letters
C             0, Successfully completed with no lower-case letters converted to
C                upper-case letters; string C remains unchanged
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range----------
C        -----------------------------Description------------------------------
C     CC     Char*1     Scalar        n/a             n/a
C        A character in string C
C     I      Integer    Scalar        n/a             1 <= I <= len(C)
C        A loop counter
C     LC     Integer    Scalar        n/a             len(C)
C        The length of string C
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description---------------------------
C     None
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description----------------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description----------------
C     None
C
C  AUTHOR
C     Lincoln D. Brown
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.5   26-May-1994
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description-----------------
C     L. Brown       26-Jun-1992  1.0 ==> Created
C     L. Brown        3-Mar-1993  1.0 ==> 1.1
C                                 Improved internal documentation.
C     L. Brown       12-Mar-1993  1.1 ==> 1.2
C                                 Improved internal documentation.
C     L. Brown       29-Mar-1994  1.2 ==> 1.3
C                                 Brought internal documentation up to specs.
C     L. Brown       18-Apr-1994  1.3 ==> 1.4
C                                 Corrected range in description of ISTAT.
C     L. Brown       26-May-1994  1.4 ==> 1.5
C                                 Optimized for speed.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description--------------
C     None
C
C  Input variables
C
      CHARACTER*(*) C
C
C  Output variables
C
      INTEGER ISTAT
C
C  Local variables
C
      INTEGER LC,I
      CHARACTER*1 CC
C
C  Convert lower-case letters in string C to upper-case letters
C
      ISTAT=0
      LC=LEN(C)
      DO 10 I=1,LC
         CC=C(I:I)
         IF((CC .GE. 'a') .AND. (CC .LE. 'z')) THEN
            C(I:I)=CHAR(ICHAR(CC)-32)
            ISTAT=ISTAT+1
         ENDIF
   10 CONTINUE
C
      RETURN
      END
C******************************************************************************
C  The dummy routines below are included for backward compatibility.  They call
C  the routines above.
C******************************************************************************
      SUBROUTINE CONCAT(C1,C2,IADC,C,ISTAT)
      INTEGER IADC,ISTAT
      CHARACTER*(*) C1,C2,C
      CALL STRCCT(C1,C2,IADC,C,ISTAT)
      RETURN
      END
      SUBROUTINE DELETE(C,CD,IADC,ISTAT)
      INTEGER IADC,ISTAT
      CHARACTER*(*) C,CD
      CALL STRDEL(C,CD,IADC,ISTAT)
      RETURN
      END
      SUBROUTINE FILL(C,IADC,ISTAT)
      INTEGER IADC,ISTAT
      CHARACTER*(*) C
      CALL STRFLL(C,IADC,ISTAT)
      RETURN
      END
      SUBROUTINE INSERT(C,CI,IPOS,IADC,ISTAT)
      INTEGER IPOS,IADC,ISTAT
      CHARACTER*(*) C,CI
      CALL STRINS(C,CI,IPOS,IADC,ISTAT)
      RETURN
      END
      SUBROUTINE LOCASE(C,ISTAT)
      INTEGER ISTAT
      CHARACTER*(*) C
      CALL STRLCA(C,ISTAT)
      RETURN
      END
      SUBROUTINE LENGTH(C,IADC,L,ISTAT)
      INTEGER IADC,L,ISTAT
      CHARACTER*(*) C
      CALL STRLEN(C,IADC,L,ISTAT)
      RETURN
      END
      SUBROUTINE NTHPOS(C,CF,NOCC,IADC,IPOS,ISTAT)
      INTEGER NOCC,IADC,IPOS,ISTAT
      CHARACTER*(*) C,CF
      CALL STRNPO(C,CF,NOCC,IADC,IPOS,ISTAT)
      RETURN
      END
      SUBROUTINE REPLAC(C,CF,CR,IADC,ISTAT)
      INTEGER IADC,ISTAT
      CHARACTER*(*) C,CF,CR
      CALL STRRPL(C,CF,CR,IADC,ISTAT)
      RETURN
      END
      SUBROUTINE SHIFTC(C,IS,ISTAT)
      INTEGER IS,ISTAT
      CHARACTER*(*) C
      CALL STRSHC(C,IS,ISTAT)
      RETURN
      END
      SUBROUTINE SHIFT(C,IS,IADC,ISTAT)
      INTEGER IS,IADC,ISTAT
      CHARACTER*(*) C
      CALL STRSHI(C,IS,IADC,ISTAT)
      RETURN
      END
      SUBROUTINE TRIM(C,IADC,ISTAT)
      INTEGER IADC,ISTAT
      CHARACTER*(*) C
      CALL STRTRM(C,IADC,ISTAT)
      RETURN
      END
      SUBROUTINE UPCASE(C,ISTAT)
      INTEGER ISTAT
      CHARACTER*(*) C
      CALL STRUCA(C,ISTAT)
      RETURN
      END
