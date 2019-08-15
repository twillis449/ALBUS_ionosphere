C******************************************************************************
C  STRNGLIB is a FORTRAN library of string manipulations.  It contains the
C  following routines:
C
C     SUBROUTINE STRCCT (C1,C2,IADC,C,ISTAT)
C                       Concatenates two character strings
C     SUBROUTINE STRLCA (C,ISTAT)
C                       Converts upper-case letters in a character string to
C                       lower-case letters
C     SUBROUTINE STRTRM (C,IADC,ISTAT)
C                       Trims specified leading characters from a character
C                       string
C
C  They use the ANSI FORTRAN 77 standard intrinsic FORTRAN functions
C
C     len(C)     The length of the string C
C     char(n)    The character represented by ASCII decimal code n
C     ichar(c)   The ASCII decimal code representing the character c
C
C  but are independent of each other.
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
C     Lincoln Brown
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
C     Lincoln Brown
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
