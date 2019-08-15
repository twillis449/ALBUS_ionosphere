      SUBROUTINE SET_UP_VLBI (LUCGDB, pkpf,pimf, pigrf, psrc,psta, prtd)
C
C  PURPOSE
C     To get the type of record-length for direct-access files and the
C     locations of data files from the PIM path names input file 
C     PATH_NAME.TXT, and to read the corrected geomagnetic coordinate 
C     conversion database from the corrected geomagnetic coordinate 
C     conversion database file.
C
C  METHOD
C     The PIM path names input file PATH_NAM.TXT is opened, the type of 
C     record-length for direct-access files and the locations of data 
C     files are read from it, and it is closed.  The location of the 
C     corrected geomagnetic coordinate conversion database is passed to 
C     subroutine RDCGDB, which reads the corrected geomagnetic 
C     coordinate conversion database from the corrected coordinate 
C     conversion database file.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range---
C        -----------------------------Description-----------------------
C     LUCGDB  Integer    Scalar        n/a             n/a
C       The logical unit number used to access the corrected geomagnetic
C       coordinate conversion database
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range---
C        -----------------------------Description-----------------------
C     None
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range---
C        -----------------------------Description-----------------------
C    ISTAT   Integer    Scalar        n/a             n/a
C        The status returned from the STRTRM routine
C    LCGDB   Char*80    Scalar        n/a             n/a
C        The location of the corrected geomagnetic coordinate conversion
C        database
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description--------------------
C     RDCGDB Reads the corrected geomagnetic coordinate conversion 
C            database from the corrected geomagnetic coordinate 
C            conversion database file
C     STRTRM Trims specified leading characters from a character string
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description--------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description---------
C     PATH_NAM.TXT Formatted  1      The PIM path names input file
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description----------
C     W. Whartenby   29-Aug-1994  1.0.6
C     L. Brown       18-Nov-1994  1.0.6 ==> 1.1
C                                 Complete rewrite.
C                                 Removed output parameter PSGP.
C                                 The location of the solar and 
C                                 geophysical parameters has been 
C                                 removed from PATH_NAM.TXT since it is
C                                  not used by PIM.
C     L. Brown       25-Sep-1995  1.1 ==> 1.3
C                                 Removed call to routine STRLEN since 
C                                 it is not needed.
C                                 Removed local variable LLCGDB since it
C                                 is no longer used.
C                                 Leading spaces are now removed from 
C                                 all of the path names.
C     Bob Campbell   13-Mar-1998  Added the 2 geophysical parameter
C                                 files and souce & station catalogues 
C                                 to read for PIMVLBI2.   PATH_NAM.TXT
C                                 also needs to be updated to reflect 
C                                 these files:  hence now 
C                                 PATH_NAM_VLBI.TXT.  
C                    19-Mar-1998  Writes locations of files to screen 
C                                 only if an OPEN-error occurs.
C                    30-Mar-1998  Added path for IGRF terrestrial 
C                                 magnetic-field spherical harmonic 
C                                 coefficient files (to allow Faraday 
C                                 rotation calc.)
C                    01-Mar-1999  Changed F10.7/Kp databse reference 
C                                 from its filename to its directory,
C                                 to allow access of different kinds
C                                 F10.7/Kp databases (final, 
C                                 preliminary, or forecast).
C                                 Added paths for GPS/ISS real-time data
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description-------
C     None
C
C  Input variables
C
      INTEGER LUCGDB
C
C  Local variables 
C   including the character variables for the geophysical data files:
C   pass these as calling parameters rather than change the existing 
C   DPATH.INC 
C
      INTEGER ISTAT
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 LCGDB, PKPF, PIMF, PSRC, PSTA, PIGRF, PRTD
C
      INCLUDE 'dpath.inc'
C
C  Open the PIM path names input file
C
      OPEN(UNIT=1,FILE='path_nam_vlbi4.txt',STATUS='OLD',
     &     FORM='FORMATTED',ERR=10)
      GOTO 20
   10 STOP 'SET_UP:Error opening PATH_NAM_VLBI.TXT.'
   20 CONTINUE
C
C  Read the type of record-length for direct-access files from the PIM path
C  names input file
C
      READ(1,*,END=100,ERR=100) JOPEN
      GOTO 110
  100 STOP 'SET_UP:Error reading direct-access record-length type.'
  110 CONTINUE
C
C  Check the validity of the type of record-length for direct-access files
C
      IF((JOPEN .NE. 1) .AND. (JOPEN .NE. 2))
     &   STOP 'SET_UP:Invalid direct-access record-length type.'
C
C  Read the location of the corrected geomagnetic coordinate database from the
C  PIM path names input file
C
      READ(1,'(A)',END=200,ERR=200) LCGDB
      GOTO 210
  200 WRITE (*,*) 'SET_UP:  Error reading CGM coordinate database locati
     1on.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected geomagnetic coordinate '//
     &                         'database location:  ',LCGDB
      STOP
  210 CALL STRTRM(LCGDB,32,ISTAT)
C
C  Read the location of the parameterized USU database from the PIM path names
C  input file
C
      READ(1,'(A)',END=300,ERR=300) PUSU
      GOTO 310
  300 WRITE (*,*) 'SET_UP:  Error reading parameterized USU database loc
     1ation.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected USU database location: ',PUSU
      STOP
  310 CALL STRTRM(PUSU,32,ISTAT)
C
C  Read the location of the parameterized mid-latitude F-region database from
C  the PIM path names input file
C
      READ(1,'(A)',END=400,ERR=400) PMID
      GOTO 410
  400 WRITE(*,*) 'SET_UP:  Error reading parameterized MLF database loca
     1tion.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected mid-latitude F-region '//
     &                         'database location:',PMID
      STOP
  410 CALL STRTRM(PMID,32,ISTAT)
C
C  Read the location of the parameterized low-latitude F-region database from
C  the PIM path names input file
C
      READ(1,'(A)',END=500,ERR=500) PLOW
      GOTO 510
  500 WRITE(*,*) 'SET_UP: Error reading parameterized LLF database locat
     1ion.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected low-latitude F-region '//
     &                         'database location:',PLOW
      STOP
  510 CALL STRTRM(PLOW,32,ISTAT)
C
C  Read the location of the parameterized low- and mid-latitude E-region
C  database from the PIM path names input file
C
      READ(1,'(A)',END=600,ERR=600) PLME
      GOTO 610
  600 WRITE(*,*) 'SET_UP: Error reading parameterized LME database locat
     1ion.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected  low- and mid-latitude '//
     &                         'E-region database location:',PLME
      STOP
  610 CALL STRTRM(PLME,32,ISTAT)
C
C  Read the location of the URSI coefficients database from the PIM path names
C  input file
C
      READ(1,'(A)',END=700,ERR=700) PAWS
      GOTO 710
  700 WRITE(*,*) 'SET_UP:Error reading URSI coefficients database.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected URSI coefficients database loca
     &tion:', PAWS
      STOP
  710 CALL STRTRM(PAWS,32,ISTAT)
C
C  Read the directory of the F10.7 & Kp data-file downloaded from NOAA
C
      READ(1,'(A)',END=730,ERR=730) PKPF
      GOTO 740
  730 WRITE(*,*) 'SET_UP:Error reading F10.7 & Kp datafile.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected F10.7 & Kp datafile location:',
     1                           PKPF
      STOP
  740 CALL STRTRM(PKPF,32,ISTAT)
C
C  Read the location of the IMF By & Bz data-file downloaded from GSFC
C
      READ(1,'(A)',END=760,ERR=760) PIMF
      GOTO 770
  760 WRITE(*,*) 'SET_UP:Error reading IMF By & Bz datafile.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected IMF By & Bz datafile location:'
     1                           , PIMF
      STOP
  770 CALL STRTRM(PIMF,32,ISTAT)
C
C  Read the location of the IGRF terrestrial magnetic-field spherical
C  harmonic coefficient databases downloaded from GSFC
C
      READ(1,'(A)',END=900,ERR=900) PIGRF
      GOTO 910
  900 WRITE(*,*) 'SET_UP: Error reading IGRF database'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected IGRF database location:',PIGRF
      STOP
  910 CALL STRTRM(PIGRF,32,ISTAT)
C
C
C  Read the location of the Source catalogue
C
      READ(1,'(A)',END=930,err=930) PSRC
      GOTO 940
  930 WRITE(*,*) 'SET_UP:Error reading Source catalogue.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected Source catalogue location:',
     1                            PSRC
      STOP
  940 CALL STRTRM(PSRC,32,ISTAT)
C
C  Read the location of the Station catalogue
C
      READ(1,'(A)',END=960,err=960) PSTA
      GOTO 970
  960 WRITE(*,*) 'SET_UP:Error reading Station catalogue.'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected Station catalogue location:',
     1                             PSTA
      STOP
  970 CALL STRTRM(PSTA,32,ISTAT)
C
C  Read the location of the Station catalogue
C
      READ(1,'(A)',END=980,err=980) PRTD
      GOTO 990
  980 WRITE(*,*) 'SET_UP:Error reading real-time data directory'
      WRITE(*,'(1X,A,/,1X,A)') 'Expected Real-time data directory locati
     1on:',  PRTD
      STOP
  990 CALL STRTRM(PRTD,32,ISTAT)
C
C  Close the PIM path names input file
C
      CLOSE(UNIT=1,ERR=800)
      GOTO 810
  800 STOP 'SET_UP:Error closing PATH_NAM.TXT.'
  810 CONTINUE
C
C  Read the corrected geomagnetic coordinate conversion database (required for
C  conversion to and from corrected geomagnetic coordinates) from the corrected
C  geomagnetic coordinate conversion database file
C
      CALL RDCGDB(LCGDB,LUCGDB)
C
      RETURN
      END



      SUBROUTINE SET_UP_VLBI2 (BASEPATH,LUCGDB,pkpf,pimf)
C
C  PURPOSE
C     To get the type of record-length for direct-access files and the
C     locations of data files from the PIM path names input file 
C     PATH_NAME.TXT, and to read the corrected geomagnetic coordinate 
C     conversion database from the corrected geomagnetic coordinate 
C     conversion database file.
C
C  METHOD
C     The PIM path names input file PATH_NAM.TXT is opened, the type of 
C     record-length for direct-access files and the locations of data 
C     files are read from it, and it is closed.  The location of the 
C     corrected geomagnetic coordinate conversion database is passed to 
C     subroutine RDCGDB, which reads the corrected geomagnetic 
C     coordinate conversion database from the corrected coordinate 
C     conversion database file.
C
C  INPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range---
C        -----------------------------Description-----------------------
C     LUCGDB  Integer    Scalar        n/a             n/a
C       The logical unit number used to access the corrected geomagnetic
C       coordinate conversion database
C
C  OUTPUT PARAMETERS
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range---
C        -----------------------------Description-----------------------
C     None
C
C  LOCAL VARIABLES
C     -Name- ---Type--- --Dimension-- -----Units----- ----------Range---
C        -----------------------------Description-----------------------
C    ISTAT   Integer    Scalar        n/a             n/a
C        The status returned from the STRTRM routine
C    LCGDB   Char*80    Scalar        n/a             n/a
C        The location of the corrected geomagnetic coordinate conversion
C        database
C
C  SUBROUTINES REQUIRED
C     -Name- ----------------------------Description--------------------
C     RDCGDB Reads the corrected geomagnetic coordinate conversion 
C            database from the corrected geomagnetic coordinate 
C            conversion database file
C     STRTRM Trims specified leading characters from a character string
C
C  FUNCTIONS REQUIRED
C     -Name- ---Type--- ----------------------Description--------------
C     None
C
C  FILES ACCESSED
C     ----Name---- ---Type--- -Unit- ---------------Description---------
C     PATH_NAM.TXT Formatted  1      The PIM path names input file
C
C  AUTHOR
C     William G. Whartenby
C     Computational Physics, Inc.
C     240 Bear Hill Road  Suite 202A
C     Waltham, MA 02154  USA
C     (617)-487-2250
C
C  VERSION
C     1.3   25-September-1995
C
C  MODIFICATIONS
C     ----Person---- ----Date---- -----------------Description----------
C     W. Whartenby   29-Aug-1994  1.0.6
C     L. Brown       18-Nov-1994  1.0.6 ==> 1.1
C                                 Complete rewrite.
C                                 Removed output parameter PSGP.
C                                 The location of the solar and 
C                                 geophysical parameters has been 
C                                 removed from PATH_NAM.TXT since it is
C                                  not used by PIM.
C     L. Brown       25-Sep-1995  1.1 ==> 1.3
C                                 Removed call to routine STRLEN since 
C                                 it is not needed.
C                                 Removed local variable LLCGDB since it
C                                 is no longer used.
C                                 Leading spaces are now removed from 
C                                 all of the path names.
C     Bob Campbell   13-Mar-1998  Added the 2 geophysical parameter
C                                 files and souce & station catalogues 
C                                 to read for PIMVLBI2.   PATH_NAM.TXT
C                                 also needs to be updated to reflect 
C                                 these files:  hence now 
C                                 PATH_NAM_VLBI.TXT.  
C                    19-Mar-1998  Writes locations of files to screen 
C                                 only if an OPEN-error occurs.
C                    30-Mar-1998  Added path for IGRF terrestrial 
C                                 magnetic-field spherical harmonic 
C                                 coefficient files (to allow Faraday 
C                                 rotation calc.)
C                    01-Mar-1999  Changed F10.7/Kp databse reference 
C                                 from its filename to its directory,
C                                 to allow access of different kinds
C                                 F10.7/Kp databases (final, 
C                                 preliminary, or forecast).
C                                 Added paths for GPS/ISS real-time data
C     2005 Sep 01  James M Anderson  --JIVE  This program will be distributed
C                                 to a group of people who are not really 
C                                 interested in the ionosphere, but will merely
C                                 want to use the program.  So, hardcode the
C                                 paths to the various datasets, relative to
C                                 a single install directory.  Also, get
C                                 rid of the station and source catalogs, as this
C                                 PIM stuff is now just a callable subroutine.
C
C  REFERENCES
C     None
C
C  SPECIAL CONSTANTS
C     -Name- ---Type--- -----Units----- --------------Description-------
C     None
C
C  Input variables
C
      INTEGER LUCGDB
      CHARACTER*256 BASEPATH
C
C  Local variables 
C   including the character variables for the geophysical data files:
C   pass these as calling parameters rather than change the existing 
C   DPATH.INC 
C
      INTEGER ISTAT
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER*256 LCGDB, PKPF, PIMF
      LOGICAL INITED
      SAVE INITED
      DATA INITED/.FALSE./
C
      INCLUDE 'dpath.inc'
      INCLUDE 'setupvlbi.inc'

C     Set the record-length descriptor for direct access
      JOPEN = BLOCK_SIZE

C     PRINT*, '*** BASEPATH *** ', BASEPATH
C
C  Read the location of the corrected geomagnetic coordinate database from the
C  PIM path names input file
C
      CALL STRCCT(BASEPATH,RELPATHCGM,32,LCGDB,ISTAT)
C
C  Read the location of the parameterized USU database from the PIM path names
C  input file
C
      CALL STRCCT(BASEPATH,RELPATHUSU,32,PUSU,ISTAT)
C
C  Read the location of the parameterized mid-latitude F-region database from
C  the PIM path names input file
C
      CALL STRCCT(BASEPATH,RELPATHMLF,32,PMID,ISTAT)
C
C  Read the location of the parameterized low-latitude F-region database from
C  the PIM path names input file
C
      CALL STRCCT(BASEPATH,RELPATHLLF,32,PLOW,ISTAT)
C
C  Read the location of the parameterized low- and mid-latitude E-region
C  database from the PIM path names input file
C
      CALL STRCCT(BASEPATH,RELPATHLME,32,PLME,ISTAT)
C
C  Read the location of the URSI coefficients database from the PIM path names
C  input file
C
      CALL STRCCT(BASEPATH,RELPATHURS,32,PAWS,ISTAT)
C
C  Read the directory of the F10.7 & Kp data-file downloaded from NOAA
C
      CALL STRCCT(BASEPATH,RELPATHNOA,32,PKPF,ISTAT)
C
C  Read the location of the IMF By & Bz data-file downloaded from GSFC
C
      CALL STRCCT(BASEPATH,RELPATHIMF,32,PIMF,ISTAT)
C
C  Read the location of the IGRF terrestrial magnetic-field spherical
C  harmonic coefficient databases downloaded from GSFC
C
C      CALL STRCCT(BASEPATH,RELPATHIGR,32,PIGRF,ISTAT)
C
C  Read the corrected geomagnetic coordinate conversion database (required for
C  conversion to and from corrected geomagnetic coordinates) from the corrected
C  geomagnetic coordinate conversion database file
C
      IF(INITED.eqv..TRUE.) THEN
      ELSE
         INITED = .TRUE.
         CALL RDCGDB(LCGDB,LUCGDB)
      ENDIF
C
      RETURN
      END
