C     iau_time.f
C     stuff for interfacing with the SOFA library for astronomical
C     time and coordinate stuff
C     2005 Aug 31  James M Anderson  --JIVE  start


      REAL*8 FUNCTION GET_IAU_GAST(YEAR, MON, DAY, UT)

      IMPLICIT NONE
C_TITLE GET_IAU_GAST --get the IAU Greenwich apparent sidereal time, in rad

C_ARGS  TYPE           VARIABLE I/O DESCRIPTION
      INTEGER YEAR             ! I  the full year
      INTEGER MON              ! I  the month, 1--12
      INTEGER DAY              ! I  the day number of the month
      REAL*8 UT                ! I  the UT time (UTC) in hours
C     REAL*8 GET_IAU_GAST        O  The apparent sidereal time at Greenwich
C                                   in radians (0--2\pi)

C_USER  any user input?

C_VARS  TYPE           VARIABLE I/O DESCRIPTION
C     put include and common stuff here
      include 'logicuni.inc'

C_DESC  full description of program

C_FILE  files used and logical units used

C_LIMS  design limitations

C_BUGS  known bugs

C_CALL  list of calls
      DOUBLE PRECISION iau_GST00A
      EXTERNAL iau_GST00A

C_KEYS  

C_HIST  DATE NAME PLACE INFO
C     2005 Aug 31  James M Anderson  --JIVE  start

C_END

C     OK, I need the UT and TT double Julian Dates.  For now, ignore
C     the difference between UTC and UT1, which should be |UTC-UT1| \leq 1
      REAL*8 FRAC_DAY             ! holder for fractional day = UT/24
      REAL*8 DELTA_T              ! holder for TAI-UTC, in s
      REAL*8 UTA, UTB, TTA, TTB   ! hold the Julian Date Times
      INTEGER RETCODE             ! information for the return codes
      REAL*8 SEC2DAY
      PARAMETER (SEC2DAY = 1.0d0 / 3600.d0 / 24.0d0)
      REAL*8 HOUR2DAY
      PARAMETER (HOUR2DAY = 1.0d0 / 24.0d0)

      
      FRAC_DAY = UT*HOUR2DAY


C     Get the JD information
      CALL iau_CAL2JD(YEAR, MON, DAY, UTA, UTB, RETCODE)
      IF(RETCODE .LT. 0) THEN
         WRITE(LUSTDERR,*) 'Bad date in JD conversion ', YEAR, MON, DAY
         STOP 'GET_IAU_GAST'
      ENDIF
C     Now update the TT information
      CALL iau_DAT(YEAR, MON, DAY, FRAC_DAY, DELTA_T, RETCODE)
      IF(RETCODE .LT. 0) THEN
         WRITE(LUSTDERR,*) 'Bad date in TAI conversion ', YEAR, MON, 
     &        DAY, FRAC_DAY
         STOP 'GET_IAU_GAST'
      ENDIF
C     Now add on the fractional days
      UTB = UTB + FRAC_DAY
C     The TT stuff is almost the same
      TTA = UTA
      TTB = UTB + (DELTA_T + 32.184) * SEC2DAY


      GET_IAU_GAST = iau_GST00A(UTA, UTB, TTA, TTB)
      RETURN
      END


