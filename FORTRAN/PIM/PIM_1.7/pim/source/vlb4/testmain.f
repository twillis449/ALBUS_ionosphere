      PROGRAM TESTMAIN
      IMPLICIT NONE
C_TITLE PIM_ASUB --astronomical subroutine for calling PIM

C_ARGS  TYPE           VARIABLE I/O DESCRIPTION
      REAL*8 STALAT            ! I  station latitude, in rad, geocentric
      REAL*8 STALON            ! I  station longitude, in rad, geocentric
      REAL*8 STAELEV           ! I  station elevation above Earth radius, in km
      REAL*8 OB_AZ             ! I  object azimuth, in rad, geocentric
      REAL*8 OB_EL             ! I  object elevation, in rad, geocentric

C	ALL TIMES ARE UT!!!
      INTEGER TYEAR            ! I  The year of the observation (must be
                               !    in the range 1981 to ~ 2015)
      INTEGER TYDAY            ! I  The day number of the year, days since Jan 0
      INTEGER TMON             ! I  The month number, Jan == 1
      INTEGER TDAY             ! I  The day of the month
      REAL*8 TUT               ! I  The time of day, in fractional hours

      REAL*8 STEC              ! O  The slant total electron content, in 
                               !    TECU (10^{16} m^{-2}) along the line of sight
      REAL*8 SRM               ! O  The slant rotation measure, in RMU
                               !    (10^{12} T m^{-2})
      INTEGER SOLCODE          ! B  Solar Magnetic Field Code
                               !    on input:
                               !    <=0 try reading frm file, if data missing,
                               !        assume by=1,bz=1
                               !      1 try reading frm file, if data missing,
                               !        assume by=1,bz=-1
                               !      2 assume by=1,bz=1
                               !    >=3 assume by=1,bz=1
                               !    On return:
                               !      0 data read ok
                               !      1 no data available, or values guessed
      INTEGER RETCODE          ! O  The return code
                               !    Note that for really fatal errors, the
                               !    software here and called from here may simply
                               !    STOP
                               !    0 all ok.
                               !    1 negative elevation
      REAL*8 PI, DEG2RAD
      PARAMETER (pi = 3.14159265358979323846d0)
      PARAMETER (DEG2RAD = pi / 1.8d2)
      CHARACTER*256 BASEPATH   ! I  The base pathname to the databases
      PARAMETER(BASEPATH='/jop30_0/anderson/astro/ionosphere/prog'
     &    // '/FORTRAN/PIM/PIM_1.7')
      REAL*8 gast
      REAL*8 GET_IAU_GAST
      EXTERNAL GET_IAU_GAST
      REAL*8 JD1, JD2, JD3
      INTEGER i
      REAL*8 STEC1, STEC2


C      52:15:57.545N   031:05:09.391E
      STALAT = (52.+(15.+57.545/60.)/60.)*DEG2RAD
      STALON = (31.+(5.+9.391/60.)/60.)*DEG2RAD
      STAELEV = 0.0
      OB_AZ = -1.606*DEG2RAD
      OB_EL = 32.792*DEG2RAD
      TYEAR = 2004
      TYDAY = 1
      TMON = 1
      TDAY = 1
      TUT = 15.0
      SOLCODE = 0
      RETCODE = 0



c$$$      CALL sidtim (TYEAR,TYDAY,TUT, gast)
c$$$      print *, gast
c$$$      gast = GET_IAU_GAST(TYEAR, TMON, TDAY, TUT)
c$$$      print *, gast
c$$$      stop 2

      call PIM_ASUB(
     &     BASEPATH,
     &     STALAT, STALON, STAELEV,
     &     OB_AZ, OB_EL,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     STEC, SRM, SOLCODE, RETCODE)

      PRINT *, STEC, SRM, SOLCODE, RETCODE


      OB_AZ = 177.210*DEG2RAD
      OB_EL = 4.781*DEG2RAD
      SOLCODE=0
      call PIM_ASUB(
     &     BASEPATH,
     &     STALAT, STALON, STAELEV,
     &     OB_AZ, OB_EL,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     STEC, SRM, SOLCODE, RETCODE)

      PRINT *, STEC, SRM, SOLCODE, RETCODE
      OB_AZ = 282.296*DEG2RAD
      OB_EL = 11.294*DEG2RAD
      SOLCODE=0
      call PIM_ASUB(
     &     BASEPATH,
     &     STALAT, STALON, STAELEV,
     &     OB_AZ, OB_EL,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     STEC, SRM, SOLCODE, RETCODE)

      PRINT *, STEC, SRM, SOLCODE, RETCODE
      OB_AZ = 255.737*DEG2RAD
      OB_EL = 23.818*DEG2RAD
      SOLCODE=0
      call PIM_ASUB(
     &     BASEPATH,
     &     STALAT, STALON, STAELEV,
     &     OB_AZ, OB_EL,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     STEC, SRM, SOLCODE, RETCODE)

      PRINT *, STEC, SRM, SOLCODE, RETCODE
      OB_AZ = 180.0*DEG2RAD
      OB_EL = 23.818*DEG2RAD
      TDAY=1
      TMON=1
      TYDAY=0
      TYEAR=2005



      CALL iau_CAL2JD(TYEAR, TMON, TDAY, JD1, JD2, i)
      

      DO i=1,100
         JD3 = JD2 + i -1
         CALL iau_JD2CAL(JD1, JD3, TYEAR, TMON, TDAY, STEC, SOLCODE)
         TYDAY = TYDAY+1
         PRINT*, TYEAR, TYDAY, TMON, TDAY
         SOLCODE=0
      call PIM_ASUB(
     &        BASEPATH,
     &     STALAT, STALON, STAELEV,
     &     OB_AZ, OB_EL,
     &     TYEAR, TYDAY, TMON, TDAY, TUT,
     &     STEC, SRM, SOLCODE, RETCODE)

      PRINT *, STEC, SRM, SOLCODE, RETCODE
      ENDDO



      STEC1 = 0.0
      STEC2 = 0.0
      DO i=1,900
         OB_EL = (0.0+i*0.1)*DEG2RAD
         SOLCODE=0
         call PIM_ASUB(
     &        BASEPATH,
     &        STALAT, STALON, STAELEV,
     &        OB_AZ, OB_EL,
     &        TYEAR, TYDAY, TMON, TDAY, TUT,
     &        STEC, SRM, SOLCODE, RETCODE)
         PRINT *, 'A', STEC, SRM, OB_EL, STEC-STEC1, STEC-STEC2
         STEC2=STEC1
         STEC1 = STEC
      ENDDO




      END
