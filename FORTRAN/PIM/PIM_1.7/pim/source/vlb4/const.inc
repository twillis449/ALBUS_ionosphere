C     const.inc
C     constants for use with PIM.  i.e. angles, Earth radius, etc.
C     2005 Sep 05  James M Anderson  --JIVE  start

C     Make something so we can use a consistent definition of the
C     radius of the Earth
C         This class represents the geodetic model defined in NIMA
C         TR8350.2, "Department of Defense World Geodetic System 1984".
C         Defined in TR8350.2, Appendix A.1
C         @return semi-major axis of Earth in kilometers.
      REAL*8 RADIUS_EARTH
      PARAMETER(RADIUS_EARTH=6378.1370D0)
C     Now, how about PI
      REAL*8 PI, DTOR, RTOD
      PARAMETER(PI=3.141592653589793238462643383279d0)
      PARAMETER(DTOR=0.0174532925199432976913d0)
      PARAMETER(RTOD=57.29577951308232087655d0)
