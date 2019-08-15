C  Declarations for high latitude region boundaries
C     As of 6 April 1990 the trough equatorward boundary is a distorted circle
C  of the form
C
C     THETA(PHI) = THETA0(Kp) + AB*EXP(-(ABS((PHI-BB)/CB)^PB))
C
C  where THETA is magnetic co-latitude (degrees), PHI is magnetic local time
C  (hours).  THETA0 definitely depends on Kp; AB, BB, CB, and PB may or may
C  not.  The equatorward auroral oval boundary is an undistorted circle whose
C  center is displaced from the magnetic pole.  The polar cap boundary is also
C  a displaced circle.  For these THETA0 is the radius (degrees), AB is the CGM
C  co-latitude of the center, BB is the MLT of the center.
C
C     At low magnetic activity, the precipitation oval and the trough overlap
C  during midmorning (0800-1000) magnetic local times.  This may (or may not)
C  be an indication of the absence of a morning trough at low activity
C  levels.  CB(1) is a minimum displacement of the oval boundary from the
C  trough boundary, i.e., a minimum width of the trough.  Likewise, CB(2) is a
C  minimum width of the oval.  PB(1) and PB(2) are currently unused.
C
C       CB (0) = the local time scale size of the distortion:
C
C       CB(1:2) are the minimum trough and oval widths
C       PB(0) is the exponent:
C       PB(1:2)  are currently unused
C
C        THETA0(0) = 24.4 + 2.12*KP(0)
C        THETA0(1) = 20.9 + 1.7*KP(1)
C        THETA0(2) = 13.4 + 1.7*KP(2)
C
C       And fix the amplitude and local time of the distortion of the trough
C         boundary:
C        AB(0) = -10.5
C        AB(1) = 2.7 + 0.267*KP(1)
C        AB(2) = 0.8 - 0.267*KP(2)
C
C
      REAL THETA0(0:2),AB(0:2),BB(0:2),CB(0:2),PB(0:2),CAB(0:2),
     1 VAB(0:2),CTH(0:2),VTH(0:2),CBB(0:1),VBB(0:1),QBB(0:1)
C
      COMMON/BNDS1/THETA0,AB,BB,CB,PB,CAB,VAB,CTH,VTH,CBB,VBB,QBB
C
