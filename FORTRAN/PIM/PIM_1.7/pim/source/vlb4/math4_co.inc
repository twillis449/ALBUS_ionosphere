C  Declares single precision (REAL*4) mathematical constants
C
C     DPR      Conversion factor from radians to degrees
C     DPH      Conversion factor from hours to degrees
C     PI       Pi
C     PIO2     Pi/2
C     RPD      Conversion factor from degrees to radians
C     TWOPI    2*Pi
C
      REAL PIO2,TWOPI,DPR,RPD,DPH
      PARAMETER(PIO2=PI/2.,TWOPI=2.*PI,DPR=180./PI,
     &          RPD=PI/180.,DPH= 360./24.)
c$$$      REAL PI,PIO2,TWOPI,DPR,RPD,DPH
c$$$      PARAMETER(PI=3.14159265,PIO2=PI/2.,TWOPI=2.*PI,DPR=180./PI,
c$$$     &          RPD=PI/180.,DPH= 360./24.)
C
C
