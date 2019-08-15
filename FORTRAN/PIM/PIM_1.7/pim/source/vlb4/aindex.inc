C
C     A(I,K,L):    I = ORDER OF COEFFICIENT
C                  K = 1 FOR EQUATORWARD EDGE, 2 FOR POLEWARD
C                  L = 1 FOR ELECTRONS, 2 FOR PROTONS
c     general form:  parameter = A(0,J,K,L) + A(1,J,K,L)*Kp + A(2,J,K,L)*Kp*Kp
C
      INTEGER K,L
      REAL A(0:1,2,2),ATR(0:1,1)
C
      COMMON/AINDEX/A,ATR,K,L
C
C
