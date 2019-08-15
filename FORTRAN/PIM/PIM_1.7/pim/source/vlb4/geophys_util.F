      FUNCTION ap_to_kptab(iap)
C
C	Purpose:  returns a Kp associated with the passed Ap, which is
C	  one of the ap's associated with a specific Kp (N[+|o|-])

      IMPLICIT INTEGER (i-n)
      REAL ap_to_kptab
      INTEGER aptab(28)
      
      DATA aptab/0,2, 3, 4, 5, 6, 7, 9, 12, 15, 18, 22, 27, 32, 39, 48,
     1        56, 67, 80, 94, 111, 132, 154, 179, 207, 236, 300, 400/
     
      DO 10 j=1,28
          IF (iap .EQ. aptab(j)) ik = j
 10   CONTINUE
      ap_to_kptab = FLOAT(ik-1) / 3.0

      RETURN
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==

      FUNCTION ap_to_kp(ap)
C
C	Purpose:  returns a Kp associated with the passed value of Ap

      IMPLICIT INTEGER (i-n)
      REAL ap, ap_to_kp, kp, xlow,xhi,delx,dely 
c     REAL aplnmid(27)
      INTEGER aptab(27)
      LOGICAL notyet
      INCLUDE 'logicuni.inc'

c     DATA aplnmid /0., 0.8959, 1.2425, 1.4979, 1.7006, 1.8689, 2.0716,
c    1          2.3411, 2.5965, 2.7993, 2.9907, 3.1934, 3.3808, 3.5647,
c    2          3.7674, 3.9483, 4.1151, 4.2934, 4.4627, 4.6264, 4.7962,
c    3          4.9599, 5.1122, 5.2601, 5.3983, 5.5838, 5.8477/
      DATA aptab/2, 3, 4, 5, 6, 7, 9, 12, 15, 18, 22, 27, 32, 39, 48, 
     1        56, 67, 80, 94, 111, 132, 154, 179, 207, 236, 300, 400/

C      1st way:  returns the "even-third" Kp via logarithmic-rounding
C	  of the Ap value (arithmatic rounding of ln(ap))
c
c     notyet = .TRUE.
c     DO 10 j=1,27
c        IF ( (ALOG(ap) .LT. aplnmid(j)) .AND. (notyet) ) THEN
c           kp = FLOAT(j-1) / 3.0
c           notyet = .FALSE.
c        END IF
c10   CONTINUE

C      2nd way (more smoothly continuous):
C	Assign Kp based on a logarithmic interpolation (linear interpolation
C	of ln(ap)) between neighboring tabulated AP points

C  Use linear interpolation for 0<= ap <=2   (0o<=Kp<=0+)
      IF (ap .LT. FLOAT(aptab(1))) THEN
         ap_to_kp = ap/6.0
         RETURN
      END IF

      notyet = .TRUE.
      DO 20 j=2,27
         IF ( (ap .LT. FLOAT(aptab(j))) .AND. (notyet) ) THEN
            kp = FLOAT(j-1) / 3.0
            jlow = j-1
            jhi = j
            notyet = .FALSE.
         END IF
 20   CONTINUE

C  If ap=400 (a whole day of Kp=9o -- pretty unlikely), just set Kp
C   The only way to get here is if NOTYET hasn't already been set .F.
C
      IF (notyet) THEN
         IF (ap .EQ. FLOAT(aptab(27))) THEN
            ap_to_kp = 9.0
            RETURN
         ELSE
#ifdef DEBUG
            WRITE (LUSTDERR,*) 'Something appears wrong in AP_TO_KP in 
     1setting the flag NOTYET'        
#endif
         END IF
      END IF

      xlow = ALOG(FLOAT(aptab(jlow)))
      xhi = ALOG(FLOAT(aptab(jhi)))
      delx = ALOG(ap) - xlow
      dely = delx / (3.0 * (xhi-xlow))
      kp = kp + dely

      ap_to_kp = kp
      RETURN 
      END

C==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==*==

      SUBROUTINE setsct(i)
      INCLUDE 'lower.inc'
      INTEGER i
      llfsct = i
      RETURN
      END
