      SUBROUTINE pimout4 (gooddat,i,smode, imfna,nsta,nsrc,nll, tec,rmi,
     1                      dodrpim,dodrazel,delt,lutext, stav,srcv,
     2                      pyr,pdy,phr,pmin,psec, azv,elv)

C  Subroutine to print out to file results for a given UT

      INTEGER maxut,maxsta,maxsrc
      PARAMETER (maxut=800,maxsta=20, maxsrc=10)
      INTEGER i, imfna,lutext, nsta,nsrc,nll, j,k,ll
      INTEGER pyr,pdy,phr,pmin
      REAL tec(maxsta,maxsrc,2,2),rmi(maxsta,maxsrc,2,2)
      REAL t3(maxsta,maxsrc,2),sigt3(maxsta,maxsrc,2)
      REAL r3(maxsta,maxsrc,2),sigr3(maxsta,maxsrc,2)
      REAL ddelt, tecdr(maxsta,maxsrc),sigtecdr(maxsta,maxsrc)
      REAL*8 delt(2), azv(maxsta,maxsrc,2),elv(maxsta,maxsrc,2),psec
      CHARACTER smode*1, stav(maxsta)*1, srcv(maxsrc)*8
      LOGICAL*1 gooddat(maxut,maxsta,maxsrc)
      LOGICAL dodrpim,dodrazel
C------------------------------
C
C  MAXUT,MAXSTA,MAXSRC - Dimension-size of arrays:  explicitly 
C                   PARAMETERed because intermediate local arrays can't
C                   be DIMENSIONed via a passed parameter
C  I - IUT
C  IMFNA - how to handle cases where IMF data missing
C  DODRPIM,DODRAZEL - whether to calculate DR
C  TEC(jsta,ksrc,lkp,ll) - TEC in [TECU]
C  RMI(jsta,ksrc,lkp,ll) - "RotationMeasure" in [RMU]
C  DELT(2) - time-offsets for DR calculation [s]
C  LUTEXT - logical unit for output file
C
C  T3,R3 - TEC,RM collapsed in LKP (different values of Bz)
C  SIGT3,SIGR3 - "uncertainties" in resulting T3,R3
C  TECDR - TEC-rate [TECU/s].... printed out in [mTECU/s]
C  SIGTECDR - "uncertainty" in TECDR (from diff. values of Bz)
C
C-----------------------------

C  Collapse the 2 TECs/FRs arising from different Kp/Ap's
C   if missing IMF data (& so specified)
C
      IF (imfna .EQ. 2) THEN
        DO 410 j = 1, nsta
          DO 415 k = 1, nsrc
            IF (gooddat(i,j,k)) THEN
              DO 420 ll = 1, nll
                t3(j,k,ll) = (tec(j,k,2,ll) + tec(j,k,1,ll)) / 2.0
                sigt3(j,k,ll) = ABS(tec(j,k,2,ll) - tec(j,k,1,ll))
                r3(j,k,ll) = (rmi(j,k,2,ll) + rmi(j,k,1,ll)) / 2.0
                sigr3(j,k,ll) = ABS(rmi(j,k,2,ll) - rmi(j,k,1,ll))
 420          CONTINUE
            END IF
 415      CONTINUE
 410    CONTINUE
      ELSE
        DO 430 j = 1, nsta
          DO 435 k = 1, nsrc
            IF (gooddat(i,j,k)) THEN
              DO 440 ll = 1, nll
                t3(j,k,ll) = tec(j,k,1,ll)
                sigt3(j,k,ll) = 0.0
                r3(j,k,ll) = rmi(j,k,1,ll)
                sigt3(j,k,ll) = 0.0
 440          CONTINUE
            END IF
 435      CONTINUE
 430    CONTINUE
      END IF

C  Compute DR if specified
C
      IF ( (dodrpim) .OR. (dodrazel) ) THEN
        ddelt = SNGL(delt(2) - delt(1))
        DO 450 j = 1, nsta
          DO 455 k = 1, nsrc
            IF (gooddat(i,j,k)) THEN 
c  use rate of TEC based on TECs using approp. IMFNA/NLL
              tecdr(j,k) = 1.e3 * (t3(j,k,2) - t3(j,k,1)) / ddelt 
c  print out sigTECDR only if IMFNA=2 (as for TECs)
              IF (imfna .EQ. 2) THEN
                 sigtecdr(j,k) = 1.e3 * ABS(tec(j,k,2,2)-tec(j,k,2,1)-
     1                            tec(j,k,1,2)+tec(j,k,1,1)) / ddelt
              ELSE
                 sigtecdr(j,k) = 0.0
              END IF
            END IF
 455      CONTINUE
 450    CONTINUE
      ELSE
        DO 470 j = 1, nsta
          DO 475 k = 1, nsrc
            IF (gooddat(i,j,k)) THEN
              tecdr(j,k) = 0.0
              sigtecdr(j,k) = 0.0
            END IF
 475      CONTINUE
 470    CONTINUE
      END IF

C  Write out to output file (LUTEXT), depending on whether there is
C    actually data there, or if we want to keep simulation-output
C    nice and uniformly sampled
C
      DO 510 k = 1, nsrc
        DO 515 j = 1, nsta

          IF (gooddat(i,j,k)) THEN
            WRITE (lutext,519) pyr,pdy,phr,pmin,psec, stav(j), srcv(k),
     1                          t3(j,k,1),sigt3(j,k,1), tecdr(j,k),
     2                          sigtecdr(j,k), r3(j,k,1),sigr3(j,k,1),
     3                          elv(j,k,1),azv(j,k,1)
          ELSE IF (smode .EQ. 's') THEN
            WRITE (lutext,519) pyr,pdy,phr,pmin,psec, stav(j), srcv(k),
     1                          -999.0,-1.0, -999.0,-1.0, -999.0,-1.0,
     2                          -6.666,0.0
          END IF

 515    CONTINUE
 510  CONTINUE


 519  FORMAT (i2,1x,i3.3,'-',2i2.2,f5.1,2x,a1,1x,a8,3(f10.4,f8.4),2f9.3)
 529  FORMAT (i2,1x,i3.3,'-',2i2.2,f5.1,2x,a1,1x,a8,2f11.5,f11.5,2f9.3)

      END
