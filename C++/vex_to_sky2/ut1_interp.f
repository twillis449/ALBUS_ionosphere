c$$$***************************************************************************
c$$$        Gazette IERS Gazette IERS Gazette IERS Gazette IERS Gazette
c$$$                                      _____________________________________
c$$$        No 13, 30 January 1997       /
c$$$____________________________________/              Contact: iers@obspm.fr
c$$$                                       ftp: hpvlbi.obspm.fr (145.238.100.7)
c$$$                      WWW: ftp://hpvlbi.obspm.fr/iers/ierscb.html
c$$$***************************************************************************
c$$$
c$$$	Title:   INTERPOLATING IERS EARTH ORIENTATION DATA
c$$$	Authors: Dennis McCarthy, Daniel Gambis
c$$$
c$$$
c$$$INTRODUCTION
c$$$
c$$$     IERS Earth Orientation Parameters (EOP) are produced at regular intervals
c$$$(daily or longer) with the effects of semidiurnal and diurnal variations 
c$$$removed. Users who require high accuracy information may want to interpolate
c$$$the published data and include the semidiurnal/diurnal variations.
c$$$ 
c$$$     This gazette provides a recommended procedure to follow in order to
c$$$determine the most accurate Earth orientation for a given instant.
c$$$
c$$$     Currently all analysis centers contributing to the IERS employ a 
c$$$procedure using a priori values of the EOP which are then corrected through 
c$$$the analysis of observations.  These a priori values are best estimates based 
c$$$on the standard knowledge of the Earth's orientation, usually the interpolated 
c$$$tabular values plus a diurnal/semidiurnal model plus lower frequency tides 
c$$$(if appropriate). The corrections to the a priori estimates are determined 
c$$$from the analyses of data taken over some period of time, ranging from minutes 
c$$$to days. Thus, they represent the mean of the unmodelled variations over the 
c$$$length of the analyzed period of time. When the reported estimates (a priori 
c$$$+ mean estimated correction) are based on distinct time intervals, they may 
c$$$be referred to as normal points.
c$$$ 
c$$$     The IERS provides to users polar motion and UT1 based on the combination 
c$$$of the analysis centers data, using either smoothed estimates in order to 
c$$$reduce observational errors (Bulletins A and B, EOP 97 C 04, see Explanatory 
c$$$Supplement to IERS Bulletins A and B), or normal points (EOP 97 C 01, 02, 03, 
c$$$see IERS 1995 Annual Report, part II.4). These results are provided without the 
c$$$effects of the diurnal/semidiurnal tides. The IERS Conventions (McCarthy 1996) 
c$$$for transformations between terrestrial and celestial frames, however, imply 
c$$$that IERS EOP data are exact estimates at the instant in time reported.
c$$$
c$$$SEMIDIURNAL/DIURNAL VARIATIONS
c$$$
c$$$     The existence of diurnal and semidiurnal variations caused by ocean tides 
c$$$is well known (Eubanks 1993; Sovers et al. 1993; Herring & Dong 1994; Ray et 
c$$$al. 1994).  In recent years, through continuous, high-precision VLBI 
c$$$experiments (e.g. ERDE, EPOCH92, CONT93, etc.), analytical models have been 
c$$$derived for the prominent diurnal and semidiurnal tides (Herring and Dong 1991;
c$$$Brosche et al. 1991; Herring 1993; Herring and Dong 1994; Gipson 1996).  
c$$$This in turn has prompted a refinement in the theoretical models (Brosche et 
c$$$al. 1989; Seiler 1991; Dickman 1989, 1990, 1991, 1993; Gross 1993; Ray et al. 
c$$$1994; Ray 1995; Seiler and Wunsch 1995).
c$$$
c$$$     As the observational data have improved in accuracy, the models (both 
c$$$empirical and theoretical) have quickly converged. Although there are 
c$$$differences between recent models (e.g. Ray et al. 1994; Ray 1995; Seiler 
c$$$and Wunsch 1995; Gipson 1996), these differences are less than 10%. The 
c$$$effects of these tides are on the order of 0.1 milliarcseconds (mas) in the 
c$$$polar motion coordinates x and y and 10**-6 s in UT1.  The inclusion of any 
c$$$of these models in high-precision, Earth orientation analysis software clearly 
c$$$improves the solution.
c$$$
c$$$     If the data do not correctly model the diurnal/semidiurnal tide, the 
c$$$errors can reach up to +/-0.1 milliarcseconds (mas). Systematic errors of this 
c$$$magnitude may be unacceptable to high-accuracy users.  Possible ambiguities 
c$$$caused by unclear procedures concerning subdiurnal EOPs and the epoch of 
c$$$observation can be eliminated.  Although the problems are only on the fringes 
c$$$of detectability, they may cause significant systematic errors. It is expected 
c$$$that eventually subdiurnal observations of EOPs will be possible routinely from 
c$$$a reduction standpoint.
c$$$
c$$$RECOMMENDATION
c$$$
c$$$     The following software is recommended to interpolate the IERS polar motion 
c$$$and Universal Time products and account for the semidiurnal/diurnal variations 
c$$$in the Earths orientation. This procedure makes use of a Lagrangian 
c$$$interpolation scheme and applies the Ray model of the semidiurnal/diurnal 
c$$$variations in the Earth's orientation as recommended in the IERS Conventions
c$$$(McCarthy, 1996). Any equivalent interpolation scheme could, of course, be 
c$$$substituted.  
c$$$
c$$$This software can be obtained in machine readable form by anonymous ftp to 
c$$$
c$$$   maia.usno.navy.mil, cd to dist, and get interp.f.
c$$$or
c$$$   hpvlbi.obspm.fr; cd iers/model; get interp.f (interp.note is the above text)
c$$$------------
      SUBROUTINE INTERP_UT1 (RJD,X,Y,T,N,RJDINT,XINT,YINT,TINT)
C
C     THIS SUBROUTINE TAKES A SERIES OF X, Y, AND UT1-UTC VALUES
C     AND INTERPOLATES THEM TO AN EPOCH OF CHOICE.  THIS ROUTINE
C     ASSUMES THAT THE VALUES OF X AND Y ARE IN SECONDS OF
C     ARC AND THAT UT1-UTC IS IN SECONDS OF TIME.  AT LEAST
C     ONE POINT BEFORE AND ONE POINT AFTER THE EPOCH OF THE
C     INTERPOLATION POINT ARE NECESSARY IN ORDER FOR THE
C     INTERPOLATION SCHEME TO WORK.
C
C     PARAMETERS ARE :
C     RJD   - ARRAY OF THE EPOCHS OF DATA (GIVEN IN MJD)
C     X     - ARRAY OF X POLAR MOTION (ARCSEC)
C     Y     - ARRAY OF Y POLAR MOTION (ARCSEC)
C     T     - ARRAY OF UT1-UTC (SEC)
C     N     - NUMBER OF POINTS IN ARRAYS
C     RJDINT- EPOCH FOR THE INTERPOLATED VALUE
C     XINT  - INTERPOLATED VALUE OF X
C     YINT  - INTERPOLATED VALUE OF Y
C     TINT  - INTERPOLATED VALUE OF UT1-UTC
C
      DOUBLE PRECISION RJD(N), X(N), Y(N), T(N),
     . RJDINT, XINT, YINT, TINT, CORX, CORY, CORT
      CALL LAGINT (RJD,X,N,RJDINT,XINT)
      CALL LAGINT (RJD,Y,N,RJDINT,YINT)
      CALL LAGINT (RJD,T,N,RJDINT,TINT)
      CALL RAY (RJDINT,CORX,CORY,CORT)
      XINT = XINT + CORX
      YINT = YINT + CORY
      TINT = TINT + CORT
      RETURN
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE LAGINT (X,Y,N,XINT,YOUT)
C 
C     THIS SUBROUTINE PERFORMS LAGRANGIAN INTERPOLATION
C     WITHIN A SET OF (X,Y) PAIRS TO GIVE THE Y
C     VALUE CORRESPONDING TO XINT.  THIS PROGRAM USES A
C     WINDOW OF 4 DATA POINTS TO PERFORM THE INTERPOLATION.
C     IF THE WINDOW SIZE NEEDS TO BE CHANGED, THIS CAN BE
C     DONE BY CHANGING THE INDICES IN THE DO LOOPS FOR
C     VARIABLES M AND J.
C
C     PARAMETERS ARE :
C     X     - ARRAY OF VALUES OF THE INDEPENDENT VARIABLE
C     Y     - ARRAY OF FUNCTION VALUES CORRESPONDING TO X
C     N     - NUMBER OF POINTS
C     XINT  - THE X-VALUE FOR WHICH ESTIMATE OF Y IS DESIRED
C     YOUT  - THE Y VALUE RETURNED TO CALLER
C
C     2005 Sep 20  James M Anderson  --JIVE  VEX files often seem to
C                              only have 3 values, so change indices
C                              for 3 from 4 (change N-2 to N-1, 
C                              K+2 to K+1)
C
      REAL*8 X(N),Y(N),XINT,YOUT,TERM
      INTEGER N,I,J
C
      YOUT = 0.0D0
      DO 5 I = 1,N-1
        IF ( XINT .GE. X(I) .AND. XINT .LT. X(I+1) ) K = I
    5 CONTINUE
      IF ( K .LT. 2 ) K = 2
      IF ( K .GT. N-1 ) K = N-1
      DO 20 M = K-1,K+1
        TERM = Y(M)
        DO 10 J = K-1,K+1
          IF ( M .NE. J ) THEN
            TERM = TERM * (XINT - X(J))/(X(M) - X(J))
          END IF
   10   CONTINUE
        YOUT = YOUT + TERM
   20 CONTINUE
      RETURN
      END

C
C----------------------------------------------------------------
C
      SUBROUTINE RAY (RJD,CORX,CORY,CORT)
C
C   THIS SUBROUTINE IMPLEMENTS THE RAY MODEL FOR
C   DIURNAL/SUBDIURNAL TIDES.  IT USES THE SIMON ET AL.
C   FUNDAMENTAL ARGUMENTS.  THE CORRECTIONS IN X AND Y ARE IN
C   UNITS OF SEC. OF ARC AND UT1-UTC IN SEC. OF TIME.  THESE
C   CORRECTIONS SHOULD BE ADDED TO "AVERAGE" EOP VALUES TO GET
C   ESTIMATES OF THE INSTANTANEOUS VALUES.
C
C     PARAMETERS ARE :
C     RJD   - EPOCH OF INTEREST GIVEN IN MJD
C     CORX  - TIDAL CORRECTION IN X (SEC. OF ARC)
C     CORY  - TIDAL CORRECTION IN Y (SEC. OF ARC)
C     CORT  - TIDAL CORRECTION IN UT1-UTC (SEC. OF TIME)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION
     .   L,        LPRIME
      HALFPI = 1.5707963267948966d0
      T = (RJD - 51544.5D0)/36525.0D0
      L = -0.00024470d0*T**4 + 0.051635d0*T**3 + 31.8792d0*T**2
     .  + 1717915923.2178d0*T + 485868.249036d0
      L = DMOD(L,1296000d0)
      LPRIME = -0.00001149d0*T**4 - 0.000136d0*T**3
     .  -  0.5532d0*T**2
     .  + 129596581.0481d0*T + 1287104.79305d0
      LPRIME = DMOD(LPRIME,1296000d0)
      CAPF = 0.00000417d0*T**4 - 0.001037d0*T**3 - 12.7512d0*T**2
     .  + 1739527262.8478d0*T + 335779.526232d0
      CAPF = DMOD(CAPF,1296000d0)
      CAPD = -0.00003169d0*T**4 + 0.006593d0*T**3 - 6.3706d0*T**2
     .  + 1602961601.2090d0*T + 1072260.70369d0
      CAPD = DMOD(CAPD,1296000d0)
      OMEGA = -0.00005939d0*T**4 + 0.007702d0*T**3
     .  + 7.4722d0*T**2
     .  - 6962890.2665d0*T + 450160.398036d0
      OMEGA = DMOD(OMEGA,1296000d0)
      THETA = (67310.54841d0 +
     .        (876600d0*3600d0 + 8640184.812866d0)*T +
     .         0.093104d0*T**2 -
     .         6.2d-6*T**3)*15.0d0 + 648000.0d0
      ARG7 = DMOD((-L - 2.0D0*CAPF - 2.0D0*OMEGA + THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0) - HALFPI
      ARG1 = DMOD((-2.0d0*CAPF - 2.0d0*OMEGA + THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0) - HALFPI
      ARG2 = DMOD((-2.0d0*CAPF + 2.0d0*CAPD - 2.0d0*OMEGA
     .       + THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0) - HALFPI
      ARG3 = DMOD(THETA *
     .        3.14159265D0/648000.0D0,6.28318530718D0)
     .     + HALFPI
      ARG4 = DMOD((-L - 2.0d0*CAPF - 2.0D0*OMEGA + 2.0d0*THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0)
      ARG5 = DMOD((-2.0D0*CAPF - 2.0D0*OMEGA + 2.0d0*THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0)
      ARG6 = DMOD((-2.0d0*CAPF + 2.0d0*CAPD - 2.0d0*OMEGA
     .     + 2.0d0*THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0)
      ARG8 = DMOD((2.0d0*THETA)
     .     * 3.14159265D0/648000.0D0,6.28318530718D0)
      CORX = - 0.026D0*DSIN(ARG7) + 0.006D0*DCOS(ARG7)
     .       - 0.133D0*DSIN(ARG1) + 0.049D0*DCOS(ARG1)
     .       - 0.050D0*DSIN(ARG2) + 0.025D0*DCOS(ARG2)
     .       - 0.152D0*DSIN(ARG3) + 0.078D0*DCOS(ARG3)
     .       - 0.057D0*DSIN(ARG4) - 0.013D0*DCOS(ARG4)
     .       - 0.330D0*DSIN(ARG5) - 0.028D0*DCOS(ARG5)
     .       - 0.145D0*DSIN(ARG6) + 0.064D0*DCOS(ARG6)
     .       - 0.036D0*DSIN(ARG8) + 0.017D0*DCOS(ARG8)
      CORY = - 0.006D0*DSIN(ARG7) - 0.026D0*DCOS(ARG7)
     .       - 0.049D0*DSIN(ARG1) - 0.133D0*DCOS(ARG1)
     .       - 0.025D0*DSIN(ARG2) - 0.050D0*DCOS(ARG2)
     .       - 0.078D0*DSIN(ARG3) - 0.152D0*DCOS(ARG3)
     .       + 0.011D0*DSIN(ARG4) + 0.033D0*DCOS(ARG4)
     .       + 0.037D0*DSIN(ARG5) + 0.196D0*DCOS(ARG5)
     .       + 0.059D0*DSIN(ARG6) + 0.087D0*DCOS(ARG6)
     .       + 0.018D0*DSIN(ARG8) + 0.022D0*DCOS(ARG8)
      CORT = + 0.0245D0*DSIN(ARG7) + 0.0503D0*DCOS(ARG7)
     .       + 0.1210D0*DSIN(ARG1) + 0.1605D0*DCOS(ARG1)
     .       + 0.0286D0*DSIN(ARG2) + 0.0516D0*DCOS(ARG2)
     .       + 0.0864D0*DSIN(ARG3) + 0.1771D0*DCOS(ARG3)
     .       - 0.0380D0*DSIN(ARG4) - 0.0154D0*DCOS(ARG4)
     .       - 0.1617D0*DSIN(ARG5) - 0.0720D0*DCOS(ARG5)
     .       - 0.0759D0*DSIN(ARG6) - 0.0004D0*DCOS(ARG6)
     .       - 0.0196D0*DSIN(ARG8) - 0.0038D0*DCOS(ARG8)
      CORX = CORX * 1.0d-3
      CORY = CORY * 1.0d-3
      CORT = CORT * 0.1d-3
      RETURN
      END
c$$$----------------
c$$$REFERENCES
c$$$
c$$$Brosche, P., Seiler, U., Sundermann, J., and Wunsch, J., 1989,
c$$$     "Periodic Changes in Earth's Rotation due to Oceanic Tides,"
c$$$     Astron. Astrophys., 220, pp. 318-320.
c$$$
c$$$Brosche, P., Wunsch, J., Campbell, J., and Schuh, H., 1991,
c$$$     "Ocean Tide Effects in Universal Time detected by VLBI,"
c$$$     Astron. Astrophys., 245, pp. 676-682.
c$$$
c$$$Dickman, S. R., 1989, "A Complete Spherical Harmonic Approach to
c$$$     Luni-Solar Tides," Geophys. J., 99, pp. 457-468.
c$$$
c$$$Dickman, S. R., 1990, "Experiments in Tidal Mass Conservation,"
c$$$     (research note), Geophys. J., 102, pp. 257-262.
c$$$
c$$$Dickman, S. R., 1991, "Ocean Tides for Satellite Geodesy," Mar.
c$$$     Geod., 14, pp. 21-56.
c$$$
c$$$Dickman, S. R., 1993, "Dynamic ocean-tide effect on Earth's
c$$$     rotation," Geophys. J. Int., 112, pp. 448-470.
c$$$
c$$$Eubanks, T. M., 1993, "Variations in the Orientation of the
c$$$     Earth," in Contributions of Space Geodesy to Geodynamics:
c$$$     Earth Dynamics Geodynamics, American Geophysical Union,
c$$$     Washington, DC, 24, pp. 1-54.
c$$$
c$$$Gipson, J., 1996, "VLBI Determination of Neglected Tidal Terms in
c$$$     High-Frequency Earth Orientation Variation," submitted to J.
c$$$     Geophys. Res.
c$$$
c$$$Gross, R. S., 1993, "The Effect of Ocean Tides on he Earth's
c$$$     Rotation as Predicted by the Results of an Ocean Tide
c$$$     Model," Geophys. Res. Lett., 20, pp. 293-296.
c$$$
c$$$Herring, T. A., 1993, "Diurnal and semi-diurnal variations in
c$$$     Earth rotation," in The Orientation of the Planet Earth as
c$$$     Observed by Modern Space Techniques, M. Feissel (ed.),
c$$$     Pergamon Press, in press.
c$$$
c$$$Herring, T. A. and Dong, D., 1991, "Current and future accuracy
c$$$     of Earth rotation measurements," in Proceedings of the
c$$$     Chapman conference on Geodetic VLBI: Monitoring Global
c$$$     Change, NOAA Technical Report NOS 137 NGS 49, pp. 306-324.
c$$$
c$$$Herring, T. A. and Dong, D., 1994, "Measurement of Diurnal and
c$$$     Semidiurnal Rotational Variations and Tidal Parameters of
c$$$     Earth," J. Geophys. Res., 99, pp. 18051-18071.
c$$$
c$$$IERS Annual Report, Available from the Central Bureau of the
c$$$     IERS, Paris Observatory, Paris.
c$$$
c$$$McCarthy, D. D., 1996, IERS Conventions, IERS Technical Note, 21.
c$$$
c$$$Ray, R., Steinberg, D. J., Chao, B. F., and Cartwright, D. E.,
c$$$     1994, " Diurnal and Semidiurnal Variations in the Earth's
c$$$     Rotation Rate Induced by Oceanic Tides," Science, 264, pp.
c$$$     830-832.
c$$$
c$$$Ray, R., 1995, Personal Communication.
c$$$
c$$$Seiler, U., 1991, "Periodic Changes of the Angular Momentum
c$$$     Budget due to the Tides of the World Ocean," J. Geophys.
c$$$     Res., 96, pp. 10287-10300.
c$$$
c$$$Seiler, U. and Wunsch, J. 1995, "A refined model for the
c$$$     influence of ocean tides on UT1 and polar motion," Astron.
c$$$     Nachr., 316, pp. 419-423.
c$$$
c$$$Sovers, O. J., Jacobs, C. S., and Gross, R. S., 1993, "Measuring
c$$$     rapid ocean tidal Earth orientation variations with VLBI, J.
c$$$     Geophys. Res., 98, 19959-19971.
