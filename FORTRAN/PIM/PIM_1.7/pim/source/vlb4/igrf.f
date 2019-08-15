C SHELLIG.FOR, Version 2.0, January 1992
C
C 11/01/91-DKB- SHELLG: lowest starting point for B0 search is 2  
C  1/27/92-DKB- Adopted to IGRF-91 coeffcients model
C  2/05/92-DKB- Reduce variable-names: INTER(P)SHC,EXTRA(P)SHC,INITI(ALI)ZE
C  8/08/95-DKB- Updated to IGRF-45-95; new coeff. DGRF90, IGRF95, IGRF95S
C
C  30 Mar 98 - RMC:  Keep only those subroutines needed for terrestrial
C                    magnetic field calculation in PIMVLBI (lose FINDB0,
C                    SHELLG, STOER)
C                    Added variable declarations to match PIM standards
C
C*********************************************************************
C  SUBROUTINES (FINDB0), (SHELLG), (STOER), FELDG, FELDCOF, GETSHC,  *
C	INTERSHC, EXTRASHC, INITIZE                                  *
C*********************************************************************
C*********************************************************************
C
C
      SUBROUTINE FELDG(GLAT,GLON,ALT,BNORTH,BEAST,BDOWN,BABS)           
C-------------------------------------------------------------------
C CALCULATES EARTH MAGNETIC FIELD FROM SPHERICAL HARMONICS MODEL
C REF: G. KLUGE, EUROPEAN SPACE OPERATIONS CENTRE, INTERNAL NOTE 61, 
C      1970.
C--------------------------------------------------------------------
C CHANGES (D. BILITZA, NOV 87):
C   - FIELD COEFFICIENTS IN BINARY DATA FILES INSTEAD OF BLOCK DATA
C   - CALCULATES DIPOL MOMENT
C--------------------------------------------------------------------
C  INPUT:  ENTRY POINT FELDG
C	 	GLAT  GEODETIC LATITUDE IN DEGREES (NORTH)
C         	GLON  GEODETIC LONGITUDE IN DEGREES (EAST)
C         	ALT   ALTITUDE IN KM ABOVE SEA LEVEL
C
C    COMMON /igrfstuff/ & /igrfstuff1/  (strings)
C		UMR     = SNGL(degrad)
C		ERA	EARTH RADIUS FOR NORMALIZATION OF CARTESIAN 
C			COORDINATES (6371.2 KM)
C		AQUAD, BQUAD   SQUARE OF MAJOR AND MINOR HALF AXIS FOR 
C			EARTH ELLIPSOID AS RECOMMENDED BY INTERNATIONAL 
C			ASTRONOMICAL UNION (6378.160, 6356.775 KM).
C		NMAX    MAXIMUM ORDER OF SPHERICAL HARMONICS
C		TIME	YEAR (DECIMAL: 1973.5) FOR WHICH MAGNETIC 
C			FIELD IS TO BE CALCULATED
C		G(M)	NORMALIZED FIELD COEFFICIENTS (SEE FELDCOF)
C			M=NMAX*(NMAX+2)
C------------------------------------------------------------------------
C  OUTPUT: BABS   MAGNETIC FIELD STRENGTH IN GAUSS
C	   BNORTH, BEAST, BDOWN   COMPONENTS OF THE FIELD WITH RESPECT
C		  TO THE LOCAL GEODETIC COORDINATE SYSTEM, WITH AXIS
C		  POINTING IN THE TANGENTIAL PLANE TO THE NORTH, EAST
C		  AND DOWNWARD.   
C-----------------------------------------------------------------------

	integer ih,il,i,imax, ihmax,last
	integer k,m, nmax
	real umr, era, aquad, bquad, time
	real glat, glon, alt, bnorth, beast, bdown, babs
	real cp,sp,ct,st,d,rlat,rlon, zzz,rho,xxx,yyy, rq,xi
	real g,h, f,x,y,z, s,t, bxxx,byyy,bzzz, brho

      CHARACTER*12 	NAME
      COMMON 		XI(3),H(144)
c     COMMON/MODEL/	NAME,NMAX,TIME,G(144)  
c     COMMON/GENER/	UMR,ERA,AQUAD,BQUAD
      COMMON /igrfstuff/ umr, era, aquad, bquad, nmax, time, g(144)
      COMMON /igrfstuff1/ name

C-- IS RECORDS ENTRY POINT
C
C*****ENTRY POINT  FELDG  TO BE USED WITH GEODETIC CO-ORDINATES         
 
      RLAT=GLAT*UMR
      CT=SIN(RLAT)                                                      
      ST=COS(RLAT)                                                      
      D=SQRT(AQUAD-(AQUAD-BQUAD)*CT*CT)                                 
      RLON=GLON*UMR
      CP=COS(RLON)                                                      
      SP=SIN(RLON)                                                      
      ZZZ=(ALT+BQUAD/D)*CT/ERA
      RHO=(ALT+AQUAD/D)*ST/ERA
      XXX=RHO*CP                                                       
      YYY=RHO*SP                                                       
      RQ=1./(XXX*XXX+YYY*YYY+ZZZ*ZZZ) 
      XI(1)=XXX*RQ                                                      
      XI(2)=YYY*RQ                                                      
      XI(3)=ZZZ*RQ                                                      

      IHMAX=NMAX*NMAX+1                                                 
      LAST=IHMAX+NMAX+NMAX                                              
      IMAX=NMAX+NMAX-1                                                  
      DO 8 I=IHMAX,LAST                                                 
 8       H(I)=G(I)                
      DO 6 K=1,3,2                                                      
         I=IMAX                                                   
         IH=IHMAX                         
 1       IL=IH-I                         
         F=2./FLOAT(I-K+2)              
         X=XI(1)*F                    
         Y=XI(2)*F                   
         Z=XI(3)*(F+F)               
         I=I-2                      
         IF (I-1) 5,4,2              
 2       DO 3 M=3,I,2             
            H(IL+M+1)=G(IL+M+1)+Z*H(IH+M+1)+X*(H(IH+M+3)-H(IH+M-1)) 
     &                                     -Y*(H(IH+M+2)+H(IH+M-2))
            H(IL+M)=G(IL+M)+Z*H(IH+M)+X*(H(IH+M+2)-H(IH+M-2))     
     &                               +Y*(H(IH+M+3)+H(IH+M-1))    
 3       CONTINUE
 4       H(IL+2)=G(IL+2)+Z*H(IH+2)+X*H(IH+4)-Y*(H(IH+3)+H(IH))
         H(IL+1)=G(IL+1)+Z*H(IH+1)+Y*H(IH+4)+X*(H(IH+3)-H(IH))
 5       H(IL)=G(IL)+Z*H(IH)+2.*(X*H(IH+1)+Y*H(IH+2))  
         IH=IL                    
         IF (I.GE.K) GOTO 1         
6     CONTINUE                                            
 
      S=.5*H(1)+2.*(H(2)*XI(3)+H(3)*XI(1)+H(4)*XI(2))                   
      T=(RQ+RQ)*SQRT(RQ)                                                
      BXXX=T*(H(3)-S*XXX)                                               
      BYYY=T*(H(4)-S*YYY)                                               
      BZZZ=T*(H(2)-S*ZZZ)                                               
      BABS=SQRT(BXXX*BXXX+BYYY*BYYY+BZZZ*BZZZ)
      BEAST=BYYY*CP-BXXX*SP                                             
      BRHO=BYYY*SP+BXXX*CP                                              
      BNORTH=BZZZ*ST-BRHO*CT                                            
      BDOWN=-BZZZ*CT-BRHO*ST                                            
      RETURN                                                            
      END                                                               
C
C
	SUBROUTINE FELDCOF(YEAR, pigrf, DIMO)
C-----------------------------------------------------------------------
C  DETERMINES COEFFICIENTS AND DIPOL MOMENT FROM IGRF MODELS
C
C	INPUT:  YEAR	DECIMAL YEAR FOR WHICH GEOMAGNETIC FIELD IS TO
C			BE CALCULATED
C               PIGRF   Path for IGRF magnetic-field Spher.Harm.Coeff.
C                       databases
C
C	OUTPUT:	DIMO	GEOMAGNETIC DIPOL MOMENT IN GAUSS (NORMALIZED 
C			TO EARTH'S RADIUS) AT THE TIME (YEAR)
C  D. BILITZA, NSSDC, GSFC, CODE 633, GREENBELT, MD 20771, 
C	(301)286-9536   NOV 1987.
C
C Mod 23jan2003 (R.Campbell)
C   Read single IGRF2000 table (each column = different year) to get [I/D]GRF
C     models (calls to GETSHC not needed); passing to INTERSHC/EXTRASHC
C     unchanged.
C-----------------------------------------------------------------------

C ### numye = numye + 1 ; is number of years represented by IGRF
C        now = 21 (1900-->2000); extrapolation model = 22nd one
C        use as parameter to dimension DTEMOD, GHARR
      INTEGER NUMYE
      PARAMETER (NUMYE=21)
      INTEGER nmax,nmax1,nmax2, iyea, i,iu,is,j,l,m,n
c     INTEGER  ier
      REAL umr, erad, aquad, bquad, dtemod,dte1,dte2
      REAL year,time, gh1,gh2,gha, gharr
      REAL*8 dimo, x, f0, f, sqrt2
      LOGICAL bestaat
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
      CHARACTER pigrf*256, filigrf*256
      CHARACTER*12 FIL1
c     CHARACTER*12 FILMOD, FIL2
      DIMENSION GH1(144),GH2(120),GHA(144),DTEMOD(numye+1)
      DIMENSION gharr(numye+1, 144)
c       DIMENSION FILMOD(12)

c	COMMON/MODEL/	FIL1,NMAX,TIME,GH1
c	COMMON/GENER/	UMR,ERAD,AQUAD,BQUAD
      COMMON /igrfstuff/ umr,erad,aquad,bquad, nmax,time, gh1
      COMMON /igrfstuff1/ fil1

C ### changed to conform with IGRF 45-95, also FILMOD, DTEMOD arrays +1
c       DATA		FILMOD /'dgrf45.dat', 'dgrf50.dat',            
c    1			'dgrf55.dat', 'dgrf60.dat', 'dgrf65.dat',      
c    2			'dgrf70.dat', 'dgrf75.dat', 'dgrf80.dat',      
c    3			'dgrf85.dat', 'dgrf90.dat', 'igrf95.dat',
c    4			'igrf95s.dat'/
c      DATA DTEMOD / 1945., 1950., 1955., 1960., 1965., 1970., 1975.,
c    1               1980., 1985., 1990., 1995., 2000. /      
C
C      Read IGRF2000 file here
C  IU  IS INPUT UNIT NUMBER FOR IGRF COEFFICIENT SETS
C
      IU = 66
      CALL STRCCT (pigrf,'igrf2000.dat',32, filigrf, i)
      INQUIRE (FILE=filigrf, EXIST=bestaat)
      IF (.NOT. bestaat) THEN
         WRITE (*,109) filigrf
 109     FORMAT ("Can't find IGRF file:  ",a)
         STOP
      END IF
      OPEN (IU, FILE=filigrf, STATUS='OLD')
      READ (IU, 119) (dtemod(i), i=1,numye)
      dtemod(numye+1) = 2005.0
 119  FORMAT (7x, 21f7.1)
      DO 120 j=1,120
        READ (IU, 129) (gharr(i,j), i=1,numye+1) 
 120  CONTINUE
 129  FORMAT (7x,21f7.0,f7.1)
      CLOSE (IU)

C  constants not read in new single-table
      NMAX1 = 10
      NMAX2 = 10
      ERAD = 6371.2

C  IS=0 FOR SCHMIDT NORMALIZATION   IS=1 GAUSS NORMALIZATION
      IS = 0

C-- DETERMINE IGRF-YEARS FOR INPUT-YEAR
      TIME = YEAR
      IYEA = INT(YEAR/5.)*5
      L = (IYEA - 1900)/5 + 1
      IF (L.LT.1) L=1
      IF (L.GT.NUMYE) L=NUMYE         
      DTE1 = DTEMOD(L)   
      DTE2 = DTEMOD(L+1) 
c     FIL1 = FILMOD(L)   
c     FIL2 = FILMOD(L+1) 

C-- GET IGRF COEFFICIENTS FOR THE BOUNDARY YEARS
C
c       CALL GETSHC (IU, FIL1, pigrf, NMAX1, ERAD, GH1, IER)  
c       IF (IER .NE. 0) THEN
c              WRITE (*,*) 'Error in 1st GETSHC'
c              write (*,*) iu, fil1, pigrf 
c              STOP
c       END IF
c       CALL GETSHC (IU, FIL2, pigrf, NMAX2, ERAD, GH2, IER)  
c       IF (IER .NE. 0) STOP

C     New IGRF2000 way - load GH1 & GH2 from GHARR
      DO 140 j=1,120
        GH1(j) = GHARR(l, j)
        GH2(j) = GHARR(l+1, j)
 140  CONTINUE
 
C-- DETERMINE IGRF COEFFICIENTS FOR YEAR
C
      IF (L .LE. NUMYE-1) THEN                        
        CALL INTERSHC (YEAR,DTE1,NMAX1,GH1,DTE2,nmax2,gh2,nmax,gha) 
      ELSE               
        CALL EXTRASHC (YEAR,DTE1,NMAX1,GH1,NMAX2,gh2,nmax,gha)
      ENDIF 

C-- DETERMINE MAGNETIC DIPOL MOMENT AND COEFFIECIENTS G
	F0=0.D0
	DO 1234 J=1,3
	   F = GHA(J) * 1.D-5
	   F0 = F0 + F * F
1234	CONTINUE
	DIMO = DSQRT(F0)

	GH1(1) =  0.0
	I=2          
      	F0=1.D-5                
      	IF(IS.EQ.0) F0=-F0 
      	SQRT2=DSQRT(2.d0)      

      DO 9 N=1,NMAX           
	X = DBLE(N)
      	F0 = F0 * X * X / (4.D0 * X - 2.D0)               
      	IF(IS.EQ.0) F0 = F0 * (2.D0 * X - 1.D0) / X
	F = F0 * 0.5D0                                    
      	IF(IS.EQ.0) F = F * SQRT2
	GH1(I) = GHA(I-1) * SNGL(F0)
	I = I+1                                         
      DO 9 M=1,N                                    
      	F = F * (X + M) / (X - M + 1.D0)                 
	IF(IS.EQ.0) F = F * DSQRT((X - M + 1.D0) / (X + M))      	
	GH1(I) = GHA(I-1) * SNGL(F)
	GH1(I+1) = GHA(I) * SNGL(F)
        I=I+2
9     CONTINUE                                          
	RETURN
	END
C
C
	SUBROUTINE GETSHC (IU, FSPEC, pigrf, NMAX, ERAD, GH, IER)
                                                              
C ===============================================================  
C                                                               
C	Version 1.01                                                 
C                                                                   
C	Reads spherical harmonic coefficients from the specified     
C	file into an array.                                          
C                                                                
C	Input:                                                       
C	    IU    - Logical unit number [66] 
C	    FSPEC - File specification
C           PIGRF - path for IGRF magnetic-field Spher.Harm.Coeff.
C                      databases (i.e., FSPEC)
C                                                              
C	Output:                                                      
C	    NMAX  - Maximum degree and order of model                
C	    ERAD  - Earth's radius associated with the spherical     
C		    harmonic coefficients, in the same units as      
C		    elevation                                        
C	    GH    - Schmidt quasi-normal internal spherical          
C		    harmonic coefficients                            
C	    IER   - Error number: =  0, no error                     
C				  = -2, records out of order         
C			     	  = FORTRAN run-time error number    
C                                                                    
C	A. Zunde                                                     
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225    
C                                                            
C ===============================================================  
        
	INTEGER i, n,m, nn,mm, iu, nmax, ier 
	REAL erad, gh, g,h
C	2005 Aug 30  James M Anderson  --JIVE  80 characters for a path length
C                                        is far too short for me.  Bump this up
C                                        to 256
        CHARACTER pigrf*256, filigrf*256, fspec*(*)
	DIMENSION	GH(*)                                        
                                            
C ---------------------------------------------------------------    
C	Open coefficient file. Read past first header record.
C          (use PIM routines to concatenate path & filename) 
C	Read degree and order of model and Earth's radius.
C ---------------------------------------------------------------  

        CALL STRCCT (pigrf, fspec, 32, filigrf, i) 
	OPEN (IU, FILE=filigrf, STATUS='OLD', IOSTAT=IER, ERR=999)     
	READ (IU, *, IOSTAT=IER, ERR=999)                            
	READ (IU, *, IOSTAT=IER, ERR=999) NMAX, ERAD                 
C ---------------------------------------------------------------   
C	Read the coefficient file, arranged as follows:              
C                                                                   
C					N     M     G     H          
C					----------------------       
C				    /   1     0    GH(1)  -          
C				   /	1     1    GH(2) GH(3)       
C				  /	2     0    GH(4)  -          
C				 /	2     1    GH(5) GH(6)       
C	    NMAX*(NMAX+3)/2 	/	2     2    GH(7) GH(8)       
C	       records		\	3     0    GH(9)  -          
C				 \      .     .     .     .          
C				  \	.     .     .     .          
C	    NMAX*(NMAX+2)	   \	.     .     .     .          
C	    elements in GH	    \  NMAX  NMAX   .     .          
C                                                               
C	N and M are, respectively, the degree and order of the       
C	coefficient.                                                 
C ---------------------------------------------------------------    
                                                                    
	I = 0                                                        
	DO 2211 NN = 1, NMAX              
	    DO 2233 MM = 0, NN
		READ (IU, *, IOSTAT=IER, ERR=999) N, M, G, H         
		IF (NN .NE. N .OR. MM .NE. M) THEN                   
		    IER = -2                                         
		    GOTO 999                                         
		ENDIF                                                
		I = I + 1                                            
		GH(I) = G                                            
		IF (M .NE. 0) THEN                                   
		    I = I + 1                                        
		    GH(I) = H                                        
		ENDIF                                                
2233   	    CONTINUE                                                    
2211	CONTINUE 
         
999	CLOSE (IU)                                                   
        
	RETURN                                                       
	END                                                          
C
C
	SUBROUTINE INTERSHC (DATE, DTE1, NMAX1, GH1, DTE2,          
     1			      NMAX2, GH2, NMAX, GH)                  
 
C =============================================================== 
C         
C	Version 1.01                                                 
C
C	Interpolates linearly, in time, between two spherical        
C	harmonic models.                                             
C
C	Input:                                                       
C	    DATE  - Date of resulting model (in decimal year)        
C	    DTE1  - Date of earlier model                            
C	    NMAX1 - Maximum degree and order of earlier model        
C	    GH1   - Schmidt quasi-normal internal spherical          
C		    harmonic coefficients of earlier model           
C	    DTE2  - Date of later model                              
C	    NMAX2 - Maximum degree and order of later model          
C	    GH2   - Schmidt quasi-normal internal spherical          
C		    harmonic coefficients of later model             
C
C	Output:                                                      
C	    GH    - Coefficients of resulting model                  
C	    NMAX  - Maximum degree and order of resulting model      
C
C	A. Zunde                                                     
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225    
C
C ===============================================================   

	integer i,k,l
	integer nmax1, nmax2, nmax
	real date, dte1, gh1, dte2, gh2, gh, factor
	DIMENSION	GH1(*), GH2(*), GH(*)                        

C ---------------------------------------------------------------   
C	The coefficients (GH) of the resulting model, at date        
C	DATE, are computed by linearly interpolating between the     
C	coefficients of the earlier model (GH1), at date DTE1,       
C	and those of the later model (GH2), at date DTE2. If one     
C	model is smaller than the other, the interpolation is        
C	performed with the missing coefficients assumed to be 0.     
C --------------------------------------------------------------- 

	FACTOR = (DATE - DTE1) / (DTE2 - DTE1)                       

	IF (NMAX1 .EQ. NMAX2) THEN                                   
	    K = NMAX1 * (NMAX1 + 2)                                  
	    NMAX = NMAX1                                             
	ELSE IF (NMAX1 .GT. NMAX2) THEN                              
	    K = NMAX2 * (NMAX2 + 2)                                  
	    L = NMAX1 * (NMAX1 + 2)                           
	    DO 1122 I = K + 1, L          
1122		GH(I) = GH1(I) + FACTOR * (-GH1(I))                  
	    NMAX = NMAX1                                             
	ELSE                                                         
	    K = NMAX1 * (NMAX1 + 2)                                  
	    L = NMAX2 * (NMAX2 + 2)                                  
	    DO 1133 I = K + 1, L         
1133		GH(I) = FACTOR * GH2(I)                              
	    NMAX = NMAX2                                             
	ENDIF                                 
  
	DO 1144 I = 1, K  
1144	    GH(I) = GH1(I) + FACTOR * (GH2(I) - GH1(I))              

	RETURN                                                       
	END                                                          
C
C
	SUBROUTINE EXTRASHC (DATE, DTE1, NMAX1, GH1, NMAX2,           
     1			      GH2, NMAX, GH)                           

C ===============================================================    
C
C	Version 1.01                                                   
C
C	Extrapolates linearly a spherical harmonic model with a        
C	rate-of-change model.                                          
C
C	Input:                                                         
C	    DATE  - Date of resulting model (in decimal year)          
C	    DTE1  - Date of base model                                 
C	    NMAX1 - Maximum degree and order of base model             
C	    GH1   - Schmidt quasi-normal internal spherical            
C		    harmonic coefficients of base model                
C	    NMAX2 - Maximum degree and order of rate-of-change         
C		    model                                              
C	    GH2   - Schmidt quasi-normal internal spherical            
C		    harmonic coefficients of rate-of-change model      
C
C	Output:                                                        
C	    GH    - Coefficients of resulting model                    
C	    NMAX  - Maximum degree and order of resulting model        
C
C	A. Zunde                                                       
C	USGS, MS 964, Box 25046 Federal Center, Denver, CO  80225      
C
C ===============================================================    

	integer i,k,l
	integer nmax1, nmax2, nmax
	real date, dte1, gh1, gh2, gh, factor
	DIMENSION	GH1(*), GH2(*), GH(*)                          

C ---------------------------------------------------------------   
C	The coefficients (GH) of the resulting model, at date          
C	DATE, are computed by linearly extrapolating the coef-         
C	ficients of the base model (GH1), at date DTE1, using          
C	those of the rate-of-change model (GH2), at date DTE2. If      
C	one model is smaller than the other, the extrapolation is      
C	performed with the missing coefficients assumed to be 0.       
C ---------------------------------------------------------------  

	FACTOR = (DATE - DTE1)                                         

	IF (NMAX1 .EQ. NMAX2) THEN                                     
	    K = NMAX1 * (NMAX1 + 2)                                    
	    NMAX = NMAX1                                               
	ELSE IF (NMAX1 .GT. NMAX2) THEN                                
	    K = NMAX2 * (NMAX2 + 2)                                    
	    L = NMAX1 * (NMAX1 + 2)                                    
	    DO 1155 I = K + 1, L  
1155		GH(I) = GH1(I)                                         
	    NMAX = NMAX1                                               
	ELSE                                                           
	    K = NMAX1 * (NMAX1 + 2)                                    
	    L = NMAX2 * (NMAX2 + 2)                                    
	    DO 1166 I = K + 1, L 
1166		GH(I) = FACTOR * GH2(I)                                
	    NMAX = NMAX2                                               
	ENDIF                                                          

	DO 1177 I = 1, K 
1177	    GH(I) = GH1(I) + FACTOR * GH2(I)                           

	RETURN                                                         
	END                                                            
