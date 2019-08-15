FUNCTION SOLDEC,DAY
;
;  PURPOSE
;     To calculate the solar declination.
;
;  METHOD
;     The expression for the solar declination was derived using spherical
;     trigonometry.
;
;  INPUT PARAMETERS
;     DAY      The day of the year
;
;  OUTPUT PARAMETERS
;     DEC      The solar declination, in degrees north
;
;  LOCAL VARIABLES
;     DAYVE    The day number of the vernal equinox
;     SIDYR    The number of days in the sidereal year
;     SOLINC   The solar inclination, in degrees
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Lincoln Brown
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Waltham, MA 02154  USA
;     (617)-487-2250
;
;  VERSION
;     1.1   19-March-1993
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     L. Brown        10-Jan-1989   1.0 ==> Created
;     L. Brown        19-Mar-1993   1.0 ==> 1.1
;                                   Improved internal documentation.
;
;  REFERENCES
;     None
;
;  SPECIAL CONSTANTS
;     None
;
;  Define constants for the solar declination calculation
;
DAYVE=80.
SIDYR=365.4
SOLINC=23.5
;
;  Calculate the solar declination
;
DEC=ASIN(SIN(2.*!PI*(DAY-DAYVE)/SIDYR)*SIN(SOLINC*!DTOR))/!DTOR
;
RETURN,DEC
END
FUNCTION KPTOAP,KP
;
;  PURPOSE
;     Calculates the quasi-linear magnetic activity index Ap from the
;     quasi-logarithmic index Kp using Table 6 from Mayaud (1980).
;
;  METHOD
;     Table lookup in which the subscript is just Kp*3, followed by
;     linear interpolation (to allow for averaged Kp's).
;
;  INPUT PARAMETERS
;     KP       The magnetic activity index Kp
;
;  OUTPUT PARAMETERS
;     AP       The magnetic activity index Ap
;
;  LOCAL VARIABLES
;    IKP3      KP3 truncated to an integer
;    KAP       Ap corresponding to Kp*3
;    KP3       KP*3
;
;  PROCEDURES REQUIRED
;     None
;
;  FUNCTIONS REQUIRED
;     None
;
;  FILES ACCESSED
;     None
;
;  AUTHOR
;     Robert E. Daniell, Jr.
;     Computational Physics, Inc.
;     240 Bear Hill Road  Suite 202A
;     Waltham, MA 02154  USA
;     (617)-487-2250
;
;  VERSION
;     1.1   19-March-1993
;
;  MODIFICATIONS
;     ----Person----  ----Date----  ----------------Description----------------
;     R. Daniell      22-Jun-1990   1.0 ==> Created
;     L. Brown        19-Mar-1993   1.0 ==> 1.1
;                                   Improved internal documentation.
;
;  REFERENCES
;     Mayaud, P. N., Derivation, 'Meaning, and Use of Geomagnetic Indices',
;     Geophysical Monograph 22, American Geophysical Union, Washington, D.C.,
;     1980.
;
;  SPECIAL CONSTANTS
;     None
;
;  Define the lookup table
;
KAP=[0.,2.,3.,4.,5.,6.,7.,9.,12.,15.,18.,22.,27.,32.,39.,48., $
     56.,67.,80.,94.,111.,132.,154.,179.,207.,236.,300.,400.]
;
;  Limit Kp to the range [0,9]
;
IF KP LT 0. THEN BEGIN
   KP3=0.
ENDIF ELSE IF KP GT 9. THEN BEGIN
   KP3=27.
ENDIF ELSE BEGIN
   KP3=KP*3.
ENDELSE
;
;  Determine the index of the lookup table
;
IKP3=MIN([FIX(KP3),26])
;
;  Linearly interpolate the Ap index from the lookup table
;
AP=KAP(IKP3)+(KAP(IKP3+1)-KAP(IKP3))*(KP3-FLOAT(IKP3))
;
RETURN,AP
END
