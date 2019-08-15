/* jma_irisub_c.h */
/* a file for calling the FORTRAN stuff */
//	2008 Feb 18  JMA  --MPIfR  --rework for gfortran



#ifdef HAVE_G2C_H_FILE
#  include <g2c.h>
#else
#  include <f2c.h>
#endif



#ifdef __cplusplus
extern "C" {
#endif

extern integer jma_irisub_c(
    const char* const BASEPATH,  // path to the IRI databases
    real ALATI,            // Station geocentric latitude, in rad
    real ALONG,            // Station geocentric longitude in rad +East
    integer IYYYY,               // Year as YYYY
    integer MM,                  // Month, January=1
    integer DD,                  // Day of month
    real DHOUR,            // UT time in hours, as in 15.245
    real HEIGHT,           // Height of point above Earth surface, in m
    real* NE_OUT           // electron density, in # m^{-3}
    );
#ifdef __cplusplus
}
#endif
