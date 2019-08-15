// sofa.h
// header file for interfacing with the FORTRAN SOFA library
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start
//	2008 Feb 18  JMA  --MPIfR  --rework for gfortran



#ifndef SOFA_H
#define SOFA_H

// INCLUDES
// Need FORTRAN stuff
#ifdef HAVE_G2C_H_FILE
#  include <g2c.h>
#else
#  include <f2c.h>
#endif
#include "JMA_code.h"







// GLOBALS


// FUNCTIONS

// This is all FORTRAN, so call it C
#ifdef __cplusplus
extern "C" {
#endif

    integer FTN_NAME(iau_cal2jd) ( integer* IY, integer* IM, integer* ID,
                                   doublereal* DJM0, doublereal* DJM,
                                   integer* J );

    integer FTN_NAME(iau_dat) ( integer* IY, integer* IM, integer* ID,
                                doublereal* FD, doublereal* DELTAT, integer* J );

    doublereal FTN_NAME(iau_gst00a) ( doublereal* UTA, doublereal* UTB,
                                      doublereal* TTA, doublereal* TTB );
    
    integer FTN_NAME(iau_jd2cal) ( doublereal* DJ1, doublereal* DJ2,
                                   integer* IY, integer* IM, integer* ID,
                                   doublereal* FD,
                                   integer* J );

    integer FTN_NAME(iau_pnm00a) ( doublereal* DATE1, doublereal* DATE2,
                                   doublereal* RBPN );

    integer FTN_NAME(iau_rxp) ( const doublereal* R, const doublereal* P,
                                doublereal* RP );
    integer FTN_NAME(iau_rxr) ( doublereal* A, doublereal* B, doublereal* ATB );
    



#ifdef __cplusplus
}
#endif




#endif // SOFA_H
