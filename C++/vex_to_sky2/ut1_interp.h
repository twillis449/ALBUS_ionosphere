// ut1_interp.h
// interface file for the FORTRAN code to interpolate in UT1-UTC
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start
//	2008 Feb 18  JMA  --MPIfR  --rework for gfortran



#ifndef UT1_INTERP_H
#define UT1_INTERP_H

// INCLUDES
#ifdef HAVE_G2C_H_FILE
#  include <g2c.h>
#else
#  include <f2c.h>
#endif
#include "JMA_code.h"



#ifdef __cplusplus
extern "C" {
#endif




// GLOBALS


// FUNCTIONS
    integer FTN_NAME(interp_ut1) (doublereal* RJD,
                                  doublereal* X,
                                  doublereal* Y,
                                  doublereal* T,
                                  integer* N,
                                  doublereal* RJDINT,
                                  doublereal* XINT,
                                  doublereal* YINT,
                                  doublereal* TINT);


#ifdef __cplusplus
    }
#endif



#endif // UT1_INTERP_H
