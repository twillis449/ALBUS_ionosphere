/* jma_irisub_c.c */
/* a file for calling the FORTRAN stuff */
//	2008 Feb 18  JMA  --MPIfR  --rework for gfortran



#ifdef HAVE_G2C_H_FILE
#  include <g2c.h>
#else
#  include <f2c.h>
#endif
#include "string.h"
#include <stdio.h>
#include <math.h>

#include "JMA_code.h"
#include "jma_irisub_c.h"



/* this is the FORTRAN function */
extern integer FTN_NAME(jma_iri_sub)(
    char* path,
    real* ALATI,
    real* ALONG,
    integer* IYYYY,
    integer* MMDD,
    real* DHOUR,
    real* HEIGHT,
    real* NE_OUT
//   ftnlen
    );

extern integer FTN_NAME(initialize)();





integer jma_irisub_c(
    const char* const BASEPATH,  // path to the IRI databases
    real ALATI,            // Station geocentric latitude, in rad
    real ALONG,            // Station geocentric longitude in rad +East
    integer IYYYY,               // Year as YYYY
    integer MM,                  // Month, January=1
    integer DD,                  // Day of month
    real DHOUR,            // UT time in hours, as in 15.245
    real HEIGHT,           // Height of point above Earth surface, in m
    real* NE_OUT           // electron density, in # m^{-3}
    )
{
    const integer SIZE = 256;
    char path[SIZE];
    int i;

    static int init_flag = 1;
    if((init_flag)) {
        FTN_NAME(initialize)();
        init_flag = 0;
    }

    /* convert height from meters to km */
    HEIGHT *= 0.001;
    /* convert radians to degrees */
    ALATI *= 180.0/M_PI;
    ALONG *= 180.0/M_PI;
    /* make a MMDD variable */
    integer MMDD = MM*100+DD;
    /* for UT time, need to add 25 hours */
    DHOUR += 25.0;

    
    
    strncpy(path, BASEPATH, SIZE);
    for(i=strlen(path); i < SIZE; i++) path[i] = ' ';
/*    printf("DEBUG calling pathe %s\n", path);fflush(stdout);
    printf("DEBUG calling ALATI %E\n", ALATI);fflush(stdout);
    printf("DEBUG calling ALONG %E\n", ALONG);fflush(stdout);
    printf("DEBUG calling HEIGHT %E\n", HEIGHT);fflush(stdout); */
    FTN_NAME(jma_iri_sub)(path,
                          &ALATI,
                          &ALONG,
                          &IYYYY,
                          &MMDD,
                          &DHOUR,
                          &HEIGHT,
                          NE_OUT
                        //  (ftnlen)SIZE
                          );
/*    printf("DEBUG electron density %E\n", *NE_OUT);fflush(stdout); */
    
    return 0;
}


/* int main(void) */
/* { */
/*     real ne_out = 0.0; */
/*     jma_irisub_c("/jop30_0/anderson/astro/ionosphere/prog/FORTRAN/IRI", */
/*                  5.390413E+01*M_PI/180.0, */
/*                  8.632128E+00*M_PI/180.0, */
/*                  2004, */
/*                  06, */
/*                  05, */
/*                  15.0, */
/*                  1.750000E+05, */
/*                  &ne_out); */
/*     return 0; */
/* } */
