// vex_ut1_interp.cxx
// deal with getting UT1 for VEX stuff
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson
//	2006 Jan 02  JMA  --allow 1 datapoint, as AIPS seems to only store 1
//                          value.  :(




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "vex_time.h"
#include "vex_ut1_interp.h"
#include "ut1_interp.h"



// set up a namespace area for stuff.
namespace JMA_VEX_AREA {




// GLOBALS


// FUNCTIONS
    VEX_UT1_Interp::VEX_UT1_Interp(const Uint32 num_points)
            : NUM_POINTS(num_points), MJD(NULL), X_Polar_Motion(NULL),
              Y_Polar_Motion(NULL), UT1_UTC(NULL)
    {
        // We need at least three points for a proper interpolation.
        // But 0 points is a special case where we assume all offsets are 0
        if((NUM_POINTS < 3) && (NUM_POINTS != 0) && (NUM_POINTS != 1)) {
            fprintf(stderr, "Error: bad number of points (%d) for UT1 interpolation\n%s:%d:%s\n",
                    NUM_POINTS,
                    __FILE__, __LINE__, __func__);
            exit(3);
        }


        if((NUM_POINTS)) {
            // Ok, get space for the arrays.  Assume that new will throw an
            // exception if there is no memory.
            MJD = new Real64[NUM_POINTS];
            X_Polar_Motion = new Real64[NUM_POINTS];
            Y_Polar_Motion = new Real64[NUM_POINTS];
            UT1_UTC = new Real64[NUM_POINTS];
            
            // set the values to some bad things so that the user is
            // required to fill this in.
            for(Uint32 i=0; i < NUM_POINTS; i++) {
                MJD[i] = X_Polar_Motion[i] = Y_Polar_Motion[i] = UT1_UTC[i] =
                    HUGE_VALF;
            }
        }
        else {
            // set the pointers to something sane
            MJD = X_Polar_Motion = Y_Polar_Motion = UT1_UTC = NULL;
        }

        return;
    }


    VEX_UT1_Interp::~VEX_UT1_Interp()
    {
        if((NUM_POINTS)) {
            delete[] UT1_UTC;
            delete[] Y_Polar_Motion;
            delete[] X_Polar_Motion;
            delete[] MJD;
        }
        return;
    }


    void VEX_UT1_Interp::get_interpolation(const Real64 MJD_calc,
                                           Real64* X_Polar_Motion_calc,
                                           Real64* Y_Polar_Motion_calc,
                                           Real64* UT1_UTC_calc) const throw()
    {
        if(NUM_POINTS > 1) {
            // We need a FORTRAN integer to call the function
            integer N = NUM_POINTS;
        
            FTN_NAME(interp_ut1) ((doublereal*)MJD,
                                  (doublereal*)X_Polar_Motion,
                                  (doublereal*)Y_Polar_Motion,
                                  (doublereal*)UT1_UTC,
                                  &N,
                                  (doublereal*)&MJD_calc,
                                  (doublereal*)X_Polar_Motion_calc,
                                  (doublereal*)Y_Polar_Motion_calc,
                                  (doublereal*)UT1_UTC_calc
                                  );
        }
        else if(NUM_POINTS == 1) {
            *X_Polar_Motion_calc = *X_Polar_Motion;
            *Y_Polar_Motion_calc = *Y_Polar_Motion;
            *UT1_UTC_calc        = *UT1_UTC;
        }
        else {
            *X_Polar_Motion_calc = *Y_Polar_Motion_calc = *UT1_UTC_calc = 0.0;
        }
        return;
    }



    



}  // end namespace


