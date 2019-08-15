// sofa_helpers.cxx
// Some functions to siplify access tot he SOFA stuff
//_HIST  DATE NAME PLACE INFO
//	2006 Jan 02  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "station_latlon.h"
#include "station_reference.h"
#include "station_maker.h"
#include "sofa.h"
#include "vex_time.h"
#include "sofa_helpers.h"


using namespace JMA_VEX_AREA;


// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS




    
// Use SOFA to perform a precession calculation
//_TITLE  get_position_of_date_rotation --precession changes for J2000 coord
    Space_Rotation_Matrix get_position_of_date(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 JD0_TT,         // I  The first part of a SOFA Julian Date, TT
        Real64 JD1_TT          // I  the second part fo the TT Julian Date
                               //    these times are for the observation
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls
//	See sofa.h

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//       2005 Dec 13  James M Anderson  --JIVE  start

//_END

    {
        // I need space for the rotation matrix
        Space_Rotation_Matrix rotation;

        // call the SOFA thing to calculate the rotation matrix
        FTN_NAME(iau_pnm00a) ( &JD0_TT, &JD1_TT, &(rotation.matrix[0][0]) );

        return rotation;
    }




    Real64 Delta_AT(
        const VEX_Time& t
        )
    {
        integer IY, IM, ID, J;
        doublereal FD, DELTAT;
        IY = t.Year();
        IM = t.Month();
        ID = t.Day();
        FD = t.Day_Fraction();
        J=0;
        DELTAT = 0.0;
        FTN_NAME(iau_dat) (&IY, &IM, &ID, &FD, &DELTAT, &J);
        return DELTAT;
    }


    Real64 GST(
        const VEX_Time& t
        )
    {
        doublereal UTA = t.JD0_1();
        doublereal UTB = t.JD1_1();
        doublereal TTA = t.JD0_C();
        doublereal TTB = t.JD1_C() + (Delta_AT(t) + 32.184)/SECONDS_PER_DAY;

        return FTN_NAME(iau_gst00a) (&UTA, &UTB, &TTA, &TTB);
    }


}  // end namespace


