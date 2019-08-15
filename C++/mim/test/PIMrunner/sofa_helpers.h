// sofa_helpers.h
// Some functions to siplify access tot he SOFA stuff
//_HIST  DATE NAME PLACE INFO
//	2006 Jan 02  James M Anderson  --JIVE  start



#ifndef SOFA_HELPERS_H
#define SOFA_HELPERS_H

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



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS


    //_TITLE  get_position_of_date_rotation --precession changes for J2000 coord
    Space_Rotation_Matrix get_position_of_date(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 JD0_TT,         // I  The first part of a SOFA Julian Date, TT
        Real64 JD1_TT          // I  the second part fo the TT Julian Date
                               //    these times are for the observation
        );

    Real64 Delta_AT(
        const JMA_VEX_AREA::VEX_Time& t
        );
    
    Real64 GST(
        const JMA_VEX_AREA::VEX_Time& t
        );




}  // end namespace


#endif // SOFA_HELPERS_H
