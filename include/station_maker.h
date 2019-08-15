// station_maker.h
// stuff to deal with making stations
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE  start



#ifndef STATION_MAKER_H
#define STATION_MAKER_H

// INCLUDES
#include "JMA_math.h"
#include "station_reference.h"
#include "station_latlon.h"



// Stuff for just the function code, here in the header file so that
// JMA can find it easily.
#ifdef STATION_MAKER_CXX
    // For a central core for the logarythmic spiral, try using Westerbork
#define SPIRAL_CORE_LAT_DEG  (52.0 + (54.0 + (55.1)   /60.0)/60.0)
#define SPIRAL_CORE_LON_DEG (+(6.0 + (38.0 + ( 0.0)   /60.0)/60.0))

#endif  // STATION_MAKER_CXX

#ifdef STATION_MAKER_CXX
#  define NUM(x) =x
#else
#  define NUM(x)
#endif



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS
    // See GPSTK software package:
         /// This class represents the geodetic model defined in NIMA
         /// TR8350.2, "Department of Defense World Geodetic System 1984".
         /// Defined in TR8350.2, Appendix A.1
         /// @return semi-major axis of Earth in meters.
   
    extern const Real64 radius_Earth NUM(6378137.0); // meters

#undef NUM
    
// FUNCTIONS

    
//_TITLE  name one line description
    Uint32 get_station_list_spiral(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_ANT_A,// I  the number of antennas on each arm
        const Uint32 NUM_ARMS, // I  total number of arms
        const Real64 alpha,    // I  the initial angle offset for arm 0 (rad)
        const Real64 a,        // I  the spiral tightness parameter (unitless)
        const Real64 beta,     // I  arm angle spacing (rad)
        const Real64 b0,       // I  initial baseline distance from center (m)
        Station_Reference** ref,//O  the reference station, at the center of
                               //    the spiral
        Station_LatLon** stations
                               // O  the stations, as an array
        );

// get_station_list_spiral        O  the total number of stations allocated



}  // end namespace


#endif // STATION_MAKER_H
