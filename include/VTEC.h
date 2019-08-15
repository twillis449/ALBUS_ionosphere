// VTEC.h
// stuff for getting Vertical TEC and back to Slant TEC
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE  start
//	2005 Nov 07  JMA  --make array versions of the functions
//	2006 Jan 05  JMA  --correct for elevation of the observer



#ifndef VTEC_H
#define VTEC_H

// INCLUDES
#include "JMA_math.h"
#include "station_reference.h"
#include "station_latlon.h"
#include "observation.h"
#include "cal_source.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS


//_TITLE  get_simple_VTEC_scaling --get the o_0 term
    Real64 get_simple_VTEC_scaling(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 El,       // I  elevation angle, in rad
        const Real64 Elev,     // I  altitude/elevation of the obsrever, in m
        const Real64 h         // I  height of ionosphere above Earth, in m
        );
// get_simple_VTEC_scaling        O  o_0 scaling factor

//_TITLE  get_simple_VTEC_scaling --get the o_0 term for a range of heights
    void get_simple_VTEC_scaling(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 El,       // I  elevation angle, in rad
        const Real64 Elev,     // I  altitude/elevation of the obsrever, in m
        const Uint32 NUM_HEIGHTS,// I The number of heights
        const Real64* const h, // I  height of ionosphere above Earth, in m
                               //    as h[NUM_HEIGHTS]
        Real64* const scaling  // O  the o_0 term for each height,
                               //    as scaling[NUM_HEIGHTS]
        );





//_TITLE  find_simple_pierce_point_and_VTEC --
    void find_simple_pierce_point_and_VTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere* const observation,
                               // B  The observations.  
        const Real64 height    // I  the assumed height of the ionosphere
                               //    in a simple 2D model of the ionosphere.
                               //    in meters.
        );



    



}  // end namespace


#endif // VTEC_H
