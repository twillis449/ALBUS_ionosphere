// iono_runner.h
// What this header file is for
//_HIST  DATE NAME PLACE INFO



#ifndef IONO_RUNNER_H
#define IONO_RUNNER_H

// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


#include "latlon_cart.h"
#include "station_latlon.h"
#include "station_maker.h"
#include "cal_source.h"
#include "pim_runner.h"
#include "observation.h"

#include "ionosphere.h"
#include "space_vector.h"
#include "space_unit_vector.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS
//_TITLE  run_ionosphere  --run an ionosphere model for stations and sources
    void run_ionosphere(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const struct tm* const Eval_Time,
                               // I  The evaluation time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
        Ionosphere_Base& Iono_Model,
                               // I  The ionosphere model to use.
        Uint32* const NUM_OBSERVATIONS,
                               // O  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere** observation
                               // O  The observations.  This is a **, so
                               //    the caller's pointer will be assigned
                               //    space using new[], and the user should
                               //    delete[] to free.
        );




//_TITLE  run_ionosphere_eF  --run an ionosphere model for stations and sources
    void run_ionosphere_eF(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const struct tm* const Eval_Time,
                               // I  The evaluation time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
        Ionosphere_Base& Iono_Model,
                               // I  The ionosphere model to use.
        Uint32* const NUM_OBSERVATIONS,
                               // O  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere** observation
                               // O  The observations.  This is a **, so
                               //    the caller's pointer will be assigned
                               //    space using new[], and the user should
                               //    delete[] to free.
        );

    

}  // end namespace


#endif // IONO_RUNNER_H
