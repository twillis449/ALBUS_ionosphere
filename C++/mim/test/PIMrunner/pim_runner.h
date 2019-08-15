// pim_runner.h
// stuff to deal with running PIMVLBI4
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 12  James M Anderson  --JIVE  start
//	2005 Sep 12  JMA  --modify to use the PIM library I have moved over to
//                          my Linux box



#ifndef PIM_RUNNER_H
#define PIM_RUNNER_H

// INCLUDES
#include "JMA_math.h"
#include "station_reference.h"
#include "station_latlon.h"
#include "observation.h"



// Stuff for just the function code, here in the header file so that
// JMA can find it easily.
#ifdef PIM_RUNNER_CXX

// Globals only this file needs to know about
namespace {
    const char PIM_STDIN_COMMANDS_FILE[] = "pim_commands.txt";
    const char PIM_STATION_FILE[] = "sta.cat";
    const char PIM_SOURCE_FILE[] = "src.cat";
    const char PIM_OUTPUT_FILE[] = "jma1.dat";
    const char PIM_TEXT_PROCESSING_FILE[] = "jma2.dat";
    const char PIM_PROGRAM_NAME[] = "./pimvlbi4";
    const char PIM_DATA_PATH[] = INSTALLDIR"/libdata/PIM/";
}

    
#endif  // PIM_RUNNER_CXX




// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


    
// FUNCTIONS


//_TITLE  run_PIMVLBI --run PIMVLBI4 to get info from stations and sources
    void run_PIMVLBI(
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
        Uint32* const NUM_OBSERVATIONS,
                               // O  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere** observation
                               // O  The observations.  This is a **, so
                               //    the caller's pointer will be assigned
                               //    space using new[], and the user should
                               //    delete[] to free.
        );

//_TITLE  run_PIMVLBI --run PIM function to get info from stations and sources
    void run_PIMVLBI2(
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
        Uint32* const NUM_OBSERVATIONS,
                               // O  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere** observation
                               // O  The observations.  This is a **, so
                               //    the caller's pointer will be assigned
                               //    space using new[], and the user should
                               //    delete[] to free.
        );

//_TITLE  load_PIMVLBI --load a PIMVLBI4 dataset from run_PIMVLBI
    void load_PIMVLBI(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const char filename[], // I  the name of the file holding the
                               //    PIM run data
                               // I  The evaluation time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
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


#endif // PIM_RUNNER_H
