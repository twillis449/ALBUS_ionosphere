// vex_iono_processor.h
// functions in the vex_iono_processor area for processing VEX files to
// ionosphere information
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start



#ifndef VEX_IONO_PROCESSOR_H
#define VEX_IONO_PROCESSOR_H

// INCLUDES
#include "JMA_code.h"
#include "JMA_math.h"



// set up a namespace area for stuff.
namespace JMA_VEX_AREA {




// GLOBALS


// FUNCTIONS
//_TITLE  process_vex_file --get information about stations and soruces for scans
    extern     Sint32 process_vex_file(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char vex_filename[],
                               // I  the path and filename of the vex file
        const char output_base[],
                               // I  the path and base filename for output
                               //    files.
        Real64 Expansion_Time, // I  The amount of time, in seconds, which
                               //    will be added *before* and *after* the
                               //    actual observing scan start and stop times
                               //    for which to calculate the necessary data.
        Real64 Grid_Interval_Time,
                               // I  The grid interval spacing, in seconds.
                               //    This is used to make sure that, for
                               //    instance, the initial time is forced to a
                               //    multiple of 30 seconds.  e.g., Only
                               //    global start times of 00:00:00, 00:00:30,
                               //    00:01:00, 00:01:30, ... would be used.
                               //    If identically 0, then no gridding is
                               //    performed.
        Real64 Calculation_Interval
                               // I  The amount of time, in seconds, between
                               //    each calculation, starting at the scan
                               //    start minus Expansion_Time, and
                               //    ending at or just beyond the scan end
                               //    plus Expansion_Time
        );
    



}  // end namespace


#endif // VEX_IONO_PROCESSOR_H
