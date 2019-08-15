// GPS.h
// Holds all of the GPS related info
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start



#ifndef GPS_H
#define GPS_H

// INCLUDES
#include "JMA_code.h"

#ifdef GPS_CXX
#  define NUM(x) =x
#else
#  define NUM(x)
#endif



// set up a namespace area for stuff.
namespace MIM_PIM {


    // This is a bad data code which is traditionally used by GPSTK and
    // some other software to indicate bad data values in GPS data.
    // This is a highly unlikely total electron content value.
    extern const Real64 GPS_BAD_DATA_VALUE NUM(-999.0); // ignore data
    extern const Real64 GPS_NOT_INITIALIZED_VALUE NUM(-1000.0); // user has not
                                                            // given all needed
                                                            // data.
    extern const Real64 TECU_SI NUM(1.0E16); // The conversion factor
                                             // from units of TECU to electrons
                                             // m^{-2}
    extern const Uint32 GPS_MAX_POSSIBLE_SATELLITES NUM(300); // maximum possible
                                                              // satellite ID
    extern const Uint32 GPS_MAX_POSSIBLE_RECEIVER_TYPE NUM(GPS_MAX_POSSIBLE_SATELLITES/100);
                                                            // Number receiver
                                                            // types

    extern const Real32 GPS_DEFAULT_MODEL_ELECTRON_PRECISION NUM(1E-3);
    extern const Real32 GPS_DEFAULT_MODEL_FARADAY_PRECISION NUM(1E-2);
#undef NUM

// GLOBALS


// FUNCTIONS



}  // end namespace


// MORE INCLUDES
#include "GPS_receiver_obs.h"
#include "GPS_receiver.h"
#include "GPS_satellites.h"
#include "GPS_times.h"
#include "GPS_criteria.h"
#include "GPS_collection.h"


#endif // GPS_H
