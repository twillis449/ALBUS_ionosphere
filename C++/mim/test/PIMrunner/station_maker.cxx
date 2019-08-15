// station_maker.cxx
// make some stations to observe with
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE  start



#ifndef STATION_MAKER_CXX
#define STATION_MAKER_CXX
#endif // STATION_MAKER_CXX



// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "station_reference.h"
#include "station_latlon.h"
#include "station_maker.h"




// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


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
        )

// get_station_list_spiral        O  the total number of stations allocated

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will make a set of stations for observations.

//	The stations will be based around a central "core" station, which
//	is currently set near Westerbork.  Then, a set of antennas on
//	a logarythmic spiral pattern go out from the core.

//	All of the stations, including the core are placed into stations[], which
//	has get_station_list_spiral elements.  This should be freed with
//	delete[].  The core station is assumed to be the reference station, and
//	is placed in *ref.  It should be freed with delete.

//	See JMA notes on 2005 Aug 17-1 for logarythmic spiral.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE start

//_END

    {
        // we actually need the absolute value of beta
        const Real64 abeta = fabs(beta);

        // Check for decent parameters
#ifdef DEBUG
        {
            bool bad_param = false;
            if((NUM_ANT_A <= 0) || (NUM_ANT_A > 100)) {
                fprintf(stderr, "Error: bad number of antennas per arm %d\n", NUM_ANT_A);
                bad_param = true;
            }
            if((NUM_ARMS <= 0) || (NUM_ARMS > 100)) {
                fprintf(stderr, "Error: bad number of arms %d\n", NUM_ARMS);
                bad_param = true;
            }
            if((alpha <= -M_PI) || (alpha > M_PI)) {
                fprintf(stderr, "Error: bad initial angle offset %E\n", alpha);
                bad_param = true;
            }
            if((a < 0.01) || (a > 100.0)) {
                fprintf(stderr, "Error: bad spiral tightness parameter %E\n", a);
                bad_param = true;
            }
            if((abeta*M_RAD2DEG < 0.1) || (abeta > M_PI)) {
                fprintf(stderr, "Error: bad arm angle spacing %E\n", beta);
                bad_param = true;
            }
            if((b0 < 1.0) || (b0 > 1E5)) {
                fprintf(stderr, "Error: bad initial baseline distance %E\n", b0);
                bad_param = true;
            }
            if(m > 1000) {
                fprintf(stderr, "Error: too many antennas %d on an arm\n", m);
                bad_param = true;
            }
            if(n >= NUM_ARMS) {
                fprintf(stderr, "Error: too many arms %d when limit %d\n", n, NUM_ARMS);
                bad_param = true;
            }
            if((bad_param)) exit(2);
        }
#endif // DEBUG


        // Ok, I need to allocate space.  How many stations are there total?
        // This is the number of arms times the number of stations per arm,
        // plus the core.
        const Uint32 NUM_STATIONS = NUM_ARMS * NUM_ANT_A + 1;
        Uint32 station_id = 0;



        {
            // Ok, make the core station.
            Station_LatLon core(SPIRAL_CORE_LAT_DEG*M_DEG2RAD,
                                SPIRAL_CORE_LON_DEG*M_DEG2RAD);
            *ref = new Station_Reference(core);
            *stations = new Station_LatLon[NUM_STATIONS];
            
            // check that this went ok
            if((*ref == NULL) || (*stations == NULL)) {
                fprintf(stderr, "Error allocating space for %u stations in %s:%d:%s\n",
                        NUM_STATIONS, __FILE__, __LINE__, __func__);
            exit(1);
            }

            // Ok, station 0 is the core
            (*stations)[station_id++] = core;
        }


//        fprintf(stderr, "Have station memory allocated for %u stations\n", NUM_STATIONS);

        // Now, for each antenna on an arm, for each arm, make a station
        for(Uint32 m = 0; m < NUM_ANT_A; m++) {
            for(Uint32 n = 0; n < NUM_ARMS; n++) {

                // Ok, calculate the radius and angle (r and \phi) in
                // the 2D plane for the logarythmic spiral

                Real64 r = b0 * exp(a*abeta*m);
                Real64 phi = beta*m + 2.0*M_PI*n/Real64(NUM_ARMS) + alpha;
                
                // Now, this is going to be mapped onto a sphere.  See
                // the Explanatory Supplement to the Astronomical
                // Almanac (of some other book which deals with
                // spherical astronomy), for rotations.
                
                // Make a zenith angle and an azimuth.  For the zenith
                // angle, just use r / r_\Earth.

                Real64 Z = r / radius_Earth;
                Real64 AZ = phi;

//                fprintf(stderr, "doing ant %u arm %u station %u with %f %f\n", m, n, station_id, AZ, Z);
                

                (*stations)[station_id++] =
                    Station_LatLon((*ref)->get_pierce_location(AZ, Z));
//                fprintf(stderr, "lat is %E\n", (*stations)[station_id-1].Lat());
            } // for n to num arms
        } // for m to num antennas per arm

        // That seems to be all, just return the number of antennas allocated
        return NUM_STATIONS;
    }
    



}  // end namespace


