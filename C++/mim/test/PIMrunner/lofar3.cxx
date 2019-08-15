// lofar3.cxx
// test LOFAR ionosphere stuff with PIM
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE start






// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "cal_source.h"
#include "station_latlon.h"
#include "station_reference.h"
#include "station_maker.h"
#include "3C.h"
#include "random_source.h"
#include "pim_runner.h"
#include "observation.h"
#include "iono_runner.h"
#include "ionosphere.h"
#include "ionosphere_fake.h"



// set up a namespace area for stuff.
using namespace MIM_PIM;


    



// GLOBALS


// FUNCTIONS



int main(int argc, char* argv[])
{
    if(argc < 9) {
        fprintf(stderr, "Error: correct usage is %s YYYY MM DD HH MM 3C.dat_file NUM_ANT_PER_ARM NUM_ARM\n", argv[0]);
        exit(2);
    }
    struct tm eval_time;
    eval_time.tm_sec = 0;
    eval_time.tm_min = atoi(argv[5]);
    eval_time.tm_hour = atoi(argv[4]);
    eval_time.tm_mday = atoi(argv[3]);
    eval_time.tm_mon = atoi(argv[2]) - 1;
    eval_time.tm_year = atoi(argv[1]) - 1900;
    eval_time.tm_isdst = -1;
    mktime(&eval_time);
    


    Station_Reference* ref;
    Station_LatLon* stations;
    const Uint32 NUM_STATIONS = get_station_list_spiral(
        atoi(argv[7]),     // I  the number of antennas on each arm       
        atoi(argv[8]),     // I  total number of arms                     
        0.0,               // I  the initial angle offset for arm 0 (rad) 
        3.0,               // I  the spiral tightness parameter (unitless)
        0.3,               // I  arm angle spacing (rad)                  
        500.0,             // I  initial baseline distance from center (m)
        &ref,
        &stations
        );

    // Get the random sources
    const Uint32 NUM_SOURCES = NUM_RANDOM_SOURCES;
    Cal_Source* Sources = read_in_random_sources(argv[6]);


    // Make a fake ionosphere
    Ionosphere_Fake test_iono(fake_Gaussian,1E-7,1E-5);


    // get the observations
    Uint32 NUM_OBSERVATIONS;
    Observation_Ionosphere* observation;
    run_ionosphere(NUM_SOURCES,
                   Sources,
                   NUM_STATIONS,
                   stations,
                   &eval_time,
                   test_iono,
                   &NUM_OBSERVATIONS,
                   &observation
                   );

    delete[] Sources;
    delete[] stations;
    delete[] observation;
    
    return 0;
}
    






