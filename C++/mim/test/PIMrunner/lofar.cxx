// lofar.cxx
// test LOFAR ionosphere stuff with PIM
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start






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
    


//     {
//         Station_Reference ref(50.0*M_DEG2RAD,10.0*M_DEG2RAD);
//         Station_LatLon a(50.0*M_DEG2RAD, 8.0*M_DEG2RAD);
//         Station_LatLon b(50.0*M_DEG2RAD,12.0*M_DEG2RAD);
//         Station_LatLon c(51.0*M_DEG2RAD,10.0*M_DEG2RAD);
//         Station_LatLon d(49.0*M_DEG2RAD,10.0*M_DEG2RAD);
//         Station_LatLon e(51.0*M_DEG2RAD,11.0*M_DEG2RAD);
//         LatLon_Cart p = ref.get_equatorial_offset(a);
//         printf("Lat %12.6f Lon %12.6f\n",
//                p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//         p = ref.get_equatorial_offset(b);
//         printf("Lat %12.6f Lon %12.6f\n",
//                p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//         p = ref.get_equatorial_offset(c);
//         printf("Lat %12.6f Lon %12.6f\n",
//                p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//         p = ref.get_equatorial_offset(d);
//         printf("Lat %12.6f Lon %12.6f\n",
//                p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//         p = ref.get_equatorial_offset(e);
//         printf("Lat %12.6f Lon %12.6f\n",
//                p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//     }

    
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

//    fprintf(stderr, "Got stations\n");

//     for(Uint32 i=0; i < NUM_STATIONS; i++) {
//         LatLon_Cart p = ref->get_polar_offset(stations[i]);
//         printf("Sta %3d Lat %12.6f Lon %12.6f\n",
//                i,p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//     }
      

//     // Get the 3C objects
//     Cal_Source* Sources_3C = read_in_3C_sources(argv[6]);

    // Get the random sources
    Cal_Source* Sources_Random = read_in_random_sources(argv[6]);



    // get the observations
    Uint32 NUM_OBSERVATIONS;
    Observation_Ionosphere* observation;
    run_PIMVLBI2(NUM_RANDOM_SOURCES,
                 Sources_Random,
                 NUM_STATIONS,
                 stations,
                 &eval_time,
                 &NUM_OBSERVATIONS,
                 &observation
                 );

//    delete[] Sources_3C;
    delete[] Sources_Random;
    delete[] stations;
    delete[] observation;
    
    return 0;
}
    






