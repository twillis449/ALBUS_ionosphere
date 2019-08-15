// lofar4.cxx
// test LOFAR ionosphere stuff with PIM
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 07  James M Anderson  --JIVE  start from copy of lofar2.cxx






// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "cal_source.h"
#include "station_latlon.h"
#include "station_reference.h"
#include "station_maker.h"
#include "3C.h"
#include "random_source.h"
#include "pim_runner.h"
#include "observation.h"
#include "VTEC.h"
#include "filter.h"
#include "linfit.h"



// set up a namespace area for stuff.
using namespace MIM_PIM;


    



// GLOBALS


// FUNCTIONS



int main(int argc, char* argv[])
{
    if(argc < 11) {
        fprintf(stderr, "Error: correct usage is %s YYYY MM DD HH MM source.dat_file NUM_ANT_PER_ARM NUM_ARM observations.dat_file iono_height(m)\n", argv[0]);
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



    const Real64 ionosphere_height = atof(argv[10]);
    



    
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
//     Cal_Source* Sources = read_in_3C_sources(argv[6]);
//    const Uint32 NUM_SOURCES = NUM_3C_SOURCES;

    // Get the random sources
    Cal_Source* Sources = read_in_random_sources(argv[6]);
    const Uint32 NUM_SOURCES = NUM_RANDOM_SOURCES;

    // get the observations
    Uint32 NUM_OBSERVATIONS;
    Observation_Ionosphere* observation;
    load_PIMVLBI(NUM_SOURCES,
                 Sources,
                 NUM_STATIONS,
                 stations,
                 argv[9],
                 &NUM_OBSERVATIONS,
                 &observation
                 );




    find_simple_pierce_point_and_VTEC(
        NUM_SOURCES,
        Sources,
        NUM_STATIONS,
        stations,
        NUM_OBSERVATIONS,
        observation,
        ionosphere_height
        );


    // Need a use_flag array
    bool* use_flag = new bool[NUM_OBSERVATIONS];
    if(use_flag == NULL) {
        fprintf(stderr, "Error: cannot allocate space for %u use_array spots in %s:%d:%s\n",
                NUM_OBSERVATIONS,
                __FILE__, __LINE__, __func__);
        exit(1);
    }
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) use_flag[i] = true;

    // Now, flag for various things
    filter_on_VTEC(0.0, 1E300, NUM_OBSERVATIONS, observation, use_flag);
    filter_on_sigma_VTEC(0.0, 1E300, NUM_OBSERVATIONS, observation, use_flag);
    filter_on_El(60.0*M_DEG2RAD, 1E300, NUM_OBSERVATIONS, observation, use_flag);
    filter_on_Flux_Density(1.0,1E300, NUM_SOURCES, Sources,
                           NUM_OBSERVATIONS, observation, use_flag);
    filter_on_station_dist(-1E10, // something to make sure I get all inner ones
                           50E3,
                           *ref,
                           NUM_STATIONS,
                           stations,
                           NUM_OBSERVATIONS,
                           observation,
                           use_flag);







    // Now do some fitting.  go up to 50 parameters
    const Uint32 MAX_NUM_PARAM = 60;
    Real64 parameters[MAX_NUM_PARAM];
    for(Uint32 num = 1; num < MAX_NUM_PARAM; num++) {
        fprintf(stderr, "Doing model with %u parameters\n", num);
//         fit_multilayer_linear_model(num,
//                                     5,
//                                     *ref,
//                                     NUM_STATIONS,
//                                     stations,
//                                     NUM_OBSERVATIONS,
//                                     use_flag,
//                                     observation,
//                                     parameters
//                                     );

        fit_manylayer_linear_model(num,
                                   5,
                                   *ref,
                                   NUM_STATIONS,
                                   stations,
                                   NUM_OBSERVATIONS,
                                   use_flag,
                                   observation,
                                   parameters
                                   );
        
        fflush(stdout);
    }






    
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
//        if(((use_flag[i])) && (observation[i].object_number == 958))
        if((use_flag[i]))
            printf("Obs %6u Sta %3u Obj %3u Az %7.3f El %7.3f Lat %7.3f Lon %7.3f STEC %8.3f Model %8.3f\n",
                   i,
                   observation[i].station_number,
                   observation[i].object_number,
                   observation[i].Az*M_RAD2DEG,
                   observation[i].El*M_RAD2DEG,
                   observation[i].pierce_point.Lat()*M_RAD2DEG,
                   observation[i].pierce_point.Lon()*M_RAD2DEG,
                   observation[i].STEC,
                   observation[i].model_VTEC
                   );
    }



    

    delete[] Sources;
    delete[] stations;
    delete[] observation;
    
    return 0;
}
    






