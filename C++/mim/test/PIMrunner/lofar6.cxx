// lofar6.cxx
// test LOFAR ionosphere stuff with IRI for Faraday prediction
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 10  James M Anderson  --JIVE start






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
#include "ionosphere_iri.h"
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


//     // Make a fake ionosphere
//     Ionosphere_Fake test_iono(fake_Gaussian,1E-6,1E-4);
    // Make an IRI ionosphere
    Ionosphere_IRI test_iono(1E-6,1E-4);




//     {
//         // Ok, if the elevation is greater than 0, run the
//         // ionosphere model. Get the Alz,Az information from the
//         // RA and Dec info
//         LatLon_Cart RADec(Sources[256].Dec(),
//                           Sources[256].RA(),
//                           0.0,1.0);
//         LatLon_Cart AltAz =
//             stations[0].convert_RADec_to_AltAz(RADec);
//         Real64 El   = AltAz.Lat();
//         Real64 Az   = AltAz.Lon();

//         // I need a unit direction vector for the direction.
//         Space_Unit_Vector direction = RADec.make_unit_vector();

                
//         for(Real64 dist = 10E3; dist < 25000E3; dist += 10E3) {
//             // Now, I also want to have the magnetic field strength
//             // along the line of sight at 350 km altitude.
//             LatLon_Cart pierce_point =
//                 stations[0].get_pierce_location(
//                     direction,
//                     dist, // m
//                     radius_Earth
//                     );
//             Space_Vector B_field = test_iono.Magnetic_Field(
//                 pierce_point,
//                 eval_time
//                 );
//             // get the dot product, which gives the B field component
//             // along the line of sight.  This is as the wavel comes
//             // toward the Earth, so it is actually antiparallel, or
//             // need to use a minus sign.
//             Real64 B_parallel = -B_field.dot_product(direction);
//             // priint out B_parallel in \micro T
//             B_parallel *= 1E6;
                    
//             fprintf(stdout, "%.3E %14.7f\n", dist, B_parallel);
//         }
//     }



    {
        // Ok, if the elevation is greater than 0, run the
        // ionosphere model. Get the Alz,Az information from the
        // RA and Dec info
        LatLon_Cart RADec(Sources[256].Dec(),
                          Sources[256].RA(),
                          0.0,1.0);
        LatLon_Cart AltAz =
            stations[0].convert_RADec_to_AltAz(RADec);
        Real64 El   = AltAz.Lat();
        Real64 Az   = AltAz.Lon();

        // I need a unit direction vector for the direction.
        Space_Unit_Vector direction = RADec.make_unit_vector();

                
        for(int i=0; i < 86400; i+= 300) {
        //for(int i=70800; i <= 70800; i+= 300) {
            struct tm new_time = eval_time;
            new_time.tm_isdst = -1;
            new_time.tm_sec = i;
            mktime(&new_time);
            Real64 STEC, SRM, STEC_err;
//             Real64 e_d = test_iono.Electron_Density_Range(stations[0],
//                                                           new_time,
//                                                           direction,
//                                                           491759.0);
//             printf(" %E\n",e_d);exit(0);
            test_iono.Integrate_Electron_Faraday(stations[0],
                                                 new_time,
                                                 direction,
                                                 &STEC,
                                                 &STEC_err,
                                                 &SRM
                                                 );
            fprintf(stdout, "%5d %10.3E %10.3E\n", i, STEC, SRM);
        }
    }


    
    exit(0);


    


    // get the observations
    Uint32 NUM_OBSERVATIONS;
    Observation_Ionosphere* observation;
    run_ionosphere_eF(NUM_SOURCES,
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
    






