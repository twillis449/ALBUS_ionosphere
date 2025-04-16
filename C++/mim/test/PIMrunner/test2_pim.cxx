// test2.cxx
// test LOFAR ionosphere stuff with IRI for Faraday prediction for
// Westerbork
//_HIST  DATE NAME PLACE INFO
//	2007 Jan 04  James M Anderson  --JIVE start






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
#include "ionosphere_pim.h"
#include "ionosphere_fake.h"
#include "sofa.h"
#include "vex_time.h"



// set up a namespace area for stuff.
using namespace MIM_PIM;
using namespace JMA_VEX_AREA;


    



// GLOBALS


// FUNCTIONS












int main(int argc, char* argv[])
{
    if(argc != 10) {
        fprintf(stderr, "Error: correct usage is %s start_YYYY MM DD HH MM Num_Seconds Num_Sources Source_File Display_Height(m)\n", argv[0]);
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

    


    LatLon_Cart WLL(3828445.659, 445223.600, 5064921.568);
    //Station_Reference Westerbork(WLL.Lat(), WLL.Lon(), 0.0, radius_Earth);
    Station_Reference Westerbork(0.0,0.0, 0.0, radius_Earth);


    Space_Unit_Vector direction = WLL.make_unit_vector();

//     // Make a fake ionosphere
//     Ionosphere_Fake test_iono(fake_Gaussian,1E-6,1E-4);
    Ionosphere_IRI test_iono(3E-7,1E-5);


    Real64 range = 30E3;
    for(Uint32 i=0; i < 40; i++, range *= 1.2) {
        Real64 elec = test_iono.Electron_Density_Range(Westerbork,
                                                       eval_time,
                                                       direction,
                                                       range-Westerbork.Elevation());
        printf("    %E, %E,\n", range-Westerbork.Elevation(), elec);
    }
    
    
    return 0;
}
    






