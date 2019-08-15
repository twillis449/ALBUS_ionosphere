// lofar7.cxx
// test VLA 74 MHz B-array isoplanatic patch size information
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 22  James M Anderson  --JIVE start






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
#include "ionosphere_iri.h"
#include "ionosphere_iriplus.h"



// set up a namespace area for stuff.
using namespace MIM_PIM;


    



// GLOBALS


// FUNCTIONS


extern "C" int Real64_compare(const void* a, const void* b)
{
    Real64 aa = *(Real64*)a;
    Real64 bb = *(Real64*)b;
    if(aa<bb) return -1;
    else if(aa>bb) return +1;
    return 0;
}



int main(int argc, char* argv[])
{
    if(argc != 7) {
        fprintf(stderr, "Error: correct usage is %s YYYY MM DD HH MM wavlength(m)\n", argv[0]);
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
    



    Real64 baseline = 6E3;

    Station_LatLon Center(34.0*M_DEG2RAD,-107.4*M_DEG2RAD,
                            2124.0, radius_Earth);
    Station_LatLon East_Arm(34.0*M_DEG2RAD,-107.4*M_DEG2RAD
                            + baseline*0.5/radius_Earth,
                            2124.0, radius_Earth);
    Station_LatLon West_Arm(34.0*M_DEG2RAD,-107.4*M_DEG2RAD
                            - baseline*0.5/radius_Earth,
                            2124.0, radius_Earth);


//     // Make a fake ionosphere
//     Ionosphere_Fake test_iono(fake_Gaussian,1E-6,1E-4);
    // Make an IRI ionosphere
//     Ionosphere_IRI_Plus test_iono1(Center, eval_time,
//                                    90.0*M_DEG2RAD,    5E3, 0.032E12, 67.0,
//                                    1E-6,1E-4);
//     Ionosphere_IRI_Plus test_iono2(Center, eval_time,
//                                    90.0*M_DEG2RAD,   14E3, 0.032E12, 67.0,
//                                    1E-6,1E-4);
//     Ionosphere_IRI_Plus test_iono3(Center, eval_time,
//                                    90.0*M_DEG2RAD,   26E3, 0.032E12, 67.0,
//                                    1E-6,1E-4);
//     Ionosphere_IRI_Plus test_iono4(Center, eval_time,
//                                    90.0*M_DEG2RAD, 0.33E3, 0.032E12, 67.0,
//                                    1E-6,1E-4);
    
    Ionosphere_IRI_Plus test_iono(Center, eval_time,
                                  90.0*M_DEG2RAD, atof(argv[6]), 0.01E12, 67.0,
                                   1E-6,1E-4);





    Real64 mean_El = 35.0 * M_DEG2RAD;
    Real64 Az1 =  90.0 * M_DEG2RAD;
//    Real64 Az2 = -90.0 * M_DEG2RAD;
   
    const Uint32 SIZE = 120;
//    const Uint32 SIZED = SIZE << 1;
    const Uint32 HALF = SIZE >> 1;
    Real64 holder[SIZE];
    



//     // test the wave part of the ionosphere
//     for(Uint32 s = 0; s < 4*3600; s+=120) {
//         eval_time.tm_sec = s;
//         // Get the integrated electron content
//         Space_Unit_Vector dir = East_Arm.get_pierce_location(
//             Az1, mean_El,
//             300E3, radius_Earth
//             ).make_unit_vector();
//         Real64 East_low = test_iono.Integrated_Electron_Density(
//             East_Arm, eval_time, dir);
//         printf("%4u %.4f\n", s, East_low * 1E-16);
//     }
//     exit(1);
    



    for(Real64 delta = 0.5; delta <= 15.0; delta += 0.5) {
        Real64 Delta = delta * M_DEG2RAD;

        Real64 El_low = mean_El - 0.5*Delta;
        Real64 El_high = mean_El + 0.5*Delta;


        
        // Get the integrated electron content
        Space_Unit_Vector dir_low1 = East_Arm.get_pierce_location(
            Az1, El_low,
            300E3, radius_Earth
            ).make_unit_vector();
        Space_Unit_Vector dir_high1 = East_Arm.get_pierce_location(
            Az1, El_high,
            300E3, radius_Earth
            ).make_unit_vector();
//         Space_Unit_Vector dir_low2 = East_Arm.get_pierce_location(
//             Az2, El_low,
//             300E3, radius_Earth
//             ).make_unit_vector();
//         Space_Unit_Vector dir_high2 = East_Arm.get_pierce_location(
//             Az2, El_high,
//             300E3, radius_Earth
//             ).make_unit_vector();



        Real64 sum = 0.0;
        Real64 sum_sqr = 0.0;
        for(Uint32 s = 0; s < SIZE; s++) {
            struct tm test_time = eval_time;
            test_time.tm_sec = s*113; // a prime number of seconds close to 2 min

            
//             Ionosphere_IRI_Plus& test_iono = (s < 15) ? test_iono1
//                 : ( (s < 30) ? test_iono2 :
//                     ( (s < 45) ? test_iono3 : test_iono4));

            Real64 diff1 =
                ( test_iono.Integrated_Electron_Density(
                    East_Arm, test_time, dir_high1)
                  - test_iono.Integrated_Electron_Density(
                      East_Arm, test_time, dir_low1) )
                - ( test_iono.Integrated_Electron_Density(
                        West_Arm, test_time, dir_high1)
                    - test_iono.Integrated_Electron_Density(
                        West_Arm, test_time, dir_low1) );
//             Real64 diff2 =
//                 (  test_iono.Integrated_Electron_Density(
//                     East_Arm, eval_time, dir_low2)
//                    - test_iono.Integrated_Electron_Density(
//                        East_Arm, eval_time, dir_high2) )
//                 - ( test_iono.Integrated_Electron_Density(
//                         West_Arm, eval_time, dir_low2)
//                     - test_iono.Integrated_Electron_Density(
//                         West_Arm, eval_time, dir_high2) );
        

//         Real64 East_low = test_iono.Integrated_Electron_Density(
//             East_Arm, eval_time, dir_low);

//         Real64 East_high = test_iono.Integrated_Electron_Density(
//             East_Arm, eval_time, dir_high);

//         Real64 West_low = test_iono.Integrated_Electron_Density(
//             West_Arm, eval_time, dir_low);

//         Real64 West_high = test_iono.Integrated_Electron_Density(
//             West_Arm, eval_time, dir_high);

            // convert from m^{-2} to radians of phase at 74 MHz
            diff1 *= 1E-16 * 8.442E3 / 74.0;
//            diff2 *= 1E-16 * 8.442E3 / 74.0;
            // convert to a position shift
            Real64 theta1 =
                asin(diff1*2.998E8/74E6 / (2.0*M_PI) / baseline) * M_RAD2DEG * 3600;
//             Real64 theta2 =
//                 asin(diff2*2.998E8/74E6 / (2.0*M_PI) / baseline) * M_RAD2DEG * 3600;


            //printf("Delta %E got %E\n", delta, theta1);
            holder[s] = theta1;
//            holder[s+SIZE] = theta2;
            sum += theta1;
            sum_sqr += theta1*theta1;
//            sum += theta1 + theta2;
        }
        Real64 average = sum / SIZE;
        for(Uint32 s=0; s < SIZE; s++) {
            holder[s] = fabs(holder[s] - average) + fabs(average);
        }
        qsort(holder, SIZE, sizeof(Real64), Real64_compare);
        //for(Uint32 s=0; s < SIZE; s++) printf("%u %E\n", s, holder[s]);
        printf("Delta %5.1f Med %.2f   sum %.17E ssq %.17E\n", delta, holder[HALF], sum, sum_sqr);
    }


    
    return 0;
}
    






