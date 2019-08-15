// test.cxx
// test LOFAR ionosphere stuff
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE start






// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "cal_source.h"
#include "space_vector.h"
#include "space_unit_vector.h"
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
#include "atmosphere_point.h"
#include "ionosphere.h"
#include "ionosphere_fake.h"



// set up a namespace area for stuff.
using namespace MIM_PIM;





int main(void)
{
//     Space_Vector V(1.,1.,1.);
//     Space_Unit_Vector U(V);
//     Space_Vector V2(U);

//     printf("%.3f %.3f %.3f\n", U.Axis(x_axis),U.Axis(y_axis),U.Axis(z_axis));
//     printf("%.3f %.3f %.3f\n", V2.Axis(x_axis),V2.Axis(y_axis),V2.Axis(z_axis));


//     Station_LatLon s(52.*M_DEG2RAD,6.*M_DEG2RAD);
//     Space_Unit_Vector D(0.,0.,1.);
//     Real64 Range = 25000E3;
//     Space_Vector p = s+Range*D;
//     printf("%.3f %.3f %.3f\n", s.Axis(x_axis),s.Axis(y_axis),s.Axis(z_axis));
//     printf("%.3f %.3f %.3f\n", p.Axis(x_axis),p.Axis(y_axis),p.Axis(z_axis));
//     printf("Station r=%.3f point r=%.3f\n", s.Radius(), p.Radius());


//     // Ok, test out the pierce points
//     Real64 h = 1000E3;
//     Real64 range = 1500E3;
//     Space_Vector P1 = s.get_pierce_location(0.0*M_DEG2RAD, (52.0)*M_DEG2RAD,
//                                             h, radius_Earth);
//     printf("%.8f  %.8f  %.8f\n",P1.Axis(x_axis),P1.Axis(y_axis),P1.Axis(z_axis));
//     Space_Vector P2 = s.get_pierce_location(D, h, radius_Earth);
//     printf("%.8f  %.8f  %.8f\n",P2.Axis(x_axis),P2.Axis(y_axis),P2.Axis(z_axis));
//     Space_Vector P3 = s.get_range_location(0.0*M_DEG2RAD, (52.0)*M_DEG2RAD,
//                                            range, radius_Earth);
//     printf("%.8f  %.8f  %.8f\n",P3.Axis(x_axis),P3.Axis(y_axis),P3.Axis(z_axis));
//     Space_Vector P4 = s.get_range_location(D, range);
//     printf("%.8f  %.8f  %.8f\n",P4.Axis(x_axis),P4.Axis(y_axis),P4.Axis(z_axis));
//     Real64 r2 = s.get_pierce_range(D, h, radius_Earth);
//     printf("got range %.8f\n", r2);
//     Real64 h2 = s.get_range_height(D, r2, radius_Earth);
//     printf("got height %.8f\n", h2);





//     // Test out the Atmosphere Point stuff
//     Atmosphere_Point a(90.*M_DEG2RAD,0.*M_DEG2RAD);
//     LatLon_Cart b=a.make_unit_vector();
//     printf("%.3f %.3f %.3f %.3f\n", a.cLat(),a.cLon(),a.sLat(),a.sLon());
//     printf("%.3f %.3f %.3f %.3f\n", b.cLat(),b.cLon(),b.sLat(),b.sLon());
//     printf("%.3f %.3f %.3f\n", b.Axis(x_axis),b.Axis(y_axis),b.Axis(z_axis));
//     Space_Vector d1(1.0,0.0,0.0);
//     Space_Vector r1 = a.Rotate_IGRF_Vector(d1);
//     printf("\n%.3f %.3f %.3f\n", d1.Axis(x_axis),d1.Axis(y_axis),d1.Axis(z_axis));
//     printf("%.3f %.3f %.3f\n", r1.Axis(x_axis),r1.Axis(y_axis),r1.Axis(z_axis));
//     Space_Vector d2(0.0,1.0,0.0);
//     Space_Vector r2 = a.Rotate_IGRF_Vector(d2);
//     printf("\n%.3f %.3f %.3f\n", d2.Axis(x_axis),d2.Axis(y_axis),d2.Axis(z_axis));
//     printf("%.3f %.3f %.3f\n", r2.Axis(x_axis),r2.Axis(y_axis),r2.Axis(z_axis));
//     Space_Vector d3(0.0,0.0,1.0);
//     Space_Vector r3 = a.Rotate_IGRF_Vector(d3);
//     printf("\n%.3f %.3f %.3f\n", d3.Axis(x_axis),d3.Axis(y_axis),d3.Axis(z_axis));
//     printf("%.3f %.3f %.3f\n", r3.Axis(x_axis),r3.Axis(y_axis),r3.Axis(z_axis));
     
//     Station_Reference s(5.99*M_DEG2RAD,5.99*M_DEG2RAD);
//     printf("%12.3f %12.3f %12.3f\n", s.Axis(x_axis),s.Axis(y_axis),s.Axis(z_axis));

//     LatLon_Cart r1 = s.get_polar_offset(LatLon_Cart(6.*M_DEG2RAD,
//                                                     6.*M_DEG2RAD));
//     printf("%12.3f %12.3f %12.3f   %8.3f %8.3f\n",
//            r1.Axis(x_axis),r1.Axis(y_axis),r1.Axis(z_axis),
//            r1.Lat()*M_RAD2DEG, r1.Lon()*M_RAD2DEG);


//     // Ok, let's fake an ionosphere observation
//     // First, the ionosphere
//     Ionosphere_Fake test_iono(fake_constant);
//     // Next, give me a station near Wb
//     Station_LatLon station(52.0*M_DEG2RAD,6.0*M_DEG2RAD);

//     // Need a time.  How about now?
//     struct tm time_tm;
//     {
//         time_t now = time(NULL);
//         gmtime_r(&now, &time_tm);
//     }

//     // Need a point in the ionosphere to look at.  Get a zenith
//     // direction
//     Space_Unit_Vector direction = station.make_unit_vector();
//     // Now find some spot about 200 km up
//     LatLon_Cart iono_point = station.get_pierce_location(direction,
//                                                          200E3,
//                                                          radius_Earth);

// //     Real64 e_density = test_iono.Electron_Density(iono_point,
// //                                                   time_tm);

// //     printf("got density %E\n", e_density);

// //     e_density = test_iono.Electron_Density_Range(
// //         station, time_tm, direction, 200E3);
// //     printf("got density %E\n", e_density);

//     Real64 e_column = test_iono.Integrated_Electron_Density(
//         station, time_tm, direction, 0, 25000E3);
//     printf("Got electron column density %14E    %10.5f TECU\n",
//            e_column, e_column*1E-16);

//     // Now, give me a direction which is toward the south at 30 degrees elevation
//     Space_Unit_Vector direction_2 =
//         (station.get_pierce_location(180.0*M_DEG2RAD,
//                                      30.0*M_DEG2RAD,
//                                      200E3,
//                                      radius_Earth)
//          - station).make_unit_vector();
//     e_column = test_iono.Integrated_Electron_Density(
//         station, time_tm, direction_2, 0, 25000E3);
//     printf("Got electron column density %14E    %10.5f TECU\n",
//            e_column, e_column*1E-16);


//     Space_Unit_Vector direction_3 =
//         (station.get_pierce_location(180.0*M_DEG2RAD,
//                                      10.0*M_DEG2RAD,
//                                      200E3,
//                                      radius_Earth)
//          - station).make_unit_vector();
//     e_column = test_iono.Integrated_Electron_Density(
//         station, time_tm, direction_3, 0, 25000E3);
//     printf("Got electron column density %14E    %10.5f TECU\n",
//            e_column, e_column*1E-16);


//     Space_Unit_Vector direction_4 =
//         (station.get_pierce_location(180.0*M_DEG2RAD,
//                                      5.0*M_DEG2RAD,
//                                      200E3,
//                                      radius_Earth)
//          - station).make_unit_vector();
//     e_column = test_iono.Integrated_Electron_Density(
//         station, time_tm, direction_4, 0, 25000E3);
//     printf("Got electron column density %14E    %10.5f TECU\n",
//            e_column, e_column*1E-16);









//     // Ok, let's fake an ionosphere observation
//     // First, the ionosphere
//     Ionosphere_Fake test_iono(fake_Gaussian,1E-7,1E-5);
//     // Next, give me a station near Wb
//     Station_LatLon station(52.0*M_DEG2RAD,6.0*M_DEG2RAD);

//     // Need a time.  How about now?
//     struct tm time_tm;
//     {
//         time_t now = time(NULL);
//         gmtime_r(&now, &time_tm);
//     }

//     // Need a point in the ionosphere to look at.  Get a zenith
//     // direction
//     Space_Unit_Vector direction = station.make_unit_vector();
//     // Now find some spot about 200 km up
//     LatLon_Cart iono_point = station.get_pierce_location(direction,
//                                                          200E3,
//                                                          radius_Earth);

//     Space_Vector B = test_iono.Magnetic_Field(iono_point,
//                                               time_tm);

//     printf("got B field  %14E %14E %14E\n", B.Axis(x_axis),B.Axis(y_axis),B.Axis(z_axis));

//     B = test_iono.Magnetic_Field_Range(
//         station, time_tm, direction, 200E3);
//     printf("got B field  %14E %14E %14E\n", B.Axis(x_axis),B.Axis(y_axis),B.Axis(z_axis));

//     Real64 B_column = test_iono.Integrated_Faraday_Rotation(
//         station, time_tm, direction, 0, 25000E3);
//     printf("Got Faraday column density %14E    %10.5f RMU\n",
//            B_column, B_column*1E-12);

//     // Now, give me a direction which is toward the south at 30 degrees elevation
//     Space_Unit_Vector direction_2 =
//         (station.get_pierce_location(180.0*M_DEG2RAD,
//                                      30.0*M_DEG2RAD,
//                                      200E3,
//                                      radius_Earth)
//          - station).make_unit_vector();
//     B_column = test_iono.Integrated_Faraday_Rotation(
//         station, time_tm, direction_2, 0, 25000E3);
//     printf("Got Faraday column density %14E    %10.5f RMU\n",
//            B_column, B_column*1E-12);


//     Space_Unit_Vector direction_3 =
//         (station.get_pierce_location(180.0*M_DEG2RAD,
//                                      10.0*M_DEG2RAD,
//                                      200E3,
//                                      radius_Earth)
//          - station).make_unit_vector();
//     B_column = test_iono.Integrated_Faraday_Rotation(
//         station, time_tm, direction_3, 0, 25000E3);
//     printf("Got Faraday column density %14E    %10.5f RMU\n",
//            B_column, B_column*1E-12);


//     Space_Unit_Vector direction_4 =
//         (station.get_pierce_location(180.0*M_DEG2RAD,
//                                      5.0*M_DEG2RAD,
//                                      200E3,
//                                      radius_Earth)
//          - station).make_unit_vector();
//     B_column = test_iono.Integrated_Faraday_Rotation(
//         station, time_tm, direction_4, 0, 25000E3);
//     printf("Got Faraday column density %14E    %10.5f RMU\n",
//            B_column, B_column*1E-12);





    Station_LatLon s(00.0*M_DEG2RAD,0.0*M_DEG2RAD);
    Space_Vector RAD(0,1,0);
    LatLon_Cart AA = s.convert_RADec_to_AltAz(RAD);
    printf("%12.3f %12.3f %12.3f   %8.3f %8.3f\n",
           AA.Axis(x_axis),AA.Axis(y_axis),AA.Axis(z_axis),
           AA.Lat()*M_RAD2DEG, AA.Lon()*M_RAD2DEG);
    
    
    return 0;
}
