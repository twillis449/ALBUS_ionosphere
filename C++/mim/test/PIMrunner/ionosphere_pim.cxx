// ionosphere_pim.cxx
// make up some iri electron densities
//_HIST  DATE NAME PLACE INFO
//	2005 Dec 13  James M Anderson  --JIVE  start from ionosphere_iri.cxx
//	2007 Apr 03  JMA  --implement new PIM electron density subroutine




// INCLUDES
#define IONOSPHERE_PIM_CXX
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>

#include "ionosphere.h"
#include "ionosphere_pim.h"
#include "station_maker.h"

#include "jma_irisub_c.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



//_TITLE  Ionosphere_PIM::Electron_Density --get the IRI ionospehre
Real64 Ionosphere_PIM::Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
	  const LatLon_Cart& position,
                               // I  The position about the Earth at which to
                               //    evaluate the ionosphere electron density
          const JMA_VEX_AREA::VEX_Time& time
                               // I  The time (UT) at which to evaluate the
                               //    ionosphere
          )

// Real64 Ionosphere_PIM::Electron_Density
//                                O  The electron density, in m^{-3} at the
//                                   position and time specified.
//	                             Negative values are unphysical, and will
//                                   be used to indicate error problems.
//                                   -2.0E300 nonsense altitude


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function generates a PIM ionosphere electron density. 

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 08  James M Anderson  --JIVE  start
//	2007 Apr 03  JMA  --call new PIM subroutine
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
    // declare the electron density with a sane value.
    doublereal n_e_density = 0.0;
    
    Real64 TUT = time.Day_Fraction() * 24.0;
    integer SOLCODE=0;
    integer RETCODE=0;
    pim_asub_ed_c(
        PIM_DATA_PATH,
        position.Lat(),
        position.Lon(),
        position.Radius() - radius_Earth,
        time.Year(),
        time.Yday(),
        time.Month(),
        time.Day(),
        TUT,
        &n_e_density,
        (integer*)&SOLCODE,
        (integer*)&RETCODE
        );
    if(RETCODE < 0) {
        fprintf(stderr, "Error: bad RETCODE=%d from pim_asub_ed_c\n",
                RETCODE);
        exit(1);
    }
    return n_e_density;
}








// //_TITLE  Integrated_Electron_Density --report the integrated electron density
// Real64 Ionosphere_PIM::Integrated_Electron_Density(
// //_ARGS  TYPE           VARIABLE I/O DESCRIPTION
//         const Station_LatLon& station,
//                                // I  the station position (telescope), which
//                                //    is used to determine where one is looking
//                                //    from.
//         const struct tm& time, // I  What time is it at the station?  In UT
//         const Space_Unit_Vector& direction,
//                                // I  The direction in which the telescope is
//                                //    looking.
//         Real64 altitude_min,
//                                //    The minimum altitude used for the
//                                //    integration of the electron density to
//                                //    column density.  Note that this is the
//                                //    *ALTITUDE* above the Earth's mean surface,
//                                //    not the minimum range. The altitude is
//                                //    expected in meters.
//         Real64 altitude_max
//                                // I  The maximum altitude of the integration,
//                                //    in meters.  Note that GPS satellite
//                                //    altitudes are about 22300E3 meters, so
//                                //    most models probably will not do well far
//                                //    above that.
//         )
// // Real64 Ionosphere_PIM::integrated_electron_density
// //                                O  The integrated electron density (hence a
// //                                   column density) in m^{-2}.
// //                                   If less than 0, then some error has occured.
// //                                   -1.0E300 direction below horizon.
// //                                   -2.0E300 nonsense altitudes

// //_USER  any user input?

// //_VARS  TYPE           VARIABLE I/O DESCRIPTION
// //       put globals used here

// //_DESC  full description of program

// //_FILE  files used and logical units used

// //_LIMS  design limitations

// //_BUGS  known bugs

// //_CALL  list of calls

// //_KEYS  

// //_HIST  DATE NAME PLACE INFO
// //	2005 Dec 13  James M Anderson  --JIVE  start

// //_END

//     {
//         // check the input altitudes
// #ifdef DEBUG
//         if((altitude_min < station.Elevation())
//            || (altitude_max <= altitude_min)) {
//             fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
//                     altitude_min, altitude_max,
//                     __FILE__, __LINE__, __func__);
//             return -2.0E300;
//         }
// #endif
//         // check that the direction is above the horizon
//         if(station.dot_product(direction) <= 0.0) {
//             // direction below horizon.  Return an error, but no warning,
//             // as this is not too severe
//             return -1.0E300;
//         }



//         Real64 electron_integral;
//         Real64 Faraday_integral;
//         Integrate_Electron_Faraday(
//             station,
//             time,
//             direction,
//             &electron_integral,
//             &Faraday_integral,
//             altitude_min,
//             altitude_max
//             );

//         return electron_integral;
//     }
    










// //_TITLE  Integrated_Faraday_Rotation --report the integrated Faraday Rotation
//     Real64 Ionosphere_PIM::Integrated_Faraday_Rotation(
// //_ARGS  TYPE           VARIABLE I/O DESCRIPTION
//         const Station_LatLon& station,
//                                // I  the station position (telescope), which
//                                //    is used to determine where one is looking
//                                //    from.
//         const struct tm& time, // I  What time is it at the station?  In UT
//         const Space_Unit_Vector& direction,
//                                // I  The direction in which the telescope is
//                                //    looking.
//         Real64 altitude_min,
//                                //    The minimum altitude used for the
//                                //    integration of the electron density to
//                                //    column density.  Note that this is the
//                                //    *ALTITUDE* above the Earth's mean surface,
//                                //    not the minimum range. The altitude is
//                                //    expected in meters.
//         Real64 altitude_max
//                                // I  The maximum altitude of the integration,
//                                //    in meters.  Note that GPS satellite
//                                //    altitudes are about 22300E3 meters, so
//                                //    most models probably will not do well far
//                                //    above that.
//         )
// // Real64 Ionosphere_PIM::Integrated_Faraday_Rotation
// //                                O  The integrated Faraday rotation stuff
// //                                   in T m^{-2}.
// //                                   If \leq -1E300, then some error has occured.
// //                                   -1.0E300 direction below horizon.
// //                                   -2.0E300 nonsense altitudes


// //_USER  any user input?

// //_VARS  TYPE           VARIABLE I/O DESCRIPTION
// //       put globals used here

// //_DESC  full description of program

// //_FILE  files used and logical units used

// //_LIMS  design limitations

// //_BUGS  known bugs

// //_CALL  list of calls

// //_KEYS  

// //_HIST  DATE NAME PLACE INFO
// //	2005 Dec 13  James M Anderson  --JIVE  start

// //_END

//     {
//         // check the input altitudes
// #ifdef DEBUG
//         if((altitude_min < station.Elevation())
//            || (altitude_max <= altitude_min)) {
//             fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
//                     altitude_min, altitude_max,
//                     __FILE__, __LINE__, __func__);
//             return -2.0E300;
//         }
// #endif
//         // check that the direction is above the horizon
//         if(station.dot_product(direction) <= 0.0) {
//             // direction below horizon.  Return an error, but no warning,
//             // as this is not too severe
//             return -1.0E300;
//         }


//         Real64 electron_integral;
//         Real64 Faraday_integral;
//         Integrate_Electron_Faraday(
//             station,
//             time,
//             direction,
//             &electron_integral,
//             &Faraday_integral,
//             altitude_min,
//             altitude_max
//             );

//         return Faraday_integral;
//     }











    










// //_TITLE  Integrate_Electron_Faraday --report the integrated electron and Faraday
//     void Ionosphere_PIM::Integrate_Electron_Faraday(
// //_ARGS  TYPE           VARIABLE I/O DESCRIPTION
//         const Station_LatLon& station,
//                                // I  the station position (telescope), which
//                                //    is used to determine where one is looking
//                                //    from.
//         const struct tm& time, // I  What time is it at the station?  In UT
//         const Space_Unit_Vector& direction,
//                                // I  The direction in which the telescope is
//                                //    looking.
//         Real64* const electron_integral,
//                                // O  The integrated electron column density,
//                                //    in m^{-2}
// //                                   If less than 0, then some error has occured.
// //                                   -1.0 direction below horizon.
// //                                   -2.0 nonsense altitudes
//         Real64* const Faraday_integral,
//                                // O  The integrated Faraday rotation measure
//                                //    in T m^{-2}
//         Real64 altitude_min,
//                                //    The minimum altitude used for the
//                                //    integration of the electron density to
//                                //    column density.  Note that this is the
//                                //    *ALTITUDE* above the Earth's mean surface,
//                                //    not the minimum range. The altitude is
//                                //    expected in meters.
//         Real64 altitude_max
//                                // I  The maximum altitude of the integration,
//                                //    in meters.  Note that GPS satellite
//                                //    altitudes are about 22300E3 meters, so
//                                //    most models probably will not do well far
//                                //    above that.
//         )


// //_USER  any user input?

// //_VARS  TYPE           VARIABLE I/O DESCRIPTION
// //       put globals used here

// //_DESC  full description of program

// //_FILE  files used and logical units used

// //_LIMS  design limitations

// //_BUGS  known bugs

// //_CALL  list of calls

// //_KEYS  

// //_HIST  DATE NAME PLACE INFO
// //	2005 Dec 13  James M Anderson  --JIVE  start

// //_END

//     {
//         // Set the output to somethng sane
//         *electron_integral = *Faraday_integral = 0.0;
        
//         // check the input altitudes
// #ifdef DEBUG
//         if((altitude_min < station.Elevation())
//            || (altitude_max <= altitude_min)) {
//             fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
//                     altitude_min, altitude_max,
//                     __FILE__, __LINE__, __func__);
//             *electron_integral = -2.0E300;
//             return;
//         }
// #endif
//         // check that the direction is above the horizon
//         if(station.dot_product(direction) <= 0.0) {
//             // direction below horizon.  Return an error, but no warning,
//             // as this is not too severe
//             *electron_integral = -1.0E300;
//             return;
//         }

//         LatLon_Cart AltAz = station.convert_RADec_to_AltAz(direction);
//         Real64 El   = AltAz.Lat();
//         Real64 Az   = AltAz.Lon();


//         // this looks like a valid sky position to get a
//         // TEC value.  Call the PIM stuff
//         Real64 TUT = (time.tm_hour +(time.tm_min
//                                      +(time.tm_sec)
//                                      /60.0)/60.0);
//         doublereal STEC, SRM;
//         integer SOLCODE=0;
//         integer RETCODE=0;
//         pim_asub_c(
//             PIM_DATA_PATH,
//             station.Lat(),
//             station.Lon(),
//             station.Radius() - radius_Earth,
//             Az,
//             El,
//             time.tm_year+1900,
//             time.tm_yday+1,
//             time.tm_mon+1,
//             time.tm_mday,
//             TUT,
//             &STEC,
//             &SRM,
//             (integer*)&SOLCODE,
//             (integer*)&RETCODE
//             );
//         if(RETCODE < 0) {
//             fprintf(stderr, "Error: bad RETCODE=%d from pim_asub_c\n",
//                     RETCODE);
//             exit(1);
//         }


//         *electron_integral = STEC * 1E16; // convert to SI units
//         *Faraday_integral = SRM * 1E12;   // convert to SI units
        
//         return;
//     }

    
    



}  // end namespace


