// ionosphere_gps.cxx
// model ionosphere from GPS receiver data
//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start from ionosphere_pim.cxx
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>

#include "ionosphere.h"
#include "ionosphere_gps.h"
#include "GPS.h"
#include "station_maker.h"
#include "station_reference.h"
#include "vex_time.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



//_TITLE  Ionosphere_GPS::Electron_Density --get the GPS electron density
Real64 Ionosphere_GPS::Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
	  const LatLon_Cart& position,
                               // I  The position about the Earth at which to
                               //    evaluate the ionosphere electron density
          const JMA_VEX_AREA::VEX_Time& time
                               // I  The time (UT) at which to evaluate the
                               //    ionosphere
          )

// Real64 Ionosphere_GPS::Electron_Density
//                                O  The electron density, in m^{-3} at the
//                                   position and time specified.
//	                             Negative values are unphysical, and will
//                                   be used to indicate error problems.
//                                   -2.0E300 nonsense altitude


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function generates a GPS ionosphere electron density. At the moment
//	this is broken, and so the function will die.  This needs to be
//	fixed.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
    // declare the electron density with a sane value.
    Real64 n_e_density = 0.0;
        
    fprintf(stderr, "Error: GPS electron density not currently working\n");
    exit(1);
    return n_e_density;
}








//_TITLE  Integrated_Electron_Density --report the integrated electron density
Real64 Ionosphere_GPS::Integrated_Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_LatLon& station,
                               // I  the station position (telescope), which
                               //    is used to determine where one is looking
                               //    from.
        const JMA_VEX_AREA::VEX_Time& time, // I  What UT time is it
        const Space_Unit_Vector& direction,
                               // I  The direction in which the telescope is
                               //    looking.
        Real64 altitude_min,
                               //    The minimum altitude used for the
                               //    integration of the electron density to
                               //    column density.  Note that this is the
                               //    *ALTITUDE* above the Earth's mean surface,
                               //    not the minimum range. The altitude is
                               //    expected in meters.
        Real64 altitude_max
                               // I  The maximum altitude of the integration,
                               //    in meters.  Note that GPS satellite
                               //    altitudes are about 22300E3 meters, so
                               //    most models probably will not do well far
                               //    above that.
        )
// Real64 Ionosphere_GPS::integrated_electron_density
//                                O  The integrated electron density (hence a
//                                   column density) in m^{-2}.
//                                   If less than 0, then some error has occured.
//                                   -1.0E300 direction below horizon.
//                                   -2.0E300 nonsense altitudes
//                                   GPS_BAD_DATA_VALUE = -999.0
//                                       means there is a gap in the data, or
//                                       no satellites meat the criteria.  The
//                                       caller should be able to deal with this
//                                       in a reasonable fashion
//                                   GPS_NOT_INITIALIZED_VALUE = -1000.0
//                                       means that some portion of the GPS
//                                       data was not initialized.  This should
//                                       be treated as a programmer error on the
//                                       caller's side.

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations
//	At this time, the GPS routines can only integrate from the surface
//	to the GPS satellite altitude.  The actual requested altitude
//	range is ignored.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Dec 13  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
    // check the input altitudes
#ifdef DEBUG
    if((altitude_min < station.Elevation())
       || (altitude_max <= altitude_min)) {
        fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
                altitude_min, altitude_max,
                __FILE__, __LINE__, __func__);
        return -2.0E300;
    }
#endif
    // check that the direction is above the horizon
    if(station.dot_product(direction) <= 0.0) {
        // direction below horizon.  Return an error, but no warning,
        // as this is not too severe
        return -1.0E300;
    }



    Real64 electron_integral;
    Real64 electron_error;
    Real64 Faraday_integral;
    Integrate_Electron_Faraday(
        station,
        time,
        direction,
        &electron_integral,
        &electron_error,
        &Faraday_integral,
        altitude_min,
        altitude_max
        );

    return electron_integral;
}
    










//_TITLE  Integrated_Faraday_Rotation --report the integrated Faraday Rotation
Real64 Ionosphere_GPS::Integrated_Faraday_Rotation(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_LatLon& station,
                               // I  the station position (telescope), which
                               //    is used to determine where one is looking
                               //    from.
        const JMA_VEX_AREA::VEX_Time& time, // I  What time is it?  In UT
        const Space_Unit_Vector& direction,
                               // I  The direction in which the telescope is
                               //    looking.
        Real64 altitude_min,
                               //    The minimum altitude used for the
                               //    integration of the electron density to
                               //    column density.  Note that this is the
                               //    *ALTITUDE* above the Earth's mean surface,
                               //    not the minimum range. The altitude is
                               //    expected in meters.
        Real64 altitude_max
                               // I  The maximum altitude of the integration,
                               //    in meters.  Note that GPS satellite
                               //    altitudes are about 22300E3 meters, so
                               //    most models probably will not do well far
                               //    above that.
        )
// Real64 Ionosphere_GPS::Integrated_Faraday_Rotation
//                                O  The integrated Faraday rotation stuff
//                                   in T m^{-2}.
//                                   If \leq -1E300, then some error has occured.
//                                   -1.0E300 direction below horizon.
//                                   -2.0E300 nonsense altitudes
//                                   GPS_BAD_DATA_VALUE = -999.0
//                                       means there is a gap in the data, or
//                                       no satellites meat the criteria.  The
//                                       caller should be able to deal with this
//                                       in a reasonable fashion
//                                   GPS_NOT_INITIALIZED_VALUE = -1000.0
//                                       means that some portion of the GPS
//                                       data was not initialized.  This should
//                                       be treated as a programmer error on the
//                                       caller's side.


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations
//	At this time, the GPS routines can only integrate from the surface
//	to the GPS satellite altitude.  The actual requested altitude
//	range is ignored.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
    // check the input altitudes
#ifdef DEBUG
    if((altitude_min < station.Elevation())
       || (altitude_max <= altitude_min)) {
        fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
                altitude_min, altitude_max,
                __FILE__, __LINE__, __func__);
        return -2.0E300;
    }
#endif
    // check that the direction is above the horizon
    if(station.dot_product(direction) <= 0.0) {
        // direction below horizon.  Return an error, but no warning,
        // as this is not too severe
        return -1.0E300;
    }


    Real64 electron_integral;
    Real64 electron_error;
    Real64 Faraday_integral;
    Integrate_Electron_Faraday(
        station,
        time,
        direction,
        &electron_integral,
        &electron_error,
        &Faraday_integral,
        altitude_min,
        altitude_max
        );

    return Faraday_integral;
}











    










//_TITLE  Integrate_Electron_Faraday --report the integrated electron and Faraday
void Ionosphere_GPS::Integrate_Electron_Faraday(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_LatLon& station,
                               // I  the station position (telescope), which
                               //    is used to determine where one is looking
                               //    from.
        const JMA_VEX_AREA::VEX_Time& time, // I  What time is it?  In UT
        const Space_Unit_Vector& direction,
                               // I  The direction in which the telescope is
                               //    looking.
        Real64* const electron_integral,
                               // O  The integrated electron column density,
                               //    in m^{-2}
//                                   If less than 0, then some error has occured.
//                                   -1.0E300 direction below horizon.
//                                   -2.0E300 nonsense altitudes
//                                   GPS_BAD_DATA_VALUE = -999.0
//                                       means there is a gap in the data, or
//                                       no satellites meat the criteria.  The
//                                       caller should be able to deal with this
//                                       in a reasonable fashion
//                                   GPS_NOT_INITIALIZED_VALUE = -1000.0
//                                       means that some portion of the GPS
//                                       data was not initialized.  This should
//                                       be treated as a programmer error on the
//                                       caller's side.
        Real64* const electron_error,
                               // O  The integrated electron column density,
                               //    in m^{-2}

        Real64* const Faraday_integral,
                               // O  The integrated Faraday rotation measure
                               //    in T m^{-2}
        Real64 altitude_min,
                               //    The minimum altitude used for the
                               //    integration of the electron density to
                               //    column density.  Note that this is the
                               //    *ALTITUDE* above the Earth's mean surface,
                               //    not the minimum range. The altitude is
                               //    expected in meters.
        Real64 altitude_max
                               // I  The maximum altitude of the integration,
                               //    in meters.  Note that GPS satellite
                               //    altitudes are about 22300E3 meters, so
                               //    most models probably will not do well far
                               //    above that.
//        Real64* const electron_error
                               // O  The integrated electron column density error,
                               //    in m^{-2}
        )


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations
//	At this time, the GPS routines can only integrate from the surface
//	to the GPS satellite altitude.  The actual requested altitude
//	range is ignored.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
//    fprintf(stdout, "In Ionosphere_GPS::Integrate_Electron_Faraday\n");
    // Set the output to somethng sane
    *electron_integral = *Faraday_integral = 0.0;
    *electron_error = 0.0;
        
    // check the input altitudes
#ifdef DEBUG
    if((altitude_min < station.Elevation())
       || (altitude_max <= altitude_min)) {
        fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
                altitude_min, altitude_max,
                __FILE__, __LINE__, __func__);
        *electron_integral = -2.0E300;
        *electron_error = 0.0;
        return;
    }
#endif
    // check that the direction is above the horizon
    if(station.dot_product(direction) <= 0.0) {
        // direction below horizon.  Return an error, but no warning,
        // as this is not too severe
        *electron_integral = *Faraday_integral = -1.0E300;
        return;
    }

    // this looks like a valid sky position to get a
    // TEC value.  Call the GPS stuff
    Real64 MJD = time.JD1_C();
    Station_Reference telescope(station);

    GPS_collection_return gps_integral = obs_data->get_GPS_prediction(
        telescope,
        direction,
        MJD,
        true
        );
    if(gps_integral.STEC == GPS_NOT_INITIALIZED_VALUE) {
        fprintf(stderr, "Error: programmer did not fully initialize GPS data\n");
        exit(1);
    }
    *electron_integral = gps_integral.STEC;
    *electron_error = gps_integral.sigma_STEC;
    *Faraday_integral = gps_integral.SRM;
    if(gps_integral.STEC == GPS_BAD_DATA_VALUE) return;

    // If we do not have a real Faraday rotation, fake one
    if(gps_integral.SRM == GPS_BAD_DATA_VALUE) {
        fprintf(stderr, "Warning: gps_integral.SRM == GPS_BAD_DATA_VALUE\n");
        // Get the magnetic field at the default ionosphere height
        LatLon_Cart iono_pos =
            station.get_pierce_location(direction,
                                        obs_data->get_default_iono_height(),
                                        radius_Earth);
        // Get the IGRF magnetic field at this position and time, in T
        Space_Vector mag_field = Magnetic_Field(iono_pos, time);
        *Faraday_integral = get_Faraday_Measure(gps_integral.STEC,
                                                mag_field,
                                                -direction
                                                );
    }
    // GPS_integral.sigma_STEC gives STEC error
    // fprintf(stderr, "ionosphere_gps got iono error  %10.1E \n", gps_integral.sigma_STEC);



    return;
}

    
    



}  // end namespace


