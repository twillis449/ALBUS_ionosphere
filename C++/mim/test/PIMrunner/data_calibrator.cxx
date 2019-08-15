// data_calibrator.cxx
// Code area for the interface code to other software packages (e.g. python)
// to calibrate real data such as in AIPS or aips++
//_HIST  DATE NAME PLACE INFO
//	2006 Jan 02  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include "station_latlon.h"
#include "station_reference.h"
#include "station_maker.h"
#include "iono_runner.h"
#include "ionosphere_gen.h"
#include "sofa.h"
#include "vex_time.h"
#include "data_calibrator.h"
#include "sofa_helpers.h"
#include "ellipsoidal_coord.h"
#include "VTEC.h"
#include "GPS.h"
#include "ionosphere_gps.h"





// No namespace.  This is in the standard area to simplify access by other
// software types (e.g. python).  But use other namespaces.

using namespace MIM_PIM;
using namespace JMA_VEX_AREA;







// GLOBALS


// FUNCTIONS

// Here are some simple functions to aid with object holding
inline void Data_Calibrator::free_ionosphere(void)
{
    if(ionosphere) delete ionosphere;
    ionosphere = NULL;
    return;
}
inline void Data_Calibrator::free_cal_data(void)
{
    if(cal_data) delete cal_data;
    cal_data = NULL;
    return;
}
inline void Data_Calibrator::free_stations(void)
{
    if(station) delete station;
    station = NULL;
    if(raw_station) delete raw_station;
    raw_station = NULL;
    return;
}
inline void Data_Calibrator::free_source(void)
{
    if(source) delete source;
    source = NULL;
}
inline void Data_Calibrator::free_reference_time(void)
{
    if(reference_time) delete reference_time;
    reference_time = NULL;
    return;
}
inline void Data_Calibrator::free_UT1_interpolator(void)
{
    if(UT1_interpolator) delete UT1_interpolator;
    UT1_interpolator = NULL;
    return;
}




// Here is the constructor.  Make sure the object pointers are NULL
Data_Calibrator::Data_Calibrator(void)
        : ionosphere(NULL), station(NULL), raw_station(NULL), source(NULL),
          cal_data(NULL),
          reference_time(NULL), UT1_interpolator(NULL),
          Num_Ionospheric_Predictions(0), Ionospheric_Prediction_Delta_Time(1.0),
          Use_Elliptical_Station_Coords(0)
{
    return;
}

// And here is the destructor.  Free any used objects
Data_Calibrator::~Data_Calibrator(void)
{
    clear_everything();
    return;
}

void Data_Calibrator::clear_everything(void)
{
    free_ionosphere();
    free_cal_data();
    free_stations();
    free_source();
    free_reference_time();
    free_UT1_interpolator();
    return;
}    




// Ok, make an IRI ionosphere
//_TITLE  set_ionosphere_IRI --use an IRI ionosphere
int Data_Calibrator::set_ionosphere_IRI(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double electron_precision_in,// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-6.  For 1 GHz, 1E-5 should be fine.
    double Faraday_precision_in// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-4.  For 1 GHz, 1E-3 should be fine.
    )
// int Data_Calibrator::set_ionosphere_IRI
//                                O  return code.
//                                   0 all ok.
//                                   -1 unable to get memory

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // Make predictions at default 300 second intervals, 
    // 5 sec is silly for smoothly varying IRI
    //    Ionospheric_Prediction_Delta_Time = 5.0;
    Ionospheric_Prediction_Delta_Time = 300.0;
    Use_Elliptical_Station_Coords = 1;
    free_ionosphere();
    ionosphere = new Ionosphere_IRI(electron_precision_in,
                                    Faraday_precision_in);
    if((ionosphere)) return 0;
    // a memory error has happened.  The caller probably ought to bail.
    return -1;
}




//_TITLE  set_ionosphere_PIM --use an PIM ionosphere
int Data_Calibrator::set_ionosphere_PIM(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double electron_precision_in,// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-6.  For 1 GHz, 1E-5 should be fine.
    double Faraday_precision_in// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-4.  For 1 GHz, 1E-3 should be fine.
    )
// int Data_Calibrator::set_ionosphere_PIM
//                                O  return code.
//                                   0 all ok.
//                                   -1 unable to get memory

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs
// 	The precisions are actually ignored by the FORTRAN code.

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // Make predictions at default 300 second intervals, 
    // 5 sec is silly for smoothly varying PIM
    //    Ionospheric_Prediction_Delta_Time = 5.0;
    Ionospheric_Prediction_Delta_Time = 300.0;
    Use_Elliptical_Station_Coords = 1;
    free_ionosphere();
    ionosphere = new Ionosphere_PIM(electron_precision_in,
                                    Faraday_precision_in);
    if((ionosphere)) return 0;
    // a memory error has happened.  The caller probably ought to bail.
    return -1;
}







//_TITLE  set_ionosphere_GPS --use a GPS ionosphere
//	Note that you must have set up a GPS_collection beforehand
int Data_Calibrator::set_ionosphere_GPS(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        double electron_precision_in,// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-6.  For 1 GHz, 1E-5 should be fine.
        double Faraday_precision_in// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-4.  For 1 GHz, 1E-3 should be fine.
        )
// int Data_Calibrator::set_ionosphere_GPS
//                                O  return code.
//                                   0 all ok.
//                                   -1 unable to get memory
//                                   -2 No valid GPS_collection ready

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations
//	The GPS_collection cal_data area must have already been set up

//_BUGS  known bugs
// 	The precisions are actually ignored by the FORTRAN code.

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start

//_END

{
    // Make default predictions at 300 second intervals
    Ionospheric_Prediction_Delta_Time = 300.0;

    // The GPS stuff JMA has written uses proper Cartesian coordinates for
    // everything.
    Use_Elliptical_Station_Coords = 0;
    // AGW trying the following
    Use_Elliptical_Station_Coords = 1;
    free_ionosphere();
    ionosphere = new Ionosphere_GPS(cal_data,
                                    electron_precision_in,
                                    Faraday_precision_in);
    if((ionosphere)) {
        // we no longer own the GPS observations
        cal_data = NULL;
        return 0;
    }
    // a memory error has happened.  The caller probably ought to bail.
    return -1;
}


//_TITLE  set_reference_time --set up the UTC reference time
int Data_Calibrator::set_reference_time(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int year,                  // I  YYYY
    int month,                 // I  MM in range 1--12
    int day,                   // I  DD in range 1--31
    int hour,                  // I  HH in range 0--23
    int minute,                // I  MM in range 0--59
    double second              // I  SS.ss in range 0--61 (for leap seconds)
    )
// int Data_Calibrator::set_reference_time
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
// 	Note that the time must be UTC, not UT! or IAT, etc.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // Free the previous time information
    free_reference_time();
    free_UT1_interpolator();
    // And get a new one.
    reference_time = new VEX_Time(year, month, day, hour, minute, second);
    if((reference_time)) return 0;
    // uh-oh.  The caller needs to deal with this.
    return -1;
}



//_TITLE  set_reference_time_AIPS --set up the UTC reference time under AIPS
int Data_Calibrator::set_reference_time_AIPS(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int year,                  // I  YYYY
    int month,                 // I  MM in range 1--12
    int day,                   // I  DD in range 1--31
    int hour,                  // I  HH in range 0--23
    int minute,                // I  MM in range 0--59
    double second,             // I  SS.ss in range 0--61 (for leap seconds)
    double polar_x,            // I  the polar X position in meters on the
                               //    reference date.
    double polar_y,            // I  the polar Y position in meters on the
                               //    reference date.
    double ut1_utc             // I  the UT1 - UTC time difference in s on the
                               //    reference date.
    )
// int Data_Calibrator::set_reference_time
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
// 	Note that the time must be UTC, not UT! or IAT, etc.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // Free the previous time information
    free_reference_time();
    free_UT1_interpolator();
    // Ok, set up a base VEX_Time so that we can get the MJD
    VEX_Time temp_time(year, month, day, hour, minute, second);
    // Now build the UT1 interpolator from this
    UT1_interpolator = new VEX_UT1_Interp(1);
    if(UT1_interpolator == NULL) return -1; // no memory
    UT1_interpolator->set_MJD(0, temp_time.JD1_C());
    UT1_interpolator->set_X_Polar_Motion(0, polar_x);
    UT1_interpolator->set_Y_Polar_Motion(0, polar_y);
    UT1_interpolator->set_UT1_UTC(0, ut1_utc);
    // And get a new one.
    reference_time = new VEX_Time(year, month, day, hour, minute, second,
                                  UT1_interpolator);
    if((reference_time)) return 0;
    // uh-oh.  The caller needs to deal with this.
    return -1;
}









//_TITLE  set_station_position --set the station location
int Data_Calibrator::set_station_position(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double x,                  // I station position from center of Earth
                               //   in x direction in meters
    double y,                  // I station position from center of Earth
                               //   in y direction in meters
    double z                   // I station position from center of Earth
                               //   in z direction in meters
    )
// int Data_Calibrator::set_station_position
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory
//                                   -7 station too heigh up, can't do satellites


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations
//       Can't do satellites, so this gives an error for stations more than
//       20 km up.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // free the last stations
    free_stations();
    // Now, the coordinates are Cartesian, but the Ionospheric procesing stuff
    // probably wants to use things assuming a spherical Earth model.  So
    // we need to have two station coordinate objects
    raw_station = new Station_Reference(x,y,z);
    if(raw_station == NULL) return -1;
    ellipsoidal_coord e(x,y,z);
    Real64 height = e.get_height();
    station = new Station_Reference(e.get_phi(), e.get_lambda(), height,
                                    radius_Earth);
    if(station == NULL) return -1;
    if(height > 20E3) return -7;
    return 0;
}



//_TITLE  set_source_position --set the source position
int Data_Calibrator::set_source_position(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double RA,                 // I  J2000 RA position in radians
    double Dec                 // I  J2000 Dec position in radians
    )
// int Data_Calibrator::set_source_position
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // free the last source
    free_source();
    source = new LatLon_Cart(Dec, RA);
    if((source)) return 0;
    // No memory if we are here, tell the caller to deal with it.
    return -1;
}

//_TITLE  set_time_step --set the time step for prediction

int Data_Calibrator::set_time_step(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double delta_time   // I  time increment for predicting, in seconds
    )
// int Data_Calibrator::set_time_step
//                                O  return code
// Use this function to over-ride the default time increment of 30 seconds

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    Ionospheric_Prediction_Delta_Time = delta_time;
    return 0;
}

//_TITLE  set_scan_times --set the scan start and end times for ionospheric calcs
int Data_Calibrator::set_scan_times(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double start_time,         // I  the start of the scan, in seconds from the
                               //    reference date
    double end_time            // I  the end of the scan, in seconds from the
                               //    reference date
    )
// int Data_Calibrator::set_scan_times
//                                O  return code
//                                   0 all ok
//                                   -1 no memory
//                                   -2 no ionosphre
//                                   -3 no station
//                                   -4 no source
//                                   -5 no reference time
//                                   -6 invalid function parameter values


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    // How many points are there?  Make sure that we start on a good 300 second
    // boundary.  Go down by at least 15 seconds to make sure we get data before
    // the scan.
//   static const Real64 grid_time = 30.0;
//   static const Real64 grid_time = 300.0;
    static const Real64 grid_time = Ionospheric_Prediction_Delta_Time;
    Ionospheric_Prediction_Time_Start = start_time - grid_time*0.5;
    if(reference_time == NULL) return -5;
    { // correct for reference_time offsets
        Real64 ref_seconds = reference_time->Sec();
        Real64 off = fmod(ref_seconds, grid_time*0.5);
        Ionospheric_Prediction_Time_Start -= off;
    }
    Real64 rem = fmod(Ionospheric_Prediction_Time_Start, grid_time);
    if(rem < 0.0) rem += grid_time;
    Ionospheric_Prediction_Time_Start -= rem;
    // Now, how many points fit into the interval?
    Num_Ionospheric_Predictions =
        int( ceil( (end_time + grid_time*0.5 - Ionospheric_Prediction_Time_Start)
                   / Ionospheric_Prediction_Delta_Time) )
        + 1;
    if(Num_Ionospheric_Predictions < 0) {
        Num_Ionospheric_Predictions = 0;
        return -6;
    }
    return 0;
}



int Data_Calibrator::get_Num_Ionospheric_Predictions(void)
{
    return Num_Ionospheric_Predictions;
}





//_TITLE  get_ionospheric_prediction --get a prediction for STEC and SRM
int Data_Calibrator::get_ionospheric_prediction(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int Prediction_Number,     // I  The sequence number of the predictions.
                               //    Must be in the range
                               //    0--(Num_Ionospheric_Predictions-1)
    double& pred_time,         // O  The time difference, in seconds, of this
                               //    prediction, from the reference time
    double& width_time,        // O  The full width over which the prediction
                               //    is intended.  For now, this is just
                               //    Ionospheric_Prediction_Delta_Time
    double& El,                // O  The Elevation angle, in radians, of the
                               //    source at the pred_time
    double& Az,                // O  The Azimuth angle, in radians, of the
                               //    source at the pred_time
    double& STEC,              // O  the prediction STEC value, in units of
                               //    m^{-2}
    double& SRM,               // O  The predicted SRM value, in units of
                               //    T m^{-2}
    double& VTEC_factor,       // O  The approximate conversion factor to get
                               //    Vertical TEC from Slant TEC.  unitless.
                               //    Multiply STEC by VTEC_factor to get VTEC.
                               //    h_iono is assumed 300E3 m.
    double& STEC_ERR           // O  error in STEC, in units of m^{-2}
    )
// int Data_Calibrator::get_ionospheric_prediction
//                                O  return code
//                                   0 all ok
//                                   -1 no memory
//                                   -2 no ionosphre
//                                   -3 no station
//                                   -4 no source
//                                   -5 no reference time
//                                   -6 invalid function parameter values
//
//                                   +1 source below horizon, values set to 0.0



//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
// 	This is the main calculation part of the Data_Calibrator class.
//	Using the ionosphere model, the station position, the target source,
//	and the time of the observation, this function asks the ionospheric
//	object to calculate the slant total electron content (STEC) and the
//	slant rotation measure (SRM), in their natural SI units.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
//    printf(" \n");
//    printf("In get_ionospheric prediction\n");
    // Set some sane output values
    pred_time = El = Az = STEC = SRM = 0.0;
    VTEC_factor = 1.0;
    width_time = Ionospheric_Prediction_Delta_Time;
    // check that we have everything
    if(ionosphere == NULL) return -2;
    if((station == NULL) || (raw_station == NULL)) return -3;
    if(source == NULL) return -4;
    if(reference_time == NULL) return -5;
    // UT1_interpolator is allowed to be NULL
    // Check that this is a valid prediction number
    if((Prediction_Number < 0)
       || (Prediction_Number > Num_Ionospheric_Predictions)) return -6;
    // Ok, I need the correct time.
    pred_time = Ionospheric_Prediction_Time_Start
        + Ionospheric_Prediction_Delta_Time * Prediction_Number;
    VEX_Time now_time = *reference_time;
    now_time.add_offset(pred_time, UT1_interpolator);

    // Now, correct the source position
    Space_Rotation_Matrix precession = get_position_of_date(
        now_time.JD0_C(),
        now_time.JD1_C() + (Delta_AT(now_time) + 32.184)/SECONDS_PER_DAY
        );

    Space_Rotation_Matrix
        lst_rotation(GST(now_time), z_axis);
    
    // Ok, get the source.  Rotate it through precession and
    // for the LST
    Space_Vector now_source =
        (*source) * precession * lst_rotation;

    Station_Reference* use_station = ((Use_Elliptical_Station_Coords)) ?
        station : raw_station;

    LatLon_Cart AltAz =
        use_station->convert_RADec_to_AltAz(now_source);
    El   = AltAz.Lat();
    Az   = AltAz.Lon();
      
    LatLon_Cart x(now_source);
//     printf("Time %.3f Station at Lat %.3f Lon %.3f Source at %.3f %.3f\n",
//            now_time.Day_Fraction(),
//            use_station->Lat()*M_RAD2DEG, use_station->Lon()*M_RAD2DEG,
//            x.Lat()*M_RAD2DEG, x.Lon()*M_RAD2DEG);
//     printf("Raw station is %.3f %.3f\n",
//            raw_station->Lat()*M_RAD2DEG, raw_station->Lon()*M_RAD2DEG);


    //printf("%u Az %E El %E\n", seconds, Az*M_RAD2DEG, El*M_RAD2DEG);

    // I need a unit direction vector for the direction.
    Space_Unit_Vector direction = now_source.make_unit_vector();


    // check that the elevation is > 0, for above the horizon.
    if(now_source.dot_product(*use_station) > 0.0) {
        // this looks like a valid sky position to get a
        // TEC value.  Call the ionosphere stuff
        Real64 STECR;
        Real64 SFRR;
        Real64 STECE;
//      printf("calling ionosphere Integrate_Electron_Faraday\n");
        ionosphere->Integrate_Electron_Faraday(
            *use_station,
            now_time,
            direction,
            &STECR,
            &STECE,
            &SFRR 
            );
        STEC = STECR;
        SRM = SFRR;
        STEC_ERR = STECE;
        // Can now compute the VTEC scaling fator
        VTEC_factor = get_simple_VTEC_scaling(El, (*use_station).Elevation(),
                                              300E3);
        return 0;
    }
    return +1;
}











//_TITLE  get_source_AzEl --get the Elevation and Azimuth of a source at a time
int Data_Calibrator::get_source_AzEl(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double pred_time,          // I  The time difference, in seconds, of this
                               //    prediction, from the reference time
    double& El,                // O  The Elevation angle, in radians, of the
                               //    source at the pred_time
    double& Az                 // O  The Azimuth angle, in radians, of the
                               //    source at the pred_time
    )
// int Data_Calibrator::get_ionospheric_prediction
//                                O  return code
//                                   0 all ok
//                                   -1 no memory
//                                   -2 no ionosphre
//                                   -3 no station
//                                   -4 no source
//                                   -5 no reference time
//                                   -6 invalid function parameter values
//
//                                   +1 source below horizon



//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here
        
//_DESC  full description of program
// 	Taking the source and the requested time, this function gets the
//	Elevation and Azimuth angles for the current station.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
// 	2006 Jan 04  James M Anderson  --JIVE  start

//_END

{
    // Set some sane output values
    El = Az = 0.0;
    // check that we have everything
    if((station == NULL) || (raw_station == NULL)) return -3;
    if(source == NULL) return -4;
    if(reference_time == NULL) return -5;
    // UT1_interpolator is allowed to be NULL
    VEX_Time now_time = *reference_time;
    now_time.add_offset(pred_time, UT1_interpolator);

    // Now, correct the source position
    Space_Rotation_Matrix precession = get_position_of_date(
        now_time.JD0_C(),
        now_time.JD1_C() + (Delta_AT(now_time) + 32.184)/SECONDS_PER_DAY
        );

    Space_Rotation_Matrix
        lst_rotation(GST(now_time), z_axis);
    
    // Ok, get the source.  Rotate it through precession and
    // for the LST
    Space_Vector now_source =
        (*source) * precession * lst_rotation;

    Station_Reference* use_station = ((Use_Elliptical_Station_Coords)) ?
        station : raw_station;

    LatLon_Cart AltAz =
        use_station->convert_RADec_to_AltAz(now_source);
    El   = AltAz.Lat();
    Az   = AltAz.Lon();

    //printf("%u Az %E El %E\n", seconds, Az*M_RAD2DEG, El*M_RAD2DEG);

    // check that the elevation is > 0, for above the horizon.
    if(now_source.dot_product(*use_station) > 0.0) {
        return 0;
    }
    return +1;
}

















//_TITLE  get_source_AzElVTEC --get the El and Az and VTEC factor for the source
int Data_Calibrator::get_source_AzElVTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double pred_time,          // I  The time difference, in seconds, of this
                               //    prediction, from the reference time
    double h_iono,             // I  The height of the ionosphere 2-D layer to
                               //    calculate the VTEC_factor, in m.
    double& El,                // O  The Elevation angle, in radians, of the
                               //    source at the pred_time
    double& Az,                // O  The Azimuth angle, in radians, of the
                               //    source at the pred_time
    double& VTEC_factor        // O  The approximate conversion factor to get
                               //    Vertical TEC from Slant TEC.  unitless.
                               //    Multiply STEC by VTEC_factor to get VTEC
    )
// int Data_Calibrator::get_ionospheric_prediction
//                                O  return code
//                                   0 all ok
//                                   -1 no memory
//                                   -2 no ionosphre
//                                   -3 no station
//                                   -4 no source
//                                   -5 no reference time
//                                   -6 invalid function parameter values
//
//                                   +1 source below horizon



//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here
        
//_DESC  full description of program
// 	Taking the source and the requested time, this function gets the
//	Elevation and Azimuth angles for the current station.

//	Also gets the VTEC scaling factor for the source, using the given
//	height of the ionosphere.  IF El < 0, VTEC_factor is set to 1.0

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
// 	2006 Jan 05  James M Anderson  --JIVE  start

//_END

{
    // Set some sane output values
    El = Az = 0.0;
    VTEC_factor = 1.0;
    // check that we have everything
    if((station == NULL) || (raw_station == NULL)) return -3;
    if(source == NULL) return -4;
    if(reference_time == NULL) return -5;
    // UT1_interpolator is allowed to be NULL
    VEX_Time now_time = *reference_time;
    now_time.add_offset(pred_time, UT1_interpolator);

    // Now, correct the source position
    Space_Rotation_Matrix precession = get_position_of_date(
        now_time.JD0_C(),
        now_time.JD1_C() + (Delta_AT(now_time) + 32.184)/SECONDS_PER_DAY
        );

    Space_Rotation_Matrix
        lst_rotation(GST(now_time), z_axis);
    
    // Ok, get the source.  Rotate it through precession and
    // for the LST
    Space_Vector now_source =
        (*source) * precession * lst_rotation;

    Station_Reference* use_station = ((Use_Elliptical_Station_Coords)) ?
        station : raw_station;

    LatLon_Cart AltAz =
        use_station->convert_RADec_to_AltAz(now_source);
    El   = AltAz.Lat();
    Az   = AltAz.Lon();

    //printf("%u Az %E El %E\n", seconds, Az*M_RAD2DEG, El*M_RAD2DEG);

    // check that the elevation is > 0, for above the horizon.
    if(now_source.dot_product(*use_station) > 0.0) {
        // Can now compute the VTEC scaling fator
        VTEC_factor = get_simple_VTEC_scaling(El, (*use_station).Elevation(),
                                              h_iono);
        return 0;
    }
    return +1;
}











//_TITLE  cal_observations_init --init the cal obs area
int Data_Calibrator::cal_observations_init(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    unsigned int Num_Cal_Stations
                               // I  The number of stations expected to
                               //    be used.  Making this number larger
                               //    than necessary does not eat up too much
                               //    space (only memory for a pointer array)
    )
//	int Data_Calibrator::cal_observations_init
//	                          O  the return code
//	                              0 all ok
//                                   -1 memory failure


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	Creates a new GPS_collection with default values

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start

//_END

{
    free_cal_data();
    cal_data = new GPS_collection(Num_Cal_Stations);
    if((cal_data)) return 0;
    return -1;
}



//_TITLE  cal_observations_init2 --correct GPS observations for bias levels
int Data_Calibrator::cal_observations_init2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    void
    )
//	int Data_Calibrator::cal_observations_init2
//	                          O  the return code
//	                              0 all ok
//                                   -1 failure
//                                   -2 cal_data object invalid

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	Calibrate data for bias levels

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 12  James M Anderson  --JIVE  start

//_END

{
    if(cal_data == NULL) return -2;
    double result = cal_data->correct_receiver_biases();
    if(result < 0.0) return -1;
    return 0;
}


//_TITLE  cal_observations_set_parameters
int Data_Calibrator::cal_observations_set_parameters(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int Default_Fit_Type_,             // I  The fit type to calculate.
                                       //    0 Single nearest calibrator
                                       //    1 single calibration station average
                                       //    2 2-D MIM
                                       //    3 3-D MIM multiple station
                                       //    4 3-D MIM many station
                                       //    5 3-D spherical harmonics
                                       //    6 2-D with time dependence
                                       //    7 2-D with gradient LSQ
                                       //    8 2-D with time, gradient LSQ
                                       //    9 3-D spherical with gradient LSQ
    float Max_Sat_Sky_Angle_,          // I  The maximum angle between the
                                       //    calibrator direction and the
                                       //    target direction, in radians
    float Min_Sat_Elev_,               // I  Minimum calibrator elevation,
                                       //    in radians.
    float Max_Rec_Dist_From_Tele_,     // I  Maximum distance of calibrator
                                       //    position from telescope, in m
    float Max_Iono_Pierce_Dist_,       // I  Maximum distance of the calibrator
                                       //    pierce point from the target
                                       //    pierce point, in m
    float Default_Iono_Height_,        // I  The default height of a 2-D
                                       //    ionosphere, in m, used for various
                                       //    calculations.
    float Averaging_Time_Half_Width_,  // I  Half width, in s, of the window
                                       //    over which to use calibrator
                                       //    measurements to determine the
                                       //    ionosphere.
    unsigned int Num_Ionosphere_Parameters_,
                                       // I  Number of parameters to try to fit
                                       //    in ionospheric models
    unsigned int Num_Ionosphere_Heights_,
                                       // I  Number of heights to use for 3-D
                                       //    models.
    unsigned int Num_Time_Terms_,
                               // I  The maximum order of time polynomials
                               //    to apply.  The ionosphere is made to have
                               //    a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                               //    dependence, where N \equiv NUM_TIME_TERMS-1
                               //    Thus, 1 means constant, 2 for linear, 3
                               //    for quadratic, and so on.
    unsigned int Theo_Model_Type_,
                               // I  which type of theoretical model to use
                               //    as part of the fitting.
                               //    0 None  -- do not use a theoretical model
                               //    1 IRI
                               //    2 IRI_Plus
                               //    3 PIM
                               //    4 Fake0
                               //
    unsigned int Bias_Fit_Type_// I  How to perform the initial bias correction.
                               //    0 Use main fit type
                               //    1 Use main fitting, but no theorectical mod
                               //    2 Use global fitting (sherical 3-D)
                               //    3 Use global, but without theoretical model
                               //    Note that if you have theoretical models 
                               //    turned on, global fitting may take hours
                               //    or days on a normal conputer.
    )
//	int Data_Calibrator::cal_observations_init
//	                          O  the return code
//	                              0 all ok
//                                   -1 object failure


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	Creates a new GPS_collection with default values

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start
//      2007 Mar 19  JMA  --add time term

//_END

{
    if((cal_data)) {
        cal_data->reset_selection_criteria(
            GPS_Fit_Type_Enum(Default_Fit_Type_),
            Max_Sat_Sky_Angle_,
            Min_Sat_Elev_,
            Max_Rec_Dist_From_Tele_,
            Max_Iono_Pierce_Dist_,
            Default_Iono_Height_,
            Averaging_Time_Half_Width_,
            Num_Ionosphere_Parameters_,
            Num_Ionosphere_Heights_,
            Num_Time_Terms_,
            Ionosphere_Theoretical_Model_Enum(Theo_Model_Type_),
            GPS_Bias_Fit_Type_Enum(Bias_Fit_Type_));
        
        return 0;
    }
    return -1;
}












//_TITLE  cal_observations_set_times --set time information
int Data_Calibrator::cal_observations_set_times(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    unsigned int N_TIMES,      // I  number of datapoints
    double* data               // I  the times, as Modified Julian Dates
    )
// int Data_Calibrator::cal_observations_set_times
//                                O  The return code
//                                   0 all ok
//                                   else, a failure

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will fill in the time data (an array of MJDs) for
//	the calibration information.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start

//_END

{
    if((cal_data)) {
        return cal_data->fill_time_data(N_TIMES, data);
    }
    return -1;        
}




//_TITLE  cal_observations_set_sat_pos --set satellite position information
int Data_Calibrator::cal_observations_set_sat_pos(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    unsigned int N_TIMES,      // I  number of datapoints
    unsigned int N_SAT,        // I  number of satellites
    unsigned int N_ELEMENTS,   // I  The number of doubles used to store
                               //    each satellite position.  3 for just
                               //    a Cartesian array, or 6 for Cartesian
                               //    plus spherical.
    double* data               // I  the satellite positions, as a big
                               //    multidimensional array
                               //    data[N_TIMES][N_SAT][N_ELEMENTS]
    )
// int Data_Calibrator::cal_observations_set_sat_pos
//                                O  The return code
//                                   0 all ok
//                                   else, a failure

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will fill in the satellite positions for the
//	the calibration information.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start

//_END

{
    if((cal_data)) {
        return cal_data->fill_satellite_pos(N_TIMES, N_SAT, N_ELEMENTS, data);
    }
    return -1;        
}




//_TITLE  cal_observations_set_cal_obs --set calibrator measurements
int Data_Calibrator::cal_observations_set_cal_obs(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const char name[],         // I  4 character (or smaller) name of station
    double position[],         // I  Cartesian coordinates in Earth-fixed frame
                               //    of the position of the station, in m
                               //    as position[3], with x=0, y=1, z=2
    unsigned int N_TIMES,      // I  number of datapoints
    unsigned int N_SAT,        // I  number of satellites
    int16_t* sat_data,         // I  satellite number value as
                               //    sat_data[N_TIMES][N_SAT]
    int16_t* track_data,       // I  satellite track values (per sat) as
                               //    track_data[N_TIMES][N_SAT]
    double* stec_data,          // I  slant TEC value, in m^{-2}
                               //    Note that this is not in TECU!
                               //    Comes as stec_data[N_TIMES][N_SAT]
    double* sigma_data,         // I  the uncertainty in each measurement,
                               //    in m^{-2]
                               //    Note that this is not in TECU!
                               //    Comes as stec_data[N_TIMES][N_SAT]
    int32_t* bias_corrected    // I  Array of flags indicating whether station
                               //    bias correction was applied for each
                               //    satellite type
    )
// int Data_Calibrator::cal_observations_set_cal_obs
//                                O  The return code
//                                   0 all ok
//                                   else, a failure

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will fill in the calibration observations for a single
//      station.  It will normally be called many times to fill in data from
//	different stations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 09  James M Anderson  --JIVE  start

//_END

{
    if((cal_data)) {
        Space_Vector pos(position);
        return cal_data->fill_receiver_data(name, pos,
                                            N_TIMES, N_SAT, sat_data, track_data,
                                            stec_data, sigma_data,
                                            bias_corrected);
    }
    return -1;        
}
