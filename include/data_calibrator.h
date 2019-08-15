// data_calibrator.h
// Declare a class for data calibration (AIPS, aips++, etc.) interaction.
// This is intended as an interface point to Python code, so only
// simple C data types are used here, and most of the underlying C++
// class and code system is hidden.
//_HIST  DATE NAME PLACE INFO
//	2006 Jan 02  James M Anderson  --JIVE  start



#ifndef DATA_CALIBRATOR_H
#define DATA_CALIBRATOR_H

// INCLUDES
#include <stdint.h>






// No namespace, to put this in the main namespace, to simplify
// access by other languages such as Python.


// Now, I am going to need access to some other classes.  But to keep
// the access information to other languages simple, I don't want to
// include huge chunks of code.  So just declare the classes as existing, and
// use pointers, so that the data_calibrator.cxx file contains all
// of the gory details.

namespace MIM_PIM {
// Need an ionosphere
class Ionosphere_Base;
// need a station/telescope
class Station_Reference;
// need a source
class LatLon_Cart;
// need info about GPS stuff
class GPS_collection;
}
namespace JMA_VEX_AREA {
// need something to hold the time
class VEX_Time;
class VEX_UT1_Interp;
}
using namespace MIM_PIM;
using namespace JMA_VEX_AREA;


//_CLASS  Data_Calibrator --interface to calibration for outside world
class Data_Calibrator {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:
    // Constructor.
    Data_Calibrator(void);
    // Destructor
    ~Data_Calibrator(void);


    // This function will free up all memory, and return the object to
    // its freshly constructed state.  With some ionosphere models, this can
    // free vast amounts of memory.
    void clear_everything(void);


//_TITLE  set_ionosphere_IRI --use an IRI ionosphere
    int set_ionosphere_IRI(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        double electron_precision_in,// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-6.  For 1 GHz, 1E-5 should be fine.
        double Faraday_precision_in// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-4.  For 1 GHz, 1E-3 should be fine.
        );
// int Data_Calibrator::set_ionosphere_IRI
//                                O  return code.
//                                   0 all ok.


//_TITLE  set_ionosphere_PIM --use a PIM ionosphere
    int set_ionosphere_PIM(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        double electron_precision_in,// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-6.  For 1 GHz, 1E-5 should be fine.
        double Faraday_precision_in// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-4.  For 1 GHz, 1E-3 should be fine.
        );
// int Data_Calibrator::set_ionosphere_PIM
//                                O  return code.
//                                   0 all ok.


//_TITLE  set_ionosphere_GPS --use a GPS ionosphere
//	Note that you must have set up a GPS_collection beforehand
    int set_ionosphere_GPS(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        double electron_precision_in,// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-6.  For 1 GHz, 1E-5 should be fine.
        double Faraday_precision_in// I  relative precision.  The code
                               //    often fails to get within about a factor of 
                               //    10 of the requested value.  For 50 MHz, use
                               //    1E-4.  For 1 GHz, 1E-3 should be fine.
        );
// int Data_Calibrator::set_ionosphere_GPS
//                                O  return code.
//                                   0 all ok.
//                                   -1 unable to get memory
//                                   -2 No valid GPS_collection ready



    
//_TITLE  set_reference_time --set up the UTC reference time
    int set_reference_time(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int year,                  // I  YYYY
    int month,                 // I  MM in range 1--12
    int day,                   // I  DD in range 1--31
    int hour,                  // I  HH in range 0--23
    int minute,                // I  MM in range 0--59
    double second              // I  SS.ss in range 0--61 (for leap seconds)
    );
// int Data_Calibrator::set_reference_time
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory

//_TITLE  set_reference_time_AIPS --set up the UTC reference time under AIPS
    int set_reference_time_AIPS(
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
    );
// int Data_Calibrator::set_reference_time
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory
    
//_TITLE  set_station_position --set the station location
    int set_station_position(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double x,                  // I station position from center of Earth
                               //   in x direction in meters
    double y,                  // I station position from center of Earth
                               //   in y direction in meters
    double z                   // I station position from center of Earth
                               //   in z direction in meters
    );
// int Data_Calibrator::set_station_position
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory


//_TITLE  set_source_position --set the source position
    int set_source_position(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double RA,                 // I  J2000 RA position in radians
    double Dec                 // I  J2000 Dec position in radians
    );
// int Data_Calibrator::set_station_position
//                                I  return code
//                                   0 all ok
//                                   -1 unable to get memory


//_TITLE  set_scan_times --set the scan start and end times for ionospheric calcs
    int set_scan_times(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double start_time,         // I  the start of the scan, in seconds from the
                               //    reference date
    double end_time            // I  the end of the scan, in seconds from the
                               //    reference date
    );
// int Data_Calibrator::set_scan_times
//                                O  return code
//                                   0 all ok
//                                   -1 no memory
//                                   -2 no ionosphre
//                                   -3 no station
//                                   -4 no source
//                                   -5 no reference time
//                                   -6 invalid function parameter values

    
    int get_Num_Ionospheric_Predictions(void);


//_TITLE  get_ionospheric_prediction --get a prediction for STEC and SRM
    int get_ionospheric_prediction(
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
    double& VTEC_factor        // O  The approximate conversion factor to get
                               //    Vertical TEC from Slant TEC.  unitless.
                               //    Multiply STEC by VTEC_factor to get VTEC
    );
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



//_TITLE  get_source_AzEl --get the Elevation and Azimuth of a source at a time
    int get_source_AzEl(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double pred_time,          // I  The time difference, in seconds, of this
                               //    prediction, from the reference time
    double& El,                // O  The Elevation angle, in radians, of the
                               //    source at the pred_time
    double& Az                 // O  The Azimuth angle, in radians, of the
                               //    source at the pred_time
    );
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

//_TITLE  get_source_AzElVTEC --get the El and Az and VTEC factor for the source
    int get_source_AzElVTEC(
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
    );
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






//_TITLE  cal_observations_init --init the cal obs area
int cal_observations_init(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    unsigned int Num_Cal_Stations
                               // I  The number of stations expected to
                               //    be used.  Making this number larger
                               //    than necessary does not eat up too much
                               //    space (only memory for a pointer array)
    );
//	int Data_Calibrator::cal_observations_init
//	                          O  the return code
//	                              0 all ok
//                                   -1 memory failure

//_TITLE  cal_observations_init --correct GPS observations for bias levels
int cal_observations_init2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    void
    );
//	int Data_Calibrator::cal_observations_init2
//	                          O  the return code
//	                              0 all ok
//                                   -1 failure
//                                   -2 cal_data object invalid

//_TITLE  cal_observations_set_parameters
int cal_observations_set_parameters(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int Default_Fit_Type_,             // I  The fit type to calculate.
                                       //    0 Single nearest calibrator
                                       //    1 singel calibration station average
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
    unsigned int Bias_Fit_Type_//I  How to perform the initial bias correction.
                               //    0 Use main fit type
                               //    1 Use main fitting, but no theorectical mod
                               //    2 Use global fitting (sherical 3-D)
                               //    3 Use global, but without theoretical model
                               //    Note that if you have theoretical models 
                               //    turned on, global fitting may take hours
                               //    or days on a normal conputer.
    );
//	int Data_Calibrator::cal_observations_init
//	                          O  the return code
//	                              0 all ok
//                                   -1 object failure

//_TITLE  cal_observations_set_times --set time information
int cal_observations_set_times(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    unsigned int N_TIMES,      // I  number of datapoints
    double* data               // I  the times, as Modified Julian Dates
    );
// int Data_Calibrator::cal_observations_set_times
//                                O  The return code
//                                   0 all ok
//                                   else, a failure

//_TITLE  cal_observations_set_sat_pos --set satellite position information
int cal_observations_set_sat_pos(
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
    );
// int Data_Calibrator::cal_observations_set_sat_pos
//                                O  The return code
//                                   0 all ok
//                                   else, a failure

//_TITLE  cal_observations_set_cal_obs --set calibrator measurements
int cal_observations_set_cal_obs(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const char name[],         // I  4 character (or smaller) name of station
    double position[],         // I  Cartesian coordinates in Earth-fixed frame
                               //    of the position of the station, in m
                               //    as position[3], with x=0, y=1, z=2
    unsigned int N_TIMES,      // I  number of datapoints
    unsigned int N_SAT,        // I  number of satellites
    int16_t* sat_data,         // I  station number value as
                               //    sat_data[N_TIMES][N_SAT]
    int16_t* track_data,       // I  satellite track values (per sat) as
                               //    track_data[N_TIMES][N_SAT]
    float* stec_data,          // I  slant TEC value, in m^{-2}
                               //    Note that this is not in TECU!
                               //    Comes as stec_data[N_TIMES][N_SAT]
    float* sigma_data,         // I  the uncertainty in each measurement,
                               //    in m^{-2]
                               //    Note that this is not in TECU!
                               //    Comes as stec_data[N_TIMES][N_SAT]
    int32_t* bias_corrected    // I  Array of flags indicating whether station
                               //    bias correction was applied for each
                               //    satellite type
    );
// int Data_Calibrator::cal_observations_set_cal_obs
//                                O  The return code
//                                   0 all ok
//                                   else, a failure






    

    
    

protected:



private:
    // Need an ionosphere
    MIM_PIM::Ionosphere_Base* ionosphere;
    // need a station/telescope
    MIM_PIM::Station_Reference* station;
    MIM_PIM::Station_Reference* raw_station;
    // need a source
    MIM_PIM::LatLon_Cart* source;
    // need something to hold GPS observations
    MIM_PIM::GPS_collection* cal_data;
    // need something to hold the time
    JMA_VEX_AREA::VEX_Time* reference_time;
    // and the UT1 interpolator
    JMA_VEX_AREA::VEX_UT1_Interp* UT1_interpolator;

    // How many ionospheric predictions are available?
    int Num_Ionospheric_Predictions;
    // Keep track of the ionospheric prediction starting time
    // and \Delta time from the reference time, in seconds
    double Ionospheric_Prediction_Time_Start;
    double Ionospheric_Prediction_Delta_Time;


    // Need a flag to indicate whether to use the raw or elliptical-based
    // coordinates for the station
    int Use_Elliptical_Station_Coords;

    

    // prevent copy and assigment of of an object of this class
    // by placing these guys here.  It would be too complicated to
    // allow multiple instances of the ionospheres and times, etc.
    Data_Calibrator( const Data_Calibrator& );
    Data_Calibrator& operator=( const Data_Calibrator& );

    void free_ionosphere(void);
    void free_cal_data(void);
    void free_stations(void);
    void free_source(void);
    void free_reference_time(void);
    void free_UT1_interpolator(void);

    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS





#endif // DATA_CALIBRATOR_H
