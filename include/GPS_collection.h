// GPS_collection.h
// Hold a group of GPS station observations for Ionospheric calibration use
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start
//	2006 Dec 28  JMA  --resume initial design
//	2007 Apr 19  JMA  --update for satellite track changes



#ifndef GPS_COLLECTION_H
#define GPS_COLLECTION_H

// INCLUDES
#include "JMA_code.h"
#include <stdio.h>
#include <stdlib.h>
#include "GPS.h"
#include "space_vector.h"
#include "latlon_cart.h"
#include "station_reference.h"
#include "observation.h"
#include "observation_3D.h"





// set up a namespace area for stuff.
namespace MIM_PIM {


struct GPS_collection_return {
public:
    GPS_collection_return(Real64 STEC_, Real64 sigma_STEC_,
                          Real64 SRM_, Real64 sigma_SRM_)
            : STEC(STEC_), sigma_STEC(sigma_STEC_),
              SRM(SRM_), sigma_SRM(sigma_SRM_) {return;}
    GPS_collection_return(Real64 error_code)
            : STEC(error_code), sigma_STEC(error_code),
              SRM(error_code), sigma_SRM(error_code)
        { if((STEC == GPS_BAD_DATA_VALUE)
             || (STEC == GPS_NOT_INITIALIZED_VALUE)) {
                // do nothing
            }
            else {
                fprintf(stderr, "Error: programmer misuse of error initilization of GPS_collection_return\n");
                exit(1);
            }
            return;
        }
    Real64 STEC;
    Real64 sigma_STEC;
    Real64 SRM;
    Real64 sigma_SRM;
};
    
            
        



//_CLASS  GPS_collection --holds a big group of station data
class GPS_collection {
//_DESC  full description of class

//	This class holds information  for analyzing the ionosphre from
//	multiple GPS (and GLONASS/Galileo) receivers.  It is intended to be
//	interfaced with JMA's Python code for reading in the GPS data, and
//	as such it takes in data one station at a time.

//	This class holds three different types of data.

//	1.  A time sequence.  The observation times of all receivers are
//	assumed to be the same.  So, one GPS_times object is held to contain
//	these times and to aid in finding the best interpolation indices.
//	These times are stored as Modified Julian Dates.

//	2.  An array of satellite positions.  The (X,Y,Z) Cartesian coordinates
//	in an Earth-fixed reference frame are stored for each time step.

//	3.  Station data.  An array of stations is set up.  For each station
//	in this array, a receiver object is created to hold the ionospheric data
//	for the satellites visible for each time step.  Each station requires
//	a significant amount of memory, so the stations are initialized one
//	at a time through the Python interface.

//	Note that the incoming receiver STEC values are expected to be in m^{-2}.

//	With these three object types, the class can arrange to make
//	fits of the ionosphere at given tiems for given positions on the Earth
//	to correct radio observations.  It can perform the ionospheric fits
//	in a variety of ways.  At this time, these ways include:

//	A.  Single receiver, nearest neighbor satellite

//	B.  Single receiver, average satellite ionosphere

//	C.  Multiple receiver, 2-D MIM

//	D.  Multiple receiver, 3-D MIM with fixed height structure

//	E.  Multiple receiver, 3-D MIM with free height structure


//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2006 Dec 28  James M Anderson  --JIVE  start

//_END


// NAMESPACE ISSUES    


public:
    GPS_collection(Uint32 Num_Receivers_,
                   GPS_Fit_Type_Enum Default_Fit_Type_ = multiple_3D_many,
                   Real32 Max_Sat_Sky_Angle_ = 2.0*M_PI,
                   Real32 Min_Sat_Elev_ = 0.1745,
                   Real32 Max_Rec_Dist_From_Tele_ = 300E3,
                   Real32 Max_Iono_Pierce_Dist_ = 1000E3,
                   Real32 Default_Iono_Height_ = 300E3,
                   Real32 Averaging_Time_Half_Width_ = 0.0,
                   Uint32 Num_Ionosphere_Parameters_ = 40,
                   Uint32 Num_Ionosphere_Heights_ = 5,
                   Uint32 Num_Time_Terms_ = 1,
                   Ionosphere_Theoretical_Model_Enum Theo_Model_Type_ = None,
                   GPS_Bias_Fit_Type_Enum Bias_Fit_Type_ =
                                                 Use_Main_Without_Theoretical
                   );
    ~GPS_collection(void);

    void reset_selection_criteria(
        GPS_Fit_Type_Enum Default_Fit_Type_ = multiple_3D_many,
        Real32 Max_Sat_Sky_Angle_ = 2.0*M_PI,
        Real32 Min_Sat_Elev_ = 0.1745,
        Real32 Max_Rec_Dist_From_Tele_ = 300E3,
        Real32 Max_Iono_Pierce_Dist_ = 1000E3,
        Real32 Default_Iono_Height_ = 300E3,
        Real32 Averaging_Time_Half_Width_ = 0.0,
        Uint32 Num_Ionosphere_Parameters_ = 40,
        Uint32 Num_Ionosphere_Heights_ = 5,
        Uint32 Num_Time_Terms_ = 1,
        Ionosphere_Theoretical_Model_Enum Theo_Model_Type_ = None,
        GPS_Bias_Fit_Type_Enum Bias_Fit_Type_ = Use_Main_Without_Theoretical
        ) throw();

    Sint32 fill_time_data(const Uint32 N_TIMES, const Real64* const data)
        {
            if((time_arr)) delete time_arr;
            time_arr = NULL;
            time_arr = new GPS_times(N_TIMES, data);
            return test_object_sizes();
        }
    Sint32 fill_satellite_pos(const Uint32 N_TIMES, const Uint32 N_SAT,
                            const Uint32 N_ELEMENTS,
                            const Real64* const s_data)
        {
            if((sat_pos)) delete sat_pos;
            sat_pos = NULL;
            sat_pos = new GPS_satellites(N_TIMES, N_SAT, N_ELEMENTS, s_data);
            return test_object_sizes();
        }
    Sint32 fill_receiver_data(const char name[],
                              const Space_Vector& position,
                              const Uint32 N_TIMES, const Uint32 N_SAT,
                              const Sint16* const sat_data,
                              const Sint16* const track_data,
                              const Real32* const stec_data,
                              const Real32* const sigma_data,
                              const Sint32* const bias_valid);


//_TITLE  get_GPS_prediction ---get a prediction for a specific direction
    GPS_collection_return get_GPS_prediction(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
        const Real64 MJD_obs,  // I  The time of the observation, as an MJD.
        const GPS_Fit_Type_Enum fit_type,
                               // I  Do a multi or a many-layer fit
        const bool use_IRI_data = false,
                               // I  Use or not an updated IRI ionosphere for the
                               //    3-D height scaling information
    const bool Fit_Station_Bias = false,//I  Fit or not the station biases
    const bool Force_Zero_Mean_Bias=false,// I  Should the bias level be forced to 0?
    const bool Hold_Valid_Station_Bias= false,//I  Hold or not receivers with valid bias
    const bool Fit_Object_Bias = false, // I  Fit or not the object(satellite) biases
    const GPS_receiver* const Central_Receiver_Only = NULL
                               // I  If this is not NULL, then allow this
                               //    and only this receiver to have its bias
                               //    level vary and be fit.
        );
// GPS_collection_return get_GPS_prediction
//                                O  The STEC value, in m^{-2}
//                                   A value of GPS_BAD_DATA_VALUE indicates
//                                   no valid data available.
//                                   A value of GPS_NOT_INITIALIZED_VALUE
//                                   indicates that the user has not filled in
//                                   all data (times, sat_pos, receiver data).
    
//_TITLE  get_GPS_prediction ---get a prediction for a specific direction
    GPS_collection_return get_GPS_prediction(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
        const Real64 MJD_obs,  // I  The time of the observation, as an MJD.
        const bool use_IRI_data = false
                               // I  Use or not an updated IRI ionosphere for the
                               //    3-D height scaling information
        )
// GPS_collection_return get_GPS_prediction
//                                O  The STEC value, in m^{-2}
//                                   A value of GPS_BAD_DATA_VALUE indicates
//                                   no valid data available.
//                                   A value of GPS_NOT_INITIALIZED_VALUE
//                                   indicates that the user has not filled in
//                                   all data (times, sat_pos, receiver data).
        {
            return get_GPS_prediction(telescope, target, MJD_obs,
                                   selection_criteria.Default_Fit_Type,
                                   use_IRI_data);
        }


    Real32 get_default_iono_height(void) const throw()
        {return selection_criteria.Default_Iono_Height;}


    Real64 correct_receiver_biases(void);
    Real64 correct_receiver_biases_global(void);

protected:



private:
    // Prevent copying!
    GPS_collection(const GPS_collection& g);
    GPS_collection& operator=(const GPS_collection& g);


    
    // Need to know how many receivers there are
    Uint32 Num_Receivers;
    Uint32 Next_Receiver_Fill;

    // Here is the main data
    GPS_times*         time_arr;    // holds the time data
    GPS_satellites*    sat_pos;     //           satellite positions
    GPS_receiver**     rec_list;    //           array of receiver objects
    GPS_receiver**     rec_templist;//           array of receiver objects


    GPS_criteria selection_criteria;

    // Holding areas for the satellite observations to fit.
    Uint32 Max_Number_Observations;
    Uint32 Max_Number_Workspace;
    Observation_Ionosphere* observations_2D;
    Observation_3D_Ionosphere* observations_3D;
    bool* use_flag;
    Real64* workspace;

    Uint32* satellite_workspace;
    Real64* satellite_bias_workspace;
    Sint32 Total_Bias_Correction_Done_Flag;

    Uint32* satellite_track_main_ID;
    Uint32* satellite_track_track_pos;
    Uint32* satellite_track_reciever_pos;
    Sint16* satellite_track_satellite_pos;
    Uint32 Satellite_Track_Max_Tracks;







    Sint32 test_object_sizes(void) const throw();

    void delete_observation_memory(const bool all = true);
    void reallocate_2D_observations(const Uint32 NUM_OBS,
                                    const Uint32 NUM_WORK);
    void reallocate_3D_observations(const Uint32 NUM_OBS,
                                    const Uint32 NUM_WORK);

    void reallocate_satellite_track_workspace(const Uint32 NUM_TRACKS);


    GPS_receiver* get_nearest_receiver(const LatLon_Cart& telescope) const;

    //_TITLE  get_nearby_receivers --get receivers close to some station
    void get_nearby_receivers(
        const LatLon_Cart& telescope,
                               // I  the telescope position to search around
        GPS_receiver**& close_rec_list,
                               // O  A pointer to a list of GPS receivers.
                               //    This function will allocate with new[] an
                               //    array in which the close receivers are
                               //    placed.  The caller is responsible for
                               //    delete[] ing this array.
        Uint32& num_close_receivers
                               // O  The total number of close receivers
                               //    which have been placed into
                               //    close_rec_list.
        ) const;

//_TITLE  get_global_receivers --get all global receivers in the collection
void get_global_receivers(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    GPS_receiver**& global_rec_list,
                               // O  A pointer to a list of GPS receivers.
                               //    The caller must not delete[] this array.
    Uint32& num_global_receivers
                               // O  The total number of global receivers
                               //    which have been placed into
                               //    global_rec_list.
    );

//_TITLE  test_satellite_criteria_fail --is the satellite outside limits
    int test_satellite_criteria_fail(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Vector& target_pierce,
                               // I  The pierce point of the target observation
                               //    from the telescope, in Earth-fixed
                               //    coordinates
        const Station_LatLon& receiver,
                               // I  The GPS receiver position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& satellite,
                               // I  The satellite direction from the
                               //    viewpoint of the receiver, in Earth-fixed
                               //    coordinates.
        const Real64 Separation_Angle,
                               // I  The angle between the target vector and
                               //    the satellite vector in radians.
        const Real64 Sat_Elevation_Angle
                               // I  The elevation angle of the satellite as
                               //    seen by the receiver, in radians.
        ) const;
// int GPS_collection::test_satellite_criteria_fail
//                                O  Flag to indicate whether the satellite
//                                   is within the boundary conditions.
//                                    0 no failure, satellite is good
//                                   -1 angular distance failure
//                                   -2 elevation angle failure
//                                   -3 intercept point distance failure
    


    Uint32 get_fake_sat_num(const Uint32 sat_ID,
                            Uint32& num_sats) throw();
    void clear_fake_sat_num() throw();
    void count_total_tracks(void);
    Uint32 get_fake_track_num(const Uint32 rec_ID,
                              const Sint16 sat_ID,
                              const Sint16 track_ID,
                              Uint32& num_tracks) throw();
    void clear_fake_track_num() throw();
    Uint32 get_fake_track_max(void) const throw();
    void get_real_track_info(const Uint32 fake_track_num,
                             Uint32& rec_ID,
                             Sint16& sat_ID,
                             Sint16& track_ID,
                             Uint32& real_track_num) const throw();




//_TITLE  get_single_receiver_nearest_STEC --
    GPS_collection_return get_single_receiver_nearest_STEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
        const Real64 MJD_obs   // I  The time of the observation, as an MJD.
        );

//_TITLE  get_single_receiver_average_STEC --
    GPS_collection_return get_single_receiver_average_STEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
        const Real64 MJD_obs   // I  The time of the observation, as an MJD.
        );

//_TITLE  get_multiple_receiver_2D_MIM_fit
    GPS_collection_return get_multiple_receiver_2D_MIM_fit(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
        const Real64 MJD_obs,  // I  The time of the observation, as an MJD.
        const GPS_Fit_Type_Enum fit_type,
                               // I  Do a simple or time dependence
    const bool Fit_Station_Bias = false,//I  Fit or not the station biases
    const bool Force_Zero_Mean_Bias=false,// I  Should the bias level be forced to 0?
    const bool Hold_Valid_Station_Bias= false,//I  Hold or not receivers with valid bias
    const bool Fit_Object_Bias = false,  // I  Fit or not the object(satellite) biases
    const GPS_receiver* const Central_Receiver_Only = NULL
                               // I  If this is not NULL, then allow this
                               //    and only this receiver to have its bias
                               //    level vary and be fit.
        );
// GPS_collection_return GPS_collection::get_multiple_receiver_2D_MIM_fit
//                                O  The STEC value, in m^{-2}
//                                   A value of GPS_BAD_DATA_VALUE indicates
//                                   no valid data available.

//_TITLE  get_multiple_receiver_3D_MIM_fit
    GPS_collection_return get_multiple_receiver_3D_MIM_fit(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
        const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
        const Real64 MJD_obs,  // I  The time of the observation, as an MJD.
        const GPS_Fit_Type_Enum fit_type,
                               // I  Do a multi or a many-layer fit
        const bool use_IRI_data,//I  Use or not an updated IRI ionosphere for the
                               //    3-D height scaling information
    const bool Fit_Station_Bias = false,//I  Fit or not the station biases
    const bool Force_Zero_Mean_Bias=false,// I  Should the bias level be forced to 0?
    const bool Hold_Valid_Station_Bias= false,//I  Hold or not receivers with valid bias
    const bool Fit_Object_Bias = false, // I  Fit or not the object(satellite) biases
    const GPS_receiver* const Central_Receiver_Only = NULL
                               // I  If this is not NULL, then allow this
                               //    and only this receiver to have its bias
                               //    level vary and be fit.
        );
// GPS_collection_return GPS_collection::get_multiple_receiver_3D_MIM_fit
//                                O  The STEC value, in m^{-2}
//                                   A value of GPS_BAD_DATA_VALUE indicates
//                                   no valid data available.

//_TITLE  get_multiple_receiver_3D_MIM_track_fit
    GPS_collection_return get_multiple_receiver_3D_MIM_track_fit(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const Station_Reference& telescope,
                               // I  the telescope position, in Earth-fixed
                               //    coordinates
    const Space_Unit_Vector& target,
                               // I  The observation direction from the
                               //    viewpoint of the telescope, in Earth-fixed
                               //    coordinates.
    const Real64 MJD_obs,      // I  The time of the observation, as an MJD.
    const GPS_Fit_Type_Enum fit_type,
                               // I  Do a multi or a many-layer fit
    const bool use_IRI_data,   // I  Use or not an updated IRI ionosphere for the
                               //    3-D height scaling information
    const bool Force_Zero_Mean_Bias,// I  Should the bias level be forced to 0?
    const bool Hold_Valid_Station_Bias,//I  Hold or not receivers with valid bias
    const bool Fit_Track_Bias,// I  Fit or not the track(satellite) biases
    const GPS_receiver* const Central_Receiver_Only
                               // I  If this is not NULL, then allow this
                               //    and only this receiver to have its bias
                               //    level vary and be fit.
    );
// GPS_collection_return GPS_collection::get_multiple_receiver_3D_MIM_track_fit
//                                O  The STEC value, in m^{-2}
//                                   A value of GPS_BAD_DATA_VALUE indicates
//                                   no valid data available.

    

//_TITLE  correct_receiver_biases_low --fix up the receiver bias levels
Real64 correct_receiver_biases_low(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const GPS_Fit_Type_Enum Fit_Type,//I The type of fit to perform
    const Uint32 NUM_PARAMETERS_IN,//I  number of polynomial parameters to allow
    const Real64 Averaging_Time,//I  Averaging time of the data in s
    const bool Apply_RMS_To_BIAS,//I Apply the RMS to correct bias level?
    const bool Force_Zero_Mean_Bias,// I  Should the bias level be forced to 0?
    const bool Hold_Valid_Station_Bias,//I  Hold or not receivers with valid bias
    const bool Release_Only_Central_Receiver
                                // I  Should this function only allow the central
                                //    receiver to have a varying bias level,
                                //    or shoud all receivers have variable bias
                                //    levels?
    );
// Real64 GPS_collection::correct_receiver_biases
//                                O  return value
//                                   normally this indicates the abs(max) bias
//	                             but if the system is not fully initialized
//                                   the value is GPS_NOT_INITIALIZED_VALUE


//_TITLE  correct_receiver_biases_force --force fixing the receiver bias levels 
Real64 correct_receiver_biases_force(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const GPS_Fit_Type_Enum Fit_Type,//I The type of fit to perform
    const Uint32 NUM_PARAMETERS_IN,  //I  number of polynomial parameters to allow
    const Real64 Averaging_Time,//I  Averaging time of the data in s
    const bool Apply_RMS_To_BIAS,//I Apply the RMS to correct bias level?
    const bool Force_Zero_Mean_Bias// I  Should the bias level be forced to 0?
    );
// Real64 GPS_collection::correct_receiver_biases_force
//                                O  return value
//                                   normally this indicates the abs(max) bias
//	                             but if the system is not fully initialized
//                                   the value is GPS_NOT_INITIALIZED_VALUE

//_TITLE  correct_receiver_biases_global_force --force global receiver biases
Real64 correct_receiver_biases_global_force(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const GPS_Fit_Type_Enum Fit_Type,//I The type of fit to perform
    Uint32 NUM_PARAMETERS,//I  number of polynomial parameters to allow
    const Real64 Averaging_Time,//I  Averaging time of the data in s
    const bool Apply_RMS_To_BIAS//I Apply the RMS to correct bias level?
    );
// Real64 GPS_collection::correct_receiver_biases_global_force
//                                O  return value
//                                   normally this indicates the abs(max) bias
//	                             but if the system is not fully initialized
//                                   the value is GPS_NOT_INITIALIZED_VALUE


//_TITLE  correct_receiver_biases_track_force --force track biases
Real64 correct_receiver_biases_track_force(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const GPS_Fit_Type_Enum Fit_Type,//I The type of fit to perform
    Uint32 NUM_PARAMETERS,      //I  number of polynomial parameters to allow
    const Real64 Averaging_Time,//I  Averaging time of the data in s
    const bool Apply_RMS_To_BIAS//I Apply the RMS to correct bias level?
    );
// Real64 GPS_collection::correct_receiver_biases_track_force
//                                O  return value
//                                   normally this indicates the abs(max) bias
//	                             but if the system is not fully initialized
//                                   the value is GPS_NOT_INITIALIZED_VALUE


    
//_TITLE  force_absolute_night_bias --force stations to have bias correction
Sint32 force_absolute_night_bias(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    GPS_receiver* receiver,    // B  The receiver to bias correct
    const Real64 Night_Bias_Level,
                               // I  The night-time vertical total electron
                               //    content to force to receiver to, in units
                               //    of electrons m^{-2}
    const Sint32 bias_status   // I  status of bias correction to apply
    );
// Sint32 GPS_collection::force_absolute_night_bias
//                                O  return value
//                                   normally this is 0
//	                             but if something bad happens, -1
//                                   -2 for NULL receiver
//                                   -3 not enough satellites

//_TITLE  enforce_all_bias_corrected --make sure all receivers corrected
Sint32 enforce_all_bias_corrected(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    void
    );
// Sint32 GPS_collection::enforce_all_bias_corrected
//                                O  return value
//                                   normally this is 0
//	                             but if something bad happens, -1
//                                   -2 for general calibration failure


// This one is a stub to calculate the model ionosphere values for receiver data
Real32 get_receiver_obs_model_STEC(
    Ionosphere_Base* const model_ionosphere,
    GPS_receiver* const Receiver,
    const Real64 MJD,
    const Space_Unit_Vector& satellite,
    const Uint32 time_slot,
    const Uint32 satellite_slot
    );



//_TITLE  dump_receiver_to_disk --dump a receiver's data to disk
Sint32 dump_receiver_to_disk(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const GPS_receiver* const receiver,
                               // I  The receiver to dump
    const char filename[]      // I  the name of the file to dump to
    );
// Sint32 GPS_collection::dump_receiver_to_disk
//                                O  return value
//                                   normally this is 0
//	                             but if something bad happens, -1
//                                   -2 for NULL receiver








    

    

//_TITLE  test_LA
Real64 test_LA(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    void
    );
    
    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace






#endif // GPS_COLLECTION_H
