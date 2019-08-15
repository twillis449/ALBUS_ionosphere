// GPS_receiver_obs.h
// Stores information from observations by a single receiver
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start



#ifndef GPS_RECEIVER_OBS_H
#define GPS_RECEIVER_OBS_H

// INCLUDES
#include "JMA_code.h"
#include "JMA_math.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  GPS_receiver_obs
class GPS_receiver_obs {
//_DESC  full description of class
//	This class holds the observations of a single GPS (GLONASS/Galileo)
//	receiver.  The STEC and uncertainty are in units of m^{-2}.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:
    GPS_receiver_obs(const Uint32 N_TIMES, const Uint32 N_SAT,
                     const Sint16* const sat_data,
                     const Sint16* const track_data,
                     const Real64* const stec_data,    // m^{-2}
                     const Real64* const sigma_data,   // m^{-2}
                     const Sint32* const bias_corrected);
    
    ~GPS_receiver_obs(void) {delete[] data; delete[] satellite;
        delete[] receiver_bias_corrected; data = NULL;
        satellite = NULL; return;}

    const Uint32 NUM_TIMES;
    const Uint32 NUM_SATELLITES;


    // For get_data, t is the time index, and s is the satellite index (not the
    // satellite number!)
    Sint32 get_data(const Uint32 t, const Uint32 s,
                    Sint16* sat,
                    Real64* STEC,   // m^{-2}
                    Real64* sigma   // m^{-2}
                    ) const throw();
    Sint32 get_data2(const Uint32 t, const Uint32 s,
                     Sint16* sat,
                     Sint16* track,
                     Real64* STEC,   // m^{-2}
                     Real64* sigma   // m^{-2}
                     ) const throw();

    Sint32 get_bias_valid(const Uint32 type) const throw()
        {return ( (type <= Max_Receiver_Type) ?
                  receiver_bias_corrected[type]:-1);}

    // Adjust the bias and standard deviation levels
    void adjust_bias(const Real64 bias, const Real64 std_dev,
                     const Uint32 type, const bool bump_bias_valid) throw();
    // Adjust the satellite bias and standard deviation levels
    void adjust_satellite_bias(const Uint16 sat,
                               const Real64 bias, const Real64 std_dev) throw();
    void adjust_satellite_bias2(const Sint16 sat,
                                const Sint16 track,
                                const Real64 bias,
                                const Real64 std_dev)
        throw();

    void clear_bias(void) throw();
    void clear_bias_2(void) throw();

    // Is there any valid data in this dataset?
    bool valid_data_exists(void) const throw();

    // Count the total number of tracks in dataset
    Uint32 total_tracks(void) const;
    Uint32 total_tracks(Uint32* const restrict sat_array) const;


    // Only do this if really necessary.  Use adjust_bias in most cases.
    void force_bias_status(const Uint32 type, const Sint32 val) throw()
        { if(type <= Max_Receiver_Type) receiver_bias_corrected[type] = val;
            return; }            


    // This will completely wipe out any model data, so only use this if
    // you mean to do that.
    void blank_model_data(void) throw();

    // How to access model data
    Sint32 get_model_data(const Uint32 t, const Uint32 s,
                          Real64* model_STEC) const throw();
    Sint32 set_model_data(const Uint32 t, const Uint32 s,
                          Real64 model_STEC) throw();
    


protected:



private:
    Sint16* satellite;
    Sint16* sat_track;
    Real64* data;
    Real64* data_sigma;
    Real64* data_model_STEC;          // some model's STEC value
    Sint32* receiver_bias_corrected;  // array of flags by receiver type

    Uint32 Max_Receiver_Type;

    // Prevent copying!
    GPS_receiver_obs(const GPS_receiver_obs& g);
    GPS_receiver_obs& operator=(const GPS_receiver_obs& g);

};


// CLASS FUNCTIONS



// HELPER FUNCTIONS





class Satellite_Range_Error {
public:
    Sint16 bad_sat;
    Satellite_Range_Error(Sint16 bad_sat_) : bad_sat(bad_sat_) {return;};
};
    
    



}  // end namespace

#endif // GPS_RECEIVER_OBS_H
