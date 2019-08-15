// GPS_receiver_obs.cxx
// hold the receiver observations
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start
// 	2007 Apr 18  JMA  --update for track info




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "JMA_code.h"
#include "GPS_receiver_obs.h"
#include "GPS.h"
#include <vector>
#include <iostream>

using namespace std;
// set up a namespace area for stuff.
namespace MIM_PIM {

GPS_receiver_obs::GPS_receiver_obs(const Uint32 N_TIMES, const Uint32 N_SAT,
                                   const Sint16* const sat_data,
                                   const Sint16* const track_data,
                                   const Real64* const stec_data,
                                   const Real64* const sigma_data,
                                   const Sint32* const bias_corrected) :
        NUM_TIMES(N_TIMES), NUM_SATELLITES(N_SAT), Max_Receiver_Type(0)
{
    satellite = new Sint16[NUM_TIMES*NUM_SATELLITES*2];
    sat_track = satellite + NUM_TIMES*NUM_SATELLITES;
    data = new Real64[NUM_TIMES*NUM_SATELLITES*3];
    data_sigma      = data            + NUM_TIMES*NUM_SATELLITES;
    data_model_STEC = data_sigma      + NUM_TIMES*NUM_SATELLITES;
    receiver_bias_corrected = new Sint32[GPS_MAX_POSSIBLE_RECEIVER_TYPE];
    memcpy(satellite, sat_data, NUM_TIMES*NUM_SATELLITES*sizeof(Sint16));
    memcpy(sat_track, track_data, NUM_TIMES*NUM_SATELLITES*sizeof(Sint16));
    memcpy(data, stec_data, NUM_TIMES*NUM_SATELLITES*sizeof(Real64));
    memcpy(data_sigma, sigma_data, NUM_TIMES*NUM_SATELLITES*sizeof(Real64));
    memcpy(receiver_bias_corrected, bias_corrected, GPS_MAX_POSSIBLE_RECEIVER_TYPE*sizeof(Sint32));
    for(Uint32 i=0; i < NUM_TIMES*NUM_SATELLITES; i++) {
        if(data[i+NUM_TIMES*NUM_SATELLITES] == GPS_BAD_DATA_VALUE) {
            data[i] = GPS_BAD_DATA_VALUE;
            data_sigma[i] = 1E34; // make sigma huge
        }
        else if(data[i] == GPS_BAD_DATA_VALUE) {
            data_sigma[i] = 1E34; // make sigma huge
        }
        if(satellite[i] >= Sint32(GPS_MAX_POSSIBLE_SATELLITES)) {
	  cout<<"error"<<satellite[i]<<" "<<GPS_MAX_POSSIBLE_SATELLITES<<endl;
          throw Satellite_Range_Error(satellite[i]);
        }
        if(satellite[i] >= 0) {
            if(satellite[i] > Sint16(Max_Receiver_Type)) Max_Receiver_Type =satellite[i];
            if(satellite[i+NUM_TIMES*NUM_SATELLITES] < 0) {
                // satellite but no track ???
                if(data[i] != GPS_BAD_DATA_VALUE)
		  {
		    cout<<"error"<<i<<" "<<satellite[i+NUM_TIMES*NUM_SATELLITES]<<" "<<NUM_TIMES*NUM_SATELLITES<<" data "<<data[i]<<endl;
		    throw Satellite_Range_Error(satellite[i]);
		  }
	    }
        }
    }
    blank_model_data();
    Max_Receiver_Type = Max_Receiver_Type/100;
    return;
}



void GPS_receiver_obs::blank_model_data(void) throw()
{
    for(Uint32 i=0; i < NUM_TIMES*NUM_SATELLITES; i++)
        data_model_STEC[i] = GPS_BAD_DATA_VALUE;
    return;
}




Sint32 GPS_receiver_obs::get_data(const Uint32 t, const Uint32 s,
                                  Sint16* sat, Real64* STEC, Real64* sigma)
        const throw()
{
    // t is the time index
    // s is the satellite index
    *sat = -1;
    *STEC = *sigma = GPS_BAD_DATA_VALUE;
    if(t >= NUM_TIMES) return -1;
    if(s >= NUM_SATELLITES) return +1;
    *sat = satellite[t*NUM_SATELLITES + s];
    *STEC = data[t*NUM_SATELLITES + s];
    if(*sat < 0) *STEC = GPS_BAD_DATA_VALUE;
    *sigma = data_sigma[t*NUM_SATELLITES + s];
    return 0;
}

Sint32 GPS_receiver_obs::get_data2(const Uint32 t, const Uint32 s,
                                   Sint16* sat, Sint16* track,
                                   Real64* STEC, Real64* sigma)
        const throw()
{
    // t is the time index
    // s is the satellite index
    *sat = *track = -1;
    *STEC = *sigma = GPS_BAD_DATA_VALUE;
    if(t >= NUM_TIMES) return -1;
    if(s >= NUM_SATELLITES) return +1;
    *sat = satellite[t*NUM_SATELLITES + s];
    *track = sat_track[t*NUM_SATELLITES + s];
    *STEC = data[t*NUM_SATELLITES + s];
    if(*sat < 0) *STEC = GPS_BAD_DATA_VALUE;
    *sigma = data_sigma[t*NUM_SATELLITES + s];
    return 0;
}


Sint32 GPS_receiver_obs::get_model_data(const Uint32 t, const Uint32 s,
                                        Real64* model_STEC) const throw()
{
    // t is the time index
    // s is the satellite index
    *model_STEC = GPS_BAD_DATA_VALUE;
    if(t >= NUM_TIMES) return -1;
    if(s >= NUM_SATELLITES) return +1;
    *model_STEC = data_model_STEC[t*NUM_SATELLITES + s];
    return 0;
}



Uint32 GPS_receiver_obs::total_tracks(void) const
{
    std::vector<Sint16> max_track(GPS_MAX_POSSIBLE_SATELLITES);
    for(Uint32 i=0; i < GPS_MAX_POSSIBLE_SATELLITES; i++) max_track[i] = -1;
    for(Uint32 i=0; i < NUM_TIMES*NUM_SATELLITES; i++) {
        if(satellite[i] >= 0) {
            if(sat_track[i] >= 0) {
                Sint16 t = max_track[satellite[i]];
                if(sat_track[i] > t)
                    max_track[satellite[i]] = sat_track[i];
            }
        }
    }
    Uint32 count = 0;
    for(Uint32 i=0; i < GPS_MAX_POSSIBLE_SATELLITES; i++) {
        if(max_track[i] >= 0) {
            count += max_track[i]+1;
        }
    }
    return count;
}

Uint32 GPS_receiver_obs::total_tracks(
    Uint32* const restrict sat_array) const
{
    std::vector<Sint16> max_track(GPS_MAX_POSSIBLE_SATELLITES);
    for(Uint32 i=0; i < GPS_MAX_POSSIBLE_SATELLITES; i++) max_track[i] = -1;
    for(Uint32 i=0; i < NUM_TIMES*NUM_SATELLITES; i++) {
        if(satellite[i] >= 0) {
            if(sat_track[i] >= 0) {
                Sint16 t = max_track[satellite[i]];
                if(sat_track[i] > t)
                    max_track[satellite[i]] = sat_track[i];
            }
        }
    }
    Uint32 count = 0;
    for(Uint32 i=0; i < GPS_MAX_POSSIBLE_SATELLITES; i++) {
        Uint32 num = Uint32(max_track[i]+1);
        count += num;
        sat_array[i] = num;
    }
    return count;
}




void GPS_receiver_obs::adjust_bias(const Real64 bias, const Real64 std_dev,
                                   const Uint32 type,
                                   const bool bump_bias_valid)
        throw()
{
    if(type > Max_Receiver_Type) return; // Ignore.
    // subtract bias first
    if(bias != 0.0) {
        for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES; i++) {
            if(satellite[i] >= 0) {
                if(Uint32(satellite[i]/100) == type) {
                    if((data[i] != GPS_BAD_DATA_VALUE)
                       && (data[i] != GPS_NOT_INITIALIZED_VALUE))
                        data[i] -= bias;
                }
            }
        }
    }
    if(std_dev != 0.0) {
        // Now adjust the standard deviation
        Real64 variance = std_dev*std_dev;
        for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES; i++) {
            if(satellite[i] >= 0) {
                if(Uint32(satellite[i]/100) == type) {
                    if((data[i] != GPS_BAD_DATA_VALUE)
                       && (data[i] != GPS_NOT_INITIALIZED_VALUE))
                        data_sigma[i] = sqrt(Real64(data_sigma[i])*data_sigma[i]
                                             + variance);
                }
            }
        }
    }
    if((bump_bias_valid))
        if(receiver_bias_corrected[type] == 0)
            receiver_bias_corrected[type] = 2;
    return;
}







    
void GPS_receiver_obs::adjust_satellite_bias(const Uint16 sat,
                                             const Real64 bias,
                                             const Real64 std_dev)
        throw()
{
    assert(sat < INT16_MAX);
    const Sint16 ssat = Sint16(sat);
    // subtract bias first
    if(bias != 0.0) {
        for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES; i++) {
            if(satellite[i] == ssat) {
                if((data[i] != GPS_BAD_DATA_VALUE)
                   && (data[i] != GPS_NOT_INITIALIZED_VALUE))
                    data[i] -= bias;
            }
        }
    }
    if(std_dev != 0.0) {
        // Now adjust the standard deviation
        Real64 variance = std_dev*std_dev;
        for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES; i++) {
            if(satellite[i] == ssat) {
                if((data[i] != GPS_BAD_DATA_VALUE)
                   && (data[i] != GPS_NOT_INITIALIZED_VALUE))
                    data_sigma[i] = sqrt(Real64(data_sigma[i])*data_sigma[i]
                                         + variance);
            }
        }
    }
    return;
}

void GPS_receiver_obs::adjust_satellite_bias2(const Sint16 sat,
                                              const Sint16 track,
                                              const Real64 bias,
                                              const Real64 std_dev)
        throw()
{
    // subtract bias first
    if(bias != 0.0) {
        Sint16* track_num = satellite + NUM_TIMES*NUM_SATELLITES;
        for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES; i++) {
            if((satellite[i] == sat) && (track_num[i] == track)){
                if((data[i] != GPS_BAD_DATA_VALUE)
                   && (data[i] != GPS_NOT_INITIALIZED_VALUE))
                    data[i] -= bias;
            }
        }
    }
    if(std_dev != 0.0) {
        // Now adjust the standard deviation
        Real64 variance = std_dev*std_dev;
        for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES; i++) {
            if((satellite[i] == sat) && (sat_track[i] == track)){
                if((data[i] != GPS_BAD_DATA_VALUE)
                   && (data[i] != GPS_NOT_INITIALIZED_VALUE))
                    data_sigma[i] = sqrt(Real64(data_sigma[i])*data_sigma[i]
                                         + variance);
            }
        }
    }
    return;
}

    
    

void GPS_receiver_obs::clear_bias(void) throw()
{
    for(Uint32 type = 0; type <= Max_Receiver_Type; type++) {
        if(receiver_bias_corrected[type] != 1)
            receiver_bias_corrected[type] = 0;
    }
    return;
}
    
void GPS_receiver_obs::clear_bias_2(void) throw()
{
    for(Uint32 type = 0; type <= Max_Receiver_Type; type++) {
        if(receiver_bias_corrected[type] == 2)
            receiver_bias_corrected[type] = 0;
    }
    return;
}



Sint32 GPS_receiver_obs::set_model_data(const Uint32 t, const Uint32 s,
                                        Real64 model_STEC) throw()
{
    // t is the time index
    // s is the satellite index
    if(t >= NUM_TIMES) return -1;
    if(s >= NUM_SATELLITES) return +1;
    data_model_STEC[t*NUM_SATELLITES + s] = model_STEC;
    return 0;
}



bool GPS_receiver_obs::valid_data_exists(void) const throw()
{
    bool any_valid_data = false;
    for(Uint32 i = 0; i < NUM_TIMES*NUM_SATELLITES*2; i++) {
        if((data[i] != GPS_BAD_DATA_VALUE)
           && (data[i] != GPS_NOT_INITIALIZED_VALUE)) {
            any_valid_data = true;
            break;
        }
    }
    return any_valid_data;
}
    


    


// GLOBALS


// FUNCTIONS



}  // end namespace


