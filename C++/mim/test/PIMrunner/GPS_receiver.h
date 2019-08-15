// GPS_receiver.h
// Keep track of a GPS receiver and observations
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start



#ifndef GPS_RECEIVER_H
#define GPS_RECEIVER_H

// INCLUDES
#include "JMA_code.h"
#include <string.h>
#include "GPS.h"
#include "station_latlon.h"
#include "space_vector.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  GPS_receiver
class GPS_receiver {
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
    GPS_receiver(const char name[],
                 const Space_Vector& position,
                 const Uint32 N_TIMES, const Uint32 N_SAT,
                 const Sint16* const sat_data,
                 const Sint16* const track_data,
                 const Real64* const stec_data,
                 const Real64* const sigma_data,
                 const Sint32* const bias_valid) :
            station_position(position),
            station_obs(N_TIMES, N_SAT, sat_data, track_data,
                        stec_data, sigma_data, bias_valid)
        {strncpy(sta_name, name, 8); sta_name[4] = 0; return;};


    const Station_LatLon station_position;
    GPS_receiver_obs station_obs;

    const char * const station_name() const throw() {return sta_name;};

    



protected:



private:
    char sta_name[8];
    


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // GPS_RECEIVER_H
