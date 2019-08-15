// observation_3D.h
// header file for calibration source class, for 3-D ionosphere
//_HIST  DATE NAME PLACE INFO
//	2007 Jan 03  James M Anderson  --JIVE start from observation.h
//	2007 Jan 19  JMA  --add in fields for a station number and an
//                          object number
//	2007 Feb 28  JMA  --need knowledge of receiver type



#ifndef OBSERVATION_3D_H
#define OBSERVATION_3D_H

// INCLUDES
#include <stdio.h>
#include "JMA_math.h"
#include "space_unit_vector.h"
#include "station_latlon.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  name one line description
class Observation_3D_Ionosphere {
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
    // Constructors
    Observation_3D_Ionosphere(void) throw()
            : direction(1.0,0.0,0.0), sigma_STEC(-1.0), sigma_model_STEC(-1.0)
        {return;}
    



    // I assume that most of this information will be assigned
    // at different times.  Therefore, make the data variables public
    Space_Unit_Vector direction;
                           // what direction from the station, in an Earth-fixed
                           // coordinate system, is being observed
    Real64 Az;             // The azimuth of the observation, from North through
                           // East, in radians
    Real64 El;             // The elevation of the observation, in radians
    Real64 MJD;            // The Modified Julain Date of the observation
    Real64 STEC;           // slant TEC to the source in m^{-2}
    Real64 sigma_STEC;     // measurement error in STEC in m^{-2}
                           // Note: if -1, then ignore this observation
    Real64 model_STEC;     // model value for STEC in m^{-2}
    Real64 sigma_model_STEC;// model error in STEC in m^{-2}
                           // Note: if -1, then ignore this model value
    const Station_LatLon* station;
                           // which station this was observed from?
    Uint32 station_number; // which station this was observed from
    Uint32 station_receiver_id; // Number indicating sttion and receiver type
    Uint32 object_number;  // which source is this?
    Uint32 object_id;      // Object ID number for bias fitting

protected:



private:




    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // OBSERVATION_3D_H
