// observation.h
// header file for calibration source class, for 2-D ionosphere
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE start
//	2007 Jan 03  JMA  --additions for GPS fitting, make clear this is
//                          principally for 2-D ionosphere work.
//	2007 Feb 28  JMA  --need knowledge of receiver type



#ifndef OBSERVATION_H
#define OBSERVATION_H

// INCLUDES
#include <stdio.h>
#include "JMA_math.h"
#include "latlon_cart.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  name one line description
class Observation_Ionosphere {
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
    Observation_Ionosphere(void) throw()
            : sigma_STEC(-1.0), sigma_VTEC(-1.0)
        {return;}
    



    // I assume that most of this information will be assigned
    // at different times.  Therefore, make the data variables public
    Uint32 station_number; // which station this was observed from
    Uint32 station_receiver_id; // Number indicating sttion and receiver type
    Uint32 object_number;  // which source is this?
    Uint32 object_id;      // Object ID number for bias fitting
    Real64 STEC;           // slant TEC to the source in TECU
    Real64 sigma_STEC;     // measurement error in STEC in TECU
                           // Note: if -1, then ignore this observation
    Real64 VTEC;           // Effective vertical TEC, from some parameterization
                           // of the ionosphere in TECU
    Real64 sigma_VTEC;     // measurement (+other) error in VTEC in TECU
                           // Note: if -1, then ignore this observation
    Real64 model_VTEC;     // model value for VTEC in TECU
    Real64 sigma_model_VTEC;// model error in VTEC in TECU
                           // Note: if -1, then ignore this model value
    Real64 Az;             // The azimuth of the observation, from North through
                           // East, in radians
    Real64 El;             // The elevation of the observation, in radians
    Real64 MJD;            // The Modified Julain Date of the observation
    LatLon_Cart pierce_point;
                           // The lat and longitude of the pierce point through
                           // the ionosphere.

protected:



private:




    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // OBSERVATION_H
