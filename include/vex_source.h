// vex_source.h
// information needed for a vex source
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start



#ifndef VEX_SOURCE_H
#define VEX_SOURCE_H

// INCLUDES
#include "JMA_math.h"
#include "space_vector.h"
#include "space_unit_vector.h"
#include "latlon_cart.h"

#include <string>

#include "vex_time.h"







// set up a namespace area for stuff.
namespace JMA_VEX_AREA {



//_CLASS  VEX_Source --a holder for Source Information
    class VEX_Source  {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
// 	2005 Sep 20  James M Anderson

//_END


// NAMESPACE ISSUES    


public:
        VEX_Source()
            {};
        VEX_Source(std::string Name_in,
                   MIM_PIM::LatLon_Cart RA_Dec_in,
                   MIM_PIM::Space_Vector RA_Dec_Rate_in,
                   std::string RefEpochStr_in,
                   VEX_Time RefEpoch_in
                   )
                : Name(Name_in),
                  RA_Dec(RA_Dec_in),
                  RA_Dec_Rate(RA_Dec_Rate_in),
                  RefEpochStr(RefEpochStr_in),
                  RefEpoch(RefEpoch_in)
            {};

        // Variables to let the user access
        std::string Name;                  // name of the source
        MIM_PIM::LatLon_Cart RA_Dec;       // position of the source
        MIM_PIM::Space_Vector RA_Dec_Rate; // radians per Julian day
        std::string RefEpochStr;
        VEX_Time RefEpoch;



protected:



private:


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // VEX_SOURCE_H
