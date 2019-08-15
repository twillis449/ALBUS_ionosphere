// location.h
// describe a location on the Earth
//_HIST  DATE NAME PLACE INFO
// 2005 Aug 17  James M Anderson  --JIVE start



#ifndef LOCATION_H
#define LOCATION_H

// INCLUDES
#include "JMA_math.h"




// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  Earth_location --spherical Earth location (lat, lon) holder
    class Earth_location {
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
        Earth_location(Real64 lat_in, Real64 lon_in)
            lat=lat_in, lon=lon_in {return;};


        Real64 Lat(void) {return lat;};
        Real64 Lon(void) {return lon;};

        

        Earth_location add_logarythmic_spiral_pattern();

protected:



private:
        Real64 lat; // the lattitude
        Real64 lon; // the longitude


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // LOCATION_H
