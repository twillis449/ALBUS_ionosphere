// ionosphere_fake.h
// make a fake ionosphere for code testing
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE  start



#ifndef IONOSPHERE_FAKE_H
#define IONOSPHERE_FAKE_H

// INCLUDES
#include "JMA_code.h"
#include "ionosphere.h"





// set up a namespace area for stuff.
namespace MIM_PIM {

    enum Ionosphere_Fake_Enum {
        fake_constant    = 0x00,         // just a constant
        fake_exponential = 0x01,
        fake_Gaussian    = 0x02
    };



//_CLASS  name one line description
class Ionosphere_Fake : public Ionosphere_Base {
//_DESC  full description of class

//	This class handles making a fake ionosphere in order to test
//	different aspects of the ionosphere code, including the integration
//	routines and the magnetic field, etc.

//	At the moment, this class will only provide an electron desnity as
//	a function of latitude.  But this may change in the future.

//	Different types of fake ionosphere electron densities can be used here.
//	See the Ionosphere_Fake_Enum stuff and function code for more info.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END


// NAMESPACE ISSUES    


public:
    // Constructors
    Ionosphere_Fake(Ionosphere_Fake_Enum type_in,
                    Real64 electron_precision_in = 1E-6,
                    Real64 Faraday_precision_in = 1E-4
                    ) throw()
            : Ionosphere_Base(electron_precision_in, Faraday_precision_in),
              fake_type(type_in)
        {};

    // Here is the important one
    virtual Real64 Electron_Density(const LatLon_Cart& position,
                                    const JMA_VEX_AREA::VEX_Time& time);



protected:



private:
    Ionosphere_Fake_Enum fake_type;


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // IONOSPHERE_FAKE_H
