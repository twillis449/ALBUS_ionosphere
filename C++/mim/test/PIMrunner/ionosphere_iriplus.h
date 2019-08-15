// ionosphere_iriplus.h
// call the IRI ionosphere stuff to generate an ionosphere, plus some waves
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 22  James M Anderson  --JIVE  start from ionosphere_fake



#ifndef IONOSPHERE_IRI_PLUS_H
#define IONOSPHERE_IRI_PLUS_H

// INCLUDES
#include <time.h>
#include "JMA_code.h"
#include "ionosphere.h"
#include "ionosphere_iri.h"
#include "station_reference.h"




// set up a namespace area for stuff.
namespace MIM_PIM {


//_CLASS  name one line description
class Ionosphere_IRI_Plus : public Ionosphere_IRI {
//_DESC  full description of class

//	This class handles making a IRI ionosphere plus a set of waves in
//      the lower ionosphere.  It uses Ionosphere_IRI to handle IRI, then
//	adds on waves to the lower ionosphere.

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
    Ionosphere_IRI_Plus(const LatLon_Cart& Home_Position, // center for waves
                        const JMA_VEX_AREA::VEX_Time& ref_time,
                                                          // a reference time
                        const Real64 direction,           // in radians
                        const Real64 wavelength,          // in m
                        const Real64 amplitude,           // in m^{-3}
                                                          //   Note: for 1 TECU
                                                          //   VTEC, use
                                                          //   1E12/sqrt(M_PI)
                        const Real64 velocity,            // in m s^{-1}
                        Real64 electron_precision_in = 1E-6,
                        Real64 Faraday_precision_in = 1E-4
                        ) throw()
            : Ionosphere_IRI(electron_precision_in, Faraday_precision_in),
              Wave_Center(Home_Position), Reference_Time(ref_time),
              Direction(direction),
              Wavelength(wavelength), Amplitude(amplitude), Velocity(velocity)
        {
            return;
        };
    Ionosphere_IRI_Plus(const LatLon_Cart& Home_Position, // center for waves
                        const struct tm& ref_time,        // a reference time
                        const Real64 direction,           // in radians
                        const Real64 wavelength,          // in m
                        const Real64 amplitude,           // in m^{-3}
                                                          //   Note: for 1 TECU
                                                          //   VTEC, use
                                                          //   1E12/sqrt(M_PI)
                        const Real64 velocity,            // in m s^{-1}
                        Real64 electron_precision_in = 1E-6,
                        Real64 Faraday_precision_in = 1E-4
                        ) throw()
            : Ionosphere_IRI(electron_precision_in, Faraday_precision_in),
              Wave_Center(Home_Position), Reference_Time(ref_time),
              Direction(direction),
              Wavelength(wavelength), Amplitude(amplitude), Velocity(velocity)
        {
            return;
        };

    // Here is the important one
    virtual Real64 Electron_Density(const LatLon_Cart& position,
                                    const JMA_VEX_AREA::VEX_Time& time);





protected:



private:
    const Station_Reference Wave_Center;
    const JMA_VEX_AREA::VEX_Time Reference_Time;
    const Real64 Direction; // in radians
    const Real64 Wavelength;// in m
    const Real64 Amplitude; // in electrons m^{-3}
    const Real64 Velocity;  // in m s^{-1}


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // IONOSPHERE_IRI_PLUS_H
