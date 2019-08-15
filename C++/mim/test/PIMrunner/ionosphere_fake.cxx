// ionosphere_fake.cxx
// make up some fake electron densities
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>

#include "ionosphere.h"
#include "ionosphere_fake.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



//_TITLE  Ionosphere_Fake::Electron_Density --fake an ionospehre
    Real64 Ionosphere_Fake::Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
	  const LatLon_Cart& position,
                               // I  The position about the Earth at which to
                               //    evaluate the ionosphere electron density
          const JMA_VEX_AREA::VEX_Time& time
                               // I  The time (UT) at which to evaluate the
                               //    ionosphere
          )

// Real64 Ionosphere_Fake::Electron_Density
//                                O  The electron density, in m^{-3} at the
//                                   position and time specified.
//	                             Negative values are unphysical, and will
//                                   be used to indicate error problems.
//                                   -2.0E300 nonsense altitude
//                                   -1.0E301 programmer error: unknown fake_type


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function generates a fake ionosphere electron density.  Based
//	on the fake_type set upon construction of a class member, this function
//	will return various kinds of fake information

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

    {
        // switch on the type of fake data we are sending
        switch (fake_type) {
            case fake_constant:
                // One seems like a nice constant for testing
                return 1.0;
            case fake_exponential:
                // a simple exponential with height
                // See JMA notes from 2005 Sep 17-1
                // \rho(H) = A\exp\left( -\frac{H}{\Delta H} \right),
                // where \Delta H is the scale height.
                // The scaling factor A is chosen so that
                // \Sigma = \int_{H_0=0~\mathrm{m}}^{H_1=\infty~\mathrm{m}}
                //          \rho \, dH
                //        = 10^{17} \mathrm{m}^{-2}
                //        = 10 TECU
            {
                const Real64 Height = position.Elevation();
                static const Real64 Delta_Height = 100E3;
                                                 // scale height of 100 km
                static const Real64 A = 1E17 / Delta_Height;
                return A*exp(-Height/Delta_Height);
            }
            case fake_Gaussian:
                // See the notes from 2005 Sep 16-2
                // This is a more complicated case involving the latitude
                // and a Gaussian exponential function, to simulate the
                // peak ionospheric electron density around ~300 km.
                // It also allows for a variable seasonal influence of the
                // Solar heating over the equator, and has the electron
                // density fall off away from the equator.
            {
                const Real64 Alt = position.Elevation();
                const Real64 latitude_offset = 20.0*M_DEG2RAD;
                const Real64 cLat_p = cos(position.Lat()-latitude_offset);
                const Real64 Height =
                    300E3 + 100E3 * (cLat_p*cLat_p*cLat_p);
                const Real64 Delta_Height = 100E3 * (Height / 300E3);
                const Real64 A =
                    0.5 + (cLat_p*cLat_p*cLat_p*cLat_p);
                const Real64 B = 1E12 * 10.0/42.49017;
                const Real64 diff = (Alt - Height) / Delta_Height;
                Real64 rho = A*B*exp( -diff*diff );
                // Now add on the exponential tail
                if(Alt <= Height) {
                    const Real64 diff2 = (Alt - Height) / 50E3;
                    rho += 0.1*A*B*exp( - diff2*diff2 );
                }
                else {
                    rho += 0.1*A*B*exp( -(Alt - Height) / 2000E3 );
                }
                return rho;
            }
                
            default:
                // we should never get here.  Return an error
                return -1.0E301;
        }
        // We should never ever get here
        fprintf(stderr, "Error: How did I get here? %s:%d:%s\n",
                __FILE__, __LINE__, __func__);
        exit(1);
        return -1.0;
    }
    



}  // end namespace


