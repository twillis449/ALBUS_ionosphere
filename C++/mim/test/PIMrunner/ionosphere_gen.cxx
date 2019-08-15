// ionosphere_gen.cxx
// code to generate a theoretical ionosphere model 
//_HIST  DATE NAME PLACE INFO
//	2007 Apr 11  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_code.h"
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include "ionosphere_gen.h"
#include "latlon_cart.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS

//_TITLE  generate_new_theoretical_ionosphere --
Ionosphere_Base* generate_new_theoretical_ionosphere(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Ionosphere_Theoretical_Model_Enum type,
        Real64 electron_precision_in,
                               // I  relative precision of electron column
        Real64 Faraday_precision_in
                               // I  relative precision of Faraday rotation
        )
// Ionosphere_Base* generate_new_theoretical_ionosphere
//	                          O  pointer to a single ionosphere object
//                                   allocated with new.  The caller must
//                                   delete it.  Watch out for NULL, as NULL
//                                   is a valid response for some types and
//                                   in no memory conditions.

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Apr 11  James M Anderson  -- JIVE  start

//_END

{
//    fprintf(stdout, "In ionosphere_gen.cxx generate_new_theoretical_ionosphere\n");
    Ionosphere_Base* ip = NULL;
    switch(type) {
        case None:
            ip = NULL;
            break;
        case IRI:
            ip = new Ionosphere_IRI(electron_precision_in,Faraday_precision_in);
            break;
        case IRI_Plus:
            {
                LatLon_Cart home(0.0,0.0);
                JMA_VEX_AREA::VEX_Time zero_time(JMA_VEX_AREA::MJD_OFFSET,50000.0);
                ip = new Ionosphere_IRI_Plus(home,
                                             zero_time,
                                             0.0,
                                             100E3,
                                             1E12,
                                             100.0,
                                             electron_precision_in,
                                             Faraday_precision_in);
            }
            break;
        case PIM:
            ip = new Ionosphere_PIM(electron_precision_in,Faraday_precision_in);
            break;
        case Fake0:
            ip = new Ionosphere_Fake(fake_Gaussian,
                                     electron_precision_in,
                                     Faraday_precision_in);
            break;
        default:
            fprintf(stderr, "Error: programmer mistake, unrecognized ionosphere model type %d\n", int(type));
            ip = NULL;
    }
    return ip;
}
    



}  // end namespace


