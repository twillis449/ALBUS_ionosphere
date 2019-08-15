// ionosphere_iri.cxx
// make up some iri electron densities
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 08  James M Anderson  --JIVE  start from ionosphere_fake.cxx
//	2006 Jul 06  JMA  --change to new data path mechanism.




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>

#include "ionosphere.h"
#define IONOSPHERE_IRI_CXX 1
#include "ionosphere_iri.h"

#include "jma_irisub_c.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



//_TITLE  Ionosphere_IRI::Electron_Density --get the IRI ionospehre
Real64 Ionosphere_IRI::Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const LatLon_Cart& position,
                               // I  The position about the Earth at which to
                               //    evaluate the ionosphere electron density
    const JMA_VEX_AREA::VEX_Time& time
                               // I  The time (UT) at which to evaluate the
                               //    ionosphere
    )

// Real64 Ionosphere_IRI::Electron_Density
//                                O  The electron density, in m^{-3} at the
//                                   position and time specified.
//	                             Negative values are unphysical, and will
//                                   be used to indicate error problems.
//                                   -2.0E300 nonsense altitude


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function generates an IRI ionosphere electron density.  This
//	function calls the jma_irisub_c function in the JMA IRI library to
//	get the electron density as the specified position.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 08  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

    {
        // declare the electron density with a sane value.
        real n_e_density = 0.0;
//        printf("DEBUG in inonosphere_IRI::Electron_Density initial n_e_density %E\n", n_e_density);fflush(stdout);
        
        jma_irisub_c(IRI_DATA_PATH,
                     position.Lat(),
                     position.Lon(),
                     time.Year(),
                     time.Month(),
                     time.Day(),
                     time.Day_Fraction()*24.0,
                     position.Elevation(),
                     &n_e_density
                     );
//       printf("DEBUG n_e_density %E\n", n_e_density);fflush(stdout);

        return n_e_density;
    }
    



}  // end namespace


