// ionosphere_gen.h
// generic ionosphere theoretical model stuff
//_HIST  DATE NAME PLACE INFO
//	2007 Apr 11  James M Anderson  --JIVE  start



#ifndef IONOSPHERE_GEN_H
#define IONOSPHERE_GEN_H

// INCLUDES
#include "JMA_code.h"
#include "ionosphere.h"
#include "ionosphere_iri.h"
#include "ionosphere_iriplus.h"
#include "ionosphere_pim.h"
#include "ionosphere_fake.h"




// set up a namespace area for stuff.
namespace MIM_PIM {

    enum Ionosphere_Theoretical_Model_Enum {
        None     = 0,
        IRI      = 1,
        IRI_Plus = 2,
        PIM      = 3,
        Fake0    = 4
    };

    



// GLOBALS


// FUNCTIONS

    Ionosphere_Base* generate_new_theoretical_ionosphere(
        Ionosphere_Theoretical_Model_Enum type,
        Real64 electron_precision_in = 1E-6,
        Real64 Faraday_precision_in = 1E-4
        );
    


}  // end namespace


#endif // IONOSPHERE_GEN_H
