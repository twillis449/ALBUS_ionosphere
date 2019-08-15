// GPS_satellites.cxx
// What this header file is for
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start




// INCLUDES
#include <stdlib.h>
#include <assert.h>
#include "JMA_code.h"
#include "GPS_satellites.h"
#include "GPS.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS

    GPS_satellites::GPS_satellites(const Uint32 N_TIMES, const Uint32 N_SAT,
                                   const Uint32 N_ELEMENTS,
                                   const Real64* const s_data)
            : NUM_TIMES(N_TIMES), NUM_SATELLITES(N_SAT)
    {
        assert(NUM_3D == 3);
        assert(N_SAT <= GPS_MAX_POSSIBLE_SATELLITES);
        data = new Space_Vector[NUM_TIMES*NUM_SATELLITES];
        for(Uint32 t=0; t < NUM_TIMES; t++) {
            for(Uint32 s = 0; s < NUM_SATELLITES; s++) {
                const Real64* const sp = &(s_data[t*NUM_SATELLITES*N_ELEMENTS
                                                  + s*N_ELEMENTS]);
                data[t*NUM_SATELLITES + s] = Space_Vector(sp);
            }
        }
        return;
    }






}  // end namespace


