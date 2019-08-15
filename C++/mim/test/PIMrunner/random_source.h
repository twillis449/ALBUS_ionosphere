// random_sources.h
// information for 3C data
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE start



#ifndef RANDOM_SOURCES_H
#define RANDOM_SOURCES_H

// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "cal_source.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS

    const Uint32 NUM_RANDOM_SOURCES = 1000;


// FUNCTIONS

//_TITLE  name one line description
    Cal_Source* read_in_random_sources(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char filename[]  // I  the filename for the 3C data
        );
//  read_in_random_sources        O  this function returns an array of Cal_Source
//                                   objects, which are the random sources.


}  // end namespace


#endif // RANDOM_SOURCES_H
