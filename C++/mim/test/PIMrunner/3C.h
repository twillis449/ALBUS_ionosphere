// 3C.h
// information for 3C data
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE start



#ifndef MY_3C_H
#define MY_3C_H

// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "cal_source.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS

    const Uint32 NUM_3C_SOURCES = 471;


// FUNCTIONS

//_TITLE  name one line description
    Cal_Source* read_in_3C_sources(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char filename[]  // I  the filename for the 3C data
        );
//  read_in_3C_sources            O  this function returns an array of Cal_Source
//                                   objects, which are the 3C sources.


}  // end namespace


#endif // MY_3C_H
