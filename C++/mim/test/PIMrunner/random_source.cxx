// random_source.cxx
// stuff for reading in random source data
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cal_source.h"
#include "random_source.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    
//_TITLE  name one line description
    Cal_Source* read_in_random_sources(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char filename[]  // I  the filename for the 3C data
        )
//  read_in_random_sources        O  this function returns an array of Cal_Source
//                                   objects, which are the random sources.

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function reads in the random sources from the random_dirs.txt
//	file.  It will read in the RA and Dec and Flux Density number, and
//	dump out the results.

//	The memory for the array of Cal_Source objects is allocated with new[]
//	and should be dumped with delete[]

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

    {
        // Ok, can we read the file?
        FILE* fp = fopen(filename, "r");
        if(fp == NULL) {
            fprintf(stderr, "Error: cannot open random data file '%s' at %s:%d:%s\n",
                    filename, __FILE__, __LINE__, __func__);
            exit(2);
        }

        // Ok, I know the format of the file, and that there are supposed to
        // be NUM_RANDOM_SOURCES (1000) sources in there

        Cal_Source* data = new Cal_Source[NUM_RANDOM_SOURCES];
        if(data == NULL) {
            fprintf(stderr, "Error: cannot allocate space for %u Cal_Source objects at %s:%d:%s\n",
                    NUM_RANDOM_SOURCES, __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now step through the file
        for(Uint32 i=0; i < NUM_RANDOM_SOURCES; i++) {
            const unsigned SIZE = 1024;
            char line[SIZE]; line[0] = 0;

            if(fgets(line, SIZE, fp) == NULL) {
                fprintf(stderr, "Error: cannot read 3C data file '%s' line %u at %s:%d:%s\n",
                        filename, i, __FILE__, __LINE__, __func__);
                exit(2);
            }

            // Ok, now break this up into parts

            // The RA starts at character 12.  Dec starts at character 27.
            // Flux Density starts at 39.  Make sure there are plenty of
            // characters
            if(strlen(line) < 50) {
                fprintf(stderr, "Error: bad line in random data file '%s' line %u at %s:%d:%s\n",
                        filename, i, __FILE__, __LINE__, __func__);
                exit(2);
            }
            Real64 Dec = atof(line+0);
            Real64 RA = atof(line+21);
            Real64 Flux_density = pow(10.0,atof(line+44)*4.0);

            // make a Cal_Source
            data[i] = Cal_Source(RA, Dec, Flux_density, RA_DEC_radians);

//             // testing
//             printf("random object %03u ", i);
//             data[i].print_RA(stdout, 4, ':'); fputc(' ', stdout);
//             data[i].print_Dec(stdout, 4, ':');
//             printf(" %10.1f\n", data[i].FluxDensity());
        } // for i over NUM_RANDOM_SOURCES

        // close the file
        fclose(fp);

        
        // That's it
        return data;
    }



}  // end namespace


