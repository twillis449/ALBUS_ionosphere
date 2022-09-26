// 3C.cxx
// stuff for reading in 3C data
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cal_source.h"
#include "3C.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    
//_TITLE  name one line description
    Cal_Source* read_in_3C_sources(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char filename[]  // I  the filename for the 3C data
        )
//  read_in_3C_sources            O  this function returns an array of Cal_Source
//                                   objects, which are the 3C sources.

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function reads in the 3C sources from the 3c.dat catalog from
//	the CDS.  It will read in the RA and Dec and Flux Density, and
//	dump out the results.

//	The memory for the array of Cal_Source objects is allocated with new[]
//	and should be dumped with delete[]

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE start

//_END

    {
        // Ok, can we read the file?
        FILE* fp = fopen(filename, "r");
        if(fp == NULL) {
            fprintf(stderr, "Error: cannot open 3C data file '%s' at %s:%d:%s\n",
                    filename, __FILE__, __LINE__, __func__);
            exit(2);
        }

        // Ok, I know the format of the file, and that there are supposed to
        // be NUM_3C_SOURCES (471) sources in there

        Cal_Source* data = new Cal_Source[NUM_3C_SOURCES];
        if(data == NULL) {
            fprintf(stderr, "Error: cannot allocate space for %u Cal_Source objects at %s:%d:%s\n",
                    NUM_3C_SOURCES, __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now step through the file
        for(Uint32 i=0; i < NUM_3C_SOURCES; i++) {
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
            if(strlen(line) < 40) {
                fprintf(stderr, "Error: bad line in 3C data file '%s' line %u at %s:%d:%s\n",
                        filename, i, __FILE__, __LINE__, __func__);
                exit(2);
            }
            Real64 h = atof(line+12);
            Real64 m = atof(line+15);
            Real64 s = atof(line+18);

            char sign = line[27];
            Real64 dd = atof(line+28);
            Real64 dm = atof(line+31);

            Real64 fd = atof(line+39);

            Real64 RA = (s/60.0 + m)/60.0 + h;
            Real64 Dec = dm/60.0 + dd;
            if(sign == '-') Dec = -Dec;

            // make a Cal_Source
            data[i] = Cal_Source(RA, Dec, fd, RA_DEC_astro);

//             // testing
//             printf("3C object %03u ", i);
//             data[i].print_RA(stdout, 4, ':'); fputc(' ', stdout);
//             data[i].print_Dec(stdout, 4, ':');
//             printf(" %10.0f\n", data[i].FluxDensity());
        } // for i over NUM_3C_SOURCES

        // close the file
        fclose(fp);

        
        // That's it
        return data;
    }



}  // end namespace


