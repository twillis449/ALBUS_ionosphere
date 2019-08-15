// GPS_satellites.h
// hold the positions of the GPS satellites as a function of time
//_HIST  DATE NAME PLACE INFO
// 2006 Dec 05  James M Anderson -- JIVE  start



#ifndef GPS_SATELLITES_H
#define GPS_SATELLITES_H

// INCLUDES
#include <assert.h>
#include "JMA_code.h"
#include "space_vector.h"





// set up a namespace area for stuff.
namespace MIM_PIM {

    

//_CLASS  GPS_satellites
class GPS_satellites {
//_DESC  full description of class
//	This class holds the positions of ALL of the GPS (and GLONASS and
//	Galileo) satellites in an Earth-fixed Cartesian coordinate scheme.

//	Coordinate units are meters.

//	For simplicity, this routine just keeps a simple Real64 array
//	of numbers, and converts these to a Space_Vector on output.  Since
//	the Space_Vector is very simple, this should have minimal overhead.

//	By the convention I use in the Python code, a position of 0,0,0
//	means that there is no data for the satellite at the requested time.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:
        
        // N_TIMES is the number of times
        // N_SAT is the total number of satellites
        // N_ELEMENTS is the number of position elements per
        //     satellite (probably 3 or 6), with x,y,z in positions
        //     0,1,2
        GPS_satellites(const Uint32 N_TIMES, const Uint32 N_SAT,
                       const Uint32 N_ELEMENTS,
                       const Real64* const s_data);
        ~GPS_satellites() 
            {if((data)) delete[] data; data = NULL;return;}


        const Uint32 NUM_TIMES;
        const Uint32 NUM_SATELLITES;

        Space_Vector get_sat(const Uint32 t, const Uint32 s) const throw()
            {
                assert((t < NUM_TIMES) && (s < NUM_SATELLITES));
                return data[t*NUM_SATELLITES+s];
            }



protected:



private:
        Space_Vector* data;


        // Prevent copying!
        GPS_satellites(const GPS_satellites& g);
        GPS_satellites& operator=(const GPS_satellites& g);
    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // GPS_SATELLITES_H
