// GPS_times.h
// handle the times for GPS processing
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE  start



#ifndef GPS_TIMES_H
#define GPS_TIMES_H

// INCLUDES
#include "JMA_code.h"
#include "GPS.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  name one line description
    class GPS_times {
//_DESC  full description of class

//	This class holds the time information for the GPS data.

//	I assume that all GPS receivers record their GPS data at the same
//	times, for all measurements, for all stations.  At this time,
//	2006 Dec 05, this seems to be a reasonable request given the RINEX
//	pulic data and its normal 30 s intervals.

//	This class holds onto an array of times which are stored as
//	MJD (Modified Julian Date) values.  The times MUST BE IN INCREASING
//	ORDER, WITH NO DUPLICATE ENTRIES.

//	This class provides routines to search within the times to get
//	an index of where you want to look, the closest time availabel to a
//	requested time, and so on.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	The times must be in order of increasing time, with no duplicates.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:
        GPS_times(const Uint32 N_TIMES, const Real64* const data);
        ~GPS_times()
            {delete[] MJD;MJD = NULL;return;}

        const Uint32 NUM_TIMES;

        Real64 get_time(const Uint32 t) const throw()
            {if(t<NUM_TIMES) return MJD[t];return GPS_BAD_DATA_VALUE;}

        // If you call reset_search_index with no argument, you reset
        // the system.  Othrwise, with an argument, you can choose
        // your own index.
        void reset_search_index(Uint32 i=UINT32_MAX) throw()
            {last_index = i;return;}
        Uint32 get_interpolation_index(const Real64 new_MJD) throw();
        Uint32 get_nearest_index(const Real64 new_MJD) throw();
        Uint32 get_nearest_index_2(const Real64 new_MJD) throw();
        Uint32 get_index_low_bound(const Real64 new_MJD) const throw();
        Uint32 get_index_high_bound(const Real64 new_MJD) const throw();


protected:

        Uint32 get_interpolation_bisect_index(const Real64 new_MJD) throw();
        Uint32 get_bisect_index(const Real64 new_MJD) throw();



private:
        Real64* MJD;          // The MJD times
        Uint32 last_index;    // Where was the last time found?


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // GPS_TIMES_H
