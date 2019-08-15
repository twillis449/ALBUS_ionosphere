// GPS_times.cxx
// Routines to look into the GPS times
//_HIST  DATE NAME PLACE INFO
//	2006 Dec 05  James M Anderson  --JIVE start




// INCLUDES
#include <stdlib.h>
#include <memory.h>
#include "JMA_code.h"
#include "JMA_math.h"
#include "GPS.h"




// set up a namespace area for stuff.
namespace MIM_PIM {

    GPS_times::GPS_times(const Uint32 N_TIMES, const Real64* const data)
            : NUM_TIMES(N_TIMES), last_index(N_TIMES)
    {
        MJD = new Real64[NUM_TIMES];
        memcpy(MJD, data, NUM_TIMES*sizeof(Real64));
        return;
    }



//_TITLE  get_interpolation_bisect_index-- use bisection to find interp time
    Uint32 GPS_times::get_interpolation_bisect_index(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the lower index which is the best index for a two-point interpolation.

//	This function uses bisection to search for the closest point, so it
//	is not fast when you are frequently asking for nearby points.

//	The user should be calling get_interpolation_index below

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(NUM_TIMES <= 2) return 0;
    Uint32 top = NUM_TIMES -2;
    Uint32 bottom = 0;
    Uint32 index = NUM_TIMES >> 1;
    while(1) {
        if(MJD[index] <= new_MJD) {
            bottom = index;
        }
        else {
            top = index;
        }
        if((top-bottom) <= 1) return (last_index = bottom);
        index = (top+bottom+1) >> 1;
    }
    // Should never get here
    return (last_index = NUM_TIMES);
}




//_TITLE  get_bisect_index-- use bisection to find nearest time
    Uint32 GPS_times::get_bisect_index(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the closest time in absolute value, and returns the index for that
//	time.

//	This function uses bisection to search for the closest point, so it
//	is not fast when you are frequently asking for nearby points.

//	The user should be calling get_nearest_index below

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(NUM_TIMES <= 1) return 0;
    Uint32 top = NUM_TIMES -1;
    Uint32 bottom = 0;
    Uint32 index = NUM_TIMES >> 1;
    while(1) {
        if(MJD[index] <= new_MJD) {
            bottom = index;
        }
        else {
            top = index;
        }
        if((top-bottom) <= 1) break;
        index = (top+bottom+1) >> 1;
    }
    Real64 diff_b = fabs(MJD[bottom] - new_MJD);
    Real64 diff_t = fabs(MJD[top] - new_MJD);

    last_index = (diff_b <= diff_t) ? bottom : top;
    return last_index;
}














//_TITLE  get_interpolation_index --get index for two-point interpolation
    Uint32 GPS_times::get_interpolation_index(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the lower index for interpolation between two points.

//	This function is designed to be called repeatedly with nearby times,
//	so it uses the GPS_times last_index variable to guess about where
//	in the array to search.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(last_index >= NUM_TIMES) return get_interpolation_bisect_index(new_MJD);
    // try going down
    Uint32 index = last_index;
    while(index > 0) {
        if(MJD[index] > new_MJD) index -= 1;
        else break;
    }
    // Check for increasing index
    while(index < NUM_TIMES-2) {
        if(MJD[index+1] <= new_MJD) index += 1;
        else break;
    }
    // Ok, this is it
    return (last_index = index);
}







    


    
    
//_TITLE  get_nearest_index
    Uint32 GPS_times::get_nearest_index(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the closest time in absolute value, and returns the index for that
//	time.

//	This function is designed to be called repeatedly with nearby times,
//	so it uses the GPS_times last_index variable to guess about where
//	in the array to search.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(last_index >= NUM_TIMES) return get_bisect_index(new_MJD);
    Real64 last_Delta = fabs(MJD[last_index] - new_MJD);
    if(last_Delta == 0.0) return last_index;
    // try going down
    Uint32 index = 0;
    if (last_index >= 1)
      index = last_index -1;
    while(index >= 0) {
        Real64 this_Delta = fabs(MJD[index] - new_MJD);
        if(last_Delta > this_Delta) {
            last_index = index;
            last_Delta = this_Delta;
            index -= 1;
        }
        else break;
    }
    // Check for increasing index
    index = last_index +1;
    while(index < NUM_TIMES) {
        Real64 this_Delta = fabs(MJD[index] - new_MJD);
        if(last_Delta > this_Delta) {
            last_index = index;
            last_Delta = this_Delta;
            index += 1;
        }
        else break;
    }
    // Ok, this is it
    return last_index;
}
        



    
    
//_TITLE  get_nearest_index_2 -- alternate search pattern for possible large hops
    Uint32 GPS_times::get_nearest_index_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the closest time in absolute value, and returns the index for that
//	time.

//	This function is designed to be called repeatedly with nearby times,
//	so it uses the GPS_times last_index variable to guess about where
//	in the array to search.

//	This function differs from get_nearest_index in that it deals with
//	large hops better, when the new point is very far from the old one.
//	It is slower for small hops, but fgar faster for big hops.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(last_index >= NUM_TIMES) return get_bisect_index(new_MJD);
    Uint32 bottom = 0;
    Uint32 top = NUM_TIMES-1;
    Uint32 hop_size = 1;
    Uint32 index = last_index;
    // Begin hunt phase
    if(MJD[index] <= new_MJD) {
        while(MJD[index] <= new_MJD) {
            // hunt upward
            bottom = index;
            index += hop_size;
            hop_size <<= 1;
            if(index >= NUM_TIMES) {
                index = NUM_TIMES-1;
                break;
            }
        }
        top = index;
    }
    else {
        while(MJD[index] > new_MJD) {
            // hunt upward
            top = index;
            if(hop_size >= top) {
                index = 0;
                break;
            }
            index -= hop_size;
            hop_size <<= 1;
        }
        bottom = index;
    }
    // Now it is down to bisection
    if((top-bottom) > 1) {
        while(1) {
            index = (top+bottom+1) >> 1;
            if(MJD[index] <= new_MJD) {
                bottom = index;
            }
            else {
                top = index;
            }
            if((top-bottom) <= 1) break;
        }
    }
    if(top == 0) {
        last_index = top;
        return top;
    }
    else if(bottom == NUM_TIMES-1) {
        last_index = bottom;
        return bottom;
    }
    Real64 bottom_diff = fabs(MJD[bottom] - new_MJD);
    Real64 top_diff = fabs(MJD[top] - new_MJD);
    last_index = (bottom_diff <= top_diff) ? bottom : top;
    return last_index;
}
        











//_TITLE  get_index_low_bound --get index of last point no greater than new_MJD
    Uint32 GPS_times::get_index_low_bound(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) const throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the index of the time which is no larger than new_MJD.

//	This function uses bisection to search for the closest point, so it
//	is not fast when you are frequently asking for nearby points.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(NUM_TIMES <= 1) return 0;
    Uint32 top = NUM_TIMES -1;
    Uint32 bottom = 0;
    Uint32 index = NUM_TIMES >> 1;
    while(1) {
        if(MJD[index] <= new_MJD) {
            bottom = index;
        }
        else {
            top = index;
        }
        if((top-bottom) <= 1) return bottom;
        index = (top+bottom+1) >> 1;
    }
    // Should never get here
    return NUM_TIMES;
}

















//_TITLE  get_index_high_bound --get index of first point no less than new_MJD
    Uint32 GPS_times::get_index_high_bound(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 new_MJD   // I  The time to look for, as a MJD
        ) const throw()

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function will search through the MJD time array to find
//	the index of the time which is no smaller than new_MJD.

//	This function uses bisection to search for the closest point, so it
//	is not fast when you are frequently asking for nearby points.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

{
    if(NUM_TIMES <= 1) return 0;
    Uint32 top = NUM_TIMES -1;
    Uint32 bottom = 0;
    Uint32 index = NUM_TIMES >> 1;
    while(1) {
        if(MJD[index] <= new_MJD) {
            bottom = index;
        }
        else {
            top = index;
        }
        if((top-bottom) <= 1) {
            if(MJD[bottom] == new_MJD) return bottom;
            return top;
        }
        index = (top+bottom+1) >> 1;
    }
    // Should never get here
    return NUM_TIMES;
}


























// GLOBALS


// FUNCTIONS



}  // end namespace


