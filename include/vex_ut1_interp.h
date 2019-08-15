// vex_ut1_interp.h
// stuff to deal with changing time from UTC to UT1
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start
//	2005 Oct 10  JMA  --if told there are 0 points, then assume zeros
//	2006 Jan 02  JMA  --allow 1 datapoint, as AIPS seems to only store 1
//                          value.  :(



#ifndef VEX_UT1_INTERP_H
#define VEX_UT1_INTERP_H

// INCLUDES
#include "JMA_math.h"
#include <stdlib.h>
#include <time.h>

#include "vex_time.h"





// set up a namespace area for stuff.
namespace JMA_VEX_AREA {



//_CLASS  VEX_UT1_Interp --deal with changing time from UTC to UT1
    class VEX_UT1_Interp {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:
        VEX_UT1_Interp(const Uint32 num_points);
        ~VEX_UT1_Interp();

        void set_MJD(const Uint32 i, const Real64 MJD_i) throw()
            { if(i<NUM_POINTS) MJD[i] = MJD_i; return;}
        void set_X_Polar_Motion(const Uint32 i, const Real64 X_Polar_Motion_i)
            throw()
            { if(i<NUM_POINTS) X_Polar_Motion[i] = X_Polar_Motion_i; return;}
        void set_Y_Polar_Motion(const Uint32 i, const Real64 Y_Polar_Motion_i)
            throw()
            { if(i<NUM_POINTS) Y_Polar_Motion[i] = Y_Polar_Motion_i; return;}
        void set_UT1_UTC(const Uint32 i, const Real64 UT1_UTC_i) throw()
            { if(i<NUM_POINTS) UT1_UTC[i] = UT1_UTC_i; return;}

        void get_interpolation(const Real64 MJD_calc,
                               Real64* X_Polar_Motion_calc,
                               Real64* Y_Polar_Motion_calc,
                               Real64* UT1_UTC_calc) const throw();

        Uint32 get_NUM_POINTS(void) const throw() {return NUM_POINTS;}

        
protected:



private:
        // How many datapoints do we have?
        Uint32 NUM_POINTS;
        // Need an array of modified Julian dates
        Real64* MJD;
        Real64* X_Polar_Motion;
        Real64* Y_Polar_Motion;
        Real64* UT1_UTC;        // UT1 minus UTC values


        // prevent copy and assigment of of an object of this class
        // by placing these guys here.  I don't feel like mucking with the
        // array stuff to allow these.
        VEX_UT1_Interp( const VEX_UT1_Interp& );
        VEX_UT1_Interp& operator=( const VEX_UT1_Interp& );
        


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // VEX_UT1_INTERP_H
