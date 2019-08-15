// vex_time.h
// holder for vex time information
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start



#ifndef VEX_TIME_H
#define VEX_TIME_H

// INCLUDES
#include "JMA_math.h"
#include <stdlib.h>
#include <time.h>
#include <string>

#include "vex_ut1_interp.h"


#ifdef VEX_TIME_CONSTANTS_H_FILE
#  define NUM(x) =x;
#else
#  define NUM(x)
#endif





// set up a namespace area for stuff.
namespace JMA_VEX_AREA {


    extern const Real64 SECONDS_PER_DAY     NUM(86400.0);
    extern const Real64 MJD_OFFSET          NUM(2400000.5);



    // Need a class before we can define it
    class VEX_UT1_Interp;



//_CLASS  VEX_Time --keep track of time for VEX stuff
    class VEX_Time {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --add some extra functionality for operators

//_END


// NAMESPACE ISSUES    


public:
        // Note: if UT1 is allowed to be NULL, then the UT1-UTC value is assumed
        // to be zero.  This can be useful if you are just looking for
        // a plain epoch time (J2000) or something.
        VEX_Time(Sint32 year_in, Sint32 yday_in,
                 Sint32 hour_in, Sint32 min_in, Real64 sec_in,
                 const VEX_UT1_Interp* const  UT1 = NULL);
        VEX_Time(Sint32 year_in, Sint32 month_in, Sint32 day_in,
                 Sint32 hour_in, Sint32 min_in, Real64 sec_in,
                 const VEX_UT1_Interp* const  UT1 = NULL);
        // This next one assumes UT time and ignores daylight savings.
        VEX_Time(const struct tm& time_in,
                 const VEX_UT1_Interp* const  UT1 = NULL);
        VEX_Time(Real64 jd0_c_in = MJD_OFFSET, Real64 jd1_c_in = 0.0,
                 const VEX_UT1_Interp* const  UT1 = NULL);
        VEX_Time(std::string& VEX_epoch,
                 const VEX_UT1_Interp* const UT1 = NULL);




        // UTC stuff
        Sint32 Year() const throw() {return year;}
        Sint32 Month() const throw() {return month;}
        Sint32 Day() const throw() {return day;}
        Sint32 Yday() const throw() {return yday;}
        Sint32 Hour() const throw() {return hour;}
        Sint32 Min() const throw() {return min;}
        Real64 Sec() const throw() {return sec;}

        Real64 JD0_C() const throw() {return jd0_c;}
        Real64 JD1_C() const throw() {return jd1_c;}

        Real64 Day_Fraction() const throw() {return day_fraction;}
        Real64 Year_Fraction() const throw();

        // UT1 information
        Real64 UT1_UTC() const throw() {return ut1_utc;}

        Real64 JD0_1() const throw() {return jd0_1;}
        Real64 JD1_1() const throw() {return jd1_1;}


        // Note that this loses sub-second accuracy.
        struct tm C_tm_time() const throw();



        // Add some number of seconds to the UTC time.
        VEX_Time& add_offset(const Real64 offset,
                             const VEX_UT1_Interp* const UT1 = NULL);
        



protected:



private:
        // Stuff to keep track of time information

        // UTC information
        Sint32 year; // full year, as in 2005
        Sint32 month; // 1--12
        Sint32 day;   // day of month, 1--31
        Sint32 yday;  // day of year, 1--366
        Sint32 hour;  // 0--23
        Sint32 min;   // 0--59
        Real64 sec;   // 0--61.999999...  (allows for leap seconds, but not
                      //                   yet implemented)

        Real64 jd0_c;   // Julian Day epoch part, see SOFA
        Real64 jd1_c;   // Modified Julian Day part, see SOFA

        Real64 day_fraction; // fraction of a day.

        // UT1 information
        Real64 ut1_utc; // UT1 - UTC, in seconds

        Real64 jd0_1;   // Julian Day epoch part, see SOFA
        Real64 jd1_1;   // Modified Julian Day part, see SOFA



        // stuff to convert between day of year number and month/day and back
        void fill_yday_from_mm_dd(void) throw();
        void fill_mm_dd_from_yday(void) throw();
        // convert hours, minutes, seconds to fraction of a day
        void fill_day_fraction(void) throw();
        void fill_hms_from_fraction(void) throw();
        // Get the Julian Dates.
        void fill_JDs_from_ymd(void);
        void fill_ymd_from_JDs(void);
        // Figure out the UT1 information for the Julian Dates
        void fill_UT1_information(const VEX_UT1_Interp* const UT1);
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS
inline Real64 operator-(const VEX_Time& t0, const VEX_Time& t1)
{
    // returns the difference IN SECONDS for the UTC times
    Real64 diff0 = t0.JD0_C() - t1.JD0_C();
    Real64 diff1 = t0.JD1_C() - t1.JD1_C();
    return (diff0 + diff1)*SECONDS_PER_DAY;
}



}  // end namespace

#undef NUM
#endif // VEX_TIME_H
