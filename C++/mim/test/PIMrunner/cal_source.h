// cal_source.h
// header file for calibration source class
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE start



#ifndef CAL_SOURCE_H
#define CAL_SOURCE_H

// INCLUDES
#include <stdio.h>
#include "JMA_math.h"





// set up a namespace area for stuff.
namespace MIM_PIM {

    enum Cal_Source_Coordinate_Enum {
        RA_DEC_radians = 0, // all in radians
        RA_DEC_astro=1,     // RA in hours, dec in degrees
        RA_DEC_degrees=2    // RA and Dec in degrees
    };


//_CLASS  name one line description
class Cal_Source {
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



    
    // Constructors
    Cal_Source(void) throw() {return;}
    // This one assumes angles in radians
    Cal_Source(Real64 ra_in, Real64 dec_in, Real64 flux_density_in) throw()
            : ra(ra_in), dec(dec_in), flux_density(flux_density_in)
        {return;}
    Cal_Source(Real64 ra_in, Real64 dec_in, Real64 flux_density_in,
               enum Cal_Source_Coordinate_Enum type) throw();



    Real64 RA(void) const throw() {return ra;}                   // rad
    Real64 Dec(void) const throw() {return dec;}                 // rad
    Real64 FluxDensity(void) const throw() {return flux_density;}// Jy


    // precision controls how to print the angle
    // -2  only degrees (hours)
    // -1  degrees (hours) and minutes
    //  0  degrees (hours) minutes and seconds
    //  1  1 decimal point after seconds
    // ...
    //  9  9 decimal points after seconds
    // separator should be soemthing like ':' or ' '
    // but if it is given as either 'd' or 'h', then the funtion will continue
    // with '\'' and '\"' or 'm' and 's' as necessary
    void print_RA(FILE* fp, Sint8 precision, char separator) const;
    void print_Dec(FILE* fp, Sint8 precision, char separator) const;
    



protected:



private:
    Real64 ra;           // rad
    Real64 dec;          // rad
    Real64 flux_density; // Jy



    void print_generic(FILE* fp, Real64 angle, Sint8 precision, char separator) const;
    


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // CAL_SOURCE_H
