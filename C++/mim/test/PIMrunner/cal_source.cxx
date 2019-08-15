// cal_source.cxx
// implementation code for cal_soruce stuff
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anderson  --JIVE  start




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "cal_source.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS

    Cal_Source::Cal_Source(Real64 ra_in, Real64 dec_in, Real64 flux_density_in,
                           enum Cal_Source_Coordinate_Enum type) throw()
            : ra(ra_in), dec(dec_in), flux_density(flux_density_in)
    {
        switch(type) {
            case RA_DEC_radians: break;
            case RA_DEC_astro: // convert RA from hours to degrees
                ra *= 15.0;
                // Intentionally fall into the next case
            case RA_DEC_degrees:
                ra *= M_DEG2RAD;
                dec *= M_DEG2RAD;
                break;
            default:
                // whoa! we should never end up here
#ifdef DEBUG
                fprintf(stderr, "Error: unknown coordinate type in %s:%d:%s with %d\n",
                        __FILE__< __LINE__, __func__, int(type));
                exit(1);
#endif
                break;
        }
        return;
    }





//_TITLE  print_generic --print off a generic angle
    void Cal_Source::print_generic(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        FILE* fp,              // I  file to print to
        Real64 angle,          // I  angle to print, must be in hours or degrees
        Sint8 precision,       // I  precision
                               // -2  only degrees (hours)
                               // -1  degrees (hours) and minutes
                               //  0  degrees (hours) minutes and seconds
                               //  1  1 decimal point after seconds
                               // ...
                               //  9  9 decimal points after seconds
                               // all other values will cause the program to die
        char separator         // separator should be soemthing like ':' or ' '
    // but if it is given as either 'd' or 'h', then the funtion will continue
    // with '\'' and '\"' or 'm' and 's' as necessary
        ) const
    

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function prints out the angle to a file, in a standard astronomical
// 	method.

//	Note that this is the generic printer, and assumes that the angle
//	is non-negative.  For declination, the caller is assumed to take
//	care of + or - printing
        

//_FILE  files used and logical units used

//_LIMS  design limitations
//	angle is assumed to be non-negative

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END

    {
        static const char print_format[] = "%02.0f";
        
#ifdef DEBUG
        // check the value of precision
        if((precision < -2) || (precision > 9)) {
            fprintf(stderr, "Error: bad printing precision %d found at %s:%d:%s",
                    precision, __FILE__, __LINE__, __func__);
            exit(1);
        }
#endif //DEBUG

        // make a copy of the separator
        char my_separator = separator;
        
        // Ok, I know that I have to print out the first bit.
        Real64 i = trunc(angle);
        fprintf(fp, print_format, i);

        // Do the minutes get printed out?
        if(precision >= -1) {
            // print out the separator too
            fputc(my_separator, fp);
            
            // check separator
            if(separator == 'd') my_separator = '\'';
            else if(separator == 'h') my_separator = 'm';
            angle = (angle-i)*60.0;
            i = trunc(angle);
            fprintf(fp, print_format, i);

            // How about seconds?
            if(precision >= 0) {
                // print out the separator too
                fputc(my_separator, fp);
            
                // check separator
                if(separator == 'd') my_separator = '\"';
                else if(separator == 'h') my_separator = 's';
                angle = (angle-i)*60.0;
                char sprint_format[16];
                sprintf(sprint_format,"%%0%d.%df",3+precision,int(precision));
                fprintf(fp, sprint_format, angle);
                if((separator == 'd') || (separator == 'h'))
                    fputc(my_separator, fp);
            }
            else if((separator == 'd') || (separator == 'h'))
                fputc(my_separator, fp);
        }
        else if((separator == 'd') || (separator == 'h'))
            fputc(my_separator, fp);
        return;
    }
    
    void Cal_Source::print_RA(FILE* fp, Sint8 precision, char separator) const
    {
        // Just check that RA is actually >= 0
        Real64 ra_temp = ra;
        if(ra_temp < 0.0) ra_temp += 2.0*M_PI;
        print_generic(fp, ra_temp*M_RAD2DEG/15.0, precision, separator);
        return;
    }
    void Cal_Source::print_Dec(FILE* fp, Sint8 precision, char separator) const
    {
        // print a + or - sign
        char sign = (dec >= 0.0) ? '+' : '-';
        fputc(sign, fp);
        print_generic(fp, fabs(dec*M_RAD2DEG), precision, separator);
        return;
    }



}  // end namespace


