// FILENAME.cxx
// What this header file is for
//_HIST  DATE NAME PLACE INFO




// INCLUDES
#include "JMA_math.h"
#include "location.h"




// Globals only this file needs to know about
namespace {
    // See GPSTK software package:
         /// This class represents the geodetic model defined in NIMA
         /// TR8350.2, "Department of Defense World Geodetic System 1984".
         /// Defined in TR8350.2, Appendix A.1
         /// @return semi-major axis of Earth in meters.
   
    const Real64 radius_Earth = 6378137.0; // meters
}




// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS


//_TITLE  name one line description
    void Earth_location::add_logarythmic_spiral_pattern(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_ARMS, // I  total number of arms
        const Real64 alpha,    // I  the initial angle offset for arm 0 (rad)
        const Real64 a,        // I  the spiral tightness parameter (unitless)
        const Real64 beta,     // I  arm angle spacing (rad)
        const Real64 b0,       // I  initial baseline distance from center (m)
        const Uint32 m,        // I  how many antennas down the arm? (0 based)
        const Uint32 n         // I  which arm? (0 based)
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  
        
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 17  James M Anderson  --JIVE start

//_END

{
    // we actually need the absolute value of beta
    const Real64 abeta = fabs(beta);
    
#ifdef DEBUG
    {
        bool bad_param = false;
        if((NUM_ARMS <= 0) || (NUM_ARMS > 100)) {
            fprintf(stderr, "Error: bad number of arms %d\n", NUM_ARMS);
            bad_param = true;
        }
        if((alpha <= -M_PI) || (alpha > M_PI)) {
            fprintf(stderr, "Error: bad initial angle offset %E\n", alpha);
            bad_param = true;
        }
        if((a < 0.01) || (a > 100.0)) {
            fprintf(stderr, "Error: bad spiral tightness parameter %E\n", a);
            bad_param = true;
        }
        if((abeta*M_RAD2DEG < 0.1) || (abeta > M_PI)) {
            fprintf(stderr, "Error: bad arm angle spacing %E\n", beta);
            bad_param = true;
        }
        if((b0 < 1.0) || (b0 > 1E5)) {
            fprintf(stderr, "Error: bad initial baseline distance %E\n", b0);
            bad_param = true;
        }
        if(m > 1000) {
            fprintf(stderr, "Error: too many antennas %d on an arm\n", m);
            bad_param = true;
        }
        if(n >= NUM_ARMS) {
            fprintf(stderr, "Error: too many arms %d when limit %d\n", n, NUM_ARMS);
            bad_param = true;
        }
        if((bad_param)) exit(2);
    }
#endif // DEBUG    
        
//     // Ok, I am not going to check that the input parameters are within range
//         const Uint32 NUM_ARMS, // I  total number of arms
//         const Real64 alpha,    // I  the initial angle offset for arm 0
//         const Real64 a,        // I  the spiral tightness parameter
//         const Real64 beta,     // I  arm angle spacing
//         const Real64 b0,       // I  the initial baseline distance from center
//         const Uint32 m,        // I  how many antennas down the arm? (0 based)
//         const Uint32 n         // I  which arm? (0 based)



    // Ok, calculate the radius and angle (r and \phi) in the 2D plane for
    // the logarythmic spiral
    Real64 r = b0 * exp(a*abeta*m);
    Real64 phi = beta*m + 2.0*M_PI*n/Real64(NUM_ARMS) + alpha;

    // Now, this is going to be mapped onto a sphere.  See the Explanatory
    // Supplement to the Astronomical Almanac (of some other book which deals
    // with spherical astronomy), for rotations.

    // Make a zenith angle and an azimuth.  For the zenith angle, just use
    // r / r_\Earth.
    Real64 Z = r / radius_Earth;
    Real64 AZ = phi;

    // First, the convention for the azimuth is an angle from North through
    // East, which is a left-handed coordinate system.  Convert to a right-
    // handed one (change the sign of AZ above).  Second, rotate around the
    // z axis by 180 degrees to have the start pointing along the positive
    // x-axis.  Then, rotate about the y
    // axis by 90\degr - latitude.  Then rotate about the z axis by the
    // longitude of the source.  This will give the (x,y,z) position for
    // the latitude and longitude of a station offset from the
    // central position given by this current location
}




}  // end namespace


