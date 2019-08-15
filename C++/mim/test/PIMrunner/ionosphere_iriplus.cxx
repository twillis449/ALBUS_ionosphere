// ionosphere_iriplus.cxx
// add a lower atmosphere wave to the IRI atmosphere
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 22  James M Anderson  --JIVE  start from ionosphere_iri.cxx
//	2005 Nov 24  JMA  --try not including IRI stuff for a bit.



// INCLUDES
#include <time.h>
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>

#include "ionosphere.h"
#include "ionosphere_iri.h"
#include "ionosphere_iriplus.h"

#include "station_maker.h"


// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



//_TITLE  Ionosphere_IRI_Plus::Electron_Density --add wave to the IRI ionospehre
    Real64 Ionosphere_IRI_Plus::Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
	  const LatLon_Cart& position,
                               // I  The position about the Earth at which to
                               //    evaluate the ionosphere electron density
          const JMA_VEX_AREA::VEX_Time& time
                               // I  The time (UT) at which to evaluate the
                               //    ionosphere
          )

// Real64 Ionosphere_IRI_Plus::Electron_Density
//                                O  The electron density, in m^{-3} at the
//                                   position and time specified.
//	                             Negative values are unphysical, and will
//                                   be used to indicate error problems.
//                                   -2.0E300 nonsense altitude


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	This function generates a lower ionosphere wave to add to the IRI
//	ionosphere.  It will add on an extra electron density of the form
//      \exp\left[ -\left(\frac{h-200 km}{10 km}\right)\right],
//	where h is the height above the mean Earth surface.

//	If you want to have 1 TECU of vertical TEC, then you should use an
//	amplitude value of 10^12/\sqrt(\pi) m^{-3}.  (See JMA notes for
//	2005 Nov 22-1.)

//	The waves are assumed to propagate in the direction Direction with
//	respect to the Azimuth of the position as viewed from the Wave_Center
// 	reference position.  Then the waves move with velocity Velocity
//	along that direction.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 22  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

    {
        //  First, get the Azimuth and radial distance from the wave center
        LatLon_Cart pos2 = Wave_Center.get_polar_offset(position);
        // Also need the height above the Earth's surface
        Real64 Height = position.Elevation();
        // Need some sort of time information
        Real64 t = time - Reference_Time; // result is in seconds

        // Ok, get the cos^2 argument
        Real64 arg = (pos2.Lat() * radius_Earth * cos(pos2.Lon() - Direction)
                      - Velocity * t) / Wavelength * M_PI;
        // Get the cos^2 value
        Real64 c = cos(arg);
        Real64 c2 = c*c;
        // get the exponential part with height
        Real64 Delta = (Height - 200E3) / 10E3;
        Real64 e = exp(-Delta*Delta);

        Real64 wave_density = Amplitude * e * c2;

        // return this plus the IRI part
        return wave_density;// + Ionosphere_IRI::Electron_Density(position,var_time);
    }
    



}  // end namespace


