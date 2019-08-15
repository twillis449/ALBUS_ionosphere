// VTEC.cxx
// handle calculations of getting Vertical TEC (and back to Slant TEC)
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start
//	2005 Nov 07  JMA  --make array versions of the functions
//	2006 Jan 05  JMA  --correct for elevation of the observer




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include "station_reference.h"
#include "station_latlon.h"
#include "observation.h"
#include "VTEC.h"

#include "station_maker.h"



// set up a namespace area for stuff.
namespace MIM_PIM{




// GLOBALS


// FUNCTIONS




//_TITLE  get_simple_VTEC_scaling --get the o_0 term
    Real64 get_simple_VTEC_scaling(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 El,       // I  elevation angle, in rad
        const Real64 Elev,     // I  altitude/elevation of the obsrever, in m
        const Real64 h         // I  height of ionosphere above Earth, in m
        )
// get_simple_VTEC_scaling        O  o_0 scaling factor

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	In addition, this program will calculate a vertical TEC (VTEC) correction
//	for the observation.  This
//	correction here is just a simple scaling factor.  (It will only
//	be applied if the sigma_STEC value is >= 0.0, otherwise the STEC
//	value will be directly copied.)  This scaling factor is a geometrical
//	correction for a curved Earth.  See the JMA 2005 Aug 16 notes, or
//	GPSTK, or Thompson, Moran, Sw???? book on interferometry.

//	This correction factor is given by
//
//	VTEC = o_0 STEC, where
//
//	o_0 = \sqrt{1-o_1^2}, where
//
//	o_1 = \frac{r_\Earth \cos{El}}{r_\Earth + h}, where
//
//	r_Earth is the radius of the Earth, El is the elevation angle, and
//	h is the height of the ionosphere above the surface of the Earth.
//	Note that STEC = VTEC / o_0, and that
//	1/o_0 = S(z) = \frac{1}{\cos\left[\sin^{-1}\left(\frac{r_\Earth \sin{z}}
//                                                 {r_\Earth + h}\right)\right]
//	where z is the zenith angle.  (See the JMA notes).

//	JMA has decided to correct this equation to allow for an observer who
//	is not at sea level.  This should be accomplished by adding the elevation
//	of the observer to the r_\Earth term in the first part of o_1.  That is,
//	o_1 = \frac{\left(r_\Earth + h_\mathrm{obs}\right) \cos{El}}
//                 {r_\Earth + h_\mathrm{iono}}
    

//_FILE  files used and logical units used

//_LIMS  design limitations
//	The elevation angle had better be positive for physical reasons, but
//	this program will not die if it is < 0.

//	However, the height had better be h >= Elev.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson
// 	2006 Jan 05  JMA  --correct for elevation of the observer.

//_END

    {
        Real64 o_1 = ((radius_Earth+Elev) * cos(El)) / (radius_Earth + h);
        Real64 o_0 = sqrt(1.0 - o_1*o_1);
        return o_0;
    }



    






//_TITLE  get_simple_VTEC_scaling --get the o_0 term for a range of heights
    void get_simple_VTEC_scaling(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 El,       // I  elevation angle, in rad
        const Real64 Elev,     // I  altitude/elevation of the obsrever, in m
        const Uint32 NUM_HEIGHTS,// I The number of heights
        const Real64* const h, // I  height of ionosphere above Earth, in m
                               //    as h[NUM_HEIGHTS]
        Real64* const scaling  // O  the o_0 term for each height,
                               //    as scaling[NUM_HEIGHTS]
        )
// get_simple_VTEC_scaling        O  o_0 scaling factor

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	In addition, this program will calculate a vertical TEC (VTEC) correction
//	for the observation.  This
//	correction here is just a simple scaling factor.  (It will only
//	be applied if the sigma_STEC value is >= 0.0, otherwise the STEC
//	value will be directly copied.)  This scaling factor is a geometrical
//	correction for a curved Earth.  See the JMA 2005 Aug 16 notes, or
//	GPSTK, or Thompson, Moran, Sw???? book on interferometry.

//	This correction factor is given by
//
//	VTEC = o_0 STEC, where
//
//	o_0 = \sqrt{1-o_1^2}, where
//
//	o_1 = \frac{r_\Earth \cos{El}}{r_\Earth + h}, where
//
//	r_Earth is the radius of the Earth, El is the elevation angle, and
//	h is the height of the ionosphere above the surface of the Earth.
//	Note that STEC = VTEC / o_0, and that
//	1/o_0 = S(z) = \frac{1}{\cos\left[\sin^{-1}\left(\frac{r_\Earth \sin{z}}
//                                                 {r_\Earth + h}\right)\right]
//	where z is the zenith angle.  (See the JMA notes).

//	JMA has decided to correct this equation to allow for an observer who
//	is not at sea level.  This should be accomplished by adding the elevation
//	of the observer to the r_\Earth term in the first part of o_1.  That is,
//	o_1 = \frac{\left(r_\Earth + h_\mathrm{obs}\right) \cos{El}}
//                 {r_\Earth + h_\mathrm{iono}}

//_FILE  files used and logical units used

//_LIMS  design limitations
//	The elevation angle had better be positive for physical reasons, but
//	this program will not die if it is < 0.

//	However, the height had better be h >= Elev.


//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson
// 	2006 Jan 05  JMA  --correct for elevation of the observer.
// 	2007 Jan 03  JMA  --don't call cos in each loop

//_END

    {
        Real64 cos_El = cos(El);
        for(Uint32 i=0; i < NUM_HEIGHTS; i++) {
            Real64 o_1 = ((radius_Earth+Elev) * cos_El) / (radius_Earth + h[i]);
            Real64 o_0 = sqrt(1.0 - o_1*o_1);
            scaling[i] = o_0;
        }
        return;
    }



    






//_TITLE  find_simple_pierce_point_and_VTEC --
    void find_simple_pierce_point_and_VTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere* const observation,
                               // B  The observations.  
        const Real64 height    // I  the assumed height of the ionosphere
                               //    in a simple 2D model of the ionosphere.
                               //    in meters.
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will calculate a simple geometic pierce point through
//	a fudicial 2D (spherical) ionosphere around an assumed spherical Earth.
//	See JMA notes from 2005 Aug 16 and onward, and GPSTK.  This calculation
//	is made using an assumed height of the ionosphere, input to this
//	function.

//	In addition, this program will calculate a vertical TEC (VTEC) value
//	for the observation, based on the slant TEC (STEC) value.  This
//	correction here is just a simple scaling factor.  (It will only
//	be applied if the sigma_STEC value is >= 0.0, otherwise the STEC
//	value will be directly copied.)  This scaling factor is a geometrical
//	correction for a curved Earth.  See the JMA 2005 Aug 16 notes, or
//	GPSTK, or Thompson, Moran, Sw???? book on interferometry.

//	This correction factor is given by
//
//	VTEC = o_0 STEC, where
//
//	o_0 = \sqrt{1-o_1^2}, where
//
//	o_1 = \frac{r_\Earth \cos{El}}{r_\Earth + h}, where
//
//	r_Earth is the radius of the Earth, El is the elevation angle, and
//	h is the height of the ionosphere above the surface of the Earth.
//	Note that STEC = VTEC / o_0, and that
//	1/o_0 = S(z) = \frac{1}{\cos\left[\sin^{-1}\left(\frac{r_\Earth \sin{z}}
//                                                 {r_\Earth + h}\right)\right]
//	where z is the zenith angle.  (See the JMA notes).

//	JMA has decided to correct this equation to allow for an observer who
//	is not at sea level.  This should be accomplished by adding the elevation
//	of the observer to the r_\Earth term in the first part of o_1.  That is,
//	o_1 = \frac{\left(r_\Earth + h_\mathrm{obs}\right) \cos{El}}
//                 {r_\Earth + h_\mathrm{iono}}

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE  start
// 	2006 Jan 05  JMA  --correct for elevation of the observer.

//_END

    {
        // Ok, for each observation, away we go!
        for(Uint32 obs = 0; obs < NUM_OBSERVATIONS; obs++) {
            // now, if the elevation is below zero, then the VTEC is useless
            if(observation[obs].El < 0.0) {
                // set the VTEC to some magic value
                observation[obs].VTEC = -999.0;
                observation[obs].sigma_VTEC = -1.0;
                // set the pierce point to some place
                observation[obs].pierce_point = LatLon_Cart(0.0,0.0,0.0);
                continue;
            }

            // Now, the the simple pierce point
            observation[obs].pierce_point =
                station[observation[obs].station_number].get_pierce_location(
                    observation[obs].Az,
                    observation[obs].El,
                    height,
                    radius_Earth
                    );
//            printf("obs%6u sta%5u Lat%6.2f Lon%6.2f   Az%7.2f El%5.2f Lat%6.2f Lon%6.2f\n",
//                    obs, observation[obs].station_number,
//                    station[observation[obs].station_number].Lat()*M_RAD2DEG,
//                    station[observation[obs].station_number].Lon()*M_RAD2DEG,
//                    observation[obs].Az*M_RAD2DEG,
//                    observation[obs].El*M_RAD2DEG,
//                    observation[obs].pierce_point.Lat()*M_RAD2DEG,
//                    observation[obs].pierce_point.Lon()*M_RAD2DEG);
            // Ok, correct the slant TEC to vertical at that point
            if(observation[obs].sigma_STEC >= 0.0) {
                const Real64 factor =
                    get_simple_VTEC_scaling(observation[obs].El,
                                            station[observation[obs].station_number].Elevation(),
                                            height);
                observation[obs].VTEC = factor * observation[obs].STEC;
                // the measurement error in the vertical TEC should go
                // down by factor as well, but I think that there is uncertainty
                // with the position and so on that is not accounted for, so
                // leave it alone.
                observation[obs].sigma_VTEC = observation[obs].sigma_STEC;
            }
        }
        return;
    }
    



}  // end namespace


