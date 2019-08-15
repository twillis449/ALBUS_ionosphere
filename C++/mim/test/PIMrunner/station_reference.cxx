// station_reference.cxx
// code for station reference point and coordinate transforms about that point
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "station_reference.h"
#include "station_latlon.h"
#include "latlon_cart.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    // Constructors
    Station_Reference::Station_Reference(Real64 lat_in, Real64 lon_in) throw()
            : Station_LatLon(lat_in,lon_in)
    {
        calculate_transformation_matrices();
        return;
    }
    Station_Reference::Station_Reference(Real64 x, Real64 y, Real64 z) throw()
            : Station_LatLon(x,y,z)
    {
        calculate_transformation_matrices();
        return;
    }
    Station_Reference::Station_Reference(Real64 lat_in, Real64 lon_in,
                                         Real64 elevation, Real64 r_Earth)
        throw()
            : Station_LatLon(lat_in,lon_in,elevation,r_Earth)
    {
        calculate_transformation_matrices();
        return;
    }
    Station_Reference::Station_Reference(const Real64 cart_vector[NUM_3D]) throw ()
            : Station_LatLon(cart_vector)
    {
        calculate_transformation_matrices();
        return;
    }
    Station_Reference::Station_Reference(const Station_LatLon& s) throw()
            : Station_LatLon(s)
    {
        calculate_transformation_matrices();
        return;
    }



    void Station_Reference::calculate_transformation_matrices(void) throw()
    {
        equatorial_matrix.matrix[x_axis][x_axis] = clon*clat;    
        equatorial_matrix.matrix[x_axis][y_axis] = -slon;
        equatorial_matrix.matrix[x_axis][z_axis] = -clon*slat;         
        equatorial_matrix.matrix[y_axis][x_axis] = slon*clat;        
        equatorial_matrix.matrix[y_axis][y_axis] = clon;         
        equatorial_matrix.matrix[y_axis][z_axis] = -slon*slat;          
        equatorial_matrix.matrix[z_axis][x_axis] = slat;   
        equatorial_matrix.matrix[z_axis][y_axis] = 0.0;
        equatorial_matrix.matrix[z_axis][z_axis] = clat;         

        polar_matrix.matrix[x_axis][x_axis] = clon*slat;
        polar_matrix.matrix[x_axis][y_axis] = -slon;
        polar_matrix.matrix[x_axis][z_axis] = clon*clat;
        polar_matrix.matrix[y_axis][x_axis] = slon*slat;
        polar_matrix.matrix[y_axis][y_axis] = clon;
        polar_matrix.matrix[y_axis][z_axis] = slon*clat;
        polar_matrix.matrix[z_axis][x_axis] = -clat;
        polar_matrix.matrix[z_axis][y_axis] = 0.0;
        polar_matrix.matrix[z_axis][z_axis] = slat;
 
        return;
    }








//_TITLE  get_Cartesian_fudge_offset --get an offset in a Cartesian system
    LatLon_Cart Station_Reference::get_Cartesian_fudge_offset(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const LatLon_Cart& pos // I  the offset position on the globe
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Aug 17.

//	This function will take a (lat,lon) position, and give an offset
//	from the central reference position to the provided location, in a 
//	grossly simple cartesian coordinate system.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start

//_END

    {
        // Now try to make a simple set of coordinates.  But check that
        // these coordinates are within a sensible range.  Be careful!  As
        // This is an offset, it could be \pm\pi in either lat or lon, not
        // pi/2 for lat.
        Real64 newLat = pos.Lat() - lat;
        Real64 newLon = pos.Lon() - lon;

        // This should never happen
#ifdef DEBUG        
        if(fabs(newLat) > M_PI) {
            newLat = fmod(newLat,2.0*M_PI);
            while(newLat > M_PI) newLat -= 2.0*M_PI;
            while(newLat < -M_PI) newLat += 2.0*M_PI;
        }
#endif // DEBUG
        if(fabs(newLon) > M_PI) {
            newLon = fmod(newLon,2.0*M_PI);
            while(newLon > M_PI) newLon -= 2.0*M_PI;
            while(newLon < -M_PI) newLon += 2.0*M_PI;
        }

        // this is our new position
        return LatLon_Cart(newLat, newLon);
    }


//_TITLE  get_polar_offset --get an offset from the central position in polar
    LatLon_Cart Station_Reference::get_polar_offset(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Vector& pos// I  the offset position on the globe
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Aug 17, 2005 Sep 16

//	This function will take a (lat,lon) position, and give an offset
//	fromthe centra reference position to the provided location, in a polar
//	coordinate system.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start

//_END

    {
        // Ok, now run through the coordinate transform
        Space_Vector newcart = pos * polar_matrix;

        // This will give us a new position at the pole.
        LatLon_Cart newlatlon(newcart);
        // But what I really want is not a latitude, but \theta
        return LatLon_Cart(M_PI_2-newlatlon.Lat(),newlatlon.Lon());
    }



//_TITLE  get_equatorial_offset --get an offset from the central position in equatorial
    Space_Vector Station_Reference::get_equatorial_offset(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Vector& pos// I  the offset position on the globe
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Aug 17, 2005 Sep 16.

//	This function will take a (lat,lon) position, and give an offset
//	from the centra reference position to the provided location, in an equatorial
//	coordinate system.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start

//_END

    {
        // Ok, now run through the coordinate transform
        Space_Vector newcart = pos * equatorial_matrix;

        // this is our new position
        return newcart;
    }
    







    
    

}  // end namespace


