// atmosphere_point.cxx
// describe a point in space above the Earth along the line of sight for an obs
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 16  James M Anderson  --JIVE start




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "atmosphere_point.h"
#include "latlon_cart.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    // Constructors
    Atmosphere_Point::Atmosphere_Point(Real64 lat_in, Real64 lon_in) throw()
            : LatLon_Cart(lat_in,lon_in)
    {
        calculate_transformation_matrices();
        return;
    }
    Atmosphere_Point::Atmosphere_Point(Real64 x, Real64 y, Real64 z) throw()
            : LatLon_Cart(x,y,z)
    {
        calculate_transformation_matrices();
        return;
    }
    Atmosphere_Point::Atmosphere_Point(const Real64 cart_vector[NUM_3D]) throw ()
            : LatLon_Cart(cart_vector)
    {
        calculate_transformation_matrices();
        return;
    }
    Atmosphere_Point::Atmosphere_Point(const Space_Vector& s) throw()
            : LatLon_Cart(s)
    {
        calculate_transformation_matrices();
        return;
    }
    Atmosphere_Point::Atmosphere_Point(const LatLon_Cart& l) throw()
            : LatLon_Cart(l)
    {
        calculate_transformation_matrices();
        return;
    }



    // See the JMA notes from 2005 Aug 21-1 for the transformation
    // matrix
    void Atmosphere_Point::calculate_transformation_matrices(void) throw()
    {
        IGRF_conversion_matrix.matrix[x_axis][x_axis] = -slat*clon;    
        IGRF_conversion_matrix.matrix[x_axis][y_axis] = -slat*slon;    
        IGRF_conversion_matrix.matrix[x_axis][z_axis] = clat;         
        IGRF_conversion_matrix.matrix[y_axis][x_axis] = -slon;        
        IGRF_conversion_matrix.matrix[y_axis][y_axis] = clon;         
        IGRF_conversion_matrix.matrix[y_axis][z_axis] = 0.0;
        IGRF_conversion_matrix.matrix[z_axis][x_axis] = -clat*clon;   
        IGRF_conversion_matrix.matrix[z_axis][y_axis] = -clat*slon;   
        IGRF_conversion_matrix.matrix[z_axis][z_axis] = -slat;         

        return;
    }



    

//_TITLE  Rotate_IGRF_Vector --rotate Magnetic Field vector from IGRF to (x,y,z)
    Space_Vector Atmosphere_Point::Rotate_IGRF_Vector(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Vector& IGRF_Mag_Field
                               // I  the magnetic field vector from the IGRF
                               //    output at the Atmosphere_Point position
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Aug 25-1, 2005 Sep 16-1.

//	This function takes in a Space_Vector describing the magnetic field
//	as produced by the IGRF functions at the position given by this
//	Atmosphere_Point.  This vector has orientation z-down toward center of
//	Earth, x-horizonatal toward the direction of the North Pole, y-horizontal
//	toward East.

//	This function will rotate the coordinate system of the vector such that
//	the (x,y,z) coordiantes line up witht he standard Earth-centric system.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 16  James M Anderson  --JIVE start

//_END

    {
        // Ok, now run through the coordinate transform
        return (IGRF_Mag_Field * IGRF_conversion_matrix);
    }
    





    
    

}  // end namespace


