// atmosphere_point.h
// describe a point in space above the Earth along the line of sight for an obs
//_HIST  DATE NAME PLACE INFO
// 2005 Aug 26  James M Anderson  --JIVE start
//	2005 Sep 16  JMA  --the base class has evolved to be a full (non-unit)
//                          vector, so make changes here to compensate.
//                          Also, upgrade to the Space_Rotation_Matrix class.



#ifndef ATMOSPHERE_POINT_H
#define ATMOSPHERE_POINT_H

// INCLUDES
#include "JMA_math.h"
#include "latlon_cart.h"
#include "space_vector.h"
#include "space_rotation_matrix.h"




// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  Atmosphere_Point --a point in space above the Earth in the obs path
    class Atmosphere_Point : public LatLon_Cart {
//_DESC  full description of class
//	This class holds information about the position of a point along the
//	line of sight of an observation.  It is intended for use with modeling
//	programs which need to know where one is in space in the ionosphere
//	or above or below for an observation, in order to model the electron
//	density at that point.

//	This class holds information about the full 3D position of the
//	point, and the meaning of x,y, and z is the full vector describing the
//	location.

//	This class also holds information which describes how to convert
//	a magnetic field vector in the orientation of the IGRF system evaluated
//	at this point.  This allows one to rotate this magnetic vector to
//	the coordinate system alighted with the Earth (x,y,z) system.  (The
//	IGRF system has, at some lat, lon, and height, z' pointing down, x'
//	pointing along the horizon toward the North, and y' pointing along the
//	horizon toward the East.)  This is just a simple rotation matrix
//	operation.

        

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
        Atmosphere_Point(Real64 lat_in, Real64 lon_in) throw();//assumes r_\Earth
        Atmosphere_Point(Real64 x, Real64 y, Real64 z) throw();
        Atmosphere_Point(const Real64 cart_vector[NUM_3D]) throw();
        Atmosphere_Point(Real64 lat_in, Real64 lon_in,
                         Real64 elevation, Real64 r_Earth) throw();
        Atmosphere_Point(const Space_Vector& s) throw();
        Atmosphere_Point(const LatLon_Cart& l) throw();



        // Access to height-related information

//_TITLE  Rotate_IGRF_Vector --rotate Magnetic Field vector from IGRF to (x,y,z)
        Space_Vector Rotate_IGRF_Vector(
            const Space_Vector& IGRF_Mag_Field
            ) const throw();
        


protected:



private:
        // a matrix to convert an IGRF magnetic field vector to the cartesian
        // system with North in the direction of Earth's North pole, etc.
        Space_Rotation_Matrix IGRF_conversion_matrix;
        void calculate_transformation_matrices(void) throw();
    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // ATMOSPHERE_POINT_H
