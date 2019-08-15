// station_reference.h
// describe a station location on the Earth for the central reference point
//_HIST  DATE NAME PLACE INFO
// 2005 Aug 18  James M Anderson  --JIVE start



#ifndef STATION_REFERENCE_H
#define STATION_REFERENCE_H

// INCLUDES
#include "JMA_math.h"
#include "latlon_cart.h"
#include "station_latlon.h"
#include "space_rotation_matrix.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  Station_Reference --the reference location for a group of antennas
    class Station_Reference : public Station_LatLon {
//_DESC  full description of class
//	This class holds information about the position of a radio station
//	on an idealized spherical Earth.  It is based ont he LatLon_Cart class,
//	which is a right handed unit-vector representation of a direction.

//	This class is intended for use with stations, that is, antennas.  The
//	stations will be looking at sources in the sky in certain directions
//	(Alt, Az) or (RA, Dec) or (HA, Dec).  This set of code is intended for
//	examination of the Ionosphere, and so it is optimized for calculating
//	the latitude and longitude of the 2-D pierce poitns of the lines of sight
//	through the ionosphere.  Therefore, a matrix is held in storage
//	to facilitate coordinate transformations for sky coordinates to a
//	ionospheric lat,lon coordinate.

//	This particular class is intended for the primary reference location
//	for a group of antennas.  In addition to the normal stuff in a
//	Station_LatLon class, this class will contain extra coordinate
//	rotation matrices with which one can convert lat and lon positions
//	into other coordinate systems, such as a polar system centered at the
//	reference station, or an equatoral system centered at the reference, etc.

        

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
        Station_Reference(Real64 lat_in, Real64 lon_in) throw();
        Station_Reference(Real64 x, Real64 y, Real64 z) throw();
        Station_Reference(Real64 lat_in, Real64 lon_in,
                          Real64 elevation, Real64 r_Earth) throw();
        Station_Reference(const Real64 cart_vector[NUM_3D]) throw();
        Station_Reference(const Station_LatLon& s) throw();



        // Ok, give a simple fudge to an offset Cartesian format
        LatLon_Cart get_Cartesian_fudge_offset(const LatLon_Cart& pos)
            const throw();
        // Ok, how about a polar mapping based on the reference center
        LatLon_Cart get_polar_offset(const Space_Vector& pos)
            const throw();
        // Ok, how about an equatorial mapping based on the reference center
        Space_Vector get_equatorial_offset(const Space_Vector& pos)
            const throw();
        


protected:



private:
        // a matrix to convert a LatLon_Cart position to a polar coordinate
        // system centered at the reference center.
        // This matrix will perform a coordinate transformation equivalent to
        // rotate about z axis by lon
        // rotate about y axis by 90\degr - lat
        Space_Rotation_Matrix polar_matrix;
        // a matrix to convert a LatLon_Cart position to an equatorial coordinate
        // system centered at the reference center.
        // This matrix will perform a coordinate transformation equivalent to
        // rotate about z axis by lon
        // rotate about y axis by -lat
        Space_Rotation_Matrix equatorial_matrix;



        void calculate_transformation_matrices(void) throw();
    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // STATION_REFERENCE_H
