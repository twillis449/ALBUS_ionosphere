// station_latlon.h
// describe a station location on the Earth
//_HIST  DATE NAME PLACE INFO
// 2005 Aug 18  James M Anderson  --JIVE start



#ifndef STATION_LATLON_H
#define STATION_LATLON_H

// INCLUDES
#include "JMA_math.h"
#include "latlon_cart.h"
#include "space_vector.h"
#include "space_unit_vector.h"
#include "space_rotation_matrix.h"




// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  Station_LatLon --the location of a station (antenna) on the ground
    class Station_LatLon : public LatLon_Cart {
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
        Station_LatLon(Real64 lat_in, Real64 lon_in) throw();
        Station_LatLon(Real64 x, Real64 y, Real64 z) throw();
        Station_LatLon(Real64 lat_in, Real64 lon_in,
                       Real64 elevation, Real64 r_Earth) throw();
        Station_LatLon(const Real64 cart_vector[NUM_3D]) throw();
        Station_LatLon(const LatLon_Cart& latlon) throw();
        Station_LatLon(const Space_Vector& sv) throw();
        // This one should only be used by new[].
        Station_LatLon(void) throw();



        // Given an azimuth and an angular distance, compute a
        // pierce point LatLon_Cart
        // both of these need to be in radians
        // This one assumes that the pierce point is at the surface of the Earth
        Space_Vector get_pierce_location(Real64 Az, const Real64 dist)
            const throw();
        // height is for ionosphere above ground, radius is the radius of the
        // Earth, both in m.  Also needs Azimuth and Elevation, in rad.
        Space_Vector get_pierce_location(const Real64 Az, const Real64 El,
                                         const Real64 height,
                                         const Real64 r_Earth)
            const throw();
        Real64 get_pierce_range(const Real64 El,
                                const Real64 height,
                                const Real64 r_Earth)
            const throw();
        void get_pierce_ranges(const Real64 El,
                               const Uint32 NUM_HEIGHTS,
                               const Real64* const restrict height,
                               const Real64 r_Earth,
                               Real64* const restrict range) const throw();
        // range is the distance (in meters) from the station to a point
        // linearly along the line of sight
        Space_Vector get_range_location(const Real64 Az, const Real64 El,
                                        const Real64 range,
                                        const Real64 r_Earth)
            const throw();

        // Ok, now handle all of this with just vectors.
        // Note that the Space_Unit_Vector& direction is a
        // unit vector.  Range is the linear distance to the point,
        // height is the height above the surface of the Earth.
        Space_Vector get_pierce_location(const Space_Unit_Vector& direction,
                                         const Real64 height,
                                         const Real64 r_Earth) const throw();
        Space_Vector get_range_location(const Space_Unit_Vector& direction,
                                        const Real64 range) const throw();
        Real64 get_pierce_range(const Space_Unit_Vector& direction,
                                const Real64 height,
                                const Real64 r_Earth) const throw();
        void get_pierce_ranges(const Space_Unit_Vector& direction,
                               const Uint32 NUM_HEIGHTS,
                               const Real64* const restrict height,
                               const Real64 r_Earth,
                               Real64* const restrict range) const throw();
        Real64 get_range_height(const Space_Unit_Vector& direction,
                                const Real64 range,
                                const Real64 r_Earth) const throw();
        void get_range_heights(const Space_Unit_Vector& direction,
                               const Uint32 NUM_RANGES,
                               const Real64* const restrict range,
                               const Real64 r_Earth,
                               Real64* const restrict height) const throw();
                                


        // Convert a HA,Dec (from 0 longitude) vector to Alt,Az
        Space_Vector convert_RADec_to_AltAz(const Space_Vector& RADec)
            const throw();
        
        


protected:



private:
        // a matrix to convert an azimuth and angular distance to
        // a lat,lon pierce location.  This matrix will perform a coordinate
        // transformation equivalent to
        // rotate about z axis by 180\degr (so Az= 0 points along +x axis)
        // rotate about y axis by lat - 90\degr
        // rotate about z axis by -lon
        Space_Rotation_Matrix pierce_matrix;
        // A matrix to convert RA and Declination to Altitude(Elevation)
        // and Azimuth.
        // This matrix will
        // rotate about z axis by lon
        // rotate about y axis by 90\degr - lat (so z axis is pointing up)
        // rotate about z axis by 180\degr (so Az=0 points North)
        // Then the system is converted to a left-handed system for Alt-Az,
        // by multiplying the y-axis matrix area by -1.
        // Note that at the moment, no correction is made for the LST!!!!
        Space_Rotation_Matrix RADec_matrix;


        void calculate_pierce_point_matrix(void) throw();
    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // STATION_LATLON_H
