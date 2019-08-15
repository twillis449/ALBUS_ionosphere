// latlon_cart.h
// describe a location on the Earth
//_HIST  DATE NAME PLACE INFO
// 2005 Aug 18  James M Anderson  --JIVE start
// 2005 Aug 26  JMA  --make a daughter class of Space_Vector
// 2005 Sep 02  JMA  --Hey, what about the elevation?  Some VLBI stations
//                     are more than 1 km above sea level



#ifndef LATLON_CART_H
#define LATLON_CART_H

// INCLUDES
#include "JMA_math.h"
#include "space_vector.h"




// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  LatLon_Cart --spherical Earth location (lat, lon) and (x,y,z) holder
    class LatLon_Cart : public Space_Vector {
//_DESC  full description of class
//	This class holds information about the direction of a position
//	on, or above (or below) an idealized spherical surface.  It holds
//	a latitude and longitude of a point on a sphere, such as the surface
//	of the Earth (idealized).  It also contains a Space_Vector (3D Cartesian)
//	vector which describes the same direction.  This vector format will
//	probably be useful for conversion between coordinate systems.

//	Note that this direction is maintained in a right-handed coordiante
//	system.  In other words, East longitudes are positive, and West negative.

//	Also note that this class assumes that angles are in units of radians,
//      lengths are in units of meters.

        

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
        LatLon_Cart(void) throw() {return;} // should only be used
                                            // within a new[] call.
        LatLon_Cart(Real64 lat_in, Real64 lon_in) throw(); // Assumes r_\Earth
        LatLon_Cart(Real64 x, Real64 y, Real64 z) throw();
        LatLon_Cart(Real64 lat_in, Real64 lon_in,
                    Real64 elevation, Real64 r_Earth) throw();
        LatLon_Cart(const Real64 cart_vector[NUM_3D]) throw();
        LatLon_Cart(const Space_Vector& sv) throw();


        Real64 Lat(void) const  throw() {return lat;};
        Real64 Lon(void) const  throw() {return lon;};
        Real64 cLat(void) const  throw() {return clat;};
        Real64 sLat(void) const  throw() {return slat;};
        Real64 cLon(void) const  throw() {return clon;};
        Real64 sLon(void) const  throw() {return slon;};
        Real64 Radius(void) const  throw() {return radius;};
        Real64 Radius_squared(void) const throw() {return radius*radius;};
        const Real64* const Cart(void) const  throw() {return vector;}
        Real64 Cart(int axis) const  throw() {return vector[axis];}
        Real64 Cart(unsigned axis) const  throw() {return vector[axis];}
        Real64 Cart(enum Space_Vector_Enum axis) const  throw()
            {return vector[axis];}

        Real64 Elevation(void) const throw(); // Elevation above Earth radius
        

        

protected:
        Real64 lat; // the lattitude
        Real64 lon; // the longitude
        Real64 clat, slat; // cosine and sine of the latitude
        Real64 clon, slon; // cosine and sine of the longitude
        Real64 radius; // the radial distance from the center, in meters


private:



        // initilization functions
        void convert_to_cartesian(void) throw();
        void convert_to_latlon(void) throw();
    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // LATLON_CART_H
