// latlon_cart.cxx
// code for LatLon_Cart class of coordinate stuff
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "latlon_cart.h"
#include "station_maker.h" // needed for the radius of the Earth



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    // Constructors
    LatLon_Cart::LatLon_Cart(Real64 lat_in, Real64 lon_in) throw()
            : lat(lat_in), lon(lon_in), radius(radius_Earth)
    {
        convert_to_cartesian();
        return;
    }
    
    LatLon_Cart::LatLon_Cart(Real64 x, Real64 y, Real64 z) throw()
    {
        vector[x_axis] = x;
        vector[y_axis] = y;
        vector[z_axis] = z;
        convert_to_latlon();
        return;
    }

    LatLon_Cart::LatLon_Cart(Real64 lat_in, Real64 lon_in,
                    Real64 elevation, Real64 r_Earth) throw()
            : lat(lat_in), lon(lon_in), radius(r_Earth+elevation)
    {
        convert_to_cartesian();
        return;
    }
    
    LatLon_Cart::LatLon_Cart(const Real64 cart_vector[NUM_3D]) throw()
    {
        vector[x_axis] = cart_vector[x_axis];
        vector[y_axis] = cart_vector[y_axis];
        vector[z_axis] = cart_vector[z_axis];
        convert_to_latlon();
        return;
    }
    
    LatLon_Cart::LatLon_Cart(const Space_Vector& sv) throw()
            : Space_Vector(sv)
    {
        convert_to_latlon();
        return;
    }



    void LatLon_Cart::convert_to_cartesian(void) throw()
    {
        sincos(lat, &slat, &clat);
        sincos(lon, &slon, &clon);

        // Ok now, remember that the latitide is 90\degr - \theta
        vector[x_axis] = clon*clat*radius;
        vector[y_axis] = slon*clat*radius;
        vector[z_axis] = slat*radius;
        return;
    }



    void LatLon_Cart::convert_to_latlon(void) throw()
    {
        Real64 r2 = Space_Vector::Radius_squared();
        radius = sqrt(r2);
        if(r2 > 0.0) {
            lon=atan2(vector[y_axis], vector[x_axis]);
            slat = vector[z_axis]/radius;
            lat = asin(slat);
            sincos(lon, &slon, &clon);
            clat = cos(lat);
        }
        else {
#ifdef DEBUG
            fprintf(stderr, "Error: zero vector in %s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
#endif // DEBUG            
            // a zero vector?  This ought to be an error, and the program
            // ought to barf, but be kind and gentle
            lat = -M_PI_2;
            slat = slon = lon = 0.0;
            clat = -1.0;
            clon = 1.0;
        }
        return;
    }





    Real64 LatLon_Cart::Elevation(void) const throw()
    {
        return radius - radius_Earth;
    }

    

}  // end namespace


