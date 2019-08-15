// ellipsoidal_coord.cc
// code are for making geodetic coordinates
//_HIST  DATE NAME PLACE INFO
// 2005 Jun 11  James M Anderson  --JIVE start




// INCLUDES
#include "JMA_math.h"
#include "ellipsoidal_coord.h"





// make a namespace to hold constants
namespace {
    // Here are some things we should need.  The algorithm and
    // data here are from the _Explanatory Supplement to the Astronomical
    // Almanac_, see section 4.22, and section 4.25

    static const double a[JMA_VEX_AREA::MAX_TYPES] = {
        6378137.0, // m GRS80
        6378137.0  // m WGS84
    };
    static const double f[JMA_VEX_AREA::MAX_TYPES] = {
        3.352810681182318790E-03, // 1.0 / 298.257222101, // GRS80
        3.352810664747480490E-03  // 1.0 / 298.257223563  // WGS84
    };
    static const double b_absolute[JMA_VEX_AREA::MAX_TYPES] = {
        // a*(1.0-f);
        6.356752314140356146E+06, // GRS80
        6.356752314245179296E+06  // WGS84
    };
    static const double e2[JMA_VEX_AREA::MAX_TYPES] = {
        // \sqrt{2f-f^2}
        8.181919104281579203E-02, // GRS80
        8.181919084262148645E-02  // WGS84
    };
    


}



// set up a namespace area for stuff.
namespace JMA_VEX_AREA {




// GLOBALS


// FUNCTIONS


// The main constructor
ellipsoidal_coord::ellipsoidal_coord(const double x,
                                     const double y,
                                     const double z,
                                     const Ellipsoid_Type type
                                     )
{
    // Here are some things we should need.  The algorithm and
    // data here are from the _Explanatory Supplement to the Astronomical
    // Almanac_, see section 4.22, and section 4.25

    const double r = sqrt(x*x+y*y);
    if(r < 0.001) {
        // we are within 1 mm of the pole, so use the simple formulas
        lambda = 0.0;
        phi = copysign(M_PI*0.5,z);
        h = fabs(z) - b_absolute[type];
        return;
    }

    if(fabs(z) < 0.001) {
        // we are within 1 mm of the equator, so use simple stuff
        h = r-a[type];
        phi = 0.0;
    }
    else {
        // we are in the general area of the planet
        
        // Ok, need a b with the same sign as z
        double b = copysign(b_absolute[type], z);

        const double E = (b*z-(a[type]*a[type]-b*b))/(a[type]*r);
        const double F = (b*z+(a[type]*a[type]-b*b))/(a[type]*r);

        const double P = 4.0/3.0*(E*F+1.0);
        const double Q = 2.0*(E*E-F*F);
        
        const double D = P*P*P+Q*Q;
        const double D_sqrt = sqrt(D);

        const double nu = pow(D_sqrt-Q,1.0/3.0) - pow(D_sqrt+Q,1.0/3.0);

        const double G = 0.5*(sqrt(E*E+nu)+E);

        const double t = sqrt(G*G+(F-nu*G)/(2.0*G-E))-G;

        phi = atan(a[type]*(1.0-t*t)/(2.0*b*t));
        h = (r-a[type]*t)*cos(phi)+(z-b)*sin(phi);
    }
    // compute the longitude
    lambda=atan2(y,x);
    

    return;
}



    



double ellipsoidal_coord::get_lambda(void) const
{
    return lambda;
}
double ellipsoidal_coord::get_phi(void) const
{
    return phi;
}
double ellipsoidal_coord::get_longitude(void) const
{
    return lambda * M_RAD2DEG;
}
double ellipsoidal_coord::get_latitude(void) const
{
    return phi * M_RAD2DEG;
}
double ellipsoidal_coord::get_height(void) const
{
    return h;
}









ellipsoidal_to_Cartesian_coord::
ellipsoidal_to_Cartesian_coord(const double latitude, // radians
                               const double longitude,// radians
                               const double height,   // meters above
                                                      // surface
                               const Ellipsoid_Type type
                               )
{
    const double slat = sin(latitude);
    const double clat = cos(latitude);
    const double slon = sin(longitude);
    const double clon = cos(longitude);

//     const double N_phi = a[type] / sqrt(1.0 - e2[type]*slat*slat);
    
//     cart[x_axis] = (N_phi+height)*clat*clon;
//     cart[y_axis] = (N_phi+height)*clat*slon;
//     cart[z_axis] = ((1.0-e2[type])*N_phi+height)*slat;

// 2006 Jul 27  James M Anderson  --try a different set of equations, from
//                                  National Geodetic Survey, XyzWin
//                                  See http://www.ngs.noaa.gov/PC_PROD/XYZWIN/


    const double g0 = 1.0 - f[type];
    const double g1 = a[type] / sqrt(1.0 - e2[type]*e2[type]*slat*slat);

    cart[x_axis] = (g1 + height) *clat*clon;
    cart[y_axis] = (g1 + height) *clat*slon;
    cart[z_axis] = (g1 * g0 * g0 + height) * slat;
    
    return;
}





ellipsoidal_to_Cartesian_coord::
ellipsoidal_to_Cartesian_coord(const ellipsoidal_coord& ell,
                               const Ellipsoid_Type type
                               )
{
    const double slat = sin(ell.get_phi());
    const double clat = cos(ell.get_phi());
    const double slon = sin(ell.get_lambda());
    const double clon = cos(ell.get_lambda());

//     const double N_phi = a[type] / sqrt(1.0 - e2[type]*slat*slat);
    
//     cart[x_axis] = (N_phi+ell.get_height())*clat*clon;
//     cart[y_axis] = (N_phi+ell.get_height())*clat*slon;
//     cart[z_axis] = ((1.0-e2[type])*N_phi+ell.get_height())*slat;

// 2006 Jul 27  James M Anderson  --try a different set of equations, from
//                                  National Geodetic Survey, XyzWin
//                                  See http://www.ngs.noaa.gov/PC_PROD/XYZWIN/

    const double g0 = 1.0 - f[type];
    const double g1 = a[type] / sqrt(1.0 - e2[type]*e2[type]*slat*slat);

    cart[x_axis] = (g1 + ell.get_height()) *clat*clon;
    cart[y_axis] = (g1 + ell.get_height()) *clat*slon;
    cart[z_axis] = (g1 * g0 * g0 + ell.get_height()) * slat;
    
    return;
}




    

}  // end namespace  JMA_VEX_AREA


