// ionosphere.cxx
// base code stuff for ionosphere models
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "atmosphere_point.h"
#include "latlon_cart.h"
#include "space_vector.h"
#include "space_unit_vector.h"
#include "station_latlon.h"
#include "station_maker.h"


#include "ionosphere.h"



// External Functions
// Get the IGRF magnetic field around the Earth
extern "C" int jma_igrf13syn(
    double date,  // I  time (years AD, in UT) valid 1900--2025                                 
    double radius,// I  the position radius, in meters                    
    double clatit,// I  the cos(latitude, in radians)                    
    double slatit,// I  the sin(latitude, in radians)                    
    double clongi,// I  the cos(longitude, in radians)                   
    double slongi,// I  the sin(longitude, in radians) 
                  //    only geocentric coordinates are supported */       
    double *x,    // O  the magnetic field in the x direction     
    double *y,    // O  the magnetic field in the y direction             
    double *z__   // O  the magnetic field in the z direction 
                  //   the directions are x--local North on surface 
                  //                      y--local East on surface 
                  //                      z--local down 
    );












// Local Functions for handling integration
namespace {
    using namespace MIM_PIM;

// Many of the functions are based on stuff from Numerical Recipes in C


// Globals for the Integration Polynomial Extrapolatino
static const Sint32 Poly_Interp_K = 5;

// FUNCTION static void iso_polint
// DOCUMENTATION Provide Polynomial interpolation (extrapolation)
// for the isotropic integration routines
void polint(Real64 xa[],
                Real64 ya[],
                Real64 x, Real64 *y, Real64 *dy)
    {
        Sint32 i,m,ns=1;
        Real64 den,dif,dift,ho,hp,w;
        Real64 c[Poly_Interp_K+1],d[Poly_Interp_K+1];

        dif=fabs(x-xa[1]);
        for (i=1;i<=Poly_Interp_K;i++) {
            if ( (dift=fabs(x-xa[i])) < dif) {
                ns=i;
                dif=dift;
            }
            c[i]=ya[i];
            d[i]=ya[i];
        }
        *y=ya[ns--];
        for (m=1;m<Poly_Interp_K;m++) {
            for (i=1;i<=Poly_Interp_K-m;i++) {
                ho=xa[i]-x;
                hp=xa[i+m]-x;
                w=c[i+1]-d[i];
                if ( (den=ho-hp) == 0.0) {
                    fprintf(stderr, "Error in polint\n%S:%d:%s\n",
                            __FILE__,__LINE__, __func__);
                    // fudge some result
                    *y = 0.0;
                    *dy = 1.0;
                }
                den=w/den;
                d[i]=hp*den;
                c[i]=ho*den;
            }
            *y += (*dy=(2*ns+2 < (Poly_Interp_K-m) ? c[ns+1] : d[ns--]));
        }
        return;
    }



// FUNCTION static Real64 trapzd_electron
// DOCUMENTATION This routine handles the trapezoidal integration
// for the electron density.  See Numerical Recipes for
// deeper insight.
Real64 trapzd_electron(
        const Uint32 n,
        const Real64 left, // I the left end of the integral, representing the
                           //   lower range end, im m
        const Real64 right,// I the right end of the integral, representing the
                           //   upper range end, im m
        Ionosphere_Base& ionosphere,
                           // I the ionosphere class
        const Station_LatLon& station,
                           // I  the location of the ground station
        const JMA_VEX_AREA::VEX_Time& time,
                           // I  what time is it?
        const Space_Unit_Vector& direction
                           // I  what direction is being look at
        )
    {
        static Real64 s;
//        fprintf(stdout, "In trapzd_electron\n");

        if (n == 1) {
            // This is the evaluation at the far left and far right.  This
            // makes it easy to calculate cos and sin
            Real64 fl = ionosphere.Electron_Density_Range(
                station,
                time,
                direction,
                left
                );
            Real64 fr = ionosphere.Electron_Density_Range(
                station,
                time,
                direction,
                right
                );
            return (s = 0.5 * (right - left) * (fl+fr));
        }
        else { 
            Real64 x,tnm,sum,del, val;
            Sint32 iterations,j;
            iterations = 0x1 << (n-2);
            tnm = Real64( iterations);
            del = (right-left)/tnm;
            x=left+0.5*del;
            for (sum=0.0,j=0;j<iterations;j++,x+=del) {
                val = ionosphere.Electron_Density_Range(
                    station,
                    time,
                    direction,
                    x
                    );
                sum += val;
//                 if(n > 8) {
//                     LatLon_Cart l=(station+x*direction);
//                     printf("Major %2u %4d %15E %15.2E %15.2E    %15E %15E\n",n,j,x,l.Elevation(),station.Elevation(),val,sum);
//                     if((val > 1E30)||(!finite(val))) {
//                         printf("huge val at height %E\n",l.Elevation());
//                         exit(1);
//                     }
//                 }
            }
            s=0.5*(s+(right-left)*sum/tnm);
            return s; 
        }
    }





// FUNCTION static Real64 trapzd_electron
// DOCUMENTATION This routine handles the trapezoidal integration
// for the electron density.  See Numerical Recipes for
// deeper insight.
Real64 trapzd_Faraday(
        const Uint32 n,
        const Real64 left, // I the left end of the integral, representing the
                           //   lower range end, im m
        const Real64 right,// I the right end of the integral, representing the
                           //   upper range end, im m
        Ionosphere_Base& ionosphere,
                           // I the ionosphere class
        const Station_LatLon& station,
                           // I  the location of the ground station
        const JMA_VEX_AREA::VEX_Time& time,
                           // I  what time is it?
        const Space_Unit_Vector& direction
                           // I  what direction is being look at
        )
    {
        static Real64 s;
//        fprintf(stdout, "In trapzd_Faraday\n");

        if (n == 1) {
            // This is the evaluation at the far left and far right.  This
            // makes it easy to calculate cos and sin
            Space_Vector magnetic_field = ionosphere.Magnetic_Field_Range(
                station, time, direction, left);
            Real64 electron_density = ionosphere.Electron_Density_Range(
                station, time, direction, left);
            Real64 fl = ionosphere.get_Faraday_Measure(electron_density,
                                                       magnetic_field,
                                                       -direction
                                                       );
            
            magnetic_field = ionosphere.Magnetic_Field_Range(
                station, time, direction, right);
            electron_density = ionosphere.Electron_Density_Range(
                station, time, direction, right);
            Real64 fr = ionosphere.get_Faraday_Measure(electron_density,
                                                       magnetic_field,
                                                       -direction
                                                       );
            return (s = 0.5 * (right - left) * (fl+fr));
        }
        else { 
            Real64 x,tnm,sum,del;
            Sint32 iterations,j;
            iterations = 0x1 << (n-2);
            tnm = Real64( iterations);
            del = (right-left)/tnm;
            x=left+0.5*del;
            for (sum=0.0,j=0;j<iterations;j++,x+=del) {
                Space_Vector magnetic_field = ionosphere.Magnetic_Field_Range(
                    station, time, direction, left);
                Real64 electron_density = ionosphere.Electron_Density_Range(
                    station, time, direction, x);
                sum += ionosphere.get_Faraday_Measure(electron_density,
                                                      magnetic_field,
                                                      -direction
                                                      );
            }
            s=0.5*(s+(right-left)*sum/tnm);
            return s; 
        }
    }





    
// FUNCTION static Real64 trapzd_electron
// DOCUMENTATION This routine handles the trapezoidal integration
// for the electron density.  See Numerical Recipes for
// deeper insight.
void trapzd_electron_Faraday(
        const Uint32 n,
        const Real64 left, // I the left end of the integral, representing the
                           //   lower range end, im m
        const Real64 right,// I the right end of the integral, representing the
                           //   upper range end, im m
        Ionosphere_Base& ionosphere,
                           // I the ionosphere class
        const Station_LatLon& station,
                           // I  the location of the ground station
        const JMA_VEX_AREA::VEX_Time& time,
                           // I  what time is it?
        const Space_Unit_Vector& direction,
                           // I  what direction is being look at
        Real64* const electron_integral,
                          //  O  current electron integration
        Real64* const Faraday_integral
                          //  O  current Faraday integration
        )
    {
        static Real64 s_e;
        static Real64 s_f;
//        fprintf(stdout, "In trapzd_electron_Faraday\n");
        if (n == 1) {
            // This is the evaluation at the far left and far right.  This
            // makes it easy to calculate cos and sin
            Space_Vector magnetic_field = ionosphere.Magnetic_Field_Range(
                station, time, direction, left);
            Real64 el = ionosphere.Electron_Density_Range(
                station, time, direction, left);
            Real64 fl = ionosphere.get_Faraday_Measure(el,
                                                       magnetic_field,
                                                       -direction
                                                       );
            magnetic_field = ionosphere.Magnetic_Field_Range(
                station, time, direction, right);
            Real64 er = ionosphere.Electron_Density_Range(
                station, time, direction, right);
            Real64 fr = ionosphere.get_Faraday_Measure(er,
                                                       magnetic_field,
                                                       -direction
                                                       );
            s_e = 0.5 * (right - left) * (el+er);
            s_f = 0.5 * (right - left) * (fl+fr);
            *electron_integral = s_e;
            *Faraday_integral = s_f;
            return;
        }
        else { 
            Real64 x,tnm,sum_e, sum_f,del;
            Sint32 iterations,j;
            iterations = 0x1 << (n-2);
            tnm = Real64( iterations);
            del = (right-left)/tnm;
            x=left+0.5*del;
            for (sum_e=0.0, sum_f=0.0,j=0;j<iterations;j++,x+=del) {
	      //MM: remove bug??
	      //Space_Vector magnetic_field = ionosphere.Magnetic_Field_Range(
	      //    station, time, direction, left);
                Space_Vector magnetic_field = ionosphere.Magnetic_Field_Range(
                    station, time, direction, x);
                Real64 electron_density = ionosphere.Electron_Density_Range(
                    station, time, direction, x);
                sum_e += electron_density;
                sum_f += ionosphere.get_Faraday_Measure(electron_density,
                                                      magnetic_field,
                                                      -direction
                                                      );
            }
            s_e=0.5*(s_e+(right-left)*sum_e/tnm);
            s_f=0.5*(s_f+(right-left)*sum_f/tnm);
            *electron_integral = s_e;
            *Faraday_integral = s_f;
            return; 
        }
    }





    
// FUNCTION static Real64 integrate_electron_density_range
// DOCUMENTATION Handle the integration of the electron density along
// a range of distances, from the low of left tot he high of right.
// Taken from Numerical Recipes.
Real64 integrate_electron_density_range(
        const Real64 left, // I the left end of the integral range, in m
        const Real64 right,// I the right end of the integral range, in m
        Ionosphere_Base& ionosphere,
                           // I the ionosphere class
        const Station_LatLon& station,
                           // I  the location of the ground station
        const JMA_VEX_AREA::VEX_Time& time,
                           // I  what time is it?
        const Space_Unit_Vector& direction
        )
    {
        // If left==right, then the integral must be zero
        if(left==right) return 0.0;
        static const Sint32 JMAX = 20;
    
        Real64 ss,dss;
        Real64 s[JMAX+2],h[JMAX+2];
        int j;



        h[1]=1.0;
        for (j=1;j<=JMAX;j++) {
            s[j]=trapzd_electron(j, left, right, ionosphere,
                                 station, time, direction);
            //if(j>8) printf("DEBUG integrate_electron_density_range %2d %E\n", j, s[j]);fflush(stdout);
            if (j >= Poly_Interp_K) {
                polint(&h[j-Poly_Interp_K],&s[j-Poly_Interp_K],0.0,&ss,&dss);
                //if(j>8) printf("DEBUG integrate_electron_density_range %2d %E        uncert %E\n", j, ss, fabs(dss)/fabs(ss));fflush(stdout);
                if (fabs(dss) <= ionosphere.get_Electron_precision()*fabs(ss)) return ss;
            }
            s[j+1]=s[j];
            h[j+1]=0.25*h[j];

        }
        fprintf(stderr, "Error: Too many steps in integrate_electron_density_range\n%s:%d:%s\n",
                __FILE__,__LINE__,__func__);
        return s[JMAX];

    }






// FUNCTION static Real64 integrate_Faraday_rotation_range
// DOCUMENTATION Handle the integration of the Faraday rotation along
// a range of distances, from the low of left tot he high of right.
// Taken from Numerical Recipes.
Real64 integrate_Faraday_rotation_range(
        const Real64 left, // I the left end of the integral range, in m
        const Real64 right,// I the right end of the integral range, in m
        Ionosphere_Base& ionosphere,
                           // I the ionosphere class
        const Station_LatLon& station,
                           // I  the location of the ground station
        const JMA_VEX_AREA::VEX_Time& time,
                           // I  what time is it?
        const Space_Unit_Vector& direction
        )
    {
        // If left==right, then the integral must be zero
        if(left==right) return 0.0;
        static const Sint32 JMAX = 20;
    
        Real64 ss,dss;
        Real64 s[JMAX+2],h[JMAX+2];
        int j;



        h[1]=1.0;
        for (j=1;j<=JMAX;j++) {
            s[j]=trapzd_Faraday(j, left, right, ionosphere,
                                 station, time, direction);
            //printf("DEBUG integrate_Faraday_rotation_range %2d %E\n", j, s[j]);fflush(stdout);
            if (j >= Poly_Interp_K) {
                polint(&h[j-Poly_Interp_K],&s[j-Poly_Interp_K],0.0,&ss,&dss);
                //printf("DEBUG integrate_Faraday_rotation_range %2d %E        uncert %E\n", j, ss, fabs(dss)/fabs(ss));fflush(stdout);
                if (fabs(dss) <= ionosphere.get_Faraday_precision()*fabs(ss)) return ss;
            }
            s[j+1]=s[j];
            h[j+1]=0.25*h[j];

        }
        fprintf(stderr, "Error: Too many steps in integrate_Faraday_rotation_range\n%s:%d:%s\n",
                __FILE__,__LINE__,__func__);
        return s[JMAX];

    }










// FUNCTION static Real64 integrate_electron_Faraday_range
// DOCUMENTATION Handle the integration of the Faraday rotation along
// a range of distances, from the low of left tot he high of right.
// Taken from Numerical Recipes.
// Seems to be current method used by JMA
void integrate_electron_Faraday_range(
        const Real64 left, // I the left end of the integral range, in m
        const Real64 right,// I the right end of the integral range, in m
        Ionosphere_Base& ionosphere,
                           // I the ionosphere class
        const Station_LatLon& station,
                           // I  the location of the ground station
        const JMA_VEX_AREA::VEX_Time& time,
                           // I  what time is it?
        const Space_Unit_Vector& direction,
                           // I  what direction is being look at
        Real64* const electron_integral,
                          //  O  current electron integration
        Real64* const Faraday_integral
                          //  O  current Faraday integration
        )
    {
        // If left==right, then the integral must be zero
        if(left==right) {
            *electron_integral = *Faraday_integral = 0.0;
        }
        static const Sint32 JMAX = 20;
    
        Real64 ss_e,dss_e;
        Real64 ss_f,dss_f;
        Real64 s_e[JMAX+2],s_f[JMAX+2],h[JMAX+2];
        int j;



        h[1]=1.0;
        for (j=1;j<=JMAX;j++) {
            trapzd_electron_Faraday(j, left, right, ionosphere,
                                    station, time, direction,
                                    &(s_e[j]),&(s_f[j]));
            //printf("DEBUG integrate_electron_Faraday_range %2d %E %E\n", j, s_e[j],s_f[j]);fflush(stdout);
            if (j >= Poly_Interp_K) {
                polint(&h[j-Poly_Interp_K],&s_e[j-Poly_Interp_K],0.0,
                       &ss_e,&dss_e);
                polint(&h[j-Poly_Interp_K],&s_f[j-Poly_Interp_K],0.0,
                       &ss_f,&dss_f);
                //printf("DEBUG integrate_electron_Faraday_range %2d %E        uncert %E\n", j, ss_e, fabs(dss_e)/fabs(ss_e));fflush(stdout);
                if((fabs(dss_e)<= ionosphere.get_Electron_precision()*fabs(ss_e))
                   && (fabs(dss_f)<= ionosphere.get_Faraday_precision()*fabs(ss_f))
                   ) {
                    *electron_integral = ss_e;
                    *Faraday_integral = ss_f;
                    return;
                }
            }
            s_e[j+1]=s_e[j];
            s_f[j+1]=s_f[j];
            h[j+1]=0.25*h[j];

        }
        fprintf(stderr, "Error: Too many steps in integrate_electron_Faraday_range\n%s:%d:%s\n",
                __FILE__,__LINE__,__func__);
        *electron_integral = s_e[JMAX];
        *Faraday_integral = s_f[JMAX];
        return;

    }




    
    
    
}


















// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS







    


//_TITLE  Integrated_Electron_Density --report the integrated electron density
Real64 Ionosphere_Base::Integrated_Electron_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_LatLon& station,
                               // I  the station position (telescope), which
                               //    is used to determine where one is looking
                               //    from.
        const JMA_VEX_AREA::VEX_Time& time, // I  The current time
        const Space_Unit_Vector& direction,
                               // I  The direction in which the telescope is
                               //    looking.
        Real64 altitude_min,
                               //    The minimum altitude used for the
                               //    integration of the electron density to
                               //    column density.  Note that this is the
                               //    *ALTITUDE* above the Earth's mean surface,
                               //    not the minimum range. The altitude is
                               //    expected in meters.
        Real64 altitude_max
                               // I  The maximum altitude of the integration,
                               //    in meters.  Note that GPS satellite
                               //    altitudes are about 22300E3 meters, so
                               //    most models probably will not do well far
                               //    above that.
        )
// Real64 Ionosphere_Base::integrated_electron_density
//                                O  The integrated electron density (hence a
//                                   column density) in m^{-2}.
//                                   If less than 0, then some error has occured.
//                                   -1.0E300 direction below horizon.
//                                   -2.0E300 nonsense altitudes

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This is the base Ionosphere_Base class virtual function to integrate
//      the electron density along a line of sight.  It provides a standard
//      mechanism by which the electron density can be integrated, but
//	derived classes may come up with something else if they are
//	so able (or limited to do so by the available data).

//	This function calculates \int_{\ell_0}^{\ell_1}
//           n_\electron d\ell,
//	where \ell_0 is the range corresponding to altitude_max
//      and \ell_1 is the range corresponding to altitude_min.  (The slightly
//	strange notation here is to help this coincide with the Faraday
//	rotation stuff.)  Since this is just a scalar integrand, I can
//	also integrate going the other direction to get the same answer.

//	This function will first convert the altitudes to ranges.

//	Then, the function will integrate along the line of sight
//	in four stages:
//	Alt_min ALt_max What
//      0       175E3   Lower atmosphere.  The ionosphere here is generally low
//      175E3   500E3   Peak region.  The electron densities are highest
//	500E3   3000E3  High region.  The electron densities are lower, but still
//                                    significant
//      3000E3  \infty  Highest region.  The electron density is very low, but
//                                    there is a long path length.

//	Breaking the integration up into these parts will help to achieve the
//	requested integration precision, as the electron densities in the peak
//	region are order of magnitude above the other regions.  But the high
//	regions have lots of path length.  So, a single numerical integral
//	along the full path would have difficulty getting a good resolution
//	on the peak region, while maintaining a low resolution in the high
//	region, to get the same relative precision in the integration.

//	This routine simply calls some trapezoidal integration stuff from
//	Numerical Recipes.  Improvements for such things as the expected
//	exponential drop-off with heigh in the highest regions is
//	not accounted for in this simple integration task here, but the
//	derived classes are welcome to implement something more exotic.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 16  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
//    fprintf(stdout, "In Integrated_Electron_Density\n");
    // check the input altitudes
#ifdef DEBUG
    if((altitude_min < station.Elevation())
       || (altitude_max <= altitude_min)) {
        fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
                altitude_min, altitude_max,
                __FILE__, __LINE__, __func__);
        return -2.0E300;
    }
#endif
    // check that the direction is above the horizon
    if(station.dot_product(direction) <= 0.0) {
        // direction below horizon.  Return an error, but no warning,
        // as this is not too severe
        return -1.0E300;
    }



    // Hold onto some specific Altitudes of interest for breaking
    // up the integration
    static const Real64 Alt_Low_Boundary = 175E3;
    static const Real64 Alt_Peak_Boundary = 500E3;
    static const Real64 Alt_High_Boundary = 3000E3;
        
        
    // Ok, I need to know the ranges for several altitudes.
    Real64 Range_Alt_Min = station.get_pierce_range(direction,
                                                    altitude_min,
                                                    radius_Earth
                                                    );
    Real64 Range_Alt_Max = station.get_pierce_range(direction,
                                                    altitude_max,
                                                    radius_Earth
                                                    );

    Real64 Range_Low_Boundary = station.get_pierce_range(direction,
                                                         Alt_Low_Boundary,
                                                         radius_Earth
                                                         );
    Real64 Range_Peak_Boundary = station.get_pierce_range(direction,
                                                          Alt_Peak_Boundary,
                                                          radius_Earth
                                                          );
    Real64 Range_High_Boundary = station.get_pierce_range(direction,
                                                          Alt_High_Boundary,
                                                          radius_Earth
                                                          );


    // Ok, there are 4 integration regions.  Get the integrals for each one
    Real64 Integ_Low = 0.0;
    Real64 Integ_Peak = 0.0;
    Real64 Integ_High = 0.0;
    Real64 Integ_Highest = 0.0;

    // Do the low area
    if(altitude_min < Alt_Low_Boundary) {
        Real64 left = Range_Alt_Min;
        Real64 right = (Range_Low_Boundary < altitude_max) ?
            Range_Low_Boundary : Range_Alt_Max;
            
        Integ_Low = integrate_electron_density_range(
            left,
            right,
            *this,
            station,
            time,
            direction
            );
    }
    // Do the Peak area
    if((altitude_min < Alt_Peak_Boundary)
       && (altitude_max > Alt_Low_Boundary)) {
        Real64 left = (altitude_min < Alt_Low_Boundary) ?
            Range_Low_Boundary : Range_Alt_Min;
        Real64 right = (Alt_Peak_Boundary < altitude_max) ?
            Range_Peak_Boundary : Range_Alt_Max;
            
        Integ_Peak = integrate_electron_density_range(
            left,
            right,
            *this,
            station,
            time,
            direction
            );
    }
    // Do the High area
    if((altitude_min < Alt_High_Boundary)
       && (altitude_max > Alt_Peak_Boundary)) {
        Real64 left = (altitude_min < Alt_Peak_Boundary) ?
            Range_Peak_Boundary : Range_Alt_Min;
        Real64 right = (Alt_High_Boundary < altitude_max) ?
            Range_High_Boundary : Range_Alt_Max;
            
        Integ_High = integrate_electron_density_range(
            left,
            right,
            *this,
            station,
            time,
            direction
            );
    }
    // Do the Highest area
    if(altitude_max > Alt_High_Boundary) {
        Real64 left = (altitude_min < Alt_High_Boundary) ?
            Range_High_Boundary : Range_Alt_Min;
        Real64 right = Range_Alt_Max;
            
        Integ_Highest = integrate_electron_density_range(
            left,
            right,
            *this,
            station,
            time,
            direction
            );
    }

    // Add everything together
    Real64 Integ_Total = Integ_Highest + Integ_High + Integ_Low + Integ_Peak;

    return Integ_Total;
}
    










//_TITLE  Integrated_Faraday_Rotation --report the integrated Faraday Rotation
Real64 Ionosphere_Base::Integrated_Faraday_Rotation(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_LatLon& station,
                               // I  the station position (telescope), which
                               //    is used to determine where one is looking
                               //    from.
        const JMA_VEX_AREA::VEX_Time& time, // I  The current time
        const Space_Unit_Vector& direction,
                               // I  The direction in which the telescope is
                               //    looking.
        Real64 altitude_min,
                               //    The minimum altitude used for the
                               //    integration of the electron density to
                               //    column density.  Note that this is the
                               //    *ALTITUDE* above the Earth's mean surface,
                               //    not the minimum range. The altitude is
                               //    expected in meters.
        Real64 altitude_max
                               // I  The maximum altitude of the integration,
                               //    in meters.  Note that GPS satellite
                               //    altitudes are about 22300E3 meters, so
                               //    most models probably will not do well far
                               //    above that.
        )
// Real64 Ionosphere_Base::Integrated_Faraday_Rotation
//                                O  The integrated Faraday rotation stuff
//                                   in T m^{-2}.
//                                   If \leq -1E300, then some error has occured.
//                                   -1.0E300 direction below horizon.
//                                   -2.0E300 nonsense altitudes


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This is the base Ionosphere_Base class virtual function to integrate
//      the Faraday rotation along a line of sight.  It provides a standard
//      mechanism by which the Faraday rotation  can be integrated, but
//	derived classes may come up with something else if they are
//	so able (or limited to do so by the available data).

//	This function calculates \int_{\ell_0}^{\ell_1}
//           n_\electron \mathbold{B \cdot} d\mathbold{\ell},
//	where \ell_0 is the range corresponding to altitude_max
//      and \ell_1 is the range corresponding to altitude_min.  That is, this
//	is an integral with a vector dot product integrand, going along a
//	path from the source to the observer (station).  (The direction
//	along \ell determines the sign of the Faraday rotation.)

//	This function will first convert the altitudes to ranges.

//	Then, the function will integrate along the line of sight
//	in four stages:
//	Alt_min ALt_max What
//      0       175E3   Lower atmosphere.  The ionosphere here is generally low
//      175E3   500E3   Peak region.  The electron densities are highest
//	500E3   3000E3  High region.  The electron densities are lower, but still
//                                    significant
//      3000E3  \infty  Highest region.  The electron density is very low, but
//                                    there is a long path length.

//	Breaking the integration up into these parts will help to achieve the
//	requested integration precision, as the electron densities in the peak
//	region are order of magnitude above the other regions.  But the high
//	regions have lots of path length.  So, a single numerical integral
//	along the full path would have difficulty getting a good resolution
//	on the peak region, while maintaining a low resolution in the high
//	region, to get the same relative precision in the integration.

//	This routine simply calls some trapezoidal integration stuff from
//	Numerical Recipes.  Improvements for such things as the expected
//	exponential drop-off with heigh in the highest regions is
//	not accounted for in this simple integration task here, but the
//	derived classes are welcome to implement something more exotic.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 16  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

    {
//        fprintf(stdout, "In Integrated_Faraday_Rotation\n");
        // check the input altitudes
#ifdef DEBUG
        if((altitude_min < station.Elevation())
           || (altitude_max <= altitude_min)) {
            fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
                    altitude_min, altitude_max,
                    __FILE__, __LINE__, __func__);
            return -2.0E300;
        }
#endif
        // check that the direction is above the horizon
        if(station.dot_product(direction) <= 0.0) {
            // direction below horizon.  Return an error, but no warning,
            // as this is not too severe
            return -1.0E300;
        }


        // Hold onto some specific Altitudes of interest for breaking
        // up the integration
        static const Real64 Alt_Low_Boundary = 175E3;
        static const Real64 Alt_Peak_Boundary = 500E3;
        static const Real64 Alt_High_Boundary = 3000E3;
        
        
        // Ok, I need to know the ranges for several altitudes.
        Real64 Range_Alt_Min = station.get_pierce_range(direction,
                                                        altitude_min,
                                                        radius_Earth
                                                        );
        Real64 Range_Alt_Max = station.get_pierce_range(direction,
                                                        altitude_max,
                                                        radius_Earth
                                                        );

        Real64 Range_Low_Boundary = station.get_pierce_range(direction,
                                                             Alt_Low_Boundary,
                                                             radius_Earth
                                                             );
        Real64 Range_Peak_Boundary = station.get_pierce_range(direction,
                                                              Alt_Peak_Boundary,
                                                              radius_Earth
                                                              );
        Real64 Range_High_Boundary = station.get_pierce_range(direction,
                                                              Alt_High_Boundary,
                                                              radius_Earth
                                                              );


        // Ok, there are 4 integration regions.  Get the integrals for each one
        Real64 Integ_Low = 0.0;
        Real64 Integ_Peak = 0.0;
        Real64 Integ_High = 0.0;
        Real64 Integ_Highest = 0.0;

        // Do the low area
        if(altitude_min < Alt_Low_Boundary) {
            Real64 left = Range_Alt_Min;
            Real64 right = (Range_Low_Boundary < altitude_max) ?
                Range_Low_Boundary : Range_Alt_Max;
            
            Integ_Low = integrate_Faraday_rotation_range(
                left,
                right,
                *this,
                station,
                time,
                direction
                );
        }
        // Do the Peak area
        if((altitude_min < Alt_Peak_Boundary)
           && (altitude_max > Alt_Low_Boundary)) {
            Real64 left = (altitude_min < Alt_Low_Boundary) ?
                Range_Low_Boundary : Range_Alt_Min;
            Real64 right = (Alt_Peak_Boundary < altitude_max) ?
                Range_Peak_Boundary : Range_Alt_Max;
            
            Integ_Peak = integrate_Faraday_rotation_range(
                left,
                right,
                *this,
                station,
                time,
                direction
                );
        }
        // Do the High area
        if((altitude_min < Alt_High_Boundary)
           && (altitude_max > Alt_Peak_Boundary)) {
            Real64 left = (altitude_min < Alt_Peak_Boundary) ?
                Range_Peak_Boundary : Range_Alt_Min;
            Real64 right = (Alt_High_Boundary < altitude_max) ?
                Range_High_Boundary : Range_Alt_Max;
            
            Integ_High = integrate_Faraday_rotation_range(
                left,
                right,
                *this,
                station,
                time,
                direction
                );
        }
        // Do the Highest area
        if(altitude_max > Alt_High_Boundary) {
            Real64 left = (altitude_min < Alt_High_Boundary) ?
                Range_High_Boundary : Range_Alt_Min;
            Real64 right = Range_Alt_Max;
            
            Integ_Highest = integrate_Faraday_rotation_range(
                left,
                right,
                *this,
                station,
                time,
                direction
                );
        }

        // Add everything together
        Real64 Integ_Total = Integ_Highest + Integ_High + Integ_Low + Integ_Peak;

        return Integ_Total;
    }











    










//_TITLE  Integrate_Electron_Faraday --report the integrated electron and Faraday
void Ionosphere_Base::Integrate_Electron_Faraday(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Station_LatLon& station,
                               // I  the station position (telescope), which
                               //    is used to determine where one is looking
                               //    from.
        const JMA_VEX_AREA::VEX_Time& time, // I  The current time
        const Space_Unit_Vector& direction,
                               // I  The direction in which the telescope is
                               //    looking.
        Real64* const electron_integral,
                               // O  The integrated electron column density,
                               //    in m^{-2}
//                                   If less than 0, then some error has occured.
//                                   -1.0 direction below horizon.
//                                   -2.0 nonsense altitudes
        Real64* const electron_error,
                               // O  The integrated electron column density error,
                               //    in m^{-2}
        Real64* const Faraday_integral,
                               // O  The integrated Faraday rotation measure
                               //    in T m^{-2}
        Real64 altitude_min,
                               //    The minimum altitude used for the
                               //    integration of the electron density to
                               //    column density.  Note that this is the
                               //    *ALTITUDE* above the Earth's mean surface,
                               //    not the minimum range. The altitude is
                               //    expected in meters.
        Real64 altitude_max
                               // I  The maximum altitude of the integration,
                               //    in meters.  Note that GPS satellite
                               //    altitudes are about 22300E3 meters, so
                               //    most models probably will not do well far
                               //    above that.
        )


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This is the base Ionosphere_Base class virtual function to integrate
//      both the electron column density and the Faraday rotation
//      along a line of sight.  It provides a standard
//      mechanism by which the two  can be integrated, but
//	derived classes may come up with something else if they are
//	so able (or limited to do so by the available data).

//	This function calculates \int_{\ell_0}^{\ell_1}
//           n_\electron d\ell,
//	where \ell_0 is the range corresponding to altitude_max
//      and \ell_1 is the range corresponding to altitude_min.  (The slightly
//	strange notation here is to help this coincide with the Faraday
//	rotation stuff.)  Since this is just a scalar integrand, I can
//	also integrate going the other direction to get the same answer.

//	This function calculates \int_{\ell_0}^{\ell_1}
//           n_\electron \mathbold{B \cdot} d\mathbold{\ell},
//	where \ell_0 is the range corresponding to altitude_max
//      and \ell_1 is the range corresponding to altitude_min.  That is, this
//	is an integral with a vector dot product integrand, going along a
//	path from the source to the observer (station).  (The direction
//	along \ell determines the sign of the Faraday rotation.)

//	This function will first convert the altitudes to ranges.

//	Then, the function will integrate along the line of sight
//	in four stages:
//	Alt_min ALt_max What
//      0       175E3   Lower atmosphere.  The ionosphere here is generally low
//      175E3   500E3   Peak region.  The electron densities are highest
//	500E3   3000E3  High region.  The electron densities are lower, but still
//                                    significant
//      3000E3  \infty  Highest region.  The electron density is very low, but
//                                    there is a long path length.

//	Breaking the integration up into these parts will help to achieve the
//	requested integration precision, as the electron densities in the peak
//	region are order of magnitude above the other regions.  But the high
//	regions have lots of path length.  So, a single numerical integral
//	along the full path would have difficulty getting a good resolution
//	on the peak region, while maintaining a low resolution in the high
//	region, to get the same relative precision in the integration.

//	This routine simply calls some trapezoidal integration stuff from
//	Numerical Recipes.  Improvements for such things as the expected
//	exponential drop-off with heigh in the highest regions is
//	not accounted for in this simple integration task here, but the
//	derived classes are welcome to implement something more exotic.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 16  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

    {
//        fprintf(stdout, "In Integrate_Faraday_Rotation\n");
        // Set the output to somethng sane
        *electron_integral = *Faraday_integral = 0.0;
        
        // check the input altitudes
#ifdef DEBUG
        if((altitude_min < station.Elevation())
           || (altitude_max <= altitude_min)) {
            fprintf(stderr, "Error: bad altitudes Min %E, Max %E in %s:%d:%s\n",
                    altitude_min, altitude_max,
                    __FILE__, __LINE__, __func__);
            *electron_integral = -2.0E300;
            return;
        }
#endif
        // check that the direction is above the horizon
        if(station.dot_product(direction) <= 0.0) {
            // direction below horizon.  Return an error, but no warning,
            // as this is not too severe
            *electron_integral = -1.0E300;
            return;
        }


        // Hold onto some specific Altitudes of interest for breaking
        // up the integration
        static const Real64 Alt_Low_Boundary = 175E3;
        static const Real64 Alt_Peak_Boundary = 500E3;
        static const Real64 Alt_High_Boundary = 3000E3;
        
        
        // Ok, I need to know the ranges for several altitudes.
        Real64 Range_Alt_Min = station.get_pierce_range(direction,
                                                        altitude_min,
                                                        radius_Earth
                                                        );
        Real64 Range_Alt_Max = station.get_pierce_range(direction,
                                                        altitude_max,
                                                        radius_Earth
                                                        );

        Real64 Range_Low_Boundary = station.get_pierce_range(direction,
                                                             Alt_Low_Boundary,
                                                             radius_Earth
                                                             );
        Real64 Range_Peak_Boundary = station.get_pierce_range(direction,
                                                              Alt_Peak_Boundary,
                                                              radius_Earth
                                                              );
        Real64 Range_High_Boundary = station.get_pierce_range(direction,
                                                              Alt_High_Boundary,
                                                              radius_Earth
                                                              );


        // Ok, there are 4 integration regions.  Get the integrals for each one
        Real64 Integ_Low_e = 0.0;
        Real64 Integ_Peak_e = 0.0;
        Real64 Integ_High_e = 0.0;
        Real64 Integ_Highest_e = 0.0;
        Real64 Integ_Low_f = 0.0;
        Real64 Integ_Peak_f = 0.0;
        Real64 Integ_High_f = 0.0;
        Real64 Integ_Highest_f = 0.0;

        // Do the low area
        if(altitude_min < Alt_Low_Boundary) {
            Real64 left = Range_Alt_Min;
            Real64 right = (Range_Low_Boundary < altitude_max) ?
                Range_Low_Boundary : Range_Alt_Max;
            
            integrate_electron_Faraday_range(
                left,
                right,
                *this,
                station,
                time,
                direction,
                &Integ_Low_e,
                &Integ_Low_f
                );
        }
        // Do the Peak area
        if((altitude_min < Alt_Peak_Boundary)
           && (altitude_max > Alt_Low_Boundary)) {
            Real64 left = (altitude_min < Alt_Low_Boundary) ?
                Range_Low_Boundary : Range_Alt_Min;
            Real64 right = (Alt_Peak_Boundary < altitude_max) ?
                Range_Peak_Boundary : Range_Alt_Max;
            
            integrate_electron_Faraday_range(
                left,
                right,
                *this,
                station,
                time,
                direction,
                &Integ_Peak_e,
                &Integ_Peak_f
                );
        }
        // Do the High area
        if((altitude_min < Alt_High_Boundary)
           && (altitude_max > Alt_Peak_Boundary)) {
            Real64 left = (altitude_min < Alt_Peak_Boundary) ?
                Range_Peak_Boundary : Range_Alt_Min;
            Real64 right = (Alt_High_Boundary < altitude_max) ?
                Range_High_Boundary : Range_Alt_Max;
            
            integrate_electron_Faraday_range(
                left,
                right,
                *this,
                station,
                time,
                direction,
                &Integ_High_e,
                &Integ_High_f
                );
        }
        // Do the Highest area
        if(altitude_max > Alt_High_Boundary) {
            Real64 left = (altitude_min < Alt_High_Boundary) ?
                Range_High_Boundary : Range_Alt_Min;
            Real64 right = Range_Alt_Max;
            
            integrate_electron_Faraday_range(
                left,
                right,
                *this,
                station,
                time,
                direction,
                &Integ_Highest_e,
                &Integ_Highest_f
                );
        }

        // Add everything together
        Real64 Integ_Total_e = Integ_Highest_e + Integ_High_e
            + Integ_Low_e + Integ_Peak_e;
        Real64 Integ_Total_f = Integ_Highest_f + Integ_High_f
            + Integ_Low_f + Integ_Peak_f;

        *electron_integral = Integ_Total_e;
        *Faraday_integral = Integ_Total_f;
        
        return;
    }




    










//_TITLE  Magnetic_Field --get the magnetic field at some point and time
Space_Vector Ionosphere_Base::Magnetic_Field(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const LatLon_Cart& position,
                               // I  the position above the Earth to get the
                               //    magnetic field
        const JMA_VEX_AREA::VEX_Time& time  // I  The current time
        )
// Space_Vector Ionosphere_Base::Magnetic_Field
//                                O  The magnetic field vector, in units of T
//                                   The vector has the standard coordinate
//                                   system with z North, etc., aligned
//	                             with the Earth's axis system.

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This is a virtual function of the Ionosphere_Base class which calculates
//	the magnetic field orientation at a given point in space and time
//	about the Earth.  The function is virtual in case any derived classes
//	wish to use a different manetic field system, more or less
//	precise, or one which takes into account the effects of Solar
//	wind, etc.

//	This function calls the IGRF13-based magnetic field calculation function
//	jma_igrf13syn, which is my C adaptation of the original FORTRAN
//	function approved by the IGRF.  This is a self-contained function,
//	meaning that all data is held internally, with no file access.
//	The original FORTRAN has been modified to accept the sines and cosines
//	of the latitude and longitude, now only accepting geocentric
//	coordinates.  It also uses units of meters and outputs resulting
//	magnetic fields in Tesla.

//	The IGRF stuff produces a magnetic field vector which has a somewhat
//	funny coordinate system.  This function will rotate the coordinate
//	axes to provide the vector in a standard coordinate system, z is North,
//	x is out the equator at 0 longitude.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	The limitations of this routine are all attributable to the IGRF13
//	software limitations.  The IGRF13 code is only valid for time in the
//	range year 1900--2025.  But I will let the IGRF code deal with that.
//	Also, the coordinate system returned by the IGRF code is ill-defined
//	along the Earth's polar axis.  That is, what is the magnetic field
//	above the North pole?  The orientation of the IGRF x,y axes is
//	undefined, as far as I can tell.  At least down is sort of ok.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 16  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

{
//    fprintf(stdout, "In Ionosphere_Base::Magnetic_Field\n");
    // What is the decimal time in years?
    Real64 year_fraction = time.Year() + time.Year_Fraction();
    
    // I need a place to store the magnetic field components
    Real64 x,y,z;

//    fprintf(stdout, "In Ionosphere_Base::Magnetic_Field - calling jma_igrf13syn\n");
//    fprintf(stderr,"Using Position year=%.3f r=%14E Lat=%.3f Lon=%.3f\n",
//              year_fraction, position.Radius(),
//              position.Lat()*M_RAD2DEG,position.Lon()*M_RAD2DEG);
    jma_igrf13syn(year_fraction,
                  position.Radius(),
                  position.cLat(),
                  position.sLat(),
                  position.cLon(),
                  position.sLon(),
                  &x,
                  &y,
                  &z
                  );

//   fprintf(stderr,"Got IGRF13 B field x=%14E y=%14E z=%14E\n",x,y,z);//exit(1);

    // Make a space vector out of the magnetic field.
    Space_Vector IGRF_mag_field(x,y,z);
    
    // Ok, I need at atmospheric point in order to do the coordinate
    // transformation
    Atmosphere_Point a(position);

    // rotate the coordinate ssytem
    Space_Vector Earth_field = a.Rotate_IGRF_Vector(IGRF_mag_field);

//    printf("Got B Field %14E %14E %14E\n", Earth_field.Axis(x_axis),Earth_field.Axis(y_axis),Earth_field.Axis(z_axis));//exit(1);

    // That's it.
    return Earth_field;
}








    

//_TITLE  Electron_and_Magnetic --get the electron density AND magnetic field
void Ionosphere_Base::Electron_and_Magnetic(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const LatLon_Cart& position,
                               // I  the position of the point at which one
                               //    wants to know the electron density and
                               //    magnetic field
        const JMA_VEX_AREA::VEX_Time& time, // I  The current time
        Real64* const electron_density,
                               // O  the electron number density, in m^{-3}
        Space_Vector* const magnetic_field
                               // O  The magnetic field vector, in T
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This is a virtual function which provies the electron number density
//	and the magnetic field vecotr for a specific point in space around the
//	Earth at a specific time.

//	This is the abse function in the base class, which just calls the
//	individual functions for the electron density and magnetic field.

//	Derived classes which might be able to do this more efficiently
//	could make better virtual functions.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 16  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END

    {
        *electron_density = Electron_Density(position, time);
        *magnetic_field = Magnetic_Field(position, time);
        return;
    }
    





    

}  // end namespace


