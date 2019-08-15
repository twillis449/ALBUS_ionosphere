// station_latlon.cxx
// code for station position and ionospheric piece point calculation stuff
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start
//	2005 Sep 16  JMA  --changed matrix system for coordinate conversion.




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "station_latlon.h"
#include "latlon_cart.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    // Constructors
Station_LatLon::Station_LatLon(Real64 lat_in, Real64 lon_in) throw()
        : LatLon_Cart(lat_in,lon_in)
{
    calculate_pierce_point_matrix();
    return;
}
Station_LatLon::Station_LatLon(Real64 x, Real64 y, Real64 z) throw()
        : LatLon_Cart(x,y,z)
{
    calculate_pierce_point_matrix();
    return;
}
Station_LatLon::Station_LatLon(Real64 lat_in, Real64 lon_in,
                               Real64 elevation, Real64 r_Earth) throw()
        : LatLon_Cart(lat_in,lon_in,elevation,r_Earth)
{
    calculate_pierce_point_matrix();
    return;
}
Station_LatLon::Station_LatLon(const Real64 cart_vector[NUM_3D]) throw ()
        : LatLon_Cart(cart_vector)
{
    calculate_pierce_point_matrix();
    return;
}
Station_LatLon::Station_LatLon(const LatLon_Cart& latlon) throw()
        : LatLon_Cart(latlon)
{
    calculate_pierce_point_matrix();
    return;
}

Station_LatLon::Station_LatLon(const Space_Vector& sv) throw()
        : LatLon_Cart(sv)
{
    calculate_pierce_point_matrix();
    return;
}


// this one should only be called by new
Station_LatLon::Station_LatLon(void) throw()
        : LatLon_Cart(0.0,0.0)
{
    pierce_matrix.matrix[x_axis][x_axis] = 0.0;
    pierce_matrix.matrix[x_axis][y_axis] = 0.0;
    pierce_matrix.matrix[x_axis][z_axis] = -1.0;
    pierce_matrix.matrix[y_axis][x_axis] = 0.0;
    pierce_matrix.matrix[y_axis][y_axis] = -1.0;
    pierce_matrix.matrix[y_axis][z_axis] = 0.0;
    pierce_matrix.matrix[z_axis][x_axis] = -1.0;
    pierce_matrix.matrix[z_axis][y_axis] = 0.0;
    pierce_matrix.matrix[z_axis][z_axis] = 0.0;

    RADec_matrix.matrix[x_axis][x_axis] = 0.0;
    RADec_matrix.matrix[x_axis][y_axis] = 0.0;
    RADec_matrix.matrix[x_axis][z_axis] = 1.0;
    RADec_matrix.matrix[y_axis][x_axis] = 0.0;
    RADec_matrix.matrix[y_axis][y_axis] = 1.0;
    RADec_matrix.matrix[y_axis][z_axis] = 0.0;
    RADec_matrix.matrix[z_axis][x_axis] = 1.0;
    RADec_matrix.matrix[z_axis][y_axis] = 0.0;
    RADec_matrix.matrix[z_axis][z_axis] = 0.0;

    return;
}




void Station_LatLon::calculate_pierce_point_matrix(void) throw()
{
    pierce_matrix.matrix[x_axis][x_axis] = -slat*clon;
    pierce_matrix.matrix[x_axis][y_axis] = -slat*slon;
    pierce_matrix.matrix[x_axis][z_axis] = clat;
    pierce_matrix.matrix[y_axis][x_axis] = slon;
    pierce_matrix.matrix[y_axis][y_axis] = -clon;
    pierce_matrix.matrix[y_axis][z_axis] = 0.0;
    pierce_matrix.matrix[z_axis][x_axis] = clat*clon;
    pierce_matrix.matrix[z_axis][y_axis] = clat*slon;
    pierce_matrix.matrix[z_axis][z_axis] = slat;

    RADec_matrix.matrix[x_axis][x_axis] = -clon*slat;
    RADec_matrix.matrix[x_axis][y_axis] = -slon;
    RADec_matrix.matrix[x_axis][z_axis] = clon*clat;
    RADec_matrix.matrix[y_axis][x_axis] = -slon*slat;
    RADec_matrix.matrix[y_axis][y_axis] = clon;
    RADec_matrix.matrix[y_axis][z_axis] = slon*clat;
    RADec_matrix.matrix[z_axis][x_axis] = clat;
    RADec_matrix.matrix[z_axis][y_axis] = 0.0;
    RADec_matrix.matrix[z_axis][z_axis] = slat;
    
    return;
}










//_TITLE  get_pierce_location -- get an ionospheric pierce point from a direction
Space_Vector Station_LatLon::get_pierce_location(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 Az,             // I  the azimuth, from North through East, in rad
        const Real64 dist      // I  the angular distance, in rad
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Aug 16-1.  Also GPSTK code for
//	Position Position::getIonosphericPiercePoint

//	This function will calculate the latitude and longitude of a pierce
//	point of a line of sight from a station given an azimuth and
//      an angular distance.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start

//_END

    {
        // Ok, we have the pierce point coordinate transformation matrix
        // and right now I need soemthing which is centered over the NOrth pole.
        // Remember that the Azimuth system is left-handed, so multiply by -1
        // to get a right-handed system.
        LatLon_Cart offset(M_PI_2-dist, -Az);

        // Ok, now run through the coordinate transform
        Space_Vector newcart = offset * pierce_matrix;

        // this is our new position
        return newcart;
    }
















//_TITLE  get_pierce_location -- get an ionospheric pierce point from a direction
Space_Vector Station_LatLon::get_pierce_location(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 Az,       // I  the azimuth, from North through East, in rad
        const Real64 El,       // I  the Elevation above the horizon, in rad
        const Real64 height,   // I  the heigh of the ionosphere above the
                               //    surface, in m
        const Real64 r_Earth   // I  the radius of the surface of the Earth, in m
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Aug 16-1 and 2005 Sep 02.  Also GPSTK code for
//	Position Position::getIonosphericPiercePoint

//	This function will calculate the latitude and longitude of a pierce
//	point of a line of sight from a station through a 2-D ionosphere a height
//	height above the surface of the Earth.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 18  James M Anderson  --JIVE start
//      2005 Sep 02  JMA  --need to think about the radius of the pierce point,
//                          for a full x,y,z position.  See notes from
//                          2005 Sep 02

//_END

    {
#ifdef DEBUG
        // Well, go ahead and check for sanity
        {
            bool bad_flag = false;
            if(r_Earth <= 0.0) {
                bad_flag = true;
                fprintf(stderr, "Error: r_\\Earth too small %E\n", r_Earth);
            }
            if(height < -0.99 * r_Earth) {
                bad_flag = true;
                 fprintf(stderr, "Error: height too small %E\n", height);
            }
            if((bad_flag)) {
                fprintf(stderr, "Error in %s:%d:%s, bailing\n",
                        __FILE__, __LINE__, __func__);
                exit(1);
            }
        }
#endif // DEBUG

        // Let \theta be the angle at the center of the Earth between
        // the station location and the ionospheric pierce point.
        // Let \alpha be the angle between the station and the center of the
        // Earth as seen from the pierce point.  Then, from the fact that
        // the angles of a triangle sum to 180\degr, I have
        // 180\degr = \theta + \alpha + El + 90\degr
        // (the elevation is measured from the horizon, which is at a 90\degr
        // angle from the vertical)
        // So, \theta = 180\degr - 90\degr - El - \alpha.
        // And, from more laws of triangles and side lengths and angles,
        // \sin{\alpha} = \frac{r_\Station \sin(90\degr + El)}{r_\Earth + height}
        // or
        // \alpha = \sin^{-1}\left[\frac{r_\Station \cos{El}}{r_\Earth + height}\right]

        Real64 alpha = asin( (radius * cos(El)) / (r_Earth + height) );

        Real64 theta = M_PI_2 - El - alpha;
        //printf("Got %7.2f %5.2f alpha%E theta%E meters %E\n", Az*M_RAD2DEG,El*M_RAD2DEG,alpha, theta, theta*radius);


        // Ok, we have an azimuth and a distance angle.  Return the
        // pierce point position
        // Ok, we have the pierce point coordinate transformation matrix
        // and right now I need something which is centered over the NOrth pole.
        // Remember that the Azimuth system is left-handed, so multiply by -1
        // to get a right-handed system.
        LatLon_Cart offset(M_PI_2-theta, -Az, height, r_Earth);
//         printf("%12.3f %12.3f %12.3f   %8.3f %8.3f\n",
//                offset.Axis(x_axis),offset.Axis(y_axis),offset.Axis(z_axis),
//                offset.Lat()*M_RAD2DEG, offset.Lon()*M_RAD2DEG);

        // Ok, now run through the coordinate transform
        Space_Vector newcart = offset * pierce_matrix;

        // this is our new position
        return newcart;
    }





//_TITLE  get_pierce_range -- get the linear distance to a pierce point
Real64  Station_LatLon::get_pierce_range(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 El,       // I  the Elevation above the horizon, in rad
        const Real64 height,   // I  the heigh of the ionosphere above the
                               //    surface, in m
        const Real64 r_Earth   // I  the radius of the surface of the Earth, in m
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	See JMA notes from 2005 Aug 16-1 and 2005 Sep 02 and 2005 Sep 05.
//	Also GPSTK code for
//	Position Position::getIonosphericPiercePoint

//	This function will calculate the linear distance (range) to a point
//	located along the line of sight described by the elevation and height
//	above the Earth's radius.



//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE start

//_END

    {
#ifdef DEBUG
        // Well, go ahead and check for sanity
        {
            bool bad_flag = false;
            if(r_Earth <= 0.0) {
                bad_flag = true;
                fprintf(stderr, "Error: r_\\Earth too small %E\n", r_Earth);
            }
            if(height < -0.99 * r_Earth) {
                bad_flag = true;
                 fprintf(stderr, "Error: height too small %E\n", height);
            }
            if((bad_flag)) {
                fprintf(stderr, "Error in %s:%d:%s, bailing\n",
                        __FILE__, __LINE__, __func__);
                exit(1);
            }
        }
#endif // DEBUG

        // Let \theta be the angle at the center of the Earth between
        // the station location and the ionospheric pierce point.
        // Let \alpha be the angle between the station and the center of the
        // Earth as seen from the pierce point.  Then, from the fact that
        // the angles of a triangle sum to 180\degr, I have
        // 180\degr = \theta + \alpha + El + 90\degr
        // (the elevation is measured from the horizon, which is at a 90\degr
        // angle from the vertical)
        // So, \theta = 180\degr - 90\degr - El - \alpha.
        // And, from more laws of triangles and side lengths and angles,
        // \sin{\alpha} = \frac{r_\Station \sin(90\degr + El)}{r_\Earth + height}
        // or
        // \alpha = \sin^{-1}\left[\frac{r_\Station \cos{El}}{r_\Earth + height}\right]

        Real64 cos_El = cos(El);
        Real64 sin_alpha = (radius * cos_El) / (r_Earth + height);
        Real64 alpha = asin( sin_alpha );

        Real64 theta = M_PI_2 - El - alpha;
//        printf("Got %7.2f %5.2f alpha%E theta%E meters %E\n", Az*M_RAD2DEG,El*M_RAD2DEG,alpha, theta, theta*radius);


        // Ok, I now have \theta.  Let's work on the range bit.
        // From the law of sines for triangles, one can work out that
        // \frac{range}{\sin\theta} = \frac{r_\Earth +h}{\cos{El}}
        // = \frac{r_\Earth + elev}{\sin\alpha}
        // So, just rearrange the algebra a bit to get the range.
        // If the
        // elevation is high (approacing 90\degr), then \cos{El} goes to
        // zero, which is dangerous.  If that is the case, then
        // switch over to the other formula from 2005 Sep 14-1
        Real64 range;
        if(El < 1.4) {
            range = (r_Earth + height) * sin(theta) / cos_El;
        }
        else {
            range = (cos(theta) * (r_Earth + height) - radius)
                / sin(El);
        }
        return range;
    }











//_TITLE  get_pierce_ranges -- get the linear distance to a pierce point
void  Station_LatLon::get_pierce_ranges(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const Real64 El,           // I  the Elevation above the horizon, in rad
    const Uint32 NUM_HEIGHTS,  // I  The number of heights to calculate for
    const Real64* const restrict height,
                               // I  The heights desired above the Earth, in m
    const Real64 r_Earth,      // I  The radius of the Earth, in m
    Real64* const restrict range
                               // O  The ranges to the various heights
    ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	See JMA notes from 2005 Aug 16-1 and 2005 Sep 02 and 2005 Sep 05.
//	Also GPSTK code for
//	Position Position::getIonosphericPiercePoint

//	This function will calculate the linear distance (range) to a point
//	located along the line of sight described by the elevation and height
//	above the Earth's radius.



//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Apr 13  James M Anderson  --JIVE start

//_END

{
#ifdef DEBUG
    // Well, go ahead and check for sanity
    {
        bool bad_flag = false;
        if(r_Earth <= 0.0) {
            bad_flag = true;
            fprintf(stderr, "Error: r_\\Earth too small %E\n", r_Earth);
        }
        if(height < -0.99 * r_Earth) {
            bad_flag = true;
            fprintf(stderr, "Error: height too small %E\n", height);
        }
        if((bad_flag)) {
            fprintf(stderr, "Error in %s:%d:%s, bailing\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
        }
    }
#endif // DEBUG

    // Let \theta be the angle at the center of the Earth between
    // the station location and the ionospheric pierce point.
    // Let \alpha be the angle between the station and the center of the
    // Earth as seen from the pierce point.  Then, from the fact that
    // the angles of a triangle sum to 180\degr, I have
    // 180\degr = \theta + \alpha + El + 90\degr
    // (the elevation is measured from the horizon, which is at a 90\degr
    // angle from the vertical)
    // So, \theta = 180\degr - 90\degr - El - \alpha.
    // And, from more laws of triangles and side lengths and angles,
    // \sin{\alpha} = \frac{r_\Station \sin(90\degr + El)}{r_\Earth + height}
    // or
    // \alpha = \sin^{-1}\left[\frac{r_\Station \cos{El}}{r_\Earth + height}\right]

    Real64 cos_El = cos(El);
    for(Uint32 i=0; i < NUM_HEIGHTS; i++) {
        Real64 sin_alpha = (radius * cos_El) / (r_Earth + height[i]);
        Real64 alpha = asin( sin_alpha );
        
        Real64 theta = M_PI_2 - El - alpha;
//        printf("Got %7.2f %5.2f alpha%E theta%E meters %E\n", Az*M_RAD2DEG,El*M_RAD2DEG,alpha, theta, theta*radius);


    // Ok, I now have \theta.  Let's work on the range bit.
    // From the law of sines for triangles, one can work out that
    // \frac{range}{\sin\theta} = \frac{r_\Earth +h}{\cos{El}}
    // = \frac{r_\Earth + elev}{\sin\alpha}
    // So, just rearrange the algebra a bit to get the range.
    // If the
    // elevation is high (approacing 90\degr), then \cos{El} goes to
    // zero, which is dangerous.  If that is the case, then
    // switch over to the other formula from 2005 Sep 14-1
        if(El < 1.4) {
            range[i] = (r_Earth + height[i]) * sin(theta) / cos_El;
        }
        else {
            range[i] = (cos(theta) * (r_Earth + height[i]) - radius)
                / sin(El);
        }
    }
    return;
}

















//_TITLE  get_range_location -- get an ionospheric position from a station range
Space_Vector Station_LatLon::get_range_location(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 Az,       // I  the azimuth, from North through East, in rad
        const Real64 El,       // I  the Elevation above the horizon, in rad
        const Real64 range,    // I  the linear distance from the station along
                               //    the Az, El, in m
        const Real64 r_Earth   // I  the radius of the surface of the Earth, in m
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//	See JMA notes from 2005 Sep 13-1 and 2005 Sep 05 and 2005 Sep 14.
//      Also GPSTK code for
//	Position Position::getIonosphericPiercePoint

//	This function will calculate the latitude and longitude of a range
//	point along line of sight from a station.

//	The mathematics boils down to getting the angle \theta from the Station
//	to the center of the Earth to the range location.  This can be
//	calculated from
//	\tan\theta = \frac{\cos{El}\frac{range}{r_\Station}}
//                   {1+\sin{El}\frac{range}{r_\Station}}

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 05  James M Anderson  --JIVE start

//_END

    {
#ifdef DEBUG
        // Well, go ahead and check for sanity
        {
            bool bad_flag = false;
            if(radius <= 0.0) {
                bad_flag = true;
                fprintf(stderr, "Error: radius too small %E\n", radius);
            }
            if(range < 0.0) {
                bad_flag = true;
                 fprintf(stderr, "Error: height too small %E\n", height);
            }
            if((bad_flag)) {
                fprintf(stderr, "Error in %s:%d:%s, bailing\n",
                        __FILE__, __LINE__, __func__);
                exit(1);
            }
        }
#endif // DEBUG

        // Let \theta be the angle at the center of the Earth between
        // the station location and the ionospheric pierce point.
        // The equation for \tan\tehta is given above, so just use
        // \tan^{-1}\theta

        Real64 cEl = cos(El);
        Real64 sEl = sin(El);
        Real64 fraction = range / radius;
        Real64 numerator = cEl * fraction;
        Real64 denominator = 1.0 + sEl*fraction;

        Real64 theta = atan2(numerator,denominator);

        // Ok, we have an azimuth and a distance angle.  Return the
        // pierce point position
        // Ok, we have the pierce point coordinate transformation matrix
        // and right now I need soemthing which is centered over the North pole.
        // Remember that the Azimuth system is left-handed, so multiply by -1
        // to get a right-handed system.
        // Calculate the height of this position
        Real64 height;
        // If \theta is big, then use the formula from 2005 Sep 13-1
        if(theta > 0.1) {
            height = radius * cEl / sin(M_PI_2 - El - theta) - r_Earth;
        }
        else {
            // use the formula from 2005 Sep 14-1
            height = (range*sEl * (radius-r_Earth) + r_Earth)/cos(theta)
                - r_Earth;
        }
        LatLon_Cart offset(M_PI_2-theta, -Az, height, r_Earth);

        // Ok, now run through the coordinate transform
        Space_Vector newcart = offset * pierce_matrix;

        // this is our new position
        return newcart;
    }
    








    // Stuff for getting pierce locations and so on with vectors
    

//_TITLE  get_pierce_location --get a position in space somewhere from here
Space_Vector Station_LatLon::get_pierce_location(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Unit_Vector& direction,
                               // I  The direction from the station
        const Real64 height,   // I  The height desired above the Earth, in m
        const Real64 r_Earth   // I  The radius of the Earth, in m
        ) const throw()
        
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function gets a position in space, in the form of a Space_Vector
//	which describes the location of a pierce point through the ionosphere
//	from this station, to a point in the direction of direction, which
//	is at a height of height above the surface of the Earth.

//	See the notes from 2005 Sep 14-1.  Also see the notes for similar
//	functions above in this file, which are based on angles.

//	Conceptually, this is rather straightforward vector math.
//	Just calculate \vec{P} = \vec{S} + R\hat{D}
//	for the intercept point.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start

//_END

    {
        Real64 range = get_pierce_range(direction, height, r_Earth);
        return (*this + range*direction);
    }



//_TITLE  get_range_location --get a position in space somewhere from here
Space_Vector Station_LatLon::get_range_location(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Unit_Vector& direction,
                               // I  The direction from the station
        const Real64 range     // I  The linear distance, in m
        ) const throw()
        
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function gets a position in space, in the form of a Space_Vector
//	which describes the location of a pierce point through the ionosphere
//	from this station, to a point in the direction of direction, which
//	is at a height of height above the surface of the Earth.

//	See the notes from 2005 Sep 14-1.  Also see the notes for similar
//	functions above in this file, which are based on angles.

//	Conceptually, this is rather straightforward vector math.
//	Just calculate \vec{P} = \vec{S} + R\hat{D}
//	for the intercept point.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start

//_END

    {
        return (*this + range*direction);
    }



















//_TITLE  get_pierce_range --get a linear distance (range) to a pierce point
Real64 Station_LatLon::get_pierce_range(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Unit_Vector& direction,
                               // I  The direction from the station
        const Real64 height,   // I  The height desired above the Earth, in m
        const Real64 r_Earth   // I  The radius of the Earth, in m
        ) const throw()
        
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	See the notes from 2005 Sep 14-1.  Also see the notes for similar
//	functions above in this file, which are based on angles.

//	Conceptually, this is rather straightforward vector math.  The
//	only real difficulty hear is that we have the heigh, and not the range,
//	to the intercept point.  So, find the range.  Unfortunately, there are
//	*two* possible solutions, as the equation for the range is actually
//	a quadratic formula solution.  So be careful.

//	What we are going to do is to solve
//	\left| \vec{S} + R\hat{D} \right| = r_\Earth + h,
//	where h is the height, \vec{S} is the station position, \hat{D} is
//	the direction unit vector, and R is the unknown range.

//	So
//	R^2 |\hat{D}|^2 + R 2\vec{S}\mathbold{\cdot}\hat{D} + |\vec{S}|^2
//                                                          - (r_\Earth +h)^2 = 0
//	And the challenge is to solve this using the quadratic solution formula.
//	Straightforward.

//	We want the smallest, positive result for R.

//	Note that because this function takes in a unit vector for \hat{D},
//	the quadratic formula can be simplified a bit, because |\hat{D}| == 1

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start

//_END

    {
        // Ok, I want to have some things to keep around.
        const Real64 S2 = Radius_squared();
        const Real64 S_dot_D = dot_product(direction);
        const Real64 S_dot_D_2 = S_dot_D*S_dot_D;
        const Real64 P_radius = r_Earth + height;

        Real64 bracket = S_dot_D_2 - (S2 - P_radius*P_radius);
        // Hey, don't barf too much
        if(bracket < 0.0) bracket = 0.0;

        Real64 RHS = sqrt(bracket);

        // Now, if the direction is above the ground (positive elevation),
        // then \vec{S}\mathbold{\cdot}\hat{D} will be positive.  In this case,
        // our desired result is the + of the \pm
        Real64 range;
        if(S_dot_D >= 0.0) {
            range = -S_dot_D + RHS;
        }
        else {
            range = (-S_dot_D - RHS >= 0.0) ? -S_dot_D - RHS : -S_dot_D + RHS;
        }
        return range;
    }




//_TITLE  get_pierce_ranges --get a linear distance (range) to a pierce point
void Station_LatLon::get_pierce_ranges(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const Space_Unit_Vector& direction,
                               // I  The direction from the station
    const Uint32 NUM_HEIGHTS,  // I  The number of heights to calculate for
    const Real64* const restrict height,
                               // I  The heights desired above the Earth, in m
    const Real64 r_Earth,      // I  The radius of the Earth, in m
    Real64* const restrict range
                               // O  The ranges to the various heights
    ) const throw()
        
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	See the notes from 2005 Sep 14-1.  Also see the notes for similar
//	functions above in this file, which are based on angles.

//	Conceptually, this is rather straightforward vector math.  The
//	only real difficulty hear is that we have the heigh, and not the range,
//	to the intercept point.  So, find the range.  Unfortunately, there are
//	*two* possible solutions, as the equation for the range is actually
//	a quadratic formula solution.  So be careful.

//	What we are going to do is to solve
//	\left| \vec{S} + R\hat{D} \right| = r_\Earth + h,
//	where h is the height, \vec{S} is the station position, \hat{D} is
//	the direction unit vector, and R is the unknown range.

//	So
//	R^2 |\hat{D}|^2 + R 2\vec{S}\mathbold{\cdot}\hat{D} + |\vec{S}|^2
//                                                          - (r_\Earth +h)^2 = 0
//	And the challenge is to solve this using the quadratic solution formula.
//	Straightforward.

//	We want the smallest, positive result for R.

//	Note that because this function takes in a unit vector for \hat{D},
//	the quadratic formula can be simplified a bit, because |\hat{D}| == 1

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Apr 13  James M Anderson  --JIVE  start

//_END

{
    // Ok, I want to have some things to keep around.
    const Real64 S2 = Radius_squared();
    const Real64 S_dot_D = dot_product(direction);
    const Real64 S_dot_D_2 = S_dot_D*S_dot_D;
    for(Uint32 i=0; i < NUM_HEIGHTS; i++) {
        const Real64 P_radius = r_Earth + height[i];

        Real64 bracket = S_dot_D_2 - (S2 - P_radius*P_radius);
        // Hey, don't barf too much
        if(bracket < 0.0) bracket = 0.0;
        
        Real64 RHS = sqrt(bracket);
        
        // Now, if the direction is above the ground (positive elevation),
        // then \vec{S}\mathbold{\cdot}\hat{D} will be positive.  In this case,
        // our desired result is the + of the \pm
        if(S_dot_D >= 0.0) {
            range[i] = -S_dot_D + RHS;
        }
        else {
            range[i] = (-S_dot_D - RHS >= 0.0) ? -S_dot_D - RHS : -S_dot_D + RHS;
        }
    }
    return;
}
    


//_TITLE  name one line description
Real64 Station_LatLon::get_range_height(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Unit_Vector& direction,
                               // I  The direction from the station
        const Real64 range,    // I  The linear distance, in m
        const Real64 r_Earth   // I  The radius of the Earth, in m
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will calculate the height of a point, which is described by
//	\vec{P} = \vec{S} + R\hat{D},
//	where \vec{S} is the position of this station, R is the range,
//	and \hat{D} is the direction unit vector.

//	Actually, this is rather simple vector math, just make a
//	vector P, find it's radius, and subtract the Earth's radius.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//  	2005 Sep 14  James M Anderson  --JIVE  start
//      2005 Nov 07  JMA  --hey, allow for a bit of stuff below sea level

//_END

    {
        Space_Vector P = *this + range*direction;
        Real64 h = P.Radius() - r_Earth;
        // The next was a check for 0, but parts of Holland (and elsewhere) are
        // below sea level, so allow for say 1 km.
        if(h < -1.0E3) h = -1.0E3;
        return h;
    }






//_TITLE  get_range_heights
void Station_LatLon::get_range_heights(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Unit_Vector& direction,
                               // I  The direction from the station
        const Uint32 NUM_RANGES,
                               // I  The number of ranges to calculate for
        const Real64* const restrict range,
                               // I  The linear distance, in m from the station
        const Real64 r_Earth,  // I  The radius of the Earth, in m
        Real64* const restrict height
                               // O  The heights above the Earth in m at the
                               //    specified ranges
        ) const throw()
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will calculate the height of a point, which is described by
//	\vec{P} = \vec{S} + R\hat{D},
//	where \vec{S} is the position of this station, R is the range,
//	and \hat{D} is the direction unit vector.

//	Actually, this is rather simple vector math, just make a
//	vector P, find it's radius, and subtract the Earth's radius.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//  	2007 Apr 13  James M Anderson  --JIVE  start

//_END

{
    for(Uint32 i=0; i < NUM_RANGES; i++) {
        Space_Vector P = *this + range[i]*direction;
        height[i] = P.Radius() - r_Earth;
    }
    
    return;
}












//_TITLE  convert_RADec_to_AltAz --pretty simple, really
Space_Vector Station_LatLon::convert_RADec_to_AltAz(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Space_Vector& RADec
                               // I  The original Hour Angle and Declination
                               //    vector to be converted.  Here, the Hour
                               //    Angle is the Hour Angle from 0 longitude
        ) const throw()

//  Space_Vector Station_LatLon::convert_RADec_to_AltAz
//                                O  The Altitude and Azimuth vector for this
//                                   station

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function converts a vector representing Hour Angle and Declination
//	into Altitude and Azimuth.  See the JMA notes from 2005 Sep 17-1.

//	Use the rotation matrix to rotate about z axis by longitude,
//      then rotate about the y axis by
//	90\degr - lat, then rotate about the z axis by 180\degr, then
//	remember that the Alt-Az system is left-handed.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs
//      DOES NOT CORRECT FOR LST!!!!

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE  start

//_END

    {
        Space_Vector AltAz = RADec * RADec_matrix;
        return AltAz;
    }


    
    

}  // end namespace


