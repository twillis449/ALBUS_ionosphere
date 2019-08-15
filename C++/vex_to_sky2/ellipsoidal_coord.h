// ellipsoidal_coord.h
// transform IRTF geocentric cartesian coordinates to geodetic
// 2005 Jun 11  James M Anderson  --JIVE start
// 2005 Aug 25  JMA  --add a class to go from WGS84 elliptical to Cartesian




#ifndef ELLIPSOIDAL_COORD_H
#define ELLIPSOIDAL_COORD_H 1

// INCLUDES
#include "JMA_math.h"







// set up a namespace area for stuff.
namespace JMA_VEX_AREA {

    enum Ellipsoid_Type {
        GRS80 = 0,
        WGS84 = 1,
        MAX_TYPES = 2
    };


//_CLASS  name one line description
class ellipsoidal_coord  {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:

    // Need a constructor.  Take in the cartesian geocentric coordinates
    // in meters.
    ellipsoidal_coord(const double x,
                      const double y,
                      const double z,
                      const Ellipsoid_Type type = GRS80
                      );
    ellipsoidal_coord() {};


    
    double get_lambda() const; // returns longitude in radians
    double get_phi() const; // returns latitude in radians
    double get_longitude() const; // returns longitude in degrees
    double get_latitude() const; // returns latitude in degrees
    double get_height() const; // returns heigh above ellipsoid in meters



protected:



private:

    double lambda;  // longitude (rad)
    double phi;     // latitude (rad)
    double h;       // height above ellipsoid (m)


    
};









    // I need a constant to describe how many points are in a 3 vector
    const unsigned NUM_3D = 3;





//_CLASS  ellipsoidal_to_Cartesian_coord  --convert WGS84 to Cartesian
class ellipsoidal_to_Cartesian_coord  {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	

//_END


// NAMESPACE ISSUES    


public:
    enum Cart_Vector_Enum {
        x_axis = 0,
        y_axis = 1,
        z_axis = 2
    };

    // Need a constructor.  Take in the cartesian geocentric coordinates
    // in meters.
    ellipsoidal_to_Cartesian_coord(const double latitude, // radians
                                   const double longitude,// radians
                                   const double height,   // meters above
                                                          // surface
                                   const Ellipsoid_Type type = GRS80
                                   );
    ellipsoidal_to_Cartesian_coord(const ellipsoidal_coord& ell,
                                   const Ellipsoid_Type type = GRS80
                                   );



    
    double X(void) const  throw() {return cart[x_axis];}
    double Y(void) const  throw() {return cart[y_axis];}
    double Z(void) const  throw() {return cart[z_axis];}
    const double* const Cart(void) const  throw() {return cart;}
    double Cart(int axis) const  throw() {return cart[axis];}
    double Cart(enum Cart_Vector_Enum axis) const  throw() {return cart[axis];}


protected:



private:

    double cart[NUM_3D]; // the Cartesian unit vector
    
};






    

// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace  JMA_VEX_AREA




#endif // ELLIPSOIDAL_COORD_H
