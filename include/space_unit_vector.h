// space_unit_vector.h
// class to describe a Cartesian 3D unit vector (hence, "normal" space)
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start
//	2007 Jan 02  JMA  --add something to get angle between two unit vectors



#ifndef SPACE_UNIT_VECTOR_H
#define SPACE_UNIT_VECTOR_H

// INCLUDES
#include "JMA_math.h"
#include "space_vector.h"
#include "MC_direction.h"






// set up a namespace area for stuff.
namespace MIM_PIM {


//_CLASS  Space_Unit_Vector --a 3D Cartesian unit vector for position stuff
    class Space_Unit_Vector : public Space_Vector {
//_DESC  full description of class
//	This class should hold a 3D Cartesian unit vector (for "normal" space).
//	This is basically just a Space_Vector which has Radius==1

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
        Space_Unit_Vector(const Space_Vector& V) throw();
        Space_Unit_Vector(Real64 x, Real64 y, Real64 z) throw();
        Space_Unit_Vector(const Real64 space_vector[NUM_3D]) throw();
        Space_Unit_Vector(const JMA_MONTE_CARLO_GEOMETRY::MC_Direction d) throw()
            {
                vector[x_axis] = d.get_u();
                vector[y_axis] = d.get_v();
                vector[z_axis] = d.get_w();
                return;                
            };
        
        // Overload the radius stuff
        Real64 Radius_squared(void) const throw() {return 1.0;}
        Real64 Radius(void) const throw() {return 1.0;}



        // Overload the make unit vector part too
        inline Space_Unit_Vector make_unit_vector(void) const throw();

        // Find the angle between two unit vectors
        Real64 Angular_Separation(const Space_Unit_Vector& a) const throw();
        

        

    protected:



    private:
        Space_Unit_Vector(void) {};  // Make sure it's impossible to just
        // create one of these guys out of thin air

        // Make sure we can't multiply or add or divide or subtract
        // to ourself.  Overload these functions, and do not provide 
        // deal with multiplying by a scalar
        Space_Vector& operator*=(const Real64 A);
        Space_Vector& operator/=(const Real64 A);
        // deal with addition
        Space_Vector& operator+=(const Space_Vector& A);
        Space_Vector& operator-=(const Space_Vector& A);


        // Actually handle the work of forcing this to be a unit vector
        inline void initialize_to_unit_vector(void) throw();

    


    
    };


// CLASS FUNCTIONS
    // Actually handle the work of forcing this to be a unit vector
    inline void Space_Unit_Vector::initialize_to_unit_vector(void) throw()
    {
        // get the radius squared
        Real64 r2 = vector[x_axis]*vector[x_axis]
            + vector[y_axis]*vector[y_axis]
            + vector[z_axis]*vector[z_axis];
        if(r2 > 0.0) {
            if(r2 != 1.0) {
                // get the inverse of the radius
                Real64 r_m1 = 1.0 / sqrt(r2);
                vector[x_axis] *= r_m1;
                vector[y_axis] *= r_m1;
                vector[z_axis] *= r_m1;
            }
            else {
                // Hey, we're already a unit vector
            }
        }
        else {
            // a zero vector won't do!
            // Fake something
            vector[x_axis] =1.0;
            vector[y_axis] =0.0;
            vector[z_axis] =0.0;
        }
        return;
    }
     
        




    // Make a unit vector out of this one.  Hey, we already are ....
    inline Space_Unit_Vector Space_Unit_Vector::make_unit_vector(void)
        const throw()
    {
        return (Space_Unit_Vector(*this));
    }


    // Now, here are some functions for the Space_Vector part

    inline Space_Vector::Space_Vector(const Space_Unit_Vector& U) throw()
    {
        vector[x_axis] = U.Axis(x_axis);
        vector[y_axis] = U.Axis(y_axis);
        vector[z_axis] = U.Axis(z_axis);
        return;
    }
    
    // Make a unit vector out of this one
    inline Space_Unit_Vector Space_Vector::make_unit_vector(void) const throw()
    {
        return (Space_Unit_Vector(*this));
    }

    

// HELPER FUNCTIONS
    
        



}  // end namespace

#endif // SPACE_UNIT_VECTOR_H
