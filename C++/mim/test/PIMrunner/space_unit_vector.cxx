// space_unit_vector.cxx
// class to describe a Cartesian 3D vector (hence, "normal" space)
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 16  James M Anderson  --JIVE start




// INCLUDES
#include <stdio.h>
#include <stdlib.h>
#include "JMA_math.h"
#include "space_vector.h"
#include "space_unit_vector.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS
    // Constructors
    Space_Unit_Vector::Space_Unit_Vector(const Space_Vector& V) throw()
    {
        vector[x_axis] = V.Axis(x_axis);
        vector[y_axis] = V.Axis(y_axis);
        vector[z_axis] = V.Axis(z_axis);
        initialize_to_unit_vector();
        return;
    }
    
    Space_Unit_Vector::Space_Unit_Vector(Real64 x, Real64 y, Real64 z) throw()
    {
        vector[x_axis] = x;
        vector[y_axis] = y;
        vector[z_axis] = z;
        initialize_to_unit_vector();
        return;}
    Space_Unit_Vector::Space_Unit_Vector(const Real64 space_vector[NUM_3D]) throw()
    {
        vector[x_axis] = space_vector[x_axis];
        vector[y_axis] = space_vector[y_axis];
        vector[z_axis] = space_vector[z_axis];
        initialize_to_unit_vector();
        return;
    };







    // Find the angle between two unit vectors
    Real64 Space_Unit_Vector::Angular_Separation(const Space_Unit_Vector& a)
        const throw()
    {
        // Ok, get the dot product
        Real64 d = this->dot_product(a);
        // Check acos boundary conditions
        if(d > 1.0) d = 1.0;
        else if(d < -1.0) d = -1.0;
        return acos(d);
    }
    

    

    

}  // end namespace


