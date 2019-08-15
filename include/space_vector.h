// space_vector.h
// class to describe a Cartesian 3D vector (hence, "normal" space)
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 26  James M Anderson  --JIVE  start
//	2005 Sep 14  JMA  --add some stuff for multiplication and addition



#ifndef SPACE_VECTOR_H
#define SPACE_VECTOR_H

// INCLUDES
#include "JMA_math.h"






// set up a namespace area for stuff.
namespace MIM_PIM {


    enum Space_Vector_Enum {
        x_axis = 0,
        y_axis = 1,
        z_axis = 2,
        NUM_3D = 3
    };
    

    // This needs to know about rotation matrices
    class Space_Rotation_Matrix;
    // And it would be useful to know about unit vectors
    class Space_Unit_Vector;



//_CLASS  Space_Vector --a 3D Cartesian vector for position rotation stuff
    class Space_Vector {
//_DESC  full description of class
//	This class should hold a 3D Cartesian vector (for "normal" space).
//	it will deal with transformations and rotations, and is designed
//	to be a quick holder for a position in space.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


    public:
        Space_Vector(void) throw() {return;} // should only be used inside of
                                             // new[]  ???
        Space_Vector(Real64 x, Real64 y, Real64 z) throw()
            {vector[x_axis] = x; vector[y_axis] = y; vector[z_axis] = z; return;}
        Space_Vector(const Real64 space_vector[NUM_3D]) throw()
            {
                vector[x_axis] = space_vector[x_axis];
                vector[y_axis] = space_vector[y_axis];
                vector[z_axis] = space_vector[z_axis];
                return;
            };
        Space_Vector(const Space_Unit_Vector& U) throw();
        
        // no need for copy or assignement constructor, as this class is simple




        // Access to individual members
        Real64 X(void) const  throw() {return vector[x_axis];}
        Real64 Y(void) const  throw() {return vector[y_axis];}
        Real64 Z(void) const  throw() {return vector[z_axis];}
        const Real64* const Vector(void) const  throw() {return vector;}
        Real64 Axis(Uint32 axis) const  throw() {return vector[axis];}
        Real64 Axis(Sint32 axis) const  throw() {return vector[axis];}
        Real64 Axis(enum Space_Vector_Enum axis) const  throw()
            {return vector[axis];}


        // a bit of computational stuff
        Real64 Radius_squared(void) const throw()
            { return (vector[x_axis]*vector[x_axis]
                      + vector[y_axis]*vector[y_axis]
                      + vector[z_axis]*vector[z_axis]); }
        Real64 Radius(void) const throw() {return sqrt(Radius_squared());}
        Real64 Magnitude_squared(void) const throw() {return Radius_squared();}
        Real64 Magnitude(void) const throw() {return Radius();}



        // deal with multiplying a vector and a matrix
        inline friend Space_Vector operator*(const Space_Vector& A,
                                             const Space_Rotation_Matrix& B);
        inline Space_Vector& operator*=(const Space_Rotation_Matrix& A);
        // deal with multiplying by a scalar
        inline Space_Vector& operator*=(const Real64 A) throw();
        inline Space_Vector& operator/=(const Real64 A) throw();
        // deal with addition
        inline Space_Vector& operator+=(const Space_Vector& A) throw();
        inline Space_Vector& operator-=(const Space_Vector& A) throw();

        // Dot product
        inline Real64 dot_product(const Space_Vector& A) const throw();


        // Make a unit vector out of this one.
        // Note, the user will need to include space_unit_vector.h
        inline Space_Unit_Vector make_unit_vector(void) const throw();
        

        

    protected:
        // give derived classes easy access to the raw data
        Real64 vector[NUM_3D]; // the Cartesian vector



    private:
    


    
    };


// CLASS FUNCTIONS
    inline Space_Vector& Space_Vector::operator*=(const Real64 A) throw()
    {
        vector[x_axis] *= A;
        vector[y_axis] *= A;
        vector[z_axis] *= A;
        return *this;
    }
    inline Space_Vector& Space_Vector::operator/=(Real64 A) throw()
    {
        A = 1.0/A;
        vector[x_axis] *= A;
        vector[y_axis] *= A;
        vector[z_axis] *= A;
        return *this;
    }
    inline Space_Vector& Space_Vector::operator+=(const Space_Vector& A) throw()
    {
        vector[x_axis] += A.Axis(x_axis);
        vector[y_axis] += A.Axis(y_axis);
        vector[z_axis] += A.Axis(z_axis);
        return *this;
    }
    inline Space_Vector& Space_Vector::operator-=(const Space_Vector& A) throw()
    {
        vector[x_axis] -= A.Axis(x_axis);
        vector[y_axis] -= A.Axis(y_axis);
        vector[z_axis] -= A.Axis(z_axis);
        return *this;
    }


    // Dot product
    inline Real64 Space_Vector::dot_product(const Space_Vector& A) const throw()
    {
        Real64 sum =
            + vector[x_axis]*A.Axis(x_axis)
            + vector[y_axis]*A.Axis(y_axis)
            + vector[z_axis]*A.Axis(z_axis);
        return sum;
    }
     
        



// HELPER FUNCTIONS
    inline Space_Vector operator*(const Real64 A,
                                  const Space_Vector& B) throw()
    {
        Space_Vector C(B);
        return (C *= A);
    }
    inline Space_Vector operator*(const Space_Vector& A,
                                  const Real64 B) throw()
    {
        Space_Vector C(A);
        return (C *= B);
    }
    inline Space_Vector operator/(const Space_Vector& A,
                                  const Real64 B) throw()
    {
        Space_Vector C(A);
        return (C /= B);
    }
    inline Space_Vector operator+(const Space_Vector& A,
                                  const Space_Vector& B) throw()
    {
        Space_Vector C(A);
        return (C += B);
    }
    inline Space_Vector operator-(const Space_Vector& A,
                                  const Space_Vector& B) throw()
    {
        Space_Vector C(A);
        return (C -= B);
    }

    // negation and unary positive
    inline Space_Vector operator-(const Space_Vector& A) throw()
    {
        Space_Vector C(-A.Axis(x_axis),
                       -A.Axis(y_axis),
                       -A.Axis(z_axis)
                       );
        return C;
    }
    inline Space_Vector& operator+(Space_Vector& A) throw()
    {
        return A;
    }
    
        



}  // end namespace

#endif // SPACE_VECTOR_H
