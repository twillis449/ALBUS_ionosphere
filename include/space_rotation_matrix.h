// space_rotation_matrix.h
// stuff for rotation matrices for dealing with Cartesian Space_Vector's 
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 26  James M Anderson  --JIVE  start



#ifndef SPACE_ROTATION_MATRIX_H
#define SPACE_ROTATION_MATRIX_H

// INCLUDES
#include "JMA_math.h"
#include "space_vector.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  Space_Rotation_Matrix --rotate a Cartesian vector
    class Space_Rotation_Matrix {
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
        // Constructors
        // The axis type is the axis about which the rotation takes place
        Space_Rotation_Matrix(void) throw() {return;}
        Space_Rotation_Matrix(const Real64 angle,
                              const enum Space_Vector_Enum type) throw()
            {
                Real64 cos_angle, sin_angle;
                sincos(angle, &sin_angle, &cos_angle);
                *this = Space_Rotation_Matrix(cos_angle, sin_angle, type);
            };
        Space_Rotation_Matrix(const Real64 cos_angle, const Real64 sin_angle,
                              enum Space_Vector_Enum type) throw();


        // give access to the maxtrix
        Real64 Matrix(Uint32 axis1, Uint32 axis2) const throw()
            {return matrix[axis1][axis2];};
        Real64 Matrix(Sint32 axis1, Sint32 axis2) const throw()
            {return matrix[axis1][axis2];};
        Real64 Matrix(enum Space_Vector_Enum axis1,
                      enum Space_Vector_Enum axis2) const throw()
            {return matrix[axis1][axis2];};
        const Real64* const Matrix(void) const throw() {return &(matrix[0][0]);}



        // deal with multiplying a vector and a matrix
        inline friend Space_Vector operator*(const Space_Vector& A,
                                             const Space_Rotation_Matrix& B);

        // matrix matrix multiplication
        inline Space_Rotation_Matrix& operator*=(const Space_Rotation_Matrix& A);


        // keep the data public, as many classes which have this as an
        // object will want to set values directly
        Real64 matrix[NUM_3D][NUM_3D];
protected:



private:


    
    };


// CLASS FUNCTIONS



// HELPER FUNCTIONS
    // Deal with multiplying matrices
    inline Space_Rotation_Matrix operator*(const Space_Rotation_Matrix& A,
                                           const Space_Rotation_Matrix& B)
    {
        Space_Rotation_Matrix C;
        for(int i=0; i < NUM_3D; i++) {
            for(int j=0; j < NUM_3D; j++) {
                Real64 sum = 0.0;
                for(int k=0; k < NUM_3D; k++) {
                    sum += A.matrix[i][k] * B.matrix[k][j];
                }
                C.matrix[i][j] = sum;
            }
        }
        return C;
    }

    // matrix matrix multiplication
    inline Space_Rotation_Matrix& Space_Rotation_Matrix::operator*=(const Space_Rotation_Matrix& A)
    {
        Space_Rotation_Matrix B(*this);
        for(int i=0; i < NUM_3D; i++) {
            for(int j=0; j < NUM_3D; j++) {
                Real64 sum = 0.0;
                for(int k=0; k < NUM_3D; k++) {
                    sum += B.matrix[i][k] * A.matrix[k][j];
                }
                matrix[i][j] = sum;
            }
        }
        return *this;
    }
    
                
    // deal with multiplying a vector and a matrix
    inline Space_Vector operator*(const Space_Vector& A,
                                  const Space_Rotation_Matrix& B)
    {
        Space_Vector C;
//         for(int i=0; i < NUM_3D; i++) {
//             Real64 sum = 0.0;
//             for(int j=0; j < NUM_3D; j++) {
//                 sum += B.matrix[j][i] * A.vector[j];
//             }
//             C.vector[i] = sum;
//         }
        C.vector[x_axis] = 0.0;
        C.vector[y_axis] = 0.0;
        C.vector[z_axis] = 0.0;

        C.vector[x_axis] += A.vector[x_axis] * B.matrix[x_axis][x_axis];
        C.vector[y_axis] += A.vector[x_axis] * B.matrix[x_axis][y_axis];
        C.vector[z_axis] += A.vector[x_axis] * B.matrix[x_axis][z_axis];

        C.vector[x_axis] += A.vector[y_axis] * B.matrix[y_axis][x_axis];
        C.vector[y_axis] += A.vector[y_axis] * B.matrix[y_axis][y_axis];
        C.vector[z_axis] += A.vector[y_axis] * B.matrix[y_axis][z_axis];

        C.vector[x_axis] += A.vector[z_axis] * B.matrix[z_axis][x_axis];
        C.vector[y_axis] += A.vector[z_axis] * B.matrix[z_axis][y_axis];
        C.vector[z_axis] += A.vector[z_axis] * B.matrix[z_axis][z_axis];
        return C;
    }

    
    inline Space_Vector& Space_Vector::operator*=(const Space_Rotation_Matrix& A)
    {
        Space_Vector B(*this);

        vector[x_axis] = 0.0;
        vector[y_axis] = 0.0;
        vector[z_axis] = 0.0;

        vector[x_axis] += B.vector[x_axis] * A.matrix[x_axis][x_axis];
        vector[y_axis] += B.vector[x_axis] * A.matrix[x_axis][y_axis];
        vector[z_axis] += B.vector[x_axis] * A.matrix[x_axis][z_axis];
                                               
        vector[x_axis] += B.vector[y_axis] * A.matrix[y_axis][x_axis];
        vector[y_axis] += B.vector[y_axis] * A.matrix[y_axis][y_axis];
        vector[z_axis] += B.vector[y_axis] * A.matrix[y_axis][z_axis];
                                               
        vector[x_axis] += B.vector[z_axis] * A.matrix[z_axis][x_axis];
        vector[y_axis] += B.vector[z_axis] * A.matrix[z_axis][y_axis];
        vector[z_axis] += B.vector[z_axis] * A.matrix[z_axis][z_axis];
        return *this;
    }

    


}  // end namespace

#endif // SPACE_ROTATION_MATRIX_H
