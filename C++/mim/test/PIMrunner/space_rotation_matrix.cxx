// space_rotation_matrix.cxx
// stuff for rotation matrices for dealing with Cartesian Space_Vector's 
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 26  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include "space_vector.h"
#include "space_rotation_matrix.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



    Space_Rotation_Matrix::Space_Rotation_Matrix(const Real64 cos_angle,
                                                 const Real64 sin_angle,
                                                 enum Space_Vector_Enum type)
        throw()
    {
        switch (type) {
            case x_axis:
                matrix[x_axis][x_axis] = 1.0;
                matrix[x_axis][y_axis] = 0.0;
                matrix[x_axis][z_axis] = 0.0;
                matrix[y_axis][x_axis] = 0.0;
                matrix[y_axis][y_axis] = cos_angle;
                matrix[y_axis][z_axis] = -sin_angle;
                matrix[z_axis][x_axis] = 0.0;
                matrix[z_axis][y_axis] = sin_angle;
                matrix[z_axis][z_axis] = cos_angle;
                break;
            case y_axis:
                matrix[x_axis][x_axis] = cos_angle;
                matrix[x_axis][y_axis] = 0.0;
                matrix[x_axis][z_axis] = sin_angle;
                matrix[y_axis][x_axis] = 0.0;
                matrix[y_axis][y_axis] = 1.0;
                matrix[y_axis][z_axis] = 0.0;
                matrix[z_axis][x_axis] = -sin_angle;
                matrix[z_axis][y_axis] = 0.0;
                matrix[z_axis][z_axis] = cos_angle;
                break;
            case z_axis:
                matrix[x_axis][x_axis] = cos_angle;
                matrix[x_axis][y_axis] = -sin_angle;
                matrix[x_axis][z_axis] = 0.0;
                matrix[y_axis][x_axis] = sin_angle;
                matrix[y_axis][y_axis] = cos_angle;
                matrix[y_axis][z_axis] = 0.0;
                matrix[z_axis][x_axis] = 0.0;
                matrix[z_axis][y_axis] = 0.0;
                matrix[z_axis][z_axis] = 1.0;
                break;
            default:;
#ifdef DEBUG
                fprintf(stderr, "Error: unknown axis type %d in %s:%d:%s\n",
                        int(type),
                        __FILE__, __LINE__, __func__);
                exit(1);
#endif // DEBUG
        };
        return;
    }





    


    

}  // end namespace


