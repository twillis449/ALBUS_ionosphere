// linfit_grad.h
// linear least squares minimization through gradient minimization
// (from JMA's stellar extinction code from USGS/ROLO)
// 2007 Mar 26  James M Anderson  --JIVE  start porting from USGS code



#ifndef LINFIT_GRAD_H
#define LINFIT_GRAD_H

// INCLUDES
#include "JMA_math.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS

//_TITLE  component_fit_gradient_1 --do the gradient search
Sint32 component_fit_gradient_1(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const Uint32 NUM_PARAMETERS,
                               // I  the total number of parameters we are
                               //    fitting for.
    const Uint32 NUM_PARAMETERS_SOLID,
                               // I  The number of parameters in A_solid
    const Uint32 NUM_PARAMETERS_SPARSE_1,
                               // I  The number of parametrs is sparese area 1
    const Uint32 NUM_PARAMETERS_SPARSE_2,
                               // I  The number of parametrs is sparese area 2
    const Uint32 NUM_PARAMETERS_SPARSE_3,
                               // I  The number of parametrs is sparese area 3
    const Uint32 NUM_DATA_POINTS,
                               // I  the number of data points
    const Real64 * const A_solid,
                               // I  The subset of the A matrix which is
                               //    the left solid columns.  This is
                               //    A_solid[NUM_DATA_POINTS][NUM_PARAMETERS_SOLID]
    const Real64 * const b,    // I  The b array as b[NUM_DATA_POINTS]
    const Real64 * const A_SPARSE_1,
                               // I  The spares area 1 part of A as
                               //    A_SPARSE_1[NUM_DATA_POINTS]
    const Uint32 * const INDEX_SPARSE_1,
                               // I  The listof index positions for the sparse
                               //    area of the matrix part 1.  This should be
                               //    the full index from the 0 start of
                               //    NUM_PARAMETERS.
    const Real64 * const A_SPARSE_2,
                               // I  The spares area 2 part of A as
                               //    A_SPARSE_1[NUM_DATA_POINTS]
    const Uint32 * const INDEX_SPARSE_2,
                               // I  The listof index positions for the sparse
                               //    area of the matrix part 2.  This should be
                               //    the full index from the 0 start of
                               //    NUM_PARAMETERS.
    const Real64 * const A_SPARSE_3,
                               // I  The spares area 3 part of A as
                               //    A_SPARSE_1[NUM_DATA_POINTS]
    const Uint32 * const INDEX_SPARSE_3,
                               // I  The listof index positions for the sparse
                               //    area of the matrix part 3.  This should be
                               //    the full index from the 0 start of
                               //    NUM_PARAMETERS.
    const Real64 Tolerance,    // I  The tolerance limit for stopping the
                               //    gradient search.  This is a relative
                               //    difference formed by
                               //    (1.0 - chi_sqr/chi_sqr_im1)
    Real64 * restrict x,       // B  The parameters being fit for as an array
                               //    x[NUM_PARAMETERS].  On input
                               //    this is the best estimate for the
                               //    parameters.  The gradient fitted values
                               //    are returned in this array.
    char * const restrict Vary_Array,
                               // W  An array as Vary_Array[NUM_PARAMETERS]
                               //    indicating which x positions are allowed
                               //    to be variable (Vary_Array[c] != 0)
                               //    and which are to be constant
                               //    Vary_Array[c] == 0
    Real64 * restrict workspace
                               // W  array[NUM_DATA_POINTS
                               //          +4*NUM_PARAMETERS] workspace
    );
// Sint32 component_fit_gradient_1
//                                O  return code
//                                    0 all ok
//                                   +1 d became too small
//                                   +2 exceeded iteration limit
//                                   
//                                   
//                                   
//                                   
//                                   
//                                   
//                                   


}  // end namespace


#endif // LINFIT_GRAD_H
