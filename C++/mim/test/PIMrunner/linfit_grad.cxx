// linfit_grad.cxx
// linear least squares minimization through gradient minimization
// (from JMA's stellar extinction code from USGS/ROLO)
// 2007 Mar 26  James M Anderson  --JIVE  start porting from USGS code




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>


#include "linfit_grad.h"
#include "MT_random.h"

// Hide the details from the world
namespace {


//_TITLE  compute_diff_chi_sqr --make difference vector and \chi^2
Real64 compute_diff_chi_sqr(
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
    const Real64 * const x,    // I  The parameters being fit for as an array
                               //    x[NUM_PARAMETERS].  On input
                               //    this is the best estimate for the
                               //    parameters.  The gradient fitted values
                               //    are returned in this array.

    Real64 diff_vector[]       // O  array[NUM_DATA_POINTS] difference vector
                               //    of Ax - b
    )

// Real64 compute_diff_chi_sqr    O  return value
//                                   the chi squared value

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	Calculate the vector  diff_vector = Ax - b
//	Using the sparse matrix in the calculations.  From this
//	vector, create the chi square value
//	chi_sqr = f(x) = 0.5 * || dif_vector ||^2

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	1998:11:04  James M Anderson  --USGS  Original Version
//	2007 Mar 26  JMA  --port to JIVE ionosphere code

//_END

{
#ifdef DEBUG
    assert(NUM_PARAMETERS == NUM_PARAMETERS_SOLID
           + NUM_PARAMETERS_SPARSE_1 + NUM_PARAMETERS_SPARSE_2
           + NUM_PARAMETERS_SPARSE_3);
#endif
    // Run through the arrays and create the diff_vector array.
    for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
        Real64 row_sum = 0.0;

        // run through the A_solid array to add up the component parts
        for(Uint32 c=0; c < NUM_PARAMETERS_SOLID; c++) {
            row_sum += A_solid[(row * NUM_PARAMETERS_SOLID) + c] * x[c];
        }

        // do we add in the sparse matrices?
        if((NUM_PARAMETERS_SPARSE_1)) {
            row_sum += A_SPARSE_1[row] * x[INDEX_SPARSE_1[row]];
        }
        if((NUM_PARAMETERS_SPARSE_2)) {
            row_sum += A_SPARSE_2[row] * x[INDEX_SPARSE_2[row]];
        }
        if((NUM_PARAMETERS_SPARSE_3)) {
            row_sum += A_SPARSE_3[row] * x[INDEX_SPARSE_3[row]];
        }

        // subtract off the b value
        row_sum -= b[row];

        diff_vector[row] = row_sum;
    }


    // now go through and calculate the chi squared value
    Real64 chi_sqr = 0.0;
    for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
        chi_sqr += diff_vector[row] * diff_vector[row];
    }
    // now account for the 1/2 factor
    chi_sqr *= 0.5;

    return chi_sqr;
}





//_TITLE  compute_gradient_grad --compute the gradient vector
void compute_gradient_grad(
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
    const Real64 * const x,    // I  The parameters being fit for as an array
                               //    x[NUM_PARAMETERS].  On input
                               //    this is the best estimate for the
                               //    parameters.  The gradient fitted values
                               //    are returned in this array.
    const Real64 * const diff_vector,
                               // I  array[NUM_DATA_POINTS] of Ax - b
    const Real64 * const slope_sqr,
                               // I  array[NUM_PARAMETERS] of the sums of the
                               //    slopes for each parameter (i.e. the
                               //    diagonal of AT A
    Real64 grad[]              // O  The gradient del f(x) = AT(Ax - b)
                               //    as a vector of grad[NUM_PARAMETERS]
    )



//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	Calculate the gradient vector del f(x) = AT(Ax - b)
//	using the sparse matrix attributes of the ROLO extinction stuff.
//	We already have the array diff_vector \equiv (Ax - b), so we just need to
//	do the A transpose calculations.

//	Then, for each parameter, divide by the slope_sqr value for that
// 	parameter, which yields the distance necessary to change that
//	parameter value to minimize the chi square value using that parameter
//	alone.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	1998:11:04  James M Anderson  --USGS  Original Version
//	2007 Mar 26  JMA  --port to JIVE ionosphere code

//_END

{
#ifdef DEBUG
    assert(NUM_PARAMETERS == NUM_PARAMETERS_SOLID
           + NUM_PARAMETERS_SPARSE_1 + NUM_PARAMETERS_SPARSE_2
           + NUM_PARAMETERS_SPARSE_3);
#endif
    // zero the grad array
    memset(grad, 0, sizeof(Real64) * NUM_PARAMETERS);

    // run through the solid A parameters
    for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
        // run through the A_solid array to add up the component parts
        for(Uint32 c=0; c < NUM_PARAMETERS_SOLID; c++) {
            grad[c] += A_solid[(row * NUM_PARAMETERS_SOLID) + c]
                * diff_vector[row];
        }
    }
//     for(Uint32 c = 0; c < NUM_PARAMETERS_SOLID; c++) {
//         Real64 col_sum = 0.0;
//         // create a pointer into the A_solid array
//         const Real64 * A_pos = &(A_solid[c]);

//         // go down the rows
//         for(Uint32 row=0; row < NUM_DATA_POINTS;
//             row++, A_pos += NUM_PARAMETERS_SOLID) {
//             col_sum += *A_pos * diff_vector[row];
//         }
//         grad[c] = col_sum;
//     }


    // do we need to use the sparse matrices?
    if((NUM_PARAMETERS_SPARSE_1)) {
        // run through the rows and do the sums in the grad array.
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            grad[INDEX_SPARSE_1[row]] += A_SPARSE_1[row] * diff_vector[row];
        }
    }
    if((NUM_PARAMETERS_SPARSE_2)) {
        // run through the rows and do the sums in the grad array.
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            grad[INDEX_SPARSE_2[row]] += A_SPARSE_2[row] * diff_vector[row];
        }
    }
    if((NUM_PARAMETERS_SPARSE_3)) {
        // run through the rows and do the sums in the grad array.
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            grad[INDEX_SPARSE_3[row]] += A_SPARSE_3[row] * diff_vector[row];
        }
    }

    // now run through and divide by the slope_sqr value
    // to compute the optimum distance to move.  Note the slope_sqr array already
    // contains the inverse.
    for(Uint32 c=0; c < NUM_PARAMETERS; c++) {
        grad[c] *= slope_sqr[c];
    }

    return;
}



//_TITLE  compute_gradient_slope2 --compute the slope^2 (inverse)
void compute_gradient_slope2(
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
    Real64 slope_sqr[]         // O  The sum of the squares of the slopes
                               //    for each parameter.  This is the diagonal
                               //    of AT A as a vector[NUM_PARAMETERS]
    )



//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	Calculate the diagonal of AT A, which is the sum of the squares of
//      the slopes for each individual parameter for all rows.
//	Then take the inverse for all non-zero elements

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	1998:11:04  James M Anderson  --USGS  Original Version
//	2007 Mar 26  JMA  --port to JIVE ionosphere code

//_END

{
#ifdef DEBUG
    assert(NUM_PARAMETERS == NUM_PARAMETERS_SOLID
           + NUM_PARAMETERS_SPARSE_1 + NUM_PARAMETERS_SPARSE_2
           + NUM_PARAMETERS_SPARSE_3);
#endif
    // zero the slope array
    memset(slope_sqr, 0, sizeof(Real64) * NUM_PARAMETERS);

    // run through the solid A parameters
    // run through the solid A parameters
    for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
        // run through the A_solid array to add up the component parts
        for(Uint32 c=0; c < NUM_PARAMETERS_SOLID; c++) {
            slope_sqr[c] += A_solid[(row * NUM_PARAMETERS_SOLID) + c]
                * A_solid[(row * NUM_PARAMETERS_SOLID) + c];
        }
    }
//     for(Uint32 c = 0; c < NUM_PARAMETERS_SOLID; c++) {
//         Real64 col_sum = 0.0;
//         // create a pointer into the A_solid array
//         const Real64 * A_pos = &(A_solid[c]);
//         // go down the rows
//         for(Uint32 row=0; row < NUM_DATA_POINTS;
//             row++, A_pos += NUM_PARAMETERS_SOLID) {
//             col_sum += *A_pos * *A_pos;
//         }
//         slope_sqr[c] = col_sum;
//     }


    // do we need to use the sparse matrices?
    if((NUM_PARAMETERS_SPARSE_1)) {
        // run through the rows and do the sums in the grad array.
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            slope_sqr[INDEX_SPARSE_1[row]] += A_SPARSE_1[row] * A_SPARSE_1[row];
        }
    }
    if((NUM_PARAMETERS_SPARSE_2)) {
        // run through the rows and do the sums in the grad array.
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            slope_sqr[INDEX_SPARSE_2[row]] += A_SPARSE_2[row] * A_SPARSE_2[row];
        }
    }
    if((NUM_PARAMETERS_SPARSE_3)) {
        // run through the rows and do the sums in the grad array.
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            slope_sqr[INDEX_SPARSE_3[row]] += A_SPARSE_3[row] * A_SPARSE_3[row];
        }
    }

    // for all of the non-zero elements, create the inverse number
    for(Uint32 c=0; c < NUM_PARAMETERS; c++) {
        if(slope_sqr[c] != 0.0) slope_sqr[c] = 1.0 / slope_sqr[c];
        else slope_sqr[c] = 0.0;
    }

    return;
}




    
//_TITLE  component_fit_gradient_0 --do the gradient search
Sint32 component_fit_gradient_0(
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
    const char * const Vary_Array,
                               // I  An array as Vary_Array[NUM_PARAMETERS]
                               //    indicating which x positions are allowed
                               //    to be variable (Vary_Array[c] != 0)
                               //    and which are to be constant
                               //    Vary_Array[c] == 0
    Real64 * restrict x,       // B  The parameters being fit for as an array
                               //    x[NUM_PARAMETERS].  On input
                               //    this is the best estimate for the
                               //    parameters.  The gradient fitted values
                               //    are returned in this array.
    Real64 * restrict chi_sqr_out,
                               // O  Final \chi^2 value
    Real64 * restrict workspace
                               // W  array[NUM_DATA_POINTS
                               //          +4*NUM_PARAMETERS] workspace
    )
// Sint32 component_fit_gradient_0
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


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	1998:11:04  James M Anderson  --USGS  Original Version
//	2007 Mar 26  JMA  --port to JIVE ionosphere code

//_END

{
#ifdef DEBUG
    assert(NUM_PARAMETERS == NUM_PARAMETERS_SOLID
           + NUM_PARAMETERS_SPARSE_1 + NUM_PARAMETERS_SPARSE_2
           + NUM_PARAMETERS_SPARSE_3);
    if((NUM_PARAMETERS_SPARSE_1)) {
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            assert(INDEX_SPARSE_1[row] < NUM_PARAMETERS);
        }
    }
    if((NUM_PARAMETERS_SPARSE_2)) {
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            assert(INDEX_SPARSE_2[row] < NUM_PARAMETERS);
        }
    }
    if((NUM_PARAMETERS_SPARSE_3)) {
        for(Uint32 row = 0; row < NUM_DATA_POINTS; row++) {
            assert(INDEX_SPARSE_3[row] < NUM_PARAMETERS);
        }
    }
#endif
    // make some pointers to hold the gradient arrays easily
    Real64 *grad_i        = workspace; workspace += NUM_PARAMETERS;// iteration i
    Real64 *grad_im1      = workspace; workspace += NUM_PARAMETERS;// i minus 1
    // make a difference vector space
    Real64 *diff_vector   = workspace; workspace += NUM_DATA_POINTS;
    // make an array to hold the Sum{slope^2} values
    Real64 *sum_slope_sqr = workspace; workspace += NUM_PARAMETERS;
    // make a holder for the previous x parameter values
    Real64 *old_x         = workspace; workspace += NUM_PARAMETERS;


    // we will need variables to hold the magnitudes
    // of the gradients
    Real64 grad_i_mag = 1.0;
    Real64 grad_im1_mag = 1.0;
    Real64 grad_dot_mag = 1.0;
    // we will hold the chi-square values for the last four iterations
    // Set the old value to a very high value
    Real64 chi_sqr = 1.0E300;
    Real64 chi_sqr_im1 = 1.0E300;
    // declare variables to hold previous toleracnce checks
    Sint32 tol_check_1 = 0;
    Sint32 tol_check_2 = 0;
    Sint32 tol_check_3 = 0;
    // set up our magic distance variables
    Real64 d = 0.4;      // good value for ROLO atm fits
    Real64 e = 4.0;//2.7182818;

    // what is our iteration limit?
    const Uint32 ITERATION_LIMIT = 100 * NUM_PARAMETERS;

    // copy the x array into a work array to make sure we retain a copy
    memcpy(old_x, x, sizeof(Real64) * NUM_PARAMETERS);
    // set the current gradient to zero
    memset(grad_i, 0, sizeof(Real64) * NUM_PARAMETERS);
    memset(grad_im1, 0, sizeof(Real64) * NUM_PARAMETERS);

    // compute the slope_square vector
    compute_gradient_slope2(
        NUM_PARAMETERS,
        NUM_PARAMETERS_SOLID,
        NUM_PARAMETERS_SPARSE_1,
        NUM_PARAMETERS_SPARSE_2,
        NUM_PARAMETERS_SPARSE_3,
        NUM_DATA_POINTS,
        A_solid,
        b,
        A_SPARSE_1,
        INDEX_SPARSE_1,
        A_SPARSE_2,
        INDEX_SPARSE_2,
        A_SPARSE_3,
        INDEX_SPARSE_3,
        sum_slope_sqr
        );
    
    // loop over the gradient seach, but make a limit of how many times
    // we can go
    Uint32 iteration;
    for(iteration=0; iteration < ITERATION_LIMIT; iteration++) {
//         printf("Starting Gradient Iteration %6u, chi_sqr %15E\n", iteration,chi_sqr);
//         if((iteration & 0xFF) == 0) {
//             printf("Starting Gradient Iteration %6u, chi_sqr %15E\n",
//                    iteration, chi_sqr);
//         }
        // loop until we get a lower chi square value
        Uint32 bad_d_flag = 0;
        do {
            // calculate the new chi_sqr value
            chi_sqr = compute_diff_chi_sqr(
                NUM_PARAMETERS,
                NUM_PARAMETERS_SOLID,
                NUM_PARAMETERS_SPARSE_1,
                NUM_PARAMETERS_SPARSE_2,
                NUM_PARAMETERS_SPARSE_3,
                NUM_DATA_POINTS,
                A_solid,
                b,
                A_SPARSE_1,
                INDEX_SPARSE_1,
                A_SPARSE_2,
                INDEX_SPARSE_2,
                A_SPARSE_3,
                INDEX_SPARSE_3,
                x,
                diff_vector
                );
            //printf("New Chi-Sqr %E\n", chi_sqr);
        
            // is this chi square value bigger than the last time?
            if(chi_sqr > chi_sqr_im1) {
                // the last step down the gradient was too large
                // Drop the distance factor and recompute the x position values
                d /= e;
                if(d < 1.0E-300) {
                    bad_d_flag++;
                    if(bad_d_flag >= 2) {
                        // whoops, notify the caller
//                         for(Uint32 c=0; c < NUM_PARAMETERS; c++) {
//                             printf("param %4d x %15E grad %15E\n",
//                                    c, old_x[c], grad_i[c]);
//                         }
                        // give up, but don't cause too much trouble
                        *chi_sqr_out = chi_sqr;
                        return +1;
                    }
                    else {
                        // for this first time, set the solid componet
                        // gradient terms to zero (except for pos 0)
                        for(Uint32 c=1; c < NUM_PARAMETERS_SOLID; c++) {
                            grad_i[c] = 0.0;
                        }
                        // reset d to something reasonable
                        d = 0.1;
                    }
                }
                //printf("Dropping distance to %E\n", d);
                // go through and recompute x
                // Note that we have placed a copy of x_i-1 in old_x
                for(Uint32 c=0; c < NUM_PARAMETERS; c++) {
                    if((Vary_Array[c])) {
                        x[c] = old_x[c] - d * grad_i[c];
                    }
                }
            }
        } while (chi_sqr > chi_sqr_im1);
        // we have decreased the chi square value.  Check if we have almost
        // reached a limit here on improvement.
        {
            tol_check_1 = ((1.0 - chi_sqr / chi_sqr_im1) < Tolerance);
            if( ((tol_check_1)) && ((tol_check_2)) && ((tol_check_3)) ) {
                //printf("Exiting on Tol Check\n");
                break;
            }
            // no, so copy the tol_checks back
            tol_check_3 = tol_check_2;
            tol_check_2 = tol_check_1;
        }
        // copy the new chi_sqr to the holder
        chi_sqr_im1 = chi_sqr;
        // Copy the old gradient (via a pointer switch)
        {
            Real64 *temp = grad_im1;
            grad_im1 = grad_i;
            grad_i = temp;
        }
        // copy the old x array into storage
        memcpy(old_x, x, sizeof(Real64) * NUM_PARAMETERS);


        grad_im1_mag = grad_i_mag;
        // now compute the gradient vector
        //printf("Computing gradient\n");fflush(stdout);
        compute_gradient_grad(
            NUM_PARAMETERS,
            NUM_PARAMETERS_SOLID,
            NUM_PARAMETERS_SPARSE_1,
            NUM_PARAMETERS_SPARSE_2,
            NUM_PARAMETERS_SPARSE_3,
            NUM_DATA_POINTS,
            A_solid,
            b,
            A_SPARSE_1,
            INDEX_SPARSE_1,
            A_SPARSE_2,
            INDEX_SPARSE_2,
            A_SPARSE_3,
            INDEX_SPARSE_3,
            x,
            diff_vector,
            sum_slope_sqr,
            grad_i
            );
        // calculate the magnitudes of the vectors
        {
            grad_i_mag = grad_dot_mag = 0.0;
            for(Uint32 c = 0; c < NUM_PARAMETERS; c++) {
                if((Vary_Array[c])) {
                    grad_i_mag   += grad_i[c] * grad_i[c];
                    grad_dot_mag += grad_i[c] * grad_im1[c];
                }
            }
            grad_i_mag = (grad_i_mag > 0.0)? sqrt(grad_i_mag) : 0.0;
//             printf("Gradients are %E %E %E\n", grad_i_mag, grad_im1_mag,
//                    grad_dot_mag);
            // if the gradient is zero, we are done
            if(grad_i_mag < 1.0E-25) {
                //printf("Exiting on zero gradient\n");
                break;
            }
        }
        // what should we do about the distance change?
        // if the new gradient is in the same direction as the old gradient,
        // increase the distance factor.  If in the opposite direction,
        // drop it.
        {
            Real64 cos_theta = grad_dot_mag / (grad_i_mag * grad_im1_mag);
            //printf("cos_theta %E\n", cos_theta);
            if(cos_theta > 1.0) {
                cos_theta = 1.0;
            }
            else if(cos_theta < -1.0) {
                cos_theta = -1.0;
            }
            if(cos_theta < -0.95) cos_theta *= 1.2; // fudge in case thrashing
            d *= pow(e, cos_theta);
            //printf("cos theta %E New d is %E\n", cos_theta, d);
        }
        // compute a new x array
        {
            for(Uint32 c = 0; c < NUM_PARAMETERS; c++) {
                if((Vary_Array[c])) {
                    x[c] -= d * grad_i[c];
                }
            }
        }
    } // for iteration to iteration limit

    *chi_sqr_out = chi_sqr;


//     printf("Exiting after %d iterations\n", iteration);
//     fflush(stdout);

    if(iteration < ITERATION_LIMIT) return 0;

    return +2;
}










    


    


// GLOBALS


// FUNCTIONS



}  // end namespace


// And now for the public stuff
namespace MIM_PIM {



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
                               //          +5*NUM_PARAMETERS] workspace
    )
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


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	1998:11:04  James M Anderson  --USGS  Original Version
//	2007 Mar 26  JMA  --port to JIVE ionosphere code

//_END

{
    Real64 * restrict x_try = workspace; workspace += NUM_PARAMETERS;
    Real64 chi_sqr = 1E300;
    Sint32 return_code = 0;
    memset(Vary_Array, 0, NUM_PARAMETERS);
    // Now allow the sparse areas to vary
    Vary_Array[0] = 1;
    for(Uint32 c=NUM_PARAMETERS_SOLID; c < NUM_PARAMETERS; c++) {
        Vary_Array[c] = 1;
    }
    return_code = component_fit_gradient_0(
        NUM_PARAMETERS,
        NUM_PARAMETERS_SOLID,
        NUM_PARAMETERS_SPARSE_1,
        NUM_PARAMETERS_SPARSE_2,
        NUM_PARAMETERS_SPARSE_3,
        NUM_DATA_POINTS,
        A_solid,
        b,
        A_SPARSE_1,
        INDEX_SPARSE_1,
        A_SPARSE_2,
        INDEX_SPARSE_2,
        A_SPARSE_3,
        INDEX_SPARSE_3,
        Tolerance,
        Vary_Array,
        x,
        &chi_sqr,
        workspace
        );
    //fprintf(stderr, "Got return_code %d from component fit 0\n", return_code);
    if(return_code < 0) return return_code;
    // now allow everything to vary
    for(Uint32 c=0; c < NUM_PARAMETERS; c++) {
        Vary_Array[c] = 1;
    }
    return_code = component_fit_gradient_0(
        NUM_PARAMETERS,
        NUM_PARAMETERS_SOLID,
        NUM_PARAMETERS_SPARSE_1,
        NUM_PARAMETERS_SPARSE_2,
        NUM_PARAMETERS_SPARSE_3,
        NUM_DATA_POINTS,
        A_solid,
        b,
        A_SPARSE_1,
        INDEX_SPARSE_1,
        A_SPARSE_2,
        INDEX_SPARSE_2,
        A_SPARSE_3,
        INDEX_SPARSE_3,
        Tolerance,
        Vary_Array,
        x,
        &chi_sqr,
        workspace
        );
    //fprintf(stderr, "Got return_code %d from component fit 0\n", return_code);
    if(return_code < 0) return return_code;


    // Now try to allow some random perturbations
    Real64 last_chi_sqr = chi_sqr;
    JMA_MONTE_CARLO_RANDOM::Random_Number_Generator_Mersenne_Twister r(0x1234);
    for(Uint32 i=0; i < 4; i++) {
        //fprintf(stderr, "Starting random pertubations with initial \\chi^2 of %E stddev %E\n", chi_sqr, sqrt(chi_sqr/(NUM_DATA_POINTS-NUM_PARAMETERS)));
        // copy over the parameters into a test array, and fudge the numbers
        // by a few percent
        const Real64 fraction = 0.4;
        for(Uint32 p=0; p < NUM_PARAMETERS; p++) {
            x_try[p] = x[p] * (1.0 + fraction * r.genrand_real4());
        }
        memset(Vary_Array, 0, NUM_PARAMETERS);
        // Now allow the sparse areas to vary
        Vary_Array[0] = 1;
        for(Uint32 c=NUM_PARAMETERS_SOLID; c < NUM_PARAMETERS; c++) {
            Vary_Array[c] = 1;
        }
        return_code = component_fit_gradient_0(
            NUM_PARAMETERS,
            NUM_PARAMETERS_SOLID,
            NUM_PARAMETERS_SPARSE_1,
            NUM_PARAMETERS_SPARSE_2,
            NUM_PARAMETERS_SPARSE_3,
            NUM_DATA_POINTS,
            A_solid,
            b,
            A_SPARSE_1,
            INDEX_SPARSE_1,
            A_SPARSE_2,
            INDEX_SPARSE_2,
            A_SPARSE_3,
            INDEX_SPARSE_3,
            Tolerance*10.0,
            Vary_Array,
            x_try,
            &chi_sqr,
            workspace
            );
        //fprintf(stderr, "Got return_code %d from component fit 0\n", return_code);
        if(return_code < 0) return return_code;
        for(Uint32 c=0; c < NUM_PARAMETERS; c++) {
            Vary_Array[c] = 1;
        }
        return_code = component_fit_gradient_0(
            NUM_PARAMETERS,
            NUM_PARAMETERS_SOLID,
            NUM_PARAMETERS_SPARSE_1,
            NUM_PARAMETERS_SPARSE_2,
            NUM_PARAMETERS_SPARSE_3,
            NUM_DATA_POINTS,
            A_solid,
            b,
            A_SPARSE_1,
            INDEX_SPARSE_1,
            A_SPARSE_2,
            INDEX_SPARSE_2,
            A_SPARSE_3,
            INDEX_SPARSE_3,
            Tolerance,
            Vary_Array,
            x_try,
            &chi_sqr,
            workspace
            );
        //fprintf(stderr, "New \\chi^2 is %E\n", chi_sqr);
        if(chi_sqr < last_chi_sqr) {
            // Hey, an improvement
            last_chi_sqr = chi_sqr;
            memcpy(x, x_try, sizeof(Real64) * NUM_PARAMETERS);
        }
    } // for i over pertubation iterations
    return return_code;
}


}
