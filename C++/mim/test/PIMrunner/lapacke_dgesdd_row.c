/*******************************************************************************
*  Copyright (C) 2009-2014 Intel Corporation. All Rights Reserved.
*  The information and material ("Material") provided below is owned by Intel
*  Corporation or its suppliers or licensors, and title to such Material remains
*  with Intel Corporation or its suppliers or licensors. The Material contains
*  proprietary information of Intel or its suppliers and licensors. The Material
*  is protected by worldwide copyright laws and treaty provisions. No part of
*  the Material may be copied, reproduced, published, uploaded, posted,  
*  transmitted, or distributed in any way without Intel's prior express written
*  permission. No license under any patent, copyright or other intellectual
*  property rights in the Material is granted to or conferred upon you, either
*  expressly, by implication, inducement, estoppel or otherwise. Any license
*  under such intellectual property rights must be express and approved by Intel
*  in writing.
*
********************************************************************************
*/
/*
   LAPACKE_dgesdd Example.
   =======================

   Program computes the singular value decomposition of a general
   rectangular matrix A using a divide and conquer method, where A is:

     7.52  -1.10  -7.95   1.08
    -0.76   0.62   9.34  -7.10
     5.13   6.62  -5.66   0.87
    -4.75   8.52   5.75   5.30
     1.33   4.91  -5.49  -3.52
    -2.40  -6.77   2.34   3.95

   Description.
   ============

   The routine computes the singular value decomposition (SVD) of a real
   m-by-n matrix A, optionally computing the left and/or right singular
   vectors. If singular vectors are desired, it uses a divide and conquer
   algorithm. The SVD is written as

   A = U*SIGMA*VT

   where SIGMA is an m-by-n matrix which is zero except for its min(m,n)
   diagonal elements, U is an m-by-m orthogonal matrix and VT (V transposed)
   is an n-by-n orthogonal matrix. The diagonal elements of SIGMA
   are the singular values of A; they are real and non-negative, and are
   returned in descending order. The first min(m, n) columns of U and V are
   the left and right singular vectors of A.

   Note that the routine returns VT, not V.

   Example Program Results.
   ========================

 LAPACKE_dgesdd (row-major, high-level) Example Program Results

 Singular values
  18.37  13.63  10.85   4.49

 Left singular vectors (stored columnwise)
  -0.57   0.18   0.01   0.53
   0.46  -0.11  -0.72   0.42
  -0.45  -0.41   0.00   0.36
   0.33  -0.69   0.49   0.19
  -0.32  -0.31  -0.28  -0.61
   0.21   0.46   0.39   0.09

 Right singular vectors (stored rowwise)
  -0.52  -0.12   0.85  -0.03
   0.08  -0.99  -0.09  -0.01
  -0.28  -0.02  -0.14   0.95
   0.81   0.01   0.50   0.31
*/
#include <stdlib.h>
#include <stdio.h>
#include "lapacke.h"

/* Auxiliary routines prototypes */
extern void print_matrix( char* desc, lapack_int m, lapack_int n, double* a,
lapack_int lda );

/* Parameters */
#define M 6
#define N 4
#define LDA N
#define LDU M
#define LDVT N

/* Main program */
int main() {
        /* Locals */
        lapack_int m = M, n = N, lda = LDA, ldu = LDU, ldvt = LDVT, info;
        /* Local arrays */
        double s[N], u[LDU*M], vt[LDVT*N];
        double a[LDA*M] = {
            7.52, -1.10, -7.95, 1.08,
           -0.76,  0.62,  9.34, -7.10,
            5.13,  6.62, -5.66, 0.87,
           -4.75,  8.52,  5.75, 5.30,
            1.33,  4.91, -5.49, -3.52,
           -2.40, -6.77,  2.34, 3.95
        };
        /* Executable statements */
        printf( "LAPACKE_dgesdd (row-major, high-level) Example Program Results\n" );
        /* Compute SVD */
        info = LAPACKE_dgesdd( LAPACK_ROW_MAJOR, 'S', m, n, a, lda, s,
                        u, ldu, vt, ldvt );
        /* Check for convergence */
        if ( info > 0 ) {
                printf( "The algorithm computing SVD failed to converge.\n" );
                exit( 1 );
        }
        /* Print singular values */
        print_matrix( "Singular values", 1, n, s, 1 );
        /* Print left singular vectors */
        print_matrix( "Left singular vectors (stored columnwise)", m, n, u, ldu );
        /* Print right singular vectors */
        print_matrix( "Right singular vectors (stored rowwise)", n, n, vt, ldvt );
        exit( 0 );
} /* End of LAPACKE_dgesdd Example */

/* Auxiliary routine: printing a matrix */
void print_matrix( char* desc, lapack_int m, lapack_int n, double* a, lapack_int
lda ) {
        lapack_int i, j;
        printf( "\n %s\n", desc );
        for ( i = 0; i < m; i++ ) {
                for ( j = 0; j < n; j++ ) printf( " %6.2f", a[i*lda+j] );
                printf( "\n" );
        }
}
