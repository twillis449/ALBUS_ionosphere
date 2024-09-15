// linfit.cxx
// a program for linear least squares fitting of stuff
// for Math 583 stuff --- now MIM_PIM
// 2001 Oct 24  James M Anderson  --NMT  Original Version
//	2005 Aug 23  JMA  --JIVE rework for MIM_PIM stuff
//	                    Much of the fancy analysis of the M583 class
//                          has been dropped.  If intersted, see the
//                          tarball in the classes programming area.
//	2007 Jan 03  JMA  --start stuff for GPS satellite observation fitting
//                          Copy and rework the main fitting functions to better
//	                    deal with GPS obs amd repeated calling sequences.





#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <time.h>

#include "station_reference.h"
#include "station_latlon.h"
#include "observation.h"
#include "observation_3D.h"
#include "space_vector.h"

#include "latlon_cart.h"
#include "cal_source.h"
#include "linfit.h"
#include "linfit_grad.h"
#include "ionosphere_iri.h"









namespace {
    

    inline Real64 SIGN(Real64 a, Real64 b)
    {
        return ( (b > 0.0f) ? fabs(a) : -fabs(a));
    }

    inline Real64 FMAX(Real64 a, Real64 b)
    {
        return ( (a > b) ? a : b );
    }

    inline Sint32 IMIN(Sint32 a, Sint32 b)
    {
        return ( (a < b) ? a : b );
    }









//_TITLE  svdcmp --perform an SVD matrix operation
Sint32 svdcmp(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 a[],                // B  matrix a in, u out  a[m][n]
        const Sint32 m,            // I  size
        const Sint32 n,            // I  size
        Real64 w[],                // O  diagonal matrix w[n]
        Real64 v[],                // O  v[n][n]
        Real64 rv1[]               // B  rv1[n] used as workspace
        )

    // Sint32 svdcmp     O  return code
    //                                    0 all ok
    //                                   !0 problem

    //_USER  any user input?

    //_VARS  TYPE           VARIABLE I/O DESCRIPTION
    //       put globals used here

    //_DESC  full description of program
    //	This function is based on the Numerical recipes svdcmp routine

    //	Perform a Singular Value Decomposition on a matrix.
    //	This routine has been modified to work better with the
    //	ROLO star sky subtraction routines and uses standard C array notation

    //	Also replaced pythag call with standard UNIX hypot

    //_FILE  files used and logical units used

    //_LIMS  design limitations

    //_BUGS  known bugs

    //_CALL  list of calls

    //_KEYS  

    //_HIST  DATE NAME PLACE INFO
    //	1997:12:31  James M Anderson  --NAU  Original Version
    //	1998:01:02  JMA  --copied for sky subtraction

    //_END

    {
        fprintf(stderr, "in svdcmp\n");
        Sint32 flag,i,its,j,jj,k,l,nm;
        Real64 anorm,c,f,g,h,s,scale,x,y,z;

        l = nm = 0;


        g=scale=anorm=0.0f;
        for (i=0;i<n;i++) {
            l=i+1;
            rv1[i]=scale*g;
            g=s=scale=0.0f;
            if (i < m) {
                for (k=i;k<m;k++) scale += fabs(a[k*n+i]);
                if (scale) {
                    for (k=i;k<m;k++) {
                        a[k*n+i] /= scale;
                        s += a[k*n+i]*a[k*n+i];
                    }
                    f=a[i*n+i];
                    g = -SIGN(sqrt(s),f);
                    h=f*g-s;
                    a[i*n+i]=f-g;
                    for (j=l;j<n;j++) {
                        for (s=0.0f,k=i;k<m;k++) s += a[k*n+i]*a[k*n+j];
                        f=s/h;
                        for (k=i;k<m;k++) a[k*n+j] += f*a[k*n+i];
                    }
                    for (k=i;k<m;k++) a[k*n+i] *= scale;
                }
            }
            w[i]=scale *g;
            g=s=scale=0.0f;
            if ((i < m) && (i != (n-1))) {
                for (k=l;k<n;k++) scale += fabs(a[i*n+k]);
                if (scale) {
                    for (k=l;k<n;k++) {
                        a[i*n+k] /= scale;
                        s += a[i*n+k]*a[i*n+k];
                    }
                    f=a[i*n+l];
                    g = -SIGN(sqrt(s),f);
                    h=f*g-s;
                    a[i*n+l]=f-g;
                    for (k=l;k<n;k++) rv1[k]=a[i*n+k]/h;
                    for (j=l;j<m;j++) {
                        for (s=0.0f,k=l;k<n;k++) s += a[j*n+k]*a[i*n+k];
                        for (k=l;k<n;k++) a[j*n+k] += s*rv1[k];
                    }
                    for (k=l;k<n;k++) a[i*n+k] *= scale;
                }
            }
            anorm=FMAX(anorm,(fabs(w[i])+fabs(rv1[i])));
        }
        for (i=n-1;i>=0;i--) {
            if (i < (n-1)) {
                if (g) {
                    for (j=l;j<n;j++)
                        v[j*n+i]=(a[i*n+j]/a[i*n+l])/g;
                    for (j=l;j<n;j++) {
                        for (s=0.0f,k=l;k<n;k++) s += a[i*n+k]*v[k*n+j];
                        for (k=l;k<n;k++) v[k*n+j] += s*v[k*n+i];
                    }
                }
                for (j=l;j<n;j++) v[i*n+j]=v[j*n+i]=0.0f;
            }
            v[i*n+i]=1.0f;
            g=rv1[i];
            l=i;
        }
        for (i=IMIN(m,n)-1;i>=0;i--) {
            l=i+1;
            g=w[i];
            for (j=l;j<n;j++) a[i*n+j]=0.0f;
            if (g) {
                g=1.0f/g;
                for (j=l;j<n;j++) {
                    for (s=0.0f,k=l;k<m;k++) s += a[k*n+i]*a[k*n+j];
                    f=(s/a[i*n+i])*g;
                    for (k=i;k<m;k++) a[k*n+j] += f*a[k*n+i];
                }
                for (j=i;j<m;j++) a[j*n+i] *= g;
            } else for (j=i;j<m;j++) a[j*n+i]=0.0f;
            a[i*n+i] += 1.0f;
        }
        for (k=n-1;k>=0;k--) {
            for (its=1;its<=30;its++) {
                flag=1;
                for (l=k;l>=0;l--) {
                    nm=l-1;  // rv1[0] is always 0.0f
                    if ((Real64)(fabs(rv1[l])+anorm) == anorm) {
                        flag=0;
                        break;
                    }
                    if ((Real64)(fabs(w[nm])+anorm) == anorm) break;
                }
                if (flag) {
                    c=0.0f;
                    s=1.0f;
                    for (i=l;i<=k;i++) {
                        f=s*rv1[i];
                        rv1[i]=c*rv1[i];
                        if ((Real64)(fabs(f)+anorm) == anorm) break;
                        g=w[i];
                        h=hypot(f,g);
                        w[i]=h;
                        h=1.0f/h;
                        c=g*h;
                        s = -f*h;
                        for (j=0;j<m;j++) {
                            y=a[j*n+nm];
                            z=a[j*n+i];
                            a[j*n+nm]=y*c+z*s;
                            a[j*n+i]=z*c-y*s;
                        }
                    }
                }
                z=w[k];
                if (l == k) {
                    if (z < 0.0f) {
                        w[k] = -z;
                        for (j=0;j<n;j++) v[j*n+k] = -v[j*n+k];
                    }
                    break;
                }
                if (its == 30) return 1;
                x=w[l];
                nm=k-1;
                y=w[nm];
                g=rv1[nm];
                h=rv1[k];
                f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0f*h*y);
                g=hypot(f,1.0f);
                f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
                c=s=1.0f;
                for (j=l;j<=nm;j++) {
                    i=j+1;
                    g=rv1[i];
                    y=w[i];
                    h=s*g;
                    g=c*g;
                    z=hypot(f,h);
                    rv1[j]=z;
                    c=f/z;
                    s=h/z;
                    f=x*c+g*s;
                    g = g*c-x*s;
                    h=y*s;
                    y *= c;
                    for (jj=0;jj<n;jj++) {
                        x=v[jj*n+j];
                        z=v[jj*n+i];
                        v[jj*n+j]=x*c+z*s;
                        v[jj*n+i]=z*c-x*s;
                    }
                    z=hypot(f,h);
                    w[j]=z;
                    if (z) {
                        z=1.0f/z;
                        c=f*z;
                        s=h*z;
                    }
                    f=c*g+s*y;
                    x=c*y-s*g;
                    for (jj=0;jj<m;jj++) {
                        y=a[jj*n+j];
                        z=a[jj*n+i];
                        a[jj*n+j]=y*c+z*s;
                        a[jj*n+i]=z*c-y*s;
                    }
                }
                rv1[l]=0.0f;
                rv1[k]=f;
                w[k]=x;
            }
        }

    
        return 0;
    }

















//_TITLE  name one line description
Sint32 svbksb(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 u[],          // I  u[m][n]
        const Real64 w[],          // I  w[n]
        const Real64 v[],          // I  v[n][n]
        const Sint32 m,            // I  size
        const Sint32 n,            // I  size
        const Real64 b[],          // I  b[m]
        Real64 tmp[],              // B  temporary work space tmp[n]
        Real64 x[]                 // O  output values x[n]
        )

    //_USER  any user input?

    //_VARS  TYPE           VARIABLE I/O DESCRIPTION
    //       put globals used here

    //_DESC  full description of program
    //	This function was derived from the Numerical Recipes svbksb routine.

    //	This routine performs backsubstitution for SVD.  It has been
    //	modified for use with the ROLO star sky fitting process.

    //_FILE  files used and logical units used

    //_LIMS  design limitations

    //_BUGS  known bugs

    //_CALL  list of calls

    //_KEYS  

    //_HIST  DATE NAME PLACE INFO
    //	1997:12:31  James M Anderson  --NAU  Original Version
    //	1998:01:02  JMA  --copied for sky subtraction

    //_END

    {
        fprintf(stderr, "in svbksb\n");
        Sint32 jj,j,i;
        Real64 s;

        for (j=0;j<n;j++) {
            s=0.0f;
            if (w[j]) {
                for (i=0;i<m;i++) s += u[i*n+j]*b[i];
                s /= w[j];
            }
            tmp[j]=s;
        }
        for (j=0;j<n;j++) {
            s=0.0f;
            for (jj=0;jj<n;jj++) s += v[j*n+jj]*tmp[jj];
            x[j]=s;
        }

        return 0;
    }













Sint32 svdvar(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Real64 v[],          // I  matrix v[m][m]
        const Sint32 m,               // I  size
        const Real64 w[],          // I  diagonal matrix w[m]
        Real64 work[],             // B  work matrix work[m]
        Real64 cvm[]               // O  covariance matrix cvm[m][m]
        )
    {
        fprintf(stderr, "in svdvar\n");
        Sint32 k, j, i;
    
        Real64 sum;

        for(i=0; i < m; i++) {
            work[i] = ((w[i] > 0.0)) ? 1.0 / (w[i]*w[i]) : 0.0f;
        }
        for(i=0;i<m;i++) {
            for(j=0;j<=i;j++) {
                for(sum=0.0f,k=0;k<m;k++) sum += v[i*m +k] * v[j*m +k] *work[k];
                cvm[j*m +i] = cvm[i*m +j] = sum;
            }
        }
        return 0;
    }




//_TITLE  linfit -- perform a linear least squares fit using SVD
Sint32 linfit(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Sint32 NUM_PARAM,
        const Sint32 NUM_DATA,

        Real64 B[],                // B comes in as Y[NUM_DATA]
        Real64 U[],                // B comes in as X[NUM_DATA][NUM_PARAM],
                                   //   goes out as the covariance matrix
                                   //   CVM[NUM_PARAM][NUM_PARAM]
        Real64 sigma[],            // B comes in as sigma[NUM_DATA]
        Real64 V[],                // B workspace V[NUM_PARAM][NUM_PARAM]
        Real64 W[],                // B Workspace W[NUM_PARAM]
        Real64 work[],             // B Workspace work[NUM_PARAM]
        Real64 parameters[]        // O fit parameters as parameters[NUM_PARAM]
        )

    // Sint32 linfit                     O  the return code
    //                                    0 all ok
    //                                   -1 could not malloc enough memory
    //                                   -2 could not malloc memory for cheby terms
    //                                   -3 error in svdcmp
    //                                   -4 bad calc number
    //                                   
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

    //_END

    {
        //fprintf(stderr, "in LINFIT\n");
        //printf("%s\t%s\n", "NUM_PARAM", "NUM_DATA");
        //printf("%d\t%d\n", NUM_PARAM, NUM_DATA);
        // Now we need to fill out the coefficients for the SVD fitting process.
        // U holds the parameterized fit and B holds the actual data values
        {
            for(Sint32 i=0; i < NUM_DATA; i++) {
                Real64 sigma_m1 = 1.0 / sigma[i];
                B[i] *= sigma_m1;
                for(Sint32 j=0; j < NUM_PARAM; j++) U[i*NUM_PARAM +j] *= sigma_m1;
            }
        }
    




//      print_matrix( "input U matrix", NUM_DATA, NUM_PARAM, U, NUM_DATA );



        // perform the singular value decomposition
        if(svdcmp(U,NUM_DATA,NUM_PARAM,W,V,work) != 0) {
            char errmsg[256];
            Sint32 errnum = -3;
            sprintf(errmsg, "%s::%s::%d Iteration overflow in svdcmp", 
                    __FILE__, __func__, __LINE__);
            fprintf(stderr, "%s\n", errmsg);
            return errnum;
        }






        // edit the singular values
        {
            Real64 thresh = 0.0f;
            Sint32 i;
            for (i=0;i<NUM_PARAM;i++)
                if (W[i] > thresh) thresh=W[i];
            thresh *= 1.0E-7f;         // JMA adjustment to Numerical Recipes value
            for (i=0;i<NUM_PARAM;i++) 
                if (W[i] < thresh) {
#ifdef DEBUG
                    fprintf(stderr, "Removing Parameter %d  as % E below threshold % E\n",i,W[i],thresh);
#endif
                    W[i]=0.0f;
                }
        }

    

        // now perform the backsubstitution to get the parameters
        svbksb(U,W,V,NUM_DATA,NUM_PARAM,B,work,parameters);




//     // prSint32 the parameter results for debugging
//     {
//         for(Sint32 i=0; i < NUM_PARAM; i++) {
//             printf("term %d is % E\n", i, parameters[i]);
//         }
//     }


        // now calculate the covariance matrix and put it Sint32o U
        svdvar(V,NUM_PARAM,W,work,U);


        // all done
        return 0;
    }














    // Get the important parameters
Sint32 fit_data(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Sint32 calc,            // I  calculation number (see below)

        const Sint32 NUM_PARAM,
        const Sint32 NUM_DATA,

        Real64 y[],                // I comes in as y[NUM_DATA]
        Real64 x[],                // I comes in as x[NUM_DATA][NUM_PARAM],
        Real64 sigma[],            // I comes in as sigma[NUM_DATA]
        Real64 *standard_dev,      // O estimated standard deviation
        Real64 parameters[],       // O model parameters as parameters[NUM_PARAM]
        Real64 model[]             // O y-fit as model[NUM_DATA]
        )
    {
        printf("in linfit: fit_data\n");
        // create some workspace
        // Since this may get pretty large with all of the observations
        // and parameters, try allocating
        Real64* B = new Real64[NUM_DATA];
        Real64* U = new Real64[NUM_DATA*NUM_PARAM];

        Real64* V = new Real64[NUM_PARAM*NUM_PARAM];
        Real64* W = new Real64[NUM_PARAM];
        Real64* work = new Real64[NUM_PARAM];

        if((B == NULL) || (U == NULL) || (V == NULL) || (W == NULL)
           || (work == NULL)) {
            fprintf(stderr, "Error: cannot allocate enough workspace for %d data %d param in %s:%d:%s\n",
                    NUM_DATA, NUM_PARAM,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }



        printf("%s\t%s\n", "NUM_PARAM", "NUM_DATA");
        printf("%d\t%d\n", NUM_PARAM, NUM_DATA);
        // copy stuff over
        for(Sint32 i=0; i < NUM_DATA; i++) B[i] = y[i];
        for(Sint32 i=0; i < NUM_DATA*NUM_PARAM; i++) U[i] = x[i];


        // do the fit
        if(linfit(NUM_PARAM, NUM_DATA, B, U, sigma, V, W, work, parameters)
           != 0) {
            fprintf(stderr, "Error in svd fit\n");
            return -1;
        }



        // calculate the residuals
        if((calc & 0x1)) {
            // print the fit values and the residuals
            printf("%s\t%s\t%s\n", "datapoint", "fit", "resid");
        }
        Real64 sum_sqr_resid = 0.0;
        for(Sint32 i=0; i < NUM_DATA; i++) {
            Real64 fit = 0.0;
            for(Sint32 j=0; j < NUM_PARAM; j++) {
                fit += parameters[j]*x[i*NUM_PARAM +j];
            }
            Real64 resid = y[i] - fit;
            sum_sqr_resid += resid*resid;
            model[i] = fit;

            if((calc & 0x1)) {
                // print the fit values and the residuals
                printf("%d\t% E\t% E\n", i, fit, resid);
            }
        }
        // calculate the estimated standard deviation
        Real64 s = sqrt(sum_sqr_resid / (NUM_DATA-NUM_PARAM));
        if((calc & 0x4)) {
            printf("Estimated standard deviation is % E for %d parameters\n",
                   s, NUM_PARAM);
        }
        *standard_dev = s;


        // print the number of degrees of freedom
        if((calc & 0x4)) {
            printf("Degrees of Freedom %d\n", NUM_DATA-NUM_PARAM);
        }


        // print R^2 and R^2_{adj}
        if((calc & 0x8)) {
            Real64 mean = 0.0;
            for(Sint32 i=0; i < NUM_DATA; i++) mean += y[i];
            mean /= NUM_DATA;
            Real64 ysqr = 0.0;
            for(Sint32 i=0; i < NUM_DATA; i++) ysqr += (y[i]-mean)*(y[i]-mean);
            printf("R^2 is % E\n", 1.0 - sum_sqr_resid/ysqr);
            printf("R^2_{adj} is % E\n", 1.0 - sum_sqr_resid/ysqr
                   * Real64((NUM_DATA-1))
                   / (NUM_DATA-NUM_PARAM));
            printf("SS Residuals is % E      SS Original is % E\n",
                   sum_sqr_resid, ysqr);
        }
    




        // correct the covariance array
        for(Sint32 i=0; i < NUM_PARAM*NUM_PARAM; i++) U[i] *= s*s;
    



        // run through and calculate the standard deviations in the parameters
        // and the t statistics
        if((calc & 0x2)) {
            printf("\n\n%s\t%s\t%s\t%s\n", "parameter", "value",
                   "standard_deviation", "t_stat");
            for(Sint32 i=0; i < NUM_PARAM; i++) {
                Real64 stan = sqrt(U[i*NUM_PARAM+i]);
                Real64 t_stat = parameters[i] / stan;
                printf("%d\t% E\t% E\t% E\n", i, parameters[i], stan, t_stat);
            }
        }



        // print the covariance matrix
        if((calc & 0x20)) {
            printf("\n\nCovariance Matrix\nparameter");
            for(Sint32 i=0; i < NUM_PARAM; i++) printf("\t%9d", i);
            fputc('\n', stdout);
            for(Sint32 i=0; i < NUM_PARAM; i++) {
                printf("%9d", i);
                for(Sint32 j=0; j < i; j++) printf("\t         ");
                for(Sint32 j=i; j < NUM_PARAM; j++) printf("\t% E", U[i*NUM_PARAM +j]);
                fputc('\n', stdout);
            }
        }






        // free up the memory
        delete[] B;
        delete[] U;
        delete[] V;
        delete[] W;
        delete[] work;
    
        return 0;
    }

    

















    // Get the important parameters, reworked for GPS observation fitting
Sint32 fit_data_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Sint32 calc,            // I  calculation number (see below)

        const Sint32 NUM_PARAM,
        const Sint32 NUM_DATA,

        Real64 y[],                // I comes in as y[NUM_DATA]
        Real64 x[],                // I comes in as x[NUM_DATA][NUM_PARAM],
        Real64 sigma[],            // I comes in as sigma[NUM_DATA]
        Real64 workspace[],        // W free workspace of size at least
                                   //   workspace[NUM_DATA*(NUM_PARAM+1)
                                   //             +NUM_PARAM*(NUM_PARAM+2)]
        Real64 *standard_dev,      // O estimated standard deviation
        Real64 parameters[],       // O model parameters as parameters[NUM_PARAM]
        Real64 model[]             // O y-fit as model[NUM_DATA]
        )
// Sint32 fit_data_2                  O return code.
//                                       0 all ok
//                                      -1 SVD failure
    {
        printf("in linfit: fit_data_2\n");
        // assign the workspace locations.  Use the caller's workspace area
        // as this routine will be called many, many times
        Real64* p = workspace;

        Real64* B = p; p += NUM_DATA;
        Real64* U = p; p += NUM_DATA*NUM_PARAM;

        Real64* V = p; p += NUM_PARAM*NUM_PARAM;
        Real64* W = p; p += NUM_PARAM;
        Real64* work = p; p += NUM_PARAM;



        // copy stuff over
        for(Sint32 i=0; i < NUM_DATA; i++) B[i] = y[i];
        for(Sint32 i=0; i < NUM_DATA*NUM_PARAM; i++) U[i] = x[i];


        // do the fit
        if(linfit(NUM_PARAM, NUM_DATA, B, U, sigma, V, W, work, parameters)
           != 0) {
            fprintf(stderr, "Error in svd fit\n");
            return -1;
        }



        // calculate the residuals
        if((calc & 0x1)) {
            // print the fit values and the residuals
            printf("%s\t%s\t%s\n", "datapoint", "fit", "resid");
        }
        Real64 sum_sqr_resid = 0.0;
        for(Sint32 i=0; i < NUM_DATA; i++) {
            Real64 fit = 0.0;
            for(Sint32 j=0; j < NUM_PARAM; j++) {
                fit += parameters[j]*x[i*NUM_PARAM +j];
            }
            Real64 resid = y[i] - fit;
            sum_sqr_resid += resid*resid;
            model[i] = fit;

            if((calc & 0x1)) {
              for(Sint32 j=0; j < NUM_PARAM; j++) printf("%8.4f ", x[i*NUM_PARAM +j]);
              // print the fit values and the residuals
              printf("%d\t% E\t% E\n", i, fit, resid);
            }
        }
        // calculate the estimated standard deviation
        if(NUM_DATA-NUM_PARAM > 0) {
//            Real64 s = sqrt(sum_sqr_resid / (NUM_DATA-NUM_PARAM));
            Real64 s = sqrt(sum_sqr_resid / (NUM_DATA));
            if((calc & 0x4)) {
              printf("Estimated standard deviation is % E for %d parameters\n",
                       s, NUM_PARAM);
            }
            *standard_dev = s;
            printf("Calculated standard deviation estimate of %E for %d NUM_PARAM and %d NUM_DATA \n",
                       *standard_dev, NUM_PARAM, NUM_DATA);
        }
        else {
            // ought to be a perfect fit.  Fudge something.
            *standard_dev = 1.0e16;
//            if((calc & 0x4)) {
                printf("Fudging standard deviation estimate of %E for %d NUM_PARAM and %d NUM_DATA \n",
                       *standard_dev, NUM_PARAM, NUM_DATA);
//            }
        }            


        // print the number of degrees of freedom
        if((calc & 0x4)) {
            printf("Degrees of Freedom %d\n", NUM_DATA-NUM_PARAM);
        }


        // print R^2 and R^2_{adj}
        if((calc & 0x8)) {
            Real64 mean = 0.0;
            for(Sint32 i=0; i < NUM_DATA; i++) mean += y[i];
            mean /= NUM_DATA;
            Real64 ysqr = 0.0;
            for(Sint32 i=0; i < NUM_DATA; i++) ysqr += (y[i]-mean)*(y[i]-mean);
            printf("R^2 is % E\n", 1.0 - sum_sqr_resid/ysqr);
            printf("R^2_{adj} is % E\n", 1.0 - sum_sqr_resid/ysqr
                   * Real64((NUM_DATA-1))
                   / (NUM_DATA-NUM_PARAM));
            printf("SS Residuals is % E      SS Original is % E\n",
                   sum_sqr_resid, ysqr);
        }
    




        // correct the covariance array
        {
            Real64 mult = (*standard_dev) * (*standard_dev);
            for(Sint32 i=0; i < NUM_PARAM*NUM_PARAM; i++) U[i] *= mult;
        }
    



        // run through and calculate the standard deviations in the parameters
        // and the t statistics
        if((calc & 0x2)) {
            printf("\n\n%s\t%s\t%s\t%s\n", "parameter", "value",
                   "standard_deviation", "t_stat");
            for(Sint32 i=0; i < NUM_PARAM; i++) {
                Real64 stan = sqrt(U[i*NUM_PARAM+i]);
                Real64 t_stat = parameters[i] / stan;
                printf("%d\t% E\t% E\t% E\n", i, parameters[i], stan, t_stat);
            }
        }



        // print the covariance matrix
        if((calc & 0x20)) {
            printf("\n\nCovariance Matrix\nparameter");
            for(Sint32 i=0; i < NUM_PARAM; i++) printf("\t%9d", i);
            fputc('\n', stdout);
            for(Sint32 i=0; i < NUM_PARAM; i++) {
                printf("%9d", i);
                for(Sint32 j=0; j < i; j++) printf("\t         ");
                for(Sint32 j=i; j < NUM_PARAM; j++) printf("\t% E", U[i*NUM_PARAM +j]);
                fputc('\n', stdout);
            }
        }
        return 0;
    }

    


















    static const Uint32 NUM_IRI_DATAPOINTS = 38;
    static Real64 IRI_data[NUM_IRI_DATAPOINTS*2] = {
        3.000000E+04, 1.499586E-05,
        3.600000E+04, 1.263987E-01,
        4.320000E+04, 4.893297E+02,
        5.184000E+04, 4.271591E+05,
        6.220800E+04, 4.283237E+07,
        7.464960E+04, 3.869404E+08,
        8.957952E+04, 1.017098E+10,
        1.074954E+05, 1.234709E+11,
        1.289945E+05, 1.286232E+11,
        1.547934E+05, 1.823406E+11,
        1.857521E+05, 2.274823E+11,
        2.229025E+05, 3.016186E+11,
        2.674830E+05, 2.817501E+11,
        3.209796E+05, 2.122693E+11,
        3.851755E+05, 1.268829E+11,
        4.622106E+05, 6.448350E+10,
        5.546528E+05, 3.248288E+10,
        6.655833E+05, 1.890196E+10,
        7.987000E+05, 1.351939E+10,
        9.584400E+05, 1.148447E+10,
        1.150128E+06, 8.498775E+09,
        1.380154E+06, 6.100467E+09,
        1.656184E+06, 4.421160E+09,
        1.987421E+06, 3.191522E+09,
        2.384905E+06, 2.282238E+09,
        2.861886E+06, 1.611467E+09,
        3.434264E+06, 1.120209E+09,
        4.121117E+06, 7.643032E+08,
        4.945340E+06, 5.102293E+08,
        5.934408E+06, 3.321347E+08,
        7.121289E+06, 2.100681E+08,
        8.545547E+06, 1.286186E+08,
        1.025466E+07, 7.590222E+07,
        1.230559E+07, 4.299852E+07,
        1.476671E+07, 2.326629E+07,
        1.772005E+07, 1.196692E+07,
        2.126406E+07, 5.816460E+06,
        2.551687E+07, 2.656580E+06
    };
    static Real64 IRI_data_work0[NUM_IRI_DATAPOINTS];
    static Real64 IRI_data_work1[NUM_IRI_DATAPOINTS];


void calculate_heights_and_scales(const Uint32 NUM_HEIGHTS,
                                  Real64* const height,
                                  Real64* const scale = NULL)
{
    //printf("in linfit: calculate_heights_and_scales\n");
    // Given a number of heights to make, NUM_HEIGHTS > 0,
    // this function figures out what good heights are to
    // make ionosphere fitting heights.  This function uses
    // an ionosphere profile generated by the IRI code for
    // above Westerbork on 2007 June 01 at noon to make the
    // ionosphere profile.  (See test2.cxx for how this was
    // done, it is relatively simple.)

    //	Then, using this profile, this function calculates heights
    // which given a uniform spacing in electron column density.
    // In other words, given an array of heights and electron densities,
    // this function will create for the caller a set of heights which
    // are equidistantly spaced in terms of the integral of the
    // electron density (which is the column density).

    // Calculate the total column.  Use a trapazoidal integration scheme
    Real64 sum_electron = 0.0;
    for(Uint32 i=0; i < NUM_IRI_DATAPOINTS-1; i++) {
        sum_electron += 0.5 * (IRI_data[2*i+2+1] + IRI_data[2*i+1])
            * (IRI_data[2*i+2] - IRI_data[2*i]);
    }
    // Now fill in the points
    const Real64 Delta_electron = sum_electron / NUM_HEIGHTS;
    Uint32 i = 0;
    Real64 last_height = IRI_data[2*i];
    for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
        Real64 sum = 0.0;
        Real64 slope = (IRI_data[2*i+2+1] - IRI_data[2*i+1])
            / (IRI_data[2*i+2] - IRI_data[2*i]);
        Real64 b = IRI_data[2*i+1] + slope * (last_height - IRI_data[2*i]);
        Real64 temp_sum = b * (IRI_data[2*i+2] - last_height)
            + 0.5 * slope * (IRI_data[2*i+2] - last_height)
            * (IRI_data[2*i+2] - last_height);
        //printf("%d %d %E %E %E to %E\n", h, i, sum, sum+temp_sum, Delta_electron, IRI_data[2*i+2]);
        Real64 next_height = 0.0;
        if(temp_sum > Delta_electron) {
            Real64 s = b*b + 2.0 * slope * (Delta_electron - sum);
            if(s<0.0) s = 0.0;
            if(slope == 0.0) {
                next_height = (Delta_electron - sum) / IRI_data[2*i+1] + last_height;
            }
            else {
                next_height = (-b + sqrt(s)) / slope + last_height;
            }
        }
        else {
            while(sum + temp_sum < Delta_electron) {
                sum += temp_sum;
                i++;
                if(i >= NUM_IRI_DATAPOINTS-1) {
                    if(h == NUM_HEIGHTS-1) {
                        // we are just at the end, fudge the answer
                        height[h] = 0.5 * (IRI_data[2*i] + last_height);
                        goto end_loop;
                    }
                    else {
                        fprintf(stderr, "Error: programmer error at %s:%d:%s\n",
                                __FILE__, __LINE__, __func__);
                        exit(1);
                    }
                }
                temp_sum = 0.5 * (IRI_data[2*i+2+1] + IRI_data[2*i+1])
                    * (IRI_data[2*i+2] - IRI_data[2*i]);
                //printf("%d %d %E %E %E to %E\n", h, i, sum, sum+temp_sum, Delta_electron, IRI_data[2*i+2]);
            }
            slope = (IRI_data[2*i+2+1] - IRI_data[2*i+1])
                / (IRI_data[2*i+2] - IRI_data[2*i]);
            b = IRI_data[2*i+1];
            Real64 s = b*b + 2.0 * slope * (Delta_electron - sum);
            if(s<0.0) s = 0.0;
            if(slope == 0.0) {
                next_height = (Delta_electron - sum) / IRI_data[2*i+1] + IRI_data[2*i];
            }
            else {
                next_height = (-b + sqrt(s)) / slope + IRI_data[2*i];
            }
        }
        height[h] = 0.5 * (next_height + last_height);
        last_height = next_height;
        //printf("got %E %E\n", height[h], last_height);
    } // for h over heights
end_loop:
    if((scale)) {
        // Now adjust the scaling.  This should be uniform
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            scale[h] = 1.0 / NUM_HEIGHTS;
        }
    }
    return;
}


    static Real64 IRI_VTEC = 1.0;
Real64 calculate_IRI_VTEC(void)
{
    for(Uint32 i=0; i < NUM_IRI_DATAPOINTS; i++)
        IRI_data_work0[i] = IRI_data[2*i];
    for(Uint32 i=0; i < NUM_IRI_DATAPOINTS; i++)
        IRI_data_work1[i] = IRI_data[2*i];
    Real64 sum = 0.0;
    // First do the 0 point
    sum += IRI_data[1] * (IRI_data_work1[1] - IRI_data_work1[0]);
    // Then the middle region
    for(Uint32 i=1; i < NUM_IRI_DATAPOINTS-i; i++) {
        Real64 dist = 0.5*(IRI_data_work1[i+1] - IRI_data_work1[i-1]);
        Real64 n_e = IRI_data[2*i+1];
        sum += n_e * dist;
    }
    // Now do the last point
    sum += IRI_data[2*(NUM_IRI_DATAPOINTS-1)+1]
        * (IRI_data_work1[NUM_IRI_DATAPOINTS-1]
           - IRI_data_work1[NUM_IRI_DATAPOINTS-2]);
    IRI_VTEC = sum;
    return sum;
}

Real64 calculate_IRI_STEC(const MIM_PIM::Station_LatLon* const station,
                          const Real64 El)
//                          const Space_Unit_Vector direction)
{
    //printf("in linfit: calculate_IRI_STEC\n");
    station->get_pierce_ranges(El, NUM_IRI_DATAPOINTS,
                               IRI_data_work0,
                               MIM_PIM::radius_Earth,
                               IRI_data_work1);
    Real64 sum = 0.0;
    // First do the 0 point
    sum += IRI_data[1] * (IRI_data_work1[1] - IRI_data_work1[0]);
    // Then the middle region
    for(Uint32 i=1; i < NUM_IRI_DATAPOINTS-i; i++) {
        Real64 dist = 0.5*(IRI_data_work1[i+1] - IRI_data_work1[i-1]);
        Real64 n_e = IRI_data[2*i+1];
        sum += n_e * dist;
    }
    // Now do the last point
    sum += IRI_data[2*(NUM_IRI_DATAPOINTS-1)+1]
        * (IRI_data_work1[NUM_IRI_DATAPOINTS-1]
           - IRI_data_work1[NUM_IRI_DATAPOINTS-2]);
    return sum;
}

//         {
//             calculate_IRI_VTEC();
//             for(Real64 El = 90.0; El > 0.0; El -= 2.5) {
//                 Real64 STEC = calculate_IRI_STEC(observation[i].station,El*M_DEG2RAD);
//                 Real64 s = get_simple_VTEC_scaling(
//                             El*M_DEG2RAD, observation[i].station->Elevation(),
//                             300E3);
//                 printf("%5.1f %E %E %E\n", El, STEC, STEC/IRI_VTEC, s);
//             }
//             exit(1);
//         }














    








} // unnamed namespace
















namespace MIM_PIM {





//_TITLE  calculate_simple_2D_linear_polynomials
void calculate_simple_2D_linear_polynomials(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters needed,
                               //    must be in 0 < NUM_PARAM < 10000
                               //    (although anything above 36 is almost
                               //    certainly suspect
        const Real64 x,        // I  the "x" value, typically the longitude here
        const Real64 y,        // I  the "y" value, typically the latitude here
        Real64* const poly     // O  the polynomial holder, as poly[NUM_PARAM]
        )


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will compute the value of a set of polynomials for
//	a 2D surface fit (fitting z as a function of x and y).  It is intended
//	for fitting ionospheric data to latitude and longitude positions.

//	This function generates a simple, straightforward set of polynomials,
//	starting with the lowest order polynomial values in x and y, and working
//	up to get NUM_PARAM values.  As I expect the ionosphere to vary more with
//	latitude, this will go up in y order slightly faster than in x.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	Does not pay attention to whether x^n or y^n is still a valid
//	Real64, so don't expect to go to too high of an order n.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 24  James M Anderson  --JIVE  start

//_END

    {
        // this function called all the time
        // printf("in linfit:  calculate_simple_2D_linear_polynomials\n");
        // if we aren't doing at least 1 poly, then why call this?
        if(NUM_PARAM <= 0) {
            fprintf(stderr, "Error: cannot do 0 polys here in %s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
        }
        
        // Ok, I am going to need to hold onto the x and y
        // max poly values.
        Real64 x_max = 1.0;
        Real64 y_max = 1.0;
        // And I need to know how many polys have been made to date.
        Uint32 num_param_done = 0;

        // ok, off we go
        for(Uint32 n_max = 0; /* no check */; n_max++, x_max *= x, y_max *= y) {
            // start up the x axis first, since I expect greater latitude
            // variations.  (this means that the y_max value is used first.)
            Real64 x_now = 1.0;
            for(Uint32 n_x = 0; n_x <= n_max; n_x++, x_now *= x) {
                Real64 value = y_max * x_now;
                poly[num_param_done++] = value;
                if(num_param_done == NUM_PARAM) return;
            }
            // And now do the other set with y, being careful not to duplicate
            // the last polynomial
            Real64 y_now = 1.0;
            for(Uint32 n_y = 0; n_y < n_max; n_y++, y_now *= y) {
                Real64 value = x_max * y_now;
                poly[num_param_done++] = value;
                if(num_param_done == NUM_PARAM) return;
            }
        }
        // it should be impossible to get here
#ifdef DEBUG
        fprintf(stderr, "Error: how did I get here? %s:%d:%s\n",
                __FILE__, __LINE__, __func__);
        exit(1);
#endif
        return;
    }












//_TITLE  fit_simple_linear_model --fit a surface to the observations
void fit_simple_linear_model(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a simple linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 24  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  fit_simple_linear_model\n");
        // Ok, how many good observations are there?
        Uint32 Num_Data = 0;
        for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
            if((use_flag[i])) Num_Data++;

        // Now, compare with the number of parameters desired
        if( (Num_Data < NUM_PARAM) || (NUM_PARAM <= 0) ) {
            fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now, allocate some space to deal with the arrays needed by the fitter
        Real64* y = new Real64[Num_Data];
        Real64* x = new Real64[Num_Data*NUM_PARAM];  //x[NUM_DATA][NUM_PARAM],
        Real64* sigma = new Real64[Num_Data];
        Real64* model = new Real64[Num_Data];

        if( (y==NULL) || (x==NULL) || (sigma==NULL)
            || (model==NULL) ) {
            fprintf(stderr, "Error: cannot allocate enough memory for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }





        // Ok, let's fill in the arrays
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) continue;

            // Fill in y and sigma
            y[n] = observation[i].VTEC;
            sigma[n] = observation[i].sigma_VTEC;

            // Now, get the model lat and lon
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(observation[i].pierce_point);
            const Real64 lat = equatorial_pos.Lat();
            const Real64 lon = equatorial_pos.Lon();
            // fill in the x stuff
            calculate_simple_2D_linear_polynomials(
                NUM_PARAM,
                lon,
                lat,
                x+(n*NUM_PARAM)
                );

            // increment the fit data position
            n++;
        } // for i,n over NUM_OBSERVATIONS


        // do the fit
        Real64 std_dev = 0.0;
        Sint32 return_code = fit_data(Sint32(0xFFFE),
                                      Sint32(NUM_PARAM),
                                      Sint32(Num_Data),
                                      y,
                                      x,
                                      sigma,
                                      &std_dev,
                                      parameters,
                                      model
                                      );
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);

        // Now, stuff in the model values
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) {
                // for testing, predict for all other observations
                LatLon_Cart equatorial_pos =
                    Ref_Point.get_equatorial_offset(observation[i].pierce_point);
                const Real64 lat = equatorial_pos.Lat();
                const Real64 lon = equatorial_pos.Lon();
                // fill in the x stuff.  Note that x is now free to use
                calculate_simple_2D_linear_polynomials(
                    NUM_PARAM,
                    lon,
                    lat,
                    x
                    );
                Real64 fit = 0.0;
                for(Uint32 j=0; j < NUM_PARAM; j++) fit += parameters[j]*x[j];
                observation[i].model_VTEC = fit;
                observation[i].sigma_model_VTEC = std_dev;
                continue;
            }

            // Fill in y and sigma
            observation[i].model_VTEC = model[n];
            observation[i].sigma_model_VTEC = std_dev;

            // increment the fit data position
            n++;
         } // for i,n over NUM_OBSERVATIONS
        


        // free up the memory
        delete[] y;
        delete[] x;
        delete[] sigma;
        delete[] model;

        return;
    }
    
    



























//_TITLE  calculate_simple_2D_polar_polynomials
void calculate_simple_2D_polar_polynomials(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters needed,
                               //    must be in 0 < NUM_PARAM < 10000
                               //    (although anything above 36 is almost
                               //    certainly suspect
        const Real64 clon,     // I  the cosine of the polar longitude
        const Real64 slon,     // I  the sine of the polar longitude
        const Real64 lat,      // I  the polar latitude
        const Real64 El,       // I  the original elevation angle
        Real64* const poly     // O  the polynomial holder, as poly[NUM_PARAM]
        )


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will compute the value of a set of polynomials for
//	a 2D surface fit (fitting z as a function of x and y).  It is intended
//	for fitting ionospheric data to latitude and longitude positions.

//	This function generates a simple, straightforward set of polynomials,
//	starting with the lowest order polynomial values in latitude and
// 	longitude, and working
//	up to get NUM_PARAM values.  As I expect the ionosphere to vary more with
//	latitude, this will go up in longitude order slightly faster than in lat.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	Does not pay attention to whether x^n is still a valid
//	Real64, so don't expect to go to too high of an order n.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 29  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  calculate_simple_2D_polar_polynomials\n");
        // if we aren't doing at least 1 poly, then why call this?
        if(NUM_PARAM <= 0) {
            fprintf(stderr, "Error: cannot do 0 polys here in %s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
        }


        // For simplicity of naming conventions below, let's just call things
        // x, y, and z
        // Ok, I am going to need to hold onto the x and y and z
        // max poly values.  We start out with just a constant
        Real64 x_max = 1.0;
        Real64 y_max = 1.0;
        Real64 z_max = 1.0;
        // Now, let's make some x, y, and z values
        Real64 x = clon * lat;
        Real64 y = slon * lat;
        Real64 z = El;
        
        // And I need to know how many polys have been made to date.
        Uint32 num_param_done = 0;

        // ok, off we go
        for(Uint32 n_max = 0; /* no check */;
            n_max++, x_max *= x, y_max *= y, z_max *= z) {
            // start up the z axis first, since I expect greater longitude
            // variations.  (this means that the y_max value is used first.)
            Real64 z_now = 1.0;
            for(Uint32 n_z = 0; n_z <= n_max; n_z++, z_now *= z) {
                // start up the x axis first, since I expect greater longitude
                // variations.  (this means that the y_max value is used first.)
                Real64 x_now = 1.0;
                for(Uint32 n_x = 0; n_x <= n_max; n_x++, x_now *= x) {
                    Real64 value = z_now * y_max * x_now;
                    poly[num_param_done++] = value;
                    if(num_param_done == NUM_PARAM) return;
                }
                // And now do the other set with y, being careful not to duplicate
                // the last polynomial
                Real64 y_now = 1.0;
                for(Uint32 n_y = 0; n_y < n_max; n_y++, y_now *= y) {
                    Real64 value = z_now * x_max * y_now;
                    poly[num_param_done++] = value;
                    if(num_param_done == NUM_PARAM) return;
                }
            }
            // And now the last side of the cube
            Real64 x_top = 1.0;
            Real64 y_top = 1.0;
            for(Uint32 m_max = 0; m_max < n_max;
                m_max++, x_top *= x, y_top *= y) {
                // start up the x axis first, since I expect greater longitude
                // variations.  (this means that the y_max value is used first.)
                Real64 x_now = 1.0;
                for(Uint32 n_x = 0; n_x < n_max; n_x++, x_now *= x) {
                    Real64 value = z_max * y_top * x_now;
                    poly[num_param_done++] = value;
                    if(num_param_done == NUM_PARAM) return;
                }
                // And now do the other set with y, being careful not to duplicate
                // the last polynomial
                Real64 y_now = 1.0;
                for(Uint32 n_y = 0; n_y < n_max; n_y++, y_now *= y) {
                    Real64 value = z_max * x_top * y_now;
                    poly[num_param_done++] = value;
                    if(num_param_done == NUM_PARAM) return;
                }
            }
        }
        // it should be impossible to get here
#ifdef DEBUG
        fprintf(stderr, "Error: how did I get here? %s:%d:%s\n",
                __FILE__, __LINE__, __func__);
        exit(1);
#endif
        return;
    }












//_TITLE  fit_simple_polar_model --fit a surface to the observations
void fit_simple_polar_model(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a simple linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 24  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  fit_simple_polar_model\n");
        // Ok, how many good observations are there?
        Uint32 Num_Data = 0;
        for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
            if((use_flag[i])) Num_Data++;

        // Now, compare with the number of parameters desired
        if( (Num_Data < NUM_PARAM) || (NUM_PARAM <= 0) ) {
            fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now, allocate some space to deal with the arrays needed by the fitter
        Real64* y = new Real64[Num_Data];
        Real64* x = new Real64[Num_Data*NUM_PARAM];  //x[NUM_DATA][NUM_PARAM],
        Real64* sigma = new Real64[Num_Data];
        Real64* model = new Real64[Num_Data];

        if( (y==NULL) || (x==NULL) || (sigma==NULL)
            || (model==NULL) ) {
            fprintf(stderr, "Error: cannot allocate enough memory for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }





        // Ok, let's fill in the arrays
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) continue;

            // Fill in y and sigma
            y[n] = observation[i].VTEC;
            sigma[n] = observation[i].sigma_VTEC;

            // Now, get the model lat and lon
            LatLon_Cart polar_pos =
                Ref_Point.get_polar_offset(observation[i].pierce_point);
            const Real64 lat = polar_pos.Lat();
            const Real64 clon = polar_pos.cLon();
            const Real64 slon = polar_pos.sLon();
            // fill in the x stuff
            calculate_simple_2D_polar_polynomials(
                NUM_PARAM,
                clon,
                slon,
                lat,
                observation[i].El,
                x+(n*NUM_PARAM)
                );

            // increment the fit data position
            n++;
        } // for i,n over NUM_OBSERVATIONS


        // do the fit
        Real64 std_dev = 0.0;
        Sint32 return_code = fit_data(Sint32(0xFFFE),
                                      Sint32(NUM_PARAM),
                                      Sint32(Num_Data),
                                      y,
                                      x,
                                      sigma,
                                      &std_dev,
                                      parameters,
                                      model
                                      );
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);

        // Now, stuff in the model values
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) {
                // for testing, predict for all other observations
                LatLon_Cart polar_pos =
                    Ref_Point.get_polar_offset(observation[i].pierce_point);
                const Real64 lat = polar_pos.Lat();
                const Real64 clon = polar_pos.cLon();
                const Real64 slon = polar_pos.sLon();
                // fill in the x stuff.  Note that x is now free to use
                calculate_simple_2D_polar_polynomials(
                    NUM_PARAM,
                    clon,
                    slon,
                    lat,
                    observation[i].El,
                    x
                    );
                Real64 fit = 0.0;
                for(Uint32 j=0; j < NUM_PARAM; j++) fit += parameters[j]*x[j];
                observation[i].model_VTEC = fit;
                observation[i].sigma_model_VTEC = std_dev;
                continue;
            }

            // Fill in y and sigma
            observation[i].model_VTEC = model[n];
            observation[i].sigma_model_VTEC = std_dev;

            // increment the fit data position
            n++;
         } // for i,n over NUM_OBSERVATIONS
        


        // free up the memory
        delete[] y;
        delete[] x;
        delete[] sigma;
        delete[] model;

        return;
    }
























//_TITLE  calculate_multilayer_2D_linear_polynomials
void calculate_multilayer_2D_linear_polynomials(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters needed,
                               //    must be in 0 < NUM_PARAM < 10000
                               //    (although anything above 36 is almost
                               //    certainly suspect)
        const Uint32 NUM_POS,  // I  The number of x and y positions to
                               //    accumulate
        const Real64* const x, // I  the "x" value, typically the longitude here
                               //    as x[NUM_POS]
        const Real64* const y, // I  the "y" value, typically the latitude here
                               //    as y[NUM_POS]
        const Real64* const scale,
                               // I  a scaling factor for each position, as
                               //    scale[NUM_POS]
        Real64* const poly     // O  the polynomial holder, as poly[NUM_PARAM]
        )


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will compute the value of a set of polynomials for
//	a 2D surface fit (fitting z as a function of x and y).  It is intended
//	for fitting ionospheric data to latitude and longitude positions.

//	This function generates a simple, straightforward set of polynomials,
//	starting with the lowest order polynomial values in x and y, and working
//	up to get NUM_PARAM values.  As I expect the ionosphere to vary more with
//	latitude, this will go up in y order slightly faster than in x.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	Does not pay attention to whether x^n or y^n is still a valid
//	Real64, so don't expect to go to too high of an order n.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 07  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  calculate_multilayer_2D_linear_polynomials\n");
        // if we aren't doing at least 1 poly, then why call this?
        if(NUM_PARAM <= 0) {
            fprintf(stderr, "Error: cannot do 0 polys here in %s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
        }
        // if we aren't doing at least 1 pod, then why call this?
        if(NUM_POS <= 0) {
            fprintf(stderr, "Error: cannot do 0 pos here in %s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
        }


        // First, zero the poly array
        for(Uint32 n=0; n < NUM_PARAM; n++) poly[n] = 0.0;


        // Now, for each position element, add on its contribution
        for(Uint32 pos = 0; pos < NUM_POS;) {
        
            // Ok, I am going to need to hold onto the x and y
            // max poly values.
            Real64 x_max = 1.0;
            Real64 y_max = 1.0;
            // And I need to know how many polys have been made to date.
            Uint32 num_param_done = 0;

            // ok, off we go
            for(Uint32 n_max = 0; /* no check */; n_max++,
                    x_max *= x[pos], y_max *= y[pos]) {
                // start up the x axis first, since I expect greater latitude
                // variations.  (this means that the y_max value is used first.)
                Real64 x_now = 1.0;
                for(Uint32 n_x = 0; n_x <= n_max; n_x++, x_now *= x[pos]) {
                    Real64 value = y_max * x_now * scale[pos];
                    poly[num_param_done++] += value;
                    if(num_param_done == NUM_PARAM) goto end_of_multilayer_loop;
                }
                // And now do the other set with y, being careful not to duplicate
                // the last polynomial
                Real64 y_now = 1.0;
                for(Uint32 n_y = 0; n_y < n_max; n_y++, y_now *= y[pos]) {
                    Real64 value = x_max * y_now * scale[pos];
                    poly[num_param_done++] += value;
                    if(num_param_done == NUM_PARAM) goto end_of_multilayer_loop;
                }
            }

            // I will sometimes have to break out of two for loops, so set up
            // a label here
end_of_multilayer_loop:
            pos++;
        } // for pos over NUM_POS
        return;
    }












//_TITLE  fit_multilayer_linear_model --fit a surface to the observations
void fit_multilayer_linear_model(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a multilayer linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.


// 	This function will generate it own set of height and pierce point
// 	information, based on the number of heights requested.  This routine
//	assumes that the main ionospheric height is around 350 km, with
//	the ionospheric electron density decreasing above and below.  For
//	each observation, the pierce points are calculated for the various
//	heights.  These are used to generate the polynomial terms, which are
//	combined into their respective x^m y^n terms for the fitting process.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 07  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  fit_multilayer_linear_model\n");
        // Ok, how many good observations are there?
        Uint32 Num_Data = 0;
        for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
            if((use_flag[i])) Num_Data++;

        // Now, compare with the number of parameters desired
        if( (Num_Data < NUM_PARAM) || (NUM_PARAM <= 0) || (NUM_HEIGHTS <= 0) ) {
            fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u HEIGHTS with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_HEIGHTS, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now, allocate some space to deal with the arrays needed by the fitter
        Real64* y = new Real64[Num_Data];
        Real64* x = new Real64[Num_Data*NUM_PARAM];  //x[NUM_DATA][NUM_PARAM],
        Real64* sigma = new Real64[Num_Data];
        Real64* model = new Real64[Num_Data];
        Real64* height = new Real64[NUM_HEIGHTS];
        Real64* density_factor = new Real64[NUM_HEIGHTS];
        Real64* scale = new Real64[NUM_HEIGHTS];
        Real64* range = new Real64[NUM_HEIGHTS];
        Real64* lat = new Real64[NUM_HEIGHTS];
        Real64* lon = new Real64[NUM_HEIGHTS];

        if( (y==NULL) || (x==NULL) || (sigma==NULL)
            || (model==NULL) || (height==NULL) || (density_factor==NULL)
            || (scale==NULL) || (range == NULL) || (lat==NULL) || (lon==NULL) ) {
            fprintf(stderr, "Error: cannot allocate enough memory for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }






        // Now, create a set of heights
        {
            if(NUM_HEIGHTS == 1) {
                // only 1 height, so use 350 km
                height[0] = 350E3; // m
                density_factor[0] = 1.0;
            }
            else if(NUM_HEIGHTS == 2) {
                // 2 heights, so use something surrounding 350 km
                height[0] = 250E3; // m
                height[1] = 500E3; // m
                density_factor[0] = 0.5;
                density_factor[1] = 0.5;
            }
            else {
                // for everything else, create a uniform spacing

//                 // linear spacing
//                 Real64 Delta = 680E3 / (NUM_HEIGHTS-1);
//                 Real64 sum = 0.0;
//                 for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
//                     height[h] = 10E3 + h*Delta;
//                     density_factor[h] = 100E3 / (100E3 + fabs(350E3-height[h]));
//                     sum += density_factor[h];
//                 }
//                 // scale so that density_factor sums to 1.0
//                 for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;

                // log spacing
                Real64 start = log(10E3);
                Real64 Delta = (log(25000E3)-start) / (NUM_HEIGHTS-1);
                Real64 sum = 0.0;
                for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
                    height[h] = exp(start + h*Delta);
                    density_factor[h] = 100E3 / (100E3 + fabs(350E3-height[h]));
                    density_factor[h] *= density_factor[h]*height[h];
//                    density_factor[h] *= density_factor[h];
                    sum += density_factor[h];
                }
                // scale so that density_factor sums to 1.0
                for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
            }
        }
                





        // Ok, let's fill in the arrays
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) continue;

            // Fill in y and sigma.  Here I am using the slant TEC, as
            // part of the scaling terms will depend on the height
            y[n] = observation[i].STEC;
            sigma[n] = observation[i].sigma_STEC;

            // get the VTEC scaling factors
            get_simple_VTEC_scaling(observation[i].El,
                                    station[observation[i].station_number].Elevation(),
                                    NUM_HEIGHTS,
                                    height,
                                    scale
                                    );
            // Now, get the model lat and lon.  For each height, calculate the
            // pierce points
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
                LatLon_Cart pierce =
                    station[observation[i].station_number].get_pierce_location(
                        observation[i].Az,
                        observation[i].El,
                        height[h],
                        radius_Earth
                        );
                LatLon_Cart equatorial_pos =
                    Ref_Point.get_equatorial_offset(pierce);
                lat[h] = equatorial_pos.Lat();
                lon[h] = equatorial_pos.Lon();
                range[h] =
                    station[observation[i].station_number].get_pierce_range(
                        observation[i].El,
                        height[h],
                        radius_Earth
                        );
                

                // correct the scaling term for the general electron density
                // scaling.  Note that this means that I want to convert a VTEC
                // fit parameter to a STEC observed value.
                scale[h] = density_factor[h] / scale[h];
            }
            if((NUM_HEIGHTS == 1) || (NUM_HEIGHTS == 2)){
                // don't bother with range stuff with only 1 or 2 points
            }
            else {
                // create a set of range deltas
                Real64 last = range[0];
                Real64 sum = 0.0;
                for(Uint32 h=1; h < NUM_HEIGHTS-1; h++) {
                    Real64 delta = 0.5*(range[h+1]-last);
                    last = range[h];
                    range[h] = delta;
                    sum += delta;
                }
                sum += (range[0] = range[1]);
                sum += (range[NUM_HEIGHTS-1] = range[NUM_HEIGHTS-2]);
                for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
                    //scale[h] *= range[h] / sum;
//                    printf("El %.3f Ht %E Rg %E Sc %E\n", observation[i].El, height[h], range[h], scale[h]);
                }
            }

            
            // fill in the x stuff
            calculate_multilayer_2D_linear_polynomials(
                NUM_PARAM,
                NUM_HEIGHTS,
                lon,
                lat,
                scale,
                x+(n*NUM_PARAM)
                );

            // Hey, try to fit for lat,lon of station
            if(NUM_PARAM>13) {
                LatLon_Cart equatorial_pos =
                    Ref_Point.get_equatorial_offset(station[observation[i].station_number]);
                Real64 lat_s = equatorial_pos.Lat();
                Real64 lon_s = equatorial_pos.Lon();
                x[((n+1)*NUM_PARAM)-1] = lat_s;
                x[((n+1)*NUM_PARAM)-2] = lon_s;
                x[((n+1)*NUM_PARAM)-3] = lat_s*lon_s;
//                 x[((n+1)*NUM_PARAM)-4] = lat_s*lat_s;
//                 x[((n+1)*NUM_PARAM)-5] = lon_s*lon_s;
            }

            // increment the fit data position
            n++;
        } // for i,n over NUM_OBSERVATIONS


        // do the fit
        Real64 std_dev = 0.0;
        Sint32 return_code = fit_data(Sint32(0xFFFE),
                                      Sint32(NUM_PARAM),
                                      Sint32(Num_Data),
                                      y,
                                      x,
                                      sigma,
                                      &std_dev,
                                      parameters,
                                      model
                                      );
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);

//         Ionosphere_IRI Iono_Model(1E-6,1E-4);
        
//         // Now, stuff in the model values
//         for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
//             // if not in use, skip
//             if(!use_flag[i]) {
//                 observation[i].model_VTEC = -999.0;
//                 observation[i].sigma_model_VTEC = std_dev;
//                 printf("Bfield 0\n");
//                 continue;
//             }

//             // Fill in y and sigma
//             observation[i].model_VTEC = model[n];
//             observation[i].sigma_model_VTEC = std_dev;

//             // get the VTEC scaling factors
//             get_simple_VTEC_scaling(observation[i].El,
//                                     station[observation[i].station_number].Elevation(),
//                                     NUM_HEIGHTS,
//                                     height,
//                                     scale
//                                     );
//             for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
//                 scale[h] = density_factor[h] / scale[h];
//             // Ok now, for each height, calculate the pierce point at that
//             // height.  Then get the polynomial coefficients.

//                 LatLon_Cart pierce =
//                     station[observation[i].station_number].get_pierce_location(
//                         observation[i].Az,
//                         observation[i].El,
//                         height[h],
//                         radius_Earth
//                         );
//                 LatLon_Cart equatorial_pos =
//                     Ref_Point.get_equatorial_offset(pierce);
//                 lat[h] = equatorial_pos.Lat();
//                 lon[h] = equatorial_pos.Lon();

//     struct tm eval_time;
//     eval_time.tm_sec = 0;
//     eval_time.tm_min = 0;
//     eval_time.tm_hour = 15;
//     eval_time.tm_mday = 5;
//         eval_time.tm_mon = 5;
//         eval_time.tm_year = 105;
//     eval_time.tm_isdst = -1;
//     mktime(&eval_time);
//                 Space_Vector B_field = Iono_Model.Magnetic_Field(
//                     pierce,
//                     eval_time
//                     );
//                 Space_Vector dir =
//                     pierce - station[observation[i].station_number];
//                 Space_Unit_Vector direction = dir.make_unit_vector();
//                 // get the dot product, which gives the B field component
//                 // along the line of sight.  This is as the wavel comes
//                 // toward the Earth, so it is actually antiparallel, or
//                 // need to use a minus sign.
//                 Real64 B_parallel = -B_field.dot_product(direction) ;
//                 scale[h] *= B_parallel;
//                 range[h] = B_parallel;
//             }
//             // fill in the x stuff
//             calculate_multilayer_2D_linear_polynomials(
//                 NUM_PARAM,
//                 NUM_HEIGHTS,
//                 lon,
//                 lat,
//                 scale,
//                 sigma
//                 );

//             Real64 sum = 0.0;
//             for(Uint32 p=0; p < NUM_PARAM-3; p++)
//                 sum += sigma[p] * parameters[p];
//             sum += x[((n+1)*NUM_PARAM)-1] * parameters[(NUM_PARAM)-1] * range[2];
//             sum += x[((n+1)*NUM_PARAM)-2] * parameters[(NUM_PARAM)-2] * range[2];
//             sum += x[((n+1)*NUM_PARAM)-3] * parameters[(NUM_PARAM)-3] * range[2];
//             fprintf(stdout, "Bfield %E\n", sum);


            

//             // increment the fit data position
//             n++;
//          } // for i,n over NUM_OBSERVATIONS
        // Now, stuff in the model values
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if((!use_flag[i]) && (observation[i].El > 0.0)) {
                // get the VTEC scaling factors
                get_simple_VTEC_scaling(observation[i].El,
                                        station[observation[i].station_number].Elevation(),
                                        NUM_HEIGHTS,
                                        height,
                                        scale
                                        );
                // Now, get the model lat and lon.  For each height, calculate
                // pierce points
                for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
                    LatLon_Cart pierce =
                        station[observation[i].station_number].get_pierce_location(
                            observation[i].Az,
                            observation[i].El,
                            height[h],
                            radius_Earth
                            );
                    LatLon_Cart equatorial_pos =
                        Ref_Point.get_equatorial_offset(pierce);
                    lat[h] = equatorial_pos.Lat();
                    lon[h] = equatorial_pos.Lon();

                    // correct the scaling term for the general electron density
                    // scaling.  Note that this means that I want to convert a VTEC
                    // fit parameter to a STEC observed value.
                    scale[h] = density_factor[h] / scale[h];
                }

            
                // fill in the x stuff
                calculate_multilayer_2D_linear_polynomials(
                    NUM_PARAM,
                    NUM_HEIGHTS,
                    lon,
                    lat,
                    scale,
                    x
                    );
                Real64 fit = 0.0;
                for(Uint32 j=0; j < NUM_PARAM; j++) fit += parameters[j]*x[j];
                observation[i].model_VTEC = fit;
                observation[i].sigma_model_VTEC = std_dev;
                continue;
            }
            else if(!use_flag[i]) {
                observation[i].model_VTEC = 0.0;
                observation[i].sigma_model_VTEC = std_dev;
                continue;
            }

            // Fill in y and sigma
            observation[i].model_VTEC = model[n];
            observation[i].sigma_model_VTEC = std_dev;

            // increment the fit data position
            n++;
         } // for i,n over NUM_OBSERVATIONS
        


        // free up the memory
        delete[] y;
        delete[] x;
        delete[] sigma;
        delete[] model;
        delete[] height;
        delete[] density_factor;
        delete[] scale;
        delete[] range;
        delete[] lat;
        delete[] lon;
        

        return;
    }
    
    




















//_TITLE  calculate_manylayer_2D_linear_polynomials
void calculate_manylayer_2D_linear_polynomials(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters needed,
                               //    must be in 0 < NUM_PARAM < 10000
                               //    (although anything above 36 is almost
                               //    certainly suspect
        const Real64 x,        // I  the "x" value, typically the longitude here
        const Real64 y,        // I  the "y" value, typically the latitude here
        const Real64 scale,    // I  a scaling constant to multiply the poly's
        Real64* const poly     // O  the polynomial holder, as poly[NUM_PARAM]
        )


//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will compute the value of a set of polynomials for
//	a 2D surface fit (fitting z as a function of x and y).  It is intended
//	for fitting ionospheric data to latitude and longitude positions.

//	This function generates a simple, straightforward set of polynomials,
//	starting with the lowest order polynomial values in x and y, and working
//	up to get NUM_PARAM values.  As I expect the ionosphere to vary more with
//	latitude, this will go up in y order slightly faster than in x.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	Does not pay attention to whether x^n or y^n is still a valid
//	Real64, so don't expect to go to too high of an order n.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 08  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  calculate_manylayer_2D_linear_polynomials\n");
        if(NUM_PARAM == 0) {
            // nothing to do
            return;
        }
        //  negative polynomials requested, so why call this?
        else if(NUM_PARAM < 0) {
            fprintf(stderr, "Error: cannot do negative polys here in %s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(1);
        }

        // FIrst, set the initial scaling
        for(Uint32 n=0; n < NUM_PARAM; n++) poly[n] = scale;
        
        // Ok, I am going to need to hold onto the x and y
        // max poly values.
        Real64 x_max = 1.0;
        Real64 y_max = 1.0;
        // And I need to know how many polys have been made to date.
        Uint32 num_param_done = 0;

        // ok, off we go
        for(Uint32 n_max = 0; /* no check */; n_max++, x_max *= x, y_max *= y) {
            // start up the x axis first, since I expect greater latitude
            // variations.  (this means that the y_max value is used first.)
            Real64 x_now = 1.0;
            for(Uint32 n_x = 0; n_x <= n_max; n_x++, x_now *= x) {
                Real64 value = y_max * x_now;
                poly[num_param_done++] *= value;
                if(num_param_done == NUM_PARAM) return;
            }
            // And now do the other set with y, being careful not to duplicate
            // the last polynomial
            Real64 y_now = 1.0;
            for(Uint32 n_y = 0; n_y < n_max; n_y++, y_now *= y) {
                Real64 value = x_max * y_now;
                poly[num_param_done++] *= value;
                if(num_param_done == NUM_PARAM) return;
            }
        }
        // it should be impossible to get here
#ifdef DEBUG
        fprintf(stderr, "Error: how did I get here? %s:%d:%s\n",
                __FILE__, __LINE__, __func__);
        exit(1);
#endif
        return;
    }












//_TITLE  fit_manylayer_linear_model --fit a surface to the observations
void fit_manylayer_linear_model(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a multilayer linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.


// 	This function will generate it own set of height and pierce point
// 	information, based on the number of heights requested.  This routine
//	assumes that the main ionospheric height is around 350 km, with
//	the ionospheric electron density decreasing above and below.  For
//	each observation, the pierce points are calculated for the various
//	heights.  These are used to generate the polynomial terms, which are
//	combined into their respective x^m y^n terms for the fitting process.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 07  James M Anderson  --JIVE  start

//_END

    {
        //printf("in linfit:  fit_manylayer_linear_model\n");
        // Ok, how many good observations are there?
        Uint32 Num_Data = 0;
        for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
            if((use_flag[i])) Num_Data++;

        // Now, compare with the number of parameters desired
        if( (Num_Data < NUM_PARAM) || (NUM_PARAM <= 0) || (NUM_HEIGHTS <= 0) ) {
            fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u HEIGHTS with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_HEIGHTS, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now, allocate some space to deal with the arrays needed by the fitter
        Real64* y = new Real64[Num_Data];
        Real64* x = new Real64[Num_Data*NUM_PARAM];  //x[NUM_DATA][NUM_PARAM],
        Real64* sigma = new Real64[Num_Data];
        Real64* model = new Real64[Num_Data];
        Real64* height = new Real64[NUM_HEIGHTS];
        Real64* path_distance = new Real64[NUM_HEIGHTS];
        Uint32* param_max = new Uint32[NUM_HEIGHTS];
        Uint32* param_off = new Uint32[NUM_HEIGHTS];

        if( (y==NULL) || (x==NULL) || (sigma==NULL)
            || (model==NULL) || (height==NULL) 
            || (path_distance==NULL) || (param_max==NULL)
            || (param_off==NULL) ) {
            fprintf(stderr, "Error: cannot allocate enough memory for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                    NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }






        // Now, create a set of heights
        {
            if(NUM_HEIGHTS == 1) {
                // only 1 height, so use 350 km
                height[0] = 350E3; // m
            }
            else if(NUM_HEIGHTS == 2) {
                // 2 heights, so use something surrounding 350 km
                height[0] = 250E3; // m
                height[1] = 500E3; // m
            }
            else if(NUM_HEIGHTS == 3) {
                // 3 heights, so use something surrounding 350 km
                height[0] = 250E3; // m
                height[1] = 350E3; // m
                height[2] = 500E3; // m
            }
            else if(NUM_HEIGHTS == 4) {
                // 4 heights, so use something surrounding 350 km
                height[0] = 250E3; // m
                height[1] = 350E3; // m
                height[2] = 500E3; // m
                height[3] = 2000E3; // m
            }
            else {
                // for everything else, create a uniform spacing
                // log spacing
                Real64 start = log(40E3);
                Real64 Delta = (log(15000E3)-start) / (NUM_HEIGHTS-1);
                for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
                    height[h] = exp(start + h*Delta);
                }
            }
        } // generating heights


        // Figure out how many parameters for each height
        {
            Uint32 min_param = NUM_PARAM / NUM_HEIGHTS;
            Uint32 overflow = NUM_PARAM - min_param*NUM_HEIGHTS;
            for(Uint32 h=0, o=0; h < NUM_HEIGHTS; h++, o++) {
                param_max[h] = (o<overflow) ? min_param+1:min_param;
            }
            param_off[0] = 0;
            for(Uint32 h=1; h < NUM_HEIGHTS; h++) {
                param_off[h] = param_off[h-1] + param_max[h-1];
            }
        }
        
                





        // Ok, let's fill in the arrays
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) continue;

            // Fill in y and sigma.  Here I am using the slant TEC, as
            // part of the scaling terms will depend on the height
            y[n] = observation[i].STEC;
            sigma[n] = observation[i].sigma_STEC;

            // For each height, calculate the range to the height. And from that,
            // get the path distances through the ionosphere at eight "height"
            {
                if(NUM_HEIGHTS == 1) {
                    // there is only 1 height.  Get the range to the height
                    path_distance[0] =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            height[0],
                            radius_Earth
                            );
                }
                else if(NUM_HEIGHTS == 2) {
                    // There are 2 points.  The path_distance for the first is
                    // going to be the range to the mid-height.  The
                    // path_distance to the second is going to be the difference
                    // with the range to the height + 200 km and the mid-height.
                    Real64 range_0 =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            0.5*(height[0]+height[1]),
                            radius_Earth
                            );
                    Real64 range_1 =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            height[1]+ 200E3,
                            radius_Earth
                            );
                    path_distance[0] = range_0;
                    path_distance[1] = range_1-range_0;
                }
                else if(NUM_HEIGHTS == 3) {
                    // There are 3 points.  The path_distance for the first is
                    // going to be the range to the 0--1 mid-height.  The
                    // path_distance to the second is going to be the difference
                    // mid 0--1 and mid 1--2.  The third is going to be the
                    // difference with that and the final height + 1000 km
                    Real64 range_0 =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            0.5*(height[0]+height[1]),
                            radius_Earth
                            );
                    Real64 range_1 =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            0.5*(height[1]+height[2]),
                            radius_Earth
                            );
                    Real64 range_2 =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            height[1]+ 1000E3,
                            radius_Earth
                            );
                    path_distance[0] = range_0;
                    path_distance[1] = range_1-range_0;
                    path_distance[2] = range_2-range_1;
                }
                else {
                    // There are many points.  The path_distance for the first is
                    // going to be the range to the 0--1 mid-height.  This goes
                    // on with the next height, and so on.
                    // The last path distance is going to be the
                    // difference with the next-to-last midpoint
                    // and 25000 km
                    Real64 range_last = 0.0;
                    for(Uint32 h=0; h < NUM_HEIGHTS-1; h++) {
                        Real64 range_now = 
                            station[observation[i].station_number].get_pierce_range(
                                observation[i].El,
                                0.5*(height[h]+height[h+1]),
                                radius_Earth
                                );
                        path_distance[h] = range_now - range_last;
                        range_last = range_now;
                    }
                    Real64 range_end =
                        station[observation[i].station_number].get_pierce_range(
                            observation[i].El,
                            25000E3,
                            radius_Earth
                            );
                    path_distance[NUM_HEIGHTS-1] = range_end - range_last;
                }
            } // calculate the path distances
                
            // Ok now, for each height, calculate the pierce point at that
            // height.  Then get the polynomial coefficients.
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
                LatLon_Cart pierce_point =
                    station[observation[i].station_number].get_pierce_location(
                        observation[i].Az,
                        observation[i].El,
                        height[h],
                        radius_Earth
                        );
                LatLon_Cart equatorial_pos =
                    Ref_Point.get_equatorial_offset(pierce_point);

                // fill in the x stuff
                calculate_manylayer_2D_linear_polynomials(
                    param_max[h],
                    equatorial_pos.Lon(),
                    equatorial_pos.Lat(),
                    path_distance[h],
                    x+(n*NUM_PARAM)+param_off[h]
                    );
            }

            

            // increment the fit data position
            n++;
        } // for i,n over NUM_OBSERVATIONS


        // do the fit
        Real64 std_dev = 0.0;
        Sint32 return_code = fit_data(Sint32(0xFFFE),
                                      Sint32(NUM_PARAM),
                                      Sint32(Num_Data),
                                      y,
                                      x,
                                      sigma,
                                      &std_dev,
                                      parameters,
                                      model
                                      );
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);


        // Now, stuff in the model values
        for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
            // if not in use, skip
            if(!use_flag[i]) {
                observation[i].model_VTEC = -999.0;
                observation[i].sigma_model_VTEC = std_dev;
                continue;
            }

            // Fill in y and sigma
            observation[i].model_VTEC = model[n];
            observation[i].sigma_model_VTEC = std_dev;



            

            // increment the fit data position
            n++;
         } // for i,n over NUM_OBSERVATIONS
        


        // free up the memory
        delete[] y;
        delete[] x;
        delete[] sigma;
        delete[] model;
        delete[] height;
        delete[] path_distance;
        delete[] param_max;
        delete[] param_off;
        

        return;
    }


    


















































////////////////////////////////////
// GPS fitting area










//_TITLE  fit_simple_linear_model_2 --fit a surface to the observations
Sint32 fit_simple_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        const bool Force_Zero_Mean_Bias,
                               // I  Should the bias level be forced to 0?
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)]
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_simple_linear_model_2
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 SVD fitter failed
//                                   -3 more stations and objects than parameters

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a simple linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 24  James M Anderson  --JIVE  start
//	2007 Jan 03  JMA  --rework as new GPS function _2
// 	2007 Jan 19  JMA  --add in stuff for station and object biases

//_END

{
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    else if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add on a final line to force zero mean bias
        Num_Data++;
    }
    const Uint32 Num_Min_Data = Num_Data;
    Num_Data += USE_BIAS_FLAG;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Min_Data < NUM_PARAM) || (NUM_PARAM <= 0) ) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Min_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_PARAM;
    // x[NUM_DATA][NUM_PARAM],
    Real64* sigma = workspace; workspace += Num_Data;
    Real64* model = workspace; workspace += Num_Data;

    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        y[n] = observation[i].VTEC;
        sigma[n] = observation[i].sigma_VTEC;

        // Now, get the model lat and lon
        LatLon_Cart equatorial_pos =
            Ref_Point.get_equatorial_offset(observation[i].pierce_point);
        const Real64 lat = equatorial_pos.Lat();
        const Real64 lon = equatorial_pos.Lon();
        // fill in the x stuff
        calculate_simple_2D_linear_polynomials(
            NUM_POLY,
            lon,
            lat,
            x+(n*NUM_PARAM)
            );
        if((NUM_POLY > 3) && (observation[i].sigma_model_VTEC >= 0.0)) {
            x[(n*NUM_PARAM)+NUM_POLY-1] = observation[i].model_VTEC;
        }
        if((USE_BIAS_FLAG)) {
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                dp[NUM_POLY + observation[i].station_receiver_id] =
                    1.0 * observation[i].VTEC/observation[i].STEC;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                dp[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0 * observation[i].VTEC/observation[i].STEC;
        }
                    
                

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add a line for zero mean bias
        // Fill in y and sigma
        y[n] = 0.0;
        sigma[n] = 1.0E10;
        Real64* dp = x+(n*NUM_PARAM);
        for(Uint32 j=0; j < NUM_POLY; j++) dp[j] = 0.0;
        for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 1.0;
        n++;
    }
    if(n != Num_Min_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }
    // Now check if there are any station/receiver/objects which have no data
    // and force their biases to zero
    for(Uint32 s = 0; s < USE_BIAS_FLAG; s++) {
        int flag = 0;
        for(Uint32 i=0; i < Num_Min_Data-1; i++) {
            Real64* dp = x+(i*NUM_PARAM);
            if(dp[NUM_POLY+s] != 0.0) {
                flag = 1;
                break;
            }
        }
        if(flag == 0) {
            // No data for this station/receiver/object.
            // force the parameter to 0
            y[n] = 0.0;
            sigma[n] = 1.0E10;
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=0; j < NUM_PARAM; j++) dp[j] = 0.0;
            dp[NUM_POLY+s] = 1.0;
            n++;
        }
    }
        


    // do the fit
    std_dev = 0.0;
    Sint32 return_code = fit_data_2(Sint32(0x0),//Sint32(0xFFFE),//
//    Sint32 return_code = fit_data_2(Sint32(0xFFFE),
                                    Sint32(NUM_PARAM),
                                    Sint32(n),
                                    y,
                                    x,
                                    sigma,
                                    workspace,
                                    &std_dev,
                                    parameters,
                                    model
                                    );
    if(return_code != 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }

    // Now, stuff in the model values
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        observation[i].model_VTEC = model[n];
        observation[i].sigma_model_VTEC = std_dev;

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    return 0;
}











//_TITLE  fit_simple_linear_model_2_grad --fit a surface to the observations
Sint32 fit_simple_linear_model_2_grad(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_POLY+7)
                               //              +NUM_PARAM*3]
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_simple_linear_model_2_grad
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 fitter failed
//                                   -3 more stations and objects than parameters

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a simple linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.

//	This function uses a gradient minimization routine to do the least
//	squares fitting rather than a matrix inversion.  It is therefore faster
//	but may not find the true minimum.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 24  James M Anderson  --JIVE  start
//	2007 Jan 03  JMA  --rework as new GPS function _2
// 	2007 Jan 19  JMA  --add in stuff for station and object biases
//	2007 Mar 26  JMA  --convert over for gradient minimization

//_END

{
    //printf("in linfit:  fit_simple_linear_model_2_grad\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Data < NUM_PARAM) || (NUM_PARAM <= 0) ) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_POLY;
                // x[NUM_DATA][NUM_POLY],
    Real64* A = workspace; workspace += Num_Data*NUM_POLY;
    char* Vary_Array = (char*)workspace; workspace += NUM_PARAM;

    Real64* station_val = NULL;
    Real64* object_val = NULL;
    Real64* station_orig = NULL;
    Real64* object_orig = NULL;
    Uint32* station_pos = (Uint32*)workspace;
    Uint32* object_pos = station_pos+Num_Data;  
    if((USE_BIAS_FLAG)) {
        workspace += Num_Data;  // allow space for the positions
        // Note that 1 Real64 can hold 2 Uint32s
        if((NUM_STATIONS)) {
            station_val = workspace; workspace += Num_Data;
            station_orig = workspace; workspace += Num_Data;
            for(Uint32 i=0; i < Num_Data; i++) {
                station_orig[i] = station_val[i] = 0.0;
                station_pos[i] = 0;
            }
        }
        if((NUM_OBJECTS)) {
            object_val = workspace; workspace += Num_Data;
            object_orig = workspace; workspace += Num_Data;
            for(Uint32 i=0; i < Num_Data; i++) {
                object_orig[i] = object_val[i] = 0.0;
                object_pos[i] = 0;
            }
        }
    }

    

    // Ok, let's fill in the arrays
    Uint32 n=0;
    Real64 sum_sqr = 0.0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        Real64 weight = observation[i].sigma_VTEC;
        if(weight > 0.0) weight = 1.0 / weight;
        else weight = 0.0;
        // Fill in y
        y[n] = observation[i].VTEC * weight;
        sum_sqr += observation[i].VTEC*observation[i].VTEC;

        // Now, get the model lat and lon
        LatLon_Cart equatorial_pos =
            Ref_Point.get_equatorial_offset(observation[i].pierce_point);
        const Real64 lat = equatorial_pos.Lat();
        const Real64 lon = equatorial_pos.Lon();
        // fill in the x stuff
        calculate_simple_2D_linear_polynomials(
            NUM_POLY,
            lon,
            lat,
            x+(n*NUM_POLY)
            );
        if((NUM_POLY > 3) && (observation[i].sigma_model_VTEC >= 0.0)) {
            x[(n*NUM_POLY)+NUM_POLY-1] = observation[i].model_VTEC;
        }
        for(Uint32 p = 0; p < NUM_POLY; p++)
            A[(n*NUM_POLY)+p] = x[(n*NUM_POLY)+p] * weight;
        if((USE_BIAS_FLAG)) {
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX)) {
                station_orig[n] = 1.0 * observation[i].VTEC/observation[i].STEC;
                station_val[n] = station_orig[n] * weight;
                station_pos[n] = NUM_POLY + observation[i].station_receiver_id;
            }
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX)) {
                object_orig[n] = 1.0 * observation[i].VTEC/observation[i].STEC;
                object_val[n] = object_orig[n] * weight;
                object_pos[n] =
                    NUM_POLY + NUM_STATIONS + observation[i].object_id;
            }
        }
        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    if(n != Num_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }

    // do the fit
    Sint32 return_code = component_fit_gradient_1(
        NUM_PARAM,
        NUM_POLY,
        NUM_STATIONS,
        NUM_OBJECTS,
        0,
        Num_Data,
        A,
        y,
        station_val,
        station_pos,
        object_val,
        object_pos,
        NULL,
        NULL,
        1.0E-3,
        parameters,
        Vary_Array,
        workspace
        );
    if(return_code < 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }

    // Now, stuff in the model values and calculate the standard deviation
    //fprintf(stderr, "Original sum_sqr value %E\n", sum_sqr);
    sum_sqr = 0.0;
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        Real64 value = 0.0;
        for(Uint32 p=0; p < NUM_POLY; p++) {
            value += x[(n*NUM_POLY)+p] * parameters[p];
        }
        if((USE_BIAS_FLAG)) {
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX)) {
                value += station_orig[n] * parameters[station_pos[n]];
            }
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX)) {
                value += object_orig[n] * parameters[object_pos[n]];
            }
        }        

        // Fill in y and sigma
        observation[i].model_VTEC = value;
        value -= observation[i].VTEC;
        sum_sqr += value*value;
        // increment the fit data position
        n++;
     } // for i,n over NUM_OBSERVATIONS
    if((sum_sqr > 0.0) && (Num_Data > NUM_PARAM))
       std_dev = sqrt(sum_sqr / (Num_Data-NUM_PARAM));
    else std_dev = 0.0;
    //fprintf(stderr, "Got residual level sum_sqr %E std_dev %E\n",sum_sqr,std_dev);
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        observation[i].sigma_model_VTEC = std_dev;
    }
//     for(Uint32 p=0; p < NUM_PARAM; p++) {
//         printf("parameter %4u %E\n", p, parameters[p]);
//     }

    return 0;
}




    
    
//_TITLE  apply_simple_linear_model_2 --apply 2-D MIM to observations
void apply_simple_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        const Real64* const parameters,
                               // I  the fit parameters, as parameters[NUM_PARAM]
        const Real64 std_dev,  // I  The standard deviation from the fit.
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_PARAM]
        Observation_Ionosphere* const observation
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function takes in the linear fit parameters and calculates model
//	ionosphere values for a set of observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 03  JMA  --JIVE  start
// 	2007 Jan 19  JMA  --add in stuff for station and object biases

//_END

{
    //printf("in linfit:  apply_simple_linear_model_2\n");
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    // loop over and create the model values
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;
        if(observation[i].El <= 0.0) {
            observation[i].sigma_model_VTEC = -1.0;
            continue;
        }

        LatLon_Cart equatorial_pos =
            Ref_Point.get_equatorial_offset(observation[i].pierce_point);
        const Real64 lat = equatorial_pos.Lat();
        const Real64 lon = equatorial_pos.Lon();
        // fill in the x stuff.  Note that x is now free to use
        calculate_simple_2D_linear_polynomials(
            NUM_POLY,
            lon,
            lat,
            workspace
            );
        if((NUM_POLY > 3) && (observation[i].sigma_model_VTEC >= 0.0)) {
            workspace[NUM_POLY-1] = observation[i].model_VTEC;
        }
        if((USE_BIAS_FLAG)) {
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) workspace[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                workspace[NUM_POLY + observation[i].station_receiver_id] =
                    1.0 * observation[i].VTEC/observation[i].STEC;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                workspace[NUM_POLY + NUM_STATIONS + observation[i].object_id] = 1.0 * observation[i].VTEC/observation[i].STEC;
        }
        Real64 fit = 0.0;
        for(Uint32 j=0; j <NUM_PARAM; j++) fit += parameters[j]*workspace[j];
        //for(Uint32 j=0; j < NUM_PARAM; j++) printf("%8.4f ", workspace[j]);
        //printf("      %E\n", fit);
        observation[i].model_VTEC = fit;
        observation[i].sigma_model_VTEC = std_dev;
     } // for i over NUM_OBSERVATIONS
    return;
}        









//_TITLE  fit_2D_time_linear_model_2 --fit a surface/time to the observations
Sint32 fit_2D_time_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_TIME_TERMS,
                               // I  The maximum order of time polynomials
                               //    to apply.  The ionosphere is made to have
                               //    a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                               //    dependence, where N \equiv NUM_TIME_TERMS-1
                               //    Thus, 1 means constant, 2 for linear, 3
                               //    for quadratic, and so on.
        const Real64 Ref_MJD,  // I  The reference Modified Julian Date to
                               //    compare against for time calculations
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        const bool Force_Zero_Mean_Bias,
                               // I  Should the bias level be forced to 0?
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)
                               //              +NUM_TIME_TERMS*2]
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_2D_time_linear_model_2
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 SVD fitter failed
//                                   -3 more stations and objects than parameters

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a simple linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 19  James M Anderson  --JIVE  start

//_END

{
    //printf("in linfit:  fit_2D_time_linear_model_2\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    else if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add on a final line to force zero mean bias
        Num_Data++;
    }
    const Uint32 Num_Min_Data = Num_Data;
    Num_Data += USE_BIAS_FLAG;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Min_Data < NUM_PARAM) || (NUM_PARAM <= 0)
        || (NUM_TIME_TERMS <= 0)) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u TIME_TERMS with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_TIME_TERMS, NUM_OBSERVATIONS, Num_Min_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_PARAM;
    // x[NUM_DATA][NUM_PARAM],
    Real64* sigma = workspace; workspace += Num_Data;
    Real64* model = workspace; workspace += Num_Data;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_TIME_TERMS;
    Uint32* param_off = param_max+NUM_TIME_TERMS; 

    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_TIME_TERMS;
        Uint32 overflow = NUM_POLY - min_param*NUM_TIME_TERMS;
        for(Uint32 h=0, o=0; h < NUM_TIME_TERMS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_TIME_TERMS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }


    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        y[n] = observation[i].VTEC;
        sigma[n] = observation[i].sigma_VTEC;

        // Now, get the model lat and lon
        LatLon_Cart equatorial_pos =
            Ref_Point.get_equatorial_offset(observation[i].pierce_point);
        const Real64 lat = equatorial_pos.Lat();
        const Real64 lon = equatorial_pos.Lon();
        // Ok, now to fill in the position polynomials and the time
        // dependence.
        Real64 Delta_t = Ref_MJD - observation[i].MJD;
        Real64 t_term = 1.0;
        for(Uint32 t=0; t < NUM_TIME_TERMS; t++, t_term *= Delta_t) {
            calculate_manylayer_2D_linear_polynomials(
                param_max[t],
                lon,
                lat,
                t_term,
                x+(n*NUM_PARAM)+param_off[t]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_VTEC >= 0.0)) {
            x[(n*NUM_PARAM)+NUM_POLY-1] = observation[i].model_VTEC;
        }
        if((USE_BIAS_FLAG)) {
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                dp[NUM_POLY + observation[i].station_receiver_id] =
                    1.0 * observation[i].VTEC/observation[i].STEC;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                dp[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0 * observation[i].VTEC/observation[i].STEC;
        }
                    
                

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add a line for zero mean bias
        // Fill in y and sigma
        y[n] = 0.0;
        sigma[n] = 1.0E10;
        Real64* dp = x+(n*NUM_PARAM);
        for(Uint32 j=0; j < NUM_POLY; j++) dp[j] = 0.0;
        for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 1.0;
        n++;
    }
    if(n != Num_Min_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }
    // Now check if there are any station/receiver/objects which have no data
    // and force their biases to zero
    for(Uint32 s = 0; s < USE_BIAS_FLAG; s++) {
        int flag = 0;
        for(Uint32 i=0; i < Num_Min_Data-1; i++) {
            Real64* dp = x+(i*NUM_PARAM);
            if(dp[NUM_POLY+s] != 0.0) {
                flag = 1;
                break;
            }
        }
        if(flag == 0) {
            // No data for this station/receiver/object.
            // force the parameter to 0
            y[n] = 0.0;
            sigma[n] = 1.0E10;
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=0; j < NUM_PARAM; j++) dp[j] = 0.0;
            dp[NUM_POLY+s] = 1.0;
            n++;
        }
    }
        


    // do the fit
    std_dev = 0.0;
    Sint32 return_code = fit_data_2(Sint32(0x0),//Sint32(0xFFFE),//
//    Sint32 return_code = fit_data_2(Sint32(0xFFFE),
                                    Sint32(NUM_PARAM),
                                    Sint32(n),
                                    y,
                                    x,
                                    sigma,
                                    workspace,
                                    &std_dev,
                                    parameters,
                                    model
                                    );
    if(return_code != 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }

    // Now, stuff in the model values
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        observation[i].model_VTEC = model[n];
        observation[i].sigma_model_VTEC = std_dev;

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    return 0;
}





//_TITLE  fit_2D_time_linear_model_2_grad --fit surface/time to the observations
Sint32 fit_2D_time_linear_model_2_grad(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_TIME_TERMS,
                               // I  The maximum order of time polynomials
                               //    to apply.  The ionosphere is made to have
                               //    a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                               //    dependence, where N \equiv NUM_TIME_TERMS-1
                               //    Thus, 1 means constant, 2 for linear, 3
                               //    for quadratic, and so on.
        const Real64 Ref_MJD,  // I  The reference Modified Julian Date to
                               //    compare against for time calculations
        const Station_Reference& Ref_Point,
                               // I  The reference point

        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_POLY+7)
                               //              +NUM_PARAM*6
                               //              +NUM_TIME_TERMS*2]
        Observation_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_2D_time_linear_model_2_grad
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 fitter failed
//                                   -3 more stations and objects than parameters

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a simple linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 19  James M Anderson  --JIVE  start
//	2007 Mar 27  JMA  --convert over for gradient minimization

//_END

{
    //printf("in linfit:  fit_2D_time_linear_model_2_grad\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Data < NUM_PARAM) || (NUM_PARAM <= 0)
        || (NUM_TIME_TERMS <= 0)) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u TIME_TERMS with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_TIME_TERMS, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_POLY;
                // x[NUM_DATA][NUM_POLY],
    Real64* A = workspace; workspace += Num_Data*NUM_POLY;
    char* Vary_Array = (char*)workspace; workspace += NUM_PARAM;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_TIME_TERMS;
    Uint32* param_off = param_max+NUM_TIME_TERMS; 

    Real64* station_val = NULL;
    Real64* object_val = NULL;
    Real64* station_orig = NULL;
    Real64* object_orig = NULL;
    Uint32* station_pos = (Uint32*)workspace;
    Uint32* object_pos = station_pos+Num_Data;  
    if((USE_BIAS_FLAG)) {
        workspace += Num_Data;  // allow space for the positions
        // Note that 1 Real64 can hold 2 Uint32s
        if((NUM_STATIONS)) {
            station_val = workspace; workspace += Num_Data;
            station_orig = workspace; workspace += Num_Data;
            for(Uint32 i=0; i < Num_Data; i++) {
                station_orig[i] = station_val[i] = 0.0;
                station_pos[i] = 0;
            }
        }
        if((NUM_OBJECTS)) {
            object_val = workspace; workspace += Num_Data;
            object_orig = workspace; workspace += Num_Data;
            for(Uint32 i=0; i < Num_Data; i++) {
                object_orig[i] = object_val[i] = 0.0;
                object_pos[i] = 0;
            }
        }
    }
    // Figure out how many parameters for each time
    {
        Uint32 min_param = NUM_POLY / NUM_TIME_TERMS;
        Uint32 overflow = NUM_POLY - min_param*NUM_TIME_TERMS;
        for(Uint32 h=0, o=0; h < NUM_TIME_TERMS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_TIME_TERMS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }


    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        Real64 weight = observation[i].sigma_VTEC;
        if(weight > 0.0) weight = 1.0 / weight;
        else weight = 0.0;
        // Fill in y
        y[n] = observation[i].VTEC * weight;

        // Now, get the model lat and lon
        LatLon_Cart equatorial_pos =
            Ref_Point.get_equatorial_offset(observation[i].pierce_point);
        const Real64 lat = equatorial_pos.Lat();
        const Real64 lon = equatorial_pos.Lon();
        // Ok, now to fill in the position polynomials and the time
        // dependence.
        Real64 Delta_t = Ref_MJD - observation[i].MJD;
        Real64 t_term = 1.0;
        for(Uint32 t=0; t < NUM_TIME_TERMS; t++, t_term *= Delta_t) {
            calculate_manylayer_2D_linear_polynomials(
                param_max[t],
                lon,
                lat,
                t_term,
                x+(n*NUM_POLY)+param_off[t]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_VTEC >= 0.0)) {
            x[(n*NUM_POLY)+NUM_POLY-1] = observation[i].model_VTEC;
        }
        for(Uint32 p = 0; p < NUM_POLY; p++)
            A[(n*NUM_POLY)+p] = x[(n*NUM_POLY)+p] * weight;
        if((USE_BIAS_FLAG)) {
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX)) {
                station_orig[n] = 1.0 * observation[i].VTEC/observation[i].STEC;
                station_val[n] = station_orig[n] * weight;
                station_pos[n] = NUM_POLY + observation[i].station_receiver_id;
            }
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX)) {
                object_orig[n] = 1.0 * observation[i].VTEC/observation[i].STEC;
                object_val[n] = object_orig[n] * weight;
                object_pos[n] =
                    NUM_POLY + NUM_STATIONS + observation[i].object_id;
            }
        }
        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS
    if(n != Num_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }

    // do the fit
    Sint32 return_code = component_fit_gradient_1(
        NUM_PARAM,
        NUM_POLY,
        NUM_STATIONS,
        NUM_OBJECTS,
        0,
        Num_Data,
        A,
        y,
        station_val,
        station_pos,
        object_val,
        object_pos,
        NULL,
        NULL,
        1.0E-3,
        parameters,
        Vary_Array,
        workspace
        );
    if(return_code < 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }

    // Now, stuff in the model values
    Real64 sum_sqr = 0.0;
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        Real64 value = 0.0;
        for(Uint32 p=0; p < NUM_POLY; p++) {
            value += x[(n*NUM_POLY)+p] * parameters[p];
        }
        if((USE_BIAS_FLAG)) {
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX)) {
                value += station_orig[n] * parameters[station_pos[n]];
            }
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX)) {
                value += object_orig[n] * parameters[object_pos[n]];
            }
        }      

        // Fill in y and sigma
        observation[i].model_VTEC = value;
        value -= observation[i].VTEC;
        sum_sqr += value*value;    
        // increment the fit data position
        n++;
     } // for i,n over NUM_OBSERVATIONS
    if((sum_sqr > 0.0) && (Num_Data > NUM_PARAM))
       std_dev = sqrt(sum_sqr / (Num_Data-NUM_PARAM));
    else std_dev = 0.0;
    //fprintf(stderr, "Got residual level sum_sqr %E std_dev %E\n",sum_sqr,std_dev);
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        observation[i].sigma_model_VTEC = std_dev;
    }

    return 0;
}




    
    
//_TITLE  apply_2D_time_linear_model_2 --apply 2-D+time MIM to observations
void apply_2D_time_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_TIME_TERMS,
                               // I  The maximum order of time polynomials
                               //    to apply.  The ionosphere is made to have
                               //    a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                               //    dependence, where N \equiv NUM_TIME_TERMS-1
                               //    Thus, 1 means constant, 2 for linear, 3
                               //    for quadratic, and so on.
        const Real64 Ref_MJD,  // I  The reference Modified Julian Date to
                               //    compare against for time calculations
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        const Real64* const parameters,
                               // I  the fit parameters, as parameters[NUM_PARAM]
        const Real64 std_dev,  // I  The standard deviation from the fit.
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_PARAM+NUM_TIME_TERMS*2]
        Observation_Ionosphere* const observation
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function takes in the linear fit parameters and calculates model
//	ionosphere values for a set of observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 19  JMA  --JIVE  start

//_END

{
    //printf("in linfit:  apply_2D_time_linear_model_2\n");
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;

    Uint32* param_max = (Uint32*)workspace; workspace += NUM_TIME_TERMS;
    Uint32* param_off = param_max+NUM_TIME_TERMS; 

    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_TIME_TERMS;
        Uint32 overflow = NUM_POLY - min_param*NUM_TIME_TERMS;
        for(Uint32 h=0, o=0; h < NUM_TIME_TERMS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_TIME_TERMS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }

    // loop over and create the model values
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;
        if(observation[i].El <= 0.0) {
            observation[i].sigma_model_VTEC = -1.0;
            continue;
        }

        LatLon_Cart equatorial_pos =
            Ref_Point.get_equatorial_offset(observation[i].pierce_point);
        const Real64 lat = equatorial_pos.Lat();
        const Real64 lon = equatorial_pos.Lon();
        // Ok, now to fill in the position polynomials and the time
        // dependence.
        Real64 Delta_t = Ref_MJD - observation[i].MJD;
        Real64 t_term = 1.0;
        for(Uint32 t=0; t < NUM_TIME_TERMS; t++, t_term *= Delta_t) {
            calculate_manylayer_2D_linear_polynomials(
                param_max[t],
                lon,
                lat,
                t_term,
                workspace+param_off[t]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_VTEC >= 0.0)) {
            workspace[NUM_POLY-1] = observation[i].model_VTEC;
        }
        if((USE_BIAS_FLAG)) {
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) workspace[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                workspace[NUM_POLY + observation[i].station_receiver_id] =
                    1.0 * observation[i].VTEC/observation[i].STEC;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                workspace[NUM_POLY + NUM_STATIONS + observation[i].object_id] = 1.0 * observation[i].VTEC/observation[i].STEC;
        }
        Real64 fit = 0.0;
        for(Uint32 j=0; j <NUM_PARAM; j++) fit += parameters[j]*workspace[j];
//             for(Uint32 j=0; j < NUM_PARAM; j++) printf("%8.4f ", workspace[j]);
//             printf("      %E\n", fit);
        observation[i].model_VTEC = fit;
        observation[i].sigma_model_VTEC = std_dev;
    } // for i over NUM_OBSERVATIONS
    return;
}        



//_TITLE  fit_multilayer_linear_model_2 --fit a surface to the observations
Sint32 fit_multilayer_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        const bool Force_Zero_Mean_Bias,
                               // I  Should the bias level be forced to 0?
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)
                               //              +NUM_HEIGHTS*6]
        Observation_3D_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_multilayer_linear_model_2
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 SVD fitter failed

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a multilayer linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.


// 	This function will generate it own set of height and pierce point
// 	information, based on the number of heights requested.  This routine
//	assumes that the main ionospheric height is around 350 km, with
//	the ionospheric electron density decreasing above and below.  For
//	each observation, the pierce points are calculated for the various
//	heights.  These are used to generate the polynomial terms, which are
//	combined into their respective x^m y^n terms for the fitting process.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 07  James M Anderson  --JIVE  start
//  	2007 Jan 03  JMA  --rework original code for GPS observations
//      2007 Mar 01  JMA  --update to be able to fit biases

//_END

{
    //printf("in linfit:  fit_multilayer_linear_model_2\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    else if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add on a final line to force zero mean bias
        Num_Data++;
    }
    const Uint32 Num_Min_Data = Num_Data;
    Num_Data += USE_BIAS_FLAG;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Min_Data <NUM_PARAM) || (NUM_PARAM <= 0) || (NUM_HEIGHTS<=0) ) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u HEIGHTS with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_HEIGHTS, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_PARAM;
    // x[NUM_DATA][NUM_PARAM],
    Real64* sigma = workspace; workspace += Num_Data;
    Real64* model = workspace; workspace += Num_Data;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Real64* scale = workspace; workspace += NUM_HEIGHTS;
    Real64* range = workspace; workspace += NUM_HEIGHTS;
    Real64* lat = workspace; workspace += NUM_HEIGHTS;
    Real64* lon = workspace; workspace += NUM_HEIGHTS;


    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    }
                





    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma.  Here I am using the slant TEC, as
        // part of the scaling terms will depend on the height
        y[n] = observation[i].STEC;
        sigma[n] = observation[i].sigma_STEC;

        // get the VTEC scaling factors
        get_simple_VTEC_scaling(observation[i].El,
                                observation[i].station->Elevation(),
                                NUM_HEIGHTS,
                                height,
                                scale
                                );
        // Now, get the model lat and lon.  For each height, calculate the
        // pierce points
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(pierce);
            lat[h] = equatorial_pos.Lat();
            lon[h] = equatorial_pos.Lon();
            range[h] = observation[i].station->get_pierce_range(
                observation[i].direction,
                height[h],
                radius_Earth
                );
                

            // correct the scaling term for the general electron density
            // scaling.  Note that this means that I want to convert a VTEC
            // fit parameter to an STEC observed value.
            scale[h] = density_factor[h] / scale[h];
        }

//             if((NUM_HEIGHTS == 1) || (NUM_HEIGHTS == 2)){
//                 // don't bother with range stuff with only 1 or 2 points
//             }
//             else {
//                 // create a set of range deltas
//                 Real64 last = range[0];
//                 Real64 sum = 0.0;
//                 for(Uint32 h=1; h < NUM_HEIGHTS-1; h++) {
//                     Real64 delta = 0.5*(range[h+1]-last);
//                     last = range[h];
//                     range[h] = delta;
//                     sum += delta;
//                 }
//                 sum += (range[0] = range[1]);
//                 sum += (range[NUM_HEIGHTS-1] = range[NUM_HEIGHTS-2]);
//                 for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
//                     //scale[h] *= range[h] / sum;
// //                    printf("El %.3f Ht %E Rg %E Sc %E\n", observation[i].El, height[h], range[h], scale[h]);
//                 }
//             }

            
        // fill in the x stuff
        calculate_multilayer_2D_linear_polynomials(
            NUM_POLY,
            NUM_HEIGHTS,
            lon,
            lat,
            scale,
            x+(n*NUM_PARAM)
            );
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[(n*NUM_PARAM)+NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            Real64* dp = x+(n*NUM_POLY);
            dp[NUM_POLY-2] = lat_s;
            dp[NUM_POLY-3] = lon_s;
            dp[NUM_POLY-4] = lat_s*lon_s;
        }
        if((USE_BIAS_FLAG)) {
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                dp[NUM_POLY + observation[i].station_receiver_id] = 1.0;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                dp[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0;
        }

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add a line for zero mean bias
        // Fill in y and sigma
        y[n] = 0.0;
        sigma[n] = 1.0E10;
        Real64* dp = x+(n*NUM_PARAM);
        for(Uint32 j=0; j < NUM_POLY; j++) dp[j] = 0.0;
        for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 1.0;
        n++;
    }
    if(n != Num_Min_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }
    // Now check if there are any station/receiver/objects which have no data
    // and force their biases to zero
    for(Uint32 s = 0; s < USE_BIAS_FLAG; s++) {
        int flag = 0;
        for(Uint32 i=0; i < Num_Min_Data-1; i++) {
            Real64* dp = x+(i*NUM_PARAM);
            if(dp[NUM_POLY+s] != 0.0) {
                flag = 1;
                break;
            }
        }
        if(flag == 0) {
            // No data for this station/receiver/object.
            // force the parameter to 0
            y[n] = 0.0;
            sigma[n] = 1.0E10;
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=0; j < NUM_PARAM; j++) dp[j] = 0.0;
            dp[NUM_POLY+s] = 1.0;
            n++;
        }
    }


    // do the fit
    std_dev = 0.0;
    Sint32 return_code = fit_data_2(Sint32(0x0),
//    Sint32 return_code = fit_data_2(Sint32(0xFFFE),
                                    Sint32(NUM_PARAM),
                                    Sint32(n),
                                    y,
                                    x,
                                    sigma,
                                    workspace,
                                    &std_dev,
                                    parameters,
                                    model
                                    );
    if(return_code != 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }
    // Now, stuff in the model values
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        observation[i].model_STEC = model[n];
        observation[i].sigma_model_STEC = std_dev;

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS
        


    return 0;
}
    
    




//_TITLE  apply_multilayer_linear_model_2 --apply a surface to the observations
void apply_multilayer_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        const Real64* const parameters,
                               // I  the fit parameters, as parameters[NUM_PARAM]
        const Real64 std_dev,  // I  The standard deviation from the fit.
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_PARAM + NUM_HEIGHTS*6]
        Observation_3D_Ionosphere* const observation
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This applys the fit parameters found by fit_multilayer_linear_model_2
//	to an arbitrary set of observations.

//	The inputs to theis function (NUM_PARAM, NUM_HEIGHTS, and so on,
//	through std_dev, with a different NUM_OBSERVATIONS possible)
//      must be the same as used in the call to
//	fit_multilayer_linear_model_2.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 04  James M Anderson  --JIVE  start

//_END
{
    //printf("in linfit: apply_multilayer_linear_model_2\n");
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;

    Real64* x = workspace; workspace += NUM_PARAM;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Real64* scale = workspace; workspace += NUM_HEIGHTS;
    Real64* range = workspace; workspace += NUM_HEIGHTS;
    Real64* lat = workspace; workspace += NUM_HEIGHTS;
    Real64* lon = workspace; workspace += NUM_HEIGHTS;

    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    }


    // Now, stuff in the model values
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;
        if(observation[i].El <= 0.0) {
            observation[i].sigma_model_STEC = -1.0;
            continue;
        }

        // get the VTEC scaling factors
        get_simple_VTEC_scaling(observation[i].El,
                                observation[i].station->Elevation(),
                                NUM_HEIGHTS,
                                height,
                                scale
                                );
        // Now, get the model lat and lon.  For each height, calculate
        // pierce points
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(pierce);
            lat[h] = equatorial_pos.Lat();
            lon[h] = equatorial_pos.Lon();
            range[h] = observation[i].station->get_pierce_range(
                observation[i].direction,
                height[h],
                radius_Earth
                );
                

            // correct the scaling term for the general electron density
            // scaling.  Note that this means that I want to convert a VTEC
            // fit parameter to an STEC observed value.
            scale[h] = density_factor[h] / scale[h];
        }

            
        // fill in the x stuff
        calculate_multilayer_2D_linear_polynomials(
            NUM_POLY,
            NUM_HEIGHTS,
            lon,
            lat,
            scale,
            x
            );
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            x[NUM_POLY-2] = lat_s;
            x[NUM_POLY-3] = lon_s;
            x[NUM_POLY-4] = lat_s*lon_s;
        }
        if((USE_BIAS_FLAG)) {
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) x[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                x[NUM_POLY + observation[i].station_receiver_id] = 1.0;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                x[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0;
        }

            
        Real64 fit = 0.0;
        for(Uint32 j=0; j < NUM_PARAM; j++) fit += parameters[j]*x[j];
        observation[i].model_STEC = fit;
        observation[i].sigma_model_STEC = std_dev;
    }
    return;
}
















//_TITLE  fit_manylayer_linear_model_2 --fit a surface to the observations
Sint32 fit_manylayer_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        const bool Force_Zero_Mean_Bias,
                               // I  Should the bias level be forced to 0?
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)
                               //              +NUM_HEIGHTS*5]
        Observation_3D_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_manylayer_linear_model_2
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 SVD fitter failed

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a multilayer linear model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.


// 	This function will generate it own set of height and pierce point
// 	information, based on the number of heights requested.  This routine
//	assumes that the main ionospheric height is around 350 km, with
//	the ionospheric electron density decreasing above and below.  For
//	each observation, the pierce points are calculated for the various
//	heights.  These are used to generate the polynomial terms, which are
//	combined into their respective x^m y^n terms for the fitting process.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 07  James M Anderson  --JIVE  start
//      2007 Jan 04  JMA  --revise a copy here for GPS analysis
//      2007 Mar 01  JMA  --update to be able to fit biases

//_END

{
    //printf("in linfit:  fit_manylayer_linear_model_2\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    else if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add on a final line to force zero mean bias
        Num_Data++;
    }
    const Uint32 Num_Min_Data = Num_Data;
    Num_Data += USE_BIAS_FLAG;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Min_Data <NUM_PARAM) || (NUM_PARAM <= 0) || (NUM_HEIGHTS<=0) ) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u HEIGHTS with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_HEIGHTS, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_PARAM;
    // x[NUM_DATA][NUM_PARAM],
    Real64* sigma = workspace; workspace += Num_Data;
    Real64* model = workspace; workspace += Num_Data;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* path_distance = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_HEIGHTS;
    Uint32* param_off = param_max+NUM_HEIGHTS; 

    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    } // generating heights


    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_HEIGHTS;
        Uint32 overflow = NUM_POLY - min_param*NUM_HEIGHTS;
        for(Uint32 h=0, o=0; h < NUM_HEIGHTS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_HEIGHTS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }
        
                





    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma.  Here I am using the slant TEC, as
        // part of the scaling terms will depend on the height
        y[n] = observation[i].STEC;
        sigma[n] = observation[i].sigma_STEC;

        // For each height, calculate the range to the height. And from that,
        // get the path distances through the ionosphere at each "height"
        {
            if(NUM_HEIGHTS == 1) {
                // there is only 1 height.  Get the range to the height
                path_distance[0] =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[0],
                        radius_Earth
                        );
            }
            else if(NUM_HEIGHTS == 2) {
                // There are 2 points.  The path_distance for the first is
                // going to be the range to the mid-height.  The
                // path_distance to the second is going to be the difference
                // with the range to twice the height and the mid-height.
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]*2.0,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
            }
            else if(NUM_HEIGHTS == 3) {
                // There are 3 points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  The
                // path_distance to the second is going to be the difference
                // mid 0--1 and mid 1--2.  The third is going to be the
                // difference with that and the final height + 1000 km
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[1]+height[2]),
                        radius_Earth
                        );
                Real64 range_2 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]+ 1000E3,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
                path_distance[2] = range_2-range_1;
            }
            else {
                // There are many points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  This goes
                // on with the next height, and so on.
                // The last path distance is going to be the
                // difference with the next-to-last midpoint
                // and the last height + 12000 km
                Real64 range_last = 0.0;
                for(Uint32 h=0; h < NUM_HEIGHTS-1; h++) {
                    Real64 range_now = 
                        observation[i].station->get_pierce_range(
                            observation[i].direction,
                            0.5*(height[h]+height[h+1]),
                            radius_Earth
                            );
                    path_distance[h] = range_now - range_last;
                    range_last = range_now;
                }
                Real64 range_end =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[NUM_HEIGHTS-1]+12000E3,
                        radius_Earth
                        );
                path_distance[NUM_HEIGHTS-1] = range_end - range_last;
            }
            // Now scale the distance down to a reasonable number.  Since
            // We are fitting to an arbitrary constant multiplier, this
            // is just to keep the coefficients all near 1.0.  So
            // convert from meters to units of 1000 km
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) path_distance[h] *= 1E-6;
        } // calculate the path distances
                
        // Ok now, for each height, calculate the pierce point at that
        // height.  Then get the polynomial coefficients.
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(pierce);

            // fill in the x stuff
            calculate_manylayer_2D_linear_polynomials(
                param_max[h],
                equatorial_pos.Lon(),
                equatorial_pos.Lat(),
                path_distance[h] * density_factor[h],
                x+(n*NUM_PARAM)+param_off[h]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[(n*NUM_PARAM)+NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            Real64* dp = x+(n*NUM_POLY);
            dp[NUM_POLY-2] = lat_s;
            dp[NUM_POLY-3] = lon_s;
            dp[NUM_POLY-4] = lat_s*lon_s;
        }
        if((USE_BIAS_FLAG)) {
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                dp[NUM_POLY + observation[i].station_receiver_id] = 1.0;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                dp[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0;
        }

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add a line for zero mean bias
        // Fill in y and sigma
        y[n] = 0.0;
        sigma[n] = 1.0E10;
        Real64* dp = x+(n*NUM_PARAM);
        for(Uint32 j=0; j < NUM_POLY; j++) dp[j] = 0.0;
        for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 1.0;
        n++;
    }
    if(n != Num_Min_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }
    // Now check if there are any station/receiver/objects which have no data
    // and force their biases to zero
    for(Uint32 s = 0; s < USE_BIAS_FLAG; s++) {
        int flag = 0;
        for(Uint32 i=0; i < Num_Min_Data-1; i++) {
            Real64* dp = x+(i*NUM_PARAM);
            if(dp[NUM_POLY+s] != 0.0) {
                flag = 1;
                break;
            }
        }
        if(flag == 0) {
            // No data for this station/receiver/object.
            // force the parameter to 0
            y[n] = 0.0;
            sigma[n] = 1.0E10;
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=0; j < NUM_PARAM; j++) dp[j] = 0.0;
            dp[NUM_POLY+s] = 1.0;
            n++;
        }
    }

        
    // do the fit
    std_dev = 0.0;
    Sint32 return_code = fit_data_2(Sint32(0x0),
//    Sint32 return_code = fit_data_2(Sint32(0xFFFE),
                                    Sint32(NUM_PARAM),
                                    Sint32(n),
                                    y,
                                    x,
                                    sigma,
                                    workspace,
                                    &std_dev,
                                    parameters,
                                    model
                                    );
    if(return_code != 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }


    // Now, stuff in the model values
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        observation[i].model_STEC = model[n];
        observation[i].sigma_model_STEC = std_dev;



            

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    return 0;
}


    






//_TITLE  apply_manylayer_linear_model_2 --apply a surface to the observations
void apply_manylayer_linear_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        const Real64* const parameters,
                               // I  the fit parameters, as parameters[NUM_PARAM]
        const Real64 std_dev,  // I  The standard deviation from the fit.
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)
                               //              +NUM_HEIGHTS*5]
        Observation_3D_Ionosphere* const observation
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This applys the fit parameters found by fit_manylayer_linear_model_2
//	to an arbitrary set of observations.

//	The inputs to theis function (NUM_PARAM, NUM_HEIGHTS, and so on,
//	through std_dev, with a different NUM_OBSERVATIONS possible)
//      must be the same as used in the call to
//	fit_manylayer_linear_model_2

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Jan 04  James M Anderson  --JIVE  start

//_END

{
    //printf("in linfit:   apply_manylayer_linear_model_2\n");
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* x = workspace; workspace += NUM_PARAM;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* path_distance = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_HEIGHTS;
    Uint32* param_off = param_max+NUM_HEIGHTS; 

    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    } // generating heights


    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_HEIGHTS;
        Uint32 overflow = NUM_POLY - min_param*NUM_HEIGHTS;
        for(Uint32 h=0, o=0; h < NUM_HEIGHTS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_HEIGHTS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }
        
                





    // Ok, let's fill in the arrays
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;
        if(observation[i].El <= 0.0) {
            observation[i].sigma_model_STEC = -1.0;
            continue;
        }
        // For each height, calculate the range to the height. And from that,
        // get the path distances through the ionosphere at each "height"
        {
            if(NUM_HEIGHTS == 1) {
                // there is only 1 height.  Get the range to the height
                path_distance[0] =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[0],
                        radius_Earth
                        );
            }
            else if(NUM_HEIGHTS == 2) {
                // There are 2 points.  The path_distance for the first is
                // going to be the range to the mid-height.  The
                // path_distance to the second is going to be the difference
                // with the range to twice the height and the mid-height.
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]*2.0,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
            }
            else if(NUM_HEIGHTS == 3) {
                // There are 3 points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  The
                // path_distance to the second is going to be the difference
                // mid 0--1 and mid 1--2.  The third is going to be the
                // difference with that and the final height + 1000 km
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[1]+height[2]),
                        radius_Earth
                        );
                Real64 range_2 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]+ 1000E3,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
                path_distance[2] = range_2-range_1;
            }
            else {
                // There are many points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  This goes
                // on with the next height, and so on.
                // The last path distance is going to be the
                // difference with the next-to-last midpoint
                // and the last height + 12000 km
                Real64 range_last = 0.0;
                for(Uint32 h=0; h < NUM_HEIGHTS-1; h++) {
                    Real64 range_now = 
                        observation[i].station->get_pierce_range(
                            observation[i].direction,
                            0.5*(height[h]+height[h+1]),
                            radius_Earth
                            );
                    path_distance[h] = range_now - range_last;
                    range_last = range_now;
                }
                Real64 range_end =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[NUM_HEIGHTS-1]+12000E3,
                        radius_Earth
                        );
                path_distance[NUM_HEIGHTS-1] = range_end - range_last;
            }
            // Now scale the distance down to a reasonable number.  Since
            // We are fitting to an arbitrary constant multiplier, this
            // is just to keep the coefficients all near 1.0.  So
            // convert from meters to units of 1000 km
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) path_distance[h] *= 1E-6;
        } // calculate the path distances
                
        // Ok now, for each height, calculate the pierce point at that
        // height.  Then get the polynomial coefficients.
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(pierce);

            // fill in the x stuff
            calculate_manylayer_2D_linear_polynomials(
                param_max[h],
                equatorial_pos.Lon(),
                equatorial_pos.Lat(),
                path_distance[h] * density_factor[h],
                x+param_off[h]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            x[NUM_POLY-2] = lat_s;
            x[NUM_POLY-3] = lon_s;
            x[NUM_POLY-4] = lat_s*lon_s;
        }
        if((USE_BIAS_FLAG)) {
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) x[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                x[NUM_POLY + observation[i].station_receiver_id] = 1.0;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                x[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0;
        }

            
        Real64 fit = 0.0;
        for(Uint32 j=0; j < NUM_PARAM; j++) fit += parameters[j]*x[j];
        observation[i].model_STEC = fit;
        observation[i].sigma_model_STEC = std_dev;
    } // for i,n over NUM_OBSERVATIONS

    return;
}


    






    




Real64 P_l_m(const Uint32 l, const Uint32 m, const Real64 x)
{
    // This function calculates the associated Legendre polynomial
    // P_\ell^m(x=\cos\theta).  Note that I use P_\ell^m(x=\cos\theta)
    // as defined by _Numerical Recipes in C_ (Press et al. 1992)
    // and not Arfkin (1985), but since this is only calculating for
    // positive m, there is no difference.
    //
    // This function calculates P_\ell^m(x=\cos\theta) for \ell \geq 0
    // and 0 \leq m \leq \ell.  Here, x = \cos\theta must naturally satisfy
    // -1 \leq x \leq +1.
    //
    // This function is intended to support spherical harmonics calculations,
    // but has not been optimized for many \ell, many m calculations.
    // Furthermore, it does not actually check that the inputs have the valid
    // range limits.
    Real64 Pmm = 1.0;
    if(m>0) {
        Real64 x2 = sqrt((1.0-x)*(1.0+x));
        Real64 fact = 1.0;
        for(Uint32 i=1; i<=m; i++) {
            Pmm *= -fact*x2;
            fact += 2.0;
        }
    }
    if(l==m) return Pmm;
    Real64 Pmmp1 = x * (2*m+1) * Pmm;
    if(l==(m+1)) return Pmmp1;
    Real64 Pll = Pmmp1;
    for(Uint32 ll = m+2; ll<l; ll++) {
        Pll = (x*(2*ll-1)*Pmmp1-(ll+m-1)*Pmm)/(ll-m);
        Pmm = Pmmp1;
        Pmmp1 = Pll;
    }
    return Pll;
}
    





//_TITLE  calculate_manylayer_2D_spherical_h_polynomials
void calculate_manylayer_2D_spherical_h_polynomials(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters needed,
                               //    must be in 0 < NUM_PARAM < 10000
                               //    (although anything above 36 is almost
                               //    certainly suspect
        const LatLon_Cart *const pierce,
                               // I  The pierce point through the ionosphere,
                               //    as a LatLon object, which includes
                               //    \cos(Lat), \sin(Lon), and \cos(Lon)
                               //    information.  Note that this is relative
                               //    to some reference position.
        const Real64 scale,    // I  a scaling constant to multiply the poly's
        Real64* const poly     // O  the polynomial holder, as poly[NUM_PARAM]
        )
//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will compute the value of a set of spherical harmonic
//  	terms.  It is intended
//	for fitting ionospheric data to latitude and longitude positions.

//	See Arfkin 1985, \S~12.6 for a description of using spherical harmonics
//	to model real values systems.  Note that this breaks up the
//	standard spherical harmonics into terms which have
//	Y_{m \ell}^e = P_\ell^m(x=\cos\theta)\cos(m\phi)     and
//	Y_{m \ell}^0 = P_\ell^m(x=\cos\theta)\sin(m\phi)    .
//	Because we are fitting data to these "harmonic" terms, I leave
//	off the normalization constants (as does Arfkin).  The representation is
//	then
//	\Sum_{\elll=0}^L \Sum_{m=0}^\ell \left\{ C_{\ell m} Y_{m \ell}^e
//                                             + S_{\ell m} Y_{m \ell}^0 \right\}
//	where the C_{\ell m} and S_{\ell m} terms are the fit parameters for
//	the cosine and sine terms.

//	In the calculation of \cos(m\phi) and \sin(m\phi) I use the
//	basic trigonometry that e^{i(m+1)\phi} = e^{i\phi} e^{i m\phi},
//	and use this recurrance relation to calculate subsequent  values of
//	\cos(m\phi) and \sin(m\phi) rather than calling cos and sin directly.
//	From my tests on 2007Mar07, using my desktop Intel pc, this results
//	in residual levels which grow approximately as
//	|resid| \lesssim 6\times 10^{-17} m
//	which should be small enough for any reasonable value of m here.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 06  James M Anderson  --JIVE  start

//_END

{
    //printf("in linfit:   calculate_manylayer_2D_spherical_h_polynomials\n");
    if(NUM_PARAM == 0) {
        // nothing to do
        return;
    }

    // First, set the initial scaling
    for(Uint32 n=0; n < NUM_PARAM; n++) poly[n] = scale;
        
    // I need to know how many terms have been made to date.
    Uint32 num_param_done = 0;

    // x = \cos\theta = \sin(Lat)
    const Real64 x = pierce->sLat();
    const Real64 c_phi = pierce->cLon();
    const Real64 s_phi = pierce->sLon();

    // Ok, here we go
    for(Uint32 l=0; /* no check */; l++) {
        Real64 c_m = 1.0;
        Real64 s_m = 0.0;
        // Handle the m=0 term specially, as the sine part is 0 and
        // only the cosine term is nonzero
        Real64 Plm = P_l_m(l, 0, x);
        poly[num_param_done++] *= Plm;
        if(num_param_done == NUM_PARAM) return;
        // Ok, now do the higher order m terms
        for(Uint32 m=1; m <= l; m++) {
            // Bump up the cosine and sine terms
            Real64 c_m1 = c_phi * c_m - s_phi * s_m;
            Real64 s_m1 = c_phi * s_m + s_phi * c_m;
            c_m = c_m1;
            s_m = s_m1;
            Plm = P_l_m(l, m, x);
            // Now for the cosine term
            poly[num_param_done++] *= Plm * c_m;
            if(num_param_done == NUM_PARAM) return;
            // Now for the sine term
            poly[num_param_done++] *= Plm * s_m;
            if(num_param_done == NUM_PARAM) return;
        } // for m to l
    } // for l until we finish
    // it should be impossible to get here
#ifdef DEBUG
    fprintf(stderr, "Error: how did I get here? %s:%d:%s\n",
            __FILE__, __LINE__, __func__);
    exit(1);
#endif
    return;
}



//_TITLE  fit_manylayer_spherical_h_model_2 --fit spherical harmonics
Sint32 fit_manylayer_spherical_h_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        const bool Force_Zero_Mean_Bias,
                               // I  Should the bias level be forced to 0?
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)
                               //              +NUM_HEIGHTS*5]
        Observation_3D_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_manylayer_spherical_h_model_2
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 SVD fitter failed

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a multilayer spherical harmonics
//      model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.


// 	This function will generate it own set of height and pierce point
// 	information, based on the number of heights requested.  This routine
//	assumes that the main ionospheric height is around 350 km, with
//	the ionospheric electron density decreasing above and below.  For
//	each observation, the pierce points are calculated for the various
//	heights.  These pierce point lattitude and longitudes are used to
//      generate the spherical harmonics terms

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 06  James M Anderson  --JIVE  start

//_END

{
    //printf("in linfit:   fit_manylayer_spherical_h_model_2\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    else if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add on a final line to force zero mean bias
        Num_Data++;
    }
    const Uint32 Num_Min_Data = Num_Data;
    Num_Data += USE_BIAS_FLAG;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Min_Data <NUM_PARAM) || (NUM_PARAM <= 0) || (NUM_HEIGHTS<=0) ) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u HEIGHTS with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_HEIGHTS, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_PARAM;
    // x[NUM_DATA][NUM_PARAM],
    Real64* sigma = workspace; workspace += Num_Data;
    Real64* model = workspace; workspace += Num_Data;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* path_distance = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_HEIGHTS;
    Uint32* param_off = param_max+NUM_HEIGHTS; 

    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    } // generating heights


    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_HEIGHTS;
        Uint32 overflow = NUM_POLY - min_param*NUM_HEIGHTS;
        for(Uint32 h=0, o=0; h < NUM_HEIGHTS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_HEIGHTS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }
        
                





    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma.  Here I am using the slant TEC, as
        // part of the scaling terms will depend on the height
        y[n] = observation[i].STEC;
        sigma[n] = observation[i].sigma_STEC;

        // For each height, calculate the range to the height. And from that,
        // get the path distances through the ionosphere at each "height"
        {
            if(NUM_HEIGHTS == 1) {
                // there is only 1 height.  Get the range to the height
                path_distance[0] =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[0],
                        radius_Earth
                        );
            }
            else if(NUM_HEIGHTS == 2) {
                // There are 2 points.  The path_distance for the first is
                // going to be the range to the mid-height.  The
                // path_distance to the second is going to be the difference
                // with the range to twice the height and the mid-height.
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]*2.0,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
            }
            else if(NUM_HEIGHTS == 3) {
                // There are 3 points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  The
                // path_distance to the second is going to be the difference
                // mid 0--1 and mid 1--2.  The third is going to be the
                // difference with that and the final height + 1000 km
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[1]+height[2]),
                        radius_Earth
                        );
                Real64 range_2 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]+ 1000E3,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
                path_distance[2] = range_2-range_1;
            }
            else {
                // There are many points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  This goes
                // on with the next height, and so on.
                // The last path distance is going to be the
                // difference with the next-to-last midpoint
                // and the last height + 12000 km
                Real64 range_last = 0.0;
                for(Uint32 h=0; h < NUM_HEIGHTS-1; h++) {
                    Real64 range_now = 
                        observation[i].station->get_pierce_range(
                            observation[i].direction,
                            0.5*(height[h]+height[h+1]),
                            radius_Earth
                            );
                    path_distance[h] = range_now - range_last;
                    range_last = range_now;
                }
                Real64 range_end =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[NUM_HEIGHTS-1]+12000E3,
                        radius_Earth
                        );
                path_distance[NUM_HEIGHTS-1] = range_end - range_last;
            }
            // Now scale the distance down to a reasonable number.  Since
            // We are fitting to an arbitrary constant multiplier, this
            // is just to keep the coefficients all near 1.0.  So
            // convert from meters to units of 1000 km
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) path_distance[h] *= 1E-6;
        } // calculate the path distances
                
        // Ok now, for each height, calculate the pierce point at that
        // height.  Then get the polynomial coefficients.
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(pierce);

            // fill in the x stuff
            calculate_manylayer_2D_spherical_h_polynomials(
                param_max[h],
                &equatorial_pos,
                path_distance[h] * density_factor[h],
                x+(n*NUM_PARAM)+param_off[h]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[(n*NUM_PARAM)+NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            Real64* dp = x+(n*NUM_POLY);
            dp[NUM_POLY-2] = lat_s;
            dp[NUM_POLY-3] = lon_s;
            dp[NUM_POLY-4] = lat_s*lon_s;
        }
        if((USE_BIAS_FLAG)) {
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                dp[NUM_POLY + observation[i].station_receiver_id] = 1.0;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                dp[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0;
        }

            

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    if ((USE_BIAS_FLAG > 0) && ((Force_Zero_Mean_Bias))){
        // add a line for zero mean bias
        // Fill in y and sigma
        y[n] = 0.0;
        sigma[n] = 1.0E10;
        Real64* dp = x+(n*NUM_PARAM);
        for(Uint32 j=0; j < NUM_POLY; j++) dp[j] = 0.0;
        for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) dp[j] = 1.0;
        n++;
    }
    if(n != Num_Min_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }
    // Now check if there are any station/receiver/objects which have no data
    // and force their biases to zero
    for(Uint32 s = 0; s < USE_BIAS_FLAG; s++) {
        int flag = 0;
        for(Uint32 i=0; i < Num_Min_Data-1; i++) {
            Real64* dp = x+(i*NUM_PARAM);
            if(dp[NUM_POLY+s] != 0.0) {
                flag = 1;
                break;
            }
        }
        if(flag == 0) {
            // No data for this station/receiver/object.
            // force the parameter to 0
            y[n] = 0.0;
            sigma[n] = 1.0E10;
            Real64* dp = x+(n*NUM_PARAM);
            for(Uint32 j=0; j < NUM_PARAM; j++) dp[j] = 0.0;
            dp[NUM_POLY+s] = 1.0;
            n++;
        }
    }

        
    // do the fit
    std_dev = 0.0;
    Sint32 return_code = fit_data_2(Sint32(0x0),
//    Sint32 return_code = fit_data_2(Sint32(0xFFFE),
                                    Sint32(NUM_PARAM),
                                    Sint32(n),
                                    y,
                                    x,
                                    sigma,
                                    workspace,
                                    &std_dev,
                                    parameters,
                                    model
                                    );
    if(return_code != 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code,
                __FILE__, __LINE__, __func__);
        return -2;
    }


    // Now, stuff in the model values
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        // Fill in y and sigma
        observation[i].model_STEC = model[n];
        observation[i].sigma_model_STEC = std_dev;



            

        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS

    return 0;
}









//_TITLE  fit_manylayer_spherical_h_model_2_grad --fit spherical harmonics
Sint32 fit_manylayer_spherical_h_model_2_grad(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Real64 Ref_MJD,  // I  The refernce time, as an MJD
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_POLY+7)
                               //              +6*NUM_PARAM
                               //              +5*NUM_HEIGHTS]
        Observation_3D_Ionosphere* const observation,
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        Real64& std_dev,       // O  The standard deviation from the fit.
        Real64* const parameters
                               // O  the fit parameters, as parameters[NUM_PARAM]
        )
// Sint32 fit_manylayer_spherical_h_model_2_grad
//                                O  The return code
//                                    0 all ok
//                                   -1 More parameters than datapoints
//                                   -2 SVD fitter failed

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function handles the fitting of a multilayer spherical harmonics
//      model to
//	the observation data.  Only observations with use_flag[i]==true will
//	be used for the fit.  The actual fitting is done by a linear least
//	squares technique using singular value decomposition.


// 	This function will generate it own set of height and pierce point
// 	information, based on the number of heights requested.  This routine
//	assumes that the main ionospheric height is around 350 km, with
//	the ionospheric electron density decreasing above and below.  For
//	each observation, the pierce points are calculated for the various
//	heights.  These pierce point lattitude and longitudes are used to
//      generate the spherical harmonics terms

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 06  James M Anderson  --JIVE  start
//	2007 Mar 26  JMA  --convert to gradient least squares

//_END

{
    //printf("in linfit: fit_manylayer_spherical_h_model_2_grad\n");
    // Ok, how many good observations are there?
    Uint32 Num_Data = 0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++)
        if((use_flag[i])) Num_Data++;

    // Now, compare with the number of parameters desired
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    if(NUM_PARAM < USE_BIAS_FLAG) {
        fprintf(stderr, "Error: bad number of parameters.\nAsked for %u PARAM with %u stations and %u objectsin %s:%d:%s\n",
                NUM_PARAM, NUM_STATIONS, NUM_OBJECTS,
                __FILE__, __LINE__, __func__);
        return -3;
    }
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;
    if( (Num_Data <NUM_PARAM) || (NUM_PARAM <= 0) || (NUM_HEIGHTS<=0) ) {
        fprintf(stderr, "Error: bad number of parameters/datapoints.\nAsked for %u PARAM %u HEIGHTS with %u obs, %u good in %s:%d:%s\n",
                NUM_PARAM, NUM_HEIGHTS, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        return -1;
    }

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* y = workspace; workspace += Num_Data;
    Real64* x = workspace; workspace += Num_Data*NUM_POLY;
                // x[NUM_DATA][NUM_POLY],
    Real64* A = workspace; workspace += Num_Data*NUM_POLY;
    char* Vary_Array = (char*)workspace; workspace += NUM_PARAM;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* path_distance = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_HEIGHTS;
    Uint32* param_off = param_max+NUM_HEIGHTS; 

    Real64* station_val = NULL;
    Real64* object_val = NULL;
    Real64* station_orig = NULL;
    Real64* object_orig = NULL;
    Uint32* station_pos = (Uint32*)workspace;
    Uint32* object_pos = station_pos+Num_Data;  
    if((USE_BIAS_FLAG)) {
        workspace += Num_Data;  // allow space for the positions
        // Note that 1 Real64 can hold 2 Uint32s
        if((NUM_STATIONS)) {
            station_val = workspace; workspace += Num_Data;
            station_orig = workspace; workspace += Num_Data;
            for(Uint32 i=0; i < Num_Data; i++) {
                station_orig[i] = station_val[i] = 0.0;
                station_pos[i] = 0;
            }
        }
        if((NUM_OBJECTS)) {
            object_val = workspace; workspace += Num_Data;
            object_orig = workspace; workspace += Num_Data;
            for(Uint32 i=0; i < Num_Data; i++) {
                object_orig[i] = object_val[i] = 0.0;
                object_pos[i] = 0;
            }
        }
    }

    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    } // generating heights


    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_HEIGHTS;
        Uint32 overflow = NUM_POLY - min_param*NUM_HEIGHTS;
        for(Uint32 h=0, o=0; h < NUM_HEIGHTS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_HEIGHTS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }

    // Ok, let's fill in the arrays
    Uint32 n=0;
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        Real64 weight = observation[i].sigma_STEC;
        if(weight > 0.0) weight = 1.0 / weight;
        else weight = 0.0;
        // Fill in y
        y[n] = observation[i].STEC * weight;
        // For each height, calculate the range to the height. And from that,
        // get the path distances through the ionosphere at each "height"
        {
            if(NUM_HEIGHTS == 1) {
                // there is only 1 height.  Get the range to the height
                path_distance[0] =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[0],
                        radius_Earth
                        );
            }
            else if(NUM_HEIGHTS == 2) {
                // There are 2 points.  The path_distance for the first is
                // going to be the range to the mid-height.  The
                // path_distance to the second is going to be the difference
                // with the range to twice the height and the mid-height.
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]*2.0,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
            }
            else if(NUM_HEIGHTS == 3) {
                // There are 3 points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  The
                // path_distance to the second is going to be the difference
                // mid 0--1 and mid 1--2.  The third is going to be the
                // difference with that and the final height + 1000 km
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[1]+height[2]),
                        radius_Earth
                        );
                Real64 range_2 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]+ 1000E3,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
                path_distance[2] = range_2-range_1;
            }
            else {
                // There are many points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  This goes
                // on with the next height, and so on.
                // The last path distance is going to be the
                // difference with the next-to-last midpoint
                // and the last height + 12000 km
                Real64 range_last = 0.0;
                for(Uint32 h=0; h < NUM_HEIGHTS-1; h++) {
                    Real64 range_now = 
                        observation[i].station->get_pierce_range(
                            observation[i].direction,
                            0.5*(height[h]+height[h+1]),
                            radius_Earth
                            );
                    path_distance[h] = range_now - range_last;
                    range_last = range_now;
                }
                Real64 range_end =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[NUM_HEIGHTS-1]+12000E3,
                        radius_Earth
                        );
                path_distance[NUM_HEIGHTS-1] = range_end - range_last;
            }
            // Now scale the distance down to a reasonable number.  Since
            // We are fitting to an arbitrary constant multiplier, this
            // is just to keep the coefficients all near 1.0.  So
            // convert from meters to units of 1000 km
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) path_distance[h] *= 1E-6;
        } // calculate the path distances

        // Make a first order correction for the sub-Solar direction and
        // Earth rotation
        Station_Reference Ref_Point_Rotate(Ref_Point.Lat(),
                                           Ref_Point.Lon() - (observation[i].MJD
                                                              -Ref_MJD)
                                           * 2.0 * M_PI);
            
        // Ok now, for each height, calculate the pierce point at that
        // height.  Then get the polynomial coefficients.
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point_Rotate.get_equatorial_offset(pierce);

            // fill in the x stuff
            calculate_manylayer_2D_spherical_h_polynomials(
                param_max[h],
                &equatorial_pos,
                path_distance[h] * density_factor[h],
                x+(n*NUM_POLY)+param_off[h]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[(n*NUM_POLY)+NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point_Rotate.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            Real64* dp = x+(n*NUM_POLY);
            dp[NUM_POLY-2] = lat_s;
            dp[NUM_POLY-3] = lon_s;
            dp[NUM_POLY-4] = lat_s*lon_s;
        }
        for(Uint32 p = 0; p < NUM_POLY; p++)
            A[(n*NUM_POLY)+p] = x[(n*NUM_POLY)+p] * weight;
        if((USE_BIAS_FLAG)) {
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX)) {
                station_orig[n] = 1.0;
                station_val[n] = station_orig[n] * weight;
                station_pos[n] = NUM_POLY + observation[i].station_receiver_id;
            }
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX)) {
                object_orig[n] = 1.0;
                object_val[n] = object_orig[n] * weight;
                object_pos[n] =
                    NUM_POLY + NUM_STATIONS + observation[i].object_id;
            }
        }
        // increment the fit data position
        n++;
    } // for i,n over NUM_OBSERVATIONS
    if(n != Num_Data) {
        fprintf(stderr, "Error: programmer error %u %u %u at %s:%d:%s\n",
                NUM_PARAM, NUM_OBSERVATIONS, Num_Data,
                __FILE__, __LINE__, __func__);
        exit(-1);
    }
    // do the fit
    Sint32 return_code = component_fit_gradient_1(
        NUM_PARAM,
        NUM_POLY,
        NUM_STATIONS,
        NUM_OBJECTS,
        0,
        Num_Data,
        A,
        y,
        station_val,
        station_pos,
        object_val,
        object_pos,
        NULL,
        NULL,
        1.0E-3,
        parameters,
        Vary_Array,
        workspace
        );
    if(return_code < 0) {
        fprintf(stderr, "Got return code of %d from fit in %s:%d:%s\n",
                return_code, __FILE__, __LINE__, __func__);
        return -2;
    }


    // Now, stuff in the model values
    Real64 sum_sqr = 0.0;
    for(Uint32 i=0, n=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;

        Real64 value = 0.0;
        for(Uint32 p=0; p < NUM_POLY; p++) {
            value += x[(n*NUM_POLY)+p] * parameters[p];
        }
        if((USE_BIAS_FLAG)) {
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX)) {
                value += station_orig[n] * parameters[station_pos[n]];
            }
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX)) {
                value += object_orig[n] * parameters[object_pos[n]];
            }
        }        

        // Fill in y and sigma
        observation[i].model_STEC = value;
        value -= observation[i].STEC;
        sum_sqr += value*value;
        // increment the fit data position
        n++;
     } // for i,n over NUM_OBSERVATIONS
    if((sum_sqr > 0.0) && (Num_Data > NUM_PARAM))
       std_dev = sqrt(sum_sqr / (Num_Data-NUM_PARAM));
    else std_dev = 0.0;
    //fprintf(stderr, "Got residual level sum_sqr %E std_dev %E\n",sum_sqr,std_dev);
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip, as there are likely uninitialized values
        // hanging around in the data for these points.
        if(!use_flag[i]) continue;

        observation[i].sigma_model_STEC = std_dev;
    }

    return 0;
}
    

    






//_TITLE  apply_manylayer_spherical_h_model_2 --apply spherical harmonics model
void apply_manylayer_spherical_h_model_2(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_PARAM,// I  the number of parameters to use in the
                               //    fit, must be > 0
        const Uint32 NUM_HEIGHTS,
                               // I  The number of heights to use.  Must be > 0
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Uint32 NUM_STATIONS,
                               // I  The total number of stations.  If 0,
                               //    then do not fit for station offset.
        const Uint32 NUM_OBJECTS,
                               // I  The total number of objects.  If 0, then
                               //    do not fit for object offset.
        const bool* const use_flag,
                               // I  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        const Real64* const parameters,
                               // I  the fit parameters, as parameters[NUM_PARAM]
        const Real64 std_dev,  // I  The standard deviation from the fit.
        Real64* workspace,
                               // W  Workspace to perform the fitting stuff.
                               //    must be of size
                               //    workspace[NUM_OBSERVATIONS * (2*NUM_PARAM+4)
                               //              +NUM_PARAM*(NUM_PARAM+2)
                               //              +NUM_HEIGHTS*5]
        Observation_3D_Ionosphere* const observation
                               // B  The observations.
                               //    the model_STEC and sigma_model_STEC
                               //    values will be filled in for those
                               //    observations whose use_flag is true
                               //    otherwise, the values are set to magic
                               //    garbage
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This applys the fit parameters found by fit_manylayer_spherical_h_model_2
//	to an arbitrary set of observations.

//	The inputs to theis function (NUM_PARAM, NUM_HEIGHTS, and so on,
//	through std_dev, with a different NUM_OBSERVATIONS possible)
//      must be the same as used in the call to
//	fit_manylayer_spherical_h_model_2

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Mar 06  James M Anderson  --JIVE  start

//_END

{
    //printf("in linfit: apply_manylayer_spherical_h_model_2\n");
    const Uint32 USE_BIAS_FLAG = NUM_STATIONS + NUM_OBJECTS;
    const Uint32 NUM_POLY = NUM_PARAM - USE_BIAS_FLAG;

    // Now, allocate some space to deal with the arrays needed by the fitter
    Real64* x = workspace; workspace += NUM_PARAM;
    Real64* height = workspace; workspace += NUM_HEIGHTS;
    Real64* path_distance = workspace; workspace += NUM_HEIGHTS;
    Real64* density_factor = workspace; workspace += NUM_HEIGHTS;
    Uint32* param_max = (Uint32*)workspace; workspace += NUM_HEIGHTS;
    Uint32* param_off = param_max+NUM_HEIGHTS; 

    // Now, create a set of heights
    {
        if(NUM_HEIGHTS == 1) {
            // only 1 height, so use 350 km
            height[0] = 350E3; // m
            density_factor[0] = 1.0;
        }
        else if(NUM_HEIGHTS == 2) {
            // 2 heights, so use something surrounding 350 km
            height[0] = 250E3; // m
            height[1] = 500E3; // m
            density_factor[0] = 0.5;
            density_factor[1] = 0.5;
        }
        else {
            // for everything else, create a uniform spacing
            calculate_heights_and_scales(NUM_HEIGHTS,height, density_factor);
            // scale so that density_factor sums to 1.0
            // The function above should have done this, but let's just be
            // sure.
            Real64 sum = 0.0;
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) sum += density_factor[h];
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) density_factor[h] /= sum;
        }
    } // generating heights


    // Figure out how many parameters for each height
    {
        Uint32 min_param = NUM_POLY / NUM_HEIGHTS;
        Uint32 overflow = NUM_POLY - min_param*NUM_HEIGHTS;
        for(Uint32 h=0, o=0; h < NUM_HEIGHTS; h++, o++) {
            param_max[h] = (o<overflow) ? min_param+1:min_param;
        }
        param_off[0] = 0;
        for(Uint32 h=1; h < NUM_HEIGHTS; h++) {
            param_off[h] = param_off[h-1] + param_max[h-1];
        }
    }
        
                





    // Ok, let's fill in the arrays
    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        // if not in use, skip
        if(!use_flag[i]) continue;
        if(observation[i].El <= 0.0) {
            observation[i].sigma_model_STEC = -1.0;
            continue;
        }
        // For each height, calculate the range to the height. And from that,
        // get the path distances through the ionosphere at each "height"
        {
            if(NUM_HEIGHTS == 1) {
                // there is only 1 height.  Get the range to the height
                path_distance[0] =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[0],
                        radius_Earth
                        );
            }
            else if(NUM_HEIGHTS == 2) {
                // There are 2 points.  The path_distance for the first is
                // going to be the range to the mid-height.  The
                // path_distance to the second is going to be the difference
                // with the range to twice the height and the mid-height.
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]*2.0,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
            }
            else if(NUM_HEIGHTS == 3) {
                // There are 3 points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  The
                // path_distance to the second is going to be the difference
                // mid 0--1 and mid 1--2.  The third is going to be the
                // difference with that and the final height + 1000 km
                Real64 range_0 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[0]+height[1]),
                        radius_Earth
                        );
                Real64 range_1 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        0.5*(height[1]+height[2]),
                        radius_Earth
                        );
                Real64 range_2 =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[1]+ 1000E3,
                        radius_Earth
                        );
                path_distance[0] = range_0;
                path_distance[1] = range_1-range_0;
                path_distance[2] = range_2-range_1;
            }
            else {
                // There are many points.  The path_distance for the first is
                // going to be the range to the 0--1 mid-height.  This goes
                // on with the next height, and so on.
                // The last path distance is going to be the
                // difference with the next-to-last midpoint
                // and the last height + 12000 km
                Real64 range_last = 0.0;
                for(Uint32 h=0; h < NUM_HEIGHTS-1; h++) {
                    Real64 range_now = 
                        observation[i].station->get_pierce_range(
                            observation[i].direction,
                            0.5*(height[h]+height[h+1]),
                            radius_Earth
                            );
                    path_distance[h] = range_now - range_last;
                    range_last = range_now;
                }
                Real64 range_end =
                    observation[i].station->get_pierce_range(
                        observation[i].direction,
                        height[NUM_HEIGHTS-1]+12000E3,
                        radius_Earth
                        );
                path_distance[NUM_HEIGHTS-1] = range_end - range_last;
            }
            // Now scale the distance down to a reasonable number.  Since
            // We are fitting to an arbitrary constant multiplier, this
            // is just to keep the coefficients all near 1.0.  So
            // convert from meters to units of 1000 km
            for(Uint32 h=0; h < NUM_HEIGHTS; h++) path_distance[h] *= 1E-6;
        } // calculate the path distances
                
        // Ok now, for each height, calculate the pierce point at that
        // height.  Then get the polynomial coefficients.
        for(Uint32 h=0; h < NUM_HEIGHTS; h++) {
            Space_Vector pierce =
                observation[i].station->get_pierce_location(
                    observation[i].direction,
                    height[h],
                    radius_Earth
                    );
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(pierce);

            // fill in the x stuff
            calculate_manylayer_2D_spherical_h_polynomials(
                param_max[h],
                &equatorial_pos,
                path_distance[h] * density_factor[h],
                x+param_off[h]
                );
        }
        if((NUM_POLY > 3) && (observation[i].sigma_model_STEC >= 0.0)) {
            x[NUM_POLY-1] = observation[i].model_STEC;
        }
        // Hey, try to fit for lat,lon of station
        if(NUM_POLY>13) {
            LatLon_Cart equatorial_pos =
                Ref_Point.get_equatorial_offset(*(observation[i].station));
            Real64 lat_s = equatorial_pos.Lat();
            Real64 lon_s = equatorial_pos.Lon();
            x[NUM_POLY-2] = lat_s;
            x[NUM_POLY-3] = lon_s;
            x[NUM_POLY-4] = lat_s*lon_s;
        }
        if((USE_BIAS_FLAG)) {
            for(Uint32 j=NUM_POLY; j < NUM_PARAM; j++) x[j] = 0.0;
            if(((NUM_STATIONS))
               && (observation[i].station_receiver_id != UINT32_MAX))
                x[NUM_POLY + observation[i].station_receiver_id] = 1.0;
            if(((NUM_OBJECTS))
               && (observation[i].object_id != UINT32_MAX))
                x[NUM_POLY + NUM_STATIONS + observation[i].object_id] =
                    1.0;
        }

            
        Real64 fit = 0.0;
        for(Uint32 j=0; j < NUM_PARAM; j++) fit += parameters[j]*x[j];
        observation[i].model_STEC = fit;
        observation[i].sigma_model_STEC = std_dev;
    } // for i,n over NUM_OBSERVATIONS

    return;
}


    






    
























void fill_IRI_electron_densities(const Station_LatLon& station,
                                     const struct tm& eval_time)
    {
        // Fill in some nominal electron densities looking straight up
        // from some position over the Earth.  This uses IRI to make the
        // ionosphere.
        Space_Unit_Vector direction = station.make_unit_vector();
        Ionosphere_IRI test_iono(3E-7,1E-5);
        for(Uint32 i=0; i < NUM_IRI_DATAPOINTS; i++) {
            IRI_data[2*i+1] = test_iono.Electron_Density_Range(
                station,
                eval_time,
                direction,
                IRI_data[2*i]-station.Elevation());
        }
        return;
    }



} // namespace MIM_PIM


/* Auxiliary routine: printing a matrix */
void print_matrix( char* desc, Uint32 m, Uint32 n, double* a, Uint32 lda ) {
        printf( "\n %s\n", desc );
        for (Uint32 i = 0; i < m; i++ ) {
                for (Uint32 j = 0; j < n; j++ ) printf( " %6.2f", a[i*lda+j] );
                printf( "\n" );
        }
}

