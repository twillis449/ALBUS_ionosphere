// MC_direction.cxx
// Monte Carlo direction functions
//_HIST  2003 Jul 15  James M Anderson  --NRAO  start




// INCLUDES
#include "JMA_code.h"
#include "JMA_math.h"
#include <stdlib.h>

#include "MC_random.h"
#include "MC_direction.h"

#include <stdio.h>

// set up a namespace area for stuff.
namespace JMA_MONTE_CARLO_GEOMETRY {

    using namespace JMA_MONTE_CARLO_RANDOM;


// GLOBALS


// FUNCTIONS


// generate from random numbers, all directions equally probable
MC_Direction::MC_Direction(JMA_MONTE_CARLO_RANDOM::Random_Number_Generator& RNG)
{
    // Ok, we need to make a randomly oriented direction.  This
    // will be done using \theta and \phi randomly generated,
    // using the fact that \cos(\theta) and \phi are both
    // evenly distributed.

    // Ok, get \cos(\theta) on [-1,1]
    w = RNG.genrand_real4();

    // Ok, now deal with u and v.
    // u = \sin(\theta)\cos(\phi).
    // Now, \sin(\theta) = [1 - \cos^2(\theta)]^{1/2}.
    // And \cos(\phi) = \frac{\xi_2}{(\xi_1^2 + \xi_2^2)^{1/2}}
    // where \xi_1 and \xi_2 are properly chose random numbers
    // (see Kalos & Whitlock 1986, _Monte Carlo Methods Volume I: Basics_)
    // A similar process gives
    // \sin(\phi) = \frac{\xi_1}{(\xi_1^2 + \xi_2^2)^{1/2}}
    // for v = \sin(\theta)\sin(\phi).

    // Combine this to get
    Real64 xi_1, xi_2;
    RNG.genrand_2_for_sincos(xi_1, xi_2);
    // Note that genrand_2_for_sincos guarantees that
    // (\xi_1^2 + \xi_2^2) > 0
    
    Real64 multiplier = sqrt( (1.0 - w*w) / (xi_1*xi_1 + xi_2*xi_2) );

    u = xi_2 * multiplier;
    v = xi_1 * multiplier;

    // This code seems to make a unit vector with length squared within
    // 10^{-15} of unity.  So don't bother to normalize
#ifdef STRICT_GEOMETRY    
    normalize();
#endif // STRICT_GEOMETRY   
    return;
}
    

// generate from a given w or \theta and generate random \phi
MC_Direction::MC_Direction(JMA_MONTE_CARLO_RANDOM::Random_Number_Generator& RNG,
                           DIRECTION_THETA_TYPE dir, Real64 w_in)
{
    // Ok, we need to make a randomly oriented direction.  This
    // will be done using \phi randomly generated,
    // using the fact that  \phi is
    // evenly distributed.

    // The user is giving us something related to w
    if(dir == COS_THETA) {
        w = w_in;
#ifdef MONTE_CARLO_DEBUG
        if(fabs(w) > 1.0) {
            abort();
        }
#endif        
    }
    else w = cos(w);

    // Ok, now deal with u and v.
    // u = \sin(\theta)\cos(\phi).
    // Now, \sin(\theta) = [1 - \cos^2(\theta)]^{1/2}
    // And \cos(\phi) = \frac{\xi_2}{(\xi_1^2 + \xi_2^2)^{1/2}}
    // where \xi_1 and \xi_2 are properly chose random numbers
    // (see Kalos & Whitlock 1986, _Monte Carlo Methods Volume I: Basics_)
    // A similar process gives
    // \sin(\phi) = \frac{\xi_1}{(\xi_1^2 + \xi_2^2)^{1/2}}
    // for v = \sin(\theta)\sin(\phi).

    // Combine this to get
    Real64 xi_1, xi_2;
    RNG.genrand_2_for_sincos(xi_1, xi_2);
    // Note that genrand_2_for_sincos guarantees that
    // (\xi_1^2 + \xi_2^2) > 0
    
    Real64 multiplier = sqrt( (1.0 - w*w) / (xi_1*xi_1 + xi_2*xi_2) );

    u = xi_2 * multiplier;
    v = xi_1 * multiplier;

    
    // This code seems to make a unit vector with length squared within
    // 10^{-15} of unity.  So don't bother to normalize
#ifdef STRICT_GEOMETRY    
    normalize();
#endif // STRICT_GEOMETRY   
    return;
}

    

// generate from a given \phi and \theta
MC_Direction::MC_Direction(Real64 phi, Real64 theta)
{
    // Ok, we need to make a direction from the given angles.

    // First, take care of w
    w = cos(w);

    // Ok, now deal with u and v.
    // u = \sin(\theta)\cos(\phi).
    // Now, \sin(\theta) = [1 - \cos^2(\theta)]^{1/2}
    Real64 multiplier = sqrt(1.0 - w*w);

    u = cos(phi) * multiplier;
    v = sin(phi) * multiplier;

    // This code seems to make a unit vector with length squared within
    // 10^{-15} of unity.  So don't bother to normalize
#ifdef STRICT_GEOMETRY    
    normalize();
#endif // STRICT_GEOMETRY   
    return;
}
    


// Make sure that (u^2 + v^2 + w^2) == 1
void MC_Direction::normalize(void)
{
    Real64 sum = u*u + v*v + w*w;
    // also define x as sum-1
    Real64 x = sum - 1.0;
    if(fabs(x) < 1E-15) {}
    else if(fabs(x) < 1.0E-5) {
        // the error in the current normalization is relatively
        // small.  Let's just use a Taylor expansion to do the correction
        // Expand out
        // (1+x)^{-1/2} = 1 - x/2 + 3x^2/8 - 15x^3/48 + 105x^4/384 - ...
        // We see that keeping terms of x^3 and lower will
        // only result in a final error of 10^{-15} for |x| < 10^{-5}.
        // I think we can live with this.
        Real64 multiplier = 1.0 + x*(-0.5 + x*(0.375 + x*(-0.3125)));
        u *= multiplier;
        v *= multiplier;
        w *= multiplier;
    }
    else {
        // a rather large error.  We'll just have to use the full
        // square root
        Real64 multiplier = sqrt(1.0/sum);
        u *= multiplier;
        v *= multiplier;
        w *= multiplier;
    }
    return;
}

        
    



} // end namespace JMA_MONTE_CARLO_GEOMETRY











// testing
// #include "MT_random.h"

// #include <stdio.h>
// using namespace JMA_MONTE_CARLO_RANDOM;
// using namespace JMA_MONTE_CARLO_GEOMETRY;

// int main(void)
// {
//     Random_Number_Generator_Mersenne_Twister RNG(5);

//     for(int i=0; i < 1000000; i++) {
//         MC_Direction d(RNG);
//         if(d.get_u() > 10.0) {printf("what?\n");}

//         //printf("%20.10E  %20.10E  %20.10E\n", d.get_u(), d.get_v(), d.get_w());
//     }

//     return 0;
// }

