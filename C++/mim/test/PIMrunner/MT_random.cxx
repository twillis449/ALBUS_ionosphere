// MT_Random.cxx
// Mersenne Twister pseudorandom number generator C++ code
// 2003 Jul 10  James M Anderson  --NRAO  convert from original C






/* 
   A C-program for MT19937, with initialization improved 2002/2/10.
   Coded by Takuji Nishimura and Makoto Matsumoto.
   This is a faster version by taking Shawn Cokus's optimization,
   Matthe Bellew's simplification, Isaku Wada's real version.

   Before using, initialize the state by using init_genrand(seed) 
   or init_by_array(init_key, key_length).

   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
   All rights reserved.                          

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

     1. Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

     2. Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.

     3. The names of its contributors may not be used to endorse or promote 
        products derived from this software without specific prior written 
        permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


   Any feedback is very welcome.
   http://www.math.keio.ac.jp/matumoto/emt.html
   email: matumoto@math.keio.ac.jp
*/


#include "MT_random.h"


#include <stdio.h>
#include <stdlib.h>



// Set up our namespace stuff
namespace JMA_MONTE_CARLO_RANDOM {


const int Random_Number_Generator_Mersenne_Twister::M;
const int Random_Number_Generator_Mersenne_Twister::N;
    


/* Period parameters */  
//#define N 624   // moved to class
//#define M 397   // moved to class
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UMASK 0x80000000UL /* most significant w-r bits */
#define LMASK 0x7fffffffUL /* least significant r bits */
#define MIXBITS(u,v) ( ((u) & UMASK) | ((v) & LMASK) )
#define TWIST(u,v) ((MIXBITS(u,v) >> 1) ^ ((v)&1UL ? MATRIX_A : 0UL))


/* initializes state[N] with a seed */
void Random_Number_Generator_Mersenne_Twister::scalar_init(const Uint32 seed)
{
    int j;
    state[0]= seed & 0xffffffffUL;
    for (j=1; j<N; j++) {
        state[j] = (1812433253UL * (state[j-1] ^ (state[j-1] >> 30)) + j); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array state[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        state[j] &= 0xffffffffUL;  /* for >32 bit machines */
    }
    left = 1; 
}



/* initializes state[N] with a seed */
Random_Number_Generator_Mersenne_Twister::
Random_Number_Generator_Mersenne_Twister(const Uint32 seed)
{
    scalar_init(seed);
    return;
}

/* initialize by an array with array-length */
/* init_key is the array for initializing keys */
/* key_length is its length */
Random_Number_Generator_Mersenne_Twister::
Random_Number_Generator_Mersenne_Twister(const int seed_length,
                                         const Uint32* const seed_array)
{
    int i, j, k;
    scalar_init(19650218UL);
    i=1; j=0;
    k = (N>seed_length ? N : seed_length);
    for (; k; k--) {
        state[i] = (state[i] ^ ((state[i-1] ^ (state[i-1] >> 30)) * 1664525UL))
          + seed_array[j] + j; /* non linear */
        state[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++; j++;
        if (i>=N) { state[0] = state[N-1]; i=1; }
        if (j>=seed_length) j=0;
    }
    for (k=N-1; k; k--) {
        state[i] = (state[i] ^ ((state[i-1] ^ (state[i-1] >> 30)) * 1566083941UL))
          - i; /* non linear */
        state[i] &= 0xffffffffUL; /* for WORDSIZE > 32 machines */
        i++;
        if (i>=N) { state[0] = state[N-1]; i=1; }
    }

    state[0] = 0x80000000UL; /* MSB is 1; assuring non-zero initial array */ 
    left = 1; 
}

void Random_Number_Generator_Mersenne_Twister::next_state(void)
{
    Uint32 *p=state;
    int j;

    left = N;
    next = state;
    
    for (j=N-M+1; --j; p++) 
        *p = p[M] ^ TWIST(p[0], p[1]);

    for (j=M; --j; p++) 
        *p = p[M-N] ^ TWIST(p[0], p[1]);

    *p = p[M-N] ^ TWIST(p[0], state[0]);
}

/* generates a random number on [0,0xffffffff]-interval */
Uint32 Random_Number_Generator_Mersenne_Twister::genrand_int32(void)
{
    Uint32 y;

    if (--left == 0) next_state();
    y = *next++;

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}

/* generates a random number on [0,0x7fffffff]-interval */
Sint32 Random_Number_Generator_Mersenne_Twister::genrand_int31(void)
{
    Uint32 y;

    if (--left == 0) next_state();
    y = *next++;

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return (Sint32)(y>>1);
}

/* generates a random number on [0,1]-real-interval */
Real64 Random_Number_Generator_Mersenne_Twister::genrand_real1(void)
{
    /* divided by 2^32-1 */ 
    static const Real64 multiplier = (1.0/4294967295.0);
    Uint32 y;

    if (--left == 0) next_state();
    y = *next++;

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return (Real64)y * multiplier;
}

/* generates a random number on [0,1)-real-interval */
Real64 Random_Number_Generator_Mersenne_Twister::genrand_real2(void)
{
    /* divided by 2^32 */
    static const Real64 multiplier = (1.0/4294967296.0);
    Uint32 y;

    if (--left == 0) next_state();
    y = *next++;

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return (Real64)y * multiplier;
}

/* generates a random number on (0,1)-real-interval */
Real64 Random_Number_Generator_Mersenne_Twister::genrand_real3(void)
{
    /* divided by 2^32 */
    static const Real64 multiplier = (1.0/4294967296.0);
    Uint32 y;

    if (--left == 0) next_state();
    y = *next++;

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return ((Real64)y + 0.5) * multiplier;
}

/* generates a random number on [0,1) with 53-bit resolution*/
Real64 Random_Number_Generator_Mersenne_Twister::genrand_res53(void) 
{
    // I (JMA) doesn't plan to use 53 bit random variables much, so
    // I'm not going to work on speeding this up.
    Uint32 a=genrand_int32()>>5, b=genrand_int32()>>6; 
    return(a*67108864.0+b)*(1.0/9007199254740992.0); 
} 
/* These real versions are due to Isaku Wada, 2002/01/09 added */





// JMA Stuff


//  generates uniform real in [-1,1] (32-bit resolution).
Real64 Random_Number_Generator_Mersenne_Twister::genrand_real4(void)
{
    /* divided by 2^32-1 */
    // but then multiply by 2.0 and subtract 1.0, so move the
    // 2.0 into the multiplier
    static const Real64 multiplier = (2.0/4294967295.0);
    Uint32 y;

    if (--left == 0) next_state();
    y = *next++;

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return (Real64)y * multiplier - 1.0;
}

// Get two 32-bit unsigned integers at once
void Random_Number_Generator_Mersenne_Twister::
genrand_2_int32(Uint32& a, Uint32& b)
{
    // This is similar to one random value, but interleae the
    // tempering.

    // Use local variables for speed
    Uint32 y;
    Uint32 z;

    if (--left == 0) next_state();
    y = *next++;
    if (--left == 0) next_state();
    z = *next++;

    /* Tempering */
    y ^= (y >> 11);
    z ^= (z >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    z ^= (z << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    z ^= (z << 15) & 0xefc60000UL;
    y ^= (y >> 18);
    z ^= (z >> 18);

    // Copy to the output area
    a = y;
    b = z;
    return;
}



// Get two random numbers suitable for sin/cos calculation
// see Kalos \& Whitlock 1986, _Monte Carlo Methods Volume I: Basics_
void Random_Number_Generator_Mersenne_Twister::
genrand_2_for_sincos(Real64& a, Real64& b)
{
    /* divided by 2^32-1 */ 
    static const Real64 multiplier = (1.0/4294967295.0);
    // This is similar to one random value, but interleae the
    // tempering.

    // Hold a couple of local Real64s for speed
    Real64 r1, r2;

    // This is an infinite loop.  But for truely
    // random variables, each individual loop has a probability of
    // \frac{\pi (0.5)^2}{1^2} \approx 0.785 of success.
    for(;;) {
        // Ok, get two random integers

        // Use local variables for speed
        Uint32 y;
        Uint32 z;

        if (--left == 0) next_state();
        y = *next++;
        if (--left == 0) next_state();
        z = *next++;

        /* Tempering */
        y ^= (y >> 11);
        z ^= (z >> 11);
        y ^= (y << 7) & 0x9d2c5680UL;
        z ^= (z << 7) & 0x9d2c5680UL;
        y ^= (y << 15) & 0xefc60000UL;
        z ^= (z << 15) & 0xefc60000UL;
        y ^= (y >> 18);
        z ^= (z >> 18);

        // Convert them to reals in the range [-0.5,+0.5]
        r1 = y * multiplier - 0.5;
        r2 = z * multiplier - 0.5;

        // Now, test that we are within a circle of radius 0.25
        if( (r1*r1 + r2*r2) <= 0.25 ) {
            // Both numbers being zero is bad.  But mathematically
            // this cannot happen from y,z integer.  
            
            // Great.  Copy to the output area and return
            a = r1;
            b = r2;
            return;
        }
    }
    // We should never get here.
}
    



// Generate a point inside a sphere of radius 1 which has an
// uniform probality distribution within the sphere.
// Note that this routine returns the x, y, z coordinates within the
// sphere.
void Random_Number_Generator_Mersenne_Twister::
genrand_3_for_sphere(Real64& x, Real64& y, Real64& z)
{
    /* divided by 2^32-1 */
    // But then multiply by 2, as we will need this later on anyway.
    static const Real64 multiplier = (2.0/4294967295.0);
    // This is similar to one random value, but interleae the
    // tempering.

    // Hold a few of local Real64s for speed
    Real64 r1, r2, r3;

    // This is an infinite loop.  But for truely
    // random variables, each individual loop has a probability of
    // \frac{4/3 \pi (1)^3}{2^3} \approx 0.5236 of success.
    // This is still much faster than calculating r, \phi, \theta
    // and converting to x, y, z
    for(;;) {
        // Ok, get two random integers

        // Use local variables for speed
        Uint32 a;
        Uint32 b;
        Uint32 c;

        if (--left == 0) next_state();
        a = *next++;
        if (--left == 0) next_state();
        b = *next++;
        if (--left == 0) next_state();
        c = *next++;

        /* Tempering */
        a ^= (a >> 11);
        b ^= (b >> 11);
        c ^= (c >> 11);
        a ^= (a << 7) & 0x9d2c5680UL;
        b ^= (b << 7) & 0x9d2c5680UL;
        c ^= (c << 7) & 0x9d2c5680UL;
        a ^= (a << 15) & 0xefc60000UL;
        b ^= (b << 15) & 0xefc60000UL;
        c ^= (c << 15) & 0xefc60000UL;
        a ^= (a >> 18);
        b ^= (b >> 18);
        c ^= (c >> 18);

        // Convert them to reals in the range [-1,+1]
        r1 = a * multiplier - 1.0;
        r2 = b * multiplier - 1.0;
        r3 = c * multiplier - 1.0;

        // Now, test that we are within a sphere of radius 1
        if( (r1*r1 + r2*r2 + r3*r3) <= 1.0 ) {
            // Great.  Copy to the output area and return
            x = r1;
            y = r2;
            z = r2;
            return;
        }
    }
    // We should never get here.
}

    









} // namespace JMA_MONTE_CARLO_RANDOM












// The stuff below is for testing


// using namespace JMA_MONTE_CARLO_RANDOM

// int main(void)
// {
//     int i;
//     Uint32 init[4]={0x123, 0x234, 0x345, 0x456}, length=4;
//     Real64 sum=0.0;

//     Random_Number_Generator_Mersenne_Twister rng(length, init);


//     /* This is an example of initializing by an array.       */
//     /* You may use init_genrand(seed) with any 32bit integer */
//     /* as a seed for a simpler initialization                */
// //     printf("1000 outputs of genrand_int32()\n");
// //     for (i=0; i<1000; i++) {
// //       printf("%10u ", rng.genrand_int32());
// //       if (i%5==4) printf("\n");
// //     }
// //     printf("\n1000 outputs of genrand_real2()\n");
// //     for (i=0; i<1000; i++) {
// //       printf("%10.8f ", rng.genrand_real2());
// //       if (i%5==4) printf("\n");
// //     }
//     for (i=0; i<7000000; i++) {
//         rng.genrand_2_int32(init[0], init[1]);
//         sum += init[0];
//         sum += init[1];
//         sum += rng.genrand_int32();
//         sum += rng.genrand_int32();
//         sum += rng.genrand_real1();
//         sum += rng.genrand_real1();
//     }
//     for (i=0; i<7000000; i++) {
//         sum += rng.genrand_int32();
//         sum += rng.genrand_int32();
//         sum += rng.genrand_real1();
//         sum += rng.genrand_real1();
//         rng.genrand_2_int32(init[0], init[1]);
//         sum += init[0];
//         sum += init[1];
//     }
//     for (i=0; i<7000000; i++) {
//         sum += rng.genrand_real1();
//         sum += rng.genrand_real1();
//         rng.genrand_2_int32(init[0], init[1]);
//         sum += init[0];
//         sum += init[1];
//         sum += rng.genrand_int32();
//         sum += rng.genrand_int32();
//     }
//     printf("final %E\n", sum);

//     return 0;
// }

