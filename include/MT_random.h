// MT_random.h
// Mersenne Twister pseudorandom number generator C++ code
// 2003 Jul 10  James M Anderson  --NRAO  start devlopment


// This header is a C++ header designed to implement a random number
// generator for Monte Carlo simulations.  




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





#ifndef MT_RANDOM_H
#define MT_RANDOM_H 1


#include "JMA_code.h"
#include "JMA_math.h"

#include "MC_random.h"


// add to the Monte Carlo random namespace
namespace JMA_MONTE_CARLO_RANDOM {





//_CLASS  Random_Number_Generator_Mersenne_Twister --pseudo random numbers
class Random_Number_Generator_Mersenne_Twister
    : public Random_Number_Generator {
//_DESC  full description of class
//	This class generates random numbers using the Mersenne Twister
//	algorithm (see the code notice above).  The generator should have
//	a period of 2^{19937}-1 and dimensionality for 32 bit numbers of
//	623.

// M. Matsumoto and T. Nishimura,
// "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform  
// Pseudo-Random Number Generator",
// ACM Transactions on Modeling and Computer Simulation,
// Vol. 8, No. 1, January 1998, pp 3--30.


//_FILE  files used and logical units used

//_LIMS  design limitations
//	Although the random numbers should be fine for Monte Carlo simulations
//	they probably are not good for cryptography.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2003 Jul 10  JMA  --NRAO  start

//_END


// NAMESPACE ISSUES    


public:

    // Constructors

    // Give this constructor one 32 bit number as a seed
    Random_Number_Generator_Mersenne_Twister(const Uint32 seed);
    // Give this constructor an array of 32 bit numbers to seed the
    // generator.  Only the first N numbers will be used, although
    // fewer numbers than N is also allowed.
    Random_Number_Generator_Mersenne_Twister(const int seed_length,
                                             const Uint32* const seed_array);


    // Destructor
    // Not necessary


    // Basic functions to get a single random number

    Uint32 genrand_int32(void); // generates unsigned 32-bit integers.
    Sint32 genrand_int31(void); // generates unsigned 31-bit integers.
    Real64 genrand_real1(void); // generates uniform real in [0,1] (32-bit resolution). 
    Real64 genrand_real2(void); //  generates uniform real in [0,1) (32-bit resolution). 
    Real64 genrand_real3(void); //  generates uniform real in (0,1) (32-bit resolution).
    Real64 genrand_real4(void); //  generates uniform real in [-1,1] (32-bit resolution).
    Real64 genrand_res53(void); //  generates uniform real in [0,1) with 53-bit resolution.



    // More complicated random numbers

    // put two 32 bit random integers into a and b
    void genrand_2_int32(Uint32& a, Uint32& b);

    // get two real numbers useful for generating \sin(\phi) and \cos(\phi)
    // see Kalos \& Whitlock 1986, _Monte Carlo Methods Volume I: Basics_
    // Note that this routine returns the *random values*, not the sine and
    // cosine values.  Also, a^2 + b^2 > 0.
    void genrand_2_for_sincos(Real64& a, Real64& b);


    // Generate a point inside a sphere of radius 1 which has an
    // uniform probality distribution within the sphere.
    // Note that this routine returns the x, y, z coordinates within the
    // sphere.
    void genrand_3_for_sphere(Real64& x, Real64& y, Real64& z);
    

    



protected:



private:

    static const int M = 397;
    static const int N = 624;

    Uint32 state[N]; /* the array for the state vector  */
    int left;
    Uint32 *next;



    // internal functions
    void next_state(void);
    void scalar_init(const Uint32 seed);

};

    

// CLASS FUNCTIONS



// HELPER FUNCTIONS



    



    
} // end namespace JMA_MONTE_CARLO_RANDOM







#endif // MT_RANDOM_H
