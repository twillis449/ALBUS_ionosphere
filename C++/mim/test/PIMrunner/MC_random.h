// MC_random.h
// Monte Carlo Random Number Generator virtual class
// 2003 Jul 15  James M Anderson  --NRAO  start devlopment


// This header is a C++ header designed to abstract a random number
// generator for Monte Carlo simulations.  Actual random number generators
// should be based on this one.









#ifndef MC_RANDOM_H
#define MC_RANDOM_H 1


#include "JMA_code.h"
#include "JMA_math.h"


// add to the Monte Carlo random namespace
namespace JMA_MONTE_CARLO_RANDOM {





//_CLASS  Random_Number_Generator --pseudo random numbers virtual class
class Random_Number_Generator {
//_DESC  full description of class
//	This class is an abstraction of pseudo random number generators.
//	It defines functions which are the same across all random number
//	implementations.  Actual random number generator classes should be
//	based on this one.


//_FILE  files used and logical units used

//_LIMS  design limitations


//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2003 Jul 15  JMA  --NRAO  start

//_END


// NAMESPACE ISSUES    


public:

    // Constructors --not needed since this is a virtual class

    // Destructor
    // Not necessary


    // Basic functions to get a single random number

    virtual Uint32 genrand_int32(void) = 0; // generates unsigned 32-bit integers.
    virtual Sint32 genrand_int31(void) = 0; // generates unsigned 31-bit integers.
    virtual Real64 genrand_real1(void) = 0; // generates uniform real in [0,1] (32-bit resolution). 
    virtual Real64 genrand_real2(void) = 0; //  generates uniform real in [0,1) (32-bit resolution). 
    virtual Real64 genrand_real3(void) = 0; //  generates uniform real in (0,1) (32-bit resolution).
    virtual Real64 genrand_real4(void) = 0; //  generates uniform real in [-1,1] (32-bit resolution).
    virtual Real64 genrand_res53(void) = 0; //  generates uniform real in [0,1) with 53-bit resolution.



    // More complicated random numbers

    // put two 32 bit random integers into a and b
    virtual void genrand_2_int32(Uint32& a, Uint32& b) = 0;

    // get two real numbers useful for generating \sin(\phi) and \cos(\phi)
    // see Kalos \& Whitlock 1986, _Monte Carlo Methods Volume I: Basics_
    // Note that this routine returns the *random values*, not the sine and
    // cosine values.  Also, a^2 + b^2 > 0.
    virtual void genrand_2_for_sincos(Real64& a, Real64& b) = 0;


    // Generate a point inside a sphere of radius 1 which has an
    // uniform probality distribution within the sphere.
    // Note that this routine returns the x, y, z coordinates within the
    // sphere.
    virtual void genrand_3_for_sphere(Real64& x, Real64& y, Real64& z) = 0;
    

    



protected:



private:

};

    

// CLASS FUNCTIONS



// HELPER FUNCTIONS



    



    
} // end namespace JMA_MONTE_CARLO_RANDOM







#endif // MC_RANDOM_H
