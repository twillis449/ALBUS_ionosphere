// MC_direction.h
// Monte Carlo direction class
//_HIST  2003 Jul 15  James M Anderson  --NRAO  start



#ifndef MC_DIRECTION_H
#define MC_DIRECTION_H

// INCLUDES

#include "JMA_code.h"
#include "JMA_math.h"

#include "MC_random.h"




// set up a namespace area for stuff.
namespace JMA_MONTE_CARLO_GEOMETRY {

    // here are some enumerations we may need
    enum DIRECTION_THETA_TYPE { COS_THETA, THETA };
    


//_CLASS  MC_Direction --Monte Carlo direction cosines holder
class MC_Direction {
//_DESC  full description of class

//	This class holds a 3-D direction unit vector for Monte Carlo
//	random walk simulations.  For numerical speed and simplicity, only
//	the Cartesian coordinate system is supported here.

//	The unit vector can be thought of as direction cosines.  So,
//	\hat{n} = ( u, v, w )^T ,
//	where  u = \hat{n} \cdot \hat{x}, v = \hat{n} \cdot \hat{y}, and
//	w = \hat{n} \cdot \hat{z}.  Or, using spherical coordinates
//	\phi and \theta for the direction,
//	u = \cos(\phi)\sin(\theta)
//	v = \sin(\phi)\sin(\theta)
//	w = \cos(\theta)

//	This class includes several functions for dealing with generating
//	directions from random numbers, changing directions using random
//	numbers, changing directions for relativistic transforms, etc.
    

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2003 Jul 15  James M Anderson  --NRAO  start

//_END


// NAMESPACE ISSUES    


public:
    // Constructors

    // generate from random numbers, all directions equally probable
    MC_Direction(JMA_MONTE_CARLO_RANDOM::Random_Number_Generator& RNG);

    // generate from a given w or \theta and generate random \phi
    // w_in can either be \cos(\theta) or \theta in radians
    MC_Direction(JMA_MONTE_CARLO_RANDOM::Random_Number_Generator& RNG,
                 DIRECTION_THETA_TYPE dir, Real64 w_in);

    // generate from a given \phi and \theta
    // Both phi and theta are in radians
    MC_Direction(Real64 phi, Real64 theta);

    // generate from cartesian vector
    MC_Direction(Real64 u_in, Real64 v_in, Real64 w_in);
    



    // Access specific components
    Real64 get_u(void) const throw() {return u;};
    Real64 get_v(void) const throw() {return v;};
    Real64 get_w(void) const throw() {return w;};



protected:



private:

    // Direction Cosines
    Real64 u, v, w;


    // Make sure that (u^2 + v^2 + w^2) == 1
    void normalize(void);


    
};


// CLASS FUNCTIONS
    inline MC_Direction::MC_Direction(Real64 u_in, Real64 v_in, Real64 w_in)
            : u(u_in), v(v_in), w(w_in)
    {
#ifdef MONTE_CARLO_DEBUG
        normalize();
#endif        
    }

    
// HELPER FUNCTIONS



}  // end namespace JMA_MONTE_CARLO_GEOMETRY

#endif // MC_DIRECTION_H
