// physical_constants.h
// hold values of the physical constants
// *_SI means SI units, *_cgs means cgs units
//_HIST  DATE NAME PLACE INFO
// 2004 Aug 26  James M Anderson  --NRAO  start
// 2004 Dec 07  JMA  --JIVE  fix mu_0_SI error
// 2007 Jan 02  JMA  --add stuff for MJD conversion help



#ifndef PHYSICAL_CONSTANTS_H
#define PHYSICAL_CONSTANTS_H

// INCLUDES
#include "JMA_code.h"
#include "JMA_math.h"


#ifdef PHYSICAL_CONSTANTS_H_FILE
#  define NUM(x) =x;
#else
#  define NUM(x)
#endif



// set up a namespace area for stuff.
namespace PHYSICAL_CONSTANTS {




// GLOBALS

    extern const Real64 alpha_SI  NUM(7.297352533E-3);
                                  // fine structure constant (unitless)
    extern const Real64 c_SI      NUM(299792485.0);
                                  // speed of light m s^{-1}
    extern const Real64 e_SI      NUM(1.602176462E-19);
                                  // elementary charge (electron) in C
    extern const Real64 GM_Sun_SI NUM(1.32712440E20);
                                  // gravitational constant
                                  // conversion factor m^3 s^{-2} M_\Sun^{-1}
    extern const Real64 h_SI      NUM(6.62606876E-34);  
                                  // Planck's Constant, in J s
    extern const Real64 h_bar_SI  NUM(1.054571596E-34);  
                                  // Planck's Constant / 2\pi, in J s
    extern const Real64 k_SI      NUM(1.3806503E-23);  
                                  // Boltzmann's constant J K^{-1}
    extern const Real64 m_e_SI    NUM(9.10938188E-31);
                                  // mass of electron kg
    extern const Real64 m_e_c2_SI NUM(8.18710414E-14); 
                                  // mass-energy of
                                  // an electron, in J (m_e c^2)
    extern const Real64 m_p_SI    NUM(1.67262158E-27); 
                                  // mass of proton kg
    extern const Real64 m_p_c2_SI NUM(1.50327731E-10); 
                                  // mass-energy of
                                  // a proton, in J (m_p c^2)
    extern const Real64 m_H_SI    NUM(1.67372E-27);    
                                  // mass of Hydrogen atom kg
                                  // assumes Solar deuterium abundance ratio
    extern const Real64 r_e_SI    NUM(2.817940285E-15);  
                                  // classical electron radius
                                  // m
    extern const Real64 epsilon_0_SI  NUM(8.8541878176203898500E-12);
                                  // electric constant
                                  // F m^{-1}, or C^2 N^{-1} m^{-2}
                                  // note that c = (\mu_0 \epsilon_0)^{-1/2}
    extern const Real64 mu_0_SI  NUM(4.0*M_PI*1E-7);
                                  // magnetic constant 4\pi 1E-7
                                  // N A^{-2}
    extern const Real64 sigma_SI  NUM(5.670400E-8);  
                                  // Stephan-Boltzmann constant
                                  // W m^{-2} K^{-4}
    extern const Real64 sigma_T_SI NUM(0.665245854E-28); 
                                  // Thomson cross section
                                  // m^{-2}
    


    // The following depend on some assumptions

    // These two assume mass fractions of 75% H, 25% He, Solar isotope
    // fractions, for full ionization.
    extern const Real64 eta_i_Z2  NUM(1.2323);         
                                  // multiplier for ion number density to
                                  // correct for Z^2 charge dependance
                                  // eta_{i Z^2} = \Sum{n_j Z^2_j} / n_i
    extern const Real64 mu_e_     NUM(1.1417);         
                                  // mass multiplier for electrons
    extern const Real64 mu_i_     NUM(1.2301);         
                                  // mass multiplier for ions


    // The coulomb logarithm is a fudgy number.  Let's take the standard
    // assumption and put it at 20
    extern const Real64 ln_Lambda_ NUM(20.0);       
                                   // Coulomb logarithm, unitless




    // The following are rather astronomy specific
    extern const Real64 AU_SI      NUM(1.49597870691E11);
                                   // Astronomical Unit, in m
    extern const Real64 pc_SI      NUM(3.085677581E16);
                                   // parsec, in m
    extern const Real64 M_Sun_SI   NUM(1.9891E30);
                                   // Solar Mass, in kg.
                                   // NOTE!!!!!
                                   // If you want mass times the gravitational
                                   // constant (G), see GM_Sun_SI above, as
                                   // this is far more accurate
    extern const Real64 R_Sun_SI   NUM(6.955E8);
                                   // Solar radius, in m
    extern const Real64 L_Sun_SI   NUM(3.85E26);
                                   // Solar luminosity, in W

    extern const Real64 SECONDS_PER_DAY  NUM(86400.0);
                                   // 24*60*60, in units of s day^{-1}
    extern const Real64 DAYS_PER_SECOND  NUM(1.0/86400.0);
                                   // units of day s^{-1}

    
    
    
    
    


// FUNCTIONS






    // What is \dot{M}_\mathrm{Edd}  (in kg s^{-1})
    // for a black hole accreting matter
    inline Real64 get_Eddinton_accretion_rate_BH(
        const Real64 Mass,  // I  Mass of the central object, in M_\Sun
        const Real64 mu_e,  // I  mass multiplier for electrons
        const Real64 eta=0.1// I  efficiency factor, typically 0.1
        ) {
        return (4.0*M_PI*GM_Sun_SI*Mass*m_H_SI)/(c_SI*sigma_T_SI*mu_e*eta); }

    // What is R_\mathrm{S} (the Schwarzschild radius) in m
    inline Real64 get_R_S(
        const Real64 Mass // I Mass of the black hole in M_\Sun
        ) {
        return (2.0*GM_Sun_SI*Mass)/(c_SI*c_SI); }
    // What is R_\mathrm{g} (the gravitational radius) in m
    // Note that this is just GM/c^2, while the Schwarzschild radius is 2GM/c^2
    inline Real64 get_R_g(
        const Real64 Mass // I Mass of the black hole in M_\Sun
        ) {
        return (GM_Sun_SI*Mass)/(c_SI*c_SI); }
        



}  // end namespace PHYSICAL_CONSTANTS


#undef NUM
#endif // PHYSICAL_CONSTANTS_H
