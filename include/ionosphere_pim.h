// ionosphere_pim.h
// call the IRI ionosphere stuff to generate an ionosphere
//_HIST  DATE NAME PLACE INFO
//	2005 Dec 13  James M Anderson  --JIVE  start from ionosphere_iri
//	2006 Jul 06  JMA  --change to allow other PIM data paths, and
//                          move the default data path area.




#ifndef IONOSPHERE_PIM_H
#define IONOSPHERE_PIM_H

// INCLUDES
#include "JMA_code.h"
#include "ionosphere.h"

#include "pim_asub_c.h"


// Stuff for just the function code, here in the header file so that
// JMA can find it easily.
#ifdef IONOSPHERE_PIM_CXX
#  define MIMSTR(x) = x
#else
#  define MIMSTR(x)
#endif  // IONOSPHERE_PIM_CXX


    

// set up a namespace area for stuff.
namespace MIM_PIM {


// Globals only this file needs to know about
extern const char PIM_DATA_PATH_DEFAULT[] MIMSTR(INSTALLDIR"/libdata/PIM/");




    

//_CLASS  Ionosphere_PIM
class Ionosphere_PIM : public Ionosphere_Base {
//_DESC  full description of class

//	This class handles making a PIM ionosphere.  It calls the JMA
//	interface subroutine to the PIM software.

//	At the moment, the PIM stuff is only providing the integrated electron
//	density and Faraday rotation stuff.  Hopefully that will be fixed in the
//	future.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	Only implements the integrated routines.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END


// NAMESPACE ISSUES    


public:
    // Constructors
    Ionosphere_PIM(Real64 electron_precision_in = 1E-6,
                   Real64 Faraday_precision_in = 1E-4,
                   const char* PIM_Data_Path = NULL
                   ) throw()
            : Ionosphere_Base(electron_precision_in, Faraday_precision_in),
              PIM_DATA_PATH(PIM_Data_Path)
        {if(PIM_DATA_PATH == NULL) PIM_DATA_PATH = PIM_DATA_PATH_DEFAULT;};

    // Here is the important one
    virtual Real64 Electron_Density(const LatLon_Cart& position,
                                    const JMA_VEX_AREA::VEX_Time& time);


//     // Next, give me the integrated total electron content,
//     // from a station, from a starting altitude of altitude_min
//     // to an altitude of altitude_max along the direction direction
//     // at time time.  The result will be in units of m^{-2}.
//     virtual Real64 Integrated_Electron_Density(
//         const Station_LatLon& station,
//         const struct tm& time,
//         const Space_Unit_Vector& direction,
//         Real64 altitude_min = 0.0,            // in m
//         Real64 altitude_max = 25000E3         // in m
//         );
//     // Next, give me the rotation measure integral,
//     // from a station, from a starting altitude of altitude_min
//     // to an altitude of altitude_max along the direction direction
//     // at time time.  The result will be in units of T m^{-2}.
//     virtual Real64 Integrated_Faraday_Rotation(
//         const Station_LatLon& station,
//         const struct tm& time,
//         const Space_Unit_Vector& direction,
//         Real64 altitude_min = 0.0,            // in m
//         Real64 altitude_max = 25000E3         // in m
//         );
//     // Now do both at once
//     virtual void Integrate_Electron_Faraday(
//         const Station_LatLon& station,
//         const struct tm& time,
//         const Space_Unit_Vector& direction,
//         Real64* const electron_integral,      // in m^{-2}
//         Real64* const Faraday_integral,       // in T m^{-2}
//         Real64 altitude_min = 0.0,            // in m
//         Real64 altitude_max = 25000E3         // in m
//         );



protected:



private:

    // Need to know where the PIM data resides    
    const char* PIM_DATA_PATH;

    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // IONOSPHERE_PIM_H
