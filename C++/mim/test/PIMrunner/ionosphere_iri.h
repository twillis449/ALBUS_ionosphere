// ionosphere_iri.h
// call the IRI ionosphere stuff to generate an ionosphere
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 08  James M Anderson  --JIVE  start from ionosphere_fake
//	2006 Jul 06  JMA  --change to new data path mechanism and default area.



#ifndef IONOSPHERE_IRI_H
#define IONOSPHERE_IRI_H

// INCLUDES
#include "JMA_code.h"
#include "ionosphere.h"




// Stuff for just the function code, here in the header file so that
// JMA can find it easily.
#ifdef IONOSPHERE_IRI_CXX
#  define MIMSTR(x) = x
#else
#  define MIMSTR(x)
#endif  // IONOSPHERE_IRI_CXX




// set up a namespace area for stuff.
namespace MIM_PIM {



extern const char IRI_DATA_PATH_DEFAULT[] MIMSTR(INSTALLDIR"/libdata/IRI/");    


//_CLASS  name one line description
class Ionosphere_IRI : public Ionosphere_Base {
//_DESC  full description of class

//	This class handles making a IRI ionosphere.  It calls the JMA
//	interface subroutine to the IRI software.

//	At the moment, the IRI stuff is only providing electron ensity as
//	a function of latitude, longitude, height, and time.  (No magnetic
//	field stuff is incorporated.  This is therefore handled by the
//	Ionosphere_Base class stuff.)

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:
    // Constructors
    Ionosphere_IRI(Real64 electron_precision_in = 1E-6,
                   Real64 Faraday_precision_in = 1E-4,
                   const char* IRI_Data_Path = NULL
                   ) throw()
            : Ionosphere_Base(electron_precision_in, Faraday_precision_in),
              IRI_DATA_PATH(IRI_Data_Path)
        {if(IRI_DATA_PATH == NULL) IRI_DATA_PATH = IRI_DATA_PATH_DEFAULT;};

    // Here is the important one
    virtual Real64 Electron_Density(const LatLon_Cart& position,
                                    const JMA_VEX_AREA::VEX_Time& time);





protected:



private:
    const char* IRI_DATA_PATH;


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // IONOSPHERE_IRI_H
