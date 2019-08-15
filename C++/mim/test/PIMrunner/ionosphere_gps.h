// ionosphere_gps.h
// Use MIM on GPS receiver data to model the ionosphere
//_HIST  DATE NAME PLACE INFO
//	2007 Jan 08  James M Anderson  --JIVE  start from ionosphere_pim




#ifndef IONOSPHERE_GPS_H
#define IONOSPHERE_GPS_H

// INCLUDES
#include "JMA_code.h"
#include "ionosphere.h"
#include "GPS.h"




    

// set up a namespace area for stuff.
namespace MIM_PIM {


    

//_CLASS  Ionosphere_GPS
class Ionosphere_GPS : public Ionosphere_Base {
//_DESC  full description of class
//	This class handles making a MIM ionosphere from GPS data

//	GPS (and GLONASS and Galileo) data can contain a large amount of
//	information about the ionosphere from dual-frequency measurements.
//	This ionosphere class uses these measurements to model the
//	ionosphere and the ionospheric delay across the sky.

    
//	At the moment, the GPS stuff is only providing the integrated electron
//	density.  No real faraday rotation predictions are offered, and no
//	electron density at given points is available.  Hopefully this will
//	be implemented in the future.

//	This routine takes in a pointer to a GPS_collection object.  This
//	should be a GPS_collection object allocated by new.  Once this object
//	has been given to the constructor for this Ionosphere_GPS object, the
//	GPS_collection object belongs to the Ionosphere_GPS object.
//	When the Ionosphere_GPS object destructor is called, it will attempt to
//	delete the GPS_collection object.

//	The external world must not delete the GPS_collection object in the
//	meantime, nor should it try to delete it afterward.  However, the
//	external world may alter the existing object (changing the selection
//	criteria, or adding new data, etc.).

//_FILE  files used and logical units used

//_LIMS  design limitations
//	Only implements the integrated electron density routines.

//	The GPS_collection* const obs_data member object belongs to this
//	ionosphere object.  This routine will attempt to delete it
//	in the destructor.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2007 Apr 13  JMA  --update for using VEX_Time instead of struct tm

//_END


// NAMESPACE ISSUES    


public:
    // Constructors
    Ionosphere_GPS(GPS_collection* obs_data_in,
                   Real64 electron_precision_in = 1E-6,
                   Real64 Faraday_precision_in = 1E-4
                   ) throw()
            : Ionosphere_Base(electron_precision_in, Faraday_precision_in),
              obs_data(obs_data_in)
        {return;};
    virtual ~Ionosphere_GPS(void)
        {
            delete obs_data;
            return;
        }

    GPS_collection* const obs_data;
    

    // Here is the important one
    virtual Real64 Electron_Density(const LatLon_Cart& position,
                                    const JMA_VEX_AREA::VEX_Time& time);


    // Next, give me the integrated total electron content,
    // from a station, from a starting altitude of altitude_min
    // to an altitude of altitude_max along the direction direction
    // at time time.  The result will be in units of m^{-2}.
    virtual Real64 Integrated_Electron_Density(
        const Station_LatLon& station,
        const JMA_VEX_AREA::VEX_Time& time,
        const Space_Unit_Vector& direction,
        Real64 altitude_min = 0.0,            // in m
        Real64 altitude_max = 25000E3         // in m
        );
    // Next, give me the rotation measure integral,
    // from a station, from a starting altitude of altitude_min
    // to an altitude of altitude_max along the direction direction
    // at time time.  The result will be in units of T m^{-2}.
    virtual Real64 Integrated_Faraday_Rotation(
        const Station_LatLon& station,
        const JMA_VEX_AREA::VEX_Time& time,
        const Space_Unit_Vector& direction,
        Real64 altitude_min = 0.0,            // in m
        Real64 altitude_max = 25000E3         // in m
        );
    // Now do both at once
    virtual void Integrate_Electron_Faraday(
        const Station_LatLon& station,
        const JMA_VEX_AREA::VEX_Time& time,
        const Space_Unit_Vector& direction,
        Real64* const electron_integral,      // in m^{-2}
        Real64* const electron_error,      // in m^{-2}
        Real64* const Faraday_integral,       // in T m^{-2}
        Real64 altitude_min = 0.0,            // in m
        Real64 altitude_max = 25000E3          // in m
//        Real64* const electron_error        // in m^{-2}
        );



protected:



private:
    // Prevent copying!
    Ionosphere_GPS(const Ionosphere_GPS& g);
    Ionosphere_GPS& operator=(const Ionosphere_GPS& g);
    

    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // IONOSPHERE_GPS_H
