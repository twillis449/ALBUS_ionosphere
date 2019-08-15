// ionosphere.h
// base class for ionosphere models
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start



#ifndef IONOSPHERE_H
#define IONOSPHERE_H

// INCLUDES
#include "JMA_math.h"
#include <time.h>

#include "latlon_cart.h"
#include "space_vector.h"
#include "space_unit_vector.h"
#include "station_latlon.h"
#include "vex_time.h"





// set up a namespace area for stuff.
namespace MIM_PIM {



//_CLASS  Ionosphere_Base
    class Ionosphere_Base {
//_DESC  full description of class

//	This class is designed to be a base class for various different kinds of
//	ionospheric models.  This class will provide a bunch of pure virtual
//	functions which other classes must implement.

//	The basic things which are of interest from this class will be
//	providing electron contents for various positions above the
//	surface of the Earth.  How each derived class implements those is
//	up to the derived class.  Basically, there is *ONE* main function
//	which *MUST* be implemented, which is to provide an electron
//	density at a specific point in space at a specific point in time.
//	Other functions, such as implementing a better electron content
//	integration along a line of sight *CAN* be improved upon, but this
//	is only a nicity.


//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 14  James M Anderson  --JIVE  start
//	2007 Apr 13  JMA  --update for using Vex_Time

//_END


// NAMESPACE ISSUES    


public:
        // Need a constructor to initialize the precision constants
        Ionosphere_Base(Real64 electron_precision_in = 1E-6,
                        Real64 Faraday_precision_in = 1E-4
                        ) throw()
                : electron_precision(electron_precision_in),
                  Faraday_precision(Faraday_precision_in)
            {};
        virtual ~Ionosphere_Base() {};

        // Ok, here is the biggie
        // Provide me with the electron density in m^{-3} at a given
        // point and time
        virtual Real64 Electron_Density(const LatLon_Cart& position,
                                        const JMA_VEX_AREA::VEX_Time& time) = 0;
        Real64 Electron_Density(const LatLon_Cart& position,
                                const Real64 MJD)
            { JMA_VEX_AREA::VEX_Time VT(JMA_VEX_AREA::MJD_OFFSET,MJD);
                return Electron_Density(position, VT); }
        Real64 Electron_Density(const LatLon_Cart& position,
                                const struct tm& time)
            { JMA_VEX_AREA::VEX_Time VT(time);
                return Electron_Density(position, VT); }

        // Next, we are going to need a magnetic field often as well.
        // Get the magnetic field vector, in T, at a given point and time.
        virtual Space_Vector Magnetic_Field(const LatLon_Cart& position,
                                            const JMA_VEX_AREA::VEX_Time& time);
        Space_Vector Magnetic_Field(const LatLon_Cart& position,
                                    const Real64 MJD)
            { JMA_VEX_AREA::VEX_Time VT(JMA_VEX_AREA::MJD_OFFSET,MJD);
                return Magnetic_Field(position, VT); }
        Space_Vector Magnetic_Field(const LatLon_Cart& position,
                                    const struct tm& time)
            { JMA_VEX_AREA::VEX_Time VT(time);
                return Magnetic_Field(position, VT); }

        // Next, give me both
        virtual void Electron_and_Magnetic(const LatLon_Cart& position,
                                           const JMA_VEX_AREA::VEX_Time& time,
                                           Real64* const electron_density,
                                           Space_Vector* const magnetic_field);
        void Electron_and_Magnetic(const LatLon_Cart& position,
                                   const Real64 MJD,
                                   Real64* const electron_density,
                                   Space_Vector* const magnetic_field)
            { JMA_VEX_AREA::VEX_Time VT(JMA_VEX_AREA::MJD_OFFSET,MJD);
                return Electron_and_Magnetic(position, VT,
                                             electron_density,magnetic_field); }
        void Electron_and_Magnetic(const LatLon_Cart& position,
                                   const struct tm& time,
                                   Real64* const electron_density,
                                   Space_Vector* const magnetic_field)
            
            { JMA_VEX_AREA::VEX_Time VT(time);
                return Electron_and_Magnetic(position, VT,
                                             electron_density,magnetic_field); }

        // Now, do the same, but with a station, direction, and RANGE
        Real64 Electron_Density_Range(
            const Station_LatLon& station,
            const JMA_VEX_AREA::VEX_Time& time,
            const Space_Unit_Vector& direction,
            const Real64 range)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Electron_Density(position, time);
            }
        Real64 Electron_Density_Range(
            const Station_LatLon& station,
            const Real64 MJD,
            const Space_Unit_Vector& direction,
            const Real64 range)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Electron_Density(position, MJD);
            }
        Real64 Electron_Density_Range(
            const Station_LatLon& station,
            const struct tm& time,
            const Space_Unit_Vector& direction,
            const Real64 range)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Electron_Density(position, time);
            }
        Space_Vector Magnetic_Field_Range(
            const Station_LatLon& station,
            const JMA_VEX_AREA::VEX_Time& time,
            const Space_Unit_Vector& direction,
            const Real64 range)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Magnetic_Field(position, time);
            }
        Space_Vector Magnetic_Field_Range(
            const Station_LatLon& station,
            const Real64 MJD,
            const Space_Unit_Vector& direction,
            const Real64 range)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Magnetic_Field(position, MJD);
            }
        Space_Vector Magnetic_Field_Range(
            const Station_LatLon& station,
            const struct tm& time,
            const Space_Unit_Vector& direction,
            const Real64 range)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Magnetic_Field(position, time);
            }
        void Electron_and_Magnetic_Range(
            const Station_LatLon& station,
            const JMA_VEX_AREA::VEX_Time& time,
            const Space_Unit_Vector& direction,
            const Real64 range,
            Real64* const electron_density,
            Space_Vector* const magnetic_field)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Electron_and_Magnetic(position, time,
                                             electron_density,
                                             magnetic_field);
            }
        void Electron_and_Magnetic_Range(
            const Station_LatLon& station,
            const Real64 MJD,
            const Space_Unit_Vector& direction,
            const Real64 range,
            Real64* const electron_density,
            Space_Vector* const magnetic_field)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Electron_and_Magnetic(position, MJD,
                                             electron_density,
                                             magnetic_field);
            }
        void Electron_and_Magnetic_Range(
            const Station_LatLon& station,
            const struct tm& time,
            const Space_Unit_Vector& direction,
            const Real64 range,
            Real64* const electron_density,
            Space_Vector* const magnetic_field)
            {
                LatLon_Cart position =
                    station.get_range_location(direction, range);
                return Electron_and_Magnetic(position, time,
                                             electron_density,
                                             magnetic_field);
            }


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
        virtual Real64 Integrated_Electron_Density(
            const Station_LatLon& station,
            const Real64 MJD,
            const Space_Unit_Vector& direction,
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            )
            { JMA_VEX_AREA::VEX_Time VT(JMA_VEX_AREA::MJD_OFFSET,MJD);
                return Integrated_Electron_Density(station, VT,
                                                   direction,
                                                   altitude_min, altitude_max); }
        virtual Real64 Integrated_Electron_Density(
            const Station_LatLon& station,
            const struct tm& time,
            const Space_Unit_Vector& direction,
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            )
            { JMA_VEX_AREA::VEX_Time VT(time);
                return Integrated_Electron_Density(station, VT,
                                                   direction,
                                                   altitude_min, altitude_max); }
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
        virtual Real64 Integrated_Faraday_Rotation(
            const Station_LatLon& station,
            const Real64 MJD,
            const Space_Unit_Vector& direction,
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            )
            { JMA_VEX_AREA::VEX_Time VT(JMA_VEX_AREA::MJD_OFFSET,MJD);
                return Integrated_Faraday_Rotation(station, VT,
                                                   direction,
                                                   altitude_min, altitude_max); }
        virtual Real64 Integrated_Faraday_Rotation(
            const Station_LatLon& station,
            const struct tm& time,
            const Space_Unit_Vector& direction,
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            ) // in m
            { JMA_VEX_AREA::VEX_Time VT(time);
                return Integrated_Faraday_Rotation(station, VT,
                                                   direction,
                                                   altitude_min, altitude_max); }
        // Now do both at once
        virtual void Integrate_Electron_Faraday(
            const Station_LatLon& station,
            const JMA_VEX_AREA::VEX_Time& time,
            const Space_Unit_Vector& direction,
            Real64* const electron_integral,      // in m^{-2}
            Real64* const Faraday_integral,       // in T m^{-2}
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            );
        void Integrate_Electron_Faraday(
            const Station_LatLon& station,
            const Real64 MJD,
            const Space_Unit_Vector& direction,
            Real64* const electron_integral,      // in m^{-2}
            Real64* const Faraday_integral,       // in T m^{-2}
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            )
            { JMA_VEX_AREA::VEX_Time VT(JMA_VEX_AREA::MJD_OFFSET,MJD);
                return Integrate_Electron_Faraday(station, VT,
                                                  direction,
                                                  electron_integral,
                                                  Faraday_integral,
                                                  altitude_min, altitude_max); }
        virtual void Integrate_Electron_Faraday(
            const Station_LatLon& station,
            const struct tm& time,
            const Space_Unit_Vector& direction,
            Real64* const electron_integral,      // in m^{-2}
            Real64* const Faraday_integral,       // in T m^{-2}
            Real64 altitude_min = 0.0,            // in m
            Real64 altitude_max = 25000E3         // in m
            )
            { JMA_VEX_AREA::VEX_Time VT(time);
                return Integrate_Electron_Faraday(station, VT,
                                                  direction,
                                                  electron_integral,
                                                  Faraday_integral,
                                                  altitude_min, altitude_max); }




        Real64 get_Electron_precision() const throw()
            {return electron_precision;}
        Real64 get_Faraday_precision() const throw()
            {return Faraday_precision;}


        // Get the Faraday Rotation integrand main bit.
        // Note that the magnetic field should be in the same coordinate system
        // as propagation_direction.  And, note that for an astronomical
        // observation, propagation_direction = -look_direction.  That is,
        // the electromagnetic wave is propagating *toward* the *observer*,
        // not toward the source.
        Real64 get_Faraday_Measure(const Real64 electron_density,
                                   const Space_Vector& magnetic_field,
                                   const Space_Unit_Vector& propagation_direction
                                   ) throw()
            {
                return electron_density *
                    magnetic_field.dot_product(propagation_direction);
            }
            



protected:
        Real64 electron_precision;     // the precision of the desired
                                       // electron content integrals, expressed
                                       // as a fraction (e.g. 1E-5)
        Real64 Faraday_precision;      // the precision of the desired Faraday
                                       // rotation integral, expressed as a
                                       // fraction (e.g. 1E-5)


        



private:


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // IONOSPHERE_H
