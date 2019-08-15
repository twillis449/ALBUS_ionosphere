// iono_runner.cxx
// run ionospheric models to generate ionosphere observations for stations
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


#include "latlon_cart.h"
#include "station_latlon.h"
#include "station_maker.h"
#include "cal_source.h"
#include "pim_runner.h"
#include "observation.h"

#include "ionosphere.h"
#include "space_vector.h"
#include "space_unit_vector.h"

#include "iono_runner.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS



//_TITLE  run_ionosphere  --run an ionosphere model for stations and sources
    void run_ionosphere(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const struct tm* const Eval_Time,
                               // I  The evaluation time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
        Ionosphere_Base& Iono_Model,
                               // I  The ionosphere model to use.
        Uint32* const NUM_OBSERVATIONS,
                               // O  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere** observation
                               // O  The observations.  This is a **, so
                               //    the caller's pointer will be assigned
                               //    space using new[], and the user should
                               //    delete[] to free.
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function is based on the run_PIMVLBI2 function of pim_runner.cxx,
//	but has been generalized to an arbitrary ionosphere model through
//	the base class Ionosphere_Base.  It takes in a set of sources and
//	stations at some time Eval_Time, and generates predictions for the
//	ionospheric electron column density along the lines of sight from the
//	stations to the sources.  

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE  start

//_END

    {
        // Ok, how many observations
        *NUM_OBSERVATIONS = NUM_SOURCES * NUM_STATIONS;
        // Now get some space
        *observation = new Observation_Ionosphere[*NUM_OBSERVATIONS];
        if(*observation == NULL) {
            fprintf(stderr, "Error: cannot get enough memory for %u observations in %s:%d:%s\n",
                    *NUM_OBSERVATIONS, __FILE__, __LINE__, __func__);
            exit(1);
        }


        // for now, use stdout for dumping information
        FILE* fp_out = stdout;


        // Now loop through the stations and sources
        for(Uint32 station_number = 0, observation_number =0;
            station_number < NUM_STATIONS; station_number++) {


            // now, for each source
            for(Uint32 source_number=0; source_number < NUM_SOURCES;
                source_number++, observation_number++ ) {


                // Ok, if the elevation is greater than 0, run the
                // ionosphere model. Get the Alz,Az information from the
                // RA and Dec info
                LatLon_Cart RADec(source[source_number].Dec(),
                                 source[source_number].RA(),
                                 0.0,1.0);
                LatLon_Cart AltAz =
                    station[station_number].convert_RADec_to_AltAz(RADec);
                (*observation)[observation_number].El   = AltAz.Lat();
                (*observation)[observation_number].Az   = AltAz.Lon();

                // I need a unit direction vector for the direction.
                Space_Unit_Vector direction = RADec.make_unit_vector();

                
                if(AltAz.Lat() > 0.0) {
                    // this looks like a valid sky position to get a
                    // TEC value.  Call the ionosphere stuff
                    Real64 STEC = Iono_Model.Integrated_Electron_Density(
                        station[station_number],
                        *Eval_Time,
                        direction
                        );
                    // convert from m^{-2} to TECU
                    STEC *= 1E-16;
                    (*observation)[observation_number].STEC       = STEC;
                    (*observation)[observation_number].sigma_STEC = 1.0;
                }
                else {
                    // the source is below the horizon.  Set the necessary
                    // values in the observations.
                    (*observation)[observation_number].STEC       = 0.0;
                    (*observation)[observation_number].sigma_STEC = -1.0;
                }

                fprintf(fp_out, "For sta%3d src%3d obs%5d got %14.7f %14.7f %14.7f\n",
                        station_number,
                        source_number,
                        observation_number,
                        (*observation)[observation_number].STEC,
                        (*observation)[observation_number].El*M_RAD2DEG,
                        (*observation)[observation_number].Az*M_RAD2DEG
                        );
            } // for source_number over sources
        } // for station_number over stations



        fclose(fp_out);

        // That's all
        return;
        
    }
    















//_TITLE  run_ionosphere_eF  --run an ionosphere model for stations and sources
    void run_ionosphere_eF(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const struct tm* const Eval_Time,
                               // I  The evaluation time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
        Ionosphere_Base& Iono_Model,
                               // I  The ionosphere model to use.
        Uint32* const NUM_OBSERVATIONS,
                               // O  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        Observation_Ionosphere** observation
                               // O  The observations.  This is a **, so
                               //    the caller's pointer will be assigned
                               //    space using new[], and the user should
                               //    delete[] to free.
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function is based on the run_PIMVLBI2 function of pim_runner.cxx,
//	but has been generalized to an arbitrary ionosphere model through
//	the base class Ionosphere_Base.  It takes in a set of sources and
//	stations at some time Eval_Time, and generates predictions for the
//	ionospheric electron column density along the lines of sight from the
//	stations to the sources.
        
//	This particular _eF version also generates the Faraday rotation value
//	from the model (the F part) along with the electron density (the e part).
//	This is dumped out as well.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Nov 10  James M Anderson  --JIVE  start from run_ionosphere

//_END

    {
        // Ok, how many observations
        *NUM_OBSERVATIONS = NUM_SOURCES * NUM_STATIONS;
        // Now get some space
        *observation = new Observation_Ionosphere[*NUM_OBSERVATIONS];
        if(*observation == NULL) {
            fprintf(stderr, "Error: cannot get enough memory for %u observations in %s:%d:%s\n",
                    *NUM_OBSERVATIONS, __FILE__, __LINE__, __func__);
            exit(1);
        }


        // for now, use stdout for dumping information
        FILE* fp_out = stdout;


        // Now loop through the stations and sources
        for(Uint32 station_number = 0, observation_number =0;
            station_number < NUM_STATIONS; station_number++) {


            // now, for each source
            for(Uint32 source_number=0; source_number < NUM_SOURCES;
                source_number++, observation_number++ ) {


                // Ok, if the elevation is greater than 0, run the
                // ionosphere model. Get the Alz,Az information from the
                // RA and Dec info
                LatLon_Cart RADec(source[source_number].Dec(),
                                 source[source_number].RA(),
                                 0.0,1.0);
                LatLon_Cart AltAz =
                    station[station_number].convert_RADec_to_AltAz(RADec);
                (*observation)[observation_number].El   = AltAz.Lat();
                (*observation)[observation_number].Az   = AltAz.Lon();

                // I need a unit direction vector for the direction.
                Space_Unit_Vector direction = RADec.make_unit_vector();

                
                if(AltAz.Lat() > 0.0) {
                    // this looks like a valid sky position to get a
                    // TEC value.  Call the ionosphere stuff
                    Real64 STEC;
                    Real64 STEC_ERR;
                    Real64 SFR;
                    Iono_Model.Integrate_Electron_Faraday(
                        station[station_number],
                        *Eval_Time,
                        direction,
                        &STEC,
                        &STEC_ERR,
                        &SFR
                        );
                    // convert from m^{-2} to TECU
                    STEC *= 1E-16;
                    // convert from T m^{-2} to RMU
                    SFR *= 1E-12;
                    (*observation)[observation_number].STEC       = STEC;
                    (*observation)[observation_number].sigma_STEC = 1.0;


                    // Now, I also want to have the magnetic field strength
                    // along the line of sight at 350 km altitude.
                    LatLon_Cart pierce_point =
                        station[station_number].get_pierce_location(
                            direction,
                            350E3, // m
                            radius_Earth
                            );
                    Space_Vector B_field = Iono_Model.Magnetic_Field(
                        pierce_point,
                        *Eval_Time
                        );
                    // get the dot product, which gives the B field component
                    // along the line of sight.  This is as the wavel comes
                    // toward the Earth, so it is actually antiparallel, or
                    // need to use a minus sign.
                    Real64 B_parallel = -B_field.dot_product(direction);
                    // print out B_parallel in \micro T
                    B_parallel *= 1E6;
                    
                    fprintf(fp_out, "For sta%3d src%3d obs%5d got %14.7f %14.7f %14.7f %14.7f %14.7f\n",
                            station_number,
                            source_number,
                            observation_number,
                            (*observation)[observation_number].STEC,
                            (*observation)[observation_number].El*M_RAD2DEG,
                            (*observation)[observation_number].Az*M_RAD2DEG,
                            SFR,
                            B_parallel
                            );
                }
                else {
                    // the source is below the horizon.  Set the necessary
                    // values in the observations.
                    (*observation)[observation_number].STEC       = 0.0;
                    (*observation)[observation_number].sigma_STEC = -1.0;
                    fprintf(fp_out, "For sta%3d src%3d obs%5d got %14.7f %14.7f %14.7f %14.7f %14.7f\n",
                            station_number,
                            source_number,
                            observation_number,
                            (*observation)[observation_number].STEC,
                            (*observation)[observation_number].El*M_RAD2DEG,
                            (*observation)[observation_number].Az*M_RAD2DEG,
                            0.0,
                            0.0
                            );
                }

            } // for source_number over sources
        } // for station_number over stations



        fclose(fp_out);

        // That's all
        return;
        
    }
    


    
}  // end namespace


