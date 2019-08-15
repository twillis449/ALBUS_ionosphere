// lofar8.cxx
// test LOFAR ionosphere stuff with IRI for Faraday prediction for
// Westerbork
//_HIST  DATE NAME PLACE INFO
//	2005 Nov 10  James M Anderson  --JIVE start






// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "cal_source.h"
#include "station_latlon.h"
#include "station_reference.h"
#include "station_maker.h"
#include "3C.h"
#include "random_source.h"
#include "pim_runner.h"
#include "observation.h"
#include "iono_runner.h"
#include "ionosphere.h"
#include "ionosphere_iri.h"
#include "ionosphere_pim.h"
#include "ionosphere_fake.h"
#include "sofa.h"
#include "vex_time.h"



// set up a namespace area for stuff.
using namespace MIM_PIM;
using namespace JMA_VEX_AREA;


    



// GLOBALS


// FUNCTIONS


// set up a blank namespeace
namespace {



    
//_TITLE  read_in_Westerbork_sources
    Space_Vector* read_in_Westerbork_sources(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char filename[], // I  the filename for the Westerbork Sources
        const Uint32 NUM_SOURCES//I  the total number of soruces to read in
        )
//  read_in_random_sources        O  this function returns an array of Cal_Source
//                                   objects, which are the random sources.

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function reads in the random sources from the random_dirs.txt
//	file.  It will read in the RA and Dec and Flux Density number, and
//	dump out the results.

//	The memory for the array of Cal_Source objects is allocated with new[]
//	and should be dumped with delete[]

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Dec 13  James M Anderson  --JIVE start

//_END

    {
        // Ok, can we read the file?
        FILE* fp = fopen(filename, "r");
        if(fp == NULL) {
            fprintf(stderr, "Error: cannot open random data file '%s' at %s:%d:%s\n",
                    filename, __FILE__, __LINE__, __func__);
            exit(2);
        }

        // Ok, I know the format of the file, and that there are supposed to
        // be NUM_SOURCES sources in there

        Space_Vector* data = new Space_Vector[NUM_SOURCES];
        if(data == NULL) {
            fprintf(stderr, "Error: cannot allocate space for %u Space_Vector objects at %s:%d:%s\n",
                    NUM_SOURCES, __FILE__, __LINE__, __func__);
            exit(1);
        }

        // Now step through the file
        for(Uint32 i=0; i < NUM_SOURCES; i++) {
            const unsigned SIZE = 1024;
            char line[SIZE]; line[0] = 0;

            if(fgets(line, SIZE, fp) == NULL) {
                fprintf(stderr, "Error: cannot read Westerbork source data file '%s' line %u at %s:%d:%s\n",
                        filename, i, __FILE__, __LINE__, __func__);
                exit(2);
            }

            // Ok, now break this up into parts

            // Read RA DEC
            Real64 RA_h, RA_m, RA_s;
            Real64 Dec_d, Dec_m, Dec_s;
            char sign = '+';
            Sint32 num_read = sscanf(line, "%lf%*c%lf%*c%lf%*c %lf%*c%lf%*c%lf",
                                     &RA_h, &RA_m, &RA_s,
                                     &Dec_d, &Dec_m, &Dec_s
                                     );
            if(num_read != 6) {
                fprintf(stderr, "Error: cannot read Westerbork source data file '%s' line %u at %s:%d:%s\n",
                        filename, i, __FILE__, __LINE__, __func__);
                exit(2);
            }
            Dec_d = fabs(Dec_d);
            {
                char* p = line;
                while(*p != 0) {
                    if(*p == '-') {
                        sign = '-';
                        break;
                    }
                    p++;
                }
            }
            Real64 Dec = Dec_d + (Dec_m + (Dec_s)/60.0)/60.0;
            if(sign == '-') Dec = -Dec;
            Real64 RA = RA_h + (RA_m + (RA_s)/60.0)/60.0;
            RA *= 15.0;
            Dec *= M_DEG2RAD;
            RA *= M_DEG2RAD;
            // make a Space_Vector from a LatLon_Cart
            data[i] = LatLon_Cart(Dec, RA);

        } // for i over NUM_RANDOM_SOURCES

        // close the file
        fclose(fp);

        
        // That's it
        return data;
    }



// Use SOFA to perform a precession calculation
//_TITLE  get_position_of_date_rotation --precession changes for J2000 coord
    Space_Rotation_Matrix get_position_of_date(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 JD0_TT,         // I  The first part of a SOFA Julian Date, TT
        Real64 JD1_TT          // I  the second part fo the TT Julian Date
                               //    these times are for the observation
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls
//	See sofa.h

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//       2005 Dec 13  James M Anderson  --JIVE  start

//_END

    {
        // I need space for the rotation matrix
        Space_Rotation_Matrix rotation;

        // call the SOFA thing to calculate the rotation matrix
        FTN_NAME(iau_pnm00a) ( &JD0_TT, &JD1_TT, &(rotation.matrix[0][0]) );

        return rotation;
    }




    Real64 Delta_AT(
        const VEX_Time& t
        )
    {
        integer IY, IM, ID, J;
        doublereal FD, DELTAT;
        IY = t.Year();
        IM = t.Month();
        ID = t.Day();
        FD = t.Day_Fraction();
        J=0;
        DELTAT = 0.0;
        FTN_NAME(iau_dat) (&IY, &IM, &ID, &FD, &DELTAT, &J);
        return DELTAT;
    }


    Real64 GST(
        const VEX_Time& t
        )
    {
        doublereal UTA = t.JD0_1();
        doublereal UTB = t.JD1_1();
        doublereal TTA = t.JD0_C();
        doublereal TTB = t.JD1_C() + (Delta_AT(t) + 32.184)/SECONDS_PER_DAY;

        return FTN_NAME(iau_gst00a) (&UTA, &UTB, &TTA, &TTB);
    }




//_TITLE  run_ionosphere_eF  --run an ionosphere model for stations and sources
    void run_ionosphere_eF(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Space_Vector* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const struct tm* const Start_Time,
                               // I  The starting time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
        const Uint32 NUM_SECONDS,
                               // I  The total number of minutes to let run
        Ionosphere_Base& Iono_Model,
                               // I  The ionosphere model to use.
        const Real64 Display_Height
                               // I  A height at which to print some intersting
                               //    things such as the B field, the lat, lon
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
//	2005 Dec 13  James M Anderson  --JIVE  start from run_ionosphere
//      2005 Dec 15  JMA  --add extra output for lat and lon

//_END

    {
        // for now, use stdout for dumping information
        FILE* fp_out = stdout;


        for(Uint32 seconds = 0; seconds < NUM_SECONDS; seconds+= 300) {

            struct tm Eval_Time = *Start_Time;
            Eval_Time.tm_sec += seconds;
            mktime(&Eval_Time);
            
            // I need a starting VEX_Time
            VEX_Time now_time(Eval_Time.tm_year + 1900,
                              Eval_Time.tm_mon + 1,
                              Eval_Time.tm_mday,
                              Eval_Time.tm_hour,
                              Eval_Time.tm_min,
                              Eval_Time.tm_sec
                              );

            

            Space_Rotation_Matrix precession = get_position_of_date(
                now_time.JD0_C(),
                now_time.JD1_C() + (Delta_AT(now_time) + 32.184)/SECONDS_PER_DAY
                );

            Space_Rotation_Matrix
                lst_rotation(GST(now_time), z_axis);
            

            // Now loop through the stations and sources
            for(Uint32 station_number = 0;
                station_number < NUM_STATIONS; station_number++) {


                // now, for each source
                for(Uint32 source_number=0; source_number < NUM_SOURCES;
                    source_number++ ) {

                    // print out the time
                    fprintf(fp_out, "%4.4u/%2.2u/%2.2u:%2.2u:%2.2u:%02.0f ",
                            now_time.Year(),
                            now_time.Month(),
                            now_time.Day(),
                            now_time.Hour(),
                            now_time.Min(),
                            now_time.Sec()
                            );
                    

                    // Ok, get the source.  Rotate it through precession and
                    // for the LST
                    Space_Vector now_source =
                        source[source_number] * precession * lst_rotation;

                    LatLon_Cart AltAz =
                        station[station_number].convert_RADec_to_AltAz(now_source);
                    Real64 El   = AltAz.Lat();
                    Real64 Az   = AltAz.Lon();

                    //printf("%u Az %E El %E\n", seconds, Az*M_RAD2DEG, El*M_RAD2DEG);

                    // I need a unit direction vector for the direction.
                    Space_Unit_Vector direction = now_source.make_unit_vector();

                
                    if(AltAz.Lat() > 0.0) {
                        // this looks like a valid sky position to get a
                        // TEC value.  Call the ionosphere stuff
                        Real64 STEC;
                        Real64 STEC_err;
                        Real64 SFR;
                        Iono_Model.Integrate_Electron_Faraday(
                            station[station_number],
                            Eval_Time,
                            direction,
                            &STEC,
                            &STEC_err,
                            &SFR
                            );
                        // convert from m^{-2} to TECU
                        STEC *= 1E-16;
                        // convert from T m^{-2} to RMU
                        SFR *= 1E-12;


                        // Now, I also want to have the magnetic field strength
                        // along the line of sight at 350 km altitude.
                        LatLon_Cart pierce_point =
                            station[station_number].get_pierce_location(
                                direction,
                                Display_Height, // m
                                radius_Earth
                                );
                        Space_Vector B_field = Iono_Model.Magnetic_Field(
                            pierce_point,
                            Eval_Time
                            );
                        // get the dot product, which gives the B field component
                        // along the line of sight.  This is as the wavel comes
                        // toward the Earth, so it is actually antiparallel, or
                        // need to use a minus sign.
                        Real64 B_parallel = -B_field.dot_product(direction);
                        // print out B_parallel in \micro T
                        B_parallel *= 1E6;
                        // Extra magnetic field information
                        LatLon_Cart pierce_100 =
                            station[station_number].get_pierce_location(
                                direction,
                                100E3, // m
                                radius_Earth
                                );
                        Space_Vector B_100 = Iono_Model.Magnetic_Field(
                            pierce_100,
                            Eval_Time
                            );
                        Real64 B_p100 = -B_100.dot_product(direction)*1E6;
                        LatLon_Cart pierce_1000 =
                            station[station_number].get_pierce_location(
                                direction,
                                1000E3, // m
                                radius_Earth
                                );
                        Space_Vector B_1000 = Iono_Model.Magnetic_Field(
                            pierce_1000,
                            Eval_Time
                            );
                        Real64 B_p1000 = -B_1000.dot_product(direction)*1E6;
                    
                        fprintf(fp_out, "%3d %3d %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f\n",
                                station_number,
                                source_number,
                                STEC,
                                El*M_RAD2DEG,
                                Az*M_RAD2DEG,
                                SFR,
                                B_parallel,
                                pierce_point.Lat()*M_RAD2DEG,
                                pierce_point.Lon()*M_RAD2DEG,
                                B_p100,
                                B_p1000
                                );
                    }
                    else {
                        // the source is below the horizon.  Set the necessary
                        // values in the observations.
                        Real64 STEC       = 0.0;
                        fprintf(fp_out, "%3d %3d %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f %14.7f\n",
                                station_number,
                                source_number,
                                STEC,
                                El*M_RAD2DEG,
                                Az*M_RAD2DEG,
                                0.0,
                                0.0,
                                0.0,
                                0.0,
                                0.0,
                                0.0
                                );
                    }

                } // for source_number over sources
            } // for station_number over stations
        } // for seconds over NUM_SECONDS


        if(fp_out != stdout) fclose(fp_out);

        // That's all
        return;
        
    }
    

}















int main(int argc, char* argv[])
{
    if(argc != 10) {
        fprintf(stderr, "Error: correct usage is %s start_YYYY MM DD HH MM Num_Seconds Num_Sources Source_File Display_Height(m)\n", argv[0]);
        exit(2);
    }
    struct tm eval_time;
    eval_time.tm_sec = 0;
    eval_time.tm_min = atoi(argv[5]);
    eval_time.tm_hour = atoi(argv[4]);
    eval_time.tm_mday = atoi(argv[3]);
    eval_time.tm_mon = atoi(argv[2]) - 1;
    eval_time.tm_year = atoi(argv[1]) - 1900;
    eval_time.tm_isdst = -1;
    mktime(&eval_time);

    const Uint32 NUM_SECONDS = atoi(argv[6]);
    


    LatLon_Cart WLL(3828445.659, 445223.600, 5064921.568);
    Station_Reference Westerbork(WLL.Lat(), WLL.Lon(), 0.0, radius_Earth);
    Station_LatLon* stations = &Westerbork;
    const Uint32 NUM_STATIONS = 1; // We are only doing Westerbork

    // Get the random sources
    const Uint32 NUM_SOURCES = atoi(argv[7]);
    Space_Vector* Sources = read_in_Westerbork_sources(argv[8], NUM_SOURCES);


//     // Make a fake ionosphere
//     Ionosphere_Fake test_iono(fake_Gaussian,1E-6,1E-4);
    // Make an PIM ionosphere
    //Ionosphere_PIM test_iono(3E-7,1E-5);
    Ionosphere_PIM test_iono(3E-6,3E-5);




    // get the observations
    run_ionosphere_eF(NUM_SOURCES,
                      Sources,
                      NUM_STATIONS,
                      stations,
                      &eval_time,
                      NUM_SECONDS,
                      test_iono,
                      atof(argv[9])
                      );

    delete[] Sources;
    
    return 0;
}
    






