// vlba_obs.cxx
// a program to compute pierce points for VLBA observations
// 2006 Nov 01  James M Anderson  --JIVE  start









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



    //_TITLE  get_station_list_file
    Uint32 get_station_list_file(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char* filename,  // I  name of the file to read stations from
        Station_Reference** ref,//O  the reference station, at the center of
                               //    the spiral
        Station_LatLon** stations
                               // O  the stations, as an array
        )

// get_station_list_spiral        O  the total number of stations allocated

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will read a set of stations for observations.

//	The file should have the format of
//	NAME X Y Z
//	Where name is a text string, and X Y Z are the Cartesion coordinates of
//	the station, in m.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2006 Nov 01  James M Anderson  --JIVE start

//_END

    {
        FILE* fp = fopen(filename, "r");
        if(fp == NULL) {
            fprintf(stderr, "Error: cannot open '%s' for getting station positions\n", filename);
            exit(2);
        }
        // How many stations are there?
        const int SIZE = 1024;
        char line[SIZE];
        Uint32 NUM_STATIONS = 0;
        while(fgets(line, SIZE, fp) != NULL) NUM_STATIONS++;
        if(NUM_STATIONS == 0) {
            fprintf(stderr, "Error: no stations read from '%s'\n", filename);
            exit(2);
        }

        rewind(fp);

        {
            // Ok, make the central station
            line[0] = 0;
            fgets(line, SIZE, fp);
            Real64 x, y, z;
            sscanf(line, "%*s X=%lf Y=%lf Z=%lf", &x, &y, &z);
            Station_LatLon central(x,y,z);
            *ref = new Station_Reference(central);
            *stations = new Station_LatLon[NUM_STATIONS];
            
            // check that this went ok
            if((*ref == NULL) || (*stations == NULL)) {
                fprintf(stderr, "Error allocating space for %u stations in %s:%d:%s\n",
                        NUM_STATIONS, __FILE__, __LINE__, __func__);
                exit(1);
            }

            // Ok, station 0 is the core
            (*stations)[0] = central;
        }

        // And now for the rest
        for(Uint32 station_id = 1; station_id < NUM_STATIONS; station_id++) {
            line[0] = 0;
            fgets(line, SIZE, fp);
            Real64 x, y, z;
            sscanf(line, "%*s X=%lf Y=%lf Z=%lf", &x, &y, &z);
            Station_LatLon central(x,y,z);
            (*stations)[station_id] = Station_LatLon(x,y,z);
        }

        // That seems to be all, just return the number of antennas allocated
        return NUM_STATIONS;
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





//_TITLE  run_obs_pierce  --find piercing points during observation
    void run_obs_pierce(
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
        const Uint32 Delta_s,  // I  The number of seconds between observations
        const Uint32 NUM_SECONDS,
                               // I  The total number of seconds to let run
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


        for(Uint32 seconds = 0; seconds <= NUM_SECONDS; seconds+= Delta_s) {

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
            printf("####################\n");

            

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
                        // piercing point
                        LatLon_Cart pierce_point =
                            station[station_number].get_pierce_location(
                                direction,
                                Display_Height, // m
                                radius_Earth
                                );
                        fprintf(fp_out, "sta%3d src%3d %14.7f %14.7f %14.7f %14.7f\n",
                                station_number,
                                source_number,
                                El*M_RAD2DEG,
                                Az*M_RAD2DEG,
                                pierce_point.Lat()*M_RAD2DEG,
                                pierce_point.Lon()*M_RAD2DEG
                                );
                    }
                    else {
                        // the source is below the horizon.  Set the necessary
                        // values in the observations.
                        fprintf(fp_out, "sta%3d src%3d %14.7f %14.7f %14.7f %14.7f\n",
                                station_number,
                                source_number,
                                El*M_RAD2DEG,
                                Az*M_RAD2DEG,
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
    for(int i=0; i < argc; i++) printf("%s ", argv[i]);
    fputc('\n', stdout);
    if(argc < 12) {
        fprintf(stderr, "Error: correct usage is %s stations.dat YYYY MM DD HH MM SS \\Delta_SS Observation_Length(s) iono_height(m) ##h##m##.### s##d##'##.##\n", argv[0]);
        exit(2);
    }
    struct tm eval_time;
    eval_time.tm_sec = atoi(argv[7]);
    eval_time.tm_min = atoi(argv[6]);
    eval_time.tm_hour = atoi(argv[5]);
    eval_time.tm_mday = atoi(argv[4]);
    eval_time.tm_mon = atoi(argv[3]) - 1;
    eval_time.tm_year = atoi(argv[2]) - 1900;
    eval_time.tm_isdst = -1;
    mktime(&eval_time);



    const Real64 ionosphere_height = atof(argv[10]);
    



    
    Station_Reference* ref;
    Station_LatLon* stations;
    const Uint32 NUM_STATIONS = get_station_list_file(
        argv[1],     // I  the filename of the station list
        &ref,
        &stations
        );

//    fprintf(stderr, "Got stations\n");

//     for(Uint32 i=0; i < NUM_STATIONS; i++) {
//         LatLon_Cart p = ref->get_polar_offset(stations[i]);
//         printf("Sta %3d Lat %12.6f Lon %12.6f\n",
//                i,p.Lat()*M_RAD2DEG, p.Lon()*M_RAD2DEG);
//     }



    Space_Vector* Sources;
    // get the source RA and Dec
    {
        Real64 d, m, s;
        d=m=s=0.0;
        sscanf(argv[11], "%lfh%lfm%lf", &d,&m,&s);
        Real64 RA = (((s)/60.0 + m)/60.0 + d) * 15.0 * M_DEG2RAD;
        
        d=m=s=0.0;
        sscanf(argv[12], "%lfd%lf'%lf", &d,&m,&s);
        d=fabs(d);
        Real64 Dec = (((s)/60.0 + m)/60.0 + d) * M_DEG2RAD;
        if(argv[12][0] == '-') Dec = -Dec;
        LatLon_Cart MySource(Dec, RA);
        Sources = new Space_Vector(MySource);
    }
    const Uint32 NUM_SOURCES = 1;

    run_obs_pierce(
        NUM_SOURCES,
        Sources,
        NUM_STATIONS,
        stations,
        &eval_time,
        atoi(argv[8]),
        atoi(argv[9]),
        ionosphere_height
        );

    return 0;
}
    






