// lofar9.cxx
// make a couple of Westerbork predictions
//_HIST  DATE NAME PLACE INFO
//	2006 May 17  James M Anderson  --JIVE  start thinking about
//                                       frequency shift problems






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




//_TITLE  run_velocities --for a 24 hour period, calculate the velocity diffs
    void run_velocities(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const struct tm* const Start_Time,
                               // I  The starting time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
        const Station_LatLon& station1,
                               // I  the first station
        const Station_LatLon& station2,
                               // I  the second station
        const Space_Vector& source1,
                               // I  the first source
        const Space_Vector& source2
                               // I  the second source
        )

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//      2006 May 17  JMA  --start

//_END

    {
        // Create a set of velocity vectors for the stations.  For right now,
        // use some simple velocity assumptions.  The rotational speed at the
        // equator is the circumference of the Earth divided by 24 hours
        const Real64 speed_equator = 2.0*M_PI*radius_Earth
            / (24.0*60.0*60.0);
        Space_Vector velocity1(-station1.cLat()*station1.sLon()*speed_equator,
                               station1.cLat()*station1.cLon()*speed_equator,
                               0.0);
        Space_Vector velocity2(-station2.cLat()*station2.sLon()*speed_equator,
                               station2.cLat()*station2.cLon()*speed_equator,
                               0.0);


        
        // for now, use stdout for dumping information
        FILE* fp_out = stdout;


        for(Uint32 seconds = 0; seconds < 24*60*60; seconds+= 600) {

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


            // Rotate sources for precession and
            // for the LST
            Space_Vector now_source1 =
                source1 * precession * lst_rotation;
            Space_Vector now_source2 =
                source2 * precession * lst_rotation;

            // Get the speeds of the stations in the directions of the
            // sources
            Real64 speed_1_1 = velocity1.dot_product(now_source1.make_unit_vector());
            Real64 speed_1_2 = velocity1.dot_product(now_source2.make_unit_vector());
            Real64 speed_2_1 = velocity2.dot_product(now_source1.make_unit_vector());
            Real64 speed_2_2 = velocity2.dot_product(now_source2.make_unit_vector());

            // Create speed differences
            Real64 diff1 = speed_1_1 - speed_2_1;
            Real64 diff2 = speed_1_2 - speed_2_2;
            Real64 diff = diff1-diff2;

            // what is the fractional frequency difference?
            Real64 dnu_o_nu = diff / 2.998E8;


            // Where are the sources in the station skies?
            LatLon_Cart AltAz;
            AltAz = station1.convert_RADec_to_AltAz(now_source1);
            Real64 El1   = AltAz.Lat();
            //Real64 Az1   = AltAz.Lon();
            AltAz = station2.convert_RADec_to_AltAz(now_source1);
            Real64 El2 = AltAz.Lat();
            
            // print out the time
            fprintf(fp_out, "%4.4u/%2.2u/%2.2u:%2.2u:%2.2u:%02.0f %5d ",
                    now_time.Year(),
                    now_time.Month(),
                    now_time.Day(),
                    now_time.Hour(),
                    now_time.Min(),
                    now_time.Sec(),
                    seconds
                    );
            // print out the other stuff
            fprintf(fp_out, "%10.3E %10.3E %10.3E %10.3E\n",
                    diff, dnu_o_nu, El1*M_RAD2DEG, El2*M_RAD2DEG);
        } // for time over the day                    


        if(fp_out != stdout) fclose(fp_out);

        // That's all
        return;
        
    }
    

}















int main(int argc, char* argv[])
{
    if(argc != 11) {
        fprintf(stderr, "Error: correct usage is %s x1(m) y1(m) z1(m) x2(m) y2(m) z2(m) RA1(hours) Dec1(degr) RA2(hours) Dec2(degr)\n", argv[0]);
        exit(2);
    }
    struct tm eval_time;
    eval_time.tm_sec = 0;
    eval_time.tm_min = 0;
    eval_time.tm_hour = 0;
    eval_time.tm_mday = 3;   // 03
    eval_time.tm_mon = 7;    // Aug
    eval_time.tm_year = 104; // 2004
    eval_time.tm_isdst = -1;
    mktime(&eval_time);

    Station_LatLon station1(atof(argv[1]),atof(argv[2]),atof(argv[3]));
    Station_LatLon station2(atof(argv[4]),atof(argv[5]),atof(argv[6]));

    // Get the source
    LatLon_Cart Source1(atof(argv[ 8])*M_DEG2RAD, atof(argv[7])*15.0*M_DEG2RAD);
    LatLon_Cart Source2(atof(argv[10])*M_DEG2RAD, atof(argv[9])*15.0*M_DEG2RAD);

    // print out the angular offset information.
    // Assume that source 1 is the phase center
    {
        Station_Reference ref(Source1);
        LatLon_Cart off_c = ref.get_Cartesian_fudge_offset(Source2);
        LatLon_Cart off_p = ref.get_polar_offset(Source2);
        LatLon_Cart off_e = ref.get_equatorial_offset(Source2);
        printf("# Fudge offset RA %12.6f arcsec Dec %12.6f arcsec\n",
               off_c.Lon()*M_RAD2DEG*3600.0, off_c.Lat()*M_RAD2DEG*3600.0);
        printf("# Polar offset Position_Angle %12.6f degrees Dist %12.6f arcsec\n",
               (M_PI-off_p.Lon())*M_RAD2DEG, off_p.Lat()*M_RAD2DEG*3600.0);
        printf("# Equatorial offset RA %12.6f arcsec Dec %12.6f arcsec\n",
               off_e.Lon()*M_RAD2DEG*3600.0, off_e.Lat()*M_RAD2DEG*3600.0);
    }      



    // get the observations
    run_velocities(&eval_time, station1, station2, Source1, Source2);

    
    return 0;
}
    






