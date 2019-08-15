// vex_iono_processor.cxx
// process a VEX file for information related to ionosphere processing
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 20  James M Anderson  --JIVE  start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "vexplus.h"

#include "vex_time.h"
#include "vex_ut1_interp.h"
#include "vex_source.h"

#include "space_rotation_matrix.h"

#include "station_reference.h"

#include "sofa.h"





// Need some globals for the new vex2005 stuff
extern "C" {
    FILE *fp = NULL;
    char *filename = NULL;
}












// stuff only this file will need for now
namespace {

    // Use SOFA to perform a precession calculation
//_TITLE  get_position_of_date --apply precession to source.  needs J2000 coord
    MIM_PIM::Space_Vector get_position_of_date(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 JD0_TT,         // I  The first part of a SOFA Julian Date, TT
        Real64 JD1_TT,         // I  the second part fo the TT Julian Date
                               //    these times are for the observation
        MIM_PIM::Space_Vector& J2000_coord
                               // I  the input coordinates, in ~J2000 epoch
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
//       2005 Oct 12  James M Anderson  --JIVE  start

//_END

    {
        // I need space for the rotation matrix
        MIM_PIM::Space_Rotation_Matrix rotation;

        // call the SOFA thing to calculate the rotation matrix
        FTN_NAME(iau_pnm00a) ( &JD0_TT, &JD1_TT, &(rotation.matrix[0][0]) );

//         // Call the SOFA routine to perform the multiplication, as I have no
//         // idea how they would order their matrix.
//         Real64 out_vector[3];
//         FTN_NAME(iau_rxp) ( &(rotation.matrix[0][0]), J2000_coord.Vector(),
//                             out_vector);

//         printf("Got SOFA %E %E %E\n", out_vector[0], out_vector[1], out_vector[2]);

        // It appears that the SOFA matrix representation will work with mine.
        // So, I can just do the multiplication here
        // Try my way
//         MIM_PIM::Space_Vector out = J2000_coord * rotation;
//         printf("Got JMA  %E %E %E\n", out.X(), out.Y(), out.Z());
        return J2000_coord * rotation;
    }
    
}

























// set up a namespace area for stuff.
namespace JMA_VEX_AREA {




// GLOBALS


// FUNCTIONS




//_TITLE  process_vex_file --get information about stations and soruces for scans
    int process_vex_file(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const char vex_filename[],
                               // I  the path and filename of the vex file
        const char output_base[],
                               // I  the path and base filename for output
                               //    files.
        Real64 Expansion_Time, // I  The amount of time, in seconds, which
                               //    will be added *before* and *after* the
                               //    actual observing scan start and stop times
                               //    for which to calculate the necessary data.
        Real64 Grid_Interval_Time,
                               // I  The grid interval spacing, in seconds.
                               //    This is used to make sure that, for
                               //    instance, the initial time is forced to a
                               //    multiple of 30 seconds.  e.g., Only
                               //    global start times of 00:00:00, 00:00:30,
                               //    00:01:00, 00:01:30, ... would be used.
                               //    If identically 0, then no gridding is
                               //    performed.
        Real64 Calculation_Interval
                               // I  The amount of time, in seconds, between
                               //    each calculation, starting at the scan
                               //    start minus Expansion_Time, and
                               //    ending at or just beyond the scan end
                               //    plus Expansion_Time
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
// 	2005 Sep 20  James M Anderson
//      2005 Oct 11  JMA  --add new stuff, add base filename for output

//_END

    {
        // Before we get started, convert the Expansion_Time and
        // Calculation_Interval to Julian Days.
        Expansion_Time /= SECONDS_PER_DAY;
        Grid_Interval_Time /= SECONDS_PER_DAY;
        Calculation_Interval /= SECONDS_PER_DAY;

        // set up a grid interval accuracy of 1 ms
        const Real64 GRID_ACCURACY = 0.001 / SECONDS_PER_DAY;


//         MIM_PIM::LatLon_Cart a(49.227750*M_PI/180.0,41.054063*M_PI/180.0);
//         MIM_PIM::LatLon_Cart b = get_position_of_date(0.0,2462088.69,a);
//         printf("now %f %f\n", b.Lon()*180.0/M_PI, b.Lat()*180.0/M_PI);
//         exit(1);




        
        // Get the VEX file information
        std::string filename_s(vex_filename);
        VexPlus VEX_info(filename_s);
        VEX_info.parseVex();


        // Need some UT1 information for processing
        VEX_UT1_Interp UT1(VEX_info.N_EOP_Points());
        if((UT1.get_NUM_POINTS())) {
            // Now, run through all of the points.  If there is no EOP
            // information, then the number of points is returned as zero,
            // and all is well because 0 tells VEX_UT1_Interp to just
            // return 0 for UT1-UTC.

            // Since we are here, there is EOP information.  
            std::string s = VEX_info.EOPEpoch();
            VEX_Time t_base(s);
            // EOP_interval is in hours
            Real64 EOP_offset = VEX_info.EOP_interval() * 3600.0;

            for(Uint32 i=0; i < UT1.get_NUM_POINTS(); i++) {
                // Get the MJD
                VEX_Time t(t_base);
                t.add_offset(i*EOP_offset);
                UT1.set_MJD(i, t.JD1_C());
                // Get the X, Y, UT1-UTC stuff
                Real64 X = VEX_info.XWobble(i);
                Real64 Y = VEX_info.YWobble(i);
                Real64 U = VEX_info.UT1_UTC(i);
                UT1.set_X_Polar_Motion(i,X);
                UT1.set_Y_Polar_Motion(i,Y);
                UT1.set_UT1_UTC(i,U);
            }
        }




        // Ok, get all of the information about the sources
        // Hold the information in an array
        const Uint32 NUM_SOURCES = VEX_info.N_Sources();
        if(NUM_SOURCES <= 0) {
            // How can there be no sources?
            fprintf(stderr, "Error: cannot find any sources!\n%s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(3);
        }
        // allocate space for an array.  If there is no memory, let the
        // exception handler take over.
        VEX_Source* Source_List = new VEX_Source[NUM_SOURCES];

        // Now, get the information
        for(Uint32 i=0; i < NUM_SOURCES; i++) {
            // Ok, what is the string name of the source?
            std::string Name = VEX_info.SourceName(i);
            // Get the position on the sky
            Real64 RA = VEX_info.Source_RA(i);  // rad
            Real64 Dec = VEX_info.Source_Dec(i); // rad
            Real64 RARATE = VEX_info.Source_RARate(i); // rad/yr
            Real64 DecRATE = VEX_info.Source_DecRate(i); // rad/yr
            // Check for rate not being present
            if(RARATE == 9E9) RARATE = 0.0;
            if(DecRATE == 9E9) DecRATE = 0.0;
            // convert to radians per Julian day
            RARATE /= 365.25;
            DecRATE /= 365.25;
            // Generate some matrix classes
            MIM_PIM::LatLon_Cart Coord(Dec,RA,1.0,0.0);
            MIM_PIM::Space_Vector Rate;
            if((RARATE == 0.0) && (DecRATE == 0.0)) {
                // no motion of source
                Rate = MIM_PIM::Space_Vector(0.0,0.0,0.0);
            }
            else {
                // Correct the proper motion RA rate for the Declination,
                // as the given value is for the given coordinates, not a
                // general space value.
                RARATE *= Coord.cLat();
                // don't loose too much accuracy.  If the rate is less than
                // 10^{-10} rad/day, create a scaling factor
                Real64 Factor = 1.0;
                if((fabs(RARATE) < 1E-10) && (fabs(DecRATE) < 1E-10)) {
                    Factor = 1E-7;
                    RARATE /= Factor;
                    DecRATE /= Factor;
                }
                // But, at the same time, don't allow the proper motion rates
                // to be too large
                if((fabs(RARATE) > 1E-1) && (fabs(DecRATE) > 1E-1)) {
                    Real64 F = (fabs(RARATE) > fabs(DecRATE))
                        ? fabs(RARATE):fabs(DecRATE);
                    F *= 10.0;
                    Factor *= F;
                    RARATE /= F;
                    DecRATE /= F;
                }
                MIM_PIM::Space_Vector
                    Rate_Raw(MIM_PIM::LatLon_Cart(DecRATE,RARATE,1.0,0.0));
                MIM_PIM::Space_Rotation_Matrix Rot1(Dec, MIM_PIM::y_axis);
                MIM_PIM::Space_Rotation_Matrix Rot2(-RA, MIM_PIM::z_axis);
                Rate = Rate_Raw * Rot1 * Rot2;
                Rate -= Coord;
                Rate *= Factor;
            }
            
            // What is the reference Epoch?
            std::string RefEpoch = VEX_info.RefEpoch(i);
            // Convert to a VEX_Time
            VEX_Time RefEpochTime;
            if(RefEpoch == "J2000") {
                RefEpochTime = VEX_Time(2400000.5, 2451545.0 - 2400000.5);
            }
            else if(RefEpoch == "B1950") {
                fprintf(stderr, "Error: this code does not yet work with B1950 coordinates\n");
                exit(1);
                RefEpochTime = VEX_Time(2400000.5, 50.0*365.242198781+2415019.81352 - 2400000.5);
            }
            else if(RefEpoch == "B1900") {
                fprintf(stderr, "Error: this code does not yet work with B1900 coordinates\n");
                exit(1);
                RefEpochTime = VEX_Time(2400000.5, 2415019.81352 - 2400000.5);
            }
            else {
                RefEpochTime = VEX_Time(RefEpoch);
            }
//             printf("%s %f %f %f %f %s\n",
//                    Name.c_str(),
//                    RA, Dec, RARATE, DecRATE,
//                    RefEpoch.c_str());

            // Make a source out of this
            Source_List[i] = VEX_Source(Name, Coord, Rate,
                                        RefEpoch, RefEpochTime);

//            printf("RA %E Dec %E\n", Source_List[i]->RA_Dec.Lon(), Source_List[i]->RA_Dec.Lat());
        }



        // Ok, open up a file to dump out the station information
        const int FILENAME_SIZE = 1024;
        char new_filename[FILENAME_SIZE];
        strncat(strncpy(new_filename, output_base, FILENAME_SIZE),
                ".sta", FILENAME_SIZE);
        new_filename[FILENAME_SIZE-1] = 0;
        //snprintf(new_filename, FILENAME_SIZE, "%s.sta", output_base);
        FILE* fp_sta = fopen(new_filename, "w");
        if(fp_sta == NULL) {
            fprintf(stderr, "Error: cannot open station log file '%s' for writing\n%s:%d:%s\n",
                    new_filename,
                    __FILE__, __LINE__, __func__);
            exit(2);
        }

        // How many stations are there?
        const Uint32 NUM_STATIONS = VEX_info.N_Stations();
        if(NUM_STATIONS <= 0) {
            // How can there be no sources?
            fprintf(stderr, "Error: cannot find any stations!\n%s:%d:%s\n",
                    __FILE__, __LINE__, __func__);
            exit(3);
        }
        fprintf(fp_sta, "%u\n", NUM_STATIONS);


        // Ok, loop through the stations
        for(Uint32 station = 0; station < NUM_STATIONS; station++) {
            // Basically, everything below is going to be based off the
            // station name.
            // get the station name
            std::string Station_Name = VEX_info.Station(station);
            // Get the 2 letter code
            std::string Station_2Letter = VEX_info.TwoLetter(Station_Name);
            // How many scans are there
            const Uint32 NUM_SCANS = VEX_info.N_Scans(Station_Name);

            printf("Processing station %s (%2s) with %u scans\n",
                   Station_Name.c_str(), Station_2Letter.c_str(), NUM_SCANS);

            // get the coordinates of the station
            MIM_PIM::Station_Reference Station_Pos(VEX_info.SiteX(Station_Name),
                                                   VEX_info.SiteY(Station_Name),
                                                   VEX_info.SiteZ(Station_Name));
            if(Station_Pos.Radius() == 0.0) {
                // the station position is unknown.  Bad!!!
                fprintf(stderr, "Error: unknown position for station %u (%s)\n%s:%d:%s\n",
                        station, Station_Name.c_str(),
                        __FILE__, __LINE__, __func__);
                exit(3);
            }

            // Ok, dump out to the station list that we are processing this one
            fprintf(fp_sta, "%2.2s %13.5f %13.5f %13.5f\n",
                    Station_2Letter.c_str(),
                    Station_Pos.X(), Station_Pos.Y(), Station_Pos.Z());

            // Open the station output file
            snprintf(new_filename, FILENAME_SIZE, "%s.%s", output_base,
                     Station_2Letter.c_str());
            FILE* fp_2l = fopen(new_filename, "w");
            if(fp_2l == NULL) {
                fprintf(stderr, "Error: cannot open station scan file '%s' for writing\n%s:%d:%s\n",
                        new_filename,
                        __FILE__, __LINE__, __func__);
                exit(2);
            }


            // If there are no scans, we're done, and skip to the next station
            if(NUM_SCANS == 0) {
                fclose(fp_2l);
                continue;
            }


            // Now, let's run through the observations, collecting information

            // I need the start and end times
            VEX_Time* scan_start_time = new VEX_Time[NUM_SCANS];
            VEX_Time* scan_end_time = new VEX_Time[NUM_SCANS];
            // I need the names of the sources beign observed
            std::string* scan_source_name = new std::string[NUM_SCANS];
            // And I need to know whether or not I have processed each scan
            bool* scan_processed = new bool[NUM_SCANS];
            
            for(Uint32 scan = 0; scan < NUM_SCANS; scan++) {
                // set the processing flag to false
                scan_processed[scan] = false;

                // now check on the scan
                if(VEX_info.ScanName(Station_Name, scan).empty()) {
                    fprintf(stderr, "Error: unable to get scan name for Station %s Scan %u\n%s:%d:%s\n",
                            Station_Name.c_str(), scan,
                            __FILE__, __LINE__, __func__);
                    exit(3);
                }

                std::string start_s = VEX_info.ScanStart(Station_Name, scan);
                if(start_s.empty()) {
                    fprintf(stderr, "Error: unable to get scan start time for Station %s Scan %u\n%s:%d:%s\n",
                            Station_Name.c_str(), scan,
                            __FILE__, __LINE__, __func__);
                    exit(3);
                }
                Real64 duration = VEX_info.ScanDuration(Station_Name, scan);

                // convert to VEX_Time
                scan_start_time[scan] = VEX_Time(start_s);
                scan_end_time[scan] = scan_start_time[scan];
                scan_end_time[scan].add_offset(duration);

                // Get the source name
                scan_source_name[scan] = VEX_info.ScanSource(Station_Name, scan);
            } // for scan over the scans



            // Now figure out what the min and max times are.  This is being
            // done in terms of JD
            Real64 JD_min = scan_start_time[0].JD1_C();
            Real64 JD_max = scan_end_time[0].JD1_C();
            for(Uint32 scan = 1; scan < NUM_SCANS; scan++) {
                if(JD_min > scan_start_time[scan].JD1_C())
                    JD_min = scan_start_time[scan].JD1_C();
                if(JD_max < scan_end_time[scan].JD1_C())
                    JD_max = scan_end_time[scan].JD1_C();
            }

            // sanity check
            if(JD_min == JD_max) {
                fprintf(stderr, "Error: no time for observations for station %s\n%s:%d:%s\n",
                        Station_Name.c_str(),
                        __FILE__, __LINE__, __func__);
                exit(3);
            }

            // Now, adjust these times for the Expansion_Time, which
            // allows us to calculate before and after the actual observations
            JD_min -= Expansion_Time;
            JD_max += Expansion_Time;
            // Snap them to the grid, always to outward times
            if(Grid_Interval_Time != 0.0) {
                Real64 off = fmod(JD_min, Grid_Interval_Time);
                if( (fabs(off) < GRID_ACCURACY)
                    || (fabs(fabs(off)-Grid_Interval_Time) < GRID_ACCURACY) ) {
                    // do nothing, this is close to the grid
                }
                else {
                    JD_min -= (off > 0.0) ? off : Grid_Interval_Time + off;
                }
                off = fmod(JD_max, Grid_Interval_Time);
                if( (fabs(off) < GRID_ACCURACY)
                    || (fabs(fabs(off)-Grid_Interval_Time) < GRID_ACCURACY) ) {
                    // do nothing, this is close to the grid
                }
                else {
                    JD_max += (off > 0.0) ? Grid_Interval_Time - off : -off;
                }
            }
            

            // Now, how many steps are possible?
            Uint32 NUM_CALC_STEPS =
                Uint32(ceil( (JD_max-JD_min) / Calculation_Interval )) + 1;
            if(NUM_CALC_STEPS < 2) {
                fprintf(stderr, "Error: programmer error? %E %E\n%s:%d:%s\n",
                        JD_min, JD_max,
                        __FILE__, __LINE__, __func__);
                exit(1);
            }

            // Allocate space to mark where we need to calculate
            char* calc_process_flag = new char[NUM_CALC_STEPS];


            // Ok, now let's go through the sources, one by one
            for(Uint32 source = 0; source < NUM_SOURCES; source++) {
                // clear the processing flag
                memset(calc_process_flag, 0, NUM_CALC_STEPS);

                // Ok, now check for each scan.  If the source matches for the
                // scan, then mark it.   Also keep track of whether or not we
                // get anything.
                bool got_source = false;

                for(Uint32 scan = 0; scan < NUM_SCANS; scan++) {
                    // does the source name match?
                    if(Source_List[source].Name != scan_source_name[scan]) {
                        // no, so skip the rest
                        continue;
                    }

                    // mark the scan flag
                    got_source = scan_processed[scan] = true;

                    // Now, go through the times.
                    Real64 JD_low = scan_start_time[scan].JD1_C()
                        - Expansion_Time;
                    Real64 JD_high = scan_end_time[scan].JD1_C()
                        + Expansion_Time;
                    // Snap them to the grid, always to outward times
                    if(Grid_Interval_Time != 0.0) {
                        Real64 off = fmod(JD_low, Grid_Interval_Time);
                        if( (fabs(off) < GRID_ACCURACY)
                            || (fabs(fabs(off)-Grid_Interval_Time) < GRID_ACCURACY) ) {
                            // do nothing, this is close to the grid
                        }
                        else {
                            JD_low -= (off > 0.0) ? off : Grid_Interval_Time + off;
                        }
                        off = fmod(JD_high, Grid_Interval_Time);
                        if( (fabs(off) < GRID_ACCURACY)
                            || (fabs(fabs(off)-Grid_Interval_Time) < GRID_ACCURACY) ) {
                            // do nothing, this is close to the grid
                        }
                        else {
                            JD_high += (off > 0.0) ? Grid_Interval_Time - off : -off;
                        }
                    }



                    // Now, make a mark where this should go in our
                    // time slots
                    Uint32 index =
                        Uint32(floor((JD_low - JD_min) / Calculation_Interval));
                    for(;index < NUM_CALC_STEPS; index++) {
                        // what does the index time correspond to?
                        Real64 this_time = index*Calculation_Interval + JD_min;

                        // if we are over the end time, stop
                        if((this_time - JD_high) > GRID_ACCURACY) break;

                        // Otherwise, mark this as a time to process
                        calc_process_flag[index] = 1;
                    }
                } // for scan over NUM_SCANS



                // if we did not get a hit on this source, then skip the rest
                if(!got_source) continue;

                // Ok, we have a block of times to get ionospheric data.
                // print out the times and other useful information
                for(Uint32 index = 0;index < NUM_CALC_STEPS; index++) {
                    // if this one has no data area, then skip
                    if(calc_process_flag[index] == 0) continue;
                 
                    // make up the time
                    Real64 this_time = index*Calculation_Interval + JD_min;
                    // Convert to a VEX_Time.  Make sure to get the
                    // proper UT1-UTC value.
                    VEX_Time VEX_t(scan_start_time[0].JD0_C(), this_time,
                                   &UT1);


                    // Get the apparent Greenwich sidereal time
                    Real64 Delta_T;
                    {
                        integer IY = VEX_t.Year();
                        integer IM = VEX_t.Month();
                        integer ID = VEX_t.Day();
                        doublereal FD = VEX_t.Day_Fraction();
                        integer J = 0;
                        FTN_NAME(iau_dat)(&IY, &IM, &ID, &FD, &Delta_T, &J);
                    }
                    Real64 GAST; // Greenwich apparent sidereal time
                    Real64 TTA = VEX_t.JD0_C();
                    Real64 TTB = VEX_t.JD1_C()
                        + (Delta_T + 32.184) / SECONDS_PER_DAY;
                    {
                        doublereal UTA = VEX_t.JD0_1();
                        doublereal UTB = VEX_t.JD1_1();
                        GAST = FTN_NAME(iau_gst00a)(&UTA, &UTB, &TTA, &TTB);
                    }

//                     printf("Got GAST %f on %4d %2d %2d %f\n",
//                            GAST * 12.0 / M_PI,
//                            VEX_t.Year(), VEX_t.Month(), VEX_t.Day(),
//                            VEX_t.Day_Fraction() * 24.0);


                    // Ok, figure out where the source is on the sky
                    // FIrst, take care of the motion of the source on the sky
                    MIM_PIM::Space_Vector source_pos =
                        Source_List[source].RA_Dec +
                        ( (VEX_t.JD0_C() - Source_List[source].RefEpoch.JD0_C())
                          + (VEX_t.JD0_C() -Source_List[source].RefEpoch.JD0_C())
                          ) * Source_List[source].RA_Dec_Rate;
                    source_pos = source_pos.make_unit_vector();
                    // Now handle precession

                    source_pos = get_position_of_date(TTA, TTB, source_pos);


                    // I am currently ignoring abberation (~20\farcs).  Change
                    // this in the future.

                    // Correct for the apparent sidereal time.
                    // Remember that the coordinate suystem being used here is
                    // actually -HA, as Hour Angle is a left-handed system.
//                    {MIM_PIM::LatLon_Cart a(source_pos);printf("RADEC %f %f\n", a.Lon()*180.0/M_PI, a.Lat()*180.0/M_PI);}
                    
                    MIM_PIM::Space_Rotation_Matrix
                        rotation(GAST, MIM_PIM::z_axis);
                    MIM_PIM::Space_Vector HADec = source_pos * rotation;
                    
//                    {MIM_PIM::LatLon_Cart a(HADec);printf("-HADec %f %f\n", a.Lon()*180.0/M_PI, a.Lat()*180.0/M_PI);}


                    // Now also calculate the AzAlt
                    MIM_PIM::LatLon_Cart AltAz =
                        Station_Pos.convert_RADec_to_AltAz(HADec);
//                    {MIM_PIM::LatLon_Cart a(AltAz);printf("AltAz %f %f\n", a.Lon()*180.0/M_PI, a.Lat()*180.0/M_PI);}

                    // Make an entry in the output file.  Give the Time and
                    // position of the object, UTC and AltAz

                    // TIME
                    {
                        // Make sure the formatting is ok
                        VEX_Time t(VEX_t);
                        if(t.Sec() >= 59.995) t.add_offset(0.0045);
                        fprintf(fp_2l, "%4.4dy%2.2dm%2.2dd%2.2dh%2.2dm%05.2fs",
                                t.Year(), t.Month(), t.Day(),
                                t.Hour(), t.Min(), t.Sec() );
                    }
                    // SOURCE NAME AND J2000 RA DEC
                    {
                        Real64 RA = Source_List[source].RA_Dec.Lon() * M_RAD2DEG;
                        if(RA < 0.0) RA += 360.0;
                        RA /= 15.0;
                        fprintf(fp_2l, "%32s  %15.11f  %15.10f",
                                Source_List[source].Name.c_str(),
                                RA,
                                Source_List[source].RA_Dec.Lat() * M_RAD2DEG);
                    }
                    // ALT AZ
                    fprintf(fp_2l, "  %15.10f  %15.10f\n",
                            AltAz.Lon()*M_RAD2DEG, AltAz.Lat()*M_RAD2DEG);

                    
                } // for index over NUM_CALC_STEPS

            } // for source over NUM_SOURCES


       
         

            
            


            // cleanup after ourselves, free memory, close files, etc.
            fclose(fp_2l);
            delete[] scan_start_time;
            delete[] scan_end_time;
            delete[] scan_source_name;
            delete[] scan_processed;
        } // for station over NUM_STATIONS
         

        return 0;
    }
    



}  // end namespace


