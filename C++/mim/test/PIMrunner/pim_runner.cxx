// pim_runner.cxx
// runs VLBIPIM4 to generate ionospheric data for sources and stations
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 19  James M Anerson  --JIVE  start





#ifndef PIM_RUNNER_CXX
#define PIM_RUNNER_CXX
#endif // PIM_RUNNER_CXX






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

#include "pim_asub_c.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS

//_TITLE  run_PIMVLBI --run PIMVLBI4 to get info from stations and sources
    void run_PIMVLBI(
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
//	This function handles the overall aspects of calling PIMVLBI4 for
//      generating ionosphere information

//	See R.M> Campbell's notes for running PIMVLBI4.  This function assumes
//	that the program is being run from the proper directory, and that the
//	necessary files and their filenames are present.

//	First, an instruction set will be generated for the "manual" operation
//	commands which PIMVLBI4 expects.  This function will then
//      generate a station file for each station.  Then,
//	for each source, a source file will be written.  Then PIMVLBI4 will be
//	run, generating an output file.  The contents of that output
//	file will be read in, and stored to the Observations area.

//	This means that a total of NUM_OBSERVATIONS runs of PIMVLBI will
//	be made.  This is somewhat inefficient, but as there are only a limited
//	set of sources and stations PIMVLBI4 can handle at once, this
//	seems the safest for now.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 22  James M Anderson  --JIVE  start

//_END

    {
        // Ok, I can probably easily do 10 sources at once with PIMVLBI4
        // based on the program barfing with 11 sources.
        const Uint32 MAX_NUM_SOURCES_PIM = 10;

        
        // Ok, how many observations
        *NUM_OBSERVATIONS = NUM_SOURCES * NUM_STATIONS;
        // Now get some space
        *observation = new Observation_Ionosphere[*NUM_OBSERVATIONS];
        if(*observation == NULL) {
            fprintf(stderr, "Error: cannot get enough memory for %u observations in %s:%d:%s\n",
                    *NUM_OBSERVATIONS, __FILE__, __LINE__, __func__);
            exit(1);
        }


        // open up the overall file for processing
        FILE* fp_out = fopen(PIM_TEXT_PROCESSING_FILE, "w");
        if(fp_out == NULL) {
            fprintf(stderr, "Error: cannot open processing file '%s' in %s:%d:%s\n",
                    PIM_TEXT_PROCESSING_FILE,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }


        // Now loop through the stations and sources
        for(Uint32 station_number = 0, observation_number =0;
            station_number < NUM_STATIONS; station_number++) {


            // Ok, generate the file input information.
            // Assume that I can do all MAX_NUM_SOURCES_PIM sources.
            {
                FILE* fp_run_info = fopen(PIM_STDIN_COMMANDS_FILE, "w");
                if(fp_run_info == NULL) {
                    fprintf(stderr, "Error: cannot open command file '%s' in %s:%d:%s\n",
                            PIM_STDIN_COMMANDS_FILE,
                            __FILE__, __LINE__, __func__);
                    exit(1);
                }
                // write out some information.  See the PIMVLBI4 instructions.
                // much of this is magic
                fprintf(fp_run_info, "s\n%02d, %03d, %02d, %02d\n120\n0.5\nA/\n",
                        Eval_Time->tm_year%100,
                        Eval_Time->tm_yday+1,
                        Eval_Time->tm_hour,
                        Eval_Time->tm_min
                        );
                for(Uint32 src = 0; src < MAX_NUM_SOURCES_PIM; src++) {
                    fputc('A' + src, fp_run_info);
                    if(src < MAX_NUM_SOURCES_PIM-1) fputc(' ',  fp_run_info);
                }
                fprintf(fp_run_info, "/\ny\nn\nn\nn\n%s\n1\n", PIM_OUTPUT_FILE);
                fclose(fp_run_info);
            }






            
            // generate a new station file
            {
                FILE* fp_sta = fopen(PIM_STATION_FILE, "w");
                if(fp_sta == NULL) {
                    fprintf(stderr, "Error: cannot open station file '%s' in %s:%d:%s\n",
                            PIM_STATION_FILE,
                            __FILE__, __LINE__, __func__);
                    exit(1);
                }
                //const char orig_line[] = "R BR-VLBA  47:56:23.592N   119:40:59.803W";

                // Ok, I need the lat and lon in degrees, min, sec
                Real64 lats = station[station_number].Lat()*M_RAD2DEG;
                char lath = (lats >= 0.0) ? 'N':'S';
                lats = fabs(lats);
                Real64 latd = trunc(lats);
                lats = (lats-latd)*60.0;
                Real64 latm = trunc(lats);
                lats = (lats-latm)*60.0;
                Real64 lons = station[station_number].Lon()*M_RAD2DEG;
                char lonh = (lons >= 0.0) ? 'E':'W';
                lons = fabs(lons);
                Real64 lond = trunc(lons);
                lons = (lons-lond)*60.0;
                Real64 lonm = trunc(lons);
                lons = (lons-lonm)*60.0;
                fprintf(fp_sta, "A A        %02.0f:%02.0f:%06.3f%c   %03.0f:%02.0f:%06.3f%c\n",
                        latd, latm, lats, lath,
                        lond, lonm, lons, lonh
                        );
                fclose(fp_sta);
            }

            // now, for each source
            for(Uint32 source_number=0; source_number < NUM_SOURCES; /*nada*/ ) {


                // Ok, how many sources can I really do?
                const Uint32 NUM_SOURCES_TO_DO =
                    (source_number + MAX_NUM_SOURCES_PIM <= NUM_SOURCES) ?
                    MAX_NUM_SOURCES_PIM : NUM_SOURCES - source_number;

                // If not doing the max number of sources, the
                // input file needs to be rewritten
                if(NUM_SOURCES_TO_DO != MAX_NUM_SOURCES_PIM) {
                    FILE* fp_run_info = fopen(PIM_STDIN_COMMANDS_FILE, "w");
                    if(fp_run_info == NULL) {
                        fprintf(stderr, "Error: cannot open command file '%s' in %s:%d:%s\n",
                                PIM_STDIN_COMMANDS_FILE,
                                __FILE__, __LINE__, __func__);
                        exit(1);
                    }
                    // write out some information.  See the PIMVLBI4 instructions.
                    // much of this is magic
                    fprintf(fp_run_info, "s\n%02d, %03d, %02d, %02d\n120\n0.5\nA/\n",
                            Eval_Time->tm_year%100,
                            Eval_Time->tm_yday+1,
                            Eval_Time->tm_hour,
                            Eval_Time->tm_min
                            );
                    for(Uint32 src = 0; src < NUM_SOURCES_TO_DO; src++) {
                        fputc('A' + src, fp_run_info);
                        if(src < NUM_SOURCES_TO_DO-1) fputc(' ',  fp_run_info);
                    }
                    fprintf(fp_run_info, "/\ny\nn\nn\nn\n%s\n1\n", PIM_OUTPUT_FILE);
                    fclose(fp_run_info);
                }







                
                // generate a new source file
                {
                    FILE* fp_source = fopen(PIM_SOURCE_FILE, "w");
                    if(fp_source == NULL) {
                        fprintf(stderr, "Error: cannot open source file '%s' in %s:%d:%s\n",
                                PIM_SOURCE_FILE,
                                __FILE__, __LINE__, __func__);
                        exit(1);
                    }

                    // const char orig_lin[] = "PSR2021   20:22:49.8703   +51:54:50.2913";
                    char line_head[] = "A         ";
                    for(Uint32 src = 0; src < NUM_SOURCES_TO_DO; src++) {
                            line_head[0] = 'A' + src;
                            fprintf(fp_source, line_head);
                            source[source_number+src].print_RA(fp_source, 4, ':');
                            fputc(' ', fp_source);
                            fputc(' ', fp_source);
                            fputc(' ', fp_source);
                            source[source_number+src].print_Dec(fp_source, 4, ':');
                            fputc('\n', fp_source);
                        }

                    fclose(fp_source);
                }



                //fprintf(stderr, "Check input\n");
                {
                    const int SIZE = 256;
                    char command[SIZE];
                    sprintf(command, "%s < %s",
                            PIM_PROGRAM_NAME,PIM_STDIN_COMMANDS_FILE);
                    system(command);
                }



                // Now, read stuff in
                {
                    FILE* fp_data = fopen(PIM_OUTPUT_FILE, "r");
                    if(fp_data == NULL) {
                        fprintf(stderr, "Error: cannot open data file '%s' in %s:%d:%s\n",
                                PIM_OUTPUT_FILE,
                                __FILE__, __LINE__, __func__);
                        exit(1);
                    }

                    // get the first line
                    const int SIZE = 256;
                    char line[SIZE];
                    
                    for(Uint32 src = 0; src < NUM_SOURCES_TO_DO; src++) {
                        line[0] = 0;
                        fgets(line, SIZE, fp_data);
                        if(strlen(line) < 99) {
                            fprintf(stderr, "Error: invalid data file '%s' in %s:%d:%s\n",
                                    PIM_OUTPUT_FILE,
                                    __FILE__, __LINE__, __func__);
                            exit(1);
                        }
                        // dump the line
                        //fputs(line,stdout);
                        // grab the information
                        (*observation)[observation_number+src].STEC = atof(line+28);
                        (*observation)[observation_number+src].El   = atof(line+82) * M_DEG2RAD;
                        (*observation)[observation_number+src].Az   = atof(line+91) * M_DEG2RAD;

                        fprintf(fp_out, "For sta%3d src%3d obs%5d got %11.4f %10.3f %10.3f\n",
                                station_number,
                                source_number+src,
                                observation_number+src,
                                (*observation)[observation_number+src].STEC,
                                (*observation)[observation_number+src].El*M_RAD2DEG,
                                (*observation)[observation_number+src].Az*M_RAD2DEG
                                );
                    }
                    //fflush(fp_out);

                    fclose(fp_data);
                    
                } // reading stuff in


                // Bob's program doesn't want to overwrite
                remove(PIM_OUTPUT_FILE);


                // increment the indices
                observation_number += NUM_SOURCES_TO_DO;
                source_number += NUM_SOURCES_TO_DO;

                
            } // for source_number over sources
        } // for station_number over stations



        fclose(fp_out);

        // That's all
        return;
        
    }
    


























//_TITLE  run_PIMVLBI2 --call PIM library to get info from stations and sources
    void run_PIMVLBI2(
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
//	This function handles the overall aspects of calling the modified PIM
//	library for generating ionosphere information

//	See R.M> Campbell's notes for running PIMVLBI4.  Modifications have been
//	made so that there is now a single function call for each observation.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Sep 12  James M Anderson  --JIVE  start

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


                // Ok, if the elevation is greater than 0, run PIM
                // Let's just pretend that Dec is elevation and RA is
                // azimuth for now.
                (*observation)[observation_number].El   =
                    source[source_number].Dec();
                (*observation)[observation_number].Az   =
                    source[source_number].RA();
                if(source[source_number].Dec() > 0.0) {
                    // this looks like a valid sky position to get a
                    // TEC value.  Call the PIM stuff
                    Real64 TUT = (Eval_Time->tm_hour +(Eval_Time->tm_min
                                                       +(Eval_Time->tm_sec)
                                                       /60.0)/60.0);
                    Real64 STEC, SRM;
                    Sint32 SOLCODE=0;
                    Sint32 RETCODE=0;
                    pim_asub_c(
                        PIM_DATA_PATH,
                        station[station_number].Lat(),
                        station[station_number].Lon(),
                        station[station_number].Radius() - radius_Earth,
                        source[source_number].RA(),
                        source[source_number].Dec(),
                        Eval_Time->tm_year+1900,
                        Eval_Time->tm_yday+1,
                        Eval_Time->tm_mon+1,
                        Eval_Time->tm_mday,
                        TUT,
                        &STEC,
                        &SRM,
                        (integer*)&SOLCODE,
                        (integer*)&RETCODE
                        );
                    if(RETCODE < 0) {
                        fprintf(stderr, "Error: bad RETCODE=%d from pim_asub_c, in station %u, source %u\n",
                                RETCODE, station_number, source_number);
                        exit(1);
                    }
                    (*observation)[observation_number].STEC       = STEC;
                    (*observation)[observation_number].sigma_STEC = 1.0e16;
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
    







    


















//_TITLE  load_PIMVLBI --load a PIMVLBI4 dataset from run_PIMVLBI
    void load_PIMVLBI(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const char filename[], // I  the name of the file holding the
                               //    PIM run data
                               // I  The evaluation time
                               //    Note that all fields must be properly
                               //    filled in (see, e.g. mktime)
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
//	This function handles reading in an output from the run_PIMVLBI function
//	above.  The PIM calculations take a long time, so multiple runs
//	to test the least squares fitting are not advisable.  Plus, the
//	PIMVLBI software is currently only running on Bob's ancient Sun,
//	so reading in the output from a text file is the best way to get the
//	data onto my PC.

//	This function will read in observations from a *limited* text file.
//	THis text file only has information about the STEC, El, and Az of
//	the observation.  In other words, it does not have full information
//	about the source position on the sky, or the station location on
//	the ground, other than integer numbers.  Thus, the caller must use
//	the same station positions and the same source positions when calling
//	this function as was used for the run_PIMVLBI call.

//	This function will return back observation objects with the STEC
//	value only filled.  The user must figure out a suitable VTEC and
//	LatLon_Cart position.

//_FILE  files used and logical units used

//_LIMS  design limitations
//	the user must call with the same station and source information as
//	used for the run_PIMVLBI call, but there is no way to check this.

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE  start

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


        // open up the input file for processing
        FILE* fp_in = fopen(filename, "r");
        if(fp_in == NULL) {
            fprintf(stderr, "Error: cannot open processing file '%s' in %s:%d:%s\n",
                    filename,
                    __FILE__, __LINE__, __func__);
            exit(1);
        }


        // Now loop through the stations and sources
        for(Uint32 station_number = 0, observation_number =0;
            station_number < NUM_STATIONS; station_number++) {

            // now, for each source
            for(Uint32 source_number=0; source_number < NUM_SOURCES;
                source_number++, observation_number++) {

                // get the next line
                const int SIZE = 256;
                char line[SIZE];
                line[0] = 0;
                fgets(line, SIZE, fp_in);

                    
                if(strlen(line) < 60) {
                    fprintf(stderr, "Error: invalid data file '%s' in %s:%d:%s\n",
                            filename,
                            __FILE__, __LINE__, __func__);
                    exit(2);
                }

                // Now get the input values
                Uint32 sta, src, obs;
                Real64 STEC, El, Az;
                if(sscanf(line, "For sta %u src%u obs%u got %lf %lf %lf",
                          &sta, &src, &obs,
                          &STEC, &El, &Az)
                   != 6) {
                    fprintf(stderr, "Error: invalid data file '%s' in %s:%d:%s\n",
                            filename,
                            __FILE__, __LINE__, __func__);
                    exit(2);
                }

                if((station_number != sta) || (source_number != src)
                   || (observation_number != obs)) {
                    fprintf(stderr, "Error: invalid data file '%s' in %s:%d:%s\n",
                            filename,
                            __FILE__, __LINE__, __func__);
                    exit(2);
                }
                (*observation)[observation_number].station_number = sta;
                (*observation)[observation_number].object_number = src;
                (*observation)[observation_number].STEC = STEC;
                (*observation)[observation_number].sigma_STEC =
                    (STEC != -999.0) ? 1.0e16 : -1.0e16;
                (*observation)[observation_number].El   = El * M_DEG2RAD;
                (*observation)[observation_number].Az   = Az * M_DEG2RAD;
            
                    
            } // for source_number over sources
        } // for station_number over stations



        fclose(fp_in);

        // That's all
        return;
        
    }





    
}  // end namespace


