// make_ap.cxx
// make some ap data from some kp data, because IRI doesn't cover enough range
//	2005 sep 13  James M Anderson  --JIVE  start


// kpYYMM.tab data files are available from
// ftp://ftp.gwdg.de/pub/geophys/kp-ap/tab
// See also
// http://www.gfz-potsdam.de/pb2/pb23/GeoMag/niemegk/kp_index/
// http://www.ngdc.noaa.gov/stp/GEOMAG/kp_ap.html
//
// The F stuff is available from
// ftp://ftp.ngdc.noaa.gov/STP/GEOMAGNETIC_DATA/INDICES/KP_AP
// see also the PIM NOAA stuff (kpf107.dat)
// Concatenate all of the necessary yearly data together in time order
// The KP_file should only have data from the same time range


#include <stdio.h>
#include <stdlib.h>
#include <time.h>






int main(int argc, char* argv[])
{
    // check the arguments
    if(argc != 6) {
        fprintf(stderr, "Error: correct usage is %s YYYY_start MM_start YYYY_end MM_end KP_file\n", argv[0]);
        fprintf(stderr, "Where the year and months are integers, and the kpYYMM.tab files are expected to be in the current directory\n");
        fprintf(stderr, "Ending year and month are inclusive\n");
        exit(2);
    }

    struct tm this_time;
    this_time.tm_sec = 0;
    this_time.tm_min = 0;
    this_time.tm_hour = 12;
    this_time.tm_mday = 1;
    this_time.tm_mon = atoi(argv[2]) - 1;
    this_time.tm_year = atoi(argv[1]) - 1900;
    this_time.tm_isdst = -1;
    mktime(&this_time);



    struct tm end_time;
    end_time.tm_sec = 0;
    end_time.tm_min = 0;
    end_time.tm_hour = 12;
    end_time.tm_mday = 1;
    end_time.tm_mon = atoi(argv[4]) - 1;
    end_time.tm_year = atoi(argv[3]) - 1900;
    end_time.tm_isdst = -1;
    mktime(&end_time);


    // Get the KP file, and figuer out where we are
    FILE* fp_kp = fopen(argv[5],"r");
    if(fp_kp == NULL) {
        fprintf(stderr, "Error: cannot open KP file '%s'\n", argv[5]);
        exit(2);
    }
    const int KP_SIZE = 128;
    char KP_line[KP_SIZE];
    for(;;) {
        KP_line[65] = KP_line[0] = 0;
        if(fgets(KP_line, KP_SIZE, fp_kp) == NULL) {
            fprintf(stderr, "Error reading from %s\n", argv[5]);
            exit(2);
        }
        int year, month;
        if(sscanf(KP_line, "%2d%2d", &year, &month) != 2) {
            fprintf(stderr, "Error reading date from %s\n", argv[5]);
            exit(2);
        }
        if(year < 50) year += 2000;
        if( (year <= this_time.tm_year + 1900)
            && (month < this_time.tm_mon + 1) ) {
            // do nothing
        }
        else break;
    }
        
    

    // keep track of the currently open file for AP stuff
    FILE* fp = NULL;
    int file_year = -1;
    int file_month = -1;

    // loop over the 
    for(;;) {
        // check if we need a new file
        char filename[128];
        if((this_time.tm_mon != file_month)
           || (this_time.tm_year != file_year)) {
            file_year = this_time.tm_year;
            file_month = this_time.tm_mon;
            if((fp)) fclose(fp);
            sprintf(filename, "kp%2.2d%2.2d.tab",
                    file_year%100, file_month+1);
            if((fp = fopen(filename,"r")) == NULL) {
                fprintf(stderr, "Error: cannot open '%s' for input\n", filename);
                exit(2);
            }
        }
        // ok, get the next day's worth of data
        const int INTERVAL = 8;
        int year, month, day;
        char block[INTERVAL][2];
        int index[INTERVAL];
        int a;
        const int SIZE = 128;
        char line[SIZE];
        line[0] = 0;
        fgets(line, SIZE, fp);
        int num_read = sscanf(line,"%2d%2d%2d %2c %2c %2c %2c %2c %2c %2c %2c",
                              &year, &month, &day,
                              &(block[0][0]),
                              &(block[1][0]),
                              &(block[2][0]),
                              &(block[3][0]),
                              &(block[4][0]),
                              &(block[5][0]),
                              &(block[6][0]),
                              &(block[7][0])
                              );
        if(num_read != 3+INTERVAL) {
            fprintf(stderr, "Error reading data from file '%s' line '%s'\n",
                    filename, line);
            exit(2);
        }
        if((year != file_year%100)
           ||(month != file_month+1)
           ||(day != this_time.tm_mday)) {
            fprintf(stderr, "Error bad date from file '%s' line '%s'\n",
                    filename, line);
            exit(2);
        }
        // Now convert the Kp index to the ap index
        for(int i=0; i < INTERVAL; i++) {
            switch (block[i][0]) {
                case '0':
                    switch (block[i][1]) {
                        case 'o': a=0; break;
                        case '+': a=2; break;
                        default: goto block_error;
                    }; break;
                case '1':
                    switch (block[i][1]) {
                        case '-': a=3; break;
                        case 'o': a=4; break;
                        case '+': a=5; break;
                        default: goto block_error;
                    }; break;
                case '2':
                    switch (block[i][1]) {
                        case '-': a=6; break;
                        case 'o': a=7; break;
                        case '+': a=9; break;
                        default: goto block_error;
                    }; break;
                case '3':
                    switch (block[i][1]) {
                        case '-': a=12; break;
                        case 'o': a=15; break;
                        case '+': a=18; break;
                        default: goto block_error;
                    }; break;
                case '4':
                    switch (block[i][1]) {
                        case '-': a=22; break;
                        case 'o': a=27; break;
                        case '+': a=32; break;
                        default: goto block_error;
                    }; break;
                case '5':
                    switch (block[i][1]) {
                        case '-': a=39; break;
                        case 'o': a=48; break;
                        case '+': a=56; break;
                        default: goto block_error;
                    }; break;
                case '6':
                    switch (block[i][1]) {
                        case '-': a=67; break;
                        case 'o': a=80; break;
                        case '+': a=94; break;
                        default: goto block_error;
                    }; break;
                case '7':
                    switch (block[i][1]) {
                        case '-': a=111; break;
                        case 'o': a=132; break;
                        case '+': a=154; break;
                        default: goto block_error;
                    }; break;
                case '8':
                    switch (block[i][1]) {
                        case '-': a=179; break;
                        case 'o': a=207; break;
                        case '+': a=236; break;
                        default: goto block_error;
                    }; break;
                case '9':
                    switch (block[i][1]) {
                        case '-': a=300; break;
                        case 'o': a=400; break;
                        default: goto block_error;
                    }; break;
                default:
block_error:
                    fprintf(stderr, "Error: unknown Kp index '%2c' in file '%s' line '%s'",
                            (block[i][0]), filename, line);
                    exit(2);
            }
            index[i] = a;
        } // for i over INTERVAL

        // Now get the KP value from the KP string
        double KP_val = 0.0;
        if(sscanf(KP_line+65, "%lf", &KP_val) != 1) {
            fprintf(stderr, "Error reading KP from %s\n", argv[5]);
            exit(2);
        }
        // get the next line from the KP file
        KP_line[65] = KP_line[0] = 0;
        fgets(KP_line, KP_SIZE, fp_kp);
        

        // Now, print out results for the IRI stuff.  I have no idea what the
        // F thing is supposed to be, su just us a 0
        printf("%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%3d%5.1f\n",
               year, month, day,
               index[0],
               index[1],
               index[2],
               index[3],
               index[4],
               index[5],
               index[6],
               index[7],
               KP_val
               );


        // Now, increment the day
        this_time.tm_mday++;
        mktime(&this_time);
        if((this_time.tm_year > end_time.tm_year)
           ||((this_time.tm_year == end_time.tm_year)
              &&(this_time.tm_mon > end_time.tm_mon))) break;
    } // until we don't match
               
    
    
    return 0;
}
