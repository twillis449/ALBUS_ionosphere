// ell_coord_converter.cxx
// elliptical coordinate conversion, to and from Cartesian coordinates
// 2006 Jul 27  James M Anderson  --JIVE  start

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "JMA_code.h"
#include "JMA_math.h"

#include "ellipsoidal_coord.h"
using namespace JMA_VEX_AREA;



//_TITLE  name one line description
Sint32 check_args(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    const int argc,            // I  number of arguments
    const char* const argv[],  // I  argument list
    Ellipsoid_Type& ell_type,  // O  the type of ellipsoid to use
    Sint8& to_Cartesian,       // O  Flag for whether to go *to* Cartesian
                               //    coordinates (non-zero) or *from*
                               //    Cartesian coordinates (0)
    Sint8& sexagesimal         //    flag to indicate whether to read/write
                               //    from/to sexagesimal coordinates
                               //    0 no, else yes
    )
// Sint32 check_args              O  argument number which is the start of the
//                                   position information

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
//	2006 Jul 27  James M Anderson  --JIVE  start

//_END

{
    // set defaults
    ell_type = GRS80;
    to_Cartesian = 1;
    sexagesimal = -1;
    Sint32 arg;
    for(arg = 1; arg < argc; arg++) {
        // check if argument
        if(argv[arg][0] != '-') break;
        // check for number
        if((isdigit(argv[arg][1])) || (argv[arg][1] == '.')) break;
        // now check for recognized options
        for(Uint32 i=1; i < strlen(argv[arg]); i++) {
            char c = argv[arg][i];
            switch (c) {
                case 'G': ell_type = GRS80; break;
                case 'W': ell_type = WGS84; break;
                case 's': sexagesimal = 1; break;
                case 'd': sexagesimal = 0; break;
                case 'E': to_Cartesian = 0; break;
                case 'C': to_Cartesian = 1; break;
                default:
                    goto arg_error;
            }
        }
    }
    // Now, check that our remaining numbr of arguments makes sense
    if((argc - arg) == 3) {
        // three arguments, input not sexagesimal
        if((to_Cartesian)) {
            // our sexagesimal flag had better be no
            if(sexagesimal == -1) sexagesimal = 0;
            if(sexagesimal == 0) return arg;
        }
        else {
            // to elliptical, any sexagesimal ok
            return arg;
        }
    }
    else if((argc - arg) == 7) {
        // 7 arguments, so we have to be going to Cartesian
        if((to_Cartesian)) {
            if(sexagesimal == -1) sexagesimal = 1;
            if(sexagesimal == 1) return arg;
        }
    }
arg_error:
    fprintf(stderr, "Correct usage is: %s [options] A B C\n", argv[0]);
    fprintf(stderr, "or                %s [options] A1 A2 A3 B1 B2 B3 C\n", argv[0]);
    fprintf(stderr, "\n\n\n");
    fprintf(stderr, "where A, B, and C default to A = longitude (degrees) (+ for North)\n");
    fprintf(stderr, "                             B = latitude (degrees) (+ for East)\n");
    fprintf(stderr, "                             C = height above ellipsiod (m).\n");
    fprintf(stderr, "The longitude and latitude may also be given in sexagesimal notation\n");
    fprintf(stderr, "of degrees, minutes, and seconds.\n");
    fprintf(stderr, "\n\nOptions:\n");
    fprintf(stderr, "    -C  convert to Cartesian (the default)\n");
    fprintf(stderr, "    -E  convert to elliptical (A B C are X Y Z in m)\n");
    fprintf(stderr, "    -G  use GRS80 ellipsoid\n");
    fprintf(stderr, "    -W  use WGS84 ellipsoid\n");
    fprintf(stderr, "    -d  output in digital notation (default)\n");
    fprintf(stderr, "    -s  output in sexagesimal notation (for elliptical coordinates)\n");
    fprintf(stderr, "\n");
    exit(2);
    return -2;
}




Real64 get_angle_from_input(const Sint32 arg_start, const char* const argv[],
                            const Sint8 sexagesimal)
{
    Real64 angle = 0.0;
    if((sexagesimal)) {
        Real64 sign = 1.0;
        Real64 d,m,s;
        if(argv[arg_start][0] == '-') {
            sign = -1.0;
            d = atof(argv[arg_start]+1);
        }
        else {
            d = atof(argv[arg_start]);
        }
        m = atof(argv[arg_start+1]);
        s = atof(argv[arg_start+2]);
        angle = ((((s) / 60.0 + m) / 60.0) + d) * sign;
    }
    else {
        angle = atof(argv[arg_start]);
    }
    angle *= M_DEG2RAD;
    return angle;
}



void print_angle(Real64 angle, const Sint8 sexagesimal)
{
    angle *= M_RAD2DEG;
    if((sexagesimal)) {
        char sign = (angle < 0.0) ? '-' : '+';
        angle = fabs(angle);
        Real64 d = trunc(angle);
        angle = (angle - d) * 60.0;
        Real64 m = trunc(angle);
        angle = (angle - m) * 60.0;
        Real64 s = angle;
        printf("%c%03.0f %02.0f %010.7f", sign, d, m, s);
    }
    else {
        printf("%+017.12f", angle);
    }
    return;
}

void print_dist(const Real64 s)
{
    printf("%15.5f", s);
    return;
}






int main(int argc, char* argv[])
{
    Ellipsoid_Type ell_type;
    Sint8 to_Cartesian;
    Sint8 sexagesimal;
    Sint32 arg_start =  check_args(argc, argv, ell_type, to_Cartesian, sexagesimal);

    if((to_Cartesian)) {
        Real64 lon = get_angle_from_input(arg_start, argv, sexagesimal);
        arg_start += ((sexagesimal)) ? 3:1;
        Real64 lat = get_angle_from_input(arg_start, argv, sexagesimal);
        arg_start += ((sexagesimal)) ? 3:1;
        Real64 h = atof(argv[arg_start]);

        ellipsoidal_to_Cartesian_coord e(lat, lon, h, ell_type);

        print_dist(e.X()); fputc(' ', stdout);
        print_dist(e.Y()); fputc(' ', stdout);
        print_dist(e.Z()); fputc('\n', stdout);
    }
    else {
        Real64 x = atof(argv[arg_start]);
        Real64 y = atof(argv[arg_start+1]);
        Real64 z = atof(argv[arg_start+2]);

        ellipsoidal_coord c(x,y,z, ell_type);

        print_angle(c.get_lambda(), sexagesimal); fputc(' ', stdout);
        print_angle(c.get_phi(), sexagesimal); fputc(' ', stdout);
        print_dist(c.get_height()); fputc('\n', stdout);
    }
    return 0;
}
