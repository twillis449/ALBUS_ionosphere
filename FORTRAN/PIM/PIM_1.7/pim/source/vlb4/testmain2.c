#include <stdio.h>
#include <math.h>
#include "pim_asub_c.h"



int main(void)
{
    const double DEG2RAD=M_PI/180.0;
    double STALAT, STALON, STAELEV,OB_AZ, OB_EL,TUT,STEC,SRM;
    integer TYEAR,TYDAY,TMON,TDAY,SOLCODE,RETCODE;

    char path[] = "/jop30_0/anderson/astro/ionosphere/prog/FORTRAN/PIM/PIM_1.7";
    
/*     STALAT = (52.+(15.+57.545/60.)/60.)*DEG2RAD; */
/*     STALON = (31.+(5.+9.391/60.)/60.)*DEG2RAD; */
/*     STAELEV = 0.0; */
/*     OB_AZ = -1.606*DEG2RAD; */
/*     OB_EL = 32.792*DEG2RAD; */
/*     TYEAR = 2004; */
/*     TYDAY = 1; */
/*     TMON = 1; */
/*     TDAY = 1; */
/*     TUT = 15.0; */


    STALAT = 9.203121E-01;
    STALON = 1.157735E-01;
    STAELEV = 0.0;
    OB_AZ = -1.178088E+00;
    OB_EL = 1.242406E+00;
    TYEAR = 2004;
    TYDAY = 137;
    TMON = 5;
    TDAY = 17;
    TUT = 1.783333E+01;
    SOLCODE = 0;
    RETCODE = 0;

    pim_asub_c(
        path,
        STALAT,
        STALON,
        STAELEV,
        OB_AZ,
        OB_EL,
        TYEAR,
        TYDAY,
        TMON,
        TDAY,
        TUT,
        &STEC,
        &SRM,
        &SOLCODE,
        &RETCODE
        );

    printf("%12.6f   %12.6f\n", STEC, SRM);
    return 0;
}

