// test.cxx



#include "JMA_code.h"
#include <stdio.h>
#include <stdlib.h>


#include "vex_iono_processor.h"
#include "vex_time.h"


using namespace JMA_VEX_AREA;



int main(void)
{
    process_vex_file("n05l2.vix", "junk", 300.0, 30.0, 30.0);

//     for(Sint32 i=-10;i < 400; i++) {
//         VEX_Time a(2005,i,0,0,0.0);
//         Sint32 month = a.Month();
//         Sint32 day = a.Day();
//         VEX_Time b(2005,month,day,0,0,0.0);
//         printf("%3d %3d %3d %3d\n", i, b.Yday(), month, day);
//     }
//     VEX_Time a(2005,1,0,0,0.0);
//     VEX_Time b(a.JD0_C(), a.JD1_C());
//     VEX_Time c(a.JD0_C(), a.JD1_C()+0.49);
//     VEX_Time d(a.JD0_C(), a.JD1_C()+0.51);
//     VEX_Time e(a.JD0_C(), a.JD1_C()+0.99);

//     printf("%f %f\n", a.JD0_C(), a.JD1_C());
//     printf("%d %d %d %d\n", b.Year(), b.Month(), b.Day(), b.Hour());
//     printf("%d %d %d %d\n", c.Year(), c.Month(), c.Day(), c.Hour());
//     printf("%d %d %d %d\n", d.Year(), d.Month(), d.Day(), d.Hour());
//     printf("%d %d %d %d\n", e.Year(), e.Month(), e.Day(), e.Hour());
    
    return 0;
}
