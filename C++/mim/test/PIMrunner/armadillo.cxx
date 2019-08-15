// lofar3.cxx
// test LOFAR ionosphere stuff with PIM
//_HIST  DATE NAME PLACE INFO
//	2005 Sep 17  James M Anderson  --JIVE start






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
#include "ionosphere_fake.h"
#include <iostream>
#include <armadillo>



// set up a namespace area for stuff.
using namespace MIM_PIM;
using namespace std;
using namespace arma;


    



// GLOBALS


// FUNCTIONS



int main(int argc, char* argv[])

    cout <<"testing armadillo"<<endl;
    mat A = randu<mat>(4,5);
    mat B = randu<mat>(4,5);
    cout << A*B.t() << endl;
  
    return 0;
}
    






