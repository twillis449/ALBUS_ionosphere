// filter.cxx
// functions to filter observations so that only a specified range is left
//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start




// INCLUDES
#include "JMA_math.h"
#include <stdio.h>
#include <stdlib.h>
#include "station_reference.h"
#include "station_latlon.h"
#include "observation.h"
#include "filter.h"

#include "station_maker.h"
#include "station_reference.h"



// set up a namespace area for stuff.
namespace MIM_PIM {




// GLOBALS


// FUNCTIONS

//_TITLE  filter_on_STEC --filter observations on STEC
    Uint32 filter_on_STEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum STEC value to accept (TECU)
                               //    note: for compatibility with Bob's
                               //    PIMVLBI software, this will be at least
                               //    -998.0
        const Real64 max,      // I  the maximum value to allow (TECU)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_STEC                 O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < -998.0) min = -998.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].STEC < min)
           ||(observation[i].STEC > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    


//_TITLE  filter_on_VTEC --filter observations on VTEC
    Uint32 filter_on_VTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum VTEC value to accept (TECU)
                               //    note: for compatibility with Bob's
                               //    PIMVLBI software, this will be at least
                               //    -998.0
        const Real64 max,      // I  the maximum value to allow (TECU)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_VTEC                 O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < -998.0) min = -998.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].VTEC < min)
           ||(observation[i].VTEC > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    


//_TITLE  filter_on_model_VTEC --filter observations on model_VTEC
    Uint32 filter_on_model_VTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum model_VTEC value to accept (TECU)
                               //    note: for compatibility with Bob's
                               //    PIMVLBI software, this will be at least
                               //    -998.0
        const Real64 max,      // I  the maximum value to allow (TECU)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_model_VTEC           O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < -998.0) min = -998.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].model_VTEC < min)
           ||(observation[i].model_VTEC > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    









//_TITLE  filter_on_sigma_STEC --filter observations on sigma_STEC
    Uint32 filter_on_sigma_STEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum STEC value to accept (TECU)
                               //    note: singe negative sigmas are a flag
                               //    for bad thigns, the real minimum will be
                               //    set to 0.0
        const Real64 max,      // I  the maximum value to allow (TECU)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_sigma_STEC           O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < 0.0) min = 0.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].sigma_STEC < min)
           ||(observation[i].sigma_STEC > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    




//_TITLE  filter_on_sigma_VTEC --filter observations on sigma_VTEC
    Uint32 filter_on_sigma_VTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum VTEC value to accept (TECU)
                               //    note: singe negative sigmas are a flag
                               //    for bad thigns, the real minimum will be
                               //    set to 0.0
        const Real64 max,      // I  the maximum value to allow (TECU)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_sigma_VTEC           O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < 0.0) min = 0.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].sigma_VTEC < min)
           ||(observation[i].sigma_VTEC > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    




//_TITLE  filter_on_sigma_model_VTEC --filter observations on sigma_model_VTEC
    Uint32 filter_on_sigma_model_VTEC(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum model_VTEC value to accept (TECU)
                               //    note: singe negative sigmas are a flag
                               //    for bad thigns, the real minimum will be
                               //    set to 0.0
        const Real64 max,      // I  the maximum value to allow (TECU)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_sigma_model_VTEC     O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < 0.0) min = 0.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].sigma_model_VTEC < min)
           ||(observation[i].sigma_model_VTEC > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    











//_TITLE  filter_on_El --filter observations on elevation angle
    Uint32 filter_on_El(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum elevation angle to allow (rad)
                               //    Note: this is had limited to 0.0
        const Real64 max,      // I  the maximum value to allow (rad)
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_El                   O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    // check that min is valid
    if(min < 0.0) min = 0.0;
    
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((observation[i].El < min)
           ||(observation[i].El > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    












//_TITLE  filter_flag_station_bad --filter observations for bad stations
    Uint32 filter_flag_station_bad(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 Bad_Station,
                               // I  the bad station number
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_flag_station_bad        O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if(observation[i].station_number == Bad_Station) use_flag[i] = false;
        else num_valid++;
    }
    return num_valid;
}
    




//_TITLE  filter_flag_source_bad --filter observations for bad sources
    Uint32 filter_flag_source_bad(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        const Uint32 Bad_Source,
                               // I  the bad station number
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_flag_source_bad         O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if(observation[i].object_number == Bad_Source) use_flag[i] = false;
        else num_valid++;
    }
    return num_valid;
}
    






    


//_TITLE  filter_on_pierce_point --filter observations on pierce point
    Uint32 filter_on_pierce_point(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  The minimum distance from the reference
                               //    point to allow, in meters.  If you want
                               //    no lower limit, try -1E10
        const Real64 max,      // I  the maximum value to allow (meters)
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_pierce_point         O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        // Ok, get the distance from the reference point for the pierce point
        LatLon_Cart offset =
            Ref_Point.get_polar_offset(observation[i].pierce_point);
        // convert the angle to a physical size in meters
        const Real64 dist = offset.Lat() * radius_Earth;
        if((dist < min) ||(dist > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    









    


//_TITLE  filter_on_station_dist --filter observations on station distance
    Uint32 filter_on_station_dist(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  The minimum distance from the reference
                               //    point to allow, in meters.  If you want
                               //    no lower limit, try -1E10
        const Real64 max,      // I  the maximum value to allow (meters)
        const Station_Reference& Ref_Point,
                               // I  The reference point
        const Uint32 NUM_STATIONS,
                               // I  the number of stations
        const Station_LatLon* const station,
                               // I  the stations
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_station_dist         O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        // Ok, get the distance from the reference point for the station
        LatLon_Cart offset =
            Ref_Point.get_polar_offset(station[observation[i].station_number]);
        // convert the angle to a physical size in meters
        const Real64 dist = offset.Lat() * radius_Earth;
        if((dist < min) ||(dist > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    







    









    


//_TITLE  filter_on_source_dist --filter observations on source distance
    Uint32 filter_on_source_dist(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  The minimum distance from the reference
                               //    point to allow, in meters.  If you want
                               //    no lower limit, try -1E10
        const Real64 max,      // I  the maximum value to allow (meters)
        const Station_Reference& Ref_Source,
                               // I  The reference point.  Even though this
                               //    is called a "station", it will work just
                               //    fine when constructed from a Cal_Source
                               //    DEc (lat) and RA (lon).
        const Uint32 NUM_SOURCES,
                               // I  the number of ssources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_station_dist         O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 24  James M Anderson  --JIVE start

//_END

{
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        // Ok, get the distance from the reference point for the station
        LatLon_Cart target(source[observation[i].station_number].Dec(),
                           source[observation[i].station_number].RA());
        LatLon_Cart offset = Ref_Source.get_polar_offset(target);
        // convert the angle to a physical size in meters
        const Real64 dist = offset.Lat() * radius_Earth;
        if((dist < min) ||(dist > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    








//_TITLE  filter_on_Flux_Density --filter observations on flux density
    Uint32 filter_on_Flux_Density(
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
        Real64 min,            // I  the minimum flux density to accept (Jy)
        const Real64 max,      // I  the maximum value to allow (Jy)
        const Uint32 NUM_SOURCES,//I the number of sources
        const Cal_Source* const source,
                               // I  the sources
        const Uint32 NUM_OBSERVATIONS,
                               // I  The total number of observations
                               //    NUM_SOURCES*NUM_STATIONS
        const Observation_Ionosphere* const observation,
                               // I  The observations.
        bool* const use_flag   // B  The use-flag array.  This function flags
                               //    values outside min and max to false.  This
                               //    must be valid before entering this function
        )
// filter_on_Flux_Density         O  the number of valid points left
        

//_USER  any user input?

//_VARS  TYPE           VARIABLE I/O DESCRIPTION
//       put globals used here

//_DESC  full description of program
//	This function will flag observations.

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO
//	2005 Aug 23  James M Anderson  --JIVE start

//_END

{
    Uint32 num_valid = 0;

    for(Uint32 i=0; i < NUM_OBSERVATIONS; i++) {
        if(!use_flag[i]) continue;
        if((source[observation[i].object_number].FluxDensity() < min)
           ||(source[observation[i].object_number].FluxDensity() > max)) {
            use_flag[i] = false;
        }
        else num_valid++;
    }
    return num_valid;
}
    








    



    


}  // end namespace


