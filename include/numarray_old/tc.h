#if defined(MEASURE_TIMING)

/* ----------------------------------------------------------------------- */

typedef struct 
{
	PyObject_HEAD
	unsigned long   start_count;       /* Total count of starts */
	unsigned long   start;             /* Last start time in usec */
	unsigned long   stop;              /* Last stop time in usec */
	double          accum;             /* Accumulated timing in usec for all start/stop cycles */
	unsigned long   entries;           /* Incrementing/Decrementing count of starts */
	unsigned long   starts_on_entry;   /* global start count at top level entry */
	unsigned long   cycles;            /* Last count of nested teacup cycles (overhead) */
} PyTeaCupObject;

/* ----------------------------------------------------------------------- */

#include "libteacup.h"

/* ----------------------------------------------------------------------- */

#define tc_start_clock(x) { static void *cache; cache = _teacup_start_clock(__FILE__, x, cache); }
#define tc_stop_clock(x)  { static void *cache; cache = _teacup_stop_clock(__FILE__, x, cache); }
#define import_libtc()  import_libteacup()

/* ----------------------------------------------------------------------- */

#else 

#define tc_start_clock(x)
#define tc_stop_clock(x)
#define import_libtc()

#endif

