// JMA_math.h
// add in stuff for good math stuff under GCC
// 2000 Sep 17  James M Anderson  --NMT  Original Version
// 2005 Sep 20  JMA  --update some \pi related constants to more precision
// 2007 Dec 03  JMA  --MPIfR update for complex

#ifndef JMA_MATH_H
#define JMA_MATH_H


// make sure we have some type things
#include "JMA_code.h"



// are we usign GCC ?
// if not, barf
#ifndef __GNUC__
# 	error "GCC required for compilation"
#elif (__GNUC__ < 2)
#	error "at least GCC 2 required for compilation"
#endif

// we want to use ISO C9X stuff
// we want to use some GNU stuff
// But this sometimes breaks time.h
#ifndef _GNU_SOURCE
#  define _GNU_SOURCE 1
#endif
#include <time.h>
#ifndef __USE_ISOC99
#  define __USE_ISOC99 1
#endif
#ifndef __USE_MISC
#  define __USE_MISC 1
#endif





// get the math functions
#ifdef __cplusplus
#  include <complex>
#  include <cmath>
typedef std::complex<float>         Complex32;
typedef std::complex<double>        Complex64;
typedef std::complex<long double>   Complex80;
#else
#  include <complex.h>
#  include <math.h>
typedef _Complex float              Complex32;
typedef _Complex double             Complex64;
typedef _Complex long double        Complex80;
#endif







// put anything else intersting here

// For some reason, GCC 2.95.2 has a broken restrict.
// Also, for C++ use until restrict becomes common, use:
#ifndef restrict
#	define restrict __restrict__
#endif






// Radian to Degree conversions
// #define M_RAD2DEG 57.29577951308232087665
// #define M_DEG2RAD  0.0174532925199432976913
#define M_RAD2DEG 57.29577951308232087679815
#define M_DEG2RAD  0.01745329251994329576923691  






#endif // JMA_MATH_H

