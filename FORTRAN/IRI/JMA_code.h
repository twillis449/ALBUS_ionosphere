// JMA_code.h 
// JMA's generic programming stuff
// 2000 Sep 17  James M Anderson  --NMT  Original Version
// 2007 Jan 08  JMA  --define __STDC_LIMIT_MACROS


#ifndef JMA_CODE_H
#define JMA_CODE_H



// MACRO TO MAKE UNIX F77 ROUTINES AVAILABLE
#define FTN_NAME(a)                               a##_



// JMA typedefs for standard type names of things
#ifdef __cplusplus
#  include <cstddef>
#else
#  include <stddef.h>
#endif
#define __STDC_LIMIT_MACROS
#include <stdint.h>
// Exact types
typedef int8_t                      Sint8;
typedef uint8_t                     Uint8;
typedef int16_t                     Sint16;
typedef uint16_t                    Uint16;
typedef int32_t                     Sint32;
typedef uint32_t                    Uint32;
typedef int64_t                     Sint64;
typedef uint64_t                    Uint64;

typedef float                       Real32;
typedef double                      Real64;
typedef long double                 Real80;

// non exact types

typedef ptrdiff_t                   Index_t;

// FORTRAN INTERFACE
// integer should be the size of a real, which is probably 32 bits
typedef int32_t                     Finteger;






#endif // JMA_CODE_H

