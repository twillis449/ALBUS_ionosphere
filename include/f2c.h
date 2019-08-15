/* f2c.h  --  Standard Fortran to C header file */

/**  barf  [ba:rf]  2.  "He suggested using FORTRAN, and everybody barfed."

	- From The Shogakukan DICTIONARY OF NEW ENGLISH (Second edition) */

/* 2008 Feb 18  James M Anderson  --MPIfR  --edit the standard f2c.h header
                                    to select just what is needed for basic
                                    FORTRAN interaction in C, as the standard
                                    f2c.h is incompatible with C++ <vector> */

#ifndef F2C_INCLUDE
#define F2C_INCLUDE

typedef int integer;
typedef unsigned int uinteger;
typedef char *address;
typedef short int shortint;
typedef float real;
typedef double doublereal;
typedef struct { real r, i; } complex;
typedef struct { doublereal r, i; } doublecomplex;
typedef int logical;
typedef short int shortlogical;
typedef char logical1;
typedef char integer1;
#ifdef INTEGER_STAR_8	/* Adjust for integer*8. */
typedef long long longint;		/* system-dependent */
typedef unsigned long long ulongint;	/* system-dependent */
#endif

/* Extern is for use with -E */
#ifndef Extern
#define Extern extern
#endif

/* procedure parameter types for -A and -C++ */



#endif /* F2C_INCLUDE */
