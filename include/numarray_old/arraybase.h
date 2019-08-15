#if !defined(_arraybase_h)
#define _arraybase_h 1

#if defined(SIZEOF_VOID_P)
#if SIZEOF_VOID_P == 8
#define LP64 1
#else
#define LP64 0
#endif
#else
#define LP64 0
#endif

#if defined(_MSC_VER)
#define HAS_UINT64 _MSC_VER >= 1300
#else
#define HAS_UINT64 1
#endif

#define REFCOUNT(obj) (((PyObject *)(obj))->ob_refcnt)
#define MAX_ELSIZE 16

#define MAXARGS 1024
#define MAXDIM  40
#define SZ_BUF  79

typedef signed char              Bool;
typedef signed char              Int8;
typedef unsigned char            UInt8;
typedef short int                Int16;
typedef unsigned short int       UInt16;
typedef int                      Int32;
#ifndef __MACTYPES__
typedef unsigned int             UInt32;
#endif
typedef float                    Float32;
typedef double                   Float64;

/* From Numeric lapack_litemodule, from f2c */
typedef struct { Float32 r, i; } Complex32;
typedef struct { Float64 r, i; } Complex64;

#if LP64
typedef long int                 Int64;
typedef unsigned long int        UInt64;
typedef Int64                    Long;
#else                  /* 32-bit platforms */
typedef Int32                    Long;
#if defined(_MSC_VER)
typedef __int64                  Int64;
typedef unsigned __int64         UInt64;
#else
typedef long long                Int64;
typedef unsigned long long       UInt64;
#endif
#endif

/* XXXXXX See also numerictypes.py for MaybeLong */
#if 0 
typedef long maybelong;  
#define ML_DEC "%ld"
#define ML_HEX "%016lx"
#else 
typedef int maybelong;
#define ML_DEC "%d"
#define ML_HEX "%08x"
#endif

#define MAX_ALIGN sizeof(double)

typedef enum 
{
  tAny,
  tBool,
  tInt8, 
  tUInt8,
  tInt16, 
  tUInt16,
  tInt32, 
  tUInt32, 
  tInt64, 
  tUInt64,
  tFloat32, 
  tFloat64,
  tComplex32, 
  tComplex64,
  tObject,                   /* placeholder... does nothing */
  tMaxType,  
  tDefault = tFloat64,
#if LP64
  tLong = tInt64
#else
  tLong = tInt32
#endif
} NumarrayType;
#define nNumarrayType (tObject+1)

enum PyArray_TYPES { 
 #if defined(USE_SIGNED_CHAR)
	PyArray_CHAR=tInt8, 
 #else
	PyArray_CHAR=tUInt8,
 #endif
	PyArray_UBYTE=tUInt8, 
	PyArray_SBYTE=tInt8,
	PyArray_SHORT=tInt16,
	PyArray_USHORT=tUInt16,
	PyArray_INT=tInt32, 
	PyArray_UINT=tUInt32,
	PyArray_LONG=tLong,
	PyArray_FLOAT=tFloat32, 
	PyArray_DOUBLE=tFloat64, 
	PyArray_CFLOAT=tComplex32, 
	PyArray_CDOUBLE=tComplex64,
	PyArray_NOTYPE=tAny,
	PyArray_OBJECT=tObject,
	PyArray_NTYPES = tMaxType,
};

typedef enum 
{
	NUM_LITTLE_ENDIAN=0,
	NUM_BIG_ENDIAN = 1
} NumarrayByteOrder;

/* Array flags */
#define CONTIGUOUS       1          /* shape/strides in C order */
#define OWN_DIMENSIONS   2          /* always false */
#define OWN_STRIDES      4          /* always false */
#define OWN_DATA         8          /* always false */
#define SAVESPACE     0x10

#define ALIGNED         0x100       /* roughly: data % itemsize == 0 */
#define NOTSWAPPED      0x200       /* byteorder == sys.byteorder    */
#define WRITABLE        0x400       /* data buffer is writable       */
#define CHECKOVERFLOW   0x800       /* Check overflows on value assignments */
#define UPDATEDICT     0x1000       /* New views update dict from original */
#define FORTRAN_CONTIGUOUS     0x2000  /* shape/strides in fortran order */

#define IS_CARRAY (CONTIGUOUS | ALIGNED | NOTSWAPPED)

/* type bit */
#define SAVESPACEBIT 0  /* From Numeric: 128,  but not needed in numarray */ 

/* ========================================================================= */

/* PyArrayObject is source compatible with Numeric,  but includes 
additional fields required to implement numarray.  This PyArrayObject
also allocates dimensions and strides as part of the struct, rather
than on the heap.

Array type descriptors, "descr", are statically allocated. 
*/
typedef struct s_PyArrayObject PyArrayObject;
typedef struct s_Array_Descr PyArray_Descr;

struct s_PyArrayObject 
{
	PyObject_HEAD
	char *data;
	int nd;
	maybelong *dimensions; 
	maybelong *strides;
	PyObject *base;         /* used in numarray as back-reference to __array_struct__ object */
	PyArray_Descr *descr;
	int flags;

	/* numarray extras */

	maybelong _dimensions[MAXDIM];
	maybelong _strides[MAXDIM];

	PyObject *_data;       /* object must meet buffer API */
	PyObject *_shadows;    /* ill-behaved original array. */
	int      nstrides;     /* elements in strides array */
	long     byteoffset;   /* offset into buffer where array data begins */
	long     bytestride;   /* basic seperation of elements in bytes */
	long     itemsize;     /* length of 1 element in bytes */

	char      byteorder;   /* NUM_BIG_ENDIAN, NUM_LITTLE_ENDIAN */

	char      _unused0;    /* XXX unused */
	char      _unused1;    /* XXX unused */

	Complex64      temp;   /* temporary for gettitem/setitem MACROS */
	char *         wptr;   /* working pointer for getitem/setitem MACROS */
};       


/* PyArray_Descr is source compatible with Numeric,  but implements
only a subset of the Numeric struct so not all Numeric code will compile.

Numarray's PyArray_Descr has also been extended with object oriented
array element get/set function pointers.
*/
typedef PyObject * (*_getfunc)(PyArrayObject *, long offset);
typedef int        (*_setfunc)(PyArrayObject *, long offset, PyObject *);

struct s_Array_Descr {
	int  type_num;  /* PyArray_TYPES */
	int  elsize;    /* bytes for 1 element */
	char type;      /* One of "cb1silfdFD "  Object array not supported. */
	_getfunc _get;
	_setfunc _set;
};

#define PyArray(m)               ((PyArrayObject *)(m))
#define PyArray_SIZE(m)          PyArray_Size((PyObject *) m)
#define PyArray_NBYTES(m)        ((PyArray(m))->descr->elsize * PyArray_SIZE(m))
#define PyArray_ISCONTIGUOUS(m)  (((PyArray(m))->flags & CONTIGUOUS) != 0)
#define PyArray_ISFORTRAN_CONTIGUOUS(m) (((PyArray(m))->flags & FORTRAN_CONTIGUOUS) != 0)
#define PyArray_ISALIGNED(m)     (((PyArray(m))->flags & ALIGNED) != 0)
#define PyArray_ISBYTESWAPPED(m) (!((PyArray(m))->flags & NOTSWAPPED))
#define PyArray_ISWRITABLE(m)    (((PyArray(m))->flags & WRITABLE) != 0)
#define PyArray_ISSPACESAVER(m)  0
#define PyArray_ISCARRAY(m)      (((PyArray(m))->flags & IS_CARRAY) == IS_CARRAY)
#define PyArray_Present()        (PyArray_API != NULL)

#include "arrayif.h"

#endif
