#if !defined(__arrayobject_h)
#define __arrayobject_h

#if defined(PY_ARRAY_UNIQUE_SYMBOL) 
#define libnumeric_UNIQUE_SYMBOL PY_ARRAY_UNIQUE_SYMBOL
#define PyArray_API PY_ARRAY_UNIQUE_SYMBOL
#else
#define PyArray_API  libnumeric_API
#endif

#define import_array() import_libnumeric();

/*  Deprecated:  If you're not already using them, not recommended. 
The following macros are not in Numeric.
*/
#define PyArray_Present()        (PyArray_API != NULL)
#define PyArray_isArray(o)       (PyArray_Present() && PyArray_Check(o))

#include "arraybase.h"
#include "libnumeric.h"

#endif
