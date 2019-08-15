#if 0

Python Inclusion Plan
There is a PEP that has been started for including the array interface with an associated generic array object into Python. Anybody who would like to help with the PEP is strongly encouraged. The location of the PEP draft is http://svn.scipy.org/svn/PEP (use an svn client to check it out).
Description
The homogeneous N-dimensional array interface is a default mechanism for objects to share N-dimensional array memory and information.  The interface consists of a set of attributes that an array-like object can support.   Objects wishing to be considered an N-dimensional array in application code should support (at least some of) these attributes.  Objects wishing to support an N-dimensional array in application code should look for (at least some of) these attributes and use the information provided appropriately.

This interface describes homogeneous arrays in the sense that each item of the array has the same "type".  This type can be very simple or it can be a quite arbitrary and complicated C-like structure.
Array Interface
There are two ways to use the interface.  Either with multiple Python attributes or with one attribute and C-based struct access.   The first method can be considered a Python-only interface as only attribute-access is needed to access the interface.
All attribute access 
This approach to the interface consists of 2 required and 5 optional attributes using special method names.  The optional attributes have implied defaults if they are not provided.

__array_shape__ (required)

    Tuple whose elements are the array size in each dimension. Each entry is an integer (a Python int or long).  Note that these integers could be larger than the platform "int" or "long" could hold (a Python int is a C long). It is up to the code using this attribute to handle this appropiately; either by raising an error when overflow is possible, or by using Py_LONG_LONG as the C type for the shapes.

__array_typestr__ (required)
    A string providing the basic type of the homogenous array  The basic string format consists of 3 parts: a character describing the byteorder of the data ('<': little-endian, '>': big-endian, '|':not-relevant), a character code giving the basic type of the array, and an integer providing the number of bytes the type uses.  The basic type character codes are:

        * 't' -- Bit field (following integer gives the number of bits in the bit field).
        * 'b' -- Boolean (integer type where all values are only True or False)
        * 'i' -- Integer
        * 'u' -- Unsigned integer
        * 'f' -- Floating point
        * 'c' -- Complex floating point
        * 'O' -- Object (i.e. the memory contains a pointer to PyObject)
        * 'S' -- String (fixed-length sequence of char)
        * 'U' -- Unicode (fixed-length sequence of Py_UNICODE)
        * 'V' -- Other (void * -- each item is a fixed-size chunk of memory)

    Thus, an array of unsigned integers using two bytes as big-endian would be described as '>u2'.   More complicated memory layouts can be described using the __array_descr__ attribute and using the 'V' type here.  A regular expression for this string is '[<|>][tbiufcOSUV][0-9]+'.  Notice that the byteorder code must be present. 


__array_descr__ (optional)
    A list of tuples providing a more detailed description of the memory layout for each item in the homogeneous array.  Each tuple in the list has two or three elements.  Normally, this attribute would be used when __array_typestr__ is 'V[0-9]+', but this is not a requirement.  The only requirement is that the number of bytes represented in __array_typestr__ is the same as the total number of bytes represented here.   The idea is to support descriptions of C-like structs (records) that make up array elements.  The elements of each tuple in the list are 

       1. A string providing a name associated with this portion of the record.
       2. Either a basic-type description string as in __array_typestr__ or another list (for nested records)
       3. An optional shape tuple providing how many times this part of the record should be repeated.  Assumed (1,) if not given.

      Very complicated structures can be described using this generic interface.  Notice, however, that each element of the array must still be the same type.   Some examples of using this interface are given below.
    Default: [('',__array_typestr__)] 

__array_data__ (optional)
    A 2-tuple whose first argument is a hexadecimal string providing a pointer to the data area storing the array contents. This pointer must point to the first element of data (in other words any __array offset__ is ignored in this case). The second entry in the tuple is a read-only flag (true means the data area is read-only).
    This attribute can also be an object exposing the buffer interface which will be used to share the data. If this attribute is not present (or returns None), then memory sharing will be done through the buffer interface of the object.  In this case, the __array_offset__ can be used to point to add to the start of the buffer. A reference to the object with this attribute must be stored by the new object if the memory area is to be secured.
    Default: None

__array_strides__ (optional)
    Either None to indicate a C-style contiguous array or a Tuple of strides which provides the number of bytes needed to jump to the next array element in the corresponding dimension. Each entry must be an integer (a Python int or long). As with __array_shape__, the values may be larger than can be represented by a C "int" or "long"; the calling code should handle this appropiately, either by raising an error, or by using Py_LONG_LONG in C. The default is None which implies a C-style contiguous memory buffer.  In this model, the last dimension of the array varies the fastest.  For example, the default __array_strides__ tuple for an object whose array entries are 8 bytes long and whose __array_shape__ is (10,20,30) would be (4800, 240, 8)
    Default: None (C-style contiguous)

__array_mask__ (optional)
    None or an object exposing the array interface.  All elements of the mask array should be interpreted only as true or not true indicating which elements of this array are valid.   The shape of this object should be "broadcastable" to the shape of the original array.   "Broadcastable" means that either 1) it is the same shape, 2) it is the same shape except for possibly a 1 in some of the shape-tuple elements, or 3) it can be made to satisfy (2) by pre-pending 1''s to the shape.  
    Default: None (All array values are valid)

__array_offset__ (optional)
    An integer offset into the array data region. This can only be used when __array_data__ is None or returns a buffer object.
    Default: 0.

One attribute and C-Struct Access
This approach to the array interface allows for faster access to an array using only one attribute lookup and a well-defined C-structure.

__array_struct__
A PyCObject that contains a pointer to a filled PyArrayInterface structure.  Memory for the structure is dynamically created and the PyCObject is also created with an appropriate destructor so the retriever of this attribute simply has to apply Py_DECREF() to the object returned by this attribute when it is finished.  Also, either the data needs to be copied out, or a reference to the object exposing this attribute must be held to ensure the data is not freed.  Objects exposing the __array_struct__ interface must also not reallocate their memory if other objects are referencing them.   Notice that __array_descr__ can still be used to describe a more complicated structure if needed.

typedef struct {
    int version;          /* contains the integer 2 as a sanity check */
    int nd;               /* number of dimensions */
    char typekind;        /* kind in array --- character code of typestr */
    int itemsize;         /* size of each element */
    int flags;            /* flags indicating how the data should be interpreted */
    Py_intptr_t *shape;   /* A length-nd array of shape information */
    Py_intptr_t *strides; /* A length-nd array of stride information */
    void *data;           /* A pointer to the first element of the array */
} PyArrayInterface;

The flags attribute may consist of 5 bits showing how the data should be interpreted: CONTIGUOUS  (1), FORTRAN (2), ALIGNED (0x100), NOTSWAPPED (0x200), and WRITEABLE (0x400).    The ALIGNED, CONTIGUOUS, and FORTRAN flags can actually be determined from the other provided information, but the NOTSWAPPED and WRITEABLE flags are critical.
Type description examples
For clarity it is useful to provide some examples of the type description and corresponding __array_descr__ attributes.   Thanks to Scott Gilbert for these examples:

In every case, the __array_descr__ attribute is optional, but of course provides more information which may be important for various applications.

     * Float data
         __array_typestr__ == '>f4'
         __array_descr__ == [('','>f4')]

     * Complex double
         __array_typestr__ == '>c8'
         __array_descr__ == [('real','>f4'), ('imag','>f4')]

     * RGB Pixel data
         __array_typestr__ == '|V3'
         __array_descr__ == [('r','|u1'), ('g','|u1'), ('b','|u1')]

     * Mixed endian (weird but could happen).
         __array_typestr__ == '|V8' (or '>u8')
         __array_descr__ == [('big','>i4'), ('little','<i4')]

     * Nested structure
         struct {
             int ival;
             struct {
                 unsigned short sval;
                 unsigned char bval;
                 unsigned char cval;
             } sub;
         }
         __array_typestr__ == '|V8' (or '<u8' if you want)
         __array_descr__ == [('ival','<i4'), ('sub', [('sval','<u2'), ('bval','|u1'), ('cval','|u1') ]) ]

     * Nested array
         struct {
             int ival;
             double data[16][4];
         }
         __array_typestr__ == '|V516'
         __array_descr__ == [('ival','>i4'), ('data','>f8',(16,4))]

     * Padded structure
         struct {
             int ival;
             double dval;
         }
         __array_typestr__ == '|V16'
         __array_descr__ == [('ival','>i4'),('','|V4'),('dval','>f8')]

It should be clear that any record type could be described using this interface.
Last updated November 1, 2005  by Travis E. Oliphant

#endif

#if !defined(_arraif_)
#define _arrayif_ 1


typedef struct {
    int version;          /* contains the integer 2 as a sanity check */
    int nd;               /* number of dimensions */
    char typekind;        /* kind in array --- character code of typestr */
    int itemsize;         /* size of each element */
    int flags;            /* flags indicating how the data should be interpreted */
    Py_intptr_t *shape;   /* A length-nd array of shape information */
    Py_intptr_t *strides; /* A length-nd array of stride information */
    void *data;           /* A pointer to the first element of the array */
} PyArrayInterface;


#if !defined(CONTIGUOUS)
#define CONTIGUOUS       1          
#endif

#if !defined(FORTRAN)
#define FORTRAN          2          
#endif

#if !defined(ALIGNED)
#define ALIGNED         0x100       
#endif

#if !defined(NOTSWAPPED)
#define NOTSWAPPED      0x200       
#endif

#if !defined(WRITABLE)
#define WRITABLE        0x400       
#endif

#endif  /* _arrayif_ */

