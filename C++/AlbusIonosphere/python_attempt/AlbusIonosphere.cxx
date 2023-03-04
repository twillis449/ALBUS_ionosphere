// AlbusIonosphere.cxx
// A file for interaction with Python for the ALbus Ionosphere software
// 2006 Jan 04  James M Anderson  --JIVE  start


// Python.h needs to be first.  Then I can include the system stuff
#include <Python.h>
#include <numpy/arrayobject.h>
#include <numpy/noprefix.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>

// some functions for debugging
void printIntArray(char label[], int *ptr, size_t start_length, size_t length)
  {         
    //for statment to print values using array             
    printf("%s ", label);        
    size_t i = 0;
    for( ; i < start_length; ++i )      
      printf(" %d", ptr[i]);        
    printf(" ... ");        
    for(i=length-start_length ; i < length; ++i )      
      printf(" %d", ptr[i]);        
    printf("\n");        
  }   

void printSIntArray(char label[], short int *ptr, size_t start_length, size_t length)
  {         
    //for statment to print values using array             
    printf("%s ", label);        
    size_t i = 0;
    for( ; i < start_length; ++i )      
      printf(" %d", ptr[i]);        
    printf(" ... ");        
    for(i=length-start_length ; i < length; ++i )      
      printf(" %d", ptr[i]);        
    printf("\n");        
  }   

void printArray(char label[], double *ptr, size_t start_length, size_t length)
  {         
    //for statment to print values using array             
    printf("%s ", label);        
    size_t i = 0;
    for( ; i < start_length; ++i )      
      printf(" %f", ptr[i]);        
    printf(" ... ");        
    for(i=length-start_length ; i < length; ++i )      
      printf(" %f", ptr[i]);        
    printf("\n");        
  }   

// I am trying to keep everything as simple as possible for the Python
// interface, so the headers and classes use only the standard C types.
// In order to get maximum simplicity, put everything here in the global
// namespace.
#include "data_calibrator.h"

#include "sofa.h"


// GLOBALS

// I need a Data_Calibrator object to hang around.  This will be initialized
// in the init function call.

Data_Calibrator* D_C = NULL;



// Declare the interface functions



PyObject* clear_everything(PyObject* self, PyObject* args)
{
    // No arguments, so just ignore any
    // Now call my stuff
    D_C->clear_everything();
    return Py_BuildValue("i", 0);
}
static char clear_everything_docs[] = "free all memory for Ionosphere stuff\n";
   


PyObject* set_ionosphere_IRI(PyObject* self, PyObject* args)
{
    double electron_precision_in;
    double Faraday_precision_in;
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "dd", &electron_precision_in, &Faraday_precision_in);
    // Now call my stuff
    int retval = D_C->set_ionosphere_IRI(electron_precision_in,
                                         Faraday_precision_in);
    return Py_BuildValue("i", retval);
}
static char set_ionosphere_IRI_docs[] = "use an IRI ionosphere\n";

PyObject* set_ionosphere_PIM(PyObject* self, PyObject* args)
{
    double electron_precision_in;
    double Faraday_precision_in;
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "dd", &electron_precision_in, &Faraday_precision_in);
    // Now call my stuff
    int retval = D_C->set_ionosphere_PIM(electron_precision_in,
                                         Faraday_precision_in);
    return Py_BuildValue("i", retval);
}
static char set_ionosphere_PIM_docs[] = "use a PIM ionosphere\n";


PyObject* set_ionosphere_GPS(PyObject* self, PyObject* args)
{
    double electron_precision_in;
    double Faraday_precision_in;
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "dd", &electron_precision_in, &Faraday_precision_in);
    // Now call my stuff
    int retval = D_C->set_ionosphere_GPS(electron_precision_in,
                                         Faraday_precision_in);
    return Py_BuildValue("i", retval);
}
static char set_ionosphere_GPS_docs[] = "use a GPS/calibrator ionosphere\n";


PyObject* set_reference_time(PyObject* self, PyObject* args)
{
    int year;                  // I  YYYY
    int month;                 // I  MM in range 1--12
    int day;                   // I  DD in range 1--31
    int hour;                  // I  HH in range 0--23
    int minute;                // I  MM in range 0--59
    double second;             // I  SS.ss in range 0--61 (for leap seconds)
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "iiiiid", &year, &month, &day,
                     &hour, &minute, &second);
    // Now call my stuff
    int retval = D_C->set_reference_time(year, month, day, hour, minute, second);
    return Py_BuildValue("i", retval);
}
static char set_reference_time_docs[] = "set up the UTC reference time\n";


PyObject* set_reference_time_AIPS(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int year;                  // I  YYYY
    int month;                 // I  MM in range 1--12
    int day;                   // I  DD in range 1--31
    int hour;                  // I  HH in range 0--23
    int minute;                // I  MM in range 0--59
    double second;             // I  SS.ss in range 0--61 (for leap seconds)
    double polar_x;            // I  the polar X position in meters on the
                               //    reference date.
    double polar_y;            // I  the polar Y position in meters on the
                               //    reference date.
    double ut1_utc;            // I  the UT1 - UTC time difference in s on the
                               //    reference date.
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "iiiiidddd", &year, &month, &day,
                     &hour, &minute, &second, &polar_x, &polar_y, &ut1_utc);
    // Now call my stuff
    int retval = D_C->set_reference_time_AIPS(year, month, day,
                                              hour, minute, second,
                                              polar_x, polar_y, ut1_utc);
    return Py_BuildValue("i", retval);
}
static char set_reference_time_AIPS_docs[] = "set up the UTC reference time under AIPS\n";

    
PyObject* set_station_position(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double x;                  // I station position from center of Earth
                               //   in x direction in meters
    double y;                  // I station position from center of Earth
                               //   in y direction in meters
    double z;                  // I station position from center of Earth
                               //   in z direction in meters
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "ddd", &x, &y, &z);
    // Now call my stuff
    int retval = D_C->set_station_position(x, y, z);
    return Py_BuildValue("i", retval);
}
static char set_station_position_docs[] = "set the station location\n";


PyObject* set_source_position(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double RA;                 // I  J2000 RA position in radians
    double Dec;                // I  J2000 Dec position in radians
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "dd", &RA, &Dec);
    // Now call my stuff
    int retval = D_C->set_source_position(RA, Dec);
    return Py_BuildValue("i", retval);
}
static char set_source_position_docs[] = "set the source position\n";

PyObject* set_time_step(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double delta_time;  // I  time step for predition increment, in seconds
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "d", &delta_time);
    // Now call my stuff
    int retval = D_C->set_time_step(delta_time);
    return Py_BuildValue("i", retval);
}
static char set_time_step_docs[] = "set the prediction time step\n";


PyObject* set_scan_times(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double start_time;         // I  the start of the scan; in seconds from the
                               //    reference date
    double end_time;           // I  the end of the scan; in seconds from the
                               //    reference date
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "dd", &start_time, &end_time);
    // Now call my stuff
    int retval = D_C->set_scan_times(start_time, end_time);
    return Py_BuildValue("i", retval);
}
static char set_scan_times_docs[] = "set the scan start and end times for ionospheric calcs\n";

    
PyObject* get_Num_Ionospheric_Predictions(PyObject* self)
{
    return Py_BuildValue("i", D_C->get_Num_Ionospheric_Predictions());
}
static char get_Num_Ionospheric_Predictions_docs[] = "ask for the number of total scan predictions\n";
    


PyObject* get_ionospheric_prediction(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int Prediction_Number;     // I  The sequence number of the predictions.
                               //    Must be in the range
                               //    0--(Num_Ionospheric_Predictions-1)
    double  pred_time;         // O  The time difference; in seconds; of this
                               //    prediction; from the reference time
    double  width_time;        // O  The full width over which the prediction
                               //    is intended.  For now, this is just
                               //    Ionospheric_Prediction_Delta_Time
    double  El;                // O  The Elevation angle; in radians; of the
                               //    source at the pred_time
    double  Az;                // O  The Azimuth angle; in radians; of the
                               //    source at the pred_time
    double  STEC;              // O  the prediction STEC value; in units of
                               //    m^{-2}
    double  SRM;               // O  The predicted SRM value; in units of
                               //    T m^{-2}
    double  VTEC_factor;       // O  The approximate conversion factor to get
                               //    Vertical TEC from Slant TEC.  unitless.
                               //    Multiply STEC by VTEC_factor to get VTEC
    double  STEC_ERR;          // O  the prediction STEC error value; in units of
                               //    m^{-2}
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "i", &Prediction_Number);
    // Now call my stuff
    int retval = D_C->get_ionospheric_prediction(Prediction_Number,
                                                 pred_time, width_time,
                                                 El, Az, STEC, SRM, 
                                                 VTEC_factor,STEC_ERR);
    return Py_BuildValue("idddddddd", retval, pred_time, width_time,
                         El, Az, STEC, SRM, VTEC_factor, STEC_ERR);
}
static char get_ionospheric_prediction_docs[] = "get a prediction for STEC and SRM\n";



PyObject* get_source_AzEl(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double pred_time;          // I  The time difference; in seconds; of this
                               //    prediction; from the reference time
    double  El;                // O  The Elevation angle; in radians; of the
                               //    source at the pred_time
    double  Az;                // O  The Azimuth angle; in radians; of the
                               //    source at the pred_time
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "d", &pred_time);
    // Now call my stuff
    int retval = D_C->get_source_AzEl(pred_time, El, Az);
    return Py_BuildValue("idd", retval, El, Az);
}
static char get_source_AzEl_docs[] = "get the Elevation and Azimuth of a source at a time\n";


PyObject* get_source_AzElVTEC(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    double pred_time;          // I  The time difference; in seconds; of this
                               //    prediction; from the reference time
    double h_iono;             // I  The height of the ionosphere 2-D layer to
                               //    calculate the VTEC_factor, in m.
    double  El;                // O  The Elevation angle; in radians; of the
                               //    source at the pred_time
    double  Az;                // O  The Azimuth angle; in radians; of the
                               //    source at the pred_time
    double  VTEC_factor;       // O  The approximate conversion factor to get
                               //    Vertical TEC from Slant TEC.  unitless.
                               //    Multiply STEC by VTEC_factor to get VTEC
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "dd", &pred_time, &h_iono);
    // Now call my stuff
    int retval = D_C->get_source_AzElVTEC(pred_time, h_iono, El, Az,
                                          VTEC_factor);
    return Py_BuildValue("iddd", retval, El, Az, VTEC_factor);
}
static char get_source_AzElVTEC_docs[] = "get the El and Az and VTEC factor for the source\n";


PyObject* cal_observations_init(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int Num_Cal_Stations;      // I  The number of stations expected to
                               //    be used.  Making this number larger
                               //    than necessary does not eat up too much
                               //    space (only memory for a pointer array)

    int retval;
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "i", &Num_Cal_Stations);
    if((Num_Cal_Stations <= 0)) {
        PyErr_Format(PyExc_RuntimeError, "Nonpositive array sizes!");
        goto fail;
    }
    // Now call my stuff
    retval = D_C->cal_observations_init(unsigned(Num_Cal_Stations));
    return Py_BuildValue("i", retval);
fail:
    return NULL;
}
static char cal_observations_init_docs[] = "initialize the GPS/calibrator data area\n";


PyObject* cal_observations_init2(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int retval;
    // Now call my stuff
    retval = D_C->cal_observations_init2();
    return Py_BuildValue("i", retval);
}
static char cal_observations_init2_docs[] = "initialize the GPS/calibrator data area\n";


PyObject* cal_observations_set_parameters(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int Default_Fit_Type_;             // I  The fit type to calculate.
                                       //    0 Single nearest calibrator
                                       //    1 single calibration station average
                                       //    2 2-D MIM
                                       //    3 3-D MIM multiple station
                                       //    4 3-D MIM many station
    double Max_Sat_Sky_Angle_;         // I  The maximum angle between the
                                       //    calibrator direction and the
                                       //    target direction, in radians
    double Min_Sat_Elev_;              // I  Minimum calibrator elevation,
                                       //    in radians.
    double Max_Rec_Dist_From_Tele_;    // I  Maximum distance of calibrator
                                       //    position from telescope, in m
    double Max_Iono_Pierce_Dist_;      // I  Maximum distance of the calibrator
                                       //    pierce point from the target
                                       //    pierce point, in m
    double Default_Iono_Height_;       // I  The default height of a 2-D
                                       //    ionosphere, in m, used for various
                                       //    calculations.
    double Averaging_Time_Half_Width_; // I  Half width, in s, of the window
                                       //    over which to use calibrator
                                       //    measurements to determine the
                                       //    ionosphere.
    int Num_Ionosphere_Parameters_;
                                       // I  Number of parameters to try to fit
                                       //    in ionospheric models
    int Num_Ionosphere_Heights_;
                                       // I  Number of heights to use for 3-D
                                       //    models.
    int Num_Time_Terms_;
                               // I  The maximum order of time polynomials
                               //    to apply.  The ionosphere is made to have
                               //    a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                               //    dependence, where N \equiv NUM_TIME_TERMS-1
                               //    Thus, 1 means constant, 2 for linear, 3
                               //    for quadratic, and so on.
    
    int Theo_Model_Type_;
                               // I  which type of theoretical model to use
                               //    as part of the fitting.
                               //    0 None  -- do not use a theoretical model
                               //    1 IRI
                               //    2 IRI_Plus
                               //    3 PIM
                               //    4 Fake0
                               //
    int Bias_Fit_Type_;        //I  How to perform the initial bias correction.
                               //    0 Use main fit type
                               //    1 Use main fitting, but no theorectical mod
                               //    2 Use global fitting (sherical 3-D)
                               //    3 Use global, but without theoretical model
                               //    Note that if you have theoretical models 
                               //    turned on, global fitting may take hours
                               //    or days on a normal conputer.

    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "iddddddiiiii",
                     &Default_Fit_Type_, &Max_Sat_Sky_Angle_,
                     &Min_Sat_Elev_, &Max_Rec_Dist_From_Tele_,
                     &Max_Iono_Pierce_Dist_, &Default_Iono_Height_,
                     &Averaging_Time_Half_Width_,
                     &Num_Ionosphere_Parameters_,
                     &Num_Ionosphere_Heights_, &Num_Time_Terms_,
                     &Theo_Model_Type_, &Bias_Fit_Type_);
    Num_Ionosphere_Parameters_ = abs(Num_Ionosphere_Parameters_);
    Num_Ionosphere_Heights_ = abs(Num_Ionosphere_Heights_);
    // Now call my stuff
    int retval = D_C->cal_observations_set_parameters(
        Default_Fit_Type_,
        float(Max_Sat_Sky_Angle_),
        float(Min_Sat_Elev_),
        float(Max_Rec_Dist_From_Tele_),
        float(Max_Iono_Pierce_Dist_),
        float(Default_Iono_Height_),
        float(Averaging_Time_Half_Width_),
        unsigned(Num_Ionosphere_Parameters_),
        unsigned(Num_Ionosphere_Heights_),
        unsigned(Num_Time_Terms_),
        unsigned(Theo_Model_Type_),
        unsigned(Bias_Fit_Type_)
        );
    return Py_BuildValue("i", retval);
}
static char cal_observations_set_parameters_docs[] = "set the selection criteria parameters for GPS/calibrator ionosphere\n";


PyObject* cal_observations_set_times(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int N_TIMES;               // I  number of datapoints
    double* data;              // I  the times, as Modified Julian Dates

    int retval;
    std::cout<<"in AlbusIonosphere cal_observations_set_times"<<std::endl;
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyObject *MJD_object = NULL;
    PyArrayObject* MJD_array = NULL;

    if(!PyArg_ParseTuple(args, "iO", &N_TIMES, &MJD_object)) {
        PyErr_Format(PyExc_RuntimeError, "Cannot parse args");
        goto fail;
    }
    if((N_TIMES <= 0)) {
        PyErr_Format(PyExc_RuntimeError, "Nonpositive array sizes!");
        goto fail;
    }
    std::cout<<"N_TIMES"<<N_TIMES<<std::endl;
    std::cout<<"MJD_object"<<MJD_object<<std::endl;
    std::cout<<"calling PyArray_FROM_OTF"<<std::endl;
    MJD_array = (PyArrayObject*) PyArray_FROM_OTF(MJD_object, NPY_DOUBLE, NPY_IN_ARRAY);
    std::cout<<"called PyArray_FROM_OTF"<<std::endl;
    if (MJD_array == NULL){
        PyErr_Format(PyExc_RuntimeError, "Cannot make MJD_array");
        goto fail;
    }
//    if((MJD_array->nd != 1) || (MJD_array->dimensions[0] < N_TIMES)) {
 //       PyErr_Format(PyExc_RuntimeError, "Invalid Numpy dimensions");
  //      goto fail;
//    }
    // Now call my stuff
    std::cout<<"calling PyArray_DATA"<<std::endl;
    data  = (double*)PyArray_DATA(MJD_array);
//    printArray("MJDarray", data, 3, N_TIMES);

    retval = D_C->cal_observations_set_times(unsigned(N_TIMES), data);
    Py_XDECREF(MJD_array);
    return Py_BuildValue("i", retval);
fail:
    Py_XDECREF(MJD_array);
    return NULL;
}
static char cal_observations_set_times_docs[] = "set the MJD times of GPS/calibrator observations\n";


PyObject* cal_observations_set_sat_pos(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int N_TIMES;               // I  number of datapoints
    int N_SAT;                 // I  number of satellites
    int N_ELEMENTS;            // I  The number of doubles used to store
                               //    each satellite position.  3 for just
                               //    a Cartesian array, or 6 for Cartesian
                               //    plus spherical.
    double* data;              // I  the times, as Modified Julian Dates

    int retval;
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyObject *SAT_object = NULL;
    PyArrayObject* SAT_array = NULL;
    if(!PyArg_ParseTuple(args, "iiiO", &N_TIMES, &N_SAT,
                         &N_ELEMENTS, &SAT_object)) {
        PyErr_Format(PyExc_RuntimeError, "Cannot parse args");
        goto fail;
    }
    if((N_TIMES <= 0) || (N_SAT <= 0) ||(N_ELEMENTS <= 0)) {
        PyErr_Format(PyExc_RuntimeError, "Nonpositive array sizes!");
        goto fail;
    }
    SAT_array = (PyArrayObject*) PyArray_FROM_OTF(SAT_object, NPY_DOUBLE, NPY_IN_ARRAY);
    if(!SAT_array) {
        PyErr_Format(PyExc_RuntimeError, "Cannot make SAT_array");
        goto fail;
    }
//    if((SAT_array->nd != 3) || (SAT_array->dimensions[0] < N_TIMES)
//       || (SAT_array->dimensions[1] != N_SAT)
 //      || (SAT_array->dimensions[2] != N_ELEMENTS)) {
  //      PyErr_Format(PyExc_RuntimeError, "Invalid Numarray dimensions");
   //     goto fail;
//    }
    // Now call my stuff
    std::cout<<"N_TIMES"<<N_TIMES<<std::endl;
    std::cout<<"N_SAT"<<N_SAT<<std::endl;
    std::cout<<"N_ELEMENTS"<<N_ELEMENTS<<std::endl;
    data  = (double*)PyArray_DATA(SAT_array);
//    printArray("SATarray", data, 3, N_TIMES);
    retval = D_C->cal_observations_set_sat_pos(unsigned(N_TIMES),
                                                   unsigned(N_SAT),
                                                   unsigned(N_ELEMENTS),
                                                   data);
    Py_XDECREF(SAT_array);
    return Py_BuildValue("i", retval);
fail:
    Py_XDECREF(SAT_array);
    return NULL;
}
static char cal_observations_set_sat_pos_docs[] = "set the satellite positions for GPS observations\n";


PyObject* cal_observations_set_cal_obs(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    char* name;                // I  4 character (or smaller) name of station
    double* pos_data;          // I  Cartesian coordinates in Earth-fixed frame
                               //    of the position of the station, in m
                               //    as position[3], with x=0, y=1, z=2
    int N_TIMES;               // I  number of datapoints
    int N_SAT;                 // I  number of satellites
    Int16* sat_data;           // I  satellite number value as
                               //    sat_data[N_TIMES][N_SAT]
    Int16* track_data;         // I  satellite track number value (per sat!) as
                               //    sat_data[N_TIMES][N_SAT]
    double* stec_data;          // I  slant TEC value, in m^{-2}
                               //    Note that this is not in TECU!
                               //    Comes as stec_data[N_TIMES][N_SAT]
    double* sigma_data;         // I  the uncertainty in each measurement,
                               //    in m^{-2]
                               //    Note that this is not in TECU!
                               //    Comes as stec_data[N_TIMES][N_SAT]
    int* bias_data;          // I  Array of flags indicating whether station
                               //    bias correction was applied for each
                               //    satellite type

    int retval;
    PyObject *pos_object = NULL;
    PyObject *sat_object = NULL;
    PyObject *track_object = NULL;
    PyObject *stec_object = NULL;
    PyObject *sigma_object = NULL;
    PyObject *bias_object = NULL;
    PyArrayObject *pos_array = NULL;
    PyArrayObject *sat_array = NULL;
    PyArrayObject *track_array = NULL;
    PyArrayObject *stec_array = NULL;
    PyArrayObject *sigma_array = NULL;
    PyArrayObject *bias_array = NULL;
    if(!PyArg_ParseTuple(args, "sOiiOOOOO", &name, &pos_object,
                         &N_TIMES, &N_SAT, &sat_object, &track_object,
                         &stec_object,
                         &sigma_object, &bias_object)) {
        PyErr_Format(PyExc_RuntimeError, "Cannot parse args");
        goto fail;
    }
    if((N_TIMES <= 0) || (N_SAT <= 0)) {
        PyErr_Format(PyExc_RuntimeError, "Nonpositive array sizes!");
        goto fail;
    }
    pos_array = (PyArrayObject*) PyArray_FROM_OTF(pos_object, NPY_DOUBLE, NPY_IN_ARRAY);
    if(!pos_array){PyErr_Format(PyExc_RuntimeError, "Cannot make pos array");
        goto fail;
    }
    sat_array = (PyArrayObject*) PyArray_FROM_OTF(sat_object, NPY_INT16, NPY_IN_ARRAY);
    if(!sat_array){PyErr_Format(PyExc_RuntimeError, "Cannot make sat array");
        goto fail;
    }
    track_array = (PyArrayObject*) PyArray_FROM_OTF(track_object, NPY_INT16, NPY_IN_ARRAY);
   if(!track_array){PyErr_Format(PyExc_RuntimeError, "Cannot make track array");
        goto fail;
    }
    stec_array = (PyArrayObject*) PyArray_FROM_OTF(stec_object, NPY_DOUBLE, NPY_IN_ARRAY);
    if(!stec_array){PyErr_Format(PyExc_RuntimeError, "Cannot make stec array");
        goto fail;
    }
    sigma_array = (PyArrayObject*) PyArray_FROM_OTF(sigma_object, NPY_DOUBLE, NPY_IN_ARRAY);
    if(!sigma_array){PyErr_Format(PyExc_RuntimeError, "Cannot make sigma array");
        goto fail;
    }
    bias_array = (PyArrayObject*) PyArray_FROM_OTF(bias_object, NPY_INT, NPY_IN_ARRAY);
    if(!bias_array) {
        PyErr_Format(PyExc_RuntimeError, "Cannot make bias arrays");
        goto fail;
    }
//   if((pos_array->nd != 1) || (pos_array->dimensions[0] < 3)
//       || (sat_array->nd != 2) || (sat_array->dimensions[1] != N_SAT)
//       || (sat_array->dimensions[0] < N_TIMES)
//       || (track_array->nd != 2) || (track_array->dimensions[1] != N_SAT)
//       || (track_array->dimensions[0] < N_TIMES)
//       || (stec_array->nd != 2) || (stec_array->dimensions[1] != N_SAT)
//       || (stec_array->dimensions[0] < N_TIMES)
//       || (sigma_array->nd != 2) || (sigma_array->dimensions[1] != N_SAT)
//       || (sigma_array->dimensions[0] < N_TIMES)
//       ) {
//        PyErr_Format(PyExc_RuntimeError, "Invalid Numarray dimensions");
//        goto fail;
//    }
    pos_data  = (double*)PyArray_DATA(pos_array);
    sat_data  = (Int16*)PyArray_DATA(sat_array);
    track_data  = (Int16*)PyArray_DATA(track_array);
    stec_data  = (double*)PyArray_DATA(stec_array);
    sigma_data  = (double*)PyArray_DATA(sigma_array);
    bias_data  = (int*)PyArray_DATA(bias_array);

//    printSIntArray("SATarray", sat_data, 3, N_TIMES);
//    printSIntArray("TRACKarray", sat_data, 3, N_TIMES);
//    printIntArray("biasarray", bias_data, 3, N_TIMES);
//    printArray("STECarray", stec_data, 3, N_TIMES);
    // Now call my stuff
    retval = D_C->cal_observations_set_cal_obs(
        name, pos_data, unsigned(N_TIMES), unsigned(N_SAT), sat_data, track_data,
        stec_data,
        sigma_data, bias_data);
        
    Py_XDECREF(pos_array);
    Py_XDECREF(sat_array);
    Py_XDECREF(track_array);
    Py_XDECREF(stec_array);
    Py_XDECREF(sigma_array);
    Py_XDECREF(bias_array);
    return Py_BuildValue("i", retval);
fail:
    Py_XDECREF(pos_array);
    Py_XDECREF(sat_array);
    Py_XDECREF(track_array);
    Py_XDECREF(stec_array);
    Py_XDECREF(sigma_array);
    Py_XDECREF(bias_array);
    return NULL;
}
static char cal_observations_set_cal_obs_docs[] = "set the calibration data of GPS/calibrator observations\n";

PyObject* get_TAI_UTC(PyObject* self, PyObject* args)
{
//_ARGS  TYPE           VARIABLE I/O DESCRIPTION
    int year;                  // I  The year as a 4 digit number
    int month;                 // I  The month, 1 based
    int day;                   // I  The day of the month
    double fday;               // I  The fractional day
    double delta_TAI_UTC;      // O  The time difference TAI-UTC for the
                               //    specified time, in s
    int retval;                // O  SOFA error checking
    
    // the Python docs say that if this has an error, it will
    // raise an exception, so don't bother to check for errors.
    PyArg_ParseTuple(args, "iiid", &year, &month, &day, &fday);
    // Now call my stuff

    integer IY = year;
    integer IM = month;
    integer ID = day;
    doublereal FD = fday;
    doublereal DELTAT;
    integer J;
    FTN_NAME(iau_dat) (&IY, &IM, &ID, &FD, &DELTAT, &J);
    delta_TAI_UTC = DELTAT;
    retval = J;

    return Py_BuildValue("id", retval, delta_TAI_UTC);
}
static char get_TAI_UTC_docs[] = "get the TAI-UTC value for the specified time, in s\n";





static PyMethodDef AlbusIonosphereMethods[] = {
    {"clear_everything",  (PyCFunction)clear_everything, METH_VARARGS, clear_everything_docs},
    {"set_ionosphere_IRI",  (PyCFunction)set_ionosphere_IRI, METH_VARARGS, set_ionosphere_IRI_docs},
    {"set_ionosphere_PIM",  (PyCFunction)set_ionosphere_PIM, METH_VARARGS, set_ionosphere_PIM_docs},
    {"set_ionosphere_GPS",  (PyCFunction)set_ionosphere_GPS, METH_VARARGS, set_ionosphere_GPS_docs},
    {"set_reference_time",  (PyCFunction)set_reference_time, METH_VARARGS, set_reference_time_docs},
    {"set_reference_time_AIPS",  (PyCFunction)set_reference_time_AIPS, METH_VARARGS, set_reference_time_AIPS_docs},
    {"set_station_position",  (PyCFunction)set_station_position, METH_VARARGS, set_station_position_docs},
    {"set_source_position",  (PyCFunction)set_source_position, METH_VARARGS, set_source_position_docs},
    {"set_time_step",  (PyCFunction)set_time_step, METH_VARARGS, set_time_step_docs},
    {"set_scan_times",  (PyCFunction)set_scan_times, METH_VARARGS, set_scan_times_docs},
    {"get_Num_Ionospheric_Predictions",  (PyCFunction)get_Num_Ionospheric_Predictions, METH_VARARGS, get_Num_Ionospheric_Predictions_docs},
    {"get_ionospheric_prediction",  (PyCFunction)get_ionospheric_prediction, METH_VARARGS, get_ionospheric_prediction_docs},
    {"get_source_AzEl",  (PyCFunction)get_source_AzEl, METH_VARARGS, get_source_AzEl_docs},
    {"get_source_AzElVTEC",  (PyCFunction)get_source_AzElVTEC, METH_VARARGS, get_source_AzElVTEC_docs},
    {"cal_observations_init",  (PyCFunction)cal_observations_init, METH_VARARGS, cal_observations_init_docs},
    {"cal_observations_init2",  (PyCFunction)cal_observations_init2, METH_VARARGS, cal_observations_init2_docs},
    {"cal_observations_set_parameters",  (PyCFunction)cal_observations_set_parameters, METH_VARARGS, cal_observations_set_parameters_docs},
    {"cal_observations_set_times",  (PyCFunction)cal_observations_set_times, METH_VARARGS, cal_observations_set_times_docs},
    {"cal_observations_set_sat_pos",  (PyCFunction)cal_observations_set_sat_pos, METH_VARARGS, cal_observations_set_sat_pos_docs},
    {"cal_observations_set_cal_obs",  (PyCFunction)cal_observations_set_cal_obs, METH_VARARGS, cal_observations_set_cal_obs_docs},
    {"get_TAI_UTC",  (PyCFunction)get_TAI_UTC, METH_VARARGS, get_TAI_UTC_docs},
    {NULL}        /* Special Value to indicate End */
};

#if PY_MAJOR_VERSION < 3
//Python2 interface
PyMODINIT_FUNC initAlbusIonosphere(void)
{
    // Start out by filling in the Python stuff
    Py_InitModule("AlbusIonosphere", AlbusIonosphereMethods);
    // Need access to numpy stuff
    import_array();

    // Now take care of my own stuff.  Initialize the Data_Calibrator.
    if((D_C)) {
        delete D_C;
        D_C = NULL;
    }
    D_C = new Data_Calibrator();
    // That's all.
    return;
}

#else:
//Python3 interface
PyDoc_STRVAR(module_doc,
"Interface to c++ ionosphere stuff.");

static struct PyModuleDef moduledef = {
        PyModuleDef_HEAD_INIT,
        "AlbusIonosphere",     /* m_name */
        module_doc,
        0,                  /* m_size */
        AlbusIonosphereMethods,    /* m_methods */
        NULL,                /* m_reload */
        NULL,                /* m_traverse */
        NULL,                /* m_clear */
        NULL,                /* m_free */
};

static PyObject * moduleinit(void)
{
    PyObject *m;
    m = PyModule_Create(&moduledef);
    return m;
}

PyMODINIT_FUNC PyInit_AlbusIonosphere(void)
{
    // Need access to numpy stuff
    import_array();

    // Now take care of my own stuff.  Initialize the Data_Calibrator.
    if((D_C)) {
        delete D_C;
        D_C = NULL;
    }
    D_C = new Data_Calibrator();
    return moduleinit();
}

#endif
