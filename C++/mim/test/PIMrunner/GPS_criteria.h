// GPS_criteria.h
// holds selectron criteria numbers for GPS fitting
//_HIST  DATE NAME PLACE INFO
//	2007 Feb 19  James M Anderson  --JIVE  start from what was in GPS_collection



#ifndef GPS_CRITERIA_H
#define GPS_CRITERIA_H

// INCLUDES
#include "JMA_code.h"
#include "ionosphere_gen.h"





// set up a namespace area for stuff.
namespace MIM_PIM {

    enum GPS_Fit_Type_Enum {
        single_nearest   = 0,
        single_all       = 1,
        multiple_2D      = 2,
        multiple_3D_mult = 3, // multiple layers with the same poly dependance
        multiple_3D_many = 4, // multiple layers with different poly dependance
        multiple_3D_sphe = 5, // spherical harmonics with different h layers
        multiple_2D_time = 6, // 2D ionosphere with time dependance
        multiple_2D_grad = 7, // 2D ionosphere with gradient least squares
        multiple_2D_timg = 8, // 2D ionosphere with time, gradient least squares
        multiple_3D_sphg = 9  // 3D ionosphere spherical, gradient least squares
    };

    enum GPS_Bias_Fit_Type_Enum {
        Use_Main_Fitting               = 0,
        Use_Main_Without_Theoretical   = 1,
        Use_Global_Fitting             = 2,
        Use_Global_Without_Theoretical = 3,
        Use_Global_Track_Fitting       = 4,
        Use_Global_Track_Without_Theo  = 5
    };





//_CLASS  name one line description
class GPS_criteria {
//_DESC  full description of class

//_FILE  files used and logical units used

//_LIMS  design limitations

//_BUGS  known bugs

//_CALL  list of calls

//_KEYS  

//_HIST  DATE NAME PLACE INFO

//_END


// NAMESPACE ISSUES    


public:

    GPS_criteria(GPS_Fit_Type_Enum Default_Fit_Type_ = multiple_3D_many,
                 Real32 Max_Sat_Sky_Angle_ = 2.0*M_PI,
                 Real32 Min_Sat_Elev_ = 0.1745,
                 Real32 Max_Rec_Dist_From_Tele_ = 300E3,
                 Real32 Max_Iono_Pierce_Dist_ = 1000E3,
                 Real32 Default_Iono_Height_ = 300E3,
                 Real32 Averaging_Time_Half_Width_ = 0.5,
                 Uint32 Num_Ionosphere_Parameters_ = 40,
                 Uint32 Num_Ionosphere_Heights_ = 5,
                 Uint32 Num_Time_Terms_ = 1,
                 Ionosphere_Theoretical_Model_Enum Theo_Model_Type_ = None,
                 GPS_Bias_Fit_Type_Enum Bias_Fit_Type_ =
                                                 Use_Main_Without_Theoretical
                 ) :
            Max_Sat_Sky_Angle(Max_Sat_Sky_Angle_),
            Min_Sat_Elev(Min_Sat_Elev_),
            Max_Rec_Dist_From_Tele(Max_Rec_Dist_From_Tele_),
            Max_Iono_Pierce_Dist(Max_Iono_Pierce_Dist_),
            Default_Iono_Height(Default_Iono_Height_),
            Averaging_Time_Half_Width(Averaging_Time_Half_Width_),
            Default_Fit_Type(Default_Fit_Type_),
            MAX_NUM_PARAMETERS(Num_Ionosphere_Parameters_),
            NUM_HEIGHTS(Num_Ionosphere_Heights_),
            NUM_TIME_TERMS(Num_Time_Terms_),
            Theo_Model_Type(Theo_Model_Type_),
            Bias_Fit_Type(Bias_Fit_Type_)
        {
            if(Min_Sat_Elev < 0.0f) Min_Sat_Elev = 0.0f;
            return;
        }
    



    // Here are the ionosphere fitting constraints
    // The code should treat these as constants, except for the reset function.
    Real32 Max_Sat_Sky_Angle; // radians, default 2\pi, angular distance
                              //                        from source
    Real32 Min_Sat_Elev;      // radians, default 10\degr\approx 0.1745 rad
                              // This should be a const, but I check that
                              // the value is no less than 0.0, which means
                              // it cannot be declared const.
    Real32 Max_Rec_Dist_From_Tele; // meters,  default 300E3 m
    Real32 Max_Iono_Pierce_Dist;   // meters,  default 1000E3 m
                              //    The distance of the main pierce point
                              //    from the telescope
    Real32 Default_Iono_Height;// meters, default 300E3 m
    Real32 Averaging_Time_Half_Width;
                              // s, default 0 s

    GPS_Fit_Type_Enum Default_Fit_Type;
                              // The normal type of fit to make, defaults to
                              // multiple_3D_many

    Uint32 MAX_NUM_PARAMETERS;// The number of parameters to try to fit
    Uint32 NUM_HEIGHTS;       // The number of heights to fit for 3-D work
    Uint32 NUM_TIME_TERMS;    // The maximum order of time polynomials
                              // to apply.  The ionosphere is made to have
                              // a (a_0 t^0 + a_1 t^1 + ... + a_N t^N)
                              // dependence, where N \equiv NUM_TIME_TERMS-1
                              // Thus, 1 means constant, 2 for linear, 3
                              // for quadratic, and so on.
    Ionosphere_Theoretical_Model_Enum Theo_Model_Type;
                              // The type of theoretical model to apply as
                              // one of the coefficints.  Defaults to None,
                              // which then provides just the basic fit type.
                              // Other useful alternatives are PIM and IRI to
                              // apply a PIM or an IRI model
    GPS_Bias_Fit_Type_Enum Bias_Fit_Type;
                              // How should the bias levels be fit?


protected:



private:


    
};


// CLASS FUNCTIONS



// HELPER FUNCTIONS



}  // end namespace

#endif // GPS_CRITERIA_H
