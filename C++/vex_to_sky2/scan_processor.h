//
//  scan_processor.h --class to process scans
//
//  This is a specialization of the VisibilityBuffer class
//
//
//  Author:
//  2005 Jun 03  James M Anderson  --JIVE  based on COFVisibilityBuffer.h work by
//                                   Harro Verkouter, 31-05-1999
//
//
//  $Id: scan_processor.h,v 0.0 2005/06/03 15:42:00 anderson Exp $
//
//
#ifndef SCAN_PROCESSOR_H
#define SCAN_PROCESSOR_H

//
//  Bool, Double etc...
//
#ifndef AIPS_AIPS_H
#include <aips/aips.h>
#endif

#ifndef AIPS_STRING_H
#include <aips/Utilities/String.h>
#endif

//
//  This is our base-class so we need it
//
#ifndef VISIBILITYBUFFER_H
#include <VisibilityBuffer.h>
#endif


#ifndef AIPS_MATRIX_H
#include <aips/Arrays/Matrix.h>
#endif

#ifndef AIPS_VECTOR_H
#include <aips/Arrays/Vector.h>
#endif

#ifndef AIPS_COMPLEX_H
#include <aips/Mathematics/Complex.h>
#endif

#ifndef AIPS_MEPOCH_H
#include <aips/Measures/MEpoch.h>
#endif

// #ifndef DATAFILE_H
// #include <data_handler/DataFile.h>
// #endif

// #include <data_handler/DataDescriptor.h>
// #include <data_handler/CorrelatedDataRecord.h>
// #include <data_handler/MappingEntry.h>
// #include <data_handler/CountedPointer.h>

// #ifndef SCAN_H
// #include <Scan.h>
// #endif

// #ifndef CONTAINER_H
// #include <tmplsrc/Container.h>
// #endif


#include <vector>
#include <string>
#include <map>




// set up a namespace area for stuff.
namespace JMA_VEX_AREA {



//
// Struct mfcSetup is filled in by parsing an MFC-setup file (if the data was
// correlated with MFC's and the user of j2ms2 indicates in the the experiment
// option).
// 
// In this MFC setup it will tell us which sources were 'aliased' (i.e. were
// correlated with different phase-centers). If we find a scan with name 'orgname', we
// will silently insert <num-phase-center> scans with the right sources, forgetting
// the original scan....
//
// Also, it should tell us how the logical channels were replicated at the SU-output,
// since from Albert we only get the ID of the physical output. Normally we use just
// the mappingsfile to go from physical output to logical channel (in order to find
// out the SUBBAND of the datum). Now we have an additional mapping because the data
// is 'multiplexed': datastreams with the same 'physical' labels
// (XANT/YANT/XPOL/YPOL/SUBBAND) are duplicated into different Albert-labels
// (=XSIGINP/YSIGINP). We need to be able to tell from the (X|Y)SIGINP labels what the
// real labels of the stream were.
//
// Further, we must know which 'scan' to take. What happens is that each scan will
// have <num-phase-center> datums per interferometer/time pair. We need to be able to
// label the streams with the correct phase-center. Thats where the physToFieldCenter
// map comes in: it tell us (sort of) which polynomials were sent to which SU-output.
// So by backtracing this mapping, we can find out from the output what the real
// coordinates of the phase-center were!
//
typedef struct _mfcSrcAlias
{
    vector<string>   aliases;
} mfcSrcAlias;

typedef struct _mfcSetup
{
    //
    // Default c'tor. Will initialize the stuff such that the mappings do not take any
    // effect. The source-aliases are empty. 
    // This has the effect that we can *always* use an mfcSetup object.
    //
    // Either it is a default object (nothing happens) OR
    // it is overwritten by stuff read from an MFC setup file.... and then mappings
    // come into play!
    //
    _mfcSetup();	
    //
    // maps physical output of SU to logical vex-channel
    // Assumption: physical output is the arrayindex into this vector
    //
    // We use the logical vex-channel to find out what the 'real' subband nr of this
    // datum is
    //
    vector<int>    physToLogMap;
	
    //
    // For each physical output of the SU, tell us which 'aliased' source (hence
    // field-center) this output contained data for (which field).
    //
    vector<int>    physToFieldCenterMap;

    //
    // Map from sourcename to list of aliases
    //
    map<string,mfcSrcAlias> srcToAliasMap;

} mfcSetup;

//
//  Forward declarations
//

class VEXperiment;
class MappingsFile;
class CorrelatedDataFile;



class scan_processor :
    public VisibilityBuffer
{
public:

	//
	// Static methods that construct default mappings for a MFC setup that has no
	// effect
	//
    static vector<int>          defaultPhysToLogMap( void );	
    static vector<int>          defaultPhysToSrcMap( void );

    
    //
    //  Create a scan_processor object
    //
    scan_processor( VEXperiment* experimentptr=0 ,
                    double ionosphere_time_step = 300.0,
                    // in units of seconds
                    // This is the time interval between requests for
                    // ionosphere data from the data source.
                    double ionosphere_interpolation_time = 300.0
                    // in units of seconds
                    // This is the amount of time before and after an
                    // observation during which ionosphere data
                    // is also collected.  This buffers the observations
                    // before and after, so that interpolation and trends can
                    // possibly be found.  It should probably be at least
                    // ionosphere_time_step in size
                    );
    
    //
    //  What's the directory of the visibilitybuffer??
    //
    const String& getRootDir( void ) const;

    //
    //  Delete all allocated resources
    //
    //virtual ~scan_processor();
    
private:
    //
    //  Our private parts
    //
    String                myRootDir;
    MappingsFile*         myMappingsFileptr;


    //
    // Just have an object of this type. By default, the mappings found in this object
    // are initialised such that they (should) have no effect. In the case of MFC
    // data, the mappings do change... (this setup is filled in from parsing an
    // mfc-file) = EXTERNALCONFIG in the JCCS system
    //
    mfcSetup                    myMFCSetup;
    //mfcSetup                    defaultSetup;
	
    //
    //  Private methods
    //
    



   
    //
    // Parse the file, assuming it is a MFC-setup file
    //
    static bool                 readMFCSetup( mfcSetup& result, const String& mfcfile );
	

    //
    //  Prohibit these
    //
    scan_processor();
    scan_processor( const scan_processor& );
    scan_processor& operator=( const scan_processor& );
};



} // end namespace JMA_VEX_AREA



#endif // SCAN_PROCESSOR_H
