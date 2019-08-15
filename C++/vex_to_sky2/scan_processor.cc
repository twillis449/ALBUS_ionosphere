// scan_processor.cc
// code to process the scans in a vex file.
//_HIST  DATE NAME PLACE INFO
// 2005 Jun 03  James M Anderson  --JIVE  start work, based on j2ms code
//                                  copied from Verkouter's
//                                  COFVisibilityBuffer.cc and modified




// INCLUDES
#ifndef VEXPERIMENT_H
#include <VEXperiment.h>
#endif

#include "scan_processor.h"

#ifndef AIPS_MATRIX_H
#include <aips/Arrays/Matrix.h>
#endif

#ifndef AIPS_VECTOR_H
#include <aips/Arrays/Vector.h>
#endif

#ifndef AIPS_IPOSITION_H
#include <aips/Lattices/IPosition.h>
#endif

#ifndef AIPS_EXCEPTIONS_H
#include <aips/Exceptions.h>
#endif

#ifndef EXCEPTIONOBJECT_H
#include <ExceptionObject.h>
#endif

#ifndef REGULAREXPRESSIONS_H
#include <RegularExpressions.h>
#endif

#include <aips/Measures/MPosition.h>

#include <aips/Measures.h>
#include <aips/Measures/MDirection.h>
#include <aips/Quanta/MVEpoch.h>
#ifndef AIPS_MVTIME_H
#include <aips/Quanta/MVTime.h>
#endif

#ifndef DAYCONVERSION_H
#include <DayConversion.h>
#endif

#ifndef AIPS_MEPOCH_H
#include <aips/Measures/MEpoch.h>
#endif

#include "ellipsoidal_coord.h"




#include <strstream>
#include <fstream>
#include <iomanip>
#include <string>

#include <string.h>    
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>


//
//  Vexplus?
//
#include <vexplus.h>



using std::cout;
using std::endl;
using std::setw;
using std::flush;
using std::strstream;
using std::ends;




// set up a namespace area for stuff.
namespace JMA_VEX_AREA {




// GLOBALS


// FUNCTIONS






ostream& operator<<( ostream& os, const mfcSetup& s )
{
	vector<int>::const_iterator             iter;
	map<string,mfcSrcAlias>::const_iterator curalias;
	
	os << "MFCSetup:" << endl;

	for( curalias=s.srcToAliasMap.begin();
		 curalias!=s.srcToAliasMap.end();
		 curalias++ )
	{
		vector<string>::const_iterator  aptr = curalias->second.aliases.begin();
		
		
		os << "Source: " << curalias->first << " - ";
		while( aptr!=curalias->second.aliases.end() )
		{
			os << *aptr << " ";
			aptr++;
		}
		os << endl;
	}

	// list the crm-shuffle
	os << "CRM: ";
	iter = s.physToLogMap.begin();

	while( iter!=s.physToLogMap.end() )
	{
		os << setw(2) << *iter << " ";
		iter++;
	}
	os << endl;
	
	// and the source shuffle
	os << "SRC: ";
	iter = s.physToFieldCenterMap.begin();

	while( iter!=s.physToFieldCenterMap.end() )
	{
		os << setw(2) << *iter << " ";
		iter++;
	}
	os << endl;

	return os;
}

//
// Default c'tor
//
_mfcSetup::_mfcSetup() :
	physToLogMap( scan_processor::defaultPhysToLogMap() ),
	physToFieldCenterMap( scan_processor::defaultPhysToSrcMap() )
{
}







    

//
//
//  The implementation of the scan_processor class
//
//
    // The constructor
scan_processor::scan_processor( VEXperiment* experimentptr,
                                double ionosphere_time_step,
                                double ionosphere_interpolation_time
                                ) :
        VisibilityBuffer( experimentptr )
{

    // Make some sanity checks on the inputs
    // for now, do not accept anything less than 0.5 seconds
    {
        const double check = 0.5;
        if((ionosphere_time_step < check)
           || (ionosphere_interpolation_time < check)) {
            cerr << "Error: ionosphere time step and interpolation time must\n";
            cerr << "be at least " << check << " seconds, but are\n";
            cerr << "time step: " << ionosphere_time_step << endl;
            cerr << "interpolation time: " << ionosphere_interpolation_time << endl;
            exit(-2);
        }
    }
    // Warn if the interpolatino time is less than the time step
    if(ionosphere_interpolation_time < ionosphere_time_step) {
        cerr << "Warning: interpolation time (" << ionosphere_interpolation_time << ") is less than the ionosphere time step (" << ionosphere_time_step << ")\n";
    }


    
    char            strmbuffer[ 1024 ];
    String          vexfilename;
    MVTime          scanmvtime;
    VexPlus*        vexfileptr;
    strstream       strm( strmbuffer, sizeof(strmbuffer) );


    


    // Now it's about time to start thinking about MultipleFieldCentre stuff. What we
    // do is check if the experiment has the mfc= option set. We assume that it
    // indicates the name of the MFC-config file.
    // From this file we deduce the source-aliases and the mapping of physical station
    // unit output to logical vex-channel number
    //
    const Experiment& expref( this->getExperiment() );
    String            exproot = expref.getRootDir()+"/";
    String            mfcfile = expref.optionValue( "mfc" );



    //cerr << "Experiment mfc file is " << mfcfile << endl;
    bool   Multiple_Field_Center_present = ( mfcfile!="" ) ? true:false;
    if(( Multiple_Field_Center_present ))
    {
        cerr << "Error: Multiple Field Center information detected.\n";
        cerr << "This program is not sure how to handle this, and is bailing out\n";
        cerr << "Contact the program author for code fixes\n";
        exit(-1);
        
        bool success
            = scan_processor::readMFCSetup( myMFCSetup, exproot+mfcfile );

        if( success )
        {
            cout << "Read setup: " << endl << myMFCSetup << endl;
        }
        else
        {
            cout << "Error reading MFC-file " << mfcfile << endl
                 << "Giving up since you indicated we need MFC! (mfc= option was set)"
                 << endl;
            throw( EXCEPTION("scan_processor - failed to read MFC setup") );
        }
    }
	
    //
    //  Ok. Now's the time to read the vexfile with VEX-stuff
    //
    //  Need to make rw-enabled copy of vexfilename. Vexfilename is 
    //  'const char*'. newer compilers fail to compile, since VexPlus
    //  takes 'char *' as argument. Cannot cast to char* (does not help)
    //  maybe a dynamic_cast<char*> or something like that might have done the 
    //  trick but this works just as well....
    //
//     char*  kutzooi = ::strdup( vexfilename.c_str() );

//     vexfileptr = new VexPlus( kutzooi );
//     ::free( kutzooi );	


    
//     if( (result=vexfileptr->parseVex())!=0 )
//     {
//         strm.seekp( 0 );
	
//         strm << "scan_processor - ";
		
//         switch( result )
//         {
//             case -1:
//                 strm << "could not open file " << vexfilename;
//                 break;
		
//             case -2:
//                 strm << "error parsing vex-file " << vexfilename << " to memory";
//                 break;
		
//             default:
//                 strm << "Unknown error?! Check VexPlus::parseVex() for error"
//                      << " return codes!!";
//                 break;
//         }
//         strm << ends;
	
//         throw( EXCEPTION(strm.str()) );
//     }



    // Hey, the incoming experiment already has a VexPlus object
    vexfileptr = experimentptr->get_myVexFileptr(); // new VexPlus( kutzooi );


    
    //
    //  Find the maximum nr. of scans and let's hope that it is the
    //  number of scans (in order for this to be true, there must be
    //  at least one station that is partaking in all the scans!)
    //
    //  HV: 10-07-2002 - Finally got round to updating this. 
    //                   There prove to be quite a number of instances
    //                   in which the predicate does not hold, which
    //                   screws the whole translation thing.
    //                   What we'll do is the following. We will loop over
    //                   *all* scans for *all* stations and keep a list
    //                   of scans we have found so far. Each time we
    //                   find a scan which was not yet found, we'll
    //                   add it into the list, in the same way as we
    //                   would have previously.
    //
    //
    //  Loop over all scans and add them
    //  If the source in the scanlist is present in the mfcSetup/srcAlias map, we add
    //  multiple scans with different sourcenames
    //
    map<string,mfcSrcAlias>&         mapref( myMFCSetup.srcToAliasMap );

    for( int stationcnt=0; stationcnt<vexfileptr->N_Stations(); stationcnt++ )
    {
        //cerr << "doing station " << stationcnt << endl;
        int         nscans;
        std::string curstation;
	
        curstation = vexfileptr->Station( stationcnt );

        if( curstation.empty() )
        {
            cerr << "scan_processor - Failed to get Station name from VEXfile\n"
                 << "                      for station #" << stationcnt << endl;
            continue;
        }
        if( (nscans=(vexfileptr->N_Scans(curstation)))==0 )
        {
            cerr << "scan_processor - Station " << curstation << " has no scans?!"
                 << endl;
            continue;
        }

        cout << "scan_processor - Reading " << nscans << " scans for "
             << curstation << " (" << stationcnt << "/" << vexfileptr->N_Stations() << ")"
             << endl;




        // open a file for output
        char FileNameBuffer[1024];
        strstream FileNameString(FileNameBuffer, sizeof(FileNameBuffer));
        FileNameString.seekp( 0 );
        FileNameString << vexfileptr->ExperName() << '.' << vexfileptr->TwoLetter(curstation) << ".fus" << '\0';
        //cerr << FileNameString.str() << endl;

        FILE* fp = fopen(FileNameString.str(),"w");
        if(fp == NULL) {
            cerr << "Error opening file " << FileNameString.str() << endl;
            exit(2);
        }


        





        // Ok, let's get the position of this station
	MPosition      station_position;
        ellipsoidal_coord station_ellipsoid;
        Vector<double> station_vector(3);
        {
            double x = vexfileptr->SiteX( curstation );
            double y = vexfileptr->SiteY( curstation );
            double z = vexfileptr->SiteZ( curstation );	    

            station_position = MPosition( MVPosition(x,y,z), MPosition::ITRF );


            station_ellipsoid = ellipsoidal_coord(x,y,z);
            station_vector(0) = x;
            station_vector(1) = y;
            station_vector(2) = z;
        }

        //cerr << "got station position of " << station_position << endl;








        // Ok, I need to know the min and max possible time range
        // Let's grab the times
        Vector<double>   scan_start_time(nscans);
        Vector<double>   scan_end_time(nscans);
        // I also need to hold onto the object names
        Vector<String>   scan_source_name(nscans);
        // And I will eventually need to know whether or not I have fully
        // processed each scan.
        Vector<bool>     scan_processed(nscans, false);



        for( int scan=0; scan<nscans; scan++ )
        {
            int                               year, daynr, hour, minute, second;
            unsigned int                      month, day;
            double                            dayfraction;
            double                            scanduration;
            std::string                       scanstart;
            std::string                       scanlab;

	
	
            scanlab   = vexfileptr->ScanName(curstation, scan);
            if( scanlab.empty() )
            {
                cerr << "scan_processor - Failed to get ScanName from VEXfile\n"
                     << "                     for Station=" << curstation << ", scan #"
                     << scan << endl;
                continue;
            }


            // Get the start time of the scan
            scanstart = vexfileptr->ScanStart(curstation, scan);
			
            if( scanstart.empty() )
            {
                cerr << "scan_processor - Failed to get ScanStart from VEXfile\n"
                     << "                     for Station=" << curstation << ", scan #"
                     << scan << endl;
                continue;
            }
	
			
            // Get the duration of the scan
            scanduration = vexfileptr->ScanDuration(curstation, scan);
	
            //
            //  Ok. Transform the scanstart into a mvepoch
            //
            if( ::sscanf(scanstart.c_str(), "%dy%dd%dh%dm%ds", &year, &daynr, &hour,
                         &minute, &second)!=5 )
            {
                continue;
            }
	
            DayConversion::dayNrToMonthDay( month, day, daynr, year );
	
            dayfraction=((hour*3600.0)+(minute*60.0)+
                         (double)second)/(double)DayConversion::secondsPerDay;
		
            scanmvtime = MVTime( year, month+1, day, dayfraction );

//             cerr << "The MVTime is " << scanmvtime << endl;
//             cerr << "The MVTime is " << scanmvtime.second()
//                 - (MVTime(2005,6,2,0.0)).second() << endl;
		
		
            //
            //  get the time and name information
            //

            scan_start_time(scan)   = scanmvtime.second();
            scan_end_time(scan)     = scan_start_time(scan) + scanduration;

            scan_source_name(scan)    = vexfileptr->ScanSource(curstation, scan);


//             cerr << "On scan " << scan << " got " <<
//                 scan_start_time(scan) << " " << scan_end_time(scan) <<
//                 " " << scan_source_name(scan) << endl;
        } // for i over nscans



        // Now figure out what the min and max times are
        double time_first = scan_start_time(0);
        double time_last  = scan_end_time(0);

        for(int scan=1; scan < nscans; scan++) {
            if(time_first > scan_start_time(scan))
                time_first = scan_start_time(scan);
            if(time_last < scan_end_time(scan))
                time_last = scan_end_time(scan);
        }

        // How far apart are these in seconds?
        double time_difference = time_last - time_first;
        // Add on extra time to allow for measurements before and after
        time_difference += 2.0*ionosphere_interpolation_time;
        // Do some sanity checks
        if(time_difference < 0.5) {
            // less than 0.5 seconds ???
            cerr << "Warning: time difference is only " << time_difference << " seconds\n";
        }
        else if(time_difference > 5.0 * DayConversion::secondsPerDay) {
            cerr << "Warning: time difference of " << time_difference << " seconds is more than 5 days\n";
        }
        // How many ionospehre measurement time units apart is this?
        const int number_time_chunks =
            int(ceil(time_difference/ionosphere_time_step)) + 1;
        if(number_time_chunks <= 1) {
            // something has gone wrong here
            cerr << "Warning: only have " << number_time_chunks << " datapoints to obtain\n";
        }


        // What is the very, very start time?
        const double time_start =
            time_first - ionosphere_interpolation_time;








        // Now I am going to loop over the scans, and dump out information
        // for each object

        for( int scan=0; scan<nscans; scan++ ) {
            // Has this scan/object been processed yet?
            if(scan_processed(scan) == true) continue;

            // What is the name of this source?
            const String this_source_name = scan_source_name(scan);


            // what is the sky coordinate of this target?
            MDirection source_direction;
            {
                String            srcname;
                String            epochstr;
                MVDirection       srcvdir;
                const String      defepoch( "J2000" );
                MDirection::Types epoch;
                bool found_source = false;

                for( int source=0; source<vexfileptr->N_Sources(); source++ ) {
                    srcname  = vexfileptr->SourceName( source );

                    if(srcname != this_source_name) continue;
                    found_source = true;
                    
                    epochstr = vexfileptr->RefEpoch( source );

	    
                    if( !(MDirection::getType(epoch, epochstr)) )
                    {
                        epoch = MDirection::J2000;
                    }
	    
                    srcvdir = MVDirection( vexfileptr->Source_RA(source), vexfileptr->Source_Dec(source) );
                    source_direction  = MDirection( srcvdir, epoch );
	    
                    break;
                } // for source over number sources
                if(!found_source) {
                    cerr << "Error: could not find source " << this_source_name << " in list of sources\n";
                    exit(-3);
                }
            }


            //Quantum<Vector<Double> > aa = source_direction.getAngle();

            //cerr << "Source direction for scan " << scan << " is " << aa << endl;





            

            // Now, for all scans of the same source, run through
            // and find all of the times to process to get ionosphere data.
            // Build up a holder so I can see where to process times.
            // 0 do not process, 1 do
            Vector<char> process_flag(number_time_chunks,0);


            for(int test_source = scan; test_source < nscans; test_source++) {
                // if this is not the same source, then go to the next.
                // But check carefully, in case there are multiple field
                // centers with different names
                if(!Multiple_Field_Center_present) {
                    if(this_source_name != scan_source_name(test_source))
                        continue;
                }
                else {
                    vector<string>                    srces;
                    vector<string>::iterator          cursrc;
                    map<string,mfcSrcAlias>::iterator alias;

                    srces.push_back( this_source_name );
                    if(	(alias=mapref.find(this_source_name))!=mapref.end() )
                    {
                        //
                        // Now replace the source with its aliases and let the system add the new
                        // scan definitions to the normal list
                        //
                        srces.resize( 0 );
                        srces = alias->second.aliases;
                    }

                    bool found_source = false;
                    for( cursrc=srces.begin(); cursrc!=srces.end(); cursrc++ )
                    {
                        if(this_source_name == String(*cursrc)) {
                            found_source = true;
                            break;
                        }
                    }
                    if(!found_source) continue;
                }




                

                // Otherwise, mark that we are processing this scan
                scan_processed(test_source) = true;

                // what are the start and stop tiems for this scan,
                // adjusted for the interpolation fudge?
                double time_first = scan_start_time(test_source)
                    - ionosphere_interpolation_time;
                double time_last  = scan_end_time(test_source)
                    + ionosphere_interpolation_time;

                // Now, make a mark where this should go in our
                // time slots
                int index = int(floor((time_first - time_start)
                                      /ionosphere_time_step));
                for(;index < number_time_chunks; index++) {
                    // what does the index time correspond to?
                    double this_time = index*ionosphere_time_step + time_start;

                    // if we are over the end time, stop
                    if(this_time > time_last) break;

                    // Otherwise, mark this as a time to process
                    process_flag(index) = 1;
                }
            } // for test_source to nscans

            // Ok, we have a block of times to get ionospheric data.
            // print out the times and other useful information
             for(int index = 0;index < number_time_chunks; index++) {
                 // if this one has no data area, the skip
                 if(process_flag(index)==0) continue;
                 
                 // make up the time
                 double this_time = index*ionosphere_time_step + time_start;
                 // Convert to a MVTime
                 MVTime this_MVTime(Quantity(this_time, "s"));

                 // Now, I need a string for FusionNumerics format
                 char FusionBuffer[1024];
                 strstream FusionString(FusionBuffer, sizeof(FusionBuffer));
                 {
                     this_MVTime.setFormat(MVTime::YMD,8);
                     FusionString << this_MVTime;
                     int year, month, day, hour, minute;
                     float second;
                     if(::sscanf(FusionString.str(), "%d/%d/%d/%d:%d:%f",
                                 &year, &month, &day,
                                 &hour, &minute, &second) != 6) {
                         cerr << "Error: bad MVTime format in date " << FusionString << endl;
                         exit(-1);
                     }
                     if(second >= 30.0f) {
                         minute++;
                     }
                     ::sprintf(FusionBuffer, "%2.2d%2.2d%4.4d%2.2d%2.2d",
                               day, month, year,
                               hour, minute);
                 }


                 // need Az/El coordinates of the source at this time
                 {
                     // The observatory position. Note that the reference is geodetic position 
                     MPosition myobs(station_position);
                     // The time I want to observe (note that it could be specified in many
                     // other ways)
                     MVEpoch tt(this_MVTime);
                     MEpoch obstime(tt,MEpoch::UTC);
                     // The frame specification for when and where to observe
                     //MeasFrame frame(myobs, obstime);
                     MeasFrame frame(myobs,obstime);
//                      MEpoch::Ref sidref( MEpoch::LAST, frame);
//                      MDirection::Ref appref( MDirection::APP, frame);
//                      MEpoch::Convert tosid(obstime, sidref);
                     // Coordinate conversion 
                     MDirection source(source_direction);
//                      MDirection apparent =
//                          MDirection::Convert ( source,
//                                                MDirection::Ref( MDirection::APP,
//                                                                 frame)
//                                                )();
//                      cout << "Apparent coordinates: " <<
//                          apparent << endl;
                     MDirection HADEC =
                         MDirection::Convert ( source,
                                               MDirection::Ref( MDirection::HADEC,
                                                                frame)
                                               )();
                     Vector<Double> dd = HADEC.getAngle().getValue();
                     //cout << dd << endl;

                     // now, make a vector to hold the cartesian coordinates.
                     // Remember, this is HA and Dec, so theta is off by 90
                     // degrees.  And negative hour angels are before, which is
                     // the wrong direction, so take the negative
                     double phi = station_ellipsoid.get_lambda() - dd(0);
                     double theta = dd(1);
                     Vector<double> object_direction(3);
                     object_direction(0) = cos(phi)*cos(theta);
                     object_direction(1) = sin(phi)*cos(theta);
                     object_direction(2) = sin(theta);


                     // Now, go off until we have a really distant position
                     // above the Earth.  We need something like an altitude of
                     // 30 000 km, or 3E7 m
                     ellipsoidal_coord fake_spacecraft;
                     for(double dist = 3E7; ; dist += 1E7) {
                         Vector<double> res(3);
                         for(int i=0;i < 3; i++) {
                             res(i) = station_vector(i)
                                 + dist * object_direction(i);
                         }
                         ellipsoidal_coord fake(res(0),res(1),res(2));
                         if(fake.get_height() > 3E7) {
                             fake_spacecraft = fake;
                             break;
                         }
                     }
                     //cerr << "have fake position " << fake_spacecraft.get_longitude() << " " << fake_spacecraft.get_latitude() << " " << fake_spacecraft.get_height() << endl;

                     // Ok, now print out the information
                     fprintf(fp, "%14.9f %14.9f %14.4f     %14.9f %14.9f %14.4f    %s\n",
                             station_ellipsoid.get_latitude(),
                             station_ellipsoid.get_longitude(),
                             station_ellipsoid.get_height(),
                             fake_spacecraft.get_latitude(),
                             fake_spacecraft.get_longitude(),
                             fake_spacecraft.get_height(),
                             FusionBuffer
                             );
                 }




                 

                 //cerr << "Doing time " << this_MVTime << " for source " << this_source_name << " has FusionTime " << FusionBuffer << endl;
             }
        } // for scan over nscan


        fclose(fp);

    } // for stationcnt over stations


    return;
}











const String& scan_processor::getRootDir( void ) const
{
    const Experiment& expref( this->getExperiment() );
    return (expref.getRootDir());
}














bool scan_processor::readMFCSetup( mfcSetup& result, const String& mfcfile )
{
    //
    // Check status of mfcfile
    //
    struct stat    sb;

    if( ::stat(mfcfile.c_str(), &sb) )
    {
        cout << "readMFCSetup: Problem with " << mfcfile << "?\n"
             << "              " << ::strerror(errno) << endl;
        return false;
    }

    if( (sb.st_mode&S_IFMT)!=S_IFREG )
    {
        cout << "readMFCSetup: File " << mfcfile << " is not a regular file?!"
             << endl;
        return false;
    }

    //
    // Typedef to indicate the status of the parser
    //
    typedef enum _match
    {
        srcaliasbeg = 0x1, srcaliasend=(0x1<<2), crmshuffle=(0x1<<3),
        sourceshuffle=(0x1<<4), other=(0x1<<5)
    } match;

    typedef struct _bogus
    {
        bool done( unsigned int r )
            {
                return ((r&srcaliasend) && (r&crmshuffle) && (r&sourceshuffle));
            }
    } bogus;
    //
    // Open and parse the file....
    //
    int                     lineNr;
    bool                    readalias;
    char                    linebuf[1024];
    char*                   commentptr;
    bogus                   grmbl;
    match                   curmatch;
    ifstream                mfc( mfcfile.c_str(), ios::in );
    vector<int>             phys2log = scan_processor::defaultPhysToLogMap();
    vector<int>             phys2src = scan_processor::defaultPhysToSrcMap();
    unsigned int            itemsRead;
    RegularExpression       rx_allws( "^[ \t]*$" );
    RegularExpression       rx_srcaliasbeg( "^[ \t]*source_alias" );
    RegularExpression       rx_srcaliasend( "^[ \t]*end_alias" );
    RegularExpression       rx_crmshuffle( "^[ \t]*crm_override[ \t]+" );
    RegularExpression       rx_srcshuffle( "^[ \t]*source_override[ \t]+" );
    map<string,mfcSrcAlias> alias;

    //
    // Read until either end-of-file or we have read all we need to reed :)
    lineNr    = 0;
    readalias = false;
    itemsRead = 0;	

    while( !mfc.eof() && !grmbl.done(itemsRead) )
    {
        mfc.getline( linebuf, sizeof(linebuf) );
        lineNr++;

        if( (commentptr=::strchr(linebuf,'#'))!=0 )
        {
            *commentptr = '\0';
        }
		
        // Nothing but whitespace? -> continue
        if( linebuf==rx_allws )
        {
            continue;
        }

        //
        // Find out what the line was...
        //
        curmatch = other;
        if( linebuf==rx_srcaliasbeg )
        {
            curmatch = srcaliasbeg;
        }
        else if( linebuf==rx_srcaliasend )
        {
            curmatch = srcaliasend;
        }
        else if( linebuf==rx_crmshuffle )
        {
            curmatch = crmshuffle;
        }
        else if( linebuf==rx_srcshuffle )
        {
            curmatch = sourceshuffle;
        }
	
        //
        // Now see how this fits into our current state....
        //
        switch( curmatch )
        {
            case srcaliasbeg:
            {
                if( readalias )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Found start of sourcealias inside a sourcealias block!"
                         << endl;
                    break;
                }
	
                if( itemsRead&srcaliasend )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Duplicate sourcealias block?!"
                         << endl;
                    break;
                }
                readalias = true;
            }
            break;
					
            case srcaliasend:
            {
                if( !readalias )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   found end of sourcealias outside of sourcealias block!"
                         << endl;
                    break;
                }
                if( itemsRead&srcaliasend )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Found duplicate sourcealias end?!"
                         << endl;
                    break;
                }
	
                //
                // Switch off reading of sourcealiases
                // And indicate in itemsread that we've read a complete sourcealias
                // block
                //
                readalias  = false;
                itemsRead |= srcaliasend;
            }
            break;
	
            case crmshuffle:
            {
                if( readalias )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Found crm override inside a sourcealias block?!"
                         << endl;
                    break;
                }
                // test if we already have seen a line 'quite like this'
                if( itemsRead&crmshuffle )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Found duplicate crm override. Discarding this one."
                         << endl;
                    break;
                }
                //
                // The phys2log map has already been initialized with the default
                // map. We are going to overwrite it with unused entries. (using
                // the 'default' initialization makes sure our vector has the
                // correct length!)
                //
                vector<int>::iterator   ptr = phys2log.begin();
		
                for( ; ptr!=phys2log.end(); *ptr=-1, ptr++ );
		
                //
                // Ok. Read all integers after the crm-shuffle command...
                //
                int         tmp;
                char*       args = ::strpbrk( linebuf, " \t" );
                istrstream  argstrm( args );
		
                ptr = phys2log.begin();
                while( !argstrm.eof() && ptr!=phys2log.end())
                {
                    argstrm >> tmp;
                    *ptr = tmp;
                    ptr++;
                }
		
                //
                // Indicate we have read another of the required items!
                //
                itemsRead |= crmshuffle;
            }
            break;
	
            case sourceshuffle:
            {
                if( readalias )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Found source override inside a sourcealias block?!"
                         << endl;
                    break;
                }
                // test if we already have seen a line 'quite like this'
                if( itemsRead&sourceshuffle )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Found duplicate source override. Discarding this one."
                         << endl;
                    break;
                }
                //
                // The phys2src map has already been initialized with the default
                // map. We are going to overwrite it with unused entries. (using
                // the 'default' initialization makes sure our vector has the
                // correct length!)
                //
                vector<int>::iterator   ptr = phys2src.begin();
		
                for( ; ptr!=phys2src.end(); *ptr=-1, ptr++ );
		
                //
                // Ok. Read all integers after the source shuffle command...
                //
                int         tmp;
                char*       args = ::strpbrk( linebuf, " \t" );
                istrstream  argstrm( args );
		
                ptr = phys2src.begin();
                while( !argstrm.eof() && ptr!=phys2src.end() )
                {
                    argstrm >> tmp;
                    *ptr = tmp;
                    ptr++;
                }
		
                //
                // Indicate we have read another of the required items!
                //
                itemsRead |= sourceshuffle;
            }
            break;
			
            case other:
            {
                // if we get to here we'd better be reading aliases....
                // otherwise the file is fuckedup!
                if( !readalias )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Unrecognized configline!"
                         << endl;
                    break;
                }
                //
                // format of the line should be:
                // <orgsrcname> <field1name> <field2name> ... <fieldNname>
                //
                string                            src;
                mfcSrcAlias                       curalias;
                istrstream                        strm( linebuf );
                map<string,mfcSrcAlias>::iterator ptr;
	
                // step 1: read the original sourcename
                strm >> src;

                // step 2: check if not duplicated
                if( (ptr=alias.find(src))!=alias.end() )
                {
                    cout << "readMFCSetup(" << lineNr << "): Syntax error\n"
                         << "   Duplicate sourcealias line for source " << src
                         << endl;
                    break;
                }
	
                // read all aliases...
                while( !strm.eof() )
                {
                    string   name;
							
                    strm >> name;
                    curalias.aliases.push_back( name );
                }
					
                // now define the map-entry
                alias[ src ] = curalias;
            }
            break;
					
            default:
                cout << "readMFCSetup(" << lineNr << ") - case default! AARGH\n"
                     << "   call H. Verkouter +31521596516 (verkouter@jive.nl)"
                     << endl;	 
        }
    }

    if( !grmbl.done(itemsRead) )
    {
        cout << "readMFCSetup: Failed to read all necessary info from\n"
             << "  " << mfcfile << endl;

        if( !(itemsRead&srcaliasend) )
        {
            cout << ">> Missing source_alias block" << endl;
        }
        if( !(itemsRead&crmshuffle) )
        {
            cout << ">> Missing crm_override statement" << endl;
        }
        if( !(itemsRead&sourceshuffle) )
        {
            cout << ">> Missing source_override statement" << endl;
        }
    }
    else
    {
        result.physToLogMap         = phys2log;
        result.srcToAliasMap        = alias;
        result.physToFieldCenterMap = phys2src;
    }
    return grmbl.done(itemsRead);
}



vector<int> scan_processor::defaultPhysToLogMap( void )
{
    vector<int>            retval( 16 );
    vector<int>::iterator  ptr = retval.begin();

    for( int i=0; ptr!=retval.end(); *ptr=i, i++, ptr++ );

    return retval;
}

vector<int> scan_processor::defaultPhysToSrcMap( void )
{
    return vector<int>( 16, 0 );
}






}  // end namespace





#include <aips/Utilities/String.h>




using namespace JMA_VEX_AREA;



int main(int argc, char* argv[])
{
    if(argc != 4) {
        cerr << "Error: correct usage is " << argv[0] << " VexFile time_step(s) interpolation_time(s)\n";
        exit(2);
    }
    String options("");
    String vex_file(argv[1]);
    String experiment_path(".");

    

    
    VEXperiment project(options, vex_file, experiment_path);

    //cerr << "got experiment\n";

    double time_step = atof(argv[2]);
    double interpolation_time = atof(argv[3]);

    scan_processor VEX_processor(&project, time_step, interpolation_time);

    //cerr << "finished\n";

    return 0;
}
    

